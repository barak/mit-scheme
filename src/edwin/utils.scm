#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; Editor Utilities

(declare (usual-integrations))

;; Allow gc and after-gc hooks.

(define-integrable interrupt-mask/gc-normal #x0025)

(define (guarantee-heap-available n-words operator old-mask)
  (gc-flip)
  (if (not ((ucode-primitive heap-available? 1) n-words))
      (begin
	(set-interrupt-enables! old-mask)
	(error:allocation-failure n-words operator))))

(define condition-type:allocation-failure
  (make-condition-type 'allocation-failure condition-type:error
      '(operator n-words)
    (lambda (condition port)
      (let ((operator (access-condition condition 'operator)))
	(if operator
	    (begin
	      (write-string "The procedure " port)
	      (write operator port)
	      (write-string " is unable" port))
	    (write-string "Unable" port)))
      (write-string " to allocate " port)
      (write (access-condition condition 'n-words) port)
      (write-string " words of storage." port))))

(define error:allocation-failure
  (condition-signaller condition-type:allocation-failure
		       '(N-WORDS OPERATOR)
		       standard-error-handler))

(define (allocate-buffer-storage n-chars)
  ;; Too much of Edwin relies on fixnum-specific arithmetic for this
  ;; to be safe.  Unfortunately, this means that Edwin can't edit
  ;; files >32MB.
  (guarantee index-fixnum? n-chars 'allocate-buffer-storage)
  (make-string n-chars))

(define-syntax chars-to-words-shift
  (sc-macro-transformer
   (lambda (form environment)
     form environment
     ;; This is written as a macro so that the shift will be a constant
     ;; in the compiled code.
     ;; It does not work when cross-compiled!
     (let ((chars-per-word (bytes-per-object)))
       (case chars-per-word
	 ((4) -2)
	 ((8) -3)
	 (else (error "Can't support this word size:" chars-per-word)))))))

(define-integrable (chars->words n-chars)
  (fix:lsh (fix:+ (fix:+ n-chars 1)	;Add 1 for NUL termination.
		  (fix:not (fix:lsh -1 (fix:- 0 (chars-to-words-shift)))))
	   (chars-to-words-shift)))

(define (edwin-string-allocate n-chars)
  (if (not (fix:fixnum? n-chars))
      (error:wrong-type-argument n-chars "fixnum" 'string-allocate))
  (if (not (fix:>= n-chars 0))
      (error:bad-range-argument n-chars 'string-allocate))
  (with-interrupt-mask interrupt-mask/none
    (lambda (mask)
      (let ((n-words			;Add two, for manifest & length.
	     (fix:+ 2 (chars->words (fix:+ n-chars 1)))))
	(if (not ((ucode-primitive heap-available? 1) n-words))
	    (with-interrupt-mask interrupt-mask/gc-normal
	      (lambda (ignore)
		ignore			; ignored
		(guarantee-heap-available n-words 'string-allocate mask))))
	(let ((result ((ucode-primitive primitive-get-free 1)
		       (ucode-type string))))
	  ((ucode-primitive primitive-object-set! 3)
	   result
	   0
	   ((ucode-primitive primitive-object-set-type 2)
	    (ucode-type manifest-nm-vector)
	    (fix:- n-words 1)))		;Subtract one for the manifest.
	  (set-string-length! result (fix:+ n-chars 1))
	  (string-set! result n-chars #\nul)
	  (set-string-length! result n-chars)
	  ((ucode-primitive primitive-increment-free 1) n-words)
	  (set-interrupt-enables! mask)
	  result)))))

(define string-allocate
  (if (compiled-procedure? edwin-string-allocate)
      edwin-string-allocate
      (ucode-primitive string-allocate)))

(define (%substring-move! source start-source end-source
			  target start-target)
  (cond ((not (fix:< start-source end-source))
	 unspecific)
	((not (eq? source target))
	 (if (fix:< (fix:- end-source start-source) 32)
	     (do ((scan-source start-source (fix:+ scan-source 1))
		  (scan-target start-target (fix:+ scan-target 1)))
		 ((fix:= scan-source end-source) unspecific)
	       (string-set! target
			    scan-target
			    (string-ref source scan-source)))
	     (substring-move-left! source start-source end-source
				   target start-target)))
	((fix:< start-source start-target)
	 (if (fix:< (fix:- end-source start-source) 32)
	     (do ((scan-source end-source (fix:- scan-source 1))
		  (scan-target
		   (fix:+ start-target (fix:- end-source start-source))
		   (fix:- scan-target 1)))
		 ((fix:= scan-source start-source) unspecific)
	       (string-set! source
			    (fix:- scan-target 1)
			    (string-ref source (fix:- scan-source 1))))
	     (substring-move-right! source start-source end-source
				    source start-target)))
	((fix:< start-target start-source)
	 (if (fix:< (fix:- end-source start-source) 32)
	     (do ((scan-source start-source (fix:+ scan-source 1))
		  (scan-target start-target (fix:+ scan-target 1)))
		 ((fix:= scan-source end-source) unspecific)
	       (string-set! source
			    scan-target
			    (string-ref source scan-source)))
	     (substring-move-left! source start-source end-source
				   source start-target)))))

(define (string-greatest-common-prefix strings)
  (let loop
      ((strings (cdr strings))
       (string (car strings))
       (index (string-length (car strings))))
    (if (null? strings)
	(substring string 0 index)
	(let ((string* (car strings)))
	  (let ((index* (string-match-forward string string*)))
	    (if (< index* index)
		(loop (cdr strings) string* index*)
		(loop (cdr strings) string index)))))))

(define (string-greatest-common-prefix-ci strings)
  (let loop
      ((strings (cdr strings))
       (string (car strings))
       (index (string-length (car strings))))
    (if (null? strings)
	(substring string 0 index)
	(let ((string* (car strings)))
	  (let ((index* (string-match-forward-ci string string*)))
	    (if (< index* index)
		(loop (cdr strings) string* index*)
		(loop (cdr strings) string index)))))))

(define (string-append-separated x y)
  (cond ((string-null? x) y)
	((string-null? y) x)
	(else (string-append x " " y))))

(define (substring->nonnegative-integer line start end)
  (let loop ((index start) (n 0))
    (if (fix:= index end)
	n
	(let ((k (fix:- (vector-8b-ref line index) (char->integer #\0))))
	  (and (fix:>= k 0)
	       (fix:< k 10)
	       (loop (fix:+ index 1) (+ (* n 10) k)))))))

(define char-set:null
  (char-set))

(define char-set:return
  (char-set #\return))

(define char-set:not-space
  (char-set-invert (char-set #\space)))

(define (merge-bucky-bits char bits)
  (make-char (char-code char)
	     (let ((bits (fix:or (char-bits char) bits)))
	       (if (ascii-controlified? char)
		   (fix:andc bits char-bit:control)
		   bits))))

(define (ascii-controlified? char)
  (fix:< (char-code char) #x20))

(define (char-base char)
  (make-char (char-code char) 0))

(define (y-or-n? . strings)
  (define (loop)
    (let ((char (read-char)))
      (cond ((or (char-ci=? char #\y)
		 (char=? char #\space))
	     (write-string "Yes")
	     #t)
	    ((or (char-ci=? char #\n)
		 (char=? char #\rubout))
	     (write-string "No")
	     #f)
	    (else
	     (if (not (char=? char #\newline))
		 (beep))
	     (loop)))))
  (newline)
  (for-each write-string strings)
  (loop))

(define (delete-directory-no-errors filename)
  (catch-file-errors (lambda (condition) condition #f)
		     (lambda () (delete-directory filename) #t)))

(define (string-or-false? object)
  ;; Useful as a type for option variables.
  (or (false? object)
      (string? object)))

(define (list-of-strings? object)
  (list-of-type? object string?))

(define (list-of-pathnames? object)
  (list-of-type? object
		 (lambda (object) (or (pathname? object) (string? object)))))

(define (list-of-type? object predicate)
  (and (list? object)
       (every predicate object)))

(define (dotimes n procedure)
  (define (loop i)
    (if (< i n)
	(begin (procedure i)
	       (loop (1+ i)))))
  (loop 0))

(define (split-list elements predicate)
  (let loop ((elements elements) (satisfied '()) (unsatisfied '()))
    (if (pair? elements)
	(if (predicate (car elements))
	    (loop (cdr elements) (cons (car elements) satisfied) unsatisfied)
	    (loop (cdr elements) satisfied (cons (car elements) unsatisfied)))
	(values satisfied unsatisfied))))

(define (weak-assq item alist)
  (let loop ((alist alist))
    (and (not (null? alist))
	 (if (eq? (weak-car (car alist)) item)
	     (car alist)
	     (loop (cdr alist))))))

(define (file-time->ls-string time #!optional now)
  ;; Returns a time string like that used by unix `ls -l'.
  (let ((time (file-time->universal-time time))
	(now
	 (if (or (default-object? now) (not now))
	     (get-universal-time)
	     now)))
    (let ((dt (decode-universal-time time))
	  (d2 (lambda (n c) (string-pad-left (number->string n) 2 c))))
      (string-append (month/short-string (decoded-time/month dt))
		     " "
		     (d2 (decoded-time/day dt) #\space)
		     " "
		     (if (<= 0 (- now time) (* 180 24 60 60))
			 (string-append (d2 (decoded-time/hour dt) #\0)
					":"
					(d2 (decoded-time/minute dt) #\0))
			 (string-append " "
					(number->string
					 (decoded-time/year dt))))))))

(define (catch-file-errors if-error thunk)
  (call-with-current-continuation
   (lambda (continuation)
     (bind-condition-handler (list condition-type:file-error
				   condition-type:port-error)
	 (lambda (condition)
	   (continuation (if-error condition)))
       thunk))))