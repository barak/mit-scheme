;;; -*-Scheme-*-
;;;
;;;	$Id: utils.scm,v 1.38 1994/08/24 19:57:15 adams Exp $
;;;
;;;	Copyright (c) 1986, 1989-94 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;
;;; NOTE: Parts of this program (Edwin) were created by translation
;;; from corresponding parts of GNU Emacs.  Users should be aware that
;;; the GNU GENERAL PUBLIC LICENSE may apply to these parts.  A copy
;;; of that license should have been included along with this file.
;;;

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
  (make-condition-type 'ALLOCATION-FAILURE condition-type:error
      '(OPERATOR N-WORDS)
    (lambda (condition port)
      (let ((operator (access-condition condition 'OPERATOR)))
	(if operator
	    (begin
	      (write-string "The procedure " port)
	      (write operator port)
	      (write-string " is unable" port))
	    (write-string "Unable" port)))
      (write-string " to allocate " port)
      (write (access-condition condition 'N-WORDS) port)
      (write-string " words of storage." port))))

(define error:allocation-failure
  (condition-signaller condition-type:allocation-failure
		       '(N-WORDS OPERATOR)
		       standard-error-handler))

(define-macro (chars-to-words-shift)
  ;; This is written as a macro so that the shift will be a constant
  ;; in the compiled code.
  ;; It does not work when cross-compiled!
  (let ((chars-per-word (vector-ref ((ucode-primitive gc-space-status 0)) 0)))
    (case chars-per-word
      ((4) -2)
      ((8) -3)
      (else (error "Can't support this word size:" chars-per-word)))))

(define (edwin-string-allocate n-chars)
  (if (not (fix:fixnum? n-chars))
      (error:wrong-type-argument n-chars "fixnum" 'STRING-ALLOCATE))
  (if (not (fix:>= n-chars 0))
      (error:bad-range-argument n-chars 'STRING-ALLOCATE))
  (with-interrupt-mask interrupt-mask/none
    (lambda (mask)
      (let ((n-words (fix:+ (fix:lsh n-chars (chars-to-words-shift)) 3)))
	(if (not ((ucode-primitive heap-available? 1) n-words))
	    (with-interrupt-mask interrupt-mask/gc-normal
	      (lambda (ignore)
		ignore			; ignored
		(guarantee-heap-available n-words 'STRING-ALLOCATE mask))))
	(let ((result ((ucode-primitive primitive-get-free 1)
		       (ucode-type string))))
	  ((ucode-primitive primitive-object-set! 3)
	   result
	   0
	   ((ucode-primitive primitive-object-set-type 2)
	    (ucode-type manifest-nm-vector)
	    (fix:- n-words 1)))
	  (set-string-length! result n-chars)
	  ;; This won't work if range-checking is turned on.
	  (string-set! result n-chars #\nul)
	  ((ucode-primitive primitive-increment-free 1) n-words)
	  (set-interrupt-enables! mask)
	  result)))))

(define (edwin-set-string-maximum-length! string n-chars)
  (if (not (string? string))
      (error:wrong-type-argument string "string" 'SET-STRING-MAXIMUM-LENGTH!))
  (if (not (fix:fixnum? n-chars))
      (error:wrong-type-argument n-chars "fixnum" 'SET-STRING-MAXIMUM-LENGTH!))
  (if (not (and (fix:>= n-chars 0)
		(fix:< n-chars
		       (fix:lsh (fix:- (system-vector-length string) 1)
				(fix:- 0 (chars-to-words-shift))))))
      (error:bad-range-argument n-chars 'SET-STRING-MAXIMUM-LENGTH!))
  (let ((mask (set-interrupt-enables! interrupt-mask/none)))
    ((ucode-primitive primitive-object-set! 3)
     string
     0
     ((ucode-primitive primitive-object-set-type 2)
      (ucode-type manifest-nm-vector)
      (fix:+ (fix:lsh n-chars (chars-to-words-shift)) 2)))
    (set-string-length! string n-chars)
    ;; This won't work if range-checking is turned on.
    (string-set! string n-chars #\nul)
    (set-interrupt-enables! mask)
    unspecific))

(define string-allocate
  (if (compiled-procedure? edwin-string-allocate)
      edwin-string-allocate
      (ucode-primitive string-allocate)))

(define set-string-maximum-length!
  (if (compiled-procedure? edwin-set-string-maximum-length!)
      edwin-set-string-maximum-length!
      (ucode-primitive set-string-maximum-length!)))

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

(define (string-append-char string char)
  (let ((size (string-length string)))
    (let ((result (string-allocate (fix:+ size 1))))
      (%substring-move! string 0 size result 0)
      (string-set! result size char)
      result)))

(define (string-append-substring string1 string2 start2 end2)
  (let ((length1 (string-length string1)))
    (let ((result (string-allocate (fix:+ length1 (fix:- end2 start2)))))
      (%substring-move! string1 0 length1 result 0)
      (%substring-move! string2 start2 end2 result length1)
      result)))

(define (string-greatest-common-prefix strings)
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

(define char-set:null
  (char-set))

(define char-set:return
  (char-set #\Return))

(define char-set:not-space
  (char-set-invert (char-set #\Space)))

(define char-set:not-graphic
  (char-set-invert char-set:graphic))

(define (char-controlify char)
  (if (ascii-controlified? char)
      char
      (make-char (char-code char)
		 (let ((bits (char-bits char)))
		   (if (odd? (quotient bits 2)) bits (+ bits 2))))))

(define (char-controlified? char)
  (or (ascii-controlified? char)
      (odd? (quotient (char-bits char) 2))))

(define (char-metafy char)
  (make-char (char-code char)
	     (let ((bits (char-bits char)))
	       (if (odd? bits) bits (1+ bits)))))

(define-integrable (char-metafied? char)
  (odd? (char-bits char)))

(define (char-control-metafy char)
  (char-controlify (char-metafy char)))

(define (char-base char)
  (make-char (char-code char) 0))

(define (read-line #!optional port)
  (read-string char-set:return
	       (if (default-object? port)
		   (current-input-port)
		   (guarantee-input-port port))))

(define (y-or-n? . strings)
  (define (loop)
    (let ((char (char-upcase (read-char))))
      (cond ((or (char=? char #\Y)
		 (char=? char #\Space))
	     (write-string "Yes")
	     true)
	    ((or (char=? char #\N)
		 (char=? char #\Rubout))
	     (write-string "No")
	     false)
	    (else
	     (if (not (char=? char #\newline))
		 (beep))
	     (loop)))))
  (newline)
  (for-each write-string strings)
  (loop))

(define (catch-file-errors if-error thunk)
  (call-with-protected-continuation
   (lambda (continuation)
     (bind-condition-handler (list condition-type:file-error
				   condition-type:port-error)
	 (lambda (condition)
	   condition
	   (continuation (if-error)))
       thunk))))

(define (delete-directory-no-errors filename)
  (catch-file-errors (lambda () #f)
		     (lambda () (delete-directory filename) #t)))

(define (string-or-false? object)
  ;; Useful as a type for option variables.
  (or (false? object)
      (string? object)))

(define (list-of-strings? object)
  (list-of-type? object string?))

(define (list-of-type? object predicate)
  (and (list? object)
       (for-all? object predicate)))

(define (dotimes n procedure)
  (define (loop i)
    (if (< i n)
	(begin (procedure i)
	       (loop (1+ i)))))
  (loop 0))

(define make-strong-eq-hash-table
  (strong-hash-table/constructor eq-hash-mod eq? #t))

(define make-weak-equal-hash-table
  (weak-hash-table/constructor equal-hash-mod equal? #t))

(define (weak-assq item alist)
  (let loop ((alist alist))
    (and (not (null? alist))
	 (if (eq? (weak-car (car alist)) item)
	     (car alist)
	     (loop (cdr alist))))))