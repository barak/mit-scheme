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

;;;; Regular Expressions
;;; package: (runtime regular-expression)

(declare (usual-integrations))

(define-deferred registers (make-vector 20 #f))

(define (re-register? object)
  (and (index-fixnum? object)
       (fix:< object 10)))

(define (re-registers)
  (make-re-registers (vector-copy registers)))

(define (set-re-registers! regs)
  (vector-copy! registers 0 (re-registers-vector regs)))

(define-record-type <re-registers>
    (make-re-registers vector)
    re-registers?
  (vector re-registers-vector))

(define (re-match-start-index i #!optional regs)
  (guarantee re-register? i 're-match-start-index)
  (vector-ref (if (or (default-object? regs) (not regs))
		  registers
		  (re-registers-vector regs))
	      i))

(define (re-match-end-index i #!optional regs)
  (guarantee re-register? i 're-match-end-index)
  (vector-ref (if (default-object? regs)
		  registers
		  (re-registers-vector regs))
	      (fix:+ i 10)))

(define (re-match-extract string regs i)
  (let ((start (re-match-start-index i regs))
	(end (re-match-end-index i regs)))
    (if (not (and start end))
	(error:bad-range-argument i 're-match-extract))
    (substring string start end)))

(define (preserving-re-registers thunk)
  (let ((registers* unspecific))
    (dynamic-wind (lambda ()
		    (set! registers* (re-registers))
		    unspecific)
		  thunk
		  (lambda ()
		    (set-re-registers! registers*)
		    (set! registers*)
		    unspecific))))

(define (make-substring-operation primitive)
  (lambda (regexp string start end #!optional case-fold? syntax-table)
    (guarantee 8-bit-string? string)
    (let ((regexp
	   (if (compiled-regexp? regexp)
	       regexp
	       (re-compile-pattern regexp
				   (if (default-object? case-fold?)
				       #f
				       case-fold?))))
	  (regs (make-vector 20 #f)))
      (and (primitive (compiled-regexp/byte-stream regexp)
		      (compiled-regexp/translation-table regexp)
		      (char-syntax-table/entries
		       (if (or (default-object? syntax-table)
			       (not syntax-table))
			   standard-char-syntax-table
			   syntax-table))
		      regs string start end)
	   (make-re-registers regs)))))

(define re-substring-match
  (make-substring-operation (ucode-primitive re-match-substring)))

(define re-substring-search-forward
  (make-substring-operation (ucode-primitive re-search-substring-forward)))

(define re-substring-search-backward
  (make-substring-operation (ucode-primitive re-search-substring-backward)))

(define (make-string-operation substring-operation)
  (lambda (regexp string #!optional case-fold? syntax-table)
    (substring-operation regexp string 0 (string-length string)
			 (if (default-object? case-fold?) #f case-fold?)
			 (if (default-object? syntax-table) #f syntax-table))))

(define re-string-match
  (make-string-operation re-substring-match))

(define re-string-search-forward
  (make-string-operation re-substring-search-forward))

(define re-string-search-backward
  (make-string-operation re-substring-search-backward))

(define (regexp-group . alternatives)
  (let ((alternatives
	 (filter identity-procedure alternatives)))
    (if (null? alternatives)
	"\\(\\)"
	(apply string-append
	       (cons "\\("
		     (let loop ((alternatives alternatives))
		       (cons (car alternatives)
			     (if (null? (cdr alternatives))
				 (list "\\)")
				 (cons "\\|" (loop (cdr alternatives)))))))))))

(define (char-set->regexp char-set)

  (define (compute-ranges chars)
    (let outer ((chars chars) (ranges '()))
      (if (pair? chars)
	  (let ((start (car chars)))
	    (receive (chars end) (find-range-end (cdr chars) start)
	      (outer chars
		     (cons (if (char=? end start)
			       start
			       (cons start end))
			   ranges))))
	  (reverse! ranges))))

  (define (find-range-end chars start)
    (if (special? start)
	(values chars start)
	(let loop ((chars chars) (end start))
	  (cond ((and (pair? chars)
		      (fix:= (fix:+ (char->integer end) 1)
			     (char->integer (car chars))))
		 (loop (cdr chars) (car chars)))
		((special? end)
		 (values (cons end chars)
			 (integer->char (fix:- (char->integer end) 1))))
		(else
		 (values chars end))))))

  (define (special? char)
    (or (char=? char #\^)
	(char=? char #\-)
	(char=? char #\])))

  (define (pull char ranges)
    (if (memv char ranges)
	(cons char (delv! char ranges))
	ranges))

  (define (push char ranges)
    (if (and (pair? ranges) (eqv? (car ranges) char))
	(append! (cdr ranges) (list char))
	ranges))

  (let ((chars (char-set-members char-set)))
    (if (pair? chars)
	(if (pair? (cdr chars))
	    (let ((builder (string-builder)))
	      (builder #\[)
	      (let loop
		  ((ranges
		    (push #\^ (pull #\- (pull #\] (compute-ranges chars))))))
		(if (pair? ranges)
		    (begin
		      (let ((range (car ranges)))
			(if (pair? range)
			    (begin
			      (builder (car range))
			      (builder #\-)
			      (builder (cdr range)))
			    (builder range)))
		      (loop (cdr ranges)))))
	      (builder #\])
	      (builder))
	    (re-quote-string (string (car chars))))
	"")))