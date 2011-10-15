#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

(define registers)

(define (initialize-package!)
  (set! registers (make-vector 20 #f))
  unspecific)

(define-structure (re-registers (type-descriptor <re-registers>))
  (vector #f read-only #t))

(define (guarantee-re-registers object procedure)
  (if (not (re-registers? object))
      (error:wrong-type-argument object "regular-expression registers"
				 procedure))
  (re-registers-vector object))

(define (re-match-start-index i #!optional regs)
  (guarantee-re-register i 'RE-MATCH-START-INDEX)
  (vector-ref (if (or (default-object? regs) (not regs))
		  registers
		  (guarantee-re-registers regs 'RE-MATCH-START-INDEX))
	      i))

(define (re-match-end-index i #!optional regs)
  (guarantee-re-register i 'RE-MATCH-END-INDEX)
  (vector-ref (if (or (default-object? regs) (not regs))
		  registers
		  (guarantee-re-registers regs 'RE-MATCH-START-INDEX))
	      (fix:+ i 10)))

(define (guarantee-re-register i operator)
  (if (not (and (exact-nonnegative-integer? i) (< i 10)))
      (error:wrong-type-argument i "regular-expression register" operator)))

(define (re-registers)
  (make-re-registers (vector-copy registers)))

(define (set-re-registers! regs)
  (let ((regs (guarantee-re-registers regs 'SET-RE-REGISTERS!)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= 20 i))
      (vector-set! registers i (vector-ref regs i)))))

(define (preserving-re-registers thunk)
  (let ((registers* unspecific))
    (dynamic-wind (lambda () (set! registers* (re-registers)) unspecific)
		  thunk
		  (lambda () (set-re-registers! registers*)))))

(define (re-match-extract string regs i)
  (let ((start (re-match-start-index i regs))
	(end (re-match-end-index i regs)))
    (if (not (and start end))
	(error:bad-range-argument i 'RE-MATCH-EXTRACT))
    (substring string start end)))

(define (make-substring-operation primitive)
  (lambda (regexp string start end #!optional case-fold? syntax-table)
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
	 (list-transform-positive alternatives identity-procedure)))
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
	    (let ((ranges
		   (push #\^ (pull #\- (pull #\] (compute-ranges chars))))))
	      (let ((n
		     (let loop ((ranges ranges) (n 2))
		       (if (pair? ranges)
			   (loop (cdr ranges)
				 (fix:+ n (if (pair? (car ranges)) 3 1)))
			   n))))
		(let ((s (make-string n)))
		  (string-set! s 0 #\[)
		  (let loop ((ranges ranges) (i 1))
		    (if (pair? ranges)
			(loop (cdr ranges)
			      (let ((range (car ranges)))
				(if (pair? range)
				    (begin
				      (string-set! s i (car range))
				      (string-set! s (fix:+ i 1) #\-)
				      (string-set! s (fix:+ i 2) (cdr range))
				      (fix:+ i 3))
				    (begin
				      (string-set! s i range)
				      (fix:+ i 1)))))
			(string-set! s i #\])))
		  s)))
	    (re-quote-string (string (car chars))))
	"")))