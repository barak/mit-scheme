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

;;;; List-based Regular Expressions

;;; This is an abstraction layer upon regular expressions, to make
;;; them easier to read and write.  Expressions written this way can
;;; be compiled into ordinary regular expressions using REXP->REGEXP.

(declare (usual-integrations))

(define (rexp? rexp)
  (or (string? rexp)
      (char-set? rexp)
      (and (pair? rexp)
	   (list? (cdr rexp))
	   (let ((one-arg
		  (lambda ()
		    (and (fix:= 1 (length (cdr rexp)))
			 (rexp? (cadr rexp))))))
	     (case (car rexp)
	       ((alternatives sequence)
		(every rexp? (cdr rexp)))
	       ((group optional * +)
		(and (one-arg)
		     (not (or (and (string? rexp)
				   (string-null? rexp))
			      (and (pair? rexp)
				   (memq (car rexp) boundary-rexp-types))))))
	       ((case-fold)
		(and (fix:= 1 (length (cdr rexp)))
		     (string? (cadr exp))))
	       ((any-char line-start line-end string-start string-end
			  word-edge not-word-edge word-start word-end
			  word-char not-word-char)
		(null? (cdr rexp)))
	       ((syntax-char not-syntax-char)
		(and (one-arg)
		     (assq (cadr rexp) syntax-type-alist)))
	       (else #f))))))

(define boundary-rexp-types
  '(line-start line-end string-start string-end word-edge not-word-edge
	       word-start word-end))

(define syntax-type-alist
  '((whitespace . " ")
    (punctuation . ".")
    (word . "w")
    (symbol . "_")
    (open . "(")
    (close . ")")
    (quote . "\'")
    (string-delimiter . "\"")
    (math-delimiter . "$")
    (escape . "\\")
    (char-quote . "/")
    (comment-start . "<")
    (comment-end . ">")))

(define (rexp-alternatives . rexps)
  `(alternatives ,@rexps))

(define (rexp-sequence . rexps)
  (let ((rexps (simplify-sequence-args rexps)))
    (if (pair? rexps)
	(if (pair? (cdr rexps))
	    `(sequence ,@rexps)
	    (car rexps))
	"")))

(define (simplify-sequence-args rexps)
  (append-map (lambda (rexp)
		(cond ((and (string? rexp) (string-null? rexp))
		       '())
		      ((and (pair? rexp) (eq? 'sequence (car rexp)))
		       (cdr rexp))
		      ((and (pair? rexp) (eq? 'alternatives (car rexp)))
		       (list `(group ,rexp)))
		      (else
		       (list rexp))))
	      rexps))

(define (rexp-group . rexps)
  (let ((rexp (apply rexp-sequence rexps)))
    (if (and (pair? rexp) (eq? (car rexp) 'group))
	rexp
	`(group ,rexp))))

(define (rexp-optional . rexps)
  `(optional ,(rexp-groupify (apply rexp-sequence rexps))))

(define (rexp* . rexps)
  `(* ,(rexp-groupify (apply rexp-sequence rexps))))

(define (rexp+ . rexps)
  `(+ ,(rexp-groupify (apply rexp-sequence rexps))))

(define (rexp-groupify rexp)
  (let ((group (lambda () `(group ,rexp)))
	(no-group (lambda () (error "Expression can't be grouped:" rexp))))
    (cond ((string? rexp)
	   (case (string-length rexp)
	     ((0) (no-group))
	     ((1) rexp)
	     (else (group))))
	  ((pair? rexp)
	   (cond ((memq (car rexp) boundary-rexp-types)
		  (no-group))
		 ((memq (car rexp) '(alternatives sequence optional * +))
		  (group))
		 (else rexp)))
	  (else rexp))))

(define (rexp-any-char) `(any-char))
(define (rexp-line-start) `(line-start))
(define (rexp-line-end) `(line-end))
(define (rexp-string-start) `(string-start))
(define (rexp-string-end) `(string-end))
(define (rexp-word-edge) `(word-edge))
(define (rexp-not-word-edge) `(not-word-edge))
(define (rexp-word-start) `(word-start))
(define (rexp-word-end) `(word-end))
(define (rexp-word-char) `(word-char))
(define (rexp-not-word-char) `(not-word-char))
(define (rexp-syntax-char type) `(syntax-char ,type))
(define (rexp-not-syntax-char type) `(not-syntax-char ,type))

(define (rexp-case-fold rexp)
  (cond ((or (string? rexp) (char-set? rexp))
	 `(case-fold ,rexp))
	((and (pair? rexp)
	      (memq (car rexp) '(alternatives sequence group optional * +))
	      (list? (cdr rexp)))
	 (cons (car rexp)
	       (map rexp-case-fold (cdr rexp))))
	(else rexp)))

(define (rexp-compile rexp)
  (re-compile-pattern (rexp->regexp rexp) #f))

(define (rexp->regexp rexp)
  (let ((lose (lambda () (error "Malformed rexp:" rexp))))
    (cond ((string? rexp)
	   (re-quote-string rexp))
	  ((char-set? rexp)
	   (char-set->regexp rexp))
	  ((and (pair? rexp) (list? (cdr rexp)))
	   (let ((one-arg
		  (lambda ()
		    (if (fix:= 1 (length (cdr rexp)))
			(cadr rexp)
			(lose))))
		 (rexp-args (lambda () (map rexp->regexp (cdr rexp)))))
	     (let ((rexp-arg (lambda () (rexp->regexp (one-arg))))
		   (syntax-type
		    (lambda ()
		      (let ((entry (assq (one-arg) syntax-type-alist)))
			(if entry
			    (cdr entry)
			    (lose))))))
	       (case (car rexp)
		 ((alternatives)
		  (decorated-string-append "" "\\|" "" (rexp-args)))
		 ((sequence) (apply string-append (rexp-args)))
		 ((group) (string-append "\\(" (rexp-arg) "\\)"))
		 ((optional) (string-append (rexp-arg) "?"))
		 ((*) (string-append (rexp-arg) "*"))
		 ((+) (string-append (rexp-arg) "+"))
		 ((case-fold)
		  (rexp->regexp
		   (let ((arg (one-arg)))
		     (cond ((string? arg) (case-fold-string arg))
			   ((char-set? arg) (case-fold-char-set arg))
			   (else (lose))))))
		 ((any-char) ".")
		 ((line-start) "^")
		 ((line-end) "$")
		 ((string-start) "\\`")
		 ((string-end) "\\'")
		 ((word-edge) "\\b")
		 ((not-word-edge) "\\B")
		 ((word-start) "\\<")
		 ((word-end) "\\>")
		 ((word-char) "\\w")
		 ((not-word-char) "\\W")
		 ((syntax-char) (string-append "\\s" (syntax-type)))
		 ((not-syntax-char) (string-append "\\S" (syntax-type)))
		 (else (lose))))))
	  (else (lose)))))

(define (case-fold-string s)
  (let ((end (string-length s)))
    (let loop ((start 0) (parts '()))
      (let ((index
	     (string-find-next-char-in-set s char-set:alphabetic start end)))
	(if index
	    (loop (fix:+ index 1)
		  (cons* (let ((char (string-ref s index)))
			   (string-append "["
					  (string (char-upcase char))
					  (string (char-downcase char))
					  "]"))
			 (re-quote-string (substring s start index))
			 parts))
	    (apply string-append
		   (reverse!
		    (cons (re-quote-string (substring s start end))
			  parts))))))))

(define (case-fold-char-set c)
  (let loop ((chars (char-set-members c)) (chars* '()))
    (if (pair? chars)
	(loop (cdr chars)
	      (if (char-alphabetic? (car chars))
		  (cons* (char-upcase (car chars))
			 (char-downcase (car chars))
			 chars*)
		  chars*))
	(apply char-set chars*))))

(define (rexp-n*m n m . rexps)
  (guarantee exact-nonnegative-integer? n 'rexp-n*m)
  (guarantee exact-nonnegative-integer? m 'rexp-n*m)
  (if (not (<= n m))
      (error:bad-range-argument m 'rexp-n*m))
  (let ((rexp (apply rexp-sequence rexps)))
    (let loop ((i 1))
      (cond ((<= i n)
	     (rexp-sequence rexp (loop (+ i 1))))
	    ((<= i m)
	     (rexp-optional rexp (loop (+ i 1))))
	    (else
	     (rexp-sequence))))))

(define (rexp-n*n n . rexps)
  (apply rexp-n*m n n rexps))

(define (rexp-*n n . rexps)
  (apply rexp-n*m 0 n rexps))

(define (rexp-n* n . rexps)
  (guarantee exact-nonnegative-integer? n 'rexp-n*)
  (let ((rexp (apply rexp-sequence rexps)))
    (if (= n 0)
	(rexp* rexp)
	(let loop ((i 1))
	  (if (< i n)
	      (rexp-sequence rexp (loop (+ i 1)))
	      (rexp+ rexp))))))