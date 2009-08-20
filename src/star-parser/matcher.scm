#| -*-Scheme-*-

$Id$

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; Pattern-matcher language

;;; A matcher is a procedure of one argument, a parser buffer.
;;; It performs a match against the contents of the buffer, starting
;;; at the location of the buffer pointer.  If the match is
;;; successful, the buffer pointer is advanced to the end of the
;;; matched segment, and #T is returned.  If the match fails, the
;;; buffer pointer is unchanged, and #F is returned.

(declare (usual-integrations))

;;;; Preprocessor

(define (preprocess-matcher-expression expression
				       external-bindings
				       internal-bindings)
  (cond ((and (pair? expression)
	      (symbol? (car expression))
	      (list? (cdr expression)))
	 (let ((preprocessor (matcher-preprocessor (car expression))))
	   (if preprocessor
	       (preprocessor expression external-bindings internal-bindings)
	       (error "Unknown matcher expression:" expression))))
	((symbol? expression)
	 (let ((preprocessor (matcher-preprocessor expression)))
	   (if preprocessor
	       (preprocessor expression external-bindings internal-bindings)
	       expression)))
	((identifier? expression)
	 expression)
	((string? expression)
	 (preprocess-matcher-expression `(STRING ,expression)
					external-bindings
					internal-bindings))
	((char? expression)
	 (preprocess-matcher-expression `(CHAR ,expression)
					external-bindings
					internal-bindings))
	(else
	 (error "Unknown matcher expression:" expression))))

(define (preprocess-matcher-expressions expressions
					external-bindings
					internal-bindings)
  (map (lambda (expression)
	 (preprocess-matcher-expression expression
					external-bindings
					internal-bindings))
       expressions))

(define (define-matcher-preprocessor name procedure)
  (if (pair? name)
      (for-each (lambda (name) (define-matcher-preprocessor name procedure))
		name)
      (hash-table/put! matcher-preprocessors name procedure))
  name)

(define-syntax define-*matcher-macro
  (rsc-macro-transformer
   (lambda (form environment)
     (let ((r-dme (close-syntax 'DEFINE-*MATCHER-EXPANDER environment))
	   (r-lambda (close-syntax 'LAMBDA environment)))
       (cond ((syntax-match? '(SYMBOL EXPRESSION) (cdr form))
	      `(,r-dme ',(cadr form)
		       (,r-lambda ()
				  ,(caddr form))))
	     ((syntax-match? '((SYMBOL . MIT-BVL) + EXPRESSION) (cdr form))
	      `(,r-dme ',(car (cadr form))
		       (,r-lambda ,(cdr (cadr form))
				  ,@(cddr form))))
	     (else
	      (ill-formed-syntax form)))))))

(define (define-*matcher-expander name procedure)
  (define-matcher-macro name
    (lambda (expression external-bindings internal-bindings)
      (preprocess-matcher-expression (if (pair? expression)
					 (apply procedure (cdr expression))
					 (procedure))
				     external-bindings
				     internal-bindings))))

(define (matcher-preprocessor name)
  (or (lookup-matcher-macro name)
      (hash-table/get matcher-preprocessors name #f)))

(define matcher-preprocessors
  (make-eq-hash-table))

(define-*matcher-expander '+
  (lambda (expression)
    `(SEQ ,expression (* ,expression))))

(define-*matcher-expander '?
  (lambda (expression)
    `(ALT ,expression (SEQ))))

(define-*matcher-expander 'COMPLETE
  (lambda (expression)
    `(SEQ ,expression (END-OF-INPUT))))

(define-*matcher-expander 'TOP-LEVEL
  (lambda (expression)
    `(SEQ ,expression (DISCARD-MATCHED))))

(define-*matcher-expander 'N*
  (lambda (n expression)
    `(SEQ (N*N ,n ,expression)
	  (* ,expression))))

(define-*matcher-expander 'N*M
  (lambda (n m expression)
    `(SEQ (N*N ,n ,expression)
	  (*N ,(- m n) ,expression))))

(define-*matcher-expander 'ERROR
  (lambda (ptr msg . irritants)
    (let ((v (generate-uninterned-symbol)))
      `(SEXP (LAMBDA (,v)
	       ,@(if ptr (list v) '())
	       (PARSER-BUFFER-ERROR ,(or ptr v) ,msg ,@irritants))))))

(define-matcher-preprocessor '(ALT SEQ)
  (lambda (expression external-bindings internal-bindings)
    `(,(car expression)
      ,@(flatten-expressions (preprocess-matcher-expressions (cdr expression)
							     external-bindings
							     internal-bindings)
			     (car expression)))))

(define-matcher-preprocessor '*
  (lambda (expression external-bindings internal-bindings)
    `(,(car expression)
      ,(preprocess-matcher-expression (check-1-arg expression)
				      external-bindings
				      internal-bindings))))

(define-matcher-preprocessor '(N*N *N)
  (lambda (expression external-bindings internal-bindings)
    (check-n-args 2 expression
      (lambda (expression)
	(exact-nonnegative-integer? (cadr expression))))
    `(,(car expression) ,(cadr expression)
			,(preprocess-matcher-expression (caddr expression)
							external-bindings
							internal-bindings))))

(define-matcher-preprocessor '(CHAR CHAR-CI NOT-CHAR NOT-CHAR-CI ALPHABET)
  (lambda (expression external-bindings internal-bindings)
    external-bindings internal-bindings
    (check-1-arg expression)
    expression))

(define-matcher-preprocessor '(STRING STRING-CI)
  (lambda (expression external-bindings internal-bindings)
    external-bindings internal-bindings
    (let ((string (check-1-arg expression)))
      (if (and (string? string) (fix:= (string-length string) 1))
	  `(,(if (eq? (car expression) 'STRING) 'CHAR 'CHAR-CI)
	    ,(string-ref string 0))
	  expression))))

(define-matcher-preprocessor 'CHAR-SET
  (lambda (expression external-bindings internal-bindings)
    internal-bindings
    (let ((arg (check-1-arg expression)))
      (if (string? arg)
	  `(,(car expression)
	    ,(handle-complex-expression
	      (if (string-prefix? "^" arg)
		  `(,(close 'RE-COMPILE-CHAR-SET) ,(string-tail arg 1) #T)
		  `(,(close 'RE-COMPILE-CHAR-SET) ,arg #F))
	      external-bindings))
	  expression))))

(define-matcher-preprocessor '(END-OF-INPUT DISCARD-MATCHED)
  (lambda (expression external-bindings internal-bindings)
    external-bindings internal-bindings
    (check-0-args expression)
    expression))

(define-matcher-preprocessor 'WITH-POINTER
  (lambda (expression external-bindings internal-bindings)
    (check-2-args expression
		  (lambda (expression) (identifier? (cadr expression))))
    `(,(car expression) ,(cadr expression)
			,(preprocess-matcher-expression (caddr expression)
							external-bindings
							internal-bindings))))

(define-matcher-preprocessor 'SEXP
  (lambda (expression external-bindings internal-bindings)
    external-bindings internal-bindings
    (check-1-arg expression)
    expression))

;;;; Compiler

(define-syntax *matcher
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(EXPRESSION) (cdr form))
	 (generate-matcher-code (cadr form) environment)
	 (ill-formed-syntax form)))))

(define (generate-matcher-code expression environment)
  (generate-external-procedure expression environment
			       preprocess-matcher-expression
    (lambda (expression free-names)
      (call-with-pointer #f
	(lambda (p)
	  (bind-delayed-lambdas
	   (lambda (ks kf)
	     (compile-matcher-expression expression #f ks kf free-names))
	   (make-matcher-ks-lambda (lambda (kf) kf `#T))
	   (backtracking-kf p (lambda () `#F))))))))

(define (compile-matcher-expression expression pointer ks kf free-names)
  (cond ((and (pair? expression)
	      (symbol? (car expression))
	      (list? (cdr expression))
	      (hash-table/get matcher-compilers (car expression) #f))
	 => (lambda (entry)
	      (let ((arity (car entry))
		    (compiler (cdr entry)))
		(if (and arity (not (= (length (cdr expression)) arity)))
		    (error "Incorrect arity for matcher:" expression))
		(apply compiler pointer ks kf free-names (cdr expression)))))
	((or (identifier? expression)
	     (and (pair? expression) (eq? (car expression) 'SEXP)))
	 (wrap-external-matcher `(,(protect (if (pair? expression)
						(cadr expression)
						expression)
					    free-names)
				  ,*buffer-name*)
				ks
				kf))
	(else
	 (error "Malformed matcher:" expression))))

(define (wrap-external-matcher matcher ks kf)
  `(IF ,matcher
       ,(delay-call ks kf)
       ,(delay-call kf)))

(define-syntax define-matcher
  (rsc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '((SYMBOL . MIT-BVL) + EXPRESSION) (cdr form))
	 (let ((name (car (cadr form)))
	       (parameters (cdr (cadr form)))
	       (compiler-body (cddr form))
	       (r-dmc (close-syntax 'DEFINE-MATCHER-COMPILER environment))
	       (r-lambda (close-syntax 'LAMBDA environment)))
	   `(,r-dmc ',name
		    ,(if (identifier? parameters) `#F (length parameters))
		    (,r-lambda (POINTER KS KF FREE-NAMES . ,parameters)
			       ,@compiler-body)))
	 (ill-formed-syntax form)))))

(define (define-matcher-compiler keyword arity compiler)
  (hash-table/put! matcher-compilers keyword (cons arity compiler))
  keyword)

(define matcher-compilers
  (make-eq-hash-table))

(define-syntax define-atomic-matcher
  (rsc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(DATUM + EXPRESSION) (cdr form))
	 (let ((r-dm (close-syntax 'DEFINE-MATCHER environment))
	       (r-wem (close-syntax 'WRAP-EXTERNAL-MATCHER environment)))
	   `(,r-dm ,(cadr form)
		   POINTER ,@(except-last-pair (cddr form))
		   (,r-wem ,(car (last-pair (cddr form))) KS KF)))
	 (ill-formed-syntax form)))))

(define-atomic-matcher (char char)
  `(MATCH-PARSER-BUFFER-CHAR ,*buffer-name* ,(protect char free-names)))

(define-atomic-matcher (char-ci char)
  `(MATCH-PARSER-BUFFER-CHAR-CI ,*buffer-name* ,(protect char free-names)))

(define-atomic-matcher (not-char char)
  `(MATCH-PARSER-BUFFER-NOT-CHAR ,*buffer-name* ,(protect char free-names)))

(define-atomic-matcher (not-char-ci char)
  `(MATCH-PARSER-BUFFER-NOT-CHAR-CI ,*buffer-name* ,(protect char free-names)))

(define-atomic-matcher (char-set char-set)
  `(MATCH-PARSER-BUFFER-CHAR-IN-SET ,*buffer-name*
				    ,(protect char-set free-names)))

(define-atomic-matcher (alphabet alphabet)
  `(MATCH-PARSER-BUFFER-CHAR-IN-ALPHABET ,*buffer-name*
					 ,(protect alphabet free-names)))

(define-atomic-matcher (string string)
  `(MATCH-PARSER-BUFFER-STRING ,*buffer-name* ,(protect string free-names)))

(define-atomic-matcher (string-ci string)
  `(MATCH-PARSER-BUFFER-STRING-CI ,*buffer-name* ,(protect string free-names)))

(define-atomic-matcher (end-of-input)
  free-names
  `(NOT (PEEK-PARSER-BUFFER-CHAR ,*buffer-name*)))

(define-matcher (discard-matched)
  pointer free-names
  `(BEGIN
     (DISCARD-PARSER-BUFFER-HEAD! ,*buffer-name*)
     ,(delay-call ks kf)))

(define-matcher (with-pointer identifier expression)
  `((LAMBDA (,identifier)
      ,(compile-matcher-expression expression (or pointer identifier) ks kf
				   (cons identifier free-names)))
    ,(or pointer (fetch-pointer))))

(define-matcher (seq . expressions)
  (if (pair? expressions)
      (let loop ((expressions expressions) (pointer pointer) (kf kf))
	(if (pair? (cdr expressions))
	    (bind-delayed-lambdas
	     (lambda (ks)
	       (compile-matcher-expression (car expressions) pointer ks kf
					   free-names))
	     (make-matcher-ks-lambda
	      (lambda (kf)
		(loop (cdr expressions) #f kf))))
	    (compile-matcher-expression (car expressions) pointer ks kf
					free-names)))
      (delay-call ks kf)))

(define-matcher (alt . expressions)
  (if (pair? expressions)
      (let loop ((expressions expressions) (pointer pointer))
	(if (pair? (cdr expressions))
	    (call-with-pointer pointer
	      (lambda (pointer)
		(bind-delayed-lambdas
		 (lambda (kf)
		   (compile-matcher-expression (car expressions) pointer ks kf
					       free-names))
		 (backtracking-kf pointer
		   (lambda ()
		     (loop (cdr expressions) pointer))))))
	    (compile-matcher-expression (car expressions) pointer ks kf
					free-names)))
      (delay-call kf)))

(define-matcher (* expression)
  pointer
  (let ((ks2 (make-ks-identifier))
	(kf2 (make-kf-identifier)))
    `(LET ,ks2 ((,kf2 ,(delay-reference kf)))
       ,(call-with-pointer #f
	  (lambda (pointer)
	    (bind-delayed-lambdas
	     (lambda (kf)
	       (compile-matcher-expression expression #f ks2 kf free-names))
	     (backtracking-kf pointer
	       (lambda ()
		 (delay-call ks kf2)))))))))

(define-matcher (n*n n expression)
  (if (<= n 4)
      (open-code-n*n n expression pointer ks kf free-names)
      (close-code-n*n n expression pointer ks kf free-names)))

(define (open-code-n*n n expression pointer ks kf free-names)
  (let loop ((n n) (pointer pointer) (kf kf))
    (if (> n 0)
	(bind-delayed-lambdas
	 (lambda (ks)
	   (compile-matcher-expression expression pointer ks kf
				       free-names))
	 (make-matcher-ks-lambda
	  (lambda (kf)
	    (loop (- n 1) #f kf))))
	(delay-call ks kf))))

(define (close-code-n*n n expression pointer ks kf free-names)
  ;; Assume (>= N 1).
  pointer
  (let ((l1 (make-loop-identifier))
	(n1 (make-value-identifier))
	(kf2 (make-kf-identifier)))
    `(LET ,l1 ((,n1 ,n) (,kf2 ,(delay-reference kf)))
       (IF (> ,n1 1)
	   ,(bind-delayed-lambdas
	     (lambda (ks)
	       (compile-matcher-expression expression #f ks kf2 free-names))
	     (make-matcher-ks-lambda
	      (lambda (kf)
		`(,l1 (- ,n1 1) ,kf))))
	   ,(compile-matcher-expression expression #f ks kf2 free-names)))))

(define-matcher (*n n expression)
  (if (<= n 4)
      (open-code-*n n expression pointer ks kf free-names)
      (close-code-*n n expression pointer ks kf free-names)))

(define (open-code-*n n expression pointer ks kf free-names)
  (bind-delayed-lambdas
   (lambda (kf)
     (let loop ((n n) (pointer pointer) (kf kf))
       (if (> n 0)
	   (bind-delayed-lambdas
	    (lambda (ks)
	      (compile-matcher-expression expression pointer ks kf free-names))
	    (make-matcher-ks-lambda
	     (lambda (kf)
	       (loop (- n 1) #f kf))))
	   (delay-call ks kf))))
   (make-kf-lambda
    (lambda ()
      (delay-call ks kf)))))

(define (close-code-*n n expression pointer ks kf free-names)
  ;; Assume (>= N 1).
  pointer
  (bind-delayed-lambdas
   (lambda (kf)
     (let ((l1 (make-loop-identifier))
	   (n1 (make-value-identifier))
	   (kf2 (make-kf-identifier)))
       `(LET ,l1 ((,n1 ,n) (,kf2 ,(delay-reference kf)))
	  (IF (> ,n1 1)
	      ,(bind-delayed-lambdas
		(lambda (ks)
		  (compile-matcher-expression expression #f ks kf2 free-names))
		(make-matcher-ks-lambda
		 (lambda (kf)
		   `(,l1 (- ,n1 1) ,kf))))
	      ,(compile-matcher-expression expression #f ks kf2 free-names)))))
   (make-kf-lambda
    (lambda ()
      (delay-call ks kf)))))