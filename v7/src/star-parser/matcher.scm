;;; -*-Scheme-*-
;;;
;;; $Id: matcher.scm,v 1.21 2001/11/09 21:38:47 cph Exp $
;;;
;;; Copyright (c) 2001 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

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

(syntax-table/define system-global-syntax-table 'DEFINE-*MATCHER-MACRO
  (lambda (bvl expression)
    (cond ((symbol? bvl)
	   `(DEFINE-*MATCHER-EXPANDER ',bvl
	      (LAMBDA ()
		,expression)))
	  ((named-lambda-bvl? bvl)
	   `(DEFINE-*MATCHER-EXPANDER ',(car bvl)
	      (LAMBDA ,(cdr bvl)
		,expression)))
	  (else
	   (error "Malformed bound-variable list:" bvl)))))

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
		  `(RE-COMPILE-CHAR-SET ,(string-tail arg 1) #T)
		  `(RE-COMPILE-CHAR-SET ,arg #F))
	      external-bindings))
	  expression))))

(define-matcher-preprocessor '(END-OF-INPUT DISCARD-MATCHED)
  (lambda (expression external-bindings internal-bindings)
    external-bindings internal-bindings
    (check-0-args expression)
    expression))

(define-matcher-preprocessor 'WITH-POINTER
  (lambda (expression external-bindings internal-bindings)
    (check-2-args expression (lambda (expression) (symbol? (cadr expression))))
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

(syntax-table/define system-global-syntax-table '*MATCHER
  (lambda (expression)
    (optimize-expression (generate-matcher-code expression))))

(define (generate-matcher-code expression)
  (generate-external-procedure expression preprocess-matcher-expression
    (lambda (expression)
      (bind-delayed-lambdas
       (lambda (ks kf) (compile-matcher-expression expression #f ks kf))
       (make-matcher-ks-lambda (lambda (kf) kf `#T))
       (make-kf-lambda (lambda () `#F))))))

(define (compile-matcher-expression expression pointer ks kf)
  (cond ((and (pair? expression)
	      (symbol? (car expression))
	      (list? (cdr expression))
	      (hash-table/get matcher-compilers (car expression) #f))
	 => (lambda (entry)
	      (let ((arity (car entry))
		    (compiler (cdr entry)))
		(if (and arity (not (= (length (cdr expression)) arity)))
		    (error "Incorrect arity for matcher:" expression))
		(apply compiler pointer ks kf (cdr expression)))))
	((or (symbol? expression)
	     (and (pair? expression) (eq? (car expression) 'SEXP)))
	 (wrap-external-matcher `((PROTECT ,(if (pair? expression)
						(cadr expression)
						expression))
				  ,*buffer-name*)
				ks
				kf))
	(else
	 (error "Malformed matcher:" expression))))

(define (wrap-external-matcher matcher ks kf)
  `(IF ,matcher
       ,(delay-call ks kf)
       ,(delay-call kf)))

(define-macro (define-matcher form . compiler-body)
  (let ((name (car form))
	(parameters (cdr form)))
    `(DEFINE-MATCHER-COMPILER ',name
       ,(if (symbol? parameters) `#F (length parameters))
       (LAMBDA (POINTER KS KF . ,parameters)
	 ,@compiler-body))))

(define (define-matcher-compiler keyword arity compiler)
  (hash-table/put! matcher-compilers keyword (cons arity compiler))
  keyword)

(define matcher-compilers
  (make-eq-hash-table))

(define-macro (define-atomic-matcher form test-expression)
  `(DEFINE-MATCHER ,form
     POINTER
     (WRAP-EXTERNAL-MATCHER ,test-expression KS KF)))

(define-atomic-matcher (char char)
  `(MATCH-PARSER-BUFFER-CHAR ,*buffer-name* (PROTECT ,char)))

(define-atomic-matcher (char-ci char)
  `(MATCH-PARSER-BUFFER-CHAR-CI ,*buffer-name* (PROTECT ,char)))

(define-atomic-matcher (not-char char)
  `(MATCH-PARSER-BUFFER-NOT-CHAR ,*buffer-name* (PROTECT ,char)))

(define-atomic-matcher (not-char-ci char)
  `(MATCH-PARSER-BUFFER-NOT-CHAR-CI ,*buffer-name* (PROTECT ,char)))

(define-atomic-matcher (char-set char-set)
  `(MATCH-PARSER-BUFFER-CHAR-IN-SET ,*buffer-name* (PROTECT ,char-set)))

(define-atomic-matcher (alphabet alphabet)
  `(MATCH-UTF8-CHAR-IN-ALPHABET ,*buffer-name* (PROTECT ,alphabet)))

(define-atomic-matcher (string string)
  `(MATCH-PARSER-BUFFER-STRING ,*buffer-name* (PROTECT ,string)))

(define-atomic-matcher (string-ci string)
  `(MATCH-PARSER-BUFFER-STRING-CI ,*buffer-name* (PROTECT ,string)))

(define-atomic-matcher (end-of-input)
  `(NOT (PEEK-PARSER-BUFFER-CHAR ,*buffer-name*)))

(define-matcher (discard-matched)
  pointer
  `(BEGIN
     (DISCARD-PARSER-BUFFER-HEAD! ,*buffer-name*)
     ,(delay-call ks kf)))

(define-matcher (with-pointer identifier expression)
  pointer
  `((LAMBDA (,identifier)
      ,(compile-matcher-expression expression identifier ks kf))
    ,(fetch-pointer)))

(define-matcher (seq . expressions)
  (if (pair? expressions)
      (if (pair? (cdr expressions))
	  (let loop ((expressions expressions) (pointer pointer) (kf kf))
	    (if (pair? (cdr expressions))
		(bind-delayed-lambdas
		 (lambda (ks)
		   (compile-matcher-expression (car expressions)
					       pointer
					       ks
					       kf))
		 (make-matcher-ks-lambda
		  (lambda (kf)
		    (loop (cdr expressions) #f kf))))
		(compile-matcher-expression (car expressions) pointer ks kf)))
	  (compile-matcher-expression (car expressions) pointer ks kf))
      (delay-call ks kf)))

(define-matcher (alt . expressions)
  (if (pair? expressions)
      (if (pair? (cdr expressions))
	  (let loop ((expressions expressions) (pointer pointer))
	    (if (pair? (cdr expressions))
		(call-with-pointer pointer
		  (lambda (pointer)
		    (bind-delayed-lambdas
		     (lambda (kf)
		       (compile-matcher-expression (car expressions)
						   pointer
						   ks
						   kf))
		     (backtracking-kf pointer
		       (lambda ()
			 (loop (cdr expressions) pointer))))))
		(compile-matcher-expression (car expressions) pointer ks kf)))
	  (compile-matcher-expression (car expressions) pointer ks kf))
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
	       (compile-matcher-expression expression #f ks2 kf))
	     (backtracking-kf pointer
	       (lambda ()
		 (delay-call ks kf2)))))))))