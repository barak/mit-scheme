;;; -*-Scheme-*-
;;;
;;; $Id: parser.scm,v 1.17 2001/07/14 11:42:31 cph Exp $
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

;;;; Parser language

;;; A parser is a procedure of one argument, a parser buffer.  It
;;; attempts to parse the contents of the buffer, starting at the
;;; location of the buffer pointer.  If the parse is successful, the
;;; buffer pointer is advanced to the end of the parsed segment, and a
;;; vector of results is returned.  If the parse fails, the buffer
;;; pointer is unchanged, and #F is returned.

(declare (usual-integrations))

;;;; Preprocessor

(define (preprocess-parser-expression expression
				      external-bindings
				      internal-bindings)
  (cond ((and (pair? expression)
	      (symbol? (car expression))
	      (list? (cdr expression)))
	 (let ((preprocessor (parser-preprocessor (car expression))))
	   (if preprocessor
	       (preprocessor expression external-bindings internal-bindings)
	       (error "Unknown parser expression:" expression))))
	((symbol? expression)
	 (let ((preprocessor (parser-preprocessor expression)))
	   (if preprocessor
	       (preprocessor expression external-bindings internal-bindings)
	       expression)))
	(else
	 (error "Unknown parser expression:" expression))))

(define (preprocess-parser-expressions expressions
				       external-bindings
				       internal-bindings)
  (map (lambda (expression)
	 (preprocess-parser-expression expression
				       external-bindings
				       internal-bindings))
       expressions))

(define (define-parser-preprocessor name procedure)
  (if (pair? name)
      (for-each (lambda (name) (define-parser-preprocessor name procedure))
		name)
      (hash-table/put! parser-preprocessors name procedure))
  name)

(syntax-table/define system-global-syntax-table 'DEFINE-*PARSER-MACRO
  (lambda (bvl expression)
    (cond ((symbol? bvl)
	   `(DEFINE-*PARSER-EXPANDER ',bvl
	      (LAMBDA ()
		,expression)))
	  ((named-lambda-bvl? bvl)
	   `(DEFINE-*PARSER-EXPANDER ',(car bvl)
	      (LAMBDA ,(cdr bvl)
		,expression)))
	  (else
	   (error "Malformed bound-variable list:" bvl)))))

(define (define-*parser-expander name procedure)
  (define-parser-macro name
    (lambda (expression external-bindings internal-bindings)
      (preprocess-parser-expression (if (pair? expression)
					(apply procedure (cdr expression))
					(procedure))
				    external-bindings
				    internal-bindings))))

(define (parser-preprocessor name)
  (or (lookup-parser-macro name)
      (hash-table/get parser-preprocessors name #f)))

(define parser-preprocessors
  (make-eq-hash-table))

(define-*parser-expander '+
  (lambda (expression)
    `(SEQ ,expression (* ,expression))))

(define-*parser-expander '?
  (lambda (expression)
    `(ALT ,expression (SEQ))))

(define-parser-preprocessor '(ALT SEQ)
  (lambda (expression external-bindings internal-bindings)
    `(,(car expression)
      ,@(flatten-expressions (preprocess-parser-expressions (cdr expression)
							    external-bindings
							    internal-bindings)
			     (car expression)))))

(define-parser-preprocessor '(* COMPLETE TOP-LEVEL)
  (lambda (expression external-bindings internal-bindings)
    `(,(car expression)
      ,(preprocess-parser-expression (check-1-arg expression)
				     external-bindings
				     internal-bindings))))

(define-parser-preprocessor '(MATCH NOISE)
  (lambda (expression external-bindings internal-bindings)
    `(,(car expression)
      ,(preprocess-matcher-expression (check-1-arg expression)
				      external-bindings
				      internal-bindings))))

(define-parser-preprocessor '(TRANSFORM MAP ENCAPSULATE)
  (lambda (expression external-bindings internal-bindings)
    (check-2-args expression)
    `(,(car expression) ,(cadr expression)
			,(preprocess-parser-expression (caddr expression)
						       external-bindings
						       internal-bindings))))

(define-parser-preprocessor 'WITH-POINTER
  (lambda (expression external-bindings internal-bindings)
    (check-2-args expression (lambda (expression) (symbol? (cadr expression))))
    `(,(car expression) ,(cadr expression)
			,(preprocess-parser-expression (caddr expression)
						       external-bindings
						       internal-bindings))))

(define-parser-preprocessor 'SEXP
  (lambda (expression external-bindings internal-bindings)
    external-bindings internal-bindings
    (check-1-arg expression)
    expression))

(define-parser-preprocessor 'VALUES
  (lambda (expression external-bindings internal-bindings)
    external-bindings internal-bindings
    expression))

;;;; Compiler

(syntax-table/define system-global-syntax-table '*PARSER
  (lambda (expression)
    (optimize-expression (generate-parser-code expression))))

(define (generate-parser-code expression)
  (let ((external-bindings (list 'BINDINGS))
	(internal-bindings (list 'BINDINGS)))
    (let ((expression
	   (preprocess-parser-expression expression
					 external-bindings
					 internal-bindings)))
      (maybe-make-let (map (lambda (b) (list (cdr b) (car b)))
			   (cdr external-bindings))
	(with-buffer-name
	  (lambda ()
	    (maybe-make-let (map (lambda (b) (list (cdr b) (car b)))
				 (cdr internal-bindings))
	      (call-with-unknown-pointer
	       (lambda (pointer)
		 (compile-parser-expression expression pointer
		   simple-backtracking-succeed
		   (simple-backtracking-continuation `#F)))))))))))

(define (compile-parser-expression expression pointer if-succeed if-fail)
  (cond ((and (pair? expression)
	      (symbol? (car expression))
	      (list? (cdr expression))
	      (hash-table/get parser-compilers (car expression) #f))
	 => (lambda (entry)
	      (let ((arity (car entry))
		    (compiler (cdr entry)))
		(if (and arity (not (= (length (cdr expression)) arity)))
		    (error "Incorrect arity for parser:" expression))
		(apply compiler pointer if-succeed if-fail
		       (if arity
			   (cdr expression)
			   (list (cdr expression)))))))
	((or (symbol? expression)
	     (and (pair? expression) (eq? (car expression) 'SEXP)))
	 (handle-pending-backtracking pointer
	   (lambda (pointer)
	     (with-variable-binding
		 `(,(if (pair? expression) (cadr expression) expression)
		   ,*buffer-name*)
	       (lambda (result)
		 `(IF ,result
		      ,(call-with-unknown-pointer
			(lambda (pointer)
			  (if-succeed pointer result)))
		      ,(if-fail pointer)))))))
	(else
	 (error "Malformed matcher:" expression))))

(define (backtracking-succeed handler)
  (lambda (pointer result)
    (handle-pending-backtracking pointer
      (lambda (pointer)
	pointer
	(handler result)))))

(define simple-backtracking-succeed
  (backtracking-succeed (lambda (result) result)))

(define-macro (define-parser form . compiler-body)
  (let ((name (car form))
	(parameters (cdr form)))
    (if (symbol? parameters)
	`(DEFINE-PARSER-COMPILER ',name #F
	   (LAMBDA (POINTER IF-SUCCEED IF-FAIL ,parameters)
	     ,@compiler-body))
	`(DEFINE-PARSER-COMPILER ',name ,(length parameters)
	   (LAMBDA (POINTER IF-SUCCEED IF-FAIL ,@parameters)
	     ,@compiler-body)))))

(define (define-parser-compiler keyword arity compiler)
  (hash-table/put! parser-compilers keyword (cons arity compiler))
  keyword)

(define parser-compilers
  (make-eq-hash-table))

(define-parser (match matcher)
  (compile-matcher-expression matcher pointer
    (lambda (pointer*)
      (with-variable-binding
	  `(VECTOR
	    (GET-PARSER-BUFFER-TAIL ,*buffer-name*
				    ,(pointer-reference pointer)))
	(lambda (v)
	  (if-succeed pointer* v))))
    if-fail))

(define-parser (noise matcher)
  (compile-matcher-expression matcher pointer
    (lambda (pointer) (if-succeed pointer `(VECTOR)))
    if-fail))

(define-parser (values . expressions)
  if-fail
  (if-succeed pointer `(VECTOR ,@expressions)))

(define-parser (transform transform expression)
  (compile-parser-expression expression pointer
    (lambda (pointer* result)
      (with-variable-binding `(,transform ,result)
	(lambda (result)
	  `(IF ,result
	       ,(if-succeed pointer* result)
	       ,(if-fail (backtrack-to pointer pointer*))))))
    if-fail))

(define-parser (map transform expression)
  (compile-parser-expression expression pointer
    (lambda (pointer result)
      (if-succeed pointer `(VECTOR-MAP ,transform ,result)))
    if-fail))

(define-parser (encapsulate transform expression)
  (compile-parser-expression expression pointer
    (lambda (pointer result)
      (if-succeed pointer `(VECTOR (,transform ,result))))
    if-fail))

(define-parser (complete expression)
  (compile-parser-expression expression pointer
    (lambda (pointer* result)
      `(IF (PEEK-PARSER-BUFFER-CHAR ,*buffer-name*)
	   ,(if-fail (backtrack-to pointer pointer*))
	   (BEGIN
	     (DISCARD-PARSER-BUFFER-HEAD! ,*buffer-name*)
	     ,(if-succeed pointer* result))))
    if-fail))

(define-parser (top-level expression)
  (compile-parser-expression expression pointer
    (lambda (pointer result)
      `(BEGIN
	 (DISCARD-PARSER-BUFFER-HEAD! ,*buffer-name*)
	 ,(if-succeed pointer result)))
    if-fail))

(define-parser (with-pointer identifier expression)
  `(LET ((,identifier ,(pointer-reference pointer)))
     ,(compile-parser-expression expression pointer
	if-succeed if-fail)))

(define-parser (* expression)
  if-fail
  (handle-pending-backtracking pointer
    (lambda (pointer)
      pointer
      (with-variable-binding
	  (let ((loop (generate-uninterned-symbol))
		(elements (generate-uninterned-symbol)))
	    `(LET ,loop ((,elements (VECTOR)))
	       ,(call-with-unknown-pointer
		 (lambda (pointer)
		   (compile-parser-expression expression pointer
		     (backtracking-succeed
		      (lambda (element)
			`(,loop (VECTOR-APPEND ,elements ,element))))
		     (simple-backtracking-continuation elements))))))
	(lambda (elements)
	  (call-with-unknown-pointer
	   (lambda (pointer)
	     (if-succeed pointer elements))))))))

(define-parser (seq . expressions)
  (if (pair? expressions)
      (if (pair? (cdr expressions))
	  (let loop
	      ((expressions expressions)
	       (pointer* pointer)
	       (results '()))
	    (compile-parser-expression (car expressions) pointer*
	      (lambda (pointer* result)
		(let ((results (cons result results)))
		  (if (pair? (cdr expressions))
		      (loop (cdr expressions) pointer* results)
		      (if-succeed pointer*
				  `(VECTOR-APPEND ,@(reverse results))))))
	      (lambda (pointer*)
		(if-fail (backtrack-to pointer pointer*)))))
	  (compile-parser-expression (car expressions) pointer
	    if-succeed
	    if-fail))
      (if-succeed pointer `(VECTOR))))

(define-parser (alt . expressions)
  (if (pair? expressions)
      (if (pair? (cdr expressions))
	  (handle-pending-backtracking pointer
	    (lambda (pointer)
	      (with-variable-binding
		  `(OR ,@(map (lambda (expression)
				(compile-parser-expression expression pointer
				  simple-backtracking-succeed
				  (simple-backtracking-continuation `#F)))
			      expressions))
		(lambda (result)
		  `(IF ,result
		       ,(call-with-unknown-pointer
			 (lambda (pointer)
			   (if-succeed pointer result)))
		       ,(if-fail pointer))))))
	  (compile-parser-expression (car expressions) pointer
	    if-succeed
	    if-fail))
      (if-fail pointer)))

;;; Edwin Variables:
;;; Eval: (scheme-indent-method 'handle-pending-backtracking 1)
;;; Eval: (scheme-indent-method 'with-buffer-name 0)
;;; Eval: (scheme-indent-method 'compile-parser-expression 2)
;;; End:
