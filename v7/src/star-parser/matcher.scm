;;; -*-Scheme-*-
;;;
;;; $Id: matcher.scm,v 1.12 2001/07/10 05:04:44 cph Exp $
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

(define (matcher-preprocessor name)
  (hash-table/get matcher-preprocessors name #f))

(define matcher-preprocessors
  (make-eq-hash-table))

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
  (define-matcher-preprocessor name
    (lambda (expression external-bindings internal-bindings)
      (preprocess-matcher-expression (if (pair? expression)
					 (apply procedure (cdr expression))
					 (procedure))
				     external-bindings
				     internal-bindings))))

(define-*matcher-expander '+
  (lambda (expression)
    `(SEQ ,expression (* ,expression))))

(define-*matcher-expander '?
  (lambda (expression)
    `(ALT ,expression (SEQ))))

(define-matcher-preprocessor '(ALT SEQ)
  (lambda (expression external-bindings internal-bindings)
    `(,(car expression)
      ,@(flatten-expressions (preprocess-matcher-expressions (cdr expression)
							     external-bindings
							     internal-bindings)
			     (car expression)))))

(define-matcher-preprocessor '(* COMPLETE)
  (lambda (expression external-bindings internal-bindings)
    `(,(car expression)
      ,(preprocess-matcher-expression (check-1-arg expression)
				      external-bindings
				      internal-bindings))))

(define-matcher-preprocessor '(CHAR CHAR-CI NOT-CHAR NOT-CHAR-CI)
  (lambda (expression external-bindings internal-bindings)
    external-bindings internal-bindings
    (check-1-arg expression)
    expression))

(define-matcher-preprocessor 'STRING
  (lambda (expression external-bindings internal-bindings)
    external-bindings internal-bindings
    (let ((string (check-1-arg expression)))
      (if (and (string? string) (fix:= (string-length string) 1))
	  `(CHAR ,(string-ref string 0))
	  expression))))

(define-matcher-preprocessor 'STRING-CI
  (lambda (expression external-bindings internal-bindings)
    external-bindings internal-bindings
    (let ((string (check-1-arg expression)))
      (if (and (string? string) (fix:= (string-length string) 1))
	  `(CHAR-CI ,(string-ref string 0))
	  expression))))

(define-matcher-preprocessor 'ALPHABET
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
  (let ((external-bindings (list 'BINDINGS))
	(internal-bindings (list 'BINDINGS)))
    (let ((expression
	   (preprocess-matcher-expression expression
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
		 (compile-isolated-matcher-expression expression
						      pointer))))))))))

(define (compile-isolated-matcher-expression expression pointer)
  (compile-matcher-expression expression pointer
    (simple-backtracking-continuation `#T)
    (simple-backtracking-continuation `#F)))

(define (compile-matcher-expression expression pointer if-succeed if-fail)
  (cond ((and (pair? expression)
	      (symbol? (car expression))
	      (list? (cdr expression))
	      (hash-table/get matcher-compilers (car expression) #f))
	 => (lambda (entry)
	      (let ((arity (car entry))
		    (compiler (cdr entry)))
		(if (and arity (not (= (length (cdr expression)) arity)))
		    (error "Incorrect arity for matcher:" expression))
		(apply compiler pointer if-succeed if-fail
		       (if arity
			   (cdr expression)
			   (list (cdr expression)))))))
	((or (symbol? expression)
	     (and (pair? expression) (eq? (car expression) 'SEXP)))
	 (handle-pending-backtracking pointer
	   (lambda (pointer)
	     `(IF (,(if (pair? expression) (cadr expression) expression)
		   ,*buffer-name*)
		  ,(call-with-unknown-pointer if-succeed)
		  ,(if-fail pointer)))))
	(else
	 (error "Malformed matcher:" expression))))

(define-macro (define-matcher form . compiler-body)
  (let ((name (car form))
	(parameters (cdr form)))
    (if (symbol? parameters)
	`(DEFINE-MATCHER-COMPILER ',name #F
	   (LAMBDA (POINTER IF-SUCCEED IF-FAIL ,parameters)
	     ,@compiler-body))
	`(DEFINE-MATCHER-COMPILER ',name ,(length parameters)
	   (LAMBDA (POINTER IF-SUCCEED IF-FAIL ,@parameters)
	     ,@compiler-body)))))

(define (define-matcher-compiler keyword arity compiler)
  (hash-table/put! matcher-compilers keyword (cons arity compiler))
  keyword)

(define matcher-compilers
  (make-eq-hash-table))

(define-macro (define-atomic-matcher form test-expression)
  `(DEFINE-MATCHER ,form
     (HANDLE-PENDING-BACKTRACKING POINTER
       (LAMBDA (POINTER)
	 `(IF ,,test-expression
	      ,(CALL-WITH-UNKNOWN-POINTER IF-SUCCEED)
	      ,(IF-FAIL POINTER))))))

(define-atomic-matcher (char char)
  `(MATCH-PARSER-BUFFER-CHAR ,*buffer-name* ,char))

(define-atomic-matcher (char-ci char)
  `(MATCH-PARSER-BUFFER-CHAR-CI ,*buffer-name* ,char))

(define-atomic-matcher (not-char char)
  `(MATCH-PARSER-BUFFER-NOT-CHAR ,*buffer-name* ,char))

(define-atomic-matcher (not-char-ci char)
  `(MATCH-PARSER-BUFFER-NOT-CHAR-CI ,*buffer-name* ,char))

(define-atomic-matcher (alphabet alphabet)
  `(MATCH-PARSER-BUFFER-CHAR-IN-SET ,*buffer-name* ,alphabet))

(define-atomic-matcher (string string)
  `(MATCH-PARSER-BUFFER-STRING ,*buffer-name* ,string))

(define-atomic-matcher (string-ci string)
  `(MATCH-PARSER-BUFFER-STRING-CI ,*buffer-name* ,string))

(define-matcher (with-pointer identifier expression)
  `(LET ((,identifier ,(pointer-reference pointer)))
     ,(compile-matcher-expression expression pointer if-succeed if-fail)))

(define-matcher (complete expression)
  (compile-matcher-expression expression pointer
    (lambda (pointer*)
      `(IF (PEEK-PARSER-BUFFER-CHAR ,*buffer-name*)
	   ,(if-fail (backtrack-to pointer pointer*))
	   (BEGIN
	     (DISCARD-PARSER-BUFFER-HEAD! ,*buffer-name*)
	     ,(if-succeed pointer*))))
    if-fail))

(define-matcher (* expression)
  if-fail
  (handle-pending-backtracking pointer
    (lambda (pointer)
      pointer
      (let ((v (generate-uninterned-symbol)))
	`(BEGIN
	   (LET ,v ()
	     ,(call-with-unknown-pointer
	       (lambda (pointer)
		 (compile-matcher-expression expression pointer
		   (simple-backtracking-continuation `(,v))
		   (simple-backtracking-continuation `UNSPECIFIC)))))
	   ,(call-with-unknown-pointer if-succeed))))))

(define-matcher (seq . expressions)
  (let loop ((expressions expressions) (pointer* pointer))
    (if (pair? expressions)
	(compile-matcher-expression (car expressions) pointer*
	  (lambda (pointer*)
	    (loop (cdr expressions) pointer*))
	  (lambda (pointer*)
	    (if-fail (backtrack-to pointer pointer*))))
	(if-succeed pointer*))))

(define-matcher (alt . expressions)
  (if (pair? expressions)
      (if (pair? (cdr expressions))
	  (handle-pending-backtracking pointer
	    (lambda (pointer)
	      `(IF (OR ,@(map (lambda (expression)
				(compile-isolated-matcher-expression expression
								     pointer))
			      expressions))
		   ,(call-with-unknown-pointer if-succeed)
		   ,(if-fail pointer))))
	  (compile-matcher-expression (car expressions) pointer
	    if-succeed
	    if-fail))
      (if-fail pointer)))

;;; Edwin Variables:
;;; Eval: (scheme-indent-method 'handle-pending-backtracking 1)
;;; Eval: (scheme-indent-method 'define-matcher-optimizer 2)
;;; Eval: (scheme-indent-method 'with-buffer-name 0)
;;; Eval: (scheme-indent-method 'compile-matcher-expression 2)
;;; End:
