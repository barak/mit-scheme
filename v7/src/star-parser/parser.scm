;;; -*-Scheme-*-
;;;
;;; $Id: parser.scm,v 1.13 2001/07/02 12:14:32 cph Exp $
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

(declare (usual-integrations))

;;; A parser is a procedure of one argument, a parser buffer.  It
;;; attempts to parse the contents of the buffer, starting at the
;;; location of the buffer pointer.  If the parse is successful, the
;;; buffer pointer is advanced to the end of the parsed segment, and a
;;; vector of results is returned.  If the parse fails, the buffer
;;; pointer is unchanged, and #F is returned.

;;; The *PARSER macro provides a concise way to define a broad class
;;; of parsers using a BNF-like syntax.

(syntax-table/define system-global-syntax-table '*PARSER
  (lambda (expression)
    (optimize-expression (generate-parser-code expression))))

(define (generate-parser-code expression)
  (with-canonical-parser-expression expression
    (lambda (expression)
      (call-with-unknown-pointer
       (lambda (pointer)
	 (compile-parser-expression expression pointer
	   simple-backtracking-succeed
	   (simple-backtracking-continuation `#F)))))))

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
	((symbol? expression)
	 (handle-pending-backtracking pointer
	   (lambda (pointer)
	     (with-variable-binding `(,expression ,*buffer-name*)
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

(syntax-table/define system-global-syntax-table 'DEFINE-*PARSER-MACRO
  (lambda (bvl expression)
    (cond ((symbol? bvl)
	   `(DEFINE-*PARSER-MACRO* ',bvl
	      (LAMBDA ()
		,expression)))
	  ((named-lambda-bvl? bvl)
	   `(DEFINE-*PARSER-MACRO* ',(car bvl)
	      (LAMBDA ,(cdr bvl)
		,expression)))
	  (else
	   (error "Malformed bound-variable list:" bvl)))))

(define (define-*parser-macro* name procedure)
  (hash-table/put! *parser-macros name procedure)
  name)

(define (*parser-expander name)
  (hash-table/get *parser-macros name #f))

(define *parser-macros
  (make-eq-hash-table))

;;;; Canonicalization

(define (with-canonical-parser-expression expression receiver)
  (let ((external-bindings (list 'BINDINGS))
	(internal-bindings (list 'BINDINGS)))
    (define (do-expression expression)
      (cond ((and (pair? expression)
		  (symbol? (car expression))
		  (list? (cdr expression)))
	     (case (car expression)
	       ((ALT SEQ)
		`(,(car expression)
		  ,@(flatten-expressions (map do-expression (cdr expression))
					 (car expression))))
	       ((* COMPLETE TOP-LEVEL)
		`(,(car expression)
		  ,(do-expression (check-1-arg expression))))
	       ((+)
		(do-expression
		 (let ((expression (check-1-arg expression)))
		   `(SEQ ,expression (* ,expression)))))
	       ((?)
		(do-expression
		 `(ALT ,(check-1-arg expression) (SEQ))))
	       ((MATCH NOISE)
		`(,(car expression)
		  ,(canonicalize-matcher-expression (check-1-arg expression)
						    external-bindings
						    internal-bindings)))
	       ((DEFAULT TRANSFORM ELEMENT-TRANSFORM ENCAPSULATE)
		(check-2-args expression)
		`(,(car expression) ,(cadr expression)
				    ,(do-expression (caddr expression))))
	       ((WITH-POINTER)
		(check-2-args expression
			      (lambda (expression)
				(symbol? (cadr expression))))
		`(,(car expression)
		  ,(cadr expression)
		  ,(do-expression (caddr expression))))
	       ((SEXP)
		(handle-complex-expression (check-1-arg expression)
					   internal-bindings))
	       (else
		(let ((expander (*parser-expander (car expression))))
		  (if expander
		      (do-expression (apply expander (cdr expression)))
		      (error "Unknown parser expression:" expression))))))
	    ((symbol? expression)
	     (let ((expander (*parser-expander expression)))
	       (if expander
		   (do-expression (expander))
		   expression)))
	    (else
	     (error "Unknown parser expression:" expression))))
    (let ((expression (do-expression expression)))
      (maybe-make-let (map (lambda (b) (list (cdr b) (car b)))
			   (cdr external-bindings))
	(with-buffer-name
	  (lambda ()
	    (maybe-make-let (map (lambda (b) (list (cdr b) (car b)))
				 (cdr internal-bindings))
	      (receiver expression))))))))

;;;; Parsers

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

(define-parser (default value parser)
  if-fail
  (compile-parser-expression parser pointer if-succeed
    (lambda (pointer)
      (if-succeed pointer `(VECTOR ,value)))))

(define-parser (transform transform parser)
  (compile-parser-expression parser pointer
    (lambda (pointer* result)
      (with-variable-binding `(,transform ,result)
	(lambda (result)
	  `(IF ,result
	       ,(if-succeed pointer* result)
	       ,(if-fail (backtrack-to pointer pointer*))))))
    if-fail))

(define-parser (element-transform transform parser)
  (compile-parser-expression parser pointer
    (lambda (pointer result)
      (if-succeed pointer `(VECTOR-MAP ,transform ,result)))
    if-fail))

(define-parser (encapsulate transform parser)
  (compile-parser-expression parser pointer
    (lambda (pointer result)
      (if-succeed pointer `(VECTOR (,transform ,result))))
    if-fail))

(define-parser (complete parser)
  (compile-parser-expression parser pointer
    (lambda (pointer* result)
      `(IF (PEEK-PARSER-BUFFER-CHAR ,*buffer-name*)
	   ,(if-fail (backtrack-to pointer pointer*))
	   (BEGIN
	     (DISCARD-PARSER-BUFFER-HEAD! ,*buffer-name*)
	     ,(if-succeed pointer* result))))
    if-fail))

(define-parser (top-level parser)
  (compile-parser-expression parser pointer
    (lambda (pointer result)
      `(BEGIN
	 (DISCARD-PARSER-BUFFER-HEAD! ,*buffer-name*)
	 ,(if-succeed pointer result)))
    if-fail))

(define-parser (with-pointer identifier expression)
  `(LET ((,identifier ,(pointer-reference pointer)))
     ,(compile-parser-expression expression pointer
	if-succeed if-fail)))

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
	       ,(if-fail pointer)))))))

(define-parser (* parser)
  if-fail
  (handle-pending-backtracking pointer
    (lambda (pointer)
      pointer
      (call-with-unknown-pointer
       (lambda (pointer)
	 (with-variable-binding
	     (let ((loop (generate-uninterned-symbol))
		   (elements (generate-uninterned-symbol)))
	       `(LET ,loop ((,elements (VECTOR)))
		  ,(compile-parser-expression parser pointer
		     (backtracking-succeed
		      (lambda (element)
			`(,loop (VECTOR-APPEND ,elements ,element))))
		     (simple-backtracking-continuation elements))))
	   (lambda (elements)
	     (if-succeed pointer elements))))))))

;;; Edwin Variables:
;;; Eval: (scheme-indent-method 'handle-pending-backtracking 1)
;;; Eval: (scheme-indent-method 'with-buffer-name 0)
;;; Eval: (scheme-indent-method 'compile-parser-expression 2)
;;; End:
