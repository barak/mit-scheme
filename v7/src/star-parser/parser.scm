;;; -*-Scheme-*-
;;;
;;; $Id: parser.scm,v 1.10 2001/06/30 03:23:41 cph Exp $
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

(syntax-table/define system-global-syntax-table 'DEFINE-*PARSER-MACRO
  (lambda (bvl expression)
    (if (not (named-lambda-bvl? bvl))
	(error "Malformed bound-variable list:" bvl))
    `(DEFINE-*PARSER-MACRO* ',(car bvl)
       (LAMBDA ,(cdr bvl)
	 ,expression))))

(define (generate-parser-code expression)
  (with-canonical-parser-expression expression
    (lambda (expression)
      (compile-parser-expression
       expression
       (no-pointers)
       (lambda (pointers result)
	 (handle-pending-backtracking pointers
	   (lambda (pointers)
	     pointers
	     result)))
       (simple-backtracking-continuation `#F)))))

(define (compile-parser-expression expression pointers if-succeed if-fail)
  (cond ((and (pair? expression)
	      (symbol? (car expression))
	      (list? (cdr expression))
	      (hash-table/get parser-compilers (car expression) #f))
	 => (lambda (entry)
	      (let ((arity (car entry))
		    (compiler (cdr entry)))
		(if (and arity (not (= (length (cdr expression)) arity)))
		    (error "Incorrect arity for parser:" expression))
		(apply compiler pointers if-succeed if-fail
		       (if arity
			   (cdr expression)
			   (list (cdr expression)))))))
	((symbol? expression)
	 (handle-pending-backtracking pointers
	   (lambda (pointers)
	     (with-variable-binding `(,expression ,*buffer-name*)
	       (lambda (result)
		 `(IF ,result
		      ,(if-succeed (unknown-location pointers) result)
		      ,(if-fail pointers)))))))
	(else
	 (error "Malformed matcher:" expression))))

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
		(let ((expander
		       (hash-table/get *parser-macros (car expression) #f)))
		  (if expander
		      (do-expression (apply expander (cdr expression)))
		      (error "Unknown parser expression:" expression))))))
	    ((symbol? expression)
	     expression)
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

(define (define-*parser-macro* name procedure)
  (hash-table/put! *parser-macros name procedure)
  name)

(define *parser-macros
  (make-eq-hash-table))

;;;; Parsers

(define-macro (define-parser form . compiler-body)
  (let ((name (car form))
	(parameters (cdr form)))
    (if (symbol? parameters)
	`(DEFINE-PARSER-COMPILER ',name #F
	   (LAMBDA (POINTERS IF-SUCCEED IF-FAIL ,parameters)
	     ,@compiler-body))
	`(DEFINE-PARSER-COMPILER ',name ,(length parameters)
	   (LAMBDA (POINTERS IF-SUCCEED IF-FAIL ,@parameters)
	     ,@compiler-body)))))

(define (define-parser-compiler keyword arity compiler)
  (hash-table/put! parser-compilers keyword (cons arity compiler))
  keyword)

(define parser-compilers
  (make-eq-hash-table))

(define-parser (match matcher)
  (with-current-pointer pointers
    (lambda (start-pointers)
      (compile-matcher-expression matcher start-pointers
	(lambda (pointers)
	  (with-variable-binding
	      `(VECTOR (GET-PARSER-BUFFER-TAIL
			,*buffer-name*
			,(current-pointer start-pointers)))
	    (lambda (v)
	      (if-succeed pointers v))))
	if-fail))))

(define-parser (noise matcher)
  (compile-matcher-expression matcher pointers
    (lambda (pointers) (if-succeed pointers `(VECTOR)))
    if-fail))

(define-parser (default value parser)
  if-fail
  (compile-parser-expression parser pointers if-succeed
    (lambda (pointers)
      (if-succeed pointers `(VECTOR ,value)))))

(define-parser (transform transform parser)
  (with-current-pointer pointers
    (lambda (start-pointers)
      (compile-parser-expression parser start-pointers
	(lambda (pointers result)
	  (with-variable-binding `(,transform ,result)
	    (lambda (result)
	      `(IF ,result
		   ,(if-succeed pointers result)
		   ,(if-fail
		     (new-backtrack-pointer start-pointers pointers))))))
	if-fail))))

(define-parser (element-transform transform parser)
  (compile-parser-expression parser pointers
    (lambda (pointers result)
      (if-succeed pointers `(VECTOR-MAP ,transform ,result)))
    if-fail))

(define-parser (encapsulate transform parser)
  (compile-parser-expression parser pointers
    (lambda (pointers result)
      (if-succeed pointers `(VECTOR (,transform ,result))))
    if-fail))

(define-parser (complete parser)
  (with-current-pointer pointers
    (lambda (start-pointers)
      (compile-parser-expression parser start-pointers
	(lambda (pointers result)
	  `(IF (PEEK-PARSER-BUFFER-CHAR ,*buffer-name*)
	       ,(if-fail (new-backtrack-pointer start-pointers pointers))
	       (BEGIN
		 (DISCARD-PARSER-BUFFER-HEAD! ,*buffer-name*)
		 ,(if-succeed pointers result))))
	if-fail))))

(define-parser (top-level parser)
  (compile-parser-expression parser pointers
    (lambda (pointers result)
      `(BEGIN
	 (DISCARD-PARSER-BUFFER-HEAD! ,*buffer-name*)
	 ,(if-succeed pointers result)))
    if-fail))

(define-parser (with-pointer identifier expression)
  (with-current-pointer pointers
    (lambda (pointers)
      `(LET ((,identifier ,(current-pointer pointers)))
	 ,(compile-parser-expression expression pointers
				     if-succeed if-fail)))))

(define-parser (seq . ps)
  (if (pair? ps)
      (if (pair? (cdr ps))
	  (with-current-pointer pointers
	    (lambda (start-pointers)
	      (let loop ((ps ps) (pointers start-pointers) (results '()))
		(compile-parser-expression (car ps) pointers
		  (lambda (pointers result)
		    (let ((results (cons result results)))
		      (if (pair? (cdr ps))
			  (loop (cdr ps) pointers results)
			  (if-succeed pointers
				      `(VECTOR-APPEND ,@(reverse results))))))
		  (lambda (pointers)
		    (if-fail
		     (new-backtrack-pointer start-pointers pointers)))))))
	  (compile-parser-expression (car ps) pointers if-succeed if-fail))
      (if-succeed pointers `(VECTOR))))

(define-parser (alt . ps)
  (with-current-pointer pointers
    (lambda (pointers)
      (with-variable-binding
	  `(OR ,@(map (lambda (p)
			(compile-parser-expression p pointers
			  (lambda (pointers result)
			    (handle-pending-backtracking pointers
			      (lambda (pointers)
				pointers
				result)))
			  (simple-backtracking-continuation `#F)))
		      ps))
	(lambda (result)
	  `(IF ,result
	       ,(if-succeed (unknown-location pointers) result)
	       ,(if-fail pointers)))))))

(define-parser (* parser)
  if-fail
  (handle-pending-backtracking pointers
    (lambda (pointers)
      (with-variable-binding
	  (let ((loop (generate-uninterned-symbol))
		(elements (generate-uninterned-symbol)))
	    `(LET ,loop ((,elements (VECTOR)))
	       ,(compile-parser-expression parser (no-pointers)
		  (lambda (pointers element)
		    (handle-pending-backtracking pointers
		      (lambda (pointers)
			pointers
			`(,loop (VECTOR-APPEND ,elements ,element)))))
		  (lambda (pointers)
		    (handle-pending-backtracking pointers
		      (lambda (pointers)
			pointers
			elements))))))
	(lambda (elements)
	  (if-succeed (unknown-location pointers) elements))))))

;;; Edwin Variables:
;;; Eval: (scheme-indent-method 'handle-pending-backtracking 1)
;;; Eval: (scheme-indent-method 'with-buffer-name 0)
;;; End:
