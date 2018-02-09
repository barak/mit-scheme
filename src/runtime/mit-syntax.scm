#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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

;;;; MIT/GNU Scheme Syntax
;;; package: (runtime syntax mit)

(declare (usual-integrations))

;;;; Macro transformers

(define (transformer-keyword procedure-name transformer->expander)
  (lambda (form senv)
    (syntax-check '(KEYWORD EXPRESSION) form)
    (let ((transformer (compile-expr (cadr form) senv)))
      (transformer->expander (transformer-eval transformer senv)
			     senv
			     (expr-item
			      (lambda ()
				(output/top-level-syntax-expander
				 procedure-name transformer)))))))

(define classifier:sc-macro-transformer
  ;; "Syntactic Closures" transformer
  (transformer-keyword 'sc-macro-transformer->expander
		       sc-macro-transformer->expander))

(define classifier:rsc-macro-transformer
  ;; "Reversed Syntactic Closures" transformer
  (transformer-keyword 'rsc-macro-transformer->expander
		       rsc-macro-transformer->expander))

(define classifier:er-macro-transformer
  ;; "Explicit Renaming" transformer
  (transformer-keyword 'er-macro-transformer->expander
		       er-macro-transformer->expander))

;;;; Core primitives

(define (compiler:lambda form environment)
  (syntax-check '(KEYWORD MIT-BVL + FORM) form)
  (receive (bvl body)
      (compile/lambda (cadr form) (cddr form) environment)
    (output/lambda bvl body)))

(define (compiler:named-lambda form environment)
  (syntax-check '(KEYWORD (IDENTIFIER . MIT-BVL) + FORM) form)
  (receive (bvl body)
      (compile/lambda (cdadr form) (cddr form) environment)
    (output/named-lambda (identifier->symbol (caadr form)) bvl body)))

(define (compile/lambda bvl body environment)
  (let ((environment (make-internal-senv environment)))
    ;; Force order -- bind names before classifying body.
    (let ((bvl
	   (map-mit-lambda-list (lambda (identifier)
				  (bind-variable identifier environment))
				bvl)))
      (values bvl
	      (compile-body-item
	       (classify-body body environment))))))

(define (compile-body-item item)
  (output/body (compile-body-items (item->list item))))

(define (classifier:begin form environment)
  (syntax-check '(KEYWORD * FORM) form)
  (classify-body (cdr form) environment))

(define (compiler:if form environment)
  (syntax-check '(KEYWORD EXPRESSION EXPRESSION ? EXPRESSION) form)
  (output/conditional
   (compile-expr (cadr form) environment)
   (compile-expr (caddr form) environment)
   (if (pair? (cdddr form))
       (compile-expr (cadddr form) environment)
       (output/unspecific))))

(define (compiler:quote form environment)
  (declare (ignore environment))
  (syntax-check '(keyword datum) form)
  (output/constant (strip-syntactic-closures (cadr form))))

(define (compiler:quote-identifier form environment)
  (syntax-check '(keyword identifier) form)
  (let ((item (lookup-identifier (cadr form) environment)))
    (if (not (var-item? item))
	(syntax-error "Can't quote a keyword identifier:" form))
    (output/quoted-identifier (var-item-id item))))

(define (compiler:set! form environment)
  (syntax-check '(KEYWORD FORM ? EXPRESSION) form)
  (receive (name environment-item)
      (classify/location (cadr form) environment)
    (let ((value
	   (if (pair? (cddr form))
	       (compile-expr (caddr form) environment)
	       (output/unassigned))))
      (if environment-item
	  (output/access-assignment
	   name
	   (compile-expr-item environment-item)
	   value)
	  (output/assignment name value)))))

(define (classify/location form environment)
  (let ((item (classify-form form environment)))
    (cond ((var-item? item)
	   (values (var-item-id item) #f))
	  ((access-item? item)
	   (values (access-item/name item) (access-item/environment item)))
	  (else
	   (syntax-error "Variable required in this context:" form)))))

(define (compiler:delay form environment)
  (syntax-check '(KEYWORD EXPRESSION) form)
  (output/delay (compile-expr (cadr form) environment)))

;;;; Definitions

(define keyword:define
  (classifier->keyword
   (lambda (form environment)
     (let ((name (cadr form)))
       (reserve-identifier name environment)
       (variable-binder defn-item
			environment
			name
			(classify-form (caddr form) environment))))))

(define (classifier:define-syntax form environment)
  (syntax-check '(keyword identifier expression) form)
  (let ((name (cadr form))
	(item (classify-form (caddr form) environment)))
    (keyword-binder environment name item)
    ;; User-defined macros at top level are preserved in the output.
    (if (and (senv-top-level? environment)
	     (expander-item? item))
	(syntax-defn-item name (expander-item-expr item))
	(seq-item '()))))

(define (keyword-binder environment name item)
  (if (not (keyword-item? item))
      (syntax-error "Keyword binding value must be a keyword:" name))
  (bind-keyword name environment item))

(define (variable-binder k environment name item)
  (if (keyword-item? item)
      (syntax-error "Variable binding value must not be a keyword:" name))
  (k (bind-variable name environment) item))

;;;; LET-like

(define keyword:let
  (classifier->keyword
   (lambda (form env)
     (let ((bindings (cadr form))
	   (body (cddr form))
	   (binding-env (make-internal-senv env)))
       (let ((bindings
	      (map (lambda (binding)
		     (variable-binder cons
				      binding-env
				      (car binding)
				      (classify-form (cadr binding) env)))
		   bindings)))
	 (expr-item
	  (let ((names (map car bindings))
		(values (map cdr bindings))
		(seq-item
		 (classify-body
		  body
		  (make-internal-senv binding-env))))
	    (lambda ()
	      (output/let names
			  (map compile-expr-item values)
			  (compile-body-item seq-item))))))))))

(define (classifier:let-syntax form env)
  (syntax-check '(keyword (* (identifier expression)) + form) form)
  (let ((bindings (cadr form))
	(body (cddr form))
	(binding-env (make-internal-senv env)))
    (for-each (lambda (binding)
		(keyword-binder binding-env
				(car binding)
				(classify-form (cadr binding) env)))
	      bindings)
    (classify-body body (make-internal-senv binding-env))))

(define keyword:let-syntax
  (classifier->keyword classifier:let-syntax))

(define (classifier:letrec-syntax form env)
  (syntax-check '(keyword (* (identifier expression)) + form) form)
  (let ((bindings (cadr form))
	(body (cddr form))
	(binding-env (make-internal-senv env)))
    (for-each (lambda (binding)
		(reserve-identifier (car binding) binding-env))
	      bindings)
    ;; Classify right-hand sides first, in order to catch references to
    ;; reserved names.  Then bind names prior to classifying body.
    (for-each (lambda (binding item)
		(keyword-binder binding-env (car binding) item))
	      bindings
	      (map (lambda (binding)
		     (classify-form (cadr binding) binding-env))
		   bindings))
    (classify-body body (make-internal-senv binding-env))))

;; TODO: this is a compiler rather than a macro because it uses the
;; special OUTPUT/DISJUNCTION.  Unfortunately something downstream in
;; the compiler wants this, but it would be nice to eliminate this
;; hack.
(define (compiler:or form environment)
  (syntax-check '(KEYWORD * EXPRESSION) form)
  (if (pair? (cdr form))
      (let loop ((expressions (cdr form)))
	(let ((compiled (compile-expr (car expressions) environment)))
	  (if (pair? (cdr expressions))
	      (output/disjunction compiled (loop (cdr expressions)))
	      compiled)))
      `#F))

;;;; MIT-specific syntax

(define-record-type <access-item>
    (make-access-item name environment)
    access-item?
  (name access-item/name)
  (environment access-item/environment))

(define keyword:access
  (classifier->keyword
   (lambda (form environment)
     (make-access-item (cadr form)
		       (classify-form (caddr form) environment)))))

(define-item-compiler access-item?
  (lambda (item)
    (output/access-reference
     (access-item/name item)
     (compile-expr-item (access-item/environment item)))))

(define (compiler:the-environment form environment)
  (syntax-check '(KEYWORD) form)
  (if (not (senv-top-level? environment))
      (syntax-error "This form allowed only at top level:" form))
  (output/the-environment))

(define keyword:unspecific
  (compiler->keyword
   (lambda (form environment)
     (declare (ignore form environment))
     (output/unspecific))))

(define keyword:unassigned
  (compiler->keyword
   (lambda (form environment)
     (declare (ignore form environment))
     (output/unassigned))))

;;;; Declarations

(define (classifier:declare form environment)
  (syntax-check '(keyword * (identifier * datum)) form)
  (decl-item
   (lambda ()
     (classify/declarations (cdr form) environment))))

(define (classify/declarations declarations environment)
  (map (lambda (declaration)
	 (classify/declaration declaration environment))
       declarations))

(define (classify/declaration declaration environment)
  (map-declaration-identifiers (lambda (identifier)
				 (var-item-id
				  (classify/variable-reference identifier
							       environment)))
			       declaration))

(define (classify/variable-reference identifier environment)
  (let ((item (classify-form identifier environment)))
    (if (not (var-item? item))
	(syntax-error "Variable required in this context:" identifier))
    item))