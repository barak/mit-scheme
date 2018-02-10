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
    (syntax-check '(_ expression) form)
    (let ((transformer (compile-expr-item (classify-form-cadr form senv))))
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

(define (compiler:lambda form senv)
  (syntax-check '(_ mit-bvl + form) form)
  (receive (bvl body)
      (compile/lambda (cadr form) (cddr form) senv)
    (output/lambda bvl body)))

(define (compiler:named-lambda form senv)
  (syntax-check '(_ (identifier . mit-bvl) + form) form)
  (receive (bvl body)
      (compile/lambda (cdadr form) (cddr form) senv)
    (output/named-lambda (identifier->symbol (caadr form)) bvl body)))

(define (compile/lambda bvl body senv)
  (let ((senv (make-internal-senv senv)))
    ;; Force order -- bind names before classifying body.
    (let ((bvl
	   (map-mit-lambda-list (lambda (identifier)
				  (bind-variable identifier senv))
				bvl)))
      (values bvl
	      (compile-body-item
	       (classify-body body senv))))))

(define (compile-body-item item)
  (output/body (compile-body-items (item->list item))))

(define (classifier:begin form senv)
  (syntax-check '(_ * form) form)
  (classify-body (cdr form) senv))

(define (compiler:if form senv)
  (syntax-check '(_ expression expression ? expression) form)
  (output/conditional
   (compile-expr-item (classify-form-cadr form senv))
   (compile-expr-item (classify-form-caddr form senv))
   (if (pair? (cdddr form))
       (compile-expr-item (classify-form-cadddr form senv))
       (output/unspecific))))

(define (compiler:quote form senv)
  (declare (ignore senv))
  (syntax-check '(_ datum) form)
  (output/constant (strip-syntactic-closures (cadr form))))

(define (compiler:quote-identifier form senv)
  (syntax-check '(_ identifier) form)
  (let ((item (lookup-identifier (cadr form) senv)))
    (if (not (var-item? item))
	(syntax-error "Can't quote a keyword identifier:" form))
    (output/quoted-identifier (var-item-id item))))

(define (compiler:set! form senv)
  (syntax-check '(_ form ? expression) form)
  (receive (name environment-item)
      (classify/location (cadr form) senv)
    (let ((value
	   (if (pair? (cddr form))
	       (compile-expr-item (classify-form-caddr form senv))
	       (output/unassigned))))
      (if environment-item
	  (output/access-assignment
	   name
	   (compile-expr-item environment-item)
	   value)
	  (output/assignment name value)))))

(define (classify/location form senv)
  (let ((item (classify-form form senv)))
    (cond ((var-item? item)
	   (values (var-item-id item) #f))
	  ((access-item? item)
	   (values (access-item/name item) (access-item/environment item)))
	  (else
	   (syntax-error "Variable required in this context:" form)))))

(define (compiler:delay form senv)
  (syntax-check '(_ expression) form)
  (output/delay (compile-expr-item (classify-form-cadr form senv))))

;;;; Definitions

(define keyword:define
  (classifier->keyword
   (lambda (form senv)
     (let ((name (cadr form)))
       (reserve-identifier name senv)
       (variable-binder defn-item
			senv
			name
			(classify-form-caddr form senv))))))

(define (classifier:define-syntax form senv)
  (syntax-check '(_ identifier expression) form)
  (let ((name (cadr form))
	(item (classify-form-caddr form senv)))
    (keyword-binder senv name item)
    ;; User-defined macros at top level are preserved in the output.
    (if (and (senv-top-level? senv)
	     (expander-item? item))
	(syntax-defn-item name (expander-item-expr item))
	(seq-item '()))))

(define (keyword-binder senv name item)
  (if (not (keyword-item? item))
      (syntax-error "Keyword binding value must be a keyword:" name))
  (bind-keyword name senv item))

(define (variable-binder k senv name item)
  (if (keyword-item? item)
      (syntax-error "Variable binding value must not be a keyword:" name))
  (k (bind-variable name senv) item))

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
				      (classify-form-cadr binding env)))
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
  (syntax-check '(_ (* (identifier expression)) + form) form)
  (let ((bindings (cadr form))
	(body (cddr form))
	(binding-env (make-internal-senv env)))
    (for-each (lambda (binding)
		(keyword-binder binding-env
				(car binding)
				(classify-form-cadr binding env)))
	      bindings)
    (classify-body body (make-internal-senv binding-env))))

(define keyword:let-syntax
  (classifier->keyword classifier:let-syntax))

(define (classifier:letrec-syntax form env)
  (syntax-check '(_ (* (identifier expression)) + form) form)
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
		     (classify-form-cadr binding binding-env))
		   bindings))
    (classify-body body (make-internal-senv binding-env))))

;; TODO: this is a compiler rather than a macro because it uses the
;; special OUTPUT/DISJUNCTION.  Unfortunately something downstream in
;; the compiler wants this, but it would be nice to eliminate this
;; hack.
(define (compiler:or form senv)
  (syntax-check '(_ * expression) form)
  (if (pair? (cdr form))
      (let loop ((expressions (cdr form)))
	(let ((compiled
	       (compile-expr-item (classify-form-car expressions senv))))
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
   (lambda (form senv)
     (make-access-item (cadr form)
		       (classify-form-caddr form senv)))))

(define-item-compiler access-item?
  (lambda (item)
    (output/access-reference
     (access-item/name item)
     (compile-expr-item (access-item/environment item)))))

(define (compiler:the-environment form senv)
  (syntax-check '(_) form)
  (if (not (senv-top-level? senv))
      (syntax-error "This form allowed only at top level:" form))
  (output/the-environment))

(define keyword:unspecific
  (compiler->keyword
   (lambda (form senv)
     (declare (ignore form senv))
     (output/unspecific))))

(define keyword:unassigned
  (compiler->keyword
   (lambda (form senv)
     (declare (ignore form senv))
     (output/unassigned))))

;;;; Declarations

(define (classifier:declare form senv)
  (syntax-check '(_ * (identifier * datum)) form)
  (decl-item
   (lambda ()
     (classify/declarations (cdr form) senv))))

(define (classify/declarations declarations senv)
  (map (lambda (declaration)
	 (classify/declaration declaration senv))
       declarations))

(define (classify/declaration declaration senv)
  (map-declaration-identifiers (lambda (identifier)
				 (var-item-id
				  (classify/variable-reference identifier
							       senv)))
			       declaration))

(define (classify/variable-reference identifier senv)
  (let ((item (classify-form identifier senv)))
    (if (not (var-item? item))
	(syntax-error "Variable required in this context:" identifier))
    item))