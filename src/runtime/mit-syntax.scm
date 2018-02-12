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
  (lambda (form senv hist)
    (syntax-check '(_ expression) form)
    (let ((transformer (compile-expr-item (classify-form-cadr form senv hist))))
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

(define (classifier:lambda form senv hist)
  (syntax-check '(_ mit-bvl + form) form)
  (classify-lambda scode-lambda-name:unnamed
		   (cadr form)
		   form senv hist))

(define (classifier:named-lambda form senv hist)
  (syntax-check '(_ (identifier . mit-bvl) + form) form)
  (classify-lambda (identifier->symbol (caadr form))
		   (cdadr form)
		   form senv hist))

(define (classify-lambda name bvl form senv hist)
  (let ((senv (make-internal-senv senv)))
    ;; Force order -- bind names before classifying body.
    (let ((bvl
	   (map-mit-lambda-list (lambda (identifier)
				  (bind-variable identifier senv))
				bvl)))
      (lambda-item name
		   bvl
		   (lambda ()
		     (body-item
		      (classify-forms-in-order-cddr form senv hist)))))))

(define (classifier:begin form senv hist)
  (syntax-check '(_ * form) form)
  (seq-item (classify-forms-in-order-cdr form senv hist)))

(define (classifier:if form senv hist)
  (syntax-check '(_ expression expression ? expression) form)
  (if-item (classify-form-cadr form senv hist)
	   (classify-form-caddr form senv hist)
	   (if (pair? (cdddr form))
	       (classify-form-cadddr form senv hist)
	       (unspecific-item))))

(define (classifier:quote form senv hist)
  (declare (ignore senv hist))
  (syntax-check '(_ datum) form)
  (constant-item (strip-syntactic-closures (cadr form))))

(define (classifier:quote-identifier form senv hist)
  (declare (ignore hist))
  (syntax-check '(_ identifier) form)
  (let ((item (lookup-identifier (cadr form) senv)))
    (if (not (var-item? item))
	(syntax-error "Can't quote a keyword identifier:" form))
    (quoted-id-item item)))

(define (classifier:set! form senv hist)
  (syntax-check '(_ form ? expression) form)
  (let ((lhs-item (classify-form-cadr form senv hist))
	(rhs-item
	 (if (pair? (cddr form))
	     (classify-form-caddr form senv hist)
	     (unassigned-item))))
    (cond ((var-item? lhs-item)
	   (assignment-item (var-item-id lhs-item) rhs-item))
	  ((access-item? lhs-item)
	   (access-assignment-item (access-item-name lhs-item)
				   (access-item-env lhs-item)
				   rhs-item))
	  (else
	   (syntax-error "Variable required in this context:" (cadr form))))))

(define (classifier:delay form senv hist)
  (syntax-check '(_ expression) form)
  (delay-item (lambda () (classify-form-cadr form senv hist))))

;;;; Definitions

(define keyword:define
  (classifier->keyword
   (lambda (form senv hist)
     (let ((name (cadr form)))
       (reserve-identifier name senv)
       (variable-binder defn-item
			senv
			name
			(classify-form-caddr form senv hist))))))

(define (classifier:define-syntax form senv hist)
  (syntax-check '(_ identifier expression) form)
  (let ((name (cadr form))
	(item (classify-form-caddr form senv hist)))
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
   (lambda (form senv hist)
     (let* ((body-senv (make-internal-senv senv))
	    (bindings
	     (map (lambda (binding hist)
		    (variable-binder cons
				     body-senv
				     (car binding)
				     (classify-form-cadr binding senv hist)))
		  (cadr form)
		  (subform-hists (cadr form) (hist-cadr hist)))))
       (let-item (map car bindings)
		 (map cdr bindings)
		 (body-item
		  (classify-forms-in-order-cddr form
						(make-internal-senv body-senv)
						hist)))))))

(define (classifier:let-syntax form senv hist)
  (syntax-check '(_ (* (identifier expression)) + form) form)
  (let ((binding-senv (make-internal-senv senv)))
    (for-each (lambda (binding hist)
		(keyword-binder binding-senv
				(car binding)
				(classify-form-cadr binding senv hist)))
	      (cadr form)
	      (subform-hists (cadr form) (hist-cadr hist)))
    (seq-item
     (classify-forms-in-order-cddr form
				   (make-internal-senv binding-senv)
				   hist))))

(define keyword:let-syntax
  (classifier->keyword classifier:let-syntax))

(define (classifier:letrec-syntax form senv hist)
  (syntax-check '(_ (* (identifier expression)) + form) form)
  (let ((binding-senv (make-internal-senv senv)))
    (let ((bindings (cadr form)))
      (for-each (lambda (binding)
		  (reserve-identifier (car binding) binding-senv))
		bindings)
      ;; Classify right-hand sides first, in order to catch references to
      ;; reserved names.  Then bind names prior to classifying body.
      (for-each (lambda (binding item)
		  (keyword-binder binding-senv (car binding) item))
		bindings
		(map (lambda (binding hist)
		       (classify-form-cadr binding binding-senv hist))
		     bindings
		     (subform-hists bindings (hist-cadr hist)))))
    (seq-item
     (classify-forms-in-order-cddr form
				   (make-internal-senv binding-senv)
				   hist))))

;; TODO: this is a classifier rather than a macro because it uses the
;; special OUTPUT/DISJUNCTION.  Unfortunately something downstream in
;; the compiler wants this, but it would be nice to eliminate this
;; hack.
(define (classifier:or form senv hist)
  (syntax-check '(_ * expression) form)
  (or-item (classify-forms-cdr form senv hist)))

;;;; MIT-specific syntax

(define-record-type <access-item>
    (access-item name env)
    access-item?
  (name access-item-name)
  (env access-item-env))

(define keyword:access
  (classifier->keyword
   (lambda (form senv hist)
     (access-item (cadr form)
		  (classify-form-caddr form senv hist)))))

(define-item-compiler access-item?
  (lambda (item)
    (output/access-reference (access-item-name item)
			     (compile-expr-item (access-item-env item)))))

(define (classifier:the-environment form senv hist)
  (declare (ignore hist))
  (syntax-check '(_) form)
  (if (not (senv-top-level? senv))
      (syntax-error "This form allowed only at top level:" form))
  (the-environment-item))

(define keyword:unspecific
  (classifier->keyword
   (lambda (form senv hist)
     (declare (ignore form senv hist))
     (unspecific-item))))

(define keyword:unassigned
  (classifier->keyword
   (lambda (form senv hist)
     (declare (ignore form senv hist))
     (unassigned-item))))

;;;; Declarations

(define (classifier:declare form senv hist)
  (syntax-check '(_ * (identifier * datum)) form)
  (decl-item
   (lambda ()
     (classify-decls (cdr form) senv (hist-cdr hist)))))

(define (classify-decls decls senv hist)
  (map (lambda (decl hist)
	 (classify-decl decl senv hist))
       decls
       (subform-hists decls hist)))

(define (classify-decl decl senv hist)
  (map-decl-ids (lambda (id)
		  ;; Need to get the right hist here.
		  (classify-id id senv hist))
		decl))

(define (classify-id id senv hist)
  (let ((item (classify-form id senv hist)))
    (if (not (var-item? item))
	(syntax-error "Variable required in this context:" id))
    (var-item-id item)))