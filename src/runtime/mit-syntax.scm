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

;;;; MIT/GNU Scheme Syntax
;;; package: (runtime syntax mit)

(declare (usual-integrations))

;;;; Macro transformers

(define (transformer-classifier transformer->keyword-item
				transformer->expander-name)
  (lambda (form senv hist)
    (scheck '(_ expression) form senv hist)
    (let ((transformer
	   (compile-expr-item (classify-form (cadr form)
					     senv
					     (hist-cadr hist)))))
      (transformer->keyword-item
       (transformer-eval transformer senv)
       senv
       (expr-item (serror-ctx form senv hist)
	 (lambda ()
	   (output/top-level-syntax-expander transformer->expander-name
					     transformer)))))))

(define $sc-macro-transformer
  ;; "Syntactic Closures" transformer
  (classifier->runtime
   (transformer-classifier sc-macro-transformer->keyword-item
			   'sc-macro-transformer->expander)))

(define $rsc-macro-transformer
  ;; "Reversed Syntactic Closures" transformer
  (classifier->runtime
   (transformer-classifier rsc-macro-transformer->keyword-item
			   'rsc-macro-transformer->expander)))

(define $er-macro-transformer
  ;; "Explicit Renaming" transformer
  (classifier->runtime
   (transformer-classifier er-macro-transformer->keyword-item
			   'er-macro-transformer->expander)))

(define $spar-macro-transformer
  ;; "Syntax PARser" transformer
  (classifier->runtime
   (transformer-classifier spar-macro-transformer->keyword-item
			   'spar-macro-transformer->expander)))

;;;; Core primitives

(define $begin
  (spar-classifier->runtime
   (delay
     (spar-call-with-values
	 (lambda (ctx . deferred-items)
	   (seq-item ctx
	     (map-in-order (lambda (p) (p))
			   deferred-items)))
       (spar-subform)
       (spar-push spar-arg:ctx)
       (spar* (spar-subform spar-push-deferred-classified))
       (spar-match-null)))))

(define $if
  (spar-classifier->runtime
   (delay
     (spar-call-with-values if-item
       (spar-subform)
       (spar-push spar-arg:ctx)
       (spar-subform spar-push-classified)
       (spar-subform spar-push-classified)
       (spar-or (spar-subform spar-push-classified)
		(spar-push-value unspecific-item spar-arg:ctx))
       (spar-match-null)))))

(define $quote
  (spar-classifier->runtime
   (delay
     (spar-call-with-values constant-item
       (spar-subform)
       (spar-push spar-arg:ctx)
       (spar-subform (spar-push-value strip-syntactic-closures spar-arg:form))
       (spar-match-null)))))

(define $quote-identifier
  (spar-classifier->runtime
   (delay
     (spar-call-with-values quoted-id-item
       (spar-subform)
       (spar-push spar-arg:ctx)
       (spar-subform
	 (spar-match identifier? spar-arg:form)
	 (spar-push-value lookup-identifier spar-arg:form spar-arg:senv)
	 (spar-or (spar-match var-item? spar-arg:value)
		  (spar-error "Can't quote a keyword identifier:"
			      spar-arg:form)))
       (spar-match-null)))))

(define $set!
  (spar-classifier->runtime
   (delay
     (spar-call-with-values
	 (lambda (ctx lhs-item rhs-item)
	   (if (var-item? lhs-item)
	       (assignment-item ctx (var-item-id lhs-item) rhs-item)
	       (access-assignment-item ctx
				       (access-item-name lhs-item)
				       (access-item-env lhs-item)
				       rhs-item)))
       (spar-subform)
       (spar-push spar-arg:ctx)
       (spar-subform
	 spar-push-classified
	 (spar-or (spar-match (lambda (lhs-item)
				(or (var-item? lhs-item)
				    (access-item? lhs-item)))
			      spar-arg:value)
		  (spar-error "Variable required in this context:"
			      spar-arg:form)))
       (spar-or (spar-subform spar-push-classified)
		(spar-push-value unassigned-item spar-arg:ctx))
       (spar-match-null)))))

;; TODO: this is a classifier rather than a macro because it uses the
;; special OUTPUT/DISJUNCTION.  Unfortunately something downstream in
;; the compiler wants this, but it would be nice to eliminate this
;; hack.
(define $or
  (spar-classifier->runtime
   (delay
     (spar-call-with-values or-item
       (spar-subform)
       (spar-push spar-arg:ctx)
       (spar* (spar-subform spar-push-classified))
       (spar-match-null)))))

;;;; Definitions

(define keyword:define
  (spar-classifier->keyword
   (delay
     (spar-call-with-values defn-item
       (spar-subform)
       (spar-push spar-arg:ctx)
       (spar-subform
	 (spar-match identifier? spar-arg:form)
	 (spar-push-value bind-variable spar-arg:form spar-arg:senv))
       (spar-subform spar-push-classified)
       (spar-match-null)))))

(define $define-syntax
  (spar-classifier->runtime
   (delay
     (spar-call-with-values
	 (lambda (ctx id item)
	   (receive (id senv)
	       (if (closed-identifier? id)
		   (values (syntactic-closure-form id)
			   (syntactic-closure-senv id))
		   (values id (serror-ctx-senv ctx)))
	     (bind-keyword id senv item)
	     ;; User-defined macros at top level are preserved in the output.
	     (if (and (keyword-item-has-expr? item)
		      (senv-top-level? senv))
		 (syntax-defn-item ctx id (keyword-item-expr item))
		 (seq-item ctx '()))))
       (spar-subform)
       (spar-push spar-arg:ctx)
       (spar-push-subform-if identifier? spar-arg:form)
       (spar-subform
	 spar-push-classified
	 (spar-or (spar-match keyword-item? spar-arg:value)
		  (spar-error "Keyword binding value must be a keyword:"
			      spar-arg:form)))
       (spar-match-null)))))

;;;; Lambdas

(define $lambda
  (spar-classifier->runtime
   (delay
     (spar-call-with-values
	 (lambda (ctx bvl body-ctx body)
	   (assemble-lambda-item ctx scode-lambda-name:unnamed bvl
				 body-ctx body))
       (spar-subform)
       (spar-push spar-arg:ctx)
       (spar-push-subform-if mit-lambda-list? spar-arg:form)
       (spar-push-body make-internal-senv)))))

(define $named-lambda
  (spar-classifier->runtime
   (delay
     (spar-call-with-values
	 (lambda (ctx name bvl body-ctx body)
	   (assemble-lambda-item ctx (identifier->symbol name) bvl
				 body-ctx body))
       (spar-subform)
       (spar-push spar-arg:ctx)
       (spar-subform
	 (spar-push-subform-if identifier? spar-arg:form)
	 (spar-push-form-if mit-lambda-list? spar-arg:form))
       (spar-push-body make-internal-senv)))))

(define (spar-push-body make-senv)
  (spar-and
    (spar-push spar-arg:ctx)
    (spar-encapsulate-values
	(lambda (elts)
	  (lambda (frame-senv)
	    (let ((body-senv (make-senv frame-senv)))
	      (map-in-order (lambda (elt) (elt body-senv))
			    elts))))
      (spar+ (spar-subform spar-push-open-classified))
      (spar-match-null))))

(define (assemble-lambda-item ctx name bvl body-ctx body)
  (let ((frame-senv (make-internal-senv (serror-ctx-senv ctx))))
    (lambda-item ctx
		 name
		 (map-mit-lambda-list (lambda (id)
					(bind-variable id frame-senv))
				      bvl)
		 (lambda ()
		   (body-item body-ctx (body frame-senv))))))

;;;; LET-like

(define spar-promise:let-syntax
  (delay
    (spar-call-with-values
	(lambda (ctx bindings body-ctx body)
	  (let ((frame-senv
                 (make-keyword-internal-senv (serror-ctx-senv ctx))))
	    (for-each (lambda (binding)
			(bind-keyword (car binding) frame-senv (cdr binding)))
		      bindings)
	    (seq-item body-ctx (body frame-senv))))
      (spar-subform)
      (spar-push spar-arg:ctx)
      (spar-subform
	(spar-call-with-values list
	 (spar*
	   (spar-call-with-values cons
	     (spar-subform (spar-push-subform-if identifier? spar-arg:form)
			   (spar-subform spar-push-classified)
			   (spar-match-null)))))
	(spar-match-null))
       (spar-push-body make-keyword-internal-senv))))

(define $let-syntax
  (spar-classifier->runtime spar-promise:let-syntax))

(define keyword:let-syntax
  (spar-classifier->keyword spar-promise:let-syntax))

(define $letrec-syntax
  (spar-classifier->runtime
   (delay
     (spar-call-with-values
	 (lambda (ctx bindings body-ctx body)
	   (let ((frame-senv
                  (make-keyword-internal-senv (serror-ctx-senv ctx)))
		 (ids (map car bindings)))
	     (for-each (lambda (id)
			 (reserve-keyword id frame-senv))
		       ids)
	     (for-each (lambda (id item)
			 (bind-keyword id frame-senv item))
		       ids
		       (map (lambda (binding)
			      ((cdr binding) frame-senv))
			    bindings))
	     (seq-item body-ctx (body frame-senv))))
       (spar-subform)
       (spar-push spar-arg:ctx)
       (spar-subform
	 (spar-call-with-values list
	   (spar*
	     (spar-call-with-values cons
	       (spar-subform (spar-push-subform-if identifier? spar-arg:form)
			     (spar-subform spar-push-open-classified)
			     (spar-match-null)))))
	 (spar-match-null))
       (spar-push-body make-keyword-internal-senv)))))

;;;; Pseudo keywords

(define (pseudo-keyword-classifier form senv hist)
  (serror (serror-ctx form senv hist)
	  "Special keyword can't be expanded:" form))

(define $...
  (classifier->runtime pseudo-keyword-classifier))

(define $=>
  (classifier->runtime pseudo-keyword-classifier))

(define $_
  (classifier->runtime pseudo-keyword-classifier))

(define $else
  (classifier->runtime pseudo-keyword-classifier))

(define $unquote
  (classifier->runtime pseudo-keyword-classifier))

(define $unquote-splicing
  (classifier->runtime pseudo-keyword-classifier))

;;;; MIT-specific syntax

(define $access
  (spar-classifier->runtime
   (delay
     (spar-call-with-values
	 (lambda (ctx names env)
	   (fold-right (lambda (name env*)
			 (access-item ctx name env*))
		       env
		       names))
       (spar-subform)
       (spar-push spar-arg:ctx)
       (spar-call-with-values list
	 (spar+ (spar-push-subform-if symbol? spar-arg:form)))
       (spar-subform spar-push-classified)
       (spar-match-null)))))

(define-record-type <access-item>
    (access-item ctx name env)
    access-item?
  (ctx access-item-ctx)
  (name access-item-name)
  (env access-item-env))

(define-item-compiler access-item?
  (lambda (item)
    (output/access-reference (access-item-name item)
			     (compile-expr-item (access-item-env item)))))

(define $the-environment
  (spar-classifier->runtime
   (delay
     (spar-and
       (spar-or (spar-match senv-top-level? spar-arg:senv)
		(spar-error "This form allowed only at top level:"
			    spar-arg:form spar-arg:senv))
       (spar-subform)
       (spar-match-null)
       (spar-push-value the-environment-item spar-arg:ctx)))))

(define keyword:unspecific
  (spar-classifier->keyword
   (delay
     (spar-and
       (spar-subform)
       (spar-match-null)
       (spar-push-value unspecific-item spar-arg:ctx)))))

(define keyword:unassigned
  (spar-classifier->keyword
   (delay
     (spar-and
       (spar-subform)
       (spar-match-null)
       (spar-push-value unassigned-item spar-arg:ctx)))))

;;;; Declarations

(define $declare
  (spar-classifier->runtime
   (delay
     (spar-call-with-values
	 (lambda (ctx decls)
	   (let ((senv (serror-ctx-senv ctx))
		 (hist (serror-ctx-hist ctx)))
	     (decl-item ctx
	       (lambda ()
		 (smap (lambda (decl hist)
			 (map-decl-ids (lambda (id selector)
					 (classify-id id
						      senv
						      (hist-select selector
								   hist)))
				       decl))
		       decls
		       (hist-cadr hist))))))
       (spar-subform)
       (spar-push spar-arg:ctx)
       (spar-call-with-values list
	 (spar*
	   (spar-push-subform-if (lambda (form)
				   (and (pair? form)
					(identifier? (car form))
					(list? (cdr form))))
				 spar-arg:form)))
       (spar-match-null)))))

(define (classify-id id senv hist)
  (let ((item (classify-form id senv hist)))
    (if (not (var-item? item))
	(serror (serror-ctx id senv hist)
		"Variable required in this context:" id))
    (var-item-id item)))

;;;; Specific expression items

(define (access-assignment-item ctx name env-item rhs-item)
  (expr-item ctx
    (lambda ()
      (output/access-assignment name
				(compile-expr-item env-item)
				(compile-expr-item rhs-item)))))

(define (assignment-item ctx id rhs-item)
  (expr-item ctx
    (lambda ()
      (output/assignment id (compile-expr-item rhs-item)))))

(define (decl-item ctx classify)
  (expr-item ctx
    (lambda ()
      (output/declaration (classify)))))

(define (if-item ctx predicate consequent alternative)
  (expr-item ctx
    (lambda ()
      (output/conditional (compile-expr-item predicate)
			  (compile-expr-item consequent)
			  (compile-expr-item alternative)))))

(define (lambda-item ctx name bvl classify-body)
  (expr-item ctx
    (lambda ()
      (output/lambda name bvl (compile-item (classify-body))))))

(define (or-item ctx . items)
  (expr-item ctx
    (lambda ()
      (output/disjunction (map compile-expr-item items)))))

(define (quoted-id-item ctx var-item)
  (expr-item ctx
    (lambda ()
      (output/quoted-identifier (var-item-id var-item)))))

(define (the-environment-item ctx)
  (expr-item ctx output/the-environment))

(define (unspecific-item ctx)
  (expr-item ctx output/unspecific))

(define (unassigned-item ctx)
  (expr-item ctx output/unassigned))