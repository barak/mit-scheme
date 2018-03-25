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
       (expr-item
	(lambda ()
	  (output/top-level-syntax-expander transformer->expander-name
					    transformer)))))))

(define :sc-macro-transformer
  ;; "Syntactic Closures" transformer
  (classifier->runtime
   (transformer-classifier sc-macro-transformer->keyword-item
			   'sc-macro-transformer->expander)))

(define :rsc-macro-transformer
  ;; "Reversed Syntactic Closures" transformer
  (classifier->runtime
   (transformer-classifier rsc-macro-transformer->keyword-item
			   'rsc-macro-transformer->expander)))

(define :er-macro-transformer
  ;; "Explicit Renaming" transformer
  (classifier->runtime
   (transformer-classifier er-macro-transformer->keyword-item
			   'er-macro-transformer->expander)))

(define :spar-macro-transformer
  ;; "Syntax PARser" transformer
  (classifier->runtime
   (transformer-classifier spar-macro-transformer->keyword-item
			   'spar-macro-transformer->expander)))

;;;; Core primitives

(define :begin
  (spar-classifier->runtime
   (delay
     (spar-encapsulate-values
	 (lambda (deferred-items)
	   (seq-item
	    (map-in-order (lambda (p) (p))
			  deferred-items)))
       (spar-elt)
       (spar* (spar-elt spar-push-deferred-classified))
       (spar-match-null)))))

(define :if
  (spar-classifier->runtime
   (delay
     (spar-call-with-values if-item
       (spar-elt)
       (spar-elt spar-push-classified)
       (spar-elt spar-push-classified)
       (spar-or (spar-elt spar-push-classified)
		(spar-push-value unspecific-item))
       (spar-match-null)))))

(define :quote
  (spar-classifier->runtime
   (delay
     (spar-call-with-values constant-item
       (spar-elt)
       (spar-elt (spar-push-value strip-syntactic-closures spar-arg:form))
       (spar-match-null)))))

(define :quote-identifier
  (spar-classifier->runtime
   (delay
     (spar-call-with-values quoted-id-item
       (spar-elt)
       (spar-elt
	 (spar-match identifier? spar-arg:form)
	 (spar-push-value lookup-identifier spar-arg:form spar-arg:senv)
	 (spar-or (spar-match var-item? spar-arg:value)
		  (spar-error "Can't quote a keyword identifier:"
			      spar-arg:form)))
       (spar-match-null)))))

(define :set!
  (spar-classifier->runtime
   (delay
     (spar-call-with-values
	 (lambda (lhs-item rhs-item)
	   (if (var-item? lhs-item)
	       (assignment-item (var-item-id lhs-item) rhs-item)
	       (access-assignment-item (access-item-name lhs-item)
				       (access-item-env lhs-item)
				       rhs-item)))
       (spar-elt)
       (spar-elt
	 spar-push-classified
	 (spar-or (spar-match (lambda (lhs-item)
				(or (var-item? lhs-item)
				    (access-item? lhs-item)))
			      spar-arg:value)
		  (spar-error "Variable required in this context:"
			      spar-arg:form)))
       (spar-or (spar-elt spar-push-classified)
		(spar-push-value unassigned-item))
       (spar-match-null)))))

;; TODO: this is a classifier rather than a macro because it uses the
;; special OUTPUT/DISJUNCTION.  Unfortunately something downstream in
;; the compiler wants this, but it would be nice to eliminate this
;; hack.
(define :or
  (spar-classifier->runtime
   (delay
     (spar-encapsulate-values or-item
       (spar-elt)
       (spar* (spar-elt spar-push-classified))
       (spar-match-null)))))

(define :delay
  (spar-classifier->runtime
   (delay
     (spar-call-with-values delay-item
       (spar-elt)
       (spar-elt spar-push-deferred-classified)
       (spar-match-null)))))

;;;; Definitions

(define keyword:define
  (spar-classifier->keyword
   (delay
     (spar-call-with-values defn-item
       (spar-elt)
       (spar-elt
	 (spar-match identifier? spar-arg:form)
	 (spar-push-value bind-variable spar-arg:form spar-arg:senv))
       (spar-elt spar-push-classified)
       (spar-match-null)))))

(define :define-syntax
  (spar-classifier->runtime
   (delay
     (spar-call-with-values
	 (lambda (id senv item)
	   (receive (id senv)
	       (if (closed-identifier? id)
		   (values (syntactic-closure-form id)
			   (syntactic-closure-senv id))
		   (values id senv))
	     (bind-keyword id senv item)
	     ;; User-defined macros at top level are preserved in the output.
	     (if (and (keyword-item-has-expr? item)
		      (senv-top-level? senv))
		 (syntax-defn-item id (keyword-item-expr item))
		 (seq-item '()))))
       (spar-elt)
       (spar-push-elt-if identifier? spar-arg:form)
       (spar-push spar-arg:senv)
       (spar-elt
	 spar-push-classified
	 (spar-or (spar-match keyword-item? spar-arg:value)
		  (spar-error "Keyword binding value must be a keyword:"
			      spar-arg:form)))
       (spar-match-null)))))

;;;; Lambdas

(define :lambda
  (spar-classifier->runtime
   (delay
     (spar-call-with-values
	 (lambda (bvl body senv)
	   (assemble-lambda-item scode-lambda-name:unnamed bvl body senv))
       (spar-elt)
       (spar-push-elt-if mit-lambda-list? spar-arg:form)
       spar-push-body))))

(define :named-lambda
  (spar-classifier->runtime
   (delay
     (spar-call-with-values
	 (lambda (name bvl body senv)
	   (assemble-lambda-item (identifier->symbol name) bvl body senv))
       (spar-elt)
       (spar-elt
	 (spar-push-elt-if identifier? spar-arg:form)
	 (spar-push-form-if mit-lambda-list? spar-arg:form))
       spar-push-body))))

(define (assemble-lambda-item name bvl body senv)
  (let ((frame-senv (make-internal-senv senv)))
    (lambda-item name
		 (map-mit-lambda-list (lambda (id)
					(bind-variable id frame-senv))
				      bvl)
		 (lambda ()
		   (body-item (body frame-senv))))))

;;;; LET-like

(define spar-promise:let-syntax
  (delay
    (spar-call-with-values
	(lambda (bindings body senv)
	  (let ((frame-senv (make-internal-senv senv)))
	    (for-each (lambda (binding)
			(bind-keyword (car binding) frame-senv (cdr binding)))
		      bindings)
	    (seq-item (body frame-senv))))
      (spar-elt)
      (spar-elt
	(spar-call-with-values list
	 (spar*
	   (spar-call-with-values cons
	     (spar-elt (spar-push-elt-if identifier? spar-arg:form)
		       (spar-elt spar-push-classified)
		       (spar-match-null)))))
	(spar-match-null))
       spar-push-body)))

(define :let-syntax
  (spar-classifier->runtime spar-promise:let-syntax))

(define keyword:let-syntax
  (spar-classifier->keyword spar-promise:let-syntax))

(define :letrec-syntax
  (spar-classifier->runtime
   (delay
     (spar-call-with-values
	(lambda (bindings body senv)
	  (let ((frame-senv (make-internal-senv senv))
		(ids (map car bindings)))
	    (for-each (lambda (id)
			(reserve-identifier id frame-senv))
		      ids)
	    (for-each (lambda (id item)
			(bind-keyword id frame-senv item))
		      ids
		      (map (lambda (binding)
			     ((cdr binding) frame-senv))
			   bindings))
	    (seq-item (body frame-senv))))
      (spar-elt)
      (spar-elt
	 (spar-call-with-values list
	   (spar*
	     (spar-call-with-values cons
	       (spar-elt (spar-push-elt-if identifier? spar-arg:form)
			 (spar-elt spar-push-open-classified)
			 (spar-match-null)))))
	 (spar-match-null))
       spar-push-body))))

;;;; MIT-specific syntax

(define-record-type <access-item>
    (access-item name env)
    access-item?
  (name access-item-name)
  (env access-item-env))

(define keyword:access
  (spar-classifier->keyword
   (delay
     (spar-call-with-values access-item
       (spar-elt)
       (spar-push-elt-if identifier? spar-arg:form)
       (spar-elt spar-push-classified)
       (spar-match-null)))))

(define-expr-item-compiler access-item?
  (lambda (item)
    (output/access-reference (access-item-name item)
			     (compile-expr-item (access-item-env item)))))

(define :the-environment
  (spar-classifier->runtime
   (delay
     (spar-seq
       (spar-or (spar-match senv-top-level? spar-arg:senv)
		(spar-error "This form allowed only at top level:"
			    spar-arg:form spar-arg:senv))
       (spar-elt)
       (spar-match-null)
       (spar-push-value the-environment-item)))))

(define keyword:unspecific
  (spar-classifier->keyword
   (delay
     (spar-seq
       (spar-elt)
       (spar-match-null)
       (spar-push-value unspecific-item)))))

(define keyword:unassigned
  (spar-classifier->keyword
   (delay
     (spar-seq
       (spar-elt)
       (spar-match-null)
       (spar-push-value unassigned-item)))))

;;;; Declarations

(define :declare
  (spar-classifier->runtime
   (delay
     (spar-call-with-values
	 (lambda (senv hist decls)
	   (decl-item
	    (lambda ()
	      (smap (lambda (decl hist)
		      (map-decl-ids (lambda (id selector)
				      (classify-id id
						   senv
						   (hist-select selector hist)))
				    decl))
		    decls
		    (hist-cadr hist)))))
       (spar-elt)
       (spar-push spar-arg:senv)
       (spar-push spar-arg:hist)
       (spar-call-with-values list
	 (spar*
	   (spar-push-elt-if (lambda (form)
			       (and (pair? form)
				    (identifier? (car form))
				    (list? (cdr form))))
			     spar-arg:form)))
       (spar-match-null)))))

(define (classify-id id senv hist)
  (let ((item (classify-form id senv hist)))
    (if (not (var-item? item))
	(serror id senv hist "Variable required in this context:" id))
    (var-item-id item)))