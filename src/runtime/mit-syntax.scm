#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

(add-boot-deps! '(runtime syntax items))

;;;; Macro transformers

(define (transformer-classifier transformer->item
				transformer->expander-name)
  (lambda (form senv hist)
    (scheck '(_ expression) form senv hist)
    (let ((transformer
	   (compile-expr-item (classify-form (cadr form)
					     senv
					     (hist-cadr hist)))))
      (transformer->item
       (transformer-eval transformer senv)
       senv
       (expr-item (serror-ctx form senv hist) '()
	 (lambda ()
	   (output/top-level-syntax-expander transformer->expander-name
					     transformer))
	 (lambda ()
	   `(,transformer->expander-name ,transformer)))))))

(define $sc-macro-transformer
  ;; "Syntactic Closures" transformer
  (classifier->runtime
   (transformer-classifier sc-macro-transformer->item
			   'sc-macro-transformer->expander)))

(define $rsc-macro-transformer
  ;; "Reversed Syntactic Closures" transformer
  (classifier->runtime
   (transformer-classifier rsc-macro-transformer->item
			   'rsc-macro-transformer->expander)))

(define $er-macro-transformer
  ;; "Explicit Renaming" transformer
  (classifier->runtime
   (transformer-classifier er-macro-transformer->item
			   'er-macro-transformer->expander)))

(define $spar-macro-transformer
  ;; "Syntax PARser" transformer
  (classifier->runtime
   (transformer-classifier spar-macro-transformer->item
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

(define classifier-item:define
  (spar-classifier-item
   (delay
     (spar-or
       (spar-call-with-values defn-item
	 (spar-subform)
	 (spar-push spar-arg:ctx)
	 (spar-subform
	   (spar-match identifier? spar-arg:form)
	   (spar-push-value bind-variable spar-arg:form spar-arg:senv))
	 (spar-or (spar-subform spar-push-classified)
		  (spar-push-value unassigned-item spar-arg:ctx))
	 (spar-match-null))
       (spar-call-with-values
	   (lambda (senv hist id bvl . body)
	     (apply-classifier-item classifier-item:define
	       `(_ ,id (,keyword:named-lambda (,id . ,bvl) ,@body))
	       senv
	       hist))
	 (spar-subform)
	 (spar-push spar-arg:senv)
	 (spar-push spar-arg:hist)
	 (spar-subform
	   (spar-push-subform-if identifier? spar-arg:form)
	   (spar-push-form-if mit-lambda-list? spar-arg:form))
	 (spar+ (spar-push-subform))
	 (spar-match-null))
       (spar-call-with-values
	   (lambda (senv hist form bvl . body)
	     (apply-classifier-item classifier-item:define
	       `(_ ,form (,keyword:lambda ,bvl ,@body))
	       senv
	       hist))
	 (spar-subform)
	 (spar-push spar-arg:senv)
	 (spar-push spar-arg:hist)
	 (spar-subform
	   (spar-push-subform)
	   (spar-push-form-if mit-lambda-list? spar-arg:form))
	 (spar+ (spar-push-subform))
	 (spar-match-null))))))

(define $define
  (classifier-item->runtime classifier-item:define))

(define $define-syntax
  (spar-classifier->runtime
   (delay
     (spar-call-with-values
	 (lambda (ctx id item)
	   (let ((senv (serror-ctx-senv ctx)))
	     (bind-keyword id senv item)
	     ;; User-defined macros at top level are preserved in the output.
	     (if (and (transformer-item-has-expr? item)
		      (senv-top-level? senv))
		 (syntax-defn-item ctx id (transformer-item-expr item))
		 (seq-item ctx '()))))
       (spar-subform)
       (spar-push spar-arg:ctx)
       (spar-subform
	 (spar-match identifier? spar-arg:form)
	 (spar-funcall reserve-keyword spar-arg:form spar-arg:senv)
	 (spar-push spar-arg:form))
       (spar-subform
	 spar-push-classified
	 (spar-or (spar-match transformer-item? spar-arg:value)
		  (spar-error "Keyword binding value must be a keyword:"
			      spar-arg:form)))
       (spar-match-null)))))

;;;; Lambdas

(define classifier-item:lambda
  (spar-classifier-item
   (delay
     (spar-call-with-values
	 (lambda (ctx bvl body)
	   (assemble-lambda-item ctx scode-lambda-name:unnamed bvl body))
       (spar-subform)
       (spar-push spar-arg:ctx)
       (spar-push-subform-if mit-lambda-list? spar-arg:form)
       (spar-push-body make-internal-senv)))))

(define $lambda
  (classifier-item->runtime classifier-item:lambda))

(define keyword:lambda
  (classifier-item->keyword classifier-item:lambda))

(define classifier-item:named-lambda
  (spar-classifier-item
   (delay
     (spar-call-with-values
	 (lambda (ctx name bvl body)
	   (assemble-lambda-item ctx (identifier->symbol name) bvl body))
       (spar-subform)
       (spar-push spar-arg:ctx)
       (spar-subform
	 (spar-push-subform-if identifier? spar-arg:form)
	 (spar-push-form-if mit-lambda-list? spar-arg:form))
       (spar-push-body make-internal-senv)))))

(define $named-lambda
  (classifier-item->runtime classifier-item:named-lambda))

(define keyword:named-lambda
  (classifier-item->keyword classifier-item:named-lambda))

(define (assemble-lambda-item ctx name bvl body)
  (let ((frame-senv (make-internal-senv (serror-ctx-senv ctx))))
    (lambda-item ctx
		 name
		 (map-mit-lambda-list (lambda (id)
					(bind-variable id frame-senv))
				      bvl)
		 (delay
		   (receive (body-ctx body-items) (body frame-senv)
		     (body-item body-ctx body-items))))))

(define (spar-push-body make-senv)
  (spar-call-with-values
      (lambda (ctx . elts)
	(lambda (frame-senv)
	  (let ((body-senv (make-senv frame-senv)))
	    (values (serror-ctx (serror-ctx-form ctx)
				body-senv
				(serror-ctx-hist ctx))
		    (map-in-order (lambda (elt) (elt body-senv))
				  elts)))))
    (spar-push spar-arg:ctx)
    (spar+ (spar-subform spar-push-open-classified))
    (spar-match-null)))

;;;; LET-like

(define spar-promise:let-syntax
  (delay
    (spar-call-with-values
	(lambda (ctx bindings body)
	  (let ((frame-senv (make-keyword-internal-senv (serror-ctx-senv ctx))))
	    (for-each (lambda (binding)
			(bind-keyword (car binding) frame-senv (cdr binding)))
		      bindings)
	    (receive (body-ctx body-items) (body frame-senv)
	      (seq-item body-ctx body-items))))
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
	 (lambda (ctx bindings body)
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
	     (receive (body-ctx body-items) (body frame-senv)
	       (seq-item body-ctx body-items))))
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

(define $safe-reference
  (spar-classifier->runtime
   (delay
     (spar-call-with-values safe-ref-item
       (spar-subform)
       (spar-push spar-arg:ctx)
       (spar-subform
	 spar-push-classified
	 (spar-or (spar-match var-item? spar-arg:value)
		  (spar-error "Variable required in this context:"
			      spar-arg:form)))
       (spar-match-null)))))

;;;; Declarations

(define $declare
  (spar-classifier->runtime
   (delay
     (spar-call-with-values
	 (lambda (ctx decls)
	   (let ((senv (serror-ctx-senv ctx))
		 (hist (serror-ctx-hist ctx)))
	     (decl-item ctx
	       (delay
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
  (expr-item ctx (list env-item rhs-item)
    (lambda (env rhs)
      (output/access-assignment name env rhs))
    (lambda (env rhs)
      `(set!-access-item ,name ,env ,rhs))))

(define (assignment-item ctx id rhs-item)
  (expr-item ctx (list rhs-item)
    (lambda (rhs)
      (output/assignment id rhs))
    (lambda (rhs)
      `(set!-item ,id ,rhs))))

(define (decl-item ctx classify)
  (expr-item ctx '()
    (lambda ()
      (output/declaration (force classify)))
    (lambda ()
      `(declare-item
	,@(if (promise-forced? classify)
	      (force classify)
	      classify)))))

(define (if-item ctx predicate consequent alternative)
  (expr-item ctx (list predicate consequent alternative)
    output/conditional
    (lambda (predicate consequent alternative)
      `(if-item ,predicate ,consequent ,alternative))))

(define (lambda-item ctx name bvl classify-body)
  (expr-item ctx (list classify-body)
    (lambda (body)
      (output/lambda name bvl body))
    (lambda (body)
      `(lambda-item ,name ,bvl ,body))))

(define (or-item ctx . items)
  (expr-item ctx items
    (lambda exprs
      (output/disjunction exprs))
    (lambda exprs
      `(or-item ,@exprs))))

(define (quoted-id-item ctx var-item)
  (expr-item ctx '()
    (lambda ()
      (output/quoted-identifier (var-item-id var-item)))
    (lambda ()
      `(quoted-id-item ,(var-item-id var-item)))))

(define (safe-ref-item ctx var-item)
  (expr-item ctx '()
    (lambda ()
      (output/variable (var-item-id var-item) #t))
    (lambda ()
      `(safe-ref-item ,(var-item-id var-item)))))

(define (the-environment-item ctx)
  (expr-item ctx '()
	     output/the-environment
	     (lambda () '(the-environment-item))))

(define (unspecific-item ctx)
  (expr-item ctx '()
	     output/unspecific
	     (lambda () '(unspecific-item))))

(define (unassigned-item ctx)
  (expr-item ctx '()
	     output/unassigned
	     (lambda () '(unassigned-item))))