#| -*-Scheme-*-

$Id: syntactic-closures.scm,v 14.18 2007/01/05 21:19:28 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

;;;; Syntactic Closures
;;;  Based on a design by Alan Bawden.

;;; This is a two-stage program: the first stage classifies input
;;; expressions into types, e.g. "definition", "lambda body",
;;; "expression", etc., and the second stage compiles those classified
;;; expressions ("items") into output code.  The classification stage
;;; permits discovery of internal definitions prior to code
;;; generation.  It also identifies keywords and variables, which
;;; allows a powerful form of syntactic binding to be implemented.

;;; See "Syntactic Closures", by Alan Bawden and Jonathan Rees, in
;;; Proceedings of the 1988 ACM Conference on Lisp and Functional
;;; Programming, page 86.

(declare (usual-integrations))

;;;; Compiler

(define (syntax form environment)
  (syntax* (list form) environment))

(define (syntax* forms environment)
  (if (not (list? forms))
      (error:wrong-type-argument forms "list" 'SYNTAX*))
  (guarantee-syntactic-environment environment 'SYNTAX*)
  (fluid-let ((*rename-database* (initial-rename-database)))
    (output/post-process-expression
     (if (syntactic-environment/top-level? environment)
	 (let ((environment
		(make-top-level-syntactic-environment environment)))
	   (compile-body-items/top-level
	    (classify/body-forms forms
				 environment
				 environment
				 (make-top-level-history forms environment)
				 select-object)))
	 (output/sequence
	  (compile/expressions forms
			       environment
			       (make-top-level-history forms environment)))))))

(define (compile-item/top-level item)
  (if (binding-item? item)
      (let ((name (binding-item/name item))
	    (value (binding-item/value item)))
	(if (keyword-value-item? value)
	    (output/top-level-syntax-definition
	     name
	     (compile-item/expression (keyword-value-item/expression value)))
	    (output/top-level-definition
	     name
	     (compile-item/expression value))))
      (compile-item/expression item)))

(define (compile-body-items/top-level body-items)
  (call-with-values (lambda () (extract-declarations-from-body body-items))
    (lambda (declaration-items body-items)
      (output/top-level-sequence (map declaration-item/text declaration-items)
				 (map compile-item/top-level body-items)))))

(define (compile-item/expression item)
  (if (not (item? item))
      (error:wrong-type-argument item "item" 'COMPILE-ITEM/EXPRESSION))
  (let ((compiler (get-item-compiler item)))
    (if (not compiler)
	(error:bad-range-argument item 'COMPILE-ITEM/EXPRESSION))
    (compiler item)))

(define (get-item-compiler item)
  (let ((entry
	 (assq (record-type-descriptor (item/record item)) item-compilers)))
    (and entry
	 (cdr entry))))

(define (define-item-compiler rtd compiler)
  (let ((entry (assq rtd item-compilers)))
    (if entry
	(set-cdr! entry compiler)
	(begin
	  (set! item-compilers (cons (cons rtd compiler) item-compilers))
	  unspecific))))

(define item-compilers '())

(define (compile/expression expression environment history)
  (compile-item/expression
   (classify/expression expression environment history)))

(define (compile/expressions expressions environment history)
  (compile/subexpressions expressions environment history select-object))

(define (compile/subexpression expression environment history selector)
  (compile-item/expression
   (classify/subexpression expression environment history selector)))

(define (compile/subexpressions expressions environment history selector)
  (select-map (lambda (expression selector)
		(compile/subexpression expression
				       environment
				       history
				       selector))
	      expressions
	      selector))

;;;; Classifier

(define (classify/form form environment definition-environment history)
  (cond ((identifier? form)
	 (let ((item
		(item/new-history (lookup-identifier environment form)
				  history)))
	   (if (keyword-item? item)
	       (make-keyword-ref-item (strip-keyword-value-item item)
				      form
				      history)
	       item)))
	((syntactic-closure? form)
	 (let ((form (syntactic-closure/form form))
	       (environment
		(make-filtered-syntactic-environment
		 (syntactic-closure/free-names form)
		 environment
		 (syntactic-closure/environment form))))
	   (classify/form form
			  environment
			  definition-environment
			  (history/replace-reduction form
						     environment
						     history))))
	((pair? form)
	 (let ((item
		(strip-keyword-value-item
		 (classify/subexpression (car form) environment history
					 select-car))))
	   (cond ((classifier-item? item)
		  ((classifier-item/classifier item) form
						     environment
						     definition-environment
						     history))
		 ((compiler-item? item)
		  (classify/compiler item form environment history))
		 ((expander-item? item)
		  (classify/expander item
				     form
				     environment
				     definition-environment
				     history))
		 (else
		  (if (not (list? (cdr form)))
		      (syntax-error history
				    "Combination must be a proper list:"
				    form))
		  (let ((items
			 (classify/subexpressions (cdr form)
						  environment
						  history
						  select-cdr)))
		    (make-expression-item
		     history
		     (lambda ()
		       (output/combination
			(compile-item/expression item)
			(map compile-item/expression items)))))))))
	(else
	 (make-expression-item history (lambda () (output/constant form))))))

(define (classify/compiler item form environment history)
  (make-expression-item history
    (lambda ()
      ((compiler-item/compiler item) form environment history))))

(define (classify/expander item form environment definition-environment
			   history)
  (let ((form
	 ((expander-item/expander item) form
					environment
					(expander-item/environment item))))
    (classify/form form
		   environment
		   definition-environment
		   (history/add-reduction form environment history))))

(define (classify/subform form environment definition-environment
			  history selector)
  (classify/form form
		 environment
		 definition-environment
		 (history/add-subproblem form environment history selector)))

(define (classify/subforms forms environment definition-environment
			   history selector)
  (select-map (lambda (form selector)
		(classify/subform form environment definition-environment
				  history selector))
	      forms
	      selector))

(define (classify/expression expression environment history)
  (classify/form expression environment null-syntactic-environment history))

(define (classify/subexpression expression environment history selector)
  (classify/subform expression environment null-syntactic-environment
		    history selector))

(define (classify/subexpressions expressions environment history selector)
  (classify/subforms expressions environment null-syntactic-environment
		     history selector))

(define (classify/body forms environment definition-environment history
		       selector)
  (make-body-item history
		  (classify/body-forms forms
				       environment
				       definition-environment
				       history
				       selector)))

(define (classify/body-forms forms environment definition-environment history
			     selector)
  ;; Top-level syntactic definitions affect all forms that appear
  ;; after them, so classify FORMS in order.
  (let forms-loop ((forms forms) (selector selector) (body-items '()))
    (if (pair? forms)
	(let items-loop
	    ((items
	      (item->list
	       (classify/subform (car forms)
				 environment
				 definition-environment
				 history
				 (selector/add-car selector))))
	     (body-items body-items))
	  (if (pair? items)
	      (items-loop (cdr items)
			  (if (null-binding-item? (car items))
			      body-items
			      (cons (car items) body-items)))
	      (forms-loop (cdr forms)
			  (selector/add-cdr selector)
			  body-items)))
	(reverse! body-items))))

(define (extract-declarations-from-body items)
  (let loop ((items items) (declarations '()) (items* '()))
    (if (pair? items)
	(if (declaration-item? (car items))
	    (loop (cdr items)
		  (cons (car items) declarations)
		  items*)
	    (loop (cdr items)
		  declarations
		  (cons (car items) items*)))
	(values (reverse! declarations) (reverse! items*)))))

(define (strip-keyword-value-item item)
  (if (keyword-value-item? item)
      (keyword-value-item/item item)
      item))

;;;; Syntactic Closures
 
(define-record-type <syntactic-closure>
    (%make-syntactic-closure environment free-names form)
    syntactic-closure?
  (environment syntactic-closure/environment)
  (free-names syntactic-closure/free-names)
  (form syntactic-closure/form))

(define (make-syntactic-closure environment free-names form)
  (guarantee-syntactic-environment environment 'MAKE-SYNTACTIC-CLOSURE)
  (guarantee-list-of-type free-names identifier?
			  "list of identifiers" 'MAKE-SYNTACTIC-CLOSURE)
  (if (or (memq form free-names)	;LOOKUP-IDENTIFIER assumes this.
	  (and (syntactic-closure? form)
	       (null? (syntactic-closure/free-names form))
	       (not (identifier? (syntactic-closure/form form))))
	  (not (or (syntactic-closure? form)
		   (pair? form)
		   (symbol? form))))
      form
      (%make-syntactic-closure environment free-names form)))

(define (strip-syntactic-closures object)
  (if (let loop ((object object))
	(if (pair? object)
	    (or (loop (car object))
		(loop (cdr object)))
	    (syntactic-closure? object)))
      (let loop ((object object))
	(if (pair? object)
	    (cons (loop (car object))
		  (loop (cdr object)))
	    (if (syntactic-closure? object)
		(loop (syntactic-closure/form object))
		object)))
      object))

(define (close-syntax form environment)
  (make-syntactic-closure environment '() form))

(define (identifier? object)
  (or (symbol? object)
      (synthetic-identifier? object)))

(define (synthetic-identifier? object)
  (and (syntactic-closure? object)
       (identifier? (syntactic-closure/form object))))

(define (make-synthetic-identifier identifier)
  (close-syntax identifier null-syntactic-environment))

(define (identifier->symbol identifier)
  (or (let loop ((identifier identifier))
	(if (syntactic-closure? identifier)
	    (loop (syntactic-closure/form identifier))
	    (and (symbol? identifier)
		 identifier)))
      (error:wrong-type-argument identifier "identifier" 'IDENTIFIER->SYMBOL)))

(define (identifier=? environment-1 identifier-1 environment-2 identifier-2)
  (let ((item-1 (lookup-identifier environment-1 identifier-1))
	(item-2 (lookup-identifier environment-2 identifier-2)))
    (or (item=? item-1 item-2)
	;; This is necessary because an identifier that is not
	;; explicitly bound by an environment is mapped to a variable
	;; item, and the variable items are not cached.  Therefore
	;; two references to the same variable result in two
	;; different variable items.
	(and (variable-item? item-1)
	     (variable-item? item-2)
	     (eq? (variable-item/name item-1)
		  (variable-item/name item-2))))))

;;;; Syntactic Environments

(define (syntactic-environment? object)
  (or (internal-syntactic-environment? object)
      (top-level-syntactic-environment? object)
      (environment? object)
      (filtered-syntactic-environment? object)
      (null-syntactic-environment? object)))

(define (guarantee-syntactic-environment object name)
  (if (not (syntactic-environment? object))
      (error:wrong-type-argument object "syntactic environment" name)))

(define (syntactic-environment/top-level? object)
  (or (top-level-syntactic-environment? object)
      (interpreter-environment? object)))

(define (lookup-identifier environment identifier)
  (let ((item (syntactic-environment/lookup environment identifier)))
    (cond (item
	   (if (reserved-name-item? item)
	       (syntax-error (item/history item)
			     "Premature reference to reserved name:"
			     identifier)
	       item))
	  ((symbol? identifier)
	   (make-variable-item identifier))
	  ((syntactic-closure? identifier)
	   (lookup-identifier (syntactic-closure/environment identifier)
			      (syntactic-closure/form identifier)))
	  (else
	   (error:wrong-type-argument identifier "identifier"
				      'LOOKUP-IDENTIFIER)))))

(define (syntactic-environment/lookup environment name)
  (cond ((internal-syntactic-environment? environment)
	 (internal-syntactic-environment/lookup environment name))
	((top-level-syntactic-environment? environment)
	 (top-level-syntactic-environment/lookup environment name))
	((environment? environment)
	 (and (symbol? name)
	      (environment/lookup environment name)))
	((filtered-syntactic-environment? environment)
	 (filtered-syntactic-environment/lookup environment name))
	((null-syntactic-environment? environment)
	 (null-syntactic-environment/lookup environment name))
	(else
	 (error:wrong-type-argument environment "syntactic environment"
				    'SYNTACTIC-ENVIRONMENT/LOOKUP))))

(define (syntactic-environment/define environment name item)
  (cond ((internal-syntactic-environment? environment)
	 (internal-syntactic-environment/define environment name item))
	((top-level-syntactic-environment? environment)
	 (top-level-syntactic-environment/define environment name item))
	((environment? environment)
	 (environment/define environment name item))
	((filtered-syntactic-environment? environment)
	 (filtered-syntactic-environment/define environment name item))
	((null-syntactic-environment? environment)
	 (null-syntactic-environment/define environment name item))
	(else
	 (error:wrong-type-argument environment "syntactic environment"
				    'SYNTACTIC-ENVIRONMENT/DEFINE))))

(define (syntactic-environment/rename environment name)
  (cond ((internal-syntactic-environment? environment)
	 (internal-syntactic-environment/rename environment name))
	((top-level-syntactic-environment? environment)
	 (top-level-syntactic-environment/rename environment name))
	((environment? environment)
	 (environment/rename environment name))
	((filtered-syntactic-environment? environment)
	 (filtered-syntactic-environment/rename environment name))
	((null-syntactic-environment? environment)
	 (null-syntactic-environment/rename environment name))
	(else
	 (error:wrong-type-argument environment "syntactic environment"
				    'SYNTACTIC-ENVIRONMENT/RENAME))))

(define (syntactic-environment->environment environment)
  (cond ((internal-syntactic-environment? environment)
	 (internal-syntactic-environment->environment environment))
	((top-level-syntactic-environment? environment)
	 (top-level-syntactic-environment->environment environment))
	((environment? environment)
	 environment)
	((filtered-syntactic-environment? environment)
	 (filtered-syntactic-environment->environment environment))
	((null-syntactic-environment? environment)
	 (null-syntactic-environment->environment environment))
	(else
	 (error:wrong-type-argument environment "syntactic environment"
				    'SYNTACTIC-ENVIRONMENT->ENVIRONMENT))))

;;; Null syntactic environments signal an error for any operation.
;;; They are used as the definition environment for expressions (to
;;; prevent illegal use of definitions) and to seal off environments
;;; used in magic keywords.

(define-record-type <null-syntactic-environment>
    (%make-null-syntactic-environment)
    null-syntactic-environment?)

(define null-syntactic-environment
  (%make-null-syntactic-environment))

(define (null-syntactic-environment/lookup environment name)
  environment
  (error "Can't lookup name in null syntactic environment:" name))

(define (null-syntactic-environment/define environment name item)
  environment
  (error "Can't bind name in null syntactic environment:" name item))

(define (null-syntactic-environment/rename environment name)
  environment
  (error "Can't rename name in null syntactic environment:" name))

(define (null-syntactic-environment->environment environment)
  environment
  (error "Can't evaluate in null syntactic environment."))

;;; Runtime environments can be used to look up keywords, but can't be
;;; modified.

(define (environment/lookup environment name)
  (let ((item (environment-lookup-macro environment name)))
    (cond ((or (item? item) (not item))
	   item)
	  ;; **** Kludge to support bootstrapping.
	  ((procedure? item)
	   (non-hygienic-macro-transformer->expander item environment))
	  (else
	   (error:wrong-type-datum item "syntactic keyword")))))

(define (environment/define environment name item)
  (environment-define-macro environment name item))

(define (environment/rename environment name)
  environment
  (rename-top-level-identifier name))

;;; Top-level syntactic environments represent top-level environments.
;;; They are always layered over a real syntactic environment.

(define-record-type <top-level-syntactic-environment>
    (%make-top-level-syntactic-environment parent bound)
    top-level-syntactic-environment?
  (parent top-level-syntactic-environment/parent)
  (bound top-level-syntactic-environment/bound
	 set-top-level-syntactic-environment/bound!))

(define (make-top-level-syntactic-environment parent)
  (guarantee-syntactic-environment parent
				   'MAKE-TOP-LEVEL-SYNTACTIC-ENVIRONMENT)
  (if (not (or (syntactic-environment/top-level? parent)
	       (null-syntactic-environment? parent)))
      (error:bad-range-argument parent "top-level syntactic environment"
				'MAKE-TOP-LEVEL-SYNTACTIC-ENVIRONMENT))
  (%make-top-level-syntactic-environment parent '()))

(define (top-level-syntactic-environment/lookup environment name)
  (let ((binding
	 (assq name (top-level-syntactic-environment/bound environment))))
    (if binding
	(cdr binding)
	(syntactic-environment/lookup
	 (top-level-syntactic-environment/parent environment)
	 name))))

(define (top-level-syntactic-environment/define environment name item)
  (let ((bound (top-level-syntactic-environment/bound environment)))
    (let ((binding (assq name bound)))
      (if binding
	  (set-cdr! binding item)
	  (set-top-level-syntactic-environment/bound!
	   environment
	   (cons (cons name item) bound))))))

(define (top-level-syntactic-environment/rename environment name)
  environment
  (rename-top-level-identifier name))

(define (top-level-syntactic-environment->environment environment)
  (syntactic-environment->environment
   (top-level-syntactic-environment/parent environment)))

;;; Internal syntactic environments represent environments created by
;;; procedure application.

(define-record-type <internal-syntactic-environment>
    (%make-internal-syntactic-environment parent bound free rename-state)
    internal-syntactic-environment?
  (parent internal-syntactic-environment/parent)
  (bound internal-syntactic-environment/bound
	 set-internal-syntactic-environment/bound!)
  (free internal-syntactic-environment/free
	set-internal-syntactic-environment/free!)
  (rename-state internal-syntactic-environment/rename-state))

(define (make-internal-syntactic-environment parent)
  (guarantee-syntactic-environment parent 'MAKE-INTERNAL-SYNTACTIC-ENVIRONMENT)
  (%make-internal-syntactic-environment parent '() '() (make-rename-id)))

(define (internal-syntactic-environment/lookup environment name)
  (let ((binding
	 (or (assq name (internal-syntactic-environment/bound environment))
	     (assq name (internal-syntactic-environment/free environment)))))
    (if binding
	(cdr binding)
	(let ((item
	       (syntactic-environment/lookup
		(internal-syntactic-environment/parent environment)
		name)))
	  (set-internal-syntactic-environment/free!
	   environment
	   (cons (cons name item)
		 (internal-syntactic-environment/free environment)))
	  item))))

(define (internal-syntactic-environment/define environment name item)
  (cond ((assq name (internal-syntactic-environment/bound environment))
	 => (lambda (binding)
	      (set-cdr! binding item)))
	((assq name (internal-syntactic-environment/free environment))
	 (if (reserved-name-item? item)
	     (syntax-error (item/history item)
			   "Premature reference to reserved name:"
			   name)
	     (error "Can't define name; already free:" name)))
	(else
	 (set-internal-syntactic-environment/bound!
	  environment
	  (cons (cons name item)
		(internal-syntactic-environment/bound environment))))))

(define (internal-syntactic-environment/rename environment name)
  (rename-identifier
   name
   (internal-syntactic-environment/rename-state environment)))

(define (internal-syntactic-environment->environment environment)
  (syntactic-environment->environment
   (internal-syntactic-environment/parent environment)))

;;; Filtered syntactic environments are used to implement syntactic
;;; closures that have free names.

(define-record-type <filtered-syntactic-environment>
    (%make-filtered-syntactic-environment names
					  names-environment
					  else-environment)
    filtered-syntactic-environment?
  (names filtered-syntactic-environment/names)
  (names-environment filtered-syntactic-environment/names-environment)
  (else-environment filtered-syntactic-environment/else-environment))

(define (make-filtered-syntactic-environment names
					     names-environment
					     else-environment)
  (if (or (null? names)
	  (eq? names-environment else-environment))
      else-environment
      (%make-filtered-syntactic-environment names
					    names-environment
					    else-environment)))

(define (filtered-syntactic-environment/lookup environment name)
  (syntactic-environment/lookup
   (if (memq name (filtered-syntactic-environment/names environment))
       (filtered-syntactic-environment/names-environment environment)
       (filtered-syntactic-environment/else-environment environment))
   name))

(define (filtered-syntactic-environment/define environment name item)
  ;; **** Shouldn't this be a syntax error?  It can happen as the
  ;; result of a misplaced definition.  ****
  (error "Can't bind name in filtered syntactic environment:"
	 environment name item))

(define (filtered-syntactic-environment/rename environment name)
  (syntactic-environment/rename
   (if (memq name (filtered-syntactic-environment/names environment))
       (filtered-syntactic-environment/names-environment environment)
       (filtered-syntactic-environment/else-environment environment))
   name))

(define (filtered-syntactic-environment->environment environment)
  ;; **** Shouldn't this be a syntax error?  It can happen as the
  ;; result of a partially-closed transformer.  ****
  (error "Can't evaluate in filtered syntactic environment:" environment))

;;;; Items

;;; Some of the item code is in "syntax-transform.scm" because it is
;;; needed during the cold load.

(define item?
  (record-predicate <item>))

(define item/history
  (record-accessor <item> 'HISTORY))

(define (item/new-history item history)
  (make-item history (item/record item)))

(define item/record
  (record-accessor <item> 'RECORD))

(define (item=? x y)
  (eq? (item/record x) (item/record y)))

(define (make-item-type name fields compiler)
  (let ((rtd (make-record-type name fields)))
    (define-item-compiler rtd compiler)
    rtd))

(define (item-predicate rtd)
  (let ((predicate (record-predicate rtd)))
    (lambda (item)
      (predicate (item/record item)))))

(define (item-accessor rtd field)
  (let ((accessor (record-accessor rtd field)))
    (lambda (item)
      (accessor (item/record item)))))

(define (illegal-expression-item item description)
  (let ((history (item/history item)))
    (syntax-error history
		  (string-append description
				 " may not be used as an expression:")
		  (history/original-form history))))

;;; Reserved name items do not represent any form, but instead are
;;; used to reserve a particular name in a syntactic environment.  If
;;; the classifier refers to a reserved name, a syntax error is
;;; signalled.  This is used in the implementation of LETREC-SYNTAX
;;; to signal a meaningful error when one of the <init>s refers to
;;; one of the names being bound.

(define <reserved-name-item>
  (make-item-type "reserved-name-item" '()
    (lambda (item)
      (illegal-expression-item item "Reserved name"))))

(define make-reserved-name-item
  (item-constructor <reserved-name-item> '()))

(define reserved-name-item?
  (item-predicate <reserved-name-item>))

;;; Keyword items represent macro keywords.  There are several flavors
;;; of keyword item.

(define (keyword-item? item)
  (or (classifier-item? item)
      (compiler-item? item)
      (expander-item? item)
      (keyword-value-item? item)))

(define (make-keyword-type name fields)
  (make-item-type name fields keyword-item-compiler))

(define (keyword-item-compiler item)
  (illegal-expression-item item "Syntactic keyword"))


(define <classifier-item>
  (make-keyword-type "classifier-item" '(CLASSIFIER)))

(define make-classifier-item
  (keyword-constructor <classifier-item> '(CLASSIFIER)))

(define classifier-item?
  (item-predicate <classifier-item>))

(define classifier-item/classifier
  (item-accessor <classifier-item> 'CLASSIFIER))


(define <compiler-item>
  (make-keyword-type "compiler-item" '(COMPILER)))

(define make-compiler-item
  (keyword-constructor <compiler-item> '(COMPILER)))

(define compiler-item?
  (item-predicate <compiler-item>))

(define compiler-item/compiler
  (item-accessor <compiler-item> 'COMPILER))


(define-item-compiler <expander-item>
  keyword-item-compiler)

(define expander-item?
  (item-predicate <expander-item>))

(define expander-item/expander
  (item-accessor <expander-item> 'EXPANDER))

(define expander-item/environment
  (item-accessor <expander-item> 'ENVIRONMENT))


(define <keyword-value-item>
  (make-keyword-type "keyword-value-item" '(ITEM EXPRESSION)))

(define make-keyword-value-item
  (keyword-constructor <keyword-value-item> '(ITEM EXPRESSION)))

(define keyword-value-item?
  (item-predicate <keyword-value-item>))

(define keyword-value-item/item
  (item-accessor <keyword-value-item> 'ITEM))

(define keyword-value-item/expression
  (item-accessor <keyword-value-item> 'EXPRESSION))

(define (make-keyword-ref-item item identifier history)
  (make-keyword-value-item item
    (make-expression-item history
      (let ((name (identifier->symbol identifier)))
	(lambda ()
	  (output/combination
	   (output/access-reference 'SYNTACTIC-KEYWORD->ITEM
				    system-global-environment)
	   (list name (output/the-environment))))))))

;;; Variable items represent run-time variables.

(define <variable-item>
  (make-item-type "variable-item" '(NAME)
    (lambda (item)
      (output/variable (variable-item/name item)))))

(define make-variable-item
  (let ((constructor (item-constructor <variable-item> '(NAME))))
    (lambda (name)
      (constructor #f name))))

(define variable-item?
  (item-predicate <variable-item>))

(define variable-item/name
  (item-accessor <variable-item> 'NAME))

;;; Expression items represent any kind of expression other than a
;;; run-time variable or a sequence.  The ANNOTATION field is used to
;;; make expression items that can appear in non-expression contexts
;;; (for example, this could be used in the implementation of SETF).

(define <expression-item>
  (make-item-type "expression-item" '(COMPILER ANNOTATION)
    (lambda (item)
      ((expression-item/compiler item)))))

(define make-special-expression-item
  (item-constructor <expression-item> '(COMPILER ANNOTATION)))

(define expression-item?
  (item-predicate <expression-item>))

(define expression-item/compiler
  (item-accessor <expression-item> 'COMPILER))

(define expression-item/annotation
  (item-accessor <expression-item> 'ANNOTATION))

(define (make-expression-item history compiler)
  (make-special-expression-item history compiler #f))

;;; Unassigned items represent the right hand side of a binding that
;;; has no explicit value.

(define <unassigned-item>
  (make-item-type "unassigned-item" '()
    (lambda (item)
      item				;ignore
      (output/unassigned))))

(define make-unassigned-item
  (item-constructor <unassigned-item> '()))

(define unassigned-item?
  (item-predicate <unassigned-item>))

;;; Declaration items represent block-scoped declarations that are to
;;; be passed through to the compiler.

(define <declaration-item>
  (make-item-type "declaration-item" '(TEXT)
    (lambda (item)
      (illegal-expression-item item "Declaration"))))

(define make-declaration-item
  (item-constructor <declaration-item> '(TEXT)))

(define declaration-item?
  (item-predicate <declaration-item>))

(define declaration-item/text
  (let ((accessor (item-accessor <declaration-item> 'TEXT)))
    (lambda (item)
      ((accessor item)))))

;;; Body items represent sequences (e.g. BEGIN).

(define <body-item>
  (make-item-type "body-item" '(COMPONENTS)
    (lambda (item)
      (compile-body-items item (body-item/components item)))))

(define (compile-body-items item items)
  (let ((items (flatten-body-items items)))
    (if (not (pair? items))
	(illegal-expression-item item "Empty sequence"))
    (output/sequence
     (map (lambda (item)
	    (if (binding-item? item)
		(let ((value (binding-item/value item)))
		  (if (keyword-value-item? value)
		      (output/sequence '())
		      (output/definition (binding-item/name item)
					 (compile-item/expression value))))
		(compile-item/expression item)))
	  items))))

(define make-body-item
  (item-constructor <body-item> '(COMPONENTS)))

(define body-item?
  (item-predicate <body-item>))

(define body-item/components
  (item-accessor <body-item> 'COMPONENTS))

;;; Binding items represent definitions, whether top-level or
;;; internal, keyword or variable.  Null binding items are for
;;; definitions that don't emit code.

(define <binding-item>
  (make-item-type "binding-item" '(NAME VALUE)
    (lambda (item)
      (illegal-expression-item item "Definition"))))

(define make-binding-item
  (item-constructor <binding-item> '(NAME VALUE)))

(define binding-item?
  (item-predicate <binding-item>))

(define binding-item/name
  (item-accessor <binding-item> 'NAME))

(define binding-item/value
  (item-accessor <binding-item> 'VALUE))

(define <null-binding-item>
  (make-item-type "null-binding-item" '()
    (lambda (item)
      (illegal-expression-item item "Definition"))))

(define make-null-binding-item
  (item-constructor <null-binding-item> '()))

(define null-binding-item?
  (item-predicate <null-binding-item>))

(define (bind-variable! environment name)
  (let ((rename (syntactic-environment/rename environment name)))
    (syntactic-environment/define environment
				  name
				  (make-variable-item rename))
    rename))

;;;; Expansion History
;;;  This records each step of the expansion process, separating it
;;;  into subproblems (really, subforms) and reductions.  The history
;;;  is attached to the items that are the result of classification,
;;;  so that meaningful debugging information is available after
;;;  classification has been performed.  The history is NOT preserved
;;;  by the compilation process, although it might be useful to
;;;  extract a small part of the recorded information and store it in
;;;  the output (for example, keeping track of what input form each
;;;  output form corresponds to).

;;;  Note: this abstraction could be implemented in a much simpler
;;;  way, to reduce memory usage.  A history need not remember
;;;  anything other than the original-form for the current reduction,
;;;  plus a bit saying whether that original-form is also the current
;;;  one (for replace-reduction).

(define (make-top-level-history forms environment)
  (list (list (cons forms environment))))

(define (history/add-reduction form environment history)
  (and history
       (cons (cons (cons form environment)
		   (car history))
	     (cdr history))))

(define (history/replace-reduction form environment history)
  ;; This is like ADD-REDUCTION, but it discards the current reduction
  ;; before adding a new one.  This is used when the current reduction
  ;; is not interesting, such as when reducing a syntactic closure.
  (and history
       (cons (cons (cons form environment)
		   (cdar history))
	     (cdr history))))

(define (history/add-subproblem form environment history selector)
  (and history
       (cons (list (cons form environment))
	     (cons (cons selector (car history))
		   (cdr history)))))

(define (history/original-form history)
  (and history
       (caar (last-pair (car history)))))

;;;; Selectors
;;;  These are used by the expansion history to record subproblem
;;;  nesting so that debugging tools can show that nesting usefully.
;;;  By using abstract selectors, it is possible to locate the cell
;;;  that holds the pointer to a given subform.

(define (selector/apply selector object)
  (if (pair? selector)
      (selector/apply (cdr selector)
		      (if (>= (car selector) 0)
			  (list-ref object (car selector))
			  (list-tail object (- (car selector)))))
      object))

(define (selector/add-car selector)
  (if (and (pair? selector) (< (car selector) 0))
      (cons (- (car selector)) (cdr selector))
      (cons 0 selector)))

(define (selector/add-cdr selector)
  (if (and (pair? selector) (< (car selector) 0))
      (cons (- (car selector) 1) (cdr selector))
      (cons -1 selector)))

(define select-object '())
(define select-car (selector/add-car select-object))
(define select-cdr (selector/add-cdr select-object))
(define select-caar (selector/add-car select-car))
(define select-cadr (selector/add-car select-cdr))
(define select-cdar (selector/add-cdr select-car))
(define select-cddr (selector/add-cdr select-cdr))
(define select-caaar (selector/add-car select-caar))
(define select-caadr (selector/add-car select-cadr))
(define select-cadar (selector/add-car select-cdar))
(define select-caddr (selector/add-car select-cddr))
(define select-cdaar (selector/add-cdr select-caar))
(define select-cdadr (selector/add-cdr select-cadr))
(define select-cddar (selector/add-cdr select-cdar))
(define select-cdddr (selector/add-cdr select-cddr))
(define select-caaaar (selector/add-car select-caaar))
(define select-caaadr (selector/add-car select-caadr))
(define select-caadar (selector/add-car select-cadar))
(define select-caaddr (selector/add-car select-caddr))
(define select-cadaar (selector/add-car select-cdaar))
(define select-cadadr (selector/add-car select-cdadr))
(define select-caddar (selector/add-car select-cddar))
(define select-cadddr (selector/add-car select-cdddr))
(define select-cdaaar (selector/add-cdr select-caaar))
(define select-cdaadr (selector/add-cdr select-caadr))
(define select-cdadar (selector/add-cdr select-cadar))
(define select-cdaddr (selector/add-cdr select-caddr))
(define select-cddaar (selector/add-cdr select-cdaar))
(define select-cddadr (selector/add-cdr select-cdadr))
(define select-cdddar (selector/add-cdr select-cddar))
(define select-cddddr (selector/add-cdr select-cdddr))

(define (selector/add-cadr selector)
  (selector/add-car (selector/add-cdr selector)))

(define (selector/add-cddr selector)
  (selector/add-cdr (selector/add-cdr selector)))

(define (select-map procedure items selector)
  (let loop ((items items) (selector selector))
    (if (pair? items)
	(cons (procedure (car items) (selector/add-car selector))
	      (loop (cdr items) (selector/add-cdr selector)))
	'())))

(define (select-for-each procedure items selector)
  (let loop ((items items) (selector selector))
    (if (pair? items)
	(begin
	  (procedure (car items) (selector/add-car selector))
	  (loop (cdr items) (selector/add-cdr selector))))))

;;;; Utilities

(define (define-classifier keyword environment classifier)
  (syntactic-environment/define environment
				keyword
				(make-classifier-item classifier)))

(define (define-compiler keyword environment compiler)
  (syntactic-environment/define environment
				keyword
				(make-compiler-item compiler)))

(define (define-expander keyword environment expander)
  (syntactic-environment/define environment
				keyword
				(make-expander-item expander environment)))

(define (classifier->keyword classifier)
  (item->keyword (make-classifier-item classifier)))

(define (compiler->keyword compiler)
  (item->keyword (make-compiler-item compiler)))

(define (expander->keyword expander environment)
  (item->keyword (make-expander-item expander environment)))

(define (item->keyword item)
  (let ((environment
	 (make-internal-syntactic-environment null-syntactic-environment)))
    (syntactic-environment/define environment 'KEYWORD item)
    (close-syntax 'KEYWORD environment)))

(define (classifier->form classifier)
  `(,(classifier->keyword classifier)))

(define (compiler->form compiler)
  `(,(compiler->keyword compiler)))

(define (expander->form expander environment)
  `(,(expander->keyword expander environment)))

(define (capture-syntactic-environment expander)
  (classifier->form
   (lambda (form environment definition-environment history)
     form				;ignore
     (let ((form (expander environment)))
       (classify/form form
		      environment
		      definition-environment
		      (history/replace-reduction form environment history))))))

(define (capture-expansion-history expander)
  (classifier->form
   (lambda (form environment definition-environment history)
     form				;ignore
     (let ((form (expander history)))
       (classify/form form
		      environment
		      definition-environment
		      (history/replace-reduction form environment history))))))

(define (call-with-syntax-error-procedure expander)
  (capture-expansion-history
   (lambda (history)
     (expander
      (lambda rest
	(apply syntax-error history rest))))))

(define (flatten-body-items items)
  (append-map item->list items))

(define (item->list item)
  (if (body-item? item)
      (flatten-body-items (body-item/components item))
      (list item)))

(define (reverse-syntactic-environments environment procedure)
  (capture-syntactic-environment
   (lambda (closing-environment)
     (close-syntax (procedure closing-environment) environment))))