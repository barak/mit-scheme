#| -*-Scheme-*-

$Id: object.scm,v 4.8 1997/07/15 18:22:21 adams Exp $

Copyright (c) 1987-1997 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; SCode Optimizer: Data Types
;;; package: (scode-optimizer)

(declare (usual-integrations))

;;;; Enumerations

(define (enumeration/make names)
  (let ((enumerands 
	 (let loop ((names names) (index 0))
	   (if (null? names)
	       '()
	       (cons (vector false (car names) index)
		     (loop (cdr names) (1+ index)))))))
    (let ((enumeration
	   (cons (list->vector enumerands)
		 (map (lambda (enumerand)
			(cons (enumerand/name enumerand) enumerand))
		      enumerands))))
      (for-each (lambda (enumerand)
		  (vector-set! enumerand 0 enumeration))
		enumerands)
      enumeration)))

(define-structure (enumerand (type vector)
			     (conc-name enumerand/))
  (enumeration false read-only true)
  (name false read-only true)
  (index false read-only true))

(define-integrable (enumeration/cardinality enumeration)
  (vector-length (car enumeration)))

(define-integrable (enumeration/index->enumerand enumeration index)
  (vector-ref (car enumeration) index))

(define (enumeration/name->enumerand enumeration name)
  (cdr (or (assq name (cdr enumeration))
	   (error "Unknown enumeration name:" name))))

(define (enumeration/name->index enumeration name)
  (enumerand/index (enumeration/name->enumerand enumeration name)))

(let-syntax
    ((define-enumeration
       (macro (enumeration-name enumerand-names)
	 `(BEGIN
	    (DEFINE ,enumeration-name
	      (ENUMERATION/MAKE ',enumerand-names))
	    ,@(map (lambda (enumerand-name)
		     `(DEFINE ,(symbol-append enumerand-name '/ENUMERAND)
			(ENUMERATION/NAME->ENUMERAND ,enumeration-name
						     ',enumerand-name)))
		   enumerand-names)))))
  (define-enumeration enumeration/random
    (block
     delayed-integration
     variable))
  (define-enumeration enumeration/expression
    (access
     assignment
     combination
     conditional
     constant
     declaration
     delay
     disjunction
     in-package
     open-block
     procedure
     quotation
     reference
     sequence
     the-environment)))

;;;; Records

(define-structure (block (type vector)
			 (named block/enumerand)
			 (conc-name block/)
			 (constructor %block/make
				      (parent safe? bound-variables)))
  parent
  (children '())
  safe?
  (declarations (declarations/make-null))
  bound-variables
  (flags '()))

(define-structure (delayed-integration
		   (type vector)
		   (named delayed-integration/enumerand)
		   (conc-name delayed-integration/)
		   (constructor delayed-integration/make (operations value)))
  (state 'NOT-INTEGRATED)
  (environment false)
  operations
  value)

(let-syntax
    ((define-simple-type
       (macro (name slots #!optional scode?)
	 `(DEFINE-STRUCTURE (,name (TYPE VECTOR)
				   (NAMED ,(symbol-append name '/ENUMERAND))
				   (CONC-NAME ,(symbol-append name '/))
				   (CONSTRUCTOR ,(symbol-append name '/MAKE)))
	    ,@(if (or (default-object? scode?) scode?)
		  `((scode false read-only true))
		  `())
	    ,@slots))))
  (define-simple-type variable (block name flags) #F)
  (define-simple-type access (environment name))
  (define-simple-type assignment (block variable value))
  (define-simple-type combination (block operator operands))
  (define-simple-type conditional (predicate consequent alternative))
  (define-simple-type constant (value))
  (define-simple-type declaration (declarations expression))
  (define-simple-type delay (expression))
  (define-simple-type disjunction (predicate alternative))
  (define-simple-type in-package (environment quotation))
  (define-simple-type open-block (block variables values actions optimized))
  (define-simple-type procedure (block name required optional rest body))
  (define-simple-type quotation (block expression))
  (define-simple-type reference (block variable))
  (define-simple-type sequence (actions))
  (define-simple-type the-environment (block)))

;; Abstraction violations

(define-integrable (object/enumerand object)
  (vector-ref object 0))

(define-integrable (set-object/enumerand! object enumerand)
  (vector-set! object 0 enumerand))

(define-integrable (object/scode object)
  (vector-ref object 1))

(define (with-new-scode scode object)
  (let ((new (vector-copy object)))
    (vector-set! new 1 scode)
    new))

;;;; Miscellany

(let-syntax
    ((define-flag
       (macro (name tester setter)
	 `(BEGIN
	    (DEFINE (,tester VARIABLE)
	      (MEMQ ',name (VARIABLE/FLAGS VARIABLE)))
	    (DEFINE (,setter VARIABLE)
	      (IF (NOT (MEMQ ',name (VARIABLE/FLAGS VARIABLE)))
		  (SET-VARIABLE/FLAGS! VARIABLE
				       (CONS ',name
					     (VARIABLE/FLAGS VARIABLE)))))))))
  (define-flag SIDE-EFFECTED variable/side-effected variable/side-effect!)
  (define-flag REFERENCED    variable/referenced    variable/reference!)
  (define-flag INTEGRATED    variable/integrated    variable/integrated!)
  (define-flag CAN-IGNORE    variable/can-ignore?   variable/can-ignore!))

(define open-block/value-marker
  ;; This must be an interned object because we will fasdump it and
  ;; fasload it back in.
  (intern "#[(scode-optimizer)open-block/value-marker]"))

(define (expression/make-dispatch-vector)
  (make-vector (enumeration/cardinality enumeration/expression)))

(define (expression/make-method-definer dispatch-vector)
  (lambda (type-name method)
    (vector-set! dispatch-vector
		 (enumeration/name->index enumeration/expression type-name)
		 method)))

(define-integrable (expression/method dispatch-vector expression)
  (vector-ref dispatch-vector (enumerand/index (object/enumerand expression))))

(define-integrable (name->method dispatch-vector name)
  ;; Useful for debugging
  (vector-ref dispatch-vector
	      (enumeration/name->index enumeration/expression name)))

(define-integrable (global-ref/make name)
  (access/make false
	       (constant/make false system-global-environment)
	       name))

(define (global-ref? object)
  (and (access? object)
       (constant? (access/environment object))
       (eq? system-global-environment
	    (constant/value (access/environment object)))
       (access/name object)))

(define-integrable (constant->integration-info constant)
  (make-integration-info (constant/make false constant)))

(define-integrable (integration-info? object)
  (and (pair? object)
       (eq? integration-info-tag (car object))))

(define-integrable (make-integration-info expression)
  (cons integration-info-tag expression))

(define-integrable (integration-info/expression integration-info)
  (cdr integration-info))

(define integration-info-tag
  (string-copy "integration-info"))