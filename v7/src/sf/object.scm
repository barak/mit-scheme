#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/sf/object.scm,v 4.2 1989/04/18 16:32:34 cph Rel $

Copyright (c) 1987, 1989 Massachusetts Institute of Technology

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

(declare (usual-integrations)
	 (automagic-integrations)
	 (open-block-optimizations))

(let-syntax
    ((define-enumerand
       (macro (name enumeration)
	 `(DEFINE ,(symbol-append name '/ENUMERAND)
	    (ENUMERATION/NAME->ENUMERAND
	     ,(symbol-append 'ENUMERATION/ enumeration)
	     ',name))))
     (define-simple-type
       (macro (name enumeration slots)
	 `(BEGIN
	    (DEFINE-ENUMERAND ,name ,enumeration)
	    (DEFINE-STRUCTURE (,name
			       (NAMED ,(symbol-append name '/ENUMERAND))
			       (CONC-NAME ,(symbol-append name '/))
			       (CONSTRUCTOR ,(symbol-append name '/MAKE)))
	      ,@slots)))))

(define-integrable (object/enumerand object)
  (vector-ref object 0))

(define-integrable (set-object/enumerand! object enumerand)
  (vector-set! object 0 enumerand))

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
	   (error "Unknown enumeration name" name))))

(define (enumeration/name->index enumeration name)
  (enumerand/index (enumeration/name->enumerand enumeration name)))

;;;; Random Types

(define enumeration/random
  (enumeration/make
   '(BLOCK
     DELAYED-INTEGRATION
     VARIABLE
     )))

(define-enumerand block random)
(define-structure (block (named block/enumerand)
			 (conc-name block/)
			 (constructor %block/make))
  parent
  children
  safe?
  declarations
  bound-variables
  flags)

(define (block/make parent safe?)
  (let ((block
	 (%block/make parent '() safe? (declarations/make-null) '() '())))
    (if parent
	(set-block/children! parent (cons block (block/children parent))))
    block))

(define-enumerand delayed-integration random)
(define-structure (delayed-integration
		   (named delayed-integration/enumerand)
		   (conc-name delayed-integration/)
		   (constructor delayed-integration/make (operations value)))
  (state 'NOT-INTEGRATED)
  (environment false)
  operations
  value)

(define-simple-type variable random
  (block name flags))

(define (variable/make&bind! block name)
  (let ((variable (variable/make block name '())))
    (set-block/bound-variables! block
				(cons variable
				      (block/bound-variables block)))
    variable))

(define-integrable (variable/flag? variable flag)
  (memq flag (variable/flags variable)))

(define (set-variable/flag! variable flag)
  (if (not (variable/flag? variable flag))
      (set-variable/flags! variable
			   (cons flag (variable/flags variable)))))

(let-syntax ((define-flag
	       (macro (name tester setter)
		 `(BEGIN
		    (DEFINE (,tester VARIABLE)
		      (VARIABLE/FLAG? VARIABLE (QUOTE ,name)))
		    (DEFINE (,setter VARIABLE)
		      (SET-VARIABLE/FLAG! VARIABLE (QUOTE ,name)))))))

  (define-flag SIDE-EFFECTED variable/side-effected variable/side-effect!)
  (define-flag REFERENCED    variable/referenced    variable/reference!)
  (define-flag INTEGRATED    variable/integrated    variable/integrated!)
  (define-flag CAN-IGNORE    variable/can-ignore?   variable/can-ignore!)
  )

(define open-block/value-marker
  ;; This must be an interned object because we will fasdump it and
  ;; fasload it back in.
  (intern "#[(scode-optimizer)open-block/value-marker]"))

;;;; Expression Types

(define enumeration/expression
  (enumeration/make
   '(ACCESS
     ASSIGNMENT
     COMBINATION
     CONDITIONAL
     CONSTANT
     DECLARATION
     DELAY
     DISJUNCTION
     IN-PACKAGE
     OPEN-BLOCK
     PROCEDURE
     QUOTATION
     REFERENCE
     SEQUENCE
     THE-ENVIRONMENT
     )))

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

(define-simple-type access expression (environment name))
(define-simple-type assignment expression (block variable value))
(define-simple-type combination expression (operator operands))
(define-simple-type conditional expression (predicate consequent alternative))
(define-simple-type constant expression (value))
(define-simple-type declaration expression (declarations expression))
(define-simple-type delay expression (expression))
(define-simple-type disjunction expression (predicate alternative))
(define-simple-type in-package expression (environment quotation))
(define-simple-type open-block expression (block variables values actions
						 optimized))
(define-simple-type procedure expression
  (block name required optional rest body))
(define-simple-type quotation expression (block expression))
(define-simple-type reference expression (block variable))
(define-simple-type sequence expression (actions))
(define-simple-type the-environment expression (block))

;;; end LET-SYNTAX
)

(define-integrable (constant->integration-info constant)
  (make-integration-info (constant/make constant) '()))

(define-integrable (make-integration-info expression uninterned-variables)
  (cons expression uninterned-variables))

(define-integrable (integration-info/expression integration-info)
  (car integration-info))

(define-integrable (integration-info/uninterned-variables integration-info)
  (cdr integration-info))