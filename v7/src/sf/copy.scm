#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/sf/copy.scm,v 4.1 1988/06/13 12:29:14 cph Rel $

Copyright (c) 1988 Massachusetts Institute of Technology

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

;;;; SCode Optimizer: Copy Expression

(declare (usual-integrations)
	 (open-block-optimizations)
	 (eta-substitution)
	 (automagic-integrations)
	 (integrate-external "object"))

(define root-block)

(define (copy/expression/intern block expression uninterned)
  (fluid-let ((root-block block)
	      (copy/variable/free copy/variable/free/intern)
	      (copy/declarations copy/declarations/intern))
    (let ((environment
	   (environment/rebind block (environment/make) uninterned)))
      (copy/expression root-block
		       environment
		       expression))))

(define (copy/expression/extern expression)
  (fluid-let ((root-block (block/make false false))
	      (copy/variable/free copy/variable/free/extern)
	      (copy/declarations copy/declarations/extern))
    (let ((environment (environment/make)))
      (let ((expression
	     (copy/expression root-block environment expression)))
	(values root-block expression)))))

(define (copy/expressions block environment expressions)
  (map (lambda (expression)
	 (copy/expression block environment expression))
       expressions))

(declare (integrate-operator copy/expression))

(define (copy/expression block environment expression)
  ((expression/method dispatch-vector expression)
   block environment expression))

(define dispatch-vector
  (expression/make-dispatch-vector))

(define define-method/copy
  (expression/make-method-definer dispatch-vector))

(define (copy/quotation quotation)
  (fluid-let ((root-block false))
    (let ((block (quotation/block quotation))
	  (environment (environment/make)))
      (quotation/make block
		      (copy/expression block
				       environment
				       (quotation/expression quotation))))))

(define (copy/block parent environment block)
  (let ((result (block/make parent (block/safe? block)))
	(old-bound (block/bound-variables block)))
    (let ((new-bound
	   (map (lambda (variable)
		  (variable/make result
				 (variable/name variable)
				 (variable/flags variable)))
		old-bound)))
      (let ((environment (environment/bind environment old-bound new-bound)))
	(set-block/bound-variables! result new-bound)
	(set-block/declarations!
	 result
	 (copy/declarations block environment (block/declarations block)))
	(set-block/flags! result (block/flags block))
	(values result environment)))))

(define copy/variable/free)

(define (copy/variable block environment variable)
  block					;ignored
  (environment/lookup environment variable
    identity-procedure
    (copy/variable/free variable)))

(define (copy/variable/free/intern variable)
  (lambda ()
    (let ((name (variable/name variable)))
      (let loop ((block root-block))
	(let ((variable* (variable/assoc name (block/bound-variables block))))
	  (cond ((eq? variable variable*)
		 variable)
		((not (block/parent block))
		 (error "Unable to find free variable during copy" name))
		((not variable*)
		 (loop (block/parent block)))
		((block/safe? (variable/block variable*))
		 (set-variable/name! variable* (rename-symbol name))
		 (loop (block/parent block)))
		(else
		 (error "Integration requires renaming unsafe variable"
			name))))))))

(define (rename-symbol symbol)
  (string->uninterned-symbol (symbol->string symbol)))

(define (copy/variable/free/extern variable)
  (lambda ()
    (block/lookup-name root-block (variable/name variable) true)))

(define copy/declarations)

(define (copy/declarations/intern block environment declarations)
  block ; ignored
  (if (null? declarations)
      '()
      (declarations/map declarations
	(lambda (variable)
	  (environment/lookup environment variable
	    identity-procedure
	    (lambda () variable)))
	identity-procedure)))

(define (copy/declarations/extern block environment declarations)
  (if (null? declarations)
      '()
      (declarations/map declarations
	(lambda (variable)
	  (environment/lookup environment variable
	    identity-procedure
	    (lambda ()
	      (block/lookup-name root-block
				 (variable/name variable) true))))
	(lambda (expression)
	  (copy/expression block environment expression)))))

(define (environment/make)
  '())

(define (environment/bind environment variables values)
  (map* environment cons variables values))

(define (environment/lookup environment variable if-found if-not)
  (let ((association (assq variable environment)))
    (if association
	(if-found (cdr association))
	(if-not))))

(define (environment/rebind block environment variables)
  (environment/bind
   environment
   variables
   (map (lambda (variable)
	  (block/lookup-name block (variable/name variable) true))
	variables)))

(define (make-renamer environment)
  (lambda (variable)
    (environment/lookup environment variable
      identity-procedure
      (lambda () (error "Missing variable during copy operation" variable)))))

(define-method/copy 'ACCESS
  (lambda (block environment expression)
    (access/make (copy/expression block environment
				  (access/environment expression))
		 (access/name expression))))

(define-method/copy 'ASSIGNMENT
  (lambda (block environment expression)
    (assignment/make
     block
     (copy/variable block environment (assignment/variable expression))
     (copy/expression block environment (assignment/value expression)))))

(define-method/copy 'COMBINATION
  (lambda (block environment expression)
    (let ((operator (combination/operator expression))
	  (operands (combination/operands expression)))
      (if (and (operator/error-procedure? operator)
	       (the-environment? (caddr operands)))
	  (combination/make
	   operator
	   (list (copy/expression block environment (car operands))
		 (copy/expression block environment (cadr operands))
		 (the-environment/make block)))
	  (combination/make
	   (copy/expression block environment operator)
	   (copy/expressions block environment operands))))))

(define (operator/error-procedure? operator)
  (or (and (constant? operator)
	   (eq? error-procedure (constant/value operator)))
      (and (access? operator)
	   (eq? 'ERROR-PROCEDURE (access/name operator))
	   (let ((environment (access/environment operator)))
	     (and (constant? environment)
		  (not (constant/value environment)))))))

(define-method/copy 'CONDITIONAL
  (lambda (block environment expression)
    (conditional/make
     (copy/expression block environment (conditional/predicate expression))
     (copy/expression block environment (conditional/consequent expression))
     (copy/expression block environment
		      (conditional/alternative expression)))))

(define-method/copy 'CONSTANT
  (lambda (block environment expression)
    block environment ; ignored
    expression))

(define-method/copy 'DECLARATION
  (lambda (block environment expression)
    (declaration/make
     (copy/declarations block environment
			(declaration/declarations expression))
     (copy/expression block environment (declaration/expression expression)))))

(define-method/copy 'DELAY
  (lambda (block environment expression)
    (delay/make
     (copy/expression block environment (delay/expression expression)))))

(define-method/copy 'DISJUNCTION
  (lambda (block environment expression)
    (disjunction/make
     (copy/expression block environment (disjunction/predicate expression))
     (copy/expression block environment
		      (disjunction/alternative expression)))))

(define-method/copy 'IN-PACKAGE
  (lambda (block environment expression)
    (in-package/make
     (copy/expression block environment (in-package/environment expression))
     (copy/quotation (in-package/quotation expression)))))

(define-method/copy 'PROCEDURE
  (lambda (block environment procedure)
    (with-values
	(lambda ()
	  (copy/block block environment (procedure/block procedure)))
      (lambda (block environment)
	(let ((rename (make-renamer environment)))
	  (procedure/make block
			  (procedure/name procedure)
			  (map rename (procedure/required procedure))
			  (map rename (procedure/optional procedure))
			  (let ((rest (procedure/rest procedure)))
			    (and rest (rename rest)))
			  (copy/expression block environment
					   (procedure/body procedure))))))))

(define-method/copy 'OPEN-BLOCK
  (lambda (block environment expression)
    (with-values
	(lambda ()
	  (copy/block block environment (open-block/block expression)))
      (lambda (block environment)
	(open-block/make
	 block
	 (map (make-renamer environment) (open-block/variables expression))
	 (copy/expressions block environment (open-block/values expression))
	 (map (lambda (action)
		(if (eq? action open-block/value-marker)
		    action
		    (copy/expression block environment action)))
	      (open-block/actions expression))
	 (open-block/optimized expression))))))

(define-method/copy 'QUOTATION
  (lambda (block environment expression)
    block environment ; ignored
    (copy/quotation expression)))

(define-method/copy 'REFERENCE
  (lambda (block environment expression)
    (reference/make block
		    (copy/variable block environment
				   (reference/variable expression)))))

(define-method/copy 'SEQUENCE
  (lambda (block environment expression)
    (sequence/make
     (copy/expressions block environment (sequence/actions expression)))))

(define-method/copy 'THE-ENVIRONMENT
  (lambda (block environment expression)
    block environment expression ; ignored
    (error "Attempt to integrate expression containing (THE-ENVIRONMENT)")))