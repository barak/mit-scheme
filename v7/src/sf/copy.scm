#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/sf/copy.scm,v 3.0 1987/03/10 13:24:44 cph Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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

(declare (usual-integrations))

(define (copy/external block expression)
  (fluid-let ((root-block block))
    (copy/expression block (environment/make) expression)))

(define (copy/expressions block environment expressions)
  (map (lambda (expression)
	 (copy/expression block environment expression))
       expressions))

(define (copy/expression block environment expression)
  ((expression/method dispatch-vector expression)
   block environment expression))

(define dispatch-vector
  (expression/make-dispatch-vector))

(define define-method/copy
  (expression/make-method-definer dispatch-vector))

(define (copy/quotation quotation)
  (fluid-let ((root-block false))
    (let ((block (quotation/block quotation)))
      (quotation/make block
		      (copy/expression block
				       (environment/make)
				       (quotation/expression quotation))))))

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
    (combination/make
     (copy/expression block environment (combination/operator expression))
     (copy/expressions block environment (combination/operands expression)))))

(define-method/copy 'CONDITIONAL
  (lambda (block environment expression)
    (conditional/make
     (copy/expression block environment (conditional/predicate expression))
     (copy/expression block environment (conditional/consequent expression))
     (copy/expression block environment
		      (conditional/alternative expression)))))

(define-method/copy 'CONSTANT
  (lambda (block environment expression)
    expression))

(define-method/copy 'DECLARATION
  (lambda (block environment expression)
    (declaration/make
     (copy/declarations environment (declaration/declarations expression))
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
    (transmit-values (copy/block block environment (procedure/block procedure))
      (lambda (block environment)
	(let ((rename (make-renamer environment)))
	  (procedure/make block
			  (rename (procedure/name procedure))
			  (map rename (procedure/required procedure))
			  (map rename (procedure/optional procedure))
			  (let ((rest (procedure/rest procedure)))
			    (and rest (rename rest)))
			  (copy/expression block
					   environment
					   (procedure/body procedure))))))))

(define-method/copy 'OPEN-BLOCK
  (lambda (block environment expression)
    (transmit-values
	(copy/block block environment (open-block/block expression))
      (lambda (block environment)
	(open-block/make block
			 (map (make-renamer environment)
			      (open-block/variables expression))
			 (copy/expressions block
					   environment
					   (open-block/values expression))
			 (map (lambda (action)
				(if (eq? action open-block/value-marker)
				    action
				    (copy/expression block
						     environment
						     action)))
			      (open-block/actions expression)))))))

(define-method/copy 'QUOTATION
  (lambda (block environment expression)
    (copy/quotation expression)))

(define-method/copy 'REFERENCE
  (lambda (block environment expression)
    (reference/make block
		    (copy/variable block
				   environment
				   (reference/variable expression)))))

(define-method/copy 'SEQUENCE
  (lambda (block environment expression)
    (sequence/make
     (copy/expressions block environment (sequence/actions expression)))))

(define-method/copy 'THE-ENVIRONMENT
  (lambda (block environment expression)
    (error "Attempt to integrate expression containing (THE-ENVIRONMENT)")))

(define (copy/block parent environment block)
  (let ((result (block/make parent (block/safe? block)))
	(old-bound (block/bound-variables block)))
    (let ((new-bound
	   (map (lambda (variable)
		  (variable/make result (variable/name variable)))
		old-bound)))
      (let ((environment (environment/bind environment old-bound new-bound)))
	(block/set-bound-variables! result new-bound)
	(block/set-declarations!
	 result
	 (copy/declarations environment (block/declarations block)))
	(return-2 result environment)))))

(define (copy/declarations environment declarations)
  (if (null? declarations)
      '()
      (declarations/rename declarations
	(lambda (variable)
	  (environment/lookup environment variable
	    identity-procedure
	    (lambda () variable))))))

(define root-block)

(define (copy/variable block environment variable)
  (environment/lookup environment variable
    identity-procedure
    (lambda ()
      (for-each rename-variable!
		(let ((name (variable/name variable)))
		  (let loop ((block root-block))
		    (let ((variable*
			   (variable/assoc name
					   (block/bound-variables block))))
		      (cond ((not variable*) (loop (block/parent block)))
			    ((eq? variable variable*) '())
			    (else
			     (cons variable* (loop (block/parent block)))))))))
      variable)))

(define (rename-variable! variable)
  (if (block/safe? (variable/block variable))
      (variable/set-name! variable (rename (variable/name variable)))
      (error "Integration requires renaming unsafe variable" variable)))

(define (rename name)
  (string->uninterned-symbol (symbol->string name)))

(define (environment/make)
  '())

(define (environment/bind environment variables values)
  (map* environment cons variables values))

(define (environment/lookup environment variable if-found if-not)
  (let ((association (assq variable environment)))
    (if association
	(if-found (cdr association))
	(if-not))))

(define (make-renamer environment)
  (lambda (variable)
    (environment/lookup environment variable
      identity-procedure
    (error "Attempt to integrate expression containing (THE-ENVIRONMENT)")))