#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
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

;;;; SCode Optimizer: Copy Expression
;;; package: (scode-optimizer copy)

(declare (usual-integrations)
	 (integrate-external "object"))

(define root-block)
(define copy/variable/free)
(define copy/declarations)

(define (copy/expression/intern block expression)
  (guarantee-block block 'copy/expression/intern)
  (fluid-let ((root-block block)
	      (copy/variable/free copy/variable/free/intern)
	      (copy/declarations copy/declarations/intern))
    (copy/expression block (environment/make) expression)))

(define (copy/expression/extern block expression)
  (guarantee-block block 'copy/expression/extern)
  (fluid-let ((root-block block)
	      (copy/variable/free copy/variable/free/extern)
	      (copy/declarations copy/declarations/extern))
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

(define (environment/make)
  '())

(define (environment/bind environment variables values)
  (map* environment cons variables values))

(define (environment/lookup environment variable if-found if-not)
  (guarantee-variable variable 'environment/lookup)
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
    (guarantee-variable variable)
    (environment/lookup environment variable
      identity-procedure
      (lambda () (error "Variable missing during copy operation:" variable)))))

(define (copy/quotation quotation)
  (fluid-let ((root-block false))
    (let ((block (quotation/block quotation))
	  (environment (environment/make)))
      (quotation/make (quotation/scode quotation)
		      block
		      (copy/expression block
				       environment
				       (quotation/expression quotation))))))

(define (copy/block parent environment block)
  (let ((result (block/make parent (block/safe? block) '()))
	(old-bound (block/bound-variables block)))
    (let ((new-bound
	   (map (lambda (variable)
		  (let ((new
			 (variable/make&bind! result
					      (variable/name variable))))
		    (set-variable/flags! new
					 (list-copy (variable/flags variable)))
		    new))
		old-bound)))
      (let ((environment (environment/bind environment old-bound new-bound)))
	(set-block/declarations!
	 result
	 (copy/declarations block environment (block/declarations block)))
	(values result environment)))))

(define (copy/variable block environment variable)
  (declare (ignore block))
  (environment/lookup environment variable
    identity-procedure
    (lambda () (copy/variable/free variable))))

(define (copy/variable/free/intern variable)
  (let ((name (variable/name variable)))
    (let loop ((block root-block))
      (let ((variable* (block/lookup-name block name false)))
	(if (not variable*)
	    (error "Unable to find free variable during copy:" name))
	(if (eq? variable variable*)
	    variable
	    (begin
	      (if (not (block/parent block))
		  (error "Unable to find free variable during copy:" name))
	      (if (not (block/safe? (variable/block variable*)))
		  (error "Integration requires renaming unsafe variable:"
			 name))
	      (set-variable/name!
	       variable*
	       (string->uninterned-symbol (symbol->string name)))
	      (loop (block/parent block))))))))

(define (copy/variable/free/extern variable)
  (block/lookup-name root-block (variable/name variable) true))

(define (copy/declarations/intern block environment declarations)
  (declare (ignore block))
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
	      (block/lookup-name root-block (variable/name variable) true))))
	(lambda (expression)
	  (copy/expression block environment expression)))))

(define-method/copy 'ACCESS
  (lambda (block environment expression)
    (call-with-values
	(lambda ()
	  (copy/block block environment (access/block expression)))
      (lambda (block environment)
	(access/make (access/scode expression)
		     block
		     (copy/expression block
				      environment
				      (access/environment expression))
		     (access/name expression))))))

(define-method/copy 'ASSIGNMENT
  (lambda (block environment expression)
    (assignment/make
     (assignment/scode expression)
     block
     (copy/variable block environment (assignment/variable expression))
     (copy/expression block environment (assignment/value expression)))))

(define-method/copy 'COMBINATION
  (lambda (block environment expression)
    (combination/%make
     (combination/scode expression)
     block
     (copy/expression block environment (combination/operator expression))
     (copy/expressions block environment (combination/operands expression)))))

(define-method/copy 'CONDITIONAL
  (lambda (block environment expression)
    (conditional/make
     (conditional/scode expression)
     (copy/expression block environment (conditional/predicate expression))
     (copy/expression block environment (conditional/consequent expression))
     (copy/expression block environment (conditional/alternative expression)))))

(define-method/copy 'CONSTANT
  (lambda (block environment expression)
    (declare (ignore block environment))
    expression))

(define-method/copy 'DECLARATION
  (lambda (block environment expression)
    (declaration/make
     (declaration/scode expression)
     (copy/declarations block
			environment
			(declaration/declarations expression))
     (copy/expression block environment (declaration/expression expression)))))

(define-method/copy 'DELAY
  (lambda (block environment expression)
    (delay/make
     (delay/scode expression)
     (copy/expression block environment (delay/expression expression)))))

(define-method/copy 'DISJUNCTION
  (lambda (block environment expression)
    (disjunction/make
     (disjunction/scode expression)
     (copy/expression block environment (disjunction/predicate expression))
     (copy/expression block
		      environment
		      (disjunction/alternative expression)))))

(define-method/copy 'PROCEDURE
  (lambda (block environment procedure)
    (call-with-values
	(lambda ()
	  (copy/block block environment (procedure/block procedure)))
      (lambda (block environment)
	(let ((rename (make-renamer environment)))
	  (procedure/make (procedure/scode procedure)
			  block
			  (procedure/name procedure)
			  (map rename (procedure/required procedure))
			  (map rename (procedure/optional procedure))
			  (let ((rest (procedure/rest procedure)))
			    (and rest
				 (rename rest)))
			  (copy/expression block
					   environment
					   (procedure/body procedure))))))))

(define-method/copy 'OPEN-BLOCK
  (lambda (block environment expression)
    (call-with-values
	(lambda ()
	  (copy/block block environment (open-block/block expression)))
      (lambda (block environment)
	(open-block/make
	 (open-block/scode expression)
	 block
	 (map (make-renamer environment) (open-block/variables expression))
	 (copy/expressions block environment (open-block/values expression))
	 (map (lambda (action)
		(if (eq? action open-block/value-marker)
		    action
		    (copy/expression block environment action)))
	      (open-block/actions expression)))))))

(define-method/copy 'QUOTATION
  (lambda (block environment expression)
    (declare (ignore block environment))
    (copy/quotation expression)))

(define-method/copy 'REFERENCE
  (lambda (block environment expression)
    (reference/make (reference/scode expression)
		    block
		    (copy/variable block environment
				   (reference/variable expression)))))

(define-method/copy 'SEQUENCE
  (lambda (block environment expression)
    (sequence/make
     (sequence/scode expression)
     (copy/expressions block environment (sequence/actions expression)))))

(define-method/copy 'THE-ENVIRONMENT
  (lambda (block environment expression)
    (declare (ignore block environment expression))
    (error "Attempt to integrate expression containing (THE-ENVIRONMENT)")))