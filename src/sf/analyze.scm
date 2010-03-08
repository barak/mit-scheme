#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

;;;; SCode Optimizer: Expression analysis
;;; package: (scode-optimizer analyze)

(declare (usual-integrations)
	 (integrate-external "object"))

;;; EXPRESSION/FREE-VARIABLES
;;
;; Returns an EQ? LSET of the free variables in an expression.

(declare (integrate-operator expression/free-variables))

(define (expression/free-variables expression)
  ((expression/method free-variables-dispatch-vector expression) expression))

(define (expressions/free-variables expressions)
  (fold-left (lambda (answer expression)
	       (lset-union eq? answer (expression/free-variables expression)))
	     (no-free-variables)
	     expressions))

(define free-variables-dispatch-vector
  (expression/make-dispatch-vector))

(define define-method/free-variables
  (expression/make-method-definer free-variables-dispatch-vector))

(define-method/free-variables 'ACCESS
  (lambda (expression)
    (expression/free-variables (access/environment expression))))

(define-method/free-variables 'ASSIGNMENT
  (lambda (expression)
    (lset-adjoin eq?
		 (expression/free-variables (assignment/value expression))
		 (assignment/variable expression))))

(define-method/free-variables 'COMBINATION
  (lambda (expression)
    (lset-union eq?
		(expression/free-variables (combination/operator expression))
		(expressions/free-variables (combination/operands expression)))))

(define-method/free-variables 'CONDITIONAL
  (lambda (expression)
    (lset-union eq?
		(expression/free-variables (conditional/predicate expression))
		(if (expression/always-false? (conditional/predicate expression))
		    (no-free-variables)
		    (expression/free-variables (conditional/consequent expression)))
		(if (expression/never-false? (conditional/predicate expression))
		    (no-free-variables)
		    (expression/free-variables (conditional/alternative expression))))))

(define-method/free-variables 'CONSTANT
  (lambda (expression)
    expression
    (no-free-variables)))

(define-method/free-variables 'DECLARATION
  (lambda (expression)
    (expression/free-variables (declaration/expression expression))))

(define-method/free-variables 'DELAY
  (lambda (expression)
    (expression/free-variables (delay/expression expression))))

(define-method/free-variables 'DISJUNCTION
  (lambda (expression)
    (lset-union eq?
		(expression/free-variables (disjunction/predicate expression))
		(if (expression/never-false? (disjunction/predicate expression))
		    (no-free-variables)
		    (expression/free-variables (disjunction/alternative expression))))))

(define-method/free-variables 'OPEN-BLOCK
  (lambda (expression)
    (let ((omit (block/bound-variables (open-block/block expression))))
     (fold-left (lambda (variables action)
		  (if (eq? action open-block/value-marker)
		      variables
		      (lset-union eq? variables (lset-difference eq? (expression/free-variables action) omit))))
		(lset-difference eq? (expressions/free-variables (open-block/values expression)) omit)
		(open-block/actions expression)))))

(define-method/free-variables 'PROCEDURE
  (lambda (expression)
    (lset-difference eq?
     (expression/free-variables (procedure/body expression))
     (block/bound-variables (procedure/block expression)))))

(define-method/free-variables 'QUOTATION
  (lambda (expression)
    (declare (ignore expression))
    (no-free-variables)))

(define-method/free-variables 'REFERENCE
  (lambda (expression)
    (singleton-variable (reference/variable expression))))

(define-method/free-variables 'SEQUENCE
  (lambda (expression)
    (expressions/free-variables (sequence/actions expression))))

(define-method/free-variables 'THE-ENVIRONMENT
  (lambda (expression)
    (declare (ignore expression))
    (no-free-variables)))

(define-integrable (no-free-variables)
  '())

(define-integrable (singleton-variable variable)
  (list variable))

;;; EXPRESSION/FREE-VARIABLE? <expression> <variable>
;;
;; Test if a particular <variable> occurs free in <expression>.  Faster
;; and cheaper than collecting the entire free variable set and then
;; using memq.

(define (expression/free-variable? expression variable)
  ((expression/method is-free-dispatch-vector expression) expression variable))

(define (expressions/free-variable? expressions variable)
  (fold-left (lambda (answer expression)
	       (or answer
		   (expression/free-variable? expression variable)))
	     #f
	     expressions))

(define is-free-dispatch-vector
  (expression/make-dispatch-vector))

(define define-method/free-variable?
  (expression/make-method-definer is-free-dispatch-vector))

(define-method/free-variable? 'ACCESS
  (lambda (expression variable)
    (expression/free-variable? (access/environment expression) variable)))

(define-method/free-variable? 'ASSIGNMENT
  (lambda (expression variable)
    (or (eq? variable (assignment/variable expression))
	(expression/free-variable? (assignment/value expression) variable))))

(define-method/free-variable? 'COMBINATION
  (lambda (expression variable)
    (or (expression/free-variable? (combination/operator expression) variable)
	(expressions/free-variable? (combination/operands expression) variable))))

(define-method/free-variable? 'CONDITIONAL
  (lambda (expression variable)
    (or (expression/free-variable? (conditional/predicate expression) variable)
	(cond ((expression/always-false? (conditional/predicate expression))
	       (expression/free-variable? (conditional/alternative expression) variable))
	      ((expression/never-false? (conditional/predicate expression))
	       (expression/free-variable? (conditional/consequent expression) variable))
	      ((expression/free-variable? (conditional/consequent expression) variable))
	      (else (expression/free-variable? (conditional/alternative expression) variable))))))

(define-method/free-variable? 'CONSTANT false-procedure)

(define-method/free-variable? 'DECLARATION
  (lambda (expression variable)
    (expression/free-variable? (declaration/expression expression) variable)))

(define-method/free-variable? 'DELAY
  (lambda (expression variable)
    (expression/free-variable? (delay/expression expression) variable)))

(define-method/free-variable? 'DISJUNCTION
  (lambda (expression variable)
    (or (expression/free-variable? (disjunction/predicate expression) variable)
	(if (expression/never-false? (disjunction/predicate expression))
	    #f
	    (expression/free-variable? (disjunction/alternative expression) variable)))))

(define-method/free-variable? 'OPEN-BLOCK
  (lambda (expression variable)
    (fold-left (lambda (answer action)
		 (or answer
		     (if (eq? action open-block/value-marker)
			 #f
			 (expression/free-variable? action variable))))
	       #f
	       (open-block/actions expression))))

(define-method/free-variable? 'PROCEDURE
  (lambda (expression variable)
    (expression/free-variable? (procedure/body expression) variable)))

(define-method/free-variable? 'QUOTATION false-procedure)

(define-method/free-variable? 'REFERENCE
  (lambda (expression variable)
    (eq? (reference/variable expression) variable)))

(define-method/free-variable? 'SEQUENCE
  (lambda (expression variable)
  (fold-left (lambda (answer action)
	       (or answer
		   (if (eq? action open-block/value-marker)
		       #f
		       (expression/free-variable? action variable))))
	     #f
	     (sequence/actions expression))))

(define-method/free-variable? 'THE-ENVIRONMENT false-procedure)
