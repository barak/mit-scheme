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

;;;; SCode Optimizer: Free Variable Computation
;;; package: (scode-optimizer free)

(declare (usual-integrations)
	 (integrate-external "object"))

(declare (integrate-operator free/expression))

(define (free/expression expression)
  ((expression/method dispatch-vector expression) expression))

(define (free/expressions expressions)
  (fold-left (lambda (answer expression)
	       (lset-union eq? answer (free/expression expression)))
	     (no-free-variables)
	     expressions))

(define dispatch-vector
  (expression/make-dispatch-vector))

(define define-method/free
  (expression/make-method-definer dispatch-vector))

(define-method/free 'ACCESS
  (lambda (expression)
    (free/expression (access/environment expression))))

(define-method/free 'ASSIGNMENT
  (lambda (expression)
    (lset-adjoin eq?
		 (free/expression (assignment/value expression))
		 (assignment/variable expression))))

(define-method/free 'COMBINATION
  (lambda (expression)
    (lset-union eq?
		(free/expression (combination/operator expression))
		(free/expressions (combination/operands expression)))))

(define-method/free 'CONDITIONAL
  (lambda (expression)
    (lset-union eq?
		(free/expression (conditional/predicate expression))
		(free/expression (conditional/consequent expression))
		(free/expression (conditional/alternative expression)))))

(define-method/free 'CONSTANT
  (lambda (expression)
    expression
    (no-free-variables)))

(define-method/free 'DECLARATION
  (lambda (expression)
    (free/expression (declaration/expression expression))))

(define-method/free 'DELAY
  (lambda (expression)
    (free/expression (delay/expression expression))))

(define-method/free 'DISJUNCTION
  (lambda (expression)
    (lset-union eq?
		(free/expression (disjunction/predicate expression))
		(free/expression (disjunction/alternative expression)))))

(define-method/free 'OPEN-BLOCK
  (lambda (expression)
    (let ((omit (block/bound-variables (open-block/block expression))))
     (fold-left (lambda (variables action)
		  (if (eq? action open-block/value-marker)
		      variables
		      (lset-union eq? variables (lset-difference eq? (free/expression action) omit))))
		(lset-difference eq? (free/expressions (open-block/values expression)) omit)
		(open-block/actions expression)))))

(define-method/free 'PROCEDURE
  (lambda (expression)
    (lset-difference eq?
     (free/expression (procedure/body expression))
     (block/bound-variables (procedure/block expression)))))

(define-method/free 'QUOTATION
  (lambda (expression)
    (declare (ignore expression))
    (no-free-variables)))

(define-method/free 'REFERENCE
  (lambda (expression)
    (singleton-variable (reference/variable expression))))

(define-method/free 'SEQUENCE
  (lambda (expression)
    (free/expressions (sequence/actions expression))))

(define-method/free 'THE-ENVIRONMENT
  (lambda (expression)
    (declare (ignore expression))
    (no-free-variables)))

(define-integrable (no-free-variables)
  '())

(define-integrable (singleton-variable variable)
  (list variable))
