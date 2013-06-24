#| -*-Scheme-*-

$Id: free.scm,v 4.4 2001/12/20 16:28:23 cph Exp $

Copyright (c) 1988, 1993, 1999, 2001 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
|#

;;;; SCode Optimizer: Free Variable Analysis

(declare (usual-integrations)
	 (integrate-external "object" "lsets"))

(declare (integrate-operator no-free-variables singleton-variable
			     list->variable-set))

(define (no-free-variables) 
  (empty-set variable? eq?))

(define (singleton-variable variable) 
  (singleton-set variable? eq? variable))

(define (list->variable-set variable-list)
  (list->set variable? eq? variable-list))

(define (free/expressions expressions)
  (if (null? expressions)
      (no-free-variables)
      (set/union (free/expression (car expressions))
		 (free/expressions (cdr expressions)))))

(declare (integrate-operator free/expression))

(define (free/expression expression)
  ((expression/method dispatch-vector expression) expression))

(define dispatch-vector
  (expression/make-dispatch-vector))

(define define-method/free
  (expression/make-method-definer dispatch-vector))

(define-method/free 'ACCESS
  (lambda (expression)
    (free/expression (access/environment expression))))

(define-method/free 'ASSIGNMENT
  (lambda (expression)
    (set/adjoin (free/expression (assignment/value expression))
		(assignment/variable expression))))

(define-method/free 'COMBINATION
  (lambda (expression)
    (set/union (free/expression (combination/operator expression))
	       (free/expressions (combination/operands expression)))))

(define-method/free 'CONDITIONAL
  (lambda (expression)
    (set/union*
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
    (set/union (free/expression (disjunction/predicate expression))
	       (free/expression (disjunction/alternative expression)))))

(define-method/free 'PROCEDURE
  (lambda (expression)
    (set/difference
     (free/expression (procedure/body expression))
     (list->variable-set
      (block/bound-variables-list (procedure/block expression))))))

(define-method/free 'OPEN-BLOCK
  (lambda (expression)
    (set/difference
     (set/union (free/expressions (open-block/values expression))
		(let loop ((actions (open-block/actions expression)))
		  (cond ((null? actions) (no-free-variables))
			((eq? (car actions) open-block/value-marker)
			 (loop (cdr actions)))
			(else
			 (set/union (free/expression (car actions))
				    (loop (cdr actions)))))))
     (list->variable-set 
      (block/bound-variables-list (open-block/block expression))))))

(define-method/free 'QUOTATION
  (lambda (expression) 
    expression
    (no-free-variables)))

(define-method/free 'REFERENCE
  (lambda (expression) 
    (singleton-variable (reference/variable expression))))

(define-method/free 'SEQUENCE
  (lambda (expression)
    (free/expressions (sequence/actions expression))))

(define-method/free 'THE-ENVIRONMENT
  (lambda (expression) 
    expression
    (no-free-variables)))