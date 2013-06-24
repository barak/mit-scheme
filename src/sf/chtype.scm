#| -*-Scheme-*-

$Id: chtype.scm,v 4.6 2003/02/14 18:28:34 cph Exp $

Copyright (c) 1988, 1993, 1999, 2001 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; SCode Optimizer: Intern object types

(declare (usual-integrations)
	 (integrate-external "object"))

(define (change-type/block block)
  (change-type/object enumeration/random block)
  (block/for-each-bound-variable block
    (lambda (variable)
      (change-type/object enumeration/random variable)))
  (for-each change-type/block (block/children block)))

(define (change-type/expressions expressions)
  (for-each change-type/expression expressions))

(declare (integrate-operator change-type/expression))

(define (change-type/expression expression)
  (change-type/object enumeration/expression expression)
  ((expression/method dispatch-vector expression) expression))

(define dispatch-vector
  (expression/make-dispatch-vector))

(define define-method/change-type
  (expression/make-method-definer dispatch-vector))

(declare (integrate-operator change-type/object))

(define (change-type/object enumeration object)
  (set-object/enumerand!
   object
   (enumeration/name->enumerand enumeration
				(enumerand/name (object/enumerand object)))))

(define-method/change-type 'ACCESS
  (lambda (expression)
    (change-type/expression (access/environment expression))))

(define-method/change-type 'ASSIGNMENT
  (lambda (expression)
    (change-type/expression (assignment/value expression))))

(define-method/change-type 'COMBINATION
  (lambda (expression)
    (change-type/expression (combination/operator expression))
    (change-type/expressions (combination/operands expression))))

(define-method/change-type 'CONDITIONAL
  (lambda (expression)
    (change-type/expression (conditional/predicate expression))
    (change-type/expression (conditional/consequent expression))
    (change-type/expression (conditional/alternative expression))))

(define-method/change-type 'CONSTANT
  (lambda (expression)
    expression ; ignored
    'DONE))

(define-method/change-type 'DECLARATION
  (lambda (expression)
    (change-type/expression (declaration/expression expression))))

(define-method/change-type 'DELAY
  (lambda (expression)
    (change-type/expression (delay/expression expression))))

(define-method/change-type 'DISJUNCTION
  (lambda (expression)
    (change-type/expression (disjunction/predicate expression))
    (change-type/expression (disjunction/alternative expression))))

(define-method/change-type 'PROCEDURE
  (lambda (expression)
    (change-type/expression (procedure/body expression))))

(define-method/change-type 'OPEN-BLOCK
  (lambda (expression)
    (change-type/expressions (open-block/values expression))
    (change-type/open-block-actions (open-block/actions expression))))

(define (change-type/open-block-actions actions)
  (cond ((null? actions) 'DONE)
	((eq? (car actions) open-block/value-marker)
	 (change-type/open-block-actions (cdr actions)))
	(else (change-type/expression (car actions))
	      (change-type/open-block-actions (cdr actions)))))

(define-method/change-type 'QUOTATION
  (lambda (expression)
    (change-type/quotation expression)))

(define (change-type/quotation quotation)
  (change-type/expression (quotation/expression quotation)))

(define-method/change-type 'REFERENCE
  (lambda (expression)
    expression ; ignored
    'DONE))

(define-method/change-type 'SEQUENCE
  (lambda (expression)
    (change-type/expressions (sequence/actions expression))))

(define-method/change-type 'THE-ENVIRONMENT
  (lambda (expression)
    expression ; ignored
    'DONE))