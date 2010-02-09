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

;;;; SCode Optimizer: Intern object types
;;; package: (scode-optimizer change-type)

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
  false-procedure)

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

(define-method/change-type 'OPEN-BLOCK
  (lambda (expression)
    (change-type/expressions (open-block/values expression))
    (for-each (lambda (action)
		(if (not (eq? action open-block/value-marker))
		    (change-type/expression action)))
	      (open-block/actions expression))))

(define-method/change-type 'PROCEDURE
  (lambda (expression)
    (change-type/expression (procedure/body expression))))

(define-method/change-type 'QUOTATION
  (lambda (expression)
    (change-type/expression (quotation/expression expression))))

(define-method/change-type 'REFERENCE
  false-procedure)

(define-method/change-type 'SEQUENCE
  (lambda (expression)
    (change-type/expressions (sequence/actions expression))))

(define-method/change-type 'THE-ENVIRONMENT
  false-procedure)