#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/sf/chtype.scm,v 1.2 1988/03/22 17:35:34 jrm Rel $

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

;;;; SCode Optimizer: Intern object types

(declare (usual-integrations))
(declare (automagic-integrations))

(define (change-type/external block expression)
  (change-type/block block)
  (change-type/expression expression)
  (return-2 expression (block/bound-variables block)))

(define (change-type/block block)
  (change-type/object enumeration/random block)
  (for-each (lambda (variable)
	      (change-type/object enumeration/random variable))
	    (block/bound-variables block))
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
  (object/set-enumerand!
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

(define-method/change-type 'IN-PACKAGE
  (lambda (expression)
    (change-type/expression (in-package/environment expression))
    (change-type/quotation (in-package/quotation expression))))

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