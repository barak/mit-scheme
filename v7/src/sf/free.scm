#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/sf/free.scm,v 3.0 1987/03/10 13:24:54 cph Exp $

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

;;;; SCode Optimizer: Free Variable Analysis

(declare (usual-integrations))

(define (free/expressions expressions)
  (if (null? expressions)
      eq?-set/null
      (eq?-set/union (free/expression (car expressions))
		     (free/expressions (cdr expressions)))))

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
    (eq?-set/adjoin (assignment/variable expression)
		    (free/expression (assignment/value expression)))))

(define-method/free 'COMBINATION
  (lambda (expression)
    (eq?-set/union (free/expression (combination/operator expression))
		   (free/expressions (combination/operands expression)))))

(define-method/free 'CONDITIONAL
  (lambda (expression)
    (eq?-set/union
     (free/expression (conditional/predicate expression))
     (eq?-set/union (free/expression (conditional/consequent expression))
		    (free/expression (conditional/alternative expression))))))

(define-method/free 'CONSTANT
  (lambda (expression)
    eq?-set/null))

(define-method/free 'DECLARATION
  (lambda (expression)
    (free/expression (declaration/expression expression))))

(define-method/free 'DELAY
  (lambda (expression)
    (free/expression (delay/expression expression))))

(define-method/free 'DISJUNCTION
  (lambda (expression)
    (eq?-set/union (free/expression (disjunction/predicate expression))
		   (free/expression (disjunction/alternative expression)))))

(define-method/free 'IN-PACKAGE
  (lambda (expression)
    (free/expression (in-package/environment expression))))

(define-method/free 'PROCEDURE
  (lambda (expression)
    (eq?-set/difference (free/expression (procedure/body expression))
			(block/bound-variables (procedure/block expression)))))

(define-method/free 'OPEN-BLOCK
  (lambda (expression)
    (eq?-set/difference
     (eq?-set/union (free/expressions (open-block/values expression))
		    (let loop ((actions (open-block/actions expression)))
		      (cond ((null? actions) eq?-set/null)
			    ((eq? (car actions) open-block/value-marker)
			     (loop (cdr actions)))
			    (else
			     (eq?-set/union (free/expression (car actions))
					    (loop (cdr actions)))))))
     (block/bound-variables (open-block/block expression)))))

(define-method/free 'QUOTATION
  (lambda (expression)
    eq?-set/null))

(define-method/free 'REFERENCE
  (lambda (expression)
    (eq?-set/singleton (reference/variable expression))))

(define-method/free 'SEQUENCE
  (lambda (expression)
    (free/expressions (sequence/actions expression))))

(define-method/free 'THE-ENVIRONMENT
  (lambda (expression)
    eq?-set/null))