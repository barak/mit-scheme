#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/scode.scm,v 4.1 1987/12/04 20:04:59 cph Exp $

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

;;;; SCode Interface

(declare (usual-integrations))

(let-syntax ((define-scode-operators
	       (macro names
		 `(BEGIN ,@(map (lambda (name)
				  `(DEFINE ,(symbol-append 'SCODE/ name)
				     (ACCESS ,name SYSTEM-GLOBAL-ENVIRONMENT)))
				names)))))
  (define-scode-operators
    make-access access? access-components
    access-environment access-name
    make-assignment assignment? assignment-components
    assignment-name assignment-value
    make-combination combination? combination-components
    combination-operator combination-operands
    make-comment comment? comment-components
    comment-expression comment-text
    make-conditional conditional? conditional-components
    conditional-predicate conditional-consequent conditional-alternative
    make-declaration declaration? declaration-components
    declaration-expression declaration-text
    make-definition definition? definition-components
    definition-name definition-value
    make-delay delay? delay-components
    delay-expression
    make-disjunction disjunction? disjunction-components
    conditional-predicate conditional-alternative
    make-in-package in-package? in-package-components
    in-package-environment in-package-expression
    make-lambda lambda? lambda-components
    make-open-block open-block? open-block-components
    primitive-procedure?
    make-quotation quotation? quotation-expression
    make-sequence sequence-actions
    symbol?
    make-the-environment the-environment?
    make-unassigned-object unassigned-object?
    make-unassigned? unassigned?? unassigned?-name
    make-unbound? unbound?? unbound?-name
    make-variable variable? variable-components variable-name
    ))

(define-integrable (scode/make-constant const)
  const)

(define scode/constant?
  (access scode-constant? system-global-environment))

(define-integrable (scode/constant-value const)
  const)

;;;; Absolute variables and combinations

(define (scode/make-absolute-reference variable-name)
  (scode/make-access '() variable-name))

(define (scode/absolute-reference? object)
  (and (scode/access? object)
       (null? (scode/access-environment object))))

(define (scode/absolute-reference-name reference)
  (scode/access-name reference))

(define (scode/make-absolute-combination name operands)
  (scode/make-combination (scode/make-absolute-reference name) operands))

(define (scode/absolute-combination? object)
  (and (scode/combination? object)
       (scode/absolute-reference? (scode/combination-operator object))))

(define (scode/absolute-combination-components combination receiver)
  (scode/combination-components combination
    (lambda (operator operands)
      (receiver (scode/absolute-reference-name operator) operands))))

(define scode/error-combination?
  (type-object-predicate error-combination-type))

(define (scode/error-combination-components combination receiver)
  (scode/combination-components combination
    (lambda (operator operands)
      (receiver (car operands)
		(let ((irritant (cadr operands)))
		  (cond ((scode/access? irritant) '())
			((scode/absolute-combination? irritant)
			 (scode/absolute-combination-components irritant
			   (lambda (name operands)
			     (if (eq? name 'LIST)
				 operands
				 (list irritant)))))
			(else (list irritant))))))))

(define (scode/make-error-combination message operand)
  (scode/make-absolute-combination
   'ERROR-PROCEDURE
   (list message operand (scode/make-the-environment))))