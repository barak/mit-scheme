#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/scode.scm,v 4.7 1989/08/15 12:58:32 cph Rel $

Copyright (c) 1988, 1989 Massachusetts Institute of Technology

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
    disjunction-predicate disjunction-alternative
    make-in-package in-package? in-package-components
    in-package-environment in-package-expression
    make-lambda lambda? lambda-components
    make-open-block open-block? open-block-components
    primitive-procedure?
    make-quotation quotation? quotation-expression
    make-sequence sequence-actions sequence-components
    symbol?
    make-the-environment the-environment?
    make-unassigned? unassigned?? unassigned?-name
    make-variable variable? variable-components variable-name
    ))

(define-integrable (scode/make-constant value) value)
(define-integrable (scode/constant-value constant) constant)
(define scode/constant? (access scode-constant? system-global-environment))

(define-integrable (scode/quotation-components quot recvr)
  (recvr (scode/quotation-expression quot)))

(define comment-tag:directive
  (intern "#[(compiler)comment-tag:directive]"))

(define (scode/make-directive code directive original-code)
  (scode/make-comment
   (list comment-tag:directive
	 directive
	 (scode/original-expression original-code))
   code))

(define (scode/original-expression scode)
  (if (and (scode/comment? scode)
	   (scode/comment-directive? (scode/comment-text scode)))
      (caddr (scode/comment-text scode))
      scode))

(define (scode/comment-directive? text . kinds)
  (and (pair? text)
       (eq? (car text) comment-tag:directive)
       (or (null? kinds)
	   (memq (caadr text) kinds))))

(define (scode/make-let names values . body)
  (scan-defines (scode/make-sequence body)
    (lambda (auxiliary declarations body)
      (scode/make-combination
       (scode/make-lambda lambda-tag:let names '() false
			  auxiliary declarations body)
       values))))

;;;; Absolute variables and combinations

(define-integrable (scode/make-absolute-reference variable-name)
  (scode/make-access '() variable-name))

(define (scode/absolute-reference? object)
  (and (scode/access? object)
       (null? (scode/access-environment object))))

(define-integrable (scode/absolute-reference-name reference)
  (scode/access-name reference))

(define-integrable (scode/make-absolute-combination name operands)
  (scode/make-combination (scode/make-absolute-reference name) operands))

(define (scode/absolute-combination? object)
  (and (scode/combination? object)
       (scode/absolute-reference? (scode/combination-operator object))))

(define-integrable (scode/absolute-combination-name combination)
  (scode/absolute-reference-name (scode/combination-operator combination)))

(define-integrable (scode/absolute-combination-operands combination)
  (scode/combination-operands combination))

(define (scode/absolute-combination-components combination receiver)
  (receiver (scode/absolute-combination-name combination)
	    (scode/absolute-combination-operands combination)))

(define (scode/error-combination? object)
  (or (and (scode/combination? object)
	   (eq? (scode/combination-operator object) error-procedure))
      (and (scode/absolute-combination? object)
	   (eq? (scode/absolute-combination-name object) 'ERROR-PROCEDURE))))

(define (scode/error-combination-components combination receiver)
  (scode/combination-components combination
    (lambda (operator operands)
      operator
      (receiver
       (car operands)
       (let loop ((irritants (cadr operands)))
	 (cond ((null? irritants) '())
	       ((and (scode/absolute-combination? irritants)
		     (eq? (scode/absolute-combination-name irritants) 'LIST))
		(scode/absolute-combination-operands irritants))
	       ((and (scode/combination? irritants)
		     (eq? (scode/combination-operator irritants) cons))
		(let ((operands (scode/combination-operands irritants)))
		  (cons (car operands)
			(loop (cadr operands)))))
	       (else
		(cadr operands))))))))

(define (scode/make-error-combination message operand)
  (scode/make-absolute-combination
   'ERROR-PROCEDURE
   (list message
	 (scode/make-combination cons (list operand '()))
	 (scode/make-the-environment))))