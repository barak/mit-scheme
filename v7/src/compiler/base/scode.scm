#| -*-Scheme-*-

$Id: scode.scm,v 4.11 2001/12/20 16:28:22 cph Exp $

Copyright (c) 1988-1999, 2001 Massachusetts Institute of Technology

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
    make-lambda lambda? lambda-components
    make-open-block open-block? open-block-components
    primitive-procedure? procedure?
    make-quotation quotation? quotation-expression
    make-sequence sequence? sequence-actions sequence-components
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