#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlbase/rtlty2.scm,v 1.1 1987/06/17 19:43:35 cph Exp $

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

;;;; Register Transfer Language Type Definitions

(declare (usual-integrations))

(define-integrable rtl:expression-type first)
(define-integrable rtl:address-register second)
(define-integrable rtl:address-number third)
(define-integrable rtl:invocation-pushed second)
(define-integrable rtl:invocation-prefix third)
(define-integrable rtl:invocation-continuation fourth)
(define-integrable rtl:test-expression second)

(define-integrable (rtl:make-entry:continuation continuation)
  (%make-entry:continuation (continuation-label continuation)))

(define-integrable (rtl:make-entry:procedure procedure)
  (%make-entry:procedure (procedure-label procedure)))

(define-integrable (rtl:make-continuation-heap-check continuation)
  (%make-continuation-heap-check (continuation-label continuation)))

(define-integrable (rtl:make-procedure-heap-check procedure)
  (%make-procedure-heap-check (procedure-label procedure)))

(define-integrable (rtl:make-setup-lexpr procedure)
  (%make-setup-lexpr (procedure-label procedure)))

(define-integrable (rtl:make-message-receiver:subproblem continuation)
  (%make-message-receiver:subproblem (continuation-label continuation)))

(define (rtl:make-constant value)
  (if (scode/unassigned-object? value)
      (rtl:make-unassigned)
      (%make-constant value)))

;;;; Locatives

;;; Locatives are used as an intermediate form by the code generator
;;; to build expressions.  Later, when the expressions are inserted
;;; into statements, any locatives they contain are eliminated by
;;; "simplifying" them into sequential instructions using pseudo
;;; registers.

(define-integrable register:environment
  'ENVIRONMENT)

(define-integrable register:frame-pointer
  'FRAME-POINTER)

(define-integrable register:stack-pointer
  'STACK-POINTER)

(define-integrable register:value
  'VALUE)

(define-integrable (rtl:interpreter-call-result:access)
  (rtl:make-fetch 'INTERPRETER-CALL-RESULT:ACCESS))

(define-integrable (rtl:interpreter-call-result:cache-reference)
  (rtl:make-fetch 'INTERPRETER-CALL-RESULT:CACHE-REFERENCE))

(define-integrable (rtl:interpreter-call-result:enclose)
  (rtl:make-fetch 'INTERPRETER-CALL-RESULT:ENCLOSE))

(define-integrable (rtl:interpreter-call-result:lookup)
  (rtl:make-fetch 'INTERPRETER-CALL-RESULT:LOOKUP))

(define-integrable (rtl:interpreter-call-result:unassigned?)
  (rtl:make-fetch 'INTERPRETER-CALL-RESULT:UNASSIGNED?))

(define-integrable (rtl:interpreter-call-result:unbound?)
  (rtl:make-fetch 'INTERPRETER-CALL-RESULT:UNBOUND?))

(define (rtl:locative-offset locative offset)
  (cond ((zero? offset) locative)
	((and (pair? locative) (eq? (car locative) 'OFFSET))
	 `(OFFSET ,(cadr locative) ,(+ (caddr locative) offset)))
	(else `(OFFSET ,locative ,offset))))

;;; Expressions that are used in the intermediate form.

(define-integrable (rtl:make-fetch locative)
  `(FETCH ,locative))

(define-integrable (rtl:make-address locative)
  `(ADDRESS ,locative))

(define-integrable (rtl:make-cell-cons expression)
  `(CELL-CONS ,expression))

(define-integrable (rtl:make-typed-cons:pair type car cdr)
  `(TYPED-CONS:PAIR ,type ,car ,cdr))

;;; Linearizer Support

(define-integrable (rtl:make-jump-statement label)
  `(JUMP ,label))

(define-integrable (rtl:make-jumpc-statement predicate label)
  `(JUMPC ,predicate ,label))

(define-integrable (rtl:make-label-statement label)
  `(LABEL ,label))

(define-integrable (rtl:negate-predicate expression)
  `(NOT ,expression))

;;; Stack

(define-integrable (stack-locative-offset locative offset)
  (rtl:locative-offset locative (stack->memory-offset offset)))

(define-integrable (stack-push-address)
  (rtl:make-pre-increment (interpreter-stack-pointer)
			  (stack->memory-offset -1)))

(define-integrable (stack-pop-address)
  (rtl:make-post-increment (interpreter-stack-pointer)
			   (stack->memory-offset 1)))