#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/contin.scm,v 4.2 1987/12/30 06:58:17 cph Exp $

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

;;;; Continuation datatype

(declare (usual-integrations))

;;; Continuations are a subtype of procedures, whose `type' is
;;; something other than PROCEDURE.

(define (make-continuation block continuation type)
  (let ((block (make-block block 'CONTINUATION)))
    (let ((required (list (make-value-variable block))))
      (set-block-bound-variables! block required)
      (make-procedure type block 'CONTINUATION required '() false '() '()
		      (snode->scfg (make-fg-noop))))))

(define-enumeration continuation-type
  (effect
   predicate
   procedure
   push
   register
   value))

(define-integrable (procedure-continuation? procedure)
  (not (eq? (procedure-type procedure) continuation-type/procedure)))

(define (rvalue/continuation? rvalue)
  (and (rvalue/procedure? rvalue)
       (procedure-continuation? rvalue)))

(define-integrable continuation/type procedure-type)
(define-integrable set-continuation/type! set-procedure-type!)
(define-integrable continuation/block procedure-block)
(define-integrable continuation/closing-block procedure-closing-block)
(define-integrable continuation/entry-node procedure-entry-node)
(define-integrable set-continuation/entry-node! set-procedure-entry-node!)
(define-integrable continuation/combinations procedure-original-rest)
(define-integrable set-continuation/combinations! set-procedure-original-rest!)
(define-integrable continuation/label procedure-label)
(define-integrable continuation/returns procedure-applications)
(define-integrable set-continuation/returns! set-procedure-applications!)
(define-integrable continuation/always-known-operator?
  procedure-always-known-operator?)
(define-integrable continuation/offset procedure-closure-offset)
(define-integrable set-continuation/offset! set-procedure-closure-offset!)
(define-integrable continuation/passed-out? procedure-passed-out?)
(define-integrable set-continuation/passed-out?! set-procedure-passed-out?!)

(define (continuation/register continuation)
  (or (procedure-register continuation)
      (let ((register (rtl:make-pseudo-register)))
	(set-procedure-register! continuation register)
	register)))

(define-integrable (continuation/parameter continuation)
  (car (procedure-original-required continuation)))

(define-integrable return-operator/subproblem? rvalue/procedure?)
(define-integrable return-operator/reduction? rvalue/reference?)

(define-integrable reduction-continuation/block reference-block)
(define-integrable reduction-continuation/lvalue reference-lvalue)

(define-integrable (reduction-continuation/popping-limit continuation)
  (variable-popping-limit (reference-lvalue continuation)))

(define (return-operator/popping-limit operator)
  (if (return-operator/reduction? operator)
      (reduction-continuation/popping-limit operator)
      (continuation/closing-block operator)))

(define (continuation/frame-size continuation)
  (let ((closing-block (continuation/closing-block continuation)))
    (+ (if (ic-block? closing-block) 1 0)
       (if (continuation/always-known-operator? continuation)
	   0
	   (if (and (stack-block? closing-block)
		    (stack-block/dynamic-link? closing-block))
	       2
	       1)))))

(define (uni-continuation? rvalue)
  (and (rvalue/procedure? rvalue)
       (procedure-arity-correct? rvalue 1)))

(define-integrable (uni-continuation/parameter continuation)
  (car (procedure-original-required continuation)))