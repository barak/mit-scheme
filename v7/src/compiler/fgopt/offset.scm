#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/offset.scm,v 4.6 1988/12/12 21:51:52 cph Rel $

Copyright (c) 1988 Massachusetts Institute of Technology

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

;;;; Compute FG Node Offsets

(declare (usual-integrations))

(define *procedure-queue*)
(define *procedures*)

(define (compute-node-offsets root-expression)
  (fluid-let ((*procedure-queue* (make-queue))
	      (*procedures* '()))
    (with-new-node-marks
     (lambda ()
       (walk-node (expression-entry-node root-expression) 0)
       (queue-map!/unsafe *procedure-queue*
	 (lambda (procedure)
	   (if (procedure-continuation? procedure)
	       (walk-next (continuation/entry-node procedure)
			  (if (eq? (continuation/type procedure)
				   continuation-type/push)
			      (1+ (continuation/offset procedure))
			      (continuation/offset procedure)))
	       (begin
		 (for-each
		  (lambda (value)
		    (if (and (rvalue/procedure? value)
			     (not (procedure-continuation? value)))
			(let ((context (procedure-closure-context value)))
			  (if (reference-context? context)
			      (update-reference-context/offset! context 0))))
		    (walk-rvalue value 0))
		  (procedure-values procedure))
		 (walk-next (procedure-entry-node procedure) 0)))))))))

(define (walk-rvalue rvalue offset)
  (if (and (rvalue/procedure? rvalue)
	   (not (procedure-continuation? rvalue)))
      (let ((context (procedure-closure-context rvalue)))
	(if (reference? context)
	    (update-reference-context/offset! (reference-context context)
					      offset))))
  (maybe-enqueue-procedure! rvalue))

(define (maybe-enqueue-procedure! rvalue)
  (let ((value (rvalue-known-value rvalue)))
    (if (and value
	     (rvalue/procedure? value)
	     (not (procedure-continuation? value))
	     (not (memq value *procedures*)))
	(enqueue-procedure! value))))

(define (enqueue-procedure! procedure)
  (set! *procedures* (cons procedure *procedures*))
  (enqueue!/unsafe *procedure-queue* procedure))

(define (walk-next node offset)
  (if (and node (not (node-marked? node)))
      (walk-node node offset)))

(define (update-reference-context/offset! context offset)
  (let ((offset* (reference-context/offset context)))
    (cond ((not offset*)	   (set-reference-context/offset! context offset))
	  ((not (= offset offset*))
	   (error "mismatched offsets" context)))))

(define (walk-return operator operand offset)
  (if (let ((continuation (rvalue-known-value operator)))
	(not (and continuation
		  (eq? continuation-type/effect
		       (continuation/type continuation)))))
      (walk-rvalue operand offset)))

(define (walk-node node offset)
  (node-mark! node)
  (cfg-node-case (tagged-vector/tag node)
    ((VIRTUAL-RETURN)
     (update-reference-context/offset! (virtual-return-context node) offset)
     (let ((operator (virtual-return-operator node))
	   (operand (virtual-return-operand node)))
       (if (virtual-continuation/reified? operator)
	   (walk-return (virtual-continuation/reification operator)
			operand
			offset)
	   (begin
	     (if (rvalue/continuation? operand)
		 (begin
		   (set-continuation/offset! operand offset)
		   (enqueue-procedure! operand)))
	     (walk-next
	      (snode-next node)
	      (enumeration-case continuation-type
		  (virtual-continuation/type operator)
		((EFFECT)
		 offset)
		((REGISTER VALUE)
		 (walk-rvalue operand offset)
		 offset)
		((PUSH)
		 (if (rvalue/continuation? operand)
		     (+ offset (block-frame-size (continuation/block operand)))
		     (begin
		       (walk-rvalue operand offset)
		       (1+ offset))))
		(else
		 (error "Unknown continuation type" return))))))))
    ((APPLICATION)
     (update-reference-context/offset! (application-context node) offset)
     (case (application-type node)
       ((COMBINATION)
	(maybe-enqueue-procedure! (combination/operator node))
	(for-each maybe-enqueue-procedure! (combination/operands node)))
       ((RETURN)
	(walk-return (return/operator node) (return/operand node) offset))))
    ((POP)
     (walk-next (snode-next node) (-1+ offset)))
    ((ASSIGNMENT)
     (update-reference-context/offset! (assignment-context node) offset)
     (if (not (lvalue-integrated? (assignment-lvalue node)))
	 (walk-rvalue (assignment-rvalue node) offset))
     (walk-next (snode-next node) offset))
    ((DEFINITION)
     (update-reference-context/offset! (definition-context node) offset)
     (walk-rvalue (definition-rvalue node) offset)
     (walk-next (snode-next node) offset))
    ((STACK-OVERWRITE)
     (let ((offset
	    (if (eq? (continuation*/type (stack-overwrite-continuation node))
		     continuation-type/push)
		(-1+ offset)
		offset)))
       (update-reference-context/offset! (stack-overwrite-context node) offset)
       (walk-next (snode-next node) offset)))
    ((FG-NOOP)
     (walk-next (snode-next node) offset))
    ((TRUE-TEST)
     (update-reference-context/offset! (true-test-context node) offset)
     (walk-next (pnode-consequent node) offset)
     (walk-next (pnode-alternative node) offset))))