#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/offset.scm,v 4.2 1988/01/02 16:45:01 cph Exp $

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

;;;; Compute FG Node Offsets

(declare (usual-integrations))

(package (compute-node-offsets)

(define *procedure-queue*)
(define *procedures*)

(define-export (compute-node-offsets root-expression)
  (fluid-let ((*procedure-queue* (make-queue))
	      (*procedures* '()))
    (walk-node (expression-entry-node root-expression) 0)
    (queue-map! *procedure-queue*
      (lambda (procedure)
	(if (procedure-continuation? procedure)
	    (walk-node (continuation/entry-node procedure)
		       (if (eq? (continuation/type procedure)
				continuation-type/push)
			   (1+ (continuation/offset procedure))
			   (continuation/offset procedure)))
	    (begin
	      (for-each walk-rvalue (procedure-values procedure))
	      (walk-node (procedure-entry-node procedure) 0)))))))

(define (walk-node node offset)
  (let ((offset* (node/offset node)))
    (cond ((not offset*)
	   (set-node/offset! node offset)
	   (walk-node* node offset))
	  ((not (= offset offset*))
	   (error "COMPUTE-NODE-OFFSETS: mismatched offsets" node)))))

(define (walk-rvalue rvalue)
  (let ((rvalue (rvalue-known-value rvalue)))
    (if (and rvalue
	     (rvalue/procedure? rvalue)
	     (not (procedure-continuation? rvalue))
	     (not (memq rvalue *procedures*)))
	(enqueue-procedure! rvalue))))

(define (enqueue-procedure! procedure)
  (set! *procedures* (cons procedure *procedures*))
  (enqueue! *procedure-queue* procedure))

(define (walk-return operator operand offset)
  (walk-rvalue operator)
  (let ((continuation (rvalue-known-value operator)))
    (if (not (and continuation
		  (eq? continuation-type/effect
		       (continuation/type continuation))))
	(walk-rvalue operand))))

(define (walk-node* node offset)
  (cfg-node-case (tagged-vector/tag node)
    ((VIRTUAL-RETURN)
     (let ((operator (virtual-return-operator node))
	   (operand (virtual-return-operand node)))
       (if (virtual-continuation/reified? operator)
	   (walk-return operator operand offset)
	   (walk-node
	    (snode-next node)
	    (enumeration-case continuation-type
		(virtual-continuation/type operator)
	      ((EFFECT)
	       offset)
	      ((REGISTER VALUE)
	       (walk-rvalue operand)
	       offset)
	      ((PUSH)
	       (if (rvalue/continuation? operand)
		   (begin
		     (set-continuation/offset! operand offset)
		     (enqueue-procedure! operand)
		     (+ offset
			(block-frame-size (continuation/block operand))))
		   (begin
		     (walk-rvalue operand)
		     (1+ offset))))
	      (else
	       (error "Unknown continuation type" return)))))))
    ((APPLICATION)
     (case (application-type node)
       ((COMBINATION)
	(walk-rvalue (combination/operator node)))
       ((RETURN)
	(walk-return (return/operator node) (return/operand node) offset))))
    ((POP)
     (let ((continuation (pop-continuation node)))
       (if (procedure? continuation)
	   (walk-rvalue continuation)))
     (walk-node (snode-next node) (-1+ offset)))
    ((ASSIGNMENT)
     (if (not (lvalue-integrated? (assignment-lvalue node)))
	 (walk-rvalue (assignment-rvalue node)))
     (walk-node (snode-next node) offset))
    ((DEFINITION)
     (walk-rvalue (definition-rvalue node))
     (walk-node (snode-next node) offset))
    ((FG-NOOP)
     (walk-node (snode-next node) offset))
    ((TRUE-TEST)
     (walk-node (pnode-consequent node) offset)
     (walk-node (pnode-alternative node) offset))))

;;; end COMPUTE-NODE-OFFSETS
)