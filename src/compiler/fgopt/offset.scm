#| -*-Scheme-*-

$Id$

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; Compute FG Node Offsets
;;; package: (compiler fg-optimizer compute-node-offsets)

(declare (usual-integrations))

(define *grafted-procedures*)
(define *procedure-queue*)
(define *procedures*)

(define (compute-node-offsets root-expression)
  (fluid-let ((*procedure-queue* (make-queue))
	      (*grafted-procedures* '())
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
		 (lambda (value name)
		   (cond ((and (rvalue/procedure? value)
			       (not (procedure-continuation? value)))
			  (let ((context (procedure-closure-context value)))
			    (if (reference-context? context)
				(let ((closing-block
				       (procedure-closing-block value)))
				  (if (eq? closing-block
					   (block-shared-block closing-block))
				      (update-reference-context/offset! context
									0)
				      (update-reference-context/fake-offset!
				       context name)))))
			  (walk-rvalue value 0))
			 ((rvalue/block? value)
			  (enqueue-grafted-procedures! value))
			 (else
			  (walk-rvalue value 0))))
		 (procedure-values procedure)
		 (procedure-names procedure))
		(walk-next (procedure-entry-node procedure) 0)))))
       ;; This is a kludge.  If the procedure hasn't been encountered
       ;; elsewhere, tag it as closed when the letrec was done.
       (for-each
	(lambda (procedure)
	  (let ((context (procedure-closure-context procedure)))
	    (if (not (reference-context/offset context))
		(set-reference-context/offset! context 0))))
	*grafted-procedures*)))))

(define (enqueue-grafted-procedures! block)
  (let ((procs (map (lambda (block)
		      (block-procedure (car (block-children block))))
		    (block-grafted-blocks block))))
    (set! *grafted-procedures* (append procs *grafted-procedures*))
    (for-each maybe-enqueue-procedure! procs)))

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
    (cond ((not offset*)
	   (set-reference-context/offset! context offset))
	  ((not (= offset offset*))
	   (error "mismatched offsets" context)))))

(define (update-reference-context/fake-offset! context name)
  (let ((offset (- -1 (variable-normal-offset name)))
	(offset* (reference-context/offset context)))
    (cond ((or (not offset*)
	       (zero? offset*))
	   (set-reference-context/offset! context offset))
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