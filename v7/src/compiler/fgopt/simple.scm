#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/simple.scm,v 4.5 1989/07/18 20:22:38 cph Exp $

Copyright (c) 1987, 1989 Massachusetts Institute of Technology

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

;;;; Argument Simplicity Analysis

(declare (usual-integrations))

(define (simplicity-analysis parallels)
  (for-each (lambda (parallel)
	      (for-each (lambda (subproblem)
			  (set-subproblem-simple?! subproblem 'UNKNOWN))
			(parallel-subproblems parallel)))
	    parallels)
  (for-each (lambda (parallel)
	      (for-each walk/subproblem (parallel-subproblems parallel)))
	    parallels))

(define (walk/subproblem subproblem)
  (if (eq? (subproblem-simple? subproblem) 'UNKNOWN)
      (update-subproblem! subproblem))
  (subproblem-simple? subproblem))

(define (new-subproblem/compute-simplicity! subproblem)
  ;; This is currently used only when `subproblem' has no prefix; if
  ;; other kinds of subproblems are supplied here, we might need to
  ;; worry about changing the node walker to handle those types of
  ;; nodes that are introduced later in the optimization process.
  (update-subproblem! subproblem))

(define (update-subproblem! subproblem)
  (set-subproblem-simple?!
   subproblem
   (if (subproblem-canonical? subproblem)
       (walk/node (subproblem-entry-node subproblem)
		  (subproblem-continuation subproblem))
       (and (walk/rvalue (subproblem-rvalue subproblem))
	    (let ((prefix (subproblem-prefix subproblem)))
	      (if (cfg-null? prefix)
		  true
		  (walk/node (cfg-entry-node prefix) false))))))
  unspecific)

(define (walk/node node continuation)
  (cfg-node-case (tagged-vector/tag node)
    ((PARALLEL)
     (and (for-all? (parallel-subproblems node) walk/subproblem)
	  (walk/next (snode-next node) continuation)))
    ((APPLICATION)
     (case (application-type node)
       ((COMBINATION)
	(if (and (combination/inline? node)
		 (combination/inline/simple? node))
	    (walk/return-operator (combination/continuation node) continuation)
	    (let ((callee (rvalue-known-value (combination/operator node))))
	      (and callee
		   (rvalue/procedure? callee)
		   (procedure-inline-code? callee)
		   (walk/next (procedure-entry-node callee) continuation)))))
       ((RETURN)
	(walk/return-operator (return/operator node) continuation))
       (else
	(error "Unknown application type" node))))
    ((ASSIGNMENT)
     (and (walk/lvalue (assignment-lvalue node))
	  (walk/rvalue (assignment-rvalue node))
	  (walk/next (snode-next node) continuation)))
    ((DEFINITION)
     (and (walk/lvalue (definition-lvalue node))
	  (walk/rvalue (definition-rvalue node))
	  (walk/next (snode-next node) continuation)))
    ((TRUE-TEST)
     (and (walk/rvalue (true-test-rvalue node))
	  (walk/next (pnode-consequent node) continuation)
	  (walk/next (pnode-alternative node) continuation)))
    ((VIRTUAL-RETURN FG-NOOP)
     (walk/next (snode-next node) continuation))))

(define (walk/next node continuation)
  (if node
      (walk/node node continuation)
      (not continuation)))

(define (walk/return-operator operator continuation)
  (and (return-operator/subproblem? operator)
       (if (eq? operator continuation)
	   true
	   (walk/next (continuation/entry-node operator) continuation))))

(define (walk/rvalue rvalue)
  (if (rvalue/reference? rvalue)
      (let ((lvalue (reference-lvalue rvalue)))
	(if (or (variable/value-variable? lvalue)
		(lvalue-known-value lvalue))
	    true
	    (walk/lvalue lvalue)))
      true))

(define (walk/lvalue lvalue)
  (not (block-passed-out? (variable-block lvalue))))