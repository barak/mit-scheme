#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlgen/rtlgen.scm,v 4.4 1988/03/14 20:55:24 jinx Exp $

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

;;;; RTL Generation

(declare (usual-integrations))

(define *generation-queue*)
(define *queued-procedures*)
(define *queued-continuations*)

(define (generate/top-level expression)
  (fluid-let ((*generation-queue* (make-queue))
	      (*queued-procedures* '())
	      (*queued-continuations* '()))
    (set! *rtl-expression* (generate/expression expression))
    (queue-map! *generation-queue* (lambda (thunk) (thunk)))
    (set! *rtl-graphs*
	  (list-transform-positive (reverse! *rtl-graphs*)
	    (lambda (rgraph)
	      (not (null? (rgraph-entry-edges rgraph))))))
    (for-each rgraph/compress! *rtl-graphs*)
    (set! *rtl-procedures* (reverse! *rtl-procedures*))
    (set! *rtl-continuations* (reverse! *rtl-continuations*))))

(define (enqueue-procedure! procedure)
  (if (not (memq procedure *queued-procedures*))
      (begin
	(enqueue! *generation-queue*
		  (lambda ()
		    (set! *rtl-procedures*
			  (cons (generate/procedure procedure)
				*rtl-procedures*))))
	(set! *queued-procedures* (cons procedure *queued-procedures*)))))

(define (enqueue-continuation! continuation)
  (if (not (memq continuation *queued-continuations*))
      (begin
	(enqueue! *generation-queue*
		  (lambda ()
		    (set! *rtl-continuations*
			  (cons (generate/continuation continuation)
				*rtl-continuations*))))
	(set! *queued-continuations*
	      (cons continuation *queued-continuations*)))))

(define (generate/expression expression)
  (transmit-values
      (generate/rgraph (expression-entry-node expression) generate/node)
    (lambda (rgraph entry-edge)
      (make-rtl-expr rgraph (expression-label expression) entry-edge))))

(define (generate/procedure procedure)
  (transmit-values
      (generate/rgraph
       (procedure-entry-node procedure)
       (lambda (node)
	 (generate/procedure-header
	  procedure
	  (generate/node node)
	  false)))
    (lambda (rgraph entry-edge)
      (make-rtl-procedure
       rgraph
       (procedure-label procedure)
       entry-edge
       (procedure-name procedure)
       (length (cdr (procedure-original-required procedure)))
       (length (procedure-original-optional procedure))
       (and (procedure-original-rest procedure) true)
       (and (procedure/closure? procedure) true)
       (procedure/type procedure)))))

(define (generate/procedure-entry/inline procedure)
  (generate/procedure-header procedure
			     (generate/node (procedure-entry-node procedure))
			     true))

(define (operator/needs-no-heap-check? op)
  (and (rvalue/constant? op)
       (let ((obj (constant-value op)))
	 (and (normal-primitive-procedure? obj)
	      (special-primitive-handler obj)))))

(define (continuation/avoid-check? continuation)
  (and (null? (continuation/returns continuation))
       (for-all?
	(continuation/combinations continuation)
	(lambda (combination)
	  (let ((op (rvalue-known-value (combination/operator combination))))
	    (and op (operator/needs-no-heap-check? op)))))))

(define (generate/continuation continuation)
  (let ((label (continuation/label continuation)))
    (transmit-values
	(generate/rgraph
	 (continuation/entry-node continuation)
	 (lambda (node)
	   (scfg-append!
	    (if (continuation/avoid-check? continuation)
		(rtl:make-continuation-entry label)
		(rtl:make-continuation-header label))
	    (generate/continuation-entry/ic-block continuation)
	    (if (block/dynamic-link?
		 (continuation/closing-block continuation))
		(rtl:make-pop-link)
		(make-null-cfg))
	    (enumeration-case continuation-type
		(continuation/type continuation)
	      ((PUSH)
	       (scfg*scfg->scfg!
		(rtl:make-push (rtl:make-fetch register:value))
		(generate/node node)))
	      ((REGISTER)
	       (scfg*scfg->scfg!
		(rtl:make-assignment (continuation/register continuation)
				     (rtl:make-fetch register:value))
		(generate/node node)))
	      (else
	       (generate/node node))))))
      (lambda (rgraph entry-edge)
	(make-rtl-continuation rgraph label entry-edge)))))

(define (generate/continuation-entry/ic-block continuation)
  (if (ic-block? (continuation/closing-block continuation))
      (rtl:make-pop register:environment)
      (make-null-cfg)))

(define (generate/node node)
  (let ((memoization (cfg-node-get node memoization-tag)))
    (cond ((not memoization)
	   (cfg-node-put! node memoization-tag loop-memoization-marker)
	   (let ((result (generate/node/no-memoize node)))
	     (cfg-node-put! node memoization-tag result)
	     result))
	  ((eq? memoization loop-memoization-marker)
	   (error "GENERATE/NODE: loop" node))
	  (else memoization))))

(define memoization-tag
  "rtlgen-memoization-tag")

(define loop-memoization-marker
  "rtlgen-loop-memoization-marker")

(define (generate/node/no-memoize node)
  (cfg-node-case (tagged-vector/tag node)
    ((APPLICATION)
     (if (snode-next node)
	 (error "Application node has next" node))
     (case (application-type node)
       ((COMBINATION) (generate/combination node))
       ((RETURN) (generate/return node))
       (else (error "Unknown application type" node))))
    ((VIRTUAL-RETURN)
     (scfg*scfg->scfg! (generate/virtual-return node)
		       (generate/node (snode-next node))))
    ((POP)
     (scfg*scfg->scfg! (generate/pop node)
		       (generate/node (snode-next node))))
    ((ASSIGNMENT)
     (scfg*scfg->scfg! (generate/assignment node)
		       (generate/node (snode-next node))))
    ((DEFINITION)
     (scfg*scfg->scfg! (generate/definition node)
		       (generate/node (snode-next node))))
    ((TRUE-TEST)
     (generate/true-test node))
    ((FG-NOOP)
     (generate/node (snode-next node)))))

(define (generate/rgraph node generator)
  (let ((rgraph (node->rgraph node)))
    (let ((entry-node
	   (cfg-entry-node
	    (fluid-let ((*current-rgraph* rgraph))
	      (with-new-node-marks (lambda () (generator node)))))))
      (add-rgraph-entry-node! rgraph entry-node)
      (return-2 rgraph (node->edge entry-node)))))

(define (node->rgraph node)
  (let ((color
	 (or (node/subgraph-color node)
	     (error "node lacking subgraph color" node))))
    (or (subgraph-color/rgraph color)
	(let ((rgraph (make-rgraph number-of-machine-registers)))
	  (set-subgraph-color/rgraph! color rgraph)
	  (set! *rtl-graphs* (cons rgraph *rtl-graphs*))
	  rgraph))))

(define (rgraph/compress! rgraph)
  (with-new-node-marks
   (lambda ()
     (for-each (lambda (edge)
		 (bblock-compress! (edge-right-node edge)))
	       (rgraph-initial-edges rgraph))))
  (set-rgraph-bblocks! rgraph (collect-rgraph-bblocks rgraph)))

(define collect-rgraph-bblocks
  (let ()
    (define (loop bblock)
      (node-mark! bblock)
      (cons bblock
	    (if (sblock? bblock)
		(next (snode-next bblock))
		(append! (next (pnode-consequent bblock))
			 (next (pnode-alternative bblock))))))

    (define (next bblock)
      (if (and bblock (not (node-marked? bblock)))
	  (loop bblock)
	  '()))

    (lambda (rgraph)
     (with-new-node-marks
      (lambda ()
	(mapcan (lambda (edge)
		  (loop (edge-right-node edge)))
		(rgraph-initial-edges rgraph)))))))