#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlgen/rtlgen.scm,v 4.11 1988/11/02 21:45:03 cph Exp $

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

;;;; RTL Generation

(declare (usual-integrations))

(define *generation-queue*)
(define *queued-procedures*)
(define *queued-continuations*)

(define *extra-continuations*)

(define (generate/top-level expression)
  (cleanup-noop-nodes
   (lambda ()
     (fluid-let ((*generation-queue* (make-queue))
		 (*queued-procedures* '())
		 (*queued-continuations* '()))
       (set! *extra-continuations* '())
       (set! *rtl-expression* (generate/expression expression))
       (queue-map!/unsafe *generation-queue* (lambda (thunk) (thunk)))
       (set! *rtl-graphs*
	     (list-transform-positive (reverse! *rtl-graphs*)
	       (lambda (rgraph)
		 (not (null? (rgraph-entry-edges rgraph))))))
       (for-each rgraph/compress! *rtl-graphs*)
       (set! *rtl-procedures* (reverse! *rtl-procedures*))
       (set! *rtl-continuations*
	     (append *extra-continuations* (reverse! *rtl-continuations*)))))))

(define (enqueue-procedure! procedure)
  (if (not (memq procedure *queued-procedures*))
      (begin
	(enqueue!/unsafe *generation-queue*
			 (lambda ()
			   (set! *rtl-procedures*
				 (cons (generate/procedure procedure)
				       *rtl-procedures*))))
	(set! *queued-procedures* (cons procedure *queued-procedures*)))))

(define (enqueue-continuation! continuation)
  (if (not (memq continuation *queued-continuations*))
      (begin
	(enqueue!/unsafe *generation-queue*
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
	    (generate/continuation-entry/pop-extra continuation)
	    (enumeration-case continuation-type
		(continuation/type continuation)
	      ((PUSH)
	       (rtl:make-push (rtl:make-fetch register:value)))
	      ((REGISTER)
	       (rtl:make-assignment (continuation/register continuation)
				    (rtl:make-fetch register:value)))
	      ((VALUE PREDICATE)
	       (if (continuation/ever-known-operator? continuation)
		   (rtl:make-assignment (continuation/register continuation)
					(rtl:make-fetch register:value))
		   (make-null-cfg)))
	      ((EFFECT)
	       (make-null-cfg))
	      (else
	       (error "Illegal continuation type" continuation)))
	    (generate/node node))))
      (lambda (rgraph entry-edge)
	(make-rtl-continuation rgraph label entry-edge)))))

(define (generate/continuation-entry/pop-extra continuation)
  (let ((block (continuation/closing-block continuation)))
    (scfg*scfg->scfg!
     (if (ic-block? block)
	 (rtl:make-pop register:environment)
	 (make-null-cfg))
     (if (block/dynamic-link? block)
	 (rtl:make-pop-link)
	 (make-null-cfg)))))

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
    (let ((entry-edge
	   (node->edge
	    (cfg-entry-node
	     (fluid-let ((*current-rgraph* rgraph))
	       (with-new-node-marks (lambda () (generator node))))))))
      (add-rgraph-entry-edge! rgraph entry-edge)
      (return-2 rgraph entry-edge))))

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
  (let ((collected-bblocks (collect-rgraph-bblocks rgraph)))
    (remove-unreachable-nodes! collected-bblocks)
    (set-rgraph-bblocks! rgraph collected-bblocks)))

(define collect-rgraph-bblocks
  ;; Before you do anything to this procedure which might change the
  ;; order of the bblocks in resultant list, please read the comment
  ;; for remove-unreachable-nodes!
  (let ((result))
    (define (loop bblock)
      (node-mark! bblock)
      (if (sblock? bblock)
	  (next (snode-next bblock))
	  (begin
	    (next (pnode-consequent bblock))
	    (next (pnode-alternative bblock))))
      (set! result (cons bblock result)))

    (define (next bblock)
      (and bblock
	   (not (node-marked? bblock))
	   (loop bblock)))
    
    (define (doit bblock)
      (set! result '())
      (loop bblock)
      result)

    (lambda (rgraph)
      (with-new-node-marks
       (lambda ()
	 (mapcan (lambda (edge)
		   (doit (edge-right-node edge)))
		 (rgraph-initial-edges rgraph)))))))

(define (remove-unreachable-nodes! collected-bblocks)
  ;; This procedure depends on the order of the nodes in
  ;; collected-bblocks. This order must be such that every node on a
  ;; path from the root of the rgraph to a given node must precede the
  ;; given node in the ordering. Another way of saying this is that
  ;; the order of node in collected-bblocks must be a partial order on
  ;; the ancestor-of relation on the rgraph DAG. Needless to say the
  ;; procedure collect-rgraph-bblocks above produces a list of bblocks
  ;; which has the correct order.
  (with-new-node-marks
   (lambda ()
     (for-each
      (lambda (bblock)
	(node-mark! bblock)
	(set-node-previous-edges! bblock
	  (list-transform-positive (node-previous-edges bblock)
	    (lambda (edge)
	      (let ((prev-node (edge-left-node edge)))
		(or (not prev-node) (node-marked? prev-node)))))))
      collected-bblocks))))