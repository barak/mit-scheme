#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlgen/rtlgen.scm,v 4.18 1989/05/31 20:02:25 jinx Rel $

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
       (for-each (lambda (rgraph)
		   (rgraph/compress! rgraph)
		   (rgraph/postcompress! rgraph))
		 *rtl-graphs*)
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
  (with-values
      (lambda ()
	(generate/rgraph (expression-entry-node expression) generate/node))
    (lambda (rgraph entry-edge)
      (make-rtl-expr rgraph
		     (expression-label expression)
		     entry-edge
		     (expression-debugging-info expression)))))

(define (generate/procedure procedure)
  (with-values
      (lambda ()
	(generate/rgraph
	 (procedure-entry-node procedure)
	 (lambda (node)
	   (generate/procedure-header procedure (generate/node node) false))))
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
       (procedure/type procedure)
       (procedure-debugging-info procedure)))))

(define (generate/procedure-entry/inline procedure)
  (generate/procedure-header procedure
			     (generate/node (procedure-entry-node procedure))
			     true))

(define (generate/continuation continuation)
  (let ((label (continuation/label continuation)))
    (with-values
	(lambda ()
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
	      (generate/node node)))))
      (lambda (rgraph entry-edge)
	(make-rtl-continuation
	 rgraph
	 label
	 entry-edge
	 (continuation/next-continuation-offset
	  (continuation/closing-block continuation)
	  (continuation/offset continuation))
	 (continuation/debugging-info continuation))))))

(define (continuation/avoid-check? continuation)
  (and (null? (continuation/returns continuation))
       (for-all?
	(continuation/combinations continuation)
	(lambda (combination)
	  (let ((op (rvalue-known-value (combination/operator combination))))
	    (and op (operator/needs-no-heap-check? op)))))))

(define (operator/needs-no-heap-check? op)
  (and (rvalue/constant? op)
       (let ((obj (constant-value op)))
	 (and (primitive-procedure? obj)
	      (special-primitive-handler obj)))))

(define (wrap-with-continuation-entry context scfg)
  (with-values (lambda () (generate-continuation-entry context))
    (lambda (label setup cleanup)
      label
      (scfg-append! setup scfg cleanup))))

(define (generate-continuation-entry context)
  (let ((label (generate-label))
	(closing-block (reference-context/block context)))
    (let ((setup (push-continuation-extra closing-block))
	  (cleanup
	   (scfg*scfg->scfg!
	    (rtl:make-continuation-entry label)
	    (pop-continuation-extra closing-block))))
      (set! *extra-continuations*
	    (cons (make-rtl-continuation
		   *current-rgraph*
		   label
		   (cfg-entry-edge cleanup)
		   (continuation/next-continuation-offset
		    closing-block
		    (reference-context/offset context))
		   (generated-dbg-continuation context label))
		  *extra-continuations*))
      (values label setup cleanup))))

(define (continuation/next-continuation-offset block offset)
  (if (stack-block? block)
      (let ((popping-limit (block-popping-limit block)))
	(and popping-limit
	     (let loop ((block block) (offset offset))
	       (let ((offset (+ offset (block-frame-size block))))
		 (if (eq? block popping-limit)
		     offset
		     (loop (block-parent block) offset))))))      offset))

(define (generate/continuation-entry/pop-extra continuation)
  (pop-continuation-extra (continuation/closing-block continuation)))

(define (push-continuation-extra closing-block)
  (cond ((ic-block? closing-block)
	 (rtl:make-push (rtl:make-fetch register:environment)))
	((and (stack-block? closing-block)
	      (stack-block/dynamic-link? closing-block))
	 (rtl:make-push-link))
	(else
	 (make-null-cfg))))

(define (pop-continuation-extra closing-block)
  (cond ((ic-block? closing-block)
	 (rtl:make-pop register:environment))
	((and (stack-block? closing-block)
	      (stack-block/dynamic-link? closing-block))
	 (rtl:make-pop-link))
	(else
	 (make-null-cfg))))

(define (generate/rgraph node generator)
  (let ((rgraph (node->rgraph node)))
    (let ((entry-edge
	   (node->edge
	    (cfg-entry-node
	     (rtl-precompress!
	      (fluid-let ((*current-rgraph* rgraph))
		(generator node)))))))
      (add-rgraph-entry-edge! rgraph entry-edge)
      (values rgraph entry-edge))))

(define (node->rgraph node)
  (let ((color
	 (or (node/subgraph-color node)
	     (error "node lacking subgraph color" node))))
    (or (subgraph-color/rgraph color)
	(let ((rgraph (make-rgraph number-of-machine-registers)))
	  (set-subgraph-color/rgraph! color rgraph)
	  (set! *rtl-graphs* (cons rgraph *rtl-graphs*))	  rgraph))))

(define (generate/node node)
  (let ((memoization (cfg-node-get node memoization-tag)))
    (cond ((not memoization)
	   (cfg-node-put! node memoization-tag loop-memoization-marker)
	   (let ((result (rtl-precompress! (generate/node/no-memoize node))))
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
  (let ((simple-snode
	 (lambda (generator)
	   (scfg*scfg->scfg! (generator node)
			     (generate/node (snode-next node))))))
    (cfg-node-case (tagged-vector/tag node)
      ((APPLICATION)
       (if (snode-next node)
	   (error "Application node has next" node))
       (case (application-type node)
	 ((COMBINATION) (generate/combination node))
	 ((RETURN) (generate/return node))
	 (else (error "Unknown application type" node))))
      ((VIRTUAL-RETURN)
       (simple-snode generate/virtual-return))
      ((POP)
       (simple-snode generate/pop))
      ((ASSIGNMENT)
       (simple-snode generate/assignment))
      ((DEFINITION)
       (simple-snode generate/definition))
      ((STACK-OVERWRITE)
       (simple-snode generate/stack-overwrite))
      ((TRUE-TEST)
       (generate/true-test node))
      ((FG-NOOP)
       (generate/node (snode-next node))))))

(define (rtl-precompress! cfg)
  (if (cfg-null? cfg)
      cfg
      (let ((edge (cfg-entry-edge cfg)))
	(with-new-node-marks
	 (lambda ()
	   (bblock-compress!
	    (edge-right-node edge)
	    (lambda (bblock)
	      (cfg-node-get bblock potential-control-merge-marker)))))
	(let ((bblock (edge-right-node edge)))
	  (edge-disconnect-right! edge)
	  (cfg-node-put! bblock potential-control-merge-marker true)
	  (case (cfg-tag cfg)
	    ((SNODE-CFG)
	     (make-scfg bblock (scfg-next-hooks cfg)))
	    ((PNODE-CFG)
	     (make-pcfg bblock
			(pcfg-consequent-hooks cfg)
			(pcfg-alternative-hooks cfg)))
	    (else
	     (error "Illegal cfg-tag" cfg)))))))

(define (rgraph/postcompress! rgraph)
  (for-each (lambda (bblock)
	      (cfg-node-remove! bblock potential-control-merge-marker))
	    (rgraph-bblocks rgraph)))

(define potential-control-merge-marker
  (intern "#[(compiler rtl-generator)potential-control-merge]"))

(define (rgraph/compress! rgraph)
  (with-new-node-marks
   (lambda ()
     (for-each (lambda (edge)
		 (bblock-compress! (edge-right-node edge) false))
	       (rgraph-initial-edges rgraph))))
  ;; This code attempts to remove backwards edges to pieces of the
  ;; graph which were generated and then not used.  It does this by
  ;; walking forward through the graph, and looking backward at each
  ;; node to find edges that have not been walked over.
  (with-new-node-marks
   (lambda ()
     (let ((initial-bblocks
	    (map->eq-set edge-right-node (rgraph-initial-edges rgraph))))
       (let ((result '()))
	 (define (loop bblock)
	   (if (sblock? bblock)
	       (next (snode-next bblock))
	       (begin
		 (next (pnode-consequent bblock))
		 (next (pnode-alternative bblock)))))

	 (define (next bblock)
	   (if (and bblock (not (node-marked? bblock)))
	       (begin
		 (node-mark! bblock)
		 (set! result (cons bblock result))
		 (loop bblock))))

	 (define (delete-block-edges! disallow-entries?)
	   (let ((delete-edges!
		  (list-deletor!
		   (lambda (edge)
		     (let ((bblock (edge-left-node edge)))
		       (if bblock
			   (not (node-marked? bblock))
			   disallow-entries?))))))
	     (lambda (bblock)
	       (set-node-previous-edges!
		bblock
		(delete-edges! (node-previous-edges bblock))))))

	 (for-each node-mark! initial-bblocks)
	 (for-each loop initial-bblocks)
	 (for-each (delete-block-edges! false) initial-bblocks)
	 (for-each (delete-block-edges! true) result)
	 (set-rgraph-bblocks! rgraph (append! initial-bblocks result)))))))

(define (bblock-compress! bblock limit-predicate)
  ;; This improved compressor should replace the original in "rtlbase/rtlcfg".
  (let ((walk-next?
	 (if limit-predicate
	     (lambda (next) (and next (not (limit-predicate next))))
	     (lambda (next) next))))
    (let walk-bblock ((bblock bblock))
      (if (not (node-marked? bblock))
	  (begin
	    (node-mark! bblock)
	    (if (sblock? bblock)
		(let ((next (snode-next bblock)))
		  (if (walk-next? next)
		      (begin
			(if (null? (cdr (node-previous-edges next)))
			    (begin
			      (set-rinst-next!
			       (rinst-last (bblock-instructions bblock))
			       (bblock-instructions next))
			      (set-bblock-instructions!
			       next
			       (bblock-instructions bblock))
			      (snode-delete! bblock)))
			(walk-bblock next))))
		(begin
		  (let ((consequent (pnode-consequent bblock)))
		    (if (walk-next? consequent)
			(walk-bblock consequent)))
		  (let ((alternative (pnode-alternative bblock)))
		    (if (walk-next? alternative)
			(walk-bblock alternative))))))))))