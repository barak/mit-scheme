#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

;;;; RTL Generation
;;; package: (compiler rtl-generator)

(declare (usual-integrations))

(define *generation-queue*)
(define *queued-procedures*)
(define *queued-continuations*)
(define *rgraphs*)
(define *procedures*)
(define *continuations*)
(define *extra-continuations*)

(define (generate/top-level expression)
  (cleanup-noop-nodes
   (lambda ()
     (fluid-let ((*generation-queue* (make-queue))
		 (*queued-procedures* '())
		 (*queued-continuations* '())
		 (*rgraphs* '())
		 (*procedures* '())
		 (*continuations* '())
		 (*extra-continuations* '()))
       (let ((expression (generate/expression expression)))
	 (queue-map!/unsafe *generation-queue* (lambda (thunk) (thunk)))
	 (let ((rgraphs
		(list-transform-positive (reverse! *rgraphs*)
		  (lambda (rgraph)
		    (not (null? (rgraph-entry-edges rgraph)))))))
	   (for-each (lambda (rgraph)
		       (rgraph/compress! rgraph)
		       (rgraph/postcompress! rgraph))
		     rgraphs)
	   (values expression
		   (reverse! *procedures*)
		   (append *extra-continuations* (reverse! *continuations*))
		   rgraphs)))))))

(define (enqueue-procedure! procedure)
  (if (not (memq procedure *queued-procedures*))
      (begin
	(enqueue!/unsafe *generation-queue*
	  (lambda ()
	    (set! *procedures*
		  (cons (generate/procedure procedure) *procedures*))
	    unspecific))
	(set! *queued-procedures* (cons procedure *queued-procedures*))
	unspecific)))

(define (enqueue-continuation! continuation)
  (if (not (memq continuation *queued-continuations*))
      (begin
	(enqueue!/unsafe *generation-queue*
	  (lambda ()
	    (set! *continuations*
		  (cons (generate/continuation continuation) *continuations*))
	    unspecific))
	(set! *queued-continuations*
	      (cons continuation *queued-continuations*))
	unspecific)))

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
       (let ((block (procedure-block procedure)))
	 (and (stack-block? block)
	      (stack-block/dynamic-link? block)
	      true))
       (procedure/type procedure)
       (procedure-debugging-info procedure)
       (block/next-continuation-offset (procedure-block procedure) 0)
       ;; This expression computes the value of STACK-LEAF? for
       ;; PROCEDURE.  This is defined to mean that the procedure
       ;; doesn't push anything, but it's not what this expression
       ;; computes.  Instead, it is true if the procedure doesn't push
       ;; any continuations on the stack.  Thus it is true of
       ;; procedures that push environment bindings on the stack,
       ;; provided that all of the procedure calls made by them are
       ;; reductions.
       (let loop ((block (procedure-block procedure)))
	 (for-all? (block-children block)
	   (lambda (block)
	     (let ((procedure (block-procedure block)))
	       (and (procedure? procedure)
		    (if (procedure-continuation? procedure)
			(continuation/always-known-operator? procedure)
			;; Inline-coded child procedures are treated
			;; as an extension of this procedure.
			(or (not (procedure-inline-code? procedure))
			    (loop block))))))))))))

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
	 (compute-next-continuation-offset
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

(define (wrap-with-continuation-entry context scfg-gen)
  (with-values (lambda () (generate-continuation-entry context))
    (lambda (label setup cleanup)
      (scfg-append! setup
		    (scfg-gen label)
		    cleanup))))

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
		   (compute-next-continuation-offset
		    closing-block
		    (reference-context/offset context))
		   (generated-dbg-continuation context label))
		  *extra-continuations*))
      (values label setup cleanup))))

(define (compute-next-continuation-offset block offset)
  (let ((nco (block/next-continuation-offset block offset)))
    (and nco
	 (+ (continuation-extra-length block) nco))))

(define (block/next-continuation-offset block offset)
  (if (stack-block? block)
      (let ((popping-limit (block-popping-limit block)))
	(and popping-limit
	     (let ((offset
		    (let loop ((block block) (offset offset))
		      (let ((offset (+ offset (block-frame-size block))))
			(if (eq? block popping-limit)
			    offset
			    (loop (block-parent block) offset)))))
		   (stack-link (block-stack-link popping-limit)))
	       (if (and stack-link
			(continuation-block? stack-link)
			(continuation/always-known-operator?
			 (block-procedure stack-link)))
		   (block/next-continuation-offset
		    (block-parent stack-link)
		    (+ (continuation/offset (block-procedure stack-link))
		       offset))
		   (let ((continuation
			  (lvalue-known-value
			   (stack-block/continuation-lvalue popping-limit))))
		     (if (and continuation
			      (continuation/always-known-operator?
			       continuation))
			 (block/next-continuation-offset
			  (continuation/closing-block continuation)
			  (+ (continuation/offset continuation) offset))
			 offset))))))
      offset))

(define (generate/continuation-entry/pop-extra continuation)
  (pop-continuation-extra (continuation/closing-block continuation)))

(define (continuation-extra-length closing-block)
  (cond ((ic-block? closing-block)
	 1)
	((and (stack-block? closing-block)
	      (stack-block/dynamic-link? closing-block))
	 1)
	(else
	 0)))

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
	  (set! *rgraphs* (cons rgraph *rgraphs*))
	  rgraph))))

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
	    (map->eq-set edge-right-node (rgraph-initial-edges rgraph)))
	   (protected-edges
	    (append! (map rtl-procedure/entry-edge *procedures*)
		     (map rtl-continuation/entry-edge *continuations*)
		     (map rtl-continuation/entry-edge *extra-continuations*))))
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
			   (and disallow-entries?
				(not (memq edge protected-edges)))))))))
	     (lambda (bblock)
	       (set-node-previous-edges!
		bblock
		(delete-edges! (node-previous-edges bblock))))))

	 (for-each node-mark! initial-bblocks)
	 (for-each loop initial-bblocks)
	 (for-each (delete-block-edges! false) initial-bblocks)
	 (for-each (delete-block-edges! true) result)
	 (set-rgraph-bblocks! rgraph (append! initial-bblocks result)))))))