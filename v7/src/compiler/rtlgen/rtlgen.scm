#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlgen/rtlgen.scm,v 4.1 1987/12/04 20:32:02 cph Exp $

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
(define *memoizations*)

(define (generate/top-level expression)
  (with-machine-register-map
   (lambda ()
     (fluid-let ((*generation-queue* (make-queue))
		 (*queued-procedures* '())
		 (*queued-continuations* '())
		 (*memoizations* '()))
       (set! *rtl-expression* (generate/expression expression))
       (queue-map! *generation-queue* (lambda (thunk) (thunk)))
       (set! *rtl-graphs*
	     (list-transform-positive (reverse! *rtl-graphs*)
	       (lambda (rgraph)
		 (not (null? (rgraph-entry-edges rgraph))))))
       (for-each rgraph/compress! *rtl-graphs*)
       (set! *rtl-procedures* (reverse! *rtl-procedures*))
       (set! *rtl-continuations* (reverse! *rtl-continuations*))))))

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
      (generate/rgraph
       (lambda ()
	 (generate/node (expression-entry-node expression) 0)))
    (lambda (rgraph entry-edge)
      (make-rtl-expr rgraph (expression-label expression) entry-edge))))

(define (generate/procedure procedure)
  (transmit-values
      (generate/rgraph
       (lambda ()
	 (generate/procedure-header
	  procedure
	  (generate/node (procedure-entry-node procedure) 0)
	  false)))
    (lambda (rgraph entry-edge)
      (make-rtl-procedure
       rgraph
       (procedure-label procedure)
       entry-edge
       (length (procedure-original-required procedure))
       (length (procedure-original-optional procedure))
       (and (procedure-original-rest procedure) true)
       (and (procedure/closure? procedure) true)))))

(define (generate/procedure-entry/inline procedure)
  (generate/procedure-header procedure
			     (generate/node (procedure-entry-node procedure) 0)
			     true))

(define (generate/continuation continuation)
  (let ((label (continuation/label continuation))
	(node (continuation/entry-node continuation))
	(offset (continuation/offset continuation)))
    (transmit-values
	(generate/rgraph
	 (lambda ()
	   (scfg-append!
	    (rtl:make-continuation-heap-check label)
	    (generate/continuation-entry/ic-block continuation)
	    (enumeration-case continuation-type
		(continuation/type continuation)
	      ((PUSH)
	       (scfg*scfg->scfg!
		(rtl:make-push (rtl:make-fetch register:value))
		(generate/node node (1+ offset))))
	      ((REGISTER)
	       (scfg*scfg->scfg!
		(rtl:make-assignment (continuation/register continuation)
				     (rtl:make-fetch register:value))
		(generate/node node offset)))
	      (else
	       (generate/node node offset))))))
      (lambda (rgraph entry-edge)
	(make-rtl-continuation rgraph label entry-edge)))))

(define (generate/continuation-entry/ic-block continuation)
  (if (ic-block? (continuation/closing-block continuation))
      (rtl:make-pop register:environment)
      (make-null-cfg)))

(define (generate/node/memoize node offset)
  (let ((entry (assq node *memoizations*)))
    (cond ((not entry)
	   (let ((entry (cons node false)))
	     (set! *memoizations* (cons entry *memoizations*))
	     (let ((result (generate/node node offset)))
	       (set-cdr! entry (cons offset result))
	       result)))
	  ((not (cdr entry))
	   (error "GENERATE/NODE/MEMOIZE: loop" node))
	  ((not (= offset (cadr entry)))
	   (error "GENERATE/NODE/MEMOIZE: mismatched offsets" node))
	  (else (cddr entry)))))

(define (generate/node node offset)
  (cfg-node-case (tagged-vector/tag node)
    ((APPLICATION)
     (if (snode-next node)
	 (error "Application node has next" node))
     (case (application-type node)
       ((COMBINATION) (generate/combination node offset))
       ((RETURN) (generate/return node offset))
       (else (error "Unknown application type" node))))
    ((VIRTUAL-RETURN)
     (transmit-values (generate/virtual-return node offset)
       (lambda (scfg offset)
	 (scfg*scfg->scfg! scfg
			   (generate/node (snode-next node) offset)))))
    ((POP)
     (scfg*scfg->scfg! (generate/pop node offset)
		       (generate/node (snode-next node) offset)))
    ((ASSIGNMENT)
     (scfg*scfg->scfg! (generate/assignment node offset)
		       (generate/node (snode-next node) offset)))
    ((DEFINITION)
     (scfg*scfg->scfg! (generate/definition node offset)
		       (generate/node (snode-next node) offset)))
    ((TRUE-TEST)
     (generate/true-test node offset))))

(define (generate/rgraph generator)
  (let ((rgraph (make-rgraph number-of-machine-registers)))
    (set! *rtl-graphs* (cons rgraph *rtl-graphs*))
    (let ((entry-node
	   (cfg-entry-node
	    (fluid-let ((*current-rgraph* rgraph))
	      (with-new-node-marks generator)))))
      (add-rgraph-entry-node! rgraph entry-node)
      (return-2 rgraph (node->edge entry-node)))))

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