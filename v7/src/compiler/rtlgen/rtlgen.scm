#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlgen/rtlgen.scm,v 1.19 1987/08/08 23:19:11 cph Exp $

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

(define (generate-rtl quotation procedures)
  (generate/rgraph
   (quotation-rgraph quotation)
   (lambda ()
     (scfg*scfg->scfg!
      (rtl:make-assignment register:frame-pointer
			   (rtl:make-fetch register:stack-pointer))
      (generate/node (let ((entry (quotation-fg-entry quotation)))
		       (if (not compiler:preserve-data-structures?)
			   (unset-quotation-fg-entry! quotation))
		       entry)
		     false))))
  (for-each (lambda (procedure)
	      (generate/rgraph
	       (procedure-rgraph procedure)
	       (lambda ()
		 (generate/procedure-header
		  procedure
		  (generate/node
		   (let ((entry (procedure-fg-entry procedure)))
		     (if (not compiler:preserve-data-structures?)
			 (unset-procedure-fg-entry! procedure))
		     entry)
		   false)))))
	    procedures))

(define (generate/rgraph rgraph generator)
  (fluid-let ((*current-rgraph* rgraph)
	      (*next-pseudo-number* number-of-machine-registers)
	      (*temporary->register-map* '())
	      (*memoizations* '()))
    (set-rgraph-edge!
     rgraph
     (node->edge (cfg-entry-node (with-new-node-marks generator))))
    (set-rgraph-n-registers! rgraph *next-pseudo-number*))
   (with-new-node-marks
    (lambda ()
      (for-each (lambda (edge)
		  (bblock-compress! (edge-right-node edge)))
		(rgraph-initial-edges rgraph))))
  (set-rgraph-bblocks!
   rgraph
   (with-new-node-marks
    (lambda ()
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

      (mapcan (lambda (edge)
		(loop (edge-right-node edge)))
	      (rgraph-initial-edges rgraph))))))

(define *memoizations*)

(define (generate/node node subproblem?)
  ;; This won't work when there are loops in the FG.
  (cond ((or (null? (node-previous-edges node))
	     (null? (cdr (node-previous-edges node))))
	 (node-mark! node)
	 ((vector-method node generate/node) node subproblem?))
	((not (node-marked? node))
	 (node-mark! node)
	 (let ((result ((vector-method node generate/node) node subproblem?)))
	   (set! *memoizations*
		 (cons (cons* node subproblem? result)
		       *memoizations*))
	   result))
	(else
	 (let ((memoization
		(cdr (or (assq node *memoizations*)
			 (error "Marked node lacking memoization" node)))))
	   (if (not (boolean=? (car memoization) subproblem?))
	       (error "Node regenerated with different arguments" node))
	   (cdr memoization)))))

(define (define-generator tag generator)
  (define-vector-method tag generate/node generator))

(define (define-statement-generator tag generator)
  (define-generator tag (normal-statement-generator generator)))

(define (normal-statement-generator generator)
  (lambda (node subproblem?)
    (generate/normal-statement node subproblem? generator)))

(define (generate/normal-statement node subproblem? generator)
  (let ((next (snode-next node)))
    (if next
	(scfg*scfg->scfg! (generator node true)
			  (generate/node next subproblem?))
	(generator node subproblem?))))

(define (define-predicate-generator tag generator)
  (define-generator tag (normal-predicate-generator generator)))

(define (normal-predicate-generator generator)
  (lambda (node subproblem?)
    (pcfg*scfg->scfg!
     (generator node)
     (let ((consequent (pnode-consequent node)))
       (and consequent
	    (generate/node consequent subproblem?)))
     (let ((alternative (pnode-alternative node)))
       (and alternative
	    (generate/node alternative subproblem?))))))

(define (generate/subproblem-cfg subproblem)
  (if (cfg-null? (subproblem-cfg subproblem))
      (make-null-cfg)
      (generate/node (cfg-entry-node (subproblem-cfg subproblem)) true)))

(define (generate/operand subproblem)
  (transmit-values (generate/rvalue (subproblem-value subproblem))
    (lambda (prefix expression)
      (return-3 (generate/subproblem-cfg subproblem)
		prefix
		expression))))

(define (generate/subproblem subproblem)
  (transmit-values (generate/operand subproblem)
    (lambda (cfg prefix expression)
      (return-2 (scfg*scfg->scfg! cfg prefix) expression))))

(define (generate/subproblem-push subproblem)
  (transmit-values (generate/subproblem subproblem)
    (lambda (cfg expression)
      (scfg*scfg->scfg! cfg (rtl:make-push expression)))))