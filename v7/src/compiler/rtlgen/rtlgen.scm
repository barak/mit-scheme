#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlgen/rtlgen.scm,v 1.13 1987/05/07 04:38:32 cph Exp $

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

(define (generate-rtl quotations procedures)
  (with-new-node-marks
   (lambda ()
     (for-each generate/quotation quotations)
     (for-each generate/procedure procedures))))

(define (generate/quotation quotation)
  (set-quotation-rtl-entry!
   quotation
   (cfg-entry-node
    (scfg*scfg->scfg!
     (rtl:make-assignment register:frame-pointer
			  (rtl:make-fetch register:stack-pointer))
     (generate/node (quotation-fg-entry quotation) false)))))

(define (generate/procedure procedure)
  (set-procedure-rtl-entry!
   procedure
   (cfg-entry-node
    (generate/procedure-header procedure
			       (generate/node (procedure-fg-entry procedure)
					      false)))))

(define (generate/node node subproblem?)
  ;; This won't work when there are loops in the RTL.
  (cond ((not (node-marked? node))
	 (node-mark! node)
	 (set-node-rtl-arguments! node subproblem?)
	 (let ((result ((vector-method node generate/node) node subproblem?)))
	   (set-node-rtl-result! node result)
	   result))
	(else
	 (if (not (boolean=? (node-rtl-arguments node) subproblem?))
	     (error "Node regenerated with different arguments" node))
	 (node-rtl-result node))))

(define (define-generator tag generator)
  (define-vector-method tag generate/node generator))

(define (generate/subproblem-cfg subproblem)
  (if (cfg-null? (subproblem-cfg subproblem))
      (make-null-cfg)
      (generate/node (cfg-entry-node (subproblem-cfg subproblem)) true)))

(define (generate/subproblem subproblem)
  ;; The subproblem-cfg must be generated before the subproblem-value,
  ;; because if it is a combination, the combination-value must be
  ;; marked as a value-temporary before the code for referencing it
  ;; can be generated.
  (let ((cfg (generate/subproblem-cfg subproblem)))
    (transmit-values (generate/rvalue (subproblem-value subproblem))
      (lambda (prefix expression)
	(return-2 (scfg*scfg->scfg! cfg prefix)
		  expression)))))

(define (generate/subproblem-push subproblem)
  (transmit-values (generate/subproblem subproblem)
    (lambda (cfg expression)
      (scfg*scfg->scfg! cfg (rtl:make-push expression)))))

(define (define-statement-generator tag generator)
  (define-generator tag (normal-statement-generator generator)))

(define (normal-statement-generator generator)
  (lambda (node subproblem?)
    (generate/normal-statement node subproblem? generator)))

(define (generate/normal-statement node subproblem? generator)
  (if (snode-next node)
      ;; Due to the side-effect on combination-value temporaries, we
      ;; must generate the nodes in the control flow order.
      (let ((cfg (generator node true)))
	(scfg*scfg->scfg! cfg
			  (generate/node (snode-next node) subproblem?)))
      (generator node subproblem?)))

(define (define-predicate-generator tag generator)
  (define-generator tag (normal-predicate-generator generator)))

(define (normal-predicate-generator generator)
  (lambda (node subproblem?)
    ;; Due to the side-effect on combination-value temporaries, we
    ;; must generate the nodes in the control flow order.
    (let ((cfg (generator node)))
      (pcfg*scfg->scfg!
       cfg
       (generate/node (pnode-consequent node) subproblem?)
       (generate/node (pnode-alternative node) subproblem?)))))

(define-integrable (node-rtl-result node)
  (node-property-get node tag/node-rtl-result))

(define-integrable (set-node-rtl-result! node cfg)
  (node-property-put! node tag/node-rtl-result cfg))

(define tag/node-rtl-result
  "node rtl result")

(define-integrable (node-rtl-arguments node)
  (node-property-get node tag/node-rtl-arguments))

(define-integrable (set-node-rtl-arguments! node arguments)
  (node-property-put! node tag/node-rtl-arguments arguments))

(define tag/node-rtl-arguments
  "node rtl arguments")