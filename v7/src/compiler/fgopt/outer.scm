#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/outer.scm,v 1.2 1987/10/05 20:44:28 jinx Exp $

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

;;;; Dataflow Analysis: Outer Analysis

(declare (usual-integrations))

(package (outer-analysis)

;;;; Outer analysis

;;; When this pass is completed, any combination which is known to
;;; call only known procedures contains all of the procedural
;;; arguments in its COMBINATION-PROCEDURES slot.  This is taken
;;; advantage of by the closure analysis.

(define more-unknowable-vnodes?)

(define-export (outer-analysis blocks vnodes combinations procedures
			       quotations)
  (fluid-let ((more-unknowable-vnodes? false))
    (define (loop)
      (if more-unknowable-vnodes?
	  (begin (set! more-unknowable-vnodes? false)
		 (for-each check-combination combinations)
		 (loop))))
    (for-each analyze-block blocks)
    ;; Don't bother to analyze ACCESSes now.
    (for-each (lambda (vnode)
		(if (access? vnode) (make-vnode-unknowable! vnode)))
	      vnodes)
    (for-each (lambda (quotation)
		(let ((value (quotation-value quotation)))
		  (if (vnode? value)
		      (for-each make-procedure-externally-visible!
				(vnode-procedures value)))))
	      quotations)
    (for-each prepare-combination combinations)
    (loop)))

(define (analyze-block block)
  (if (ic-block? block)
      (begin (if (block-outer? block)
		 (for-each make-vnode-externally-assignable!
			   (block-free-variables block)))
	     (for-each make-vnode-externally-accessible!
		       (block-bound-variables block)))))

(define (prepare-combination combination)
  (set-combination-procedures!
   combination
   (mapcan (lambda (operand)
	     (list-copy (subproblem-procedures operand)))
	   (combination-operands combination)))
  (if (not (null? (subproblem-values (combination-operator combination))))
      (begin (combination-operator-unknowable! combination)
	     (make-vnode-unknowable! (combination-value combination)))))

(define any-primitives?
  (there-exists? primitive-procedure-constant?))

(define (check-combination combination)
  (if (subproblem-unknowable? (combination-operator combination))
      (begin (combination-operator-unknowable! combination)
	     (make-vnode-unknowable! (combination-value combination))))
  (if (any-unknowable-subproblems? (combination-operands combination))
      (make-vnode-unknowable! (combination-value combination))))

(define any-unknowable-subproblems?
  (there-exists? subproblem-unknowable?))

(define (combination-operator-unknowable! combination)
  (let ((procedures (combination-procedures combination)))
    (set-combination-procedures! combination '())
    (for-each make-procedure-externally-visible! procedures)))

(define (make-vnode-externally-assignable! vnode)
  (make-vnode-unknowable! vnode)
  (make-vnode-externally-visible! vnode))

(define (make-vnode-externally-accessible! vnode)
  (cond ((not (memq 'CONSTANT (variable-declarations vnode)))
	 (make-vnode-externally-assignable! vnode))
	((not (vnode-externally-visible? vnode))
	 (make-vnode-externally-visible! vnode))))

(define (make-vnode-externally-visible! vnode)
  (if (not (vnode-externally-visible? vnode))
      (begin (vnode-externally-visible! vnode)
	     (for-each make-procedure-externally-visible!
		       (vnode-procedures vnode)))))

(define (make-procedure-externally-visible! procedure)
  (if (not (procedure-externally-visible? procedure))
      (begin (procedure-externally-visible! procedure)
	     (closure-procedure! procedure)
	     (for-each make-vnode-unknowable! (procedure-required procedure))
	     (for-each make-vnode-unknowable! (procedure-optional procedure))
	     (if (procedure-rest procedure)
		 ;; This is not really unknowable -- it is a list
		 ;; whose length and elements are unknowable.
		 (make-vnode-unknowable! (procedure-rest procedure)))
	     (for-each make-procedure-externally-visible!
		       (rvalue-procedures (procedure-value procedure))))))

(define (make-vnode-unknowable! vnode)
  (if (not (vnode-unknowable? vnode))
      (begin (set! more-unknowable-vnodes? true)
	     (vnode-unknowable! vnode)
	     (make-vnode-forward-links-unknowable! vnode))))

(define (make-vnode-forward-links-unknowable! vnode)
  ;; No recursion is needed here because the graph is transitively
  ;; closed, and thus the forward links of a node's forward links are
  ;; a subset of the node's forward links.
  (for-each (lambda (vnode)
	      (if (not (vnode-unknowable? vnode))
		  (begin (set! more-unknowable-vnodes? true)
			 (vnode-unknowable! vnode))))
	    (vnode-forward-links vnode)))

)