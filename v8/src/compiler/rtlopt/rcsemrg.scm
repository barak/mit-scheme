#| -*-Scheme-*-

$Id: rcsemrg.scm,v 1.1 1994/11/19 02:06:38 adams Exp $

Copyright (c) 1994 Massachusetts Institute of Technology

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

;;;; RTL CSE merge
;;; package: (compiler rtl-cse)

(declare (usual-integrations))

;;; For now, this is really dumb.
;;; It takes the intersection of the states.
;;; A better solution is to check whether a subexpression is redundant
;;; with one of the predecessors, and if so, insert it into the other
;;; predecessors.  In order to avoid code blow-up a distinguished predecessor
;;; can be chosen, and the rest can be intersected in the usual way.
;;; Then there is no net code growth (except for perhaps one branch instr.)
;;; because the expression would have been computed anyway after the merge.

(define (state/merge* infos)
  ;; each info is either #F (predecessor not yet processed),
  ;; or a list of a bblock a state, and a flag signalling whether
  ;; there are preserved registers in the bblock.
  ;; #F only occurs when a predecessor has not been processed,
  ;; which can only occur when there is a loop in the flow graph.
  (if (there-exists? infos (lambda (pair) (not (pair? pair))))
      ;; Loop in flow graph.  For now, flush everything.
      (state/make-empty)
      (let ((states (map cadr infos)))
	(state/set! (state/copy (car states)))
	(let loop ((states (cdr states)))
	  (if (null? states)
	      (state/get)
	      (begin
		(state/merge! (car states))
		(loop (cdr states))))))))

(define (state/merge! state)
  (register-tables/merge! *register-tables*
			  (state/register-tables state))
  ;; For now, drop all stack references
  (set! *stack-offset* 0)
  (set! *stack-reference-quantities* '())
  unspecific)

(define (register-tables/merge! tables tables*)
  (define (%register-invalidate! reg)
    (let ((expression (register-expression reg)))
      (if expression
	  (register-expression-invalidate! expression))))

  (define (quantity-registers tables quantity)
    (let loop ((reg (quantity-first-register quantity))
	       (all '()))
      (if (not reg)
	  all
	  (loop (%register-next-equivalent tables reg)
		(cons reg all)))))

  (let ((n-registers (vector-length (vector-ref tables 0)))
	(quantities (vector-ref tables 0))
	(quantities* (vector-copy (vector-ref tables* 0))))
    (do ((reg 0 (+ reg 1)))
	((>= reg n-registers))
      (let ((quantity (vector-ref quantities reg))
	    (quantity* (vector-ref quantities* reg)))
	(cond ((or (not quantity)
		   ;; Already merged
		   (eq? quantity quantity*)))
	      ((or (not quantity*)
		   ;; This could check if the expressions happened
		   ;; to be the same!
		   (not (= (quantity-number quantity)
			   (quantity-number quantity*))))
	       (%register-invalidate! reg))
	      (else
	       ;; Merge the quantities
	       (let ((regs (quantity-registers tables quantity))
		     (regs* (quantity-registers tables* quantity*)))
		 (for-each %register-invalidate!
			   (eq-set-difference regs regs*))
		 (for-each (lambda (reg)
			     (vector-set! quantities* reg quantity))
			   regs*))))))))