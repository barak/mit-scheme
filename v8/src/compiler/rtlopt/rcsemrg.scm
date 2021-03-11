#| -*-Scheme-*-

Copyright (c) 1994, 1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

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