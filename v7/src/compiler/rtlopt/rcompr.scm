#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlopt/rcompr.scm,v 1.2 1987/08/04 06:56:48 cph Exp $

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

;;;; RTL Dead Code Elimination
;;;  Based on the GNU C Compiler

(declare (usual-integrations))

(package (dead-code-elimination)

(define-export (dead-code-elimination rgraphs)
  (for-each walk-rgraph rgraphs))

(define (walk-rgraph rgraph)
  (fluid-let ((*current-rgraph* rgraph))
    (for-each walk-bblock (rgraph-bblocks rgraph))))

(define (walk-bblock bblock)
  (if (not (eq? (bblock-entry bblock) (bblock-exit bblock)))
      (let ((live (regset-copy (bblock-live-at-entry bblock)))
	    (births (make-regset (rgraph-n-registers *current-rgraph*))))
	(bblock-walk-forward bblock
	  (lambda (rnode next)
	    (if next
		(begin (optimize-rtl live rnode next)
		       (regset-clear! births)
		       (mark-set-registers! live
					    births
					    (rnode-rtl rnode)
					    false)
		       (for-each (lambda (register)
				   (regset-delete! live register))
				 (rnode-dead-registers rnode))
		       (regset-union! live births))))))))

)

(define (optimize-rtl live rnode next)
  (let ((rtl (rnode-rtl rnode)))
    (if (rtl:assign? rtl)
	(let ((address (rtl:assign-address rtl)))
	  (if (rtl:register? address)
	      (let ((register (rtl:register-number address)))
		(if (and (pseudo-register? register)
			 (= 2 (register-n-refs register))
			 (rnode-dead-register? next register)
			 (rtl:any-subexpression? (rnode-rtl next)
			   (lambda (expression)
			     (and (rtl:register? expression)
				  (= (rtl:register-number expression)
				     register)))))
		    (begin
		      (let ((dead (rnode-dead-registers rnode)))
			(for-each increment-register-live-length! dead)
			(set-rnode-dead-registers!
			 next
			 (eqv-set-union dead
					(delv! register
					       (rnode-dead-registers next)))))
		      (for-each-regset-member live 
			decrement-register-live-length!)
		      (rtl:modify-subexpressions (rnode-rtl next)
			(lambda (expression set-expression!)
			  (if (and (rtl:register? expression)
				   (= (rtl:register-number expression)
				      register))
			      (set-expression! (rtl:assign-expression rtl)))))
		      (snode-delete! rnode)
		      (reset-register-n-refs! register)
		      (reset-register-n-deaths! register)
		      (reset-register-live-length! register)
		      (set-register-next-use! register false)
		      (set-register-bblock! register false)))))))))