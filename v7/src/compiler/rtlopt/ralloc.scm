#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlopt/ralloc.scm,v 1.10 1987/03/19 00:46:34 cph Exp $

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

;;;; Register Allocation
;;;  Based on the GNU C Compiler

(declare (usual-integrations))

(define (register-allocation bblocks)
  ;; First, renumber all the registers remaining to be allocated.
  (let ((next-renumber 0)
	(register->renumber (make-vector *n-registers* false)))
    (define (renumbered-registers n)
      (if (< n *n-registers*)
	  (if (vector-ref register->renumber n)
	      (cons n (renumbered-registers (1+ n)))
	      (renumbered-registers (1+ n)))
	  '()))
    (for-each-pseudo-register
     (lambda (register)
       (if (positive? (register-n-refs register))
	   (begin (vector-set! register->renumber register next-renumber)
		  (set! next-renumber (1+ next-renumber))))))
    ;; Now create a conflict matrix for those registers and fill it.
    (let ((conflict-matrix
	   (make-initialized-vector next-renumber
	     (lambda (i)
	       (make-regset next-renumber)))))
      (for-each (lambda (bblock)
		  (let ((live (make-regset next-renumber)))
		    (for-each-regset-member (bblock-live-at-entry bblock)
		      (lambda (register)
			(let ((renumber
			       (vector-ref register->renumber register)))
			  (if renumber
			      (regset-adjoin! live renumber)))))
		    (bblock-walk-forward bblock
		      (lambda (rnode next)
			(for-each-regset-member live
			  (lambda (renumber)
			    (regset-union! (vector-ref conflict-matrix
						       renumber)
					   live)))
			(for-each (lambda (register)
				    (let ((renumber
					   (vector-ref register->renumber
						       register)))
				      (if renumber
					  (regset-delete! live renumber))))
				  (rnode-dead-registers rnode))
			(mark-births! live
				      (rnode-rtl rnode)
				      register->renumber)))))
		bblocks)

      ;; Finally, sort the renumbered registers into an allocation
      ;; order, and then allocate them into registers one at a time.
      ;; Return the number of required real registers as a value.
      (let ((next-allocation 0)
	    (allocated (make-vector next-renumber 0)))
	(for-each (lambda (register)
		    (let ((renumber (vector-ref register->renumber register)))
		      (define (loop allocation)
			(if (< allocation next-allocation)
			    (if (regset-disjoint?
				 (vector-ref conflict-matrix renumber)
				 (vector-ref allocated allocation))
				allocation
				(loop (1+ allocation)))
			    (let ((allocation next-allocation))
			      (set! next-allocation (1+ next-allocation))
			      (vector-set! allocated allocation
					   (make-regset next-renumber))
			      allocation)))
		      (let ((allocation (loop 0)))
			(vector-set! *register-renumber* register allocation)
			(regset-adjoin! (vector-ref allocated allocation)
					renumber))))
		  (sort (renumbered-registers number-of-machine-registers)
			allocate<?))
	next-allocation))))

(define (allocate<? x y)
  (< (/ (register-n-refs x) (register-live-length x))
     (/ (register-n-refs y) (register-live-length y))))

(define (mark-births! live rtl register->renumber)
  (if (rtl:assign? rtl)
      (let ((address (rtl:assign-address rtl)))
	(if (rtl:register? address)
	    (let ((register (rtl:register-number address)))
	      (if (pseudo-register? register)
		  (regset-adjoin! live
				  (vector-ref register->renumber
					      register))))))))