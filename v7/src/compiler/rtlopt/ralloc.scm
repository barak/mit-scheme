#| -*-Scheme-*-

$Id: ralloc.scm,v 1.20 2003/02/14 18:28:08 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; Register Allocation
;;;  Based on the GNU C Compiler

(declare (usual-integrations))

(package (register-allocation)

(define-export (register-allocation rgraphs)
  (for-each (lambda (rgraph)
	      (let ((n-temporaries (walk-rgraph rgraph)))
		(if (> n-temporaries number-of-temporary-registers)
		    (error "Too many temporary quantities" n-temporaries))))
	    rgraphs))

(define (walk-rgraph rgraph)
  (let ((n-registers (rgraph-n-registers rgraph)))
    (set-rgraph-register-renumber!
     rgraph
     (make-vector n-registers false))
    (fluid-let ((*current-rgraph* rgraph))
      (walk-bblocks n-registers (rgraph-bblocks rgraph)))))

(define (walk-bblocks n-registers bblocks)
  ;; First, renumber all the registers remaining to be allocated.
  (let ((next-renumber 0)
	(register->renumber (make-vector n-registers false)))
    (define (renumbered-registers n)
      (if (< n n-registers)
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
	       i
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
		      (lambda (rinst)
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
				  (rinst-dead-registers rinst))
			(mark-births! live
				      (rinst-rtl rinst)
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
			(set-register-renumber! register allocation)
			(regset-adjoin! (vector-ref allocated allocation)
					renumber))))
		  (sort (renumbered-registers number-of-machine-registers)
			allocate<?))
	next-allocation))))

(define (allocate<? x y)
  (and (not (= (register-live-length x) 0))
       (or (= (register-live-length y) 0)
	   (< (/ (register-n-refs x) (register-live-length x))
	      (/ (register-n-refs y) (register-live-length y))))))

(define (mark-births! live rtl register->renumber)
  (if (rtl:assign? rtl)
      (let ((address (rtl:assign-address rtl)))
	(if (rtl:register? address)
	    (let ((register (rtl:register-number address)))
	      (if (pseudo-register? register)
		  (regset-adjoin! live
				  (vector-ref register->renumber
					      register))))))))

)