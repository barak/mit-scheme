#| -*-Scheme-*-

$Id: e4e7d6ae962143fc76a20e74c24ce8c6343d8f4e $

Copyright (c) 1987-1999 Massachusetts Institute of Technology

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

;;;; RTL Optimizer Debugging Output

(declare (usual-integrations))

(define (dump-register-info rgraph)
  (fluid-let ((*current-rgraph* rgraph))
    (for-each-pseudo-register
     (lambda (register)
       (if (positive? (register-n-refs register))
	   (begin (newline)
		  (write register)
		  (write-string ": renumber ")
		  (write (register-renumber register))
		  (write-string "; nrefs ")
		  (write (register-n-refs register))
		  (write-string "; length ")
		  (write (register-live-length register))
		  (write-string "; ndeaths ")
		  (write (register-n-deaths register))
		  (let ((bblock (register-bblock register)))
		    (cond ((eq? bblock 'NON-LOCAL)
			   (if (register-crosses-call? register)
			       (write-string "; crosses calls")
			       (write-string "; multiple blocks")))
			  (bblock
			   (write-string "; block ")
			   (write (unhash bblock)))
			  (else
			   (write-string "; no block!"))))))))))

(define (dump-block-info rgraph)
  (fluid-let ((*current-rgraph* rgraph))
    (let ((machine-regs (make-regset (rgraph-n-registers rgraph))))
      (for-each-machine-register
       (lambda (register)
	 (regset-adjoin! machine-regs register)))
      (for-each (lambda (bblock)
		  (newline)
		  (newline)
		  (write bblock)
		  (bblock-walk-forward bblock
		    (lambda (rinst)
		      (pp (rinst-rtl rinst))))
		  (let ((live-at-exit (bblock-live-at-exit bblock)))
		    (regset-difference! live-at-exit machine-regs)
		    (if (not (regset-null? live-at-exit))
			(begin (newline)
			       (write-string "Registers live at end:")
			       (for-each-regset-member live-at-exit
				 (lambda (register)
				   (write-string " ")
				   (write register)))))))
		(rgraph-bblocks rgraph)))))