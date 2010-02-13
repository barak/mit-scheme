#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

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
		  (write bblock)
		  (newline)
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