#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlopt/rdebug.scm,v 1.2 1987/08/07 17:08:01 cph Rel $

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