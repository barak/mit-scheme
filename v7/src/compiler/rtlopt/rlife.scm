#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlopt/rlife.scm,v 1.57 1987/08/04 06:57:18 cph Exp $

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

;;;; RTL Register Lifetime Analysis
;;;  Based on the GNU C Compiler

(declare (usual-integrations))

;;;; Lifetime Analysis

(package (lifetime-analysis)

(define-export (lifetime-analysis rgraphs)
  (for-each walk-rgraph rgraphs))

(define (walk-rgraph rgraph)
  (let ((n-registers (rgraph-n-registers rgraph))
	(bblocks (rgraph-bblocks rgraph)))
    (set-rgraph-register-bblock! rgraph (make-vector n-registers false))
    (set-rgraph-register-next-use! rgraph (make-vector n-registers false))
    (set-rgraph-register-n-refs! rgraph (make-vector n-registers 0))
    (set-rgraph-register-n-deaths! rgraph (make-vector n-registers 0))
    (set-rgraph-register-live-length! rgraph (make-vector n-registers 0))
    (set-rgraph-register-crosses-call?! rgraph (make-bit-string n-registers false))
    (for-each (lambda (bblock)
		(bblock-initialize-regsets! bblock n-registers))
	      bblocks)
    (fluid-let ((*current-rgraph* rgraph))
      (walk-bblock bblocks))))

(define (walk-bblock bblocks)
  (let ((changed? false))
    (define (loop first-pass?)
      (for-each (lambda (bblock)
		  (if (or first-pass?
			  (not (regset=? (bblock-live-at-exit bblock)
					 (bblock-new-live-at-exit bblock))))
		      (begin (set! changed? true)
			     (regset-copy! (bblock-live-at-exit bblock)
					   (bblock-new-live-at-exit bblock))
			     (regset-copy! (bblock-live-at-entry bblock)
					   (bblock-live-at-exit bblock))
			     (propagate-block bblock)
			     (for-each-previous-node (bblock-entry bblock)
			       (lambda (rnode)
				 (regset-union!
				  (bblock-new-live-at-exit (node-bblock rnode))
				  (bblock-live-at-entry bblock)))))))
		bblocks)
      (if changed?
	  (begin (set! changed? false)
		 (loop false))
	  (for-each (lambda (bblock)
		      (regset-copy! (bblock-live-at-entry bblock)
				    (bblock-live-at-exit bblock))
		      (propagate-block&delete! bblock))
		    bblocks)))
    (loop true)))

)

(define (propagate-block bblock)
  (propagation-loop bblock
    (lambda (old dead live rtl rnode)
      (update-live-registers! old dead live rtl false))))

(define (propagate-block&delete! bblock)
  (for-each-regset-member (bblock-live-at-entry bblock)
    (lambda (register)
      (set-register-bblock! register 'NON-LOCAL)))
  (propagation-loop bblock
    (lambda (old dead live rtl rnode)
      (if (rtl:invocation? rtl)
	  (for-each-regset-member old register-crosses-call!))
      (if (instruction-dead? rtl old)
	  (snode-delete! rnode)
	  (begin (update-live-registers! old dead live rtl rnode)
		 (for-each-regset-member old
		   increment-register-live-length!))))))

(define (propagation-loop bblock procedure)
  (let ((old (bblock-live-at-entry bblock))
	(dead (regset-allocate (rgraph-n-registers *current-rgraph*)))
	(live (regset-allocate (rgraph-n-registers *current-rgraph*))))
    (bblock-walk-backward bblock
      (lambda (rnode previous)
	(regset-clear! dead)
	(regset-clear! live)
	(procedure old dead live (rnode-rtl rnode) rnode)))))

(define (update-live-registers! old dead live rtl rnode)
  (mark-set-registers! old dead rtl rnode)
  (mark-used-registers! old live rtl rnode)
  (regset-difference! old dead)
  (regset-union! old live))

(define (instruction-dead? rtl needed)
  (and (rtl:assign? rtl)
       (let ((address (rtl:assign-address rtl)))
	 (and (rtl:register? address)
	      (let ((register (rtl:register-number address)))
		(and (pseudo-register? register)
		     (not (regset-member? needed register))))))))

(define (mark-set-registers! needed dead rtl rnode)
  ;; **** This code safely ignores PRE-INCREMENT and POST-INCREMENT
  ;; modes, since they are only used on the stack pointer.
  (if (rtl:assign? rtl)
      (let ((address (rtl:assign-address rtl)))
	(if (interesting-register? address)
	    (let ((register (rtl:register-number address)))
	      (regset-adjoin! dead register)
	      (if rnode
		  (let ((rnode* (register-next-use register)))
		    (record-register-reference register rnode)
		    (if (and (regset-member? needed register)
			     rnode*
			     (eq? (node-bblock rnode) (node-bblock rnode*)))
			(set-rnode-logical-link! rnode* rnode)))))))))

(define (mark-used-registers! needed live rtl rnode)
  (define (loop expression)
    (if (interesting-register? expression)
	(let ((register (rtl:register-number expression)))
	  (regset-adjoin! live register)
	  (if rnode
	      (begin (record-register-reference register rnode)
		     (set-register-next-use! register rnode)
		     (if (and (not (regset-member? needed register))
			      (not (rnode-dead-register? rnode register)))
			 (begin (set-rnode-dead-registers!
				 rnode
				 (cons register
				       (rnode-dead-registers rnode)))
				(increment-register-n-deaths! register))))))
	(rtl:for-each-subexpression expression loop)))
  (if (and (rtl:assign? rtl)
	   (rtl:register? (rtl:assign-address rtl)))
      (if (let ((register (rtl:register-number (rtl:assign-address rtl))))
	    (or (machine-register? register)
		(regset-member? needed register)))
	  (loop (rtl:assign-expression rtl)))
      (rtl:for-each-subexpression rtl loop)))

(define (record-register-reference register rnode)
  (let ((bblock (node-bblock rnode))
	(bblock* (register-bblock register)))
    (cond ((not bblock*)
	   (set-register-bblock! register bblock))
	  ((not (eq? bblock bblock*))
	   (set-register-bblock! register 'NON-LOCAL)))
    (increment-register-n-refs! register)))

(define (interesting-register? expression)
  (and (rtl:register? expression)
       (pseudo-register? (rtl:register-number expression))))