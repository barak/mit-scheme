#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlopt/rlife.scm,v 1.60 1988/12/15 17:27:22 cph Rel $

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

(define (lifetime-analysis rgraphs)
  (for-each walk-rgraph rgraphs))

(define (walk-rgraph rgraph)
  (let ((n-registers (rgraph-n-registers rgraph))
	(bblocks (rgraph-bblocks rgraph)))
    (set-rgraph-register-bblock! rgraph (make-vector n-registers false))
    (set-rgraph-register-n-refs! rgraph (make-vector n-registers 0))
    (set-rgraph-register-n-deaths! rgraph (make-vector n-registers 0))
    (set-rgraph-register-live-length! rgraph (make-vector n-registers 0))
    (set-rgraph-register-crosses-call?! rgraph
					(make-bit-string n-registers false))
    (for-each (lambda (bblock)
		(set-bblock-live-at-entry! bblock (make-regset n-registers))
		(set-bblock-live-at-exit! bblock (make-regset n-registers))
		(set-bblock-new-live-at-exit! bblock
					      (make-regset n-registers)))
	      bblocks)
    (fluid-let ((*current-rgraph* rgraph))
      (walk-bblocks bblocks))
    (for-each (lambda (bblock)
		(set-bblock-new-live-at-exit! bblock false))
	      (rgraph-bblocks rgraph))))

(define (walk-bblocks bblocks)
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
			     (for-each-previous-node bblock
			       (lambda (bblock*)
				 (regset-union!
				  (bblock-new-live-at-exit bblock*)
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

(define (propagate-block bblock)
  (propagation-loop bblock
    (lambda (dead live rinst)
      (update-live-registers! (bblock-live-at-entry bblock)
			      dead
			      live
			      (rinst-rtl rinst)
			      false false))))

(define (propagate-block&delete! bblock)
  (for-each-regset-member (bblock-live-at-entry bblock)
    (lambda (register)
      (set-register-bblock! register 'NON-LOCAL)))
  (propagation-loop bblock
    (lambda (dead live rinst)
      (let ((rtl (rinst-rtl rinst))
	    (old (bblock-live-at-entry bblock)))
	(if (rtl:invocation? rtl)
	    (for-each-regset-member old register-crosses-call!))
	(if (instruction-dead? rtl old)
	    (set-rinst-rtl! rinst false)
	    (begin
	      (update-live-registers! old dead live rtl bblock rinst)
	      (for-each-regset-member old increment-register-live-length!))))))
  (bblock-perform-deletions! bblock))

(define (propagation-loop bblock procedure)
  (let ((dead (regset-allocate (rgraph-n-registers *current-rgraph*)))
	(live (regset-allocate (rgraph-n-registers *current-rgraph*))))
    (bblock-walk-backward bblock
      (lambda (rinst)
	(regset-clear! dead)
	(regset-clear! live)
	(procedure dead live rinst)))))

(define (update-live-registers! old dead live rtl bblock rinst)
  (mark-set-registers! old dead rtl bblock)
  (mark-used-registers! old live rtl bblock rinst)
  (regset-difference! old dead)
  (regset-union! old live))

(define (mark-set-registers! needed dead rtl bblock)
  ;; **** This code safely ignores PRE-INCREMENT and POST-INCREMENT
  ;; modes, since they are only used on the stack pointer.
  needed
  (if (rtl:assign? rtl)
      (let ((address (rtl:assign-address rtl)))
	(if (interesting-register? address)
	    (let ((register (rtl:register-number address)))
	      (regset-adjoin! dead register)
	      (if bblock (record-register-reference register bblock)))))))

(define (mark-used-registers! needed live rtl bblock rinst)
  (define (loop expression)
    (if (interesting-register? expression)
	(let ((register (rtl:register-number expression)))
	  (regset-adjoin! live register)
	  (if bblock
	      (begin (record-register-reference register bblock)
		     (if (and (not (regset-member? needed register))
			      (not (rinst-dead-register? rinst register)))
			 (begin (set-rinst-dead-registers!
				 rinst
				 (cons register
				       (rinst-dead-registers rinst)))
				(increment-register-n-deaths! register))))))
	(rtl:for-each-subexpression expression loop)))
  (if (and (rtl:assign? rtl)
	   (rtl:register? (rtl:assign-address rtl)))
      (if (let ((register (rtl:register-number (rtl:assign-address rtl))))
	    (or (machine-register? register)
		(regset-member? needed register)))
	  (loop (rtl:assign-expression rtl)))
      (rtl:for-each-subexpression rtl loop)))

(define (record-register-reference register bblock)
  (let ((bblock* (register-bblock register)))
    (cond ((not bblock*)
	   (set-register-bblock! register bblock))
	  ((not (eq? bblock bblock*))
	   (set-register-bblock! register 'NON-LOCAL)))
    (increment-register-n-refs! register)))

(define (instruction-dead? rtl needed)
  (and (rtl:assign? rtl)
       (let ((address (rtl:assign-address rtl)))
	 (and (rtl:register? address)
	      (let ((register (rtl:register-number address)))
		(and (pseudo-register? register)
		     (not (regset-member? needed register))))))
       (not (rtl:expression-contains? (rtl:assign-expression rtl)
				      rtl:volatile-expression?))))

(define (interesting-register? expression)
  (and (rtl:register? expression)
       (pseudo-register? (rtl:register-number expression))))