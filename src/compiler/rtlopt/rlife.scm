#| -*-Scheme-*-

$Id: rlife.scm,v 1.65 2007/01/05 21:19:23 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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