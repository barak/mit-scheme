;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1986 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; RTL Register Lifetime Analysis
;;;  Based on the GNU C Compiler

;;; $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlopt/rlife.scm,v 1.50 1986/12/15 05:27:44 cph Exp $

(declare (usual-integrations))
(using-syntax (access compiler-syntax-table compiler-package)

;;;; Basic Blocks

(define *blocks*)
(define *block-number*)

(define (find-blocks rnodes)
  (fluid-let ((*generation* (make-generation))
	      (*blocks* '())
	      (*block-number* 0))
    (for-each (lambda (rnode)
		(set-node-generation! rnode *generation*))
	      rnodes)
    (for-each walk-entry rnodes)
    *blocks*))

(define (walk-next next)
  (if (not (eq? (node-generation next) *generation*))
      (walk-entry next)))

(define (walk-entry rnode)
  (let ((bblock (make-bblock *block-number* rnode *n-registers*)))
    (set! *block-number* (1+ *block-number*))
    (set! *blocks* (cons bblock *blocks*))
    (walk-rnode bblock rnode)))

(define (walk-rnode bblock rnode)
  (set-node-generation! rnode *generation*)
  (set-rnode-bblock! rnode bblock)
  ((vector-method rnode walk-rnode) bblock rnode))

(define-vector-method rtl-snode-tag walk-rnode
  (lambda (bblock snode)
    (let ((next (snode-next snode)))
      (cond ((not next)
	     (set-bblock-exit! bblock snode))
	    ((or (node-previous>1? next)
		 (rtl:invocation? (rnode-rtl snode)))
	     (set-bblock-exit! bblock snode)
	     (walk-next next))
	    (else
	     (walk-rnode bblock next))))))

(define-vector-method rtl-pnode-tag walk-rnode
  (lambda (bblock pnode)
    (set-bblock-exit! bblock pnode)
    (walk-next (pnode-consequent pnode))
    (walk-next (pnode-alternative pnode))))

;;;; Lifetime Analysis

(define (lifetime-analysis bblocks)
  (let ((changed? false))
    (define (loop first-pass?)
      (for-each (lambda (bblock)
		  (let ((live-at-entry (bblock-live-at-entry bblock))
			(live-at-exit (bblock-live-at-exit bblock))
			(new-live-at-exit (bblock-new-live-at-exit bblock)))
		    (if (or first-pass?
			    (not (regset=? live-at-exit new-live-at-exit)))
			(begin (set! changed? true)
			       (regset-copy! live-at-exit new-live-at-exit)
			       (regset-copy! live-at-entry live-at-exit)
			       (propagate-block bblock)
			       (for-each-previous-node (bblock-entry bblock)
				 (lambda (rnode)
				   (regset-union! (bblock-new-live-at-exit
						   (rnode-bblock rnode))
						  live-at-entry)))))))
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
	  (rtl-snode-delete! rnode)
	  (begin (update-live-registers! old dead live rtl rnode)
		 (for-each-regset-member old
		   increment-register-live-length!))))))

(define (propagation-loop bblock procedure)
  (let ((old (bblock-live-at-entry bblock))
	(dead (regset-allocate *n-registers*))
	(live (regset-allocate *n-registers*)))
    (walk-bblock-backward bblock
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
			     (eq? (rnode-bblock rnode) (rnode-bblock rnode*)))
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
  (let ((bblock (rnode-bblock rnode))
	(bblock* (register-bblock register)))
    (cond ((not bblock*)
	   (set-register-bblock! register bblock))
	  ((not (eq? bblock bblock*))
	   (set-register-bblock! register 'NON-LOCAL)))
    (increment-register-n-refs! register)))

(define (interesting-register? expression)
  (and (rtl:register? expression)
       (pseudo-register? (rtl:register-number expression))))

;;;; Optimization

(define (optimize-block bblock)
  (if (not (eq? (bblock-entry bblock) (bblock-exit bblock)))
      (let ((live (regset-copy (bblock-live-at-entry bblock)))
	    (births (make-regset *n-registers*)))
	(walk-bblock-forward bblock
	  (lambda (rnode next)
	    (if next
		(begin (optimize-rtl live rnode next)
		       (regset-clear! births)
		       (mark-set-registers! live births (rnode-rtl rnode)
					    false)
		       (for-each (lambda (register)
				   (regset-delete! live register))
				 (rnode-dead-registers rnode))
		       (regset-union! live births))))))))

(define (rtl-snode-delete! rnode)
  (bblock-edit! (rnode-bblock rnode)
    (lambda ()
      (snode-delete! rnode))))

(define (bblock-edit! bblock thunk)
  (if (rtl-pnode? (bblock-exit bblock))
      (let ((entry (make-entry-holder)))
	(entry-holder-connect! entry (bblock-entry bblock))
	(thunk)
	(set-bblock-entry! bblock (entry-holder-disconnect! entry)))
      (let ((entry (make-entry-holder))
	    (exit (make-exit-holder)))
	(entry-holder-connect! entry (bblock-entry bblock))
	(snode-next-connect! (bblock-exit bblock) exit)
	(thunk)
	(let ((next (entry-holder-disconnect! entry))
	      (hooks (node-previous-disconnect! exit)))
	  (if next
	      (begin (set-bblock-entry! bblock next)
		     (set-bblock-exit! bblock (hook-node (car hooks)))))))))

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
			 (set-union dead
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
		      (rtl-snode-delete! rnode)
		      (reset-register-n-refs! register)
		      (reset-register-n-deaths! register)
		      (reset-register-live-length! register)
		      (set-register-next-use! register false)
		      (set-register-bblock! register false)))))))))

(define set-union
  (let ()
    (define (loop x y)
      (if (null? x)
	  y
	  (loop (cdr x)
		(if (memv (car x) y)
		    y
		    (cons (car x) y)))))
    (named-lambda (set-union x y)
      (if (null? y)
	  x
	  (loop x y)))))

;;;; Debugging Output

(define (dump-register-info)
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
			 (write (bblock-number bblock)))
			(else
			 (write-string "; no block!")))))))))

(define (dump-block-info bblocks)
  (let ((null-set (make-regset *n-registers*))
	(machine-regs (make-regset *n-registers*)))
    (for-each-machine-register
     (lambda (register)
       (regset-adjoin! machine-regs register)))
    (for-each (lambda (bblock)
		(newline)
		(newline)
		(write bblock)
		(let ((exit (bblock-exit bblock)))
		  (let loop ((rnode (bblock-entry bblock)))
		    (pp (rnode-rtl rnode))
		    (if (not (eq? rnode exit))
			(loop (snode-next rnode)))))
		(let ((live-at-exit (bblock-live-at-exit bblock)))
		  (regset-difference! live-at-exit machine-regs)
		  (if (not (regset=? null-set live-at-exit))
		      (begin (newline)
			     (write-string "Registers live at end:")
			     (for-each-regset-member live-at-exit
			       (lambda (register)
				 (write-string " ")
				 (write register)))))))
	      (reverse bblocks))))

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: (access rtl-analyzer-package compiler-package)
;;; Scheme Syntax Table: (access compiler-syntax-table compiler-package)
;;; Tags Table Pathname: (access compiler-tags-pathname compiler-package)
;;; End:
       (pseudo-register? (rtl:register-number expression))))