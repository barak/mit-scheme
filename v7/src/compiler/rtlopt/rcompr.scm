#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlopt/rcompr.scm,v 1.7 1988/08/30 02:13:14 cph Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

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

;;;; RTL Compression
;;;  Based on the GNU C Compiler

(declare (usual-integrations))

(package (code-compression)

(define-export (code-compression rgraphs)
  (for-each (lambda (rgraph)
	      (fluid-let ((*current-rgraph* rgraph))
		(for-each walk-bblock (rgraph-bblocks rgraph))))
	    rgraphs))

(define (walk-bblock bblock)
  (if (rinst-next (bblock-instructions bblock))
      (begin
	(let ((live (regset-copy (bblock-live-at-entry bblock)))
	      (births (make-regset (rgraph-n-registers *current-rgraph*))))
	  (bblock-walk-forward bblock
	    (lambda (rinst)
	      (if (rinst-next rinst)
		  (let ((rtl (rinst-rtl rinst)))
		    (optimize-rtl bblock live rinst rtl)
		    (regset-clear! births)
		    (mark-set-registers! live births rtl false)
		    (for-each (lambda (register)
				(regset-delete! live register))
			      (rinst-dead-registers rinst))
		    (regset-union! live births))))))
	(bblock-perform-deletions! bblock))))

(define (optimize-rtl bblock live rinst rtl)
  ;; Look for assignments whose address is a pseudo register.  If that
  ;; register has exactly one reference which is known to be in this
  ;; basic block, it is a candidate for expression folding.
  (let ((register
	 (and (rtl:assign? rtl)
	      (let ((address (rtl:assign-address rtl)))
		(and (rtl:register? address)
		     (rtl:register-number address))))))
    (if (and register
	     (pseudo-register? register)
	     (eq? (register-bblock register) bblock)
	     (= 2 (register-n-refs register)))
	(find-reference-instruction live
				    rinst
				    register
				    (rtl:assign-expression rtl)))))

(define (fold-instructions! live rinst next register expression)
  ;; Attempt to fold `expression' into the place of `register' in the
  ;; RTL instruction `next'.  If the resulting instruction is
  ;; reasonable (i.e. if the LAP generator informs us that it has a
  ;; pattern for generating that instruction), the folding is
  ;; performed.
  (let ((rtl (rinst-rtl next)))
    (if (rtl:refers-to-register? rtl register)
	(let ((rtl (rtl:subst-register rtl register expression)))
	  (if (lap-generator/match-rtl-instruction rtl)
	      (begin
		(set-rinst-rtl! rinst false)
		(set-rinst-rtl! next rtl)
		(let ((dead (rinst-dead-registers rinst)))
		  (for-each increment-register-live-length! dead)
		  (set-rinst-dead-registers!
		   next
		   (eqv-set-union dead
				  (delv! register
					 (rinst-dead-registers next)))))
		(for-each-regset-member live decrement-register-live-length!)
		(reset-register-n-refs! register)
		(reset-register-n-deaths! register)
		(reset-register-live-length! register)
		(set-register-bblock! register false)))))))

(define (find-reference-instruction live rinst register expression)
  ;; Find the instruction which contains the single reference to
  ;; `register', and determine if it is possible to fold `expression'
  ;; into that instruction in `register's place.
  (let ((next (rinst-next rinst)))
    (let ((search-stopping-at
	   (lambda (predicate)
	     (let loop ((next next))
	       (if (not (predicate (rinst-rtl next)))
		   (let ((next (rinst-next next)))
		     (if (rinst-dead-register? next register)
			 (fold-instructions! live rinst next register
					     expression)
			 (loop next))))))))
      (cond ((rinst-dead-register? next register)
	     (fold-instructions! live rinst next register expression))
	    ((interpreter-value-register? expression)
	     (search-stopping-at
	      (lambda (rtl)
		(and (rtl:assign? rtl)
		     (interpreter-value-register? (rtl:assign-address rtl))))))
	    ((rtl:stack-reference? expression)
	     (search-stopping-at expression-clobbers-stack-pointer?))
	    ((rtl:constant-expression? expression)
	     (let loop ((next (rinst-next next)))
	       (if (rinst-dead-register? next register)
		   (fold-instructions! live rinst next register expression)
		   (loop (rinst-next next)))))))))

(define (rtl:stack-reference? expression)
  (and (rtl:offset? expression)
       (interpreter-stack-pointer? (rtl:offset-register expression))))

(define (expression-clobbers-stack-pointer? rtl)
  (or (and (rtl:assign? rtl)
	   (rtl:register? (rtl:assign-address rtl))
	   (interpreter-stack-pointer? (rtl:assign-address rtl)))
      ;; This should also test for all invocations, and
      ;; pop-return as well, but those never have a next
      ;; instruction.
      (memq (rtl:expression-type rtl)
	    '(INVOCATION-PREFIX:MOVE-FRAME-UP
	      INVOCATION-PREFIX:DYNAMIC-LINK))
      (let loop ((expression rtl))
	(rtl:any-subexpression? expression
	  (lambda (expression)
	    (cond ((rtl:pre-increment? expression)
		   (interpreter-stack-pointer?
		    (rtl:pre-increment-register expression)))
		  ((rtl:post-increment? expression)
		   (interpreter-stack-pointer?
		    (rtl:post-increment-register expression)))
		  (else
		   (loop expression))))))))

)