#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlopt/rcompr.scm,v 1.8 1988/12/12 21:30:30 cph Rel $

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

(define (code-compression rgraphs)
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
	(let ((expression (rtl:assign-expression rtl)))
	  (if (not (rtl:expression-contains? expression
					     rtl:volatile-expression?))
	      (let ((next
		     (find-reference-instruction (rinst-next rinst)
						 register
						 expression)))
		(if next
		    (fold-instructions! live
					rinst
					next
					register
					expression))))))))

(define (find-reference-instruction next register expression)
  ;; Find the instruction which contains the single reference to
  ;; `register', and determine if it is possible to fold `expression'
  ;; into that instruction in `register's place.
  (let ((search-stopping-at
	 (lambda (predicate)
	   (define (phi-1 next)
	     (and (not (predicate (rinst-rtl next)))
		  (phi-2 (rinst-next next))))
	   (define (phi-2 next)
	     (if (rinst-dead-register? next register)
		 next
		 (phi-1 next)))
	   (phi-1 next))))
    (cond ((rinst-dead-register? next register) next)
	  ((interpreter-value-register? expression)
	   (search-stopping-at
	    (lambda (rtl)
	      (and (rtl:assign? rtl)
		   (interpreter-value-register? (rtl:assign-address rtl))))))
	  ((rtl:stack-reference-expression? expression)
	   (search-stopping-at
	    (lambda (rtl)
	      (or (and (rtl:assign? rtl)
		       (equal? (rtl:assign-address rtl) expression))
		  (expression-clobbers-stack-pointer? rtl)))))
	  ((rtl:constant-expression? expression)
	   (let loop ((next (rinst-next next)))
	     (if (rinst-dead-register? next register)
		 next
		 (loop (rinst-next next)))))
	  (else false))))

(define (expression-clobbers-stack-pointer? rtl)
  (or (and (rtl:assign? rtl)
	   (rtl:register? (rtl:assign-address rtl))
	   (interpreter-stack-pointer? (rtl:assign-address rtl)))
      (rtl:invocation? rtl)
      (rtl:invocation-prefix? rtl)
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
