#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlopt/rcompr.scm,v 1.10 1990/01/18 22:47:38 cph Rel $

Copyright (c) 1988, 1989, 1990 Massachusetts Institute of Technology

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
  ;; register has exactly one reference that is known to be in this
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
	      (with-values
		  (lambda ()
		    (let ((next (rinst-next rinst)))
		      (if (rinst-dead-register? next register)
			  (values next expression)
			  (find-reference-instruction next
						      register
						      expression))))
		(lambda (next expression)
		  (if next
		      (fold-instructions! live
					  rinst
					  next
					  register
					  expression)))))))))

(define (find-reference-instruction next register expression)
  ;; Find the instruction that contains the single reference to
  ;; `register', and determine if it is possible to fold `expression'
  ;; into that instruction in `register's place.
  (let loop ((expression expression))
    (let ((search-stopping-at
	   (lambda (expression predicate)
	     (define (phi-1 next)
	       (if (predicate (rinst-rtl next))
		   (values false false)
		   (phi-2 (rinst-next next))))
	     (define (phi-2 next)
	       (if (rinst-dead-register? next register)
		   (values next expression)
		   (phi-1 next)))
	     (phi-1 next)))
	  (recursion
	   (lambda (unwrap wrap)
	     (with-values
		 (lambda ()
		   (loop (unwrap expression)))
	       (lambda (next expression)
		 (if next
		     (values next (wrap expression))
		     (values false false)))))))
      (cond ((interpreter-value-register? expression)
	     (search-stopping-at expression
	       (lambda (rtl)
		 (and (rtl:assign? rtl)
		      (interpreter-value-register?
		       (rtl:assign-address rtl))))))
	    ((and (rtl:offset? expression)
		  (interpreter-stack-pointer? (rtl:offset-base expression)))
	     (let ()
	       (define (phi-1 next offset)
		 (let ((rtl (rinst-rtl next)))
		   (cond ((expression-is-stack-push? rtl)
			  (phi-2 (rinst-next next) (1+ offset)))
			 ((or (and (rtl:assign? rtl)
				   (rtl:expression=? (rtl:assign-address rtl)
						     expression))
			      (expression-clobbers-stack-pointer? rtl))
			  (values false false))
			 (else
			  (phi-2 (rinst-next next) offset)))))
	       (define (phi-2 next offset)
		 (if (rinst-dead-register? next register)
		     (values next
			     (rtl:make-offset (rtl:offset-base expression)
					      offset))
		     (phi-1 next offset)))
	       (phi-1 next (rtl:offset-number expression))))
	    ((and (rtl:offset-address? expression)
		  (interpreter-stack-pointer?
		   (rtl:offset-address-base expression)))
	     (search-stopping-at expression
				 expression-clobbers-stack-pointer?))
	    ((rtl:constant-expression? expression)
	     (let loop ((next (rinst-next next)))
	       (if (rinst-dead-register? next register)
		   (values next expression)
		   (loop (rinst-next next)))))
	    ((rtl:offset? expression)
	     (search-stopping-at expression
	       (lambda (rtl)
		 (or (and (rtl:assign? rtl)
			  (memq (rtl:expression-type
				 (rtl:assign-address rtl))
				'(OFFSET POST-INCREMENT PRE-INCREMENT)))
		     (expression-clobbers-stack-pointer? rtl)))))
	    ((rtl:object->address? expression)
	     (recursion rtl:object->address-expression
			rtl:make-object->address))
	    ((rtl:object->datum? expression)
	     (recursion rtl:object->datum-expression rtl:make-object->datum))
	    ((rtl:object->fixnum? expression)
	     (recursion rtl:object->fixnum-expression rtl:make-object->fixnum))
	    ((rtl:object->type? expression)
	     (recursion rtl:object->type-expression rtl:make-object->type))
	    ((rtl:object->unsigned-fixnum? expression)
	     (recursion rtl:object->unsigned-fixnum-expression
			rtl:make-object->unsigned-fixnum))
	    (else
	     (values false false))))))

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

(define (expression-is-stack-push? rtl)
  (and (rtl:assign? rtl)
       (let ((address (rtl:assign-address rtl)))
	 (and (rtl:pre-increment? address)
	      (interpreter-stack-pointer?
	       (rtl:pre-increment-register address))
	      (= -1 (rtl:pre-increment-number address))))))

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
		(for-each-regset-member live decrement-register-live-length!)
		(let ((dead
		       (new-dead-registers
			(rinst-next rinst)
			next
			(rinst-dead-registers rinst)
			(rtl:expression-register-references expression))))
		  (set-rinst-dead-registers!
		   next
		   (eqv-set-union dead
				  (delv! register
					 (rinst-dead-registers next)))))
		(reset-register-n-refs! register)
		(reset-register-n-deaths! register)
		(reset-register-live-length! register)
		(set-register-bblock! register false)))))))

(define (new-dead-registers rinst next old-dead registers)
  (let loop ((rinst rinst) (new-dead old-dead))
    (for-each increment-register-live-length! new-dead)
    (if (eq? rinst next)
	new-dead
	(let* ((dead (rinst-dead-registers rinst))
	       (dead* (eqv-set-intersection dead registers)))
	  (if (not (null? dead*))
	      (begin
		(set-rinst-dead-registers!
		 rinst
		 (eqv-set-difference dead dead*))
		(loop (rinst-next rinst) (eqv-set-union dead* new-dead)))
	      (loop (rinst-next rinst) new-dead))))))

(define (rtl:expression-register-references expression)
  (let ((registers '()))
    (let loop ((expression expression))
      (if (rtl:pseudo-register-expression? expression)
	  (let ((register (rtl:register-number expression)))
	    (if (not (memv register registers))
		(set! registers (cons register registers))))
	  (rtl:for-each-subexpression expression loop)))
    registers))