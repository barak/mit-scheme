#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; RTL Compression
;;;  Based on the GNU C Compiler
;;; package: (compiler rtl-optimizer code-compression)

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
      (let ((recurse-and-search
	     (lambda (unwrap wrap)
	       (with-values (lambda ()
			      (recursion unwrap wrap))
		 (lambda (next expression*)
		   (if next
		       (values next expression*)
		       (search-stopping-at expression
					   (lambda (rtl)
					     rtl ; ignored
					     false))))))))
	       
	(cond ((interpreter-value-register? expression)
	       (search-stopping-at expression
				   (lambda (rtl)
				     (and (rtl:assign? rtl)
					  (interpreter-value-register?
					   (rtl:assign-address rtl))))))
	      ((and (rtl:offset? expression)
		    (interpreter-stack-pointer? (rtl:offset-base expression))
		    (rtl:machine-constant? (rtl:offset-offset expression)))
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
						(rtl:make-machine-constant
						 offset)))
		       (phi-1 next offset)))
		 (phi-1 next
			(rtl:machine-constant-value
			 (rtl:offset-offset expression)))))
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
	      ((or (rtl:offset? expression)
		   (rtl:byte-offset? expression)
		   (rtl:float-offset? expression))
	       (search-stopping-at
		expression
		(lambda (rtl)
		  (or (and (rtl:assign? rtl)
			   (memq (rtl:expression-type
				  (rtl:assign-address rtl))
				 '(OFFSET POST-INCREMENT PRE-INCREMENT)))
		      (expression-clobbers-stack-pointer? rtl)))))
	      ((and (rtl:cons-pointer? expression)
		    (rtl:machine-constant? (rtl:cons-pointer-type expression)))
	       (recursion rtl:cons-pointer-datum
			  (lambda (datum)
			    (rtl:make-cons-pointer
			     (rtl:cons-pointer-type expression)
			     datum))))
	      ((and (rtl:cons-non-pointer? expression)
		    (rtl:machine-constant?
		     (rtl:cons-non-pointer-type expression)))
	       (recursion rtl:cons-non-pointer-datum
			  (lambda (datum)
			    (rtl:make-cons-non-pointer
			     (rtl:cons-non-pointer-type expression)
			     datum))))
	      ((rtl:object->address? expression)
	       (recursion rtl:object->address-expression
			  rtl:make-object->address))
	      ((rtl:object->datum? expression)
	       (recurse-and-search rtl:object->datum-expression
				   rtl:make-object->datum))
	      ((rtl:object->fixnum? expression)
	       (recurse-and-search rtl:object->fixnum-expression
				   rtl:make-object->fixnum))
	      ((rtl:object->type? expression)
	       (recursion rtl:object->type-expression rtl:make-object->type))
	      ((rtl:object->unsigned-fixnum? expression)
	       (recursion rtl:object->unsigned-fixnum-expression
			  rtl:make-object->unsigned-fixnum))
	      (else
	       (values false false)))))))

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