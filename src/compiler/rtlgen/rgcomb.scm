#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

;;;; RTL Generation: Combinations
;;; package: (compiler rtl-generator generate/combination)

(declare (usual-integrations))

(define (generate/combination combination)
  (if (combination/inline? combination)
      (combination/inline combination)
      (let ((model (combination/model combination)))
	((cond ((not model)
		(if (reference? (combination/operator combination))
		    invocation/reference
		    invocation/apply))
	       ((rvalue/constant? model)
		(if (primitive-procedure? (constant-value model))
		    invocation/primitive
		    invocation/apply))
	       ((rvalue/procedure? model)
		(case (procedure/type model)
		  ((OPEN-EXTERNAL OPEN-INTERNAL) invocation/jump)
		  ((CLOSURE TRIVIAL-CLOSURE)
		   ;; Known lexpr closures are invoked through apply.
		   ;; This makes the code simpler and probably does
		   ;; not matter much.
		   (if (procedure-rest model)
		       invocation/apply
		       invocation/jump))
		  ((IC) invocation/ic)
		  (else (error "Unknown procedure type" model))))
	       (else
		invocation/apply))
	 model
	 (combination/operator combination)
	 (combination/frame-size combination)
	 (let ((continuation (combination/continuation combination)))
	   (and (return-operator/subproblem? continuation)
		(not (continuation/always-known-operator? continuation))
		(continuation/label continuation)))
	 (prefix/append (generate/link-prefix combination)
			(generate/invocation-prefix combination))))))

;;;; Invocations

(define (invocation/jump model operator frame-size continuation prefix)
  (let ((callee (rvalue-known-value operator)))
    (scfg*scfg->scfg!
     (prefix frame-size 0)
     (cond ((not callee)
	    (if (not model)
		(error "invocation/jump: Going to hyperspace!"))
	    ((if (procedure-rest model)
		 rtl:make-invocation:computed-lexpr
		 rtl:make-invocation:computed-jump)
	     frame-size
	     continuation))
	   ((procedure-inline-code? callee)
	    (generate/procedure-entry/inline callee))
	   (else
	    (enqueue-procedure! callee)
	    (let ((trivial-call
		   (lambda ()
		     (rtl:make-invocation:jump
		      frame-size
		      continuation
		      (procedure-label callee)))))
	      (cond ((procedure-rest callee)
		     ;; Note that callee can't be a closure because of
		     ;; the dispatch in generate/combination!
		     (let* ((callee-block (procedure-block callee))
			    (core
			     (lambda (frame-size)
			       (rtl:make-invocation:lexpr
				(if (stack-block/static-link? callee-block)
				    (-1+ frame-size)
				    frame-size)
				continuation
				(procedure-label callee)))))
		       (if (not (block/dynamic-link? callee-block))
			   (core frame-size)
			   (scfg*scfg->scfg!
			    (rtl:make-push-link)
			    (core (1+ frame-size))))))
		    ((and (procedure/closure? callee)
			  (not (procedure/trivial-closure? callee)))
		     (let* ((block (procedure-closing-block callee))
			    (block* (block-shared-block block)))
		       (if (eq? block block*)
			   (trivial-call)
			   (invocation/adjust-closure-prefix block block*
							     (trivial-call)))))
		    (else
		     (trivial-call)))))))))

(define (invocation/adjust-closure-prefix block block* call-code)
  (let ((distance (closure-environment-adjustment
		   (block-number-of-entries block*)
		   (closure-block-entry-number block))))
    (if (zero? distance)
	call-code
	(let ((locative
	       (rtl:locative-offset
		(rtl:make-fetch (interpreter-stack-pointer))
		(stack->memory-offset 0))))
	  (scfg*scfg->scfg!
	   (rtl:make-assignment locative
				(rtl:bump-closure (rtl:make-fetch locative)
						  distance))
	   call-code)))))

(define (rtl:bump-closure closure distance)
  (rtl:make-typed-cons:procedure
   (rtl:make-address
    (rtl:locative-byte-offset closure distance))))

(define (invocation/apply model operator frame-size continuation prefix)
  model operator			; ignored
  (invocation/apply* frame-size 0 continuation prefix))

(define (invocation/apply* frame-size extra continuation prefix)
  (scfg*scfg->scfg! (prefix frame-size extra)
		    (rtl:make-invocation:apply frame-size continuation)))

(define invocation/ic
  ;; For now, use apply.  Later we can optimize for the cases where
  ;; the callee's closing frame is easily available, such as calling a
  ;; sibling, self-recursion, or an ancestor.
  invocation/apply)

(define (invocation/primitive model operator frame-size continuation prefix)
  model					; ignored
  (scfg*scfg->scfg!
   (prefix frame-size 0)
   (let ((primitive (constant-value (rvalue-known-value operator))))
     (if (not (primitive-arity-correct? primitive frame-size))
	 (error "Primitive called with incorrect number of arguments."
		primitive
		frame-size))
     ((or (special-primitive-handler primitive)
	  rtl:make-invocation:primitive)
      (1+ frame-size)
      continuation
      primitive))))

(package (invocation/reference)

(define-export (invocation/reference model operator frame-size continuation
				     prefix)
  model					; ignored
  (if (reference-to-known-location? operator)
      (invocation/apply* frame-size 0 continuation prefix)
      (let ((context (reference-context operator))
	    (variable (reference-lvalue operator)))
	(find-variable/value context variable
	  (lambda (expression)
	    (scfg*scfg->scfg!
	     (rtl:make-push expression)
	     (invocation/apply* (1+ frame-size) 1 continuation prefix)))
	  (lambda (environment name)
	    (invocation/lookup frame-size
			       continuation
			       (prefix frame-size 0)
			       environment
			       (intern-scode-variable!
				(reference-context/block context)
				name)))
	  (lambda (name)
	    (cond ((and (pair? name) (eq? (cdr name) '*GLOBAL*))
		   (invocation/global-link frame-size
					   continuation
					   (prefix frame-size 0)
					   (car name)))
		  ((memq 'UUO-LINK (variable-declarations variable))
		   (invocation/uuo-link frame-size
					continuation
					(prefix frame-size 0)
					name))
		  (else
		   (invocation/cache-reference frame-size
					       continuation
					       prefix
					       name))))))))

(define (invocation/lookup frame-size continuation prefix environment variable)
  (let ((make-invocation
	 (lambda (environment)
	   (expression-simplify-for-statement environment
	     (lambda (environment)
	       (rtl:make-invocation:lookup (1+ frame-size)
					   continuation
					   environment
					   variable))))))
    (if (cfg-null? prefix)
	(make-invocation environment)
	(scfg-append!
	 (rtl:make-assignment register:environment environment)
	 prefix
	 (make-invocation (rtl:make-fetch register:environment))))))

(define (invocation/uuo-link frame-size continuation prefix name)
  (scfg*scfg->scfg! prefix
		    (rtl:make-invocation:uuo-link (1+ frame-size)
						  continuation
						  name)))

(define (invocation/global-link frame-size continuation prefix name)
  (scfg*scfg->scfg! prefix
		    (rtl:make-invocation:global-link (1+ frame-size)
						     continuation
						     name)))

(define (invocation/cache-reference frame-size continuation prefix name)
  (load-temporary-register scfg*scfg->scfg!
			   (rtl:make-variable-cache name)
    (lambda (cell)
      (let ((contents (rtl:make-fetch cell)))
	(let ((n2
	       (rtl:make-type-test (rtl:make-object->type contents)
				   (ucode-type reference-trap)))
	      (n3
	       (scfg*scfg->scfg!
		(rtl:make-push contents)
		(invocation/apply* (1+ frame-size) 1 continuation prefix)))
	      (n4
	       (scfg*scfg->scfg!
		(prefix frame-size 0)
		(expression-simplify-for-statement cell
		  (lambda (cell)
		    (rtl:make-invocation:cache-reference (1+ frame-size)
							 continuation
							 cell))))))
	  (pcfg-consequent-connect! n2 n4)
	  (pcfg-alternative-connect! n2 n3)
	  (make-scfg (cfg-entry-node n2)
		     (hooks-union (scfg-next-hooks n3)
				  (scfg-next-hooks n4))))))))

;;; end INVOCATION/REFERENCE
)

;;;; Prefixes

(define (prefix/append prefix prefix*)
  (if prefix
      (if prefix*
	  (lambda (frame-size extra)
	    (scfg*scfg->scfg! (prefix frame-size extra)
			      (prefix* frame-size extra)))
	  prefix)
      (if prefix*
	  prefix*
	  (lambda (frame-size extra)
	    frame-size extra
	    (make-null-cfg)))))

(define (generate/link-prefix combination)
  (and (let ((callee (combination/model combination)))
	 (and callee
	      (rvalue/procedure? callee)
	      (block/dynamic-link? (procedure-block callee))))
       (if (return-operator/subproblem? (combination/continuation combination))
	   link-prefix/subproblem
	   (let ((context (combination/context combination)))
	     (let ((popping-limit
		    (block-popping-limit (reference-context/block context))))
	       (and popping-limit
		    (link-prefix/reduction context popping-limit)))))))

(define (link-prefix/subproblem frame-size extra)
  extra
  (rtl:make-assignment
   register:dynamic-link
   (rtl:make-address
    (stack-locative-offset (rtl:make-fetch register:stack-pointer)
			   frame-size))))

(define (link-prefix/reduction context block)
  (lambda (frame-size extra)
    frame-size
    (rtl:make-assignment register:dynamic-link
			 (popping-limit/locative context block extra 0))))

(define (generate/invocation-prefix combination)
  (let ((context (combination/context combination))
	(overwritten-block (combination/reuse-existing-frame? combination)))
    (if overwritten-block
	(invocation-prefix/reuse-adjustment context overwritten-block)
	(let ((adjustment (combination/frame-adjustment combination)))
	  (and adjustment
	       (let ((block (cdr adjustment)))
		 (cond ((eq? (car adjustment) 'KNOWN)
			(invocation-prefix/move-frame-up context block))
		       ((block/external? block)
			;; If the adjustment is external, it says to
			;; try and pop all of the stack frames for
			;; this procedure.  We need not compare the
			;; dynamic link to the adjustment pointer
			;; because the dynamic link will always be
			;; less than or equal to the adjustment
			;; pointer.
			(lambda (frame-size extra)
			  (make-move-frame-up
			   frame-size
			   (stack-locative-offset (interpreter-dynamic-link)
						  extra))))
		       (else
			(invocation-prefix/dynamic-link context block)))))))))

(define (invocation-prefix/reuse-adjustment context block)
  (lambda (frame-size extra)
    ;; We've overwritten `(- frame-size extra)' items starting at `block',
    ;; and pushed another `extra' items at the top of stack.  We must
    ;; shift the `extra' items down to be adjacent to the overwritten
    ;; items.  Usually, `extra' is zero, in which case this just means
    ;; adjusting the stack pointer to the appropriate place.
    (let ((overwriting-size (- frame-size extra)))
      (if (<= (let loop ((block* (reference-context/block context)))
		(let ((size (block-frame-size block*)))
		   (if (eq? block block*)
		       size
		       (+ size (loop (block-parent block*))))))
	      overwriting-size)
	  ;; We've overwritten everything; no shift required.
	  (make-null-cfg)
	  (let ((locative
		 (popping-limit/locative context
					 block
					 extra
					 (- overwriting-size))))
	    (if (zero? extra)
		(rtl:make-assignment register:stack-pointer locative)
		(make-move-frame-up extra locative)))))))

(define (invocation-prefix/move-frame-up context block)
  (lambda (frame-size extra)
    (make-move-frame-up frame-size
			(popping-limit/locative context block extra 0))))

(define (make-move-frame-up frame-size locative)
  (expression-simplify-for-statement
   locative
   (lambda (locative)
     (rtl:make-invocation-prefix:move-frame-up frame-size locative))))

(define (invocation-prefix/dynamic-link context block)
  (lambda (frame-size extra)
    (expression-simplify-for-statement
     (popping-limit/locative context block extra 0)
     (lambda (locative)
       (expression-simplify-for-statement (interpreter-dynamic-link)
	 (lambda (dynamic-link)
	   (rtl:make-invocation-prefix:dynamic-link frame-size
						    locative
						    dynamic-link)))))))