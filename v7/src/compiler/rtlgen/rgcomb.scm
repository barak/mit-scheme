#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlgen/rgcomb.scm,v 4.9 1988/12/12 21:52:32 cph Exp $

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

;;;; RTL Generation: Combinations

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
		(if (normal-primitive-procedure? (constant-value model))
		    invocation/primitive
		    invocation/apply))
	       ((rvalue/procedure? model)
		(case (procedure/type model)
		  ((OPEN-EXTERNAL OPEN-INTERNAL) invocation/jump)
		  ((CLOSURE TRIVIAL-CLOSURE)
		   ;; *** For the time being, known lexpr closures are
		   ;; invoked through apply.  This makes the code
		   ;; simpler and probably does not matter much. ***
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
	    ((if (procedure-rest callee)
		 rtl:make-invocation:lexpr
		 rtl:make-invocation:jump)
	     frame-size
	     continuation
	     (procedure-label callee)))))))

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
	(find-variable context variable
	  (lambda (locative)
	    (scfg*scfg->scfg!
	     (rtl:make-push (rtl:make-fetch locative))
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
	    (if (memq 'UUO-LINK (variable-declarations variable))
		(invocation/uuo-link frame-size
				     continuation
				     (prefix frame-size 0)
				     name)
		(invocation/cache-reference frame-size
					    continuation
					    prefix
					    name)))))))

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
	      (procedure/open-internal? callee)
	      (internal-block/dynamic-link? (procedure-block callee))))       (if (return-operator/subproblem? (combination/continuation combination))
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
	       ((if (eq? (car adjustment) 'KNOWN)
		    invocation-prefix/move-frame-up
		    invocation-prefix/dynamic-link)
		context
		(cdr adjustment)))))))

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