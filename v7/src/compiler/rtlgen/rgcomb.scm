#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlgen/rgcomb.scm,v 1.16 1987/05/07 04:36:15 cph Exp $

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

;;;; RTL Generation: Combinations

(declare (usual-integrations))

(define-generator combination-tag
  (lambda (combination subproblem?)
    ((cond ((combination-constant? combination) combination/constant)
	   ((let ((operator (combination-known-operator combination)))
	      (and operator
		   (normal-primitive-constant? operator)))
	    combination/primitive)
	   (else combination/normal))
     combination subproblem?)))

(define combination/constant
  (normal-statement-generator
   (lambda (combination subproblem?)
     (let ((value (combination-value combination)))
       (cond ((temporary? value)
	      (transmit-values (generate/rvalue (vnode-known-value value))
		(lambda (prefix expression)
		  (scfg*scfg->scfg!
		   prefix
		   (generate/assignment (combination-block combination)
					value
					expression
					subproblem?)))))
	     ((value-ignore? value)
	      (make-null-cfg))
	     (else
	      (error "Unknown combination value" value)))))))

(define combination/normal
  (normal-statement-generator
   (lambda (combination subproblem?)
     ;; For the time being, all close-coded combinations will return
     ;; their values in the value register.
     (let ((value (combination-value combination)))
       (cond ((temporary? value)
	      (let ((type (temporary-type value)))
		(if type
		    (if (not (eq? 'VALUE type))
			(error "Bad temporary type" type))
		    (set-temporary-type! value 'VALUE))))
	     ((not (value-ignore? value))
	      (error "Unknown combination value" value))))
     ((if subproblem? combination/subproblem combination/reduction)
      combination))))

;;;; Subproblems

(define (combination/subproblem combination)
  (let ((block (combination-block combination))
	(finish
	 (lambda (call-prefix continuation-prefix)
	   (let ((continuation (make-continuation)))
	     (let ((continuation-cfg
		    (scfg*scfg->scfg!
		     (rtl:make-continuation-heap-check continuation)
		     continuation-prefix)))
	       (set-continuation-rtl-entry! continuation
					    (cfg-entry-node continuation-cfg))
	       (make-scfg (cfg-entry-node
			   (scfg*scfg->scfg!
			    (call-prefix continuation)
			    (combination/subproblem-body combination
							 continuation)))
			  (scfg-next-hooks continuation-cfg)))))))
    (cond ((ic-block? block)
	   ;; **** Actually, should only do this if the environment
	   ;; will be needed by the continuation.
	   (finish (lambda (continuation)
		     (scfg*scfg->scfg!
		      (rtl:make-push (rtl:make-fetch register:environment))
		      (rtl:make-push-return continuation)))
		   (rtl:make-pop register:environment)))
	  ((and (stack-block? block)
		(let ((operator (combination-known-operator combination)))
		  (and operator
		       (procedure? operator)
		       (procedure/open-internal? operator))))
	   (finish rtl:make-message-receiver:subproblem (make-null-cfg)))
	  (else
	   (finish rtl:make-push-return (make-null-cfg))))))

(define (combination/subproblem-body combination continuation)
  ((let ((operator (combination-known-operator combination)))
     (cond ((normal-primitive-constant? operator) make-call/primitive)
	   ((or (not operator) (not (procedure? operator))) make-call/unknown)
	   (else
	    (case (procedure/type operator)
	      ((OPEN-INTERNAL) make-call/stack-with-link)
	      ((OPEN-EXTERNAL) make-call/open-external)
	      ((CLOSURE) make-call/closure)
	      ((IC) make-call/ic)
	      (else (error "Unknown callee type" operator))))))
   combination invocation-prefix/null continuation))

;;;; Reductions

(define (combination/reduction combination)
  ((let ((callee (combination-known-operator combination))
	 (block (combination-block combination)))
     (define (choose-generator ic external internal)
       (let ((caller (block-procedure block)))
	 (cond ((or (not caller) (procedure/ic? caller)) ic)
	       ((procedure/external? caller) external)
	       (else internal))))
     (cond ((normal-primitive-constant? callee)
	    (choose-generator reduction/ic->primitive
			      reduction/external->primitive
			      reduction/internal->primitive))
	   ((or (not callee)
		(not (procedure? callee)))
	    (choose-generator reduction/ic->unknown
			      reduction/external->unknown
			      reduction/internal->unknown))
	   (else
	    (case (procedure/type callee)
	      ((IC)
	       (choose-generator reduction/ic->ic
				 reduction/external->ic
				 reduction/internal->ic))
	      ((CLOSURE)
	       (choose-generator reduction/ic->closure
				 reduction/external->closure
				 reduction/internal->closure))
	      ((OPEN-EXTERNAL)
	       (choose-generator reduction/ic->open-external
				 reduction/external->open-external
				 reduction/internal->open-external))
	      ((OPEN-INTERNAL)
	       (choose-generator reduction/ic->child
				 reduction/external->child
				 (let ((block* (procedure-block callee)))
				   (cond ((block-child? block block*)
					  reduction/internal->child)
					 ((block-sibling? block block*)
					  reduction/internal->sibling)
					 (else
					  reduction/internal->ancestor)))))
	      (else (error "Unknown callee type" callee))))))
     combination))

(define (reduction/ic->unknown combination)
  (make-call/unknown combination invocation-prefix/null false))

(define (reduction/ic->ic combination)
  (make-call/ic combination invocation-prefix/null false))

(define (reduction/ic->primitive combination)
  (make-call/primitive combination invocation-prefix/null false))

(define (reduction/ic->closure combination)
  (make-call/closure combination invocation-prefix/null false))

(define (reduction/ic->open-external combination)
  (make-call/open-external combination invocation-prefix/null false))

(define (reduction/ic->child combination)
  (error "Calling internal procedure from IC procedure"))

(define (reduction/external->unknown combination)
  (make-call/unknown combination invocation-prefix/move-frame-up false))

(define (reduction/external->ic combination)
  (make-call/ic combination invocation-prefix/move-frame-up false))

(define (reduction/external->primitive combination)
  (make-call/primitive combination invocation-prefix/move-frame-up false))

(define (reduction/external->closure combination)
  (make-call/closure combination invocation-prefix/move-frame-up false))

(define (reduction/external->open-external combination)
  (make-call/open-external combination invocation-prefix/move-frame-up false))

(define (reduction/external->child combination)
  (make-call/child combination
		   rtl:make-message-receiver:closure
		   rtl:message-receiver-size:closure))

(define (reduction/internal->unknown combination)
  (make-call/unknown combination invocation-prefix/internal->closure false))

(define (reduction/internal->ic combination)
  (make-call/ic combination invocation-prefix/internal->closure false))

(define (reduction/internal->primitive combination)
  (make-call/primitive combination invocation-prefix/internal->closure false))

(define (reduction/internal->closure combination)
  (make-call/closure combination invocation-prefix/internal->closure false))

(define (reduction/internal->open-external combination)
  (make-call/open-external combination invocation-prefix/internal->closure
			   false))

(define (reduction/internal->child combination)
  (make-call/child combination
		   rtl:make-message-receiver:stack
		   rtl:message-receiver-size:stack))

(define (reduction/internal->sibling combination)
  (make-call/stack combination invocation-prefix/internal->sibling false))

(define (reduction/internal->ancestor combination)
  (make-call/stack-with-link combination invocation-prefix/internal->ancestor
			     false))

;;;; Calls

(define (make-call/apply combination invocation-prefix continuation)
  (make-call/push-operator combination
    (lambda (number-pushed)
      (rtl:make-invocation:apply number-pushed
				 (invocation-prefix combination number-pushed)
				 continuation))))

(define (make-call/lookup combination invocation-prefix continuation)
  (make-call/dont-push-operator combination
    (lambda (number-pushed)
      (let ((operator (subproblem-value (combination-operator combination))))
	(let ((block (reference-block operator))
	      (name (variable-name (reference-variable operator))))
	  (rtl:make-invocation:lookup
	   number-pushed
	   (invocation-prefix combination number-pushed)
	   continuation
	   (nearest-ic-block-expression block)
	   (intern-scode-variable! block name)))))))

(define (make-call/unknown combination invocation-prefix continuation)
  (let ((operator (subproblem-value (combination-operator combination))))
    ((cond ((or (not (reference? operator))
		(reference-to-known-location? operator))
	    make-call/apply)
	   ;; **** Need to add code for links here.
	   (else make-call/lookup))
     combination invocation-prefix continuation)))

;;; For now, use apply.  Later we can optimize for the cases where
;;; the callee's closing frame is easily available, such as calling a
;;; sibling, self-recursion, or an ancestor.

(define make-call/ic make-call/apply)

(define (make-call/primitive combination invocation-prefix continuation)
  (make-call/dont-push-operator combination
    (lambda (number-pushed)
      (rtl:make-invocation:primitive
       number-pushed
       (invocation-prefix combination number-pushed)
       continuation
       (constant-value (combination-known-operator combination))))))

(define (make-call/closure combination invocation-prefix continuation)
  (make-call/push-operator combination
    (internal-call combination invocation-prefix continuation 0)))

(define (make-call/open-external combination invocation-prefix continuation)
  (scfg*scfg->scfg!
   (rtl:make-push (rtl:make-fetch register:environment))
   (make-call/dont-push-operator combination
     (internal-call combination invocation-prefix continuation 0))))

(define (make-call/stack combination invocation-prefix continuation)
  (stack-call combination invocation-prefix continuation 0))

(define (make-call/stack-with-link combination invocation-prefix continuation)
  (link-call combination invocation-prefix continuation 0))

(define (make-call/child combination make-receiver receiver-size)
  (scfg*scfg->scfg!
   (make-receiver (block-frame-size (combination-block combination)))
   (link-call combination invocation-prefix/null false (receiver-size))))

(define (link-call combination invocation-prefix continuation extra)
  (scfg*scfg->scfg!
   (rtl:make-push
    (rtl:make-address
     (block-ancestor-or-self->locative
      (combination-block combination)
      (block-parent
       (procedure-block (combination-known-operator combination))))))
   (stack-call combination invocation-prefix continuation (1+ extra))))

(define (stack-call combination invocation-prefix continuation extra)
  (make-call/dont-push-operator combination
    (internal-call combination invocation-prefix continuation extra)))

(define (internal-call combination invocation-prefix continuation extra)
  (lambda (number-pushed)
    (let ((operator (combination-known-operator combination))
	  (number-pushed (+ number-pushed extra)))
      ((if (procedure-rest operator)
	   rtl:make-invocation:lexpr
	   rtl:make-invocation:jump)
       number-pushed
       (invocation-prefix combination number-pushed)
       continuation
       operator))))

;;;; Prefixes

(define (invocation-prefix/null combination number-pushed)
  '(NULL))

(define (invocation-prefix/move-frame-up combination number-pushed)
  `(MOVE-FRAME-UP ,number-pushed
		  ,(block-frame-size (combination-block combination))))

(define (invocation-prefix/internal->closure combination number-pushed)
  ;; The message sender will shift the new stack frame down to the
  ;; correct position when it is done, then reset the stack pointer.
  `(APPLY-CLOSURE ,number-pushed
		  ,(block-frame-size (combination-block combination))))

(define (invocation-prefix/internal->ancestor combination number-pushed)
  (let ((block (combination-block combination)))
    `(APPLY-STACK ,number-pushed
		  ,(block-frame-size block)
		  ,(-1+
		    (block-ancestor-distance
		     block
		     (block-parent
		      (procedure-block
		       (combination-known-operator combination))))))))

(define (invocation-prefix/internal->sibling combination number-pushed)
   `(MOVE-FRAME-UP ,number-pushed
		   ;; -1+ means reuse the existing static link.
		   ,(-1+ (block-frame-size (combination-block combination)))))

;;;; Call Sequence Kernels

(package (make-call/dont-push-operator make-call/push-operator)

(define (make-call-maker generate/operator wrap-n)
  (lambda (combination make-invocation)
    (let ((operator (combination-known-operator combination))
	  (operands (combination-operands combination)))
      (scfg-append!
       (scfg*->scfg!
	(map generate/subproblem-push (reverse operands)))
       (generate/operator (combination-operator combination))
       (let ((n-operands (length operands)))
	 (if (and operator
		  (procedure? operator)
		  (not (procedure-rest operator))
		  (stack-block? (procedure-block operator)))
	     (let ((n-parameters (+ (length (procedure-required operator))
				    (length (procedure-optional operator)))))
		 (scfg*scfg->scfg!
		  (scfg*->scfg!
		   (push-n-unassigned (- n-parameters n-operands)))
		  (make-invocation (wrap-n n-parameters))))
	     (make-invocation (wrap-n n-operands))))))))

(define (push-n-unassigned n)
  (if (zero? n)
      '()
      (cons (rtl:make-push (rtl:make-unassigned))
	    (push-n-unassigned (-1+ n)))))

(define-export make-call/dont-push-operator
  (make-call-maker generate/subproblem-cfg identity-procedure))

(define-export make-call/push-operator
  (make-call-maker generate/subproblem-push 1+))

		   ,(-1+ (block-frame-size (combination-block combination)))))