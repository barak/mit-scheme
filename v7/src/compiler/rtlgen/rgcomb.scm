#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlgen/rgcomb.scm,v 4.1 1987/12/04 20:30:36 cph Exp $

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

(package (generate/combination)

(define (generate/combination combination offset)
  (if (combination/inline? combination)
      (combination/inline combination offset)
      (combination/normal combination offset)))

(define (combination/normal combination offset)
  (let ((block (combination/block combination))
	(operator (combination/operator combination))
	(frame-size (combination/frame-size combination))
	(continuation (combination/continuation combination)))
    (let ((callee (rvalue-known-value operator)))
      (let ((finish
	     (lambda (invocation callee-external?)
	       (if (return-operator/subproblem? continuation)
		   (invocation operator
			       offset
			       frame-size
			       (continuation/label continuation)
			       invocation-prefix/null)
		   (invocation operator
			       offset
			       frame-size
			       false
			       (generate/invocation-prefix
				block
				offset
				callee
				continuation
				callee-external?))))))
	(cond ((not callee)
	       (finish (if (reference? operator)
			   invocation/reference
			   invocation/apply)
		       true))
	      ((rvalue/constant? callee)
	       (finish
		(if (normal-primitive-procedure? (constant-value callee))
		    invocation/primitive
		    invocation/apply)
		true))
	      ((rvalue/procedure? callee)
	       (case (procedure/type callee)
		 ((OPEN-EXTERNAL) (finish invocation/jump true))
		 ((OPEN-INTERNAL) (finish invocation/jump false))
		 ((CLOSURE) (finish invocation/jump true))
		 ((IC) (finish invocation/ic true))
		 (else (error "Unknown procedure type" callee))))
	      (else
	       (finish invocation/apply true)))))))

;;;; Invocations

(define (invocation/jump operator offset frame-size continuation prefix)
  (let ((callee (rvalue-known-value operator)))
    (scfg*scfg->scfg!
     (prefix frame-size)
     (if (procedure-inline-code? callee)
	 (generate/procedure-entry/inline callee)
	 (begin
	   (enqueue-procedure! callee)
	   ((if (procedure-rest callee)
		rtl:make-invocation:lexpr
		rtl:make-invocation:jump)
	    frame-size
	    continuation
	    (procedure-label callee)))))))

(define (invocation/apply operator offset frame-size continuation prefix)
  (invocation/apply* frame-size continuation prefix))

(define (invocation/apply* frame-size continuation prefix)
  (scfg*scfg->scfg! (prefix frame-size)
		    (rtl:make-invocation:apply frame-size continuation)))

(define invocation/ic
  ;; For now, use apply.  Later we can optimize for the cases where
  ;; the callee's closing frame is easily available, such as calling a
  ;; sibling, self-recursion, or an ancestor.
  invocation/apply)

(define (invocation/primitive operator offset frame-size continuation prefix)
  (scfg*scfg->scfg!
   (prefix frame-size)
   (let ((primitive
	  (let ((primitive (constant-value (rvalue-known-value operator))))
	    (if (eq? primitive compiled-error-procedure)
		primitive
		(primitive-procedure-name primitive)))))
     ((if (memq primitive special-primitive-handlers)
	  rtl:make-invocation:special-primitive
	  rtl:make-invocation:primitive)
      (1+ frame-size)
      continuation
      primitive))))

(package (invocation/reference)

(define-export (invocation/reference operator offset frame-size continuation
				     prefix)
  (let ((block (reference-block operator))
	(variable (reference-lvalue operator)))
    (find-variable block variable offset
      (lambda (locative)
	(scfg*scfg->scfg!
	 (rtl:make-push (rtl:make-fetch locative))
	 (invocation/apply* (1+ frame-size) continuation prefix)))
      (lambda (environment name)
	(invocation/lookup frame-size
			   continuation
			   (prefix frame-size)
			   environment
			   (intern-scode-variable! block name)))
      (lambda (name)
	(if (memq 'UUO-LINK (variable-declarations variable))
	    (invocation/uuo-link frame-size
				 continuation
				 (prefix frame-size)
				 name)
	    (invocation/cache-reference frame-size
					continuation
					prefix
					name))))))

(define (invocation/lookup frame-size
			   continuation
			   prefix
			   environment
			   variable)
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
	(scfg-append! (rtl:make-assignment register:environment environment)
		      prefix
		      (make-invocation register:environment)))))

(define (invocation/uuo-link frame-size continuation prefix name)
  (scfg*scfg->scfg! prefix
		    (rtl:make-invocation:uuo-link (1+ frame-size)
						  continuation
						  name)))

(define (invocation/cache-reference frame-size continuation prefix name)
  (let* ((temp (rtl:make-pseudo-register))
	 (cell (rtl:make-fetch temp))
	 (contents (rtl:make-fetch cell)))
    (let ((n1 (rtl:make-assignment temp (rtl:make-variable-cache name)))
	  (n2
	   (rtl:make-type-test (rtl:make-object->type contents)
			       (ucode-type reference-trap)))
	  (n3
	   (scfg*scfg->scfg!
	    (rtl:make-push contents)
	    (invocation/apply* (1+ frame-size) continuation prefix)))
	  (n4
	   (scfg*scfg->scfg!
	    (prefix frame-size)
	    (expression-simplify-for-statement cell
	      (lambda (cell)
		(rtl:make-invocation:cache-reference (1+ frame-size)
						     continuation
						     cell))))))
      (scfg-next-connect! n1 n2)
      (pcfg-consequent-connect! n2 n4)
      (pcfg-alternative-connect! n2 n3)
      (make-scfg (cfg-entry-node n1)
		 (hooks-union (scfg-next-hooks n3)
			      (scfg-next-hooks n4))))))

;;; end INVOCATION/REFERENCE
)

;;;; Prefixes

(package (generate/invocation-prefix invocation-prefix/null)

(define-export (generate/invocation-prefix block
					   offset
					   callee
					   continuation
					   callee-external?)
  (let ((caller (block-procedure block)))
    (cond ((or (not (rvalue/procedure? caller))
	       (procedure/ic? caller))
	   invocation-prefix/null)
	  ((procedure/external? caller)
	   (if callee-external?
	       (invocation-prefix/move-frame-up block offset block)
	       invocation-prefix/null))
	  (callee-external?
	   (invocation-prefix/erase-to block
				       offset
				       continuation
				       (stack-block/external-ancestor block)))
	  (else
	   (let ((block* (procedure-block callee)))
	     (cond ((block-child? block block*)
		    invocation-prefix/null)
		   ((block-sibling? block block*)
		    (invocation-prefix/move-frame-up block offset block))
		   (else
		    (invocation-prefix/erase-to
		     block
		     offset
		     continuation
		     (block-farthest-uncommon-ancestor block block*)))))))))

(define (invocation-prefix/erase-to block offset continuation callee-limit)
  (let ((popping-limit (reduction-continuation/popping-limit continuation)))
    (if popping-limit
	(invocation-prefix/move-frame-up block
					 offset
					 (if (block-ancestor? callee-limit
							      popping-limit)
					     callee-limit
					     popping-limit))
	(invocation-prefix/dynamic-link
	 (popping-limit/locative block offset callee-limit 0)))))

;;; The invocation prefix is always one of the following:

(define-export (invocation-prefix/null frame-size)
  (make-null-cfg))

(define (invocation-prefix/move-frame-up block offset block*)
  (invocation-prefix/move-frame-up*
   (popping-limit/locative block offset block* 0)))

(define (invocation-prefix/move-frame-up* locative)
  (lambda (frame-size)
    (expression-simplify-for-statement locative
      (lambda (locative)
	(rtl:make-invocation-prefix:move-frame-up frame-size locative)))))

(define (invocation-prefix/dynamic-link locative)
  (lambda (frame-size)
    (expression-simplify-for-statement locative
      (lambda (locative)
	(rtl:make-invocation-prefix:dynamic-link frame-size locative)))))

;;; end GENERATE/INVOCATION-PREFIX
)

;;; end GENERATE/COMBINATION
)