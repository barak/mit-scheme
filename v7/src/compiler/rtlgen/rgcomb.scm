#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlgen/rgcomb.scm,v 1.34 1987/09/03 05:10:05 jinx Exp $

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
    (if (combination-constant? combination)
	(combination/constant combination subproblem?)
	(let ((callee (combination-known-operator combination)))
	  (let ((operator
		 (generate/subproblem-cfg (combination-operator combination)))
		(operands
		 (if (and callee
			  (procedure? callee)
			  (not (procedure-externally-visible? callee)))
		     (generate-operands (procedure-original-required callee)
					(procedure-original-optional callee)
					(procedure-original-rest callee)
					(combination-operands combination))
		     (map generate/operand
			  (combination-operands combination)))))
	    (or (and callee
		     (normal-primitive-constant? callee)
		     (let ((open-coder
			    (assq (constant-value callee)
				  (cdr primitive-open-coders))))
		       (and open-coder
			    ((cdr open-coder) combination
					      subproblem?
					      operator
					      operands))))
		(combination/normal combination
				    subproblem?
				    operator
				    operands)))))))

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

(define (generate-operands required optional rest operands)
  (define (required-loop required operands)
    (if (null? required)
	(optional-loop optional operands)
	(cons ((if (integrated-vnode? (car required))
		   generate/operand-no-value
		   generate/operand)
	       (car operands))
	      (required-loop (cdr required) (cdr operands)))))

  (define (optional-loop optional operands)
    (cond ((null? operands) '())
	  ((null? optional)
	   (if (not rest)
	       '()
	       (map (if (integrated-vnode? rest)
			generate/operand-no-value
			generate/operand)
		    operands)))
	  (else
	   (cons ((if (integrated-vnode? (car optional))
		      generate/operand-no-value
		      generate/operand)
		  (car operands))
		 (optional-loop (cdr optional) (cdr operands))))))

  (required-loop required operands))

(define (generate/operand-no-value operand)
  (return-3 (generate/subproblem-cfg operand) (make-null-cfg) false))

(define (combination/normal combination subproblem? operator operands)
  ;; For the time being, all close-coded combinations will return
  ;; their values in the value register.
  (generate/normal-statement combination subproblem?
    (lambda (combination subproblem?)
      (let ((value (combination-value combination)))
	(cond ((temporary? value)
	       (if (not subproblem?)
		   (error "Reduction targeted to temporary!" combination))
	       (scfg*scfg->scfg!
		(combination/subproblem combination operator operands)
		(rtl:make-assignment value (rtl:make-fetch register:value))))
	      ((or (value-register? value)
		   (value-ignore? value))
	       ((if subproblem? combination/subproblem combination/reduction)
		combination
		operator
		operands))
	      (else
	       (error "Unknown combination value" value)))))))

(define (define-primitive-handler data-base)
  (lambda (primitive handler)
    (let ((kernel
	   (lambda (primitive)
	     (let ((entry (assq primitive (cdr data-base))))
	       (if entry
		   (set-cdr! entry handler)
		   (set-cdr! data-base
			     (cons (cons primitive handler)
				   (cdr data-base))))))))
      (if (pair? primitive)
	  (for-each kernel primitive)
	  (kernel primitive)))
    primitive))

(define primitive-open-coders
  (list 'PRIMITIVE-OPEN-CODERS))

(define define-open-coder
  (define-primitive-handler primitive-open-coders))

(define (combination/subproblem combination operator operands)
  (let ((block (combination-block combination)))
    (define (finish call-prefix continuation-prefix)
      (let ((continuation (make-continuation block *current-rgraph*)))
	(let ((continuation-cfg
	       (scfg*scfg->scfg!
		(rtl:make-continuation-heap-check continuation)
		continuation-prefix)))
	  (set-continuation-rtl-edge!
	   continuation
	   (node->edge (cfg-entry-node continuation-cfg)))
	  (make-scfg
	   (cfg-entry-node
	    (scfg*scfg->scfg!
	     (call-prefix continuation)
	     ((let ((callee (combination-known-operator combination)))
		(cond ((normal-primitive-constant? callee)
		       make-call/primitive)
		      ((or (not callee) (not (procedure? callee)))
		       make-call/unknown)
		      (else
		       (case (procedure/type callee)
			 ((OPEN-INTERNAL) make-call/stack-with-link)
			 ((OPEN-EXTERNAL) make-call/stack-with-link)
			 ((CLOSURE) make-call/closure)
			 ((IC) make-call/ic)
			 (else (error "Unknown callee type" callee))))))
	      combination operator operands invocation-prefix/null
	      continuation)))
	   (scfg-next-hooks continuation-cfg)))))

    (cond ((ic-block? block)
	   ;; **** Actually, should only do this if the environment
	   ;; will be needed by the continuation.
	   (finish (lambda (continuation)
		     (scfg*scfg->scfg!
		      (rtl:make-push (rtl:make-fetch register:environment))
		      (rtl:make-push-return continuation)))
		   (rtl:make-pop register:environment)))
	  ((and (stack-block? block)
		(let ((callee (combination-known-operator combination)))
		  (and callee
		       (procedure? callee)
		       (procedure/open-internal? callee))))
	   (finish rtl:make-message-receiver:subproblem (make-null-cfg)))
	  (else
	   (finish rtl:make-push-return (make-null-cfg))))))

(define (combination/reduction combination operator operands)
  (let ((block (combination-block combination))
	(callee (combination-known-operator combination)))
    (let ((caller (block-procedure block))
	  (generator
	   (cond ((normal-primitive-constant? callee)
		  make-call/primitive)
		 ((or (not callee)
		      (not (procedure? callee)))
		  make-call/unknown)
		 (else
		  (case (procedure/type callee)
		    ((IC) make-call/ic)
		    ((CLOSURE) make-call/closure)
		    ((OPEN-EXTERNAL) make-call/stack-with-link)
		    ((OPEN-INTERNAL) false)
		    (else (error "Unknown callee type" callee)))))))
      (cond ((or (not caller) (procedure/ic? caller))
	     (if generator
		 (generator combination operator operands
			    invocation-prefix/null false)
		 (error "Calling internal procedure from IC procedure")))
	    ((procedure/external? caller)
	     (if generator
		 (generator combination operator operands
			    invocation-prefix/move-frame-up false)
		 (make-call/child combination operator operands
				  rtl:make-message-receiver:closure)))
	    (else
	     (if generator
		 (generator combination operator operands
			    invocation-prefix/internal->closure false)
		 (let ((block* (procedure-block callee)))
		   (cond ((block-child? block block*)
			  (make-call/child combination operator operands
					   rtl:make-message-receiver:stack))
			 ((block-sibling? block block*)
			  (make-call/stack combination operator operands
					   invocation-prefix/internal->sibling
					   false))
			 (else
			  (make-call/stack-with-link
			   combination operator operands
			   invocation-prefix/internal->ancestor
			   false))))))))))

;;;; Calls

(define (make-call/apply combination operator operands prefix continuation)
  (make-call true combination operator operands
    (lambda (frame-size)
      (rtl:make-invocation:apply frame-size
				 (prefix combination frame-size)
				 continuation))))

(define (make-call/unknown combination operator operands prefix
			   continuation)
  (let ((callee (subproblem-value (combination-operator combination))))
    ((if (reference? callee)
	 make-call/reference
	 make-call/apply)
     combination operator operands prefix continuation)))

;;; For now, use apply.  Later we can optimize for the cases where
;;; the callee's closing frame is easily available, such as calling a
;;; sibling, self-recursion, or an ancestor.

(define make-call/ic make-call/apply)

(define (make-call/primitive combination operator operands prefix continuation)
  (make-call false combination operator operands
   (let* ((prim (constant-value (combination-known-operator combination)))
	  (special-handler (assq prim (cdr special-primitive-handlers))))
     (if special-handler
	 ((cdr special-handler) combination prefix continuation)
	 (lambda (number-pushed)
	   (rtl:make-invocation:primitive
	    (1+ number-pushed)
	    (prefix combination number-pushed)
	    continuation
	    prim))))))

(define special-primitive-handlers
  (list 'SPECIAL-PRIMITIVE-HANDLERS))

(define define-special-primitive-handler
  (define-primitive-handler special-primitive-handlers))

(define (make-call/reference combination operator operands prefix continuation)
  (make-call false combination operator operands
    (lambda (number-pushed)
      (let ((operator (subproblem-value (combination-operator combination)))
	    (frame-size (1+ number-pushed)))
	(let ((variable (reference-variable operator))
	      (make-application
	       (lambda (operator)
		 (scfg*scfg->scfg!
		  (rtl:make-push operator)
		  (rtl:make-invocation:apply
		   frame-size
		   (prefix combination frame-size)
		   continuation)))))
	  (find-variable (reference-block operator) variable
	    (lambda (locative)
	      (make-application (rtl:make-fetch locative)))
	    (lambda (environment name)
	      (rtl:make-invocation:lookup
	       frame-size
	       (prefix combination number-pushed)
	       continuation
	       environment
	       (intern-scode-variable! (reference-block operator) name)))
	    (lambda (name)
	      (if (memq 'UUO-LINK (variable-declarations variable))
		  (rtl:make-invocation:uuo-link
		   frame-size
		   (prefix combination number-pushed)
		   continuation
		   name)
		  (let* ((temp (make-temporary))
			 (cell (rtl:make-fetch temp))
			 (contents (rtl:make-fetch cell)))
		    (let ((n1
			   (rtl:make-assignment
			    temp
			    (rtl:make-variable-cache name)))
			  (n2
			   (rtl:make-type-test (rtl:make-object->type contents)
					       (ucode-type reference-trap)))
			  (n3 (make-application contents))
			  (n4
			   (rtl:make-invocation:cache-reference
			    frame-size
			    (prefix combination number-pushed)
			    continuation
			    cell)))
		      (scfg-next-connect! n1 n2)
		      (pcfg-consequent-connect! n2 n4)
		      (pcfg-alternative-connect! n2 n3)
		      (make-scfg (cfg-entry-node n1)
				 (hooks-union (scfg-next-hooks n3)
					      (scfg-next-hooks n4)))))))))))))

(define (make-call/child combination operator operands make-receiver)
  (scfg*scfg->scfg!
   (make-receiver (block-frame-size (combination-block combination)))
   (make-call/stack-with-link combination operator operands
			      invocation-prefix/null false)))

(package (make-call/closure make-call/stack make-call/stack-with-link)

(define-export (make-call/closure combination operator operands prefix
				  continuation)
  (make-call true combination operator operands
    (internal-call combination prefix continuation 0)))

(define-export (make-call/stack combination operator operands prefix
				continuation)
  (stack-call combination operator operands prefix continuation 0))

(define-export (make-call/stack-with-link combination operator operands prefix
					  continuation)
  (scfg*scfg->scfg!
   (rtl:make-push
    (rtl:make-address
     (block-ancestor-or-self->locative
      (combination-block combination)
      (block-parent
       (procedure-block (combination-known-operator combination))))))
   (stack-call combination operator operands prefix continuation 1)))

(define (stack-call combination operator operands prefix continuation extra)
  (make-call false combination operator operands
    (internal-call combination prefix continuation extra)))

(define (internal-call combination prefix continuation extra)
  (lambda (number-pushed)
    (let ((operator (combination-known-operator combination)))
      ((if (procedure-rest operator)
	   rtl:make-invocation:lexpr
	   rtl:make-invocation:jump)
       number-pushed
       (prefix combination (+ number-pushed extra))
       continuation
       operator))))

)

(define (make-call push-operator? combination operator operands generator)
  (let ((callee (combination-known-operator combination))
	(n-operands (count-operands operands))
	(finish
	 (lambda (frame-size)
	   (scfg-append!
	    (scfg*->scfg!
	     (map (lambda (operand)
		    (transmit-values operand
		      (lambda (cfg prefix expression)
			(if expression
			    (scfg-append! cfg
					  prefix
					  (rtl:make-push expression))
			    cfg))))
		  (reverse operands)))
	    operator
	    (if push-operator?
		(transmit-values
		    (generate/rvalue
		     (subproblem-value (combination-operator combination)))
		  (lambda (prefix expression)
		    (scfg-append! prefix
				  (rtl:make-push expression)
				  (generator (1+ frame-size)))))
		(generator frame-size))))))
    (if (and callee
	     (procedure? callee)
	     (not (procedure-rest callee))
	     (stack-block? (procedure-block callee)))
	(let ((n-parameters (+ (length (procedure-required callee))
			       (length (procedure-optional callee)))))
	    (scfg*scfg->scfg!
	     (scfg*->scfg!
	      (let loop ((n (- n-parameters n-operands)))
		(if (zero? n)
		    '()
		    (cons (rtl:make-push (rtl:make-unassigned))
			  (loop (-1+ n))))))
	     (finish n-parameters)))
	(finish n-operands))))

(define (count-operands operands)
  (cond ((null? operands)
	 0)
	((transmit-values (car operands)
	   (lambda (cfg prefix expression)
	     expression))
	 (1+ (count-operands (cdr operands))))
	(else
	 (count-operands (cdr operands)))))

;;;; Prefixes

(define (invocation-prefix/null combination frame-size)
  '(NULL))

(define (invocation-prefix/move-frame-up combination frame-size)
  `(MOVE-FRAME-UP ,frame-size
		  ,(block-frame-size (combination-block combination))))

(define (invocation-prefix/internal->closure combination frame-size)
  ;; The message sender will shift the new stack frame down to the
  ;; correct position when it is done, then reset the stack pointer.
  `(APPLY-CLOSURE ,frame-size
		  ,(block-frame-size (combination-block combination))))

(define (invocation-prefix/internal->ancestor combination frame-size)
  (let ((block (combination-block combination)))
    `(APPLY-STACK ,frame-size
		  ,(block-frame-size block)
		  ,(-1+
		    (block-ancestor-distance
		     block
		     (block-parent
		      (procedure-block
		       (combination-known-operator combination))))))))

(define (invocation-prefix/internal->sibling combination frame-size)
   `(MOVE-FRAME-UP ,frame-size
		   ;; -1+ means reuse the existing static link.
		   ,(-1+ (block-frame-size (combination-block combination)))))