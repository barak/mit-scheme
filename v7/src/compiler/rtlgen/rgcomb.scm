#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlgen/rgcomb.scm,v 1.13 1987/04/27 16:28:49 cph Exp $

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
  (lambda (combination offset rest-generator)
    ((cond ((combination-constant? combination) combination:constant)
	   ((let ((operator (combination-known-operator combination)))
	      (and operator
		   (normal-primitive-constant? operator)))
	    combination:primitive)
	   (else combination:normal))
     combination offset rest-generator)))

(define (combination:normal combination offset rest-generator)
  ;; For the time being, all close-coded combinations will return
  ;; their values in the value register.  If the value of a
  ;; combination is not a temporary, it is a value-ignore, which is
  ;; alright.
  (let ((value (combination-value combination)))
    (if (temporary? value)
	(let ((type (temporary-type value)))
	  (if type
	      (if (not (eq? 'VALUE type))
		  (error "COMBINATION:NORMAL: Bad temporary type" type))
	      (set-temporary-type! value 'VALUE)))))
  (if (generate:next-is-null? (snode-next combination) rest-generator)
      (combination:reduction combination offset)
      (combination:subproblem combination offset rest-generator)))

(define (combination:constant combination offset rest-generator)
  (let ((value (combination-value combination))
	(next (snode-next combination)))
    (cond ((temporary? value)
	   (generate-assignment (combination-block combination)
				value
				(vnode-known-value value)
				next
				offset
				rest-generator
				rvalue->sexpression))
	  ((value-ignore? value)
	   (generate:next next offset rest-generator))
	  (else (error "Unknown combination value" value)))))

(define (combination:primitive combination offset rest-generator)
  (let ((open-coder
	 (assq (constant-value (combination-known-operator combination))
	       primitive-open-coders)))
    (or (and open-coder
	     ((cdr open-coder) combination offset rest-generator))
	(combination:normal combination offset rest-generator))))

(define (define-open-coder primitive open-coder)
  (let ((entry (assq primitive primitive-open-coders)))
    (if entry
	(set-cdr! entry open-coder)
	(set! primitive-open-coders
	      (cons (cons primitive open-coder)
		    primitive-open-coders))))
  primitive)

(define primitive-open-coders
  '())

(define-open-coder pair?
  (lambda (combination offset rest-generator)
    (and (combination-compiled-for-predicate? combination)
	 (open-code:type-test combination offset rest-generator
			      (ucode-type pair) 0))))

(define-open-coder primitive-type?
  (lambda (combination offset rest-generator)
    (and (combination-compiled-for-predicate? combination)
	 (operand->index combination 0
	   (lambda (type)
	     (open-code:type-test combination offset rest-generator
				  type 1))))))

(define (open-code:type-test combination offset rest-generator type operand)
  (let ((next (snode-next combination))
	(operand (list-ref (combination-operands combination) operand)))
    (generate:subproblem operand offset
      (lambda (offset)
	(generate:predicate next offset rest-generator
	  (rvalue->pexpression (subproblem-value operand) offset
	    (lambda (expression)
	      (rtl:make-type-test (rtl:make-object->type expression)
				  type))))))))

(define-integrable (combination-compiled-for-predicate? combination)
  (eq? 'PREDICATE (combination-compilation-type combination)))

(define-open-coder car
  (lambda (combination offset rest-generator)
    (open-code:memory-reference combination offset rest-generator 0)))

(define-open-coder cdr
  (lambda (combination offset rest-generator)
    (open-code:memory-reference combination offset rest-generator 1)))

(define-open-coder cell-contents
  (lambda (combination offset rest-generator)
    (open-code:memory-reference combination offset rest-generator 0)))

(define-open-coder vector-length
  (lambda (combination offset rest-generator)
    (open-code-expression-1 combination offset rest-generator
      (lambda (operand)
	(rtl:make-cons-pointer
	 (rtl:make-constant (ucode-type fixnum))
	 (rtl:make-fetch (rtl:locative-offset operand 0)))))))

(define-open-coder vector-ref
  (lambda (combination offset rest-generator)
    (operand->index combination 1
      (lambda (index)
	(open-code:memory-reference combination offset rest-generator
				    (1+ index))))))

(define (open-code:memory-reference combination offset rest-generator index)
  (open-code-expression-1 combination offset rest-generator
    (lambda (operand)
      (rtl:make-fetch (rtl:locative-offset operand index)))))

(define (open-code-expression-1 combination offset rest-generator receiver)
  (let ((operand (car (combination-operands combination))))
    (generate:subproblem operand offset
      (lambda (offset)
	(generate-assignment (combination-block combination)
			     (combination-value combination)
			     (subproblem-value operand)
			     (snode-next combination)
			     offset
			     rest-generator
			     (lambda (rvalue offset receiver*)
			       (rvalue->sexpression rvalue offset
				 (lambda (expression)
				   (receiver* (receiver expression))))))))))

(define (operand->index combination n receiver)
  (let ((operand (list-ref (combination-operands combination) n)))
    (and (subproblem-known-constant? operand)
	 (let ((value (subproblem-constant-value operand)))
	   (and (integer? value)
		(not (negative? value))
		(receiver value))))))

;;;; Subproblems

(define (combination:subproblem combination offset rest-generator)
  (let ((block (combination-block combination))
	(finish
	 (lambda (offset delta call-prefix continuation-prefix)
	   (let ((continuation (make-continuation delta)))
	     (set-continuation-rtl-entry!
	      continuation
	      (scfg*node->node!
	       (scfg*scfg->scfg!
		(rtl:make-continuation-heap-check continuation)
		continuation-prefix)
	       (generate:next (snode-next combination) offset rest-generator)))
	     (scfg*node->node! (call-prefix continuation)
			       (combination:subproblem-body combination
							    (+ offset delta)
							    continuation))))))
    (cond ((ic-block? block)
	   ;; **** Actually, should only do this if the environment
	   ;; will be needed by the continuation.
	   (finish (1+ offset) 1
		   (lambda (continuation)
		     (scfg*scfg->scfg!
		      (rtl:make-push (rtl:make-fetch register:environment))
		      (rtl:make-push-return continuation)))
		   (rtl:make-pop register:environment)))
	  ((and (stack-block? block)
		(let ((operator (combination-known-operator combination)))
		  (and operator
		       (procedure? operator)
		       (procedure/open-internal? operator))))
	   (finish offset
		   (rtl:message-receiver-size:subproblem)
		   rtl:make-message-receiver:subproblem
		   (make-null-cfg)))
	  (else
	   (finish offset 1 rtl:make-push-return (make-null-cfg))))))

(define (combination:subproblem-body combination offset continuation)
  ((let ((operator (combination-known-operator combination)))
     (cond ((normal-primitive-constant? operator) make-call:primitive)
	   ((or (not operator) (not (procedure? operator))) make-call:unknown)
	   (else
	    (case (procedure/type operator)
	      ((OPEN-INTERNAL) make-call:stack-with-link)
	      ((OPEN-EXTERNAL) make-call:open-external)
	      ((CLOSURE) make-call:closure)
	      ((IC) make-call:ic)
	      (else (error "Unknown callee type" operator))))))
   combination offset invocation-prefix:null continuation))

;;;; Reductions

(define (combination:reduction combination offset)
  (let ((callee (combination-known-operator combination))
	(block (combination-block combination)))
    (define (choose-generator ic external internal)
      ((let ((caller (block-procedure block)))
	 (cond ((or (not caller) (procedure/ic? caller)) ic)
	       ((procedure/external? caller) external)
	       (else internal)))
       combination offset))
    (cond ((normal-primitive-constant? callee)
	   (choose-generator reduction:ic->primitive
			     reduction:external->primitive
			     reduction:internal->primitive))
	  ((or (not callee)
	       (not (procedure? callee)))
	   (choose-generator reduction:ic->unknown
			     reduction:external->unknown
			     reduction:internal->unknown))
	  (else
	   (case (procedure/type callee)
	     ((IC)
	      (choose-generator reduction:ic->ic
				reduction:external->ic
				reduction:internal->ic))
	     ((CLOSURE)
	      (choose-generator reduction:ic->closure
				reduction:external->closure
				reduction:internal->closure))
	     ((OPEN-EXTERNAL)
	      (choose-generator reduction:ic->open-external
				reduction:external->open-external
				reduction:internal->open-external))
	     ((OPEN-INTERNAL)
	      (choose-generator reduction:ic->child
				reduction:external->child
				(let ((block* (procedure-block callee)))
				  (cond ((block-child? block block*)
					 reduction:internal->child)
					((block-sibling? block block*)
					 reduction:internal->sibling)
					(else
					 reduction:internal->ancestor)))))
	     (else (error "Unknown callee type" callee)))))))

(define (reduction:ic->unknown combination offset)
  (make-call:unknown combination offset invocation-prefix:null false))

(define (reduction:ic->ic combination offset)
  (make-call:ic combination offset invocation-prefix:null false))

(define (reduction:ic->primitive combination offset)
  (make-call:primitive combination offset invocation-prefix:null false))

(define (reduction:ic->closure combination offset)
  (make-call:closure combination offset invocation-prefix:null false))

(define (reduction:ic->open-external combination offset)
  (make-call:open-external combination offset invocation-prefix:null false))

(define (reduction:ic->child combination offset)
  (error "Calling internal procedure from IC procedure"))

(define (reduction:external->unknown combination offset)
  (make-call:unknown combination offset invocation-prefix:move-frame-up false))

(define (reduction:external->ic combination offset)
  (make-call:ic combination offset invocation-prefix:move-frame-up false))

(define (reduction:external->primitive combination offset)
  (make-call:primitive combination offset invocation-prefix:move-frame-up
		       false))

(define (reduction:external->closure combination offset)
  (make-call:closure combination offset invocation-prefix:move-frame-up false))

(define (reduction:external->open-external combination offset)
  (make-call:open-external combination offset invocation-prefix:move-frame-up
			   false))

(define (reduction:external->child combination offset)
  (make-call:child combination offset
		   rtl:make-message-receiver:closure
		   rtl:message-receiver-size:closure))

(define (reduction:internal->unknown combination offset)
  (make-call:unknown combination offset invocation-prefix:internal->closure
		     false))

(define (reduction:internal->ic combination offset)
  (make-call:ic combination offset invocation-prefix:internal->closure false))

(define (reduction:internal->primitive combination offset)
  (make-call:primitive combination offset invocation-prefix:internal->closure
		       false))

(define (reduction:internal->closure combination offset)
  (make-call:closure combination offset invocation-prefix:internal->closure
		     false))

(define (reduction:internal->open-external combination offset)
  (make-call:open-external combination offset
			   invocation-prefix:internal->closure
			   false))

(define (reduction:internal->child combination offset)
  (make-call:child combination offset
		   rtl:make-message-receiver:stack
		   rtl:message-receiver-size:stack))

(define (reduction:internal->sibling combination offset)
  (make-call:stack combination offset invocation-prefix:internal->sibling
		   false))

(define (reduction:internal->ancestor combination offset)
  (make-call:stack-with-link combination offset
			     invocation-prefix:internal->ancestor false))

;;;; Calls

(define (make-call:apply combination offset invocation-prefix continuation)
  (make-call:push-operator combination offset
    (lambda (number-pushed)
      (rtl:make-invocation:apply number-pushed
				 (invocation-prefix combination number-pushed)
				 continuation))))

(define (make-call:lookup combination offset invocation-prefix continuation)
  (make-call:dont-push-operator combination offset
    (lambda (number-pushed)
      (let ((operator (subproblem-value (combination-operator combination))))
	(let ((block (reference-block operator))
	      (name (variable-name (reference-variable operator))))
	  (rtl:make-invocation:lookup
	   number-pushed
	   (invocation-prefix combination number-pushed)
	   continuation
	   (nearest-ic-block-expression block (+ offset number-pushed))
	   (intern-scode-variable! block name)))))))

(define (make-call:unknown combination offset invocation-prefix continuation)
  (let ((operator (subproblem-value (combination-operator combination))))
    ((cond ((or (not (reference? operator))
		(reference-to-known-location? operator))
	    make-call:apply)
	   ;; **** Need to add code for links here.
	   (else make-call:lookup))
     combination offset invocation-prefix continuation)))

;;; For now, use apply.  Later we can optimize for the cases where
;;; the callee's closing frame is easily available, such as calling a
;;; sibling, self-recursion, or an ancestor.

(define make-call:ic make-call:apply)

(define (make-call:primitive combination offset invocation-prefix continuation)
  (make-call:dont-push-operator combination offset
    (lambda (number-pushed)
      (rtl:make-invocation:primitive
       number-pushed
       (invocation-prefix combination number-pushed)
       continuation
       (constant-value (combination-known-operator combination))))))

(define (make-call:closure combination offset invocation-prefix continuation)
  (make-call:push-operator combination offset
    (external-call combination invocation-prefix continuation)))

(define (make-call:open-external combination offset invocation-prefix
				 continuation)
  (scfg*node->node!
   (rtl:make-push (rtl:make-fetch register:environment))
   (make-call:dont-push-operator combination offset
     (external-call combination invocation-prefix continuation))))

(define (external-call combination invocation-prefix continuation)
  (lambda (number-pushed)
    (let ((operator (combination-known-operator combination)))
      ((if (procedure-rest operator)
	   rtl:make-invocation:lexpr
	   rtl:make-invocation:jump)
       number-pushed
       (invocation-prefix combination number-pushed)
       continuation
       operator))))

(package (make-call:stack make-call:stack-with-link make-call:child)

(define-export (make-call:stack combination offset invocation-prefix
				continuation)
  (stack-call combination offset invocation-prefix continuation 0))

(define-export (make-call:stack-with-link combination offset invocation-prefix
					  continuation)
  (link-call combination offset invocation-prefix continuation 0))

(define-export (make-call:child combination offset make-receiver receiver-size)
  (scfg*node->node!
   (make-receiver (block-frame-size (combination-block combination)))
   (let ((extra (receiver-size)))
     (link-call combination (+ offset extra) invocation-prefix:null false
		extra))))

(define (link-call combination offset invocation-prefix continuation extra)
  (scfg*node->node!
   (rtl:make-push
    (rtl:make-address
     (block-ancestor-or-self->locative
      (combination-block combination)
      (block-parent (procedure-block (combination-known-operator combination)))
      offset)))
   (stack-call combination (1+ offset) invocation-prefix continuation
	       (1+ extra))))

(define (stack-call combination offset invocation-prefix continuation extra)
  (make-call:dont-push-operator combination offset
    (lambda (number-pushed)
      (let ((number-pushed (+ number-pushed extra))
	    (operator (combination-known-operator combination)))
	((if (procedure-rest operator)
	     rtl:make-invocation:lexpr
	     rtl:make-invocation:jump)
	 number-pushed
	 (invocation-prefix combination number-pushed)
	 continuation
	 operator)))))

)

;;;; Prefixes

(define (invocation-prefix:null combination number-pushed)
  '(NULL))

(define (invocation-prefix:move-frame-up combination number-pushed)
  `(MOVE-FRAME-UP ,number-pushed
		  ,(block-frame-size (combination-block combination))))

(define (invocation-prefix:internal->closure combination number-pushed)
  ;; The message sender will shift the new stack frame down to the
  ;; correct position when it is done, then reset the stack pointer.
  `(APPLY-CLOSURE ,number-pushed
		  ,(+ number-pushed
		      (block-frame-size (combination-block combination)))))

(define (invocation-prefix:internal->ancestor combination number-pushed)
  (let ((block (combination-block combination)))
    `(APPLY-STACK ,number-pushed
		  ,(+ number-pushed (block-frame-size block))
		  ,(-1+
		    (block-ancestor-distance
		     block
		     (block-parent
		      (procedure-block
		       (combination-known-operator combination))))))))

(define (invocation-prefix:internal->sibling combination number-pushed)
   `(MOVE-FRAME-UP ,number-pushed
		   ;; -1+ means reuse the existing static link.
		   ,(-1+ (block-frame-size (combination-block combination)))))

;;;; Call Sequence Kernels

(package (make-call:dont-push-operator make-call:push-operator)

(define (make-call-maker generate:operator wrap-n)
  (lambda (combination offset make-invocation)
    (let ((operator (combination-known-operator combination))
	  (operands (combination-operands combination)))
      (let ((n-operands (length operands))
	    (finish
	     (lambda (n offset)
	       (let operand-loop
		   ((operands (reverse operands))
		    (offset offset))
		 (if (null? operands)
		     (generate:operator (combination-operator combination)
					offset
		       (lambda (offset)
			 (cfg-entry-node (make-invocation (wrap-n n)))))
		     (subproblem->push (car operands) offset
		       (lambda (offset)
			 (operand-loop (cdr operands) offset))))))))
	(if (and operator
		 (procedure? operator)
		 (not (procedure-rest operator))
		 (stack-block? (procedure-block operator)))
	    (let ((n-parameters (+ (length (procedure-required operator))
				   (length (procedure-optional operator)))))
	      (let ((delta (- n-parameters n-operands)))
		(scfg*scfg->scfg! (scfg*->scfg! (push-n-unassigned delta))
				  (finish n-parameters (+ offset delta)))))
	    (finish n-operands offset))))))

(define (push-n-unassigned n)
  (if (zero? n)
      '()
      (cons (rtl:make-push (rtl:make-unassigned))
	    (push-n-unassigned (-1+ n)))))

(define (subproblem->push subproblem offset receiver)
  (generate:subproblem subproblem offset
    (lambda (offset)
      (scfg*node->node!
       (rvalue->sexpression (subproblem-value subproblem) offset rtl:make-push)
       (receiver (1+ offset))))))

(define-export make-call:dont-push-operator
  (make-call-maker generate:subproblem identity-procedure))

(define-export make-call:push-operator
  (make-call-maker subproblem->push 1+))

		   ,(-1+ (block-frame-size (combination-block combination)))))