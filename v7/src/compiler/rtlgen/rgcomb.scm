;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1986 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; RTL Generation: Combinations

;;; $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlgen/rgcomb.scm,v 1.2 1986/12/20 23:48:42 cph Exp $

(declare (usual-integrations))
(using-syntax (access compiler-syntax-table compiler-package)

(define-generator combination-tag
  (lambda (combination offset)
    ((cond ((combination-constant? combination) combination:constant)
	   ((let ((operator (combination-known-operator combination)))
	      (and operator
		   (normal-primitive-constant? operator)))
	    combination:primitive)
	   (else combination:normal))
     combination offset)))

(define (combination:normal combination offset)
  ;; For the time being, all close-coded combinations will return
  ;; their values in the value register.  If the value of a
  ;; combination is not a temporary, it is either a value-register
  ;; or a value-ignore, which is alright.
  (let ((value (combination-value combination)))
    (if (temporary? value)
	(let ((type* (temporary-type value)))
	  (if type*
	      (if (not (eq? 'VALUE type*))
		  (error "COMBINATION:NORMAL: bad temporary type" type*))
	      (set-temporary-type! value 'VALUE)))))
  ((if (snode-next combination) combination:subproblem combination:reduction)
   combination offset))

(define (combination:constant combination offset)
  (let ((value (combination-value combination))
	(next (snode-next combination)))
    (cond ((or (value-register? value)
	       (value-temporary? value))
	   (generate-assignment (combination-block combination)
				value
				(combination-constant-value combination)
				next
				offset))
	  ((value-ignore? value)
	   (generate:next next))
	  (else (error "Unknown combination value" value)))))

(define (combination:primitive combination offset)
  (let ((open-coder
	 (assq (constant-value (combination-known-operator combination))
	       primitive-open-coders)))
    (or (and open-coder
	     ((cdr open-coder) combination offset))
	(combination:normal combination offset))))

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
  (lambda (combination offset)
    (and (combination-compiled-for-predicate? combination)
	 (open-code:type-test combination offset (ucode-type pair) 0))))

(define-open-coder primitive-type?
  (lambda (combination offset)
    (and (combination-compiled-for-predicate? combination)
	 (operand->index combination 0
	   (lambda (type)
	     (open-code:type-test combination offset type 1))))))

(define (open-code:type-test combination offset type operand)
  (let ((next (snode-next combination))
	(operand (list-ref (combination-operands combination) operand)))
    (scfg*pcfg->pcfg!
     (generate:cfg (subproblem-cfg operand) offset)
     (pcfg*scfg->pcfg!
      (rvalue->pexpression (subproblem-value operand) offset
	(lambda (expression)
	  (rtl:make-type-test (rtl:make-object->type expression) type)))
      (generate:next (pnode-consequent next) offset)
      (generate:next (pnode-alternative next) offset)))))

(define-open-coder car
  (lambda (combination offset)
    (open-code:memory-reference combination offset 0)))

(define-open-coder cdr
  (lambda (combination offset)
    (open-code:memory-reference combination offset 1)))

(define-open-coder cell-contents
  (lambda (combination offset)
    (open-code:memory-reference combination offset 0)))

(define-open-coder vector-length
  (lambda (combination offset)
    (open-code-expression-1 combination offset
      (lambda (operand)
	(rtl:make-cons-pointer
	 (rtl:make-constant (ucode-type fixnum))
	 (rtl:make-fetch (rtl:locative-offset operand 0)))))))

(define-open-coder vector-ref
  (lambda (combination offset)
    (operand->index combination 1
      (lambda (index)
	(open-code:memory-reference combination offset index)))))

(define (open-code:memory-reference combination offset index)
  (open-code-expression-1 combination offset
    (lambda (operand)
      (rtl:make-fetch (rtl:locative-offset operand index)))))

(define (open-code-expression-1 combination offset receiver)
  (let ((operand (car (combination-operands combination))))
    (scfg*scfg->scfg!
     (generate:cfg (subproblem-cfg operand) offset)
     (rvalue->sexpression (subproblem-value operand)
       (lambda (expression)
	 (generate-assignment (combination-block combination)
			      (combination-value combination)
			      (receiver expression)
			      (snode-next combination)
			      offset))))))

(define (operand->index combination n receiver)
  (let ((operand (list-ref (combination-operands combination) n)))
    (and (subproblem-known-constant? operand)
	 (let ((value (subproblem-constant-value operand)))
	   (and (integer? value)
		(not (negative? value))
		(receiver value))))))

(define-integrable (combination-compiled-for-predicate? combination)
  (eq? 'PREDICATE (combination-compilation-type combination)))

;;;; Subproblems

(define (combination:subproblem combination offset)
  (let ((block (combination-block combination))
	(finish
	 (lambda (offset delta call-prefix continuation-prefix)
	   (let ((continuation
		  (make-continuation
		   (scfg*scfg->scfg! continuation-prefix
				     (generate:next (snode-next combination)
						    offset))
		   delta)))
	     (scfg*scfg->scfg! (call-prefix continuation)
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
		       (stack-procedure? operator))))
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
	   ((ic-procedure? operator) make-call:ic)
	   ((closure-procedure? operator) make-call:closure)
	   ((stack-procedure? operator)
	    (let ((block (combination-block combination)))
	      (cond ((stack-block? block) make-call:stack-with-link)
		    ((ic-block? block)
		     (error "IC procedure calling stack procedure"
			    combination))
		    (else (error "Unknown caller type" block)))))
	   (else (error "Unknown callee type" operator))))
   combination offset invocation-prefix:null continuation))

;;;; Reductions

(define (combination:reduction combination offset)
  (let ((operator (combination-known-operator combination))
	(block (combination-block combination)))
    (define (choose-generator ic closure stack)
      ((cond ((ic-block? block) ic)
	     ((closure-procedure-block? block) closure)
	     ((stack-procedure-block? block) stack)
	     (else (error "Unknown caller type" block)))
       combination offset))
    (cond ((normal-primitive-constant? operator)
	   (choose-generator reduction:ic->primitive
			     reduction:closure->primitive
			     reduction:stack->primitive))
	  ((or (not operator)
	       (not (procedure? operator)))
	   (choose-generator reduction:ic->unknown
			     reduction:closure->unknown
			     reduction:stack->unknown))
	  ((ic-procedure? operator)
	   (choose-generator reduction:ic->ic
			     reduction:closure->ic
			     reduction:stack->ic))
	  ((closure-procedure? operator)
	   (choose-generator reduction:ic->closure
			     reduction:closure->closure
			     reduction:stack->closure))
	  ((stack-procedure? operator)
	   (choose-generator reduction:ic->stack
			     reduction:closure->stack
			     (let ((block* (procedure-block operator)))
			       (cond ((block-child? block block*)
				      reduction:stack->child)
				     ((block-sibling? block block*)
				      reduction:stack->sibling)
				     (else
				      reduction:stack->ancestor)))))
	  (else (error "Unknown callee type" operator)))))

(define (reduction:ic->unknown combination offset)
  (make-call:unknown combination offset invocation-prefix:null false))

(define (reduction:ic->ic combination offset)
  (make-call:ic combination offset invocation-prefix:null false))

(define (reduction:ic->primitive combination offset)
  (make-call:primitive combination offset invocation-prefix:null false))

(define (reduction:ic->closure combination offset)
  (make-call:closure combination offset invocation-prefix:null false))

(define (reduction:ic->stack combination offset)
  ;; The callee must be a child of the caller, but in that case it
  ;; should be a closure -- so this is a logic error.
  (error "IC procedure calling stack procedure" combination))

(define (reduction:closure->unknown combination offset)
  (make-call:unknown combination offset invocation-prefix:move-frame-up false))

(define (reduction:closure->ic combination offset)
  (make-call:ic combination offset invocation-prefix:move-frame-up false))

(define (reduction:closure->primitive combination offset)
  (make-call:primitive combination offset invocation-prefix:move-frame-up
		       false))

(define (reduction:closure->closure combination offset)
  (make-call:closure combination offset invocation-prefix:move-frame-up false))

(define (reduction:closure->stack combination offset)
  ;; The callee is known to be a child of the caller because the
  ;; analyzer prohibits the other cases.
  (make-call:child combination offset
		   rtl:make-message-receiver:closure
		   rtl:message-receiver-size:closure))

(define (reduction:stack->unknown combination offset)
  (make-call:unknown combination offset invocation-prefix:stack->closure
		     false))

(define (reduction:stack->ic combination offset)
  (make-call:ic combination offset invocation-prefix:stack->closure false))

(define (reduction:stack->primitive combination offset)
  (make-call:primitive combination offset invocation-prefix:stack->closure
		        false))

(define (reduction:stack->closure combination offset)
  (make-call:closure combination offset invocation-prefix:stack->closure
		     false))

(define (reduction:stack->child combination offset)
  (make-call:child combination offset
		   rtl:make-message-receiver:stack
		   rtl:message-receiver-size:stack))

(define (reduction:stack->sibling combination offset)
  (make-call:stack combination offset invocation-prefix:stack->sibling false))

(define (reduction:stack->ancestor combination offset)
  (make-call:stack-with-link combination offset
			     invocation-prefix:stack->ancestor false))

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
    (lambda (number-pushed)
      (let ((operator (combination-known-operator combination)))
	((if (procedure-rest operator)
	     rtl:make-invocation:lexpr
	     rtl:make-invocation:jump)
	 number-pushed
	 (invocation-prefix combination number-pushed)
	 continuation
	 operator)))))

(define (make-call:stack combination offset invocation-prefix continuation)
  (make-call:dont-push-operator combination offset
    (lambda (number-pushed)
      (let ((operator (combination-known-operator combination)))
	((if (procedure-rest operator)
	     rtl:make-invocation:lexpr
	     rtl:make-invocation:jump)
	 number-pushed
	 (invocation-prefix combination number-pushed)
	 continuation
	 operator)))))

(define (make-call:stack-with-link combination offset invocation-prefix
				   continuation)
  (scfg*scfg->scfg!
   (rtl:make-push
    (rtl:make-address
     (block-ancestor-or-self->locative
      (combination-block combination)
      (block-parent (procedure-block (combination-known-operator combination)))
      offset)))
   (make-call:stack combination (1+ offset) invocation-prefix continuation)))

(define (make-call:child combination offset make-receiver receiver-size)
  (scfg*scfg->scfg!
   (make-receiver (block-frame-size (combination-block combination)))
   (make-call:stack-with-link combination (+ offset (receiver-size))
			      invocation-prefix:null false)))

;;;; Prefixes

(define (invocation-prefix:null combination number-pushed)
  '(NULL))

(define (invocation-prefix:move-frame-up combination number-pushed)
  `(MOVE-FRAME-UP ,number-pushed
		  ,(block-frame-size (combination-block combination))))

(define (invocation-prefix:stack->closure combination number-pushed)
  ;; The message sender will shift the new stack frame down to the
  ;; correct position when it is done, then reset the stack pointer.
  `(APPLY-CLOSURE ,number-pushed
		  ,(+ number-pushed
		      (block-frame-size (combination-block combination)))))

(define (invocation-prefix:stack->ancestor combination number-pushed)
  (let ((block (combination-block combination)))
    `(APPLY-STACK ,number-pushed
		  ,(+ number-pushed (block-frame-size block))
		  ,(block-ancestor-distance
		   block
		   (procedure-block
		    (combination-known-operator combination))))))

(define (invocation-prefix:stack->sibling combination number-pushed)
   `(MOVE-FRAME-UP ,number-pushed
		   ;; -1+ means reuse the existing static link.
		   ,(-1+ (block-frame-size (combination-block combination)))))

;;;; Call Sequence Kernels

(define (make-call-maker operator-cfg wrap-n)
  (lambda (combination offset make-invocation)
    (let ((operator (combination-known-operator combination))
	  (operands (combination-operands combination)))
      (let ((n-operands (length operands))
	    (finish
	     (lambda (n offset)
	       (scfg*->scfg!
		(let operand-loop
		    ((operands (reverse operands))
		     (offset offset))
		  (if (null? operands)
		      (list
		       (operator-cfg (combination-operator combination) offset)
		       (make-invocation (wrap-n n)))
		      (cons (subproblem->push (car operands) offset)
			    (operand-loop (cdr operands) (1+ offset)))))))))
	(if (and operator
		 (procedure? operator)
		 (not (procedure-rest operator))
		 (stack-block? (procedure-block operator)))
	    (let ((n-parameters (+ (length (procedure-required operator))
				   (length (procedure-optional operator)))))
	      (let ((delta (- n-parameters n-operands)))
		(scfg*scfg->scfg!
		 (scfg*->scfg! (push-n-unassigned delta))
		 (finish n-parameters (+ offset delta)))))
	    (finish n-operands offset))))))

(define (push-n-unassigned n)
  (if (zero? n)
      '()
      (cons (rtl:make-push (rtl:make-unassigned))
	    (push-n-unassigned (-1+ n)))))

(define (subproblem->push subproblem offset)
  (scfg*scfg->scfg! (generate:cfg (subproblem-cfg subproblem) offset)
		    (rvalue->sexpression (subproblem-value subproblem) offset
					 rtl:make-push)))

(define make-call:dont-push-operator
  (make-call-maker subproblem-cfg identity-procedure))

(define make-call:push-operator
  (make-call-maker subproblem->push 1+))

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: (access rtl-generator-package compiler-package)
;;; Scheme Syntax Table: (access compiler-syntax-table compiler-package)
;;; Tags Table Pathname: (access compiler-tags-pathname compiler-package)
;;; End:
		   ,(-1+ (block-frame-size (combination-block combination)))))