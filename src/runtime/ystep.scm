#| -*-Scheme-*-

$Id$

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

;;;; YStep - a step away from ZStep
;;; package: (runtime stepper)

(declare (usual-integrations))

(define-structure (stepper (constructor make-stepper (stack)))
  (stack '())
  (run? #f)				;#t => run; #f => step
  (step-over #f)			;#f or top node of step-over
  (step-until? #f)			;the step-over is really a step-until
  next					;continuation of stepped program
  continuation
  last-event				;last thing that happened
  (finished #f)				;when completed, stack is
					;empty and this points to top node
  hooks					;low-level stepper hooks
  (trace '())				;low-level trace recording
  )

(define (stack-push! state node)
  (set-stepper-stack! state (cons node (stepper-stack state))))

(define (stack-pop! state)
  (set-stepper-stack! state (cdr (stepper-stack state))))

(define (stack-top state)
  (car (stepper-stack state)))

(define (stack-bottom state)
  (car (last-pair (stepper-stack state))))

(define (stack-empty? state)
  (null? (stepper-stack state)))

(define (stepper-root-node state)
  (if (stack-empty? state)
      (stepper-finished state)
      (stack-bottom state)))

;;; The magic numbers here represent the number of eval and return
;;; events that occur during the startup process.  They will very
;;; likely have to change when the system changes.

(define (step-form expression environment)
  ;; start a new evaluation
  (step-start (make-ynode #f 'TOP ynode-exp:top-level)
	      (lambda () (eval expression environment))
	      (if (stepper-compiled?) 0 6)
	      (if (stepper-compiled?) 1 5)))

(define (step-proceed)
  ;; proceed from breakpoint
  (step-start (make-ynode #f 'PROCEED ynode-exp:proceed)
	      (lambda () (continue))
	      (if (stepper-compiled?) 0 4)
	      (if (stepper-compiled?) 5 7)))

(define (stepper-compiled?)
  (compiled-procedure? (lambda () unspecific)))

(define (step-start top-node thunk skip-evals skip-returns)
  (if (not (step-hooks-present?))
      (error "Sorry, this copy of Scheme does not support stepping."))
  (let ((state (make-stepper (list top-node))))
    (set-stepper-hooks! state (make-stepper-hooks state))
    (set-stepper-next! state
		       (lambda ()
			 (dummy-eval-step
			  (make-starting-hooks state skip-evals skip-returns))
			 (thunk)))
    (step-output-initialize state)
    (step state)))

(define (step state)
  (set-stepper-run?! state #f)
  (raw-step state))

(define (step-run state)
  (set-stepper-run?! state #t)
  (raw-step state))

(define (step-quit state)
  ;; [entry] not working yet
  (dummy-eval-step no-step-hooks)
  ((stepper-next state)))

(define (step-n state n)
  (do ((n n (- n 1))
       (value unspecific (step state)))
      ((<= n 0) value)))

(define (step-over state)
  (set-stepper-step-until?! state #f)
  (step-over-1 state))

(define (step-until state)
  (set-stepper-step-until?! state #t)
  (step-over-1 state))

(define (step-until-visibly state)
  (set-stepper-step-until?! state 'ANIMATE)
  (step-over-1 state))

(define (step-over-1 state)
  (if (not (eq? (car (stepper-last-event state)) 'CALL))
      (error "Last event was not a call:" (stepper-last-event state)))
  (set-stepper-step-over! state (stack-top state))
  (new-ynode-type! (stack-top state)
		   (if (stepper-step-until? state) 'EVAL 'STEP-OVER))
  (raw-step state))

(define (raw-step state)
  ;; the workhorse
  (if (stepper-finished state)
      (step-output-final-result state (ynode-result (stepper-finished state)))
      (begin
	(set-stepper-next! state
			   (call-with-current-continuation
			    (lambda (kk)
			      (set-stepper-continuation! state kk)
			      ((stepper-next state)))))
	(if (stepper-run? state)
	    (raw-step state)
	    (step-output state #f)))))

;;; Output Stubs:

(define (step-output-initialize state)
  state
  unspecific)

(define (step-output state redisplay?)
  state redisplay?
  unspecific)

(define (step-output-final-result state value)
  state
  value)

;;;; Low-level Hooks

(define (make-stepper-hooks state)
  (letrec
      ((hooks
	(hunk3-cons
	 (lambda (expr env)
	   (hook-record state
			(list 'EVAL (map-reference-trap (lambda () expr)) env))
	   (process-eval state (map-reference-trap (lambda () expr)))
	   (primitive-eval-step expr env hooks))
	 (lambda (proc . args)
	   (hook-record state
			(list 'APPLY
			      proc
			      (map (lambda (arg)
				     (map-reference-trap (lambda () arg)))
				   args)))
	   (process-apply state proc)
	   (primitive-apply-step proc args hooks))
	 (lambda (value)
	   (hook-record state
			(list 'RETURN (map-reference-trap (lambda () value))))
	   (process-return state (map-reference-trap (lambda () value)))
	   (primitive-return-step value hooks)))))
    hooks))

(define (make-starting-hooks state skip-evals skip-returns)
  (letrec
      ((hooks
	(hunk3-cons
	 (lambda (expr env)
	   (if (and (<= skip-evals 0) (<= skip-returns 0))
	       ((system-hunk3-cxr0 (stepper-hooks state)) expr env)
	       (begin
		 (set! skip-evals (- skip-evals 1))
		 (hook-record state (list 'EVAL expr env))
		 (primitive-eval-step expr env hooks))))
	 #f
	 (lambda (result)
	   (if (and (<= skip-evals 0) (<= skip-returns 0))
	       ((system-hunk3-cxr2 (stepper-hooks state)) result)
	       (begin
		 (set! skip-returns (- skip-returns 1))
		 (hook-record state (list 'RESULT result))
		 (primitive-return-step result hooks)))))))
    hooks))

(define no-step-hooks
  (hunk3-cons #f #f #f))

(define-integrable primitive-eval-step
  (ucode-primitive primitive-eval-step))

(define-integrable primitive-apply-step
  (ucode-primitive primitive-apply-step))

(define-integrable primitive-return-step
  (ucode-primitive primitive-return-step))

;;;; Worker Bees

(define (process-eval state exp)
  (if (reduction? exp (ynode-exp (stack-top state)))
      (process-reduction state))
  (let ((node
	 (make-ynode (and (not (stack-empty? state))
			  (stack-top state))
		     (if (and (stepper-step-over state)
			      (not (stepper-step-until? state)))
			 'STEPPED-OVER
			 'EVAL)
		     exp)))
    (stack-push! state node)
    (set-stepper-last-event! state `(CALL ,node))
    (maybe-redisplay state)))

(define (process-apply state proc)
  (if (compound-procedure? proc)
      (process-reduction state)))

(define (process-return state result)
  (if (stepper-step-over state)
      (maybe-end-step-over state))
  (let ((node
	 (let ((node (stack-top state)))
	   (if (eq? (ynode-type node) 'PROCEED)
	       (ynode-splice-under node)
	       (begin
		 (stack-pop! state)
		 node)))))
    (new-ynode-result! node result)
    (if (stack-empty? state)
	(set-stepper-finished! state node))
    (set-stepper-last-event! state `(RETURN ,node))
    (maybe-redisplay state)))

(define (maybe-redisplay state)
  (if (stepper-step-over state)
      (if (eq? (stepper-step-until? state) 'ANIMATE)
	  (step-output state #t))
      (call-with-current-continuation
       (lambda (k)
	 ((stepper-continuation state) (lambda () (k unspecific)))))))

(define (maybe-end-step-over state)
  (if (ynode-reduces-to? (stack-top state) (stepper-step-over state))
      (begin
	(set-stepper-step-over! state #f)
	(set-stepper-step-until?! state #f))))

(define (process-reduction state)
  (new-ynode-result! (stack-top state) ynode-result:reduced)
  (stack-pop! state))

(define (reduction? f1 f2)
  ;; Args are SCode expressions.  True if F2 is a reduction of F1.
  (cond ((conditional? f2)
	 (or (eq? f1 (conditional-consequent f2))
	     (eq? f1 (conditional-alternative f2))))
	((sequence? f2)
	 (eq? f1 (car (last-pair (sequence-actions f2)))))
	(else #f)))

;;;; Stepper nodes

(define-structure (ynode
		   (constructor make-ynode-1
				(parent type exp redisplay-flags)))
  ;; Could easily store environment as well.
  parent
  type
  (exp #f read-only #t)
  (children '())
  (result #f)
  (redisplay-flags #f read-only #t))

(define ynode-exp:top-level (list 'STEPPER-TOP-LEVEL))
(define ynode-exp:proceed   (list 'STEPPER-PROCEED))

(define (ynode-exp-special node)
  (let ((exp (ynode-exp node)))
    (and (or (eq? ynode-exp:top-level exp)
	     (eq? ynode-exp:proceed exp))
	 (car exp))))

(define ynode-result:waiting (list 'WAITING))
(define ynode-result:reduced (list 'REDUCED))
(define ynode-result:unknown (list 'UNKNOWN))

(define (ynode-result-special node)
  (let ((result (ynode-result node)))
    (and (or (eq? ynode-result:waiting result)
	     (eq? ynode-result:reduced result)
	     (eq? ynode-result:unknown result))
	 (car result))))

(define (ynode-reduced? node)
  (eq? (ynode-result node) ynode-result:reduced))

(define (make-ynode parent type exp)
  (let ((node
	 (make-ynode-1 parent type exp
		       (cons #t
			     (if parent (ynode-redisplay-flags parent) '())))))
    (set-ynode-result! node ynode-result:waiting)
    (if parent
	(set-ynode-children! parent (cons node (ynode-children parent))))
    (ynode-needs-redisplay! node)
    node))

(define (ynode-previous node)
  (let loop ((sibs (ynode-children (ynode-parent node))))
    (and (pair? sibs)
	 (if (eq? (car sibs) node)
	     (and (pair? (cdr sibs))
		  (cadr sibs))
	     (loop (cdr sibs))))))

(define (ynode-next node)
  (let loop ((sibs (ynode-children (ynode-parent node))))
    (and (pair? sibs)
	 (pair? (cdr sibs))
	 (if (eq? (cadr sibs) node)
	     (car sibs)
	     (loop (cdr sibs))))))

(define (ynode-value-node node)
  (if (ynode-reduced? node)
      (let ((next (ynode-next node)))
	(and next
	     (ynode-value-node next)))
      node))

(define (ynode-reduces-to? node reduces-to)
  (and node
       (or (eq? node reduces-to)
	   (let ((previous (ynode-previous node)))
	     (and previous
		  (ynode-reduced? previous)
		  (ynode-reduces-to? previous reduces-to))))))

(define (ynode-splice-under node)
  (let ((children (ynode-children node)))
    (set-ynode-children! node '())
    (let ((new-node (make-ynode node 'EVAL ynode-result:unknown)))
      (set-ynode-children! new-node children)
      (for-each (lambda (c) (set-ynode-parent! c new-node)) children)
      (let loop ((node new-node))
	(ynode-needs-redisplay! node)
	(for-each loop (ynode-children node)))
      new-node)))

(define (ynode-reductions node)
  (if (ynode-reduced? node)
      (let ((next (ynode-next node)))
	(cons next (ynode-reductions next)))
      '()))

(define (ynode-dependents node)
  ;; A dependent (misnomer) roughly means nodes that are directly
  ;; called by another node (which is not the same as children,
  ;; because reductions muck things up).
  (if (ynode-reduced? node)
      (cons (ynode-next node)
	    (ynode-direct-children node))
      (ynode-direct-children node)))

(define (ynode-direct-children node)
  ;; A "direct" child is one that is not a reduction of another child...
  (let loop ((children (ynode-children node)) (dependents '()))
    (if (null? children)
	dependents
	(loop (cdr children)
	      (if (and (not (null? (cdr children)))
		       (ynode-reduced? (cadr children)))
		  dependents
		  (cons (car children) dependents))))))

(define (ynode-hidden-children? node)
  ;; used to control drawing of arrow
  (and (eq? (ynode-type node) 'STEP-OVER)
       (not (null? (ynode-children node)))))

(define (ynode-needs-redisplay! ynode)
  (if (not (car (ynode-redisplay-flags ynode)))
      (begin
	(set-car! (ynode-redisplay-flags ynode) #t)
	(if (ynode-parent ynode)
	    (ynode-needs-redisplay! (ynode-parent ynode))))))

(define (ynode-needs-redisplay? ynode)
  (car (ynode-redisplay-flags ynode)))

(define (ynode-doesnt-need-redisplay! ynode)
  (set-car! (ynode-redisplay-flags ynode) #f))

(define (new-ynode-type! ynode type)
  (set-ynode-type! ynode type)
  (ynode-needs-redisplay! ynode))

(define (new-ynode-result! ynode result)
  (set-ynode-result! ynode result)
  (ynode-needs-redisplay! ynode))

(define (ynode-expand! node)
  (new-ynode-type! node 'EVAL)
  (for-each (lambda (dependent)
	      (if (eq? (ynode-type dependent) 'STEPPED-OVER)
		  (new-ynode-type! dependent 'STEP-OVER)))
	    (ynode-dependents node)))

(define (ynode-contract! node)
  (new-ynode-type! node 'STEP-OVER)
  (for-each (lambda (dependent)
	      (new-ynode-type! dependent 'STEPPED-OVER))
	    (ynode-reductions node)))

;;;; Miscellaneous

(define (dummy-eval-step hooks)
  (primitive-eval-step #f system-global-environment hooks))

(define (step-hooks-present?)
  (let ((flag #f))
    (dummy-eval-step
     (hunk3-cons #f
		 #f
		 (lambda (value)
		   (set! flag #t)
		   (primitive-return-step value no-step-hooks))))
    flag))

;;; Debugging trace:

;;; disabled, see next definition
;;; (define (hook-record state item)
;;;   (set-stepper-trace! state (cons item (stepper-trace state))))

(define-integrable (hook-record state item)
  ;; DEFINE-INTEGRABLE guarantees that argument in ITEM position is
  ;; not evaluated.
  state item
  unspecific)

(define (print-hook-trace state)
  (pp (let loop ((thing (stepper-trace state)))
	(cond ((list? thing) (map loop thing))
	      ((symbol? thing) thing)
	      (else (unsyntax thing))))))