#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/wind.scm,v 14.4 1992/02/08 15:08:46 cph Exp $

Copyright (c) 1988-92 Massachusetts Institute of Technology

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

;;;; State Space Model
;;; package: (runtime state-space)

(declare (usual-integrations))

;;; A STATE-SPACE is a tree of STATE-POINTs, except that the pointers
;;; in the tree point towards the root of the tree rather than its
;;; leaves.  These pointers are the NEARER-POINT of each point.

;;; Each point in the space has two procedures, TO-NEARER and
;;; FROM-NEARER. To move the root of the space to an adjacent point,
;;; one executes the FROM-NEARER of that point, then makes the
;;; TO-NEARER and FROM-NEARER of the old root be the FROM-NEARER and
;;; TO-NEARER of the new root, respectively.

(define-integrable with-stack-marker
  (ucode-primitive with-stack-marker 3))

(define-structure (state-space
		   (conc-name state-space/)
		   (constructor %make-state-space))
  nearest-point)

(define (make-state-space)
  (let ((space (%make-state-space '())))
    ;; Save the state space in the TO-NEARER field of the root point,
    ;; because it is needed by %TRANSLATE-TO-STATE-POINT.
    (set-state-space/nearest-point! space (make-state-point false space false))
    space))

(define-structure (state-point (conc-name state-point/))
  nearer-point
  to-nearer
  from-nearer)

(define (%execute-at-new-state-point space before during after)
  (let ((old-root
	 (without-interrupts
	  (lambda ()
	    (let ((old-root (state-space/nearest-point space)))
	      (let ((new-point (make-state-point false space false)))
		(set-state-point/nearer-point! old-root new-point)
		(set-state-point/to-nearer! old-root before)
		(set-state-point/from-nearer! old-root after)
		(set-state-space/nearest-point! space new-point))
	      (before)
	      old-root)))))
    (let ((value
	   (with-stack-marker during %translate-to-state-point old-root)))
      (%translate-to-state-point old-root)
      value)))

(define (%translate-to-state-point point)
  (without-interrupts
   (lambda ()
     (let find-nearest ((point point) (chain '()))
       (let ((nearer-point (state-point/nearer-point point)))
	 (if nearer-point
	     (find-nearest nearer-point (cons point chain))
	     (let ((space (state-point/to-nearer point)))
	       (let traverse-chain ((old-root point) (chain chain))
		 (if (not (null? chain))
		     (let ((new-root (car chain)))
		       ;; Move to NEW-ROOT.
		       (let ((to-nearer (state-point/to-nearer new-root))
			     (from-nearer (state-point/from-nearer new-root)))
			 (set-state-point/nearer-point! old-root new-root)
			 (set-state-point/to-nearer! old-root from-nearer)
			 (set-state-point/from-nearer! old-root to-nearer)
			 (set-state-point/nearer-point! new-root false)
			 (set-state-point/to-nearer! new-root space)
			 (set-state-point/from-nearer! new-root false)
			 (set-state-space/nearest-point! space new-root)
			 (with-stack-marker from-nearer
			   set-interrupt-enables! interrupt-mask/gc-ok))
		       ;; Disable interrupts again in case FROM-NEARER
		       ;; re-enabled them.
		       (set-interrupt-enables! interrupt-mask/gc-ok)
		       ;; Make sure that NEW-ROOT is still the root,
		       ;; because FROM-NEARER might have moved it.  If
		       ;; it has been moved, find the new root, and
		       ;; adjust CHAIN as needed.
		       (let find-root ((chain chain))
			 (let ((nearer-point
				(state-point/nearer-point (car chain))))
			   (cond ((not nearer-point)
				  ;; (CAR CHAIN) is the root.
				  (traverse-chain (car chain) (cdr chain)))
				 ((and (not (null? (cdr chain)))
				       (eq? nearer-point (cadr chain)))
				  ;; The root has moved along CHAIN.
				  (find-root (cdr chain)))
				 (else
				  ;; The root has moved elsewhere.
				  (find-nearest nearer-point
						chain)))))))))))))))

(define-integrable (guarantee-state-space space procedure)
  (if (not (state-space? space))
      (error:wrong-type-argument space "state space" procedure)))

(define-integrable (guarantee-state-point point procedure)
  (if (not (state-point? point))
      (error:wrong-type-argument point "state point" procedure)))

(define (current-state-point space)
  (guarantee-state-space space current-state-point)
  (state-space/nearest-point space))

(define (execute-at-new-state-point space before during after)
  (guarantee-state-space space execute-at-new-state-point)
  (%execute-at-new-state-point space before during after))

(define (translate-to-state-point point)
  (guarantee-state-point point translate-to-state-point)
  (%translate-to-state-point point))

(define (state-point/space point)
  (guarantee-state-point point state-point/space)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (let loop ((point point))
      (let ((nearer-point (state-point/nearer-point point)))
	(if nearer-point
	    (loop nearer-point)
	    (begin
	      (set-interrupt-enables! interrupt-mask)
	      point))))))

(define state-space:global)
(define state-space:local)

(define (shallow-fluid-bind before during after)
  (%execute-at-new-state-point state-space:global before during after))

(define (dynamic-wind before during after)
  (let ((fluid-bindings (state-space/nearest-point state-space:global)))
    (%execute-at-new-state-point
     state-space:local
     (lambda ()
       (%translate-to-state-point fluid-bindings)
       (before))
     during
     (lambda ()
       (%translate-to-state-point fluid-bindings)
       (after)))))

(define (initialize-package!)
  (set! state-space:global (make-state-space))
  (set! state-space:local (make-state-space))
  unspecific)

(define-structure (dynamic-state (conc-name dynamic-state/))
  (global false read-only true)
  (local false read-only true))

(define (get-dynamic-state)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (let ((state
	   (make-dynamic-state
	    (state-space/nearest-point state-space:global)
	    (state-space/nearest-point state-space:local))))
      (set-interrupt-enables! interrupt-mask)
      state)))

(define (set-dynamic-state! state global-only?)
  (if (not (dynamic-state? state))
      (error:wrong-type-argument state "dynamic state" set-dynamic-state!))
  (if (not global-only?)
      (%translate-to-state-point (dynamic-state/local state)))
  (%translate-to-state-point (dynamic-state/global state)))

(define (merge-dynamic-state state point)
  (let ((space (state-point/space point))
	(global (dynamic-state/global state))
	(local (dynamic-state/local state)))
    (cond ((eq? space (state-point/space global))
	   (make-dynamic-state point local))
	  ((eq? space (state-point/space local))
	   (make-dynamic-state global point))
	  (else
	   state))))