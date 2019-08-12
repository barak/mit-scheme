#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; Floating-Point Environment
;;; package: (runtime floating-point-environment)

(declare (usual-integrations))

;;;; Floating-point environment

;;; A floating-point environment descriptor is either #F, representing
;;; the default environment, or a platform-dependent description of the
;;; environment encoded in a byte vector.  A floating-point environment
;;; descriptor may be represented by a platform-dependent byte vector
;;; even if it is operationally equivalent to the default environment.
;;;
;;; The floating-point environment is stored on the physical machine,
;;; saved in the thread records of threads that are not running, and
;;; cached in the thread record of the thread that is running.
;;;
;;; When the physical machine is updated, we invalidate the cache by
;;; setting the current thread's floating-point environment to #T.
;;; When switching threads, if the old thread's floating-point
;;; environment is #T, we grab the environment from the machine and
;;; stash it in that thread before entering the new thread.  During
;;; thread-switching, we need to be in the default floating-point
;;; environment so that the thread system logic doesn't get confused.
;;;
;;; The default environment must have a platform-independent
;;; representation so that threads that have not modified their
;;; floating-point environments can be saved to disk in platform-
;;; independent bands.

;;; The routines on this page are hooks for the thread system.

;;; Save the floating-point environment and enter the default
;;; environment for the thread timer interrupt handler.

(define (enter-default-float-environment interrupted-thread)
  (let ((fp-env
	 (if interrupted-thread
	     (let ((fp-env (thread-float-environment interrupted-thread)))
	       (if (eqv? fp-env #t)
		   (let ((fp-env ((ucode-primitive float-environment 0))))
		     (set-thread-float-environment! interrupted-thread fp-env)
		     fp-env)
		   fp-env))
	     ;; No idea what environment we're in.  Assume the worst.
	     ((ucode-primitive float-environment 0)))))
    (if fp-env
	((ucode-primitive set-float-environment 1) default-environment))
    fp-env))

;;; Restore the environment saved by ENTER-DEFAULT-FLOAT-ENVIRONMENT
;;; when resuming a thread from the thread timer interrupt handler
;;; without switching.

(define (restore-float-environment-from-default fp-env)
  (if fp-env
      ((ucode-primitive set-float-environment 1) fp-env)))

;;; Enter a floating-point environment for switching to a thread.

(define (enter-float-environment fp-env)
  ((ucode-primitive set-float-environment 1) (or fp-env default-environment)))

;;; Save a floating-point environment when a thread yields or is
;;; preempted and must let another thread run.  FP-ENV is absent when
;;; explicitly yielding with YIELD-CURRENT-THREAD, or is the result of
;;; ENTER-DEFAULT-FLOAT-ENVIRONMENT from the thread timer interrupt
;;; handler.

(define (maybe-save-thread-float-environment! thread #!optional fp-env)
  (if (eqv? #t (thread-float-environment thread))
      (set-thread-float-environment!
       thread
       (if (or (default-object? fp-env)
	       (eqv? #t fp-env))
	   ((ucode-primitive float-environment 0))
	   fp-env))))

(define (use-floating-point-environment!)
  (set-thread-float-environment! (current-thread) #t))

(define (flo:environment)
  (let ((fp-env (thread-float-environment (current-thread))))
    (if (eqv? fp-env #t)
	(let ((fp-env ((ucode-primitive float-environment 0))))
	  ;; Cache it now so we don't need to ask the machine again
	  ;; when we next switch threads.  There is a harmless race
	  ;; here if we are preempted.
	  (set-thread-float-environment! (current-thread) fp-env)
	  fp-env)
	fp-env)))

(define (flo:set-environment! fp-env)
  (let ((old-fp-env (thread-float-environment (current-thread))))
    (if (not (eqv? fp-env old-fp-env))
	(begin
	  ;; Update the thread cache first; if we updated the machine
	  ;; first, then we might be preempted after that but before
	  ;; updating the thread cache, and the thread starts running
	  ;; again, there would be nothing to set the machine straight.
	  (set-thread-float-environment! (current-thread) fp-env)
	  ((ucode-primitive set-float-environment 1)
	   (or fp-env default-environment))))))

(define (flo:update-environment! fp-env)
  (let ((old-fp-env (thread-float-environment (current-thread))))
    (if (not (eqv? fp-env old-fp-env))
	;; We need to prevent thread-switching between saving the
	;; floating-point environment in the thread record and updating
	;; the machine's state because we need the *old* state to be
	;; still in place when the update happens so that exceptions
	;; will be trapped.
	;;
	;; XXX We could just disable preemption, but we'd have to do
	;; that in DYNAMIC-WIND in case UPDATE-FLOAT-ENVIRONMENT
	;; signals an error, and DYNAMIC-WIND is super-expensive.
	(without-interrupts
	 (lambda ()
	   (set-thread-float-environment! (current-thread) fp-env)
	   ((ucode-primitive update-float-environment 1)
	    (or fp-env default-environment)))))))

(define default-environment)

(define (flo:default-environment)
  #f)

(define (reset-package!)
  (set! default-environment
	(without-interrupts
	 (lambda ()
	   (let ((fp-env ((ucode-primitive float-environment 0))))
	     ((ucode-primitive set-float-rounding-mode 1)
	      (%mode-name->number
	       (flo:default-rounding-mode)
	       '|#[(runtime floating-point-environment)reset-package!]|))
	     ((ucode-primitive clear-float-exceptions 1)
	      (flo:supported-exceptions))
	     ((ucode-primitive set-trapped-float-exceptions 1)
	      (flo:default-trapped-exceptions))
	     (let ((fp-env* ((ucode-primitive float-environment 0))))
	       ((ucode-primitive set-float-environment 1) fp-env)
	       fp-env*)))))
  (initialize-flonum-infinities!))

(define (initialize-package!)
  (reset-package!)
  (add-event-receiver! event:after-restore reset-package!))

;;;; Floating-point rounding mode

(define-primitives
  (float-rounding-modes 0)
  (get-float-rounding-mode 0)
  (set-float-rounding-mode 1))

(define float-rounding-mode-names
  '#(to-nearest toward-zero downward upward))

(define (flo:rounding-modes)
  (let ((n (vector-length float-rounding-mode-names))
	(m (float-rounding-modes)))
    (let loop ((i 0) (names '()))
      (if (fix:< i n)
	  (loop (fix:+ i 1)
		(if (fix:= (fix:and (fix:lsh 1 i) m) 0)
		    names
		    (cons (vector-ref float-rounding-mode-names i) names)))
	  names))))

(define (flo:default-rounding-mode)
  'to-nearest)

(define (flo:rounding-mode)
  (let ((m (get-float-rounding-mode)))
    (if (not (fix:< m (vector-length float-rounding-mode-names)))
	(error "Unknown float rounding mode:" m))
    (vector-ref float-rounding-mode-names m)))

(define (flo:set-rounding-mode! mode)
  (use-floating-point-environment!)
  (set-float-rounding-mode (%mode-name->number mode 'flo:set-rounding-mode!)))

(define (flo:with-rounding-mode mode thunk)
  (let ((mode (%mode-name->number mode 'flo:with-rounding-mode)))
    (flo:preserving-environment
     (lambda ()
       (use-floating-point-environment!)
       (set-float-rounding-mode mode)
       (thunk)))))

(define (%mode-name->number mode caller)
  (guarantee interned-symbol? mode caller)
  (let ((n (vector-length float-rounding-mode-names)))
    (let loop ((i 0))
      (if (not (fix:< i n))
	  (error:bad-range-argument mode caller))
      (if (eq? mode (vector-ref float-rounding-mode-names i))
	  i
	  (loop (fix:+ i 1))))))

;;;; Floating-point exceptions and trapping

(define-primitives
  (flo:have-environment? have-float-environment? 0)
  (flo:have-trap-enable/disable? have-float-trap-enable/disable? 0)
  (flo:supported-exceptions float-exceptions 0)
  (flo:exception:divide-by-zero float-divide-by-zero-exception 0)
  (flo:exception:invalid-operation float-invalid-operation-exception 0)
  (flo:exception:underflow float-underflow-exception 0)
  (flo:exception:overflow float-overflow-exception 0)
  (flo:exception:inexact-result float-inexact-result-exception 0)
  (flo:test-exceptions test-float-exceptions 1)
  (flo:save-exception-flags save-float-exception-flags 1)
  (flo:test-exception-flags test-float-exception-flags 2)
  (flo:trapped-exceptions trapped-float-exceptions 0)
  (flo:trappable-exceptions trappable-float-exceptions 0))

(define (flo:clear-exceptions! exceptions)
  (use-floating-point-environment!)
  ((ucode-primitive clear-float-exceptions 1) exceptions))

(define (flo:raise-exceptions! exceptions)
  (use-floating-point-environment!)
  ((ucode-primitive raise-float-exceptions 1) exceptions))

(define (flo:restore-exception-flags! fexcept exceptions)
  (use-floating-point-environment!)
  ((ucode-primitive restore-float-exception-flags 2) fexcept exceptions))

(define (flo:set-trapped-exceptions! exceptions)
  (use-floating-point-environment!)
  ((ucode-primitive set-trapped-float-exceptions 1) exceptions))

(define (flo:trap-exceptions! exceptions)
  (use-floating-point-environment!)
  ((ucode-primitive trap-float-exceptions 1) exceptions))

(define (flo:untrap-exceptions! exceptions)
  (use-floating-point-environment!)
  ((ucode-primitive untrap-float-exceptions 1) exceptions))

(define (flo:defer-exception-traps!)
  (use-floating-point-environment!)
  ((ucode-primitive defer-float-exception-traps 0)))

(define (flo:default-trapped-exceptions)
  ;; By default, we trap the standard IEEE 754 exceptions that Scheme
  ;; can safely run with trapped, in order to report errors as soon as
  ;; they happen.  Scheme cannot safely run with the inexact result
  ;; exception trapped (which you almost never want anyway), and there
  ;; are some non-standard exceptions which we will not trap in order
  ;; to keep behaviour consistent between host systems.
  ;;
  ;; XXX If you want to read the exceptions that don't trap by default,
  ;; you must disable interrupts so that the lazy floating-point
  ;; environment switching mechanism will work.  Is that too much of a
  ;; burden?
  (fix:or (fix:or (flo:exception:divide-by-zero)
		  (flo:exception:invalid-operation))
	  (fix:or (flo:exception:overflow)
		  (flo:exception:underflow))))

;++ Include machine-dependent bits, by number rather than by name.

(define (flo:exceptions->names exceptions)
  (define (n name bits tail)
    (if (fix:zero? (fix:and bits exceptions))
	tail
	(cons name tail)))
  (guarantee index-fixnum? exceptions 'flo:exceptions->names)
  (if (not (fix:zero? (fix:andc exceptions (flo:supported-exceptions))))
      (error:bad-range-argument exceptions 'flo:exceptions->names))
  (n 'divide-by-zero (flo:exception:divide-by-zero)
     (n 'inexact-result (flo:exception:inexact-result)
	(n 'invalid-operation (flo:exception:invalid-operation)
	   (n 'overflow (flo:exception:overflow)
	      (n 'underflow (flo:exception:underflow)
		 '()))))))

(define (flo:names->exceptions names)
  (define (name->exceptions name)
    (case name
      ((divide-by-zero) (flo:exception:divide-by-zero))
      ((inexact-result) (flo:exception:inexact-result))
      ((invalid-operation) (flo:exception:invalid-operation))
      ((overflow) (flo:exception:overflow))
      ((underflow) (flo:exception:underflow))
      (else (error:bad-range-argument names 'flo:names->exceptions))))
  (guarantee list-of-unique-symbols? names 'flo:names->exceptions)
  (reduce fix:or 0 (map name->exceptions names)))

;;;; Floating-point environment utilities

(define (flo:deferring-exception-traps procedure)
  (flo:preserving-environment
   (lambda ()
     (let ((environment (flo:defer-exception-traps!)))
       (begin0 (procedure)
	 (flo:update-environment! environment))))))

(define (flo:ignoring-exception-traps procedure)
  (flo:preserving-environment
   (lambda ()
     (flo:defer-exception-traps!)
     (procedure))))

(define (flo:preserving-environment procedure)
  (let ((environment (flo:environment)))
    (define (swap)
      (let ((temporary environment))
	(set! environment (flo:environment))
	(flo:set-environment! temporary)))
    (dynamic-wind swap procedure swap)))

(define (flo:with-default-environment procedure)
  (flo:preserving-environment
   (lambda ()
     (flo:set-environment! (flo:default-environment))
     (procedure))))

(define (flo:with-trapped-exceptions exceptions procedure)
  (flo:preserving-environment
   (lambda ()
     (flo:set-trapped-exceptions! exceptions)
     (procedure))))

(define (flo:with-exceptions-trapped exceptions procedure)
  (flo:preserving-environment
   (lambda ()
     (flo:trap-exceptions! exceptions)
     (procedure))))

(define (flo:with-exceptions-untrapped exceptions procedure)
  (flo:preserving-environment
   (lambda ()
     (flo:untrap-exceptions! exceptions)
     (procedure))))

(define flo:nan.0)
(define flo:+inf.0)
(define flo:-inf.0)
;;; ZERO can be eliminated after 9.3 is released.  It works around
;;; overly-aggressive constant folding in SF and LIAR.
(define (initialize-flonum-infinities!)
  (let ((zero (lambda () (identity-procedure 0.))))
    (if (flo:have-trap-enable/disable?)
        (begin
          (set! flo:nan.0
                (named-lambda (flo:nan.0)
                  (flo:with-exceptions-untrapped (flo:exception:invalid-operation)
                    (lambda ()
                      (flo:/ 0. (zero))))))
          (set! flo:+inf.0
                (named-lambda (flo:+inf.0)
                  (flo:with-exceptions-untrapped (flo:exception:divide-by-zero)
                    (lambda ()
                      (flo:/ +1. (zero))))))
          (set! flo:-inf.0
                (named-lambda (flo:-inf.0)
                  (flo:with-exceptions-untrapped (flo:exception:divide-by-zero)
                    (lambda ()
                      (flo:/ -1. (zero))))))
          unspecific)
        ;; This works on macOS.  YMMV.
        (begin
          (set! flo:nan.0 (named-lambda (flo:nan.0) (flo:/ 0. (zero))))
          (set! flo:+inf.0 (named-lambda (flo:+inf.0) (flo:/ +1. (zero))))
          (set! flo:-inf.0 (named-lambda (flo:-inf.0) (flo:/ -1. (zero))))
          unspecific))))