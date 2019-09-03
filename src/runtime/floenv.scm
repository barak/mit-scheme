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
;;; and saved in the thread records of threads that are not running.
;;;
;;; When switching threads, if the old thread's floating-point
;;; environment is #T meaning it cared about the floating-point
;;; environment, we grab the environment from the machine and stash
;;; it in that thread before entering the new thread.  During
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
	     (let ((fp-env? (thread-float-environment interrupted-thread)))
	       ;; If the thread was just interrupted, it can't have a
	       ;; saved environment -- only a marker indicating
	       ;; whether it is in use or not.
	       (assert (or (eqv? fp-env? #t) (eqv? fp-env? #f)))
	       (if fp-env?
		   (let ((fp-env ((ucode-primitive float-environment 0))))
		     (set-thread-float-environment! interrupted-thread fp-env)
		     fp-env)
		   #f))
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

(define-integrable (using-floating-point-environment?)
  (thread-float-environment (current-thread)))

(define-integrable (use-floating-point-environment!)
  (set-thread-float-environment! (current-thread) #t))

(define (flo:environment)
  (if (using-floating-point-environment?)
      ((ucode-primitive float-environment 0))
      #f))

(define (flo:set-environment! fp-env)
  ;; If we are transitioning back to the default environment, do it
  ;; once while the thread is still considered to be using the
  ;; environment so that the thread system will take care to set the
  ;; machine to the default environment.
  (if (not fp-env)
      ((ucode-primitive set-float-environment 1) default-environment))
  ;; Next, if we are transitioning back to the default environment,
  ;; mark the thread as not using the floating-point environment;
  ;; otherwise mark the thread as using it -- but do this _before_ we
  ;; set the machine state so that if we are preempted, the scheduler
  ;; will know to save and restore it.
  (set-thread-float-environment! (current-thread) (if fp-env #t #f))
  ;; Finally, set the machine state if we are transitioning to a
  ;; nondefault environment.
  (if fp-env
      ((ucode-primitive set-float-environment 1) fp-env)))

(define (flo:update-environment! fp-env)
  (use-floating-point-environment!)
  ((ucode-primitive update-float-environment 1)
   (or fp-env default-environment)))

(define default-environment)

(define (flo:default-environment)
  #f)

(define (reset-package!)
  (set! default-environment
	(without-interrupts
	 (lambda ()
	   ((ucode-primitive set-float-rounding-mode 1)
	    (%mode-name->number
	     (flo:default-rounding-mode)
	     '|#[(runtime floating-point-environment)reset-package!]|))
	   ((ucode-primitive clear-float-exceptions 1)
	    (flo:supported-exceptions))
	   ((ucode-primitive set-trapped-float-exceptions 1)
	    (flo:default-trapped-exceptions))
	   ((ucode-primitive float-environment 0)))))
  unspecific)

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
  (flo:exception:subnormal-operand float-subnormal-operand-exception 0)
  (flo:test-exceptions test-float-exceptions 1)
  (flo:save-exception-flags save-float-exception-flags 1)
  (flo:test-exception-flags test-float-exception-flags 2)
  (flo:trapped-exceptions trapped-float-exceptions 0)
  (flo:trappable-exceptions trappable-float-exceptions 0))

(define (flo:clear-exceptions! exceptions)
  (use-floating-point-environment!)
  ((ucode-primitive clear-float-exceptions 1) exceptions))

(define (flo:raise-exceptions! exceptions)
  (if (using-floating-point-environment?)
      ((ucode-primitive raise-float-exceptions 1) exceptions)))

(define (flo:restore-exception-flags! fexcept exceptions)
  (use-floating-point-environment!)
  ((ucode-primitive restore-float-exception-flags 2) fexcept exceptions))

(define (flo:set-trapped-exceptions! exceptions)
  (if (not (using-floating-point-environment?))
      (begin
	(use-floating-point-environment!)
	((ucode-primitive clear-float-exceptions 1) exceptions)))
  ((ucode-primitive set-trapped-float-exceptions 1) exceptions))

(define (flo:trap-exceptions! exceptions)
  (if (not (using-floating-point-environment?))
      (begin
	(use-floating-point-environment!)
	((ucode-primitive clear-float-exceptions 1) exceptions)))
  ((ucode-primitive trap-float-exceptions 1) exceptions))

(define (flo:untrap-exceptions! exceptions)
  ((ucode-primitive untrap-float-exceptions 1) exceptions))

(define (flo:defer-exception-traps!)
  (use-floating-point-environment!)
  ((ucode-primitive defer-float-exception-traps 0)))

(define (flo:default-trapped-exceptions)
  ;; By default, we trap nothing, following iEEE 754-2008's default
  ;; exception handling recommendation.
  ;;
  ;; This is a change from the past in MIT Scheme which used to trap
  ;; all the standard exceptions except for inexact-result.
  ;;
  ;; Note that for querying the status of floating-point operations,
  ;; the floating-point environment is not reliable in the current
  ;; thread until you touch it, e.g. with flo:clear-exceptions! or
  ;; flo:preserving-environment.
  0)

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
		 (n 'subnormal-operand (flo:exception:subnormal-operand)
		    '())))))))

(define (flo:names->exceptions names)
  (define (name->exceptions name)
    (case name
      ((divide-by-zero) (flo:exception:divide-by-zero))
      ((inexact-result) (flo:exception:inexact-result))
      ((invalid-operation) (flo:exception:invalid-operation))
      ((overflow) (flo:exception:overflow))
      ((underflow) (flo:exception:underflow))
      ((subnormal-operand) (flo:exception:subnormal-operand))
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

(define (flo:nan.0)
  (flo:make-nan #f #t 0))

(define (flo:+inf.0) +inf.0)
(define (flo:-inf.0) -inf.0)