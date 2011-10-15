#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

;;;; Floating-Point Environment

(define-primitives
  (flo:environment float-environment 0)
  (flo:set-environment! set-float-environment 1)
  (flo:defer-exception-traps! defer-float-exception-traps 0)
  (flo:update-environment! update-float-environment 1))

(define (flo:deferring-exception-traps procedure)
  (flo:preserving-environment
   (lambda ()
     (let ((environment (flo:defer-exception-traps!)))
       (let ((result (procedure)))
	 (flo:update-environment! environment)
	 result)))))

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

(define default-environment)

(define (flo:default-environment)
  default-environment)

(define (initialize-package!)
  (set! default-environment
	(let ((environment (flo:environment)))
	  (flo:set-rounding-mode! (flo:default-rounding-mode))
	  (flo:clear-exceptions! (flo:supported-exceptions))
	  (flo:set-trapped-exceptions! (flo:default-trapped-exceptions))
	  (let ((environment* (flo:environment)))
	    (flo:set-environment! environment)
	    environment*)))
  unspecific)

(define-primitives
  (float-rounding-modes 0)
  (get-float-rounding-mode 0)
  (set-float-rounding-mode 1))

(define float-rounding-mode-names
  '#(TO-NEAREST TOWARD-ZERO DOWNWARD UPWARD))

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
  'TO-NEAREST)

(define (flo:rounding-mode)
  (let ((m (get-float-rounding-mode)))
    (if (not (fix:< m (vector-length float-rounding-mode-names)))
	(error "Unknown float rounding mode:" m))
    (vector-ref float-rounding-mode-names m)))

(define (flo:set-rounding-mode! mode)
  (set-float-rounding-mode (%mode-name->number mode 'FLO:SET-ROUNDING-MODE!)))

(define (flo:with-rounding-mode mode thunk)
  (let ((mode (%mode-name->number mode 'FLO:WITH-ROUNDING-MODE)))
    (flo:preserving-environment
     (lambda ()
       (set-float-rounding-mode mode)
       (thunk)))))

(define (%mode-name->number mode caller)
  (guarantee-interned-symbol mode caller)
  (let ((n (vector-length float-rounding-mode-names)))
    (let loop ((i 0))
      (if (not (fix:< i n))
	  (error:bad-range-argument mode caller))
      (if (eq? mode (vector-ref float-rounding-mode-names i))
	  i
	  (loop (fix:+ i 1))))))

(define-primitives
  (flo:supported-exceptions float-exceptions 0)
  (flo:exception:divide-by-zero float-divide-by-zero-exception 0)
  (flo:exception:invalid-operation float-invalid-operation-exception 0)
  (flo:exception:underflow float-underflow-exception 0)
  (flo:exception:overflow float-overflow-exception 0)
  (flo:exception:inexact-result float-inexact-result-exception 0)
  (flo:test-exceptions test-float-exceptions 1)
  (flo:clear-exceptions! clear-float-exceptions 1)
  (flo:raise-exceptions! raise-float-exceptions 1)
  (flo:save-exception-flags save-float-exception-flags 1)
  (flo:test-exception-flags test-float-exception-flags 2)
  (flo:restore-exception-flags! restore-float-exception-flags 2)
  (flo:trapped-exceptions trapped-float-exceptions 0)
  (flo:set-trapped-exceptions! set-trapped-float-exceptions 1)
  (flo:trap-exceptions! trap-float-exceptions 1)
  (flo:untrap-exceptions! untrap-float-exceptions 1)
  (flo:trappable-exceptions trappable-float-exceptions 0))

(define (flo:default-trapped-exceptions)
  ;; By default, we trap the standard IEEE 754 exceptions that Scheme
  ;; can safely run with trapped, in order to report errors as soon as
  ;; they happen.  Scheme cannot safely run with the inexact result
  ;; exception trapped (which you almost never want anyway), and there
  ;; are some non-standard exceptions which we will not trap in order
  ;; to keep behaviour consistent between host systems.
  (fix:or (fix:or (flo:exception:divide-by-zero)
                  (flo:exception:invalid-operation))
          (fix:or (flo:exception:overflow)
                  (flo:exception:underflow))))

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

;++ Include machine-dependent bits, by number rather than by name.

(define (flo:exceptions->names exceptions)
  (define (n name bits tail)
    (if (fix:zero? (fix:and bits exceptions))
	tail
	(cons name tail)))
  (guarantee-index-fixnum exceptions 'FLO:EXCEPTIONS->NAMES)
  (if (not (fix:zero? (fix:andc exceptions (flo:supported-exceptions))))
      (error:bad-range-argument exceptions 'FLO:EXCEPTIONS->NAMES))
  (n 'DIVIDE-BY-ZERO (flo:exception:divide-by-zero)
     (n 'INEXACT-RESULT (flo:exception:inexact-result)
	(n 'INVALID-OPERATION (flo:exception:invalid-operation)
	   (n 'OVERFLOW (flo:exception:overflow)
	      (n 'UNDERFLOW (flo:exception:underflow)
		 '()))))))

(define (flo:names->exceptions names)
  (define (name->exceptions name)
    (case name
      ((DIVIDE-BY-ZERO) (flo:exception:divide-by-zero))
      ((INEXACT-RESULT) (flo:exception:inexact-result))
      ((INVALID-OPERATION) (flo:exception:invalid-operation))
      ((OVERFLOW) (flo:exception:overflow))
      ((UNDERFLOW) (flo:exception:underflow))
      (else (error:bad-range-argument names 'FLO:NAMES->EXCEPTIONS))))
  (guarantee-list-of-unique-symbols names 'FLO:NAMES->EXCEPTIONS)
  (reduce fix:or 0 (map name->exceptions names)))