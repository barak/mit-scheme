#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018 Massachusetts Institute of Technology

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

;;;; Tests of flonum exceptions

(declare (usual-integrations))

(define (no-op x)
  x)

(define (flo:subnormal? x)
  (flo:< (flo:abs x) flo:smallest-positive-normal))

(define assert-flonum
  (predicate-assertion flo:flonum? "object"))

(define (assert-nan object)
  (assert-flonum object)
  ((predicate-assertion flo:nan? "NaN") object))

(define (assert-inf object)
  (assert-flonum object)
  ((predicate-assertion flo:infinite? "infinity") object))

(define (assert-subnormal object)
  (assert-flonum object)
  ((predicate-assertion flo:subnormal? "subnormal") object))

(define assert-nonzero
  (predicate-assertion (lambda (x) (not (zero? x))) "nonzero"))

(define assert-nothing
  (predicate-assertion (lambda (x) x #t) "nothing"))

(define (with-failure-expected xfail procedure)
  (if (default-object? xfail)
      (procedure)
      (xfail procedure)))

(define (define-exception-flag-test name excname exception assertion procedure
	  #!optional xfail)
  (define-test (symbol name '/ excname '/ 'flag)
    (lambda ()
      (flo:preserving-environment
       (lambda ()
	 (flo:clear-exceptions! (flo:supported-exceptions))
	 ((lambda (body)
	    (if (flo:have-trap-enable/disable?)
		(flo:with-exceptions-untrapped (flo:supported-exceptions) body)
		(body)))
	  (lambda ()
	    (with-failure-expected xfail
	      (lambda ()
		(assertion (procedure))
		(assert-nonzero (flo:test-exceptions exception)))))))))))

(define (define-exception-trap-test name excname exception condition-type
	  procedure #!optional xfail)
  (define-test (symbol name '/ excname '/ 'trap)
    (lambda ()
      (flo:preserving-environment
       (lambda ()
	 (flo:clear-exceptions! (flo:supported-exceptions))
	 (with-failure-expected
	     (if (flo:have-trap-enable/disable?)
		 xfail
		 assert-error)
	   (lambda ()
	     (assert-error
	      (lambda ()
		(flo:with-exceptions-untrapped (flo:supported-exceptions)
		  (lambda ()
		    (flo:with-exceptions-trapped exception procedure))))
	      (list condition-type)))))))))

(define (define-invop-flag-test name procedure #!optional xfail)
  (define-exception-flag-test name 'invalid-operation
    (flo:exception:invalid-operation)
    assert-nan procedure xfail))

(define (define-invop-trap-test name procedure #!optional xfail)
  (define-exception-trap-test name 'invalid-operation
    (flo:exception:invalid-operation)
    condition-type:invalid-floating-point-operation
    procedure xfail))

(define (define-invop-compare-test name procedure #!optional xfail)
  (define-exception-flag-test name 'invalid-operation
    (flo:exception:invalid-operation)
    assert-false procedure xfail)
  (define-exception-trap-test name 'invalid-operation
    (flo:exception:invalid-operation)
    condition-type:invalid-floating-point-operation
    procedure xfail))

(define (define-divbyzero-flag-test name procedure #!optional xfail)
  (define-exception-flag-test name 'divide-by-zero
    (flo:exception:divide-by-zero)
    assert-inf procedure xfail))

(define (define-divbyzero-trap-test name procedure #!optional xfail)
  (define-exception-trap-test name 'divide-by-zero
    (flo:exception:divide-by-zero)
    condition-type:floating-point-divide-by-zero
    procedure xfail))

(define (define-overflow-flag-test name procedure #!optional xfail)
  (define-exception-flag-test name 'overflow
    (flo:exception:overflow)
    assert-inf procedure xfail))

(define (define-overflow-trap-test name procedure #!optional xfail)
  (define-exception-trap-test name 'overflow
    (flo:exception:overflow)
    condition-type:floating-point-overflow
    procedure xfail))

(define (define-underflow-flag-test name procedure #!optional xfail)
  (define-exception-flag-test name 'underflow
    (flo:exception:underflow)
    assert-subnormal procedure xfail))

(define (define-underflow-trap-test name procedure #!optional xfail)
  (define-exception-trap-test name 'underflow
    (flo:exception:underflow)
    condition-type:floating-point-underflow
    procedure xfail))

(define (define-inexact-flag-test name procedure #!optional xfail)
  (define-exception-flag-test name 'inexact-result
    (flo:exception:inexact-result)
    assert-nothing procedure xfail))

(define (applicator procedure . arguments)
  (lambda ()
    (apply (no-op procedure)
	   (no-op arguments))))

;;; IEEE 754, Sec. 5.3.1 (see also Sec. 6.2)

(for-each
 (lambda (x)
   (define-invop-trap-test 'min (applicator flo:min x (flo:snan)))
   (define-invop-trap-test 'min (applicator flo:min (flo:snan) x))
   (define-invop-trap-test 'min-mag (applicator flo:min-mag x (flo:snan)))
   (define-invop-trap-test 'min-mag (applicator flo:min-mag (flo:snan) x))
   (define-invop-trap-test 'max (applicator flo:max x (flo:snan)))
   (define-invop-trap-test 'max (applicator flo:max (flo:snan) x))
   (define-invop-trap-test 'max-mag (applicator flo:max-mag x (flo:snan)))
   (define-invop-trap-test 'max-mag (applicator flo:max-mag (flo:snan) x)))
 '(-inf.0 -1. -0. +0. +1. +inf.0))

;;; IEEE 754, Sec. 7.2

(define-invop-flag-test 'flonum-multiply ;(b)
  (applicator flo:* 0. (flo:+inf.0)))
(define-invop-flag-test 'flonum-multiply ;(b)
  (applicator flo:* (flo:+inf.0) 0.))
(define-invop-trap-test 'flonum-multiply ;(b)
  (applicator flo:* 0. (flo:+inf.0)))
(define-invop-trap-test 'flonum-multiply ;(b)
  (applicator flo:* (flo:+inf.0) 0.))
;; XXX fma (c)
(define-invop-flag-test 'flonum-add+-inf	;(d)
  (applicator flo:+ (flo:+inf.0) (flo:-inf.0)))
(define-invop-flag-test 'flonum-add-+inf	;(d)
  (applicator flo:+ (flo:-inf.0) (flo:+inf.0)))
(define-invop-trap-test 'flonum-add+-inf	;(d)
  (applicator flo:+ (flo:+inf.0) (flo:-inf.0)))
(define-invop-trap-test 'flonum-add-+inf	;(d)
  (applicator flo:+ (flo:-inf.0) (flo:+inf.0)))
(define-invop-flag-test 'flonum-sub++inf	;(d)
  (applicator flo:- (flo:+inf.0) (flo:+inf.0)))
(define-invop-flag-test 'flonum-sub--inf	;(d)
  (applicator flo:- (flo:-inf.0) (flo:-inf.0)))
(define-invop-trap-test 'flonum-sub++inf	;(d)
  (applicator flo:- (flo:+inf.0) (flo:+inf.0)))
(define-invop-trap-test 'flonum-sub--inf	;(d)
  (applicator flo:- (flo:-inf.0) (flo:-inf.0)))
(define-invop-flag-test 'flonum-divide (applicator flo:/ 0. 0.)) ;(e)
(define-invop-trap-test 'flonum-divide (applicator flo:/ 0. 0.)) ;(e)
(define-invop-flag-test 'flonum-divide                           ;(e)
  (applicator (make-primitive-procedure 'flonum-divide) 0. 0.))
(define-invop-trap-test 'flonum-divide                           ;(e)
  (applicator (make-primitive-procedure 'flonum-divide) 0. 0.))
;; XXX remainder ;(f)
(define-invop-flag-test 'flonum-sqrt (applicator flo:sqrt -1.))	;(g)
(define-invop-trap-test 'flonum-sqrt (applicator flo:sqrt -1.))	;(g)

(define-invop-flag-test 'flonum-sqrt
  (applicator (make-primitive-procedure 'flonum-sqrt) -1.))
(define-invop-trap-test 'flonum-sqrt
  (applicator (make-primitive-procedure 'flonum-sqrt) -1.))

;;; IEEE 754-2008, Sec. 7.3

;; XXX Check sign of infinity.
(define-divbyzero-flag-test 'flonum-divide (applicator flo:/ 1. 0.))
(define-divbyzero-trap-test 'flonum-divide (applicator flo:/ 1. 0.))
(define-divbyzero-flag-test 'flonum-divide
  (applicator (make-primitive-procedure 'flonum-divide) 1. 0.))
(define-divbyzero-trap-test 'flonum-divide
  (applicator (make-primitive-procedure 'flonum-divide) 1. 0.))
(define-divbyzero-flag-test 'flonum-log (applicator flo:log 0.))
(define-divbyzero-trap-test 'flonum-log (applicator flo:log 0.))

;;; IEEE 754-2008, Sec. 7.4

;; XXX Check rounding modes.
(define-overflow-flag-test 'flonum-multiply
  (applicator flo:* flo:radix. (flo:scalbn 1. flo:normal-exponent-max)))
(define-overflow-trap-test 'flonum-multiply
  (applicator flo:* flo:radix. (flo:scalbn 1. flo:normal-exponent-max)))

;;; IEEE 754-2008, Sec. 7.5

(define-underflow-flag-test 'flonum-multiply
  (applicator flo:* .50000001 (flo:scalbn 1. flo:normal-exponent-min)))
(define-underflow-trap-test 'flonum-multiply
  (applicator flo:* .50000001 (flo:scalbn 1. flo:normal-exponent-min)))

;;; IEEE 754-2008, Sec. 7.6

(define-inexact-flag-test 'flonum-multiply
  (applicator flo:* .50000001 (flo:scalbn 1. flo:normal-exponent-min)))

;;; Miscellaneous inexact results

(define-inexact-flag-test 'flonum-add-1+ulp1/2
  (applicator flo:+ 1. (/ flo:ulp-of-one 2)))
(define-inexact-flag-test 'flonum-exp (applicator flo:exp -800.))

;;; IEEE 754-2008, Sec. 9.2, Table 9.1

(define-overflow-flag-test 'flonum-exp (applicator flo:exp 800.))
(define-overflow-trap-test 'flonum-exp (applicator flo:exp 800.))
(define-underflow-flag-test 'flonum-exp (applicator flo:exp -800.))
(define-underflow-trap-test 'flonum-exp (applicator flo:exp -800.))
;; XXX expm1, exp2, exp2m1, exp10, exp10m1

;; divide by zero covered above
(define-invop-flag-test 'flonum-log (applicator flo:log -1.))
(define-invop-trap-test 'flonum-log (applicator flo:log -1.))
;; XXX log1p, log21p, log101p

;; XXX hypot, rsqrt, compound, rootn, pown, pow, powr

(define-invop-flag-test 'flonum-sin (applicator flo:sin (flo:+inf.0)))
(define-invop-trap-test 'flonum-sin (applicator flo:sin (flo:+inf.0)))
;; XXX Not clear how to make tan underflow reliably.
(define-invop-flag-test 'flonum-cos (applicator flo:cos (flo:+inf.0)))
(define-invop-trap-test 'flonum-cos (applicator flo:cos (flo:+inf.0)))
(define-invop-flag-test 'flonum-tan (applicator flo:tan (flo:+inf.0)))
(define-invop-trap-test 'flonum-tan (applicator flo:tan (flo:+inf.0)))
;; XXX Not clear how to make tan underflow reliably.

;; XXX sinpi, cospi

;; XXX atanpi, atan2pi

(define-invop-flag-test 'flonum-asin (applicator flo:asin 2.))
(define-invop-trap-test 'flonum-asin (applicator flo:asin 2.))
;; XXX Not clear how to make asin underflow reliably.
(define-invop-flag-test 'flonum-acos (applicator flo:acos 2.))
(define-invop-trap-test 'flonum-acos (applicator flo:acos 2.))
;; XXX Not clear how to make atan underflow reliably.

;; XXX sinh, cosh, tanh, asinh, acosh, atanh

(let ((expect-failure
       (if (and (memq microcode-id/compiled-code-type '(x86-64 i386))
		(compiled-procedure? flo:=))
	   #!default
	   expect-failure)))
  (define-invop-compare-test 'flo:= (applicator flo:= 0. +nan.0) expect-failure)
  (define-invop-compare-test 'flo:= (applicator flo:= +nan.0 0.) expect-failure)
  (define-invop-compare-test 'flo:= (applicator flo:= +nan.0 +nan.0) expect-failure)

  (define-invop-compare-test 'flo:< (applicator flo:< 0. +nan.0) expect-failure)
  (define-invop-compare-test 'flo:< (applicator flo:< +nan.0 0.) expect-failure)
  (define-invop-compare-test 'flo:< (applicator flo:< +nan.0 +nan.0) expect-failure)

  (define-invop-compare-test 'flo:> (applicator flo:> 0. +nan.0) expect-failure)
  (define-invop-compare-test 'flo:> (applicator flo:> +nan.0 0.) expect-failure)
  (define-invop-compare-test 'flo:> (applicator flo:> +nan.0 +nan.0) expect-failure)

  (define-invop-compare-test 'flo:<= (applicator flo:<= 0. +nan.0) expect-failure)
  (define-invop-compare-test 'flo:<= (applicator flo:<= +nan.0 0.) expect-failure)
  (define-invop-compare-test 'flo:<= (applicator flo:<= +nan.0 +nan.0) expect-failure)

  (define-invop-compare-test 'flo:>= (applicator flo:>= 0. +nan.0) expect-failure)
  (define-invop-compare-test 'flo:>= (applicator flo:>= +nan.0 0.) expect-failure)
  (define-invop-compare-test 'flo:>= (applicator flo:>= +nan.0 +nan.0) expect-failure))
