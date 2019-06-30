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

;;;; Test of flonum operations

(declare (usual-integrations))

(define (define-enumerated-test name cases procedure)
  (define-test name
    (map (lambda (arguments)
           (lambda ()
             (apply procedure arguments)))
         cases)))

(define assert-exact-integer
  (predicate-assertion exact-integer? "integer"))

(define assert-flonum
  (predicate-assertion flo:flonum? "flonum"))

(define (assert-zero- object)
  (assert-eqv object -0.))

(define (assert-zero+ object)
  (assert-eqv object 0.))

(define (assert-inf- object)
  (assert-eqv object (flo:-inf.0)))

(define (assert-inf+ object)
  (assert-eqv object (flo:+inf.0)))

(define assert-nan
  (predicate-assertion flo:nan? "NaN"))

(define assert-qnan
  (predicate-assertion flo:qnan? "qNaN"))

(define assert-snan
  (predicate-assertion flo:snan? "sNaN"))

(define assert-normal
  (predicate-assertion flo:normal? "normal floating-point number"))

(define assert-subnormal
  (predicate-assertion flo:subnormal? "subnormal floating-point number"))

(define (eqv-nan? x y)
  (if (flo:nan? x)
      (and (flo:nan? y)
           (eqv? (flo:sign-negative? x) (flo:sign-negative? y))
           (eqv? (flo:nan-quiet? x) (flo:nan-quiet? y))
           (eqv? (flo:nan-payload x) (flo:nan-payload y)))
      (and (not (flo:nan? y))
           (eqv? x y))))

(define-comparator eqv-nan? 'eqv-nan?)

(define assert-eqv-nan
  (simple-binary-assertion eqv-nan? #f))

(define (assert-only-except/no-traps except procedure #!optional mask)
  (assert-eqv
   (flo:preserving-environment
    (lambda ()
      (flo:clear-exceptions! (flo:supported-exceptions))
      (no-traps
       (lambda ()
         (procedure)
         (flo:test-exceptions
          (if (default-object? mask)
              (flo:supported-exceptions)
              mask))))))
   except))

(define (assert-no-except/yes-traps procedure)
  (assert-eqv
   (flo:preserving-environment
    (lambda ()
      (flo:clear-exceptions! (flo:supported-exceptions))
      (yes-traps
       (lambda ()
         (procedure)
         (flo:test-exceptions (flo:supported-exceptions))))))
   0))

(define (with-expected-failure xfail body)
  (if (default-object? xfail)
      (body)
      (xfail body)))

(define (no-traps f)
  (if (flo:have-trap-enable/disable?)
      (flo:with-trapped-exceptions 0 f)
      (f)))

(define (yes-traps f)
  (if (flo:have-trap-enable/disable?)
      ;; XXX Should enable all traps.
      (begin
        (flo:clear-exceptions! (flo:supported-exceptions))
        (flo:with-trapped-exceptions
            (fix:or (flo:exception:invalid-operation) (flo:exception:overflow))
          f))
      (f)))

(define subnormal+ flo:smallest-positive-subnormal)
(define subnormal- (no-traps (lambda () (- subnormal+))))

(define-test 'radix
  (lambda ()
    (assert-exact-integer flo:radix)
    (assert-flonum flo:radix.)
    (assert-= flo:radix flo:radix.)))

(define-test 'error-ulp
  (lambda ()
    (assert-flonum flo:ulp-of-one)
    (assert-flonum flo:error-bound)
    (assert-> flo:ulp-of-one 0)
    (assert-< flo:ulp-of-one 1)
    (assert-= (/ flo:ulp-of-one 2) flo:error-bound)))

(define-test 'exponents
  (lambda ()
    (assert-exact-integer flo:normal-exponent-max)
    (assert-exact-integer flo:normal-exponent-min)
    (assert-exact-integer flo:subnormal-exponent-min)))

(define-test 'extremes
  (lambda ()
    (assert-flonum flo:smallest-positive-subnormal)
    (assert-flonum flo:smallest-positive-normal)
    (assert-flonum flo:largest-positive-normal)
    (assert-eqv flo:smallest-positive-subnormal
                (flo:scalbn 1. flo:subnormal-exponent-min))
    (assert-eqv flo:smallest-positive-normal
                (flo:scalbn 1. flo:normal-exponent-min))
    (assert-eqv flo:largest-positive-normal
                (flo:scalbn (- flo:radix. flo:ulp-of-one)
                            flo:normal-exponent-max))
    (assert-subnormal flo:smallest-positive-subnormal)
    (assert-zero+ (flo:nextafter flo:smallest-positive-subnormal (flo:-inf.0)))
    (assert-normal flo:smallest-positive-normal)
    (assert-subnormal
     (flo:nextafter flo:smallest-positive-normal (flo:-inf.0)))
    (assert-normal flo:largest-positive-normal)
    (assert-inf+ (flo:nextafter flo:largest-positive-normal (flo:+inf.0)))))

(define-test 'least-subnormal-exponents
  (lambda ()
    (assert-flonum flo:least-subnormal-exponent-base-2)
    (assert-flonum flo:least-subnormal-exponent-base-e)
    (assert-flonum flo:least-subnormal-exponent-base-10)
    (assert-subnormal (expt 2. flo:least-subnormal-exponent-base-2))
    (assert-subnormal (exp flo:least-subnormal-exponent-base-e))
    (assert-subnormal (expt 10. flo:least-subnormal-exponent-base-10))
    (assert-zero+
     (expt 2.
           (flo:nextafter flo:least-subnormal-exponent-base-2 (flo:-inf.0))))
    (assert-zero+
     (exp (flo:nextafter flo:least-subnormal-exponent-base-e (flo:-inf.0))))
    (assert-zero+
     (expt 10.
           (flo:nextafter flo:least-subnormal-exponent-base-10
                          (flo:-inf.0))))))

(define-test 'least-normal-exponents
  (lambda ()
    (assert-flonum flo:least-normal-exponent-base-2)
    (assert-flonum flo:least-normal-exponent-base-e)
    (assert-flonum flo:least-normal-exponent-base-10)
    (assert-normal (expt 2. flo:least-normal-exponent-base-2))
    (assert-normal (exp flo:least-normal-exponent-base-e))
    (assert-normal (expt 10. flo:least-normal-exponent-base-10))
    (assert-subnormal
     (expt 2.
           (flo:nextafter flo:least-normal-exponent-base-2 (flo:-inf.0))))
    (assert-subnormal
     (exp (flo:nextafter flo:least-normal-exponent-base-e (flo:-inf.0))))
    (assert-subnormal
     (expt 10.
           (flo:nextafter flo:least-normal-exponent-base-10 (flo:-inf.0))))))

(define-test 'greatest-normal-exponents
  (lambda ()
    (assert-flonum flo:greatest-normal-exponent-base-2)
    (assert-flonum flo:greatest-normal-exponent-base-e)
    (assert-flonum flo:greatest-normal-exponent-base-10)
    (assert-normal (expt 2. flo:greatest-normal-exponent-base-2))
    (assert-normal (exp flo:greatest-normal-exponent-base-e))
    (assert-normal (expt 10. flo:greatest-normal-exponent-base-10))
    (assert-inf+
     (expt 2.
           (flo:nextafter flo:greatest-normal-exponent-base-2 (flo:+inf.0))))
    (assert-inf+
     (exp (flo:nextafter flo:greatest-normal-exponent-base-e (flo:+inf.0))))
    (assert-inf+
     (expt 10.
           (flo:nextafter flo:greatest-normal-exponent-base-2 (flo:+inf.0))))))

(define-enumerated-test 'flo:ulp
  (list
   (list (flo:-inf.0) (flo:+inf.0))
   (list (+ -3. (* 2 flo:ulp-of-one)) (* 2 flo:ulp-of-one))
   (list -3. (* 2 flo:ulp-of-one))
   (list -2. (* 2 flo:ulp-of-one))
   (list -1. flo:ulp-of-one)
   (list -0. "4.9406564584124654e-324")
   (list 0. "4.9406564584124654e-324")
   (list 1. flo:ulp-of-one)
   (list 2. (* 2 flo:ulp-of-one))
   (list 3. (* 2 flo:ulp-of-one))
   (list (- 3. (* 2 flo:ulp-of-one)) (* 2 flo:ulp-of-one))
   (list (flo:+inf.0) (flo:+inf.0))
   (list +nan.123 +nan.123))
  (lambda (x u #!optional xfail)
    (flo:with-trapped-exceptions 0
      (lambda ()
        (let ((u
               (if (string? u)
                   (string->number u)
                   u)))
          (with-expected-failure xfail
            (lambda ()
              (assert-eqv-nan (flo:ulp x) u))))))))

(define-enumerated-test 'copysign
  `((0. 0. 0.)
    (0. -0. -0.)
    (0. 1. 0.)
    (0. -1. -0.)
    (0. +inf.0 0.)
    (0. -inf.0 -0.)
    (0. ,(flo:make-nan #t #t 0) -0.)
    (0. ,(flo:make-nan #f #t 0) 0.)
    (0. ,(flo:make-nan #t #f 1) -0.)
    (0. ,(flo:make-nan #f #f 1) 0.)
    (-0. 0. 0.)
    (-0. -0. -0.)
    (-0. 1. 0.)
    (-0. -1. -0.)
    (-0. +inf.0 0.)
    (-0. -inf.0 -0.)
    (-0. ,(flo:make-nan #t #t 0) -0.)
    (-0. ,(flo:make-nan #f #t 0) 0.)
    (-0. ,(flo:make-nan #t #f 1) -0.)
    (-0. ,(flo:make-nan #f #f 1) 0.)
    (1. 0. 1.)
    (1. -0. -1.)
    (1. 1. 1.)
    (1. -1. -1.)
    (1. +inf.0 1.)
    (1. -inf.0 -1.)
    (1. ,(flo:make-nan #t #t 0) -1.)
    (1. ,(flo:make-nan #f #t 0) 1.)
    (1. ,(flo:make-nan #t #f 1) -1.)
    (1. ,(flo:make-nan #f #f 1) 1.)
    (-1. 0. 1.)
    (-1. -0. -1.)
    (-1. 1. 1.)
    (-1. -1. -1.)
    (-1. +inf.0 1.)
    (-1. -inf.0 -1.)
    (-1. ,(flo:make-nan #t #t 0) -1.)
    (-1. ,(flo:make-nan #f #t 0) 1.)
    (-1. ,(flo:make-nan #t #f 1) -1.)
    (-1. ,(flo:make-nan #f #f 1) 1.)
    (+inf.0 0. +inf.0)
    (+inf.0 -0. -inf.0)
    (+inf.0 1. +inf.0)
    (+inf.0 -1. -inf.0)
    (+inf.0 +inf.0 +inf.0)
    (+inf.0 -inf.0 -inf.0)
    (+inf.0 ,(flo:make-nan #t #t 0) -inf.0)
    (+inf.0 ,(flo:make-nan #f #t 0) +inf.0)
    (+inf.0 ,(flo:make-nan #t #f 1) -inf.0)
    (+inf.0 ,(flo:make-nan #f #f 1) +inf.0)
    (-inf.0 0. +inf.0)
    (-inf.0 -0. -inf.0)
    (-inf.0 1. +inf.0)
    (-inf.0 -1. -inf.0)
    (-inf.0 +inf.0 +inf.0)
    (-inf.0 -inf.0 -inf.0)
    (-inf.0 ,(flo:make-nan #t #t 0) -inf.0)
    (-inf.0 ,(flo:make-nan #f #t 0) +inf.0)
    (-inf.0 ,(flo:make-nan #t #f 1) -inf.0)
    (-inf.0 ,(flo:make-nan #f #f 1) +inf.0)
    (,(flo:make-nan #f #t 0) 0. ,(flo:make-nan #f #t 0))
    (,(flo:make-nan #t #t 0) 0. ,(flo:make-nan #f #t 0))
    (,(flo:make-nan #f #t 0) -0. ,(flo:make-nan #t #t 0))
    (,(flo:make-nan #t #t 0) -0. ,(flo:make-nan #t #t 0))
    (,(flo:make-nan #f #f 1) 0. ,(flo:make-nan #f #f 1))
    (,(flo:make-nan #t #f 1) 0. ,(flo:make-nan #f #f 1))
    (,(flo:make-nan #f #f 1) -0. ,(flo:make-nan #t #f 1))
    (,(flo:make-nan #t #f 1) -0. ,(flo:make-nan #t #f 1)))
  (lambda (x y z)
    (assert-eqv-nan (yes-traps (lambda () (flo:copysign x y))) z)
    (assert-no-except/yes-traps (lambda () (flo:copysign x y)))
    (assert-eqv-nan
     (yes-traps (lambda () (flo:copysign (flo:negate x) y)))
     z)
    (assert-no-except/yes-traps (lambda () (flo:copysign (flo:negate x) y)))
    (assert-eqv-nan
     (yes-traps (lambda () (flo:copysign x (flo:negate y))))
     (flo:negate z))
    (assert-no-except/yes-traps (lambda () (flo:copysign x (flo:negate y))))
    (assert-eqv-nan
     (yes-traps (lambda () (flo:copysign (flo:negate x) (flo:negate y))))
     (flo:negate z))
    (assert-no-except/yes-traps
     (lambda ()
       (flo:copysign (flo:negate x) (flo:negate y))))))

(define-enumerated-test 'copysign-var/neg
  `((-inf.0 -inf.0)
    (-1. -1.)
    (,subnormal- ,subnormal-)
    (-0. -0.)
    (0. -0.)
    (,subnormal+ ,subnormal-)
    (1. -1.)
    (+inf.0 -inf.0)
    (,(flo:make-nan #t #t 1234) ,(flo:make-nan #t #t 1234))
    (,(flo:make-nan #f #t 1234) ,(flo:make-nan #t #t 1234))
    (,(flo:make-nan #t #f 1234) ,(flo:make-nan #t #f 1234))
    (,(flo:make-nan #f #f 1234) ,(flo:make-nan #t #f 1234)))
  (lambda (x z)
    (assert-eqv-nan (yes-traps (lambda () (flo:copysign x -1.23))) z)
    (assert-no-except/yes-traps (lambda () (flo:copysign x -1.23)))))

(define-enumerated-test 'copysign-var/pos
  `((-inf.0 +inf.0)
    (-1. +1.)
    (,subnormal- ,subnormal+)
    (-0. +0.)
    (0. +0.)
    (,subnormal+ ,subnormal+)
    (1. +1.)
    (+inf.0 +inf.0)
    (,(flo:make-nan #t #t 1234) ,(flo:make-nan #f #t 1234))
    (,(flo:make-nan #f #t 1234) ,(flo:make-nan #f #t 1234))
    (,(flo:make-nan #t #f 1234) ,(flo:make-nan #f #f 1234))
    (,(flo:make-nan #f #f 1234) ,(flo:make-nan #f #f 1234)))
  (lambda (x z)
    (assert-eqv-nan (yes-traps (lambda () (flo:copysign x +1.23))) z)
    (assert-no-except/yes-traps (lambda () (flo:copysign x +1.23)))))

(define-enumerated-test 'copysign-1.23/var
  `((-inf.0 -1.23)
    (-1. -1.23)
    (,subnormal- -1.23)
    (-0. -1.23)
    (0. 1.23)
    (,subnormal+ 1.23)
    (1. 1.23)
    (+inf.0 1.23)
    (,(flo:make-nan #t #t 1234) -1.23)
    (,(flo:make-nan #f #t 1234) 1.23)
    (,(flo:make-nan #t #f 1234) -1.23)
    (,(flo:make-nan #f #f 1234) 1.23))
  (lambda (x z)
    (assert-eqv-nan (yes-traps (lambda () (flo:copysign -1.23 x))) z)
    (assert-eqv-nan (yes-traps (lambda () (flo:copysign +1.23 x))) z)
    (assert-no-except/yes-traps (lambda () (flo:copysign -1.23 x)))
    (assert-no-except/yes-traps (lambda () (flo:copysign +1.23 x)))))

(define-enumerated-test 'copysign-0/var
  `((-inf.0 -0.)
    (-1. -0.)
    (,subnormal- -0.)
    (-0. -0.)
    (0. +0.)
    (1. +0.)
    (,subnormal+ +0.)
    (+inf.0 +0.)
    (,(flo:make-nan #t #t 1234) -0.)
    (,(flo:make-nan #f #t 1234) +0.)
    (,(flo:make-nan #t #f 1234) -0.)
    (,(flo:make-nan #f #f 1234) +0.))
  (lambda (x z)
    (assert-eqv-nan (yes-traps (lambda () (flo:copysign -0. x))) z)
    (assert-eqv-nan (yes-traps (lambda () (flo:copysign +0. x))) z)
    (assert-no-except/yes-traps (lambda () (flo:copysign -0. x)))
    (assert-no-except/yes-traps (lambda () (flo:copysign +1. x)))))

(define-enumerated-test 'copysign-inf/var
  `((-inf.0 -inf.0)
    (-1. -inf.0)
    (,subnormal- -inf.0)
    (-0. -inf.0)
    (0. +inf.0)
    (,subnormal+ +inf.0)
    (1. +inf.0)
    (+inf.0 +inf.0)
    (,(flo:make-nan #t #t 1234) -inf.0)
    (,(flo:make-nan #f #t 1234) +inf.0)
    (,(flo:make-nan #t #f 1234) -inf.0)
    (,(flo:make-nan #f #f 1234) +inf.0))
  (lambda (x z)
    (assert-eqv-nan (yes-traps (lambda () (flo:copysign -inf.0 x))) z)
    (assert-eqv-nan (yes-traps (lambda () (flo:copysign +inf.0 x))) z)
    (assert-no-except/yes-traps (lambda () (flo:copysign -0. x)))
    (assert-no-except/yes-traps (lambda () (flo:copysign +1. x)))))

(define-enumerated-test 'copysign-qnan/var
  `((-inf.0 ,(flo:make-nan #t #t 54321))
    (-1. ,(flo:make-nan #t #t 54321))
    (,subnormal- ,(flo:make-nan #t #t 54321))
    (-0. ,(flo:make-nan #t #t 54321))
    (0. ,(flo:make-nan #f #t 54321))
    (,subnormal+ ,(flo:make-nan #f #t 54321))
    (1. ,(flo:make-nan #f #t 54321))
    (+inf.0 ,(flo:make-nan #f #t 54321))
    (,(flo:make-nan #t #t 1234) ,(flo:make-nan #t #t 54321))
    (,(flo:make-nan #f #t 1234) ,(flo:make-nan #f #t 54321))
    (,(flo:make-nan #t #f 1234) ,(flo:make-nan #t #t 54321))
    (,(flo:make-nan #f #f 1234) ,(flo:make-nan #f #t 54321)))
  (lambda (x z)
    (let ((nan+ (flo:make-nan #f #t 54321))
          (nan- (flo:make-nan #t #t 54321)))
      (assert-eqv-nan (yes-traps (lambda () (flo:copysign nan+ x))) z)
      (assert-eqv-nan (yes-traps (lambda () (flo:copysign nan- x))) z))))

(define-enumerated-test 'copysign-snan/var
  `((-inf.0 ,(flo:make-nan #t #f 54321))
    (-1. ,(flo:make-nan #t #f 54321))
    (,subnormal- ,(flo:make-nan #t #f 54321))
    (-0. ,(flo:make-nan #t #f 54321))
    (0. ,(flo:make-nan #f #f 54321))
    (,subnormal+ ,(flo:make-nan #f #f 54321))
    (1. ,(flo:make-nan #f #f 54321))
    (+inf.0 ,(flo:make-nan #f #f 54321))
    (,(flo:make-nan #t #t 1234) ,(flo:make-nan #t #f 54321))
    (,(flo:make-nan #f #t 1234) ,(flo:make-nan #f #f 54321))
    (,(flo:make-nan #t #f 1234) ,(flo:make-nan #t #f 54321))
    (,(flo:make-nan #f #f 1234) ,(flo:make-nan #f #f 54321)))
  (lambda (x z)
    (let ((nan+ (flo:make-nan #f #f 54321))
          (nan- (flo:make-nan #t #f 54321)))
      (assert-eqv-nan (yes-traps (lambda () (flo:copysign nan+ x))) z)
      (assert-eqv-nan (yes-traps (lambda () (flo:copysign nan- x))) z))))

(define-enumerated-test 'nextafter
  `((0. 1. ,subnormal+)
    (0. +inf.0 ,subnormal+)
    (0. -1. ,subnormal-)
    (0. -inf.0 ,subnormal-)
    (,subnormal+ -1. 0.)
    (,subnormal+ -inf.0 0.)
    (,subnormal- +1. -0.)
    (,subnormal- +inf.0 -0.)
    (,flo:largest-positive-normal +inf.0 +inf.0)
    (+inf.0 0. ,flo:largest-positive-normal)
    (,(- flo:largest-positive-normal) -inf.0 -inf.0)
    (-inf.0 0. ,(- flo:largest-positive-normal)))
  (lambda (x y z)
    (assert-eqv (no-traps (lambda () (flo:nextafter x y))) z)))

(define-enumerated-test 'flo:classify
  `((0. zero)
    (-0. zero)
    (,(flo:nextafter 0. 1.) subnormal)
    (,subnormal+ subnormal)
    (,flo:smallest-positive-normal normal)
    (1. normal)
    (+inf.0 infinite)
    (-inf.0 infinite)
    (+nan.0 nan)
    (-nan.0 nan)
    (,(flo:qnan) nan)
    (,(flo:snan) nan))
  (lambda (x c)
    (assert-eq (flo:classify x) c)))

(define-enumerated-test 'zero?
  `((-inf.0 #f)
    (-1. #f)
    (,subnormal- #f)
    (-0. #t)
    (+0. #t)
    (,subnormal+ #f)
    (+1. #f)
    (+inf.0 #f)
    (+nan.0 #f)
    (,(flo:qnan) #f)
    (,(flo:snan) #f))
  (lambda (x v)
    (assert-eqv (yes-traps (lambda () (flo:safe-zero? x))) v)))

(define-enumerated-test 'subnormal?
  `((-inf.0 #f)
    (-1. #f)
    (,subnormal- #t)
    (-0. #f)
    (+0. #f)
    (,subnormal+ #t)
    (+1. #f)
    (+inf.0 #f)
    (+nan.0 #f)
    (,(flo:qnan) #f)
    (,(flo:snan) #f))
  (lambda (x v)
    (assert-eqv (yes-traps (lambda () (flo:subnormal? x))) v)))

(define-enumerated-test 'normal?
  `((-inf.0 #f)
    (-1. #t)
    (,subnormal- #f)
    (-0. #f)
    (+0. #f)
    (,subnormal+ #f)
    (+1. #t)
    (+inf.0 #f)
    (+nan.0 #f)
    (,(flo:qnan) #f)
    (,(flo:snan) #f))
  (lambda (x v)
    (assert-eqv (yes-traps (lambda () (flo:normal? x))) v)))

(define-enumerated-test 'finite?
  `((-inf.0 #f)
    (-1. #t)
    (,subnormal- #t)
    (-0. #t)
    (+0. #t)
    (,subnormal+ #t)
    (+1. #t)
    (+inf.0 #f)
    (+nan.0 #f)
    (,(flo:qnan) #f)
    (,(flo:snan) #f))
  (lambda (x v)
    (assert-eqv (yes-traps (lambda () (flo:finite? x))) v)))

(define-enumerated-test 'infinite?
  `((-inf.0 #t)
    (-1. #f)
    (,subnormal- #f)
    (-0. #f)
    (+0. #f)
    (,subnormal+ #f)
    (+1. #f)
    (+inf.0 #t)
    (+nan.0 #f)
    (,(flo:qnan) #f)
    (,(flo:snan) #f))
  (lambda (x v)
    (assert-eqv (yes-traps (lambda () (flo:infinite? x))) v)))

(define-enumerated-test 'nan?
  `((-inf.0 #f)
    (-1. #f)
    (,subnormal- #f)
    (-0. #f)
    (+0. #f)
    (,subnormal+ #f)
    (+1. #f)
    (+inf.0 #f)
    (+nan.0 #t)
    (,(flo:qnan) #t)
    (,(flo:snan) #t))
  (lambda (x v)
    (assert-eqv (yes-traps (lambda () (flo:nan? x))) v)))

(define-enumerated-test 'sign-negative?
  `((-inf.0 #t)
    (-1. #t)
    (,subnormal- #t)
    (-0. #t)
    (+0. #f)
    (,subnormal+ #f)
    (+1. #f)
    (+inf.0 #f)
    ;; (+nan.0 ...)  ; indeterminate
    (,(flo:make-nan #f #t 0) #f)
    (,(flo:make-nan #t #t 0) #t)
    (,(flo:make-nan #f #f 1) #f)
    (,(flo:make-nan #t #f 1) #t))
  (lambda (x n?)
    (assert-eqv (yes-traps (lambda () (flo:sign-negative? x))) n?)
    (assert-eqv (yes-traps (lambda () (flo:sign-negative? (flo:abs x)))) #f)
    (assert-eqv (yes-traps (lambda () (flo:sign-negative? (flo:negate x))))
                (not n?))
    (assert-no-except/yes-traps (lambda () (flo:sign-negative? x)))
    (assert-no-except/yes-traps (lambda () (flo:sign-negative? (flo:abs x))))
    (assert-no-except/yes-traps
     (lambda ()
       (flo:sign-negative? (flo:negate x))))))

(define-syntax define-comparison-test
  (syntax-rules ()
    ((define-comparison-test name safe-compare unsafe-compare cases)
     (define-test name
       (map (lambda (x)
              (map (lambda (y)
                     (lambda ()
                       (assert-eqv
                        (yes-traps (lambda () (safe-compare x y)))
                        (if (or (flo:nan? x) (flo:nan? y))
                            #f
                            (unsafe-compare x y)))
                       (assert-eqv
                        (yes-traps (lambda () (not (safe-compare x y))))
                        (if (or (flo:nan? x) (flo:nan? y))
                            #t
                            (not (unsafe-compare x y))))
                       (if (safe-compare x y)
                           (begin
                             (assert-true (not (flo:nan? x)))
                             (assert-true (not (flo:nan? y)))
                             (assert-true (unsafe-compare x y))))
                       (if (not (safe-compare x y))
                           (begin
                             (assert-true
                              (or (flo:nan? x)
                                  (flo:nan? y)
                                  (not (unsafe-compare x y))))))
                       (if (not (or (flo:nan? x) (flo:nan? y)))
                           (begin
                             (if (unsafe-compare x y)
                                 (assert-true (safe-compare x y)))
                             (if (not (unsafe-compare x y))
                                 (assert-false (safe-compare x y)))))))
                   cases))
            cases)))))

(define-syntax define-snan-comparison-test
  (syntax-rules ()
    ((define-snan-comparison-test name safe-compare unsafe-compare cases)
     (define-test name
       (map (lambda (x)
              (lambda ()
                (with-test-properties
                    (lambda ()
                      (let ((snan (identity-procedure (flo:snan 1234)))
                            (mask
                             (fix:andc (flo:supported-exceptions)
                                       ;; Not reliable.
                                       (flo:exception:subnormal-operand))))
                        (assert-only-except/no-traps
                         (flo:exception:invalid-operation)
                         (lambda () (safe-compare x snan))
                         mask)
                        (assert-only-except/no-traps
                         (flo:exception:invalid-operation)
                         (lambda () (safe-compare snan x))
                         mask)
                        (assert-only-except/no-traps
                         (flo:exception:invalid-operation)
                         (lambda () (safe-compare snan snan)))
                        (assert-false
                         (no-traps (lambda () (safe-compare x snan))))
                        (assert-false
                         (no-traps (lambda () (safe-compare snan x))))
                        (assert-false
                         (no-traps (lambda () (safe-compare snan snan))))
                        (assert-only-except/no-traps
                         (flo:exception:invalid-operation)
                         (lambda () (unsafe-compare x snan))
                         mask)
                        (assert-only-except/no-traps
                         (flo:exception:invalid-operation)
                         (lambda () (unsafe-compare snan x))
                         mask)
                        (assert-only-except/no-traps
                         (flo:exception:invalid-operation)
                         (lambda () (unsafe-compare snan snan)))
                        (assert-false
                         (no-traps (lambda () (unsafe-compare x snan))))
                        (assert-false
                         (no-traps (lambda () (unsafe-compare snan x))))
                        (assert-false
                         (no-traps (lambda () (unsafe-compare snan snan))))))
                  'SEED x)))
            cases)))))

(let* ((subnormal+ flo:smallest-positive-subnormal)
       (subnormal- (no-traps (lambda () (- subnormal+))))
       (cases
        `(-inf.0 -1. ,subnormal- -0. +0. ,subnormal+ +1. +inf.0 +nan.0)))
  (define-comparison-test '< flo:safe< flo:< cases)
  (define-comparison-test '> flo:safe> flo:> cases)
  (define-comparison-test '>= flo:safe>= flo:>= cases)
  (define-comparison-test '<= flo:safe<= flo:<= cases)
  (define-comparison-test '<> flo:safe<> flo:<> cases)
  (define-comparison-test '= flo:safe= flo:= cases)
  (define-snan-comparison-test '</snan flo:safe< flo:< cases)
  (define-snan-comparison-test '>/snan flo:safe> flo:> cases)
  (define-snan-comparison-test '>=/snan flo:safe>= flo:>= cases)
  (define-snan-comparison-test '<=/snan flo:safe<= flo:<= cases)
  (define-snan-comparison-test '<>/snan flo:safe<> flo:<> cases)
  (define-snan-comparison-test '=/snan flo:safe= flo:= cases)
  (define-test 'unordered?
    (map (lambda (x)
           (map (lambda (y)
                  (lambda ()
                    (assert-eqv (yes-traps (lambda () (flo:unordered? x y)))
                                (or (flo:nan? x) (flo:nan? y)))
                    (assert-eqv (yes-traps (lambda ()
                                             (not (flo:unordered? x y))))
                                (not (or (flo:nan? x) (flo:nan? y))))
                    (if (flo:unordered? x y)
                        (assert-true (or (flo:nan? x) (flo:nan? y))))
                    (if (not (flo:unordered? x y))
                        (begin
                          (assert-false (flo:nan? x))
                          (assert-false (flo:nan? y))))))
                cases))
         cases))
  (define-test 'tetrachotomy
    (map (lambda (x)
           (map (lambda (y)
                  (lambda ()
                    (define (n b) (if b 1 0))
                    (assert-eqv
                     (yes-traps
                      (lambda ()
                        (+ (n (flo:safe< x y))
                           (n (flo:safe> x y))
                           (n (and (flo:safe<= x y) (flo:safe>= x y)))
                           (n (flo:unordered? x y)))))
                     1)))
                cases))
         cases)))

(define-syntax define-*constcomp-test
  (syntax-rules ()
    ((define-*constcomp-test name safe-compare unsafe-compare x0
       x y a b u v c d cases)
     (define-test name
       (map (lambda (arguments)
              (apply (lambda (y u v #!optional xfail)
                       d
                       (let ((x x0))
                         (declare (integrate x))
                         (lambda ()
                           (with-expected-failure xfail
                             (lambda ()
                               (assert-eqv
                                (yes-traps (lambda () (safe-compare a b)))
                                c)
                               (assert-eqv
                                (no-traps (lambda () (unsafe-compare a b)))
                                c)
                               (if (yes-traps (lambda () (safe-compare a b)))
                                   (begin
                                     (assert-true (not (flo:nan? a)))
                                     (assert-true (not (flo:nan? b)))
                                     (assert-true (unsafe-compare a b))))
                               (if (yes-traps
                                    (lambda () (not (safe-compare a b))))
                                   (assert-true
                                    (or (flo:nan? a)
                                        (flo:nan? b)
                                        (not (unsafe-compare a b)))))
                               (if (not (or (flo:nan? a) (flo:nan? b)))
                                   (begin
                                     (if (unsafe-compare a b)
                                         (assert-true (safe-compare a b)))
                                     (if (not (unsafe-compare a b))
                                         (assert-false
                                          (safe-compare a b))))))))))
                     arguments))
            cases)))))

(define-syntax define-lconstcomp-test
  (syntax-rules ()
    ((define-lconstcomp-test name safe-compare unsafe-compare x0 cases)
     (define-*constcomp-test name safe-compare unsafe-compare x0
       x y x y u v u v
       cases))))

(define-syntax define-rconstcomp-test
  (syntax-rules ()
    ((define-lconstcomp-test name safe-compare unsafe-compare x0 cases)
     (define-*constcomp-test name safe-compare unsafe-compare x0
       x y y x u v v u
       cases))))

(define-syntax define-constcomp-test
  (syntax-rules ()
    ((define-constcomp-test name safe unsafe x0 cases)
     (begin
       (define-lconstcomp-test (symbol name '/lconst) safe unsafe x0 cases)
       (define-rconstcomp-test (symbol name '/rconst) safe unsafe x0 cases)))))

(define-constcomp-test '< flo:safe< flo:< 0.
  `((-inf.0 #f #t)
    (-1. #f #t)
    (,subnormal- #f #t)
    (-0. #f #f)
    (+0. #f #f)
    (,subnormal+ #t #f)
    (+1. #t #f)
    (+inf.0 #t #f)
    (+nan.0 #f #f)))

(define-constcomp-test '> flo:safe> flo:> 0.
  `((-inf.0 #t #f)
    (-1. #t #f)
    (,subnormal- #t #f)
    (-0. #f #f)
    (+0. #f #f)
    (,subnormal+ #f #t)
    (+1. #f #t)
    (+inf.0 #f #t)
    (+nan.0 #f #f)))

(define-constcomp-test '<= flo:safe<= flo:<= 0.
  `((-inf.0 #f #t)
    (-1. #f #t)
    (,subnormal- #f #t)
    (-0. #t #t)
    (+0. #t #t)
    (,subnormal+ #t #f)
    (+1. #t #f)
    (+inf.0 #t #f)
    (+nan.0 #f #f)))

(define-constcomp-test '>= flo:safe>= flo:>= 0.
  `((-inf.0 #t #f)
    (-1. #t #f)
    (,subnormal- #t #f)
    (-0. #t #t)
    (+0. #t #t)
    (,subnormal+ #f #t)
    (+1. #f #t)
    (+inf.0 #f #t)
    (+nan.0 #f #f)))

(define-constcomp-test '= flo:safe= flo:= 0.
  `((-inf.0 #f #f)
    (-1. #f #f)
    (,subnormal- #f #f)
    (-0. #t #t)
    (+0. #t #t)
    (,subnormal+ #f #f)
    (+1. #f #f)
    (+inf.0 #f #f)
    (+nan.0 #f #f)))

(define-constcomp-test '<> flo:safe<> flo:<> 0.
  `((-inf.0 #t #t)
    (-1. #t #t)
    (,subnormal- #t #t)
    (-0. #f #f)
    (+0. #f #f)
    (,subnormal+ #t #t)
    (+1. #t #t)
    (+inf.0 #t #t)
    (+nan.0 #f #f)))

(define-constcomp-test '< flo:safe< flo:< 1.
  `((-inf.0 #f #t)
    (-1. #f #t)
    (,subnormal- #f #t)
    (-0. #f #t)
    (+0. #f #t)
    (,subnormal+ #f #t)
    (+1. #f #f)
    (+inf.0 #t #f)
    (+nan.0 #f #f)))

(define-constcomp-test '> flo:safe> flo:> 1.
  `((-inf.0 #t #f)
    (-1. #t #f)
    (,subnormal- #t #f)
    (-0. #t #f)
    (+0. #t #f)
    (,subnormal+ #t #f)
    (+1. #f #f)
    (+inf.0 #f #t)
    (+nan.0 #f #f)))

(define-constcomp-test '<= flo:safe<= flo:<= 1.
  `((-inf.0 #f #t)
    (-1. #f #t)
    (,subnormal- #f #t)
    (-0. #f #t)
    (+0. #f #t)
    (,subnormal+ #f #t)
    (+1. #t #t)
    (+inf.0 #t #f)
    (+nan.0 #f #f)))

(define-constcomp-test '>= flo:safe>= flo:>= 1.
  `((-inf.0 #t #f)
    (-1. #t #f)
    (,subnormal- #t #f)
    (-0. #t #f)
    (+0. #t #f)
    (,subnormal+ #t #f)
    (+1. #t #t)
    (+inf.0 #f #t)
    (+nan.0 #f #f)))

(define-constcomp-test '= flo:safe= flo:= 1.
  `((-inf.0 #f #f)
    (-1. #f #f)
    (,subnormal- #f #f)
    (-0. #f #f)
    (+0. #f #f)
    (,subnormal+ #f #f)
    (+1. #t #t)
    (+inf.0 #f #f)
    (+nan.0 #f #f)))

(define-constcomp-test '<> flo:safe<> flo:<> 1.
  `((-inf.0 #t #t)
    (-1. #t #t)
    (,subnormal- #t #t)
    (-0. #t #t)
    (+0. #t #t)
    (,subnormal+ #t #t)
    (+1. #f #f)
    (+inf.0 #t #t)
    (+nan.0 #f #f)))

(define-enumerated-test 'nan
  `(;;(#f #f 0)   ; infinity
    (#f #t 0)
    ;;(#t #f 0)   ; infinity
    (#t #t 0)
    (#f #f 1)
    (#f #t 1)
    (#t #f 1)
    (#t #t 1)
    (#f #f 12345)
    (#f #t 12345)
    (#t #f 12345)
    (#t #t 12345)
    (#f #f ,(- (expt 2 51) 1))
    (#f #t ,(- (expt 2 51) 1))
    (#t #f ,(- (expt 2 51) 1))
    (#f #t ,(- (expt 2 51) 1)))
  (lambda (negative? quiet? payload)
    (let ((nan (flo:make-nan negative? quiet? payload)))
      (assert-flonum nan)
      (assert-nan nan)
      (if quiet?
          (assert-qnan nan)
          (assert-snan nan))
      (assert-eqv (flo:sign-negative? nan) negative?)
      (assert-eqv (flo:nan-quiet? nan) quiet?)
      (assert-eqv (flo:nan-payload nan) payload))))

(let ((inputs '((-inf.0) (-1.) (-0.) (+0.) (+1.) (+inf.0)))
      (quiet-cases
       `((-inf.0 -inf.0 -inf.0 -inf.0 -inf.0 -inf.0)
         (-inf.0 -1. -inf.0 -1. -1. -inf.0)
         (-inf.0 -0. -inf.0 -0. -0. -inf.0)
         (-inf.0 +0. -inf.0 +0. +0. -inf.0)
         (-inf.0 +1. -inf.0 +1. +1. -inf.0)
         (-inf.0 +inf.0 -inf.0 +inf.0 -inf.0 +inf.0)
         (-inf.0 ,(flo:qnan) -inf.0 -inf.0 -inf.0 -inf.0)
         (-1. -inf.0 -inf.0 -1. -1. -inf.0)
         (-1. -1. -1. -1. -1. -1.)
         (-1. -0. -1. -0. -0. -1.)
         (-1. +0. -1. +0. +0. -1.)
         (-1. +1. -1. +1. -1. +1.)
         (-1. +inf.0 -1. +inf.0 -1. +inf.0)
         (-1. ,(flo:qnan) -1. -1. -1. -1.)
         (-0. -inf.0 -inf.0 -0. -0. -inf.0)
         (-0. -1. -1. -0. -0. -1.)
         (-0. -0. -0. -0. -0. -0.)
         (-0. +0. -0. +0. -0. +0.)      ;arbitrary
         (-0. +1. -0. +1. -0. +1.)
         (-0. +inf.0 -0. +inf.0 -0. +inf.0)
         (-0. ,(flo:qnan) -0. -0. -0. -0.)
         (+0. -inf.0 -inf.0 +0. +0. -inf.0)
         (+0. -1. -1. +0. +0. -1.)
         (+0. -0. +0. -0. +0. -0.)      ;arbitrary
         (+0. +0. +0. +0. +0. +0.)
         (+0. +1. +0. +1. +0. +1.)
         (+0. +inf.0 +0. +inf.0 +0. +inf.0)
         (+0. ,(flo:qnan) +0. +0. +0. +0.)
         (+1. -inf.0 -inf.0 +1. +1. -inf.0)
         (+1. -1. -1. +1. -1. +1.)
         (+1. -0. -0. +1. -0. +1.)
         (+1. +0. +0. +1. +0. +1.)
         (+1. +1. +1. +1. +1. +1.)
         (+1. +inf.0 +1. +inf.0 +1. +inf.0)
         (+1. ,(flo:qnan) +1. +1. +1. +1.)
         (+inf.0 -inf.0 -inf.0 +inf.0 -inf.0 +inf.0)
         (+inf.0 -1. -1. +inf.0 -1. +inf.0)
         (+inf.0 -0. -0. +inf.0 -0. +inf.0)
         (+inf.0 +0. +0. +inf.0 +0. +inf.0)
         (+inf.0 +1. +1. +inf.0 +1. +inf.0)
         (+inf.0 +inf.0 +inf.0 +inf.0 +inf.0 +inf.0)
         (+inf.0 ,(flo:qnan) +inf.0 +inf.0 +inf.0 +inf.0)
         (,(flo:qnan) -inf.0 -inf.0 -inf.0 -inf.0 -inf.0)
         (,(flo:qnan) -1. -1. -1. -1. -1.)
         (,(flo:qnan) -0. -0. -0. -0. -0.)
         (,(flo:qnan) +0. +0. +0. +0. +0.)
         (,(flo:qnan) +1. +1. +1. +1. +1.)
         (,(flo:qnan) +inf.0 +inf.0 +inf.0 +inf.0 +inf.0)
         (,(flo:qnan) ,(flo:qnan)
          ,(flo:qnan) ,(flo:qnan)
          ,(flo:qnan) ,(flo:qnan)))))
  (define-enumerated-test 'min quiet-cases
    (lambda (x y min max min-mag max-mag)
      max min-mag max-mag
      (assert-eqv-nan (yes-traps (lambda () (flo:min x y))) min)))
  (define-enumerated-test 'max quiet-cases
    (lambda (x y min max min-mag max-mag)
      min min-mag max-mag
      (assert-eqv-nan (yes-traps (lambda () (flo:max x y))) max)))
  (define-enumerated-test 'min-mag quiet-cases
    (lambda (x y min max min-mag max-mag)
      min max max-mag
      (assert-eqv-nan (yes-traps (lambda () (flo:min-mag x y))) min-mag)))
  (define-enumerated-test 'max-mag quiet-cases
    (lambda (x y min max min-mag max-mag)
      min max min-mag
      (assert-eqv-nan (yes-traps (lambda () (flo:max-mag x y))) max-mag)))
  (define-enumerated-test 'min-snan-left inputs
    (lambda (x)
      (assert-eqv (no-traps (lambda () (flo:min (flo:snan) x))) x)))
  (define-enumerated-test 'max-snan-left inputs
    (lambda (x)
      (assert-eqv (no-traps (lambda () (flo:max (flo:snan) x))) x)))
  (define-enumerated-test 'min-snan-right inputs
    (lambda (x)
      (assert-eqv (no-traps (lambda () (flo:min x (flo:snan)))) x)))
  (define-enumerated-test 'max-snan-right inputs
    (lambda (x)
      (assert-eqv (no-traps (lambda () (flo:max x (flo:snan)))) x)))
  (define-test 'min-snan-both
    (lambda ()
      (assert-nan (no-traps (lambda () (flo:min (flo:snan) (flo:snan)))))))
  (define-test 'max-snan-both
    (lambda ()
      (assert-nan (no-traps (lambda () (flo:max (flo:snan) (flo:snan)))))))
  (define-enumerated-test 'min-mag-snan-left inputs
    (lambda (x)
      (assert-eqv (no-traps (lambda () (flo:min-mag (flo:snan) x))) x)))
  (define-enumerated-test 'max-mag-snan-left inputs
    (lambda (x)
      (assert-eqv (no-traps (lambda () (flo:max-mag (flo:snan) x))) x)))
  (define-enumerated-test 'min-mag-snan-right inputs
    (lambda (x)
      (assert-eqv (no-traps (lambda () (flo:min-mag x (flo:snan)))) x)))
  (define-enumerated-test 'max-mag-snan-right inputs
    (lambda (x)
      (assert-eqv (no-traps (lambda () (flo:max-mag x (flo:snan)))) x)))
  (define-test 'min-mag-snan-both
    (lambda ()
      (assert-nan
       (no-traps (lambda () (flo:min-mag (flo:snan) (flo:snan)))))))
  (define-test 'max-mag-snan-both
    (lambda ()
      (assert-nan
       (no-traps (lambda () (flo:max-mag (flo:snan) (flo:snan))))))))

(define-enumerated-test 'abs
  `((-inf.0)
    (-1.)
    (-0.)
    (+0.)
    (+1.)
    (+inf.0)
    (,(flo:make-nan #t #t 0))
    (,(flo:make-nan #f #t 0))
    (,(flo:make-nan #t #t 1))
    (,(flo:make-nan #f #t 1))
    (,(flo:make-nan #t #t (- (expt 2 51) 1)))
    (,(flo:make-nan #f #t (- (expt 2 51) 1)))
    (,(flo:make-nan #t #f 1))
    (,(flo:make-nan #f #f 1))
    (,(flo:make-nan #t #f (- (expt 2 51) 1)))
    (,(flo:make-nan #f #f (- (expt 2 51) 1))))
  (lambda (x #!optional xfail)
    (with-expected-failure xfail
      (lambda ()
        (let ((y (yes-traps (lambda () (flo:abs x)))))
          (assert-false (flo:sign-negative? y))
          (assert-eqv (flo:classify y) (flo:classify x))
          (if (flo:nan? x)
              (begin
                (assert-nan y)
                (assert-eqv (flo:nan-quiet? x) (flo:nan-quiet? y))
                (assert-eqv (flo:nan-payload x) (flo:nan-payload y)))))))))

(define-enumerated-test 'negate
  `((-inf.0 +inf.0)
    (-1. +1.)
    (-0. +0.)
    (+0. -0.)
    (+1. -1.)
    (+inf.0 -inf.0)
    (,(flo:make-nan #t #t 0) ,(flo:make-nan #f #t 0))
    (,(flo:make-nan #f #t 0) ,(flo:make-nan #t #t 0))
    (,(flo:make-nan #t #t 1) ,(flo:make-nan #f #t 1))
    (,(flo:make-nan #f #t 1) ,(flo:make-nan #t #t 1))
    (,(flo:make-nan #t #t (- (expt 2 51) 1))
     ,(flo:make-nan #f #t (- (expt 2 51) 1)))
    (,(flo:make-nan #f #t (- (expt 2 51) 1))
     ,(flo:make-nan #t #t (- (expt 2 51) 1)))
    (,(flo:make-nan #t #f 1) ,(flo:make-nan #f #f 1))
    (,(flo:make-nan #f #f 1) ,(flo:make-nan #t #f 1))
    (,(flo:make-nan #t #f (- (expt 2 51) 1))
     ,(flo:make-nan #f #f (- (expt 2 51) 1)))
    (,(flo:make-nan #f #f (- (expt 2 51) 1))
     ,(flo:make-nan #t #f (- (expt 2 51) 1))))
  (lambda (x z #!optional xfail)
    (with-expected-failure xfail
      (lambda ()
        (let ((y (yes-traps (lambda () (flo:negate x)))))
          (assert-eqv-nan y z)
          (assert-eqv-nan (flo:abs x) (flo:abs y))
          (assert-eqv (flo:sign-negative? y)
                      (not (flo:sign-negative? x)))
          (assert-eqv (flo:classify y) (flo:classify x))
          (if (flo:nan? x)
              (begin
                (assert-nan y)
                (assert-eqv (flo:nan-quiet? x) (flo:nan-quiet? y))
                (assert-eqv (flo:nan-payload x) (flo:nan-payload y)))))))))

(let ((cases
       (vector (flo:make-nan #t #t 0)
               (flo:make-nan #t #t 1)
               (flo:make-nan #t #t 2)
               (flo:make-nan #t #t (- (expt 2 51) 1))
               (flo:make-nan #t #f 1)
               (flo:make-nan #t #f 2)
               (flo:make-nan #t #f (- (expt 2 51) 1))
               -inf.0
               -1.
               (- flo:smallest-positive-normal)
               subnormal-
               -0.
               +0.
               subnormal+
               flo:smallest-positive-normal
               +1.
               +inf.0
               (flo:make-nan #f #f 1)
               (flo:make-nan #f #f 2)
               (flo:make-nan #f #f (- (expt 2 51) 1))
               (flo:make-nan #f #t 0)
               (flo:make-nan #f #t 1)
               (flo:make-nan #f #t 2)
               (flo:make-nan #f #t (- (expt 2 51) 1)))))
  ((lambda (f)
     (for-each (lambda (i)
                 (for-each (lambda (j)
                             (let ((x (vector-ref cases i))
                                   (y (vector-ref cases j)))
                               (define-test (symbol 'total-order/ x '/ y)
                                 (lambda ()
                                   (f i j x y)))))
                           (iota (vector-length cases))))
               (iota (vector-length cases))))
   (lambda (i j x y)
     (yes-traps
      (lambda ()
        (if (< i j)
            (assert-true (flo:total< x y))
            (assert-false (flo:total< x y)))
        (assert-eqv (flo:total-order x y)
                    (cond ((< i j) -1) ((< j i) +1) (else 0)))
        (assert-eqv (flo:total-mag< x y) (flo:total< (flo:abs x) (flo:abs y)))
        (assert-eqv (flo:total-order-mag x y)
                    (flo:total-order (flo:abs x) (flo:abs y))))))))

(define-enumerated-test 'flo:logb/finite
  `((1. 0)
    (,flo:radix. 1)
    (,(+ 1 flo:radix.) 1)
    (,(expt flo:radix. 2) 2)
    (,(+ 1 (expt flo:radix. 2)) 2)
    (,flo:smallest-positive-subnormal ,flo:subnormal-exponent-min)
    (,flo:smallest-positive-normal ,flo:normal-exponent-min)
    (,flo:largest-positive-normal ,flo:normal-exponent-max))
  (lambda (x l)
    (assert-eqv (flo:logb x) l)
    (assert->= (flo:scalbn x (- (flo:logb x))) 1)
    (assert-< (flo:scalbn x (- (flo:logb x))) flo:radix)
    (let ((y (flo:scalbn x (- (flo:logb x)))))
      (assert-= (flo:scalbn y l) x))))

(define-enumerated-test 'flo:logb/invalid
  `((0.)
    (+inf.0)
    (,(flo:qnan))
    (,(flo:snan)))
  (lambda (x)
    (assert-eqv (no-traps (lambda () (flo:logb x))) #f)
    (assert-eqv (no-traps (lambda () (flo:logb (flo:negate x)))) #f)
    (assert-error (lambda () (yes-traps (lambda () (flo:logb x)))))
    (assert-error
     (lambda () (yes-traps (lambda () (flo:logb (flo:negate x))))))
    (assert-only-except/no-traps (flo:exception:invalid-operation)
                                 (lambda () (flo:logb x)))
    (assert-only-except/no-traps (flo:exception:invalid-operation)
                                 (lambda () (flo:logb (flo:negate x))))))

(define-enumerated-test 'flo:make-nan/error
  `((#f #f 0)
    (#t #f 0))
  (lambda (sign quiet? payload #!optional xfail)
    (with-expected-failure xfail
      (lambda ()
        (assert-error
         (lambda ()
           (flo:make-nan sign quiet? payload))
         (list condition-type:bad-range-argument))))))