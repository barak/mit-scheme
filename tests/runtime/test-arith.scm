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

;;;; Test of arithmetic

(declare (usual-integrations))

(define (assert-nan object)
  (assert-true (flo:flonum? object))
  (assert-true (flo:nan? object)))

(define (define-enumerated-test prefix elements procedure)
  (let ((n (vector-length elements)))
    (do ((i 0 (+ i 1))) ((>= i n))
      (define-test (symbol prefix '/ (vector-ref elements i))
        (lambda ()
          (procedure (vector-ref elements i)))))))

(define (define-enumerated^2-test* prefix elements procedure)
  (let ((n (vector-length elements)))
    (do ((i 0 (+ i 1))) ((>= i n))
      (do ((j 0 (+ j 1))) ((>= j n))
        (define-test
            (symbol prefix
                    '/ (vector-ref elements i)
                    '/ (vector-ref elements j))
          (lambda ()
            (procedure i (vector-ref elements i)
                       j (vector-ref elements j))))))))

(define (define-enumerated^2-test prefix elements procedure)
  (define-enumerated^2-test* prefix elements
    (lambda (i vi j vj)
      i j                               ;ignore
      (procedure vi vj))))

(define-enumerated^2-test 'ZEROS-ARE-EQUAL (vector -0. 0 +0.) =)

(define-enumerated^2-test* 'ORDER-WITH-INFINITIES
  (vector (flo:-inf.0) -2. -1 -0.5 0 +0.5 +1 +2. (flo:+inf.0))
  (lambda (i vi j vj)
    (if (< i j)
	(assert-true (< vi vj))
	(assert-false (< vi vj)))))

(let ((elements (vector (flo:-inf.0) -2. -1 -0. 0 +0. +1 +2. (flo:+inf.0))))
  (define-enumerated-test '!NAN<X elements
    (lambda (v) (assert-false (< (flo:nan.0) v))))
  (define-enumerated-test '!X<NAN elements
    (lambda (v) (assert-false (< v (flo:nan.0))))))
(let ((elements (vector -2. -1 -0. 0 +0. +1 +2.)))

  (define-enumerated-test 'MIN-INF-/X elements
    (lambda (v) (assert-= (min (flo:-inf.0) v) (flo:-inf.0))))
  (define-enumerated-test 'MIN-INF+/X elements
    (lambda (v) (assert-= (min (flo:+inf.0) v) v)))
  (define-enumerated-test 'MIN-X/INF- elements
    (lambda (v) (assert-= (min v (flo:-inf.0)) (flo:-inf.0))))
  (define-enumerated-test 'MIN-X/INF+ elements
    (lambda (v) (assert-= (min v (flo:+inf.0)) v)))

  (define-enumerated-test 'MAX-INF-/X elements
    (lambda (v) (assert-= (max (flo:-inf.0) v) v)))
  (define-enumerated-test 'MAX-INF+/X elements
    (lambda (v) (assert-= (max (flo:+inf.0) v) (flo:+inf.0))))
  (define-enumerated-test 'MAX-X/INF- elements
    (lambda (v) (assert-= (max v (flo:-inf.0)) v)))
  (define-enumerated-test 'MAX-X/INF+ elements
    (lambda (v) (assert-= (max v (flo:+inf.0)) (flo:+inf.0)))))

(let ((elements (vector (flo:-inf.0) -2. -1 -0. 0 +0. +1 +2. (flo:+inf.0))))
  (define-enumerated-test 'MIN-NAN/X elements
    (lambda (v) (assert-= (min (flo:nan.0) v) v)))
  (define-enumerated-test 'MIN-X/NAN elements
    (lambda (v) (assert-= (min v (flo:nan.0)) v)))
  (define-enumerated-test 'MAX-NAN/X elements
    (lambda (v) (assert-= (max (flo:nan.0) v) v)))
  (define-enumerated-test 'MAX-X/NAN elements
    (lambda (v) (assert-= (max v (flo:nan.0)) v))))

(define-enumerated-test 'NAN*X
  (vector (flo:-inf.0) -2. -1 -0. 0 +0. +1 +2. (flo:+inf.0))
  (lambda (v) (assert-nan (* (flo:nan.0) v))))

(define-enumerated-test 'X*NAN
  (vector (flo:-inf.0) -2. -1 -0. 0 +0. +1 +2. (flo:+inf.0))
  (lambda (v) (assert-nan (* v (flo:nan.0)))))

(define-enumerated-test 'NAN/X
  (vector (flo:-inf.0) -2. -1 -0. 0 +0. +1 +2. (flo:+inf.0))
  (lambda (v) (assert-nan (/ (flo:nan.0) v))))

(define-enumerated-test 'X/NAN
  (vector (flo:-inf.0) -2. -1 -0. 0 +0. +1 +2. (flo:+inf.0))
  (lambda (v) (assert-nan (/ v (flo:nan.0)))))

(define-enumerated-test 'log1p-exact
  (vector
   (cons 0 0)
   (cons 1 (log 2))
   (cons (- (exp 1) 1) 1.)
   (cons (expt 2 -53) (expt 2. -53))
   (cons (- (expt 2 -53)) (- (expt 2. -53))))
  (lambda (v)
    (assert-eqv (cdr v) (log1p (car v)))))

(define-enumerated-test 'expm1-exact
  (vector
   (cons 0 0)
   (cons (log 2) 1.)
   (cons 1 (- (exp 1) 1))
   (cons (expt 2 -53) (expt 2. -53))
   (cons (- (expt 2 -53)) (- (expt 2. -53))))
  (lambda (v)
    (assert-eqv (cdr v) (expm1 (car v)))))

(define (relerr e a)
  (if (= e 0)
      (if (= a 0) 0 1)
      (abs (/ (- e a) a))))

(define-enumerated-test 'expm1-approx
  (vector
   (cons -0.7 -0.5034146962085905)
   (cons (- (log 2)) -0.5)
   (cons -0.6 -0.45118836390597356)
   (cons 0.6 .8221188003905089)
   (cons (log 2) 1.)
   (cons 0.7 1.0137527074704766))
  (lambda (v)
    (assert-<= (relerr (cdr v) (expm1 (car v))) 1e-15)))

(define-enumerated-test 'log1p-approx
  (vector
   (cons -0.3 -.35667494393873245)
   (cons (- (sqrt 1/2) 1) -0.34657359027997264)
   (cons -0.25 -.2876820724517809)
   (cons 0.25 .22314355131420976)
   (cons (- 1 (sqrt 1/2)) 0.25688251232181475)
   (cons 0.3 .26236426446749106))
  (lambda (v)
    (assert-<= (relerr (cdr v) (log1p (car v))) 1e-15)))

(define-enumerated-test 'log1mexp
  (vector
   (cons -1e-17 -39.1439465808987777)
   (cons -0.69 -0.696304297144056727)
   (cons (- (log 2)) (- (log 2)))
   (cons -0.70 -0.686341002808385170)
   (cons -708 -3.30755300363840783e-308))
  (lambda (v)
    (assert-<= (relerr (cdr v) (log1mexp (car v))) 1e-15)))

(define-enumerated-test 'log1pexp
  (vector
   (cons -1000 0)
   (cons -708 3.30755300363840783e-308)
   (cons -38 3.13913279204802960e-17)
   (cons -37 8.53304762574406580e-17)
   (cons -36 2.31952283024356914e-16)
   (cons 0 (log 2))
   (cons 17 17.0000000413993746)
   (cons 18 18.0000000152299791)
   (cons 19 19.0000000056027964)
   (cons 33 33.0000000000000071)
   (cons 34 34))
  (lambda (v)
    (assert-<= (relerr (cdr v) (log1pexp (car v))) 1e-15)))

(define-enumerated-test 'logit-logistic
  (vector
   (vector -36.7368005696771
           1.1102230246251565e-16
           (log 1.1102230246251565e-16))
   (vector -1.0000001 0.2689414017088022 (log .2689414017088022))
   (vector -0.9999999 0.26894144103118883 (log .26894144103118883))
   (vector -1 0.2689414213699951 (log 0.2689414213699951))
   (vector -4.000000000537333e-5 .49999 (log .49999))
   (vector -4.000000108916879e-9 .499999999 (log .499999999))
   (vector 0 1/2 (log 1/2))
   (vector 3.999999886872274e-9 .500000001 (log .500000001))
   (vector 8.000042667076279e-3 .502 (log .502))
   (vector +0.9999999 0.7310585589688111 (log 0.7310585589688111))
   (vector +1 0.7310585786300049 (log 0.7310585786300049))
   (vector +1.0000001 0.7310585982911977 (log 0.7310585982911977))
   ;; Would like to do +/-710 but we get inexact result traps.
   (vector +708 1 -3.307553003638408e-308)
   (vector -708 3.307553003638408e-308 -708)
   (vector +1000 1 0)
   (vector -1000 0 -1000))
  (lambda (v)
    (let ((x (vector-ref v 0))
          (p (vector-ref v 1))
          (t (vector-ref v 2)))
      (assert-<= (relerr p (logistic x)) 1e-15)
      (if (and (not (= p 0))
               (not (= p 1)))
          (assert-<= (relerr x (logit p)) 1e-15))
      (if (< p 1)
          (begin
            (assert-<= (relerr (- 1 p) (logistic (- x))) 1e-15)
            (if (<= 1/2 p)
                ;; In this case, 1 - p is evaluated exactly.
                (assert-<= (relerr (- x) (logit (- 1 p))) 1e-15)))
          (assert-<= (logistic (- x)) 1e-300))
      (assert-<= (relerr t (log-logistic x)) 1e-15)
      (if (<= x 709)
          (assert-<= (relerr x (logit-exp t)) 1e-15))
      (if (< p 1)
          (assert-<= (relerr (log1p (- p)) (log-logistic (- x))) 1e-15)))))

(define-enumerated-test 'logit-logistic-1/2
  (vector
   (vector 1e-300 4e-300)
   (vector 1e-16 4e-16)
   (vector .2310585786300049 1.)
   (vector .49999999999999994 37.42994775023705))
  (lambda (v)
    (let ((p (vector-ref v 0))
          (x (vector-ref v 1)))
      (assert-<= (relerr x (logit1/2+ p)) 1e-15)
      (assert-<= (relerr p (logistic-1/2 x)) 1e-15)
      (assert-= (- (logit1/2+ p)) (logit1/2+ (- p)))
      (assert-= (- (logistic-1/2 x)) (logistic-1/2 (- x))))))

(define-enumerated-test 'expt-exact
  (vector
   (vector 2. -1075 "0.")
   (vector 2. -1074 "4.9406564584124654e-324")
   (vector 2. -1024 "5.562684646268004e-309")
   (vector 2. -1023 "1.1125369292536007e-308")
   (vector 2. -1022 "2.2250738585072014e-308"))
  (lambda (v)
    (flo:with-trapped-exceptions 0
      (lambda ()
        (let ((x (vector-ref v 0))
              (y (vector-ref v 1))
              (x^y (string->number (vector-ref v 2))))
          (assert-eqv (expt x y) x^y)
          ;; For all the inputs, reciprocal is exact.
          (assert-eqv (expt (/ 1 x) (- y)) x^y)
          (assert-eqv (expt (* 2 x) (/ y 2)) x^y)
          (assert-eqv (expt (/ 1 (* 2 x)) (- (/ y 2))) x^y))))))