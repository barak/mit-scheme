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

;;;; Test of flonum operations

(declare (usual-integrations))

(define (define-enumerated-test name cases procedure)
  (define-test name
    (map (lambda (arguments)
           (lambda ()
             (apply procedure arguments)))
         cases)))

(define (no-traps f)
  (if (flo:have-trap-enable/disable?)
      (flo:with-trapped-exceptions 0 f)
      (f)))

(define (yes-traps f)
  #;
  (if (flo:have-trap-enable/disable?)
      (flo:with-trapped-exceptions (flo:trappable-exceptions) f)
      (f))
  ;; XXX Temporary workaround for my setup.
  (no-traps f))

(define (flo:<> x y)                    ;XXX
  (if (or (flo:nan? x) (flo:nan? y))
      (begin
        (flo:raise-exceptions! (flo:exception:invalid-operation))
        #f)
      (not (flo:= x y))))

(define subnormal+ flo:smallest-positive-subnormal)
(define subnormal- (no-traps (lambda () (- subnormal+))))

(define-enumerated-test 'copysign
  '((0. 0. 0.)
    (0. -0. -0.)
    (0. 1. 0.)
    (0. -1. -0.)
    (0. +inf.0 0.)
    (0. -inf.0 -0.)
    (1. 0. 1.)
    (1. -0. -1.)
    (1. 1. 1.)
    (1. -1. -1.)
    (1. +inf.0 1.)
    (1. -inf.0 -1.)
    (+inf.0 0. +inf.0)
    (+inf.0 -0. -inf.0)
    (+inf.0 1. +inf.0)
    (+inf.0 -1. -inf.0)
    (+inf.0 +inf.0 +inf.0)
    (+inf.0 -inf.0 -inf.0)
    (-inf.0 0. +inf.0)
    (-inf.0 -0. -inf.0)
    (-inf.0 1. +inf.0)
    (-inf.0 -1. -inf.0)
    (-inf.0 +inf.0 +inf.0)
    (-inf.0 -inf.0 -inf.0))
  (lambda (x y z)
    (assert-eqv (yes-traps (lambda () (flo:copysign x y))) z)))

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

(define-enumerated-test 'zero?
  `((-inf.0 #f)
    (-1. #f)
    (,subnormal- #f)
    (-0. #t)
    (+0. #t)
    (,subnormal+ #f)
    (+1. #f)
    (+inf.0 #f)
    (+nan.0 #f))
  (lambda (x v)
    (assert-eqv (yes-traps (lambda () (flo:zero? x))) v)))

(define-enumerated-test 'subnormal?
  `((-inf.0 #f)
    (-1. #f)
    (,subnormal- #t)
    (-0. #f)
    (+0. #f)
    (,subnormal+ #t)
    (+1. #f)
    (+inf.0 #f)
    (+nan.0 #f))
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
    (+nan.0 #f))
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
    (+nan.0 #f))
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
    (+nan.0 #f))
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
    (+nan.0 #t))
  (lambda (x v)
    (assert-eqv (yes-traps (lambda () (flo:nan? x))) v)))

(define-enumerated-test 'safe-negative?   ;XXX Maybe call it signbit?
  `((-inf.0 #t)
    (-1. #t)
    (,subnormal- #t)
    (-0. #t)
    (+0. #f)
    (,subnormal+ #f)
    (+1. #f)
    (+inf.0 #f)
    ;; (+nan.0 ...)  ; indeterminate
    )
  (lambda (x n?)
    (assert-eqv (yes-traps (lambda () (flo:safe-negative? x))) n?)))

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
                                 (assert-false (unsafe-compare x y)))))))
                   cases))
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
