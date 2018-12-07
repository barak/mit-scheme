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

;;;; Tests of number parser

(declare (usual-integrations))

(define (define-eqv-test s v #!optional xfail)
  (define-eqv-test-1 s v xfail)
  (if (not (string=? s (string-upcase s)))
      (define-eqv-test-1 (string-upcase s) v xfail)))

(define (define-eqv-test-1 s v #!optional xfail)
  (define-test s
    (lambda ()
      (with-xfail xfail
        (lambda ()
          (assert-eqv-nan (string->number s) v))))))

(define (define-relerr-test s v bound #!optional xfail)
  (define-test s
    (lambda ()
      (with-xfail xfail
        (lambda ()
          (let ((u (string->number s)))
            (assert-number u)
            (assert-<= (magnitude (/ (- u v) v)) bound)))))))

(define (define-error-test s #!optional xfail)
  (define-test s
    (lambda ()
      (with-xfail xfail
        (lambda ()
          (assert-false (string->number s)))))))

(define (eqv-nan? x y)
  (if (and (flo:flonum? x) (flo:nan? x))
      (and (flo:flonum? y)
           (flo:nan? y)
           (eqv? (flo:sign-negative? x) (flo:sign-negative? y))
           (eqv? (flo:nan-quiet? x) (flo:nan-quiet? y))
           (eqv? (flo:nan-payload x) (flo:nan-payload y)))
      (and (not (and (flo:flonum? y) (flo:nan? y)))
           (eqv? x y))))

(define-comparator eqv-nan? 'eqv-nan?)

(define assert-eqv-nan
  (simple-binary-assertion eqv-nan? #f))

(define assert-number
  (predicate-assertion number? "number"))

(define (with-xfail xfail body)
  (if (default-object? xfail)
      (body)
      (xfail body)))

(define-eqv-test "#e1e9" (expt 10 9))
(define-eqv-test "#e1f9" (expt 10 9))
(define-eqv-test "#e1s9" (expt 10 9))
(define-eqv-test "#e1l9" (expt 10 9))
(define-eqv-test "#e1e-9" (expt 10 -9))
(define-eqv-test "#e1f-9" (expt 10 -9))
(define-eqv-test "#e1s-9" (expt 10 -9))
(define-eqv-test "#e1l-9" (expt 10 -9))
(define-eqv-test "#e1.1e1" 11)
(define-eqv-test "#b1000000000000000000000000000000000000000000000000000000000000000" (expt 2 63))
(define-eqv-test "#o1000000000000000000000000000000000000000000000000000000000000000" (expt 8 63))
(define-eqv-test "#x1000000000000000000000000000000000000000000000000000000000000000" (expt 16 63))
(define-eqv-test "0." (int:->flonum 0))
(define-eqv-test "-0." (flo:negate (int:->flonum 0)))
(define-eqv-test "#b100" 4)
(define-eqv-test "#o100" 64)
(define-eqv-test "#d100" 100)
(define-eqv-test "#x100" 256)
(define-eqv-test "#e#b100" 4)
(define-eqv-test "#e#o100" 64)
(define-eqv-test "#e#d100" 100)
(define-eqv-test "#e#x100" 256)
(define-eqv-test "#b#e100" 4)
(define-eqv-test "#o#e100" 64)
(define-eqv-test "#d#e100" 100)
(define-eqv-test "#x#e100" 256)
(define-eqv-test "#b-100" -4)
(define-eqv-test "#o-100" -64)
(define-eqv-test "#d-100" -100)
(define-eqv-test "#x-100" -256)
(define-eqv-test "#e#b-100" -4)
(define-eqv-test "#e#o-100" -64)
(define-eqv-test "#e#d-100" -100)
(define-eqv-test "#e#x-100" -256)
(define-eqv-test "#b#e-100" -4)
(define-eqv-test "#o#e-100" -64)
(define-eqv-test "#d#e-100" -100)
(define-eqv-test "#x#e-100" -256)

(define-eqv-test "#e1e1+1e1i" 10+10i)

(define-eqv-test "#e#x1p10" (expt 2 10))
(define-eqv-test "#e#x1.1p4" #x11)
(define-eqv-test "#e#x1.1p-1" (* #x11 (expt 2 (- (+ 1 4)))))
(define-eqv-test "#x1.1p-1" (exact->inexact (* #x11 (expt 2 (- (+ 1 4))))))
(define-eqv-test "#x1p-1022" flo:smallest-positive-normal)
(define-eqv-test "#x1.fffffffffffffp+1023" flo:largest-positive-normal)

(define-eqv-test "#b0." 0.)
(define-eqv-test "#b0.+0.i" 0.+0.i)
(define-eqv-test "#b0.-0.i" 0.-0.i)
(define-eqv-test "#b0.+10.i" 0.+2.i)
(define-eqv-test "#b0.-10.i" 0.-2.i)
(define-eqv-test "#b-0." -0.)
(define-eqv-test "#b-0.+0.i" -0.+0.i)
(define-eqv-test "#b-0.-0.i" -0.-0.i)
(define-eqv-test "#b-0.+10.i" -0.+2.i)
(define-eqv-test "#b-0.-10.i" -0.-2.i)
(define-eqv-test "#o0." 0.)
(define-eqv-test "#o0.+0.i" 0.+0.i)
(define-eqv-test "#o0.-0.i" 0.-0.i)
(define-eqv-test "#o0.+10.i" 0.+8.i)
(define-eqv-test "#o0.-10.i" 0.-8.i)
(define-eqv-test "#o-0." -0.)
(define-eqv-test "#o-0.+0.i" -0.+0.i)
(define-eqv-test "#o-0.-0.i" -0.-0.i)
(define-eqv-test "#o-0.+10.i" -0.+8.i)
(define-eqv-test "#o-0.-10.i" -0.-8.i)
(define-eqv-test "#x0." 0.)
(define-eqv-test "#x0.+0.i" 0.+0.i)
(define-eqv-test "#x0.-0.i" 0.-0.i)
(define-eqv-test "#x0.+10.i" 0.+16.i)
(define-eqv-test "#x0.-10.i" 0.-16.i)
(define-eqv-test "#x-0." -0.)
(define-eqv-test "#x-0.+0.i" -0.+0.i)
(define-eqv-test "#x-0.-0.i" -0.-0.i)
(define-eqv-test "#x-0.+10.i" -0.+16.i)
(define-eqv-test "#x-0.-10.i" -0.-16.i)

(define-eqv-test "+2i" (make-rectangular 0 2))
(define-eqv-test "-2i" (make-rectangular 0 -2))
(define-eqv-test "0+2i" (make-rectangular 0 2))
(define-eqv-test "0-2i" (make-rectangular 0 -2))
(define-eqv-test "0.+2i" (make-rectangular +0. 2))
(define-eqv-test "0.-2i" (make-rectangular +0. -2))
(define-eqv-test "-0.+2i" (make-rectangular -0. 2))
(define-eqv-test "-0.-2i" (make-rectangular -0. -2))

(define-eqv-test "2" (make-rectangular 2 0))
(define-eqv-test "-2" (make-rectangular -2 0))
(define-eqv-test "2+0i" (make-rectangular 2 0))
(define-eqv-test "2-0i" (make-rectangular 2 0))
(define-eqv-test "-2+0i" (make-rectangular -2 0))
(define-eqv-test "-2-0i" (make-rectangular -2 0))
(define-eqv-test "2+0.i" (make-rectangular 2 0.))
(define-eqv-test "-2+0.i" (make-rectangular -2 0.))
(define-eqv-test "2-0.i" (make-rectangular 2 -0.))
(define-eqv-test "-2-0.i" (make-rectangular -2 -0.))

(define-eqv-test "1@0" 1)
(define-relerr-test "1@3.141592653589793" -1 1e-15)

(define-eqv-test "+nan.0" (flo:make-nan #f #t 0))
(define-eqv-test "-nan.0" (flo:make-nan #t #t 0))
(define-eqv-test "+nan.1" (flo:make-nan #f #t 1))
(define-eqv-test "-nan.1" (flo:make-nan #t #t 1))
(define-eqv-test "+nan.123" (flo:make-nan #f #t 123))
(define-eqv-test "-nan.123" (flo:make-nan #t #t 123))
(define-eqv-test "#x+nan.123" (flo:make-nan #f #t #x123))
(define-eqv-test "#x-nan.123" (flo:make-nan #t #t #x123))
(define-eqv-test "#x+nan.deadbeef" (flo:make-nan #f #t #xdeadbeef))
(define-eqv-test "#x-nan.deadbeef" (flo:make-nan #t #t #xdeadbeef))
(define-error-test "+snan.0")
(define-error-test "-snan.0")
(define-eqv-test "+snan.1" (flo:make-nan #f #f 1))
(define-eqv-test "-snan.1" (flo:make-nan #t #f 1))
(define-eqv-test "+snan.123" (flo:make-nan #f #f 123))
(define-eqv-test "-snan.123" (flo:make-nan #t #f 123))
(define-eqv-test "#x+snan.123" (flo:make-nan #f #f #x123))
(define-eqv-test "#x-snan.123" (flo:make-nan #t #f #x123))
(define-eqv-test "#x+snan.deadbeef" (flo:make-nan #f #f #xdeadbeef))
(define-eqv-test "#x-snan.deadbeef" (flo:make-nan #t #f #xdeadbeef))
(define-eqv-test "+inf.0" (flo:+inf.0))
(define-eqv-test "-inf.0" (flo:-inf.0))

(define-eqv-test "#i+nan.0" (flo:make-nan #f #t 0))
(define-eqv-test "#i-nan.0" (flo:make-nan #t #t 0))
(define-error-test "#i+snan.0")
(define-error-test "#i-snan.0")
(define-eqv-test "#i+inf.0" (flo:+inf.0))
(define-eqv-test "#i-inf.0" (flo:-inf.0))

(define-error-test "#e+nan.0")
(define-error-test "#e-nan.0")
(define-error-test "#e+snan.0")         ;correctly errors by accident
(define-error-test "#e-snan.0")
(define-error-test "#e+inf.0")
(define-error-test "#e-inf.0")

(define-error-test "+0+0")
(define-error-test "0+0")
(define-error-test "+1+0")
(define-error-test "1+0")
(define-error-test "1+0")
(define-error-test "1+0.")
(define-error-test "1+0.0")
(define-error-test "1+.0")
(define-error-test "1+0/1")
(define-error-test "+0+0+i")
(define-error-test "0+0+i")
