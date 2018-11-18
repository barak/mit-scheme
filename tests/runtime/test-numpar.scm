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

(define (define-eqv-test s v #!optional xfail?)
  (define-eqv-test-1 s v xfail?)
  (if (not (string=? s (string-upcase s)))
      (define-eqv-test-1 (string-upcase s) v xfail?)))

(define (define-eqv-test-1 s v #!optional xfail?)
  (define-test s
    (lambda ()
      (with-xfail xfail?
        (lambda ()
          (assert-eqv (string->number s) v))))))

(define (with-xfail xfail? body)
  (case xfail?
    ((xfail) (expect-failure body))
    ((xerror) (assert-error body))
    (else (body))))

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
