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

;;;; Tests of flonum casting

(declare (usual-integrations))

;;;; Utilities

(define (factorial n)
  (if (< n 2)
      1
      (* n (factorial (- n 1)))))

(define (define-cast-test name flonum integer
          prim:flonum->integer prim:integer->flonum)
  (define (->procedure object)
    (if (procedure? object) object (lambda () object)))
  (let ((flonum->integer (make-primitive-procedure prim:flonum->integer 1))
        (integer->flonum (make-primitive-procedure prim:integer->flonum 1))
        (flonum (->procedure flonum))
        (integer (->procedure integer)))
    (define-test (symbol prim:flonum->integer ': name)
      (lambda ()
        (assert-equal (flonum->integer (flonum)) (integer))))
    (define-test (symbol prim:integer->flonum ': name)
      (lambda ()
        (assert-equal (integer->flonum (integer)) (flonum))))))

(define (define-double-cast-test name double integer)
  (define-cast-test name double integer
    'CAST-IEEE754-DOUBLE-TO-INTEGER
    'CAST-INTEGER-TO-IEEE754-DOUBLE))

(define (define-single-cast-test name single integer)
  (define-cast-test name single integer
    'CAST-IEEE754-SINGLE-TO-INTEGER
    'CAST-INTEGER-TO-IEEE754-SINGLE))

(define (flo:infinite? flonum)
  (not (flo:finite? flonum)))

(define assert-flo:infinite
  (predicate-assertion flo:infinite? "infinite flonum"))

(define assert-flo:positive
  (predicate-assertion flo:positive? "positive flonum"))

(define assert-flo:negative
  (predicate-assertion flo:negative? "negative flonum"))

;;;; Double

(define-double-cast-test 'POSITIVE-ZERO +0.0
  #b0000000000000000000000000000000000000000000000000000000000000000)

(define-double-cast-test 'NEGATIVE-ZERO -0.0
  #b1000000000000000000000000000000000000000000000000000000000000000)

(define-double-cast-test 'POSITIVE-ONE +1.0
  #b0011111111110000000000000000000000000000000000000000000000000000)

(define-double-cast-test 'POSITIVE-TWO +2.0
  #b0100000000000000000000000000000000000000000000000000000000000000)

(define-double-cast-test 'POSITIVE-FOUR +4.0
  #b0100000000010000000000000000000000000000000000000000000000000000)

(define-double-cast-test 'POSITIVE-EIGHT +8.0
  #b0100000000100000000000000000000000000000000000000000000000000000)

(define-double-cast-test 'ONE-HUNDRED-FACTORIAL
  (lambda () (->flonum (factorial 100)))
  #b0110000010111011001100001001011001001110110000111001010111011100)

(define-double-cast-test 'NEGATIVE-ONE -1.0
  #b1011111111110000000000000000000000000000000000000000000000000000)

(define-test 'DOUBLE-POSITIVE-INFINITY-IS-INFINITE
  (lambda ()
    (assert-flo:infinite
     ((make-primitive-procedure 'CAST-INTEGER-TO-IEEE754-DOUBLE 1)
      #b0111111111110000000000000000000000000000000000000000000000000000))))

(define-test 'DOUBLE-POSITIVE-INFINITY-IS-POSITIVE
  (lambda ()
    (assert-flo:positive
     ((make-primitive-procedure 'CAST-INTEGER-TO-IEEE754-DOUBLE 1)
      #b0111111111110000000000000000000000000000000000000000000000000000))))

(define-test 'DOUBLE-NEGATIVE-INFINITY-IS-INFINITE
  (lambda ()
    (assert-flo:infinite
     ((make-primitive-procedure 'CAST-INTEGER-TO-IEEE754-DOUBLE 1)
      #b0111111111110000000000000000000000000000000000000000000000000000))))

(define-test 'DOUBLE-NEGATIVE-INFINITY-IS-NEGATIVE
  (lambda ()
    (assert-flo:negative
     ((make-primitive-procedure 'CAST-INTEGER-TO-IEEE754-DOUBLE 1)
      #b1111111111110000000000000000000000000000000000000000000000000000))))

;;;; Single

(define-single-cast-test 'POSITIVE-ZERO +0.0
  #b00000000000000000000000000000000)

(define-single-cast-test 'NEGATIVE-ZERO -0.0
  #b10000000000000000000000000000000)

(define-single-cast-test 'POSITIVE-ONE +1.0
  #b00111111100000000000000000000000)

(define-single-cast-test 'POSITIVE-TWO +2.0
  #b01000000000000000000000000000000)

(define-single-cast-test 'POSITIVE-FOUR +4.0
  #b01000000100000000000000000000000)

(define-single-cast-test 'POSITIVE-EIGHT +8.0
  #b01000001000000000000000000000000)

(define-single-cast-test 'TEN-FACTORIAL (lambda () (->flonum (factorial 10)))
  #b01001010010111010111110000000000)

(define-single-cast-test 'NEGATIVE-ONE -1.0
  #b10111111100000000000000000000000)

(define-test 'SINGLE-POSITIVE-INFINITY-IS-INFINITE
  (lambda ()
    (assert-flo:infinite
     ((make-primitive-procedure 'CAST-INTEGER-TO-IEEE754-SINGLE 1)
      #b01111111100000000000000000000000))))

(define-test 'SINGLE-POSITIVE-INFINITY-IS-POSITIVE
  (lambda ()
    (assert-flo:positive
     ((make-primitive-procedure 'CAST-INTEGER-TO-IEEE754-SINGLE 1)
      #b01111111100000000000000000000000))))

(define-test 'SINGLE-NEGATIVE-INFINITY-IS-INFINITE
  (lambda ()
    (assert-flo:infinite
     ((make-primitive-procedure 'CAST-INTEGER-TO-IEEE754-SINGLE 1)
      #b11111111100000000000000000000000))))

(define-test 'SINGLE-NEGATIVE-INFINITY-IS-NEGATIVE
  (lambda ()
    (assert-flo:negative
     ((make-primitive-procedure 'CAST-INTEGER-TO-IEEE754-SINGLE 1)
      #b11111111100000000000000000000000))))
