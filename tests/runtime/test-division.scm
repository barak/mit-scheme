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

;;;; Tests of integer division operators

(declare (usual-integrations))

(define (check-division n d correct-q q r)
  (let ((correct-r (- n (* d correct-q))))
    (assert-eqv q correct-q)
    (assert-eqv r correct-r)))

(define division-test-iterations #x1000)

;; Such a huge bound as this tests bignum arithmetic, not just fixnum
;; arithmetic.
(define division-test-bound #x100000000000000000000000000000000)

(define (random-sign a b)
  ((if (zero? (random-integer 2)) - +) a b))

(define (randomly-generate-operands n+ d+ receiver)
  (do ((i 0 (+ i 1))) ((>= i division-test-iterations))
    (let ((n (n+ 0 (random-integer division-test-bound)))
          (d (d+ 0 (+ 1 (random-integer (- division-test-bound 1))))))
      (receiver n d))))

(define (randomly-generate-divisors d+ receiver)
  (do ((i 0 (+ i 1))) ((>= i division-test-iterations))
    (let ((d (d+ 0 (+ 1 (random-integer (- division-test-bound 1))))))
      (receiver d))))

(define (randomly-test-division n+ d+ / quotient remainder divider)
  (randomly-generate-operands n+ d+
    (lambda (n d)
      (let ((correct-q (divider n d)))
        (check-division n d correct-q (quotient n d) (remainder n d))
        (receive (q r) (/ n d)
          (check-division n d correct-q q r))))))

(define-test 'RANDOM-CORRECTNESS-TESTS:CEILING+/+
  (lambda ()
    (randomly-test-division + + ceiling/ ceiling-quotient ceiling-remainder
      (lambda (n d) (ceiling (/ n d))))))

(define-test 'RANDOM-CORRECTNESS-TESTS:CEILING-/+
  (lambda ()
    (randomly-test-division - + ceiling/ ceiling-quotient ceiling-remainder
      (lambda (n d) (ceiling (/ n d))))))

(define-test 'RANDOM-CORRECTNESS-TESTS:CEILING+/-
  (lambda ()
    (randomly-test-division + - ceiling/ ceiling-quotient ceiling-remainder
      (lambda (n d) (ceiling (/ n d))))))

(define-test 'RANDOM-CORRECTNESS-TESTS:CEILING-/-
  (lambda ()
    (randomly-test-division - - ceiling/ ceiling-quotient ceiling-remainder
      (lambda (n d) (ceiling (/ n d))))))

(define-test 'RANDOM-CORRECTNESS-TESTS:EUCLIDEAN+/+
  (lambda ()
    (randomly-test-division
        + + euclidean/ euclidean-quotient euclidean-remainder
      (lambda (n d) ((if (< d 0) ceiling floor) (/ n d))))))

(define-test 'RANDOM-CORRECTNESS-TESTS:EUCLIDEAN-/+
  (lambda ()
    (randomly-test-division
        - + euclidean/ euclidean-quotient euclidean-remainder
      (lambda (n d) ((if (< d 0) ceiling floor) (/ n d))))))

(define-test 'RANDOM-CORRECTNESS-TESTS:EUCLIDEAN+/-
  (lambda ()
    (randomly-test-division
        + - euclidean/ euclidean-quotient euclidean-remainder
      (lambda (n d) ((if (< d 0) ceiling floor) (/ n d))))))

(define-test 'RANDOM-CORRECTNESS-TESTS:EUCLIDEAN-/-
  (lambda ()
    (randomly-test-division
        - - euclidean/ euclidean-quotient euclidean-remainder
      (lambda (n d) ((if (< d 0) ceiling floor) (/ n d))))))

(define-test 'RANDOM-CORRECTNESS-TESTS:FLOOR+/+
  (lambda ()
    (randomly-test-division + + floor/ floor-quotient floor-remainder
      (lambda (n d) (floor (/ n d))))))

(define-test 'RANDOM-CORRECTNESS-TESTS:FLOOR-/+
  (lambda ()
    (randomly-test-division - + floor/ floor-quotient floor-remainder
      (lambda (n d) (floor (/ n d))))))

(define-test 'RANDOM-CORRECTNESS-TESTS:FLOOR+/-
  (lambda ()
    (randomly-test-division + - floor/ floor-quotient floor-remainder
      (lambda (n d) (floor (/ n d))))))

(define-test 'RANDOM-CORRECTNESS-TESTS:FLOOR-/-
  (lambda ()
    (randomly-test-division - - floor/ floor-quotient floor-remainder
      (lambda (n d) (floor (/ n d))))))

(define-test 'RANDOM-CORRECTNESS-TESTS:ROUND+/+
  (lambda ()
    (randomly-test-division + + round/ round-quotient round-remainder
      (lambda (n d) (round (/ n d))))))

(define-test 'RANDOM-CORRECTNESS-TESTS:ROUND-/+
  (lambda ()
    (randomly-test-division - + round/ round-quotient round-remainder
      (lambda (n d) (round (/ n d))))))

(define-test 'RANDOM-CORRECTNESS-TESTS:ROUND+/-
  (lambda ()
    (randomly-test-division + - round/ round-quotient round-remainder
      (lambda (n d) (round (/ n d))))))

(define-test 'RANDOM-CORRECTNESS-TESTS:ROUND-/-
  (lambda ()
    (randomly-test-division - - round/ round-quotient round-remainder
      (lambda (n d) (round (/ n d))))))

(define-test 'RANDOM-CORRECTNESS-TESTS:TRUNCATE+/+
  (lambda ()
    (randomly-test-division + + truncate/ truncate-quotient truncate-remainder
      (lambda (n d) (truncate (/ n d))))))

(define-test 'RANDOM-CORRECTNESS-TESTS:TRUNCATE-/+
  (lambda ()
    (randomly-test-division - + truncate/ truncate-quotient truncate-remainder
      (lambda (n d) (truncate (/ n d))))))

(define-test 'RANDOM-CORRECTNESS-TESTS:TRUNCATE+/-
  (lambda ()
    (randomly-test-division + - truncate/ truncate-quotient truncate-remainder
      (lambda (n d) (truncate (/ n d))))))

(define-test 'RANDOM-CORRECTNESS-TESTS:TRUNCATE-/-
  (lambda ()
    (randomly-test-division - - truncate/ truncate-quotient truncate-remainder
      (lambda (n d) (truncate (/ n d))))))

(define (randomly-test-properties / assert-property)
  (randomly-generate-operands random-sign random-sign
    (lambda (n d) (receive (q r) (/ n d) (assert-property n d q r)))))

(define (assert-n=dq+r n d q r)
  (assert-eqv (+ (* d q) r) n))

(define-test 'N=DQ+R-TESTS:EUCLIDEAN
  (lambda () (randomly-test-properties euclidean/ assert-n=dq+r)))

(define-test 'N=DQ+R-TESTS:FLOOR
  (lambda () (randomly-test-properties floor/ assert-n=dq+r)))

(define-test 'N=DQ+R-TESTS:ROUND
  (lambda () (randomly-test-properties round/ assert-n=dq+r)))

(define-test 'N=DQ+R-TESTS:TRUNCATE
  (lambda () (randomly-test-properties truncate/ assert-n=dq+r)))

(define (assert-r<d n d q r)
  n q                                   ;ignore
  (assert-< (abs r) (abs d)))

(define (assert-r<d* n d q r)
  n q                                   ;ignore
  (assert-< r (abs d)))

(define-test 'R<D-TESTS:CEILING
  (lambda () (randomly-test-properties ceiling/ assert-r<d)))

(define-test 'R<D-TESTS:EUCLIDEAN
  (lambda () (randomly-test-properties euclidean/ assert-r<d)))

(define-test 'R<D-TESTS:EUCLIDEAN*
  (lambda () (randomly-test-properties euclidean/ assert-r<d*)))

(define-test 'R<D-TESTS:FLOOR
  (lambda () (randomly-test-properties floor/ assert-r<d)))

(define-test 'R<D-TESTS:ROUND
  (lambda () (randomly-test-properties round/ assert-r<d)))

(define-test 'R<D-TESTS:TRUNCATE
  (lambda () (randomly-test-properties truncate/ assert-r<d)))

(define (assert-integral-quotient n d q r)
  n d r                                 ;ignore
  (assert integer? "integer" q))

(define-test 'INTEGRAL-QUOTIENT-TESTS:CEILING
  (lambda () (randomly-test-properties ceiling/ assert-integral-quotient)))

(define-test 'INTEGRAL-QUOTIENT-TESTS:EUCLIDEAN
  (lambda () (randomly-test-properties euclidean/ assert-integral-quotient)))

(define-test 'INTEGRAL-QUOTIENT-TESTS:FLOOR
  (lambda () (randomly-test-properties floor/ assert-integral-quotient)))

(define-test 'INTEGRAL-QUOTIENT-TESTS:ROUND
  (lambda () (randomly-test-properties round/ assert-integral-quotient)))

(define-test 'INTEGRAL-QUOTIENT-TESTS:TRUNCATE
  (lambda () (randomly-test-properties truncate/ assert-integral-quotient)))

(define (test-trivial-quotient quotient)
  (assert-eqv (quotient +1 +1) +1)
  (assert-eqv (quotient -1 +1) -1)
  (assert-eqv (quotient +1 -1) -1)
  (assert-eqv (quotient -1 -1) +1)
  (assert-eqv (quotient 0 +1) 0)
  (assert-eqv (quotient 0 -1) 0))

(define (test-trivial/ /)
  (test-trivial-quotient (lambda (n d) (receive (q r) (/ n d) r q))))

(define-test 'TRIVIAL-DIVIDEND/TRIVIAL-DIVISOR-TESTS:CEILING-QUOTIENT
  (lambda () (test-trivial-quotient ceiling-quotient)))

(define-test 'TRIVIAL-DIVIDEND/TRIVIAL-DIVISOR-TESTS:CEILING/
  (lambda () (test-trivial/ ceiling/)))

(define-test 'TRIVIAL-DIVIDEND/TRIVIAL-DIVISOR-TESTS:EUCLIDEAN-QUOTIENT
  (lambda () (test-trivial-quotient euclidean-quotient)))

(define-test 'TRIVIAL-DIVIDEND/TRIVIAL-DIVISOR-TESTS:EUCLIDEAN/
  (lambda () (test-trivial/ euclidean/)))

(define-test 'TRIVIAL-DIVIDEND/TRIVIAL-DIVISOR-TESTS:FLOOR-QUOTIENT
  (lambda () (test-trivial-quotient floor-quotient)))

(define-test 'TRIVIAL-DIVIDEND/TRIVIAL-DIVISOR-TESTS:FLOOR/
  (lambda () (test-trivial/ floor/)))

(define-test 'TRIVIAL-DIVIDEND/TRIVIAL-DIVISOR-TESTS:ROUND-QUOTIENT
  (lambda () (test-trivial-quotient round-quotient)))

(define-test 'TRIVIAL-DIVIDEND/TRIVIAL-DIVISOR-TESTS:ROUND/
  (lambda () (test-trivial/ round/)))

(define-test 'TRIVIAL-DIVIDEND/TRIVIAL-DIVISOR-TESTS:TRUNCATE-QUOTIENT
  (lambda () (test-trivial-quotient truncate-quotient)))

(define-test 'TRIVIAL-DIVIDEND/TRIVIAL-DIVISOR-TESTS:TRUNCATE/
  (lambda () (test-trivial/ truncate/)))

(define-test 'TRIVIAL-DIVIDEND/RANDOM-DIVISOR-TESTS:CEILING
  (lambda ()
    (randomly-generate-divisors random-sign
      (lambda (d)
        (assert-eqv (ceiling-quotient 0 d) 0)
        (if (< 1 (abs d))
            (begin
              (assert-eqv (ceiling-quotient +1 d) (if (negative? d) 0 +1))
              (assert-eqv (ceiling-quotient -1 d)
                          (if (negative? d) +1 0))))))))

(define-test 'TRIVIAL-DIVIDEND/RANDOM-DIVISOR-TESTS:EUCLIDEAN
  (lambda ()
    (randomly-generate-divisors random-sign
      (lambda (d)
        (assert-eqv (euclidean-quotient 0 d) 0)
        (if (< 1 (abs d))
            (begin
              (assert-eqv (euclidean-quotient +1 d) 0)
              (assert-eqv (euclidean-quotient -1 d)
                          (if (negative? d) +1 -1))))))))

(define-test 'TRIVIAL-DIVIDEND/RANDOM-DIVISOR-TESTS:FLOOR
  (lambda ()
    (randomly-generate-divisors random-sign
      (lambda (d)
        (assert-eqv (floor-quotient 0 d) 0)
        (if (< 1 (abs d))
            (begin
              (assert-eqv (floor-quotient -1 d) (if (negative? d) 0 -1))
              (assert-eqv (floor-quotient +1 d) (if (negative? d) -1 0))))))))

(define-test 'TRIVIAL-DIVIDEND/RANDOM-DIVISOR-TESTS:ROUND
  (lambda ()
    (randomly-generate-divisors random-sign
      (lambda (d)
        (assert-eqv (round-quotient -1 d) 0)
        (assert-eqv (round-quotient 0 d) 0)
        (assert-eqv (round-quotient +1 d) 0)))))

(define-test 'TRIVIAL-DIVIDEND/RANDOM-DIVISOR-TESTS:TRUNCATE
  (lambda ()
    (randomly-generate-divisors random-sign
      (lambda (d)
        (assert-eqv (truncate-quotient -1 d) 0)
        (assert-eqv (truncate-quotient 0 d) 0)
        (assert-eqv (truncate-quotient +1 d) 0)))))