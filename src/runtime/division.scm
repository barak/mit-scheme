#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

;;;; Integer Division
;;;; package: (runtime integer-division)

(declare (usual-integrations))

;;;; Ceiling

(define (ceiling/ n d)
  (if (and (exact-integer? n) (exact-integer? d))
      (cond ((and (negative? n) (negative? d))
             (ceiling-/- n d))
            ((negative? n)
             (let ((n (- 0 n)))
               (values (- 0 (quotient n d)) (- 0 (remainder n d)))))
            ((negative? d)
             (let ((d (- 0 d)))
               (values (- 0 (quotient n d)) (remainder n d))))
            (else
             (ceiling+/+ n d)))
      (let ((q (ceiling (/ n d))))
        (values q (- n (* d q))))))

(define (ceiling-/- n d)
  (let ((n (- 0 n)) (d (- 0 d)))
    (let ((q (quotient n d)) (r (remainder n d)))
      (if (zero? r)
          (values q r)
          (values (+ q 1) (- d r))))))

(define (ceiling+/+ n d)
  (let ((q (quotient n d)) (r (remainder n d)))
    (if (zero? r)
        (values q r)
        (values (+ q 1) (- r d)))))

(define (ceiling-quotient n d)
  (if (and (exact-integer? n) (exact-integer? d))
      (cond ((and (negative? n) (negative? d))
             (receive (q r) (ceiling-/- n d) r q))
            ((negative? n) (- 0 (quotient (- 0 n) d)))
            ((negative? d) (- 0 (quotient n (- 0 d))))
            (else (receive (q r) (ceiling+/+ n d) r q)))
      (ceiling (/ n d))))

(define (ceiling-remainder n d)
  (if (and (exact-integer? n) (exact-integer? d))
      (cond ((and (negative? n) (negative? d))
             (receive (q r) (ceiling-/- n d) q r))
            ((negative? n) (- 0 (remainder (- 0 n) d)))
            ((negative? d) (remainder n (- 0 d)))
            (else (receive (q r) (ceiling+/+ n d) q r)))
      (- n (* d (ceiling (/ n d))))))

;;;; Euclidean Division

;;; 0 < r < |d|

(define (euclidean/ n d)
  (if (and (exact-integer? n) (exact-integer? d))
      (cond ((and (negative? n) (negative? d)) (ceiling-/- n d))
            ((negative? n) (floor-/+ n d))
            ((negative? d)
             (let ((d (- 0 d)))
               (values (- 0 (quotient n d)) (remainder n d))))
            (else (values (quotient n d) (remainder n d))))
      (let ((q (if (negative? d) (ceiling (/ n d)) (floor (/ n d)))))
        (values q (- n (* d q))))))

(define (euclidean-quotient n d)
  (if (and (exact-integer? n) (exact-integer? d))
      (cond ((and (negative? n) (negative? d))
             (receive (q r) (ceiling-/- n d) r q))
            ((negative? n) (receive (q r) (floor-/+ n d) r q))
            ((negative? d) (- 0 (quotient n (- 0 d))))
            (else (quotient n d)))
      (if (negative? d) (ceiling (/ n d)) (floor (/ n d)))))

(define (euclidean-remainder n d)
  (if (and (exact-integer? n) (exact-integer? d))
      (cond ((and (negative? n) (negative? d))
             (receive (q r) (ceiling-/- n d) q r))
            ((negative? n) (receive (q r) (floor-/+ n d) q r))
            ((negative? d) (remainder n (- 0 d)))
            (else (remainder n d)))
      (- n (* d (if (negative? d) (ceiling (/ n d)) (floor (/ n d)))))))

;;;; Floor

(define (floor/ n d)
  (if (and (exact-integer? n) (exact-integer? d))
      (cond ((and (negative? n) (negative? d))
             (let ((n (- 0 n)) (d (- 0 d)))
               (values (quotient n d) (- 0 (remainder n d)))))
            ((negative? n) (floor-/+ n d))
            ((negative? d) (floor+/- n d))
            (else (values (quotient n d) (remainder n d))))
      (let ((q (floor (/ n d))))
        (values q (- n (* d q))))))

(define (floor-/+ n d)
  (let ((n (- 0 n)))
    (let ((q (quotient n d)) (r (remainder n d)))
      (if (zero? r)
          (values (- 0 q) r)
          (values (- (- 0 q) 1) (- d r))))))

(define (floor+/- n d)
  (let ((d (- 0 d)))
    (let ((q (quotient n d)) (r (remainder n d)))
      (if (zero? r)
          (values (- 0 q) r)
          (values (- (- 0 q) 1) (- r d))))))

(define (floor-quotient n d)
  (if (and (exact-integer? n) (exact-integer? d))
      (cond ((and (negative? n) (negative? d)) (quotient (- 0 n) (- 0 d)))
            ((negative? n) (receive (q r) (floor-/+ n d) r q))
            ((negative? d) (receive (q r) (floor+/- n d) r q))
            (else (quotient n d)))
      (floor (/ n d))))

(define (floor-remainder n d)
  (if (and (exact-integer? n) (exact-integer? d))
      (cond ((and (negative? n) (negative? d))
             (- 0 (remainder (- 0 n) (- 0 d))))
            ((negative? n) (receive (q r) (floor-/+ n d) q r))
            ((negative? d) (receive (q r) (floor+/- n d) q r))
            (else (remainder n d)))
      (- n (* d (floor (/ n d))))))

;;;; Round Ties to Even

(define (round/ n d)
  (define (divide n d adjust leave)
    (let ((q (quotient n d)) (r (remainder n d)))
      (if (and (not (zero? r))
               (or (and (odd? q) (even? d) (divisible? n (quotient d 2)))
                   (< d (* 2 r))))
          (adjust (+ q 1) (- r d))
          (leave q r))))
  (if (and (exact-integer? n) (exact-integer? d))
      (cond ((and (negative? n) (negative? d))
             (divide (- 0 n) (- 0 d)
               (lambda (q r) (values q (- 0 r)))
               (lambda (q r) (values q (- 0 r)))))
            ((negative? n)
             (divide (- 0 n) d
               (lambda (q r) (values (- 0 q) (- 0 r)))
               (lambda (q r) (values (- 0 q) (- 0 r)))))
            ((negative? d)
             (divide n (- 0 d)
               (lambda (q r) (values (- 0 q) r))
               (lambda (q r) (values (- 0 q) r))))
            (else
             (let ((return (lambda (q r) (values q r))))
               (divide n d return return))))
      (let ((q (round (/ n d))))
        (values q (- n (* d q))))))

(define (divisible? n d)
  ;; This operation admits a faster implementation than the one given
  ;; here.
  (zero? (remainder n d)))

(define (round-quotient n d)
  (if (and (exact-integer? n) (exact-integer? d))
      (receive (q r) (round/ n d)
        r                               ;ignore
        q)
      (round (/ n d))))

(define (round-remainder n d)
  (if (and (exact-integer? n) (exact-integer? d))
      (receive (q r) (round/ n d)
        q                               ;ignore
        r)
      (- n (* d (round (/ n d))))))

;;;; Truncate

(define (truncate/ n d)
  (if (and (exact-integer? n) (exact-integer? d))
      (cond ((and (negative? n) (negative? d))
             (let ((n (- 0 n)) (d (- 0 d)))
               (values (quotient n d) (- 0 (remainder n d)))))
            ((negative? n)
             (let ((n (- 0 n)))
               (values (- 0 (quotient n d)) (- 0 (remainder n d)))))
            ((negative? d)
             (let ((d (- 0 d)))
               (values (- 0 (quotient n d)) (remainder n d))))
            (else
             (values (quotient n d) (remainder n d))))
      (let ((q (truncate (/ n d))))
        (values q (- n (* d q))))))

(define (truncate-quotient n d)
  (if (and (exact-integer? n) (exact-integer? d))
      (cond ((and (negative? n) (negative? d)) (quotient (- 0 n) (- 0 d)))
            ((negative? n) (- 0 (quotient (- 0 n) d)))
            ((negative? d) (- 0 (quotient n (- 0 d))))
            (else (quotient n d)))
      (truncate (/ n d))))

(define (truncate-remainder n d)
  (if (and (exact-integer? n) (exact-integer? d))
      (cond ((and (negative? n) (negative? d))
             (- 0 (remainder (- 0 n) (- 0 d))))
            ((negative? n) (- 0 (remainder (- 0 n) d)))
            ((negative? d) (remainder n (- 0 d)))
            (else (remainder n d)))
      (- n (* d (truncate (/ n d))))))