#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

;;;; Tests of integer bit operations

(declare (usual-integrations))

(define (random-integer-of-weight w n)
  (let ((bit-string (make-bit-string n #f)))
    (do ((i 0 (+ i 1))) ((>= i w)) (bit-string-set! bit-string i))
    (do ((i 1 (+ i 1))) ((>= i n))
      (let ((j (random-integer (+ i 1))))
        (let ((t (bit-string-ref bit-string i)))
          ((if (bit-string-ref bit-string j) bit-string-set! bit-string-clear!)
           bit-string i)
          ((if t bit-string-set! bit-string-clear!) bit-string j))))
    (let ((integer (bit-string->unsigned-integer bit-string)))
      (if (zero? (random-integer 2))
          (- -1 integer)
          integer))))

(define (random-fixnum)
  (+ (smallest-fixnum)
     (random-integer (- (+ 1 (largest-fixnum)) (smallest-fixnum)))))

(define (random-large-integer)
  (let ((n (random-integer #x100)))
    (random-integer-of-weight (random-integer (+ n 1)) n)))

(define (randomly-generate-integers procedure)
  (do ((i 0 (+ i 1))) ((= i #x100))
    (procedure (random-large-integer))))

(define (randomly-generate-fixnums procedure)
  (do ((i 0 (+ i 1))) ((= i #x1000))
    (procedure (random-fixnum))))

(define (randomly-generate-integer-pairs procedure)
  (do ((i 0 (+ i 1))) ((= i #x100))
    (procedure (random-large-integer) (random-large-integer))))

(define (randomly-generate-fixnum-pairs procedure)
  (do ((i 0 (+ i 1))) ((= i #x1000))
    (procedure (random-fixnum) (random-fixnum))))

(define (define-random-unary-fixnum-test name procedure)
  (define-test name
    (lambda ()
      (randomly-generate-fixnums procedure))))

(define (define-random-unary-integer-test name procedure)
  (define-test name
    (lambda ()
      (randomly-generate-integers procedure))))

(define (define-random-binary-fixnum-test name procedure)
  (define-test name
    (lambda ()
      (randomly-generate-fixnum-pairs procedure))))

(define (define-random-binary-integer-test name procedure)
  (define-test name
    (lambda ()
      (randomly-generate-integer-pairs procedure))))

;;;; Shift

(define-test 'SHIFT-LEFT:0
  (lambda ()
    (do ((i 0 (+ i 1))) ((>= i #x1000))
      (assert-eqv (shift-left 0 i) 0))))

(define-test 'SHIFT-LEFT:+1
  (lambda ()
    (do ((i 0 (+ i 1))) ((>= i #x1000))
      (assert-eqv (shift-left 1 i) (expt 2 i)))))

(define-test 'SHIFT-LEFT:-1
  (lambda ()
    (do ((i 0 (+ i 1))) ((>= i #x1000))
      (assert-eqv (shift-left -1 i) (* -1 (expt 2 i))))))

(define-test 'SHIFT-LEFT:POSITIVE
  (lambda ()
    (do ((i 0 (+ i 1))) ((>= i #x10))
      (let ((n (random-integer #x1000)))
        (do ((i 0 (+ i 1))) ((>= i #x100))
          (assert-eqv (shift-left n i) (* n (expt 2 i))))))))

(define-test 'SHIFT-LEFT:NEGATIVE
  (lambda ()
    (do ((i 0 (+ i 1))) ((>= i #x10))
      (let ((n (- -1 (random-integer #x1000))))
        (do ((i 0 (+ i 1))) ((>= i #x100))
          (assert-eqv (shift-left n i) (* n (expt 2 i))))))))

(define-test 'SHIFT-RIGHT:0
  (lambda ()
    (do ((i 0 (+ i 1))) ((>= i #x1000))
      (assert-eqv (shift-right 0 i) 0))))

(define-test 'SHIFT-RIGHT:1
  (lambda ()
    (assert-eqv (shift-right 1 0) 1)
    (do ((i 1 (+ i 1))) ((>= i #x1000))
      (assert-eqv (shift-right 0 i) 0))))

(define-test 'SHIFT-RIGHT:-1
  (lambda ()
    (do ((i 0 (+ i 1))) ((>= i #x1000))
      (assert-eqv (shift-right -1 i) -1))))

(define-test 'SHIFT-LEFT-THEN-RIGHT:POSITIVE
  (lambda ()
    (do ((i 0 (+ i 1))) ((>= i #x10))
      (let ((n (random-integer #x1000)))
        (do ((i 0 (+ i 1))) ((>= i #x100))
          (assert-eqv (shift-right (shift-left n i) i) n))))))

(define-test 'SHIFT-LEFT-THEN-RIGHT:NEGATIVE
  (lambda ()
    (do ((i 0 (+ i 1))) ((>= i #x10))
      (let ((n (- -1 (random-integer #x1000))))
        (do ((i 0 (+ i 1))) ((>= i #x100))
          (assert-eqv (shift-right (shift-left n i) i) n))))))

;;;; Bitwise NOT

(define-test 'BITWISE-NOT:0
  (lambda ()
    (assert-eqv (bitwise-not 0) -1)
    (assert-eqv (bitwise-not -1) 0)))

(define-test 'BITWISE-NOT:1
  (lambda ()
    (assert-eqv (bitwise-not 1) -2)
    (assert-eqv (bitwise-not -2) 1)))

(define-test 'BITWISE-NOT:EXTREME-FIXNUM
  (lambda ()
    (assert-eqv (bitwise-not (largest-fixnum)) (smallest-fixnum))
    (assert-eqv (bitwise-not (smallest-fixnum)) (largest-fixnum))))

(define-random-unary-fixnum-test 'BITWISE-NOT:FIXNUM
  (lambda (n)
    (assert-eqv (bitwise-not n) (- -1 n))
    (assert-eqv (bitwise-not (bitwise-not n)) n)
    (assert-eqv (bitwise-not n) (fix:not n))
    (assert-eqv (bitwise-not (fix:not n)) n)))

(define-random-unary-integer-test 'BITWISE-NOT
  (lambda (n)
    (assert-eqv (bitwise-not n) (- -1 n))
    (assert-eqv (bitwise-not (bitwise-not n)) n)))

;;;; Binary Bitwise Operators

(define (define-bitwise/fixnum-test name general-operator fixnum-operator)
  (define-random-binary-fixnum-test (symbol name ': 'FIXNUM)
    (lambda (a b)
      (assert-eqv (general-operator a b) (fixnum-operator a b)))))

(define-bitwise/fixnum-test 'BITWISE-AND bitwise-and fix:and)
(define-bitwise/fixnum-test 'BITWISE-ANDC2 bitwise-andc2 fix:andc)
(define-bitwise/fixnum-test 'BITWISE-IOR bitwise-ior fix:or)
(define-bitwise/fixnum-test 'BITWISE-XOR bitwise-xor fix:xor)

(define (euclidean-divide n d)
  (let ((q ((if (negative? d) ceiling floor) (/ n d))))
    (values q (- n (* d q)))))

(define (define-binary-bitwise-test name operator ff ft tf tt)
  (define (bit a b)
    (vector-ref (vector ff ft tf tt) (+ (* a 2) b)))
  (define (result? object)
    (or (= object -1) (= object 0)))
  (define (result-bit result)
    (case result ((-1) 1) ((0) 0) (else -1)))
  (define (check-integer a b)
    (let ((a*b (operator a b)))
      (define (check-bit a0 b0 a*b0)
        (if (not (eqv? a*b0 (bit a0 b0)))
            (error "Failed:" `(,name ,a ,b) '=> a*b)))
      (let loop ((a a) (b b) (a*b a*b))
        (if (and (result? a) (result? b))
            (check-bit (result-bit a) (result-bit b) (result-bit a*b))
            (receive (a a0) (euclidean-divide a 2)
              (receive (b b0) (euclidean-divide b 2)
                (receive (a*b a*b0) (euclidean-divide a*b 2)
                  (check-bit a0 b0 a*b0)
                  (loop a b a*b))))))))
  (define (define-trivial-test subname a b)
    (define (signum x) (cond ((< x 0) -1) ((< 0 x) +1) (else 0)))
    (define-test (symbol name ': subname ': (signum a) ': (signum b))
      (lambda ()
        (check-integer a b))))
  (define-trivial-test 'TRIVIAL-FIXNUM 0 0)
  (define-trivial-test 'TRIVIAL-FIXNUM 0 1)
  (define-trivial-test 'TRIVIAL-FIXNUM 1 0)
  (define-trivial-test 'TRIVIAL-FIXNUM 1 1)
  (define-trivial-test 'TRIVIAL-FIXNUM 0 -1)
  (define-trivial-test 'TRIVIAL-FIXNUM -1 0)
  (define-trivial-test 'TRIVIAL-FIXNUM -1 -1)
  (let ((s (expt 2 100)))
    (define-trivial-test 'TRIVIAL-BIGNUM 0 s)
    (define-trivial-test 'TRIVIAL-BIGNUM s 0)
    (define-trivial-test 'TRIVIAL-BIGNUM s s))
  (define-random-binary-integer-test (symbol name ': 'RANDOM) check-integer))

;; (define-binary-bitwise-test 'BITWISE-CLEAR bitwise-clear 0 0 0 0)
(define-binary-bitwise-test 'BITWISE-AND bitwise-and 0 0 0 1)
(define-binary-bitwise-test 'BITWISE-ANDC2 bitwise-andc2 0 0 1 0)
;; (define-binary-bitwise-test 'BITWISE-ARG1 bitwise-arg1 0 0 1 1)
(define-binary-bitwise-test 'BITWISE-ANDC1 bitwise-andc1 0 1 0 0)
;; (define-binary-bitwise-test 'BITWISE-ARG2 bitwise-arg2 0 1 0 1)
(define-binary-bitwise-test 'BITWISE-XOR bitwise-xor 0 1 1 0)
(define-binary-bitwise-test 'BITWISE-IOR bitwise-ior 0 1 1 1)
(define-binary-bitwise-test 'BITWISE-NOR bitwise-nor 1 0 0 0)
(define-binary-bitwise-test 'BITWISE-EQV bitwise-eqv 1 0 0 1)
;; (define-binary-bitwise-test 'BITWISE-NOT2 bitwise-not2 1 0 1 0)
(define-binary-bitwise-test 'BITWISE-ORC2 bitwise-orc2 1 0 1 1)
;; (define-binary-bitwise-test 'BITWISE-NOT1 bitwise-not2 1 1 0 0)
(define-binary-bitwise-test 'BITWISE-ORC1 bitwise-orc1 1 1 0 1)
(define-binary-bitwise-test 'BITWISE-NAND bitwise-nand 1 1 1 0)
;; (define-binary-bitwise-test 'BITWISE-SET bitwise-set 1 1 1 1)

;;;;; Binary Bitwise Identities

(define (define-bitwise-identity-test name operator identity)
  (define-random-unary-integer-test (symbol name ': 'IDENTITY)
    (lambda (n)
      (assert-eqv (operator n identity) n)
      (assert-eqv (operator identity n) n))))

(define-bitwise-identity-test 'BITWISE-AND bitwise-and -1)
(define-bitwise-identity-test 'BITWISE-XOR bitwise-xor 0)
(define-bitwise-identity-test 'BITWISE-IOR bitwise-ior 0)
(define-bitwise-identity-test 'BITWISE-EQV bitwise-eqv -1)

(define (define-bitwise-commutativity-test name operator)
  (define-random-binary-integer-test (symbol name ': 'COMMUTATIVITY)
    (lambda (a b)
      (assert-eqv (operator a b) (operator b a)))))

(define-bitwise-commutativity-test 'BITWISE-AND bitwise-and)
(define-bitwise-commutativity-test 'BITWISE-XOR bitwise-xor)
(define-bitwise-commutativity-test 'BITWISE-IOR bitwise-ior)
(define-bitwise-commutativity-test 'BITWISE-NOR bitwise-nor)
(define-bitwise-commutativity-test 'BITWISE-EQV bitwise-eqv)
(define-bitwise-commutativity-test 'BITWISE-NAND bitwise-nand)

(define-random-binary-integer-test 'BITWISE-AND:DEMORGAN
  (lambda (a b)
    (assert-eqv (bitwise-and a b)
                (bitwise-not (bitwise-ior (bitwise-not a) (bitwise-not b))))))

(define-random-binary-integer-test 'BITWISE-ANDC2:AND-NOT2
  (lambda (a b)
    (assert-eqv (bitwise-andc2 a b) (bitwise-and a (bitwise-not b)))))

(define-random-binary-integer-test 'BITWISE-ANDC1:AND-NOT1
  (lambda (a b)
    (assert-eqv (bitwise-andc1 a b) (bitwise-and (bitwise-not a) b))))

(define-random-binary-integer-test 'BITWISE-NOR:NOT-IOR
  (lambda (a b)
    (assert-eqv (bitwise-nor a b) (bitwise-not (bitwise-ior a b)))))

(define-random-binary-integer-test 'BITWISE-XOR:AND-NAND-IOR
  (lambda (a b)
    (assert-eqv (bitwise-xor a b)
                (bitwise-and (bitwise-nand a b) (bitwise-ior a b)))))

(define-random-binary-integer-test 'BITWISE-EQV:NOT-XOR
  (lambda (a b)
    (assert-eqv (bitwise-eqv a b) (bitwise-not (bitwise-xor a b)))))

(define-random-binary-integer-test 'BITWISE-ORC1:IOR-NOT1
  (lambda (a b)
    (assert-eqv (bitwise-orc1 a b) (bitwise-ior (bitwise-not a) b))))

(define-random-binary-integer-test 'BITWISE-ORC2:IOR-NOT2
  (lambda (a b)
    (assert-eqv (bitwise-orc2 a b) (bitwise-ior a (bitwise-not b)))))

(define-random-binary-integer-test 'BITWISE-NAND:NOT-AND
  (lambda (a b)
    (assert-eqv (bitwise-nand a b) (bitwise-not (bitwise-and a b)))))

(define-test 'BIT-COUNT
  (lambda ()
    (do ((i 0 (+ i 1))) ((= i #x100))
      (let* ((w (random-integer #x1000))
             (n (random-integer-of-weight w #x1000))
             (c (bit-count n)))
        (if (not (eqv? c w))
            (error "Failed:" `(BIT-COUNT ,n) '=> c 'EXPECTED w))))))

(define-test 'BIT-COUNT/COMPLEMENT
  (lambda ()
    (do ((i 0 (+ i 1))) ((= i #x100))
      (let* ((w (random-integer #x1000))
             (n (random-integer-of-weight w #x1000))
             (c (bit-count (bitwise-not n))))
        (if (not (eqv? c w))
            (error "Failed:" `(BIT-COUNT (BITWISE-NOT ,n))
                   '=> c
                   'EXPECTED w))))))

(define-random-unary-integer-test 'INTEGER-LENGTH
  (let ((integer-length-in-bits
         (make-primitive-procedure 'INTEGER-LENGTH-IN-BITS 1)))
    (lambda (n)
      (assert-eqv (integer-length n)
                  (integer-length-in-bits
                   (if (negative? n) (bitwise-not n) n))))))

(define-test 'FIRST-SET-BIT:0 (lambda () (assert-eqv (first-set-bit 0) -1)))
(define-test 'FIRST-SET-BIT:+1 (lambda () (assert-eqv (first-set-bit +1) 0)))
(define-test 'FIRST-SET-BIT:-1 (lambda () (assert-eqv (first-set-bit -1) 0)))

(define-random-unary-integer-test 'FIRST-SET-BIT:ODD
  (lambda (n)
    (if (not (zero? n))
        (let ((i (random-integer #x1000)))
          (assert-eqv (first-set-bit (shift-left (bitwise-ior n 1) i)) i)))))

(define-random-unary-integer-test 'FIRST-SET-BIT:RANDOM
  (lambda (n)
    (if (not (zero? n))
        (let ((i (random-integer #x1000)))
          (assert-eqv (first-set-bit (shift-left n i))
                      (+ i (first-set-bit n)))))))

((lambda (procedure)
   (for-each (lambda (entry) (procedure (car entry) (cadr entry) (cddr entry)))
             '((0 0 . 0)
               (0 -1 . -1)
               (-1 0 . -1)
               (-1 -1 . 0))))
 (lambda (a b a*b)
   (define-test (symbol 'HAMMING-DISTANCE ': a ': b)
     (lambda ()
       (assert-eqv (hamming-distance a b) a*b)))))

(define-random-binary-integer-test 'HAMMING-DISTANCE
  (lambda (a b)
    (if (not (eqv? (hamming-distance a b)
                   (if (eqv? (negative? a) (negative? b))
                       (bit-count (bitwise-xor a b))
                       -1)))
        (error "Failed:" `(HAMMING-DISTANCE ,a ,b)
               '=> (hamming-distance a b)))))

(define-test 'BIT-MASK
  (lambda ()
    (do ((i 0 (+ i 1))) ((>= i #x1000))
      (let ((size (random-integer #x1000))
            (position (random-integer #x1000)))
        (assert-eqv (bit-mask size position)
                    (shift-left
                     (bitwise-not (shift-left -1 size))
                     position))))))

(define-test 'BIT-ANTIMASK
  (lambda ()
    (do ((i 0 (+ i 1))) ((>= i #x1000))
      (let ((size (random-integer #x1000))
            (position (random-integer #x1000)))
        (assert-eqv (bit-antimask size position)
                    (bitwise-not
                     (shift-left (bitwise-not (shift-left -1 size))
                                 position)))))))

(define (define-per-bit-test name procedure)
  (define-test name
    (lambda ()
      (do ((i 0 (+ i 1))) ((>= i #x100))
        (procedure (random-integer-of-weight (random-integer #x1000) #x1000)
                   (random-integer #x1000))))))

(define-per-bit-test 'SET-BIT
  (lambda (n i) (assert-true (bit-set? i (set-bit i n)))))

(define-per-bit-test 'CLEAR-BIT
  (lambda (n i) (assert-true (bit-clear? i (clear-bit i n)))))

(define-per-bit-test 'TOGGLE-BIT
  (lambda (n i) (assert-eqv (bit-clear? i (toggle-bit i n)) (bit-set? i n))))

(define-per-bit-test 'EXTRACT-BIT
  (lambda (n i) (assert-eqv (zero? (extract-bit i n)) (bit-clear? i n))))
