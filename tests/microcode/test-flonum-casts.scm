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

;;;; Tests of flonum casting

(declare (usual-integrations))

(define (factorial n)
  (if (< n 2)
      1
      (* n (factorial (- n 1)))))

(define ((make-cast-tester cast-to-integer cast-to-flonum size-in-bits)
	 flonum
	 integer-as-bit-string)
  (assert-equal
   (unsigned-integer->bit-string size-in-bits (cast-to-integer flonum))
   integer-as-bit-string)
  (assert-equal
   flonum
   (cast-to-flonum integer-as-bit-string)))

(define-test 'test-casting-doubles
  (lambda ()
    (define cast-ieee754-double-to-integer
      (make-primitive-procedure 'cast-ieee754-double-to-integer))

    (define cast-integer-to-ieee754-double
      (make-primitive-procedure 'cast-integer-to-ieee754-double))

    (define (integer-to-double integer-as-bit-string)
      (cast-integer-to-ieee754-double
       (bit-string->unsigned-integer integer-as-bit-string)))

    (define test-double
      (make-cast-tester cast-ieee754-double-to-integer
			integer-to-double
			64))

    (test-double
     0.0
     #*0000000000000000000000000000000000000000000000000000000000000000)
    (test-double
     -0.0
     #*1000000000000000000000000000000000000000000000000000000000000000)
    (test-double
     1.0
     #*0011111111110000000000000000000000000000000000000000000000000000)
    (test-double
     2.0
     #*0100000000000000000000000000000000000000000000000000000000000000)
    (test-double
     4.0
     #*0100000000010000000000000000000000000000000000000000000000000000)
    (test-double
     8.0
     #*0100000000100000000000000000000000000000000000000000000000000000)
    (test-double
     (->flonum (factorial 100))
     #*0110000010111011001100001001011001001110110000111001010111011100)
    (test-double
     -1.0
     #*1011111111110000000000000000000000000000000000000000000000000000)

    ;; We should a procedure that tests for floating-point infinity
    ;; and use it here.
    (let ((positive-infinity
	   (integer-to-double
	    #*0111111111110000000000000000000000000000000000000000000000000000)))
      (assert-true (flo:positive? positive-infinity)))
    (let ((negative-infinity
	   (integer-to-double
	    #*1111111111110000000000000000000000000000000000000000000000000000)))
      (assert-true (flo:negative? negative-infinity)))

    (assert-true
     (flo:nan?
      (integer-to-double
       #*0111111111110000000000000000000000000000000000000000000000000001)))
    (assert-true
     (flo:nan?
      (integer-to-double
       #*0111111111111111111111111111111111111111111111111111111111111111)))))

(define-test 'test-casting-singles
  (lambda ()
    (define cast-ieee754-single-to-integer
      (make-primitive-procedure 'cast-ieee754-single-to-integer))

    (define cast-integer-to-ieee754-single
      (make-primitive-procedure 'cast-integer-to-ieee754-single))

    (define (integer-to-single integer-as-bit-string)
      (cast-integer-to-ieee754-single
       (bit-string->unsigned-integer integer-as-bit-string)))

    (define test-single
      (make-cast-tester cast-ieee754-single-to-integer
			integer-to-single
			32))

    (test-single 0.0
		 #*00000000000000000000000000000000)
    (test-single -0.0
		 #*10000000000000000000000000000000)
    (test-single 1.0
		 #*00111111100000000000000000000000)
    (test-single 2.0
		 #*01000000000000000000000000000000)
    (test-single 4.0
		 #*01000000100000000000000000000000)
    (test-single 8.0
		 #*01000001000000000000000000000000)
    (test-single (->flonum (factorial 10))
		 #*01001010010111010111110000000000)
    (test-single -1.0
		 #*10111111100000000000000000000000)

    ;; We should a procedure that tests for floating-point infinity
    ;; and use it here.
    (let ((positive-infinity
	   (integer-to-single #*01111111100000000000000000000000)))
      (assert-true (flo:positive? positive-infinity)))
    (let ((negative-infinity
	   (integer-to-single #*11111111100000000000000000000000)))
      (assert-true (flo:negative? negative-infinity)))

    (assert-true
     (flo:nan?
      (integer-to-single
       #*01111111100000000000000000000001)))
    (assert-true
     (flo:nan?
      (integer-to-single
       #*01111111111111111111111111111111)))))