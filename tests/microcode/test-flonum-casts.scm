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

(define-test 'test-casting
  (lambda ()

    (define cast-flonum-to-integer
      (make-primitive-procedure 'cast-flonum-to-integer))

    (define cast-integer-to-flonum
      (make-primitive-procedure 'cast-integer-to-flonum))

    (define (itof integer-as-bit-string)
       (cast-integer-to-flonum
	(bit-string->unsigned-integer integer-as-bit-string)))

    (define (factorial n)
      (if (< n 2)
	  1
	  (* n (factorial (- n 1)))))

    (define (test flonum integer-as-bit-string)
      (assert-equal
       (unsigned-integer->bit-string 64 (cast-flonum-to-integer flonum))
       integer-as-bit-string)
      (assert-equal
       flonum
       (itof integer-as-bit-string)))

    (test 0.0
	  #*0000000000000000000000000000000000000000000000000000000000000000)
    (test -0.0
	  #*1000000000000000000000000000000000000000000000000000000000000000)
    (test 1.0
	  #*0011111111110000000000000000000000000000000000000000000000000000)
    (test 2.0
	  #*0100000000000000000000000000000000000000000000000000000000000000)
    (test 4.0
	  #*0100000000010000000000000000000000000000000000000000000000000000)
    (test 8.0
	  #*0100000000100000000000000000000000000000000000000000000000000000)
    (test (->flonum (factorial 100))
	  #*0110000010111011001100001001011001001110110000111001010111011100)
    (test -1.0
	  #*1011111111110000000000000000000000000000000000000000000000000000)

    (assert-true
     (flo:nan?
      (itof
       #*0111111111110000000000000000000000000000000000000000000000000001)))
    (assert-true
     (flo:nan?
      (itof
       #*0111111111111111111111111111111111111111111111111111111111111111)))))