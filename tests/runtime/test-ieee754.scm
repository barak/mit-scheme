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

;;;; Test of IEEE 754 utilities

(declare (usual-integrations))

(define (define-enumerated-test name elements procedure)
  (define-test name
    (map (lambda (element)
           (lambda () (procedure element)))
         elements)))

(define ((test-ieee754-roundtrip w t bexp-inf/nan compose exact? decompose)
         bits)
  (let ((sign (extract-bit-field 1 (+ w t) bits))
        (biased-exponent (extract-bit-field w t bits))
        (trailing-significand (extract-bit-field t 0 bits)))
    (if (not (= biased-exponent bexp-inf/nan))
        (let ((x (compose sign biased-exponent trailing-significand)))
          (assert (exact? x))
          (receive (sign* biased-exponent* trailing-significand*)
                   (decompose x)
            (assert-= sign* sign)
            (assert-= biased-exponent* biased-exponent)
            (assert-= trailing-significand* trailing-significand))))))

(define-test 'binary32-roundtrip-exhaustive
  (lambda ()
    (define test
      (test-ieee754-roundtrip 8 23 255
                              compose-ieee754-binary32
                              ieee754-binary32-exact?
                              decompose-ieee754-binary32))
    (let ((increment (if keep-it-fast!? 347911 1))
          (maximum (expt 2 32)))
      (do ((i 0 (+ i increment)))
          ((>= i maximum))
        (test i)))))

(define-enumerated-test 'binary64-roundtrip-selective
  '(#x0000000000000000
    #xffffffffffffffff
    #x0000000000000001
    #x1000000000000000
    #x1000000000000001
    #x7ff0000000000000
    #xfff0000000000000
    #x0123456789abcdef
    #xfedcba9876543210)
  (test-ieee754-roundtrip 11 52 2047
                          compose-ieee754-binary64
                          ieee754-binary64-exact?
                          decompose-ieee754-binary64))

(define-enumerated-test 'decompose-ieee754-binary64
  `((0. (zero +))
    (-0. (zero -))
    (,(expt 2 -1074) (subnormal + 1))
    (,(- (expt 2 -1074)) (subnormal - 1))
    (,(expt 2 -1050) (subnormal + #x1000000))
    (,(- (expt 2 -1050)) (subnormal - #x1000000))
    (,(- (expt 2 -1022) (expt 2 -1074)) (subnormal + #xfffffffffffff))
    (,(- (expt 2 -1074) (expt 2 -1022)) (subnormal - #xfffffffffffff))
    (,(expt 2 -1022) (normal + -1022 #x10000000000000))
    (,(- (expt 2 -1022)) (normal - -1022 #x10000000000000))
    (,(+ (expt 2 -1022) (expt 2 -1074)) (normal + -1022 #x10000000000001))
    (,(- (+ (expt 2 -1022) (expt 2 -1074))) (normal - -1022 #x10000000000001))
    (1 (normal + 0 #x10000000000000))
    (-1 (normal - 0 #x10000000000000))
    (3/2 (normal + 0 #x18000000000000))
    (-3/2 (normal - 0 #x18000000000000))
    (2 (normal + 1 #x10000000000000))
    (-2 (normal - 1 #x10000000000000))
    (,(flo:+inf.0) (infinity +))
    (,(flo:-inf.0) (infinity -))
    (,(flo:nan.0) (nan + s 1)))
  (lambda (c)
    (let ((x (list-ref c 0))
          (y (list-ref c 1)))
      (define (signify sign)
        (case sign
          ((0) '+)
          ((1) '-)
          (else (error "Invalid sign:" sign))))
      (flo:with-trapped-exceptions 0
        (lambda ()
          ((lambda (z)
             (assert-equal z y)
             (flo:clear-exceptions! (flo:supported-exceptions)))
           (let ((exponent-bits 11)
                 (precision 53))
             (receive (base emin emax bias exp-subnormal exp-inf/nan)
                      (ieee754-binary-parameters exponent-bits precision)
               emin bias exp-subnormal exp-inf/nan ;ignore
               (decompose-ieee754 x base emax precision
                 (lambda (sign) `(zero ,(signify sign)))
                 (lambda (sign significand)
                   `(subnormal ,(signify sign) ,significand))
                 (lambda (sign exponent significand)
                   `(normal ,(signify sign) ,exponent ,significand))
                 (lambda (sign)
                   `(infinity ,(signify sign)))
                 (lambda (sign quiet payload)
                   `(nan ,(signify sign)
                         ,(case quiet
                            ((0) 's)
                            ((1) 'q)
                            (else (error "Quiet bit:" quiet)))
                         ,payload)))))))))))

(define-enumerated-test 'ieee754-binary64-hex
  '((0 "0x0p+0")
    (-0. "-0x0p+0")
    (1 "0x1p+0")
    (-1 "-0x1p+0")
    (1/2 "0x1p-1")
    (-1/2 "-0x1p-1")
    (12345 "0x1.81c8p+13")
    (123456 "0x1.e24p+16")
    (1.2061684984132626e-11 "0x1.a862p-37"))
  (lambda (c)
    (let ((x (list-ref c 0))
          (s (list-ref c 1)))
      (assert-string= (ieee754-binary64-hex-string x) s))))
