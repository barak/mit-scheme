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

;;;; Test of fixnum operations

(declare (usual-integrations))

(define (call procedure)
  (procedure))

;; Reduce the size of the object a little by collecting all the test
;; lambdas into a single top-level lambda for compilation.

(define-syntax define-a-test
  (syntax-rules ()
    ((DEFINE-A-TEST name cases)
     (CALL (LAMBDA () (DEFINE-TEST name cases))))))

(define-integrable (fix:negate x)
  (fix:- 0 x))

(define-integrable (fix:noop-not x)
  (fix:not (identity-procedure (fix:not x))))

(define-integrable (fix:noop-neg x)
  (fix:negate (identity-procedure (fix:negate x))))

(define-integrable (fix:noop-neg+ x)
  (fix:- 123 (identity-procedure (fix:- 123 x))))

(define-integrable (fix:noop-neg- x)
  ;; Just something different from -1 - x, since that's NOT.
  (fix:- -123 (identity-procedure (fix:- -123 x))))

(define-integrable (fix:id x)
  x)

(define-integrable (fix:and-via-not/or/not x y)
  (fix:not (fix:or (fix:not x) (fix:not y))))

(define-integrable (fix:or-via-not/and/not x y)
  (fix:not (fix:and (fix:not x) (fix:not y))))

(define-integrable (fix:and-via-andc/not x y)
  (fix:andc x (fix:not y)))

(define-integrable (fix:andc-via-and/not x y)
  (fix:and x (fix:not y)))

(define-syntax unary-cases
  (syntax-rules ()
    ((UNARY-CASES f x noop)
     (LIST (LAMBDA () (f x))
           (LAMBDA () (noop (f x)))
           (LAMBDA () (f (IDENTITY-PROCEDURE x)))
           (LAMBDA () (noop (f (IDENTITY-PROCEDURE x))))
           (LAMBDA () (f (noop x)))
           (LAMBDA () (noop (f (noop x))))
           (LAMBDA () (f (noop (IDENTITY-PROCEDURE x))))
           (LAMBDA () (noop (f (noop (IDENTITY-PROCEDURE x)))))))))

(define-syntax binary-cases
  (syntax-rules ()
    ((BINARY-CASES f x y noop)
     (LIST
      ;; Two constant arguments.
      (LAMBDA () (f x y))
      ;; Two unknown arguments.
      (LAMBDA () (f (IDENTITY-PROCEDURE x) (IDENTITY-PROCEDURE y)))
      ;; Two noop-constant arguments.
      (LAMBDA () (f (noop x) (noop y)))
      ;; Two noop-unknown arguments.
      (LAMBDA ()
        (f (noop (IDENTITY-PROCEDURE x)) (noop (IDENTITY-PROCEDURE y))))
      ;; One unknown argument, one constant argument.
      (LAMBDA () (f (IDENTITY-PROCEDURE x) y))
      (LAMBDA () (f x (IDENTITY-PROCEDURE y)))
      ;; One noop-constant argument, one constant argument.
      (LAMBDA () (f (noop x) y))
      (LAMBDA () (f x (noop y)))
      ;; One noop-unknown argument, one constant argument.
      (LAMBDA () (f (noop (IDENTITY-PROCEDURE x)) y))
      (LAMBDA () (f x (noop (IDENTITY-PROCEDURE y))))
      ;; One noop-constant argument, one unknown argument.
      (LAMBDA () (f (noop x) (IDENTITY-PROCEDURE y)))
      (LAMBDA () (f (IDENTITY-PROCEDURE x) (noop y)))
      ;; One noop-constant argument, one noop-unknown argument.
      (LAMBDA () (f (noop x) (noop (IDENTITY-PROCEDURE y))))
      (LAMBDA () (f (noop (IDENTITY-PROCEDURE x)) (noop y)))
      ;; All of the above, with a noop afterward.
      (LAMBDA () (noop (f x y)))
      (LAMBDA () (noop (f (IDENTITY-PROCEDURE x) (IDENTITY-PROCEDURE y))))
      (LAMBDA () (noop (f (noop x) (noop y))))
      (LAMBDA ()
        (noop (f (noop (IDENTITY-PROCEDURE x)) (noop (IDENTITY-PROCEDURE y)))))
      (LAMBDA () (noop (f (IDENTITY-PROCEDURE x) y)))
      (LAMBDA () (noop (f x (IDENTITY-PROCEDURE y))))
      (LAMBDA () (noop (f (noop x) y)))
      (LAMBDA () (noop (f x (noop y))))
      (LAMBDA () (noop (f (noop (IDENTITY-PROCEDURE x)) y)))
      (LAMBDA () (noop (f x (noop (IDENTITY-PROCEDURE y)))))
      (LAMBDA () (noop (f (noop x) (IDENTITY-PROCEDURE y))))
      (LAMBDA () (noop (f (IDENTITY-PROCEDURE x) (noop y))))
      (LAMBDA () (noop (f (noop x) (noop (IDENTITY-PROCEDURE y)))))
      (LAMBDA () (noop (f (noop (IDENTITY-PROCEDURE x)) (noop y))))))))

(define-syntax define-unary-test
  (syntax-rules ()
    ((DEFINE-UNARY-TEST f noop (x y) ...)
     (BEGIN
       (DEFINE-A-TEST (SYMBOL 'f '/ x)
         (MAP (LAMBDA (P)
                (LAMBDA ()
                  (ASSERT-EQV (P) y)))
              (UNARY-CASES f x noop)))
       ...))))

(define-syntax define-binary-test
  (syntax-rules ()
    ((DEFINE-BINARY-TEST f noop (x y z) ...)
     (BEGIN
       (DEFINE-A-TEST (SYMBOL 'f '/ x '/ y)
         (MAP (LAMBDA (P)
                (LAMBDA ()
                  (ASSERT-EQV (P) z)))
              (BINARY-CASES f x y noop)))
       ...))))

(define-syntax define-binary-left-identity-test
  (syntax-rules ()
    ((DEFINE-BINARY-LEFT-IDENTITY-TEST f noop id x ...)
     (DEFINE-BINARY-TEST f noop (id x x) ...))))

(define-syntax define-binary-right-identity-test
  (syntax-rules ()
    ((DEFINE-BINARY-RIGHT-IDENTITY-TEST f noop id x ...)
     (DEFINE-BINARY-TEST f noop (x id x) ...))))

(define-syntax define-binary-equivalent-test
  (syntax-rules ()
    ((DEFINE-BINARY-EQUIVALENT-TEST f g noop (x y) ...)
     (BEGIN
       (DEFINE-BINARY-TEST f noop (x y (g x y)) ...)
       (DEFINE-BINARY-TEST f noop (y x (g y x)) ...)))))

(define-syntax define-commutative-binary-test
  (syntax-rules ()
    ((DEFINE-COMMUTATIVE-BINARY-TEST f noop (x y z) ...)
     (BEGIN
       (DEFINE-BINARY-TEST f noop (x y z) ...)
       (DEFINE-BINARY-TEST f noop (y x z) ...)))))

(define-syntax define-commutative-binary-equivalent-test
  (syntax-rules ()
    ((DEFINE-COMMUTATIVE-BINARY-EQUIVALENT-TEST f g noop (x y) ...)
     (DEFINE-COMMUTATIVE-BINARY-TEST f noop (x y (g x y)) ...))))

(define-syntax define-commutative-binary-inverse-test
  (syntax-rules ()
    ((DEFINE-COMMUTATIVE-BINARY-INVERSE-TEST f f- inv id noop (x y) ...)
     (BEGIN
       (DEFINE-COMMUTATIVE-BINARY-TEST f noop (x y id) ...)
       (DEFINE-BINARY-TEST f- noop (x (inv y) id) ...)
       (DEFINE-BINARY-TEST f- noop ((inv x) y id) ...)))))

(define-syntax define-commutative-binary-identity-test
  (syntax-rules ()
    ((DEFINE-COMMUTATIVE-BINARY-IDENTITY-TEST f noop id x ...)
     (DEFINE-COMMUTATIVE-BINARY-TEST f noop (x id x) ...))))

(define-syntax define-commutative-binary-inv/id-test
  (syntax-rules ()
    ((DEFINE-COMMUTATIVE-BINARY-INV/ID-TEST f f- inv id noop x ...)
     (BEGIN
       (DEFINE-COMMUTATIVE-BINARY-IDENTITY-TEST f noop id x ...)
       (DEFINE-BINARY-TEST f- noop (x id x) ...)
       (DEFINE-BINARY-TEST f- noop (id x (inv x)) ...)))))

(define-integrable (smallest-negatable-fixnum)
  (fix:+ 1 (smallest-fixnum)))

(define-integrable (smallest-negatable-fixnum-negation)
  (largest-fixnum))

(define-integrable (largest-negatable-fixnum)
  (largest-fixnum))

(define-integrable (largest-negatable-fixnum-negation)
  (fix:+ 1 (smallest-fixnum)))

(define-unary-test fix:negate fix:noop-not
  (0 0) (-1 1) (-2 2) (-3 3) (-4 4) (-5 5) (-6 6) (-7 7) (-8 8) (-9 9)
  (126 -126) (127 -127) (128 -128) (129 -129) (130 -130)
  (254 -254) (255 -255) (256 -256) (257 -257) (258 -258)
  (32766 -32766) (32767 -32767) (32768 -32768) (32769 -32769) (32770 -32770)
  ((smallest-negatable-fixnum) (smallest-negatable-fixnum-negation))
  ((largest-negatable-fixnum) (largest-negatable-fixnum-negation)))

(define-unary-test fix:not fix:noop-neg
  (-1 0) (-2 1) (-3 2) (-4 3) (-5 4) (-6 5) (-7 6) (-8 7) (-9 8) (-10 9)
  (126 -127) (127 -128) (128 -129) (129 -130) (130 -131)
  (254 -255) (255 -256) (256 -257) (257 -258) (258 -259)
  (32766 -32767) (32767 -32768) (32768 -32769) (32769 -32770) (32770 -32771)
  ((smallest-fixnum) (largest-fixnum)))

(define-commutative-binary-inverse-test fix:+ fix:- fix:negate 0 fix:noop-not
  (0 0) (-1 1) (-2 2) (-3 3) (-4 4) (-5 5) (-6 6) (-7 7) (-8 8) (-9 9)
  (126 -126) (127 -127) (128 -128) (129 -129) (130 -130)
  (254 -254) (255 -255) (256 -256) (257 -257) (258 -258)
  (32766 -32766) (32767 -32767) (32768 -32768) (32769 -32769) (32770 -32770)
  ((smallest-negatable-fixnum) (smallest-negatable-fixnum-negation))
  ((largest-negatable-fixnum) (largest-negatable-fixnum-negation)))

(define-commutative-binary-inv/id-test fix:+ fix:- fix:negate 0 fix:noop-not
  -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9
  -126 -127 -128 -129 -130
  126 127 128 129 130
  -254 -255 -256 -257 -258
  254 255 256 257 258
  -32766 -32767 -32768 -32769 -32770
  32766 32767 32768 32769 32770
  (smallest-negatable-fixnum)
  (largest-negatable-fixnum))

;; Can't use inv/id-test for this because we can't necessarily negate.
(define-commutative-binary-identity-test fix:+ fix:noop-not 0
  (smallest-fixnum)
  (largest-fixnum))

(define-commutative-binary-identity-test fix:and fix:noop-neg -1
  -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9
  -126 -127 -128 -129 -130
  126 127 128 129 130
  -254 -255 -256 -257 -258
  254 255 256 257 258
  -32766 -32767 -32768 -32769 -32770
  32766 32767 32768 32769 32770
  (smallest-negatable-fixnum)
  (largest-negatable-fixnum))

(define-commutative-binary-identity-test fix:and fix:noop-neg+ -1
  (largest-negatable-fixnum))

(define-commutative-binary-identity-test fix:and fix:noop-neg- -1
  (smallest-negatable-fixnum))

(define-commutative-binary-identity-test fix:or fix:noop-neg 0
  -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9
  -126 -127 -128 -129 -130
  126 127 128 129 130
  -254 -255 -256 -257 -258
  254 255 256 257 258
  -32766 -32767 -32768 -32769 -32770
  32766 32767 32768 32769 32770
  (smallest-negatable-fixnum)
  (largest-negatable-fixnum))

(define-commutative-binary-identity-test fix:or fix:noop-neg+ 0
  (largest-negatable-fixnum))

(define-commutative-binary-identity-test fix:or fix:noop-neg- 0
  (smallest-negatable-fixnum))

(define-commutative-binary-identity-test fix:xor fix:noop-neg 0
  -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9
  -126 -127 -128 -129 -130
  126 127 128 129 130
  -254 -255 -256 -257 -258
  254 255 256 257 258
  -32766 -32767 -32768 -32769 -32770
  32766 32767 32768 32769 32770
  (smallest-negatable-fixnum)
  (largest-negatable-fixnum))

(define-commutative-binary-identity-test fix:xor fix:noop-neg+ 0
  (largest-negatable-fixnum))

(define-commutative-binary-identity-test fix:xor fix:noop-neg- 0
  (smallest-negatable-fixnum))

(define-commutative-binary-inv/id-test fix:xor fix:xor fix:id 0 fix:noop-neg
  -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9
  -126 -127 -128 -129 -130
  126 127 128 129 130
  -254 -255 -256 -257 -258
  254 255 256 257 258
  -32766 -32767 -32768 -32769 -32770
  32766 32767 32768 32769 32770
  (smallest-negatable-fixnum)
  (largest-negatable-fixnum))

(define-commutative-binary-identity-test fix:xor fix:noop-neg+ 0
  (largest-fixnum))

(define-commutative-binary-identity-test fix:xor fix:noop-neg- 0
  (smallest-fixnum))

(define-binary-right-identity-test fix:andc fix:noop-neg 0
  -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9
  -126 -127 -128 -129 -130
  126 127 128 129 130
  -254 -255 -256 -257 -258
  254 255 256 257 258
  -32766 -32767 -32768 -32769 -32770
  32766 32767 32768 32769 32770
  (smallest-negatable-fixnum)
  (largest-negatable-fixnum))

(define-binary-right-identity-test fix:andc fix:noop-neg+ 0
  (largest-fixnum))

(define-binary-right-identity-test fix:andc fix:noop-neg- 0
  (smallest-fixnum))

(define-commutative-binary-equivalent-test fix:and fix:and-via-not/or/not
  fix:noop-neg
  (-128 -128) (-128 -127) (-128 -2) (-128 -1)
  (-128 0) (-128 1) (-128 2) (-128 127) (-128 128)
  (-127 -127) (-127 -2) (-127 -1)
  (-127 0) (-127 1) (-127 2) (-127 127) (-127 128)
  (-2 -2) (-2 -1)
  (-2 0) (-2 1) (-2 2) (-2 127) (-2 128)
  (-1 -1)
  (-1 0) (-1 1) (-1 2) (-1 127) (-1 128)
  (0 0) (0 1) (0 2) (0 127) (0 128)
  (1 1) (1 2) (1 127) (1 128)
  (2 2) (2 127) (2 128)
  (127 127) (127 128)
  (128 128))

(define-commutative-binary-equivalent-test fix:or fix:or-via-not/and/not
  fix:noop-neg
  (-128 -128) (-128 -127) (-128 -2) (-128 -1)
  (-128 0) (-128 1) (-128 2) (-128 127) (-128 128)
  (-127 -127) (-127 -2) (-127 -1)
  (-127 0) (-127 1) (-127 2) (-127 127) (-127 128)
  (-2 -2) (-2 -1)
  (-2 0) (-2 1) (-2 2) (-2 127) (-2 128)
  (-1 -1)
  (-1 0) (-1 1) (-1 2) (-1 127) (-1 128)
  (0 0) (0 1) (0 2) (0 127) (0 128)
  (1 1) (1 2) (1 127) (1 128)
  (2 2) (2 127) (2 128)
  (127 127) (127 128)
  (128 128))

(define-binary-equivalent-test fix:andc fix:andc-via-and/not fix:noop-neg
  (-128 -128) (-128 -127) (-128 -2) (-128 -1)
  (-128 0) (-128 1) (-128 2) (-128 127) (-128 128)
  (-127 -127) (-127 -2) (-127 -1)
  (-127 0) (-127 1) (-127 2) (-127 127) (-127 128)
  (-2 -2) (-2 -1)
  (-2 0) (-2 1) (-2 2) (-2 127) (-2 128)
  (-1 -1)
  (-1 0) (-1 1) (-1 2) (-1 127) (-1 128)
  (0 0) (0 1) (0 2) (0 127) (0 128)
  (1 1) (1 2) (1 127) (1 128)
  (2 2) (2 127) (2 128)
  (127 127) (127 128)
  (128 128))

(define-commutative-binary-equivalent-test fix:and fix:and-via-andc/not
  fix:noop-neg
  (-128 -128) (-128 -127) (-128 -2) (-128 -1)
  (-128 0) (-128 1) (-128 2) (-128 127) (-128 128)
  (-127 -127) (-127 -2) (-127 -1)
  (-127 0) (-127 1) (-127 2) (-127 127) (-127 128)
  (-2 -2) (-2 -1)
  (-2 0) (-2 1) (-2 2) (-2 127) (-2 128)
  (-1 -1)
  (-1 0) (-1 1) (-1 2) (-1 127) (-1 128)
  (0 0) (0 1) (0 2) (0 127) (0 128)
  (1 1) (1 2) (1 127) (1 128)
  (2 2) (2 127) (2 128)
  (127 127) (127 128)
  (128 128))