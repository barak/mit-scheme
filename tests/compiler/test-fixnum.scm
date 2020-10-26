#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020 Massachusetts Institute of Technology

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

(define (prelude)
  `(begin

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
       (fix:and x (fix:not y)))))

(define (map-cases cases f)
  (map (lambda (c)
	 (apply f c))
       cases))

(define (trunc L)
  (if keep-it-fast!?
      (append (take L (min (length L) 2)) (take-right L (min (length L) 2)))
      L))

(define (unary-cases f x noop)
  `((,f ,x)
    (,noop (,f ,x))
    (,f (identity-procedure ,x))
    (,noop (,f (identity-procedure ,x)))
    (,f (,noop ,x))
    (,noop (,f (,noop ,x)))
    (,f (,noop (identity-procedure ,x)))
    (,noop (,f (,noop (identity-procedure ,x))))))

(define (binary-cases f x y noop)
  `(;; Two constant arguments.
    (,f ,x ,y)
    ;; Two unknown arguments.
    (,f (identity-procedure ,x) (identity-procedure ,y))
    ;; Two noop-constant arguments.
    (,f (,noop ,x) (,noop ,y))
    ;; Two noop-unknown arguments.
    (,f (,noop (identity-procedure ,x)) (,noop (identity-procedure ,y)))
    ;; One unknown argument, one constant argument.
    (,f (identity-procedure ,x) ,y)
    (,f ,x (identity-procedure ,y))
    ;; One noop-constant argument, one constant argument.
    (,f (,noop ,x) ,y)
    (,f ,x (,noop ,y))
    ;; One noop-unknown argument, one constant argument.
    (,f (,noop (identity-procedure ,x)) ,y)
    (,f ,x (,noop (identity-procedure ,y)))
    ;; One noop-constant argument, one unknown argument.
    (,f (,noop ,x) (identity-procedure ,y))
    (,f (identity-procedure ,x) (,noop ,y))
    ;; One noop-constant argument, one noop-unknown argument.
    (,f (,noop ,x) (,noop (identity-procedure ,y)))
    (,f (,noop (identity-procedure ,x)) (,noop ,y))
    ;; All of the above, with a noop afterward.
    (,noop (,f ,x ,y))
    (,noop (,f (identity-procedure ,x) (identity-procedure ,y)))
    (,noop (,f (,noop ,x) (,noop ,y)))
    (,noop
     (,f (,noop (identity-procedure ,x)) (,noop (identity-procedure ,y))))
    (,noop (,f (identity-procedure ,x) ,y))
    (,noop (,f ,x (identity-procedure ,y)))
    (,noop (,f (,noop ,x) ,y))
    (,noop (,f ,x (,noop ,y)))
    (,noop (,f (,noop (identity-procedure ,x)) ,y))
    (,noop (,f ,x (,noop (identity-procedure ,y))))
    (,noop (,f (,noop ,x) (identity-procedure ,y)))
    (,noop (,f (identity-procedure ,x) (,noop ,y)))
    (,noop (,f (,noop ,x) (,noop (identity-procedure ,y))))
    (,noop (,f (,noop (identity-procedure ,x)) (,noop ,y)))))

(define (eval-compiled expression)
  (with-test-properties
      (lambda ()
	(let* ((env (make-top-level-environment))
	       (program `(begin ,(prelude) ,expression))
	       (scode (syntax&integrate program '((usual-integrations)) env))
	       (compiled (compile-scode scode)))
	  (eval compiled env)))
    'expression expression))

(define (eval-compiled* expressions)
  (map (lambda (procedure) (procedure))
       (eval-compiled
	`(list
	  ,@(map (lambda (expression)
		   `(lambda () ,expression))
		 expressions)))))

(define (define-unary-test f noop cases)
  (for-each (lambda (xy)
	      (let ((x (car xy))
		    (y (cadr xy)))
		(define-test (symbol f '/ x)
		  (lambda ()
		    (let* ((expressions (trunc (unary-cases f x noop)))
			   (results (eval-compiled* expressions))
			   (expected (eval-compiled y)))
		      (for-each (lambda (expression result)
				  (assert-eqv result expected
					      'expression expression))
				expressions results))))))
	    (trunc cases)))

(define (define-binary-test f noop cases)
  (for-each (lambda (xyz)
	      (let ((x (car xyz))
		    (y (cadr xyz))
		    (z (caddr xyz)))
		(define-test (symbol f '/ x '/ y)
		  (lambda ()
		    (let* ((expressions (trunc (binary-cases f x y noop)))
			   (results (eval-compiled* expressions))
			   (expected (eval-compiled z)))
		      (for-each (lambda (expression result)
				  (assert-eqv result expected
					      'expression expression))
				expressions results))))))
	    (trunc cases)))

(define (define-binary-left-identity-test f noop id xs)
  (define-binary-test f noop (map (lambda (x) (list id x x)) xs)))

(define (define-binary-right-identity-test f noop id xs)
  (define-binary-test f noop (map (lambda (x) (list x id x)) xs)))

(define (define-binary-equivalent-test f g noop cases)
  (define-binary-test f noop
    (append (map-cases cases (lambda (x y) (list x y `(,g ,x ,y))))
	    (map-cases cases (lambda (x y) (list y x `(,g ,y ,x)))))))

(define (define-commutative-binary-test f noop cases)
  (define-binary-test f noop
    (append cases
	    (map-cases cases (lambda (x y z) (list y x z))))))

(define (define-commutative-binary-equivalent-test f g noop cases)
  (define-commutative-binary-test f noop
    (map-cases cases (lambda (x y) (list x y `(,g ,x ,y))))))

(define (define-commutative-binary-inverse-test f f- inv id noop cases)
  (define-commutative-binary-test f noop
    (map-cases cases (lambda (x y) (list x y id))))
  (define-binary-test f- noop
    (append (map-cases cases (lambda (x y) (list x `(,inv ,y) id)))
	    (map-cases cases (lambda (x y) (list `(,inv ,x) y id))))))

(define (define-commutative-binary-identity-test f noop id xs)
  (define-commutative-binary-test f noop (map (lambda (x) (list x id x)) xs)))

(define (define-commutative-binary-inv/id-test f f- inv id noop xs)
  (define-commutative-binary-identity-test f noop id xs)
  (define-binary-test f- noop
    (append (map (lambda (x) (list x id x)) xs)
	    (map (lambda (x) (list id x `(,inv ,x))) xs))))

(define (smallest-negatable-fixnum)
  (fix:+ 1 (smallest-fixnum)))

(define (smallest-negatable-fixnum-negation)
  (largest-fixnum))

(define (largest-negatable-fixnum)
  (largest-fixnum))

(define (largest-negatable-fixnum-negation)
  (fix:+ 1 (smallest-fixnum)))

(define-unary-test 'fix:negate 'fix:noop-not
  `((0 0) (-1 1) (-2 2) (-3 3) (-4 4) (-5 5) (-6 6) (-7 7) (-8 8) (-9 9)
    (126 -126) (127 -127) (128 -128) (129 -129) (130 -130)
    (254 -254) (255 -255) (256 -256) (257 -257) (258 -258)
    (32766 -32766) (32767 -32767) (32768 -32768) (32769 -32769) (32770 -32770)
    (,(smallest-negatable-fixnum) ,(smallest-negatable-fixnum-negation))
    (,(largest-negatable-fixnum) ,(largest-negatable-fixnum-negation))))

(define-unary-test 'fix:not 'fix:noop-neg
  `((-1 0) (-2 1) (-3 2) (-4 3) (-5 4) (-6 5) (-7 6) (-8 7) (-9 8) (-10 9)
    (126 -127) (127 -128) (128 -129) (129 -130) (130 -131)
    (254 -255) (255 -256) (256 -257) (257 -258) (258 -259)
    (32766 -32767) (32767 -32768) (32768 -32769) (32769 -32770) (32770 -32771)
    (,(smallest-fixnum) ,(largest-fixnum))))

(define-commutative-binary-inverse-test 'fix:+ 'fix:-
  'fix:negate 0 'fix:noop-not
  `((0 0) (-1 1) (-2 2) (-3 3) (-4 4) (-5 5) (-6 6) (-7 7) (-8 8) (-9 9)
    (126 -126) (127 -127) (128 -128) (129 -129) (130 -130)
    (254 -254) (255 -255) (256 -256) (257 -257) (258 -258)
    (32766 -32766) (32767 -32767) (32768 -32768) (32769 -32769) (32770 -32770)
    (,(smallest-negatable-fixnum) ,(smallest-negatable-fixnum-negation))
    (,(largest-negatable-fixnum) ,(largest-negatable-fixnum-negation))))

(define-commutative-binary-inv/id-test 'fix:+ 'fix:-
  'fix:negate 0 'fix:noop-not
  `(-9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9
    -126 -127 -128 -129 -130
    126 127 128 129 130
    -254 -255 -256 -257 -258
    254 255 256 257 258
    -32766 -32767 -32768 -32769 -32770
    32766 32767 32768 32769 32770
    ,(smallest-negatable-fixnum)
    ,(largest-negatable-fixnum)))

;; Can't use inv/id-test for this because we can't necessarily negate.
(define-commutative-binary-identity-test 'fix:+ 'fix:noop-not 0
  `(,(smallest-fixnum)
    ,(largest-fixnum)))

(define-commutative-binary-identity-test 'fix:and 'fix:noop-neg -1
  `(-9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9
    -126 -127 -128 -129 -130
    126 127 128 129 130
    -254 -255 -256 -257 -258
    254 255 256 257 258
    -32766 -32767 -32768 -32769 -32770
    32766 32767 32768 32769 32770
    ,(smallest-negatable-fixnum)
    ,(largest-negatable-fixnum)))

(define-commutative-binary-identity-test 'fix:and 'fix:noop-neg+ -1
  `(,(largest-negatable-fixnum)))

(define-commutative-binary-identity-test 'fix:and 'fix:noop-neg- -1
  `(,(smallest-negatable-fixnum)))

(define-commutative-binary-identity-test 'fix:or 'fix:noop-neg 0
  `(-9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9
    -126 -127 -128 -129 -130
    126 127 128 129 130
    -254 -255 -256 -257 -258
    254 255 256 257 258
    -32766 -32767 -32768 -32769 -32770
    32766 32767 32768 32769 32770
    ,(smallest-negatable-fixnum)
    ,(largest-negatable-fixnum)))

(define-commutative-binary-identity-test 'fix:or 'fix:noop-neg+ 0
  `(,(largest-negatable-fixnum)))

(define-commutative-binary-identity-test 'fix:or 'fix:noop-neg- 0
  `(,(smallest-negatable-fixnum)))

(define-commutative-binary-identity-test 'fix:xor 'fix:noop-neg 0
  `(-9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9
    -126 -127 -128 -129 -130
    126 127 128 129 130
    -254 -255 -256 -257 -258
    254 255 256 257 258
    -32766 -32767 -32768 -32769 -32770
    32766 32767 32768 32769 32770
    ,(smallest-negatable-fixnum)
    ,(largest-negatable-fixnum)))

(define-commutative-binary-identity-test 'fix:xor 'fix:noop-neg+ 0
  `(,(largest-negatable-fixnum)))

(define-commutative-binary-identity-test 'fix:xor 'fix:noop-neg- 0
  `(,(smallest-negatable-fixnum)))

(define-commutative-binary-inv/id-test 'fix:xor 'fix:xor
  'fix:id 0 'fix:noop-neg
  `(-9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9
    -126 -127 -128 -129 -130
    126 127 128 129 130
    -254 -255 -256 -257 -258
    254 255 256 257 258
    -32766 -32767 -32768 -32769 -32770
    32766 32767 32768 32769 32770
    ,(smallest-negatable-fixnum)
    ,(largest-negatable-fixnum)))

(define-commutative-binary-identity-test 'fix:xor 'fix:noop-neg+ 0
  `(,(largest-fixnum)))

(define-commutative-binary-identity-test 'fix:xor 'fix:noop-neg- 0
  `(,(smallest-fixnum)))

(define-binary-right-identity-test 'fix:andc 'fix:noop-neg 0
  `(-9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9
    -126 -127 -128 -129 -130
    126 127 128 129 130
    -254 -255 -256 -257 -258
    254 255 256 257 258
    -32766 -32767 -32768 -32769 -32770
    32766 32767 32768 32769 32770
    ,(smallest-negatable-fixnum)
    ,(largest-negatable-fixnum)))

(define-binary-right-identity-test 'fix:andc 'fix:noop-neg+ 0
  `(,(largest-fixnum)))

(define-binary-right-identity-test 'fix:andc 'fix:noop-neg- 0
  `(,(smallest-fixnum)))

(define-commutative-binary-equivalent-test 'fix:and 'fix:and-via-not/or/not
  'fix:noop-neg
  `((-128 -128) (-128 -127) (-128 -2) (-128 -1)
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
    (128 128)))

(define-commutative-binary-equivalent-test 'fix:or 'fix:or-via-not/and/not
  'fix:noop-neg
  `((-128 -128) (-128 -127) (-128 -2) (-128 -1)
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
    (128 128)))

(define-binary-equivalent-test 'fix:andc 'fix:andc-via-and/not 'fix:noop-neg
  `((-128 -128) (-128 -127) (-128 -2) (-128 -1)
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
    (128 128)))

(define-commutative-binary-equivalent-test 'fix:and 'fix:and-via-andc/not
  'fix:noop-neg
  `((-128 -128) (-128 -127) (-128 -2) (-128 -1)
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
    (128 128)))