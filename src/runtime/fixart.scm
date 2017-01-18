#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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

;;;; Low-level arithmetic
;;; package: (runtime fixnum-arithmetic)

(declare (usual-integrations))

;;;; Fixnums

(define-primitives
  (fix:fixnum? fixnum? 1)
  (fixnum? fixnum? 1)
  (index-fixnum? index-fixnum? 1)
  (fix:zero? zero-fixnum? 1)
  (fix:negative? negative-fixnum? 1)
  (fix:positive? positive-fixnum? 1)
  (fix:= equal-fixnum? 2)
  (fix:< less-than-fixnum? 2)
  (fix:> greater-than-fixnum? 2)
  (fix:1+ one-plus-fixnum 1)
  (fix:-1+ minus-one-plus-fixnum 1)
  (fix:+ plus-fixnum 2)
  (fix:- minus-fixnum 2)
  (fix:* multiply-fixnum 2)
  (fix:divide divide-fixnum 2)
  (fix:quotient fixnum-quotient 2)
  (fix:remainder fixnum-remainder 2)
  (fix:gcd gcd-fixnum 2)
  (fix:andc fixnum-andc 2)
  (fix:and fixnum-and 2)
  (fix:or fixnum-or 2)
  (fix:xor fixnum-xor 2)
  (fix:not fixnum-not 1)
  (fix:lsh fixnum-lsh 2))

(define (positive-fixnum? object)
  (and (fixnum? object)
       (fix:positive? object)))

(define (negative-fixnum? object)
  (and (fixnum? object)
       (fix:negative? object)))

(define (non-negative-fixnum? object)
  (and (fixnum? object)
       (not (fix:negative? object))))

(define (non-positive-fixnum? object)
  (and (fixnum? object)
       (not (fix:positive? object))))

(define-guarantee fixnum "fixnum")
(define-guarantee positive-fixnum "positive fixnum")
(define-guarantee negative-fixnum "negative fixnum")
(define-guarantee non-positive-fixnum "non-positive fixnum")
(define-guarantee non-negative-fixnum "non-negative fixnum")

(define (guarantee-index-fixnum object #!optional caller)
  (if (not (index-fixnum? object))
      (error:wrong-type-argument object "index integer" caller)))

(define (guarantee-limited-index-fixnum object limit #!optional caller)
  (guarantee-index-fixnum object caller)
  (if (not (fix:< object limit))
      (error:bad-range-argument object caller)))

(define (fix:<= n m) (not (fix:> n m)))
(define (fix:>= n m) (not (fix:< n m)))
(define (fix:min n m) (if (fix:< n m) n m))
(define (fix:max n m) (if (fix:> n m) n m))

;;;; Flonums

(define-primitives
  (flo:flonum? flonum? 1)
  (flo:zero? flonum-zero? 1)
  (flo:positive? flonum-positive? 1)
  (flo:negative? flonum-negative? 1)
  (flo:= flonum-equal? 2)
  (flo:< flonum-less? 2)
  (flo:> flonum-greater? 2)
  (flo:+ flonum-add 2)
  (flo:- flonum-subtract 2)
  (flo:* flonum-multiply 2)
  (flo:/ flonum-divide 2)
  (flo:modulo flonum-modulo 2)
  (flo:negate flonum-negate 1)
  (flo:abs flonum-abs 1)
  (flo:exp flonum-exp 1)
  (flo:expm1 flonum-expm1 1)
  (flo:log flonum-log 1)
  (flo:log1p flonum-log1p 1)
  (flo:sin flonum-sin 1)
  (flo:cos flonum-cos 1)
  (flo:tan flonum-tan 1)
  (flo:sinh flonum-sinh 1)
  (flo:cosh flonum-cosh 1)
  (flo:tanh flonum-tanh 1)
  (flo:asin flonum-asin 1)
  (flo:acos flonum-acos 1)
  (flo:atan flonum-atan 1)
  (flo:asinh flonum-asinh 1)
  (flo:acosh flonum-acosh 1)
  (flo:atanh flonum-atanh 1)
  (flo:atan2 flonum-atan2 2)
  (flo:sqrt flonum-sqrt 1)
  (flo:cbrt flonum-cbrt 1)
  (flo:hypot flonum-hypot 2)
  (flo:expt flonum-expt 2)
  (flo:lgamma flonum-lgamma 1)
  (flo:gamma flonum-gamma 1)
  (flo:erf flonum-erf 1)
  (flo:erfc flonum-erfc 1)
  (flo:j0 flonum-j0 1)
  (flo:j1 flonum-j1 1)
  (flo:jn flonum-jn 2)
  (flo:y0 flonum-y0 1)
  (flo:y1 flonum-y1 1)
  (flo:yn flonum-yn 2)
  (flo:floor flonum-floor 1)
  (flo:ceiling flonum-ceiling 1)
  (flo:truncate flonum-truncate 1)
  (flo:round flonum-round 1)
  (flo:floor->exact flonum-floor->exact 1)
  (flo:ceiling->exact flonum-ceiling->exact 1)
  (flo:truncate->exact flonum-truncate->exact 1)
  (flo:round->exact flonum-round->exact 1)
  (flo:copysign flonum-copysign 2)
  (flo:nextafter flonum-nextafter 2)
  (flo:vector-cons floating-vector-cons 1)
  (flo:vector-length floating-vector-length 1)
  (flo:vector-ref floating-vector-ref 2)
  (flo:vector-set! floating-vector-set! 3))

(define (flo:<= x y) (or (flo:< x y) (flo:= x y)))
(define (flo:>= x y) (or (flo:> x y) (flo:= x y)))

(define (flo:min x y)
  (cond ((flo:< x y) x)
	((flo:> x y) y)
	((flo:= x y) x)
	(else (error:bad-range-argument (if (flo:finite? x) x y) 'FLO:MIN))))

(define (flo:max x y)
  (cond ((flo:< x y) y)
	((flo:> x y) x)
	((flo:= x y) y)
	(else (error:bad-range-argument (if (flo:finite? x) x y) 'FLO:MAX))))

;;; XXX FLO:FINITE?, FLO:NAN?, FLO:EQV?, &c., are cute, but should be
;;; replaced by primitives.

(define (flo:finite? x)
  (if (or (flo:> x 1.) (flo:< x -1.))
      (not (flo:= x (flo:/ x 2.)))
      (and (flo:<= x 1.) (flo:>= x -1.))))

(define (flo:eqv? x y)
  ;; (bit-string=? (flo:->bit-string x) (flo:->bit-string y))
  (if (flo:= x y)
      (or (not (flo:zero? x))
          ;; XXX Kludgey but expedient test for zero sign.
          (flo:= (flo:atan2 x -1.) (flo:atan2 y -1.)))
      ;; XXX (and (flo:nan? x) (flo:nan? y) ...)
      #f))

(define (int:->flonum n)
  ((ucode-primitive integer->flonum 2) n #b10))

(define (->flonum x)
  (guarantee-real x '->FLONUM)
  (exact->inexact (real-part x)))

;;;; Exact integers

(define-primitives
  (int:integer? integer? 1)
  (int:zero? integer-zero? 1)
  (int:positive? integer-positive? 1)
  (int:negative? integer-negative? 1)
  (int:= integer-equal? 2)
  (int:< integer-less? 2)
  (int:> integer-greater? 2)
  (int:negate integer-negate 1)
  (int:1+ integer-add-1 1)
  (int:-1+ integer-subtract-1 1)
  (int:+ integer-add 2)
  (int:- integer-subtract 2)
  (int:* integer-multiply 2)
  (int:divide integer-divide 2)
  (int:quotient integer-quotient 2)
  (int:remainder integer-remainder 2))

(define-integrable (int:<= n m) (not (int:> n m)))
(define-integrable (int:>= n m) (not (int:< n m)))