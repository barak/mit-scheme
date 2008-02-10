#| -*-Scheme-*-

$Id: fixart.scm,v 1.19 2008/02/10 06:14:05 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; Fixnum Arithmetic
;;; package: (runtime fixnum-arithmetic)

(declare (usual-integrations))

(define-unary-primitive fix:-1+ minus-one-plus-fixnum)
(define-unary-primitive fix:1+ one-plus-fixnum)
(define-unary-primitive fix:fixnum? fixnum?)
(define-unary-primitive fix:negative? negative-fixnum?)
(define-unary-primitive fix:not fixnum-not)
(define-unary-primitive fix:positive? positive-fixnum?)
(define-unary-primitive fix:zero? zero-fixnum?)
(define-unary-primitive fixnum? fixnum?)
(define-unary-primitive index-fixnum? index-fixnum?)

(define-binary-primitive fix:= equal-fixnum?)
(define-binary-primitive fix:< less-than-fixnum?)
(define-binary-primitive fix:> greater-than-fixnum?)
(define-binary-primitive fix:+ plus-fixnum)
(define-binary-primitive fix:- minus-fixnum)
(define-binary-primitive fix:* multiply-fixnum)
(define-binary-primitive fix:divide divide-fixnum)
(define-binary-primitive fix:quotient fixnum-quotient)
(define-binary-primitive fix:remainder fixnum-remainder)
(define-binary-primitive fix:gcd gcd-fixnum)
(define-binary-primitive fix:andc fixnum-andc)
(define-binary-primitive fix:and fixnum-and)
(define-binary-primitive fix:or fixnum-or)
(define-binary-primitive fix:xor fixnum-xor)
(define-binary-primitive fix:lsh fixnum-lsh)

(define-unary-primitive int:-1+ integer-subtract-1)
(define-unary-primitive int:1+ integer-add-1)
(define-unary-primitive int:integer? integer?)
(define-unary-primitive int:negate integer-negate)
(define-unary-primitive int:negative? integer-negative?)
(define-unary-primitive int:positive? integer-positive?)
(define-unary-primitive int:zero? integer-zero?)

(define-binary-primitive int:= integer-equal?)
(define-binary-primitive int:< integer-less?)
(define-binary-primitive int:> integer-greater?)
(define-binary-primitive int:+ integer-add)
(define-binary-primitive int:- integer-subtract)
(define-binary-primitive int:* integer-multiply)
(define-binary-primitive int:divide integer-divide)
(define-binary-primitive int:quotient integer-quotient)
(define-binary-primitive int:remainder integer-remainder)

(define-unary-primitive flo:abs flonum-abs)
(define-unary-primitive flo:acos flonum-acos)
(define-unary-primitive flo:asin flonum-asin)
(define-unary-primitive flo:atan flonum-atan)
(define-unary-primitive flo:ceiling flonum-ceiling)
(define-unary-primitive flo:ceiling->exact flonum-ceiling->exact)
(define-unary-primitive flo:cos flonum-cos)
(define-unary-primitive flo:exp flonum-exp)
(define-unary-primitive flo:flonum? flonum?)
(define-unary-primitive flo:floor flonum-floor)
(define-unary-primitive flo:floor->exact flonum-floor->exact)
(define-unary-primitive flo:log flonum-log)
(define-unary-primitive flo:negate flonum-negate)
(define-unary-primitive flo:negative? flonum-negative?)
(define-unary-primitive flo:positive? flonum-positive?)
(define-unary-primitive flo:round flonum-round)
(define-unary-primitive flo:round->exact flonum-round->exact)
(define-unary-primitive flo:sin flonum-sin)
(define-unary-primitive flo:sqrt flonum-sqrt)
(define-unary-primitive flo:tan flonum-tan)
(define-unary-primitive flo:truncate flonum-truncate)
(define-unary-primitive flo:truncate->exact flonum-truncate->exact)
(define-unary-primitive flo:vector-cons floating-vector-cons)
(define-unary-primitive flo:vector-length floating-vector-length)
(define-unary-primitive flo:zero? flonum-zero?)

(define-binary-primitive flo:= flonum-equal?)
(define-binary-primitive flo:< flonum-less?)
(define-binary-primitive flo:> flonum-greater?)
(define-binary-primitive flo:+ flonum-add)
(define-binary-primitive flo:- flonum-subtract)
(define-binary-primitive flo:* flonum-multiply)
(define-binary-primitive flo:/ flonum-divide)
(define-binary-primitive flo:atan2 flonum-atan2)
(define-binary-primitive flo:expt flonum-expt)
(define-binary-primitive flo:vector-ref floating-vector-ref)

(define-integrable (flo:vector-set! v i x)
  ((ucode-primitive floating-vector-set!) v i x))

(define-integrable (guarantee-index-fixnum object caller)
  (if (not (index-fixnum? object))
      (error:wrong-type-argument object "index integer" caller)))

(define (guarantee-limited-index-fixnum object limit caller)
  (guarantee-index-fixnum object caller)
  (if (not (fix:< object limit))
      (error:bad-range-argument object caller)))

(define-integrable (fix:<= n m) (not (fix:> n m)))
(define-integrable (fix:>= n m) (not (fix:< n m)))
(define-integrable (int:<= n m) (not (int:> n m)))
(define-integrable (int:>= n m) (not (int:< n m)))

(define (fix:min n m) (if (fix:< n m) n m))
(define (fix:max n m) (if (fix:> n m) n m))

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

(define (flo:finite? x)
  (if (or (flo:> x 1.) (flo:< x -1.))
      (not (flo:= x (flo:/ x 2.)))
      (and (flo:<= x 1.) (flo:>= x -1.))))

(define-integrable (int:->flonum n)
  ((ucode-primitive integer->flonum 2) n #b10))

(define (->flonum x)
  (guarantee-real x '->FLONUM)
  (exact->inexact (real-part x)))