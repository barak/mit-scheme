#| -*-Scheme-*-

$Id: fixart.scm,v 1.11 2003/02/25 20:35:26 cph Exp $

Copyright 1994,1996,1999,2000,2001,2003 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; Fixnum Arithmetic
;;; package: (runtime fixnum-arithmetic)

(declare (usual-integrations))

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
  (fix:lsh fixnum-lsh 2)

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
  (int:remainder integer-remainder 2)

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
  (flo:negate flonum-negate 1)
  (flo:abs flonum-abs 1)
  (flo:exp flonum-exp 1)
  (flo:log flonum-log 1)
  (flo:sin flonum-sin 1)
  (flo:cos flonum-cos 1)
  (flo:tan flonum-tan 1)
  (flo:asin flonum-asin 1)
  (flo:acos flonum-acos 1)
  (flo:atan flonum-atan 1)
  (flo:atan2 flonum-atan2 2)
  (flo:sqrt flonum-sqrt 1)
  (flo:expt flonum-expt 2)
  (flo:floor flonum-floor 1)
  (flo:ceiling flonum-ceiling 1)
  (flo:truncate flonum-truncate 1)
  (flo:round flonum-round 1)
  (flo:floor->exact flonum-floor->exact 1)
  (flo:ceiling->exact flonum-ceiling->exact 1)
  (flo:truncate->exact flonum-truncate->exact 1)
  (flo:round->exact flonum-round->exact 1)
  (flo:vector-cons floating-vector-cons 1)
  (flo:vector-length floating-vector-length 1)
  (flo:vector-ref floating-vector-ref 2)
  (flo:vector-set! floating-vector-set! 3))

(define (guarantee-index-fixnum object caller)
  (if (not (index-fixnum? object))
      (error:wrong-type-argument object "index integer" caller)))

(define-integrable (fix:<= x y)
  (not (fix:> x y)))

(define-integrable (fix:>= x y)
  (not (fix:< x y)))

(define (fix:min n m)
  (if (fix:< n m) n m))

(define (fix:max n m)
  (if (fix:> n m) n m))

(define-integrable (int:<= x y)
  (not (int:> x y)))

(define-integrable (int:>= x y)
  (not (int:< x y)))

(define-integrable (int:->flonum n)
  ((ucode-primitive integer->flonum 2) n #b10))

(define-integrable (flo:<= x y)
  (not (flo:> x y)))

(define-integrable (flo:>= x y)
  (not (flo:< x y)))

(define (flo:min n m)
  (if (flo:< n m) n m))

(define (flo:max n m)
  (if (flo:> n m) n m))

(define (->flonum x)
  (if (not (real? x))
      (error:wrong-type-argument x "real number" '->FLONUM))
  (exact->inexact (real-part x)))

(define (flo:finite? x)
  (not (cond ((flo:> x 0.)
	      (and (flo:> x 1.)
		   (flo:= x (flo:/ x 2.))))
	     ((flo:< x 0.)
	      (and (flo:< x -1.)
		   (flo:= x (flo:/ x 2.))))
	     (else
	      (flo:= x 0.)))))