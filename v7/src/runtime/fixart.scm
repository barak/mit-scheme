#| -*-Scheme-*-

$Id: fixart.scm,v 1.2 1996/04/24 03:03:00 cph Exp $

Copyright (c) 1988-96 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; Fixnum Arithmetic
;;; package: ()

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

(define-integrable (fix:<= x y)
  (not (fix:> x y)))

(define-integrable (fix:>= x y)
  (not (fix:< x y)))

(define-integrable (int:<= x y)
  (not (int:> x y)))

(define-integrable (int:>= x y)
  (not (int:< x y)))

(define-integrable (int:->flonum n)
  ((ucode-primitive integer->flonum 2) n #b10))