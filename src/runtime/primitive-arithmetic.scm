#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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
;;; package: (runtime primitive-arithmetic)

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

(define (guarantee-limited-index-fixnum object limit #!optional caller)
  (guarantee index-fixnum? object caller)
  (if (not (fix:< object limit))
      (error:bad-range-argument object caller)))

(define (fix:<= n m) (not (fix:> n m)))
(define (fix:>= n m) (not (fix:< n m)))
(define (fix:min n m) (if (fix:< n m) n m))
(define (fix:max n m) (if (fix:> n m) n m))

(define (fix:largest-value)
  (force largest-fixnum-promise))

(define largest-fixnum-promise
  (delay
    (let loop ((n 1))
      (if (fix:fixnum? n)
	  (loop (* n 2))
	  (let ((n (- n 1)))
	    (if (not (fix:fixnum? n))
		(error "Unable to compute largest fixnum:" n))
	    n)))))

(define (fix:smallest-value)
  (force smallest-fixnum-promise))

(define smallest-fixnum-promise
  (delay
    (let loop ((n -1))
      (if (fix:fixnum? n)
	  (loop (* n 2))
	  (let ((n (quotient n 2)))
	    (if (not (fix:fixnum? n))
		(error "Unable to compute smallest fixnum:" n))
	    n)))))

(define (fix:iota count #!optional start step)
  (guarantee index-fixnum? count 'fix:iota)
  (let ((start
	 (if (default-object? start)
	     0
	     (begin
	       (guarantee fix:fixnum? start 'fix:iota)
	       start)))
	(step
	 (if (default-object? step)
	     1
	     (begin
	       (guarantee fix:fixnum? step 'fix:iota)
	       step))))
    (let loop
	((index (fix:- count 1))
	 (value (fix:+ start (fix:* step (fix:- count 1))))
	 (result '()))
      (if (fix:>= index 0)
	  (loop (fix:- index 1)
		(fix:- value step)
		(cons value result))
	  result))))

(define (fix:end-index end length #!optional caller)
  (if (default-object? end)
      length
      (begin
	(guarantee index-fixnum? end caller)
	(if (not (fix:<= end length))
	    (error:bad-range-argument end caller))
	end)))

(define (fix:start-index start end #!optional caller)
  (if (default-object? start)
      0
      (begin
	(guarantee index-fixnum? start caller)
	(if (not (fix:<= start end))
	    (error:bad-range-argument start caller))
	start)))

;;;; Flonums

(define-primitives
  (flo:flonum? flonum? 1)
  (flo:zero? flonum-zero? 1)
  (flo:positive? flonum-positive? 1)
  (flo:negative? flonum-negative? 1)
  (flo:finite? flonum-is-finite? 1)
  (flo:infinite? flonum-is-infinite? 1)
  (flo:nan? flonum-is-nan? 1)
  (flo:normal? flonum-is-normal? 1)
  (flo:safe-negative? flonum-is-negative? 1)
  (flo:= flonum-equal? 2)
  (flo:< flonum-less? 2)
  (flo:> flonum-greater? 2)
  (flo:safe> flonum-is-greater? 2)
  (flo:safe>= flonum-is-greater-or-equal? 2)
  (flo:safe< flonum-is-less? 2)
  (flo:safe<= flonum-is-less-or-equal? 2)
  (flo:safe<> flonum-is-less-or-greater? 2)
  (flo:unordered? flonum-is-unordered? 2)
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
  (flo:denormalize flonum-denormalize 2)
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
	(else (error:bad-range-argument (if (flo:finite? x) x y) 'flo:min))))

(define (flo:max x y)
  (cond ((flo:< x y) y)
	((flo:> x y) x)
	((flo:= x y) y)
	(else (error:bad-range-argument (if (flo:finite? x) x y) 'flo:max))))

(define (flo:eqv? x y)
  (and (not (flo:nan? x))
       (not (flo:nan? y))
       (flo:= x y)
       (or (not (flo:zero? x))
	   (eq? (flo:safe-negative? x)
		(flo:safe-negative? y)))))

(define (int:->flonum n)
  ((ucode-primitive integer->flonum 2) n #b10))

(define (->flonum x)
  (guarantee real? x '->flonum)
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

(define (int:modulo n d)
  (let ((r (int:remainder n d)))
    (if (or (int:zero? r)
	    (if (int:negative? n)
		(int:negative? d)
		(not (int:negative? d))))
	r
	(int:+ r d))))

;;; Fairly standard Newton's method implementation.  Cribbed from
;;; https://www.akalin.com/computing-isqrt which has proof of correctness and
;;; shows that this is O(lg lg n) in time.

(define (exact-integer-sqrt n)
  (guarantee exact-nonnegative-integer? n 'exact-integer-sqrt)
  (if (int:= 0 n)
      (values 0 0)
      (let loop
	  ((i
	    (shift-left 1
			(let ((n-bits (integer-length n)))
			  (if (int:= 0 (remainder n-bits 2))
			      (int:quotient n-bits 2)
			      (int:+ (int:quotient n-bits 2) 1))))))
	(let ((j (int:quotient (int:+ i (int:quotient n i)) 2)))
	  (if (int:>= j i)
	      (values i (int:- n (int:* i i)))
	      (loop j))))))