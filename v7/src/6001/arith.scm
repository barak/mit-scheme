#| -*-Scheme-*-

$Id: arith.scm,v 1.9 2002/02/03 03:38:53 cph Exp $

Copyright (c) 1989-1999, 2001, 2002 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
|#

;;;; Scheme Arithmetic for 6.001
;;; package: (student number)

(declare (usual-integrations))

(define-integrable (int:->flonum n)
  ((ucode-primitive integer->flonum 2) n #b10))

(define-integrable (flonum? object)
  (object-type? (ucode-type big-flonum) object))

(declare (integrate flo:integer?))
(define (flo:integer? x)
  (flo:= x (flo:round x)))

(define (flo:->integer x)
  (if (not (flo:integer? x))
      (error:wrong-type-argument x "integer" 'FLONUM->INTEGER))
  (flo:truncate->exact x))

(define-integrable (guarantee-integer object procedure)
  (if (not (int:integer? object))
      (error:wrong-type-argument object "number" procedure)))

(let-syntax
    ((define-standard-unary
       (sc-macro-transformer
	(lambda (form environment)
	  `(DEFINE (,(close-syntax (list-ref form 1) environment) X)
	     (IF (FLONUM? X)
		 (,(close-syntax (list-ref form 2) environment) X)
		 (,(close-syntax (list-ref form 3) environment) X)))))))
  (define-standard-unary rational? (lambda (x) x true) int:integer?)
  (define-standard-unary integer? flo:integer? int:integer?)
  (define-standard-unary exact? (lambda (x) x false)
    (lambda (x)
      (guarantee-integer x 'EXACT?)
      true))
  (define-standard-unary zero? flo:zero? int:zero?)
  (define-standard-unary negative? flo:negative? int:negative?)
  (define-standard-unary positive? flo:positive? int:positive?)
  (define-standard-unary abs flo:abs int:abs)
  (define-standard-unary floor flo:floor (lambda (x) x))
  (define-standard-unary ceiling flo:ceiling (lambda (x) x))
  (define-standard-unary truncate flo:truncate (lambda (x) x))
  (define-standard-unary round flo:round (lambda (x) x))
  (define-standard-unary exact->inexact (lambda (x) x) int:->flonum)
  (define-standard-unary inexact->exact
    (lambda (x)
      (if (not (flo:integer? x))
	  (error:bad-range-argument x 'INEXACT->EXACT))
      (flo:truncate->exact x))
    (lambda (x)
      (guarantee-integer x 'INEXACT->EXACT)
      x)))

(let-syntax
    ((define-standard-binary
       (sc-macro-transformer
	(lambda (form environment)
	  (let ((flo:op (close-syntax (list-ref form 2) environment))
		(int:op (close-syntax (list-ref form 3) environment)))
	    `(DEFINE (,(close-syntax (list-ref form 1) environment) X Y)
	       (IF (FLONUM? X)
		   (IF (FLONUM? Y)
		       (,flo:op X Y)
		       (,flo:op X (INT:->FLONUM Y)))
		   (IF (FLONUM? Y)
		       (,flo:op (INT:->FLONUM X) Y)
		       (,int:op X Y)))))))))
  (define-standard-binary real:+ flo:+ int:+)
  (define-standard-binary real:- flo:- int:-)
  (define-standard-binary rationalize
    flo:rationalize
    int:rationalize))

(define (int:rationalize q e)
  (int:simplest-rational (int:- q e) (int:+ q e)))

(define (int:simplest-rational x y)
  (let ((x<y
	 (lambda (x y)
	   (cond ((int:positive? x) x)
		 ((int:negative? y) y)
		 (else 0)))))
    (cond ((int:< x y) (x<y x y))
	  ((int:< y x) (x<y y x))
	  (else x))))

(define (real:* x y)
  (cond ((flonum? x)
	 (cond ((flonum? y) (flo:* x y))
	       ((int:zero? y) y)
	       (else (flo:* x (int:->flonum y)))))
	((int:zero? x) x)
	((flonum? y) (flo:* (int:->flonum x) y))
	(else (int:* x y))))

(define (real:/ x y)
  (cond ((flonum? x) (flo:/ x (if (flonum? y) y (int:->flonum y))))
	((flonum? y) (if (int:zero? x) x (flo:/ (int:->flonum x) y)))
	((int:= (int:remainder x y) 0) (int:quotient x y))
	(else (flo:/ (int:->flonum x) (int:->flonum y)))))

(define (real:invert x)
  (cond ((flonum? x) (flo:/ 1. x))
	((int:= 1 x) x)
	(else (flo:/ 1. (int:->flonum x)))))

(define (real:= x y)
  (if (flonum? x)
      (if (flonum? y)
	  (flo:= x y)
	  (begin
	    (guarantee-integer y '=)
	    (and (flo:= x (flo:truncate x))
		 (int:= (flo:truncate->exact x) y))))
      (if (flonum? y)
	  (begin
	    (guarantee-integer x '=)
	    (and (flo:= y (flo:truncate y))
		 (int:= x (flo:truncate->exact y))))
	  (int:= x y))))

(define (real:< x y)
  (if (flonum? x)
      (if (flonum? y)
	  (flo:< x y)
	  (flo/int:< x y))
      (if (flonum? y)
	  (int/flo:< x y)
	  (int:< x y))))

(define (real:max x y)
  (if (flonum? x)
      (if (flonum? y)
	  (if (flo:< x y) y x)
	  (if (flo/int:< x y) (int:->flonum y) x))
      (if (flonum? y)
	  (if (int/flo:< x y) y (int:->flonum x))
	  (if (int:< x y) y x))))

(define (real:min x y)
  (if (flonum? x)
      (if (flonum? y)
	  (if (flo:< x y) x y)
	  (if (flo/int:< x y) x (int:->flonum y)))
      (if (flonum? y)
	  (if (int/flo:< x y) (int:->flonum x) y)
	  (if (int:< x y) x y))))

(define-integrable (flo/int:< x y)
  (let ((ix (flo:truncate->exact x)))
    (cond ((int:< ix y) true)
	  ((int:< y ix) false)
	  (else (flo:< x (flo:truncate x))))))

(define-integrable (int/flo:< x y)
  (let ((iy (flo:truncate->exact y)))
    (cond ((int:< x iy) true)
	  ((int:< iy x) false)
	  (else (flo:< (flo:truncate y) y)))))

(define (even? n)
  (int:even? (if (flonum? n) (flo:->integer n) n)))

(let-syntax
    ((define-integer-binary
      (sc-macro-transformer
       (lambda (form environment)
	 (let ((operator (close-syntax (list-ref form 3) environment))
	       (flo->int
		(lambda (n)
		  `(IF (FLO:INTEGER? ,n)
		       (FLO:->INTEGER ,n)
		       (ERROR:WRONG-TYPE-ARGUMENT ,n "integer"
						  ',(list-ref form 2))))))
	   `(DEFINE (,(close-syntax (list-ref form 1) environment) N M)
	      (IF (FLONUM? N)
		  (INT:->FLONUM
		   (,operator ,(flo->int 'N)
			      (IF (FLONUM? M) (FLO:->INTEGER M) M)))
		  (IF (FLONUM? M)
		      (INT:->FLONUM (,operator N ,(flo->int 'M)))
		      (,operator N M)))))))))
  (define-integer-binary quotient quotient int:quotient)
  (define-integer-binary remainder remainder int:remainder)
  (define-integer-binary modulo modulo int:modulo)
  (define-integer-binary real:gcd gcd int:gcd)
  (define-integer-binary real:lcm lcm int:lcm))

(define (numerator q)
  (if (flonum? q)
      (int:->flonum (rat:numerator (flo:->rational q)))
      (begin
	(guarantee-integer q 'NUMERATOR)
	q)))

(define (denominator q)
  (if (flonum? q)
      (int:->flonum (rat:denominator (flo:->rational q)))
      (begin
	(guarantee-integer q 'DENOMINATOR)
	1)))

(let-syntax
    ((define-transcendental-unary
       (sc-macro-transformer
	(lambda (form environment)
	  `(DEFINE (,(close-syntax (list-ref form 1) environment) X)
	     (IF (,(close-syntax (list-ref form 2) environment) X)
		 ,(close-syntax (list-ref form 3) environment)
		 (,(close-syntax (list-ref form 4) environment)
		  (REAL:->FLONUM X))))))))
  (define-transcendental-unary exp real:exact0= 1 flo:exp)
  (define-transcendental-unary log real:exact1= 0 flo:log)
  (define-transcendental-unary sin real:exact0= 0 flo:sin)
  (define-transcendental-unary cos real:exact0= 1 flo:cos)
  (define-transcendental-unary tan real:exact0= 0 flo:tan)
  (define-transcendental-unary asin real:exact0= 0 flo:asin)
  (define-transcendental-unary acos real:exact1= 0 flo:acos)
  (define-transcendental-unary real:atan real:exact0= 0 flo:atan))

(define (real:atan2 y x)
  (if (and (real:exact0= y) (exact? x))
      0
      (flo:atan2 (real:->flonum y) (real:->flonum x))))

(define-integrable (real:exact0= x)
  (if (flonum? x) false (int:zero? x)))

(define-integrable (real:exact1= x)
  (if (flonum? x) false (int:= 1 x)))

(define (real:->flonum x)
  (if (flonum? x)
      x
      (int:->flonum x)))

(define (sqrt x)
  (if (flonum? x)
      (begin
	(if (flo:negative? x)
	    (error:bad-range-argument x 'SQRT))
	(flo:sqrt x))
      (int:sqrt x)))

(define (int:sqrt x)
  (if (int:negative? x)
      (error:bad-range-argument x 'SQRT))
  (let ((guess (flo:sqrt (int:->flonum x))))
    (let ((n (flo:round->exact guess)))
      (if (int:= x (int:* n n))
	  n
	  guess))))

(define (expt x y)
  (let ((general-case
	 (lambda (x y)
	   (cond ((flo:zero? y) 1.)
		 ((flo:zero? x)
		  (if (not (flo:positive? y))
		      (error:divide-by-zero 'EXPT (list x y)))
		  x)
		 (else
		  (if (and (flo:negative? x)
			   (not (flo:integer? y)))
		      (error:bad-range-argument x 'EXPT))
		  (flo:expt x y))))))
    (if (flonum? x)
	(if (flonum? y)
	    (general-case x y)
	    (let ((exact-method
		   (lambda (y)
		     (if (int:= 1 y)
			 x
			 (let loop ((x x) (y y) (answer 1.))
			   (let ((qr (int:divide y 2)))
			     (let ((x (flo:* x x))
				   (y (integer-divide-quotient qr))
				   (answer
				    (if (int:zero?
					 (integer-divide-remainder qr))
					answer
					(flo:* answer x))))
			       (if (int:= 1 y)
				   (flo:* answer x)
				   (loop x y answer)))))))))
	      (cond ((int:positive? y) (exact-method y))
		    ((int:negative? y)
		     (flo:/ 1. (exact-method (int:negate y))))
		    (else 1.))))
	(if (flonum? y)
	    (general-case (int:->flonum x) y)
	    (if (int:negative? y)
		(real:invert (int:expt x (int:negate y)))
		(int:expt x y))))))

(define number? rational?)
(define complex? rational?)
(define real? rational?)

(define (inexact? z)
  (not (exact? z)))

(define (odd? n)
  (not (even? n)))

(define (inc z)
  (+ z 1))

(define (dec z)
  (- z 1))

(define (= . zs)
  (reduce-comparator real:= zs '=))

(define (< . xs)
  (reduce-comparator real:< xs '<))

(define (> . xs)
  (reduce-comparator (lambda (x y) (real:< y x)) xs '>))

(define (<= . xs)
  (reduce-comparator (lambda (x y) (not (real:< y x))) xs '<=))

(define (>= . xs)
  (reduce-comparator (lambda (x y) (not (real:< x y))) xs '>=))

(define (max x . xs)
  (reduce-max/min real:max x xs 'MAX))

(define (min x . xs)
  (reduce-max/min real:min x xs 'MIN))

(define (+ . zs)
  (cond ((null? zs)
	 0)
	((null? (cdr zs))
	 (if (not (number? (car zs)))
	     (error:wrong-type-argument (car zs) false '+))
	 (car zs))
	((null? (cddr zs))
	 (real:+ (car zs) (cadr zs)))
	(else
	 (real:+ (car zs)
		 (real:+ (cadr zs)
			 (reduce real:+ 0 (cddr zs)))))))

(define (* . zs)
  (cond ((null? zs)
	 1)
	((null? (cdr zs))
	 (if (not (number? (car zs)))
	     (error:wrong-type-argument (car zs) false '*))
	 (car zs))
	((null? (cddr zs))
	 (real:* (car zs) (cadr zs)))
	(else
	 (real:* (car zs)
		 (real:* (cadr zs)
			 (reduce real:* 1 (cddr zs)))))))

(define (- z1 . zs)
  (cond ((null? zs)
	 (if (flonum? z1) (flo:negate z1) (int:negate z1)))
	((null? (cdr zs))
	 (real:- z1 (car zs)))
	(else
	 (real:- z1
		 (real:+ (car zs)
			 (real:+ (cadr zs)
				 (reduce real:+ 0 (cddr zs))))))))

(define (/ z1 . zs)
  (cond ((null? zs)
	 (real:invert z1))
	((null? (cdr zs))
	 (real:/ z1 (car zs)))
	(else
	 (real:/ z1
		 (real:* (car zs)
			 (real:* (cadr zs)
				 (reduce real:* 1 (cddr zs))))))))

(define (gcd . integers)
  (reduce real:gcd 0 integers))

(define (lcm . integers)
  (reduce real:lcm 1 integers))

(define (atan z #!optional x)
  (if (default-object? x) (real:atan z) (real:atan2 z x)))