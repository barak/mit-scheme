#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/numint.scm,v 1.1 1989/11/09 04:02:53 cph Exp $

Copyright (c) 1989 Massachusetts Institute of Technology

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

;;;; Scheme Arithmetic Interface
;;; package: (runtime number interface)

(declare (usual-integrations))

(define (make-=-operator =)
  (lambda zs
    (reduce-comparator = zs)))

(define (make-<-operator <)
  (lambda zs
    (reduce-comparator < zs)))

(define (make->-operator <)
  (lambda zs
    (reduce-comparator (lambda (x y) (< y x)) 
		       zs)))

(define (make-<=-operator <)
  (lambda zs
    (reduce-comparator (lambda (x y) (not (< y x)))
		       zs)))
  
(define (make->=-operator <)
  (lambda zs
    (reduce-comparator (lambda (x y) (not (< x y)))
		       zs)))

(define (make-max/min-operator max)
  (lambda (x . xs)
    (reduce-max/min max x xs)))

(define (make-atan-operator atan1 atan2)
  (lambda (z . xs)
    (if (null? xs)
	(atan1 z)
	(atan2 z (car xs)))))

(define (make-accumulation-operator op identity)
  (lambda zs (reduce op identity zs)))

(define (make-inverse-accumulation-operator
	 accumulate-op identity unary-invert-op binary-invert-op)
  (lambda (z1 . zs)
    (if (null? zs)
	(unary-invert-op z1)
	(binary-invert-op z1
			  (reduce accumulate-op identity zs)))))

(define (make-arithmetic-package package-name . operations) 
  (lambda (m . opt)
    (cond ((eq? m 'bound-names) (map car operations))
	  ((eq? m 'package-name) package-name)
	  (else
	   (let ((entry (assq m operations)))
	     (if entry
		 (cadr entry)
		 (if (not (null? opt))
		     (car opt)
		     (error "Object not available" package-name m))))))))

(define integer-package
  (make-arithmetic-package
   'integer-package
   `(zero 0)
   `(one 1)
   `(integer? ,int:integer?)
   `(characteristic-predicate ,int:integer?)
   `(= ,(make-=-operator int:=))
   `(< ,(make-<-operator int:<))
   `(> ,(make->-operator int:<))
   `(<= ,(make-<=-operator int:<))
   `(>= ,(make->=-operator int:<))
   `(zero? ,int:zero?)
   `(positive? ,int:positive?)
   `(negative? ,int:negative?)
   `(even? ,int:even?)
   `(odd? ,(lambda (n) (not (int:even? n))))
   `(max ,(make-max/min-operator int:max))
   `(min ,(make-max/min-operator int:min))
   `(+ ,(make-accumulation-operator int:+ 0))
   `(1+ ,int:1+)
   `(-1+ ,int:-1+)
   `(- ,(make-inverse-accumulation-operator int:+ 0 int:negate int:-))
   `(* ,(make-accumulation-operator int:* 1))
   `(negate ,int:negate)
   `(abs ,int:abs)
   `(expt ,int:expt)
   `(quotient ,int:quotient)
   `(remainder ,int:remainder)
   `(modulo ,int:modulo)
   `(integer-divide ,int:divide)
   `(gcd ,(make-accumulation-operator int:gcd 0))
   `(lcm ,(make-accumulation-operator int:lcm 1))
   ))

(define rational-package
  (make-arithmetic-package
   'rational-package
   `(zero 0)
   `(one 1)
   `(integer? ,rat:integer?)
   `(rational? ,rat:rational?)
   `(characteristic-predicate ,rat:rational?)
   `(= ,(make-=-operator rat:=))
   `(< ,(make-<-operator rat:<))
   `(> ,(make->-operator rat:<))
   `(<= ,(make-<=-operator rat:<))
   `(>= ,(make->=-operator rat:<))
   `(zero? ,rat:zero?)
   `(one? ,(lambda (p) (rat:= p 1)))
   `(positive? ,rat:positive?)
   `(negative? rat:negative?)
   `(max ,(make-max/min-operator rat:max))
   `(min ,(make-max/min-operator rat:min))
   `(1+ ,rat:1+)
   `(-1+ ,rat:-1+)
   `(+ ,(make-accumulation-operator rat:+ 0))
   `(- ,(make-inverse-accumulation-operator rat:+ 0 rat:negate rat:-))
   `(* ,(make-accumulation-operator rat:* 1))
   `(/ ,(make-inverse-accumulation-operator rat:* 1 rat:invert rat:/))
   `(negate ,rat:negate)
   `(invert ,rat:invert)
   `(abs ,rat:abs)
   `(expt ,rat:expt)
   `(make-rational ,make-rational)
   `(numerator ,rat:numerator)
   `(denominator ,rat:denominator)
   ))

(define real-package
  (make-arithmetic-package
   'real-package
   `(zero 0)
   `(one 1)
   `(integer? ,real:integer?)
   `(rational? ,real:rational?)
   `(real? ,real:real?)
   `(characteristic-predicate ,real:real?)
   `(= ,(make-=-operator real:=))
   `(< ,(make-<-operator real:<))
   `(> ,(make->-operator real:<))
   `(<= ,(make-<=-operator real:<))
   `(>= ,(make->=-operator real:<))
   `(zero? ,real:zero?)
   `(positive? ,real:positive?)
   `(negative? ,real:negative?)
   `(max ,(make-max/min-operator real:max))
   `(min ,(make-max/min-operator real:min))
   `(1+ ,real:1+)
   `(-1+ ,real:-1+)
   `(+ ,(make-accumulation-operator real:+ 0))
   `(- ,(make-inverse-accumulation-operator real:+ 0 real:negate real:-))
   `(* ,(make-accumulation-operator real:* 1))
   `(/ ,(make-inverse-accumulation-operator real:* 1 real:invert real:/))
   `(negate ,real:negate)
   `(invert ,real:invert)
   `(abs ,real:abs)
   `(exp ,real:exp)
   `(log ,real:log)
   `(sin ,real:sin)
   `(cos ,real:cos)
   `(tan ,real:tan)
   `(asin ,real:asin)
   `(acos ,real:acos)
   `(atan ,(make-atan-operator real:atan real:atan2))
   `(sqrt ,real:sqrt)
   `(expt ,real:expt)
   ))

(define complex-package
  (make-arithmetic-package
   'complex-package
   `(zero 0)
   `(one 1)
   `(imag-unit ,+i)
   `(integer? ,complex:integer?)
   `(rational? ,complex:rational?)
   `(real? ,complex:real?)
   `(complex? ,complex:complex?)
   `(characteristic-predicate ,complex:complex?)
   `(= ,(make-=-operator complex:=))
   `(zero? ,complex:zero?)
   `(1+ ,complex:1+)
   `(-1+ ,complex:-1+)
   `(+ ,(make-accumulation-operator complex:+ 0))
   `(- ,(make-inverse-accumulation-operator complex:+
					    0
					    complex:negate
					    complex:-))
   `(* ,(make-accumulation-operator complex:* 1))
   `(/ ,(make-inverse-accumulation-operator complex:*
					    1
					    complex:invert
					    complex:/))
   `(negate ,complex:negate)
   `(invert ,complex:invert)
   `(abs ,complex:abs)
   `(exp ,complex:exp)
   `(log ,complex:log)
   `(sin ,complex:sin)
   `(cos ,complex:cos)
   `(tan ,complex:tan)
   `(asin ,complex:asin)
   `(acos ,complex:acos)
   `(atan ,(make-atan-operator complex:atan complex:atan2))
   `(sqrt ,complex:sqrt)
   `(expt ,complex:expt)
   `(make-rectangular ,complex:make-rectangular)
   `(make-polar ,complex:make-polar)
   `(real-part ,complex:real-part)
   `(imag-part ,complex:imag-part)
   `(magnitude ,complex:magnitude)
   `(angle ,complex:angle)
   `(conjugate ,complex:conjugate)
   ))