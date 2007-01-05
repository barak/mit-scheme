#| -*-Scheme-*-

$Id: numint.scm,v 1.9 2007/01/05 15:33:10 cph Exp $

Copyright (c) 1989-1999 Massachusetts Institute of Technology

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

;;;; Scheme Arithmetic Interface
;;; package: (runtime number interface)

(declare (usual-integrations))

;;(define (make-=-operator =)
;;  (lambda zs
;;    (reduce-comparator = zs 'make-=-operator)))

(define (make-=-operator =)
  (make-arity-dispatched-procedure
   (lambda (self . zs)
     self				; ignored
     (reduce-comparator = zs 'make-=-operator))
   (lambda () #T)
   (lambda (z) z #T)
   (lambda (z1 z2) (= z1 z2))))

;;(define (make-<-operator <)
;;  (lambda zs
;;    (reduce-comparator < zs 'make-<-operator)))

(define (make-comparison-operator comparator name)
  (make-arity-dispatched-procedure
   (lambda (self . zs)
     self				; ignored
     (reduce-comparator comparator zs name))
   (lambda () #T)
   (lambda (z) z #T)
   comparator))

(define (make-<-operator <)
  (make-comparison-operator < 'make-<-operator))

(define (make->-operator <)
  (make-comparison-operator (lambda (x y) (< y x)) 'make->-operator))

(define (make-<=-operator <)
  (make-comparison-operator (lambda (x y) (not (< y x))) 'make-<=-operator))
  
(define (make->=-operator <)
  (make-comparison-operator (lambda (x y) (not (< x y))) 'make->=-operator))

;;(define (make-max/min-operator max/min)
;;  (lambda (x . xs)
;;    (reduce-max/min max/min x xs 'make-max/min-operator)))

(define (make-max/min-operator max/min)
  (make-arity-dispatched-procedure
   (lambda (self x . xs)
     self				;ignored
     (reduce-max/min max/min x xs 'make-max/min-operator))
   #F
   (lambda (x) x)
   max/min))

;;(define (make-atan-operator atan1 atan2)
;;  (lambda (z . xs)
;;    (if (null? xs)
;;	(atan1 z)
;;	(atan2 z (car xs)))))

(define (make-atan-operator atan1 atan2)
  (make-arity-dispatched-procedure
   (lambda (self z1 #!optional z2)	; required for arity
     (error "ATAN operator: should never get to this case" self z1 z2))
   #F
   atan1
   atan2))

;;(define (make-accumulation-operator op identity)
;;  (lambda zs (reduce op identity zs)))
;;
;;(define (make-inverse-accumulation-operator
;;	 accumulate-op identity unary-invert-op binary-invert-op)
;;  (lambda (z1 . zs)
;;    (if (null? zs)
;;	(unary-invert-op z1)
;;	(binary-invert-op z1
;;			  (reduce accumulate-op identity zs)))))

(define (make-accumulation-operator op identity)
  (make-arity-dispatched-procedure
   (lambda (self . zs)
     self				; ignored
     (reduce op identity zs))
   (lambda () identity)
   (lambda (z) z)
   op))

(define (make-inverse-accumulation-operator
	 accumulate-op identity unary-invert-op binary-invert-op)
  (make-arity-dispatched-procedure
   (lambda (self z1 . zs)
     self				; ignored
     (binary-invert-op z1
		       (reduce accumulate-op identity zs)))
   #F					; no nullary case
   unary-invert-op
   binary-invert-op))


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