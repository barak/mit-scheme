#| -*-Scheme-*-

$Id: 20d11a4b910754ec5afcf5ef7f979b69f88d48a4 $

Copyright (c) 1992-1999 Massachusetts Institute of Technology

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
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; RTL Rewrite Rules
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Synthesized Data

(define-rule rewriting
  (CONS-NON-POINTER (? type) (? datum))
  ;; On i386, there's no difference between an address and a datum,
  ;; so the rules for constructing non-pointer objects are the same as
  ;; those for pointer objects.
  (rtl:make-cons-pointer type datum))

(define-rule rewriting
  (CONS-POINTER (REGISTER (? type register-known-value)) (? datum))
  (QUALIFIER (rtl:machine-constant? type))
  (rtl:make-cons-pointer type datum))

(define-rule rewriting
  (CONS-POINTER (REGISTER (? type register-known-value)) (? datum))
  (QUALIFIER
   (and (rtl:object->type? type)
	(rtl:constant? (rtl:object->type-expression type))))
  (rtl:make-cons-pointer
   (rtl:make-machine-constant
    (object-type (rtl:constant-value (rtl:object->type-expression datum))))
   datum))

(define-rule rewriting
  (CONS-POINTER (? type) (REGISTER (? datum register-known-value)))
  (QUALIFIER (rtl:machine-constant? datum))
  (rtl:make-cons-pointer type datum))

(define-rule rewriting
  (CONS-POINTER (? type) (REGISTER (? datum register-known-value)))
  (QUALIFIER
   (and (rtl:object->datum? datum)
	(rtl:constant-non-pointer? (rtl:object->datum-expression datum))))
  (rtl:make-cons-pointer
   type
   (rtl:make-machine-constant
    (386-object-datum
     (rtl:constant-value (rtl:object->datum-expression datum))))))

(define-rule rewriting
  (OBJECT->TYPE (REGISTER (? source register-known-value)))
  (QUALIFIER (rtl:constant? source))
  (rtl:make-machine-constant (object-type (rtl:constant-value source))))

(define-rule rewriting
  (OBJECT->DATUM (REGISTER (? source register-known-value)))
  (QUALIFIER (rtl:constant-non-pointer? source))
  (rtl:make-machine-constant
   (386-object-datum (rtl:constant-value source))))

(define (rtl:constant-non-pointer? expression)
  (and (rtl:constant? expression)
       (non-pointer-object? (rtl:constant-value expression))))

;;; These rules are losers because there's no abstract way to cons a
;;; statement or a predicate without also getting some CFG structure.

(define-rule rewriting
  (ASSIGN (? target) (REGISTER (? comparand register-known-value)))
  (QUALIFIER (rtl:immediate-zero-constant? comparand))
  (list 'ASSIGN target comparand))

(define-rule rewriting
  (ASSIGN (OFFSET (REGISTER (? address)) (MACHINE-CONSTANT (? offset)))
	  (REGISTER (? source register-known-value)))
  (QUALIFIER
   (and (rtl:byte-offset-address? source)
	(rtl:machine-constant? (rtl:byte-offset-address-offset source))
	(let ((base (let ((base (rtl:byte-offset-address-base source)))
		      (if (rtl:register? base)
			  (register-known-value (rtl:register-number base))
			  base))))
	  (and base
	       (rtl:offset? base)
	       (let ((base* (rtl:offset-base base))
		     (offset* (rtl:offset-offset base)))
		 (and (rtl:machine-constant? offset*)
		      (= (rtl:register-number base*) address)
		      (= (rtl:machine-constant-value offset*) offset)))))))
  (let ((target (let ((base (rtl:byte-offset-address-base source)))
		  (if (rtl:register? base)
		      (register-known-value (rtl:register-number base))
		      base))))
    (list 'ASSIGN
	  target
	  (rtl:make-byte-offset-address
	   target
	   (rtl:byte-offset-address-offset source)))))

(define-rule rewriting
  (EQ-TEST (? source) (REGISTER (? comparand register-known-value)))
  (QUALIFIER (rtl:immediate-zero-constant? comparand))
  (list 'EQ-TEST source comparand))

(define-rule rewriting
  (EQ-TEST (REGISTER (? comparand register-known-value)) (? source))
  (QUALIFIER (rtl:immediate-zero-constant? comparand))
  (list 'EQ-TEST source comparand))

(define (rtl:immediate-zero-constant? expression)
  (cond ((rtl:constant? expression)
	 (let ((value (rtl:constant-value expression)))
	   (and (non-pointer-object? value)
		(zero? (object-type value))
		(zero? (386-object-datum value)))))
	((rtl:cons-pointer? expression)
	 (and (let ((expression (rtl:cons-pointer-type expression)))
		(and (rtl:machine-constant? expression)
		     (zero? (rtl:machine-constant-value expression))))
	      (let ((expression (rtl:cons-pointer-datum expression)))
		(and (rtl:machine-constant? expression)
		     (zero? (rtl:machine-constant-value expression))))))
	(else false)))

;;;; Fixnums

(define-rule rewriting
  (OBJECT->FIXNUM (REGISTER (? source register-known-value)))
  (QUALIFIER (rtl:constant-fixnum? source))
  (rtl:make-object->fixnum source))

(define-rule rewriting
  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
		 (REGISTER (? operand-1 register-known-value))
		 (? operand-2)
		 (? overflow?))
  (QUALIFIER (rtl:constant-fixnum-test operand-1 (lambda (n) n true)))
  (rtl:make-fixnum-2-args 'MULTIPLY-FIXNUM operand-1 operand-2 overflow?))

(define-rule rewriting
  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
		 (? operand-1)
		 (REGISTER (? operand-2 register-known-value))
		 (? overflow?))
  (QUALIFIER
   (and (rtl:constant-fixnum-test operand-2 (lambda (n) n true))))
  (rtl:make-fixnum-2-args 'MULTIPLY-FIXNUM operand-1 operand-2 overflow?))

(define-rule rewriting
  (FIXNUM-2-ARGS (? operator)
		 (? operand-1)
		 (REGISTER (? operand-2 register-known-value))
		 (? overflow?))
  (QUALIFIER
   (and (memq operator '(PLUS-FIXNUM MINUS-FIXNUM))
	(rtl:register? operand-1)
	(rtl:constant-fixnum-test operand-2 zero?)))
  (rtl:make-fixnum-2-args operator operand-1 operand-2 overflow?))

(define-rule rewriting
  (FIXNUM-2-ARGS (? operator)
		 (? operand-1)
		 (REGISTER (? operand-2 register-known-value))
		 (? overflow?))
  (QUALIFIER
   (and (memq operator '(FIXNUM-QUOTIENT FIXNUM-REMAINDER))
	(rtl:register? operand-1)
	(rtl:constant-fixnum-test operand-2
	  (lambda (n)
	    (integer-power-of-2? (abs n))))))
  (rtl:make-fixnum-2-args operator operand-1 operand-2 overflow?))

(define-rule rewriting
  (FIXNUM-2-ARGS FIXNUM-LSH
		 (? operand-1)
		 (REGISTER (? operand-2 register-known-value))
		 #F)
  (QUALIFIER (and (rtl:register? operand-1)
		  (rtl:constant-fixnum-test operand-2 (lambda (n) n true))))
  (rtl:make-fixnum-2-args 'FIXNUM-LSH operand-1 operand-2 #F))

(define (rtl:constant-fixnum? expression)
  (and (rtl:constant? expression)
       (fix:fixnum? (rtl:constant-value expression))))

(define (rtl:constant-fixnum-test expression predicate)
  (and (rtl:object->fixnum? expression)
       (let ((expression (rtl:object->fixnum-expression expression)))
	 (and (rtl:constant? expression)
	      (let ((n (rtl:constant-value expression)))
		(and (fix:fixnum? n)
		     (predicate n)))))))

(define-rule rewriting
  (OBJECT->FLOAT (REGISTER (? operand register-known-value)))
  (QUALIFIER
   (rtl:constant-flonum-test operand
			     (lambda (v)
			       (or (flo:zero? v) (flo:one? v)))))
  (rtl:make-object->float operand))

(define-rule rewriting
  (FLONUM-2-ARGS FLONUM-SUBTRACT
		 (REGISTER (? operand-1 register-known-value))
		 (? operand-2)
		 (? overflow?))
  (QUALIFIER (rtl:constant-flonum-test operand-1 flo:zero?))
  (rtl:make-flonum-2-args 'FLONUM-SUBTRACT operand-1 operand-2 overflow?))

(define-rule rewriting
  (FLONUM-2-ARGS (? operation)
		 (REGISTER (? operand-1 register-known-value))
		 (? operand-2)
		 (? overflow?))
  (QUALIFIER
   (and (memq operation
	      '(FLONUM-ADD FLONUM-SUBTRACT FLONUM-MULTIPLY FLONUM-DIVIDE))
	(rtl:constant-flonum-test operand-1 flo:one?)))
  (rtl:make-flonum-2-args operation operand-1 operand-2 overflow?))

(define-rule rewriting
  (FLONUM-2-ARGS (? operation)
		 (? operand-1)
		 (REGISTER (? operand-2 register-known-value))
		 (? overflow?))
  (QUALIFIER
   (and (memq operation
	      '(FLONUM-ADD FLONUM-SUBTRACT FLONUM-MULTIPLY FLONUM-DIVIDE))
	(rtl:constant-flonum-test operand-2 flo:one?)))
  (rtl:make-flonum-2-args operation operand-1 operand-2 overflow?))

(define-rule rewriting
  (FLONUM-PRED-2-ARGS (? predicate)
		      (? operand-1)
		      (REGISTER (? operand-2 register-known-value)))
  (QUALIFIER (rtl:constant-flonum-test operand-2 flo:zero?))
  (list 'FLONUM-PRED-2-ARGS predicate operand-1 operand-2))

(define-rule rewriting
  (FLONUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? operand-1 register-known-value))
		      (? operand-2))
  (QUALIFIER (rtl:constant-flonum-test operand-1 flo:zero?))
  (list 'FLONUM-PRED-2-ARGS predicate operand-1 operand-2))

#|
;; These don't work as written.  They are not simplified and are
;; therefore passed whole to the back end, and there is no way to
;; construct the graph at this level.

;; acos (x) = atan ((sqrt (1 - x^2)) / x)

(define-rule add-pre-cse-rewriting-rule!
  (FLONUM-1-ARG FLONUM-ACOS (? operand) #f)
  (rtl:make-flonum-2-args
   'FLONUM-ATAN2
   (rtl:make-flonum-1-arg
    'FLONUM-SQRT
    (rtl:make-flonum-2-args
     'FLONUM-SUBTRACT
     (rtl:make-object->float (rtl:make-constant 1.))
     (rtl:make-flonum-2-args 'FLONUM-MULTIPLY operand operand false)
     false)
    false)
   operand
   false))

;; asin (x) = atan (x / (sqrt (1 - x^2)))

(define-rule add-pre-cse-rewriting-rule!
  (FLONUM-1-ARG FLONUM-ASIN (? operand) #f)
  (rtl:make-flonum-2-args
   'FLONUM-ATAN2
   operand
   (rtl:make-flonum-1-arg
    'FLONUM-SQRT
    (rtl:make-flonum-2-args
     'FLONUM-SUBTRACT
     (rtl:make-object->float (rtl:make-constant 1.))
     (rtl:make-flonum-2-args 'FLONUM-MULTIPLY operand operand false)
     false)
    false)
   false))

|#

(define (rtl:constant-flonum-test expression predicate)
  (and (rtl:object->float? expression)
       (let ((expression (rtl:object->float-expression expression)))
	 (and (rtl:constant? expression)
	      (let ((n (rtl:constant-value expression)))
		(and (flo:flonum? n)
		     (predicate n)))))))

(define (flo:one? value)
  (flo:= value 1.))

;;;; Indexed addressing modes

(define-rule rewriting
  (OFFSET (REGISTER (? base register-known-value))
	  (MACHINE-CONSTANT (? value)))
  (QUALIFIER (and (rtl:offset-address? base)
		  (rtl:simple-subexpressions? base)))
  (rtl:make-offset base (rtl:make-machine-constant value)))

(define-rule rewriting
  (BYTE-OFFSET (REGISTER (? base register-known-value))
	       (MACHINE-CONSTANT (? value)))
  (QUALIFIER (and (rtl:byte-offset-address? base)
		  (rtl:simple-subexpressions? base)))
  (rtl:make-byte-offset base (rtl:make-machine-constant value)))

(define-rule rewriting
  (FLOAT-OFFSET (REGISTER (? base register-known-value))
		(MACHINE-CONSTANT (? value)))
  (QUALIFIER (and (rtl:float-offset-address? base)
		  (rtl:simple-subexpressions? base)))
  (if (zero? value)
      (rtl:make-float-offset
       (rtl:float-offset-address-base base)
       (rtl:float-offset-address-offset base))
      (rtl:make-float-offset base (rtl:make-machine-constant value))))

(define-rule rewriting
  (FLOAT-OFFSET (REGISTER (? base register-known-value))
		(MACHINE-CONSTANT (? value)))
  (QUALIFIER
   (and (rtl:offset-address? base)
	(rtl:simple-subexpressions? base)
	(rtl:machine-constant? (rtl:offset-address-offset base))))   
  (rtl:make-float-offset base (rtl:make-machine-constant value)))

;; This is here to avoid generating things like
;;
;; (offset (offset-address (object->address (constant #(foo bar baz gack)))
;;                         (register 29))
;;         (machine-constant 1))
;;
;; since the offset-address subexpression is constant, and therefore
;; known!

(define (rtl:simple-subexpressions? expr)
  (for-all? (cdr expr)
    (lambda (sub)
      (or (rtl:machine-constant? sub)
	  (rtl:register? sub)))))


;; New rules added for Scheme 8.0

(define-rule add-pre-cse-rewriting-rule!
  (COERCE-VALUE-CLASS (? frob) (? class))
  class					; ignored
  (error "Unknown expression for " frob)
  frob)

(define-rule add-pre-cse-rewriting-rule!
  (COERCE-VALUE-CLASS (REGISTER (? frob register-known-expression)) (? class))
  class					; ignored
  frob)

(define-rule add-pre-cse-rewriting-rule!
  (PRED-1-ARG INDEX-FIXNUM? (? source))

  ;; This is a predicate so we can't use rtl:make-type-test

  (list 'TYPE-TEST (rtl:make-object->type source) (ucode-type positive-fixnum)))
