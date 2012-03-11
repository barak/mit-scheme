#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

;;;; RTL Rewrite Rules
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Synthesized Data

(define-rule rewriting
  (CONS-NON-POINTER (? type) (? datum))
  ;; Since we use DEP instructions to insert type codes, there's no
  ;; difference between the way that pointers and non-pointers are
  ;; constructed.
  (rtl:make-cons-pointer type datum))

(define-rule rewriting
  (CONS-POINTER (REGISTER (? type register-known-value))
		(REGISTER (? datum register-known-value)))
  (QUALIFIER (and (rtl:machine-constant? type)
		  (rtl:machine-constant? datum)))
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
  (QUALIFIER (and (rtl:object->datum? datum)
		  (not (rtl:constant-non-pointer?
			(rtl:object->datum-expression datum)))))
  ;; Since we use DEP/DEPI, there is no need to clear the old bits
  (rtl:make-cons-pointer type (rtl:object->datum-expression datum)))

(define-rule rewriting
  (OBJECT->TYPE (REGISTER (? source register-known-value)))
  (QUALIFIER (rtl:constant? source))
  (rtl:make-machine-constant (object-type (rtl:constant-value source))))

(define-rule rewriting
  (OBJECT->DATUM (REGISTER (? source register-known-value)))
  (QUALIFIER (rtl:constant-non-pointer? source))
  (rtl:make-machine-constant (careful-object-datum (rtl:constant-value source))))

(define (rtl:constant-non-pointer? expression)
  (and (rtl:constant? expression)
       (non-pointer-object? (rtl:constant-value expression))))

;;; These rules are losers because there's no abstract way to cons a
;;; statement or a predicate without also getting some CFG structure.

(define-rule rewriting
  ;; Use register 0, always 0.
  (ASSIGN (? target) (REGISTER (? comparand register-known-value)))
  (QUALIFIER (rtl:immediate-zero-constant? comparand))
  (list 'ASSIGN target (rtl:make-machine-constant 0)))

(define-rule rewriting
  ;; Compare to register 0, always 0.
  (EQ-TEST (? source) (REGISTER (? comparand register-known-value)))
  (QUALIFIER (rtl:immediate-zero-constant? comparand))
  (list 'EQ-TEST source (rtl:make-machine-constant 0)))

(define-rule rewriting
  ;; Compare to register 0, always 0.
  (EQ-TEST (REGISTER (? comparand register-known-value)) (? source))
  (QUALIFIER (rtl:immediate-zero-constant? comparand))
  (list 'EQ-TEST source (rtl:make-machine-constant 0)))

(define (rtl:immediate-zero-constant? expression)
  (cond ((rtl:constant? expression)
	 (let ((value (rtl:constant-value expression)))
	   (and (non-pointer-object? value)
		(zero? (object-type value))
		(zero? (careful-object-datum value)))))
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
  (FIXNUM-2-ARGS FIXNUM-LSH
		 (? operand-1)
		 (REGISTER (? operand-2 register-known-value))
		 #F)
  (QUALIFIER (and (rtl:register? operand-1)
		  (rtl:constant-fixnum-test operand-2 (lambda (n) n true))))
  (rtl:make-fixnum-2-args 'FIXNUM-LSH operand-1 operand-2 #F))

(define-rule rewriting
  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
		 (REGISTER (? operand-1 register-known-value))
		 (? operand-2)
		 #F)
  (QUALIFIER (and (rtl:register? operand-2)
		  (rtl:constant-fixnum-test operand-1 spectrum-inline-multiply?)))
  (rtl:make-fixnum-2-args 'MULTIPLY-FIXNUM operand-1 operand-2 #F))

(define-rule rewriting
  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
		 (? operand-1)
		 (REGISTER (? operand-2 register-known-value))
		 #F)
  (QUALIFIER (and (rtl:register? operand-1)
		  (rtl:constant-fixnum-test operand-2 spectrum-inline-multiply?)))
  (rtl:make-fixnum-2-args 'MULTIPLY-FIXNUM operand-1 operand-2 #F))

(define (spectrum-inline-multiply? n)
  #|
  (let ((absn (abs n)))
    (and (integer-log-base-2? absn)
	 (<= absn 64)))
  |#
  n					; fnord
  true)

(define-rule rewriting
  (FIXNUM-2-ARGS FIXNUM-QUOTIENT
		 (? operand-1)
		 (REGISTER (? operand-2 register-known-value))
		 #F)
  (QUALIFIER (and (rtl:register? operand-1)
		  (rtl:constant-fixnum-test
		   operand-2
		   (lambda (n)
		     (integer-log-base-2? (abs n))))))
  (rtl:make-fixnum-2-args 'FIXNUM-QUOTIENT operand-1 operand-2 #F))

(define-rule rewriting
  (FIXNUM-2-ARGS FIXNUM-REMAINDER
		 (? operand-1)
		 (REGISTER (? operand-2 register-known-value))
		 #F)
  (QUALIFIER (and (rtl:register? operand-1)
		  (rtl:constant-fixnum-test
		   operand-2
		   (lambda (n)
		     (integer-log-base-2? (abs n))))))
  (rtl:make-fixnum-2-args 'FIXNUM-REMAINDER operand-1 operand-2 #F))

;; These are used by vector-ref and friends with computed indices.

(define-rule rewriting
  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
		 (REGISTER (? operand-1 register-known-value))
		 (REGISTER (? operand-2 register-known-value))
		 #F)
  (QUALIFIER
   (and (rtl:object->fixnum-of-register? operand-1)
	(rtl:constant-fixnum-test
	 operand-2
	 (lambda (n)
	   (integer-log-base-2? n)))))
  (rtl:make-fixnum-2-args 'MULTIPLY-FIXNUM operand-1 operand-2 #F))

(define-rule rewriting
  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
		 (REGISTER (? operand-1 register-known-value))
		 (REGISTER (? operand-2 register-known-value))
		 #F)
  (QUALIFIER
   (and (rtl:constant-fixnum-test
	 operand-1
	 (lambda (n)
	   (integer-log-base-2? n)))
	(rtl:object->fixnum-of-register? operand-2)))
  (rtl:make-fixnum-2-args 'MULTIPLY-FIXNUM operand-1 operand-2 #F))

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

(define (rtl:object->fixnum-of-register? expression)
   (and (rtl:object->fixnum? expression)
	(rtl:register? (rtl:object->fixnum-expression expression))))

;;;; Closures and othe optimizations.  

;; These rules are Spectrum specific

(define-rule rewriting
  (CONS-POINTER (REGISTER (? type register-known-value))
		(REGISTER (? datum register-known-value)))
  (QUALIFIER (and (rtl:machine-constant? type)
		  (= (rtl:machine-constant-value type)
		     (ucode-type compiled-entry))
		  (or (rtl:entry:continuation? datum)
		      (rtl:entry:procedure? datum)
		      (rtl:cons-closure? datum))))
  (rtl:make-cons-pointer type datum))

#|
;; Not yet written.

;; A type is compatible when a depi instruction can put it in assuming that
;; the datum has the quad bits set.
;; A register is a machine-address-register if it is a machine register and
;; always contains an address (ie. free pointer, stack pointer,
;; or dlink register)

(define-rule rewriting
  (CONS-POINTER (REGISTER (? type register-known-value))
		(REGISTER (? datum machine-address-register)))
  (QUALIFIER
   (and (rtl:machine-constant? type)
	(spectrum-type-optimizable? (rtl:machine-constant-value type))))
  (rtl:make-cons-pointer type datum))
|#

(define-rule rewriting
  (FLOAT-OFFSET (REGISTER (? base register-known-value))
		(MACHINE-CONSTANT 0))
  (QUALIFIER (rtl:simple-float-offset-address? base))
  (rtl:make-float-offset (rtl:float-offset-address-base base)
			 (rtl:float-offset-address-offset base)))

;; This is here to avoid generating things like
;;
;; (float-offset (offset-address (object->address (constant #(foo bar baz gack)))
;; 			         (machine-constant 1))
;; 	         (register 84))
;;
;; since the offset-address subexpression is constant, and therefore
;; known!

(define (rtl:simple-float-offset-address? expr)
  (and (rtl:float-offset-address? expr)
       (let ((offset (rtl:float-offset-address-offset expr)))
	 (or (rtl:machine-constant? offset)
	     (rtl:register? offset)
	     (and (rtl:object->datum? offset)
		  (rtl:register? (rtl:object->datum-expression offset)))))
       (let ((base (rtl:float-offset-address-base expr)))
	 (or (rtl:register? base)
	     (and (rtl:offset-address? base)
		  (let ((base* (rtl:offset-address-base base))
			(offset* (rtl:offset-address-offset base)))
		    (and (rtl:machine-constant? offset*)
			 (or (rtl:register? base*)
			     (and (rtl:object->address? base*)
				  (rtl:register?
				   (rtl:object->address-expression
				    base*)))))))))))