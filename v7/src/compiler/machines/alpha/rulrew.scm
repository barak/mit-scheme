#| -*-Scheme-*-

$Id: rulrew.scm,v 1.1 1992/08/29 13:51:35 jinx Exp $

Copyright (c) 1992 Digital Equipment Corporation (D.E.C.)

This software was developed at the Digital Equipment Corporation
Cambridge Research Laboratory.  Permission to copy this software, to
redistribute it, and to use it for any purpose is granted, subject to
the following restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to both the Digital Equipment Corporation Cambridge Research
Lab (CRL) and the MIT Scheme project any improvements or extensions
that they make, so that these may be included in future releases; and
(b) to inform CRL and MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. D.E.C. has made no warrantee or representation that the operation
of this software will be error-free, and D.E.C. is under no obligation
to provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Digital Equipment Corporation
nor of any adaptation thereof in any advertising, promotional, or
sales literature without prior written consent from D.E.C. in each
case.

|#

;;;; RTL Rewrite Rules
;;; Package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Synthesized Data

(define-rule rewriting
  (CONS-NON-POINTER (REGISTER (? type register-known-value))
		    (REGISTER (? datum register-known-value)))
  (QUALIFIER (and (rtl:machine-constant? type)
		  (rtl:machine-constant? datum)))
  (rtl:make-cons-non-pointer type datum))

(define-rule rewriting
  (CONS-POINTER (REGISTER (? type register-known-value)) (? datum))
  (QUALIFIER
   (and (rtl:object->type? type)
	(rtl:constant? (rtl:object->type-expression type))))
  (rtl:make-cons-pointer
   (rtl:make-machine-constant
    (object-type (rtl:object->type-expression datum)))
   datum))

(define-rule rewriting
  (CONS-POINTER (REGISTER (? type register-known-value)) (? datum))
  (QUALIFIER (rtl:machine-constant? type))
  (rtl:make-cons-pointer type datum))

(define-rule rewriting
  (CONS-NON-POINTER (REGISTER (? type register-known-value)) (? datum))
  (QUALIFIER (rtl:machine-constant? type))
  (rtl:make-cons-non-pointer type datum))

(define-rule rewriting
  (CONS-NON-POINTER (REGISTER (? type register-known-value)) (? datum))
  (QUALIFIER
   (and (rtl:object->type? type)
	(rtl:constant? (rtl:object->type-expression type))))
  (rtl:make-cons-non-pointer
   (rtl:make-machine-constant
    (object-type (rtl:object->type-expression datum)))
   datum))

(define-rule rewriting
  (CONS-NON-POINTER (? type) (REGISTER (? datum register-known-value)))
  (QUALIFIER
   (and (rtl:object->datum? datum)
	(rtl:constant-non-pointer? (rtl:object->datum-expression datum))))
  (rtl:make-cons-non-pointer
   type
   (rtl:make-machine-constant
    (careful-object-datum (rtl:object->datum-expression datum)))))

(define-rule rewriting
  (OBJECT->TYPE (REGISTER (? source register-known-value)))
  (QUALIFIER (rtl:constant? source))
  (rtl:make-machine-constant (object-type (rtl:constant-value source))))

(define-rule rewriting
  (OBJECT->DATUM (REGISTER (? source register-known-value)))
  (QUALIFIER (rtl:constant-non-pointer? source))
  (rtl:make-machine-constant (careful-object-datum source)))

(define (rtl:constant-non-pointer? expression)
  (and (rtl:constant? expression)
       (non-pointer-object? (rtl:constant-value expression))))

;;; These rules are losers because there's no abstract way to cons a
;;; statement or a predicate without also getting some CFG structure.

(define-rule rewriting
  (ASSIGN (? target) (REGISTER (? comparand register-known-value)))
  (QUALIFIER (rtl:immediate-zero-constant? comparand))
  (list 'ASSIGN target (rtl:make-machine-register regnum:zero)))

(define-rule rewriting
  (EQ-TEST (? source) (REGISTER (? comparand register-known-value)))
  (QUALIFIER (rtl:immediate-zero-constant? comparand))
  (list 'EQ-TEST source (rtl:make-machine-register regnum:zero)))

(define-rule rewriting
  (EQ-TEST (REGISTER (? comparand register-known-value)) (? source))
  (QUALIFIER (rtl:immediate-zero-constant? comparand))
  (list 'EQ-TEST source (rtl:make-machine-register regnum:zero)))

(define (rtl:immediate-zero-constant? expression)
  (cond ((rtl:constant? expression)
	 (let ((value (rtl:constant-value expression)))
	   (and (non-pointer-object? value)
		(zero? (object-type value))
		(zero? (careful-object-datum value)))))
	((rtl:cons-non-pointer? expression)
	 (and (let ((expression (rtl:cons-non-pointer-type expression)))
		(and (rtl:machine-constant? expression)
		     (zero? (rtl:machine-constant-value expression))))
	      (let ((expression (rtl:cons-non-pointer-datum expression)))
		(and (rtl:machine-constant? expression)
		     (zero? (rtl:machine-constant-value expression))))))
	(else false)))

;;;; Fixnums

;; I've copied this rule from the MC68020.  -- Jinx
;; It should probably be qualified to be in the immediate range.

(define-rule rewriting
  (OBJECT->FIXNUM (REGISTER (? source register-known-value)))
  (QUALIFIER (rtl:constant-fixnum? source))
  (rtl:make-object->fixnum source))

(define-rule rewriting
  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
		 (REGISTER (? operand-1 register-known-value))
		 (? operand-2)
		 #F)
  (QUALIFIER (rtl:constant-fixnum-4? operand-1))
  (rtl:make-fixnum-2-args 'MULTIPLY-FIXNUM operand-1 operand-2 #F))

(define-rule rewriting
  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
		 (? operand-1)
		 (REGISTER (? operand-2 register-known-value))
		 #F)
  (QUALIFIER (rtl:constant-fixnum-4? operand-2))
  (rtl:make-fixnum-2-args 'MULTIPLY-FIXNUM operand-1 operand-2 #F))

(define-rule rewriting
  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
		 (REGISTER (? operand-1 register-known-value))
		 (? operand-2)
		 #F)
  (QUALIFIER
   (and (rtl:object->fixnum-of-register? operand-1)
	(rtl:constant-fixnum-4? operand-2)))
  (rtl:make-fixnum-2-args 'MULTIPLY-FIXNUM operand-1 operand-2 #F))

(define-rule rewriting
  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
		 (? operand-1)
		 (REGISTER (? operand-2 register-known-value))
		 #F)
  (QUALIFIER
   (and (rtl:constant-fixnum-4? operand-1)
	(rtl:object->fixnum-of-register? operand-2)))
  (rtl:make-fixnum-2-args 'MULTIPLY-FIXNUM operand-1 operand-2 #F))

(define (rtl:constant-fixnum? expression)
  (and (rtl:constant? expression)
       (fix:fixnum? (rtl:constant-value expression))))

(define (rtl:constant-fixnum-4? expression)
  (and (rtl:object->fixnum? expression)
       (let ((expression (rtl:object->fixnum-expression expression)))
	 (and (rtl:constant? expression)
	      (eqv? 4 (rtl:constant-value expression))))))

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
;; always contains an address (ie. free pointer, stack pointer, or dlink register)

(define-rule rewriting
  (CONS-POINTER (REGISTER (? type register-known-value))
		(REGISTER (? datum machine-address-register)))
  (QUALIFIER (and (rtl:machine-constant? type)
		  (spectrum-type-optimizable? (rtl:machine-constant-value type))))
  (rtl:make-cons-pointer type datum))
|#


	     