#| -*-Scheme-*-

$Id: rulrew.scm,v 1.6 2007/01/05 15:33:05 cph Exp $

Copyright (c) 1990-1999 Massachusetts Institute of Technology

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
  ;; On the VAX, there's no difference between an address and a datum,
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
    (careful-object-datum
     (rtl:constant-value (rtl:object->datum-expression datum))))))

(define-rule rewriting
  (OBJECT->TYPE (REGISTER (? source register-known-value)))
  (QUALIFIER (rtl:constant? source))
  (rtl:make-machine-constant (object-type (rtl:constant-value source))))

(define-rule rewriting
  (OBJECT->DATUM (REGISTER (? source register-known-value)))
  (QUALIFIER (rtl:constant-non-pointer? source))
  (rtl:make-machine-constant
   (careful-object-datum (rtl:constant-value source))))

(define (rtl:constant-non-pointer? expression)
  (and (rtl:constant? expression)
       (non-pointer-object? (rtl:constant-value expression))))

;;; These rules are losers because there's no abstract way to cons a
;;; statement or a predicate without also getting some CFG structure.

;;; Shouldn't these rules use (rtl:make-machine-constant 0)
;;; rather than comparand?  Of course, there would have to
;;; be more translation rules, but... -- Jinx

(define-rule rewriting
  ;; CLR instruction
  (ASSIGN (? target) (REGISTER (? comparand register-known-value)))
  (QUALIFIER (rtl:immediate-zero-constant? comparand))
  (list 'ASSIGN target comparand))

(define-rule rewriting
  ;; TST instruction
  (EQ-TEST (? source) (REGISTER (? comparand register-known-value)))
  (QUALIFIER (rtl:immediate-zero-constant? comparand))
  (list 'EQ-TEST source comparand))

(define-rule rewriting
  ;; TSTL instruction
  (EQ-TEST (REGISTER (? comparand register-known-value)) (? source))
  (QUALIFIER (rtl:immediate-zero-constant? comparand))
  (list 'EQ-TEST source comparand))

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
  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
		 (REGISTER (? operand-1 register-known-value))
		 (? operand-2)
		 (? overflow?))
  (QUALIFIER
   (rtl:constant-fixnum-test operand-1
     (lambda (n)
       (or (zero? n)
	   (integer-power-of-2? n)))))
  (rtl:make-fixnum-2-args 'MULTIPLY-FIXNUM operand-1 operand-2 overflow?))

(define-rule rewriting
  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
		 (? operand-1)
		 (REGISTER (? operand-2 register-known-value))
		 (? overflow?))
  (QUALIFIER
   (rtl:constant-fixnum-test operand-2
     (lambda (n)
       (or (zero? n)
	   (= -1 n)
	   (integer-power-of-2? n)))))
  (rtl:make-fixnum-2-args 'MULTIPLY-FIXNUM operand-1 operand-2 overflow?))

(define-rule rewriting
  (FIXNUM-2-ARGS (? operator)
		 (? operand-1)
		 (REGISTER (? operand-2 register-known-value))
		 (? overflow?))
  (QUALIFIER
   (and (memq operator '(PLUS-FIXNUM MINUS-FIXNUM))
	(rtl:constant-fixnum-test operand-2 zero?)))
  (rtl:make-fixnum-2-args operator operand-1 operand-2 overflow?))

(define-rule rewriting
  (FIXNUM-2-ARGS (? operator)
		 (? operand-1)
		 (REGISTER (? operand-2 register-known-value))
		 (? overflow?))
  (QUALIFIER
   (and (memq operator '(FIXNUM-QUOTIENT FIXNUM-REMAINDER))
	(rtl:constant-fixnum-test operand-2
	  (lambda (n)
	    (or (= -1 n)
		(integer-power-of-2? n))))))
  (rtl:make-fixnum-2-args operator operand-1 operand-2 overflow?))

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