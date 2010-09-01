#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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
  ;; On x86, there's no difference between an address and a datum,
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
  (OBJECT->FIXNUM (REGISTER (? source register-known-value)))
  (QUALIFIER (rtl:cons-non-pointer? source))
  (rtl:make-object->fixnum (rtl:cons-non-pointer-datum source)))

(define-rule rewriting
  (OBJECT->FIXNUM (REGISTER (? source register-known-value)))
  (QUALIFIER (rtl:object->datum? source))
  (rtl:make-object->fixnum (rtl:object->datum-expression source)))

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
       (fix:fixnum? (rtl:constant-value expression))
       (rtl:constant-value expression)))

(define (rtl:constant-fixnum-test expression predicate)
  (and (rtl:object->fixnum? expression)
       (let ((expression (rtl:object->fixnum-expression expression)))
	 (and (rtl:constant? expression)
	      (let ((n (rtl:constant-value expression)))
		(and (fix:fixnum? n)
		     (predicate n)))))))

(define-rule rewriting
  (OBJECT->FLOAT (REGISTER (? operand register-known-value)))
  ;; This is not quite what we want.  We really want to rewrite all
  ;; OBJECT->FLOAT expressions with known constant operands, not just
  ;; the nonzero ones, and then decide later whether to put it in
  ;; memory based on whether there is a temporary register that we can
  ;; zero with XOR.  By not rewriting this case when the constant is
  ;; zero, using a temporary may cause some other register to be
  ;; written to memory, which defeats the purpose of using XOR to
  ;; avoid memory access.
  (QUALIFIER (rtl:constant-flonum-test operand flo:nonzero?))
  (rtl:make-object->float operand))

(define-rule rewriting
  (FLONUM-2-ARGS FLONUM-SUBTRACT
                 (REGISTER (? operand1 register-known-value))
                 (? operand2)
                 (? overflow?))
  (QUALIFIER (rtl:constant-flonum-test operand1 flo:zero?))
  (rtl:make-flonum-1-arg 'FLONUM-NEGATE operand2 overflow?))

(define (rtl:constant-flonum-test expression predicate)
  (and (rtl:object->float? expression)
       (let ((expression (rtl:object->float-expression expression)))
	 (and (rtl:constant? expression)
	      (let ((n (rtl:constant-value expression)))
		(and (flo:flonum? n)
		     (predicate n)))))))

(define-integrable (flo:nonzero? value)
  (not (flo:= value 0.)))

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


