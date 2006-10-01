#| -*-Scheme-*-

$Id: rulfix.scm,v 1.9 2006/10/01 05:38:32 cph Exp $

Copyright 1993,2001,2002,2006 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; LAP Generation Rules: Fixnum Rules
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Conversions

(define (object->fixnum source target)
  (LAP ,(c:= target (c:ecall "FIXNUM_TO_LONG" source))))

(define (address->fixnum source target)
  (LAP ,(c:= target (c:ecall "ADDRESS_TO_LONG" source))))

(define (fixnum->object source target)
  (LAP ,(c:= target (c:ecall "LONG_TO_FIXNUM" source))))

(define (fixnum->address source target)
  (LAP ,(c:= target (c:ecall "LONG_TO_ADDRESS" source))))

(define-rule statement
  ;; convert a fixnum object to a "fixnum integer"
  (ASSIGN (REGISTER (? target)) (OBJECT->FIXNUM (REGISTER (? source))))
  (standard-unary-conversion source 'SCHEME_OBJECT target 'LONG
			     object->fixnum))

(define-rule statement
  ;; load a fixnum constant as a "fixnum integer"
  (ASSIGN (REGISTER (? target)) (OBJECT->FIXNUM (CONSTANT (? constant))))
  (let ((target (standard-target! target 'LONG)))
    (LAP ,(c:= target (longify constant)))))

(define-rule statement
  ;; convert a memory address to a "fixnum integer"
  (ASSIGN (REGISTER (? target)) (ADDRESS->FIXNUM (REGISTER (? source))))
  (standard-unary-conversion source 'SCHEME_OBJECT* target 'LONG
			     address->fixnum))

(define-rule statement
  ;; convert an object's address to a "fixnum integer"
  (ASSIGN (REGISTER (? target))
	  (ADDRESS->FIXNUM (OBJECT->ADDRESS (REGISTER (? source)))))
  (standard-unary-conversion source 'SCHEME_OBJECT target 'LONG
			     object->fixnum))

(define-rule statement
  ;; convert a "fixnum integer" to a fixnum object
  (ASSIGN (REGISTER (? target)) (FIXNUM->OBJECT (REGISTER (? source))))
  (standard-unary-conversion source 'LONG target 'SCHEME_OBJECT
			     fixnum->object))

(define-rule statement
  ;; convert a "fixnum integer" to a memory address
  (ASSIGN (REGISTER (? target)) (FIXNUM->ADDRESS (REGISTER (? source))))
  (standard-unary-conversion source 'LONG target 'SCHEME_OBJECT*
			     fixnum->address))

;; "Fixnum" in this context means a C long

(define (no-overflow-branches!)
  (set-current-branches! (lambda (label) label (LAP))
			 (lambda (label) (LAP ,(c:goto label)))))

(define (standard-overflow-branches! overflow? result)
  (if overflow?
      (branch-on-expr (c:! (c:ecall "LONG_TO_FIXNUM_P" result)))))

(define (guarantee-signed-fixnum n)
  (if (not (signed-fixnum? n)) (error "Not a signed fixnum" n))
  n)

(define (signed-fixnum? n)
  (and (exact-integer? n)
       (>= n signed-fixnum/lower-limit)
       (< n signed-fixnum/upper-limit)))

;;;; Arithmetic Operations

(define-rule statement
  ;; execute a unary fixnum operation
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-1-ARG (? operation)
			(REGISTER (? source))
			(? overflow?)))
  (standard-unary-conversion source 'LONG target 'LONG
    (lambda (source target)
      ((fixnum-1-arg/operator operation) target source overflow?))))

(define (fixnum-1-arg/operator operation)
  (lookup-arithmetic-method operation fixnum-methods/1-arg))

(define fixnum-methods/1-arg
  (list 'FIXNUM-METHODS/1-ARG))

(define-arithmetic-method 'ONE-PLUS-FIXNUM fixnum-methods/1-arg
  (lambda (tgt src overflow?)
    (fixnum-add-constant tgt src 1 overflow?)))

(define-arithmetic-method 'MINUS-ONE-PLUS-FIXNUM fixnum-methods/1-arg
  (lambda (tgt src overflow?)
    (fixnum-add-constant tgt src -1 overflow?)))

(define (fixnum-add-constant tgt src constant overflow?)
  (standard-overflow-branches! overflow? tgt)
  (LAP ,(c:= tgt
	     (cond ((back-end:= constant 0)
		    src)
		   ((and (number? constant) (< constant 0))
		    (c:- src (longify (- constant))))
		   (else
		    (c:+ src (longify constant)))))))

(define-arithmetic-method 'FIXNUM-NOT fixnum-methods/1-arg
  (lambda (tgt src1 overflow?)
    (if overflow? (no-overflow-branches!))
    (LAP ,(c:= tgt (c:~ src1)))))

(define-rule statement
  ;; execute a binary fixnum operation
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS (? operation)
			 (REGISTER (? source1))
			 (REGISTER (? source2))
			 (? overflow?)))
  (standard-binary-conversion source1 'LONG source2 'LONG target 'LONG
    (lambda (source1 source2 target)
      ((fixnum-2-args/operator operation) target source1 source2 overflow?))))

(define (fixnum-2-args/operator operation)
  (lookup-arithmetic-method operation fixnum-methods/2-args))

(define fixnum-methods/2-args
  (list 'FIXNUM-METHODS/2-ARGS))

(define-arithmetic-method 'FIXNUM-AND fixnum-methods/2-args
  (lambda (tgt src1 src2 overflow?)
    (if overflow? (no-overflow-branches!))
    (LAP ,(c:= tgt (c:& src1 src2)))))

(define-arithmetic-method 'FIXNUM-OR fixnum-methods/2-args
  (lambda (tgt src1 src2 overflow?)
    (if overflow? (no-overflow-branches!))
    (LAP ,(c:= tgt (c:\| src1 src2)))))

(define-arithmetic-method 'FIXNUM-XOR fixnum-methods/2-args
  (lambda (tgt src1 src2 overflow?)
    (if overflow? (no-overflow-branches!))
    (LAP ,(c:= tgt (c:^ src1 src2)))))

(define-arithmetic-method 'FIXNUM-ANDC fixnum-methods/2-args
  (lambda (tgt src1 src2 overflow?)
    (if overflow? (no-overflow-branches!))
    (LAP ,(c:= tgt (c:&~ src1 src2)))))

(define-arithmetic-method 'FIXNUM-REMAINDER fixnum-methods/2-args
  (lambda (tgt src1 src2 overflow?)
    (if overflow? (no-overflow-branches!))
    (LAP ,(c:= tgt (c:ecall "FIXNUM_REMAINDER" src1 src2)))))

(define-arithmetic-method 'FIXNUM-LSH fixnum-methods/2-args
  (lambda (tgt src1 src2 overflow?)
    (if overflow? (no-overflow-branches!))
    (LAP ,(c:= tgt (c:ecall "FIXNUM_LSH" src1 src2)))))

(define-arithmetic-method 'FIXNUM-QUOTIENT fixnum-methods/2-args
  (lambda (tgt src1 src2 overflow?)
    (standard-overflow-branches! overflow? tgt)
    (LAP ,(c:= tgt (c:ecall "FIXNUM_QUOTIENT" src1 src2)))))

(define-arithmetic-method 'PLUS-FIXNUM fixnum-methods/2-args
  (lambda (tgt src1 src2 overflow?)
    (standard-overflow-branches! overflow? tgt)
    (LAP ,(c:= tgt (c:+ src1 src2)))))

(define-arithmetic-method 'MINUS-FIXNUM fixnum-methods/2-args
  (lambda (tgt src1 src2 overflow?)
    (if overflow?
	(if (eqv? src1 src2)		;probably won't ever happen.
	    (begin
	      (no-overflow-branches!)
	      ; we don't use zero directly because we care about the tag
	      (LAP ,(c:= tgt (c:- src2 src2))))
	    (do-overflow-subtraction tgt src1 src2))
	(LAP ,(c:= tgt (c:- src1 src2))))))

(define (do-overflow-subtraction tgt src1 src2)
  (standard-overflow-branches! true tgt)
  (LAP ,(c:= tgt (c:- src1 src2))))

(define-arithmetic-method 'MULTIPLY-FIXNUM fixnum-methods/2-args
  (lambda (target src1 src2 overflow?)
    (if (not overflow?)
	(LAP ,(c:= target (c:* src1 src2)))
	(overflow-product! target src1 src2))))

(define (overflow-product! target src1 src2)
  (branch-on-expr (c:ecall "multiply_with_overflow" src1 src2 (c:& target))))

(define-rule statement
  ;; execute binary fixnum operation with constant second arg
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS (? operation)
			 (REGISTER (? source))
			 (OBJECT->FIXNUM (CONSTANT (? constant)))
			 (? overflow?)))
  (standard-unary-conversion source 'LONG target 'LONG
    (lambda (source target)
      ((fixnum-2-args/operator/register*constant operation)
       target source constant overflow?))))

(define-rule statement
  ;; execute binary fixnum operation with constant first arg
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS (? operation)
			 (OBJECT->FIXNUM (CONSTANT (? constant)))
			 (REGISTER (? source))
			 (? overflow?)))
  (QUALIFIER (not (memq operation
			'(FIXNUM-QUOTIENT FIXNUM-REMAINDER FIXNUM-LSH))))
  (standard-unary-conversion source 'LONG target 'LONG
    (lambda (source target)
      (if (fixnum-2-args/commutative? operation)
	  ((fixnum-2-args/operator/register*constant operation)
	   target source constant overflow?)
	  ((fixnum-2-args/operator/constant*register operation)
	   target constant source overflow?)))))

(define (fixnum-2-args/commutative? operator)
  (memq operator
	'(PLUS-FIXNUM MULTIPLY-FIXNUM FIXNUM-AND FIXNUM-OR FIXNUM-XOR)))

(define (fixnum-2-args/operator/register*constant operation)
  (lookup-arithmetic-method operation fixnum-methods/2-args/register*constant))

(define fixnum-methods/2-args/register*constant
  (list 'FIXNUM-METHODS/2-ARGS/REGISTER*CONSTANT))

(define (fixnum-2-args/operator/constant*register operation)
  (lookup-arithmetic-method operation
			    fixnum-methods/2-args/constant*register))

(define fixnum-methods/2-args/constant*register
  (list 'FIXNUM-METHODS/2-ARGS/CONSTANT*REGISTER))

(define-arithmetic-method 'PLUS-FIXNUM
  fixnum-methods/2-args/register*constant
  (lambda (tgt src constant overflow?)
    (guarantee-signed-fixnum constant)
    (fixnum-add-constant tgt src constant overflow?)))

(define-arithmetic-method 'MINUS-FIXNUM
  fixnum-methods/2-args/register*constant
  (lambda (tgt src constant overflow?)
    (guarantee-signed-fixnum constant)
    (fixnum-add-constant tgt src
			 (back-end:- 0 constant)
			 overflow?)))

(define (power-of-2? value)
  (let loop ((n value))
    (and (> n 0)
	 (if (= n 1)
	     0
	     (and (even? n)
		  (let ((m (loop (quotient n 2))))
		    (and m
			 (+ m 1))))))))

(define-arithmetic-method 'MULTIPLY-FIXNUM
  fixnum-methods/2-args/register*constant
  (lambda (tgt src constant overflow?)
    (cond ((back-end:= constant 0)
	   (if overflow? (no-overflow-branches!))
	   (LAP ,(c:= tgt (c:cast 'long 0))))
	  ((back-end:= constant 1)
	   (if overflow? (no-overflow-branches!))
	   (LAP ,(c:= tgt src)))
	  ((and (number? constant)
		(power-of-2? (abs constant)))
	   =>
	   (lambda (power-of-two)
	     (if (not overflow?)
		 (LAP ,(c:= tgt
			    (let ((shift
				   (c:ecall "LEFT_SHIFT" src power-of-two)))
			      (if (< constant 0)
				  (c:- shift)
				  shift))))
		 (overflow-product! tgt src constant))))
	  ((not overflow?)
	   (LAP ,(c:= tgt (c:* src (longify constant)))))
	  (else
	   (overflow-product! tgt src constant)))))

(define-arithmetic-method 'MINUS-FIXNUM
  fixnum-methods/2-args/constant*register
  (lambda (tgt constant src overflow?)
    (guarantee-signed-fixnum constant)
    (if overflow?
	(do-overflow-subtraction tgt constant src)
	(LAP ,(c:= tgt (c:- (longify constant) src))))))

(define-arithmetic-method 'FIXNUM-QUOTIENT
  fixnum-methods/2-args/register*constant
  (lambda (tgt src constant overflow?)
    (cond ((back-end:= constant 0)
	   (error "fixnum-quotient constant division by zero."))
	  ((back-end:= constant 1)
	   (if overflow? (no-overflow-branches!))
	   (LAP ,(c:= tgt src)))
	  ((back-end:= constant -1)
	   (standard-overflow-branches! overflow? tgt)
	   (LAP ,(c:= tgt (c:- src))))
	  ((and (number? constant)
		(power-of-2? (abs constant)))
	   =>
	   (lambda (power-of-two)
	     (if overflow?
		 (no-overflow-branches!))
	     (LAP ,(c:= tgt
			(let ((shift
			       (c:?: (c:< src 0)
				     (c:ecall "RIGHT_SHIFT"
					      (c:+ src (- (abs constant) 1))
					      power-of-two)
				     (c:ecall "RIGHT_SHIFT"
					      src
					      power-of-two))))
			  (if (< constant 0)
			      (c:- shift)
			      shift))))))
	  (else
	   (standard-overflow-branches! overflow? tgt)
	   (LAP ,(c:= tgt
		      (c:ecall "FIXNUM_QUOTIENT" src (longify constant))))))))

(define-arithmetic-method 'FIXNUM-REMAINDER
  fixnum-methods/2-args/register*constant
  (lambda (tgt src s-constant overflow?)
    (let ((constant (abs s-constant)))
      (if overflow? (no-overflow-branches!))
      (cond ((back-end:= constant 0)
	     (error "fixnum-remainder constant division by zero."))
	    ((back-end:= constant 1)
	     (LAP ,(c:= tgt 0)))
	    ((and (number? constant)
		  (power-of-2? constant))
	     =>
	     (lambda (power-of-two)
	       (LAP ,(c:brace-group
		      (c:decl 'long 'temp
			      (c:& src (c:cast 'long (- constant 1))))
		      (c:= tgt
			   (c:?: (c:>= src 0)
				 'temp
				 (c:== 'temp 0)
				 0
				 (c:\| 'temp
				       (c:ecall "LEFT_SHIFT"
						(c:cast 'long -1)
						power-of-two))))))))
	    (else
	     (LAP ,(c:= tgt
			(c:ecall "FIXNUM_REMAINDER"
				 src
				 (longify constant)))))))))

(define-arithmetic-method 'FIXNUM-LSH
  fixnum-methods/2-args/register*constant
  (lambda (tgt src constant overflow?)
    (cond (overflow? 
	   (error "fixnum-lsh overflow what??"))
	  ((back-end:= constant 0)
	   (LAP ,(c:= tgt src)))
	  ((not (number? constant))
	   (LAP ,(c:= tgt (c:ecall "FIXNUM_LSH" src constant))))
	  ((positive? constant)
	   (LAP ,(c:= tgt (c:ecall "LEFT_SHIFT" src constant))))
	  (else
	   (LAP ,(c:= tgt
		      (c:cast 'long
			      (c:ecall "RIGHT_SHIFT_UNSIGNED"
				       (c:cast 'ulong src)
				       (- constant)))))))))

(define-arithmetic-method 'FIXNUM-AND fixnum-methods/2-args/register*constant
  (lambda (tgt src1 constant overflow?)
    (if overflow? (no-overflow-branches!))
    (LAP ,(c:= tgt (c:& src1 (longify constant))))))

(define-arithmetic-method 'FIXNUM-OR fixnum-methods/2-args/register*constant
  (lambda (tgt src1 constant overflow?)
    (if overflow? (no-overflow-branches!))
    (LAP ,(c:= tgt (c:\| src1 (longify constant))))))

(define-arithmetic-method 'FIXNUM-XOR fixnum-methods/2-args/register*constant
  (lambda (tgt src1 constant overflow?)
    (if overflow? (no-overflow-branches!))
    (LAP ,(c:= tgt (c:^ src1 (longify constant))))))

(define-arithmetic-method 'FIXNUM-ANDC fixnum-methods/2-args/register*constant
  (lambda (tgt src1 constant overflow?)
    (if overflow? (no-overflow-branches!))
    (LAP ,(c:= tgt (c:&~ src1 (longify constant))))))

(define-arithmetic-method 'FIXNUM-ANDC fixnum-methods/2-args/constant*register
  (lambda (tgt constant src2 overflow?)
    (if overflow? (no-overflow-branches!))
    (LAP ,(c:= tgt (c:&~ (longify constant) src2)))))

;;;; Predicates

(define-rule predicate
  (OVERFLOW-TEST)
  ;; The RTL code generate guarantees that this instruction is always
  ;; immediately preceded by a fixnum operation with the OVERFLOW?
  ;; flag turned on.  Furthermore, it also guarantees that there are
  ;; no other fixnum operations with the OVERFLOW? flag set.  So all
  ;; the processing of overflow tests has been moved into the fixnum
  ;; operations.
  (LAP))

(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate) (REGISTER (? source)))
  (compare (case predicate
	     ((ZERO-FIXNUM?) c:==)
	     ((NEGATIVE-FIXNUM?) c:<)
	     ((POSITIVE-FIXNUM?) c:>)
	     (else (error "unknown fixnum predicate" predicate)))
	   (standard-source! source 'LONG)
	   0))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? source1))
		      (REGISTER (? source2)))
  (fix-compare-2 predicate
		 (standard-source! source1 'LONG)
		 (standard-source! source2 'LONG)))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? source))
		      (OBJECT->FIXNUM (CONSTANT (? constant))))
  (fix-compare-2 predicate
		 (standard-source! source 'LONG)
		 (longify constant)))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (OBJECT->FIXNUM (CONSTANT (? constant)))
		      (REGISTER (? source)))
  (fix-compare-2 predicate
		 (longify constant)
		 (standard-source! source 'LONG)))
 
(define (fix-compare-2 predicate src1 src2)
  (case predicate
    ((EQUAL-FIXNUM?)
     (compare c:== src1 src2))
    ((LESS-THAN-FIXNUM?)
     (compare c:< src1 src2))
    ((GREATER-THAN-FIXNUM?)
     (compare c:> src1 src2))
    ((UNSIGNED-LESS-THAN-FIXNUM?)
     (compare c:<
	      (c:cast 'ulong src1)
	      (c:cast 'ulong src2)))
    ((UNSIGNED-GREATER-THAN-FIXNUM?)
     (compare c:>
	      (c:cast 'ulong src1)
	      (c:cast 'ulong src2)))
    (else
     (error "unknown fixnum predicate" predicate))))

(define (longify constant)
  (if (number? constant)
      (c:cast 'long constant)
      constant))