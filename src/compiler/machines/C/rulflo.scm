#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

;;;; LAP Generation Rules: Flonum rules
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

(define-rule statement
  ;; convert a floating-point number to a flonum object
  (ASSIGN (REGISTER (? target))
	  (FLOAT->OBJECT (REGISTER (? source))))
  (let ((source (standard-source! source 'DOUBLE)))
    (let ((target (standard-target! target 'SCHEME_OBJECT)))
      (LAP ,(c:scall "INLINE_DOUBLE_TO_FLONUM" source target)))))

(define-rule statement
  ;; convert a flonum object to a floating-point number
  (ASSIGN (REGISTER (? target)) (OBJECT->FLOAT (REGISTER (? source))))
  (let ((source (standard-source! source 'SCHEME_OBJECT)))
    (let ((target (standard-target! target 'DOUBLE)))
      (LAP ,(c:= target (c:ecall "FLONUM_TO_DOUBLE" source))))))

;;;; Floating-point vector support

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET (REGISTER (? base))
			(MACHINE-CONSTANT (? offset))))
  (standard-unary-conversion
   base 'DOUBLE*
   target 'DOUBLE
   (lambda (base target)
     (LAP ,(c:= target (c:aref base offset))))))
  
(define-rule statement
  (ASSIGN (FLOAT-OFFSET (REGISTER (? base))
			(MACHINE-CONSTANT (? offset)))
	  (REGISTER (? source)))
  (let ((base (standard-source! base 'DOUBLE*))
	(source (standard-source! source 'DOUBLE)))
    (LAP ,(c:= (c:aref base offset) source))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET (REGISTER (? base)) (REGISTER (? index))))
  (standard-binary-conversion
   base 'DOUBLE*
   index 'LONG
   target 'DOUBLE
   (lambda (base index target)
     (LAP ,(c:= target (c:aref base index))))))

(define-rule statement
  (ASSIGN (FLOAT-OFFSET (REGISTER (? base)) (REGISTER (? index)))
	  (REGISTER (? source)))
  (let ((base (standard-source! base 'DOUBLE*))
	(source (standard-source! source 'DOUBLE))
	(index (standard-source! index 'LONG)))
     (LAP ,(c:= (c:aref base index) source))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET (OFFSET-ADDRESS (REGISTER (? base))
					(MACHINE-CONSTANT (? w-offset)))
			(MACHINE-CONSTANT (? f-offset))))
  (standard-unary-conversion
   base 'SCHEME_OBJECT*
   target 'DOUBLE
   (lambda (base target)
     (LAP ,(c:= target
		(c:aref (c:cast 'double* (c:aptr base w-offset))
			f-offset))))))

(define-rule statement
  (ASSIGN (FLOAT-OFFSET (OFFSET-ADDRESS (REGISTER (? base))
					(MACHINE-CONSTANT (? w-offset)))
			(MACHINE-CONSTANT (? f-offset)))
	  (REGISTER (? source)))
  (let ((base (standard-source! base 'SCHEME_OBJECT*))
	(source (standard-source! source 'DOUBLE)))
    (LAP ,(c:= (c:aref (c:cast 'double* (c:aptr base w-offset))
		       f-offset)
	       source))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET (OFFSET-ADDRESS (REGISTER (? base))
					(MACHINE-CONSTANT (? w-offset)))
			(REGISTER (? index))))
  (standard-binary-conversion
   base 'SCHEME_OBJECT*
   index 'LONG
   target 'DOUBLE
   (lambda (base index target)
     (LAP ,(c:= target
		(c:aref (c:cast 'double* (c:aptr base w-offset))
			index))))))

(define-rule statement
  (ASSIGN (FLOAT-OFFSET (OFFSET-ADDRESS (REGISTER (? base))
					(MACHINE-CONSTANT (? w-offset)))
			(REGISTER (? index)))
	  (REGISTER (? source)))
  (let ((base (standard-source! base 'SCHEME_OBJECT*))
	(index (standard-source! index 'LONG))
	(source (standard-source! source 'DOUBLE)))
    (LAP ,(c:= (c:aref (c:cast 'double* (c:aptr base w-offset))
		       index)
	       source))))

;;;; Flonum Arithmetic

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLONUM-1-ARG (? operation) (REGISTER (? source)) (? overflow?)))
  overflow?				;ignore
  (let ((source (standard-source! source 'DOUBLE)))
    ((flonum-1-arg/operator operation)
     (standard-target! target 'DOUBLE)
     source)))

(define (flonum-1-arg/operator operation)
  (lookup-arithmetic-method operation flonum-methods/1-arg))

(define flonum-methods/1-arg
  (list 'FLONUM-METHODS/1-ARG))

(define-arithmetic-method 'FLONUM-ABS flonum-methods/1-arg
  (lambda (target source)
    (LAP ,(c:= target
	       (c:?: (c:< source 0.)
		     (c:- source)
		     source)))))

(define-arithmetic-method 'FLONUM-NEGATE flonum-methods/1-arg
  (lambda (target source)
    (LAP ,(c:= target (c:- source)))))

(let ((define-use-function
	(lambda (name function)
	  (define-arithmetic-method name flonum-methods/1-arg
	    (lambda (target source)
	      (LAP ,(c:= target (c:ecall function source))))))))
  (define-use-function 'FLONUM-ACOS "DOUBLE_ACOS")
  (define-use-function 'FLONUM-ASIN "DOUBLE_ASIN")
  (define-use-function 'FLONUM-ATAN "DOUBLE_ATAN")
  (define-use-function 'FLONUM-CEILING "DOUBLE_CEILING")
  (define-use-function 'FLONUM-COS "DOUBLE_COS")
  (define-use-function 'FLONUM-EXP "DOUBLE_EXP")
  (define-use-function 'FLONUM-EXPM1 "DOUBLE_EXPM1")
  (define-use-function 'FLONUM-FLOOR "DOUBLE_FLOOR")
  (define-use-function 'FLONUM-LOG "DOUBLE_LOG")
  (define-use-function 'FLONUM-LOG1P "DOUBLE_LOG1P")
  (define-use-function 'FLONUM-ROUND "DOUBLE_ROUND")
  (define-use-function 'FLONUM-SIN "DOUBLE_SIN")
  (define-use-function 'FLONUM-SQRT "DOUBLE_SQRT")
  (define-use-function 'FLONUM-TAN "DOUBLE_TAN")
  (define-use-function 'FLONUM-TRUNCATE "DOUBLE_TRUNCATE"))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLONUM-2-ARGS (? operation)
			 (REGISTER (? source1))
			 (REGISTER (? source2))
			 (? overflow?)))
  overflow?				;ignore
  (let ((source1 (standard-source! source1 'DOUBLE))
	(source2 (standard-source! source2 'DOUBLE)))
    ((flonum-2-args/operator operation)
     (standard-target! target 'DOUBLE)
     source1
     source2)))

(define (flonum-2-args/operator operation)
  (lookup-arithmetic-method operation flonum-methods/2-args))

(define flonum-methods/2-args
  (list 'FLONUM-METHODS/2-ARGS))

(define-arithmetic-method 'FLONUM-ADD flonum-methods/2-args
  (lambda (target source1 source2)
    (LAP ,(c:= target (c:+ source1 source2)))))

(define-arithmetic-method 'FLONUM-SUBTRACT flonum-methods/2-args
  (lambda (target source1 source2)
    (LAP ,(c:= target (c:- source1 source2)))))

(define-arithmetic-method 'FLONUM-MULTIPLY flonum-methods/2-args
  (lambda (target source1 source2)
    (LAP ,(c:= target (c:* source1 source2)))))

(define-arithmetic-method 'FLONUM-DIVIDE flonum-methods/2-args
  (lambda (target source1 source2)
    (LAP ,(c:= target (c:/ source1 source2)))))

(define-arithmetic-method 'FLONUM-ATAN2 flonum-methods/2-args
  (lambda (target source1 source2)
    (LAP ,(c:= target (c:ecall "DOUBLE_ATAN2" source1 source2)))))

;;;; Flonum Predicates

(define-rule predicate
  (FLONUM-PRED-1-ARG (? predicate) (REGISTER (? source)))
  (compare (case predicate
	     ((FLONUM-ZERO?) c:==)
	     ((FLONUM-NEGATIVE?) c:<)
	     ((FLONUM-POSITIVE?) c:>)
	     (else (error "unknown flonum predicate" predicate)))
	   (standard-source! source 'DOUBLE)
	   "0.0"))

(define-rule predicate
  (FLONUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? source1))
		      (REGISTER (? source2)))
  (compare (case predicate
	     ((FLONUM-EQUAL?) c:==)
	     ((FLONUM-LESS?) c:<)
	     ((FLONUM-GREATER?) c:>)
	     (else (error "unknown flonum predicate" predicate)))
	   (standard-source! source1 'DOUBLE)
	   (standard-source! source2 'DOUBLE)))