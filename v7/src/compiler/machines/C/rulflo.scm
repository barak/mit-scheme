#| -*-Scheme-*-

$Id: rulflo.scm,v 1.9 2003/02/14 18:28:02 cph Exp $

Copyright (c) 1992-1999, 2001, 2002 Massachusetts Institute of Technology

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

;;;; LAP Generation Rules: Flonum rules
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

(define-rule statement
  ;; convert a floating-point number to a flonum object
  (ASSIGN (REGISTER (? target))
	  (FLOAT->OBJECT (REGISTER (? source))))
  (let ((source (standard-source! source 'DOUBLE)))
    (let ((target (standard-target! target 'SCHEME_OBJECT)))
      (LAP "INLINE_DOUBLE_TO_FLONUM (" ,source ", " ,target ");\n\t"))))

(define-rule statement
  ;; convert a flonum object to a floating-point number
  (ASSIGN (REGISTER (? target)) (OBJECT->FLOAT (REGISTER (? source))))
  (let ((source (standard-source! source 'SCHEME_OBJECT)))
    (let ((target (standard-target! target 'DOUBLE)))
      (LAP ,target " = (FLONUM_TO_DOUBLE (" ,source "));\n\t"))))

;;;; Floating-point vector support

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET (REGISTER (? base))
			(MACHINE-CONSTANT (? offset))))
  (standard-unary-conversion
   base 'DOUBLE*
   target 'DOUBLE
   (lambda (base target)
     (LAP ,target " = " ,base "[" ,offset "];\n\t"))))
  
(define-rule statement
  (ASSIGN (FLOAT-OFFSET (REGISTER (? base))
			(MACHINE-CONSTANT (? offset)))
	  (REGISTER (? source)))
  (let ((base (standard-source! base 'DOUBLE*))
	(source (standard-source! source 'DOUBLE)))
    (LAP ,base "[" ,offset "] = " ,source ";\n\t")))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET (REGISTER (? base)) (REGISTER (? index))))
  (standard-binary-conversion
   base 'DOUBLE*
   index 'LONG
   target 'DOUBLE
   (lambda (base index target)
     (LAP ,target " = " ,base "[" ,index "];\n\t"))))

(define-rule statement
  (ASSIGN (FLOAT-OFFSET (REGISTER (? base)) (REGISTER (? index)))
	  (REGISTER (? source)))
  (let ((base (standard-source! base 'DOUBLE*))
	(source (standard-source! source 'DOUBLE))
	(index (standard-source! index 'LONG)))
    (LAP ,base "[" ,index "] = " ,source ";\n\t")))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET (OFFSET-ADDRESS (REGISTER (? base))
					(MACHINE-CONSTANT (? w-offset)))
			(MACHINE-CONSTANT (? f-offset))))
  (standard-unary-conversion
   base 'SCHEME_OBJECT*
   target 'DOUBLE
   (lambda (base target)
     (LAP ,target
	  " = ((double *) &" ,base "[" ,w-offset "])[" ,f-offset "];\n\t"))))

(define-rule statement
  (ASSIGN (FLOAT-OFFSET (OFFSET-ADDRESS (REGISTER (? base))
					(MACHINE-CONSTANT (? w-offset)))
			(MACHINE-CONSTANT (? f-offset)))
	  (REGISTER (? source)))
  (let ((base (standard-source! base 'SCHEME_OBJECT*))
	(source (standard-source! source 'DOUBLE)))
    (LAP "((double *) &" ,base "[" ,w-offset "])[" ,f-offset "] = "
	 ,source ";\n\t")))

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
     (LAP ,target
	  " = ((double *) &" ,base "[" ,w-offset "])[" ,index "];\n\t"))))

(define-rule statement
  (ASSIGN (FLOAT-OFFSET (OFFSET-ADDRESS (REGISTER (? base))
					(MACHINE-CONSTANT (? w-offset)))
			(REGISTER (? index)))
	  (REGISTER (? source)))
  (let ((base (standard-source! base 'SCHEME_OBJECT*))
	(index (standard-source! index 'LONG))
	(source (standard-source! source 'DOUBLE)))
    (LAP "((double *) &" ,base "[" ,w-offset "])[" ,index "] = "
	 ,source ";\n\t")))

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
    (LAP ,target " =  ((" ,source " >= 0.) ? " ,source " : (-" ,source
	 "));\n\t")))

(define-arithmetic-method 'FLONUM-NEGATE flonum-methods/1-arg
  (lambda (target source)
    (LAP ,target " = (- " ,source ");\n\t")))

(let ((define-use-function
	(lambda (name function)
	  (define-arithmetic-method name flonum-methods/1-arg
	    (lambda (target source)
	      (LAP ,target " = (" ,function " (" ,source "));\n\t"))))))
  (define-use-function 'FLONUM-ACOS "DOUBLE_ACOS")
  (define-use-function 'FLONUM-ASIN "DOUBLE_ASIN")
  (define-use-function 'FLONUM-ATAN "DOUBLE_ATAN")
  (define-use-function 'FLONUM-CEILING "DOUBLE_CEILING")
  (define-use-function 'FLONUM-COS "DOUBLE_COS")
  (define-use-function 'FLONUM-EXP "DOUBLE_EXP")
  (define-use-function 'FLONUM-FLOOR "DOUBLE_FLOOR")
  (define-use-function 'FLONUM-LOG "DOUBLE_LOG")
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

(let-syntax
    ((define-flonum-operation
       (sc-macro-transformer
	(lambda (form environment)
	  environment
	  `(DEFINE-ARITHMETIC-METHOD ',(cadr form) FLONUM-METHODS/2-ARGS
	     (LAMBDA (TARGET SOURCE1 SOURCE2)
	       (LAP ,',target " = (" ,',source1 ,(caddr form) ,',source2
		    ");\n\t")))))))
  (define-flonum-operation flonum-add " + ")
  (define-flonum-operation flonum-subtract " - ")
  (define-flonum-operation flonum-multiply " * ")
  (define-flonum-operation flonum-divide " / "))

(define-arithmetic-method 'FLONUM-ATAN2 flonum-methods/2-args
  (lambda (target source1 source2)
    (LAP ,target " = (DOUBLE_ATAN2 (" ,source1 ", " ,source2
	 "));\n\t")))

;;;; Flonum Predicates

(define-rule predicate
  (FLONUM-PRED-1-ARG (? predicate) (REGISTER (? source)))
  (compare (case predicate
	     ((FLONUM-ZERO?) " == ")
	     ((FLONUM-NEGATIVE?) " < ")
	     ((FLONUM-POSITIVE?) " > ")
	     (else (error "unknown flonum predicate" predicate)))
	   (standard-source! source 'DOUBLE)
	   "0.0"))

(define-rule predicate
  (FLONUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? source1))
		      (REGISTER (? source2)))
  (compare (case predicate
	     ((FLONUM-EQUAL?) " == ")
	     ((FLONUM-LESS?) " < ")
	     ((FLONUM-GREATER?) " > ")
	     (else (error "unknown flonum predicate" predicate)))
	   (standard-source! source1 'DOUBLE)
	   (standard-source! source2 'DOUBLE)))