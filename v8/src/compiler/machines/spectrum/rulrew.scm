#| -*-Scheme-*-

$Id: 0e69eb2abe37be14472fa81049a57f591aff201f $

Copyright (c) 1990-1999 Massachusetts Institute of Technology

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
  ;; Since we use DEP instructions to insert type codes, there's no
  ;; difference between the way that pointers and non-pointers are
  ;; constructed.
  (rtl:make-cons-pointer type datum))


(define-rule add-pre-cse-rewriting-rule!
  (CONS-POINTER (REGISTER (? type register-known-value))
		(? datum))
  (QUALIFIER 
   (and (rtl:machine-constant? type)
	(let ((value (rtl:machine-constant-value type))
	      (class (rtl:expression-value-class datum)))
	  ;; Typecode values that we can use for DEPI instruction, even
	  ;; though the type cant be specified in 6 bits (01xxxx/10xxxx)
	  ;; If the quad mask bits are 0xxxx0 then we can do (0xxxxx/xxxxx0)
	  ;; In a single DEPI.
	  ;; Forcing them to be constants prevents any cse on the values.
	  (and (value-class=address? class)
	       (fix:fixnum? value)
	       (or (even? (fix:or quad-mask-value value))
		   (fix:<= (fix:or quad-mask-value value) #b11111))))))
  (rtl:make-cons-pointer type datum))


;(define-rule add-pre-cse-rewriting-rule!
;  (CONS-POINTER (REGISTER (? type register-known-value))
;		(? datum))
;  (QUALIFIER 
;   (and (rtl:machine-constant? type)
;	(let ((value (rtl:machine-constant-value type))
;	      (class (rtl:expression-value-class datum)))
;	  ;; Elide a (CONS-POINTER address-bits address-register)
;	  (and (eq? class value-class=address)
;	       (fix:fixnum? value)
;	       (fix:= value quad-mask-value value)))))
;  datum)

(define-rule add-pre-cse-rewriting-rule!
  (CONS-POINTER (REGISTER (? type register-known-value))
		(? datum))
  (QUALIFIER 
   (and (rtl:machine-constant? type)
	(let ((value (rtl:machine-constant-value type)))
	  ;; Typecode values that we can use for DEPI instructions.
	  ;; Forcing them to be constants prevents any cse on the values.
	  (or (fits-in-5-bits-signed? value)
	      (fits-in-5-bits-signed? (- value (1+ max-type-code)))
	      (= value quad-mask-value) ; for which we use r5
	      ))))
  (rtl:make-cons-pointer type datum))

(define-rule add-pre-cse-rewriting-rule!
  (CONS-NON-POINTER (REGISTER (? type register-known-value))
		    (? datum))
  (QUALIFIER 
   (and (rtl:machine-constant? type)
	(let ((value (rtl:machine-constant-value type)))
	  ;; Typecode values that we can use for DEPI instructions.
	  ;; Forcing them to be constants prevents any cse on the values.
	  (or (fits-in-5-bits-signed? value)
	      (fits-in-5-bits-signed? (- value (1+ max-type-code)))
	      (= value quad-mask-value) ; for which we use 
	      ))))
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
    (target-object-type
     (rtl:constant-value (rtl:object->type-expression datum))))
   datum))

(define-rule rewriting
  (CONS-POINTER (? type) (REGISTER (? datum register-known-value)))
  (QUALIFIER (and (rtl:object->datum? datum)
		  (let ((datum-expression (rtl:object->datum-expression datum)))
		    (and (rtl:constant? datum-expression)
			 (not (rtl:constant-non-pointer? datum-expression))))))
  ;; Since we use DEP/DEPI, there is no need to clear the old bits
  (rtl:make-cons-pointer type (rtl:object->datum-expression datum)))

(define-rule rewriting
  (OBJECT->TYPE (REGISTER (? source register-known-value)))
  (QUALIFIER (rtl:constant? source))
  (rtl:make-machine-constant (target-object-type (rtl:constant-value source))))

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
  ;; Use registers that cache common constants: 0, #F '()
  (ASSIGN (? target) (REGISTER (? value register-known-value)))
  (QUALIFIER (constant-register-expression value))
  ;; Use `(REGISTER ...) to prevent CSE (it will be a machine register)
  (list 'ASSIGN target `(REGISTER ,(register-expression value))))

(define-rule add-pre-cse-rewriting-rule!
  ;; Compare to registers that cache common constants: 0, #F '()
  (EQ-TEST (? source) (REGISTER (? comparand register-known-value)))
  (QUALIFIER (constant-register-expression comparand))
  (list 'EQ-TEST source comparand))

(define-rule rewriting
  ;; Compare to registers that cache common constants: 0, #F '()
  (EQ-TEST (REGISTER (? comparand register-known-value)) (? source))
  (QUALIFIER (constant-register-expression comparand))
  (list 'EQ-TEST source comparand))

(define-rule add-pre-cse-rewriting-rule!
  (EQ-TEST (REGISTER (? comparand register-known-fixnum-constant)) (? source))
  (QUALIFIER
   (fits-in-5-bits-signed? (known-fixnum-constant/value comparand)))
  (list `EQ-TEST comparand source))

(define-rule add-pre-cse-rewriting-rule!
  (EQ-TEST (? source) (REGISTER (? comparand register-known-fixnum-constant)))
  (QUALIFIER
   (fits-in-5-bits-signed? (known-fixnum-constant/value comparand)))
  (list `EQ-TEST source comparand))

;;;; Fixnums
;;;
;; Some constants should always be folded into the operation because either
;; they are encodable as an immediate value in the instruction at no cost
;; or they are open coded specially in a way that does not put the value in
;; a register.  We detect these cases by inspecting the arithconst predicates
;; in fulfix.scm.
;; This is done pre-cse so that cse doesn't decide to hide the constant in a
;; register in expressions like (cons (fix:quotient x 8) (fix:remainder x 8)))

(define-rule add-pre-cse-rewriting-rule!
  (FIXNUM-2-ARGS (? operation)
		 (REGISTER (? operand-1 register-known-fixnum-constant))
		 (? operand-2)
		 (? overflow?))
  (QUALIFIER
   (and (rtl:register? operand-2)
	(fixnum-2-args/operator/constant*register?
	 operation
	 (known-fixnum-constant/value operand-1)
	 overflow?)))
  (rtl:make-fixnum-2-args operation operand-1 operand-2 overflow?))

(define-rule add-pre-cse-rewriting-rule!
  (FIXNUM-2-ARGS (? operation)
		 (? operand-1)
		 (REGISTER (? operand-2 register-known-fixnum-constant))
		 (? overflow?))
  (QUALIFIER
   (and (rtl:register? operand-1)
	(fixnum-2-args/operator/register*constant?
	 operation
	 (known-fixnum-constant/value operand-2)
	 overflow?)))
  (rtl:make-fixnum-2-args operation operand-1 operand-2 overflow?))

		
(define (register-known-fixnum-constant regnum)
  ;; Returns the RTL of a constant that is a fixnum, i.e (CONSTANT 1000)
  ;;  recognizes: (CONSTANT x)
  (let ((expr (register-known-value regnum)))
    (and expr
	 (rtl:constant? expr)
	 (fixnum? (rtl:constant-value expr))
	 expr)))
		  

(define (known-fixnum-constant/value constant)
  (rtl:constant-value constant))

(define-rule add-pre-cse-rewriting-rule!
  (PRED-1-ARG INDEX-FIXNUM? (? source))

  ;; This is a predicate so we can't use rtl:make-type-test

  (list 'TYPE-TEST (rtl:make-object->type source) (ucode-type positive-fixnum)))

;; The fixnum comparisons do not appear use the same mechanisom ast the
;; operators, so we code the bit field size dependencies here:
  
(define-rule add-pre-cse-rewriting-rule!
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? comparand register-known-fixnum-constant))
		      (? source))
  (QUALIFIER
   (fits-in-5-bits-signed? (known-fixnum-constant/value comparand)))
  (list `FIXNUM-PRED-2-ARGS predicate comparand source))

(define-rule add-pre-cse-rewriting-rule!
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (? source)
		      (REGISTER (? comparand register-known-fixnum-constant)))
  (QUALIFIER
   (fits-in-5-bits-signed? (known-fixnum-constant/value comparand)))
  (list `FIXNUM-PRED-2-ARGS predicate source comparand))


(define-rule add-pre-cse-rewriting-rule!
  (PRED-2-ARGS WORD-LESS-THAN-UNSIGNED?
	       (REGISTER (? comparand register-known-fixnum-constant))
	       (? source))
  (QUALIFIER
   (fits-in-5-bits-signed? (known-fixnum-constant/value comparand)))
  (list `PRED-2-ARGS 'WORD-LESS-THAN-UNSIGNED? comparand source))

(define-rule add-pre-cse-rewriting-rule!
  (PRED-2-ARGS WORD-LESS-THAN-UNSIGNED?
	       (? source)
	       (REGISTER (? comparand register-known-fixnum-constant)))
  (QUALIFIER
   (fits-in-5-bits-signed? (known-fixnum-constant/value comparand)))
  (list `PRED-2-ARGS 'WORD-LESS-THAN-UNSIGNED? source comparand))

;;;; Closures and other optimizations.  

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

(define-rule add-pre-cse-rewriting-rule!
  ;; Prevent CSE of machine floating point constants with object flonums
  (OBJECT->FLOAT (REGISTER (? value register-known-value)))
  (QUALIFIER (and (rtl:constant? value)
		  (flo:flonum? (rtl:constant-value value))))
  `(OBJECT->FLOAT ,value))


;;;
;; (CONS-NON-POINTER (MACHINE-CONSTANT 0)
;;                   (? thing-with-known-type-already=0)) => thing
;;

(define-rule add-pre-cse-rewriting-rule!
  (CONS-NON-POINTER (REGISTER (? type register-known-value))
		    (? datum))
  (QUALIFIER
   (and (rtl:machine-constant? type)
	(= 0 (rtl:machine-constant-value type))
	(rtl:has-type-zero? datum)))
  datum)

(define-rule add-pre-cse-rewriting-rule!
  (CONS-NON-POINTER (MACHINE-CONSTANT 0) (? datum))
  (QUALIFIER (rtl:has-type-zero? datum))
  datum)

(define (rtl:has-type-zero? expr)
  (or (value-class=ascii? (rtl:expression-value-class expr))
      (value-class=datum? (rtl:expression-value-class expr))
      #F))


(define-rule add-pre-cse-rewriting-rule!
  (COERCE-VALUE-CLASS (? frob) (? class))
  class					; ignored
  (error "Unknown expression for " frob)
  frob)

(define-rule add-pre-cse-rewriting-rule!
  (COERCE-VALUE-CLASS (REGISTER (? frob register-known-expression)) (? class))
  class					; ignored
  frob)

;;; Canonicalize flonum comparisons against 0.0 to use unary operators.

(define-rule add-pre-cse-rewriting-rule!
  (FLONUM-2-ARGS FLONUM-SUBTRACT
		 (REGISTER (? operand-1 register-known-flonum-zero?))
		 (? operand-2)
		 (? overflow?))
  (rtl:make-flonum-1-arg 'FLONUM-NEGATE operand-2 overflow?))

(define-rule add-pre-cse-rewriting-rule!
  (FLONUM-PRED-2-ARGS (? predicate)
		      (? operand-1)
		      (REGISTER (? operand-2 register-known-flonum-zero?)))
  (list 'FLONUM-PRED-1-ARG
	(case predicate
	  ((FLONUM-LESS?)     'FLONUM-NEGATIVE?)
	  ((FLONUM-GREATER?)  'FLONUM-POSITIVE?)
	  ((FLONUM-EQUAL?)    'FLONUM-ZERO?))
	operand-1))

(define-rule add-pre-cse-rewriting-rule!
  (FLONUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? operand-1 register-known-flonum-zero?))
		      (? operand-2))
  (list 'FLONUM-PRED-1-ARG
	(case predicate
	  ((FLONUM-LESS?)     'FLONUM-POSITIVE?)
	  ((FLONUM-GREATER?)  'FLONUM-NEGATIVE?)
	  ((FLONUM-EQUAL?)    'FLONUM-ZERO?))
	operand-2))  
		
(define (register-known-flonum-zero? regnum)
  ;; returns the rtl of a constant that is a fixnum, i.e (CONSTANT 1000)
  ;; recognizes (OBJECT->FLOAT (CONSTANT 0.0))
  (let ((expr (register-known-value regnum)))
    (and expr
	 (rtl:object->float? expr)
	 (rtl:constant? (rtl:object->float-expression expr))
	 (equal? 0.0
		 (rtl:constant-value (rtl:object->float-expression expr))))))
