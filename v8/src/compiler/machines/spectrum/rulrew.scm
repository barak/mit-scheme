#| -*-Scheme-*-

$Id: rulrew.scm,v 1.1 1994/11/19 02:08:04 adams Exp $

Copyright (c) 1990-1993 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

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
		  (not (rtl:constant-non-pointer?
			(rtl:object->datum-expression datum)))))
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
		(zero? (target-object-type value))
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
;;;
;; Some constants should always be folded into the operation because either
;; they are encodable as an immediate value in the instruction at no cost
;; or they are open coded specially in a way that does not put the value in
;; a register.  We detect these cases by inspecting the arithconst predicates
;; in fulfix.scm.
;; This is done pre-cse so that cse doesnt decide to hide the constant in a
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
	 (known-fixnum-constant/fixnum-value operand-1)
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
	 (known-fixnum-constant/fixnum-value operand-2)
	 overflow?)))
  (rtl:make-fixnum-2-args operation operand-1 operand-2 overflow?))

		
(define (register-known-fixnum-constant regnum)
  ;; returns the rtl of a constant that is a fixnum, i.e (CONSTANT 1000)
  ;; recognizes (CONSTANT x)
  ;;            (OBJECT->FIXNUM (CONSTANT x))
  ;;            (OBJECT->FIXNUM (REGISTER y)) where y also satisfies this pred
  (let ((expr (register-known-value regnum)))
    (and expr
	 (cond ((and (rtl:constant? expr)
		     (fix:fixnum? (rtl:constant-value expr)))
		expr)
	       ((and (rtl:object->fixnum? expr)
		     (rtl:constant? (rtl:object->fixnum-expression expr))
		     (fix:fixnum?  (rtl:constant-value
				    (rtl:object->fixnum-expression expr))))
		(rtl:object->fixnum-expression expr))
	       ((and (rtl:object->fixnum? expr)
		     (rtl:register? (rtl:object->fixnum-expression expr)))
		(register-known-fixnum-constant 
		 (rtl:register-number (rtl:object->fixnum-expression expr))))
	       (else #F)))))

(define (known-fixnum-constant/fixnum-value constant)
  (rtl:constant-value constant))

(define-rule add-pre-cse-rewriting-rule!
  (PRED-1-ARG INDEX-FIXNUM? (? source))

  ;; This is a predicate so we can't use rtl:make-type-test

  (list 'TYPE-TEST (rtl:make-object->type source) (ucode-type positive-fixnum)))
  

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


;;
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

(define (rtl:has-type-zero? expr)
  (or (value-class=ascii? (rtl:expression-value-class expr))
      (value-class=datum? (rtl:expression-value-class expr))
      #F))


;; Remove all object->fixnum and fixnum->object and object->unsigned-fixnum

(define-rule add-pre-cse-rewriting-rule!
  (OBJECT->FIXNUM (? frob))
  frob)

(define-rule add-pre-cse-rewriting-rule!
  (OBJECT->UNSIGNED-FIXNUM (? frob))
  frob)

(define-rule add-pre-cse-rewriting-rule!
  (FIXNUM->OBJECT (? frob))
  frob)

(define-rule add-pre-cse-rewriting-rule!
  (COERCE-VALUE-CLASS (? frob) (? class))
  class					; ignored
  (error "Unknown expression for " frob)
  frob)

(define-rule add-pre-cse-rewriting-rule!
  (COERCE-VALUE-CLASS (REGISTER (? frob register-known-expression)) (? class))
  class					; ignored
  frob)
