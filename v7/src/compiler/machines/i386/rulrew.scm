#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/i386/rulrew.scm,v 1.9 1992/02/18 22:57:48 jinx Exp $
$MC68020-Header: /scheme/src/compiler/machines/bobcat/RCS/rulrew.scm,v 1.4 1991/10/25 06:50:06 cph Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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
  (ASSIGN (OFFSET (REGISTER (? address)) (? offset))
	  (REGISTER (? source register-known-value)))
  (QUALIFIER
   (and (rtl:byte-offset-address? source)
	(let ((base (let ((base (rtl:byte-offset-address-base source)))
		      (if (rtl:register? base)
			  (register-known-value (rtl:register-number base))
			  base))))
	  (and base
	       (rtl:offset? base)
	       (let ((base* (rtl:offset-base base))
		     (offset* (rtl:offset-number base)))
		 (and (= (rtl:register-number base*) address)
		      (= offset* offset)))))))
  (let ((target (let ((base (rtl:byte-offset-address-base source)))
		  (if (rtl:register? base)
		      (register-known-value (rtl:register-number base))
		      base))))
    (list 'ASSIGN
	  target
	  (rtl:make-byte-offset-address target
					(rtl:byte-offset-address-number
					 source)))))

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
  (QUALIFIER (rtl:constant-fixnum-test operand-2 (lambda (n) n true)))
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
	    (integer-power-of-2? (abs n))))))
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