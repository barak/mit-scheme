#| -*-Scheme-*-

$Id: scomb.scm,v 14.17 2001/12/20 21:24:08 cph Exp $

Copyright (c) 1988-1999, 2001 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
|#

;;;; SCode Combinator Abstractions
;;; package: (runtime scode-combinator)

(declare (usual-integrations))

(define (initialize-package!)
  (set! combination/constant-folding-operators
	(map make-primitive-procedure
	     '(
	       &*
	       &+
	       &-
	       &/
	       -1+
	       1+
	       ASCII->CHAR
	       CELL?
	       CHAR->ASCII
	       CHAR->INTEGER
	       CHAR-ASCII?
	       CHAR-BITS
	       CHAR-CODE
	       CHAR-DOWNCASE
	       CHAR-UPCASE
	       COMPILED-CODE-ADDRESS->BLOCK
	       COMPILED-CODE-ADDRESS->OFFSET
	       DIVIDE-FIXNUM
	       EQ?
	       EQUAL-FIXNUM?
	       FIXNUM-AND
	       FIXNUM-ANDC
	       FIXNUM-LSH
	       FIXNUM-NOT
	       FIXNUM-OR
	       FIXNUM-QUOTIENT
	       FIXNUM-REMAINDER
	       FIXNUM-XOR
	       FLONUM-ABS
	       FLONUM-ACOS
	       FLONUM-ADD
	       FLONUM-ASIN
	       FLONUM-ATAN
	       FLONUM-ATAN2
	       FLONUM-CEILING
	       FLONUM-CEILING->EXACT
	       FLONUM-COS
	       FLONUM-DIVIDE
	       FLONUM-EQUAL?
	       FLONUM-EXP
	       FLONUM-EXPT
	       FLONUM-FLOOR
	       FLONUM-FLOOR->EXACT
	       FLONUM-GREATER?
	       FLONUM-LESS?
	       FLONUM-LOG
	       FLONUM-MULTIPLY
	       FLONUM-NEGATE
	       FLONUM-NEGATIVE?
	       FLONUM-POSITIVE?
	       FLONUM-ROUND
	       FLONUM-ROUND->EXACT
	       FLONUM-SIN
	       FLONUM-SQRT
	       FLONUM-SUBTRACT
	       FLONUM-TAN
	       FLONUM-TRUNCATE
	       FLONUM-TRUNCATE->EXACT
	       FLONUM-ZERO?
	       GCD-FIXNUM
	       GREATER-THAN-FIXNUM?
	       INDEX-FIXNUM?
	       INTEGER->CHAR
	       LESS-THAN-FIXNUM?
	       MAKE-CHAR
	       MAKE-NON-POINTER-OBJECT
	       MINUS-FIXNUM
	       MINUS-ONE-PLUS-FIXNUM
	       MULTIPLY-FIXNUM
	       NEGATIVE-FIXNUM?
	       NEGATIVE?
	       NOT
	       OBJECT-TYPE
	       OBJECT-TYPE?
	       ONE-PLUS-FIXNUM
	       PAIR?
	       PLUS-FIXNUM
	       POSITIVE-FIXNUM?
	       POSITIVE?
	       PRIMITIVE-PROCEDURE-ARITY
	       ;; STRING->SYMBOL is a special case.  Strings have can
	       ;; be side-effected, but it is useful to be able to
	       ;; constant fold this primitive anyway.
	       STRING->SYMBOL
	       ZERO-FIXNUM?
	       ZERO?
	       ))))

;;;; Sequence

(define (make-sequence actions)
  (if (null? actions)
      (error "MAKE-SEQUENCE: No actions"))
  (let loop ((actions actions))
    (cond ((null? (cdr actions))
	   (car actions))
	  ((null? (cddr actions))
	   (&typed-pair-cons (ucode-type sequence-2)
			     (car actions)
			     (cadr actions)))
	  (else
	   (&typed-triple-cons (ucode-type sequence-3)
			       (car actions)
			       (cadr actions)
			       (loop (cddr actions)))))))

(define (sequence? object)
  (or (object-type? (ucode-type sequence-2) object)
      (object-type? (ucode-type sequence-3) object)))

(define (sequence-actions expression)
  (cond ((object-type? (ucode-type sequence-2) expression)
	 (append! (sequence-actions (&pair-car expression))
		  (sequence-actions (&pair-cdr expression))))
	((object-type? (ucode-type sequence-3) expression)
	 (append! (sequence-actions (&triple-first expression))
		  (sequence-actions (&triple-second expression))
		  (sequence-actions (&triple-third expression))))
	(else
	 (list expression))))

(define (sequence-immediate-actions expression)
  (cond ((object-type? (ucode-type sequence-2) expression)
	 (list (&pair-car expression)
	       (&pair-cdr expression)))
	((object-type? (ucode-type sequence-3) expression)
	 (list (&triple-first expression)
	       (&triple-second expression)
	       (&triple-third expression)))
	(else
	 (error:wrong-type-argument expression "SCode sequence"
				    'SEQUENCE-IMMEDIATE-ACTIONS))))

(define-integrable (sequence-components expression receiver)
  (receiver (sequence-actions expression)))

;;;; Conditional

(define (make-conditional predicate consequent #!optional alternative)
  (let ((alternative
	 (if (default-object? alternative)
	     undefined-conditional-branch
	     alternative)))
    (if (and (combination? predicate)
	     (eq? (combination-operator predicate) (ucode-primitive not)))
	(make-conditional (car (combination-operands predicate))
			  alternative
			  consequent)
	(&typed-triple-cons (ucode-type conditional)
			    predicate
			    consequent
			    alternative))))

(define (conditional? object)
  (object-type? (ucode-type conditional) object))

(define undefined-conditional-branch unspecific)

(define-integrable (conditional-predicate conditional)
  (&triple-first conditional))

(define-integrable (conditional-consequent conditional)
  (&triple-second conditional))

(define-integrable (conditional-alternative conditional)
  (&triple-third conditional))

(define (conditional-components conditional receiver)
  (receiver (conditional-predicate conditional)
	    (conditional-consequent conditional)
	    (conditional-alternative conditional)))

;;;; Disjunction

(define (make-disjunction predicate alternative)
  (if (and (combination? predicate)
	   (eq? (combination-operator predicate) (ucode-primitive not)))
      (make-conditional (car (combination-operands predicate))
			alternative
			true)
      (&typed-pair-cons (ucode-type disjunction) predicate alternative)))

(define-integrable (disjunction? object)
  (object-type? (ucode-type disjunction) object))

(define-integrable (disjunction-predicate disjunction)
  (&pair-car disjunction))

(define-integrable (disjunction-alternative disjunction)
  (&pair-cdr disjunction))

(define (disjunction-components disjunction receiver)
  (receiver (disjunction-predicate disjunction)
	    (disjunction-alternative disjunction)))

;;;; Combination

(define (combination? object)
  (or (object-type? (ucode-type combination) object)
      (object-type? (ucode-type combination-1) object)
      (object-type? (ucode-type combination-2) object)
      (object-type? (ucode-type primitive-combination-0) object)
      (object-type? (ucode-type primitive-combination-1) object)
      (object-type? (ucode-type primitive-combination-2) object)
      (object-type? (ucode-type primitive-combination-3) object)))

(define (make-combination operator operands)
  (if (and (memq operator combination/constant-folding-operators)
	   (let loop ((operands operands))
	     (or (null? operands)
		 (and (scode-constant? (car operands))
		      (loop (cdr operands))))))
      (apply operator operands)
      (%make-combination operator operands)))

(define combination/constant-folding-operators)

(define (%make-combination operator operands)
  (cond ((null? operands)
	 (if (and (primitive-procedure? operator)
		  (= (primitive-procedure-arity operator) 0))
	     (object-new-type (ucode-type primitive-combination-0) operator)
	     (&typed-vector-cons (ucode-type combination)
				 (cons operator '()))))
	((null? (cdr operands))
	 (&typed-pair-cons
	  (if (and (primitive-procedure? operator)
		   (= (primitive-procedure-arity operator) 1))
	      (ucode-type primitive-combination-1)
	      (ucode-type combination-1))
	  operator
	  (car operands)))
	((null? (cddr operands))
	 (&typed-triple-cons
	  (if (and (primitive-procedure? operator)
		   (= (primitive-procedure-arity operator) 2))
	      (ucode-type primitive-combination-2)
	      (ucode-type combination-2))
	  operator
	  (car operands)
	  (cadr operands)))
	(else
	 (&typed-vector-cons
	  (if (and (null? (cdddr operands))
		   (primitive-procedure? operator)
		   (= (primitive-procedure-arity operator) 3))
	      (ucode-type primitive-combination-3)
	      (ucode-type combination))
	  (cons operator operands)))))

(let-syntax
    ((combination-dispatch
      (lambda (name combination case-0 case-1 case-2 case-n)
	`(COND ((OBJECT-TYPE? (UCODE-TYPE PRIMITIVE-COMBINATION-0)
			      ,combination)
		,case-0)
	       ((OR (OBJECT-TYPE? (UCODE-TYPE COMBINATION-1) ,combination)
		    (OBJECT-TYPE? (UCODE-TYPE PRIMITIVE-COMBINATION-1)
				  ,combination))
		,case-1)
	       ((OR (OBJECT-TYPE? (UCODE-TYPE COMBINATION-2) ,combination)
		    (OBJECT-TYPE? (UCODE-TYPE PRIMITIVE-COMBINATION-2)
				  ,combination))
		,case-2)
	       ((OR (OBJECT-TYPE? (UCODE-TYPE COMBINATION) ,combination)
		    (OBJECT-TYPE? (UCODE-TYPE PRIMITIVE-COMBINATION-3)
				  ,combination))
		,case-n)
	       (ELSE
		(ERROR:WRONG-TYPE-ARGUMENT ,combination "SCode combination"
					   ',name))))))

(define (combination-size combination)
  (combination-dispatch combination-size combination
			1 2 3 (&vector-length combination)))

(define (combination-operator combination)
  (combination-dispatch combination-operator combination
			(object-new-type (ucode-type primitive) combination)
			(&pair-car combination)
			(&triple-first combination)
			(&vector-ref combination 0)))

(define (combination-operands combination)
  (combination-dispatch
   combination-operands combination
   '()
   (list (&pair-cdr combination))
   (list (&triple-second combination) (&triple-third combination))
   (&subvector->list combination 1 (&vector-length combination))))

(define (combination-components combination receiver)
  (combination-dispatch
   combination-components combination
   (receiver (object-new-type (ucode-type primitive) combination) '())
   (receiver (&pair-car combination) (list (&pair-cdr combination)))
   (receiver (&triple-first combination)
	     (list (&triple-second combination) (&triple-third combination)))
   (receiver (&vector-ref combination 0)
	     (&subvector->list combination 1 (&vector-length combination)))))

)

;;;; Unassigned?

(define (make-unassigned? name)
  (make-combination (ucode-primitive lexical-unassigned?)
		    (list (make-the-environment) name)))

(define (unassigned?? object)
  (and (combination? object)
       (eq? (combination-operator object)
	    (ucode-primitive lexical-unassigned?))
       (let ((operands (combination-operands object)))
	 (and (the-environment? (car operands))
	      (symbol? (cadr operands))))))

(define-integrable (unassigned?-name expression)
  (cadr (combination-operands expression)))

(define-integrable (unassigned?-components expression receiver)
  (receiver (unassigned?-name expression)))