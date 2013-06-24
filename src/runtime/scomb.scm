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

;;;; SCode Combinator Abstractions
;;; package: (runtime scode-combinator)

(declare (usual-integrations))

(define (initialize-package!)
  (set! combination/constant-folding-operators
	(map (lambda (name)
	       (make-primitive-procedure name #t))
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
	       NULL?
	       OBJECT-TYPE
	       OBJECT-TYPE?
	       ONE-PLUS-FIXNUM
	       PAIR?
	       PLUS-FIXNUM
	       POSITIVE-FIXNUM?
	       POSITIVE?
	       PRIMITIVE-PROCEDURE-ARITY
	       ;; STRING->SYMBOL is a special case.  Strings can
	       ;; be side-effected, but it is useful to be able to
	       ;; constant fold this primitive anyway.
	       STRING->SYMBOL
	       STRING-LENGTH
	       ZERO-FIXNUM?
	       ZERO?
	       ))))

;;;; Sequence

(define (make-sequence actions)
  (if (null? actions)
      (error "MAKE-SEQUENCE: No actions"))
  (let loop ((actions actions))
    (if (null? (cdr actions))
	(car actions)
	(&typed-pair-cons (ucode-type sequence-2)
			  (car actions)
			  (loop (cdr actions))))))

(define (sequence? object)
  (or (object-type? (ucode-type sequence-2) object)
      (object-type? (ucode-type sequence-3) object)))

(define-guarantee sequence "SCode sequence")

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
	 (error:not-sequence expression 'SEQUENCE-IMMEDIATE-ACTIONS))))

(define (sequence-components expression receiver)
  (receiver (sequence-actions expression)))

;;;; Conditional

(define (make-conditional predicate consequent alternative)
  (if (and (combination? predicate)
	   (eq? (combination-operator predicate) (ucode-primitive not)))
      (make-conditional (car (combination-operands predicate))
			alternative
			consequent)
      (&typed-triple-cons (ucode-type conditional)
			  predicate
			  consequent
			  alternative)))

(define (conditional? object)
  (object-type? (ucode-type conditional) object))

(define-guarantee conditional "SCode conditional")

(define undefined-conditional-branch unspecific)

(define (conditional-predicate conditional)
  (guarantee-conditional conditional 'CONDITIONAL-PREDICATE)
  (&triple-first conditional))

(define (conditional-consequent conditional)
  (guarantee-conditional conditional 'CONDITIONAL-CONSEQUENT)
  (&triple-second conditional))

(define (conditional-alternative conditional)
  (guarantee-conditional conditional 'CONDITIONAL-ALTERNATIVE)
  (&triple-third conditional))

(define (conditional-components conditional receiver)
  (receiver (conditional-predicate conditional)
	    (conditional-consequent conditional)
	    (conditional-alternative conditional)))

(define (conditional-subexpressions expression)
  (conditional-components expression list))

;;;; Disjunction

(define (make-disjunction predicate alternative)
  (if (and (combination? predicate)
	   (eq? (combination-operator predicate) (ucode-primitive not)))
      (make-conditional (car (combination-operands predicate))
			alternative
			true)
      (&typed-pair-cons (ucode-type disjunction) predicate alternative)))

(define (disjunction? object)
  (object-type? (ucode-type disjunction) object))

(define-guarantee disjunction "SCode disjunction")

(define (disjunction-predicate disjunction)
  (guarantee-disjunction disjunction 'DISJUNCTION-PREDICATE)
  (&pair-car disjunction))

(define (disjunction-alternative disjunction)
  (guarantee-disjunction disjunction 'DISJUNCTION-ALTERNATIVE)
  (&pair-cdr disjunction))

(define (disjunction-components disjunction receiver)
  (receiver (disjunction-predicate disjunction)
	    (disjunction-alternative disjunction)))

(define (disjunction-subexpressions expression)
  (disjunction-components expression list))

;;;; Combination

(define (combination? object)
  (or (object-type? (ucode-type combination) object)
      (object-type? (ucode-type combination-1) object)
      (object-type? (ucode-type combination-2) object)
      (object-type? (ucode-type primitive-combination-0) object)
      (object-type? (ucode-type primitive-combination-1) object)
      (object-type? (ucode-type primitive-combination-2) object)
      (object-type? (ucode-type primitive-combination-3) object)))

(define-guarantee combination "SCode combination")

(define (make-combination operator operands)
  (if (and (procedure? operator)
	   (not (primitive-procedure? operator)))
      (error:wrong-type-argument operator
				 "operator expression"
				 'MAKE-COMBINATION))
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

(define-syntax combination-dispatch
  (sc-macro-transformer
   (lambda (form environment)
     (let ((name (list-ref form 1))
	   (combination (close-syntax (list-ref form 2) environment))
	   (case-0 (close-syntax (list-ref form 3) environment))
	   (case-1 (close-syntax (list-ref form 4) environment))
	   (case-2 (close-syntax (list-ref form 5) environment))
	   (case-n (close-syntax (list-ref form 6) environment)))
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
	       (ERROR:NOT-COMBINATION ,combination ',name)))))))

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

(define (combination-subexpressions expression)
  (combination-components expression cons))

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

(define-guarantee unassigned? "SCode unassigned test")

(define (unassigned?-name expression)
  (guarantee-unassigned? expression 'UNASSIGNED?-NAME)
  (cadr (combination-operands expression)))

(define (unassigned?-components expression receiver)
  (receiver (unassigned?-name expression)))