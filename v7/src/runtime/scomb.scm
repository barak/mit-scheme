;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1985 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; SCODE Combinator Abstractions

(declare (usual-integrations))

;;;; SEQUENCE

(define sequence?)
(define make-sequence)
(define sequence-actions)
(let ()

(define type-2
  (microcode-type 'SEQUENCE-2))

(define type-3
  (microcode-type 'SEQUENCE-3))

(set! sequence?
(named-lambda (sequence? object)
  (or (primitive-type? type-2 object)
      (primitive-type? type-3 object))))

(set! make-sequence
(lambda (actions)
  (if (null? actions)
      (error "MAKE-SEQUENCE: No actions")
      (actions->sequence actions))))

(define (actions->sequence actions)
  (cond ((null? (cdr actions))
	 (car actions))
	((null? (cddr actions))
	 (&typed-pair-cons type-2
			   (car actions)
			   (cadr actions)))
	(else
	 (&typed-triple-cons type-3
			     (car actions)
			     (cadr actions)
			     (actions->sequence (cddr actions))))))

(set! sequence-actions
(named-lambda (sequence-actions sequence)
  (cond ((primitive-type? type-2 sequence)
	 (append! (sequence-actions (&pair-car sequence))
		  (sequence-actions (&pair-cdr sequence))))
	((primitive-type? type-3 sequence)
	 (append! (sequence-actions (&triple-first sequence))
		  (sequence-actions (&triple-second sequence))
		  (sequence-actions (&triple-third sequence))))
	(else
	 (list sequence)))))

)

(define (sequence-components sequence receiver)
  (receiver (sequence-actions sequence)))

;;;; CONDITIONAL

(define conditional?)
(define make-conditional)
(let ()

(define type
  (microcode-type 'CONDITIONAL))

(set! conditional?
(named-lambda (conditional? object)
  (primitive-type? type object)))

(set! make-conditional
(named-lambda (make-conditional predicate consequent alternative)
  (if (combination? predicate)
      (combination-components predicate
	(lambda (operator operands)
	  (if (eq? operator not)
	      (make-conditional (first operands)
				alternative
				consequent)
	      (&typed-triple-cons type
				  predicate
				  consequent
				  alternative))))
      (&typed-triple-cons type predicate consequent alternative))))

)

(define (conditional-components conditional receiver)
  (receiver (conditional-predicate conditional)
	    (conditional-consequent conditional)
	    (conditional-alternative conditional)))

(define conditional-predicate &triple-first)
(define conditional-consequent &triple-second)
(define conditional-alternative &triple-third)

;;;; DISJUNCTION

(define disjunction?)
(define make-disjunction)
(let ()

(define type
  (microcode-type 'DISJUNCTION))

(set! disjunction?
(named-lambda (disjunction? object)
  (primitive-type? type object)))

(set! make-disjunction
(named-lambda (make-disjunction predicate alternative)
  (if (combination? predicate)
      (combination-components predicate
	(lambda (operator operands)
	  (if (eq? operator not)
	      (make-conditional (first operands) alternative #!TRUE)
	      (&typed-pair-cons type predicate alternative))))
      (&typed-pair-cons type predicate alternative))))

)

(define (disjunction-components disjunction receiver)
  (receiver (disjunction-predicate disjunction)
	    (disjunction-alternative disjunction)))

(define disjunction-predicate &pair-car)
(define disjunction-alternative &pair-cdr)

;;;; COMBINATION

(define combination?)
(define make-combination)
(define combination-size)
(define combination-components)
(define combination-operator)
(define combination-operands)
(let ()

(define type-1 (microcode-type 'COMBINATION-1))
(define type-2 (microcode-type 'COMBINATION-2))
(define type-N (microcode-type 'COMBINATION))
(define p-type (microcode-type 'PRIMITIVE))
(define p-type-0 (microcode-type 'PRIMITIVE-COMBINATION-0))
(define p-type-1 (microcode-type 'PRIMITIVE-COMBINATION-1))
(define p-type-2 (microcode-type 'PRIMITIVE-COMBINATION-2))
(define p-type-3 (microcode-type 'PRIMITIVE-COMBINATION-3))

(define (primitive-procedure? object)
  (primitive-type? p-type object))

(set! combination?
(named-lambda (combination? object)
  (or (primitive-type? type-1 object)
      (primitive-type? type-2 object)
      (primitive-type? type-N object)
      (primitive-type? p-type-0 object)
      (primitive-type? p-type-1 object)
      (primitive-type? p-type-2 object)
      (primitive-type? p-type-3 object))))

(set! make-combination
(lambda (operator operands)
  (cond ((and (memq operator constant-folding-operators)
	      (all-constants? operands))
	 (apply operator operands))
	((null? operands)
	 (if (and (primitive-procedure? operator)
		  (= (primitive-procedure-arity operator) 0))
	     (primitive-set-type p-type-0 operator)
	     (&typed-vector-cons type-N (cons operator '()))))
	((null? (cdr operands))
	 (&typed-pair-cons
	  (if (and (primitive-procedure? operator)
		   (= (primitive-procedure-arity operator) 1))
	      p-type-1
	      type-1)
	  operator
	  (car operands)))
	((null? (cddr operands))
	 (&typed-triple-cons
	  (if (and (primitive-procedure? operator)
		   (= (primitive-procedure-arity operator) 2))
	      p-type-2
	      type-2)
	  operator
	  (car operands)
	  (cadr operands)))
	(else
	 (&typed-vector-cons
	  (if (and (null? (cdddr operands))
		   (primitive-procedure? operator)
		   (= (primitive-procedure-arity operator) 3))
	      p-type-3
	      type-N)
	  (cons operator operands))))))

(define constant-folding-operators
  (map make-primitive-procedure
       '(PRIMITIVE-TYPE
	 CAR CDR VECTOR-LENGTH VECTOR-REF
	 &+ &- &* &/ INTEGER-DIVIDE 1+ -1+
	 TRUNCATE ROUND FLOOR CEILING
	 SQRT EXP LOG SIN COS &ATAN)))

(define (all-constants? expressions)
  (or (null? expressions)
      (and (scode-constant? (car expressions))
	   (all-constants? (cdr expressions)))))

(set! combination-size
(lambda (combination)
  (cond ((primitive-type? p-type-0 combination)
	 1)
	((or (primitive-type? type-1 combination)
	     (primitive-type? p-type-1 combination))
	 2)
	((or (primitive-type? type-2 combination)
	     (primitive-type? p-type-2 combination))
	 3)
	((primitive-type? p-type-3 combination)
	 4)
	((primitive-type? type-N combination)
	 (&vector-size combination))
	(else
	 (error "Not a combination -- COMBINATION-SIZE" combination)))))

(set! combination-operator
(lambda (combination)
  (cond ((primitive-type? p-type-0 combination)
	 (primitive-set-type p-type combination))
	((or (primitive-type? type-1 combination)
	     (primitive-type? p-type-1 combination))
	 (&pair-car combination))
	((or (primitive-type? type-2 combination)
	     (primitive-type? p-type-2 combination))
	 (&triple-first combination))
	((or (primitive-type? p-type-3 combination)
	     (primitive-type? type-N combination))
	 (&vector-ref combination 0))
	(else
	 (error "Not a combination -- COMBINATION-OPERATOR"
		combination)))))

(set! combination-operands
(lambda (combination)
  (cond ((primitive-type? p-type-0 combination)
	 '())
	((or (primitive-type? type-1 combination)
	     (primitive-type? p-type-1 combination))
	 (list (&pair-cdr combination)))
	((or (primitive-type? type-2 combination)
	     (primitive-type? p-type-2 combination))
	 (list (&triple-second combination)
	       (&triple-third combination)))
	((or (primitive-type? p-type-3 combination)
	     (primitive-type? type-N combination))
	 (&subvector-to-list combination 1 (&vector-size combination)))
	(else
	 (error "Not a combination -- COMBINATION-OPERANDS"
		combination)))))

(set! combination-components
(lambda (combination receiver)
  (cond ((primitive-type? p-type-0 combination)
	 (receiver (primitive-set-type p-type combination)
		   '()))
	((or (primitive-type? type-1 combination)
	     (primitive-type? p-type-1 combination))
	 (receiver (&pair-car combination)
		   (list (&pair-cdr combination))))
	((or (primitive-type? type-2 combination)
	     (primitive-type? p-type-2 combination))
	 (receiver (&triple-first combination)
		   (list (&triple-second combination)
			 (&triple-third combination))))
	((or (primitive-type? p-type-3 combination)
	     (primitive-type? type-N combination))
	 (receiver (&vector-ref combination 0)
		   (&subvector-to-list combination 1
				       (&vector-size combination))))
	(else
	 (error "Not a combination -- COMBINATION-COMPONENTS"
		combination)))))

)

;;;; UNASSIGNED?

(define unassigned??)
(define make-unassigned?)
(define unbound??)
(define make-unbound?)
(let ()

(define ((envop-characteristic envop) object)
  (and (combination? object)
       (combination-components object
	 (lambda (operator operands)
	   (and (eq? operator envop)
		(the-environment? (first operands))
		(symbol? (second operands)))))))

(define ((envop-maker envop) name)
  (make-combination envop
		    (list (make-the-environment) name)))

(set! unassigned??
      (envop-characteristic lexical-unassigned?))

(set! make-unassigned?
      (envop-maker lexical-unassigned?))

(set! unbound??
      (envop-characteristic lexical-unbound?))

(set! make-unbound?
      (envop-maker lexical-unbound?))

)

(define (unassigned?-name unassigned?)
  (second (combination-operands unassigned?)))

(define (unassigned?-components unassigned? receiver)
  (receiver (unassigned?-name unassigned?)))

(define unbound?-name unassigned?-name)
(define unbound?-components unassigned?-components)
(define unbound?-components unassigned?-components)