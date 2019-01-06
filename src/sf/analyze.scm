#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; SCode Optimizer: Expression analysis
;;; package: (scode-optimizer analyze)

(declare (usual-integrations)
         (integrate-external "object"))

;;; EXPRESSION/ALWAYS-FALSE?

;; True iff expression can be shown to always return #F.
;; That is, the expression counts as #f to a conditional.
;; Expression is not shown to be side-effect free.
(declare (integrate-operator expression/always-false?))
(define (expression/always-false? expression)
  ((expression/method always-false?-dispatch-vector expression) expression))

(define always-false?-dispatch-vector
  (expression/make-dispatch-vector))

(define define-method/always-false?
  (expression/make-method-definer always-false?-dispatch-vector))

(define-method/always-false? 'access false-procedure)

(define-method/always-false? 'assignment false-procedure)

(define-method/always-false? 'combination
  (lambda (expression)
    (cond ((expression/call-to-not? expression)
           (expression/never-false? (first (combination/operands expression))))
          ((procedure? (combination/operator expression))
           (expression/always-false?
	    (procedure/body (combination/operator expression))))
          (else #f))))

(define-method/always-false? 'conditional
  (lambda (expression)
    (and (or (expression/always-false? (conditional/predicate expression))
             (expression/always-false? (conditional/consequent expression)))
         (or (expression/never-false? (conditional/predicate expression))
             (expression/always-false? (conditional/alternative expression))))))

(define-method/always-false? 'constant
  (lambda (expression)
    (not (constant/value expression))))

(define-method/always-false? 'declaration
  (lambda (expression)
    (expression/always-false?
     (declaration/expression expression))))

;; A promise is not a false value.
(define-method/always-false? 'delay false-procedure)

(define-method/always-false? 'disjunction
  (lambda (expression)
    (and (expression/always-false? (disjunction/predicate expression))
         (expression/always-false? (disjunction/alternative expression)))))

(define-method/always-false? 'open-block
  (lambda (expression)
    (expression/always-false?
     (last (open-block/actions expression)))))

;; A closure is not a false value.
(define-method/always-false? 'procedure false-procedure)

(define-method/always-false? 'quotation false-procedure)

(define-method/always-false? 'reference false-procedure)

(define-method/always-false? 'sequence
  (lambda (expression)
    (expression/always-false?
     (last (sequence/actions expression)))))

(define-method/always-false? 'the-environment false-procedure)

;;; EXPRESSION/BOOLEAN?
;;
;; T if expression can be shown to return only #T or #F.
;;
(declare (integrate-operator expression/boolean?))
(define (expression/boolean? expression)
  ((expression/method boolean?-dispatch-vector expression) expression))

(define boolean?-dispatch-vector
  (expression/make-dispatch-vector))

(define define-method/boolean?
  (expression/make-method-definer boolean?-dispatch-vector))

(define-method/boolean? 'access false-procedure)

(define-method/boolean? 'assignment false-procedure)

(define-method/boolean? 'combination
  (lambda (expression)
    (or (expression/call-to-boolean-predicate? expression)
        (and (procedure? (combination/operator expression))
             (boolean? (procedure/body (combination/operator expression)))))))

(define-method/boolean? 'conditional
  (lambda (expression)
    (and (or (expression/always-false? (conditional/predicate expression))
             (expression/boolean? (conditional/consequent expression)))
         (or (expression/never-false? (conditional/predicate expression))
             (expression/boolean? (conditional/alternative expression))))))

(define-method/boolean? 'constant
  (lambda (expression)
    ;; jrm:  do not accept unspecific here.
    (or (not (constant/value expression))
        (eq? (constant/value expression) #t))))

(define-method/boolean? 'declaration
  (lambda (expression)
    (expression/boolean? (declaration/expression expression))))

(define-method/boolean? 'delay  false-procedure)

(define-method/boolean? 'disjunction
  (lambda (expression)
    (and (expression/boolean? (disjunction/predicate expression))
         (or (expression/never-false? (disjunction/predicate expression))
             (expression/boolean? (disjunction/alternative expression))))))

(define-method/boolean? 'open-block
  (lambda (expression)
    (expression/boolean?
     (last (open-block/actions expression)))))

(define-method/boolean? 'procedure false-procedure)

(define-method/boolean? 'quotation false-procedure)

(define-method/boolean? 'reference false-procedure)

(define-method/boolean? 'sequence
  (lambda (expression)
    (expression/boolean? (last (sequence/actions expression)))))

(define-method/boolean? 'the-environment false-procedure)

;;; EXPRESSION/EFFECT-FREE?
;;
;; True iff evaluation of expression has no side effects.
(declare (integrate-operator expression/effect-free?))
(define (expression/effect-free? expression)
  ((expression/method effect-free?-dispatch-vector expression) expression))

(define effect-free?-dispatch-vector
  (expression/make-dispatch-vector))

(define define-method/effect-free?
  (expression/make-method-definer effect-free?-dispatch-vector))

(define-method/effect-free? 'access
  (lambda (expression)
    (expression/effect-free? (access/environment expression))))

(define-method/effect-free? 'assignment false-procedure)

(define-method/effect-free? 'combination
  (lambda (expression)
    (and (every expression/effect-free? (combination/operands expression))
         (or (expression/call-to-effect-free-primitive? expression)
             (and (procedure? (combination/operator expression))
                  (expression/effect-free?
		   (procedure/body (combination/operator expression))))))))

(define-method/effect-free? 'conditional
  (lambda (expression)
    (and (expression/effect-free? (conditional/predicate expression))
         (or (expression/always-false? (conditional/predicate expression))
             (expression/effect-free? (conditional/consequent expression)))
         (or (expression/never-false? (conditional/predicate expression))
             (expression/effect-free? (conditional/alternative expression))))))

(define-method/effect-free? 'constant true-procedure)

(define-method/effect-free? 'declaration
  (lambda (expression)
    (expression/effect-free? (declaration/expression expression))))

;; Consing a promise is not considered an effect.
(define-method/effect-free? 'delay true-procedure)

(define-method/effect-free? 'disjunction
  (lambda (expression)
    (and (expression/effect-free? (disjunction/predicate expression))
         (or (expression/never-false? (disjunction/predicate expression))
             (expression/effect-free? (disjunction/alternative expression))))))

;; This could be smarter and skip the assignments
;; done for the letrec, but it is easier to just
;; assume it causes effects.
(define-method/effect-free? 'open-block
  (lambda (expression)
    (declare (ignore expression))
    #f))

;; Just consing a closure is not considered a side-effect.
(define-method/effect-free? 'procedure true-procedure)

(define-method/effect-free? 'quotation false-procedure)

(define-method/effect-free? 'reference true-procedure)

(define-method/effect-free? 'sequence
  (lambda (expression)
    (every expression/effect-free? (sequence/actions expression))))

(define-method/effect-free? 'the-environment true-procedure)

;;; EXPRESSION/FREE-VARIABLES
;;
;; Returns an EQ? LSET of the free variables in an expression.

(declare (integrate-operator expression/free-variables))

(define (expression/free-variables expression)
  ((expression/method free-variables-dispatch-vector expression) expression))

(define (expressions/free-variables expressions)
  (fold-left (lambda (answer expression)
               (lset-union eq? answer (expression/free-variables expression)))
             (no-free-variables)
             expressions))

(define free-variables-dispatch-vector
  (expression/make-dispatch-vector))

(define define-method/free-variables
  (expression/make-method-definer free-variables-dispatch-vector))

(define-method/free-variables 'access
  (lambda (expression)
    (expression/free-variables (access/environment expression))))

(define-method/free-variables 'assignment
  (lambda (expression)
    (lset-adjoin eq?
                 (expression/free-variables (assignment/value expression))
                 (assignment/variable expression))))

(define-method/free-variables 'combination
  (lambda (expression)
    (lset-union
     eq?
     (expression/free-variables (combination/operator expression))
     (expressions/free-variables (combination/operands expression)))))

(define-method/free-variables 'conditional
  (lambda (expression)
    (lset-union
     eq?
     (expression/free-variables (conditional/predicate expression))
     (if (expression/always-false? (conditional/predicate expression))
	 (no-free-variables)
	 (expression/free-variables (conditional/consequent expression)))
     (if (expression/never-false? (conditional/predicate expression))
	 (no-free-variables)
	 (expression/free-variables (conditional/alternative expression))))))

(define-method/free-variables 'constant
  (lambda (expression)
    expression
    (no-free-variables)))

(define-method/free-variables 'declaration
  (lambda (expression)
    (expression/free-variables (declaration/expression expression))))

(define-method/free-variables 'delay
  (lambda (expression)
    (expression/free-variables (delay/expression expression))))

(define-method/free-variables 'disjunction
  (lambda (expression)
    (lset-union
     eq?
     (expression/free-variables (disjunction/predicate expression))
     (if (expression/never-false? (disjunction/predicate expression))
	 (no-free-variables)
	 (expression/free-variables (disjunction/alternative expression))))))

(define-method/free-variables 'open-block
  (lambda (expression)
    (let ((omit (block/bound-variables (open-block/block expression))))
     (fold-left (lambda (variables action)
                  (if (eq? action open-block/value-marker)
                      variables
                      (lset-union eq?
				  variables
				  (lset-difference
				   eq?
				   (expression/free-variables action)
				   omit))))
                (lset-difference eq?
				 (expressions/free-variables
				  (open-block/values expression))
				 omit)
                (open-block/actions expression)))))

(define-method/free-variables 'procedure
  (lambda (expression)
    (lset-difference eq?
     (expression/free-variables (procedure/body expression))
     (block/bound-variables (procedure/block expression)))))

(define-method/free-variables 'quotation
  (lambda (expression)
    (declare (ignore expression))
    (no-free-variables)))

(define-method/free-variables 'reference
  (lambda (expression)
    (singleton-variable (reference/variable expression))))

(define-method/free-variables 'sequence
  (lambda (expression)
    (expressions/free-variables (sequence/actions expression))))

(define-method/free-variables 'the-environment
  (lambda (expression)
    (declare (ignore expression))
    (no-free-variables)))

(define-integrable (no-free-variables)
  '())

(define-integrable (singleton-variable variable)
  (list variable))

;;; EXPRESSION/FREE-VARIABLE? <expression> <variable>
;;
;; Test if a particular <variable> occurs free in <expression>.  Faster
;; and cheaper than collecting the entire free variable set and then
;; using memq.

(define (expression/free-variable? expression variable)
  ((expression/method is-free-dispatch-vector expression) expression variable))

(define (expressions/free-variable? expressions variable)
  (fold-left (lambda (answer expression)
               (or answer
                   (expression/free-variable? expression variable)))
             #f
             expressions))

(define is-free-dispatch-vector
  (expression/make-dispatch-vector))

(define define-method/free-variable?
  (expression/make-method-definer is-free-dispatch-vector))

(define-method/free-variable? 'access
  (lambda (expression variable)
    (expression/free-variable? (access/environment expression) variable)))

(define-method/free-variable? 'assignment
  (lambda (expression variable)
    (or (eq? variable (assignment/variable expression))
        (expression/free-variable? (assignment/value expression) variable))))

(define-method/free-variable? 'combination
  (lambda (expression variable)
    (or (expression/free-variable? (combination/operator expression) variable)
        (expressions/free-variable?
	 (combination/operands expression) variable))))

(define-method/free-variable? 'conditional
  (lambda (expression variable)
    (or (expression/free-variable? (conditional/predicate expression) variable)
        (cond ((expression/always-false? (conditional/predicate expression))
               (expression/free-variable? (conditional/alternative expression)
					  variable))
              ((expression/never-false? (conditional/predicate expression))
               (expression/free-variable? (conditional/consequent expression)
					  variable))
              ((expression/free-variable? (conditional/consequent expression)
					  variable))
              (else
	       (expression/free-variable? (conditional/alternative expression)
					  variable))))))

(define-method/free-variable? 'constant false-procedure)

(define-method/free-variable? 'declaration
  (lambda (expression variable)
    (expression/free-variable? (declaration/expression expression) variable)))

(define-method/free-variable? 'delay
  (lambda (expression variable)
    (expression/free-variable? (delay/expression expression) variable)))

(define-method/free-variable? 'disjunction
  (lambda (expression variable)
    (or (expression/free-variable? (disjunction/predicate expression) variable)
        (if (expression/never-false? (disjunction/predicate expression))
            #f
            (expression/free-variable? (disjunction/alternative expression)
				       variable)))))

(define-method/free-variable? 'open-block
  (lambda (expression variable)
    (fold-left (lambda (answer action)
                 (or answer
                     (if (eq? action open-block/value-marker)
                         #f
                         (expression/free-variable? action variable))))
               #f
               (open-block/actions expression))))

(define-method/free-variable? 'procedure
  (lambda (expression variable)
    (expression/free-variable? (procedure/body expression) variable)))

(define-method/free-variable? 'quotation false-procedure)

(define-method/free-variable? 'reference
  (lambda (expression variable)
    (eq? (reference/variable expression) variable)))

(define-method/free-variable? 'sequence
  (lambda (expression variable)
  (fold-left (lambda (answer action)
               (or answer
                   (if (eq? action open-block/value-marker)
                       #f
                       (expression/free-variable? action variable))))
             #f
             (sequence/actions expression))))

(define-method/free-variable? 'the-environment false-procedure)

;;; EXPRESSION/FREE-VARIABLE-INFO <expression> <variable>
;;
;; Returns a PAIR, the car of which contains a count of the number
;; of times the variable appears as an operator, the cdr contains
;; the number of times the variable appears as an argument.
;; Used to determine if adding an INTEGRATE-OPERATOR declaration
;; is a good idea.

(define (expression/free-variable-info expression variable)
  (expression/free-variable-info-dispatch expression variable (cons 0 0)))

(define (expression/free-variable-info-dispatch expression variable info)
  ((expression/method free-info-dispatch-vector expression)
   expression variable info))

(define (expressions/free-variable-info expressions variable info)
  (fold-left (lambda (answer expression)
               (expression/free-variable-info-dispatch expression variable
						       answer))
             info
             expressions))

(define free-info-dispatch-vector
  (expression/make-dispatch-vector))

(define define-method/free-variable-info
  (expression/make-method-definer free-info-dispatch-vector))

(define-method/free-variable-info 'access
  (lambda (expression variable info)
    (expression/free-variable-info-dispatch (access/environment expression)
					    variable info)))

(define-method/free-variable-info 'assignment
  (lambda (expression variable info)
    (or (eq? variable (assignment/variable expression))
        (expression/free-variable-info-dispatch (assignment/value expression)
						variable info))))

(define-method/free-variable-info 'combination
  (lambda (expression variable info)
    (let ((operator (combination/operator expression))
          (inner-info
	   (expressions/free-variable-info (combination/operands expression)
					   variable info)))
      (if (and (reference? operator)
               (eq? (reference/variable operator) variable))
          (cons (fix:1+ (car inner-info)) (cdr inner-info))
          (expression/free-variable-info-dispatch operator variable
						  inner-info)))))

(define-method/free-variable-info 'conditional
  (lambda (expression variable info)
    (expression/free-variable-info-dispatch
     (conditional/predicate expression) variable
     (expression/free-variable-info-dispatch
      (conditional/consequent expression) variable
      (expression/free-variable-info-dispatch
       (conditional/alternative expression)
       variable info)))))

(define-method/free-variable-info 'constant
  (lambda (expression variable info)
    (declare (ignore expression variable))
    info))

(define-method/free-variable-info 'declaration
  (lambda (expression variable info)
    (expression/free-variable-info-dispatch (declaration/expression expression)
					    variable info)))

(define-method/free-variable-info 'delay
  (lambda (expression variable info)
    (expression/free-variable-info-dispatch (delay/expression expression)
					    variable info)))

(define-method/free-variable-info 'disjunction
  (lambda (expression variable info)
    (expression/free-variable-info-dispatch
     (disjunction/predicate expression) variable
     (expression/free-variable-info-dispatch
      (disjunction/alternative expression) variable
      info))))

(define-method/free-variable-info 'open-block
  (lambda (expression variable info)
    (fold-left (lambda (info action)
                 (if (eq? action open-block/value-marker)
                     info
                     (expression/free-variable-info-dispatch action variable
							     info)))
               info
               (open-block/actions expression))))

(define-method/free-variable-info 'procedure
  (lambda (expression variable info)
    (expression/free-variable-info-dispatch (procedure/body expression)
					    variable info)))

(define-method/free-variable-info 'quotation
  (lambda (expression variable info)
    (declare (ignore expression variable))
    info))

(define-method/free-variable-info 'reference
  (lambda (expression variable info)
    (if (eq? (reference/variable expression) variable)
        (cons (car info) (fix:1+ (cdr info)))
        info)))

(define-method/free-variable-info 'sequence
  (lambda (expression variable info)
    (expressions/free-variable-info (sequence/actions expression)
				    variable info)))

(define-method/free-variable-info 'the-environment
  (lambda (expression variable info)
    (declare (ignore expression variable))
    info))

;;; EXPRESSION/NEVER-FALSE?
;;
;; True iff expression can be shown to never return #F.
;; That is, the expression counts as #t to a conditional.
;; Expression is not shown to be side-effect free.
(declare (integrate-operator expression/never-false?))
(define (expression/never-false? expression)
  ((expression/method never-false?-dispatch-vector expression) expression))

(define never-false?-dispatch-vector
  (expression/make-dispatch-vector))

(define define-method/never-false?
  (expression/make-method-definer never-false?-dispatch-vector))

(define-method/never-false? 'access false-procedure)

(define-method/never-false? 'assignment false-procedure)

(define-method/never-false? 'combination
  (lambda (expression)
    (cond ((expression/call-to-not? expression)
           (expression/always-false? (first (combination/operands expression))))
          ((procedure? (combination/operator expression))
           (expression/never-false?
	    (procedure/body (combination/operator expression))))
          (else #f))))

(define-method/never-false? 'conditional
  (lambda (expression)
    (and (or (expression/always-false? (conditional/predicate expression))
             (expression/never-false? (conditional/consequent expression)))
         (or (expression/never-false? (conditional/predicate expression))
             (expression/never-false? (conditional/alternative expression))))))

(define-method/never-false? 'constant        constant/value)

(define-method/never-false? 'declaration
  (lambda (expression)
    (expression/never-false? (declaration/expression expression))))

(define-method/never-false? 'delay true-procedure)

(define-method/never-false? 'disjunction
  (lambda (expression)
    (or (expression/never-false? (disjunction/predicate expression))
        (expression/never-false? (disjunction/alternative expression)))))

(define-method/never-false? 'open-block
  (lambda (expression)
    (expression/never-false?
     (last (open-block/actions expression)))))

(define-method/never-false? 'procedure true-procedure)

(define-method/never-false? 'quotation false-procedure)

(define-method/never-false? 'reference false-procedure)

(define-method/never-false? 'sequence
  (lambda (expression)
    (expression/never-false? (last (sequence/actions expression)))))

(define-method/never-false? 'the-environment true-procedure)

;;; EXPRESSION/PURE-FALSE?

;; True iff all paths through expression end in returning
;; exactly #F or unspecified, and no path has side effects.
;; Expression is observationally equivalent to #F.
(define (expression/pure-false? expression)
  ((expression/method pure-false?-dispatch-vector expression) expression))

(define pure-false?-dispatch-vector
  (expression/make-dispatch-vector))

(define define-method/pure-false?
  (expression/make-method-definer pure-false?-dispatch-vector))

(define-method/pure-false? 'access false-procedure)

(define-method/pure-false? 'assignment false-procedure)

(define-method/pure-false? 'combination
  (lambda (expression)
    (cond ((expression/call-to-not? expression)
           (expression/pure-true? (first (combination/operands expression))))
          ((procedure? (combination/operator expression))
           (and (every expression/effect-free?
		       (combination/operands expression))
                (expression/pure-false?
		 (procedure/body (combination/operator expression)))))
          (else #f))))

(define-method/pure-false? 'conditional
  (lambda (expression)
    (and (expression/effect-free? (conditional/predicate expression))
         (or (expression/always-false? (conditional/predicate expression))
             (expression/pure-false? (conditional/consequent expression)))
         (or (expression/never-false? (conditional/predicate expression))
             (expression/pure-false? (conditional/alternative expression))))))

(define-method/pure-false? 'constant
  (lambda (expression)
    (not (constant/value expression))))

(define-method/pure-false? 'declaration
  (lambda (expression)
    (expression/pure-false?
     (declaration/expression expression))))

(define-method/pure-false? 'delay false-procedure)

(define-method/pure-false? 'disjunction
  (lambda (expression)
    (and (expression/pure-false? (disjunction/predicate expression))
         (expression/pure-false? (disjunction/alternative expression)))))

;; Could be smarter
(define-method/pure-false? 'open-block false-procedure)

(define-method/pure-false? 'procedure false-procedure)

(define-method/pure-false? 'quotation false-procedure)

(define-method/pure-false? 'reference false-procedure)

(define-method/pure-false? 'sequence
  (lambda (expression)
    (and (every expression/effect-free? ; unlikely
		(except-last-pair (sequence/actions expression)))
         (expression/pure-false? (last (sequence/actions expression))))))

(define-method/pure-false? 'the-environment false-procedure)

;;; EXPRESSION/PURE-TRUE?
;;
;; True iff all paths through expression end in returning
;; exactly #T or unspecified, and no path has side effects.
;; Expression is observationally equivalent to #T.
(declare (integrate-operator expression/pure-true?))
(define (expression/pure-true? expression)
  ((expression/method pure-true?-dispatch-vector expression) expression))

(define pure-true?-dispatch-vector
  (expression/make-dispatch-vector))

(define define-method/pure-true?
  (expression/make-method-definer pure-true?-dispatch-vector))

(define-method/pure-true? 'access false-procedure)

(define-method/pure-true? 'assignment false-procedure)

(define-method/pure-true? 'combination
  (lambda (expression)
    (cond ((expression/call-to-not? expression)
           (expression/pure-false? (first (combination/operands expression))))
          ((procedure? (combination/operator expression))
           (and (every expression/effect-free?
		       (combination/operands expression))
                (expression/pure-true?
		 (procedure/body (combination/operator expression)))))
          (else #f))))

(define-method/pure-true? 'conditional
  (lambda (expression)
    (and (expression/effect-free? (conditional/predicate expression))
         (or (expression/always-false? (conditional/predicate expression))
             (expression/pure-true? (conditional/consequent expression)))
         (or (expression/never-false? (conditional/predicate expression))
             (expression/pure-true? (conditional/alternative expression))))))

(define-method/pure-true? 'constant
  (lambda (expression)
    (eq? (constant/value expression) #t)))

(define-method/pure-true? 'declaration
  (lambda (expression)
    (expression/pure-true? (declaration/expression expression))))

(define-method/pure-true? 'delay false-procedure)

(define-method/pure-true? 'disjunction
  (lambda (expression)
    (and (expression/effect-free? (disjunction/predicate expression))
         (expression/boolean? (disjunction/predicate expression))
         (expression/pure-true? (disjunction/alternative expression)))))

(define-method/pure-true? 'open-block false-procedure)

(define-method/pure-true? 'procedure false-procedure)

(define-method/pure-true? 'quotation false-procedure)

(define-method/pure-true? 'reference false-procedure)

(define-method/pure-true? 'sequence
  (lambda (expression)
    (and (every expression/effect-free?
		(except-last-pair (sequence/actions expression)))
         (expression/pure-true? (last (sequence/actions expression))))))

(define-method/pure-true? 'the-environment false-procedure)

;;; EXPRESSION/SIZE <expr>
;;
;; Returns an integer count of the number of SCode nodes in the expression.
;; Used to avoid exponential code bloat when adding INTEGRATE-OPERATOR
;; declarations.
(declare (integrate-operator expression/size))

(define (expression/size expression)
  ((expression/method size-dispatch-vector expression) expression))

(define size-dispatch-vector
  (expression/make-dispatch-vector))

(define define-method/size
  (expression/make-method-definer size-dispatch-vector))

(define-method/size 'access
  (lambda (expression)
    (fix:1+ (expression/size (access/environment expression)))))

(define-method/size 'assignment
  (lambda (expression)
    (fix:1+ (expression/size (assignment/value expression)))))

(define-method/size 'combination
  (lambda (expression)
    (fold-left (lambda (total operand)
                 (fix:+ total (expression/size operand)))
               (fix:1+ (expression/size (combination/operator expression)))
               (combination/operands expression))))

(define-method/size 'conditional
  (lambda (expression)
    (fix:+
     (expression/size (conditional/predicate expression))
     (fix:+
      (expression/size (conditional/consequent expression))
      (fix:1+ (expression/size (conditional/alternative expression)))))))

(define-method/size 'constant
  (lambda (expression) (declare (ignore expression)) 1))

(define-method/size 'declaration
  (lambda (expression)
    (fix:1+ (expression/size (declaration/expression expression)))))

(define-method/size 'delay
  (lambda (expression)
    (fix:1+ (expression/size (delay/expression expression)))))

(define-method/size 'disjunction
  (lambda (expression)
    (fix:+ (expression/size (disjunction/predicate expression))
           (fix:1+ (expression/size (disjunction/alternative expression))))))

(define-method/size 'open-block
  (lambda (expression)
    (fold-left (lambda (total action)
                (if (eq? action open-block/value-marker)
                    total
                    (fix:+ total (expression/size action))))
              1
              (open-block/actions expression))))

(define-method/size 'procedure
  (lambda (expression)
    (fix:1+ (expression/size (procedure/body expression)))))

(define-method/size 'quotation
  (lambda (expression)
    (fix:1+ (expression/size (quotation/expression expression)))))

(define-method/size 'reference
  (lambda (expression)
    (declare (ignore expression))
    1))

(define-method/size 'sequence
  (lambda (expression)
    (fold-left (lambda (total action)
                 (fix:+ total (expression/size action)))
               1
               (sequence/actions expression))))

;;; EXPRESSION->list <expr>
;;
;; Returns an list representation of the SCode nodes in the expression.
;; Used for debugging sf.

(define (expression->list expression)
  ((expression/method expression->list-dispatch-vector expression) expression))

(define expression->list-dispatch-vector
  (expression/make-dispatch-vector))

(define define-method/expression->list
  (expression/make-method-definer expression->list-dispatch-vector))

(define-method/expression->list 'access
  (lambda (expression)
    `(access ,(access/name expression)
	     ,(expression->list (access/environment expression)))))

(define-method/expression->list 'assignment
  (lambda (expression)
    `(set! ,(assignment/variable expression)
	   ,(expression->list (assignment/value expression)))))

(define-method/expression->list 'combination
  (lambda (expression)
    (cons (expression->list (combination/operator expression))
	  (map expression->list (combination/operands expression)))))

(define-method/expression->list 'conditional
  (lambda (expression)
    `(if ,(expression->list (conditional/predicate expression))
	 ,(expression->list (conditional/consequent expression))
	 ,(expression->list (conditional/alternative expression)))))

(define-method/expression->list 'constant
  (lambda (expression) (constant/value expression)))

(define-method/expression->list 'declaration
  (lambda (expression)
    `(declare ,(declaration/declarations expression)
	      ,(expression->list (declaration/expression expression)))))

(define-method/expression->list 'delay
  (lambda (expression)
    `(delay ,(expression->list (delay/expression expression)))))

(define-method/expression->list 'disjunction
  (lambda (expression)
    `(or ,(expression->list (disjunction/predicate expression))
	 ,(expression->list (disjunction/alternative expression)))))

(define-method/expression->list 'open-block
  (lambda (expression)
    `(open-block
      ',(map variable/name (open-block/variables expression))
      ,@(map (lambda (action)
	       (if (eq? action open-block/value-marker)
		   `(quote ,action)
		   (expression->list action)))
	     (open-block/actions expression)))))

(define-method/expression->list 'procedure
  (lambda (expression)
    (let ((name (procedure/name expression))
	  (required (map variable/name (procedure/required expression)))
	  (optional (map variable/name (procedure/optional expression)))
	  (rest     (let ((rest-arg (procedure/rest expression)))
		      (and rest-arg
			   (variable/name rest-arg)))))
      `(procedure ,name
		  ,(make-lambda-list required optional rest '())
		  ,(expression->list (procedure/body expression))))))

(define-method/expression->list 'quotation
  (lambda (expression)
    `(quote ,(quotation/expression expression))))

(define-method/expression->list 'reference
  (lambda (expression)
    (variable/name (reference/variable expression))))

(define-method/expression->list 'sequence
  (lambda (expression)
    `(begin ,@(map expression->list (sequence/actions expression)))))
