#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/unsyn.scm,v 14.2 1988/06/14 14:45:31 cph Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

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

;;;; UNSYNTAX: SCode -> S-Expression
;;; package: (runtime unsyntaxer)

(declare (usual-integrations))

(define (initialize-package!)
  (set! unsyntaxer/scode-walker
	(make-scode-walker unsyntax-constant
			   `((ACCESS ,unsyntax-ACCESS-object)
			     (ASSIGNMENT ,unsyntax-ASSIGNMENT-object)
			     (COMBINATION ,unsyntax-COMBINATION-object)
			     (COMMENT ,unsyntax-COMMENT-object)
			     (CONDITIONAL ,unsyntax-CONDITIONAL-object)
			     (DECLARATION ,unsyntax-DECLARATION-object)
			     (DEFINITION ,unsyntax-DEFINITION-object)
			     (DELAY ,unsyntax-DELAY-object)
			     (DISJUNCTION ,unsyntax-DISJUNCTION-object)
			     (ERROR-COMBINATION
			      ,unsyntax-ERROR-COMBINATION-object)
			     (IN-PACKAGE ,unsyntax-IN-PACKAGE-object)
			     (LAMBDA ,unsyntax-LAMBDA-object)
			     (OPEN-BLOCK ,unsyntax-OPEN-BLOCK-object)
			     (QUOTATION ,unsyntax-QUOTATION)
			     (SEQUENCE ,unsyntax-SEQUENCE-object)
			     (THE-ENVIRONMENT ,unsyntax-THE-ENVIRONMENT-object)
			     (UNASSIGNED? ,unsyntax-UNASSIGNED?-object)
			     (VARIABLE ,unsyntax-VARIABLE-object)))))

(define (unsyntax scode)
  (unsyntax-object
   (if (compound-procedure? scode) (procedure-lambda scode) scode)))

(define (unsyntax-object object)
  ((scode-walk unsyntaxer/scode-walker object) object))

(define unsyntaxer/scode-walker)

(define (unsyntax-objects objects)
  (if (null? objects)
      '()
      (cons (unsyntax-object (car objects))
	    (unsyntax-objects (cdr objects)))))

(define (unsyntax-error keyword message . irritants)
  (error (string-append "UNSYNTAX: "
			(symbol->string keyword)
			": "
			message)
	 (cond ((null? irritants) *the-non-printing-object*)
	       ((null? (cdr irritants)) (car irritants))
	       (else irritants))))

;;;; Unsyntax Quanta

(define (unsyntax-constant object)
  (if (or (pair? object) (symbol? object))
      `(QUOTE ,object)
      object))

(define (unsyntax-QUOTATION quotation)
  `(SCODE-QUOTE ,(unsyntax-object (quotation-expression quotation))))

(define (unsyntax-VARIABLE-object object)
  (variable-name object))

(define (unsyntax-ACCESS-object object)
  `(ACCESS ,@(unexpand-access object)))

(define (unexpand-access object)
  (if (access? object)
      (access-components object
	(lambda (environment name)
	  `(,name ,@(unexpand-access environment))))
      `(,(unsyntax-object object))))

(define (unsyntax-DEFINITION-object definition)
  (definition-components definition unexpand-definition))

(define (unsyntax-ASSIGNMENT-object assignment)
  (assignment-components assignment
    (lambda (name value)
      `(SET! ,name
	     ,@(if (unassigned-reference-trap? value)
		   '()
		   `(,(unsyntax-object value)))))))

(define (unexpand-definition name value)
  (if (lambda? value)
      (lambda-components** value
	(lambda (lambda-name required optional rest body)
	  (if (eq? lambda-name name)
	      `(DEFINE (,name . ,(lambda-list required optional rest))
		 ,@(unsyntax-sequence body))
	      `(DEFINE ,name ,@(unexpand-binding-value value)))))
      `(DEFINE ,name ,@(unexpand-binding-value value))))

(define (unexpand-binding-value value)
  (if (unassigned-reference-trap? value)
      '()
      `(,(unsyntax-object value))))

(define (unsyntax-UNASSIGNED?-object unassigned?)
  `(UNASSIGNED? ,(unassigned?-name unassigned?)))

(define (unsyntax-COMMENT-object comment)
  (comment-components comment
    (lambda (text expression)
      `(COMMENT ,text ,(unsyntax-object expression)))))
(define (unsyntax-DECLARATION-object declaration)
  (declaration-components declaration
    (lambda (text expression)
      `(LOCAL-DECLARE ,text ,(unsyntax-object expression)))))

(define (unsyntax-SEQUENCE-object sequence)
  `(BEGIN ,@(unsyntax-sequence sequence)))

(define (unsyntax-sequence sequence)
  (unsyntax-objects (sequence-actions sequence)))

(define (unsyntax-OPEN-BLOCK-object open-block)
  (open-block-components open-block
    (lambda (auxiliary declarations expression)
      `(OPEN-BLOCK ,auxiliary
		   ,declarations
		   ,@(unsyntax-sequence expression)))))

(define (unsyntax-DELAY-object object)
  `(DELAY ,(unsyntax-object (delay-expression object))))

(define (unsyntax-IN-PACKAGE-object in-package)
  (in-package-components in-package
    (lambda (environment expression)
      `(IN-PACKAGE ,(unsyntax-object environment)
	 ,@(unsyntax-sequence expression)))))

(define (unsyntax-THE-ENVIRONMENT-object object)
  object
  `(THE-ENVIRONMENT))

(define (unsyntax-DISJUNCTION-object object)
  `(OR ,@(disjunction-components object unexpand-disjunction)))

(define (unexpand-disjunction predicate alternative)
  `(,(unsyntax-object predicate)
    ,@(if (disjunction? alternative)
	  (disjunction-components alternative unexpand-disjunction)
	  `(,(unsyntax-object alternative)))))

(define (unsyntax-CONDITIONAL-object conditional)
  (conditional-components conditional unsyntax-conditional))

(define (unsyntax-conditional predicate consequent alternative)
  (cond ((false? alternative)
	 `(AND ,@(unexpand-conjunction predicate consequent)))
	((eq? alternative undefined-conditional-branch)
	 `(IF ,(unsyntax-object predicate)
	      ,(unsyntax-object consequent)))
	((eq? consequent undefined-conditional-branch)
	 `(IF (,not ,(unsyntax-object predicate))
	      ,(unsyntax-object alternative)))
	((conditional? alternative)
	 `(COND ,@(unsyntax-cond-conditional predicate
					     consequent
					     alternative)))
	(else
	 `(IF ,(unsyntax-object predicate)
	      ,(unsyntax-object consequent)
	      ,(unsyntax-object alternative)))))

(define (unsyntax-cond-conditional predicate consequent alternative)
  `((,(unsyntax-object predicate) ,@(unsyntax-sequence consequent))
    ,@(unsyntax-cond-alternative alternative)))

(define (unsyntax-cond-disjunction predicate alternative)
  `((,(unsyntax-object predicate))
    ,@(unsyntax-cond-alternative alternative)))

(define (unsyntax-cond-alternative alternative)
  (cond ((eq? alternative undefined-conditional-branch) '())
	((disjunction? alternative)
	 (disjunction-components alternative unsyntax-cond-disjunction))
	((conditional? alternative)
	 (conditional-components alternative unsyntax-cond-conditional))
	(else `((ELSE ,@(unsyntax-sequence alternative))))))

(define (unexpand-conjunction predicate consequent)
  (if (conditional? consequent)
      `(,(unsyntax-object predicate)
	,@(conditional-components consequent
	    (lambda (predicate consequent alternative)
	      (if (false? alternative)
		  (unexpand-conjunction predicate consequent)
		  `(,(unsyntax-conditional predicate
					   consequent
					   alternative))))))
      `(,(unsyntax-object predicate) ,(unsyntax-object consequent))))

;;;; Lambdas

(define (unsyntax-LAMBDA-object expression)
  (lambda-components** expression
    (lambda (name required optional rest body)
      (let ((bvl (lambda-list required optional rest))
	    (body (unsyntax-sequence body)))
	(if (eq? name lambda-tag:unnamed)
	    `(LAMBDA ,bvl ,@body)
	    `(NAMED-LAMBDA (,name . ,bvl) ,@body))))))

(define (unsyntax-lambda-list expression)
  (if (not (lambda? expression))
      (error "Must be a lambda expression" expression))
  (lambda-components** expression
    (lambda (name required optional rest body)
      name body
      (lambda-list required optional rest))))

(define (lambda-list required optional rest)
  (cond ((null? rest)
	 (if (null? optional)
	     required
	     `(,@required ,lambda-optional-tag ,@optional)))
	((null? optional)
	 `(,@required . ,rest))
	(else
	 `(,@required ,lambda-optional-tag ,@optional . ,rest))))

(define (lambda-components** expression receiver)
  (lambda-components expression
    (lambda (name required optional rest auxiliary declarations body)
      (receiver name required optional rest
		(unscan-defines auxiliary declarations body)))))

;;;; Combinations

(define (unsyntax-COMBINATION-object combination)
  (combination-components combination
    (lambda (operator operands)
      (let ((ordinary-combination
	     (lambda ()
	       (cons (unsyntax-object operator)
		     (unsyntax-objects operands)))))
	(cond ((and (or (eq? operator cons)
			(absolute-reference-to? operator 'CONS))
		    (= (length operands) 2)
		    (delay? (cadr operands)))
	       `(CONS-STREAM ,(unsyntax-object (car operands))
			     ,(unsyntax-object
			       (delay-expression (cadr operands)))))
	      ((absolute-reference-to? operator 'BREAKPOINT-PROCEDURE)
	       (unsyntax-error-like-form operands 'BKPT))
	      ((lambda? operator)
	       (lambda-components** operator
		 (lambda (name required optional rest body)
		   (if (and (null? optional)
			    (null? rest))
		       (cond ((or (eq? name lambda-tag:unnamed)
				  (eq? name lambda-tag:let))
			      `(LET ,(unsyntax-let-bindings required operands)
				 ,@(unsyntax-sequence body)))
			     ((eq? name lambda-tag:fluid-let)
			      (unsyntax/fluid-let required
						  operands
						  body
						  ordinary-combination))
			     ((and (eq? name lambda-tag:make-environment)
				   (the-environment?
				    (car (last-pair (sequence-actions body)))))
			      `(MAKE-ENVIRONMENT
				 ,@(unsyntax-objects
				    (except-last-pair
				     (sequence-actions body)))))
			     (else (ordinary-combination)))
		       (ordinary-combination)))))
	      (else
	       (ordinary-combination)))))))

(define (unsyntax-let-bindings names values)
  (map unsyntax-let-binding names values))

(define (unsyntax-let-binding name value)
  `(,name ,@(unexpand-binding-value value)))
(define (unsyntax-ERROR-COMBINATION-object combination)
  (unsyntax-error-like-form (combination-operands combination) 'ERROR))

(define (unsyntax-error-like-form operands name)
  (cons* name
	 (unsyntax-object (first operands))
	 (let ((operand (second operands)))
	   (cond ((absolute-reference-to? operand '*THE-NON-PRINTING-OBJECT*)
		  '())
		 ((combination? operand)
		  (combination-components operand
		    (lambda (operator operands)
		      (if (absolute-reference-to? operator 'LIST)
			  (unsyntax-objects operands)
			  `(,(unsyntax-object operand))))))
		 (else
		  `(,(unsyntax-object operand)))))))

(define (unsyntax/fluid-let names values body if-malformed)
  (combination-components body
    (lambda (operator operands)
      (cond ((or (absolute-reference-to? operator 'DYNAMIC-WIND)
		 (and (variable? operator)
		      (eq? (variable-name operator) 'DYNAMIC-WIND)))
	     (unsyntax/fluid-let/shallow names values operands))
	    ((and (eq? operator (ucode-primitive with-saved-fluid-bindings 1))
		  (null? names)
		  (null? values)
		  (not (null? operands))
		  (null? (cdr operands)))
	     (unsyntax/fluid-let/deep (car operands)))
	    (else
	     (if-malformed))))))

(define (unsyntax/fluid-let/shallow names values operands)
  names
  `(FLUID-LET ,(unsyntax-let-bindings
		(map extract-transfer-var
		     (sequence-actions (lambda-body (car operands))))
		(let every-other ((values values))
		  (if (null? values)
		      '()
		      (cons (car values) (every-other (cddr values))))))
     ,@(lambda-components** (cadr operands)
	 (lambda (name required optional rest body)
	   name required optional rest
	   (unsyntax-sequence body)))))

(define (extract-transfer-var assignment)
  (assignment-components assignment
    (lambda (name value)
      name
      (cond ((assignment? value)
	     (assignment-components value (lambda (name value) value name)))
	    ((combination? value)
	     (combination-components value
	       (lambda (operator operands)
		 (cond ((eq? operator lexical-assignment)
			`(ACCESS ,(cadr operands)
				 ,@(unexpand-access (car operands))))
		       (else
			(unsyntax-error 'FLUID-LET
					"Unknown SCODE form"
					assignment))))))
	    (else
	     (unsyntax-error 'FLUID-LET "Unknown SCODE form" assignment))))))

(define (unsyntax/fluid-let/deep expression)
  (let ((body (lambda-body expression)))
    (let loop
	((actions (sequence-actions body))
	 (receiver
	  (lambda (bindings body)
	    `(FLUID-LET ,bindings ,@body))))
      (let ((action (car actions)))
	(if (and (combination? action)
		 (or (eq? (combination-operator action)
			  (ucode-primitive add-fluid-binding! 3))
		     (eq? (combination-operator action)
			  (ucode-primitive make-fluid-binding! 3))))
	    (loop (cdr actions)
	      (lambda (bindings body)
		(receiver (cons (unsyntax-fluid-assignment action) bindings)
			  body)))
	    (receiver '() (unsyntax-objects actions)))))))

(define (unsyntax-fluid-assignment combination)
  (let ((operands (combination-operands combination)))
    (let ((environment (car operands))
	  (name (cadr operands))
	  (value (caddr operands)))
      (cond ((symbol? name)
	     `((ACCESS ,name ,(unsyntax-object environment))
	       ,(unsyntax-object value)))
	    ((quotation? name)
	     (let ((variable (quotation-expression name)))
	       (if (variable? variable)
		   `(,(variable-name variable) ,(unsyntax-object value))
		   (unsyntax-error 'FLUID-LET "unexpected name" name))))
	    (else
	     (unsyntax-error 'FLUID-LET "unexpected name" name))))))