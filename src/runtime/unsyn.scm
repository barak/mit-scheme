#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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
			     (EXTENDED-LAMBDA ,unsyntax-EXTENDED-LAMBDA-object)
			     (LAMBDA ,unsyntax-LAMBDA-object)
			     (OPEN-BLOCK ,unsyntax-OPEN-BLOCK-object)
			     (QUOTATION ,unsyntax-QUOTATION)
			     (SEQUENCE ,unsyntax-SEQUENCE-object)
			     (THE-ENVIRONMENT ,unsyntax-THE-ENVIRONMENT-object)
			     (VARIABLE ,unsyntax-VARIABLE-object))))
  unspecific)

;;; If UNSYNTAXER:MACROIZE? is #f, then the unsyntaxed output will
;;; closely match the concrete structure that is given to SCODE-EVAL.
;;; If it is #t, then the unsyntaxed output will more closely match
;;; the abstract structure of the SCODE (as output by syntax and sf).

(define unsyntaxer:macroize? #t)

(define unsyntaxer:elide-global-accesses? #t)
(define unsyntaxer:fold-sequence-tail? #t)
(define unsyntaxer:show-comments? #f)

;;; The substitutions mechanism is for putting the '### marker in
;;; debugger output.
(define substitutions '())

(define (unsyntax-with-substitutions scode alist)
  (if (not (alist? alist))
      (error:wrong-type-argument alist "alist" 'UNSYNTAX-WITH-SUBSTITUTIONS))
  (fluid-let ((substitutions alist))
    (unsyntax scode)))

(define-integrable (maybe-substitute object thunk)
  (let ((association (has-substitution? object)))
    (if association
	(cdr association)
	(thunk))))

(define-integrable (has-substitution? object)
  (and (pair? substitutions)
       (assq object substitutions)))

(define (with-bindings environment required optional rest receiver)
  (if (and unsyntaxer:elide-global-accesses?
	   unsyntaxer:macroize?)
      (receiver (append (if rest (list rest) '()) required optional environment))
      (receiver environment)))

(define (unsyntax scode)
  (unsyntax-object '()
		   (if (procedure? scode) (procedure-lambda scode) scode)))

(define (unsyntax-object environment object)
  (maybe-substitute
   object
   (lambda ()
     ((scode-walk unsyntaxer/scode-walker object) environment object))))

(define unsyntaxer/scode-walker)


;;;; Unsyntax Quanta

(define (unsyntax-constant environment object)
  (cond ((or (boolean? object)
	     (number? object)
	     (char? object)
	     (string? object))
	 ;; R4RS self-evaluating objects:
	 object)
	((or (pair? object)
	     (null? object)
	     (symbol? object)
	     (vector? object))
	 ;; R4RS quoted data (in addition to above)
	 `(QUOTE ,object))
	((compiled-expression? object)
	 (let ((scode (compiled-expression/scode object)))
	   (if (eq? scode object)
	       `(SCODE-QUOTE ,object)
	       (unsyntax-object environment scode))))
	(else
	 object)))

(define (unsyntax-QUOTATION environment quotation)
  `(SCODE-QUOTE
    ,(unsyntax-object environment (quotation-expression quotation))))

(define (unsyntax-VARIABLE-object environment object)
  (declare (ignore environment))
  (variable-name object))

(define (unsyntax-ACCESS-object environment object)
  (or (and unsyntaxer:elide-global-accesses?
	   unsyntaxer:macroize?
	   (access-components object
	     (lambda (access-environment name)
	       (and (or (eq? access-environment system-global-environment)
			(and (variable? access-environment)
			     (eq? (variable-name access-environment)
				  'system-global-environment)))
		    (not (memq name environment))
		    name))))
      `(ACCESS ,@(unexpand-access environment object))))

(define (unexpand-access environment object)
  (let loop ((object object) (separate? #t))
    (if (and separate?
	     (access? object)
	     (not (has-substitution? object)))
	(access-components object
	  (lambda (environment name)
	    `(,name ,@(loop environment (eq? #t unsyntaxer:macroize?)))))
	`(,(unsyntax-object environment object)))))

(define (unsyntax-DEFINITION-object environment definition)
  (definition-components definition
    (lambda (name value) (unexpand-definition environment name value))))

(define (unsyntax-ASSIGNMENT-object environment assignment)
  (assignment-components assignment
    (lambda (name value)
      `(SET! ,name ,@(unexpand-binding-value environment value)))))

(define (unexpand-definition environment name value)
  (cond ((macro-reference-trap-expression? value)
	 `(DEFINE-SYNTAX ,name
	    ,(unsyntax-object
	      environment
	      (macro-reference-trap-expression-transformer value))))
	((and (eq? #t unsyntaxer:macroize?)
	      (lambda? value)
	      (not (has-substitution? value)))
	 (lambda-components* value
	   (lambda (lambda-name required optional rest body)
	     (if (eq? lambda-name name)
		 `(DEFINE (,name . ,(make-lambda-list required optional rest '()))
		    ,@(with-bindings environment required optional rest
				     (lambda (environment*)
				       (unsyntax-lambda-body environment* body))))
		 `(DEFINE ,name ,@(unexpand-binding-value environment value))))))
	(else
	 `(DEFINE ,name ,@(unexpand-binding-value environment value)))))

(define (unexpand-binding-value environment value)
  (if (unassigned-reference-trap? value)
      '()
      `(,(unsyntax-object environment value))))

(define (unsyntax-COMMENT-object environment comment)
  (let ((expression
	 (unsyntax-object environment (comment-expression comment))))
    (if unsyntaxer:show-comments?
	`(COMMENT ,(comment-text comment) ,expression)
	expression)))

(define (unsyntax-DECLARATION-object environment declaration)
  (declaration-components declaration
    (lambda (text expression)
      `(LOCAL-DECLARE ,text ,(unsyntax-object environment expression)))))

(define (unsyntax-SEQUENCE-object environment seq)
  (let ((first-action (sequence-immediate-first seq)))
    (if (block-declaration? first-action)
	`(BEGIN
	  (DECLARE ,@(block-declaration-text first-action))
	  ,@(unsyntax-sequence environment (sequence-immediate-second seq)))
	`(BEGIN
	  ,@(unsyntax-sequence-actions environment seq)))))

(define (unsyntax-sequence environment seq)
  (if (sequence? seq)
      (if (eq? #t unsyntaxer:macroize?)
	  (unsyntax-sequence-actions environment seq)
	  `((BEGIN ,@(unsyntax-sequence-actions environment seq))))
      (list (unsyntax-object environment seq))))

(define (unsyntax-sequence-actions environment seq)
  (let ((tail (if (and unsyntaxer:fold-sequence-tail?
		       (sequence? (sequence-immediate-second seq)))
		  (unsyntax-sequence-actions environment (sequence-immediate-second seq))
		  (list (unsyntax-object environment (sequence-immediate-second seq))))))
   (let ((substitution (has-substitution? (sequence-immediate-first seq))))
     (cond (substitution
	    (cons (cdr substitution) tail))
	   ((and (eq? #t unsyntaxer:macroize?)
		 (sequence? (sequence-immediate-first seq)))
	    (append (unsyntax-sequence-actions environment
					       (sequence-immediate-first seq))
		    tail))
	   (else
	    (cons (unsyntax-object environment
				   (sequence-immediate-first seq)) tail))))))

(define (unsyntax-OPEN-BLOCK-object environment open-block)
  (if (eq? #t unsyntaxer:macroize?)
      (open-block-components open-block
	(lambda (auxiliary declarations expression)
	  (unsyntax-object environment
			   (unscan-defines auxiliary declarations expression))))
      (unsyntax-SEQUENCE-object environment open-block)))

(define (unsyntax-DELAY-object environment object)
  `(DELAY ,(unsyntax-object environment (delay-expression object))))

(define (unsyntax-THE-ENVIRONMENT-object environment object)
  (declare (ignore environment object))
  `(THE-ENVIRONMENT))

(define (unsyntax-DISJUNCTION-object environment object)
  `(OR ,@(disjunction-components object
	   (if (eq? #t unsyntaxer:macroize?)
	       (lambda (predicate alternative)
		 (unexpand-disjunction environment predicate alternative))
	       (lambda (predicate alternative)
		 (list (unsyntax-object environment predicate)
		       (unsyntax-object environment alternative)))))))

(define (unexpand-disjunction environment predicate alternative)
  `(,(unsyntax-object environment predicate)
    ,@(if (disjunction? alternative)
	  (disjunction-components alternative
	    (lambda (predicate alternative)
	      (unexpand-disjunction environment predicate alternative)))
	  `(,(unsyntax-object environment alternative)))))

(define (unsyntax-CONDITIONAL-object environment conditional)
  (conditional-components conditional
    (if (eq? #t unsyntaxer:macroize?)
	(lambda (predicate consequent alternative)
	  (unsyntax-conditional environment predicate consequent alternative))
	(lambda (predicate consequent alternative)
	  (unsyntax-conditional/default
	   environment predicate consequent alternative)))))

(define (unsyntax-conditional/default environment
				      predicate consequent alternative)
  `(IF ,(unsyntax-object environment predicate)
       ,(unsyntax-object environment consequent)
       ,(unsyntax-object environment alternative)))

(define (unsyntax-conditional environment predicate consequent alternative)
  (cond ((not alternative)
	 `(AND ,@(unexpand-conjunction environment predicate consequent)))
	((eq? alternative undefined-conditional-branch)
	 `(IF ,(unsyntax-object environment predicate)
	      ,(unsyntax-object environment consequent)))
	((eq? consequent undefined-conditional-branch)
	 `(IF (,(ucode-primitive not) ,(unsyntax-object environment predicate))
	      ,(unsyntax-object environment alternative)))
	((and (conditional? alternative)
	      (not (has-substitution? alternative)))
	 `(COND ,@(unsyntax-cond-conditional environment predicate
					     consequent
					     alternative)))
	(else
	 (unsyntax-conditional/default environment
				       predicate consequent alternative))))

(define (unsyntax-cond-conditional environment
				   predicate consequent alternative)
  `((,(unsyntax-object environment predicate)
     ,@(unsyntax-sequence environment consequent))
    ,@(unsyntax-cond-alternative environment alternative)))

(define (unsyntax-cond-disjunction environment predicate alternative)
  `((,(unsyntax-object environment predicate))
    ,@(unsyntax-cond-alternative environment alternative)))

(define (unsyntax-cond-alternative environment alternative)
  (cond ((eq? alternative undefined-conditional-branch)
	 '())
	((has-substitution? alternative)
	 =>
	 (lambda (substitution)
	   `((ELSE ,substitution))))
	((disjunction? alternative)
	 (disjunction-components alternative
	   (lambda (predicate alternative)
	     (unsyntax-cond-disjunction environment predicate alternative))))
	((conditional? alternative)
	 (conditional-components alternative
	   (lambda (predicate consequent alternative)
	     (unsyntax-cond-conditional environment
					predicate consequent alternative))))
	(else
	 `((ELSE ,@(unsyntax-sequence environment alternative))))))

(define (unexpand-conjunction environment predicate consequent)
  (if (and (conditional? consequent)
	   (not (has-substitution? consequent)))
      `(,(unsyntax-object environment predicate)
	,@(conditional-components consequent
	    (lambda (predicate consequent alternative)
	      (if (not alternative)
		  (unexpand-conjunction environment predicate consequent)
		  `(,(unsyntax-conditional environment predicate
					   consequent
					   alternative))))))
      `(,(unsyntax-object environment predicate)
	,(unsyntax-object environment consequent))))

;;;; Lambdas

(define (unsyntax-EXTENDED-LAMBDA-object environment expression)
  (if unsyntaxer:macroize?
      (unsyntax-lambda environment expression)
      `(&XLAMBDA (,(lambda-name expression) ,@(lambda-interface expression))
		 ,(unsyntax-object environment (lambda-immediate-body expression)))))

(define (unsyntax-LAMBDA-object environment expression)
  (if unsyntaxer:macroize?
      (unsyntax-lambda environment expression)
      (collect-lambda (lambda-name expression)
		      (lambda-interface expression)
		      (list (unsyntax-object environment
			     (lambda-immediate-body expression))))))

(define (unsyntax-lambda environment expression)
  (lambda-components* expression
    (lambda (name required optional rest body)
      (collect-lambda name
		      (make-lambda-list required optional rest '())
		      (with-bindings environment required optional rest
				     (lambda (environment*)
				       (unsyntax-lambda-body environment* body)))))))

(define (collect-lambda name bvl body)
  (if (eq? name lambda-tag:unnamed)
      `(LAMBDA ,bvl ,@body)
      `(NAMED-LAMBDA (,name . ,bvl) ,@body)))

(define (unsyntax-lambda-list expression)
  (if (not (lambda? expression))
      (error:wrong-type-argument expression "SCode lambda"
				 'UNSYNTAX-LAMBDA-LIST))
  (lambda-components* expression
    (lambda (name required optional rest body)
      name body
      (make-lambda-list required optional rest '()))))

(define (unsyntax-lambda-body environment body)
  (if (open-block? body)
      (open-block-components body
	(lambda (names declarations open-block-body)
	  (unsyntax-lambda-body-sequence environment
	   (unscan-defines names declarations open-block-body))))
      (unsyntax-lambda-body-sequence environment body)))

(define (unsyntax-lambda-body-sequence environment body)
  (if (sequence? body)
      (let ((first-action (sequence-immediate-first body)))
	(if (block-declaration? first-action)
	    `((DECLARE ,@(block-declaration-text first-action))
	      ,@(unsyntax-sequence environment (sequence-immediate-second body)))
	    (unsyntax-sequence environment body)))
      (list (unsyntax-object environment body))))

;;;; Combinations

(define (unsyntax-COMBINATION-object environment combination)
  (rewrite-named-let
   (combination-components combination
     (lambda (operator operands)
       (let ((ordinary-combination
	      (lambda ()
		`(,(unsyntax-object environment operator)
		  ,@(map (lambda (operand)
			   (unsyntax-object environment operand))
			 operands)))))
	 (cond ((or (not (eq? #t unsyntaxer:macroize?))
		    (has-substitution? operator))
		(ordinary-combination))
	       ((and (or (eq? operator (ucode-primitive cons))
			 (absolute-reference-to? operator 'CONS))
		     (= (length operands) 2)
		     (delay? (cadr operands))
		     (not (has-substitution? (cadr operands))))
		`(CONS-STREAM ,(unsyntax-object environment (car operands))
			      ,(unsyntax-object environment
				(delay-expression (cadr operands)))))
	       ((lambda? operator)
		(lambda-components* operator
		  (lambda (name required optional rest body)
		    (if (and (null? optional)
			     (not rest)
			     (= (length required) (length operands)))
			(if (or (eq? name lambda-tag:unnamed)
				(eq? name lambda-tag:let))
			    `(LET ,(unsyntax-let-bindings environment required operands)
			       ,@(with-bindings environment required '() #F
						(lambda (environment*)
						  (unsyntax-lambda-body environment* body))))
			    (ordinary-combination))
			(ordinary-combination)))))
	       (else
		(ordinary-combination))))))))

(define (unsyntax-let-bindings environment names values)
  (map (lambda (name value)
	 (unsyntax-let-binding environment name value))
       names values))

(define (unsyntax-let-binding environment name value)
  `(,name ,@(unexpand-binding-value environment value)))

(define (rewrite-named-let expression)
  (if (and (pair? expression)
	   (let ((expression (car expression)))
	     (and (list? expression)
		  (= 4 (length expression))
		  (eq? 'LET (car expression))
		  (eq? '() (cadr expression))
		  (symbol? (cadddr expression))
		  (let ((definition (caddr expression)))
		    (and (pair? definition)
			 (eq? 'DEFINE (car definition))
			 (pair? (cadr definition))
			 (eq? (caadr definition) (cadddr expression))
			 (list? (cdadr definition))
			 (for-all? (cdadr definition) symbol?))))))
      `(LET ,(cadddr (car expression))
	 ,(map (lambda (name value)
		 `(,name
		   ,@(if (unassigned-reference-trap? value)
			 '()
			 `(,value))))
	       (cdadr (caddr (car expression)))
	       (cdr expression))
	 ,@(cddr (caddr (car expression))))
      expression))