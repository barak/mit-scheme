#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/unsyn.scm,v 14.11 1991/02/15 18:07:27 cph Exp $

Copyright (c) 1988-91 Massachusetts Institute of Technology

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
  (set! lambda-auxiliary-tag (intern "#!aux"))
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
			     (IN-PACKAGE ,unsyntax-IN-PACKAGE-object)
			     (LAMBDA ,unsyntax-LAMBDA-object)
			     (OPEN-BLOCK ,unsyntax-OPEN-BLOCK-object)
			     (QUOTATION ,unsyntax-QUOTATION)
			     (SEQUENCE ,unsyntax-SEQUENCE-object)
			     (THE-ENVIRONMENT ,unsyntax-THE-ENVIRONMENT-object)
			     (UNASSIGNED? ,unsyntax-UNASSIGNED?-object)
			     (VARIABLE ,unsyntax-VARIABLE-object))))
  unspecific)

(define unsyntaxer:macroize?
  true)

(define unsyntaxer:show-comments?
  false)

(define substitutions '())

(define (unsyntax-with-substitutions scode alist)
  (if (not (alist? alist))
      (error:wrong-type-argument alist "alist" 'UNSYNTAX-WITH-SUBSTITUTIONS))
  (fluid-let ((substitutions alist))
    (unsyntax scode)))

(define (maybe-substitute object action)
  (let ((association (has-substitution? object)))
    (if association
	(cdr association)
	(action object))))

(define-integrable (has-substitution? object)
  (and (not (null? substitutions))
       (assq object substitutions)))

(define (unsyntax scode)
  (unsyntax-object (if (procedure? scode) (procedure-lambda scode) scode)))

(define (unsyntax-object object)
  (maybe-substitute
   object
   (lambda (object) ((scode-walk unsyntaxer/scode-walker object) object))))

(define unsyntaxer/scode-walker)

(define (unsyntax-objects objects)
  (if (null? objects)
      '()
      (cons (unsyntax-object (car objects))
	    (unsyntax-objects (cdr objects)))))

(define (unsyntax-error keyword message . irritants)
  (apply error
	 (string-append "UNSYNTAX: " (symbol->string keyword) ": " message)
	 irritants))

;;;; Unsyntax Quanta

(define (unsyntax-constant object)
  (cond ((or (pair? object) (symbol? object))
	 `(QUOTE ,object))
	((compiled-expression? object)
	 (let ((scode (compiled-expression/scode object)))
	   (if (eq? scode object)
	       `(SCODE-QUOTE ,object)
	       (unsyntax-object scode))))
	(else
	 object)))

(define (unsyntax-QUOTATION quotation)
  `(SCODE-QUOTE ,(unsyntax-object (quotation-expression quotation))))

(define (unsyntax-VARIABLE-object object)
  (variable-name object))

(define (unsyntax-ACCESS-object object)
  `(ACCESS ,@(unexpand-access object)))

(define (unexpand-access object)
  (let loop ((object object) (separate? true))
    (if (and separate?
	     (access? object)
	     (not (has-substitution? object)))
	(access-components object
	  (lambda (environment name)
	    `(,name ,@(loop environment unsyntaxer:macroize?))))
	`(,(unsyntax-object object)))))

(define (unsyntax-DEFINITION-object definition)
  (definition-components definition unexpand-definition))

(define (unsyntax-ASSIGNMENT-object assignment)
  (assignment-components assignment
    (lambda (name value)
      `(SET! ,name ,@(unexpand-binding-value value)))))

(define (unexpand-definition name value)
  (if (and unsyntaxer:macroize?
	   (lambda? value)
	   (not (has-substitution? value)))
      (lambda-components** value
	(lambda (lambda-name required optional rest body)
	  (if (eq? lambda-name name)
	      `(DEFINE (,name . ,(lambda-list required optional rest '()))
		 ,@(unsyntax-sequence body))
	      `(DEFINE ,name ,@(unexpand-binding-value value)))))
      `(DEFINE ,name ,@(unexpand-binding-value value))))

(define (unexpand-binding-value value)
  (if (unassigned-reference-trap? value)
      '()
      `(,(unsyntax-object value))))

(define (unsyntax-UNASSIGNED?-object object)
  `(UNASSIGNED? ,(unassigned?-name object)))

(define (unsyntax-COMMENT-object comment)
  (let ((expression (unsyntax-object (comment-expression comment))))
    (if unsyntaxer:show-comments?
	`(COMMENT ,(comment-text comment) ,expression)
	expression)))

(define (unsyntax-DECLARATION-object declaration)
  (declaration-components declaration
    (lambda (text expression)
      `(LOCAL-DECLARE ,text ,(unsyntax-object expression)))))

(define (unsyntax-SEQUENCE-object seq)
  `(BEGIN ,@(unsyntax-sequence-actions seq)))

(define (unsyntax-sequence seq)
  (if (sequence? seq)
      (if unsyntaxer:macroize?
	  (unsyntax-sequence-actions seq)
	  `((BEGIN ,@(unsyntax-sequence-actions seq))))
      (list (unsyntax-object seq))))

(define (unsyntax-sequence-actions seq)
  (let ((actions (sequence-immediate-actions seq)))
    (let loop ((actions actions))
      (if (null? actions)
	  '()
	  (let ((substitution (has-substitution? (car actions))))
	    (cond (substitution
		   (cons (cdr substitution)
			 (loop (cdr actions))))
		  ((and unsyntaxer:macroize?
			(sequence? (car actions)))
		   (append (unsyntax-sequence-actions (car actions))
			   (loop (cdr actions))))
		  (else
		   (cons (unsyntax-object (car actions))
			 (loop (cdr actions))))))))))

(define (unsyntax-OPEN-BLOCK-object open-block)
  (if unsyntaxer:macroize?
      (open-block-components open-block
	(lambda (auxiliary declarations expression)
	  `(OPEN-BLOCK ,auxiliary
		       ,declarations
		       ,@(unsyntax-sequence expression))))
      (unsyntax-SEQUENCE-object open-block)))

(define (unsyntax-DELAY-object object)
  `(DELAY ,(unsyntax-object (delay-expression object))))

(define (unsyntax-IN-PACKAGE-object object)
  (in-package-components object
    (lambda (environment expression)
      `(IN-PACKAGE ,(unsyntax-object environment)
	 ,@(unsyntax-sequence expression)))))

(define (unsyntax-THE-ENVIRONMENT-object object)
  object
  `(THE-ENVIRONMENT))

(define (unsyntax-DISJUNCTION-object object)
  `(OR ,@(disjunction-components object
	   (if unsyntaxer:macroize?
	       unexpand-disjunction
	       (lambda (predicate alternative)
		 (list (unsyntax-object predicate)
		       (unsyntax-object alternative)))))))	   

(define (unexpand-disjunction predicate alternative)
  `(,(unsyntax-object predicate)
    ,@(if (disjunction? alternative)
	  (disjunction-components alternative unexpand-disjunction)
	  `(,(unsyntax-object alternative)))))

(define (unsyntax-CONDITIONAL-object conditional)
  (conditional-components conditional
    (if unsyntaxer:macroize?
	unsyntax-conditional
	unsyntax-conditional/default)))

(define (unsyntax-conditional/default predicate consequent alternative)
  `(IF ,(unsyntax-object predicate)
       ,(unsyntax-object consequent)
       ,(unsyntax-object alternative)))

(define (unsyntax-conditional predicate consequent alternative)
  (cond ((false? alternative)
	 `(AND ,@(unexpand-conjunction predicate consequent)))
	((eq? alternative undefined-conditional-branch)
	 `(IF ,(unsyntax-object predicate)
	      ,(unsyntax-object consequent)))
	((eq? consequent undefined-conditional-branch)
	 `(IF (,not ,(unsyntax-object predicate))
	      ,(unsyntax-object alternative)))
	((and (conditional? alternative)
	      (not (has-substitution? alternative)))
	 `(COND ,@(unsyntax-cond-conditional predicate
					     consequent
					     alternative)))
	(else
	 (unsyntax-conditional/default predicate consequent alternative))))

(define (unsyntax-cond-conditional predicate consequent alternative)
  `((,(unsyntax-object predicate) ,@(unsyntax-sequence consequent))
    ,@(unsyntax-cond-alternative alternative)))

(define (unsyntax-cond-disjunction predicate alternative)
  `((,(unsyntax-object predicate))
    ,@(unsyntax-cond-alternative alternative)))

(define (unsyntax-cond-alternative alternative)
  (cond ((eq? alternative undefined-conditional-branch)
	 '())
	((has-substitution? alternative)
	 =>
	 (lambda (substitution)
	   `((ELSE ,substitution))))
	((disjunction? alternative)
	 (disjunction-components alternative unsyntax-cond-disjunction))
	((conditional? alternative)
	 (conditional-components alternative unsyntax-cond-conditional))
	(else
	 `((ELSE ,@(unsyntax-sequence alternative))))))

(define (unexpand-conjunction predicate consequent)
  (if (and (conditional? consequent)
	   (not (has-substitution? consequent)))
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
  (if unsyntaxer:macroize?
      (lambda-components** expression
	(lambda (name required optional rest body)
	  (collect-lambda name
			  (lambda-list required optional rest '())
			  (unsyntax-sequence body))))
      (lambda-components expression
	(lambda (name required optional rest auxiliary declarations body)
	  (collect-lambda name
			  (lambda-list required optional rest auxiliary)
			  (let ((body (unsyntax-sequence body)))
			    (if (null? declarations)
				body
				`((DECLARE ,@declarations)
				  ,@body))))))))

(define (collect-lambda name bvl body)
  (if (eq? name lambda-tag:unnamed)
      `(LAMBDA ,bvl ,@body)
      `(NAMED-LAMBDA (,name . ,bvl) ,@body)))

(define (unsyntax-lambda-list expression)
  (if (not (lambda? expression))
      (error:wrong-type-argument expression "SCode lambda"
				 'UNSYNTAX-LAMBDA-LIST))
  (lambda-components** expression
    (lambda (name required optional rest body)
      name body
      (lambda-list required optional rest '()))))

(define lambda-auxiliary-tag)

(define (lambda-list required optional rest auxiliary)
  (let ((optional (if (null? optional)
		      '()
		      (cons lambda-optional-tag optional)))
	(rest (cond ((not rest) '())
		    ((null? auxiliary) rest)
		    (else (list lambda-rest-tag rest)))))
    (if (null? auxiliary)
	`(,@required ,@optional . ,rest)
	`(,@required ,@optional ,@rest ,lambda-auxiliary-tag ,@auxiliary))))

(define (lambda-components** expression receiver)
  (lambda-components expression
    (lambda (name required optional rest auxiliary declarations body)
      (receiver name required optional rest
		(unscan-defines auxiliary declarations body)))))

;;;; Combinations

(define (unsyntax-COMBINATION-object combination)
  (rewrite-named-let
   (combination-components combination
     (lambda (operator operands)
       (let ((ordinary-combination
	      (lambda ()
		`(,(unsyntax-object operator) ,@(unsyntax-objects operands)))))
	 (cond ((or (not unsyntaxer:macroize?)
		    (has-substitution? operator))
		(ordinary-combination))
	       ((and (or (eq? operator cons)
			 (absolute-reference-to? operator 'CONS))
		     (= (length operands) 2)
		     (delay? (cadr operands))
		     (not (has-substitution? (cadr operands))))
		`(CONS-STREAM ,(unsyntax-object (car operands))
			      ,(unsyntax-object
				(delay-expression (cadr operands)))))
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
				     (car
				      (last-pair (sequence-actions body)))))
			       `(MAKE-ENVIRONMENT
				  ,@(unsyntax-objects
				     (except-last-pair
				      (sequence-actions body)))))
			      (else (ordinary-combination)))
			(ordinary-combination)))))
	       (else
		(ordinary-combination))))))))

(define (unsyntax-let-bindings names values)
  (map unsyntax-let-binding names values))

(define (unsyntax-let-binding name value)
  `(,name ,@(unexpand-binding-value value)))

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

(define (unsyntax/fluid-let names values body if-malformed)
  (combination-components body
    (lambda (operator operands)
      ;; `fluid-let' expressions are complicated.  Rather than scan
      ;; the entire expresion to find out if it has any substitutable
      ;; subparts, we just treat it as malformed if there are active
      ;; substitutions.
      (cond ((not (null? substitutions))
	     (if-malformed))
	    ((and (or (absolute-reference-to? operator 'DYNAMIC-WIND)
		      (and (variable? operator)
			   (eq? (variable-name operator) 'DYNAMIC-WIND)))
		  (pair? operands)
		  (lambda? (car operands))
		  (pair? (cdr operands))
		  (lambda? (cadr operands))
		  (pair? (cddr operands))
		  (lambda? (caddr operands))
		  (null? (cdddr operands)))
	     (unsyntax/fluid-let/shallow names values operands))
	    ((and (eq? operator (ucode-primitive with-saved-fluid-bindings 1))
		  (null? names)
		  (null? values)
		  (not (null? operands))
		  (lambda? (car operands))
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