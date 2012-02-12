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

(define unsyntaxer:elide-global-accesses? #f)
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

(define (maybe-substitute object action)
  (let ((association (has-substitution? object)))
    (if association
	(cdr association)
	(action object))))

(define-integrable (has-substitution? object)
  (and (pair? substitutions)
       (assq object substitutions)))

(define bound (list #F '()))

(define (with-bindings required optional rest action argument)
  (if (and unsyntaxer:elide-global-accesses?
	   unsyntaxer:macroize?)
      (let* ((bound bound)
	     (old   (cdr bound)))
	(set-cdr! bound
		  (append (if rest (list rest) '()) required optional old))
	(let ((value (action argument)))
	  (set-cdr! bound old)
	  value))
      (action argument)))

(define (unsyntax scode)
  (fluid-let ((bound (list #F '())))
    (unsyntax-object (if (procedure? scode) (procedure-lambda scode) scode))))

(define (unsyntax-object object)
  (maybe-substitute
   object
   (lambda (object) ((scode-walk unsyntaxer/scode-walker object) object))))

(define unsyntaxer/scode-walker)


;;;; Unsyntax Quanta

(define (unsyntax-constant object)
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
	       (unsyntax-object scode))))
	(else
	 object)))

(define (unsyntax-QUOTATION quotation)
  `(SCODE-QUOTE ,(unsyntax-object (quotation-expression quotation))))

(define (unsyntax-VARIABLE-object object)
  (variable-name object))

(define (unsyntax-ACCESS-object object)
  (or (and unsyntaxer:elide-global-accesses?
	   unsyntaxer:macroize?
	   (access-components object
	     (lambda (environment name)
	       (and (eq? environment system-global-environment)
		    (not (memq name (cdr bound)))
		    name))))
      `(ACCESS ,@(unexpand-access object))))

(define (unexpand-access object)
  (let loop ((object object) (separate? #t))
    (if (and separate?
	     (access? object)
	     (not (has-substitution? object)))
	(access-components object
	  (lambda (environment name)
	    `(,name ,@(loop environment (eq? #t unsyntaxer:macroize?)))))
	`(,(unsyntax-object object)))))

(define (unsyntax-DEFINITION-object definition)
  (definition-components definition unexpand-definition))

(define (unsyntax-ASSIGNMENT-object assignment)
  (assignment-components assignment
    (lambda (name value)
      `(SET! ,name ,@(unexpand-binding-value value)))))

(define (unexpand-definition name value)
  (cond ((macro-reference-trap-expression? value)
	 `(DEFINE-SYNTAX ,name
	    ,(unsyntax-object
	      (macro-reference-trap-expression-transformer value))))
	((and (eq? #t unsyntaxer:macroize?)
	      (lambda? value)
	      (not (has-substitution? value)))
	 (lambda-components** value
	   (lambda (lambda-name required optional rest body)
	     (if (eq? lambda-name name)
		 `(DEFINE (,name . ,(make-lambda-list required optional rest '()))
		    ,@(with-bindings required optional rest
				     unsyntax-lambda-body body))
		 `(DEFINE ,name ,@(unexpand-binding-value value))))))
	(else
	 `(DEFINE ,name ,@(unexpand-binding-value value)))))

(define (unexpand-binding-value value)
  (if (unassigned-reference-trap? value)
      '()
      `(,(unsyntax-object value))))

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
      (if (eq? #t unsyntaxer:macroize?)
	  (unsyntax-sequence-actions seq)
	  `((BEGIN ,@(unsyntax-sequence-actions seq))))
      (list (unsyntax-object seq))))

(define (unsyntax-sequence-actions seq)
  (let ((tail (if (and unsyntaxer:fold-sequence-tail?
		       (sequence? (sequence-immediate-second seq)))
		  (unsyntax-sequence-actions (sequence-immediate-second seq))
		  (list (unsyntax-object (sequence-immediate-second seq))))))
   (let ((substitution (has-substitution? (sequence-immediate-first seq))))
     (cond (substitution
	    (cons (cdr substitution) tail))
	   ((and (eq? #t unsyntaxer:macroize?)
		 (sequence? (sequence-immediate-first seq)))
	    (append (unsyntax-sequence-actions (sequence-immediate-first seq))
		    tail))
	   (else
	    (cons (unsyntax-object (sequence-immediate-first seq)) tail))))))

(define (unsyntax-OPEN-BLOCK-object open-block)
  (if (eq? #t unsyntaxer:macroize?)
      (open-block-components open-block
	(lambda (auxiliary declarations expression)
	  `(OPEN-BLOCK ,auxiliary
		       ,declarations
		       ,@(unsyntax-sequence expression))))
      (unsyntax-SEQUENCE-object open-block)))

(define (unsyntax-DELAY-object object)
  `(DELAY ,(unsyntax-object (delay-expression object))))

(define (unsyntax-THE-ENVIRONMENT-object object)
  object
  `(THE-ENVIRONMENT))

(define (unsyntax-DISJUNCTION-object object)
  `(OR ,@(disjunction-components object
	   (if (eq? #t unsyntaxer:macroize?)
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
    (if (eq? #t unsyntaxer:macroize?)
	unsyntax-conditional
	unsyntax-conditional/default)))

(define (unsyntax-conditional/default predicate consequent alternative)
  `(IF ,(unsyntax-object predicate)
       ,(unsyntax-object consequent)
       ,(unsyntax-object alternative)))

(define (unsyntax-conditional predicate consequent alternative)
  (cond ((not alternative)
	 `(AND ,@(unexpand-conjunction predicate consequent)))
	((eq? alternative undefined-conditional-branch)
	 `(IF ,(unsyntax-object predicate)
	      ,(unsyntax-object consequent)))
	((eq? consequent undefined-conditional-branch)
	 `(IF (,(ucode-primitive not) ,(unsyntax-object predicate))
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
	      (if (not alternative)
		  (unexpand-conjunction predicate consequent)
		  `(,(unsyntax-conditional predicate
					   consequent
					   alternative))))))
      `(,(unsyntax-object predicate) ,(unsyntax-object consequent))))

;;;; Lambdas

(define (unsyntax-EXTENDED-LAMBDA-object expression)
  (if unsyntaxer:macroize?
      (unsyntax-lambda expression)
      `(&XLAMBDA (,(lambda-name expression) ,@(lambda-interface expression))
		 ,(unsyntax-object (lambda-immediate-body expression)))))

(define (unsyntax-LAMBDA-object expression)
  (if unsyntaxer:macroize?
      (unsyntax-lambda expression)
      (collect-lambda (lambda-name expression)
		      (lambda-interface expression)
		      (list (unsyntax-object
			     (lambda-immediate-body expression))))))

(define (unsyntax-lambda expression)
  (lambda-components** expression
    (lambda (name required optional rest body)
      (collect-lambda name
		      (make-lambda-list required optional rest '())
		      (with-bindings required optional rest
				     unsyntax-lambda-body body)))))

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
      (make-lambda-list required optional rest '()))))

(define (lambda-components** expression receiver)
  (lambda-components expression
    (lambda (name required optional rest auxiliary declarations body)
      (define (bind-auxilliaries aux body*)
	(with-bindings aux '() #F
			   (lambda (body*)
			     (receiver name required optional rest body*))
			   body*))
      (if (and (null? auxiliary)
	       (null? declarations))
	  (scan-defines body
			(lambda (internal-defines declarations* body*)
			  declarations* body*
			  (bind-auxilliaries internal-defines body)))
	  (bind-auxilliaries auxiliary
			     (unscan-defines auxiliary declarations body))))))

(define (unsyntax-lambda-body body)
  (if (open-block? body)
      (open-block-components body
	(lambda (names declarations open-block-body)
	  (unsyntax-lambda-body-sequence
	   (unscan-defines names declarations open-block-body))))
      (unsyntax-lambda-body-sequence body)))

(define (unsyntax-lambda-body-sequence body)
  (if (sequence? body)
      (let ((first-action (sequence-immediate-first body)))
	(if (block-declaration? first-action)
	    `((DECLARE ,@(block-declaration-text first-action))
	      ,@(unsyntax-sequence (sequence-immediate-second body)))
	    (unsyntax-sequence body)))
      (list (unsyntax-object body))))

;;;; Combinations

(define (unsyntax-COMBINATION-object combination)
  (rewrite-named-let
   (combination-components combination
     (lambda (operator operands)
       (let ((ordinary-combination
	      (lambda ()
		`(,(unsyntax-object operator) ,@(map unsyntax-object operands)))))
	 (cond ((or (not (eq? #t unsyntaxer:macroize?))
		    (has-substitution? operator))
		(ordinary-combination))
	       ((and (or (eq? operator (ucode-primitive cons))
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
			     (not rest)
			     (= (length required) (length operands)))
			(if (or (eq? name lambda-tag:unnamed)
				(eq? name lambda-tag:let))
			    `(LET ,(unsyntax-let-bindings required operands)
			       ,@(with-bindings required '() #F
						unsyntax-lambda-body body))
			    (ordinary-combination))
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