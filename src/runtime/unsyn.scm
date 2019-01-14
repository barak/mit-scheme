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

;;;; UNSYNTAX: SCode -> S-Expression
;;; package: (runtime unsyntaxer)

(declare (usual-integrations))

;;; If UNSYNTAXER:MACROIZE? is #f, then the unsyntaxed output will
;;; closely match the concrete structure that is given to SCODE-EVAL.
;;; If it is #t, then the unsyntaxed output will more closely match
;;; the abstract structure of the SCODE (as output by syntax and sf).

(define-deferred unsyntaxer:macroize? (make-settable-parameter #t))
(define-deferred unsyntaxer:elide-global-accesses? (make-settable-parameter #t))
(define-deferred unsyntaxer:show-comments? (make-settable-parameter #f))

;;; The substitutions mechanism is for putting the '### marker in
;;; debugger output.
(define-deferred substitutions (make-unsettable-parameter '()))

(define (unsyntax-with-substitutions scode alist)
  (if (not (alist? alist))
      (error:wrong-type-argument alist "alist" 'unsyntax-with-substitutions))
  (parameterize ((substitutions alist))
    (unsyntax scode)))

(define-integrable (maybe-substitute object thunk)
  (let ((association (has-substitution? object)))
    (if association
	(cdr association)
	(thunk))))

(define-integrable (has-substitution? object)
  (let ((substs (substitutions)))
    (and (pair? substs) (assq object substs))))

(define (with-bindings environment lambda receiver)
  (if (and (unsyntaxer:elide-global-accesses?)
	   (unsyntaxer:macroize?))
      (receiver (cons lambda environment))
      (receiver environment)))

(define (is-bound? name environment)
  (any (lambda (binding-lambda)
	 (scode-lambda-bound? binding-lambda name))
       environment))

(define (unsyntax scode)
  (unsyntax-object '()
		   (if (procedure? scode) (procedure-lambda scode) scode)))

(define (unsyntax-object environment object)
  (maybe-substitute
   object
   (lambda ()
     ((scode-walk unsyntaxer/scode-walker object) environment object))))

(define-deferred unsyntaxer/scode-walker
  (make-scode-walker unsyntax-constant
		     `((access ,unsyntax-access-object)
		       (assignment ,unsyntax-assignment-object)
		       (combination ,unsyntax-combination-object)
		       (comment ,unsyntax-comment-object)
		       (conditional ,unsyntax-conditional-object)
		       (declaration ,unsyntax-declaration-object)
		       (definition ,unsyntax-definition-object)
		       (delay ,unsyntax-delay-object)
		       (disjunction ,unsyntax-disjunction-object)
		       (extended-lambda ,unsyntax-extended-lambda-object)
		       (lambda ,unsyntax-lambda-object)
		       (open-block ,unsyntax-open-block-object)
		       (quotation ,unsyntax-quotation)
		       (sequence ,unsyntax-sequence-object)
		       (the-environment ,unsyntax-the-environment-object)
		       (variable ,unsyntax-variable-object))))

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
	 `(quote ,object))
	((compiled-expression? object)
	 (let ((scode (compiled-expression/scode object)))
	   (if (eq? scode object)
	       object
	       (unsyntax-object environment scode))))
	(else
	 object)))

(define (unsyntax-quotation environment quotation)
  `(scode-quote
    ,(unsyntax-object environment (scode-quotation-expression quotation))))

(define (unsyntax-variable-object environment object)
  (declare (ignore environment))
  (scode-variable-name object))

(define (unsyntax-access-object environment object)
  (or (and (unsyntaxer:elide-global-accesses?)
	   (unsyntaxer:macroize?)
	   (let ((access-environment (scode-access-environment object))
		 (name (scode-access-name object)))
	     (and (or (eq? access-environment system-global-environment)
		      (and (scode-variable? access-environment)
			   (eq? (scode-variable-name access-environment)
				'system-global-environment)))
		  (not (is-bound? name environment))
		  name)))
      `(access ,@(unexpand-access environment object))))

(define (unexpand-access environment object)
  (let loop ((object object) (separate? #t))
    (if (and separate?
	     (scode-access? object)
	     (not (has-substitution? object)))
	`(,(scode-access-name object)
	  ,@(loop (scode-access-environment object)
		  (eq? #t (unsyntaxer:macroize?))))
	`(,(unsyntax-object environment object)))))

(define (unsyntax-definition-object environment definition)
  (unexpand-definition environment
		       (scode-definition-name definition)
		       (scode-definition-value definition)))

(define (unexpand-definition environment name value)
  (cond ((and (eq? #t (unsyntaxer:macroize?))
	      (macro-reference-trap-expression? value))
	 (or (rewrite-macro-defn
	      environment
	      name
	      (macro-reference-trap-expression-transformer value))
	     `(define ,name ,(unsyntax-object environment value))))
	((and (eq? #t (unsyntaxer:macroize?))
	      (scode-lambda? value)
	      (not (has-substitution? value)))
	 (lambda-components* value
	   (lambda (lambda-name required optional rest body)
	     (if (eq? lambda-name name)
		 `(define (,name
			   . ,(make-lambda-list required optional rest '()))
		    ,@(with-bindings environment value
			(lambda (environment*)
			  (unsyntax-lambda-body environment* body))))
		 `(define ,name
		    ,@(unexpand-binding-value environment value))))))
	(else
	 `(define ,name ,@(unexpand-binding-value environment value)))))

(define (rewrite-macro-defn environment name transformer)
  (and (scode-combination? transformer)
       (let ((operator (scode-combination-operator transformer))
	     (operands (scode-combination-operands transformer)))
	 (and (scode-access? operator)
	      (eq? system-global-environment
		   (scode-access-environment operator))
	      ;; Two args for legacy; three for new.
	      ;; Erase legacy support after 9.3 release.
	      (or (= 2 (length operands))
		  (= 3 (length operands)))
	      (scode-lambda? (car operands))
	      (scode-the-environment? (cadr operands))
	      (let ((rewrite
		     (lambda (keyword)
		       `(define-syntax ,name
			  (,keyword
			   ,(unsyntax-object environment (car operands)))))))
		(case (scode-access-name operator)
		  ((sc-macro-transformer->expander)
		   (rewrite 'sc-macro-transformer))
		  ((rsc-macro-transformer->expander)
		   (rewrite 'rsc-macro-transformer))
		  ((er-macro-transformer->expander)
		   (rewrite 'er-macro-transformer))
		  (else #f)))))))

(define (unsyntax-assignment-object environment assignment)
  `(set! ,(scode-assignment-name assignment)
	 ,@(unexpand-binding-value environment
				   (scode-assignment-value assignment))))

(define (unexpand-binding-value environment value)
  (if (unassigned-reference-trap? value)
      '()
      `(,(unsyntax-object environment value))))

(define (unsyntax-comment-object environment comment)
  (let ((expression
	 (unsyntax-object environment (scode-comment-expression comment))))
    (if (unsyntaxer:show-comments?)
	`(comment ,(scode-comment-text comment) ,expression)
	expression)))

(define (unsyntax-declaration-object environment declaration)
  `(local-declare
    ,(scode-declaration-text declaration)
    ,(unsyntax-object environment (scode-declaration-expression declaration))))

(define (unsyntax-sequence-object environment seq)
  (let ((actions (scode-sequence-actions seq)))
    (if (and (scode-block-declaration? (car actions))
	     (pair? (cdr actions)))
	`(begin
	  (declare ,@(scode-block-declaration-text (car actions)))
	  ,@(unsyntax-sequence-actions environment (cdr actions)))
	`(begin
	  ,@(unsyntax-sequence-actions environment actions)))))

(define (unsyntax-sequence-for-splicing environment seq)
  (if (scode-sequence? seq)
      (let ((actions
	     (unsyntax-sequence-actions environment
					(scode-sequence-actions seq))))
	(if (eq? #t (unsyntaxer:macroize?))
	    actions
	    `((begin ,@actions))))
      (list (unsyntax-object environment seq))))

(define (unsyntax-sequence-actions environment actions)
  (map (lambda (action)
	 (maybe-substitute action
			   (lambda ()
			     (unsyntax-object environment action))))
       actions))

(define (unsyntax-open-block-object environment open-block)
  (if (eq? #t (unsyntaxer:macroize?))
      (unsyntax-object
       environment
       (unscan-defines (scode-open-block-names open-block)
		       (scode-open-block-declarations open-block)
		       (scode-open-block-actions open-block)))
      (unsyntax-sequence-object environment open-block)))

(define (unsyntax-delay-object environment object)
  `(delay ,(unsyntax-object environment (scode-delay-expression object))))

(define (unsyntax-the-environment-object environment object)
  (declare (ignore environment object))
  `(the-environment))

(define (unsyntax-disjunction-object environment object)
  `(or ,@(let ((predicate (scode-disjunction-predicate object))
	       (alternative (scode-disjunction-alternative object)))
	   (if (eq? #t (unsyntaxer:macroize?))
	       (unexpand-disjunction environment predicate alternative)
	       (list (unsyntax-object environment predicate)
		     (unsyntax-object environment alternative))))))

(define (unexpand-disjunction environment predicate alternative)
  `(,(unsyntax-object environment predicate)
    ,@(if (scode-disjunction? alternative)
	  (unexpand-disjunction environment
				(scode-disjunction-predicate alternative)
				(scode-disjunction-alternative alternative))
	  `(,(unsyntax-object environment alternative)))))

(define (unsyntax-conditional-object environment conditional)
  (let ((predicate (scode-conditional-predicate conditional))
	(consequent (scode-conditional-consequent conditional))
	(alternative (scode-conditional-alternative conditional)))
    (if (eq? #t (unsyntaxer:macroize?))
	(unsyntax-conditional environment predicate consequent alternative)
	(unsyntax-conditional/default
	 environment predicate consequent alternative))))

(define (unsyntax-conditional/default environment
				      predicate consequent alternative)
  `(if ,(unsyntax-object environment predicate)
       ,(unsyntax-object environment consequent)
       ,(unsyntax-object environment alternative)))

(define (unsyntax-conditional environment predicate consequent alternative)
  (cond ((not alternative)
	 `(and ,@(unexpand-conjunction environment predicate consequent)))
	((eq? alternative undefined-scode-conditional-branch)
	 `(if ,(unsyntax-object environment predicate)
	      ,(unsyntax-object environment consequent)))
	((eq? consequent undefined-scode-conditional-branch)
	 `(if (,(ucode-primitive not) ,(unsyntax-object environment predicate))
	      ,(unsyntax-object environment alternative)))
	((and (scode-conditional? alternative)
	      (not (has-substitution? alternative)))
	 `(cond ,@(unsyntax-cond-conditional environment predicate
					     consequent
					     alternative)))
	(else
	 (unsyntax-conditional/default environment
				       predicate consequent alternative))))

(define (unsyntax-cond-conditional environment
				   predicate consequent alternative)
  `((,(unsyntax-object environment predicate)
     ,@(unsyntax-sequence-for-splicing environment consequent))
    ,@(unsyntax-cond-alternative environment alternative)))

(define (unsyntax-cond-disjunction environment predicate alternative)
  `((,(unsyntax-object environment predicate))
    ,@(unsyntax-cond-alternative environment alternative)))

(define (unsyntax-cond-alternative environment alternative)
  (cond ((eq? alternative undefined-scode-conditional-branch)
	 '())
	((has-substitution? alternative)
	 =>
	 (lambda (substitution)
	   `((else ,substitution))))
	((scode-disjunction? alternative)
	 (unsyntax-cond-disjunction
	  environment
	  (scode-disjunction-predicate alternative)
	  (scode-disjunction-alternative alternative)))
	((scode-conditional? alternative)
	 (unsyntax-cond-conditional
	  environment
	  (scode-conditional-predicate alternative)
	  (scode-conditional-consequent alternative)
	  (scode-conditional-alternative alternative)))
	(else
	 `((else ,@(unsyntax-sequence-for-splicing environment alternative))))))

(define (unexpand-conjunction environment predicate consequent)
  (if (and (scode-conditional? consequent)
	   (not (has-substitution? consequent)))
      `(,(unsyntax-object environment predicate)
	,@(let ((predicate (scode-conditional-predicate consequent))
		(consequent (scode-conditional-consequent consequent))
		(alternative (scode-conditional-alternative consequent)))
	    (if (not alternative)
		(unexpand-conjunction environment predicate consequent)
		`(,(unsyntax-conditional environment predicate
					 consequent
					 alternative)))))
      `(,(unsyntax-object environment predicate)
	,(unsyntax-object environment consequent))))

;;;; Lambdas

(define (unsyntax-extended-lambda-object environment expression)
  (if (unsyntaxer:macroize?)
      (unsyntax-lambda environment expression)
      `(&xlambda (,(scode-lambda-name expression)
		  ,@(scode-lambda-interface expression))
		 ,(unsyntax-object environment
				   (lambda-immediate-body expression)))))

(define (unsyntax-lambda-object environment expression)
  (if (unsyntaxer:macroize?)
      (unsyntax-lambda environment expression)
      (collect-lambda (scode-lambda-name expression)
		      (scode-lambda-interface expression)
		      (list (unsyntax-object environment
			     (lambda-immediate-body expression))))))

(define (unsyntax-lambda environment expression)
  (lambda-components* expression
    (lambda (name required optional rest body)
      (collect-lambda name
		      (make-lambda-list required optional rest '())
		      (with-bindings environment expression
			(lambda (environment*)
			  (unsyntax-lambda-body environment* body)))))))

(define (collect-lambda name bvl body)
  (if (eq? name scode-lambda-name:unnamed)
      `(lambda ,bvl ,@body)
      `(named-lambda (,name . ,bvl) ,@body)))

(define (unsyntax-lambda-list expression)
  (if (not (scode-lambda? expression))
      (error:wrong-type-argument expression "SCode lambda"
				 'unsyntax-lambda-list))
  (lambda-components* expression
    (lambda (name required optional rest body)
      name body
      (make-lambda-list required optional rest '()))))

(define (unsyntax-lambda-body environment body)
  (if (scode-open-block? body)
      (unsyntax-lambda-body-sequence
       environment
       (unscan-defines (scode-open-block-names body)
		       (scode-open-block-declarations body)
		       (scode-open-block-actions body)))
      (unsyntax-lambda-body-sequence environment body)))

(define (unsyntax-lambda-body-sequence environment body)
  (if (scode-sequence? body)
      (let ((actions (scode-sequence-actions body)))
	(if (and (scode-block-declaration? (car actions))
		 (pair? (cdr actions)))
	    `((declare ,@(scode-block-declaration-text (car actions)))
	      ,@(unsyntax-sequence-for-splicing
		 environment
		 (make-scode-sequence (cdr actions))))
	    (unsyntax-sequence-for-splicing environment body)))
      (list (unsyntax-object environment body))))

;;;; Combinations

(define (unsyntax-combination-object environment combination)
  (rewrite-named-let
   (let ((operator (scode-combination-operator combination))
	 (operands (scode-combination-operands combination)))
     (let ((ordinary-combination
	    (lambda ()
	      `(,(unsyntax-object environment operator)
		,@(map (lambda (operand)
			 (unsyntax-object environment operand))
		       operands)))))
       (cond ((or (not (eq? #t (unsyntaxer:macroize?)))
		  (has-substitution? operator))
	      (ordinary-combination))
	     ((and (or (eq? operator (ucode-primitive cons))
		       (scode-absolute-reference-to? operator 'cons))
		   (= (length operands) 2)
		   (scode-delay? (cadr operands))
		   (not (has-substitution? (cadr operands))))
	      `(cons-stream ,(unsyntax-object environment (car operands))
			    ,(unsyntax-object environment
			      (scode-delay-expression (cadr operands)))))
	     ((scode-lambda? operator)
	      (lambda-components* operator
		(lambda (name required optional rest body)
		  (if (and (null? optional)
			   (not rest)
			   (= (length required) (length operands)))
		      (if (or (eq? name scode-lambda-name:unnamed)
			      (eq? name scode-lambda-name:let))
			  `(let ,(unsyntax-let-bindings environment
							required
							operands)
			     ,@(with-bindings environment operator
				 (lambda (environment*)
				   (unsyntax-lambda-body environment* body))))
			  (ordinary-combination))
		      (ordinary-combination)))))
	     (else
	      (ordinary-combination)))))))

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
		  (eq? 'let (car expression))
		  (eq? '() (cadr expression))
		  (symbol? (cadddr expression))
		  (let ((definition (caddr expression)))
		    (and (pair? definition)
			 (eq? 'define (car definition))
			 (pair? (cadr definition))
			 (eq? (caadr definition) (cadddr expression))
			 (list? (cdadr definition))
			 (every symbol? (cdadr definition)))))))
      `(let ,(cadddr (car expression))
	 ,(map (lambda (name value)
		 `(,name
		   ,@(if (unassigned-reference-trap? value)
			 '()
			 `(,value))))
	       (cdadr (caddr (car expression)))
	       (cdr expression))
	 ,@(cddr (caddr (car expression))))
      expression))