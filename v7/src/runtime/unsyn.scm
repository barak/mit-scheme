;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/unsyn.scm,v 13.49 1988/02/18 16:46:02 jrm Rel $
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
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
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; UNSYNTAX: SCODE -> S-Expressions

(declare (usual-integrations))

(define unsyntax)
(define unsyntax-lambda-list)
(define make-unsyntax-table)
(define unsyntax-table?)
(define current-unsyntax-table)
(define set-current-unsyntax-table!)
(define with-unsyntax-table)

(define unsyntaxer-package
  (make-environment

(set! unsyntax
  (named-lambda (unsyntax scode #!optional unsyntax-table)
    (let ((object (if (compound-procedure? scode)
		      (procedure-lambda scode)
		      scode)))
      (if (unassigned? unsyntax-table)
	  (unsyntax-object object)
	  (with-unsyntax-table unsyntax-table
	    (lambda ()
	      (unsyntax-object object)))))))

(define (unsyntax-object object)
  ((unsyntax-dispatcher object) object))

(define (unsyntax-objects objects)
  (if (null? objects)
      '()
      (cons (unsyntax-object (car objects))
	    (unsyntax-objects (cdr objects)))))

(define (absolute-reference? object)
  (and (access? object)
       (eq? (access-environment object) system-global-environment)))

(define (absolute-reference-name reference)
  (access-name reference))

(define (absolute-reference-to? object name)
  (and (absolute-reference? object)
       (eq? (absolute-reference-name object) name)))

;;;; Unsyntax Quanta

(define (unsyntax-QUOTATION quotation)
  `(SCODE-QUOTE ,(unsyntax-object (quotation-expression quotation))))

(define (unsyntax-constant object)
  `(QUOTE ,object))

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
	     ,@(if (unassigned-object? value)
		   '()
		   `(,(unsyntax-object value)))))))

(define ((definition-unexpander key lambda-key) name value)
  (if (lambda? value)
      (lambda-components** value
	(lambda (lambda-name required optional rest body)
	  (if (eq? lambda-name name)
	      `(,lambda-key (,name . ,(lambda-list required optional rest))
		 ,@(unsyntax-sequence body))
	      `(,key ,name ,@(unexpand-binding-value value)))))
      `(,key ,name ,@(unexpand-binding-value value))))

(define (unexpand-binding-value value)
  (if (unassigned-object? value)
      '()
      `(,(unsyntax-object value))))

(define unexpand-definition
  (definition-unexpander 'DEFINE 'DEFINE))

(define (unsyntax-UNBOUND?-object unbound?)
  `(UNBOUND? ,(unbound?-name unbound?)))

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

(define (unsyntax-LAMBDA-object lambda)
  (lambda-components** lambda
    (lambda (name required optional rest body)
      (let ((bvl (lambda-list required optional rest))
	    (body (unsyntax-sequence body)))
	(if (eq? name lambda-tag:unnamed)
	    `(LAMBDA ,bvl ,@body)
	    `(NAMED-LAMBDA (,name . ,bvl) ,@body))))))

(set! unsyntax-lambda-list
  (named-lambda (unsyntax-lambda-list lambda)
    (if (not (lambda? lambda))
	(error "Must be a lambda expression" lambda))
    (lambda-components** lambda
      (lambda (name required optional rest body)
	(lambda-list required optional rest)))))

(define (lambda-list required optional rest)
  (cond ((null? rest)
	 (if (null? optional)
	     required
	     `(,@required ,(access lambda-optional-tag lambda-package)
			  ,@optional)))
	((null? optional)
	 `(,@required . ,rest))
	(else
	 `(,@required ,(access lambda-optional-tag lambda-package)
		      ,@optional . ,rest))))

(define (lambda-components** lambda receiver)
  (lambda-components lambda
    (lambda (name required optional rest auxiliary declarations body)
      (receiver name required optional rest
		(unscan-defines auxiliary declarations body)))))

;;;; Combinations

(define (unsyntax-COMBINATION-object combination)
  (combination-components combination
    (lambda (operator operands)

      (define (unsyntax-default)
	(cons (unsyntax-object operator)
	      (unsyntax-objects operands)))

      (cond ((and (or (eq? operator cons)
		      (and (variable? operator)
			   (eq? (variable-name operator) 'CONS)))
		  (= (length operands) 2)
		  (delay? (cadr operands)))
	     `(CONS-STREAM ,(unsyntax-object (car operands))
			   ,(unsyntax-object
			     (delay-expression (cadr operands)))))
	    ((eq? operator error-procedure)
	     (unsyntax-error-like-form operands 'ERROR))
	    ((absolute-reference? operator)
	     (case (absolute-reference-name operator)
	       ((ERROR-PROCEDURE)
		(unsyntax-error-like-form operands 'ERROR))
	       ((BREAKPOINT-PROCEDURE)
		(unsyntax-error-like-form operands 'BKPT))
	       (else (unsyntax-default))))
	    ((lambda? operator)
	     (lambda-components** operator
	       (lambda (name required optional rest body)
		 (if (and (null? optional)
			  (null? rest))
		     (cond ((or (eq? name lambda-tag:unnamed)
				(eq? name lambda-tag:let))
			    `(LET ,(unsyntax-let-bindings required operands)
			       ,@(unsyntax-sequence body)))
			   ((eq? name lambda-tag:deep-fluid-let)
			    (unsyntax-deep-fluid-let required operands body))
			   ((eq? name lambda-tag:shallow-fluid-let)
			    (unsyntax-shallow-fluid-let required operands
							body))
			   ((eq? name lambda-tag:common-lisp-fluid-let)
			    (unsyntax-common-lisp-fluid-let required operands
							    body))
			   ((eq? name lambda-tag:make-environment)
			    (unsyntax-make-environment required operands body))
			   #|
		            Old way when named-lambda was a letrec
			    `(LET ,name
			       ,(unsyntax-let-bindings required operands)
			       ,@(unsyntax-sequence body))))
			   |#
			   (else (unsyntax-default)))
		     (unsyntax-default)))))
	    (else (unsyntax-default))))))


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

(define (unsyntax-shallow-FLUID-LET names values body)
  (combination-components body
    (lambda (operator operands)
      `(FLUID-LET ,(unsyntax-let-bindings
		    (map extract-transfer-var
			 (sequence-actions (lambda-body (car operands))))
		    (let every-other ((values values))
		      (if (null? values)
			  '()
			  (cons (car values) (every-other (cddr values))))))
	 ,@(lambda-components** (cadr operands)
	     (lambda (name required optional rest body)
	       (unsyntax-sequence body)))))))

(define (extract-transfer-var assignment)
  (assignment-components assignment
    (lambda (name value)
      (cond ((assignment? value)
	     (assignment-components value (lambda (name value) name)))
	    ((combination? value)
	     (combination-components value
	       (lambda (operator operands)
		 (cond ((eq? operator lexical-assignment)
			`(ACCESS ,(cadr operands)
				 ,@(unexpand-access (car operands))))
		       (else
			(error "FLUID-LET: Unknown SCODE form" assignment))))))
	    (else
	     (error "FLUID-LET: Unknown SCODE form" assignment))))))

(define ((unsyntax-deep-or-common-FLUID-LET name prim)
	 ignored-required ignored-operands body)
  (define (sequence->list seq)
    (if (sequence? seq)
	(sequence-actions seq)
	(list seq)))
  (define (unsyntax-fluid-bindings l)
    (define (unsyntax-fluid-assignment combi)
      (let ((operands (combination-operands combi)))
	(let ((env (first operands))
	      (name (second operands))
	      (val (third operands)))
	  (cond ((symbol? name)
		 `((ACCESS ,name ,(unsyntax-object env))
		   ,(unsyntax-object val)))
		((quotation? name)
		 (let ((var (quotation-expression name)))
		   (if (variable? var)
		       `(,(variable-name var) ,(unsyntax-object val))
		       (error "FLUID-LET unsyntax: unexpected name" name))))
		(else
		 (error "FLUID-LET unsyntax: unexpected name" name))))))
    (let ((first (car l)))
      (if (and (combination? first)
	       (eq? (combination-operator first) prim))
	  (let ((remainder (unsyntax-fluid-bindings (cdr l))))
	    (cons
	     (cons (unsyntax-fluid-assignment first) (car remainder))
	     (cdr remainder)))
	  (cons '() (unsyntax-objects l)))))
	  
  (let* ((thunk (car (combination-operands body)))
	 (real-body (lambda-body thunk))
	 (seq-list (sequence->list real-body))
	 (fluid-binding-list (unsyntax-fluid-bindings seq-list)))
    `(,name ,(car fluid-binding-list) ,@(cdr fluid-binding-list))))

(define unsyntax-deep-FLUID-LET
  (unsyntax-deep-or-common-FLUID-LET
   'FLUID-LET (make-primitive-procedure 'add-fluid-binding! 3)))

(define unsyntax-common-lisp-FLUID-LET
  (unsyntax-deep-or-common-FLUID-LET
   'FLUID-BIND (make-primitive-procedure 'make-fluid-binding! 3)))

(define (unsyntax-MAKE-ENVIRONMENT names values body)
  `(MAKE-ENVIRONMENT ,@(except-last-pair (unsyntax-sequence body))))

(define (unsyntax-let-bindings names values)
  (map unsyntax-let-binding names values))

(define (unsyntax-let-binding name value)
  `(,name ,@(unexpand-binding-value value)))

;;;; Unsyntax Tables

(define unsyntax-table-tag
  '(UNSYNTAX-TABLE))

(set! make-unsyntax-table
  (named-lambda (make-unsyntax-table alist)
    (cons unsyntax-table-tag
	  (make-type-dispatcher alist identity-procedure))))

(set! unsyntax-table?
  (named-lambda (unsyntax-table? object)
    (and (pair? object)
	 (eq? (car object) unsyntax-table-tag))))

(set! current-unsyntax-table
  (named-lambda (current-unsyntax-table)
    *unsyntax-table))

(set! set-current-unsyntax-table!
  (named-lambda (set-current-unsyntax-table! table)
    (if (not (unsyntax-table? table))
	(error "Not an unsyntax table" 'SET-CURRENT-UNSYNTAX-TABLE! table))
    (set-table! table)))

(set! with-unsyntax-table
  (named-lambda (with-unsyntax-table table thunk)
    (define old-table)
    (if (not (unsyntax-table? table))
	(error "Not an unsyntax table" 'WITH-UNSYNTAX-TABLE table))
    (dynamic-wind (lambda ()
		    (set! old-table (set-table! table)))
		  thunk
		  (lambda ()
		    (set! table (set-table! old-table))))))

(define unsyntax-dispatcher)
(define *unsyntax-table)

(define (set-table! table)
  (set! unsyntax-dispatcher (cdr table))
  (set! *unsyntax-table table))

;;;; Default Unsyntax Table

(set-table!
 (make-unsyntax-table
  `((,(microcode-type-object 'LIST) ,unsyntax-constant)
    (,symbol-type ,unsyntax-constant)
    (,variable-type ,unsyntax-VARIABLE-object)
    (,unbound?-type ,unsyntax-UNBOUND?-object)
    (,unassigned?-type ,unsyntax-UNASSIGNED?-object)
    (,combination-type ,unsyntax-COMBINATION-object)
    (,quotation-type ,unsyntax-QUOTATION)
    (,access-type ,unsyntax-ACCESS-object)
    (,definition-type ,unsyntax-DEFINITION-object)
    (,assignment-type ,unsyntax-ASSIGNMENT-object)
    (,conditional-type ,unsyntax-CONDITIONAL-object)
    (,disjunction-type ,unsyntax-DISJUNCTION-object)
    (,comment-type ,unsyntax-COMMENT-object)
    (,declaration-type ,unsyntax-DECLARATION-object)
    (,sequence-type ,unsyntax-SEQUENCE-object)
    (,open-block-type ,unsyntax-OPEN-BLOCK-object)
    (,delay-type ,unsyntax-DELAY-object)
    (,in-package-type ,unsyntax-IN-PACKAGE-object)
    (,the-environment-type ,unsyntax-THE-ENVIRONMENT-object)
    (,lambda-type ,unsyntax-LAMBDA-object))))

;;; end UNSYNTAXER-PACKAGE
))