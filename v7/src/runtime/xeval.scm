#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/xeval.scm,v 1.4 1991/04/15 20:48:03 jinx Exp $

Copyright (c) 1989-91 Massachusetts Institute of Technology

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

;;;; SCode Evaluator extended for compiled-code environments
;;; package: (runtime extended-scode-eval)

(declare (usual-integrations))

(define hook/extended-scode-eval)

(define (default/extended-scode-eval expression environment)
  (scode-eval expression environment))

(define (extended-scode-eval expression environment)
  (cond ((interpreter-environment? environment)
	 (hook/extended-scode-eval expression environment))
	((scode-constant? expression)
	 expression)
	(else
	 (with-values (lambda () (split-environment environment))
	   (lambda (bound-names interpreter-environment)
	     (hook/extended-scode-eval
	      (cond ((null? bound-names)
		     expression)
		    ((or (definition? expression)
			 (and (open-block? expression)
			      (open-block-components expression
				(lambda (names declarations body)
				  declarations body
				  (not (null? names))))))
		     (error
		      "Can't perform definition in compiled-code environment:"
		      (unsyntax expression)))
		    (else
		     (rewrite/expression expression environment bound-names)))
	      interpreter-environment))))))

(define (split-environment environment)
  (let ((finish
	 (lambda (bound-names environment)
	   (values (apply append (reverse! bound-names)) environment))))
    (let loop ((bound-names '()) (environment environment))
      (if (interpreter-environment? environment)
	  (finish bound-names environment)
	  (let ((bound-names
		 (cons (environment-bound-names environment) bound-names)))
	    (if (environment-has-parent? environment)
		(loop bound-names (environment-parent environment))
		(finish bound-names
			(make-null-interpreter-environment))))))))

(define (difference items items*)
  (list-transform-negative items
    (lambda (item)
      (memq item items*))))

(define (environment-which-binds environment name)
  (let loop ((environment environment))
    (if (environment-bound? environment name)
	environment
	(loop
	 (if (environment-has-parent? environment)
	     (environment-parent environment)
	     (error "Environment has no parent:" environment))))))

(define (rewrite/expression expression environment bound-names)
  ((scode-walk rewrite-walker expression) expression environment bound-names))

(define (rewrite/expressions expressions environment bound-names)
  (map (lambda (expression)
	 (rewrite/expression expression environment bound-names))
       expressions))

(define rewrite-walker)

(define (initialize-package!)
  (set! rewrite-walker
	(make-scode-walker
	 rewrite/constant
	 `((ACCESS ,rewrite/access)
	   (ASSIGNMENT ,rewrite/assignment)
	   (COMBINATION ,rewrite/combination)
	   (COMMENT ,rewrite/comment)
	   (CONDITIONAL ,rewrite/conditional)
	   (DELAY ,rewrite/delay)
	   (DISJUNCTION ,rewrite/disjunction)
	   (IN-PACKAGE ,rewrite/in-package)
	   (LAMBDA ,rewrite/lambda)
	   (SEQUENCE ,rewrite/sequence)
	   (THE-ENVIRONMENT ,rewrite/the-environment)
	   (UNASSIGNED? ,rewrite/unassigned?)
	   (VARIABLE ,rewrite/variable))))
  (set! hook/extended-scode-eval default/extended-scode-eval)
  unspecific)

(define (rewrite/variable expression environment bound-names)
  (let ((name (variable-name expression)))
    (if (memq name bound-names)
	(ccenv-lookup environment name)
	expression)))

(define (rewrite/unassigned? expression environment bound-names)
  (let ((name (unassigned?-name expression)))
    (if (memq name bound-names)
	(make-combination (make-absolute-reference 'UNASSIGNED-REFERENCE-TRAP?)
			  (list (ccenv-lookup environment name)))
	expression)))

(define (ccenv-lookup environment name)
  (make-combination (make-absolute-reference 'ENVIRONMENT-LOOKUP)
		    (list (environment-which-binds environment name) name)))

(define (rewrite/assignment expression environment bound-names)
  (let ((name (assignment-name expression))
	(value
	 (rewrite/expression (assignment-value expression)
			     environment
			     bound-names)))
    (if (memq name bound-names)
	(let ((environment (environment-which-binds environment name)))
	  (if (not (environment-assignable? environment name))
	      (error
	       "Cannot perform assignment to this compiled-code variable:"
	       name))
	  (make-combination (make-absolute-reference 'ENVIRONMENT-ASSIGN!)
			    (list environment name value)))
	(make-assignment name value))))

(define (rewrite/lambda expression environment bound-names)
  (lambda-components expression
    (lambda (name required optional rest auxiliary declarations body)
      (make-lambda
       name required optional rest auxiliary declarations
       (rewrite/expression body
			   environment
			   (difference bound-names
				       (append required
					       optional
					       auxiliary
					       (if rest (list rest) '()))))))))

(define (rewrite/the-environment expression environment bound-names)
  expression environment bound-names
  (error "Can't take (the-environment) of compiled-code environment"))

(define (rewrite/access expression environment bound-names)
  (make-access (rewrite/expression (access-environment expression)
				   environment
				   bound-names)
	       (access-name expression)))

(define (rewrite/combination expression environment bound-names)
  (make-combination (rewrite/expression (combination-operator expression)
					environment
					bound-names)
		    (rewrite/expressions (combination-operands expression)
					 environment
					 bound-names)))

(define (rewrite/comment expression environment bound-names)
  (make-comment (comment-text expression)
		(rewrite/expression (comment-expression expression)
				    environment
				    bound-names)))

(define (rewrite/conditional expression environment bound-names)
  (make-conditional (rewrite/expression (conditional-predicate expression)
					environment
					bound-names)
		    (rewrite/expression (conditional-consequent expression)
					environment
					bound-names)
		    (rewrite/expression (conditional-alternative expression)
					environment
					bound-names)))

(define (rewrite/delay expression environment bound-names)
  (make-delay (rewrite/expression (delay-expression expression)
				  environment
				  bound-names)))

(define (rewrite/disjunction expression environment bound-names)
  (make-disjunction (rewrite/expression (disjunction-predicate expression)
					environment
					bound-names)
		    (rewrite/expression (disjunction-alternative expression)
					environment
					bound-names)))

(define (rewrite/in-package expression environment bound-names)
  (make-in-package (rewrite/expression (in-package-environment expression)
				       environment
				       bound-names)
		   (in-package-expression expression)))

(define (rewrite/sequence expression environment bound-names)
  (make-sequence (rewrite/expressions (sequence-actions expression)
				      environment
				      bound-names)))

(define (rewrite/constant expression environment bound-names)
  environment bound-names
  expression)