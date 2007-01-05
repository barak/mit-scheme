#| -*-Scheme-*-

$Id: xeval.scm,v 1.14 2007/01/05 21:19:28 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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
				  (pair? names)))))
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
			(make-root-top-level-environment))))))))

(define (difference items items*)
  (list-transform-negative items
    (lambda (item)
      (memq item items*))))

(define (environment-that-binds environment name)
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
		    (list (environment-that-binds environment name) name)))

(define (rewrite/assignment expression environment bound-names)
  (let ((name (assignment-name expression))
	(value
	 (rewrite/expression (assignment-value expression)
			     environment
			     bound-names)))
    (if (memq name bound-names)
	(let ((environment (environment-that-binds environment name)))
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

(define (rewrite/sequence expression environment bound-names)
  (make-sequence (rewrite/expressions (sequence-actions expression)
				      environment
				      bound-names)))

(define (rewrite/constant expression environment bound-names)
  environment bound-names
  expression)