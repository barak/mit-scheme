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
		    ((or (scode-definition? expression)
			 (and (scode-open-block? expression)
			      (pair? (scode-open-block-names expression))))
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
  (remove (lambda (item)
	    (memq item items*))
	  items))

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
	 `((access ,rewrite/access)
	   (assignment ,rewrite/assignment)
	   (combination ,rewrite/combination)
	   (comment ,rewrite/comment)
	   (conditional ,rewrite/conditional)
	   (delay ,rewrite/delay)
	   (disjunction ,rewrite/disjunction)
	   (lambda ,rewrite/lambda)
	   (sequence ,rewrite/sequence)
	   (the-environment ,rewrite/the-environment)
	   (unassigned? ,rewrite/unassigned?)
	   (variable ,rewrite/variable))))
  (set! hook/extended-scode-eval default/extended-scode-eval)
  unspecific)

(define (rewrite/variable expression environment bound-names)
  (let ((name (scode-variable-name expression)))
    (if (memq name bound-names)
	(ccenv-lookup environment name)
	expression)))

(define (rewrite/unassigned? expression environment bound-names)
  (let ((name (scode-unassigned?-name expression)))
    (if (memq name bound-names)
	(make-scode-combination
	 (make-scode-absolute-reference 'unassigned-reference-trap?)
	 (list (ccenv-lookup environment name)))
	expression)))

(define (ccenv-lookup environment name)
  (make-scode-combination (make-scode-absolute-reference 'environment-lookup)
			  (list (environment-that-binds environment name)
				name)))

(define (rewrite/assignment expression environment bound-names)
  (let ((name (scode-assignment-name expression))
	(value
	 (rewrite/expression (scode-assignment-value expression)
			     environment
			     bound-names)))
    (if (memq name bound-names)
	(let ((environment (environment-that-binds environment name)))
	  (if (not (environment-assignable? environment name))
	      (error
	       "Cannot perform assignment to this compiled-code variable:"
	       name))
	  (make-scode-combination
	   (make-scode-absolute-reference 'environment-assign!)
	   (list environment name value)))
	(make-scode-assignment name value))))

(define (rewrite/lambda expression environment bound-names)
  (lambda-components* expression
    (lambda (name required optional rest body)
      (make-lambda*
       name required optional rest
       (rewrite/expression body
			   environment
			   (difference bound-names
				       (scode-lambda-bound expression)))))))

(define (rewrite/the-environment expression environment bound-names)
  expression environment bound-names
  (error "Can't take (the-environment) of compiled-code environment"))

(define (rewrite/access expression environment bound-names)
  (make-scode-access (rewrite/expression (scode-access-environment expression)
					 environment
					 bound-names)
		     (scode-access-name expression)))

(define (rewrite/combination expression environment bound-names)
  (make-scode-combination
   (rewrite/expression (scode-combination-operator expression)
		       environment
		       bound-names)
   (rewrite/expressions (scode-combination-operands expression)
			environment
			bound-names)))

(define (rewrite/comment expression environment bound-names)
  (make-scode-comment (scode-comment-text expression)
		      (rewrite/expression (scode-comment-expression expression)
					  environment
					  bound-names)))

(define (rewrite/conditional expression environment bound-names)
  (make-scode-conditional
   (rewrite/expression (scode-conditional-predicate expression)
		       environment
		       bound-names)
   (rewrite/expression (scode-conditional-consequent expression)
		       environment
		       bound-names)
   (rewrite/expression (scode-conditional-alternative expression)
		       environment
		       bound-names)))

(define (rewrite/delay expression environment bound-names)
  (make-scode-delay (rewrite/expression (scode-delay-expression expression)
					environment
					bound-names)))

(define (rewrite/disjunction expression environment bound-names)
  (make-scode-disjunction
   (rewrite/expression (scode-disjunction-predicate expression)
		       environment
		       bound-names)
   (rewrite/expression (scode-disjunction-alternative expression)
		       environment
		       bound-names)))

(define (rewrite/sequence expression environment bound-names)
  (make-scode-sequence (rewrite/expressions (scode-sequence-actions expression)
					    environment
					    bound-names)))

(define (rewrite/constant expression environment bound-names)
  environment bound-names
  expression)