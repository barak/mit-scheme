#| -*-Scheme-*-

$Id: xform.scm,v 4.11 2001/12/20 16:28:23 cph Exp $

Copyright (c) 1988-2001 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
|#

;;;; SCode Optimizer: Transform Input Expression
;;; package: (scode-optimizer transform)

(declare (usual-integrations)
	 (integrate-external "object"))

(define (transform/top-level expression shadowed-names)
  (let ((block (block/make false false '())))
    (for-each (lambda (name)
		(variable/make&bind! block name))
	      shadowed-names)
    (values block (transform/top-level-1 true block block expression))))

(define (transform/recursive block top-level-block expression)
  (transform/top-level-1 false top-level-block block expression))

(define top-level?)
(define top-level-block)
(define root-block)

(define (transform/top-level-1 tl? tl-block block expression)
  (fluid-let ((top-level? tl?)
	      (top-level-block tl-block)
	      (root-block block))
    (let ((environment
	   (if top-level?
	       (environment/bind (environment/make)
				 (block/bound-variables-list block))
	       (environment/make))))
      (if (scode-open-block? expression)
	  (begin
	    (if (not top-level?)
		(error "Open blocks allowed only at top level:" expression))
	    (call-with-values
		(lambda () (open-block-components expression values))
	      (lambda (auxiliary declarations body)
		(if (not (assq 'USUAL-INTEGRATIONS declarations))
		    (write-string ui-warning (notification-output-port)))
		(transform/open-block* expression
				       block
				       environment
				       auxiliary
				       declarations
				       body))))
	  (transform/expression block environment expression)))))

(define ui-warning
  ";This program does not have a USUAL-INTEGRATIONS declaration.
;Without this declaration, the compiler will be unable to perform
;many optimizations, and as a result the compiled program will be
;slower and perhaps larger than it could be.  Please read the MIT
;Scheme User's Guide for more information about USUAL-INTEGRATIONS.
")

(define (transform/expressions block environment expressions)
  (map (lambda (expression)
	 (transform/expression block environment expression))
       expressions))

(declare (integrate-operator transform/expression))

(define (transform/expression block environment expression)
  ((scode-walk transform/dispatch expression) block environment expression))

(define (environment/make)
  '())

(define (environment/lookup environment name)
  (let ((association (assq name environment)))
    (if association
	(cdr association)
	(or (block/lookup-name root-block name false)
	    (variable/make&bind! top-level-block name)))))

(define (environment/bind environment variables)
  (map* environment
	(lambda (variable)
	  (cons (variable/name variable) variable))
	variables))

(define (transform/open-block block environment expression)
  (call-with-values (lambda () (open-block-components expression values))
    (lambda (auxiliary declarations body)
      (transform/open-block* expression
			     (block/make block true '())
			     environment
			     auxiliary
			     declarations
			     body))))

(define (transform/open-block* expression block environment auxiliary
			       declarations body)
  (let ((variables
	 (map (lambda (name) (variable/make&bind! block name))
	      auxiliary)))
    (set-block/declarations! block (declarations/parse block declarations))
    (call-with-values
	(lambda ()
	  (let ((environment (environment/bind environment variables)))
	    (let ((transform
		   (lambda (subexpression)
		     (transform/expression block environment subexpression))))
	      (let loop
		  ((variables variables)
		   (actions (sequence-actions body)))
		(cond ((null? variables)
		       (values '() (map transform actions)))
		      ((null? actions)
		       (error "Extraneous auxiliaries" variables))
		      ;; Because `scan-defines' returns the auxiliary
		      ;; names in a particular order, we can expect to
		      ;; encounter them in that same order when
		      ;; looking through the body's actions.
		      ((and (scode-assignment? (car actions))
			    (eq? (assignment-name (car actions))
				 (variable/name (car variables))))
		       (call-with-values
			   (lambda () (loop (cdr variables) (cdr actions)))
			 (lambda (vals actions*)
			   (values
			    (cons (transform (assignment-value (car actions)))
				  vals)
			    (cons open-block/value-marker actions*)))))
		      (else
		       (call-with-values
			   (lambda () (loop variables (cdr actions)))
			 (lambda (vals actions*)
			   (values vals
				   (cons (transform (car actions))
					 actions*))))))))))
      (lambda (vals actions)
	(open-block/make expression block variables vals actions false)))))

(define (transform/variable block environment expression)
  (reference/make expression
		  block
		  (environment/lookup environment
				      (variable-name expression))))

(define (transform/assignment block environment expression)
  (assignment-components expression
    (lambda (name value)
      (let ((variable (environment/lookup environment name)))
	(variable/side-effect! variable)
	(assignment/make expression
			 block
			 variable
			 (transform/expression block environment value))))))

(define (transform/lambda block environment expression)
  (lambda-components* expression
    (lambda (name required optional rest body)
      (let ((block (block/make block true '())))
	(call-with-values
	    (lambda ()
	      (let ((name->variable 
		     (lambda (name) (variable/make&bind! block name))))
		(values (map name->variable required)
			(map name->variable optional)
			(and rest (name->variable rest)))))
	  (lambda (required optional rest)
	    (let ((environment
		   (environment/bind environment
				     (block/bound-variables-list block))))
	      (procedure/make
	       expression block name required optional rest
	       (transform/procedure-body block
					 environment
					 body)))))))))

(define (transform/procedure-body block environment expression)
  (if (scode-open-block? expression)
      (open-block-components expression
	(lambda (auxiliary declarations body)
	  (if (null? auxiliary)
	      (begin (set-block/declarations!
		      block
		      (declarations/parse block declarations))
		     (transform/expression block environment body))
	      (transform/open-block block environment expression))))
      (transform/expression block environment expression)))

(define (transform/definition block environment expression)
  (definition-components expression
    (lambda (name value)
      (if (not (eq? block top-level-block))
	  (error "Unscanned definition encountered (unable to proceed):" name))
      (transform/combination*
       expression block environment
       (make-combination (make-primitive-procedure 'LOCAL-ASSIGNMENT)
			 (list (make-the-environment) name value))))))

(define (transform/access block environment expression)
  (access-components expression
    (lambda (environment* name)
      (access/make expression
		   (transform/expression block environment environment*)
		   name))))

(define (transform/combination block environment expression)
  (transform/combination* expression block environment expression))

(define (transform/combination* expression block environment expression*)
  (combination-components expression*
    (lambda (operator operands)
      (combination/make expression
			block
			(transform/expression block environment operator)
			(transform/expressions block environment operands)))))

(define (transform/comment block environment expression)
  (transform/expression block environment (comment-expression expression)))

(define (transform/conditional block environment expression)
  (conditional-components expression
    (lambda (predicate consequent alternative)
      (conditional/make
       expression
       (transform/expression block environment predicate)
       (transform/expression block environment consequent)
       (transform/expression block environment alternative)))))

(define (transform/constant block environment expression)
  block environment ; ignored
  (constant/make expression expression))

(define (transform/declaration block environment expression)
  (declaration-components expression
    (lambda (declarations expression*)
      (declaration/make expression
			(declarations/parse block declarations)
			(transform/expression block environment
					      expression*)))))

(define (transform/delay block environment expression)
  (delay/make
   expression
   (transform/expression block environment (delay-expression expression))))

(define (transform/disjunction block environment expression)
  (disjunction-components expression
    (lambda (predicate alternative)
      (disjunction/make
       expression
       (transform/expression block environment predicate)
       (transform/expression block environment alternative)))))

(define (transform/quotation block environment expression)
  block environment			;ignored
  (transform/quotation* expression (quotation-expression expression)))

(define (transform/quotation* expression expression*)
  (call-with-values (lambda () (transform/top-level expression* '()))
    (lambda (block expression**)
      (quotation/make expression block expression**))))

(define (transform/sequence block environment expression)
  (sequence/make
   expression
   (transform/expressions block environment (sequence-actions expression))))

(define (transform/the-environment block environment expression)
  environment ; ignored
  (block/unsafe! block)
  (the-environment/make expression block))

(define transform/dispatch
  (make-scode-walker
   transform/constant
   `((ACCESS ,transform/access)
     (ASSIGNMENT ,transform/assignment)
     (COMBINATION ,transform/combination)
     (COMMENT ,transform/comment)
     (CONDITIONAL ,transform/conditional)
     (DECLARATION ,transform/declaration)
     (DEFINITION ,transform/definition)
     (DELAY ,transform/delay)
     (DISJUNCTION ,transform/disjunction)
     (LAMBDA ,transform/lambda)
     (OPEN-BLOCK ,transform/open-block)
     (QUOTATION ,transform/quotation)
     (SEQUENCE ,transform/sequence)
     (THE-ENVIRONMENT ,transform/the-environment)
     (VARIABLE ,transform/variable))))