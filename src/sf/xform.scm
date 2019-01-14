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

(define (transform/r7rs-library imports expression)
  (let ((block (block/make #f #f '())))
    (for-each (lambda (import)
		(variable/make&bind! block (library-import-to import)))
	      imports)
    (set-block/declarations! block (r7rs-usual-integrations block imports))
    (values block
	    (transform/top-level-1 'r7rs
				   block
				   (block/make block #f '())
				   expression))))

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
				 (block/bound-variables block))
	       (environment/make))))
      (if (scode-open-block? expression)
	  (begin
	    (if (not top-level?)
		(error "Open blocks allowed only at top level:" expression))
	    (let ((declarations (scode-open-block-declarations expression)))
	      (if (not (or (eq? tl? 'r7rs)
			   (assq 'usual-integrations declarations)))
		  (ui-warning))
	      (transform/open-block* expression
				     block
				     environment
				     (scode-open-block-names expression)
				     declarations
				     (scode-open-block-actions expression))))
	  (transform/expression block environment expression)))))

(define (ui-warning)
  (for-each
   (lambda (line)
     (write-notification-line
      (lambda (port)
	(write-string line port))))
   '("This program does not have a USUAL-INTEGRATIONS declaration."
     "Without this declaration, the compiler will be unable to perform"
     "many optimizations, and as a result the compiled program will be"
     "slower and perhaps larger than it could be.  Please read the MIT"
     "Scheme User's Guide for more information about USUAL-INTEGRATIONS.")))

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
  (transform/open-block* expression
			 (block/make block true '())
			 environment
			 (scode-open-block-names expression)
			 (scode-open-block-declarations expression)
			 (scode-open-block-actions expression)))

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
		   (actions (scode-sequence-actions body)))
		(cond ((null? variables)
		       (values '() (map transform actions)))
		      ((null? actions)
		       (error "Extraneous auxiliaries" variables))
		      ;; Because `scan-defines' returns the auxiliary
		      ;; names in a particular order, we can expect to
		      ;; encounter them in that same order when
		      ;; looking through the body's actions.
		      ((and (scode-assignment? (car actions))
			    (eq? (scode-assignment-name (car actions))
				 (variable/name (car variables))))
		       (call-with-values
			   (lambda () (loop (cdr variables) (cdr actions)))
			 (lambda (vals actions*)
			   (values
			    (cons (transform
				   (scode-assignment-value (car actions)))
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
	(open-block/make expression block variables vals actions)))))

(define (transform/variable block environment expression)
  (reference/make expression
		  block
		  (environment/lookup environment
				      (scode-variable-name expression))))

(define (transform/assignment block environment expression)
  (let ((name (scode-assignment-name expression))
	(value (scode-assignment-value expression)))
    (let ((variable (environment/lookup environment name)))
      (variable/side-effect! variable)
      (assignment/make expression
		       block
		       variable
		       (transform/expression block environment value)))))

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
				     (block/bound-variables block))))
	      (build-procedure expression block name required optional rest
			       (transform/procedure-body block environment
							 body)))))))))

;; If procedure body is a sequence, scan the first elements and turn variable
;; references into IGNORE declarations.
(define (build-procedure expression block name required optional rest body)
  (if (scode-sequence? body)
      (do ((actions (sequence/actions body) (cdr actions))
	   (ignores '()
		    (cons (variable/name (reference/variable (car actions)))
			  ignores)))
	  ((or (null? (cdr actions))
	       (not (reference? (car actions))))
	   (let ((final-body (if (null? (cdr actions))
				 (car actions)
				 (sequence/make (object/scode body) actions))))
	     (procedure/make
	      expression block name required optional rest
	      (if (null? ignores)
		  final-body
		  (declaration/make #f
				    (declarations/parse block
							`((ignore ,@ignores)))
				    final-body))))))
      (procedure/make expression block name required optional rest body)))

(define (transform/procedure-body block environment expression)
  (if (scode-open-block? expression)
      (if (null? (scode-open-block-names expression))
	  (begin
	    (set-block/declarations!
	     block
	     (declarations/parse block
				 (scode-open-block-declarations expression)))
	    (transform/expression block
				  environment
				  (scode-open-block-actions expression)))
	  (transform/open-block block environment expression))
      (transform/expression block environment expression)))

(define (transform/definition block environment expression)
  (let ((name (scode-definition-name expression))
	(value (scode-definition-value expression)))
    (if (not (eq? block top-level-block))
	(error "Unscanned definition encountered (unable to proceed):" name))
    (transform/combination*
     expression block environment
     (make-scode-combination
      (make-primitive-procedure 'local-assignment)
      (list (make-scode-the-environment) name value)))))

(define (transform/access block environment expression)
  (access/make expression
	       block
	       (transform/expression block
				     environment
				     (scode-access-environment expression))
	       (scode-access-name expression)))

(define (transform/combination block environment expression)
  (transform/combination* expression block environment expression))

(define (transform/combination* expression block environment expression*)
  (let ((operator (scode-combination-operator expression*))
	(operands (scode-combination-operands expression*)))
    (combination/%make expression
		       block
		       (transform/expression block environment operator)
		       (transform/expressions block environment operands))))

(define (transform/comment block environment expression)
  (transform/expression block environment
			(scode-comment-expression expression)))

(define (transform/conditional block environment expression)
  (let ((predicate (scode-conditional-predicate expression))
	(consequent (scode-conditional-consequent expression))
	(alternative (scode-conditional-alternative expression)))
    (conditional/make
     expression
     (transform/expression block environment predicate)
     (transform/expression block environment consequent)
     (transform/expression block environment alternative))))

(define (transform/constant block environment expression)
  block environment ; ignored
  (constant/make expression expression))

(define (transform/declaration block environment expression)
  (declaration/make
   expression
   (declarations/parse block (scode-declaration-text expression))
   (transform/expression block environment
			 (scode-declaration-expression expression))))

(define (transform/delay block environment expression)
  (delay/make
   expression
   (transform/expression block environment
			 (scode-delay-expression expression))))

(define (transform/disjunction block environment expression)
  (disjunction/make
   expression
   (transform/expression block environment
			 (scode-disjunction-predicate expression))
   (transform/expression block environment
			 (scode-disjunction-alternative expression))))

(define (transform/quotation block environment expression)
  block environment			;ignored
  (transform/quotation* expression (scode-quotation-expression expression)))

(define (transform/quotation* expression expression*)
  (call-with-values (lambda () (transform/top-level expression* '()))
    (lambda (block expression**)
      (quotation/make expression block expression**))))

(define (transform/sequence block environment expression)
  ;; Don't remove references from sequences here.  We want them
  ;; to signal ignored variables.
  (sequence/%make
   expression
   (transform/expressions block environment
			  (scode-sequence-actions expression))))

(define (transform/the-environment block environment expression)
  environment ; ignored
  (block/unsafe! block)
  (the-environment/make expression block))

(define transform/dispatch
  (make-scode-walker
   transform/constant
   `((access ,transform/access)
     (assignment ,transform/assignment)
     (combination ,transform/combination)
     (comment ,transform/comment)
     (conditional ,transform/conditional)
     (declaration ,transform/declaration)
     (definition ,transform/definition)
     (delay ,transform/delay)
     (disjunction ,transform/disjunction)
     (lambda ,transform/lambda)
     (open-block ,transform/open-block)
     (quotation ,transform/quotation)
     (sequence ,transform/sequence)
     (the-environment ,transform/the-environment)
     (variable ,transform/variable))))