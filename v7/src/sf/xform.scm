#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/sf/xform.scm,v 4.3 1990/06/11 16:34:51 jinx Rel $

Copyright (c) 1988, 1990 Massachusetts Institute of Technology

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

;;;; SCode Optimizer: Transform Input Expression

(declare (usual-integrations)
	 (eta-substitution)
	 (automagic-integrations)
	 (open-block-optimizations)
	 (integrate-external "object"))

;;; GLOBAL-BLOCK is used to handle (USUAL-INTEGRATIONS), as follows.
;;; This declaration refers to a large group of names, which are
;;; normally defined in the global environment.  Names in this group
;;; are supposed to be shadowed by top-level definitions in the user's
;;; program.

;;; Normally we would intern the variable objects corresponding to
;;; those names in the block corresponding to the outermost
;;; environment in the user's program.  However, if the user had a
;;; top-level definition which was intended to shadow one of those
;;; names, both the definition and the declaration would refer to the
;;; same variable object.  So, instead we intern them in GLOBAL-BLOCK,
;;; which never has any user defined names in it.

(define (transform/top-level expression shadowed-names)
  (let ((block (block/make (block/make false false) false)))
    (set-block/bound-variables!
     block
     (map (lambda (name) (variable/make block name '())) shadowed-names))
    (values block (transform/top-level-1 true block block expression))))

(define (transform/recursive block top-level-block expression)
  (transform/top-level-1 false block top-level-block expression))

(define top-level?)
(define global-block)

(define (transform/top-level-1 top? block top-level-block expression)
  (fluid-let ((top-level? top?)
	      (global-block
	       (let block/global-parent ((block top-level-block))
		 (if (block/parent block)
		     (block/global-parent (block/parent block))
		     block))))
    (let ((environment
	   (if top-level?
	       (environment/bind (environment/make)
				 (block/bound-variables block))
	       (environment/make))))
      (if (scode-open-block? expression)
	  (begin
	    (if (not top-level?)
		(error "TRANSFORM/TOP-LEVEL-1: open blocks disallowed"
		       expression))
	    (open-block-components expression
	      (transform/open-block* block environment)))
	  (transform/expression block environment expression)))))

(define (transform/expressions block environment expressions)
  (map (lambda (expression)
	 (transform/expression block environment expression))
       expressions))

(declare (integrate-operator transform/expression))

(define (transform/expression block environment expression)
  ((scode-walk transform/dispatch expression) block environment expression))

(define (environment/make)
  '())

(define (environment/lookup block environment name)
  (let ((association (assq name environment)))
    (if association
	(cdr association)
	(or (and (not top-level?)
		 (block/lookup-name block name false))
	    (block/lookup-name global-block name true)))))

(define (environment/bind environment variables)
  (map* environment
	(lambda (variable)
	  (cons (variable/name variable) variable))
	variables))

(define (transform/open-block block environment expression)
  (open-block-components expression
    (transform/open-block* (block/make block true) environment)))

(define ((transform/open-block* block environment) auxiliary declarations body)
  (let ((variables (map (lambda (name) (variable/make block name '()))
			auxiliary)))
    (set-block/bound-variables! block
				(append (block/bound-variables block)
					variables))
    (set-block/declarations! block (declarations/parse block declarations))
    (let ((environment (environment/bind environment variables)))

      (define (loop variables actions)
	(cond ((null? variables)
	       (values '() (map transform actions)))
	      ((null? actions)
	       (error "Extraneous auxiliaries" variables))

	      ;; Because `scan-defines' returns the auxiliary names in a
	      ;; particular order, we can expect to encounter them in that
	      ;; same order when looking through the body's actions.

	      ((and (scode-assignment? (car actions))
		    (eq? (assignment-name (car actions))
			 (variable/name (car variables))))
	       (with-values (lambda () (loop (cdr variables) (cdr actions)))
		 (lambda (vals actions*)
		   (values
		    (cons (transform (assignment-value (car actions))) vals)
		    (cons open-block/value-marker actions*)))))
	      (else
	       (with-values (lambda () (loop variables (cdr actions)))
		 (lambda (vals actions*)
		   (values vals (cons (transform (car actions)) actions*)))))))

      (define-integrable (transform subexpression)
	(transform/expression block environment subexpression))

      (with-values (lambda () (loop variables (sequence-actions body)))
	(lambda (vals actions)
	  (open-block/make block variables vals actions false))))))

(define (transform/variable block environment expression)
  (reference/make block
		  (environment/lookup block
				      environment
				      (variable-name expression))))

(define (transform/assignment block environment expression)
  (assignment-components expression
    (lambda (name value)
      (let ((variable (environment/lookup block environment name)))
	(variable/side-effect! variable)
	(assignment/make block
			 variable
			 (transform/expression block environment value))))))

(define (transform/lambda block environment expression)
  (lambda-components* expression
    (lambda (name required optional rest body)
      (let ((block (block/make block true)))
	(with-values
	    (lambda ()
	      (let ((name->variable 
		     (lambda (name) (variable/make block name '()))))
		(values (map name->variable required)
			(map name->variable optional)
			(and rest (name->variable rest)))))
	  (lambda (required optional rest)
	    (let* ((bound `(,@required ,@optional ,@(if rest `(,rest) '())))
		   (environment (environment/bind environment bound)))
	      (set-block/bound-variables! block bound)
	      (procedure/make
	       block name required optional rest
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

#|
;; In-package no longer scans the body, so definitions at top-level are legal.

(define (transform/definition block environment expression)
  block environment ; ignored
  (definition-components expression
    (lambda (name value)
      value ; ignored
      (error "Unscanned definition encountered.  Unable to proceed." name))))
|#

(define (transform/definition block environment expression)
  (definition-components expression
    (lambda (name value)
      (if (not (top-level-block? block))
	  (error "Unscanned definition encountered.  Unable to proceed." name)
	  (transform/combination
	   block environment
	   (make-combination
	    (make-primitive-procedure 'local-assignment)
	    (list (make-the-environment)
		  name
		  value)))))))

;; Kludge!

(define (top-level-block? block)
  (let ((parent (block/parent block)))
    (and parent (eq? parent global-block))))

(define (transform/access block environment expression)
  (access-components expression
    (lambda (environment* name)
      (access/make (transform/expression block environment environment*)
		   name))))

(define (transform/combination block environment expression)
  (combination-components expression
    (lambda (operator operands)
      (combination/make (transform/expression block environment operator)
			(transform/expressions block environment operands)))))

(define (transform/comment block environment expression)
  (transform/expression block environment (comment-expression expression)))

(define (transform/conditional block environment expression)
  (conditional-components expression
    (lambda (predicate consequent alternative)
      (conditional/make
       (transform/expression block environment predicate)
       (transform/expression block environment consequent)
       (transform/expression block environment alternative)))))

(define (transform/constant block environment expression)
  block environment ; ignored
  (constant/make expression))

(define (transform/declaration block environment expression)
  (declaration-components expression
    (lambda (declarations expression)
      (declaration/make (declarations/parse block declarations)
			(transform/expression block environment expression)))))

(define (transform/delay block environment expression)
  (delay/make
   (transform/expression block environment (delay-expression expression))))

(define (transform/disjunction block environment expression)
  (disjunction-components expression
    (lambda (predicate alternative)
      (disjunction/make
       (transform/expression block environment predicate)
       (transform/expression block environment alternative)))))

(define (transform/error-combination block environment expression)
  (combination-components expression
    (lambda (operator operands)
      (combination/make
       (transform/expression block environment operator)
       (list (transform/expression block environment (car operands))
	     (transform/expression block environment (cadr operands))
	     (the-environment/make block))))))

(define (transform/in-package block environment expression)
  (in-package-components expression
    (lambda (environment* expression)
      (in-package/make (transform/expression block environment environment*)
		       (transform/quotation* expression)))))

(define (transform/quotation block environment expression)
  block environment			;ignored
  (transform/quotation* (quotation-expression expression)))

(define (transform/quotation* expression)
  (with-values (lambda () (transform/top-level expression '()))
    quotation/make))

(define (transform/sequence block environment expression)
  (sequence/make
   (transform/expressions block environment (sequence-actions expression))))

(define (transform/the-environment block environment expression)
  environment expression ; ignored
  (block/unsafe! block)
  (the-environment/make block))

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
     (ERROR-COMBINATION ,transform/error-combination)
     (IN-PACKAGE ,transform/in-package)
     (LAMBDA ,transform/lambda)
     (OPEN-BLOCK ,transform/open-block)
     (QUOTATION ,transform/quotation)
     (SEQUENCE ,transform/sequence)
     (THE-ENVIRONMENT ,transform/the-environment)
     (VARIABLE ,transform/variable))))