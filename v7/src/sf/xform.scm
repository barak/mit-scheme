#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/sf/xform.scm,v 3.0 1987/03/10 13:25:33 cph Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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

(declare (usual-integrations))

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

(define (transform/top-level expression)
  (let ((block (block/make (block/make false false) false)))
    (return-2 block (transform/top-level-1 block expression))))

(define (transform/top-level-1 block expression)
  (fluid-let ((global-block
	       (let block/global-parent ((block block))
		 (if (block/parent block)
		     (block/global-parent (block/parent block))
		     block))))
    (let ((environment (environment/make)))
      (if (scode-open-block? expression)
	  (open-block-components expression
	    (transform/open-block* block environment))
	  (transform/expression block environment expression)))))

(define (transform/expressions block environment expressions)
  (map (lambda (expression)
	 (transform/expression block environment expression))
       expressions))

(define (transform/expression block environment expression)
  ((transform/dispatch expression) block environment expression))

(define global-block)

(define (environment/make)
  '())

(define (environment/lookup environment name)
  (let ((association (assq name environment)))
    (if association
	(cdr association)
	(block/lookup-name global-block name))))

(define (environment/bind environment variables)
  (map* environment
	(lambda (variable)
	  (cons (variable/name variable) variable))
	variables))

(define (transform/open-block block environment expression)
  (open-block-components expression
    (transform/open-block* (block/make block true) environment)))

(define ((transform/open-block* block environment) auxiliary declarations body)
  (let ((variables (map (lambda (name) (variable/make block name)) auxiliary)))
    (block/set-bound-variables! block variables)
    (block/set-declarations! block (declarations/parse block declarations))
    (let ((environment (environment/bind environment variables)))

      (define (loop variables actions)
	(cond ((null? variables)
	       (return-2 '() (map transform actions)))
	      ((null? actions)
	       (error "Extraneous auxiliaries" variables))

	      ;; Because `scan-defines' returns the auxiliary names in a
	      ;; particular order, we can expect to encounter them in that
	      ;; same order when looking through the body's actions.

	      ((and (scode-assignment? (car actions))
		    (eq? (assignment-name (car actions))
			 (variable/name (car variables))))
	       (transmit-values (loop (cdr variables) (cdr actions))
		 (lambda (values actions*)
		   (return-2
		    (cons (transform (assignment-value (car actions))) values)
		    (cons open-block/value-marker actions*)))))
	      (else
	       (transmit-values (loop variables (cdr actions))
		 (lambda (values actions*)
		   (return-2 values
			     (cons (transform (car actions)) actions*)))))))

      (define (transform subexpression)
	(transform/expression block environment subexpression))

      (transmit-values (loop variables (sequence-actions body))
	(lambda (values actions)
	  (open-block/make block variables values actions))))))

(define (transform/variable block environment expression)
  (reference/make block
		  (environment/lookup environment (variable-name expression))))

(define (transform/assignment block environment expression)
  (assignment-components expression
    (lambda (name value)
      (assignment/make block
		       (environment/lookup environment name)
		       (transform/expression block environment value)))))

(define (transform/lambda block environment expression)
  (lambda-components* expression
    (lambda (name required optional rest body)
      (let ((block (block/make block true)))
	(transmit-values
	    (let ((name->variable (lambda (name) (variable/make block name))))
	      (return-4 (name->variable name)
			(map name->variable required)
			(map name->variable optional)
			(and rest (name->variable rest))))
	  (lambda (name required optional rest)
	    (let ((bound
		   `(,name ,@required ,@optional ,@(if rest `(,rest) '()))))
	      (block/set-bound-variables! block bound)
	      (procedure/make
	       block name required optional rest
	       (transform/procedure-body block
					 (environment/bind environment bound)
					 body)))))))))

(define (transform/procedure-body block environment expression)
  (if (scode-open-block? expression)
      (open-block-components expression
	(lambda (auxiliary declarations body)
	  (if (null? auxiliary)
	      (begin (block/set-declarations!
		      block
		      (declarations/parse block declarations))
		     (transform/expression block environment body))
	      (transform/open-block block environment expression))))
      (transform/expression block environment expression)))

(define (transform/definition block environment expression)
  (definition-components expression
    (lambda (name value)
      (error "Unscanned definition encountered.  Unable to proceed." name))))

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
  (transform/expression block (comment-expression environment expression)))

(define (transform/conditional block environment expression)
  (conditional-components expression
    (lambda (predicate consequent alternative)
      (conditional/make
       (transform/expression block environment predicate)
       (transform/expression block environment consequent)
       (transform/expression block environment alternative)))))

(define (transform/constant block environment expression)
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

(define (transform/in-package block environment expression)
  (in-package-components expression
    (lambda (environment* expression)
      (in-package/make (transform/expression block environment environment*)
		       (transform/quotation* expression)))))

(define (transform/quotation block environment expression)
  (transform/quotation* (quotation-expression expression)))

(define (transform/quotation* expression)
  (transmit-values (transform/top-level expression)
    quotation/make))

(define (transform/sequence block environment expression)
  (sequence/make
   (transform/expressions block environment (sequence-actions expression))))

(define (transform/the-environment block environment expression)
  (block/unsafe! block)
  (the-environment/make block))

(define transform/dispatch
  (make-type-dispatcher
   `((,access-type ,transform/access)
     (,assignment-type ,transform/assignment)
     (,combination-type ,transform/combination)
     (,comment-type ,transform/comment)
     (,conditional-type ,transform/conditional)
     (,declaration-type ,transform/declaration)
     (,definition-type ,transform/definition)
     (,delay-type ,transform/delay)
     (,disjunction-type ,transform/disjunction)
     (,in-package-type ,transform/in-package)
     (,lambda-type ,transform/lambda)
     (,open-block-type ,transform/open-block)
     (,quotation-type ,transform/quotation)
     (,sequence-type ,transform/sequence)
     (,the-environment-type ,transform/the-environment)
     (,variable-type ,transform/variable))
   transform/constant))