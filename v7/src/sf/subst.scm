#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/sf/subst.scm,v 3.6 1987/07/08 04:43:11 jinx Rel $

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

;;;; SCode Optimizer: Beta Substitution

(declare (usual-integrations))

(define *top-level-block*)

(define (integrate/get-top-level-block)
  *top-level-block*)

(define (integrate/top-level block expression)
  (fluid-let ((*top-level-block* block))
    (let ((operations (operations/bind-block (operations/make) block))
	  (environment (environment/make)))
      (if (open-block? expression)
	  (transmit-values
	   (environment/recursive-bind operations environment
				       (open-block/variables expression)
				       (open-block/values expression))
	   (lambda (environment values)
	     (return-3 operations
		       environment
		       (quotation/make block
				       (integrate/open-block operations
							     environment
							     expression
							     values)))))
	  (return-3 operations
		    environment
		    (quotation/make block
				    (integrate/expression operations
							  environment
							  expression)))))))

(define (operations/bind-block operations block)
  (let ((declarations (block/declarations block)))
    (if (null? declarations)
	(operations/shadow operations (block/bound-variables block))
	(transmit-values (declarations/binders declarations)
	  (lambda (before-bindings after-bindings)
	    (after-bindings
	     (operations/shadow (before-bindings operations)
				(block/bound-variables block))))))))

(define (integrate/expressions operations environment expressions)
  (map (lambda (expression)
	 (integrate/expression operations environment expression))
       expressions))

(define (integrate/expression operations environment expression)
  ((expression/method dispatch-vector expression)
   operations environment expression))

(define dispatch-vector
  (expression/make-dispatch-vector))

(define define-method/integrate
  (expression/make-method-definer dispatch-vector))

;;;; Lookup

(define-method/integrate 'REFERENCE
  (lambda (operations environment expression)
    (operations/lookup operations (reference/variable expression)
      (lambda (operation info)
	(case operation
	  ((INTEGRATE-OPERATOR EXPAND) expression)
	  ((INTEGRATE)
	   (integrate/name expression info environment
	     identity-procedure
	     (lambda () expression)))
	  (else (error "Unknown operation" operation))))
      (lambda () expression))))

(define (integrate/reference-operator operations environment operator operands)
  (let ((dont-integrate
	 (lambda ()
	   (combination/make operator operands))))
    (operations/lookup operations (reference/variable operator)
      (lambda (operation info)
	(case operation
	  ((#F) (dont-integrate))
	  ((INTEGRATE INTEGRATE-OPERATOR)
	   (integrate/name operator info environment
	     (lambda (operator)
	       (integrate/combination operations environment operator
				      operands))
	     dont-integrate))
	  ((EXPAND)
	   (info operands
		 (lambda (new-expression)
		   (integrate/expression operations environment new-expression))
		 dont-integrate
		 (reference/block operator)))
	  (else (error "Unknown operation" operation))))
      dont-integrate)))

(define-method/integrate 'ASSIGNMENT
  (lambda (operations environment assignment)
    (let ((variable (assignment/variable assignment)))
      (operations/lookup operations variable
	(lambda (operation info)
	  (case operation
	    ((INTEGRATE INTEGRATE-OPERATOR EXPAND)
	     (warn "Attempt to assign integrated name"
		   (variable/name variable)))
	    (else (error "Unknown operation" operation))))
	(lambda () 'DONE))
      (assignment/make (assignment/block assignment)
		       variable
		       (integrate/expression operations
					     environment
					     (assignment/value assignment))))))

;;;; Binding

(define-method/integrate 'OPEN-BLOCK
  (lambda (operations environment expression)
    (let ((operations
	   (operations/bind-block operations (open-block/block expression))))
      (transmit-values
	  (environment/recursive-bind operations
				      environment
				      (open-block/variables expression)
				      (open-block/values expression))
	(lambda (environment values)
	  (integrate/open-block operations
				environment
				expression
				values))))))

(define (integrate/open-block operations environment expression values)
  (open-block/make (open-block/block expression)
		   (open-block/variables expression)
		   values
		   (map (lambda (action)
			  (if (eq? action open-block/value-marker)
			      action
			      (integrate/expression operations
						    environment
						    action)))
			(open-block/actions expression))))

(define (integrate/procedure operations environment procedure)
  (let ((block (procedure/block procedure)))
    (procedure/make block
		    (procedure/name procedure)
		    (procedure/required procedure)
		    (procedure/optional procedure)
		    (procedure/rest procedure)
		    (integrate/expression (operations/bind-block operations
								 block)
					  environment
					  (procedure/body procedure)))))

(define-method/integrate 'PROCEDURE
  integrate/procedure)

(define-method/integrate 'COMBINATION
  (lambda (operations environment combination)
    (integrate/combination
     operations
     environment
     (combination/operator combination)
     (integrate/expressions operations
			    environment
			    (combination/operands combination)))))

(define (integrate/combination operations environment operator operands)
  (cond ((reference? operator)
	 (integrate/reference-operator operations
				       environment
				       operator
				       operands))
	((and (access? operator)
	      (system-global-environment? (access/environment operator)))
	 (integrate/access-operator operations environment operator operands))
	(else
	 (combination/optimizing-make
	  (if (procedure? operator)
	      (integrate/procedure-operator operations
					    environment
					    operator
					    operands)
	      (let ((operator
		     (integrate/expression operations environment operator)))
		(if (procedure? operator)
		    (integrate/procedure-operator operations
						  environment
						  operator
						  operands)
		    operator)))
	  operands))))

(define (integrate/procedure-operator operations environment procedure
				      operands)
  (integrate/procedure operations
		       (simulate-application environment procedure operands)
		       procedure))

(define-method/integrate 'DECLARATION
  (lambda (operations environment declaration)
    (let ((declarations (declaration/declarations declaration)))
      (declaration/make
       declarations
       (transmit-values (declarations/binders declarations)
	 (lambda (before-bindings after-bindings)
	   (integrate/expression (after-bindings (before-bindings operations))
				 environment
				 (declaration/expression declaration))))))))

;;;; Easy Cases

(define-method/integrate 'CONSTANT
  (lambda (operations environment expression)
    expression))

(define-method/integrate 'THE-ENVIRONMENT
  (lambda (operations environment expression)
    expression))

(define-method/integrate 'QUOTATION
  (lambda (operations environment expression)
    (integrate/quotation expression)))

(define-method/integrate 'CONDITIONAL
  (lambda (operations environment expression)
    (conditional/make
     (integrate/expression operations environment
			   (conditional/predicate expression))
     (integrate/expression operations environment
			   (conditional/consequent expression))
     (integrate/expression operations environment
			   (conditional/alternative expression)))))

(define-method/integrate 'DISJUNCTION
  (lambda (operations environment expression)
    (disjunction/make
     (integrate/expression operations environment
			   (disjunction/predicate expression))
     (integrate/expression operations environment
			   (disjunction/alternative expression)))))

(define-method/integrate 'SEQUENCE
  (lambda (operations environment expression)
    (sequence/make
     (integrate/expressions operations environment
			    (sequence/actions expression)))))

(define-method/integrate 'ACCESS
  (lambda (operations environment expression)
    (let ((environment* (access/environment expression))
	  (name (access/name expression)))
      (if (system-global-environment? environment*)
	  (let ((entry (assq name usual-integrations/constant-alist)))
	    (if entry
		(cdr entry)
		(access/make environment* name)))
	  (access/make (integrate/expression operations environment
					     environment*)
		       name)))))

(define (integrate/access-operator operations environment operator operands)
  (let ((name (access/name operator))
	(dont-integrate
	 (lambda ()
	   (combination/make operator operands))))
    (let ((entry (assq name usual-integrations/constant-alist)))
      (if entry
	  (integrate/combination operations environment (cdr entry) operands)
	  (let ((entry (assq name usual-integrations/expansion-alist)))
	    (if entry
		((cdr entry) operands identity-procedure
			     dont-integrate false)
		(dont-integrate)))))))

(define (system-global-environment? expression)
  (and (constant? expression)
       (eq? false (constant/value expression))))

(define-method/integrate 'DELAY
  (lambda (operations environment expression)
    (delay/make
     (integrate/expression operations environment
			   (delay/expression expression)))))

(define-method/integrate 'IN-PACKAGE
  (lambda (operations environment expression)
    (in-package/make (integrate/expression operations environment
					   (in-package/environment expression))
		     (integrate/quotation (in-package/quotation expression)))))

(define (integrate/quotation quotation)
  (transmit-values (integrate/top-level (quotation/block quotation)
					(quotation/expression quotation))
    (lambda (operations environment expression)
      expression)))

;;;; Environment

(define (environment/recursive-bind operations environment variables values)
  ;; Used to implement mutually-recursive definitions that can
  ;; integrate one another.  When circularities are detected within
  ;; the definition-reference graph, integration is disabled.
  (let ((values
	 (map (lambda (value)
		(delayed-integration/make operations value))
	      values)))
    (let ((environment
	   (environment/bind-multiple environment variables values)))
      (for-each (lambda (value)
		  (delayed-integration/set-environment! value environment))
		values)
      (return-2 environment
		(map delayed-integration/force values)))))

(define (integrate/name reference info environment if-integrated if-not)
  (let ((variable (reference/variable reference)))
    (let ((finish
	   (lambda (value uninterned)
	     (if-integrated
	      (copy/expression (reference/block reference) value
			       uninterned)))))
      (if info
	  (transmit-values info finish)
	  (environment/lookup environment variable
	    (lambda (value)
	      (if (delayed-integration? value)
		  (if (delayed-integration/in-progress? value)
		      (if-not)
		      (finish (delayed-integration/force value) '()))
		  (finish value '())))
	    if-not)))))

(define (variable/final-value variable environment if-value if-not)
  (environment/lookup environment variable
    (lambda (value)
      (if (delayed-integration? value)
	  (if (delayed-integration/in-progress? value)
	      (error "Unfinished integration" value)
	      (if-value (delayed-integration/force value)))
	  (if-value value)))
    (lambda ()
      (warn "Unable to integrate" (variable/name variable))
      (if-not))))

(define (simulate-application environment procedure operands)

  (define (match-required environment required operands)
    (cond ((null? required)
	   (match-optional environment
			   (procedure/optional procedure)
			   operands))
	  ((null? operands)
	   (error "Too few operands in call to procedure" procedure))
	  (else
	   (match-required (environment/bind environment
					     (car required)
					     (car operands))
			   (cdr required)
			   (cdr operands)))))

  (define (match-optional environment optional operands)
    (cond ((null? optional)
	   (match-rest environment (procedure/rest procedure) operands))
	  ((null? operands)
	   (match-rest environment (procedure/rest procedure) '()))
	  (else
	   (match-optional (environment/bind environment
					     (car optional)
					     (car operands))
			   (cdr optional)
			   (cdr operands)))))

  (define (match-rest environment rest operands)
    (cond (rest
	   ;; Other cases are too hairy -- don't bother.
	   (if (null? operands)
	       (environment/bind environment rest (constant/make '()))
	       environment))
	  ((null? operands)
	   environment)
	  (else
	   (error "Too many operands in call to procedure" procedure))))

  (match-required environment (procedure/required procedure) operands))

(define (environment/make)
  '())

(define (environment/bind environment variable value)
  (cons (cons variable value) environment))

(define (environment/bind-multiple environment variables values)
  (map* environment cons variables values))

(define (environment/lookup environment variable if-found if-not)
  (let ((association (assq variable environment)))
    (if association
	(if-found (cdr association))
	(if-not))))

(define (delayed-integration/in-progress? delayed-integration)
  (eq? (delayed-integration/state delayed-integration) 'BEING-INTEGRATED))

(define (delayed-integration/force delayed-integration)
  (case (delayed-integration/state delayed-integration)
    ((NOT-INTEGRATED)
     (let ((value
	    (let ((environment
		   (delayed-integration/environment delayed-integration))
		  (operations
		   (delayed-integration/operations delayed-integration))
		  (expression (delayed-integration/value delayed-integration)))
	      (delayed-integration/set-state! delayed-integration
					      'BEING-INTEGRATED)
	      (delayed-integration/set-environment! delayed-integration false)
	      (delayed-integration/set-operations! delayed-integration false)
	      (delayed-integration/set-value! delayed-integration false)
	      (integrate/expression operations environment expression))))
       (delayed-integration/set-state! delayed-integration 'INTEGRATED)
       (delayed-integration/set-value! delayed-integration value)))
    ((INTEGRATED) 'DONE)
    ((BEING-INTEGRATED)
     (error "Attempt to re-force delayed integration" delayed-integration))
    (else
     (error "Delayed integration has unknown state" delayed-integration)))
  (delayed-integration/value delayed-integration))

;;;; Optimizations

(define combination/optimizing-make)
(let ()

(set! combination/optimizing-make
  (lambda (operator operands)
    (if (and (procedure? operator)
	     (null? (procedure/optional operator))
	     (not (procedure/rest operator))
	     (block/safe? (procedure/block operator))
	     (not (open-block? (procedure/body operator))))
	;; Simple LET-like combination.  Delete any unreferenced
	;; parameters.  If no parameters remain, delete the
	;; combination and lambda.
	(transmit-values ((delete-integrated-parameters
			   (declarations/integrated-variables
			    (block/declarations (procedure/block operator))))
			  (procedure/required operator)
			  operands)
	  (lambda (required operands)
	    (if (null? required)
		(procedure/body operator)
		(combination/make (procedure/make (procedure/block operator)
						  (procedure/name operator)
						  required
						  '()
						  false
						  (procedure/body operator))
				  operands))))
	(combination/make operator operands))))

(define (delete-integrated-parameters integrated)
  (define (loop parameters operands)
    (if (null? parameters)
	(return-2 '() operands)
	(let ((rest (loop (cdr parameters) (cdr operands))))
	  (if (memq (car parameters) integrated)
	      rest
	      (transmit-values rest
		(lambda (parameters* operands*)
		  (return-2 (cons (car parameters) parameters*)
			    (cons (car operands) operands*))))))))
  loop)

;;; end COMBINATION/OPTIMIZING-MAKE
)

#| This is too much of a pain to do now.  Maybe later.

(define procedure/optimizing-make)
(let ()

(set! procedure/optimizing-make
  (lambda (block name required optional rest auxiliary body)
    (if (and (not (null? auxiliary))
	     optimize-open-blocks?
	     (block/safe? block))
	(let ((used
	       (used-auxiliaries (list-transform-positive auxiliary
				   variable-value)
				 (free/expression body))))
	  (procedure/make block name required optional rest used
			  (delete-unused-definitions used body)))
	(procedure/make block name required optional rest auxiliary body))))

(define (delete-unused-definitions used body)
  ???)

;;; A non-obvious program: (1) Collect all of the free references to
;;; the block's bound variables which occur in the body of the block.
;;; (2) Examine each of the values associated with that set of free
;;; references, and add any new free references to the collection.
;;; (3) Continue looping until no more free references are added.

(define (used-auxiliaries auxiliary initial-used)
  (let ((used (eq?-set/intersection auxiliary initial-used)))
    (if (null? used)
	'()
	(let loop ((previous-used used) (new-used used))
	  (for-each (lambda (value)
		      (for-each (lambda (variable)
				  (if (and (memq variable auxiliary)
					   (not (memq variable used)))
				      (set! used (cons variable used))))
				(free/expression value)))
		    (map variable/value new-used))
	  (let ((diffs
		 (let note-diffs ((used used))
		   (if (eq? used previous-used)
		       '()
		       (cons (cdar used)
			     (note-diffs (cdr used)))))))
	    (if (null? diffs)
		used
		(loop used diffs)))))))

;;; end PROCEDURE/OPTIMIZING-MAKE
)
|#