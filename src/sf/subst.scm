#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

;;;; SCode Optimizer: Beta Substitution
;;; package: (scode-optimizer integrate)

(declare (usual-integrations)
	 (integrate-external "object"))

(define *top-level-block*)

(define (integrate/get-top-level-block)
  *top-level-block*)

;;; Block names are added to this list so warnings can be more
;;; descriptive.
(define *current-block-names*)

(define (integrate/top-level block expression)
  (integrate/top-level* (object/scode expression) block expression))

(define (integrate/top-level* scode block expression)
  (fluid-let ((*top-level-block* block)
	      (*current-block-names* '()))
    (call-with-values
	(lambda ()
	  (let ((operations (operations/make))
		(environment (environment/make)))
	    (if (open-block? expression)
		(integrate/open-block operations environment expression)
		(let ((operations
		       (declarations/bind operations
					  (block/declarations block))))
		  (values operations
			  environment
			  (integrate/expression operations
						environment
						expression))))))
     (lambda (operations environment expression)
       (values operations environment
	       (quotation/make scode
			       block
			       expression))))))

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

;;;; Variables

(define-method/integrate 'ASSIGNMENT
  (lambda (operations environment assignment)
    (let ((variable (assignment/variable assignment)))
      (operations/lookup operations variable
	(lambda (operation info)
	  info				;ignore
	  (case operation
	    ((INTEGRATE INTEGRATE-OPERATOR EXPAND)
	     (warn "Attempt to assign integrated name"
		   (variable/name variable)))
	    (else (error "Unknown operation" operation))))
	(lambda () 'DONE))
      ;; The value of an assignment is the old value
      ;; of the variable, hence, it is refernced.
      (variable/reference! variable)
      (assignment/make (assignment/scode assignment)
		       (assignment/block assignment)
		       variable
		       (integrate/expression operations
					     environment
					     (assignment/value assignment))))))

(define-method/integrate 'REFERENCE
  (lambda (operations environment expression)
    (let ((variable (reference/variable expression)))
      (letrec ((integration-success
		(lambda (new-expression)
		  (variable/integrated! variable)
		  new-expression))
	       (integration-failure
		(lambda ()
		  (variable/reference! variable)
		  expression)))
	(operations/lookup operations variable
	 (lambda (operation info)
	   (case operation
	     ((INTEGRATE-OPERATOR EXPAND)
	      (variable/reference! variable)
	      expression)
	     ((INTEGRATE)
	      (let ((new-expression
		     (integrate/name expression expression info environment)))
		(if new-expression
		    (integration-success new-expression)
		    (integration-failure))))
	     (else
	      (error "Unknown operation" operation))))
	 (lambda ()
	   (integration-failure)))))))

(define (reassign expr object)
  (if (and expr (object/scode expr))
      (with-new-scode (object/scode expr) object)
      object))

(define (integrate/reference-operator expression operations environment
				      block operator operands)
  (let ((variable (reference/variable operator)))
    (letrec ((mark-integrated!
	      (lambda ()
		(variable/integrated! variable)))
	     (integration-failure
	      (lambda ()
		(variable/reference! variable)
		(combination/make expression block
					     operator operands)))
	     (integration-success
	      (lambda (operator)
		(mark-integrated!)
		(integrate/combination expression operations environment
				       block operator operands))))
      (operations/lookup operations variable
       (lambda (operation info)
	 (case operation
	   ((#F) (integration-failure))

	   ((EXPAND)
	    (let ((new-expression (info expression operands (reference/block operator))))
	      (if new-expression
		  (begin
		    (mark-integrated!)
		    (integrate/expression operations environment new-expression))
		  (integration-failure))))

	   ((INTEGRATE INTEGRATE-OPERATOR)
	    (let ((new-expression (integrate/name expression
			    operator info environment)))
	      (if new-expression
		  (integration-success new-expression)
		  (integration-failure))))

	   (else
	    (error "Unknown operation" operation))))
       (lambda ()
	 (integration-failure))))))

;;;; Binding

(define (integrate/open-block operations environment expression)
  (let ((variables (open-block/variables expression))
	(block (open-block/block expression)))
    (let ((operations
	   (declarations/bind (operations/shadow operations variables)
			      (block/declarations block))))
      (call-with-values
	  (lambda ()
	    (environment/recursive-bind operations
					environment
					variables
					(open-block/values expression)))
	(lambda (environment vals)
	  (let ((actions
		 (integrate/actions operations
				    environment
				    (open-block/actions expression))))
	    ;; Complain about unreferenced variables.
	    ;; If the block is unsafe, then it is likely that
	    ;; there will be a lot of them on purpose (top level or
	    ;; the-environment) so no complaining.
	    (if (block/safe? (open-block/block expression))
		(for-each (lambda (variable)
			    (if (variable/unreferenced? variable)
				(warn "Unreferenced defined variable:"
				      (variable/name variable))))
			  variables))
	    (values operations
		    environment
		    (open-block/make
		     (and expression (object/scode expression))
		     block variables
		     vals actions))))))))

(define-method/integrate 'OPEN-BLOCK
  (lambda (operations environment expression)
    (call-with-values
	(lambda () (integrate/open-block operations environment expression))
      (lambda (operations environment expression)
	operations environment
	expression))))

(define (variable/unreferenced? variable)
  (and (not (variable/integrated variable))
       (not (variable/referenced variable))
       (not (variable/may-ignore? variable))
       (not (variable/must-ignore? variable))))

(define-method/integrate 'PROCEDURE
  (lambda (operations environment procedure)
    (integrate/procedure operations
			 (simulate-unknown-application environment procedure)
			 procedure)))

;;; If not #f, display the top-level procedure names as they are
;;; processed.  Useful for debugging.
(define sf:display-top-level-procedure-names? #f)

(define (maybe-displaying-name name thunk)
  (if (and sf:display-top-level-procedure-names?
	   (null? *current-block-names*))
      (with-notification
       (lambda (port)
	 (write-string "Integrating procedure " port)
	 (write name port))
       thunk)
      (thunk)))

(define (integrate/procedure operations environment procedure)
  (let ((block (procedure/block procedure))
	(name  (procedure/name procedure))
	(required (procedure/required procedure))
	(optional (procedure/optional procedure))
	(rest (procedure/rest procedure)))
    (maybe-displaying-name
     name
     (lambda ()
       (fluid-let ((*current-block-names* (cons name *current-block-names*)))
	 (let ((body
		(integrate/expression
		 (declarations/bind
		  (operations/shadow
		   operations
		   (append required optional (if rest (list rest) '())))
		  (block/declarations block))
		 environment
		 (procedure/body procedure))))
	   ;; Possibly complain about variables bound and not
	   ;; referenced.
	   (if (block/safe? block)
	       (for-each (lambda (variable)
			   (if (variable/unreferenced? variable)
			       (warn "Unreferenced bound variable:"
				     (variable/name variable)
				     *current-block-names*)))
			 (if rest
			     (append required optional (list rest))
			     (append required optional))))
	   (procedure/make (procedure/scode procedure)
			   block
			   name
			   required
			   optional
			   rest
			   body)))))))

(define-method/integrate 'COMBINATION
  (lambda (operations environment combination)
    (integrate/combination
     combination operations environment
     (combination/block combination)
     (combination/operator combination)
     (integrate/expressions operations
			    environment
			    (combination/operands combination)))))

(define (integrate/combination expression operations environment
			       block operator operands)
  (cond ((reference? operator)
	 (integrate/reference-operator expression operations environment
				       block operator operands))
	((and (access? operator)
	      (constant/system-global-environment?
	       (integrate/expression operations environment (access/environment operator))))
	 (integrate/access-operator expression operations environment
				    block operator operands))
	((and (constant? operator)
	      (primitive-procedure? (constant/value operator)))
	 (let ((operands*
		(and (eq? (constant/value operator) (ucode-primitive apply))
		     (integrate/hack-apply? operands))))
	   (if operands*
	       (integrate/combination expression operations environment
				      block (car operands*) (cdr operands*))
	       (integrate/primitive-operator expression operations environment
					     block operator operands))))
	(else
	 (combination/make
	  expression
	  block
	  (let* ((integrate-procedure
		  (lambda (operator)
		    (integrate/procedure-operator operations environment
						  block operator operands)))
		 (operator
		  (if (procedure? operator)
		      (integrate-procedure operator)
		      (let ((operator
			     (integrate/expression operations
						   environment
						   operator)))
			(if (procedure? operator)
			    (integrate-procedure operator)
			    operator)))))
	    (cond ((integrate/compound-operator operator operands)
		   => integrate-procedure)
		  (else operator)))
	  operands))))

(define (integrate/procedure-operator operations environment
				      block procedure operands)
  (integrate/procedure operations
		       (simulate-application environment block
					     procedure operands)
		       procedure))

(define (integrate/primitive-operator expression operations environment
				      block operator operands)
  (declare (ignore operations environment))
  (combination/make expression block operator operands))

;;; ((let ((a (foo)) (b (bar)))
;;;    (lambda (receiver)
;;;      ...body...))
;;;  (lambda (x y z)
;;;    ...))
;;;
;;; =>
;;;
;;; (let ((receiver (lambda (x y z) ...)))
;;;   (let ((a (foo)) (b (bar)))
;;;     ...))
;;;
;;; We do this transformation conservatively, only if the operands of
;;; the original combination have no side effects, so that this
;;; transformation does not have the consequence of committing to a
;;; particular order of evaluation when the original program didn't
;;; request one.  Omitting the NON-SIDE-EFFECTING? test would transform
;;;
;;; ((let ((a (foo)) (b (bar)))
;;;    (lambda (x y)
;;;      ...body...))
;;;  (mumble)
;;;  (frotz))
;;;
;;; =>
;;;
;;; (let ((x (mumble)) (y (frotz)))
;;;   (let ((a (foo)) (b (bar)))
;;;     ...body...))
;;;
;;; Here, the input program required that (foo) and (bar) be evaluated
;;; in some sequence without (mumble) or (frotz) intervening, and
;;; otherwise requested no particular order of evaluation.  The output
;;; of the more aggressive transformation evaluates both (mumble) and
;;; (frotz) in some sequence before evaluating (foo) and (bar) in some
;;; sequence.
;;;
;;; INTEGRATE/COMPOUND-OPERATOR takes any expression (usually from an
;;; operator position), and, if it is a nested sequence of LETs,
;;; BEGINs, or DECLAREs followed by a LAMBDA, returns a LAMBDA that is
;;; equivalent to the expression if used in an operator position; or
;;; otherwise returns #F.

(define (integrate/compound-operator operator operands)
  (define (scan-body body encloser)
    (if (procedure? body)
	(and (not (open-block? (procedure/body body)))
	     (procedure-with-body body (encloser (procedure/body body))))
	(scan-operator body encloser)))
  (define (scan-operator operator encloser)
    (cond ((sequence? operator)
	   (let ((reversed-actions (reverse (sequence/actions operator))))
	     (scan-body (car reversed-actions)
			(let ((commands (cdr reversed-actions)))
			  (lambda (expression)
			    (encloser
			     (sequence-with-actions
			      operator
			      (reverse (cons expression commands)))))))))
	  ((combination? operator)
	   (let ((descend
		  (lambda (operator*)
		    (and (not (open-block? (procedure/body operator*)))
			 (scan-body
			  (procedure/body operator*)
			  (lambda (body*)
			    (encloser
			     (combination-with-operator
			      operator
			      (procedure-with-body operator* body*))))))))
		 (operator* (combination/operator operator)))
	     (cond ((procedure? operator*) (descend operator*))
		   ((integrate/compound-operator
		     operator*
		     (combination/operands operator))
		    => descend)
		   (else #f))))
	  ((declaration? operator)
	   (scan-body (declaration/expression operator)
		      (lambda (expression)
			(encloser
			 (declaration-with-expression operator expression)))))
	  (else #f)))
  (and (for-all? operands non-side-effecting?)
       (scan-operator operator (lambda (body) body))))

(define (combination-with-operator combination operator)
  (combination/make combination
		    (combination/block combination)
		    operator
		    (combination/operands combination)))

(define (declaration-with-expression declaration expression)
  (declaration/make (declaration/scode declaration)
		    (declaration/declarations declaration)
		    expression))

;;; Replacing the body may cause variables from outside the original
;;; body to be shadowed, so we use a sleazy stupid hack to work around
;;; this, because cgen doesn't do alphatization itself.  (This is the
;;; same hack as used in copy.scm to copy integrated expressions that
;;; have free variables.)

(define (procedure-with-body procedure body)
  (for-each hackify-variable (procedure/required procedure))
  (for-each hackify-variable (procedure/optional procedure))
  (cond ((procedure/rest procedure) => hackify-variable))
  (procedure/make (procedure/scode procedure)
		  (procedure/block procedure)
		  (procedure/name procedure)
		  (procedure/required procedure)
		  (procedure/optional procedure)
		  (procedure/rest procedure)
		  body))

(define (hackify-variable variable)
  (set-variable/name!
   variable
   (string->uninterned-symbol (symbol-name (variable/name variable)))))

(define (sequence-with-actions sequence actions)
  (sequence/make (sequence/scode sequence) actions))

(define (non-side-effecting? expression)
  (or (reference? expression)
      (non-side-effecting-in-sequence? expression)))

(define-method/integrate 'DECLARATION
  (lambda (operations environment declaration)
    (let ((declarations (declaration/declarations declaration))
	  (expression (declaration/expression declaration)))
      (declaration/make
       (declaration/scode declaration)
       declarations
       (integrate/expression (declarations/bind operations declarations)
			     environment
			     expression)))))

;;;; Easy Cases

(define-method/integrate 'CONSTANT
  (lambda (operations environment expression)
    operations
    environment
    expression))

(define-method/integrate 'THE-ENVIRONMENT
  (lambda (operations environment expression)
    operations
    environment
    expression))

(define-method/integrate 'QUOTATION
  (lambda (operations environment expression)
    operations
    environment
    (integrate/quotation expression)))

(define-method/integrate 'CONDITIONAL
  (lambda (operations environment expression)
    (conditional/make
     (conditional/scode expression)
     (integrate/expression
      operations environment
      (conditional/predicate expression))
     (integrate/expression
      operations environment
      (conditional/consequent expression))
     (integrate/expression
      operations environment
      (conditional/alternative expression)))))

(define-method/integrate 'DISJUNCTION
  (lambda (operations environment expression)
    (disjunction/make 
     (disjunction/scode expression)
     (integrate/expression operations environment (disjunction/predicate expression))
     (integrate/expression operations environment (disjunction/alternative expression)))))

(define-method/integrate 'SEQUENCE
  (lambda (operations environment expression)
    ;; Optimize (begin (foo)) => (foo)
    ;; Optimize (begin a b (foo) 22 (bar)) => (begin (foo) (bar))
    (sequence/optimizing-make
     expression
     (integrate/actions operations environment
			(sequence/actions expression)))))

(define (integrate/actions operations environment actions)
  (let ((action (car actions)))
    (if (null? (cdr actions))
	(list (if (eq? action open-block/value-marker)
		  action
		  (integrate/expression operations environment action)))
	(cons (cond ((reference? action)
		     ;; This clause lets you ignore a variable by
		     ;; mentioning it in a sequence.
		     (variable/may-ignore! (reference/variable action))
		     action)
		    ((eq? action open-block/value-marker)
		     action)
		    (else
		     (integrate/expression operations environment action)))
	      (integrate/actions operations environment (cdr actions))))))

(define (sequence/optimizing-make expression actions)
  (let ((actions (remove-non-side-effecting actions)))
    (if (null? (cdr actions))
	(car actions)
	(sequence/make (and expression (object/scode expression))
		       actions))))

(define (remove-non-side-effecting actions)
  ;; Do not remove references from sequences, because they have
  ;; meaning as declarations.  The output code generator will take
  ;; care of removing them when they are no longer needed.
  (if (null? (cdr actions))
      actions
      (let ((rest (remove-non-side-effecting (cdr actions))))
	(if (non-side-effecting-in-sequence? (car actions))
	    rest
	    (cons (car actions) rest)))))

(define (non-side-effecting-in-sequence? expression)
  ;; Compiler does a better job of this because it is smarter about
  ;; what kinds of expressions can cause side effects.  But this
  ;; should be adequate to catch most of the simple cases.
  (or (constant? expression)
      (quotation? expression)
      (delay? expression)
      (procedure? expression)
      (and (access? expression)
	   (non-side-effecting-in-sequence? (access/environment expression)))))

(define-method/integrate 'ACCESS
  (lambda (operations environment expression)
    (let ((environment* (integrate/expression operations environment
					      (access/environment expression)))
	  (name (access/name expression)))
      (cond ((and (constant/system-global-environment? environment*)
		  (assq name usual-integrations/constant-alist))
	     => (lambda (entry)
		  (constant/make (access/scode expression)
				 (constant/value (cdr entry)))))
	    (else (access/make (access/scode expression)
			       environment* name))))))

(define (constant/system-global-environment? expression)
  (and (constant? expression)
       (system-global-environment? (constant/value expression))))

(define-method/integrate 'DELAY
  (lambda (operations environment expression)
    (delay/make
     (delay/scode expression)
     (integrate/expression operations environment
			   (delay/expression expression)))))

(define (integrate/quotation quotation)
  (call-with-values
      (lambda ()
	(integrate/top-level* (quotation/scode quotation)
			      (quotation/block quotation)
			      (quotation/expression quotation)))
    (lambda (operations environment expression)
      operations environment		;ignore
      expression)))

(define (integrate/access-operator expression operations environment block operator operands)
  (let ((name (access/name operator))
	(environment*
	 (integrate/expression operations environment (access/environment operator))))

    (define (dont-integrate)
      (combination/make
       expression block
       (access/make (access/scode operator) environment* name) operands))

    (if (not (constant/system-global-environment? environment*))
	(dont-integrate)
	(operations/lookup-global
	 operations name
	 (lambda (operation info)
	   (case operation
	     ((#F) (dont-integrate));; shadowed

	     ((EXPAND)
	      (cond ((info expression operands (reference/block operator))
		     => (lambda (new-expression)
			  (integrate/expression operations environment new-expression))) 
		    (else (dont-integrate))))

	     ((INTEGRATE INTEGRATE-OPERATOR)
	      ;; This branch is never taken because all the global
	      ;; operators are defined via expansions.  But if that
	      ;; ever changes...
	      (let ((new-expression (integrate/name expression operator info environment)))
		(if new-expression
		    (integrate/combination
		     expression operations environment
		     block new-expression operands)
		    (dont-integrate))))

	     (else
	      (error "unknown operation" operation))))
	 dont-integrate))))

;;;; Environment

(define (environment/recursive-bind operations environment variables vals)
  ;; Used to implement mutually-recursive definitions that can
  ;; integrate one another.  When circularities are detected within
  ;; the definition-reference graph, integration is disabled.
  (let ((vals
	 (map (lambda (value)
		(delayed-integration/make operations value))
	      vals)))
    (let ((environment
	   (environment/bind-multiple environment variables vals)))
      (for-each (lambda (value)
		  (set-delayed-integration/environment! value environment))
		vals)
      (values environment (map delayed-integration/force vals)))))

(define (integrate/name expr reference info environment)
  (let ((variable (reference/variable reference)))
    (let ((finish
	   (lambda (value)
	     (reassign
	      expr
	      (copy/expression/intern (reference/block reference) value)))))
      (if info
	  (finish (integration-info/expression info))
	  (environment/lookup environment variable
	    (lambda (value)
	      (if (delayed-integration? value)
		  (if (delayed-integration/in-progress? value)
		      #f
		      (finish (delayed-integration/force value)))
		  (finish value)))
	    false-procedure
	    false-procedure)))))

(define (variable/final-value variable environment if-value if-not)
  (environment/lookup environment variable
    (lambda (value)
      (if (delayed-integration? value)
	  (if (delayed-integration/in-progress? value)
	      (error "Unfinished integration" value)
	      (if-value (delayed-integration/force value)))
	  (if-value value)))
    (lambda ()
      (if-not))
    (lambda ()
      (warn "Unable to integrate" (variable/name variable))
      (if-not))))


(define (integrate/hack-apply? operands)
  (define (check operand)
    (cond ((constant? operand)
	   (if (null? (constant/value operand))
	       '()
	       'FAIL))
	  ((not (combination? operand))
	   'FAIL)
	  (else
	   (let ((rator (combination/operator operand)))
	     (if (or (and (constant? rator)
			  (eq? (ucode-primitive cons)
			       (constant/value rator)))
		     (eq? 'cons (global-ref? rator)))
		 (let* ((rands (combination/operands operand))
			(next (check (cadr rands))))
		   (if (eq? next 'FAIL)
		       'FAIL
		       (cons (car rands) next)))
		 'FAIL)))))

  (and (not (null? operands))
       (let ((tail (check (car (last-pair operands)))))
	 (and (not (eq? tail 'FAIL))
	      (append (except-last-pair operands)
		      tail)))))


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
	      (set-delayed-integration/state! delayed-integration
					      'BEING-INTEGRATED)
	      (set-delayed-integration/environment! delayed-integration #f)
	      (set-delayed-integration/operations! delayed-integration #f)
	      (set-delayed-integration/value! delayed-integration #f)
	      (integrate/expression operations environment expression))))
       (set-delayed-integration/state! delayed-integration 'INTEGRATED)
       (set-delayed-integration/value! delayed-integration value)))
    ((INTEGRATED) 'DONE)
    ((BEING-INTEGRATED)
     (error "Attempt to re-force delayed integration"
	    delayed-integration))
    (else
     (error "Delayed integration has unknown state"
	    delayed-integration)))
  (delayed-integration/value delayed-integration))