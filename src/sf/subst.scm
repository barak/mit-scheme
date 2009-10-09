#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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
	 (integrate-external "object" "lsets"))

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
		  (process-block-flags (block/flags block)
		    (lambda ()
		      (values operations
			      environment
			      (integrate/expression operations
						    environment
						    expression))))))))
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

(define *eager-integration-switch #f)

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
		  expression))
	       (try-safe-integration
		(lambda ()
		  (integrate/name-if-safe expression expression
					  environment operations
					  '(INTEGRATE INTEGRATE-SAFELY)
					  integration-success
					  integration-failure))))
	(operations/lookup operations variable
	 (lambda (operation info)
	   (case operation
	     ((INTEGRATE-OPERATOR EXPAND)
	      (variable/reference! variable)
	      expression)
	     ((INTEGRATE)
	      (integrate/name expression expression info environment
			      integration-success integration-failure))
	     ((INTEGRATE-SAFELY)
	      (try-safe-integration))
	     (else
	      (error "Unknown operation" operation))))
	 (lambda ()
	   (if *eager-integration-switch
	       (try-safe-integration)
	       (integration-failure))))))))

(define (integrate/name-if-safe expr reference environment
				operations safe-operations if-win if-fail)
  (let ((variable (reference/variable reference)))
    (if (or (variable/side-effected variable)
	    (not (block/safe? (variable/block variable))))
	(if-fail)
	(let ((finish
	       (lambda (value)
		 (if (safely-integrable-value? value environment operations
					       safe-operations)
		     (if-win
		      (reassign
		       expr
		       (copy/expression/intern (reference/block reference)
					       value)))
		     (if-fail)))))
	  (environment/lookup environment variable
            (lambda (value)
	      (if (delayed-integration? value)
		  (if (delayed-integration/in-progress? value)
		      (if-fail)
		      (finish (delayed-integration/force value)))
		  (finish value)))
	    (lambda () (if-fail))
	    (lambda () (if-fail)))))))

(define (reassign expr object)
  (if (and expr (object/scode expr))
      ;; Abstraction violation
      (with-new-scode (object/scode expr) object)
      object))

(define (safely-integrable-value? value environment operations safe-operations)
  (let check ((value value) (top? #t))
    (or (constant? value)
	(and (reference? value)
	     (or (not top?)
		 (let ((variable (reference/variable value)))
		   (or (operations/lookup operations variable
			 (lambda (operation info)
			   info		;ignore
			   (memq operation safe-operations))
			 (lambda () #f))
		       (and (not (variable/side-effected variable))
			    (block/safe? (variable/block variable))
			    (environment/lookup environment variable
			      (lambda (value*)
				(check value* #f))
			      (lambda ()
				;; unknown value
				(operations/lookup operations variable
				  (lambda (operation info)
				    operation info
				    #f)
				  (lambda ()
				    ;; No operations
				    #t)))
			      (lambda ()
				;; not found variable
				#t))))))))))

(define (integrate/reference-operator expression operations environment
				      block operator operands)
  (let ((variable (reference/variable operator)))
    (letrec ((mark-integrated!
	      (lambda ()
		(variable/integrated! variable)))
	     (integration-failure
	      (lambda ()
		(variable/reference! variable)
		(combination/optimizing-make expression block
					     operator operands)))
	     (integration-success
	      (lambda (operator)
		(mark-integrated!)
		(integrate/combination expression operations environment
				       block operator operands)))
	     (try-safe-integration
	      (lambda ()
		(integrate/name-if-safe expression operator
					environment operations
					'(EXPAND INTEGRATE INTEGRATE-OPERATOR
						 INTEGRATE-SAFELY)
					integration-success
					integration-failure))))
      (operations/lookup operations variable
       (lambda (operation info)
	 (case operation
	   ((#F) (integration-failure))
	   ((INTEGRATE INTEGRATE-OPERATOR)
	    (integrate/name expression
			    operator info environment
			    integration-success
			    integration-failure))
	   ((INTEGRATE-SAFELY)
	    (try-safe-integration))
	   ((EXPAND)
	    (info expression
		  operands
		  (lambda (new-expression)
		    (mark-integrated!)
		    (integrate/expression operations environment
					  new-expression))
		  integration-failure
		  (reference/block operator)))
	   (else
	    (error "Unknown operation" operation))))
       (lambda ()
	 (if *eager-integration-switch
	     (try-safe-integration)
	     (integration-failure)))))))

;;;; Binding

(define (integrate/open-block operations environment expression)
  (let ((variables (open-block/variables expression))
	(block (open-block/block expression)))
    (let ((operations
	   (declarations/bind (operations/shadow operations variables)
			      (block/declarations block))))
      (process-block-flags (block/flags block)
	(lambda ()
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
			(if (open-block/optimized expression)
			    (open-block/make
			     (and expression (object/scode expression))
			     block variables
			     vals actions #t)
			    (open-block/optimizing-make
			     expression block variables vals
			     actions operations environment)))))))))))

(define-method/integrate 'OPEN-BLOCK
  (lambda (operations environment expression)
    (call-with-values
	(lambda () (integrate/open-block operations environment expression))
      (lambda (operations environment expression)
	operations environment
	expression))))

(define (process-block-flags flags continuation)
  (if (null? flags)
      (continuation)
      (let ((this-flag (car flags)))
	(case this-flag
	  ((AUTOMAGIC-INTEGRATIONS)
	   (fluid-let ((*eager-integration-switch #T))
	     (process-block-flags (cdr flags) continuation)))
	  ((NO-AUTOMAGIC-INTEGRATIONS)
	   (fluid-let ((*eager-integration-switch #F))
	     (process-block-flags (cdr flags) continuation)))
	  ((ETA-SUBSTITUTION)
	   (fluid-let ((*eta-substitution-switch #T))
	     (process-block-flags (cdr flags) continuation)))
	  ((NO-ETA-SUBSTITUTION)
	   (fluid-let ((*eta-substitution-switch #F))
	     (process-block-flags (cdr flags) continuation)))
	  ((OPEN-BLOCK-OPTIMIZATIONS)
	   (fluid-let ((*block-optimizing-switch #T))
	     (process-block-flags (cdr flags) continuation)))
	  ((NO-OPEN-BLOCK-OPTIMIZATIONS)
	   (fluid-let ((*block-optimizing-switch #F))
	     (process-block-flags (cdr flags) continuation)))
	  (else (error "Bad flag"))))))

(define (variable/unreferenced? variable)
  (and (not (variable/integrated variable))
       (not (variable/referenced variable))
       (not (variable/can-ignore? variable))))

(define-method/integrate 'PROCEDURE
  (lambda (operations environment procedure)
    (integrate/procedure operations
			 (simulate-unknown-application environment procedure)
			 procedure)))

;; Cannot optimize (lambda () (bar)) => bar (eta substitution) because
;; BAR may be a procedure with different arity than the lambda

#| You can get some weird stuff with this

(define (foo x)
  (define (loop1) (loop2))
  (define (loop2) (loop3))
  (define (loop3) (loop1))
  (bar x))

will optimize into

(define (foo x)
  (define loop1 loop3)
  (define loop2 loop3)
  (define loop3 loop3)
  (bar x))

and if you have automagic integrations on, this won't finish
optimizing.  Well, you told the machine to loop forever, and it
determines that it can do this at compile time, so you get what
you ask for.

|#

(define *eta-substitution-switch #F)

(define (integrate/procedure operations environment procedure)
  (let ((block (procedure/block procedure))
	(required (procedure/required procedure))
	(optional (procedure/optional procedure))
	(rest (procedure/rest procedure)))
    (fluid-let ((*current-block-names*
		 (cons (procedure/name procedure)
		       *current-block-names*)))
      (process-block-flags (block/flags block)
	(lambda ()
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
	    (if (and *eta-substitution-switch
		     (combination? body)
		     (null? optional)
		     (null? rest)
		     (let ((operands (combination/operands body)))
		       (match-up? operands required))
		     (set/empty?
		      (set/intersection
		       (list->set variable? eq? required)
		       (free/expression (combination/operator body)))))
		(combination/operator body)
		(procedure/make (procedure/scode procedure)
				block
				(procedure/name procedure)
				required
				optional
				rest
				body))))))))

(define (match-up? operands required)
  (if (null? operands)
      (null? required)
      (and (not (null? required))
	   (let ((this-operand (car operands))
		 (this-required (car required)))
	     (and (reference? this-operand)
		  (eq? (reference/variable this-operand) this-required)
		  (match-up? (cdr operands) (cdr required)))))))

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
	 (combination/optimizing-make
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
  (let ((integration-failure
	 (lambda ()
	   (combination/optimizing-make expression block operator operands))))
    (operations/lookup operations (constant/value operator)
      (lambda (operation info)
	(case operation
	  ((#F) (integration-failure))
	  ((EXPAND)
	   (info expression
		 operands
		 (lambda (expression)
		   (integrate/expression operations environment expression))
		 integration-failure
		 block))
	  (else (error "Unknown operation" operation))))
      integration-failure)))

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
  (combination/make (combination/scode combination)
		    (combination/block combination)
		    operator
		    (combination/operands combination)))

(define (declaration-with-expression declaration expression)
  (declaration/make (declaration/scode declaration)
		    (declaration/declarations declaration)
		    expression))

(define (procedure-with-body procedure body)
  (procedure/make (procedure/scode procedure)
		  (procedure/block procedure)
		  (procedure/name procedure)
		  (procedure/required procedure)
		  (procedure/optional procedure)
		  (procedure/rest procedure)
		  body))

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

;; Optimize (if #f a b) => b; (if #t a b) => a
;;   (if (let (...) t) a b) => (let (...) (if t a b))
;;   (if (begin ... t) a b) => (begin ... (if t a b))

(define-method/integrate 'CONDITIONAL
  (lambda (operations environment expression)
    (let ((predicate (integrate/expression
		      operations environment
		      (conditional/predicate expression)))
	  (consequent (integrate/expression
		       operations environment
		       (conditional/consequent expression)))
	  (alternative (integrate/expression
			operations environment
			(conditional/alternative expression))))
      (let loop ((predicate predicate))
	(cond ((constant? predicate)
	       (if (constant/value predicate)
		   consequent
		   alternative))
	      ((sequence? predicate)
	       (sequence-with-actions
		predicate
		(let ((actions (reverse (sequence/actions predicate))))
		  (reverse
		   (cons (loop (car actions))
			 (cdr actions))))))
	      ((and (combination? predicate)
		    (procedure? (combination/operator predicate))
		    (not
		     (open-block?
		      (procedure/body (combination/operator predicate)))))
	       (combination-with-operator
		predicate
		(procedure-with-body
		 (combination/operator predicate)
		 (loop (procedure/body (combination/operator predicate))))))
	      (else
	       (conditional/make (conditional/scode expression)
				 predicate consequent alternative)))))))

;; Optimize (or #f a) => a; (or #t a) => #t

(define-method/integrate 'DISJUNCTION
  (lambda (operations environment expression)
    (let ((predicate (integrate/expression operations environment
					   (disjunction/predicate expression)))
	  (alternative (integrate/expression
			operations environment
			(disjunction/alternative expression))))
      (if (constant? predicate)
	  (if (constant/value predicate)
	      predicate
	      alternative)
	  (disjunction/make (disjunction/scode expression)
			    predicate alternative)))))

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
		     (variable/can-ignore! (reference/variable action))
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

(define (integrate/access-operator expression operations environment
				   block operator operands)
  (let ((name (access/name operator))
	(dont-integrate
	 (lambda ()
	   (combination/make
	    (and expression (object/scode expression))
	    block
	    (integrate/expression operations environment operator)
	    (integrate/expressions operations environment operands)))))
    (cond ((and (eq? name 'APPLY)
		(integrate/hack-apply? operands))
	   => (lambda (operands*)
		(integrate/combination expression operations environment
				       block (car operands*) (cdr operands*))))
	  ((assq name usual-integrations/constant-alist)
	   => (lambda (entry)
		(integrate/combination expression operations environment
				       block (cdr entry) operands)))
	  ((assq name usual-integrations/expansion-alist)
	   => (lambda (entry)
		((cdr entry) expression operands
			     identity-procedure dont-integrate #f)))
	  (else
	   (dont-integrate)))))

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

(define (integrate/name expr reference info environment if-integrated if-not)
  (let ((variable (reference/variable reference)))
    (let ((finish
	   (lambda (value)
	     (if-integrated
	      (reassign
	       expr
	       (copy/expression/intern (reference/block reference) value))))))
      (if info
	  (finish (integration-info/expression info))
	  (environment/lookup environment variable
	    (lambda (value)
	      (if (delayed-integration? value)
		  (if (delayed-integration/in-progress? value)
		      (if-not)
		      (finish (delayed-integration/force value)))
		  (finish value)))
	    if-not
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
      (if-not))
    (lambda ()
      (warn "Unable to integrate" (variable/name variable))
      (if-not))))

(define *unknown-value "Unknown Value")

(define (simulate-unknown-application environment procedure)
  (define (bind-required environment required)
    (if (null? required)
	(bind-optional environment (procedure/optional procedure))
	(bind-required
	 (environment/bind environment (car required) *unknown-value)
	 (cdr required))))

  (define (bind-optional environment optional)
    (if (null? optional)
	(bind-rest environment (procedure/rest procedure))
	(bind-optional
	 (environment/bind environment (car optional) *unknown-value)
	 (cdr optional))))

  (define (bind-rest environment rest)
    (if (null? rest)
	environment
	(environment/bind environment rest *unknown-value)))

  (bind-required environment (procedure/required procedure)))

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

(define (simulate-application environment block procedure operands)
  (define (procedure->pretty procedure)
    (if (procedure/scode procedure)
	(unsyntax (procedure/scode procedure))
	(let ((arg-list (append (procedure/required procedure)
				(if (null? (procedure/optional procedure))
				    '()
				    (cons lambda-tag:optional
					  (procedure/optional procedure)))
				(if (not (procedure/rest procedure))
				    '()
				    (procedure/rest procedure)))))
	  (if (procedure/name procedure)
	      `(named-lambda (,(procedure/name procedure) ,@arg-list)
		 ...)
	      `(lambda ,arg-list
		 ...)))))

  (define (match-required environment required operands)
    (cond ((null? required)
	   (match-optional environment
			   (procedure/optional procedure)
			   operands))
	  ((null? operands)
	   (error "Too few operands in call to procedure"
		  procedure
		  (procedure->pretty procedure)))
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

  (define (listify-tail operands)
    (let ((const-null (constant/make #f '())))
      (if (null? operands)
	  const-null
	  (let ((const-cons (constant/make #f (ucode-primitive cons))))
	    (let walk ((operands operands))
	      (if (null? operands)
		  const-null
		  (combination/make #f
				    block
				    const-cons
				    (list (car operands)
					  (walk (cdr operands))))))))))

  (define (match-rest environment rest operands)
    (cond (rest
	   (environment/bind environment rest (listify-tail operands)))
	  ((null? operands)
	   environment)
	  (else
	   (error "Too many operands in call to procedure"
		  procedure
		  (procedure->pretty procedure)))))

  (match-required environment (procedure/required procedure) operands))

(define (environment/make)
  '())

(define-integrable (environment/bind environment variable value)
  (cons (cons variable value) environment))

(define-integrable (environment/bind-multiple environment variables values)
  (map* environment cons variables values))

(define (environment/lookup environment variable if-found if-unknown if-not)
  (let ((association (assq variable environment)))
    (if association
	(if (eq? (cdr association) *unknown-value)
	    (if-unknown)
	    (if-found (cdr association)))
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

;;;; Optimizations

#|
Simple LET-like combination.  Delete any unreferenced
parameters.  If no parameters remain, delete the
combination and lambda.  Values bound to the unreferenced
parameters are pulled out of the combination.  But integrated
forms are simply removed.

(define (foo a)
  (let ((a (+ a 3))
	(b (bar a))
	(c (baz a)))
    (declare (integrate c))
    (+ c a)))

        ||
        \/

(define (foo a)
  (bar a)
  (let ((a (+ a 3)))
    (+ (baz a) a)))

|#

(define (foldable-constant? thing)
  (constant? thing))

(define (foldable-constants? list)
  (or (null? list)
      (and (foldable-constant? (car list))
	   (foldable-constants? (cdr list)))))

(define (foldable-constant-value thing)
  (cond ((constant? thing)
	 (constant/value thing))
	(else
	 (error "foldable-constant-value: can't happen" thing))))

(define *foldable-primitive-procedures
  (map make-primitive-procedure
       '(OBJECT-TYPE OBJECT-TYPE?
         NOT EQ? NULL? PAIR? ZERO? POSITIVE? NEGATIVE?
	 &= &< &> &+ &- &* &/ 1+ -1+)))

(define (foldable-operator? operator)
  (and (constant? operator)
       (primitive-procedure? (constant/value operator))
       (memq (constant/value operator) *foldable-primitive-procedures)))

;;; deal with (let () (define ...))
;;; deal with (let ((x 7)) (let ((y 4)) ...)) => (let ((x 7) (y 4)) ...)
;;; Actually, we really don't want to hack with these for various
;;; reasons

(define (combination/optimizing-make expression block operator operands)
  (cond (
	 ;; fold constants
	 (and (foldable-operator? operator)
	      (foldable-constants? operands))
	 (constant/make (and expression (object/scode expression))
			(apply (constant/value operator)
			       (map foldable-constant-value operands))))

	(
	 ;; (force (delay x)) ==> x
	 (and (constant? operator)
	      (eq? (constant/value operator) force)
	      (= (length operands) 1)
	      (delay? (car operands)))
	 (delay/expression (car operands)))

	((and (procedure? operator)
	      (block/safe? (procedure/block operator))
	      (for-all? (declarations/original
			 (block/declarations (procedure/block operator)))
		declarations/known?)
	      (for-all? (procedure/optional operator)
		variable/integrated)
	      (or (not (procedure/rest operator))
		  (variable/integrated (procedure/rest operator))))
	 (delete-unreferenced-parameters
	  (append (procedure/required operator)
		  (procedure/optional operator))
	  (procedure/rest operator)
	  (procedure/body operator)
	  operands
	  (lambda (required referenced-operands unreferenced-operands)
	    (let ((form
		   (if (and (null? required)
			    ;; need to avoid things like this
			    ;; (foo bar (let () (define (baz) ..) ..))
			    ;; optimizing into
			    ;; (foo bar (define (baz) ..) ..)
			    (not (open-block? (procedure/body operator))))
		       (reassign expression (procedure/body operator))
		       (combination/make
			(and expression (object/scode expression))
			block
			(procedure/make
			 (procedure/scode operator)
			 (procedure/block operator)
			 (procedure/name operator)
			 required
			 '()
			 #f
			 (procedure/body operator))
			referenced-operands))))
	      (if (null? unreferenced-operands)
		  form
		  (sequence/optimizing-make
		   expression
		   (append unreferenced-operands (list form))))))))
	(else
	 (combination/make (and expression (object/scode expression))
			   block operator operands))))

(define (delete-unreferenced-parameters parameters rest body operands receiver)
  (let ((free-in-body (free/expression body)))
    (let loop ((parameters 		parameters)
	       (operands   		operands)
	       (required-parameters	'())
	       (referenced-operands	'())
	       (unreferenced-operands	'()))
    (cond ((null? parameters)
	   (if (or rest (null? operands))
	       (receiver (reverse required-parameters) ; preserve order
			 (reverse referenced-operands)
			 (if (or (null? operands)
				 (variable/integrated rest))
			     unreferenced-operands
			     (append operands unreferenced-operands)))
	       (error "Argument mismatch" operands)))
	  ((null? operands)
	   (error "Argument mismatch" parameters))
	  (else
	   (let ((this-parameter (car parameters))
		 (this-operand   (car operands)))
	     (cond ((set/member? free-in-body this-parameter)
		    (loop (cdr parameters)
			  (cdr operands)
			  (cons this-parameter required-parameters)
			  (cons this-operand   referenced-operands)
			  unreferenced-operands))
		   ((variable/integrated this-parameter)
		    (loop (cdr parameters)
			  (cdr operands)
			  required-parameters
			  referenced-operands
			  unreferenced-operands))
		   (else
		    (loop (cdr parameters)
			  (cdr operands)
			  required-parameters
			  referenced-operands
			  (cons this-operand
				unreferenced-operands))))))))))

(define *block-optimizing-switch #f)

;; This is overly hairy, but if it works, no one need know.
;; What we do is this:
;; 1 Make a directed graph of the dependencies in an open
;;    block.
;; 2 Identify the circular dependencies and place them in
;;    a open block.
;; 3 Identify the bindings that can be made in parallel and
;;    make LET type statements.
;; 4 This deletes unused bindings in an open block and
;;    compartmentalizes the environment.
;; 5 Re-optimize the code in the body.  This can help if the
;;    eta-substitution-switch is on.

(define (open-block/optimizing-make expression block vars values
				    actions operations environment)
  (if (and *block-optimizing-switch
	   (block/safe? block))
      (let ((table:var->vals (associate-vars-and-vals vars values))
	    (bound-variables (varlist->varset vars)))
	(let ((table:vals->free
	       (get-free-vars-in-bindings bound-variables values))
	      (body-free (get-body-free-vars bound-variables actions)))
	  ;; (write-string "Free vars in body")
	  ;; (display (map variable/name body-free))
	  (let ((graph (build-graph vars
				    table:var->vals
				    table:vals->free
				    body-free)))
	    (collapse-circularities! graph)
	    ;; (print-graph graph)
	    (label-node-depth! graph)
	    (let ((template (linearize graph)))
	      ;; (print-template template)
	      (integrate/expression
	       operations environment
	       (build-new-code expression
			       template
			       (block/parent block)
			       table:var->vals actions))))))
      (open-block/make
       (and expression (object/scode expression))
       block vars values actions #t)))

#|
(define (print-template template)
  (if (null? template)
      '()
      (let ((this (car template)))
	(newline)
	(display (car this))
	(display (map variable/name (cdr this)))
	(print-template (cdr template)))))
|#

(define (associate-vars-and-vals vars vals)
  (let ((table (make-generic-eq?-table)))
    (define (fill-table vars vals)
      (cond ((null? vars) (if (null? vals) '() (error "Mismatch")))
	    ((null? vals) (error "Mismatch"))
	    (else (table-put! table (car vars) (car vals))
		  (fill-table (cdr vars) (cdr vals)))))
    (fill-table vars vals)
    table))

(declare (integrate varlist->varset nodelist->nodeset
		    empty-nodeset singleton-nodeset
		    empty-varset singleton-varset))

(define (varlist->varset list)
  (declare (integrate list))
  (list->set variable? eq? list))

(define (nodelist->nodeset list)
  (declare (integrate list))
  (list->set node? eq? list))

(define (empty-nodeset)
  (empty-set node? eq?))

(define (singleton-nodeset node)
  (declare (integrate node))
  (singleton-set node? eq? node))

(define (empty-varset)
  (declare (integrate node))
  (empty-set variable? eq?))

(define (singleton-varset variable)
  (declare (integrate variable))
  (singleton-set variable? eq? variable))

(define (get-free-vars-in-bindings bound-variables vals)
  ;; find variables in bindings that are scoped to these
  ;; bound variables
  (let ((table (make-generic-eq?-table)))
    (define (kernel val)
      (let ((free-variables (free/expression val)))
	(table-put! table val
		    (set/intersection bound-variables free-variables))))
    (for-each kernel vals)
    table))

(define (get-body-free-vars bound-variables actions)
  (let ((body-forms (get-body actions)))
    (let loop ((body-forms body-forms)
	       (free (empty-varset)))
      (if (null? body-forms)
	  free
	  (loop (cdr body-forms)
		(set/union free
			   (set/intersection bound-variables
					     (free/expression
					      (car body-forms)))))))))

(define (get-body actions)
  (cond ((null? actions) '())
	((eq? (car actions) open-block/value-marker) (get-body (cdr actions)))
	(else (cons (car actions) (get-body (cdr actions))))))

;;; Graph structure for figuring out dependencies in a LETREC

(define-structure (node
		   (constructor %make-node (type vars))
		   (conc-name %node-))
  type
  (vars #f read-only #t)
  (needs (empty-nodeset))
  (needed-by (empty-nodeset))
  (depth #f))

(define-integrable (make-base-node)
  (%make-node 'BASE (empty-varset)))

(define-integrable (variable->node variable)
  (%make-node 'SETUP (singleton-varset variable)))

(define-integrable (make-letrec-node variable-set)
  (%make-node 'LETREC variable-set))

(define-integrable (add-node-need! needer what-i-need)
  (set-%node-needs! needer (set/adjoin (%node-needs needer) what-i-need)))

(define-integrable (remove-node-need! needer what-i-no-longer-need)
  (set-%node-needs! needer
		    (set/remove (%node-needs needer) what-i-no-longer-need)))

(define-integrable (add-node-needed-by! needee what-needs-me)
  (set-%node-needed-by! needee
			(set/adjoin (%node-needed-by needee) what-needs-me)))

(define-integrable (remove-node-needed-by! needee what-needs-me)
  (set-%node-needed-by! needee
			(set/remove (%node-needed-by needee) what-needs-me)))

(define (build-graph vars table:var->vals table:vals->free body-free)
  (let ((table:variable->node (make-generic-eq?-table)))

    (define (kernel variable)
      (let ((node (variable->node variable)))
	(table-put! table:variable->node variable node)))

    (for-each kernel vars)

    (link-nodes! body-free table:var->vals table:vals->free vars
		 table:variable->node)))

(define-integrable (link-2-nodes! from-node to-node)
  (add-node-need! from-node to-node)
  (add-node-needed-by! to-node from-node))

(define (unlink-node! node)
  (set/for-each (lambda (needer)
		  (remove-node-needed-by! needer node))
		(%node-needs node))
  (set/for-each (lambda (needee)
		  (remove-node-need! needee node))
		(%node-needed-by node))
  (set-%node-type! node 'UNLINKED))

(define-integrable (unlink-nodes! nodelist)
  (for-each unlink-node! nodelist))

(define (link-nodes! body-free
		    table:var->vals table:vals->free variables table:var->node)

  (define (kernel variable)
    (table-get table:var->node variable
      (lambda (node)
	(table-get-chain variable
	  (lambda (free-vars)
	    (set/for-each
	      (lambda (needed-var)
		(table-get table:var->node needed-var
			   (lambda (needed-node)
			     (link-2-nodes! node needed-node))
			   (lambda ()
			     (error "Broken analysis: can't get node"))))
	      free-vars))
	  (lambda () (error "Broken analysis: can't get free variable info"))
	  table:var->vals table:vals->free))
      (lambda () (error "Broken analysis: no node for variable"))))

  (for-each kernel variables)

  (let ((base-node (make-base-node)))
    (set/for-each
     (lambda (needed-var)
       (table-get table:var->node needed-var
		  (lambda (needed-node)
		    (link-2-nodes! base-node needed-node))
		  (lambda () (error "Broken analysis: free var"))))
     body-free)
    base-node))

(define (collapse-circularities! graph)
  ;; Search for a circularity:  if found, collapse it, and repeat
  ;; until none are found.
  (define (loop)
    (find-circularity graph
      (lambda (nodelist)
	(collapse-nodelist! nodelist)
	(loop))
      (lambda () graph)))
  (loop))

(define (find-circularity graph if-found if-not)
  ;; Walk the tree keeping track of nodes visited
  ;; If a node is encountered more than once, there is
  ;; a circularitiy.  NODES-VISITED is a list kept in
  ;; base node first order.  If a node is found on the
  ;; list, the tail of the list is the nodes in the
  ;; circularity.

  (define (fc this-node nodes-visited if-found if-not)
    (if (null? this-node)
	(if-not)
	(let ((circularity (memq this-node nodes-visited)))
	  (if circularity
	      (if-found circularity)
	      ;; Add this node to the visited list, and loop
	      ;; over the needs of this node.
	      (let ((new-visited (append nodes-visited (list this-node))))
		(let loop ((needs (set->list (%node-needs this-node))))
		  (if (null? needs)
		      (if-not)
		      (fc (car needs) new-visited if-found
			  (lambda () (loop (cdr needs)))))))))))

  (fc graph '() if-found if-not))

(define (collapse-nodelist! nodelist)
  ;; Replace the nodes in the nodelist with a single node that
  ;; has all the variables in it.  This node will become a LETREC
  ;; form.

  ;; Error check:  make sure graph is consistant.
  (for-each (lambda (node) (if (eq? (%node-type node) 'UNLINKED)
			       (error "node not linked")))
	    nodelist)

  (let ((nodeset (nodelist->nodeset nodelist)))
    (let ((varset (apply set/union* (map %node-vars nodelist)))
	  (needs-set  (set/difference
		       (apply set/union* (map %node-needs nodelist))
		       nodeset))
	  (needed-by (set/difference
		      (apply set/union* (map %node-needed-by nodelist))
		      nodeset)))

    (let ((letrec-node (make-letrec-node varset)))
      (set/for-each (lambda (need) (link-2-nodes! letrec-node need))
		    needs-set)
      (set/for-each
       (lambda (needer) (link-2-nodes! needer letrec-node)) needed-by)
      ;; now delete nodes in nodelist
      (unlink-nodes! nodelist)))))

(define (label-node-depth! graph)
  (define (label-nodes! nodeset depth)
    (if (set/empty? nodeset)
	'()
	(begin
	  (set/for-each (lambda (node) (set-%node-depth! node depth)) nodeset)
	  (label-nodes!
	   (apply set/union* (map %node-needs (set->list nodeset)))
	   (1+ depth)))))
  (label-nodes! (singleton-nodeset graph) 0))

#|
(define (print-graph node)
  (if (null? node)
      '()
      (begin
	(newline)
	(display (%node-depth node))
	(display (%node-type node))
	(set/for-each (lambda (variable)
			(display " ")
			(display (variable/name variable)))
		      (%node-vars node))
	(set/for-each print-graph (%node-needs node)))))
|#

(define (collapse-parallel-nodelist depth nodeset)
  (if (set/empty? nodeset)
      '()
      (let loop ((nodestream      (set->list nodeset))
		 (let-children    (empty-varset))
		 (letrec-children (empty-varset))
		 (children        (empty-nodeset)))
	(if (null? nodestream)
	    (let ((outer-contour
		   (collapse-parallel-nodelist (1+ depth) children)))
	      (append (if (set/empty? let-children)
			  '()
			  (list (cons 'LET (set->list let-children))))
		      (if (set/empty? letrec-children)
			  '()
			  (list (cons 'LETREC (set->list letrec-children))))
		      outer-contour))
	    (let ((this-node (car nodestream)))
	      (if (= (%node-depth this-node) (1+ depth))
		  (if (eq? (%node-type this-node) 'LETREC)
		      (loop (cdr nodestream)
			    let-children
			    (set/union (%node-vars this-node) letrec-children)
			    (set/union (%node-needs this-node) children))
		      (loop (cdr nodestream)
			    (set/union (%node-vars this-node) let-children)
			    letrec-children
			    (set/union (%node-needs this-node) children)))
		  ;; deeper nodes will be picked up later
		  (loop (cdr nodestream)
			let-children
			letrec-children
			children)))))))

(define (linearize graph)
  (collapse-parallel-nodelist 0 (%node-needs graph)))

(define (build-new-code expression template parent vars->vals actions)
  (let ((body (sequence/optimizing-make expression (get-body actions))))
    (let loop ((template template)
	       (block    parent)
	       (code     body))
      (if (null? template)
	   code
	   (let ((this (car template)))
	     (let ((this-type (car this))
		   (this-vars (cdr this)))
	       (let ((this-vals
		      (map (lambda (var)
			     (table-get vars->vals var
					(lambda (val) val)
					(lambda () (error "broken"))))
			   this-vars)))

	       (if (eq? this-type 'LET)
		   (let ((block (block/make block #t this-vars)))
		     (loop (cdr template)
			   block
			   (combination/optimizing-make
			    expression
			    block
			    (procedure/make
			     #f
			     block
			     lambda-tag:let
			     this-vars
			     '()
			     #f
			     code)
			    this-vals)))
		   (let ((block (block/make block #t this-vars)))
		     (loop (cdr template)
			   block
			   (open-block/make
			    (and expression (object/scode expression))
			    block this-vars this-vals
			    (append (make-list
				     (length this-vals)
				     open-block/value-marker)
				    (list code))
			    #t)))))))))))