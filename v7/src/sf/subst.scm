#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/sf/subst.scm,v 4.4 1988/11/05 22:14:02 cph Rel $

Copyright (c) 1988 Massachusetts Institute of Technology

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

(declare (usual-integrations)
	 (eta-substitution)
	 (open-block-optimizations)
	 (integrate-external "object" "lsets"))

(define *top-level-block*)

(define (integrate/get-top-level-block)
  *top-level-block*)

;;; Block names are added to this list so warnings can be more
;;; descriptive.
(define *current-block-names*)

(define (integrate/top-level block expression)
  (fluid-let ((*top-level-block* block)
	      (*current-block-names* '()))
    (process-block-flags (block/flags block)
      (lambda ()
	(let ((operations (operations/bind-block (operations/make) block))
	      (environment (environment/make)))
	  (if (open-block? expression)
	      (with-values
		  (lambda ()
		    (environment/recursive-bind
		     operations environment
		     (open-block/variables expression)
		     (open-block/values expression)))
	       (lambda (environment vals)
		 (values operations
			 environment
			 (quotation/make block
					 (integrate/open-block operations
							       environment
							       expression
							       vals)))))
	      (values operations
		      environment
		      (quotation/make block
				      (integrate/expression operations
							    environment
							    expression)))
	      ))))))

(define (operations/bind-block operations block)
  (let ((declarations (block/declarations block)))
    (if (null? declarations)
	(operations/shadow operations (block/bound-variables block))
	(with-values (lambda () (declarations/binders declarations))
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

(define *eager-integration-switch #f)

(define-method/integrate 'REFERENCE
  (lambda (operations environment expression)
    (let ((variable (reference/variable expression)))
      (operations/lookup operations variable
        (lambda (operation info)
	  (case operation
	    ((INTEGRATE-OPERATOR EXPAND)
	     (variable/reference! variable)
	     expression)
	    ((INTEGRATE)
	     (integrate/name expression info environment
			     (lambda (new-expression)
			       (variable/integrated! variable)
			       new-expression)
			     (lambda ()
			       (variable/reference! variable)
			       expression)))
	    (else (error "Unknown operation" operation))))
	(lambda ()
	  (if *eager-integration-switch
	      (integrate/name-if-safe expression environment
				      (lambda (new-expression)
					(variable/integrated! variable)
					new-expression)
				      (lambda ()
					(variable/reference! variable)
					expression))
	      (begin (variable/reference! variable)
		     expression)))))))

(define (integrate/name-if-safe reference environment if-win if-fail)
  (let ((variable (reference/variable reference)))
    (if (or (variable/side-effected variable)
	    (not (block/safe? (variable/block variable))))
	(if-fail)
	(let ((finish
	       (lambda (value)
		 (if (constant-value? value)
		     (if-win
		      (copy/expression/intern (reference/block reference)
					      value
					      #f))
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

(define (constant-value? value)
  (or (constant? value)
      (and (reference? value)
	   (not (variable/side-effected (reference/variable value)))
	   (block/safe? (variable/block (reference/variable value))))))

(define (integrate/reference-operator operations environment operator operands)
  (let ((variable (reference/variable operator)))
    (let ((dont-integrate
	   (lambda ()
	     (variable/reference! variable)
	     (combination/optimizing-make operator operands)))
	  (mark-integrated!
	   (lambda ()
	     (variable/integrated! variable))))
      (operations/lookup operations variable
        (lambda (operation info)
	  (case operation
	    ((#F) (dont-integrate))
	    ((INTEGRATE INTEGRATE-OPERATOR)
	     (integrate/name operator info environment
			     (lambda (operator)
			       (mark-integrated!)
			       (integrate/combination operations environment
						      operator
						      operands))
			     dont-integrate))
	    ((EXPAND)
	     (info operands
		   (lambda (new-expression)
		     (mark-integrated!)
		     (integrate/expression operations environment
					   new-expression))
		   dont-integrate
		   (reference/block operator)))
	    (else (error "Unknown operation" operation))))
	(lambda ()
	  (if *eager-integration-switch
	      (integrate/name-if-safe operator environment
				      (lambda (operator)
					(mark-integrated!)
					(integrate/combination operations
							       environment
							       operator
							       operands))
				      dont-integrate)
	      (dont-integrate)))))))

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
      (process-block-flags (block/flags (open-block/block expression))
        (lambda ()
	  (with-values
	      (lambda ()
		(environment/recursive-bind operations
					    environment
					    (open-block/variables expression)
					    (open-block/values expression)))
	   (lambda (environment vals)
	     (integrate/open-block operations environment expression
				   vals))))))))

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

(define (integrate/open-block operations environment expression values)
  (let ((actions
	 (integrate/actions operations environment
			    (open-block/actions expression)))
	(vars (open-block/variables expression)))
    ;; Complain about unreferenced variables.
    ;; If the block is unsafe, then it is likely that
    ;; there will be a lot of them on purpose (top level or
    ;; the-environment) so no complaining.
    (if (block/safe? (open-block/block expression))
	(for-each (lambda (variable)
		    (if (variable/unreferenced? variable)
			(warn "Unreferenced defined variable:"
			      (variable/name variable))))
		  vars))
    (if (open-block/optimized expression)
	(open-block/make (open-block/block expression) vars values actions #t)
	(open-block/optimizing-make (open-block/block expression)
				    vars
				    values
				    actions
				    operations
				    environment))))

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
  (let ((block    (procedure/block    procedure))
	(required (procedure/required procedure))
	(optional (procedure/optional procedure))
	(rest     (procedure/rest     procedure)))
    (fluid-let ((*current-block-names*
		 (cons (procedure/name procedure)
		       *current-block-names*)))
      (process-block-flags (block/flags block)
	(lambda ()
	  (let ((body
		 (integrate/expression (operations/bind-block operations block)
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
		(procedure/make block
				(procedure/name procedure)
				required
				optional
				rest
				body))))))))

(define (match-up? operands required)
  (cond ((null? operands) (null? required))
	((null? required) #f)
	(else (let ((this-operand  (car operands))
		    (this-required (car required)))
		(and (reference? this-operand)
		     (eq? (reference/variable this-operand) this-required)
		     (match-up? (cdr operands) (cdr required)))))))


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
       (with-values (lambda () (declarations/binders declarations))
	 (lambda (before-bindings after-bindings)
	   (integrate/expression (after-bindings (before-bindings operations))
				 environment
				 (declaration/expression declaration))))))))

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

;; Optimize (if () a b) => b; (if #t a b) => a

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
      (if (constant? predicate)
	  (if (null? (constant/value predicate))
	      alternative
	      consequent)
	  (conditional/make predicate consequent alternative)))))

;; Optimize (or () a) => a; (or #t a) => #t

(define-method/integrate 'DISJUNCTION
  (lambda (operations environment expression)
    (let ((predicate (integrate/expression operations environment
					   (disjunction/predicate expression)))
	  (alternative (integrate/expression
			operations environment
			(disjunction/alternative expression))))
      (if (constant? predicate)
	  (if (null? (constant/value predicate))
	      alternative
	      predicate)
	  (disjunction/make predicate alternative)))))

(define-method/integrate 'SEQUENCE
  (lambda (operations environment expression)
    ;; Optimize (begin (foo)) => (foo)
    ;; Optimize (begin a b (foo) 22 (bar)) => (begin (foo) (bar))
    (sequence/optimizing-make
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

(define (sequence/optimizing-make actions)
  (let ((actions (remove-non-side-effecting actions)))
    (if (null? (cdr actions))
	(car actions)
	(sequence/make actions))))

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
  (with-values
      (lambda ()
	(integrate/top-level (quotation/block quotation)
			     (quotation/expression quotation)))
    (lambda (operations environment expression)
      operations environment		;ignore
      expression)))

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

(define (integrate/name reference info environment if-integrated if-not)
  (let ((variable (reference/variable reference)))
    (let ((finish
	   (lambda (value uninterned)
	     (if-integrated
	      (copy/expression/intern (reference/block reference)
				      value
				      uninterned)))))
      (if info
	  (finish (integration-info/expression info)
		  (integration-info/uninterned-variables info))
	  (environment/lookup environment variable
	    (lambda (value)
	      (if (delayed-integration? value)
		  (if (delayed-integration/in-progress? value)
		      (if-not)
		      (finish (delayed-integration/force value) '()))
		  (finish value '())))
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
	      (set-delayed-integration/environment! delayed-integration false)
	      (set-delayed-integration/operations! delayed-integration false)
	      (set-delayed-integration/value! delayed-integration false)
	      (integrate/expression operations environment expression))))
       (set-delayed-integration/state! delayed-integration 'INTEGRATED)
       (set-delayed-integration/value! delayed-integration value)))
    ((INTEGRATED) 'DONE)
    ((BEING-INTEGRATED)
     (error "Attempt to re-force delayed integration" delayed-integration))
    (else
     (error "Delayed integration has unknown state" delayed-integration)))
  (delayed-integration/value delayed-integration))

;;;; Optimizations

(define combination/optimizing-make)
(let ()

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
  (cond ((constant? thing) (constant/value thing))
	(else (error "can't happen"))))

(define *foldable-primitive-procedures
  (map make-primitive-procedure
       '(PRIMITIVE-TYPE PRIMITIVE-TYPE?
         NOT EQ? NULL? PAIR? ZERO? POSITIVE? NEGATIVE?
	 &= &< &> &+ &- &* &/ INTEGER-DIVIDE 1+ -1+
	 TRUNCATE ROUND FLOOR CEILING
	 SQRT EXP LOG SIN COS &ATAN)))
(define (foldable-operator? operator)
  (and (constant? operator)
       (primitive-procedure? (constant/value operator))
       (memq (constant/value operator) *foldable-primitive-procedures)))

;;; deal with (let () (define ...))
;;; deal with (let ((x 7)) (let ((y 4)) ...)) => (let ((x 7) (y 4)) ...)
;;; Actually, we really don't want to hack with these for various
;;; reasons

(set! combination/optimizing-make
  (lambda (operator operands)
    (cond (
	   ;; fold constants
	   (and (foldable-operator? operator)
		(foldable-constants? operands))
	   (constant/make (apply (constant/value operator)
				 (map foldable-constant-value operands))))

	  (
	   ;; (force (delay x)) ==> x
	   (and (constant? operator)
		(eq? (constant/value operator) force)
		(= (length operands) 1)
		(delay? (car operands)))
	   (delay/expression (car operands)))

	  ((and (procedure? operator)
		(null? (procedure/optional operator))
		(not (procedure/rest operator))
		(block/safe? (procedure/block operator)))
	   (delete-unreferenced-parameters
	    (procedure/required operator)
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
			 (procedure/body operator)
			 (combination/make
			  (procedure/make
			   (procedure/block operator)
			   (procedure/name operator)
			   required
			   '()
			   false
			   (procedure/body operator))
			  referenced-operands))))
		(if (null? unreferenced-operands)
		    form
		    (sequence/optimizing-make
		     (append unreferenced-operands (list form))))))))
	  (else
	   (combination/make operator operands)))))

(define (delete-unreferenced-parameters parameters body operands receiver)
  (let ((free-in-body (free/expression body)))
    (let loop ((parameters 		parameters)
	       (operands   		operands)
	       (required-parameters	'())
	       (referenced-operands	'())
	       (unreferenced-operands	'()))
    (cond ((null? parameters)
	   (if (null? operands)
	       (receiver (reverse required-parameters) ; preserve order
			 (reverse referenced-operands)
			 unreferenced-operands)
	       (error "Argument mismatch" operands)))
	  ((null? operands)
	   (error "Argument mismatch" parameters))
	  (else (let ((this-parameter (car parameters))
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
			       (cons this-operand unreferenced-operands))))))))
      ))


;;; end COMBINATION/OPTIMIZING-MAKE
)


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

(define open-block/optimizing-make)

(let ()

(set! open-block/optimizing-make
  (named-lambda (open-block/optimizing-make block vars values actions
					    operations environment)
  (if (and *block-optimizing-switch
	   (block/safe? block))
      (let ((table:var->vals (associate-vars-and-vals vars values))
	    (bound-variables (varlist->varset vars)))
	(let ((table:vals->free
	       (get-free-vars-in-bindings bound-variables values))
	      (body-free  (get-body-free-vars bound-variables actions)))
;	  (write-string "Free vars in body")
;	  (display (map variable/name body-free))
	  (let ((graph (build-graph vars
				    table:var->vals
				    table:vals->free
				    body-free)))
	    (collapse-circularities! graph)
	    ;(print-graph graph)
	    (label-node-depth! graph)
	    (let ((template (linearize graph)))
	     ; (print-template template)
	      (integrate/expression
	       operations
	       environment (build-new-code template
			       (block/parent block)
			       table:var->vals actions))))))
      (open-block/make block vars values actions #t))))

(define (print-template template)
  (if (null? template)
      '()
      (let ((this (car template)))
	(newline)
	(display (car this))
	(display (map variable/name (cdr this)))
	(print-template (cdr template)))))

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
  (vars false read-only true)
  (needs (empty-nodeset))
  (needed-by (empty-nodeset))
  (depth false))

(define-integrable (make-base-node)
  (%make-node 'BASE (empty-varset)))

(define-integrable (variable->node variable)
  (%make-node 'SETUP (singleton-varset variable)))

(define-integrable (make-letrec-node variable-set)
  (%make-node 'LETREC variable-set))

(declare (integrate add-node-need!
		    remove-node-need!
		    add-node-needed-by!
		    remove-node-needed-by!))

(define (add-node-need! needer what-i-need)
  (declare (integrate what-i-need))
  (set-%node-needs! needer (set/adjoin (%node-needs needer) what-i-need)))

(define (remove-node-need! needer what-i-no-longer-need)
  (declare (integrate what-i-no-longer-need))
  (set-%node-needs! needer
		    (set/remove (%node-needs needer) what-i-no-longer-need)))

(define (add-node-needed-by! needee what-needs-me)
  (declare (integrate what-needs-me))
  (set-%node-needed-by! needee
			(set/adjoin (%node-needed-by needee) what-needs-me)))

(define (remove-node-needed-by! needee what-needs-me)
  (declare (integrate what-needs-me))
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

(declare (integrate link-2-nodes!))

(define (link-2-nodes! from-node to-node)
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

(declare (integrate unlink-nodes!))

(define (unlink-nodes! nodelist)
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
      (set/for-each (lambda (need) (link-2-nodes! letrec-node need)) needs-set)
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

(define (build-new-code template parent vars->vals actions)
  (let ((body (sequence/optimizing-make (get-body actions))))
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
		   (let ((block (block/make block true)))
		     (set-block/bound-variables! block this-vars)
		     (loop (cdr template)
			   block
			   (combination/optimizing-make
			    (procedure/make
			     block
			     lambda-tag:let
			     this-vars
			     '()
			     false
			     code)
			    this-vals)))
		   (let ((block (block/make block true)))
		     (set-block/bound-variables! block this-vars)
		     (loop (cdr template)
			   block
			   (open-block/make
			    block this-vars this-vals
			    (append (make-list
				     (length this-vals)
				     open-block/value-marker)
				    (list code))
			    #t)))))))))))

;; End of OPEN-BLOCK/OPTIMIZING-MAKE
)