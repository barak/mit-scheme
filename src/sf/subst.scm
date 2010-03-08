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

(define (ignored-variable-warning name)
  (warn (string-append "Variable \""
		       (symbol->string name)
		       "\" was declared IGNORE, but used anyway.")
	name *current-block-names*))

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

(define (integrate/actions operations environment actions)
  (let ((action (car actions)))
    (if (null? (cdr actions))
	(list (if (eq? action open-block/value-marker)
		  action
		  (integrate/expression operations environment action)))
	(cons (cond ((eq? action open-block/value-marker)
		     action)
		    (else
		     (integrate/expression operations environment action)))
	      (integrate/actions operations environment (cdr actions))))))

(define (integrate/expression operations environment expression)
  ((expression/method dispatch-vector expression)
   operations environment expression))

(define dispatch-vector
  (expression/make-dispatch-vector))

(define define-method/integrate
  (expression/make-method-definer dispatch-vector))

;;;; ACCESS
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

;;;; ASSIGNMENT
(define-method/integrate 'ASSIGNMENT
  (lambda (operations environment assignment)
    (let ((variable (assignment/variable assignment)))
      (operations/lookup operations variable
	(lambda (operation info)
	  info				;ignore
	  (case operation
	    ((IGNORE)
	     (ignored-variable-warning (variable/name variable)))
	    ((EXPAND INTEGRATE INTEGRATE-OPERATOR)
	     (warn "Attempt to assign integrated name"
		   (variable/name variable)))
	    (else (error "Unknown operation" operation))))
			 false-procedure)

      (variable/reference! variable)
      (assignment/make (assignment/scode assignment)
		       (assignment/block assignment)
		       variable
		       (integrate/expression operations
					     environment
					     (assignment/value assignment))))))

;;;; COMBINATION
(define-method/integrate 'COMBINATION
  (lambda (operations environment combination)
    (integrate/combination
     combination operations environment
     (combination/block combination)
     (combination/operator combination)
     (integrate/expressions operations
			    environment
			    (combination/operands combination)))))

;;;; CONDITIONAL

(define-method/integrate 'CONDITIONAL
  (lambda (operations environment expression)
    (integrate/conditional operations environment expression
			   (integrate/expression
			    operations environment
			    (conditional/predicate expression))
			   (conditional/consequent expression)
			   (conditional/alternative expression))))

(define sf:enable-conditional-folding? #t)

;; If true, then when a conditional depends on a variable,
;; and that variable is not side effected and has no declarations,
;; we declare the variable to be integrable to a constant #F
;; in the alternative branch.
(define sf:enable-conditional-propagation? #t)

(define (integrate/conditional operations environment expression
			       integrated-predicate
			       consequent
			       alternative)
  (cond ((and (expression/never-false? integrated-predicate)
	      (noisy-test sf:enable-conditional-folding? "Fold constant true conditional"))
	 (let ((integrated-consequent (integrate/expression operations environment consequent)))
	   (if (expression/effect-free? integrated-predicate)
	       integrated-consequent
	       (sequence/make (and expression (conditional/scode expression))
			      (list integrated-predicate integrated-consequent)))))

	((and (expression/always-false? integrated-predicate)
	      (noisy-test sf:enable-conditional-folding? "Fold constant false conditional"))
	 (let ((integrated-alternative (integrate/expression operations environment alternative)))
	   (if (expression/effect-free? integrated-predicate)
	       integrated-alternative
	       (sequence/make (and expression (conditional/scode expression))
			      (list integrated-predicate integrated-alternative)))))

	((and (expression/call-to-not? integrated-predicate)
	      (noisy-test sf:enable-conditional-inversion? "Invert conditional"))
	 ;; (if (not <e1>) <e2> <e3>) => (if <e1> <e3> <e2>)
	 (integrate/conditional operations environment expression
				(first (combination/operands integrated-predicate))
				alternative consequent))

	((and (reference? integrated-predicate)
	      (variable/safely-integrable? (reference/variable integrated-predicate) operations)
	      (noisy-test sf:enable-conditional-propagation? "Propagating conditional information"))
	 (let ((icons (integrate/expression operations environment consequent))
	       (ialt (integrate/expression
		      (operations/bind operations
				       'INTEGRATE
				       (reference/variable integrated-predicate)
				       (make-integration-info (constant/make #f #f)))
		      environment
		      alternative)))
	   (conditional/make (and expression (conditional/scode expression)) integrated-predicate icons ialt)))

	(else
	 (conditional/make (and expression (conditional/scode expression))
			   integrated-predicate
			   (integrate/expression operations environment consequent)
			   (integrate/expression operations environment alternative)))))

;;; CONSTANT
(define-method/integrate 'CONSTANT
  (lambda (operations environment expression)
    (declare (ignore operations environment))
    expression))

;;; DECLARATION
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

;;; DELAY
(define-method/integrate 'DELAY
  (lambda (operations environment expression)
    (delay/make
     (delay/scode expression)
     (integrate/expression operations environment
			   (delay/expression expression)))))


;;; DISJUNCTION
(define-method/integrate 'DISJUNCTION
  (lambda (operations environment expression)
    (integrate/disjunction
     operations environment expression
     (integrate/expression
      operations environment (disjunction/predicate expression))
     (disjunction/alternative expression))))

(define sf:enable-disjunction-folding? #t)
(define sf:enable-disjunction-inversion? #t)
(define sf:enable-disjunction-linearization? #t)
(define sf:enable-rewrite-conditional-in-disjunction? #t)

(define (integrate/disjunction operations environment expression
			       integrated-predicate alternative)
  ;; Predicate has been integrated, but alternative has not.
  ;; We can use information from the predicate to help in
  ;; integrating the alternative.
  (cond ((and (expression/never-false? integrated-predicate)
	      (noisy-test sf:enable-disjunction-folding? "Folding constant true disjunction"))
	 ;; (or <exp1> <exp2>) => <exp1> if <exp1> is never false
	 integrated-predicate)

	((and (expression/call-to-not? integrated-predicate)
	      (noisy-test sf:enable-disjunction-inversion? "Inverting disjunction"))
	 ;; (or (not e1) e2) => (if e1 e2 #t)
	 (integrate/conditional operations environment expression
				(first (combination/operands integrated-predicate))
				alternative
				(constant/make #f #t)))

	((and (expression/always-false? integrated-predicate)
	      (noisy-test sf:enable-disjunction-folding? "Folding constant false disjunction"))
	 ;; (or <exp1> <exp2>) => (begin <exp1> <exp2>) if <exp1> is always false
	 (let ((integrated-alternative (integrate/expression operations environment alternative)))
	   (if (expression/effect-free? integrated-predicate)
	       integrated-alternative
	       (sequence/make (and expression (object/scode expression))
			      (list integrated-predicate
				    integrated-alternative)))))

	((and (conditional? integrated-predicate)
	      (noisy-test sf:enable-rewrite-conditional-in-disjunction?
			  "Rewriting conditional within disjunction."))
	 (integrate/conditional-in-disjunction
	  operations environment expression
	  integrated-predicate alternative))

	((and (disjunction? integrated-predicate)
	      (noisy-test sf:enable-disjunction-linearization? "Linearizing disjunction"))
	 ;; (or (or <e1> <e2>) <e3>) => (or <e1> (or <e2> <e3>))
	 (disjunction/linearize operations environment expression
				(disjunction/predicate integrated-predicate)
				(disjunction/alternative integrated-predicate)
				alternative))

	((and (reference? integrated-predicate)
	      (variable/safely-integrable? (reference/variable integrated-predicate) operations)
	      (noisy-test sf:enable-conditional-propagation? "Propagating conditional information"))
	 ;; If <e1> is a reference, then <e1> must be #f in the alternative.
	 (disjunction/make (and expression (object/scode expression))
			   integrated-predicate
			   (integrate/expression
			    (operations/bind operations
					     'INTEGRATE
					     (reference/variable integrated-predicate)
					     (make-integration-info (constant/make #f #f)))
			    environment
			    alternative)))

	(else
	 (disjunction/make (and expression (object/scode expression))
			   integrated-predicate
			   (integrate/expression operations environment alternative)))))

(define (disjunction/linearize operations environment expression e1 e2 alternative)
  ;; (or (or <e1> <e2>) <alternative>) => (or <e1> (or <e2> <alternative>))
  ;; We don't make anoter pass through integrate/disjunction here
  ;; because the inner disjunction has already been integrated and there
  ;; is no further optimization to be done.
  (disjunction/make
   (and expression (object/scode expression))
   e1
   (integrate/disjunction
    (if (and (reference? e1)
	     (variable/safely-integrable? (reference/variable e1) operations)
	     (noisy-test sf:enable-conditional-propagation? "Propagating conditional information"))
	(operations/bind operations
			 'INTEGRATE
			 (reference/variable e1)
			 (make-integration-info (constant/make #f #f)))
	operations)
    environment #f e2 alternative)))

(define (integrate/conditional-in-disjunction
	  operations environment expression
	  integrated-predicate
	  alternative)
  (let ((e1 (conditional/predicate integrated-predicate))
	(e2 (conditional/consequent integrated-predicate))
	(e3 (conditional/alternative integrated-predicate)))

  ;; (or (if e1 e2 e3) alternative) =>
  ;;    (if e1 (or e2 alternative) (or e3 alternative))
  ;; provided alternative can be duplicated, or e2 or e3 are
  ;; such that alternative doesn't need to be duplicated.
  ;;
  ;; e1 e2 and e3 have been integrated, alternative has not.

  (cond ((or (expression/never-false? e2)
	     (expression/unspecific? e2))
	 ;; If e2 is never false, then we can rewrite like this:
	 ;; (if e1 e2 (or e3 alternative))
	 (conditional/make (and expression (object/scode expression))
			   e1
			   e2
			   (integrate/disjunction
			    ;; alternative is only taken when e1 is false
			    (if (and (reference? e1)
				     (variable/safely-integrable? (reference/variable e1) operations)
				     (noisy-test sf:enable-conditional-propagation? "Propagating conditional information"))
				(operations/bind operations
						 'INTEGRATE
						 (reference/variable e1)
						 (make-integration-info (constant/make #f #f)))
				operations)
			    environment
			    #f
			    e3 alternative)))

	((or (expression/never-false? e3)
	     (expression/unspecific? e3))
	 ;; If e3 is never false, then we can rewrite like this:
	 ;; (if e1 (or e2 alternative) e3)
	 (conditional/make (and expression (object/scode expression))
			   e1
			   (integrate/disjunction operations environment #f e2 alternative)
			   e3))
	(else
	 ;; See if we can duplicate the alternative.
	 (let ((e4 (integrate/expression operations environment alternative)))
	   (if (expression/can-duplicate? e4)
	       (conditional/make
		(and expression (object/scode expression))
		e1
		;; Consequent clause of new conditional
		;; (or e2 alternative)
		;; if e2 is always false, construct a sequence.
		(if (expression/always-false? e2)
		    (if (expression/effect-free? e2) e4 (sequence/make #f (list e2 e4)))
		    (disjunction/make #f
				      e2
				      ;; if e2 is a variable that appears in e4,
				      ;; re-integrate e4 to eliminate it as a known false.
				      (if (and (reference? e2)
					       (variable/safely-integrable? (reference/variable e2) operations)
					       (expression/free-variable? e4 (reference/variable e2))
					       (noisy-test sf:enable-conditional-propagation? "Propagating conditional information"))
					  (integrate/expression
					   (operations/bind operations
							    'INTEGRATE
							    (reference/variable e2)
							    (make-integration-info (constant/make #f #f)))
					   environment
					   e4)
					  e4)))
		;; Alternative clause of new conditional
		;; (or e3 alternative)
		;; first see if e1 is a variable that appears in e4
		(cond ((and (reference? e1)
			    (variable/safely-integrable? (reference/variable e1) operations)
			    (expression/free-variable? e4 (reference/variable e1))
			    (noisy-test sf:enable-conditional-propagation? "Propagating condition information"))
		       ;; re-integrate e4 to take advantage of information about e1
		       (let ((e4b (integrate/expression
				   (operations/bind
				    (if (and (reference? e3)
					     (variable/safely-integrable? (reference/variable e3) operations)
					     (expression/free-variable? e4 (reference/variable e3)))
					(operations/bind operations
							 'integrate
							 (reference/variable e3)
							 (make-integration-info (constant/make #f #f)))
					operations)
				    'integrate
				    (reference/variable e1)
				    (make-integration-info (constant/make #F #F)))
				   environment
				   e4)))
			 (if (expression/always-false? e3)
			     (if (expression/effect-free? e3) e4b (sequence/make #f (list e3 e4b)))
			     (disjunction/make #f e3 e4b))))
		      ((expression/always-false? e3)
		       (if (expression/effect-free? e3) e4 (sequence/make #f (list e3 e4))))
		      (else (disjunction/make #f
					      e3
					      ;; if e3 is a variable that appears in e4,
					      ;; re-integrate e4 to eliminate it as a known false.
					      (if (and (reference? e3)
						       (variable/safely-integrable? (reference/variable e3) operations)
						       (expression/free-variable? e4 (reference/variable e3))
						       (noisy-test sf:enable-conditional-propagation? "Propagating conditional information"))
						  (integrate/expression
						   (operations/bind operations
								    'INTEGRATE
								    (reference/variable e3)
								    (make-integration-info (constant/make #f #f)))
						   environment
						   e4)
						  e4)))))
	       ;; can't rewrite.
	       (disjunction/make (and expression (object/scode expression))
				 integrated-predicate
				 e4)))))))

;;; OPEN-BLOCK
(define-method/integrate 'OPEN-BLOCK
  (lambda (operations environment expression)
    (call-with-values
	(lambda () (integrate/open-block operations environment expression))
      (lambda (operations environment expression)
	(declare (ignore operations environment))
	expression))))

;;; PROCEDURE
(define-method/integrate 'PROCEDURE
  (lambda (operations environment procedure)
    (integrate/procedure operations
			 (simulate-unknown-application environment procedure)
			 procedure)))

;;;; Quotation
(define-method/integrate 'QUOTATION
  (lambda (operations environment expression)
    (declare (ignore operations environment))
    (integrate/quotation expression)))

(define (integrate/quotation quotation)
  (call-with-values
      (lambda ()
	(integrate/top-level* (quotation/scode quotation)
			      (quotation/block quotation)
			      (quotation/expression quotation)))
    (lambda (operations environment expression)
      operations environment		;ignore
      expression)))

;;;; Reference
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
	     ((IGNORE)
	      (ignored-variable-warning (variable/name variable))
	      (integration-failure))
	     ((EXPAND INTEGRATE-OPERATOR)
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

;;; SEQUENCE
(define-method/integrate 'SEQUENCE
  (lambda (operations environment expression)
    ;; Optimize (begin (foo)) => (foo)
    ;; Optimize (begin a b (foo) 22 (bar)) => (begin (foo) (bar))
    (sequence/optimizing-make
     expression
     (integrate/actions operations environment
			(sequence/actions expression)))))

;;; THE-ENVIRONMENT
(define-method/integrate 'THE-ENVIRONMENT
  (lambda (operations environment expression)
    operations
    environment
    expression))


;;;; Binding

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

(define (variable/unreferenced? variable)
  (and (not (variable/integrated variable))
       (not (variable/referenced variable))
       (not (variable/may-ignore? variable))
       (not (variable/must-ignore? variable))))

(define (variable/safely-integrable? variable operations)
  (guarantee-variable variable 'variable/safely-integrable?)
  (and (not (variable/side-effected variable))
       (block/safe? (variable/block variable))
       (operations/lookup operations variable false-procedure true-procedure)))

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


;;; INTEGRATE-COMBINATION
(define integrate-combination-dispatch-vector
  (expression/make-dispatch-vector))

(define define-method/integrate-combination
  (expression/make-method-definer integrate-combination-dispatch-vector))

(define (integrate/combination expression operations environment
			       block operator operands)
  ((expression/method integrate-combination-dispatch-vector operator)
   expression operations environment block operator operands))

;;;; access-operator
(define-method/integrate-combination 'ACCESS
  (lambda (expression operations environment block operator operands)
    (integrate/access-operator expression operations environment
			       block operator operands)))

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
	     ((#F) (dont-integrate))

	     ((EXPAND)
	      (cond ((info expression operands (reference/block operator))
		     => (lambda (new-expression)
			  (integrate/expression operations environment new-expression)))
		    (else (dont-integrate))))

	     ((IGNORE)
	      (ignored-variable-warning (variable/name name))
	      (dont-integrate))

	     ((INTEGRATE INTEGRATE-OPERATOR)
	      (let ((new-operator
		     (reassign operator
			       (copy/expression/intern block (integration-info/expression info)))))
		(integrate/combination expression operations environment block new-operator operands)))

	     (else
	      (error "unknown operation" operation))))
	 dont-integrate))))

;;; assignment-operator
(define-method/integrate-combination 'ASSIGNMENT
  (lambda (expression operations environment block operator operands)
    (warn "Value of assignment used as an operator.")
    ;; We don't try to make sense of this, we just
    ;; build the code and let the runtime raise an error.
    (combination/make expression
		      block
		      (integrate/expression operations environment operator)
		      operands)))

;;; combination-operator
(define-method/integrate-combination 'COMBINATION
  (lambda (expression operations environment block operator operands)
    (integrate-combination/default expression operations environment block operator operands)))

;;; conditional-operator
(define-method/integrate-combination 'CONDITIONAL
  (lambda (expression operations environment block operator operands)
    (integrate-combination/default expression operations environment block operator operands)))

;;; constant-operator
(define sf:enable-elide-double-negatives? #t)

(define-method/integrate-combination 'CONSTANT
  (lambda (expression operations environment block operator operands)
    ;; Elide a double negative only if it doesn't change the type of the answer.
    (cond ((and (expression/constant-eq? operator (ucode-primitive not))
		(length=? operands 1)
		(expression/call-to-not? (first operands))
		(expression/boolean? (first (combination/operands (first operands))))
		(noisy-test sf:enable-elide-double-negatives? "Eliding double negative"))
	   (first (combination/operands (first operands))))
	  ((primitive-procedure? (constant/value operator))
	   (let ((operands*
		  (and (eq? (constant/value operator) (ucode-primitive apply))
		       (integrate/hack-apply? operands))))
	     (if operands*
		 (integrate/combination expression operations environment
					block (car operands*) (cdr operands*))
		 (integrate/primitive-operator expression operations environment
					       block operator operands))))
	  (else
	   (warn "Application of constant value" (constant/value operator))
	   (integrate-combination/default expression operations environment block operator operands)))))

(define (integrate/primitive-operator expression operations environment
				      block operator operands)
  (declare (ignore operations environment))
  (combination/make expression block operator operands))

;;; declaration-operator
(define-method/integrate-combination 'DECLARATION
  (lambda (expression operations environment block operator operands)
    (integrate-combination/default expression operations environment block operator operands)))

;;; delay-operator
(define-method/integrate-combination 'DELAY
  (lambda (expression operations environment block operator operands)
    ;; Nonsense - generate a warning.
    (warn "Delayed object in operator position.  This will cause a runtime error.")
    (combination/make expression
		      block
		      (integrate/expression operations environment operator)
		      operands)))

;;; disjunction-operator
(define-method/integrate-combination 'DISJUNCTION
  (lambda (expression operations environment block operator operands)
    (integrate-combination/default expression operations environment block operator operands)))

;;; open-block-operator
(define-method/integrate-combination 'OPEN-BLOCK
  (lambda (expression operations environment block operator operands)
    (declare (ignore expression operations environment block operator operands))
    ;; This shouldn't be possible.
    (error "INTERNAL-ERROR: integrate-combination 'open-block")))

;;; procedure-operator (let)
(define-method/integrate-combination 'PROCEDURE
  (lambda (expression operations environment block operator operands)
    (integrate-combination/default expression operations environment block operator operands)))

(define (integrate/procedure-operator operations environment
				      block procedure operands)
  (integrate/procedure operations
		       (simulate-application environment block
					     procedure operands)
		       procedure))

;;; quotation-operator
(define-method/integrate-combination 'QUOTATION
  (lambda (expression operations environment block operator operands)
    (integrate-combination/default expression operations environment block operator operands)))

;;; reference-operator
(define-method/integrate-combination 'REFERENCE
  (lambda (expression operations environment block operator operands)
    (integrate/reference-operator expression operations environment
				  block operator operands)))

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

	    ((IGNORE)
	     (ignored-variable-warning (variable/name variable))
	     (integration-failure))

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

;;; sequence-operator
(define-method/integrate-combination 'SEQUENCE
  (lambda (expression operations environment block operator operands)
    (integrate-combination/default expression operations environment block operator operands)))

;;; the-environment-operator
(define-method/integrate-combination 'THE-ENVIRONMENT
  (lambda (expression operations environment block operator operands)
    (warn "(THE-ENVIRONMENT) used as an operator.  Will cause a runtime error.")
    (combination/make expression block
		      (integrate/expression operations environment operator)
		      operands)))

(define (integrate-combination/default expression operations environment
				       block operator operands)
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
   operands))

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

(define (constant/system-global-environment? expression)
  (and (constant? expression)
       (system-global-environment? (constant/value expression))))

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