#| -*-Scheme-*-

$Id: closan.scm,v 4.29 2001/11/05 18:57:11 cph Exp $

Copyright (c) 1987-1991, 1998, 1999, 2001 Massachusetts Institute of Technology

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

;;;; Closure Analysis
;;; package: (compiler fg-optimizer closure-analysis)

(declare (usual-integrations))

;;; Theory behind this analysis.
;;;
;;; The closure analysis is really performed in two parts.  The first
;;; part is called "environment optimization", and is in
;;; "fgopt/envopt.scm".  The second part is called "closure analysis"
;;; and is in this file.  This naming is misleading, because only part
;;; of the environment optimization is in "envopt", while the code
;;; here implements a combination of closure analysis and environment
;;; optimization.  It would be more appropriate to merge the two files
;;; together.
;;;
;;; The analysis performed here consists of two basic operations.  The
;;; first operation is to decide what procedures need to be closures,
;;; and conversely what procedures may be closed over the stack.
;;; These latter are called "open" procedures.  We will call this
;;; operation "closure analysis".
;;;
;;; The second operation is to reorganize the environment "genealogy"
;;; in order to minimize the number of procedures that need to be
;;; closed.  We will call this operation "environment optimization".
;;;
;;; The closure analysis uses a handful of straightforward rules to
;;; decide whether to close a procedure.  These rules fall into two
;;; general classes: (1) The procedure is stored or invoked in a place
;;; where it's ancestor environments aren't available on the stack;
;;; (2) The procedure is invoked from a place in which other
;;; procedures are also invoked, and so must be closed to provide a
;;; uniform calling interface amongst the procedures.
;;;
;;; The environment analysis is considerably more complex.  The basic
;;; algorithm is to take each procedure and raise it up in the
;;; environment heirarchy so that its closing environment is the
;;; top-level environment.  Then, a number of constraints are applied
;;; to the procedure, which together have the effect of pulling it
;;; back down the heirarchy as far as is necessary to satisfy the
;;; constraints.
;;;
;;; There are three basic constraints applied: (1) The closing
;;; environment must contain bindings for all of the free variables
;;; referenced by the procedure.  (2) The invocation environment of
;;; the procedure must include the closing environment of any
;;; procedure called from this procedure.  (3) Any environment in
;;; which this procedure is stored must include the closing
;;; environment of this procedure.  If all of these constraints are
;;; satisfied, then the procedure may remain open; otherwise it must
;;; be closed.

;;; The constraints are applied as a relaxation process in which the
;;; closing environments are slowly moved down the tree.  At points
;;; where it is discovered that it's not possible to satisfy the
;;; constraints for a given procedure, that procedure is closed.
;;; Closing of a procedure changes some of the constraints that depend
;;; on that procedure, allowing other constrained procedures to rise
;;; back up on the environment tree.
;;;
;;; There are several problems with this analysis, some of which are
;;; fundamental to the analysis, and some of which have to do with the
;;; implementation.  I will examine the implementation first.
;;;
;;; This implementation doesn't have a clear structure in which the
;;; constraints are properly reified and applied.  Instead, the
;;; application of the constraints is somewhat ad hoc, and what is
;;; reified is the end result of the constraint application rather
;;; than the constraint itself.  This means that the actual
;;; constraints are embedded in the code rather than being stated as
;;; data structures that can be reflected on.  Consequently there are
;;; several places where a given constraint is applied, and in each
;;; place the application is slightly different.
;;;
;;; A further problem is that several of the constraints are applied
;;; non-locally.  For example, places in which caller/callee
;;; relationships are analyzed look at the transitive closure of the
;;; caller/callee graph.  Instead, they should only examine the direct
;;; relationships.  This has two undesirable effects.  The first is
;;; that the optimization suffers, because the relationships are
;;; overconstrained.  The second is that the transitive closure
;;; depends on the _current_ state of the environment heirarchy, which
;;; means that there is feedback in the relaxation process.  This
;;; feedback makes analysis of the algorithm very difficult.
;;;
;;; This brings up my final point about the implementation.  The
;;; entire relaxation process involves a great deal of undiscipled
;;; feedback.  In addition to the transitive closure problem, there is
;;; also the ad hoc intermixing of constraint application and closing
;;; of procedures.  The end result of all of this is that the specific
;;; optimization computed here can't be described, because it depends
;;; on the specific order in which each action occurs.  And this in
;;; turn depends on the specific order in which procedures appear in
;;; lists, and the order in which MAP iterates over its argument list.
;;;
;;; In short, there's no way to prove that this implementation will
;;; converge, or that if it does it will result in correct code.
;;;
;;; An alternate strategy would be as follows.
;;;
;;; (1) Write down all of the constraints on the code as a set of
;;; equations.  Some of these equations would be conditional, in that
;;; they depend on things other than the environment heirarchy, such
;;; as whether or not a procedure is closed.
;;;
;;; (2) Raise up all of the procedures to the top level.
;;;
;;; (3) Apply all of the constraints to generate a new set of closing
;;; blocks.  Note that this computation must be based entirely on the
;;; old set of closing blocks, and thus has no feedback.
;;;
;;; (4) Any constraints that are left unsatisfied indicate procedures
;;; that may need to be closed.  Close one or more of them.
;;;
;;; (5) Continue iterating (3) and (4) until all of the constraints
;;; are satisfied, then modify the closing blocks of all of the
;;; procedures to the new ones implied by the constraints.
;;;
;;; This strategy has the major advantage that it is clear.  The
;;; absence of feedback means that the analysis can be understood, and
;;; it can be proven that this algorithm converges.
;;;
;;; The only real uncertainty is how effective the algorithm will be,
;;; and that is determined solely by step (4).  The choice of
;;; procedures to close isn't obvious.  A conservative choice would be
;;; to close all of them.  This will certainly result in correct code,
;;; but it is almost certainly non-optimal.  And this is the
;;; fundamental problem with this analysis: it is attempting to search
;;; a large space that doesn't have a known metric.  Consequently it
;;; is difficult to determine how to make it work well.

(define (identify-closure-limits! procs&conts applications lvalues)
  (let ((procedures
	 (delete-matching-items procs&conts procedure-continuation?))
	(combinations
	 (keep-matching-items applications application/combination?)))
    (for-each (lambda (procedure)
		(set-procedure-variables! procedure '()))
	      procedures)
    (for-each
     (lambda (lvalue)
       (if (lvalue/variable? lvalue)
	   (for-each (lambda (value)
		       (if (rvalue/procedure? value)
			   (set-procedure-variables!
			    value
			    (cons lvalue (procedure-variables value)))))
		     (lvalue-values lvalue))))
     lvalues)
    (for-each
     (lambda (combination)
       (let ((values
	      (let ((operands (application-operands combination)))
		(if (pair? operands)
		    (eq-set-union* (rvalue-values (car operands))
				   (map rvalue-values (cdr operands)))
		    '()))))
	 (set-application-operand-values! combination values)
	 (for-each (lambda (value)
		     (if (rvalue/true-procedure? value)
			 (set-procedure-virtual-closure?! value #t)))
		   values))
       (set-combination/model!
	combination
	(rvalue-known-value (combination/operator combination))))
     combinations)
    (undrift-procedures!
     (fluid-let ((*undrifting-constraints* '()))
       (with-new-node-marks
	(lambda ()
	  (transitive-closure
	   (lambda ()
	     (for-each
	      (lambda (procedure)
		(if (procedure-passed-out? procedure)
		    (close-procedure! procedure 'PASSED-OUT #f)
		    (analyze-procedure
		     procedure
		     (procedure-closing-block procedure))))
	      procedures))
	   analyze-combination
	   combinations)))
       *undrifting-constraints*))))

(define (analyze-procedure procedure block)
  (for-each (lambda (variable)
	      ;; If this procedure is the value of a variable that is
	      ;; bound in a non-descendant block, we must close it.
	      (if (not (procedure-closure-context procedure))
		  (close-if-unreachable! (variable-block variable) block
					 (make-condition procedure
							 'EXPORTED
							 variable
							 '()))))
	    (procedure-variables procedure)))

(define (analyze-combination combination)
  (let* ((operator (combination/operator combination))
	 (proc (rvalue-known-value operator))
	 (procs (rvalue-values operator)))
    (cond ((rvalue-passed-in? operator)
	   ;; We don't need to close the operands because they have
	   ;; been marked as passed out already.
	   (close-values! (rvalue-values operator)
			  'APPLY-COMPATIBILITY
			  combination))
	  ((null? procs)
	   ;; This is the NOP node case.  This combination should not
	   ;; be executed, so it should have no effect on any items
	   ;; involved in it.
	   unspecific)
	  ((not proc)
	   (let ((class (compatibility-class procs))
		 (model (car procs)))
	     (set-combination/model! combination
				     (if (eq? class 'APPLY-COMPATIBILITY)
					 #f
					 model))
	     (if (eq? class 'POTENTIAL)
		 (for-each (lambda (proc)
			     (set-procedure-virtual-closure?! proc #t))
			   procs)
		 (begin
		   (close-values! (rvalue-values operator) class combination)
		   (close-combination-arguments! combination)))))
	  ((or (not (rvalue/procedure? proc))
	       (procedure-closure-context proc))
	   (close-combination-arguments! combination)))))

(define (compatibility-class procs)
  (if (for-all? procs rvalue/procedure?)
      (let* ((model (car procs))
	     (model-env (procedure-closing-block model)))
	(call-with-values (lambda () (procedure-arity-encoding model))
	  (lambda (model-min model-max)
	    (let loop
		((procs (cdr procs))
		 (class
		  (if (or (procedure/closure? model)
			  (pending-undrifting? model))
		      ;; Cop-out.  Could be postponed until later.
		      'COMPATIBILITY
		      'POTENTIAL)))
	      (if (pair? procs)
		  (let ((this (car procs)))
		    (call-with-values
			(lambda () (procedure-arity-encoding this))
		      (lambda (this-min this-max)
			(if (and (= model-min this-min)
				 (= model-max this-max))
			    (loop (cdr procs)
				  (if (and (not (procedure/closure? this))
					   (eq? (procedure-closing-block this)
						model-env)
					   (not (pending-undrifting? this)))
				      class
				      'COMPATIBILITY))
			    'APPLY-COMPATIBILITY))))
		  class)))))
      'APPLY-COMPATIBILITY))

(define (close-combination-arguments! combination)
  (if (not (node-marked? combination))
      (begin
	(node-mark! combination)
	(close-values! (application-operand-values combination)
		       'ARGUMENT
		       combination))))

(define (close-values! values class combination)
  (for-each (lambda (value)
	      (if (rvalue/true-procedure? value)
		  (close-procedure! value class combination)))
	    values))

(define (close-if-unreachable! block block* condition)
  ;; If BLOCK* is not an ancestor of BLOCK, close PROCEDURE.  However,
  ;; if it was an ancestor before procedure-drifting took place, don't
  ;; close, just undrift.
  (let ((procedure (condition-procedure condition)))
    (cond ((block-ancestor-or-self? block block*)
	   unspecific)
	  ((and (original-block-ancestor? block block*)
		(not (procedure-closure-context procedure)))
	   (undrifting-constraint! block block* condition))
	  (else
	   (close-procedure! procedure
			     (condition-keyword condition)
			     (condition-argument condition))))))

(define (close-procedure! procedure keyword argument)
  (add-closure-reason! procedure keyword argument)
  (if (not (procedure-closure-context procedure))
      (let ((block (procedure-block procedure))
	    (condition (make-condition #f 'CONTAGION procedure '())))

	;; Force the procedure's type to CLOSURE.  Don't change the
	;; closing block yet -- that will be taken care of by
	;; SETUP-BLOCK-TYPES!.
	(set-procedure-closure-context! procedure #t)
	(if (procedure-virtual-closure? procedure)
	    (set-procedure-virtual-closure?! procedure #f))

	;; This procedure no longer requires undrifting of others
	;; since it has been closed anyway.
	(cancel-dependent-undrifting-constraints! procedure condition)

	;; The procedure-drifting may have moved some procedures in
	;; the environment tree based on the (now incorrect)
	;; assumption that this procedure was not closed.  Fix this.
	;; On the other hand, if it was trivial before, it is still
	;; trivial now, so the callers are not affected.

	(if (not (procedure/trivial-closure? procedure))
	    (begin
	      (undrift-disowned-children! block block condition)
	      (undrift-free-callers! procedure block condition)
	      (guarantee-access-to-free-variables! procedure condition)
	      (close-non-descendant-callees! block block condition)))

	;; We need to reexamine those applications that may have this
	;; procedure as an operator, since the compatibility class of
	;; the operator may have changed.
	(enqueue-nodes! (procedure-applications procedure)))))

(define (undrift-disowned-children! block block* condition)
  ;; Undrift disowned children of BLOCK so that BLOCK* is an ancestor,
  ;; if variables bound by BLOCK* are needed.
  (let loop ((block block))
    (for-each-block-descendant! block
      (lambda (descendant)
	(for-each
	 (lambda (block**)
	   (let ((procedure (block-procedure block**)))
	     (if (not procedure)
		 (error "Non-procedure block:" block**))
	     (if (not (or (procedure-continuation? procedure)
			  (procedure/trivial-closure? procedure)
			  (block-ancestor? block** block*)))
		 (undrifting-constraint! block** block* condition))
	     (for-each loop (block-children block**))))
	 (block-disowned-children descendant))))))

(define (undrift-free-callers! procedure block condition)
  ;; Undrift blocks holding variables through which PROCEDURE is
  ;; called, so that they are descendants of BLOCK.
  (for-each
   (lambda (procedure*)
     (let ((block* (procedure-block procedure*)))
       (for-each
	(lambda (block**)
	  ;; Don't constrain the caller to be any lower than BLOCK.
	  ;; If BLOCK** is a descendant of BLOCK, it will impose a
	  ;; separate constraint in
	  ;; GUARANTEE-ACCESS-TO-FREE-VARIABLES!.
	  (let ((block**
		 (if (original-block-ancestor? block** block)
		     block
		     block**)))
	    (if (not (block-ancestor-or-self? block* block**))
		(undrifting-constraint! block* block** condition))))
	(cdr (or (assq procedure (procedure-free-callees procedure*))
		 (error "Missing free callee:" procedure procedure*))))))
   (procedure-free-callers procedure)))

(define (guarantee-access-to-free-variables! procedure condition)
  ;; Guarantee that PROCEDURE's free variables are accessible through
  ;; its parent chain.
  (let* ((block (procedure-block procedure))
	 (block* (original-block-parent block)))
    (for-each
     (lambda (variable)
       ;; This is the same as UNINTERESTING-VARIABLE? in
       ;; CLOSE-PROCEDURE? in "blktyp.scm".
       ;; Are virtual closures OK?
       (if (not (lvalue-integrated? variable))
	   (if (not (let ((value (lvalue-known-value variable)))
		      (and value
			   (rvalue/procedure? value)
			   (procedure/trivial-or-virtual? value))))
	       (let ((block** (variable-block variable)))
		 (if (not (block-ancestor-or-self? block* block**))
		     (undrifting-constraint! block* block** condition))))))
     (block-free-variables block))))

(define (close-non-descendant-callees! block block* condition)
  ;; Guarantee that any procedure called from BLOCK's procedure is
  ;; able to reach BLOCK*.
  (for-each-callee! block
    (lambda (procedure)
      (close-if-unreachable! (procedure-block procedure)
			     block*
			     (condition-new-procedure condition procedure)))))

(define (for-each-callee! block action)
  (let ((mark (list 'MARK)))
    (let loop ((block block))
      (for-each (lambda (application)
		  (for-each (lambda (value)
			      (if (and (rvalue/true-procedure? value)
				       (not (eq? (procedure-closure-size value)
						 mark)))
				  (begin
				    (set-procedure-closure-size! value mark)
				    (action value))))
			    (rvalue-values
			     (application-operator application))))
		(block-applications block))
      (for-each loop (block-children block))
      (for-each loop (block-disowned-children block)))))

(define (undrifting-constraint! block block* condition)
  ;; Undrift BLOCK so it is a descendant of BLOCK*, due to CONDITION.
  (if (block-ancestor? block block*)
      (error "Attempt to undrift block below an ancestor:" block block*))
  (if (let ((procedure (condition-procedure condition)))
	(not (and procedure
		  (or (procedure-closure-context procedure)
		      (procedure/trivial-closure? procedure)))))
      (let ((block
	     (let loop ((block block))
	       (if (or (eq? (block-parent block)
			    (original-block-parent block))
		       (original-block-ancestor? (block-parent block)
						 block*))
		   (loop (block-parent block))
		   block))))
	(if (not (and (eq? (condition-keyword condition) 'CONTAGION)
		      (let ((procedure (block-procedure block)))
			(and procedure
			     (procedure/trivial-closure? procedure)))))
	    (if (add-constraint block block* condition)
		(update-callers-and-callees! block block* condition))))))

(define (update-callers-and-callees! block block* condition)
  ;; The context of BLOCK has changed, so it may be necessary to
  ;; undrift callers and callees.  IMPORTANT: It is not clear whether
  ;; this is a source of non-optimality.  If this call is a
  ;; (transitive) consequence of a call to CLOSE-PROCEDURE!, the
  ;; callees need to be closed anyway.  If it is only the result of an
  ;; UNDRIFTING-CONSTRAINT! (due to a call to ANALYZE-PROCEDURE, for
  ;; example), we may be closing too eagerly.
  (let ((procedure (block-procedure block)))
    (if (not (and procedure
		  (rvalue/procedure? procedure)
		  (procedure/trivial-closure? procedure)))
	(begin
	  (undrift-disowned-children! block block* condition)
	  (close-non-descendant-callees! block block* condition)))
    (if (and procedure
	     (rvalue/procedure? procedure)
	     (not (procedure/trivial-closure? procedure)))
	(begin
	  (undrift-free-callers! procedure block* condition)
	  ;; Reanalyze BLOCK's procedure, since BLOCK may have been
	  ;; been moved to an inaccessible location.
	  (analyze-procedure procedure block*)
	  ;; Reanalyze the combinations calling BLOCK's procedure.
	  (enqueue-nodes! (procedure-applications procedure))))))

(define (cancel-dependent-undrifting-constraints! procedure condition)
  (remove-condition procedure)
  (for-each (let ((block (procedure-block procedure)))
	      (lambda (entry)
		(if (there-exists? (cdr entry)
		      (lambda (entry*)
			(block-ancestor-or-self? (car entry*) block)))
		    (close-non-descendant-callees! (car entry) block
						   condition))))
	    *undrifting-constraints*))

(define *undrifting-constraints*)
(define debug:trace-constraints? #f)

(define (add-constraint block block* condition)
  (debug:add-constraint block block* condition)
  (let ((entry (assq block *undrifting-constraints*))
	(condition* (if (condition-procedure condition) condition #f)))
    (if entry
	(let ((entry* (assq block* (cdr entry))))
	  (if entry*
	      (set-cdr! entry* (cons condition* (cdr entry*)))
	      (set-cdr! entry (cons (list block* condition*) (cdr entry))))
	  (not entry*))
	(begin
	  (set! *undrifting-constraints*
		(cons (list block (list block* condition*))
		      *undrifting-constraints*))
	  #t))))

(define (debug:add-constraint block block* condition)
  (if debug:trace-constraints?
      (write-line (cons* 'ADD block block*
			 (condition-procedure condition)
			 (condition-keyword condition)
			 (condition-argument condition)
			 (condition-dependencies condition)))))

(define (remove-condition procedure)
  (set! *undrifting-constraints*
	(remove-condition-1 procedure *undrifting-constraints*))
  unspecific)

(define (remove-condition-1 procedure constraints)
  (delete-matching-items! constraints
    (lambda (entry)
      (let ((tail
	     (delete-matching-items! (cdr entry)
	       (lambda (entry*)
		 (let ((conditions
			(delete-matching-items! (cdr entry*)
			  (lambda (condition)
			    (and condition
				 (or (eq? procedure
					  (condition-procedure condition))
				     (memq procedure
					   (condition-dependencies condition)))
				 (begin
				   (debug:remove-condition (car entry)
							   (car entry*)
							   condition)
				   #t))))))
		   (set-cdr! entry* conditions)
		   (null? conditions))))))
	(set-cdr! entry tail)
	(null? tail)))))

(define (debug:remove-condition block block* condition)
  (if debug:trace-constraints?
      (write-line (cons* 'REMOVE block block*
			 (condition-procedure condition)
			 (condition-keyword condition)
			 (condition-argument condition)
			 (condition-dependencies condition)))))

(define (pending-undrifting? procedure)
  (let ((entry (assq (procedure-block procedure) *undrifting-constraints*)))
    (and entry
	 (there-exists? (cdr entry) valid-constraint-conditions?))))

(define (undrift-procedures! constraints)
  (for-each
   (lambda (entry)
     (let ((block
	    (let loop ((entries (cdr entry)) (block #f))
	      (if (pair? entries)
		  (loop (cdr entries)
			(if (valid-constraint-conditions? (car entries))
			    (let ((block* (car (car entries))))
			      (if block
				  (original-block-nearest-ancestor block
								   block*)
				  block*))
			    block))
		  block))))
       (if block
	   (transfer-block-child! (car entry)
				  (block-parent (car entry))
				  block))))
   constraints))

(define (valid-constraint-conditions? entry)
  (there-exists? (cdr entry)
    (lambda (condition)
      (not
       (and condition
	    (eq? 'CONTAGION (condition-keyword condition))
	    (procedure/trivial-closure? (condition-argument condition)))))))

(define-structure condition
  (procedure #f read-only #t)
  (keyword #f read-only #t)
  (argument #f read-only #t)
  (dependencies #f read-only #t))

(define (condition-new-procedure condition procedure)
  (make-condition procedure
		  (condition-keyword condition)
		  (condition-argument condition)
		  (if (condition-procedure condition)
		      (cons (condition-procedure condition)
			    (condition-dependencies condition))
		      (condition-dependencies condition))))

(define (original-block-ancestor? block block*)
  (let loop ((block (original-block-parent block)))
    (and block
	 (or (eq? block block*)
	     (loop (original-block-parent block))))))

(define (original-block-nearest-ancestor block block*)
  (cond ((or (eq? block block*)
	     (original-block-ancestor? block block*))
	 block)
	((original-block-ancestor? block* block)
	 block*)
	(else
	 (error "Unrelated blocks:" block block*))))