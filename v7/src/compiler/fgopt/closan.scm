#| -*-Scheme-*-

$Id: closan.scm,v 4.25 2001/11/02 14:57:50 cph Exp $

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

(define (identify-closure-limits! procs&conts applications lvalues)
  (let ((procedures
	 (list-transform-negative procs&conts procedure-continuation?))
	(combinations
	 (list-transform-positive applications application/combination?)))
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
							 #f))))
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
	    (condition (make-condition #f 'CONTAGION procedure #f)))

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
      (close-if-unreachable! (procedure-block procedure) block*
			     (condition-new-procedure condition procedure)))))

(define (for-each-callee! block action)
  (for-each-block-descendant! block
    (lambda (block)
      (for-each (lambda (application)
		  (for-each (lambda (value)
			      (if (rvalue/true-procedure? value)
				  (action value)))
			    (rvalue-values
			     (application-operator application))))
		(block-applications block)))))

(define (undrifting-constraint! block block* condition)
  ;; Undrift BLOCK so it is a descendant of BLOCK*, due to CONDITION.
  (if (block-ancestor? block block*)
      (error "Attempt to undrift block below an ancestor:" block block*))
  (let ((procedure (condition-procedure condition)))
    (if (not (and procedure
		  (or (procedure-closure-context procedure)
		      (procedure/trivial-closure? procedure))))
	(let ((block
	       (let loop ((block block))
		 (if (or (eq? (block-parent block)
			      (original-block-parent block))
			 (original-block-ancestor? (block-parent block)
						   block*))
		     (loop (block-parent block))
		     block))))
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
  (remove-condition procedure condition)
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
	      (begin
		(if (not
		     (if condition*
			 (there-exists? (cdr entry*)
			   (lambda (condition**)
			     (and condition**
				  (condition=? condition** condition*))))
			 (memq condition* (cdr entry*))))
		    (set-cdr! entry* (cons condition* (cdr entry*))))
		#f)
	      (begin
		(set-cdr! entry
			  (cons (list block* condition*)
				(cdr entry)))
		#t)))
	(begin
	  (set! *undrifting-constraints*
		(cons (list block (list block* condition*))
		      *undrifting-constraints*))
	  #t))))

(define (debug:add-constraint block block* condition)
  (if debug:trace-constraints?
      (write-line (list 'ADD block block*
			(condition-procedure condition)
			(condition-keyword condition)
			(condition-argument condition)
			(condition-dependency condition)))))

(define (remove-condition procedure condition)
  (for-each (lambda (entry)
	      (for-each
	       (lambda (entry*)
		 (set-cdr! entry*
			   (list-transform-negative! (cdr entry*)
			     (lambda (condition)
			       (and condition
				    (eq? procedure
					 (condition-procedure condition))
				    (begin
				      (debug:remove-condition (car entry)
							      (car entry*)
							      condition)
				      #t))))))
	       (cdr entry))
	      (set-cdr! entry
			(list-transform-negative! (cdr entry)
			  (lambda (entry*)
			    (null? (cdr entry*))))))
	    *undrifting-constraints*)
  (set! *undrifting-constraints*
	(list-transform-negative! *undrifting-constraints*
	  (lambda (entry)
	    (null? (cdr entry)))))
  unspecific)

(define (debug:remove-condition block block* condition)
  (if debug:trace-constraints?
      (write-line (list 'REMOVE block block*
			(condition-procedure condition)
			(condition-keyword condition)
			(condition-argument condition)
			(condition-dependency condition)))))

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
  (dependency #f read-only #t))

(define (condition=? c1 c2)
  (and (eq? (condition-procedure c1) (condition-procedure c2))
       (eq? (condition-keyword c1) (condition-keyword c2))
       (eqv? (condition-argument c1) (condition-argument c2))
       (eq? (condition-dependency c1) (condition-dependency c2))))

(define (condition-new-procedure condition procedure)
  (make-condition procedure
		  (condition-keyword condition)
		  (condition-argument condition)
		  (condition-procedure condition)))

(define (list-transform-negative! items predicate)
  ((list-deletor! predicate) items))

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