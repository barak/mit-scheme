#| -*-Scheme-*-

$Id: closan.scm,v 4.21 2001/11/01 18:42:59 cph Exp $

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
  (for-each
   (lambda (variable)
     ;; If this procedure is the value of a variable which is bound
     ;; in a non-descendant block, we must close it.
     (if (not (procedure-closure-context procedure))
	 (close-if-unreachable! (variable-block variable)
				block
				procedure
				'EXPORTED
				variable)))
   (procedure-variables procedure)))

(define (analyze-combination combination)
  (let* ((operator (combination/operator combination))
	 (proc (rvalue-known-value operator))
	 (procs (rvalue-values operator)))
    (cond ((rvalue-passed-in? operator)
	   ;; We don't need to close the operands because
	   ;; they have been marked as passed out already.
	   (close-rvalue! operator 'APPLY-COMPATIBILITY combination))
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
		   (close-rvalue! operator class combination)
		   (close-combination-arguments! combination)))))
	  ((or (not (rvalue/procedure? proc))
	       (procedure-closure-context proc))
	   (close-combination-arguments! combination))
	  (else
	   unspecific))))

(define (close-combination-arguments! combination)
  (if (not (node-marked? combination))
      (begin
	(node-mark! combination)
	(close-values! (application-operand-values combination)
		       'ARGUMENT
		       combination))))

(define (compatibility-class procs)
  (if (not (for-all? procs rvalue/procedure?))
      'APPLY-COMPATIBILITY
      (let* ((model (car procs))
	     (model-env (procedure-closing-block model)))
	(with-values (lambda () (procedure-arity-encoding model))
	  (lambda (model-min model-max)
	    (let loop
		((procs (cdr procs))
		 (class
		  (if (or (procedure/closure? model)
			  (pending-undrifting? model))
		      'COMPATIBILITY ;Cop-out.  Could be postponed 'til later.
		      'POTENTIAL)))
	      (if (pair? procs)
		  (let ((this (car procs)))
		    (with-values (lambda () (procedure-arity-encoding this))
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
		  class)))))))

(define-integrable (close-rvalue! rvalue reason1 reason2)
  (close-values! (rvalue-values rvalue) reason1 reason2))

(define (close-values! values reason1 reason2)
  (for-each (lambda (value)
	      (if (rvalue/true-procedure? value)
		  (close-procedure! value reason1 reason2)))
	    values))

(define (close-if-unreachable! block block* procedure reason1 reason2)
  ;; If `block*' is not an ancestor of `block', close `procedure'.
  ;; However, if it was an ancestor before procedure-drifting took
  ;; place, don't close, just undo the drifting.
  (cond ((block-ancestor-or-self? block block*)
	 unspecific)
	((not (original-block-ancestor? block block*))
	 (close-procedure! procedure reason1 reason2))
	((procedure-closure-context procedure)
	 (add-closure-reason! procedure reason1 reason2))
	(else
	 (undrifting-constraint! block block* procedure reason1 reason2))))

(define (close-procedure! procedure reason1 reason2)
  (add-closure-reason! procedure reason1 reason2)
  (if (not (procedure-closure-context procedure))
      (let ((block (procedure-block procedure)))
	;; Force the procedure's type to CLOSURE.  Don't change the
	;; closing block yet -- that will be taken care of by
	;; `setup-block-types!'.
	(set-procedure-closure-context! procedure #t)
	(if (procedure-virtual-closure? procedure)
	    (set-procedure-virtual-closure?! procedure #f))
	;; This procedure no longer requires undrifting of others
	;; since it has been closed anyway.
	(cancel-dependent-undrifting-constraints! procedure)
	;; The procedure-drifting may have moved some procedures in
	;; the environment tree based on the (now incorrect)
	;; assumption that this procedure was not closed.  Fix this.
	;; On the other hand, if it was trivial before, it is still
	;; trivial now, so the callers are not affected.
	(if (not (procedure/trivial-closure? procedure))
	    (begin
	      (undrift-disowned-children! block block #f
					  'CONTAGION procedure)
	      (examine-free-callers! procedure block #f
				     'CONTAGION procedure)
	      (guarantee-connectivity! procedure)
	      ;; Guarantee that all callees are contained within.
	      (close-non-descendant-callees! block block
					     'CONTAGION procedure)))
	;; We need to reexamine those applications which may have
	;; this procedure as an operator, since the compatibility
	;; class of the operator may have changed.
	(enqueue-nodes! (procedure-applications procedure)))))

(define (guarantee-connectivity! procedure)
  ;; Make sure that my free variables are accessible through my
  ;; parent chain.
  (let* ((block (procedure-block procedure))
	 (block* (original-block-parent block)))
    (for-each
     (lambda (var)
       ;; This is the same as uninteresting-variable? in
       ;; CLOSE-PROCEDURE? in blktyp.
       ;; Are virtual closures OK?
       (if (not (lvalue-integrated? var))
	   (let ((val (lvalue-known-value var)))
	     (if (or (not val)
		     (not (rvalue/procedure? val))
		     (not (procedure/trivial-or-virtual? val)))
		 (let ((block** (variable-block var)))
		   (if (not (block-ancestor-or-self? block* block**))
		       (undrifting-constraint!
			block* block** #f 'CONTAGION procedure)))))))
     (block-free-variables block))))

(define (undrift-disowned-children! block block* procedure reason1 reason2)
  ;; Undrift disowned children of `block' so that `block*'
  ;; is an ancestor if free variables captured by `block*' are needed.

  (define (process-descendant block)
    (for-each-block-descendant!
     block
     (lambda (block*)
       (for-each process-disowned (block-disowned-children block*)))))

  (define (process-disowned block**)
    (let ((proc (block-procedure block**)))
      (cond ((not proc)
	     (error "undrift-disowned-children!: Non-procedure block" block**))
	    ((and (not (procedure-continuation? proc))
		  (not (procedure/trivial-closure? proc))
		  (not (block-ancestor? block** block*)))
	     (undrifting-constraint! block** block* procedure
				     reason1 reason2)))
      (for-each process-descendant (block-children block**))))

  (process-descendant block))

(define (close-non-descendant-callees! block block* reason1 reason2)
  ;; close/undrift all descendants of `block' that are not descendants
  ;; of `block*' for <reason1,reason2>
  (for-each-callee! block
   (lambda (value)
     (close-if-unreachable! (procedure-block value) block*
			    value reason1 reason2))))

(define (examine-free-callers! procedure block savedproc reason1 reason2)
  (for-each
   (lambda (procedure*)
     (let ((block* (procedure-block procedure*)))
       (for-each
	(lambda (block**)
	  ;; Don't constrain the caller to be any lower than BLOCK.
	  ;; If BLOCK** is a descendant of BLOCK, it will impose a
	  ;; separate constraint in GUARANTEE-CONNECTIVITY!.
	  (let ((block**
		 (if (original-block-ancestor? block** block)
		     block
		     block**)))
	    (if (not (block-ancestor-or-self? block* block**))
		(undrifting-constraint! block* block**
					savedproc reason1 reason2))))
	(cdr (or (assq procedure (procedure-free-callees procedure*))
		 (error "missing free-callee" procedure procedure*))))))
   (procedure-free-callers procedure)))

(define (update-callers-and-callees! block block* procedure** reason1 reason2)
  ;; My context has changed.  Fix my dependencies and `dependees'.
  ;; IMPORTANT: It is not clear whether this is a source of
  ;; non-optimality or not.  If this call is a (transitive)
  ;; consequence of a call to CLOSE-PROCEDURE!, the callees need to be
  ;; closed anyway.  If it is only the result of an UNDRIFTING-CONSTRAINT!
  ;; (due to a call to ANALYZE-PROCEDURE, for example), we may be closing
  ;; too eagerly.
  (let ((procedure (block-procedure block)))
    (if (or (not procedure)
	    (not (rvalue/procedure? procedure))
	    (not (procedure/trivial-closure? procedure)))
	(begin
	  ;; 1: Undrift disowned children and close transitively.
	  (undrift-disowned-children! block block* procedure** reason1 reason2)
	  (close-non-descendant-callees! block block* reason1 reason2)))
    (if (and procedure
	     (rvalue/procedure? procedure)
	     (not (procedure/trivial-closure? procedure)))
	(begin
	  ;; 2: Undrift all free callers.
	  (examine-free-callers! procedure block* procedure** reason1 reason2)
	  ;; 3: Reanalyze.
	  ;; I may have been moved to an inaccessible location.
	  (analyze-procedure procedure block*)
	  ;; 4: Reanalyze the combinations whose operator I am.
	  (enqueue-nodes! (procedure-applications procedure))))))

(define *undrifting-constraints*)

(define (undrifting-constraint! block block* procedure reason1 reason2)
  ;; Undrift `block' so it is a descendant of `block*' in order not
  ;; to close `procedure' for <`reason1',`reason2'>
  ;; If `procedure' is #f, undrift unconditionally
  (if (block-ancestor? block block*)
      (error "Attempt to undrift block below an ancestor:" block block*))
  (if (or (not procedure)
	  (and (not (procedure-closure-context procedure))
	       (not (procedure/trivial-closure? procedure))))
      (let ((block
	     (let loop ((block block))
	       (if (or (eq? (block-parent block) (original-block-parent block))
		       (original-block-ancestor? (block-parent block) block*))
		   (loop (block-parent block))
		   block)))
	    (condition (and procedure (list procedure reason1 reason2))))
	(let ((entry (assq block *undrifting-constraints*))
	      (generate-caller-constraints
	       (lambda ()
		 (update-callers-and-callees! block block* procedure
					      reason1 reason2))))
	  (if (not entry)
	      (begin
		(set! *undrifting-constraints*
		      (cons (list block (list block* condition))
			    *undrifting-constraints*))
		(generate-caller-constraints))
	      (let ((entry* (assq block* (cdr entry))))
		(cond ((not entry*)
		       (set-cdr! entry
				 (cons (list block* condition) (cdr entry)))
		       (generate-caller-constraints))
		      ((not condition)
		       (if (not (memq condition (cdr entry*)))
			   (begin
			     (set-cdr! entry* (cons condition (cdr entry*)))
			     unspecific)))
		      ((not
			(there-exists?
			 (cdr entry*)
			 (lambda (condition*)
			   (and condition*
				(eq? (car condition) (car condition*))
				(eqv? (cadr condition) (cadr condition*))
				(eqv? (caddr condition) (caddr condition*))))))
		       (set-cdr! entry* (cons condition (cdr entry*)))
		       unspecific))))))))

(define (cancel-dependent-undrifting-constraints! procedure)
  (for-each
   (let ((block (procedure-block procedure)))
     (lambda (entry)
       (for-each
	(lambda (entry*)
	  (set-cdr! entry*
		    (list-transform-negative! (cdr entry*)
		      (lambda (condition)
			(and condition (eq? procedure (car condition)))))))
	(cdr entry))
       (if (there-exists? (cdr entry)
	     (lambda (entry*)
	       (and (pair? (cdr entry*))
		    (block-ancestor-or-self? (car entry*) block))))
	   (close-non-descendant-callees! (car entry) block
					  'CONTAGION procedure))))
   *undrifting-constraints*))

(define (pending-undrifting? procedure)
  (assq (procedure-block procedure) *undrifting-constraints*))

(define (undrift-procedures! constraints)
  (for-each
   (lambda (entry)
     (let ((entries
	    (list-transform-negative! (cdr entry)
	      (lambda (entry*)
		(for-all? (cdr entry*)
		  (lambda (condition)
		    (and condition
			 (eq? 'CONTAGION (cadr condition))
			 (procedure/trivial-closure? (caddr condition)))))))))
       (if (pair? entries)
	   (undrift-block! (car entry)
			   (reduce original-block-nearest-ancestor
				   #f
				   (map car entries))))))
   constraints))

(define (undrift-block! block new-parent)
  (transfer-block-child! block (block-parent block) new-parent))

;;;; Utilities

(define-integrable (list-transform-negative! items predicate)
  ((list-deletor! predicate) items))

(define (original-block-ancestor? block block*)
  (let loop ((block (original-block-parent block)))
    (and block
	 (or (eq? block block*)
	     (loop (original-block-parent block))))))

(define (original-block-nearest-ancestor block block*)
  (cond ((or (eq? block block*) (original-block-ancestor? block block*)) block)
	((original-block-ancestor? block* block) block*)
	(else (error "unrelated blocks" block block*))))

;; This should be moved elsewhere.
;; envopt has an identical definition commented out.

(define (for-each-callee! block action)
  (for-each-block-descendant! block
    (lambda (block*)
      (for-each (lambda (application)
		  (for-each (lambda (value)
			      (if (rvalue/true-procedure? value)
				  (action value)))
			    (rvalue-values
			     (application-operator application))))
		(block-applications block*)))))