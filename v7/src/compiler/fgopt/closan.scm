#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/closan.scm,v 4.11 1989/12/02 21:19:29 cph Exp $

Copyright (c) 1987, 1988, 1989 Massachusetts Institute of Technology

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

;;;; Closure Analysis

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
		(if (null? operands)
		    '()
		    (eq-set-union* (rvalue-values (car operands))
				   (map rvalue-values (cdr operands)))))))
	 (set-application-operand-values! combination values)
	 (for-each
	  (lambda (value)
	    (if (and (rvalue/procedure? value)
		     (not (procedure-continuation? value)))
		(set-procedure-virtual-closure?! value true)))
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
	     (for-each (lambda (procedure)
			 (if (procedure-passed-out? procedure)
			     (close-procedure! procedure 'PASSED-OUT false)
			     (analyze-procedure procedure)))
		       procedures))
	   analyze-combination
	   combinations)))
       *undrifting-constraints*))))

(define (analyze-procedure procedure)
  (for-each
   (lambda (variable)
     ;; If this procedure is the value of a variable which is bound
     ;; in a non-descendent block, we must close it.
     (if (not (procedure-closure-context procedure))
	 (close-if-unreachable! (variable-block variable)
				(procedure-closing-block procedure)
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
	   ;; The (null? procs) case is the NOP node case.  This combination
	   ;; should not be executed, so it should have no effect on any items
	   ;; involved in it.
	   unspecific)
	  ((not proc)
	   (let ((class (compatibility-class procs))
		 (model (car procs)))
	     (set-combination/model! combination
				     (if (eq? class 'APPLY-COMPATIBILITY)
					 false
					 model))
	     (if (eq? class 'POTENTIAL)
		 (for-each (lambda (proc)
			     (set-procedure-virtual-closure?! proc true))
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
		  (if (procedure/closure? model) 'COMPATIBILITY 'POTENTIAL)))
	      (if (null? procs)
		  class
		  (let ((this (car procs)))
		    (with-values (lambda () (procedure-arity-encoding this))
		      (lambda (this-min this-max)
			(if (and (= model-min this-min)
				 (= model-max this-max))
			    (loop (cdr procs)
				  (if (and (not (procedure/closure? this))
					   (eq? (procedure-closing-block this)
						model-env))
				      class
				      'COMPATIBILITY))
			    'APPLY-COMPATIBILITY)))))))))))

(define-integrable (close-rvalue! rvalue reason1 reason2)
  (close-values! (rvalue-values rvalue) reason1 reason2))

(define (close-values! values reason1 reason2)
  (for-each (lambda (value)
	      (if (and (rvalue/procedure? value)
		       (not (procedure-continuation? value)))
		  (close-procedure! value reason1 reason2)))
	    values))

(define (close-if-unreachable! block block* procedure reason1 reason2)
  ;; If `block*' is not an ancestor of `block', close `procedure'.
  (if (not (block-ancestor-or-self? block block*))
      ;; However, if it was an ancestor before procedure-drifting took
      ;; place, don't close, just undo the drifting.
      (if (original-block-ancestor? block block*)
	  (undrifting-constraint! block block* procedure reason1 reason2)
	  (close-procedure! procedure reason1 reason2))))

(define (close-procedure! procedure reason1 reason2)
  (add-closure-reason! procedure reason1 reason2)
  (if (not (procedure-closure-context procedure))
      (begin

	;; Force the procedure's type to CLOSURE.  Don't change the
	;; closing block yet -- that will be taken care of by
	;; `setup-block-types!'.
	(set-procedure-closure-context! procedure true)
	(if (procedure-virtual-closure? procedure)
	    (set-procedure-virtual-closure?! procedure false))
	(cancel-dependent-undrifting-constraints! procedure)
	(close-non-descendent-callees! procedure (procedure-block procedure))

	;; The procedure-drifting may have moved some procedures in
	;; the environment tree based on the (now incorrect)
	;; assumption that this procedure was not closed.  Fix this.
	;; On the other hand, if it was trivial before, it is still
	;; trivial now, so the callers are not affected.
	(if (not (procedure/trivial-closure? procedure))
	    (examine-free-callers! procedure))

	;; We need to reexamine those applications which may have
	;; this procedure as an operator, since the compatibility
	;; class of the operator may have changed.
	(enqueue-nodes! (procedure-applications procedure)))))

(define (close-non-descendent-callees! procedure block)
  (for-each-block-descendent! block
    (lambda (block*)
      (for-each
       (lambda (application)
	 (for-each (lambda (value)
		     (if (and (rvalue/procedure? value)
			      (not (procedure-continuation? value)))
			 (close-if-unreachable! (procedure-block value) block
						value 'CONTAGION procedure)))
		   (rvalue-values (application-operator application))))
       (block-applications block*)))))

(define (examine-free-callers! procedure)
  (for-each
   (lambda (procedure*)
     (let ((block (procedure-block procedure*)))
       (for-each
	(lambda (block*)
	  (if (not (block-ancestor-or-self? block block*))
	      (undrifting-constraint! block block* false false false)))
	(map->eq-set
	 variable-block
	 (cdr (or (assq procedure (procedure-free-callees procedure*))
		  (error "missing free-callee" procedure procedure*)))))))
   (procedure-free-callers procedure)))

(define *undrifting-constraints*)

(define (undrifting-constraint! block block* procedure reason1 reason2)
  (if (and procedure (procedure-closure-context procedure))
      (add-closure-reason! procedure reason1 reason2)
      (let ((block
	     (let loop ((block block))
	       (if (or (eq? (block-parent block) (original-block-parent block))
		       (original-block-ancestor? (block-parent block) block*))
		   (loop (block-parent block))
		   block)))
	    (condition (and procedure (list procedure reason1 reason2))))
	(let ((entry (assq block *undrifting-constraints*))
	      (check-inheritance
	       (lambda ()
		 (let loop ((block* block*))
		   (if block*
		       (let ((procedure (block-procedure block*)))
			 (if (eq? true (procedure-closure-context procedure))
			     (close-non-descendent-callees! procedure block)
			     (loop (block-parent block*)))))))))
	  (if (not entry)
	      (begin
		(set! *undrifting-constraints*
		      (cons (list block (list block* condition))
			    *undrifting-constraints*))
		(check-inheritance))
	      (let ((entry* (assq block* (cdr entry))))
		(cond ((not entry*)
		       (set-cdr! entry
				 (cons (list block* condition) (cdr entry)))
		       (check-inheritance))
		      ((not
			(if condition
			    (list-search-positive (cdr entry*)
			      (lambda (condition*)
				(and
				 (eq? (car condition) (car condition*))
				 (eqv? (cadr condition) (cadr condition*))
				 (eqv? (caddr condition) (caddr condition*)))))
			    (memq false (cdr entry*))))
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
		      (lambda (constraint)
			(and constraint (eq? procedure (car constraint)))))))
	(cdr entry))
       (if (there-exists? (cdr entry)
	     (lambda (entry*)
	       (and (not (null? (cdr entry*)))
		    (block-ancestor-or-self? (car entry*) block))))
	   (close-non-descendent-callees! procedure (car entry)))))
   *undrifting-constraints*))

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
       (if (not (null? entries))
	   (undrift-block! (car entry)
			   (reduce original-block-nearest-ancestor
				   false
				   (map car entries))))))
   constraints))

(define-integrable (list-transform-negative! items predicate)
  ((list-deletor! predicate) items))

(define (undrift-block! block new-parent)
  (let ((parent (block-parent block)))
    (set-block-children! parent (delq! block (block-children parent))))
  (own-block-child! new-parent block)
  (if (eq? new-parent (original-block-parent block))
      (set-block-disowned-children!
       new-parent
       (delq! block (block-disowned-children new-parent)))))

(define (original-block-ancestor? block block*)
  (let loop ((block (original-block-parent block)))
    (and block
	 (or (eq? block block*)
	     (loop (original-block-parent block))))))

(define (original-block-nearest-ancestor block block*)
  (cond ((or (eq? block block*) (original-block-ancestor? block block*)) block)
	((original-block-ancestor? block* block) block*)
	(else (error "unrelated blocks" block block*))))