#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/closan.scm,v 4.5 1988/12/06 18:56:18 jinx Exp $

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

;;;; Closure Analysis

(declare (usual-integrations))

#|

The closure analysis operates by identifying the "closing limit" of
each procedure, which is defined as the nearest ancestor of the
procedure's closing block which is active during the procedure's
lifetime.  The closing limit is false whenever the extent of the
procedure is not fully known, or if the procedure must be fully closed
for any reason (including canonicalization).

Procedures that are called from a closed procedure must inherit that
procedure's closing limit since only the blocks farther away than the
closing limit can be assumed to exist when those procedures are
called.

The procedure's free variables which are bound in blocks up to the
closing limit (exclusive) must be consed in the heap.  Other free
variables don't necessarily need to be allocated on the heap, provided
that there is a known way to get to them.

This analysis is maximal in that it is required for ANY closure
construction mechanism that optimizes by means of a stack, because use
of a stack associates procedure extent with block scope.  For many
simple techniques it generates more information than is needed.

**** Unfortunately the analysis is not compatible with the current
implementation of closures.  If a closure invokes another procedure
which is not a child, the current implementation requires that the
other procedure also be a closure.  However, if the closing-limit of
the caller is the same as the closure-block of the callee, the callee
will not be marked as a closure.  This has disastrous results.  As a
result, the analysis has been modified to force the closing-limit to
#F whenever a closure is identified.

|#

(package (identify-closure-limits!)

(define-export (identify-closure-limits! procs&conts applications lvalues)
  (let ((procedures
	 (list-transform-negative procs&conts procedure-continuation?)))
    (for-each initialize-lvalues-lists! lvalues)
    (for-each initialize-closure-limit! procedures)
    (for-each initialize-arguments! applications)
    (transitive-closure
     (lambda ()
       (for-each close-passed-out! procedures))
     (lambda (item)
       (if (rvalue/procedure? item)
	   (analyze-procedure item)
	   (analyze-application item)))
     (append procedures applications))
    ;; Clean up
    (if (not compiler:preserve-data-structures?)
	(for-each (lambda (procedure)
		    (set-procedure-free-callees! procedure '())
		    (set-procedure-free-callers! procedure '())
		    (set-procedure-variables! procedure '()))
		  procedures))))

(define (initialize-lvalues-lists! lvalue)
  (if (lvalue/variable? lvalue)
      (for-each (lambda (val)
		  (if (rvalue/procedure? val)
		      (set-procedure-variables!
		       val
		       (cons lvalue (procedure-variables val))))
		  'DONE)
		(lvalue-values lvalue))))

(define (initialize-closure-limit! procedure)
  (set-procedure-closing-limit! procedure
				(procedure-closing-block procedure))
  'DONE)

(define (initialize-arguments! application)
  (if (application/combination? application)
      (begin
	(let ((values
	       (let ((operands (application-operands application)))
		 (if (null? operands)
		     '()
		     (eq-set-union* (rvalue-values (car operands))
				    (map rvalue-values (cdr operands)))))))
	  (set-application-operand-values! application values)
	  (for-each
	   (lambda (value)
	     (if (and (rvalue/procedure? value)
		      (not (procedure-continuation? value)))
		 (set-procedure-virtual-closure?! value true)))
	   values)))))

(define (close-passed-out! procedure)
  (if (and (not (procedure-continuation? procedure))
	   (procedure-passed-out? procedure))
      (maybe-close-procedure! procedure false 'PASSED-OUT false)))

(define (analyze-procedure procedure)
  (for-each (lambda (variable)
	      (maybe-close-procedure! procedure
				      (variable-block variable)
				      'EXPORTED
				      variable))
	    (procedure-variables procedure)))

(define (analyze-application application)
  (let* ((operator (application-operator application))
	 (proc (rvalue-known-value operator))
	 (procs (rvalue-values operator)))
    (cond ((not (application/combination? application))
	   ;; If the combination is not an application, we need not
	   ;; examine the operators for compatibility.
	   'DONE)
	  ((rvalue-passed-in? operator)
	   ;; We don't need to close the operands because
	   ;; they have been marked as passed out already.
	   (close-rvalue! operator false 'APPLY-COMPATIBILITY application))
	  ((null? procs)
	   ;; The (null? procs) case is the NOP node case.  This combination
	   ;; should not be executed, so it should have no effect on any items
	   ;; involved in it.
	   'DONE)
	  ((not proc)
	   (let ((class (compatibility-class procs))
		 (model (car procs)))
	     (set-combination/model! application
				     (if (eq? class 'APPLY-COMPATIBILITY)
					 false
					 model))
	     (if (eq? class 'POTENTIAL)
		 (for-each (lambda (proc)
			     (set-procedure-virtual-closure?! proc true))
			   procs)
		 (begin
		   (close-rvalue! operator false class application)
		   (close-application-arguments! application false)))))
	  ((or (not (rvalue/procedure? proc))
	       (procedure-closure-block proc))
	   (close-application-arguments! application false))
	  (else
	   'DONE))))

(define (close-application-arguments! application block)
  (let* ((previous (application-destination-block application))
	 (new (cond ((eq? previous true)
		     block)
		    ((or (false? previous)
			 (false? block))
		     false)
		    (else
		     (block-nearest-common-ancestor block previous)))))
    (if (not (eq? new previous))
	(begin
	  (set-application-destination-block! application new)
	  (close-values!
	   (application-operand-values application)
	   new
	   'ARGUMENT
	   application)))))

(define (with-procedure-arity proc receiver)
  (let ((req (length (procedure-required proc))))
    (receiver req
	      (if (procedure-rest proc)
		  -1
		  (+ req (length (procedure-optional proc)))))))

(define (compatibility-class procs)
  (if (not (for-all? procs rvalue/procedure?))
      'APPLY-COMPATIBILITY
      (let* ((model (car procs))
	     (model-env (procedure-closing-limit model)))
	(with-procedure-arity
	 model
	 (lambda (model-min model-max)
	   (let loop ((procs (cdr procs))
		      (class (if (procedure/closure? model)
				 'COMPATIBILITY
				 'POTENTIAL)))
	     (if (null? procs)
		 class
		 (let ((this (car procs)))
		   (with-procedure-arity
		    this
		    (lambda (this-min this-max)
		      (cond ((not (and (= model-min this-min)
				       (= model-max this-max)))
			     'APPLY-COMPATIBILITY)
			    ((or (procedure/closure? this)
				 (not (eq? (procedure-closing-limit this)
					   model-env)))
			     (loop (cdr procs) 'COMPATIBILITY))
			    (else
			     (loop (cdr procs) class)))))))))))))

(define-integrable (close-rvalue! rvalue binding-block reason1 reason2)
  (close-values! (rvalue-values rvalue) binding-block reason1 reason2))

(define (close-values! values binding-block reason1 reason2)
  (for-each (lambda (value)
	      (if (and (rvalue/procedure? value)
		       (not (procedure-continuation? value)))
		  (maybe-close-procedure! value binding-block
					  reason1 reason2)))
	    values))

(define (maybe-close-procedure! procedure binding-block reason1 reason2)
  (let* ((closing-limit (procedure-closing-limit procedure))
	 (new-closing-limit
	  (and binding-block
	       closing-limit
	       (block-nearest-common-ancestor binding-block closing-limit))))
    (cond ((not (eq? new-closing-limit closing-limit))
	   (if (procedure-virtual-closure? procedure)
	       (set-procedure-virtual-closure?! procedure false))
	   (close-procedure! procedure new-closing-limit reason1 reason2))
	  ((false? new-closing-limit)
	   (add-closure-reason! procedure reason1 reason2)))))

(define (close-procedure! procedure new-closing-limit reason1 reason2)
  new-closing-limit
  ;; **** Force trivial closure limit due to poor code generator. ****
  (let ((new-closing-limit false))
    (let ((previously-trivial? (procedure/trivial-closure? procedure)))
      (set-procedure-closing-limit! procedure new-closing-limit)
      ;; We can't change the closing block yet.
      ;; blktyp has a consistency check that depends on the closing block
      ;; remaining the same.
      (add-closure-reason! procedure reason1 reason2)
      ;; Force the procedure's type to CLOSURE.
      (if (not (procedure-closure-block procedure))
	  (set-procedure-closure-block! procedure true))
      ;; The code generator needs all callees to be closed.
      (close-callees! (procedure-block procedure)
		      new-closing-limit
		      procedure)
      ;; The environment optimizer may have moved some procedures in the
      ;; environment tree based on the (now incorrect) assumption that this
      ;; procedure was not closed.  Fix this.
      ;; On the other hand, if it was trivial before, it is still trivial
      ;; now, so the callers are not affected.
      (if (not previously-trivial?)
	  (examine-free-callers! procedure))
      ;; We need to reexamine those applications which may have this procedure
      ;; as an operator, since the compatibility class of the operator may have
      ;; changed.
      (enqueue-nodes! (procedure-applications procedure)))))

;; These are like the corresponding standard block operations, but
;; they ignore any block drifting caused by envopt.

(define-integrable (original-block-parent block)
  (let ((procedure (block-procedure block)))
    (and procedure
	 (rvalue/procedure? procedure)
	 (procedure-target-block procedure))))

(define (original-block-ancestor-or-self? block block*)
  (define (loop block)
    (and block
	 (or (eq? block block*)
	     (loop (original-block-parent block)))))

  (or (eq? block block*)
      (loop (original-block-parent block))))

(define (original-block-ancestry block path)
  (if (block-parent block)
      (original-block-ancestry (original-block-parent block) (cons block path))
      (cons block path)))

(define (original-block-nearest-common-ancestor block block*)
  (let loop
      ((join false)
       (ancestry (original-block-ancestry block '()))
       (ancestry* (original-block-ancestry block* '())))
    (if (and (not (null? ancestry))
	     (not (null? ancestry*))
	     (eq? (car ancestry) (car ancestry*)))
	(loop (car ancestry) (cdr ancestry) (cdr ancestry*))
	join)))

(define-integrable (block<= ancestor descendant)
  (block-ancestor-or-self? descendant ancestor))

(define (undrift-procedure! procedure block)
  (let ((myblock (procedure-block procedure))
	(closing-block (procedure-closing-limit procedure))
	(original-closing-block (procedure-target-block procedure)))
    (set-procedure-closing-limit! procedure block)
    (set-block-children! closing-block
			 (delq! myblock (block-children closing-block)))
    (set-block-children! block (cons myblock (block-children block)))
    (enqueue-nodes! (cons procedure (procedure-applications procedure)))
    (cond ((eq? block original-closing-block)
	   (set-block-disowned-children! original-closing-block
					 (delq! myblock
						(block-disowned-children
						 original-closing-block))))
	  ((and (not (block<= block original-closing-block))
		(rvalue/procedure? (block-procedure original-closing-block))
		(not (procedure-closure-block
		      (block-procedure original-closing-block))))
	   ;; My original parent has drifted to a place where I can't
	   ;; be closed.  I must drag it back.
	   (if (not (original-block-ancestor-or-self? original-closing-block
						      block))
	       (error "Procedure has free variables in hyperspace!"
		      procedure))
	   (undrift-procedure! (block-procedure original-closing-block)
			       block)))
    (examine-free-callers! procedure)))

(define (examine-free-callers! procedure)
  (let ((myblock (procedure-block procedure)))
    (for-each
     (lambda (procedure*)
       (if (false? (procedure-closure-block procedure*))
	   (let ((closing-block (procedure-closing-limit procedure*))
		 (original-closing-block (procedure-target-block procedure*)))
	     ;; No need to do anything if PROCEDURE* hasn't drifted
	     ;; relative to PROCEDURE.
	     (if (and (not (eq? closing-block original-closing-block))
		      (not (block<= myblock closing-block)))
		 (let ((binding-block
			(reduce original-block-nearest-common-ancestor
				false
				(map variable-block
				     (cdr (assq procedure
						(procedure-free-callees
						 procedure*)))))))
		   (if (not (block<= binding-block closing-block))
		       ;; PROCEDURE* has drifted towards the
		       ;; environment root past the point where we
		       ;; have access to PROCEDURE (by means of free
		       ;; variables).  We must drift it away from
		       ;; the root until we regain access to PROCEDURE.
		       (undrift-procedure! procedure* binding-block)))))))
     (procedure-free-callers procedure))))

(define (close-callees! block new-closing-limit culprit)
  (for-each-callee! block
    (lambda (value)
      (if (not (block-ancestor-or-self? (procedure-block value) block))
	  (maybe-close-procedure! value new-closing-limit
				  'CONTAGION culprit)))))

(define (for-each-callee! block procedure)
  (for-each-block-descendent! block
    (lambda (block*)
      (for-each (lambda (application)
		  (for-each (lambda (value)
			      (if (and (rvalue/procedure? value)
				       (not (procedure-continuation? value)))
				  (procedure value)))
			    (rvalue-values
			     (application-operator application))))
		(block-applications block*)))))

)