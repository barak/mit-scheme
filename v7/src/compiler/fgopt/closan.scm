#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/closan.scm,v 4.7 1989/03/14 19:45:15 cph Exp $

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
the (closed) caller is the same as that of the (open) callee, the
callee will not be marked as a closure.  This has disastrous results.
As a result, the analysis has been modified to force the closing-limit
to #F whenever a closure is identified.

|#

(define (identify-closure-limits! procs&conts applications lvalues)
  (let ((procedures
	 (list-transform-negative procs&conts procedure-continuation?)))
    (for-each initialize-lvalues-lists! lvalues)
    (for-each initialize-closure-limit! procedures)
    (for-each initialize-arguments! applications)
    (transitive-closure
     (lambda ()
       (for-each (lambda (procedure)
		   (if (procedure-passed-out? procedure)
		       (maybe-close-procedure! procedure
					       false
					       'PASSED-OUT
					       false)))
		 procedures))
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
      (for-each (lambda (value)
		  (if (rvalue/procedure? value)
		      (set-procedure-variables!
		       value
		       (cons lvalue (procedure-variables value)))))
		(lvalue-values lvalue))))

(define (initialize-closure-limit! procedure)
  (set-procedure-closing-limit! procedure (procedure-closing-block procedure)))

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
	   values))
	(set-combination/model!
	 application
	 (rvalue-known-value (combination/operator application))))))

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
	   unspecific)
	  ((rvalue-passed-in? operator)
	   ;; We don't need to close the operands because
	   ;; they have been marked as passed out already.
	   (close-rvalue! operator 'APPLY-COMPATIBILITY application))
	  ((null? procs)
	   ;; The (null? procs) case is the NOP node case.  This combination
	   ;; should not be executed, so it should have no effect on any items
	   ;; involved in it.
	   unspecific)
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
		   (close-rvalue! operator class application)
		   (close-application-arguments! application false)))))
	  ((or (not (rvalue/procedure? proc))
	       (procedure-closure-context proc))
	   (close-application-arguments! application false))
	  (else
	   unspecific))))

(define (close-application-arguments! application block)
  (let ((previous (application-destination-block application)))
    (let ((new
	   (if (eq? previous true)
	       block
	       (and previous
		    block
		    (block-nearest-common-ancestor block previous)))))
      (if (not (eq? new previous))
	  (begin
	    (set-application-destination-block! application new)
	    (close-values! (application-operand-values application)
			   new
			   'ARGUMENT
			   application))))))

(define (compatibility-class procs)
  (if (not (for-all? procs rvalue/procedure?))
      'APPLY-COMPATIBILITY
      (let* ((model (car procs))
	     (model-env (procedure-closing-limit model)))
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
					   (eq? (procedure-closing-limit this)
						model-env))
				      class
				      'COMPATIBILITY))
			    'APPLY-COMPATIBILITY)))))))))))

(define-integrable (close-rvalue! rvalue reason1 reason2)
  (close-values! (rvalue-values rvalue) false reason1 reason2))

(define (close-values! values binding-block reason1 reason2)
  (for-each (lambda (value)
	      (if (and (rvalue/procedure? value)
		       (not (procedure-continuation? value)))
		  (maybe-close-procedure! value
					  binding-block
					  reason1
					  reason2)))
	    values))

(define (maybe-close-procedure! procedure binding-block reason1 reason2)
  (let ((closing-limit (procedure-closing-limit procedure)))
    (cond ((not closing-limit)
	   (add-closure-reason! procedure reason1 reason2))
	  ((not (and binding-block
		     (block-ancestor-or-self? binding-block closing-limit)))
	   (set-procedure-closing-limit! procedure false)
	   (if (procedure-virtual-closure? procedure)
	       (set-procedure-virtual-closure?! procedure false))
	   (close-procedure! procedure reason1 reason2)))))

(define (close-procedure! procedure reason1 reason2)
  (let ((previously-trivial? (procedure/trivial-closure? procedure)))
    ;; We can't change the closing block yet.
    ;; blktyp has a consistency check that depends on the closing block
    ;; remaining the same.
    (add-closure-reason! procedure reason1 reason2)
    ;; Force the procedure's type to CLOSURE.
    (if (not (procedure-closure-context procedure))
	(set-procedure-closure-context! procedure true))
    ;; The code generator needs all callees to be closed.
    (let ((block (procedure-block procedure)))
      (for-each-callee! block
	(lambda (value)
	  (if (not (block-ancestor-or-self? (procedure-block value) block))
	      (maybe-close-procedure! value false 'CONTAGION procedure)))))
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
    (enqueue-nodes! (procedure-applications procedure))))

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

(define (examine-free-callers! procedure)
  (let ((block (procedure-block procedure)))
    (for-each
     (lambda (procedure*)
       (if (not (procedure-closure-context procedure*))
	   (let ((parent (procedure-closing-block procedure*))
		 (original-parent (procedure-target-block procedure*)))
	     ;; No need to do anything if PROCEDURE* hasn't drifted
	     ;; relative to PROCEDURE.
	     (if (and (not (eq? parent original-parent))
		      (not (block-ancestor-or-self? parent block)))
		 (let ((binding-block
			(reduce original-block-nearest-common-ancestor
				false
				(map variable-block
				     (cdr (assq procedure
						(procedure-free-callees
						 procedure*)))))))
		   (if (not (block-ancestor-or-self? parent binding-block))
		       ;; PROCEDURE* has drifted towards the
		       ;; environment root past the point where we
		       ;; have access to PROCEDURE (by means of free
		       ;; variables).  We must drift it away from
		       ;; the root until we regain access to PROCEDURE.
		       (undrift-procedure! procedure* binding-block)))))))
     (procedure-free-callers procedure))))

(define (undrift-procedure! procedure new-parent)
  (let ((block (procedure-block procedure))
	(parent (procedure-closing-block procedure))
	(original-parent (procedure-target-block procedure)))
    ;; (assert! (eq? parent (procedure-closing-limit procedure)))
    (set-block-children! parent (delq! block (block-children parent)))
    (set-block-parent! block new-parent)
    (set-block-children! new-parent (cons block (block-children new-parent)))
    (set-procedure-closing-limit! procedure new-parent)
    (enqueue-nodes! (cons procedure (procedure-applications procedure)))
    (if (eq? new-parent original-parent)
	(set-block-disowned-children!
	 original-parent
	 (delq! block (block-disowned-children original-parent)))
	(let ((parent-procedure (block-procedure original-parent)))
	  (if (and (not (block-ancestor-or-self? original-parent new-parent))
		   (rvalue/procedure? parent-procedure)
		   (not (procedure-closure-context parent-procedure)))
	      ;; My original parent has drifted to a place where I
	      ;; can't be closed.  I must drag it back.
	      (if (original-block-ancestor-or-self? original-parent new-parent)
		  (undrift-procedure! parent-procedure new-parent)
		  (error "Procedure has free variables in hyperspace!"
			 procedure)))))
    (examine-free-callers! procedure)))

;; These are like the corresponding standard block operations, but
;; they ignore any block drifting caused by envopt.

(define (original-block-ancestor-or-self? block block*)
  (define (loop block)
    (and block
	 (or (eq? block block*)
	     (loop (original-block-parent block)))))

  (or (eq? block block*)
      (loop (original-block-parent block))))

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

(define (original-block-ancestry block path)
  (let ((parent (original-block-parent block)))
    (if parent
	(original-block-ancestry parent (cons block path))
	(cons block path))))