#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/closan.scm,v 4.4 1988/11/01 04:51:03 jinx Exp $

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
result, the analysis has been modified to force the closure-limit to
#F whenever a closure is identified.

|#

(package (identify-closure-limits!)

(define-export (identify-closure-limits! procedures applications assignments)
  (for-each initialize-closure-limit! procedures)
  (for-each close-passed-out! procedures)
  (for-each close-assignment-values! assignments)
  (close-application-elements! applications))

(define (initialize-closure-limit! procedure)
  (if (not (procedure-continuation? procedure))
      (set-procedure-closing-limit! procedure
				    (procedure-closing-block procedure))))

(define (close-passed-out! procedure)
  (if (and (not (procedure-continuation? procedure))
	   (procedure-passed-out? procedure))
      (close-procedure! procedure false 'PASSED-OUT false)))

(define (close-assignment-values! assignment)
  (close-rvalue! (assignment-rvalue assignment)
		 (variable-block (assignment-lvalue assignment))
		 'ASSIGNMENT
		 (assignment-lvalue assignment)))

(define-integrable (close-application-arguments! application)
  (close-values!
   (application-operand-values application)
   (let ((procedure (rvalue-known-value (application-operator application))))
     (and procedure
	  (rvalue/procedure? procedure)
	  (procedure-always-known-operator? procedure)
	  (procedure-block procedure)))
   'ARGUMENT
   application))

;; This attempts to find the cases where all procedures are closed in
;; same block.  This case can be solved by introduction of another
;; kind of closure, which has a fixed environment but carries around a
;; pointer to the code.

(define (close-application-elements! applications)
  (let loop ((applications applications)
	     (potential-winners '()))
    (if (null? applications)
	(maybe-close-multiple-operators! potential-winners)
	(let ((application (car applications)))
	  (close-application-arguments! application)
	  (let ((operator (application-operator application)))
	    (cond ((not (application/combination? application))
		   (loop (cdr applications) potential-winners))
		  ((rvalue-passed-in? operator)
		   (close-rvalue! operator false
				  'APPLY-COMPATIBILITY application)
		   (loop (cdr applications) potential-winners))
		  ((or (rvalue-known-value operator)
		       ;; Paranoia
		       (and (null? (rvalue-values operator))
			    (error "Operator has no values and not passed in"
				   operator application)))
		   (loop (cdr applications) potential-winners))
		  (else
		   (let ((class
			  (compatibility-class (rvalue-values operator))))
		     (if (not (eq? class 'APPLY-COMPATIBILITY))
			 (set-combination/model!
			  application
			  (car (rvalue-values operator))))
		     (if (eq? class 'POTENTIAL)
			 (loop (cdr applications)
			       (cons application potential-winners))
			 (begin
			   (close-rvalue! operator false class application)
			   (loop (cdr applications)
				 potential-winners)))))))))))

(define (with-procedure-arity proc receiver)
  (let ((req (length (procedure-required proc))))
    (receiver req
	      (if (procedure-rest proc)
		  -1
		  (+ req (length (procedure-optional proc)))))))

;; The reason each application may have to be examined more than once
;; is because the same procedure may be a potential operator in more
;; than one application.  The procedure may be forced into becoming a
;; closure due to one combination, forcing the others to become a
;; closure in other combinations, etc.  The procedure dependency graph
;; could be built, but since the number of applications in this
;; category is usually VERY small, it does not seem worth it.

(define (maybe-close-multiple-operators! applications)
  (define (virtually-close-operators! application)
    (for-each (lambda (proc)
		(set-procedure-virtual-closure?! proc true))
	      (rvalue-values (application-operator application))))

  (define (relax applications still-good any-bad?)
    (cond ((not (null? applications))
	   (let ((application (car applications)))
	     (if (there-exists?
		  (rvalue-values (application-operator application))
		  procedure/closure?)
		 (begin
		   (close-rvalue! (application-operator application)
				  false
				  'COMPATIBILITY
				  application)
		   (relax (cdr applications) still-good true))
		 (relax (cdr applications)
			(cons application still-good)
			any-bad?))))
	  (any-bad?
	   (relax still-good '() false))
	  (else
	   (for-each virtually-close-operators! still-good))))

  (relax applications '() false))

(define (compatibility-class procs)
  (if (not (for-all? procs rvalue/procedure?))
      'APPLY-COMPATIBILITY
      (let* ((model (car procs))
	     (model-env (procedure-closing-block model)))
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
				 (not (eq? (procedure-closing-block this)
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
		  (close-procedure! value binding-block reason1 reason2)))
	    values))

(define (close-procedure! procedure binding-block reason1 reason2)
  (let* ((closing-limit (procedure-closing-limit procedure))
	 (new-closing-limit
	  (and binding-block
	       closing-limit
	       (block-nearest-common-ancestor binding-block closing-limit))))
    (cond ((not (eq? new-closing-limit closing-limit))
	   ;; **** Force trivial closure limit due to poor code generator.
	   (let ((new-closing-limit false))
	     (set-procedure-closing-limit! procedure new-closing-limit)
	     (add-closure-reason! procedure reason1 reason2)
	     (if (not (procedure-closure-block procedure))
		 ;; Force the procedure's type to CLOSURE.
		 (set-procedure-closure-block! procedure true))
	     (close-callees! (procedure-block procedure)
			     new-closing-limit
			     procedure)))
	  ((false? new-closing-limit)
	   (add-closure-reason! procedure reason1 reason2)))))

(define (close-callees! block new-closing-limit culprit)
  (for-each-callee! block
    (lambda (value)
      (if (not (block-ancestor-or-self? (procedure-block value) block))
	  (close-procedure! value new-closing-limit 'CONTAGION culprit)))))

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