#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/order.scm,v 4.13 1989/10/26 07:36:55 cph Exp $

Copyright (c) 1988, 1989 Massachusetts Institute of Technology

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

;;;; Argument Ordering Analysis

(declare (usual-integrations))

(define (subproblem-ordering parallels)
  (for-each (lambda (parallel)
	      (order-parallel! parallel false))
	    parallels))

(define (order-parallel! parallel constraints)
  constraints ;ignore
  (let ((previous-edges (node-previous-edges parallel))
	(next-edge (snode-next-edge parallel)))
    (let ((rest (edge-next-node next-edge)))
      (if rest
	  (begin
	    (edges-disconnect-right! previous-edges)
	    (edge-disconnect! next-edge)
	    (with-values
		(lambda ()
		  (order-subproblems/application
		   (parallel-application-node parallel)
		   (parallel-subproblems parallel)
		   rest))
	      (lambda (cfg subproblem-order)
		subproblem-order
		(edges-connect-right! previous-edges cfg)
		cfg)))))))

(define (order-subproblems/application application subproblems rest)
  (case (application-type application)
    ((COMBINATION)
     (if (and (combination/inline? application)
	      (or (combination/simple-inline? application)
		  (not (return-operator/reduction?
			(combination/continuation application)))))
	 (order-subproblems/inline application subproblems rest)
	 (order-subproblems/out-of-line application subproblems rest)))
    ((RETURN)
     (values
      (linearize-subproblems! continuation-type/effect subproblems rest)
      subproblems))
    (else
     (error "Unknown application type" application))))

(define (linearize-subproblems! continuation-type subproblems rest)
  (set-subproblem-types! subproblems continuation-type)
  (linearize-subproblems subproblems rest))

(define (linearize-subproblem! continuation-type subproblem rest)
  (set-subproblem-type! subproblem continuation-type)
  (linearize-subproblem subproblem rest))

(define (linearize-subproblems subproblems rest)
  (let loop ((subproblems subproblems))
    (if (null? subproblems)
	rest
	(linearize-subproblem (car subproblems)
			      (loop (cdr subproblems))))))

(define (linearize-subproblem subproblem rest)
  (let ((continuation (subproblem-continuation subproblem))
	(prefix (subproblem-prefix subproblem)))
    (if (subproblem-canonical? subproblem)
	(begin
	  (let ((node (continuation/entry-node continuation)))
	    (cond ((not node)
		   (set-continuation/entry-node! continuation rest))
		  ((and (cfg-node/noop? node)
			(not (snode-next-edge node)))
		   (create-edge! node set-snode-next-edge! rest))
		  (else
		   (error "Attempt to reattach continuation body"
			  continuation))))
	  (cfg-entry-node prefix))
	(scfg*node->node!
	 prefix
	 (scfg*node->node!
	  (if (eq? continuation-type/effect
		   (virtual-continuation/type continuation))
	      (make-null-cfg)
	      (make-virtual-return (virtual-continuation/context continuation)
				   continuation
				   (subproblem-rvalue subproblem)))
	  rest)))))

(define (order-subproblems/inline combination subproblems rest)
  (let ((inliner (combination/inliner combination))
	(context (combination/context combination)))
    (let ((operator (car subproblems))
	  (operands
	   (list-filter-indices (cdr subproblems) (inliner/operands inliner))))
      (set-inliner/operands! inliner operands)
      (with-values
	  (lambda ()
	    (discriminate-items operands subproblem-simple?))
	(lambda (simple complex)
	  (if (null? complex)
	      (begin
		(inline-subproblem-types! context
					  simple
					  continuation-type/register)
		(values
		 (linearize-subproblem! continuation-type/effect
					operator
					(linearize-subproblems simple rest))
		 (cons operator simple)))
	      (let ((push-set (cdr complex))
		    (value-set (cons (car complex) simple)))
		(inline-subproblem-types! context
					  push-set
					  continuation-type/push)
		(inline-subproblem-types! context
					  value-set
					  continuation-type/register)
		(values
		 (linearize-subproblem!
		  continuation-type/effect
		  operator
		  (linearize-subproblems
		   push-set
		   (linearize-subproblems
		    value-set
		    (scfg*node->node!
		     (scfg*->scfg!
		      (reverse!
		       (map (lambda (subproblem)
			      (let ((continuation
				     (subproblem-continuation subproblem)))
				(if (eq? (continuation*/type continuation)
					 continuation-type/effect)
				    (make-null-cfg)
				    (make-pop continuation))))
			    push-set)))
		     rest))))
		 (cons operator (append push-set value-set))))))))))

(define (inline-subproblem-types! context subproblems continuation-type)
  (for-each
   (lambda (subproblem)
     (set-subproblem-type!
      subproblem
      (if (let ((rvalue (subproblem-rvalue subproblem)))
	    (or (rvalue-known-constant? rvalue)
		(and (rvalue/reference? rvalue)
		     (not (variable/value-variable? (reference-lvalue rvalue)))
		     (reference-to-known-location? rvalue))))
	  (begin
	    (update-subproblem-contexts! context subproblem)
	    continuation-type/effect)
	  continuation-type)))
   subproblems))

(define (order-subproblems/out-of-line combination subproblems rest)
  (with-values
      (combination-ordering (combination/context combination)
			    (car subproblems)
			    (cdr subproblems)
			    (combination/model combination))
    (lambda (effect-subproblems push-subproblems)
      (set-combination/frame-size! combination (length push-subproblems))
      (with-values
	  (lambda ()
	    (order-subproblems/maybe-overwrite-block
	     combination push-subproblems rest
	     (lambda ()
	       (values (linearize-subproblems! continuation-type/push
					       push-subproblems
					       rest)
		       push-subproblems))))
	(lambda (cfg push-subproblem-order)
	  (values (linearize-subproblems! continuation-type/effect
					  effect-subproblems
					  cfg)
		  (append effect-subproblems push-subproblem-order)))))))

(define (combination-ordering context operator operands model)
  (let ((standard
	 (lambda ()
	   (handle-operator context
			    operator
			    (operator-needed? (subproblem-rvalue operator))
			    '()
			    (reverse operands))))
	(optimized
	 (lambda ()
	   (optimized-combination-ordering context operator operands model)))
	(known
	 (lambda ()
	   (known-combination-ordering context operator operands model))))
    (if (and model (rvalue/procedure? model))
	(let ((model-block (procedure-block model)))
	  (if (not (stack-block? model-block))
	      standard
	      (let ((thunk
		     (cond

		      ;; At this point, the following should be true.
		      ;; (procedure-interface-optimizible? model)
		      ((procedure-always-known-operator? model) optimized)

		      ;; The behavior of known lexpr closures should
		      ;; be improved at least when the listification
		      ;; is trivial (0 or 1 args).
		      ((procedure-rest model) standard)

		      (else known))))
		(if (and (procedure/open? model)
			 (stack-block/static-link? model-block))
		    (lambda ()
		      (with-values thunk
			(lambda (effect-subproblems push-subproblems)
			  (values
			   effect-subproblems
			   (cons (new-subproblem context
						 (block-parent model-block))
				 push-subproblems)))))
		    thunk))))
	standard)))

(define (optimized-combination-ordering context operator operands callee)
  (with-values
      (lambda ()
	(sort-subproblems/out-of-line operands callee))
    (lambda (n-unassigned integrated non-integrated)
      (handle-operator context
		       operator
		       (operator-needed? (subproblem-rvalue operator))
		       integrated
		       (make-unassigned-subproblems context
						    n-unassigned
						    non-integrated)))))

(define (known-combination-ordering context operator operands procedure)
  (if (and (not (procedure/closure? procedure))
	   (not (procedure-virtual-closure? procedure)))
      (error "known-combination-ordering: known non-closure" procedure))
  (handle-operator
   context
   operator
   (or (not (rvalue-known-value (subproblem-rvalue operator)))
       (and (procedure/closure? procedure)
	    (closure-procedure-needs-operator? procedure)))
   '()
   (make-unassigned-subproblems
    context
    (let ((n-supplied (length operands))
	  (n-required
	   (length (cdr (procedure-original-required procedure))))
	  (n-optional (length (procedure-original-optional procedure))))
      (let ((n-expected (+ n-required n-optional)))
	(if (or (< n-supplied n-required) (> n-supplied n-expected))
	    (error "known-combination-ordering: wrong number of arguments"
		   procedure n-supplied n-expected))
	(- n-expected n-supplied)))
    (reverse operands))))

(define (handle-operator context operator operator-needed? effect push)
  (if operator-needed?
      (values effect (append! push (list operator)))
      (begin
	(update-subproblem-contexts! context operator)
	(values (cons operator effect) push))))

(define (make-unassigned-subproblems context n rest)
  (let ((unassigned (make-constant (make-unassigned-reference-trap))))
    (let loop ((n n) (rest rest))
      (if (zero? n)
	  rest
	  (loop (-1+ n)
		(cons (new-subproblem context unassigned) rest))))))

(define (new-subproblem context rvalue)
  (let ((subproblem
	 (make-subproblem
	  (make-null-cfg)
	  (virtual-continuation/make
	   (make-reference-context (reference-context/block context))
	   continuation-type/value)
	  rvalue)))
    (new-subproblem/compute-simplicity! subproblem)
    (new-subproblem/compute-free-variables! subproblem)
    subproblem))

(define (set-subproblem-types! subproblems type)
  (for-each (lambda (subproblem)
	      (set-subproblem-type! subproblem type))
	    subproblems))

(define (sort-subproblems/out-of-line all-subproblems callee)
  (with-values
      (lambda ()
	(sort-integrated (cdr (procedure-original-required callee))
			 all-subproblems
			 '()
			 '()))
    (lambda (required subproblems integrated non-integrated)
      (let ((unassigned-count 0))
	(if (not (null? required))
	    (begin
	      ;; This is a wrong number of arguments case, so the code
	      ;; we generate will not be any good.
	      ;; The missing arguments are defaulted.
	      (error "sort-subproblems/out-of-line: Too few arguments"
		     callee all-subproblems)
	      ;; This does not take into account potential integrated
	      ;; required parameters, but they better not be integrated
	      ;; if they are not always provided!
	      (set! unassigned-count (length required))))
	(with-values
	    (lambda ()
	      (sort-integrated (procedure-original-optional callee)
			       subproblems
			       integrated
			       non-integrated))
	  (lambda (optional subproblems integrated non-integrated)
	    (let ((rest (procedure-original-rest callee)))
	      (cond ((not (null? optional))
		     (values (if rest
				 0	; unassigned-count might work too
				 ;; In this case the caller will
				 ;; make slots for the optionals.
				 (+ unassigned-count
				    (length
				     (list-transform-negative optional
				       lvalue-integrated?))))
			     integrated
			     non-integrated))
		    ((and (not (null? subproblems)) (not rest))
		     (error "sort-subproblems/out-of-line: Too many arguments"
			    callee all-subproblems)
		     ;; This is a wrong number of arguments case, so
		     ;; the code we generate will not be any good.
		     ;; The extra arguments are dropped!  Note that in
		     ;; this case unassigned-count should be 0, since
		     ;; we cannot have both too many and too few
		     ;; arguments simultaneously.
		     (values unassigned-count
			     integrated
			     non-integrated))
		    ((and rest (variable-unused? rest))
		     (values unassigned-count
			     (append! (reverse subproblems) integrated)
			     non-integrated))
		    (else
		     (values unassigned-count
			     integrated
			     (append! (reverse subproblems)
				      non-integrated)))))))))))

(define (sort-integrated lvalues subproblems integrated non-integrated)
  (cond ((or (null? lvalues) (null? subproblems))
	 (values lvalues subproblems integrated non-integrated))
	((variable-unused? (car lvalues))
	 (sort-integrated (cdr lvalues)
			  (cdr subproblems)
			  (cons (car subproblems) integrated)
			  non-integrated))
	(else
	 (sort-integrated (cdr lvalues)
			  (cdr subproblems)
			  integrated
			  (cons (car subproblems) non-integrated)))))

(define (operator-needed? operator)
  (let ((callee (rvalue-known-value operator)))
    (cond ((not callee)
	   (or (not (reference? operator))
	       (reference-to-known-location? operator)))
	  ((rvalue/constant? callee)
	   (not (primitive-procedure? (constant-value callee))))
	  ((rvalue/procedure? callee)
	   (case (procedure/type callee)
	     ((OPEN-EXTERNAL OPEN-INTERNAL) false)
	     ((TRIVIAL-CLOSURE) (procedure-rest callee))
	     ((CLOSURE IC) true)
	     (else (error "Unknown procedure type" callee))))
	  (else
	   true))))

(define (update-subproblem-contexts! context subproblem)
  (if (not (subproblem-canonical? subproblem))
      (update-rvalue-contexts! context (subproblem-rvalue subproblem))))

(define (update-rvalue-contexts! context rvalue)
  (let ((check-old
	 (lambda (context*)
	   (if (not (eq? (reference-context/block context)
			 (reference-context/block context*)))
	       (error "mismatched reference contexts" context context*))
	   (not (eq? context context*)))))
    (enumeration-case rvalue-type (tagged-vector/index rvalue)
      ((REFERENCE)
       (if (check-old (reference-context rvalue))
	   (set-reference-context! rvalue context)))
      ((UNASSIGNED-TEST)
       (if (check-old (unassigned-test-context rvalue))
	   (set-unassigned-test-context! rvalue context)))
      ((PROCEDURE)
       (if (let ((context* (procedure-closure-context rvalue)))
	     (and (reference-context? context*)
		  (check-old context*)))
	   (set-procedure-closure-context! rvalue context))))))