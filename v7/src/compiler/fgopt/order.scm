#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/order.scm,v 4.9 1988/11/01 04:52:18 jinx Exp $

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

;;;; Argument Ordering Analysis

(declare (usual-integrations))

(package (subproblem-ordering)

(define-export (subproblem-ordering parallels)
  (for-each (lambda (parallel)
	      (let ((previous-edges (node-previous-edges parallel))
		    (next-edge (snode-next-edge parallel)))
		(let ((rest
		       (or (edge-next-node next-edge)
			   (error "PARALLEL node missing next" parallel))))
		  (edges-disconnect-right! previous-edges)
		  (edge-disconnect! next-edge)
		  (edges-connect-right!
		   previous-edges
		   (parallel-replacement-node parallel rest)))))
	    parallels))

(define (parallel-replacement-node parallel rest)
  (transmit-values
      (order-subproblems/application (parallel-application-node parallel)
				     (parallel-subproblems parallel))
    (lambda (subproblems suffix)
      (linearize-subproblems subproblems (scfg*node->node! suffix rest)))))

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
	      (make-virtual-return (virtual-continuation/block continuation)
				   continuation
				   (subproblem-rvalue subproblem)))
	  rest)))))

(define (order-subproblems/application application subproblems)
  (case (application-type application)
    ((COMBINATION)
     (if (combination/inline? application)
	 (order-subproblems/combination/inline application subproblems)
	 (return-2 (order-subproblems/combination/out-of-line application
							      subproblems)
		   (make-null-cfg))))
    ((RETURN)
     (set-subproblem-types! subproblems continuation-type/effect)
     (return-2 subproblems (make-null-cfg)))
    (else
     (error "Unknown application type" application))))

(define (order-subproblems/combination/inline combination subproblems)
  (let ((inliner (combination/inliner combination)))
    (let ((operands
	   (list-filter-indices (cdr subproblems) (inliner/operands inliner))))
      (set-inliner/operands! inliner operands)
      (order-subproblems/inline (car subproblems) operands))))

(define (order-subproblems/inline operator operands)
  (set-subproblem-type! operator continuation-type/effect)
  (transmit-values (discriminate-items operands subproblem-simple?)
    (lambda (simple complex)
      (if (null? complex)
	  (begin
	    (inline-subproblem-types! simple continuation-type/register)
	    (return-2 (cons operator operands) (make-null-cfg)))
	  (let ((push-set (cdr complex))
		(value-set (cons (car complex) simple)))
	    (inline-subproblem-types! push-set continuation-type/push)
	    (inline-subproblem-types! value-set continuation-type/register)
	    (return-2 (cons operator (append! push-set value-set))
		      (scfg*->scfg!
		       (reverse!
			(map (lambda (subproblem)
			       (make-pop (subproblem-continuation subproblem)))
			     push-set)))))))))

(define (inline-subproblem-types! subproblems continuation-type)
  (for-each (lambda (subproblem)
	      (set-subproblem-type!
	       subproblem
	       (if (let ((rvalue (subproblem-rvalue subproblem)))
		     (or (rvalue-known-constant? rvalue)
			 (and (rvalue/reference? rvalue)
			      (not (variable/value-variable?
				    (reference-lvalue rvalue)))
			      (reference-to-known-location? rvalue))))
		   continuation-type/effect
		   continuation-type)))
	    subproblems))

(define (order-subproblems/combination/out-of-line combination subproblems)
  (let ((subproblems
	 (order-subproblems/out-of-line
	  (combination/block combination)
	  (car subproblems)
	  (cdr subproblems)
	  (or (rvalue-known-value (combination/operator combination))
	      (combination/model combination)))))
    (set-combination/frame-size!
     combination
     (let loop ((subproblems subproblems) (accumulator 0))
       (if (null? subproblems)
	   accumulator
	   (loop (cdr subproblems)
		 (if (eq? (subproblem-type (car subproblems))
			  continuation-type/push)
		     (1+ accumulator)
		     accumulator)))))
    subproblems))

(define (order-subproblems/out-of-line block operator operands model)
  (set-subproblem-type! operator (operator-type (subproblem-rvalue operator)))
  (if (and model (rvalue/procedure? model))
      (let ((rest
	     (cond ((not (stack-block? (procedure-block model)))
		    (standard-combination-ordering operator operands))
		   ((procedure-always-known-operator? model)
		    ;; At this point, the following should be true.
		    ;; (procedure-interface-optimizible? model)
		    (optimized-combination-ordering block
						    operator
						    operands
						    model))
		   (else
		    (known-combination-ordering block operator
						operands model)))))
	(if (procedure/open? model)
	    (generate/static-link block model rest)
	    rest))
      (standard-combination-ordering operator operands)))

(define (optimized-combination-ordering block operator operands callee)
  (transmit-values (sort-subproblems/out-of-line operands callee)
    (lambda (prefix integrated non-integrated)
      (set-subproblem-types! integrated continuation-type/effect)
      (set-subproblem-types! non-integrated continuation-type/push)
      (push-unassigned block
		       prefix
		       (append! integrated non-integrated (list operator))))))

(define (standard-combination-ordering operator operands)
  (set-subproblem-types! operands continuation-type/push)
  (reverse (cons operator operands)))

(define (known-combination-ordering block operator operands procedure)
  (if (and (not (procedure/closure? procedure))
	   (not (procedure-virtual-closure? procedure)))
      (error "known-combination-ordering: known non-closure" procedure))
  ;; The behavior of known lexpr closures should be improved
  ;; at least when the listification is trivial (0 or 1 args).
  (if (procedure-rest procedure)
      (standard-combination-ordering operator operands)
      (begin
	(set-subproblem-types! operands continuation-type/push)
	(set-subproblem-type!
	 operator
	 (if (or (not (rvalue-known-value (subproblem-rvalue operator)))
		 (and (procedure/closure? procedure)
		      (closure-procedure-needs-operator? procedure)))
	     continuation-type/push
	     continuation-type/effect))
	(push-unassigned block
			 (known-combination/number-of-unassigned operands
								 procedure)
			 (reverse (cons operator operands))))))

(define (known-combination/number-of-unassigned operands procedure)
  (let ((n-supplied (length operands))
	(n-required (length (cdr (procedure-original-required procedure))))
	(n-optional (length (procedure-original-optional procedure))))
    (let ((n-expected (+ n-required n-optional)))
      (if (or (< n-supplied n-required) (> n-supplied n-expected))
	  (error "known-combination-ordering: wrong number of arguments"
		 procedure n-supplied n-expected))
      (- n-expected n-supplied))))

(define (generate/static-link block procedure rest)
  (if (stack-block/static-link? (procedure-block procedure))
      (cons (make-push block (block-parent (procedure-block procedure))) rest)
      rest))

(define (push-unassigned block n rest)
  (let ((unassigned (make-constant (make-unassigned-reference-trap))))
    (let loop ((n n) (rest rest))
      (if (zero? n)
	  rest
	  (loop (-1+ n)
		(cons (make-push block unassigned) rest))))))

(define (make-push block rvalue)
  (make-subproblem (make-null-cfg)
		   (virtual-continuation/make block continuation-type/push)
		   rvalue))

(define (set-subproblem-types! subproblems type)
  (for-each (lambda (subproblem)
	      (set-subproblem-type! subproblem type))
	    subproblems))

(define (sort-subproblems/out-of-line all-subproblems callee)
  (transmit-values
      (sort-integrated (cdr (procedure-original-required callee))
		       all-subproblems
		       '()
		       '())
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
	(transmit-values
	 (sort-integrated (procedure-original-optional callee)
			  subproblems
			  integrated
			  non-integrated)
	 (lambda (optional subproblems integrated non-integrated)
	   (let ((rest (procedure-original-rest callee)))
	     (cond ((not (null? optional))
		    (return-3 (if rest
				  0	; unassigned-count might work too
				  ;; In this case the caller will
				  ;; make slots for the optionals.
				  (+ unassigned-count (length optional)))
			      integrated
			      non-integrated))
		   ((and (not (null? subproblems)) (not rest))
		    (error "sort-subproblems/out-of-line: Too many arguments"
			   callee all-subproblems)
		    ;; This is a wrong number of arguments case, so
		    ;; the code we generate will not be any good.
		    ;; The extra arguments are dropped!
		    ;; Note that in this case unassigned-count should be 0,
		    ;; since we cannot have both too many and too few arguments
		    ;; simultaneously.
		    (return-3 unassigned-count
			      integrated
			      non-integrated))
		   ((and rest (lvalue-integrated? rest))
		    (return-3 unassigned-count
			      (append! (reverse subproblems) integrated)
			      non-integrated))
		   (else
		    (return-3 unassigned-count
			      integrated
			      (append! (reverse subproblems)
				       non-integrated)))))))))))

(define (sort-integrated lvalues subproblems integrated non-integrated)
  (cond ((or (null? lvalues) (null? subproblems))
	 (return-4 lvalues subproblems integrated non-integrated))
	((lvalue-integrated? (car lvalues))
	 (sort-integrated (cdr lvalues)
			  (cdr subproblems)
			  (cons (car subproblems) integrated)
			  non-integrated))
	(else
	 (sort-integrated (cdr lvalues)
			  (cdr subproblems)
			  integrated
			  (cons (car subproblems) non-integrated)))))

(define (operator-type operator)
  (let ((callee (rvalue-known-value operator)))
    (cond ((not callee)
	   (if (and (reference? operator)
		    (not (reference-to-known-location? operator)))
	       continuation-type/effect
	       continuation-type/apply))
	  ((rvalue/constant? callee)
	   (if (normal-primitive-procedure? (constant-value callee))
	       continuation-type/effect
	       continuation-type/apply))
	  ((rvalue/procedure? callee)
	   (case (procedure/type callee)
	     ((OPEN-EXTERNAL OPEN-INTERNAL) continuation-type/effect)
	     ((CLOSURE)
	      (if (and (procedure/trivial-closure? callee)
		       (not (procedure-rest callee)))
		  continuation-type/effect
		  continuation-type/apply))
	     ((IC) continuation-type/apply)
	     (else (error "Unknown procedure type" callee))))
	  (else
	   continuation-type/apply))))

(define-integrable continuation-type/apply
  continuation-type/push)

)