#| -*-Scheme-*-

$Id: order.scm,v 4.21 2007/01/05 15:33:03 cph Exp $

Copyright 1987,1988,1989,1990,2000,2004 Massachusetts Institute of Technology

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

;;;; Argument Ordering Analysis

(declare (usual-integrations))

(define (subproblem-ordering parallels)
  (for-each (lambda (parallel)
	      (order-parallel! parallel #f))
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
      (linearize-subproblems! continuation-type/effect subproblems '() rest)
      subproblems))
    (else
     (error "Unknown application type" application))))

(define (linearize-subproblems! continuation-type subproblems alist rest)
  (set-subproblem-types! subproblems continuation-type)
  (linearize-subproblems subproblems alist rest))

(define (linearize-subproblem! continuation-type subproblem lvalue rest)
  (set-subproblem-type! subproblem continuation-type)
  (linearize-subproblem subproblem lvalue rest))

(define (linearize-subproblems subproblems alist rest)
  (let loop ((subproblems subproblems))
    (if (null? subproblems)
	rest
	(linearize-subproblem (car subproblems)
			      (let ((entry (assq (car subproblems) alist)))
				(and entry
				     (cdr entry)))
			      (loop (cdr subproblems))))))

(define (linearize-subproblem subproblem lvalue rest)
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
	      (let ((cfg
		     (make-virtual-return
		      (virtual-continuation/context continuation)
		      continuation
		      (subproblem-rvalue subproblem))))
		(if lvalue
		    (let ((node (cfg-entry-node cfg)))
		      (set-variable-source-node! lvalue node)
		      (set-virtual-return/target-lvalue! node lvalue)))
		cfg))
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
					#f
					(linearize-subproblems simple
							       '()
							       rest))
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
		  #f
		  (linearize-subproblems
		   push-set
		   '()
		   (linearize-subproblems
		    value-set
		    '()
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
  (let ((alist (add-defaulted-subproblems! combination subproblems)))
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
	       combination push-subproblems rest alist
	       (lambda ()
		 (values (linearize-subproblems! continuation-type/push
						 push-subproblems
						 alist
						 rest)
			 push-subproblems))))
	  (lambda (cfg push-subproblem-order)
	    (values (linearize-subproblems! continuation-type/effect
					    effect-subproblems
					    alist
					    cfg)
		    (append effect-subproblems push-subproblem-order))))))))

(define (add-defaulted-subproblems! combination subproblems)
  (let ((model (combination/model combination)))
    (if (and model
	     (rvalue/procedure? model)
	     (stack-block? (procedure-block model))
	     (or (procedure-always-known-operator? model)
		 (not (procedure-rest model))))
	(let ((n-defaulted
	       (let ((n-supplied (length (cdr subproblems)))
		     (n-required
		      (length (cdr (procedure-original-required model)))))
		 (let ((n-expected
			(+ n-required
			   (length (procedure-original-optional model)))))
		   (if (or (< n-supplied n-required)
			   (and (> n-supplied n-expected)
				(not (procedure-rest model))))
		       (warn "wrong number of arguments"
			     n-supplied
			     (error-irritant/noise char:newline)
			     (error-irritant/noise "in call to procedure")
			     (procedure-name model)
			     (error-irritant/noise char:newline)
			     (error-irritant/noise
			      "minimum/maximum number of arguments:")
			     n-required
			     n-expected))
		   (- n-expected n-supplied))))
	      (parallel (application-parallel-node combination)))
	  (if (positive? n-defaulted)
	      (set-parallel-subproblems!
	       parallel
	       (append! subproblems
			(make-defaulted-subproblems
			 (combination/context combination)
			 n-defaulted
			 '()))))
	  (let ((parameters
		 (append (cdr (procedure-original-required model))
			 (procedure-original-optional model)))
		(arguments (cdr (parallel-subproblems parallel))))
	    (map (lambda (variable subproblem)
		   (cons subproblem variable))
		 parameters
		 (let ((n-parameters (length parameters)))
		   (if (> (length arguments) n-parameters)
		       (list-head arguments n-parameters)
		       arguments)))))
	'())))

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
		     (cond ((procedure-always-known-operator? model) optimized)
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
    (lambda (integrated non-integrated)
      (handle-operator context
		       operator
		       (operator-needed? (subproblem-rvalue operator))
		       integrated
		       non-integrated))))

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
   (reverse operands)))

(define (handle-operator context operator operator-needed? effect push)
  (if operator-needed?
      (values effect (append! push (list operator)))
      (begin
	(update-subproblem-contexts! context operator)
	(values (cons operator effect) push))))

(define (make-defaulted-subproblems context n rest)
  (let ((default (make-constant (default-object))))
    (let loop ((n n) (rest rest))
      (if (zero? n)
	  rest
	  (loop (-1+ n)
		(cons (new-subproblem context default) rest))))))

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
    (lambda (subproblems integrated non-integrated)
      (with-values
	  (lambda ()
	    (sort-integrated (procedure-original-optional callee)
			     subproblems
			     integrated
			     non-integrated))
	(lambda (subproblems integrated non-integrated)
	  (let ((rest (procedure-original-rest callee)))
	    (cond ((and (not (null? subproblems)) (not rest))
		   ;; This is a wrong number of arguments case, so
		   ;; the code we generate will not be any good.
		   ;; The extra arguments are dropped!
		   (values integrated
			   non-integrated))
		  ((and rest (variable-unused? rest))
		   (values (append! (reverse subproblems) integrated)
			   non-integrated))
		  (else
		   (values integrated
			   (append! (reverse subproblems)
				    non-integrated))))))))))

(define (sort-integrated lvalues subproblems integrated non-integrated)
  (cond ((null? lvalues)
	 (values subproblems integrated non-integrated))
	((null? subproblems)
	 (error "sort-integrated: not enough subproblems" lvalues))
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
	     ((OPEN-EXTERNAL OPEN-INTERNAL) #f)
	     ((TRIVIAL-CLOSURE) (procedure-rest callee))
	     ((CLOSURE IC) #t)
	     (else (error "Unknown procedure type" callee))))
	  (else #t))))

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