#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/reuse.scm,v 1.2 1989/04/21 17:09:50 markf Exp $

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

;;;; Reduction Combinations: Frame Reuse

(declare (usual-integrations))

(define (setup-frame-adjustments applications)
  (for-each
   (lambda (combination)
     (if (application/combination? combination)
	 (set-combination/frame-adjustment!
	  combination
	  (let ((block (combination/block combination)))
	    (let ((adjustment
		   (let ((callee (combination/model combination)))
		     (let ((callee-internal?
			    (and callee
				 (rvalue/procedure? callee)
				 (procedure/open-internal? callee)))
			   (caller (block-procedure block)))
		       (and (not (combination/inline? combination))
			    (return-operator/reduction?
			     (combination/continuation combination))
			    (rvalue/procedure? caller)
			    (not (procedure/ic? caller))
			    (cond ((procedure/external? caller)
				   (and (not callee-internal?) block))
				  (callee-internal?
				   (let ((block* (procedure-block callee)))
				     (and (not (block-child? block block*))
					  (block-farthest-uncommon-ancestor
					   block
					   (block-parent block*)))))
				  (else
				   (stack-block/external-ancestor block))))))))
	      (and adjustment
		   (if (for-all? (block-popping-limits block)
			 (lambda (limit)
			   (block-ancestor-or-self? adjustment limit)))
		       (cons 'KNOWN adjustment)
		       (let ((limit (block-popping-limit block)))
			 (if limit
			     (cons 'KNOWN
				   (if (block-ancestor? adjustment limit)
				       adjustment
				       limit))
			     (cons 'UNKNOWN adjustment))))))))))
   applications))

(define (order-subproblems/maybe-overwrite-block combination subproblems rest
						 if-no-overwrite)
  (let ((caller-block (combination/block combination))
	;; This reduces code size.
	(if-no-overwrite (lambda () (if-no-overwrite))))
    (let ((overwritten-block
	   (let ((adjustment (combination/frame-adjustment combination)))
	     (and adjustment
		  (eq? (car adjustment) 'KNOWN)
		  (cdr adjustment)))))
      (if overwritten-block
	  (with-values
	      (lambda ()
		(subproblems->nodes subproblems
				    caller-block
				    overwritten-block))
	    (lambda (terminal-nodes non-terminal-nodes extra-subproblems)
	      (if (< (length non-terminal-nodes) reuse-size-limit)
		  (begin
		    (set-combination/reuse-existing-frame?! combination
							    overwritten-block)
		    (with-values
			(lambda ()
			  (order-subproblems/overwrite-block
			   caller-block
			   overwritten-block
			   terminal-nodes
			   non-terminal-nodes
			   rest))
		      (lambda (cfg subproblem-ordering)
			(let ((cfg (linearize-subproblems!
				    continuation-type/push
				    extra-subproblems
				    cfg)))
			  (values
			   cfg
			   (append extra-subproblems subproblem-ordering))))))
		  (if-no-overwrite))))
	  (if-no-overwrite)))))

(define reuse-size-limit 7)

(define (subproblems->nodes subproblems caller-block overwritten-block)
  (with-values
      (lambda ()
	(let ((n-subproblems (length subproblems)))
	  (let ((targets
		 (overwritten-objects! caller-block
				       overwritten-block
				       n-subproblems)))
	    (let ((n-targets (length targets))
		  (make-nodes
		   (lambda (subproblems)
		      ;; The subproblems are given to us in pushing order.
		     (let ((subproblems (reverse subproblems)))
		       (make-node-set
			targets
			subproblems
			(map (subproblem/dependency-set targets
							overwritten-block)
			     subproblems))))))
	      (if (< n-targets n-subproblems)
		  (values (make-nodes (list-head subproblems n-targets))
			  (list-tail subproblems n-targets))
		  (values (make-nodes subproblems) '()))))))
    (lambda (nodes extra-subproblems)
      (with-values
	  (lambda ()
	    (discriminate-items nodes
	      (lambda (node)
		(let ((dependents (node-original-dependents node)))
		  (or (null? dependents)
		      (and (null? (cdr dependents))
			   (eq? (node-target (car dependents))
				(node-target node))))))))
	(lambda (terminal-nodes non-terminal-nodes)
	  (values terminal-nodes non-terminal-nodes extra-subproblems))))))

(define (overwritten-objects! caller-block overwritten-block overwriting-size)
  (let ((stack-layout
	 (let loop ((block caller-block))
	   (set-block-layout-frozen?! block true)
	   (if (eq? block overwritten-block)
	       (block-layout block)
	       (append! (block-layout block) (loop (block-parent block)))))))
    (let ((n-items (length stack-layout)))
      (if (< overwriting-size n-items)
	  (list-tail stack-layout (- n-items overwriting-size))
	  stack-layout))))

(define (block-layout block)
  ;; When representing static links or closures in the result, we use
  ;; `block' rather than its parent, because it simplifies locating
  ;; the parent pointer on the stack.
  (let ((procedure (block-procedure block)))
    (append
     (procedure-names procedure)
     (if (and (procedure/closure? procedure)
	      (closure-procedure-needs-operator? procedure))
	 (list block)
	 '())
     (list-transform-negative
	 (cdr (procedure-required procedure))
       (lambda (variable)
	 (or (lvalue-integrated? variable)
	     (variable-register variable))))
     (procedure-optional procedure)
     (if (procedure-rest procedure) (list (procedure-rest procedure)) '())
     (if (and (not (procedure/closure? procedure))
	      (stack-block/static-link? block))
	 (list block)
	 '()))))

(define (subproblem/dependency-set targets overwritten-block)
  (let ((block (and (memq overwritten-block targets) overwritten-block)))
    (if (not block)
	(lambda (subproblem)
	  (list-transform-positive (subproblem-free-variables subproblem)
	    (lambda (variable)
	      (memq variable targets))))
	(lambda (subproblem)
	  (let loop
	      ((variables (subproblem-free-variables subproblem))
	       (dependencies '())
	       (block-dependency false))
	    (cond ((null? variables)
		   (if (or block-dependency
			   (let ((rvalue (subproblem-rvalue subproblem)))
			     (and (rvalue/block? rvalue)
				  (block-ancestor? block rvalue))))
		       (cons block dependencies)
		       dependencies))
		  ((memq (car variables) targets)
		   (loop (cdr variables)
			 (cons (car variables) dependencies)
			 block-dependency))
		  (else
		   (loop (cdr variables)
			 dependencies
			 (or block-dependency
			     (block-dependency? block (car variables)))))))))))

(define (block-dependency? block variable)
  (let ((definition-block (variable-block variable)))
    (if (ic-block? definition-block)
	(and (not (ic-block/use-lookup? definition-block))
	     (block-ancestor? block definition-block))
	(let ((value (lvalue-known-value variable)))
	  (or (and value
		   (rvalue/procedure? value)
		   (procedure/closure? value)
		   (eq? value (block-procedure block)))
	      (block-ancestor? block definition-block)
	      (let loop ((block block))
		(if (closure-block? block)
		    (memq variable (block-bound-variables block))
		    (let ((parent (block-parent block)))
		      (and parent (loop parent))))))))))
(define (order-subproblems/overwrite-block caller-block
					   overwritten-block
					   terminal-nodes
					   non-terminal-nodes
					   rest)
  (let* ((reordered-non-terms (reorder-assignments non-terminal-nodes))
	 (node
	  (trivial-assignments
	   terminal-nodes
	   (generate-assignments reordered-non-terms rest))))
      (if (not (eq? caller-block overwritten-block))
	  (modify-reference-contexts! node rest
	    (let ((blocks
		   (block-partial-ancestry caller-block overwritten-block)))
	      (lambda (context)
		(add-reference-context/adjacent-parents! context blocks)))))
      (values node
	      (map node-value
		   (list-transform-negative
		       (append terminal-nodes reordered-non-terms)
		     node/noop?)))))

(define (generate-assignments nodes rest)
  (cond ((null? nodes)
	 rest)
	((first-node-needs-temporary? nodes)
	 (linearize-subproblem!
	  (if (for-all? (cdr nodes)
		(lambda (node)
		  (subproblem-simple? (node-value node))))
	      continuation-type/register
	      continuation-type/push)
	  (node-value (car nodes))
	  (generate-assignments (cdr nodes)
				(overwrite (car nodes) rest))))
	(else
	 (trivial-assignment (car nodes)
			     (generate-assignments (cdr nodes) rest)))))

(define (trivial-assignments nodes rest)
  (let loop ((nodes
	      (order-nodes-per-current-constraints nodes)))
    (if (null? nodes)
	rest
	(trivial-assignment (car nodes) (loop (cdr nodes))))))

(define (trivial-assignment node rest)
  (if (node/noop? node)
      (begin
	(let ((target (node-target node)))
	  (and (lvalue? target)
	       (lvalue/variable? target)
	       (set-variable-stack-overwrite-target?! target
						      true)))
	rest)
      (linearize-subproblem! continuation-type/register
			     (node-value node)
			     (overwrite node rest))))

(define (node/noop? node)
  (let ((target (node-target node))
	(subproblem (node-value node)))
    (and (cfg-null? (subproblem-prefix subproblem))
	 (let ((rvalue (subproblem-rvalue subproblem)))
	   (cond ((rvalue/reference? rvalue)
		  (let ((variable (reference-lvalue rvalue)))
		    (and (eq? target variable)
			 (not (variable-in-cell? variable)))))
		 ((rvalue/block? rvalue)
		  (and (block? target)
		       (eq? (block-parent target) rvalue)))
		 (else false))))))

(define (overwrite node rest)
  (let ((subproblem (node-value node))
	(target (node-target node)))
    (if (and (lvalue? target)
	     (lvalue/variable? target))
	(set-variable-stack-overwrite-target?! target
					       true))
    (scfg*node->node!
     (make-stack-overwrite (subproblem-context subproblem)
			   target
			   (subproblem-continuation subproblem))
     rest)))

(define (order-nodes-per-current-constraints nodes)
  (if *current-constraints*
      (order-per-constraints/extracted
       nodes
       *current-constraints*
       node-value)
      nodes))

