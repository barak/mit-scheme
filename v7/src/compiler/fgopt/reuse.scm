#| -*-Scheme-*-

$Id: reuse.scm,v 1.9 2002/11/20 19:45:49 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

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
		       (and (not (combination/simple-inline? combination))
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
						 alist if-no-overwrite)
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
			(values
			 (linearize-subproblems! continuation-type/push
						 extra-subproblems
						 alist
						 cfg)
			 (append extra-subproblems subproblem-ordering)))))
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
			   (eq? (car dependents)
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
		   (or (and (rvalue/procedure? value)
			    (procedure/closure? value)
			    (eq? value (block-procedure block)))
		       (and (rvalue/block? value)
			    (block-shares? block value))))
	      (block-ancestor? block definition-block)
	      (let loop ((block block))
		(if (closure-block? block)
		    (memq variable (block-bound-variables block))
		    (let ((parent (block-parent block)))
		      (and parent (loop parent))))))))))

(define (block-shares? block block*)
  (or (block-ancestor-or-self? block block*)
      (and (closure-block? block*)
	   (let ((bclos (block-nearest-closure-ancestor block)))
	     (and bclos
		  (eq? (block-shared-block (block-parent bclos))
		       (block-shared-block block*)))))))

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
	  false
	  (generate-assignments (cdr nodes)
				(overwrite (car nodes) rest))))
	(else
	 (trivial-assignment (car nodes)
			     (generate-assignments (cdr nodes) rest)))))

(define (trivial-assignments nodes rest)
  (let loop ((nodes nodes))
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
			     false
			     (overwrite node rest))))

(define (node/noop? node)
  (let ((target (node-target node))
	(subproblem (node-value node)))
    (and (cfg-null? (subproblem-prefix subproblem))
	 (let ((rvalue (subproblem-rvalue subproblem)))
	   (cond ((rvalue/reference? rvalue)
		  (let ((variable (reference-lvalue rvalue)))
		    (and (eq? target variable)
			 ;; This is no good because this field
			 ;; is not yet set up at this stage
			 ;; of the compilation.
			 ;; (not (variable-in-cell? variable))
			 (not (variable-assigned? variable)))))
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