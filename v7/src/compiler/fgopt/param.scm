#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/param.scm,v 1.2 1989/10/26 07:36:59 cph Rel $

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

;;;; Procedure parameter analysis
#|

A procedure is eligible for having its parameters be placed in
registers (i.e. the procedure is "registerizable") if the procedure
will be inlined and the frame reuse routine has not tried to overwrite
anything in the stack frame of this procedure or the stack frame
associated with any ancestors of this procedure's block.

Assuming that a procedure is registerizable, the parameter analysis
phase determines which of it's parameters will indeed be passed in
registers.

A parameter will be passed in a register if all references to that
parameter in the procedure occur before any calls to complex
procedures. A complex procedure is essentially a non-inlined,
non-open-coded procedure. Additionally, we must check to make sure
that there are no references to the parameter in any closures or
descendent blocks. Note that inlined and open-coded procedures that
are called within the analysed procedure are considered to be part of
that procedure.

At certain times (when we hit an as yet unordered parallel) we have
the opportunity to suggest an ordering of subproblems for a particular
parallel. We take that opportunity to request an ordering which would
place a reference to a parameter before any calls to complex procedures.
The order-parallel! procedure is free to ignore our suggestions.

A major deficit with the current scheme is the restriction on
registerizable procedures caused by the frame reuse stuff. The frame
reuse code is very aggressive and consequently there are very few
occasions where we can in fact place parameters in registers. The
problem is that the frame reuse code needs to know the stack layout,
but the placing of parameters in registers affects the use of the
stack. And because the parameter analysis code may call the subproblem
ordering code which may call the frame reuse code, we have a very
tricky timing problem. The correct solution may be to use a relaxation
technique and iterate the subproblem ordering so that we can put more
parameters in registers.

|#

(define (parameter-analysis procedure)
  (fluid-let ((*inlined-procedures* '()))
    (let ((interesting-parameters
	   (list-transform-positive (procedure-required procedure)
	     interesting-variable?)))
      (if interesting-parameters
	  (let ((registerizable-parameters
		 (with-new-node-marks
		  (lambda ()
		    (walk-node-for-search
		     (procedure-entry-node procedure))))))
	    ;; We have to check here if this procedure's block layout
	    ;; has been frozen by the frame reuse stuff which may
	    ;; have been called due to a call to order-parallel!
	    (if (block-layout-frozen? (procedure-block procedure))
		'()
		(eq-set-difference
		 (eq-set-difference interesting-parameters
				    registerizable-parameters)
		 (bad-free-variables procedure))))
	  '()))))

(define (walk-node-for-search node)
  (if (and node
	   (or (node-marked? node)
	       (begin
		 (node-mark! node)
		 (not (node-previous>1? node)))))
      (or (node/bad-variables node)
	  (cond ((and (application? node)
		      (application/combination? node)
		      (not (combination/simple-inline? node))
		      (not (let ((operator
				  (rvalue-known-value
				   (application-operator node))))
			     (and operator
				  (rvalue/procedure? operator)
				  (procedure-inline-code? operator)))))
		 (walk-next node walk-node-for-variables))
		((parallel? node)
		 (walk-node-for-search
		  (order-parallel!
		   node
		   (let ((subproblems (parallel-subproblems node)))
		     (if (for-all? subproblems subproblem-simple?)
			 false
			 (complex-parallel-constraints
			  subproblems
			  (walk-next node walk-node-for-variables)))))))
		(else
		 (walk-next node walk-node-for-search))))
      '()))

(define (walk-next node walker)
  (cond ((application? node)
	 (case (application-type node)
	   ((COMBINATION)
	    (let ((operator (rvalue-known-value (application-operator node))))
	      (if (and operator
		       (rvalue/procedure? operator)
		       (procedure-inline-code? operator))
		  (begin
		    (set! *inlined-procedures*
			  (cons operator *inlined-procedures*))
		    (walker (procedure-entry-node operator)))
		  (walk-continuation (combination/continuation node) walker))))
	   ((RETURN)
	    (walk-continuation (return/operator node) walker))
	   (else
	    (error "Illegal application type" node))))
	((snode? node)
	 (walker (snode-next node)))
	((pnode? node)
	 (eq-set-union (walker (pnode-consequent node))
		       (walker (pnode-alternative node))))
	(else
	 (error "Illegal node" node))))

(define *inlined-procedures*)

(define (walk-continuation continuation walker)
  (let ((rvalue (rvalue-known-value continuation)))
    (walker (and rvalue (continuation/entry-node rvalue)))))

(define (walk-node-for-variables node)
  (if node
      (if (parallel? node)
	  (walk-node-for-variables (order-parallel! node false))
	  (begin
	    (node-mark! node)
	    (or (node/bad-variables node)
		(let ((bad-variables
		       (eq-set-union
			(with-values (lambda () (find-node-values node))
			  values->variables)
			(walk-next node walk-node-for-variables))))
		  (set-node/bad-variables! node bad-variables)
		  bad-variables))))
      '()))

(define (find-node-values node)
  (let ((finish
	 (lambda (lvalue rvalue)
	   (values lvalue (if rvalue (list rvalue) '())))))
    (cfg-node-case (tagged-vector/tag node)
      ((APPLICATION)
       (case (application-type node)
	 ((COMBINATION)
	  (values false
		  (cons (combination/operator node)
			(combination/operands node))))
	 ((RETURN)
	  (finish false (return/operand node)))
	 (else
	  (error "Illegal application type" node))))
      ((VIRTUAL-RETURN)
       (finish false (virtual-return-operand node)))
      ((ASSIGNMENT)
       (finish (assignment-lvalue node)
	       (assignment-rvalue node)))
      ((DEFINITION)
       (finish (definition-lvalue node)
	       (definition-rvalue node)))
      ((STACK-OVERWRITE)
       (values (let ((target (stack-overwrite-target node)))
		 (and (lvalue? target) target))
	       '()))
      ((PARALLEL)
       (values false
	       (append-map subproblem-free-variables
			   (parallel-subproblems node))))
      ((POP FG-NOOP)
       (values false '()))
      ((TRUE-TEST)
       (finish false (true-test-rvalue node))))))

(define (values->variables lvalue rvalues)
  (eq-set-union
   (list->eq-set
    (and lvalue
	 (lvalue/variable? lvalue)
	 (interesting-variable? lvalue)
	 (list lvalue)))
   (map->eq-set (lambda (rvalue) (reference-lvalue rvalue))
		(list-transform-positive rvalues
		  (lambda (rvalue)
		    (and (rvalue/reference? rvalue)
			 (let ((lvalue (reference-lvalue rvalue)))
			   (and lvalue
				(lvalue/variable? lvalue)
				(interesting-variable? lvalue)))))))))

(define (complex-parallel-constraints subproblems vars-referenced-later)
  (with-values (lambda () (discriminate-items subproblems subproblem-simple?))
    (lambda (simple complex)
      (let ((discriminate-by-bad-vars
	     (lambda (subproblems)
	       (discriminate-items subproblems
		 (lambda (subproblem)
		   (there-exists? (subproblem-free-variables subproblem)
		     (lambda (var)
		       (memq var vars-referenced-later)))))))
	    (constraint-graph (make-constraint-graph)))
	(with-values (lambda () (discriminate-by-bad-vars simple))
	  (lambda (good-simples bad-simples)
	    (with-values (lambda () (discriminate-by-bad-vars complex))
	      (lambda (good-complex bad-complex)
		(add-constraint-set! good-simples
				     good-complex
				     constraint-graph)
		(add-constraint-set! good-complex
				     (append bad-simples bad-complex)
				     constraint-graph)))
	    constraint-graph))))))

(define-integrable (node/bad-variables node)
  (cfg-node-get node node/bad-variables-tag))

(define-integrable (set-node/bad-variables! node refs)
  (cfg-node-put! node node/bad-variables-tag refs))

(define node/bad-variables-tag
  "bad-variables-tag")

(define (bad-free-variables procedure)
  (append-map block-variables-nontransitively-free
	      (list-transform-negative
		  (cdr (linearize-block-tree (procedure-block procedure)))
		(lambda (block)
		  (memq (block-procedure block) *inlined-procedures*)))))

;;; Since the order of this linearization is not important we could
;;; make this routine more efficient. I'm not sure that it is worth
;;; it. If anyone does change it you should probably alter the line in
;;; bad-free-variables that says "(cdr (line..." to
;;; "(delq block (line..."
(define (linearize-block-tree block)
  (let ((children
	 (append (block-children block) (block-disowned-children block))))
    (if (null? children)
	(list block)
	(cons block (mapcan linearize-block-tree children)))))

(define (interesting-variable? variable)
  ;;; variables that will be in cells are eliminated from
  ;;; being put in registers because I couldn't figure out
  ;;; how to get the right code generated for them. Oh well,
  ;;; sigh! 
  (not (or (variable-assigned? variable)
	   (variable-stack-overwrite-target? variable)
	   (variable/continuation-variable? variable)
	   (variable/value-variable? variable))))