#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/blktyp.scm,v 4.13 1989/10/26 07:36:36 cph Exp $

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

;;;; Environment Type Assignment

(declare (usual-integrations))

(define (setup-block-types! root-block)
  (define (loop block)
    (enumeration-case block-type (block-type block)
      ((PROCEDURE)
       (if (block-passed-out? block)
	   (block-type! block block-type/ic)
	   (begin
	     (block-type! block block-type/stack)
	     (maybe-close-procedure! (block-procedure block)))))
      ((CONTINUATION)
       (for-each loop (block-children block)))
      ((EXPRESSION)
       (if (not (block-passed-out? block))
	   (error "Expression block not passed out" block))
       (block-type! block block-type/ic))
      (else
       (error "Illegal block type" block))))

  (define (block-type! block type)
    (set-block-type! block type)
    (for-each loop (block-children block)))

  (loop root-block))

(define (maybe-close-procedure! procedure)
  (if (eq? true (procedure-closure-context procedure))
      (close-procedure! procedure)))

(define (close-procedure! procedure)
  (let ((block (procedure-block procedure))
	(previously-trivial? (procedure/trivial-closure? procedure))
	(original-parent (procedure-target-block procedure)))
    (let ((parent (block-parent block)))
      (set-procedure-closure-context! procedure
				      (make-reference-context original-parent))
      (with-values
	  (lambda ()
	    (let ((uninteresting-variable?
		   (lambda (variable)
		     (or (lvalue-integrated? variable)
			 (let ((value (lvalue-known-value variable)))
			   (and value
				(or (eq? value procedure)
				    (and (rvalue/procedure? value)
					 (procedure/trivial-or-virtual?
					  value)))))))))
	      (find-closure-bindings
	       original-parent
	       (list-transform-negative (block-free-variables block)
		 (lambda (lvalue)
		   (or (uninteresting-variable? lvalue)
		       (begin
			 (set-variable-closed-over?! lvalue true)
			 false))))
	       '()
	       (list-transform-negative
		   (block-variables-nontransitively-free block)
		 uninteresting-variable?))))
	(lambda (closure-frame-block size)
	  (set-block-parent! block closure-frame-block)
	  (set-procedure-closure-size! procedure size)))
      (if (if previously-trivial?
	      (not (procedure/trivial-closure? procedure))
	      (procedure/trivial-closure? procedure))
	  (error "trivial procedure becoming non-trivial or vice-versa"
		 procedure))
      (set-block-children! parent (delq! block (block-children parent)))
      (if (eq? parent original-parent)
	  (set-block-disowned-children!
	   parent
	   (cons block (block-disowned-children parent)))))))

(define (find-closure-bindings block free-variables bound-variables
			       variables-nontransitively-free)
  (if (or (not block) (ic-block? block))
      (let ((grandparent (and (not (null? free-variables)) block)))
	(if (null? bound-variables)
	    (values grandparent (if grandparent 1 0))
	    (make-closure-block grandparent
				free-variables
				bound-variables
				variables-nontransitively-free)))
      (with-values
	  (lambda ()
	    (filter-bound-variables (block-bound-variables block)
				    free-variables
				    bound-variables))
	(lambda (free-variables bound-variables)
	  (find-closure-bindings (original-block-parent block)
				 free-variables
				 bound-variables
				 variables-nontransitively-free)))))

(define (filter-bound-variables bindings free-variables bound-variables)
  (cond ((null? bindings)
	 (values free-variables bound-variables))
	((memq (car bindings) free-variables)
	 (filter-bound-variables (cdr bindings)
				 (delq! (car bindings) free-variables)
				 (cons (car bindings) bound-variables)))
	(else
	 (filter-bound-variables (cdr bindings)
				 free-variables
				 bound-variables))))

;; Note: The use of closure-block-first-offset below implies that
;; closure frames are not shared between different closures.
;; This may have to change if we ever do simultaneous closing of multiple
;; procedures sharing structure.

(define (make-closure-block parent free-variables bound-variables
			    variables-nontransitively-free)
  (let ((block (make-block parent 'CLOSURE)))
    (set-block-free-variables! block free-variables)
    (set-block-bound-variables! block bound-variables)
    (set-block-variables-nontransitively-free!
     block
     variables-nontransitively-free)
    (do ((variables (block-bound-variables block) (cdr variables))
	 (size (if (and parent (ic-block/use-lookup? parent)) 1 0) (1+ size))
	 (table '()
		(cons (cons (car variables)
			    (+ closure-block-first-offset size))
		      table)))
	((null? variables)
	 (set-block-closure-offsets! block table)
	 (values block size))
      (if (lvalue-integrated? (car variables))
	  (error "make-closure-block: integrated lvalue" (car variables))))))

(define (setup-closure-contexts! expression procedures)
  (with-new-node-marks
   (lambda ()
     (setup-closure-contexts/node (expression-entry-node expression))
     (for-each
      (lambda (procedure)
	(setup-closure-contexts/next (procedure-entry-node procedure)))
      procedures))))

(define (setup-closure-contexts/next node)
  (if (and node (not (node-marked? node)))
      (setup-closure-contexts/node node)))

(define (setup-closure-contexts/node node)
  (node-mark! node)
  (cfg-node-case (tagged-vector/tag node)
    ((PARALLEL)
     (for-each
      (lambda (subproblem)
	(let ((prefix (subproblem-prefix subproblem)))
	  (if (not (cfg-null? prefix))
	      (setup-closure-contexts/next (cfg-entry-node prefix))))
	(if (not (subproblem-canonical? subproblem))
	    (setup-closure-contexts/rvalue
	     (virtual-continuation/context
	      (subproblem-continuation subproblem))
	     (subproblem-rvalue subproblem))))
      (parallel-subproblems node))
     (setup-closure-contexts/next (snode-next node)))
    ((APPLICATION)
     (if (application/return? node)
	 (let ((context (application-context node)))
	   (setup-closure-contexts/rvalue context (application-operator node))
	   (for-each (lambda (operand)
		       (setup-closure-contexts/rvalue context operand))
		     (application-operands node))))
     (setup-closure-contexts/next (snode-next node)))
    ((VIRTUAL-RETURN)
     (let ((context (virtual-return-context node)))
       (setup-closure-contexts/rvalue context (virtual-return-operand node))
       (let ((continuation (virtual-return-operator node)))
	 (if (virtual-continuation/reified? continuation)
	     (setup-closure-contexts/rvalue
	      context
	      (virtual-continuation/reification continuation)))))
     (setup-closure-contexts/next (snode-next node)))
    ((ASSIGNMENT)
     (setup-closure-contexts/rvalue (assignment-context node)
				    (assignment-rvalue node))
     (setup-closure-contexts/next (snode-next node)))
    ((DEFINITION)
     (setup-closure-contexts/rvalue (definition-context node)
				    (definition-rvalue node))
     (setup-closure-contexts/next (snode-next node)))
    ((TRUE-TEST)
     (setup-closure-contexts/rvalue (true-test-context node)
				    (true-test-rvalue node))
     (setup-closure-contexts/next (pnode-consequent node))
     (setup-closure-contexts/next (pnode-alternative node)))
    ((STACK-OVERWRITE POP FG-NOOP)
     (setup-closure-contexts/next (snode-next node)))))

(define (setup-closure-contexts/rvalue context rvalue)
  (if (and (rvalue/procedure? rvalue)
	   (let ((context* (procedure-closure-context rvalue)))
	     (and (reference-context? context*)
		  (begin
		    (if (not (eq? (reference-context/block context)
				  (reference-context/block context*)))
			(error "mismatched reference contexts"
			       context context*))
		    (not (eq? context context*))))))
      (set-procedure-closure-context! rvalue context)))