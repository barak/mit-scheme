#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/folcon.scm,v 1.2 1987/10/05 20:45:00 jinx Exp $

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

;;;; Dataflow Analysis: Constant Folding

(declare (usual-integrations))

(package (fold-constants)

;;;; Fold constants

(define-export (fold-constants vnodes combinations receiver)
  (define (loop vnodes combinations)
    (let ((unknown-vnodes (eliminate-known-nodes vnodes)))
      (fold-combinations combinations
	(lambda (any-folded? not-folded)
	  (if any-folded?
	      (loop unknown-vnodes not-folded)
	      (receiver unknown-vnodes not-folded))))))
  (loop vnodes combinations))

(define (eliminate-known-nodes vnodes)
  (let ((knowable-nodes
	 (list-transform-positive vnodes
	   (lambda (vnode)
	     (and (not (or (vnode-unknowable? vnode)
			   ;; Does this really matter?  Seems like it
			   ;; should be a noop if there is only one
			   ;; value.
			   (and (variable? vnode)
				(variable-assigned? vnode)
				(not (memq 'CONSTANT
					   (variable-declarations vnode))))))
		  (let ((procedures (vnode-procedures vnode))
			(values (vnode-values vnode)))
		    (if (null? values)
			(and (not (null? procedures))
			     (null? (cdr procedures)))
			(and (null? procedures)
			     (null? (cdr values))
			     (let ((value (car values)))
			       (or (block? value)
				   (and (constant? value)
					(object-immutable?
					 (constant-value value)))))))))))))
    (for-each vnode-knowable! knowable-nodes)
    (transitive-closure delete-if-known! knowable-nodes))
  ;; **** Could flush KNOWABLE? and UNKNOWABLE? marks at this point.
  (list-transform-negative vnodes vnode-known?))

(define (delete-if-known! vnode)
  (if (and (not (vnode-known? vnode))
	   (null? (vnode-backward-links vnode)))
      (let ((value (car (if (null? (vnode-procedures vnode))
			    (vnode-values vnode)
			    (vnode-procedures vnode))))
	    (forward-links (vnode-forward-links vnode)))
	(vnode-delete! vnode)
	(for-each (lambda (vnode*)
		    ;; This is needed because, previously, VNODE*
		    ;; inherited this value from VNODE.
		    (vnode-connect! vnode* value)
		    (if (vnode-knowable? vnode*)
			(enqueue-node! vnode*)))
		  forward-links)
	(set-vnode-known-value! vnode value))))

(define (fold-combinations combinations receiver)
  (if (null? combinations)
      (receiver false '())
      (fold-combinations (cdr combinations)
	(lambda (any-folded? not-folded)
	  (if (fold-combination (car combinations))
	      (receiver true not-folded)
	      (receiver any-folded? (cons (car combinations) not-folded)))))))

(define (fold-combination combination)
  (let ((operator (combination-operator combination))
	(operands (combination-operands combination)))
    (and (subproblem-known-constant? operator)
	 (all-known-constants? operands)
	 (let ((operator (subproblem-constant-value operator)))
	   (and (operator-constant-foldable? operator)
		(begin (let ((value
			      (make-constant
			       (apply operator
				      (map subproblem-constant-value
					   operands))))
			     (cvalue (combination-value combination)))
			 (vnode-known! cvalue value)
			 (set-vnode-known-value! cvalue value))
		       (set-combination-constant?! combination true)
		       ;; Discard useless information to save space.
		       (let ((block (combination-block combination)))
			 (set-block-combinations!
			  block
			  (delq! combination (block-combinations block))))
		       (set-combination-operator! combination false)
		       (set-combination-operands! combination '())
		       (set-combination-procedures! combination '())
		       (set-combination-known-operator! combination false)
		       true))))))

(define all-known-constants?
  (for-all? subproblem-known-constant?))

)