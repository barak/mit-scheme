#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/subfre.scm,v 1.2 1989/04/03 22:03:55 cph Rel $

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

;;;; Subproblem Free Variables

(declare (usual-integrations))

(define (compute-subproblem-free-variables parallels)
  (for-each (lambda (parallel)
	      (for-each (lambda (subproblem)
			  (set-subproblem-free-variables! subproblem 'UNKNOWN))
			(parallel-subproblems parallel)))
	    parallels)
  (for-each (lambda (parallel)
	      (for-each walk-subproblem (parallel-subproblems parallel)))
	    parallels))

(define (new-subproblem/compute-free-variables! subproblem)
  (walk-subproblem subproblem))

(define (walk-subproblem subproblem)
  (let ((free (subproblem-free-variables subproblem)))
    (if (eq? free 'UNKNOWN)
	(let ((free
	       (let ((free (walk-rvalue (subproblem-rvalue subproblem))))
		 (if (subproblem-canonical? subproblem)
		     (eq-set-union
		      free
		      (walk-node (subproblem-entry-node subproblem)))
		     free))))
	  (set-subproblem-free-variables! subproblem free)
	  free)
	free)))

(define (walk-next next free)
  (if next
      (eq-set-union (walk-node next) free)
      free))

(define (walk-node node)
  (cfg-node-case (tagged-vector/tag node)
    ((PARALLEL)
     (walk-next (snode-next node)
		(map-union walk-subproblem (parallel-subproblems node))))
    ((APPLICATION)
     (walk-next
      (snode-next node)
      (eq-set-union (walk-operator (application-operator node))
		    (map-union walk-rvalue (application-operands node)))))
    ((VIRTUAL-RETURN)
     (walk-next
      (snode-next node)
      (let ((operator (virtual-return-operator node))
	    (free (walk-rvalue (virtual-return-operand node))))
	(cond ((not (virtual-continuation? operator))
	       (eq-set-union (walk-rvalue operator) free))
	      ((virtual-continuation/reified? operator)
	       (eq-set-union
		(walk-rvalue (virtual-continuation/reification operator))
		free))
	      (else free)))))
    ((ASSIGNMENT)
     (walk-next
      (snode-next node)
      (eq-set-union (walk-lvalue (assignment-lvalue node) walk-rvalue)
		    (walk-rvalue (assignment-rvalue node)))))
    ((DEFINITION)
     (walk-next
      (snode-next node)
      (eq-set-union (walk-lvalue (definition-lvalue node) walk-rvalue)
		    (walk-rvalue (definition-rvalue node)))))
    ((TRUE-TEST)
     (walk-next (pnode-consequent node)
		(walk-next (pnode-alternative node)
			   (walk-rvalue (true-test-rvalue node)))))
    ((FG-NOOP)
     (walk-next (snode-next node) '()))))

(define (map-union procedure items)
  (let loop ((items items) (set '()))
    (if (null? items)
	set
	(loop (cdr items)
	      (eq-set-union (procedure (car items)) set)))))

(define (walk-operator rvalue)
  (enumeration-case rvalue-type (tagged-vector/index rvalue)
    ((REFERENCE) (walk-lvalue (reference-lvalue rvalue) walk-operator))
    ((PROCEDURE)
     (if (procedure-continuation? rvalue)
	 (walk-next (continuation/entry-node rvalue) '())
	 (map-union (lambda (procedure)
		      (list-transform-negative
			  (block-free-variables (procedure-block procedure))
			lvalue-integrated?))
		    (eq-set-union (list rvalue)
				  (procedure-callees rvalue)))))
    (else '())))

(define (walk-rvalue rvalue)
  (enumeration-case rvalue-type (tagged-vector/index rvalue)
    ((REFERENCE) (walk-lvalue (reference-lvalue rvalue) walk-rvalue))
    ((PROCEDURE)
     (if (procedure-continuation? rvalue)
	 (walk-next (continuation/entry-node rvalue) '())
	 (list-transform-negative
	     (block-free-variables (procedure-block rvalue))
	   lvalue-integrated?)))
    (else '())))

(define (walk-lvalue lvalue walk-rvalue)
  (let ((value (lvalue-known-value lvalue)))
    (cond ((not value) (list lvalue))
	  ((lvalue-integrated? lvalue) (walk-rvalue value))
	  (else (eq-set-adjoin lvalue (walk-rvalue value))))))