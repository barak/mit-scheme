#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/simple.scm,v 4.1 1987/12/04 19:28:21 cph Exp $

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

;;;; Argument Simplicity Analysis

(declare (usual-integrations))

(package (simplicity-analysis)

(define-export (simplicity-analysis parallels)
  (for-each (lambda (parallel)
	      (for-each (lambda (subproblem)
			  (set-subproblem-simple?! subproblem 'UNKNOWN))
			(parallel-subproblems parallel)))
	    parallels)
  (for-each (lambda (parallel)
	      (if (let ((application (parallel-application-node parallel)))
		    (and application
			 (application/combination? application)
			 (combination/inline? application)))
		  (for-each %subproblem-simple?
			    (parallel-subproblems parallel))))
	    parallels))

(define (%subproblem-simple? subproblem)
  (let ((simple? (subproblem-simple? subproblem)))
    (if (eq? simple? 'UNKNOWN)
	(let ((simple?
	       (and (rvalue-simple? (subproblem-rvalue subproblem))
		    (or (not (subproblem-canonical? subproblem))
			(node-simple? (subproblem-entry-node subproblem)
				      (subproblem-continuation subproblem))))))
	  (set-subproblem-simple?! subproblem simple?)
	  simple?)
	simple?)))

(define (node-simple? node continuation)
  ((cfg-node-case (tagged-vector/tag node)
     ((PARALLEL) parallel-simple?)
     ((APPLICATION)
      (case (application-type node)
	((COMBINATION) combination-simple?)
	((RETURN) return-simple?)
	(else (error "Unknown application type" node))))
     ((VIRTUAL-RETURN) virtual-return-simple?)
     ((ASSIGNMENT) assignment-simple?)
     ((DEFINITION) definition-simple?)
     ((TRUE-TEST) true-test-simple?)
     ((FG-NOOP) fg-noop-simple?))
   node continuation))

(define (parallel-simple? parallel continuation)
  (and (for-all? (parallel-subproblems parallel) %subproblem-simple?)
       (node-simple? (snode-next parallel) continuation)))

(define (combination-simple? combination continuation)
  (and (combination/inline? combination)
       (continuation-simple? (combination/continuation combination)
			     continuation)))

(define (return-simple? return continuation)
  (continuation-simple? (return/operator return) continuation))

(define (virtual-return-simple? return continuation)
  (continuation-simple? (virtual-return-operator return) continuation))

(define (continuation-simple? rvalue continuation)
  (or (eq? rvalue continuation)
      (and (rvalue/continuation? rvalue)
	   (node-simple? (continuation/entry-node rvalue) continuation))))

(define (assignment-simple? assignment continuation)
  (and (lvalue-simple? (assignment-lvalue assignment))
       (rvalue-simple? (assignment-rvalue assignment))
       (node-simple? (snode-next assignment) continuation)))

(define (definition-simple? definition continuation)
  (and (lvalue-simple? (definition-lvalue definition))
       (rvalue-simple? (definition-rvalue definition))
       (node-simple? (snode-next definition) continuation)))

(define (true-test-simple? true-test continuation)
  (and (rvalue-simple? (true-test-rvalue true-test))
       (node-simple? (pnode-consequent true-test) continuation)
       (node-simple? (pnode-alternative true-test) continuation)))

(define (fg-noop-simple? fg-noop continuation)
  (node-simple? (snode-next fg-noop) continuation))

(define (rvalue-simple? rvalue)
  (or (not (rvalue/reference? rvalue))
      (let ((lvalue (reference-lvalue rvalue)))
	(or (lvalue-known-value lvalue)
	    (lvalue-simple? lvalue)))))

(define (lvalue-simple? lvalue)
  (not (block-passed-out? (variable-block lvalue))))

)