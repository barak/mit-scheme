#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/conect.scm,v 4.3 1988/08/18 03:28:41 cph Exp $

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

;;;; FG Connectivity Analysis

(declare (usual-integrations))

(package (connectivity-analysis)

(define-export (connectivity-analysis expression procedures)
  (walk-node (expression-entry-node expression) (make-subgraph-color))
  (for-each (lambda (procedure)
	      (if (not (procedure-direct-linked? procedure))
		  (walk-node (procedure-entry-node procedure)
			     (make-subgraph-color))))
	    procedures))

(define (procedure-direct-linked? procedure)
  (if (procedure-continuation? procedure)
      (and (continuation/ever-known-operator? procedure)
	   (there-exists? (continuation/combinations procedure)
	     (lambda (combination)
	       (and (combination/inline? combination)
		    (combination/continuation-push combination)))))
      (procedure-inline-code? procedure)))

(define (walk-node node color)
  (let ((color* (node/subgraph-color node)))
    (cond ((not color*)
	   (color-node! node color)
	   (walk-next node color))
	  ((not (eq? color color*))
	   (recolor-nodes! (subgraph-color/nodes color*) color)))))

(define (color-node! node color)
  (set-node/subgraph-color! node color)
  (set-subgraph-color/nodes! color (cons node (subgraph-color/nodes color))))

(define (recolor-nodes! nodes color)
  (for-each (lambda (node)
	      (set-node/subgraph-color! node color))
	    nodes)
  (set-subgraph-color/nodes! color
			     (append! nodes (subgraph-color/nodes color))))

(define (walk-next node color)
  (cfg-node-case (tagged-vector/tag node)
    ((APPLICATION)
     (case (application-type node)
       ((COMBINATION)
	(if (combination/inline? node)
	    (walk-continuation (combination/continuation node) color)
	    (let ((operator (rvalue-known-value (application-operator node))))
	      (if (and operator
		       (rvalue/procedure? operator)
		       (procedure-inline-code? operator))
		  (walk-node (procedure-entry-node operator) color)))))
       ((RETURN)
	(walk-continuation (return/operator node) color))))
    ((VIRTUAL-RETURN POP ASSIGNMENT DEFINITION FG-NOOP)
     (walk-node (snode-next node) color))
    ((TRUE-TEST)
     (walk-node (pnode-consequent node) color)
     (walk-node (pnode-alternative node) color))))

(define (walk-continuation continuation color)
  (let ((rvalue (rvalue-known-value continuation)))
    (if rvalue
	(walk-node (continuation/entry-node rvalue) color))))

)