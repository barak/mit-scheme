#| -*-Scheme-*-

$Id: conect.scm,v 4.6 2001/10/16 16:38:37 cph Exp $

Copyright (c) 1987, 1988, 1999, 2001 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
|#

;;;; FG Connectivity Analysis

(declare (usual-integrations))

(define (connectivity-analysis expression procedures)
  (walk-node (expression-entry-node expression) (make-subgraph-color))
  (for-each (lambda (procedure)
	      (walk-node (procedure-entry-node procedure)
			 (make-subgraph-color)))
	    procedures))

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
    ((VIRTUAL-RETURN POP ASSIGNMENT DEFINITION FG-NOOP STACK-OVERWRITE)
     (walk-node (snode-next node) color))
    ((TRUE-TEST)
     (walk-node (pnode-consequent node) color)
     (walk-node (pnode-alternative node) color))))

(define (walk-continuation continuation color)
  (let ((rvalue (rvalue-known-value continuation)))
    (if rvalue
	(walk-node (continuation/entry-node rvalue) color))))