#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/cfg1.scm,v 1.150 1987/08/07 17:02:34 cph Exp $

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

;;;; Control Flow Graph Abstraction

(declare (usual-integrations))

;;;; Node Datatypes

(define cfg-node-tag (make-vector-tag false 'CFG-NODE))
(define cfg-node? (tagged-vector-subclass-predicate cfg-node-tag))
(define-vector-slots node 1 generation previous-edges)

(define-vector-method cfg-node-tag ':DESCRIBE
  (lambda (node)
    (descriptor-list node generation previous-edges)))

(define snode-tag (make-vector-tag cfg-node-tag 'SNODE))
(define snode? (tagged-vector-subclass-predicate snode-tag))
(define-vector-slots snode 3 next-edge)

(define (make-snode tag . extra)
  (list->vector (cons* tag false '() false extra)))

(define-vector-method snode-tag ':DESCRIBE
  (lambda (snode)
    (append! ((vector-tag-parent-method snode-tag ':DESCRIBE) snode)
	     (descriptor-list snode next-edge))))

(define pnode-tag (make-vector-tag cfg-node-tag 'PNODE))
(define pnode? (tagged-vector-subclass-predicate pnode-tag))
(define-vector-slots pnode 3 consequent-edge alternative-edge)

(define (make-pnode tag . extra)
  (list->vector (cons* tag false '() false false extra)))

(define-vector-method pnode-tag ':DESCRIBE
  (lambda (pnode)
    (append! ((vector-tag-parent-method pnode-tag ':DESCRIBE) pnode)
	     (descriptor-list pnode consequent-edge alternative-edge))))

(define (edge-next-node edge)
  (and edge (edge-right-node edge)))

(define-integrable (snode-next snode)
  (edge-next-node (snode-next-edge snode)))

(define-integrable (pnode-consequent pnode)
  (edge-next-node (pnode-consequent-edge pnode)))

(define-integrable (pnode-alternative pnode)
  (edge-next-node (pnode-alternative-edge pnode)))

;;;; Edge Datatype

(define-vector-slots edge 0 left-node left-connect right-node)

(define-integrable (make-edge left-node left-connect right-node)
  (vector left-node left-connect right-node))

(define (create-edge! left-node left-connect right-node)
  (let ((edge (make-edge left-node left-connect right-node)))
    (if left-node
	(left-connect left-node edge))
    (if right-node
	(let ((previous (node-previous-edges right-node)))
	  (if (not (memq right-node previous))
	      (set-node-previous-edges! right-node (cons edge previous)))))))

(define (edge-connect-left! edge left-node left-connect)
  (set-edge-left-node! edge left-node)
  (set-edge-left-connect! edge left-connect)
  (if left-node
      (left-connect left-node edge)))

(define (edge-connect-right! edge right-node)
  (set-edge-right-node! edge right-node)
  (if right-node
      (let ((previous (node-previous-edges right-node)))
	(if (not (memq right-node previous))
	    (set-node-previous-edges! right-node (cons edge previous))))))

(define (edges-connect-right! edges right-node)
  (for-each (lambda (edge)
	      (edge-connect-right! edge right-node))
	    edges))

(define (edge-disconnect-left! edge)
  (let ((left-node (set-edge-left-node! edge false))
	(left-connect (set-edge-left-connect! edge false)))
    (if left-node
	(left-connect left-node false))))

(define (edge-disconnect-right! edge)
  (let ((right-node (set-edge-right-node! edge false)))
    (if right-node
	(set-node-previous-edges! right-node
				  (delq! edge
					 (node-previous-edges right-node))))))

(define (edge-disconnect! edge)
  (edge-disconnect-left! edge)
  (edge-disconnect-right! edge))

(define (edges-disconnect-right! edges)
  (for-each edge-disconnect-right! edges))