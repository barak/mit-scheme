#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/cfg1.scm,v 4.2 1987/12/30 06:57:50 cph Exp $

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

(define cfg-node-tag (make-vector-tag false 'CFG-NODE false))
(define cfg-node? (tagged-vector/subclass-predicate cfg-node-tag))
(define-vector-slots node 1 generation alist previous-edges)

(set-vector-tag-description!
 cfg-node-tag
 (lambda (node)
   (descriptor-list node generation alist previous-edges)))

(define snode-tag (make-vector-tag cfg-node-tag 'SNODE false))
(define snode? (tagged-vector/subclass-predicate snode-tag))
(define-vector-slots snode 4 next-edge)

(define (make-snode tag . extra)
  (list->vector (cons* tag false '() '() false extra)))

(set-vector-tag-description!
 snode-tag
 (lambda (snode)
   (append! ((vector-tag-description (vector-tag-parent snode-tag)) snode)
	    (descriptor-list snode next-edge))))

(define pnode-tag (make-vector-tag cfg-node-tag 'PNODE false))
(define pnode? (tagged-vector/subclass-predicate pnode-tag))
(define-vector-slots pnode 4 consequent-edge alternative-edge)

(define (make-pnode tag . extra)
  (list->vector (cons* tag false '() '() false false extra)))

(set-vector-tag-description!
 pnode-tag
 (lambda (pnode)
   (append! ((vector-tag-description (vector-tag-parent pnode-tag)) pnode)
	    (descriptor-list pnode consequent-edge alternative-edge))))

(define (add-node-previous-edge! node edge)
  (set-node-previous-edges! node (cons edge (node-previous-edges node))))

(define (delete-node-previous-edge! node edge)
  (set-node-previous-edges! node (delq! edge (node-previous-edges node))))

(define (edge-next-node edge)
  (and edge (edge-right-node edge)))

(define-integrable (snode-next snode)
  (edge-next-node (snode-next-edge snode)))

(define-integrable (pnode-consequent pnode)
  (edge-next-node (pnode-consequent-edge pnode)))

(define-integrable (pnode-alternative pnode)
  (edge-next-node (pnode-alternative-edge pnode)))

;;;; Edge Datatype

(define-structure (edge (type vector)) left-node left-connect right-node)

(define (create-edge! left-node left-connect right-node)
  (let ((edge (make-edge left-node left-connect right-node)))
    (if left-node
	(left-connect left-node edge))
    (if right-node
	(add-node-previous-edge! right-node edge))
    edge))

(define (edge-connect-left! edge left-node left-connect)
  (if (edge-left-node edge)
      (error "Attempt to doubly connect left node of edge" edge))
  (if left-node
      (begin
	(set-edge-left-node! edge left-node)
	(set-edge-left-connect! edge left-connect)
	(left-connect left-node edge))))

(define (edge-connect-right! edge right-node)
  (if (edge-right-node edge)
      (error "Attempt to doubly connect right node of edge" edge))
  (if right-node
      (begin
	(set-edge-right-node! edge right-node)
	(add-node-previous-edge! right-node edge))))

(define (edge-disconnect-left! edge)
  (let ((left-node (edge-left-node edge))
	(left-connect (edge-left-connect edge)))
    (if left-node
	(begin
	  (set-edge-left-node! edge false)
	  (set-edge-left-connect! edge false)
	  (left-connect left-node false)))))

(define (edge-disconnect-right! edge)
  (let ((right-node (edge-right-node edge)))
    (if right-node
	(begin
	  (set-edge-right-node! edge false)
	  (delete-node-previous-edge! right-node edge)))))

(define (edges-connect-right! edges right-node)
  (for-each (lambda (edge) (edge-connect-right! edge right-node)) edges))

(define (edge-disconnect! edge)
  (edge-disconnect-left! edge)
  (edge-disconnect-right! edge))

(define (edges-disconnect-right! edges)
  (for-each edge-disconnect-right! edges))

;;;; Node Properties

(define (cfg-node-get node key)
  (let ((entry (assq key (node-alist node))))
    (and entry
	 (cdr entry))))

(define (cfg-node-put! node key item)
  (let ((entry (assq key (node-alist node))))
    (if entry
	(set-cdr! entry item)
	(set-node-alist! node (cons (cons key item) (node-alist node))))))

(define (cfg-node-remove! node key)
  (set-node-alist! node (del-assq! key (node-alist node))))