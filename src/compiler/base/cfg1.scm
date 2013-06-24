#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; Control Flow Graph Abstraction

(declare (usual-integrations))

;;;; Node Datatypes

(define cfg-node-tag (make-vector-tag #f 'CFG-NODE #f))
(define cfg-node? (tagged-vector/subclass-predicate cfg-node-tag))
(define-vector-slots node 1 generation alist previous-edges)

(set-vector-tag-description!
 cfg-node-tag
 (lambda (node)
   (descriptor-list node node generation alist previous-edges)))

(define snode-tag (make-vector-tag cfg-node-tag 'SNODE #f))
(define snode? (tagged-vector/subclass-predicate snode-tag))
(define-vector-slots snode 4 next-edge)

;;; converted to a macro.
;;; (define (make-snode tag . extra)
;;;   (list->vector (cons* tag #f '() '() #f extra)))

(set-vector-tag-description!
 snode-tag
 (lambda (snode)
   (append! ((vector-tag-description (vector-tag-parent snode-tag)) snode)
	    (descriptor-list snode snode next-edge))))

(define pnode-tag (make-vector-tag cfg-node-tag 'PNODE #f))
(define pnode? (tagged-vector/subclass-predicate pnode-tag))
(define-vector-slots pnode 4 consequent-edge alternative-edge)

;;; converted to a macro.
;;; (define (make-pnode tag . extra)
;;;   (list->vector (cons* tag #f '() '() #f #f extra)))

(set-vector-tag-description!
 pnode-tag
 (lambda (pnode)
   (append! ((vector-tag-description (vector-tag-parent pnode-tag)) pnode)
	    (descriptor-list pnode pnode consequent-edge alternative-edge))))

(define (add-node-previous-edge! node edge)
  (set-node-previous-edges! node (cons edge (node-previous-edges node))))

(define (delete-node-previous-edge! node edge)
  (set-node-previous-edges! node (delq! edge (node-previous-edges node))))

(define-integrable (snode-next snode)
  (edge-next-node (snode-next-edge snode)))

(define-integrable (pnode-consequent pnode)
  (edge-next-node (pnode-consequent-edge pnode)))

(define-integrable (pnode-alternative pnode)
  (edge-next-node (pnode-alternative-edge pnode)))

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

;;;; Edge Datatype

(define-structure (edge (type vector))
  left-node
  left-connect
  right-node)

(define (create-edge! left-node left-connect right-node)
  (let ((edge (make-edge left-node left-connect right-node)))
    (if left-node
	(left-connect left-node edge))
    (if right-node
	(add-node-previous-edge! right-node edge))
    edge))

(define-integrable (node->edge node)
  (create-edge! #f #f node))

(define (edge-next-node edge)
  (and edge (edge-right-node edge)))

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
	  (set-edge-left-node! edge #f)
	  (set-edge-left-connect! edge #f)
	  (left-connect left-node #f)))))

(define (edge-disconnect-right! edge)
  (let ((right-node (edge-right-node edge)))
    (if right-node
	(begin
	  (set-edge-right-node! edge #f)
	  (delete-node-previous-edge! right-node edge)))))

(define (edge-disconnect! edge)
  (edge-disconnect-left! edge)
  (edge-disconnect-right! edge))

(define (edge-replace-left! edge left-node left-connect)
  (edge-disconnect-left! edge)
  (edge-connect-left! edge left-node left-connect))

(define (edge-replace-right! edge right-node)
  (edge-disconnect-right! edge)
  (edge-connect-right! edge right-node))

(define (edges-connect-right! edges right-node)
  (for-each (lambda (edge) (edge-connect-right! edge right-node)) edges))

(define (edges-disconnect-right! edges)
  (for-each edge-disconnect-right! edges))

(define (edges-replace-right! edges right-node)
  (for-each (lambda (edge) (edge-replace-right! edge right-node)) edges))