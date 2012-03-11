#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

;;;; Editing

(define (snode-delete! snode)
  (let ((next-edge (snode-next-edge snode)))
    (if next-edge
	(begin
	  (edges-replace-right! (node-previous-edges snode)
				(edge-right-node next-edge))
	  (edge-disconnect! next-edge))
	(edges-disconnect-right! (node-previous-edges snode)))))

(define (edge-insert-snode! edge snode)
  (let ((next (edge-right-node edge)))
    (edge-replace-right! edge snode)
    (create-edge! snode set-snode-next-edge! next)))

(define (node-insert-snode! node snode)
  (edges-replace-right! (node-previous-edges node) snode)
  (create-edge! snode set-snode-next-edge! node))

(define-integrable (node-disconnect-on-right! node)
  (edges-disconnect-right! (node-previous-edges node)))

(define (node-disconnect-on-left! node)
  (if (snode? node)
      (snode-disconnect-on-left! node)
      (pnode-disconnect-on-left! node)))

(define (snode-disconnect-on-left! node)
  (let ((edge (snode-next-edge node)))
    (if edge
	(edge-disconnect-left! edge))))

(define (pnode-disconnect-on-left! node)
  (let ((edge (pnode-consequent-edge node)))
    (if edge
	(edge-disconnect-left! edge)))
  (let ((edge (pnode-alternative-edge node)))
    (if edge
	(edge-disconnect-left! edge))))

(define (node-replace! old-node new-node)
  (if (snode? old-node)
      (snode-replace! old-node new-node)
      (pnode-replace! old-node new-node)))

(define (snode-replace! old-node new-node)
  (node-replace-on-right! old-node new-node)
  (snode-replace-on-left! old-node new-node))

(define (pnode-replace! old-node new-node)
  (node-replace-on-right! old-node new-node)
  (pnode-replace-on-left! old-node new-node))

(define-integrable (node-replace-on-right! old-node new-node)
  (edges-replace-right! (node-previous-edges old-node) new-node))

(define (node-replace-on-left! old-node new-node)
  (if (snode? old-node)
      (snode-replace-on-left! old-node new-node)
      (pnode-replace-on-left! old-node new-node)))

(define (snode-replace-on-left! old-node new-node)
  (let ((edge (snode-next-edge old-node)))
    (if edge
	(edge-replace-left! edge new-node set-snode-next-edge!))))

(define (pnode-replace-on-left! old-node new-node)
  (let ((edge (pnode-consequent-edge old-node)))
    (if edge
	(edge-replace-left! edge new-node set-pnode-consequent-edge!)))
  (let ((edge (pnode-alternative-edge old-node)))
    (if edge
	(edge-replace-left! edge new-node set-pnode-alternative-edge!))))

;;;; Previous Connections

(define-integrable (node-previous=0? node)
  (edges=0? (node-previous-edges node)))

(define (edges=0? edges)
  (cond ((null? edges) true)
	((edge-left-node (car edges)) false)
	(else (edges=0? (cdr edges)))))

(define-integrable (node-previous>0? node)
  (edges>0? (node-previous-edges node)))

(define (edges>0? edges)
  (cond ((null? edges) false)
	((edge-left-node (car edges)) true)
	(else (edges>0? (cdr edges)))))

(define-integrable (node-previous=1? node)
  (edges=1? (node-previous-edges node)))

(define (edges=1? edges)
  (if (null? edges)
      false
      ((if (edge-left-node (car edges)) edges=0? edges=1?) (cdr edges))))

(define-integrable (node-previous>1? node)
  (edges>1? (node-previous-edges node)))

(define (edges>1? edges)
  (if (null? edges)
      false
      ((if (edge-left-node (car edges)) edges>0? edges>1?) (cdr edges))))

(define-integrable (node-previous-first node)
  (edges-first-node (node-previous-edges node)))

(define (edges-first-node edges)
  (if (null? edges)
      (error "No first hook")
      (or (edge-left-node (car edges))
	  (edges-first-node (cdr edges)))))

(define (for-each-previous-node node procedure)
  (for-each (lambda (edge)
	      (let ((node (edge-left-node edge)))
		(if node
		    (procedure node))))
	    (node-previous-edges node)))

;;;; Noops

(define-integrable (cfg-node-tag/noop! tag)
  (set-vector-tag-noop! tag true))

(define-integrable (cfg-node-tag/noop? tag)
  (vector-tag-noop tag))

(define-integrable (cfg-node/noop? node)
  (cfg-node-tag/noop? (tagged-vector/tag node)))

(define noop-node-tag
  (let ((tag (make-vector-tag snode-tag 'NOOP false)))
    (cfg-node-tag/noop! tag)
    tag))

(define-integrable (make-noop-node)
  (let ((node (make-snode noop-node-tag)))
    (set! *noop-nodes* (cons node *noop-nodes*))
    node))

(define *noop-nodes*)

(define (cleanup-noop-nodes thunk)
  (fluid-let ((*noop-nodes* '()))
    (let ((value (thunk)))
      (for-each snode-delete! *noop-nodes*)
      value)))

(define (make-false-pcfg)
  (snode->pcfg-false (make-noop-node)))

(define (make-true-pcfg)
  (snode->pcfg-true (make-noop-node)))

;;;; Miscellaneous

(package (with-new-node-marks
	  node-marked?
	  node-mark!)

(define *generation*)

(define-export (with-new-node-marks thunk)
  (fluid-let ((*generation* (make-generation)))
    (thunk)))

(define make-generation
  (let ((generation 0))
    (named-lambda (make-generation)
      (let ((value generation))
	(set! generation (1+ generation))
	value))))

(define-export (node-marked? node)
  (eq? (node-generation node) *generation*))

(define-export (node-mark! node)
  (set-node-generation! node *generation*))

)