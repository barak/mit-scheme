#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/cfg2.scm,v 1.1 1987/06/13 21:16:51 cph Exp $

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

;;;; Editing

;;; BBlock information is preserved only for deletions.  Doing the
;;; same for insertions is more difficult and not currently needed.

(define (snode-delete! snode)
  (let ((bblock (node-bblock snode)))
    (if (and bblock
	     (eq? snode (bblock-exit bblock))
	     (not (eq? snode (bblock-entry bblock))))
	(set-bblock-exit! bblock (node-previous-first snode))))
  (let ((previous-edges (node-previous-edges snode))
	(next-edge (snode-next-edge snode)))
    (let ((node (edge-right-node next-edge)))
      (edges-disconnect-right! previous-edges)
      (edge-disconnect! next-edge)
      (edges-connect-right! previous-edges node))))

(define (edge-insert-snode! edge snode)
  (let ((next (edge-right-node edge)))
    (edge-disconnect-right! edge)
    (edge-connect-right! edge snode)
    (create-edge! snode set-snode-next-edge! next)))

(define (node-insert-snode! node snode)
  (let ((previous-edges (node-previous-edges node)))
    (edges-disconnect-right! previous-edges)
    (edges-connect-right! previous-edges snode)
    (create-edge! snode set-snode-next-edge! node)))

(define (node->edge node)
  (let ((edge (make-edge false false false)))
    (edge-connect-right! edge node)
    edge))

(define-integrable (cfg-entry-edge cfg)
  (node->edge (cfg-entry-node cfg)))
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

(define noop-node-tag (make-vector-tag snode-tag 'NOOP))
(define *noop-nodes*)

(define-integrable (make-noop-node)
  (let ((node (make-snode noop-node-tag)))
    (set! *noop-nodes* (cons node *noop-nodes*))
    node))

(define (delete-noop-nodes!)
  (for-each snode-delete! *noop-nodes*)
  (set! *noop-nodes* '()))

(define (constant->pcfg value)
  ((if value make-true-pcfg make-false-pcfg)))

(define (make-false-pcfg)
  (let ((node (make-noop-node)))
    (make-pcfg node
	       '()
	       (list (make-hook node set-snode-next-edge!)))))

(define (make-true-pcfg)
  (let ((node (make-noop-node)))
    (make-pcfg node
	       (list (make-hook node set-snode-next-edge!))
	       '())))

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

(define (node-property-get node key)
  (let ((entry (assq key (node-alist node))))
    (and entry (cdr entry))))

(define (node-property-put! node key item)
  (let ((entry (assq key (node-alist node))))
    (if entry
	(set-cdr! entry item)
	(set-node-alist! node (cons (cons key item) (node-alist node))))))

(define (node-property-remove! node key)
  (set-node-alist! node (del-assq! key (node-alist node))))

(define (node-label node)
  (or (node-labelled? node)
      (let ((label (generate-label)))
	(set-node-label! node label)
	label)))

(define-integrable (node-labelled? node)
  (node-property-get node node-label))

(define-integrable (set-node-label! node label)
  (node-property-put! node node-label label))