;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1986 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Control Flow Graph Abstraction

;;; $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/cfg1.scm,v 1.144 1986/12/20 23:48:20 cph Exp $

(declare (usual-integrations))
(using-syntax (access compiler-syntax-table compiler-package)

;;;; Node Datatypes

(define cfg-node-tag (make-vector-tag false 'CFG-NODE))
(define cfg-node? (tagged-vector-subclass-predicate cfg-node-tag))
(define-vector-slots node 1 generation bblock alist previous-edges)

(define-vector-method cfg-node-tag ':DESCRIBE
  (lambda (node)
    (descriptor-list node generation bblock alist previous-edges)))

(define snode-tag (make-vector-tag cfg-node-tag 'SNODE))
(define snode? (tagged-vector-subclass-predicate snode-tag))
(define-vector-slots snode 5 next-edge)

(define (make-snode tag . extra)
  (list->vector (cons* tag false false '() '() false extra)))

(define-integrable (snode-next snode)
  (edge-right-node (snode-next-edge snode)))

(define-vector-method snode-tag ':DESCRIBE
  (lambda (snode)
    (append! ((vector-tag-parent-method snode-tag ':DESCRIBE) snode)
	     (descriptor-list snode next-edge))))

(define pnode-tag (make-vector-tag cfg-node-tag 'PNODE))
(define pnode? (tagged-vector-subclass-predicate pnode-tag))
(define-vector-slots pnode 5 consequent-edge alternative-edge)

(define (make-pnode tag . extra)
  (list->vector (cons* tag false false '() '() false false extra)))

(define-integrable (pnode-consequent pnode)
  (edge-right-node (pnode-consequent-edge pnode)))

(define-integrable (pnode-alternative pnode)
  (edge-right-node (pnode-alternative-edge pnode)))

(define-vector-method pnode-tag ':DESCRIBE
  (lambda (pnode)
    (append! ((vector-tag-parent-method pnode-tag ':DESCRIBE) pnode)
	     (descriptor-list pnode consequent-edge alternative-edge))))

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
    (create-edge! snode set-snode-next! next)))

(define (node-insert-snode! node snode)
  (let ((previous-edges (node-previous-edges node)))
    (edges-disconnect-right! previous-edges)
    (edges-connect-right! previous-edges snode)
    (create-edge! snode set-snode-next! node)))

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
	       (list (make-hook node set-snode-next!)))))

(define (make-true-pcfg)
  (let ((node (make-noop-node)))
    (make-pcfg node
	       (list (make-hook node set-snode-next!))
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

;;;; CFG Datatypes

;;; A CFG is a compound CFG-node, so there are different types of CFG
;;; corresponding to the (connective-wise) different types of
;;; CFG-node.  One may insert a particular type of CFG anywhere in a
;;; graph that its corresponding node may be inserted.

(define-integrable (make-scfg node next-hooks)
  (vector 'SNODE-CFG node next-hooks))

(define-integrable (make-scfg* node consequent-hooks alternative-hooks)
  (make-scfg node (hooks-union consequent-hooks alternative-hooks)))

(define-integrable (make-pcfg node consequent-hooks alternative-hooks)
  (vector 'PNODE-CFG node consequent-hooks alternative-hooks))

(define-integrable (cfg-tag cfg)
  (vector-ref cfg 0))

(define-integrable (cfg-entry-node cfg)
  (vector-ref cfg 1))

(define-integrable (scfg-next-hooks scfg)
  (vector-ref scfg 2))

(define-integrable (pcfg-consequent-hooks pcfg)
  (vector-ref pcfg 2))

(define-integrable (pcfg-alternative-hooks pcfg)
  (vector-ref pcfg 3))

(define-integrable (make-null-cfg) false)
(define-integrable cfg-null? false?)

(define-integrable (snode->scfg snode)
  (node->scfg snode set-snode-next-edge!))

(define (node->scfg node set-node-next!)
  (make-scfg node
	     (list (make-hook node set-node-next!))))

(define-integrable (pnode->pcfg pnode)
  (node->pcfg pnode
	      set-pnode-consequent-edge!
	      set-pnode-alternative-edge!))

(define (node->pcfg node set-node-consequent! set-node-alternative!)
  (make-pcfg node
	     (list (make-hook node set-node-consequent!))
	     (list (make-hook node set-node-alternative!))))

;;;; Hook Datatype

(define-integrable make-hook cons)
(define-integrable hook-node car)
(define-integrable hook-connect cdr)

(define (hook=? x y)
  (and (eq? (hook-node x) (hook-node y))
       (eq? (hook-connect x) (hook-connect y))))

(define hook-member?
  (member-procedure hook=?))

(define (hooks-union x y)
  (let loop ((x x))
    (cond ((null? x) y)
	  ((hook-member? (car x) y) (loop (cdr x)))
	  (else (cons (car x) (loop (cdr x)))))))

(define (hooks-connect! hooks node)
  (for-each (lambda (hook)
	      (hook-connect! hook node))
	    hooks))

(define (hook-connect! hook node)
  (create-edge! (hook-node hook) (hook-connect hook) node))

;;;; CFG Construction

(define-integrable (scfg-next-connect! scfg cfg)
  (hooks-connect! (scfg-next-hooks scfg) (cfg-entry-node cfg)))

(define-integrable (pcfg-consequent-connect! pcfg cfg)
  (hooks-connect! (pcfg-consequent-hooks pcfg) (cfg-entry-node cfg)))

(define-integrable (pcfg-alternative-connect! pcfg cfg)
  (hooks-connect! (pcfg-alternative-hooks pcfg) (cfg-entry-node cfg)))

(define (scfg*scfg->scfg! scfg scfg*)
  (cond ((not scfg) scfg*)
	((not scfg*) scfg)
	(else (scfg-next-connect! scfg scfg*)
	      (make-scfg (cfg-entry-node scfg) (scfg-next-hooks scfg*)))))

(package (scfg-append! scfg*->scfg!)

(define-export (scfg-append! . scfgs)
  (scfg*->scfg! scfgs))

(define-export (scfg*->scfg! scfgs)
  (let ((first (find-non-null scfgs)))
    (and (not (null? first))
	 (let ((second (find-non-null (cdr first))))
	   (if (null? second)
	       (car first)
	       (make-scfg (cfg-entry-node (car first))
			  (scfg-next-hooks
			   (loop (car first)
				 (car second)
				 (find-non-null (cdr second))))))))))

(define (loop first second third)
  (scfg-next-connect! first second)
  (if (null? third)
      second
      (loop second (car third) (find-non-null (cdr third)))))

(define (find-non-null scfgs)
  (if (or (null? scfgs)
	  (car scfgs))
      scfgs
      (find-non-null (cdr scfgs))))

)

(define (pcfg->scfg! pcfg)
  (make-scfg* (cfg-entry-node pcfg)
	      (pcfg-consequent-hooks pcfg)
	      (pcfg-alternative-hooks pcfg)))

(package (scfg*pcfg->pcfg! scfg*pcfg->scfg!)

(define ((scfg*pcfg->cfg! transformer constructor) scfg pcfg)
  (cond ((not pcfg) (error "SCFG*PCFG->CFG!: Can't have null predicate"))
	((not scfg) (transformer pcfg))
	(else
	 (scfg-next-connect! scfg pcfg)
	 (constructor (cfg-entry-node scfg)
		      (pcfg-consequent-hooks pcfg)
		      (pcfg-alternative-hooks pcfg)))))

(define scfg*pcfg->pcfg!
  (scfg*pcfg->cfg! identity-procedure make-pcfg))

(define scfg*pcfg->scfg!
  (scfg*pcfg->cfg! pcfg->scfg! make-scfg*))

)

(package (pcfg*scfg->pcfg! pcfg*scfg->scfg!)

(define ((pcfg*scfg->cfg! transformer constructor) pcfg consequent alternative)
  (cond ((not pcfg) (error "PCFG*SCFG->CFG!: Can't have null predicate"))
	((not consequent)
	 (if (not alternative)
	     (transformer pcfg)
	     (begin (pcfg-alternative-connect! pcfg alternative)
		    (constructor (cfg-entry-node pcfg)
				 (pcfg-consequent-hooks pcfg)
				 (scfg-next-hooks alternative)))))
	((not alternative)
	 (pcfg-consequent-connect! pcfg consequent)
	 (constructor (cfg-entry-node pcfg)
		      (scfg-next-hooks consequent)
		      (pcfg-alternative-hooks pcfg)))
	(else
	 (pcfg-consequent-connect! pcfg consequent)
	 (pcfg-alternative-connect! pcfg alternative)
	 (constructor (cfg-entry-node pcfg)
		      (scfg-next-hooks consequent)
		      (scfg-next-hooks alternative)))))

(define pcfg*scfg->pcfg!
  (pcfg*scfg->cfg! identity-procedure make-pcfg))

(define pcfg*scfg->scfg!
  (pcfg*scfg->cfg! pcfg->scfg! make-scfg*))

)

(package (pcfg*pcfg->pcfg! pcfg*pcfg->scfg!)

(define ((pcfg*pcfg->cfg! transformer constructor) pcfg consequent alternative)
  (cond ((not pcfg)
	 (error "PCFG*PCFG->CFG!: Can't have null predicate"))
	((not consequent)
	 (if (not alternative)
	     (transformer pcfg)
	     (begin (pcfg-alternative-connect! pcfg alternative)
		    (constructor
		     (cfg-entry-node pcfg)
		     (hooks-union (pcfg-consequent-hooks pcfg)
				  (pcfg-consequent-hooks alternative))
		     (pcfg-alternative-hooks alternative)))))
	((not alternative)
	 (pcfg-consequent-connect! pcfg consequent)
	 (constructor (cfg-entry-node pcfg)
		      (pcfg-consequent-hooks consequent)
		      (hooks-union (pcfg-alternative-hooks consequent)
				   (pcfg-alternative-hooks pcfg))))
	(else
	 (pcfg-consequent-connect! pcfg consequent)
	 (pcfg-alternative-connect! pcfg alternative)
	 (constructor (cfg-entry-node pcfg)
		      (hooks-union (pcfg-consequent-hooks consequent)
				   (pcfg-consequent-hooks alternative))
		      (hooks-union (pcfg-alternative-hooks consequent)
				   (pcfg-alternative-hooks alternative))))))

(define pcfg*pcfg->pcfg!
  (pcfg*pcfg->cfg! identity-procedure make-pcfg))

(define pcfg*pcfg->scfg!
  (pcfg*pcfg->cfg! pcfg->scfg! make-scfg*))

)

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: compiler-package
;;; Scheme Syntax Table: (access compiler-syntax-table compiler-package)
;;; Tags Table Pathname: (access compiler-tags-pathname compiler-package)
;;; End:
  (for-each edge-disconnect-right! edges))