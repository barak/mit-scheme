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

;;; $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/cfg1.scm,v 1.138 1986/12/16 23:45:57 cph Exp $

(declare (usual-integrations))
(using-syntax (access compiler-syntax-table compiler-package)

;;;; Node Types

(define cfg-node-tag (make-vector-tag false 'CFG-NODE))
(define cfg-node? (tagged-vector-subclass-predicate cfg-node-tag))
(define-vector-slots node 1 previous alist generation)

(define (cfg-node-describe node)
  `((NODE-PREVIOUS ,(node-previous node))
    (NODE-ALIST ,(node-alist node))
    (NODE-GENERATION ,(node-generation node))))

(define-vector-method cfg-node-tag ':DESCRIBE
  cfg-node-describe)

(define snode-tag (make-vector-tag cfg-node-tag 'SNODE))
(define snode? (tagged-vector-subclass-predicate snode-tag))
(define-vector-slots snode 4 &next)

(define (make-snode tag . extra)
  (list->vector (cons* tag '() '() false false extra)))

(define (snode-describe snode)
  (append! (cfg-node-describe snode)
	   `((SNODE-&NEXT ,(snode-&next snode)))))

(define-vector-method snode-tag ':DESCRIBE
  snode-describe)

(define pnode-tag (make-vector-tag cfg-node-tag 'PNODE))
(define pnode? (tagged-vector-subclass-predicate pnode-tag))
(define-vector-slots pnode 4 &consequent &alternative)

(define (make-pnode tag . extra)
  (list->vector (cons* tag '() '() false false false extra)))

(define (pnode-describe pnode)
  (append! (cfg-node-describe pnode)
	   `((PNODE-&CONSEQUENT ,(pnode-&consequent pnode))
	     (PNODE-&ALTERNATIVE ,(pnode-&alternative pnode)))))

(define-vector-method pnode-tag ':DESCRIBE
  pnode-describe)

;;;; Holders

;;; Entry/Exit holder nodes are used to hold onto the edges of a
;;; graph.  Entry holders need only a next connection, and exit
;;; holders need only a previous connection.

(define entry-holder-tag (make-vector-tag cfg-node-tag 'ENTRY-HOLDER))
(define-vector-slots entry-holder 1 &next)

(define (entry-holder? node)
  (eq? (vector-ref node 0) entry-holder-tag))

(define-integrable (make-entry-holder)
  (vector entry-holder-tag false))

(define (node->holder node)
  (let ((holder (make-entry-holder)))
    (entry-holder-connect! holder node)
    holder))

(define (set-entry-holder-next! entry-holder node)
  (entry-holder-disconnect! entry-holder)
  (entry-holder-connect! entry-holder node))

(define-vector-method entry-holder-tag ':DESCRIBE
  (lambda (entry-holder)
    `((ENTRY-HOLDER-&NEXT ,(entry-holder-&next entry-holder)))))

(define exit-holder-tag (make-vector-tag cfg-node-tag 'EXIT-HOLDER))

(define (exit-holder? node)
  (eq? (vector-ref node 0) exit-holder-tag))

(define-integrable (make-exit-holder)
  (vector exit-holder-tag '()))

(define-vector-method exit-holder-tag ':DESCRIBE
  (lambda (exit-holder)
    `((NODE-PREVIOUS ,(node-previous exit-holder)))))

(define (next-reference node)
  (and node (not (exit-holder? node)) node))

(define-integrable (snode-next snode)
  (next-reference (snode-&next snode)))

(define-integrable (pnode-consequent pnode)
  (next-reference (pnode-&consequent pnode)))

(define-integrable (pnode-alternative pnode)
  (next-reference (pnode-&alternative pnode)))

(define-integrable (entry-holder-next entry)
  (next-reference (entry-holder-&next entry)))

(define-integrable (entry-holder-hook? hook)
  (entry-holder? (hook-node hook)))

(define-integrable (node-previous=0? node)
  (hooks=0? (node-previous node)))

(define (hooks=0? hooks)
  (or (null? hooks)
      (and (entry-holder-hook? (car hooks))
	   (hooks=0? (cdr hooks)))))

(define-integrable (node-previous>0? node)
  (hooks>0? (node-previous node)))

(define (hooks>0? hooks)
  (and (not (null? hooks))
       (or (not (entry-holder-hook? (car hooks)))
	   (hooks>0? (cdr hooks)))))

(define-integrable (node-previous=1? node)
  (hooks=1? (node-previous node)))

(define (hooks=1? hooks)
  (and (not (null? hooks))
       ((if (entry-holder-hook? (car hooks)) hooks=1? hooks=0?)
	(cdr hooks))))

(define-integrable (node-previous>1? node)
  (hooks>1? (node-previous node)))

(define (hooks>1? hooks)
  (and (not (null? hooks))
       ((if (entry-holder-hook? (car hooks)) hooks>1? hooks>0?)
	(cdr hooks))))

(define-integrable (node-previous-first node)
  (hook-node (hooks-first (node-previous node))))

(define (hooks-first hooks)
  (cond ((null? hooks) (error "No first hook"))
	((entry-holder-hook? (car hooks)) (hooks-first (cdr hooks)))
	(else (car hooks))))

(define (for-each-previous-node node procedure)
  (for-each (lambda (hook)
	      (let ((node (hook-node hook)))
		(if (not (entry-holder? node))
		    (procedure node))))
	    (node-previous node)))

;;;; Frames

(define frame-tag (make-vector-tag false 'FRAME))
(define-vector-slots frame 1 &entry)

(define-integrable (frame-entry-node frame)
  (entry-holder-next (frame-&entry frame)))

(define (frame-describe frame)
  `((FRAME-&ENTRY ,(frame-&entry frame))))

(define sframe-tag (make-vector-tag frame-tag 'SFRAME))
(define-vector-slots sframe 2 &next)

(define-integrable (make-sframe entry next)
  (vector sframe-tag entry next))

(define-integrable (sframe-next-hooks sframe)
  (node-previous (sframe-&next sframe)))

(define-vector-method sframe-tag ':DESCRIBE
  (lambda (sframe)
    (append! (frame-describe sframe)
	     `((SFRAME-&NEXT ,(sframe-&next sframe))))))

(define (scfg->sframe scfg)
  (let ((entry (make-entry-holder))
	(next (make-exit-holder)))
    (entry-holder-connect! entry (cfg-entry-node scfg))
    (hooks-connect! (scfg-next-hooks scfg) next)
    (make-sframe entry next)))

(define (sframe-replace-cfg! sframe scfg)
  (let ((entry (frame-&entry sframe))
	(next (sframe-&next sframe)))
    (node-disconnect! entry (entry-holder-&next entry))
    (hooks-disconnect! (node-previous next) next)
    (entry-holder-connect! entry (cfg-entry-node scfg))
    (hooks-connect! (scfg-next-hooks scfg) next)))

(define (sframe->scfg sframe)
  (let ((entry (frame-entry-node sframe)))
    (if entry
	(make-scfg entry (sframe-next-hooks sframe))
	(make-null-cfg))))

(define pframe-tag (make-vector-tag frame-tag 'PFRAME))
(define-vector-slots pframe 2 &consequent &alternative)

(define-integrable (make-pframe entry consequent alternative)
  (vector pframe-tag entry consequent alternative))

(define-integrable (pframe-consequent-hooks pframe)
  (node-previous (pframe-&consequent pframe)))

(define-integrable (pframe-alternative-hooks pframe)
  (node-previous (pframe-&alternative pframe)))

(define-vector-method pframe-tag ':DESCRIBE
  (lambda (pframe)
    (append! (frame-describe pframe)
	     `((PFRAME-&CONSEQUENT ,(pframe-&consequent pframe))
	       (PFRAME-&ALTERNATIVE ,(pframe-&alternative pframe))))))

(define (pcfg->pframe pcfg)
  (let ((entry (make-entry-holder))
	(consequent (make-exit-holder))
	(alternative (make-exit-holder)))
    (entry-holder-connect! entry (cfg-entry-node pcfg))
    (hooks-connect! (pcfg-consequent-hooks pcfg) consequent)
    (hooks-connect! (pcfg-alternative-hooks pcfg) alternative)
    (make-pframe entry consequent alternative)))

(define (pframe-replace-cfg! pframe pcfg)
  (let ((entry (frame-&entry pframe))
	(consequent (pframe-&consequent pframe))
	(alternative (pframe-&alternative pframe)))
    (node-disconnect! entry (entry-holder-&next entry))
    (hooks-disconnect! (node-previous consequent) consequent)
    (hooks-disconnect! (node-previous alternative) alternative)
    (entry-holder-connect! entry (cfg-entry-node pcfg))
    (hooks-connect! (pcfg-consequent-hooks pcfg) consequent)
    (hooks-connect! (pcfg-alternative-hooks pcfg) alternative)))

(define (pframe->scfg pframe)
  (let ((entry (frame-entry-node pframe)))
    (if entry
	(make-scfg entry
		   (pframe-consequent-hooks pframe)
		   (pframe-alternative-hooks pframe))
	(make-null-cfg))))

;;;; Noops

(define noop-node-tag (make-vector-tag cfg-node-tag 'NOOP))
(define-vector-slots noop-node 1 previous next)
(define *noop-nodes*)

(define-integrable (make-noop-node)
  (let ((node (vector noop-node-tag '() false)))
    (set! *noop-nodes* (cons node *noop-nodes*))
    node))

(define (delete-noop-nodes!)
  (for-each noop-node-delete! *noop-nodes*)
  (set! *noop-nodes* '()))

(define (noop-node-delete! noop-node)
  (hooks-replace! (let ((previous (noop-node-previous noop-node)))
		    (hooks-disconnect! previous noop-node)
		    previous)
		  noop-node noop-node-next))

(define (make-false-pcfg)
  (let ((node (make-noop-node)))
    (make-pcfg node
	       '()
	       (list (make-hook node set-noop-node-next!)))))

(define (make-true-pcfg)
  (let ((node (make-noop-node)))
    (make-pcfg node
	       (list (make-hook node set-noop-node-next!))
	       '())))

(define (constant->pcfg value)
  ((if value make-true-pcfg make-false-pcfg)))

;;;; Simple Construction

(define ((node-connector set-node-next!) node next)
  (hook-connect! (make-hook node set-node-next!) next))

(define snode-next-connect! (node-connector set-snode-&next!))
(define pnode-consequent-connect! (node-connector set-pnode-&consequent!))
(define pnode-alternative-connect! (node-connector set-pnode-&alternative!))
(define entry-holder-connect! (node-connector set-entry-holder-&next!))

(define ((node-disconnector node-next) node)
  (let ((next (node-next node)))
    (if next (node-disconnect! node next))
    next))

(define (node-disconnect! node next)
  (hook-disconnect! (find-hook node next) next))

(define snode-next-disconnect! (node-disconnector snode-&next))
(define pnode-consequent-disconnect! (node-disconnector pnode-&consequent))
(define pnode-alternative-disconnect! (node-disconnector pnode-&alternative))
(define entry-holder-disconnect! (node-disconnector entry-holder-next))

(define (node-previous-disconnect! node)
  (let ((hooks (node-previous node)))
    (hooks-disconnect! hooks node)
    hooks))

(define (node-get node key)
  (let ((entry (assq key (node-alist node))))
    (and entry (cdr entry))))

(define (node-put! node key item)
  (let ((entry (assq key (node-alist node))))
    (if entry
	(set-cdr! entry item)
	(set-node-alist! node (cons (cons key item) (node-alist node))))))

(define *generation*)

(define make-generation
  (let ((generation 0))
    (named-lambda (make-generation)
      (let ((value generation))
	(set! generation (1+ generation))
	value))))

;;;; CFG Objects

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

(define (node->scfg node set-node-next!)
  (make-scfg node
	     (list (make-hook node set-node-next!))))

(define-integrable (snode->scfg snode)
  (node->scfg snode set-snode-&next!))

(define (node->pcfg node set-node-consequent! set-node-alternative!)
  (make-pcfg node
	     (list (make-hook node set-node-consequent!))
	     (list (make-hook node set-node-alternative!))))

(define-integrable (pnode->pcfg pnode)
  (node->pcfg pnode
	      set-pnode-&consequent!
	      set-pnode-&alternative!))

(define-integrable (make-null-cfg)
  false)

(define-integrable (cfg-null? cfg)
  (false? cfg))

;;;; Hooks

;;; There are several different types of node, each of which has
;;; different types of "next" connections, for example, the predicate
;;; node has a consequent and an alternative connection.  Any kind of
;;; node can be connected to either of these connections.  Since it is
;;; desirable to be able to splice nodes in and out of the graph, we
;;; would like to be able to dis/connect a node from its previous node
;;; without knowing anything about that node.  Hooks provide this
;;; capability by providing an operation for setting the previous
;;; node's appropriate "next" connection to any value.

(define-integrable make-hook cons)
(define-integrable hook-node car)
(define-integrable hook-basher cdr)
(define-integrable hooks-union append!)

(define-integrable (find-hook node next)
  (assq node (node-previous next)))

(define (hook-connect! hook node)
  (set-node-previous! node (cons hook (node-previous node)))
  ((hook-basher hook) (hook-node hook) node))

(define (hooks-connect! hooks node)
  (define (loop hooks)
    (if (not (null? hooks))
	(begin (hook-connect! (car hooks) node)
	       (loop (cdr hooks)))))
  (loop hooks))

(define (hook-disconnect! hook node)
  (set-node-previous! node (delq! hook (node-previous node)))
  ((hook-basher hook) (hook-node hook) false))

(define (hooks-disconnect! hooks node)
  (define (loop hooks)
    (if (not (null? hooks))
	(begin (hook-disconnect! (car hooks) node)
	       (loop (cdr hooks)))))
  (loop hooks))

;;;; CFG Construction

(define-integrable (scfg-next-connect! scfg cfg)
  (hooks-connect! (scfg-next-hooks scfg) (cfg-entry-node cfg)))

(define-integrable (pcfg-consequent-connect! pcfg cfg)
  (hooks-connect! (pcfg-consequent-hooks pcfg) (cfg-entry-node cfg)))

(define-integrable (pcfg-alternative-connect! pcfg cfg)
  (hooks-connect! (pcfg-alternative-hooks pcfg) (cfg-entry-node cfg)))

(package (scfg-append! scfg*->scfg!)

(define (scfg-append! . scfgs)
  (scfg*->scfg! scfgs))

(define (scfg*->scfg! scfgs)
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

(define (scfg*scfg->scfg! scfg scfg*)
  (cond ((not scfg) scfg*)
	((not scfg*) scfg)
	(else (scfg-next-connect! scfg scfg*)
	      (make-scfg (cfg-entry-node scfg) (scfg-next-hooks scfg*)))))

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

;;;; CFG Editing Support

(define node-edit!
  (let ((tail
	 (lambda (procedure entry)
	   (procedure (entry-holder-next entry))
	   (entry-holder-disconnect! entry))))
    (lambda (node procedure)
      (let ((entry (make-entry-holder)))
	(entry-holder-connect! entry node)
	(tail procedure entry)))))

(define scfg-edit!
  (let ((tail
	 (lambda (procedure entry exit)
	   (procedure (entry-holder-next entry))
	   (let ((node (entry-holder-disconnect! entry)))
	     (if node
		 (make-scfg node
			    (node-previous-disconnect! exit))
		 (make-null-cfg))))))
    (lambda (scfg procedure)
      (and (not (cfg-null? scfg))
	   (let ((entry (make-entry-holder))
		 (exit (make-exit-holder)))
	     (entry-holder-connect! entry (cfg-entry-node scfg))
	     (hooks-connect! (scfg-next-hooks scfg) exit)
	     (tail procedure entry exit))))))

(define pcfg-edit!
  (let ((tail
	 (lambda (procedure entry consequent alternative)
	   (procedure (entry-holder-next entry))
	   (make-pcfg (entry-holder-disconnect! entry)
		      (node-previous-disconnect! consequent)
		      (node-previous-disconnect! alternative)))))
    (lambda (pcfg procedure)
      (and (not (cfg-null? pcfg))
	   (let ((entry (make-entry-holder))
		 (exit (make-exit-holder)))
	     (entry-holder-connect! entry (cfg-entry-node pcfg))
	     (hooks-connect! (pcfg-consequent-hooks pcfg) consequent)
	     (hooks-connect! (pcfg-alternative-hooks pcfg) alternative)
	     (tail procedure entry consequent alternative))))))

(define (node-replace! node cfg)
  ((vector-method node node-replace!) node cfg))

(define (snode-replace! snode scfg)
  (hooks-replace! (let ((previous (node-previous snode)))
		    (hooks-disconnect! previous snode)
		    (if (not scfg)
			previous
			(begin (hooks-connect! previous (cfg-entry-node scfg))
			       (scfg-next-hooks scfg))))
		  snode snode-&next))

(define (pnode-replace! pnode pcfg)
  (if (not pcfg)
      (error "PNODE-REPLACE!: Cannot delete pnode"))
  (let ((previous (node-previous pnode))
	(consequent (pnode-&consequent pnode))
	(alternative (pnode-&alternative pnode)))
    (hooks-disconnect! previous pnode)
    (hooks-connect! previous (cfg-entry-node pcfg))
    (hooks-replace! (pcfg-consequent-hooks pcfg) pnode pnode-&consequent)
    (hooks-replace! (pcfg-alternative-hooks pcfg) pnode pnode-&alternative)))

(define-vector-method snode-tag node-replace! snode-replace!)
(define-vector-method pnode-tag node-replace! pnode-replace!)

(define (snode-delete! snode)
  (hooks-replace! (let ((previous (node-previous snode)))
		    (hooks-disconnect! previous snode)
		    previous)
		  snode snode-&next))

(define (hooks-replace! hooks node next)
  (let ((next (next node)))
    (if next
	(begin (node-disconnect! node next)
	       (hooks-connect! hooks next)))))

(define (hook-insert-scfg! hook next scfg)
  (if scfg
      (begin (hook-disconnect! hook next)
	     (hook-connect! hook (cfg-entry-node scfg))
	     (hooks-connect! (scfg-next-hooks scfg) next))))

(define (node-insert-scfg! node scfg)
  (if scfg
      (let ((previous (node-previous node)))
	(hooks-disconnect! previous node)
	(hooks-connect! previous (cfg-entry-node scfg))
	(hooks-connect! (scfg-next-hooks scfg) node))))

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: compiler-package
;;; Scheme Syntax Table: (access compiler-syntax-table compiler-package)
;;; Tags Table Pathname: (access compiler-tags-pathname compiler-package)
;;; End:
  (for-each edge-disconnect-right! edges))