#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

(define-integrable (cfg-entry-edge cfg)
  (node->edge (cfg-entry-node cfg)))

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

(define (snode->pcfg-false snode)
  (make-pcfg snode
	     (make-null-hooks)
	     (list (make-hook snode set-snode-next-edge!))))

(define (snode->pcfg-true snode)
  (make-pcfg snode
	     (list (make-hook snode set-snode-next-edge!))
	     (make-null-hooks)))

(define (pcfg-invert pcfg)
  (make-pcfg (cfg-entry-node pcfg)
	     (pcfg-alternative-hooks pcfg)
	     (pcfg-consequent-hooks pcfg)))

;;;; Hook Datatype

(define-integrable make-hook cons)
(define-integrable hook-node car)
(define-integrable hook-connect cdr)

(define (hook=? x y)
  (and (eq? (hook-node x) (hook-node y))
       (eq? (hook-connect x) (hook-connect y))))

(define hook-member?
  (member-procedure hook=?))

(define-integrable (make-null-hooks)
  '())

(define-integrable hooks-null?
  null?)

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

;;;; Simplicity Tests

(define (scfg-simple? scfg)
  (cfg-branch-simple? (cfg-entry-node scfg) (scfg-next-hooks scfg)))

(define (pcfg-simple? pcfg)
  (let ((entry-node (cfg-entry-node pcfg)))
    (and (cfg-branch-simple? entry-node (pcfg-consequent-hooks pcfg))
	 (cfg-branch-simple? entry-node (pcfg-alternative-hooks pcfg)))))

(define (cfg-branch-simple? entry-node hooks)
  (and (not (null? hooks))
       (null? (cdr hooks))
       (eq? entry-node (hook-node (car hooks)))))

(define (scfg-null? scfg)
  (or (cfg-null? scfg)
      (cfg-branch-null? (cfg-entry-node scfg)
			(scfg-next-hooks scfg))))

(define (pcfg-true? pcfg)
  (and (hooks-null? (pcfg-alternative-hooks pcfg))
       (cfg-branch-null? (cfg-entry-node pcfg)
			 (pcfg-consequent-hooks pcfg))))

(define (pcfg-false? pcfg)
  (and (hooks-null? (pcfg-consequent-hooks pcfg))
       (cfg-branch-null? (cfg-entry-node pcfg)
			 (pcfg-alternative-hooks pcfg))))

(define (cfg-branch-null? entry-node hooks)
  (and (cfg-branch-simple? entry-node hooks)
       (cfg-node/noop? entry-node)))

;;;; Node-result Constructors

(define (scfg*node->node! scfg next-node)
  (if (scfg-null? scfg)
      next-node
      (begin
	(hooks-connect! (scfg-next-hooks scfg) next-node)
	(cfg-entry-node scfg))))

(define (pcfg*node->node! pcfg consequent-node alternative-node)
  (if (cfg-null? pcfg)
      (error "PCFG*NODE->NODE!: Can't have null predicate"))
  (cond ((pcfg-true? pcfg) consequent-node)
	((pcfg-false? pcfg) alternative-node)
	(else
	 (hooks-connect! (pcfg-consequent-hooks pcfg) consequent-node)
	 (hooks-connect! (pcfg-alternative-hooks pcfg) alternative-node)
	 (cfg-entry-node pcfg))))

;;;; CFG Construction

(define-integrable (scfg-next-connect! scfg cfg)
  (hooks-connect! (scfg-next-hooks scfg) (cfg-entry-node cfg)))

(define-integrable (pcfg-consequent-connect! pcfg cfg)
  (hooks-connect! (pcfg-consequent-hooks pcfg) (cfg-entry-node cfg)))

(define-integrable (pcfg-alternative-connect! pcfg cfg)
  (hooks-connect! (pcfg-alternative-hooks pcfg) (cfg-entry-node cfg)))

(define (scfg*scfg->scfg! scfg scfg*)
  (cond ((scfg-null? scfg) scfg*)
	((scfg-null? scfg*) scfg)
	(else
	 (scfg-next-connect! scfg scfg*)
	 (make-scfg (cfg-entry-node scfg) (scfg-next-hooks scfg*)))))

(define (scfg-append! . scfgs)
  (scfg*->scfg! scfgs))

(define scfg*->scfg!
  (let ()
    (define (find-non-null scfgs)
      (if (and (not (null? scfgs))
	       (scfg-null? (car scfgs)))
	  (find-non-null (cdr scfgs))
	  scfgs))

    (define (loop first second rest)
      (scfg-next-connect! first second)
      (if (null? rest)
	  second
	  (loop second (car rest) (find-non-null (cdr rest)))))

    (named-lambda (scfg*->scfg! scfgs)
      (let ((first (find-non-null scfgs)))
	(if (null? first)
	    (make-null-cfg)
	    (let ((second (find-non-null (cdr first))))
	      (if (null? second)
		  (car first)
		  (make-scfg (cfg-entry-node (car first))
			     (scfg-next-hooks
			      (loop (car first)
				    (car second)
				    (find-non-null (cdr second))))))))))))

(package (scfg*pcfg->pcfg! scfg*pcfg->scfg!)

(define ((scfg*pcfg->cfg! constructor) scfg pcfg)
  (if (cfg-null? pcfg)
      (error "SCFG*PCFG->CFG!: Can't have null predicate"))
  (cond ((scfg-null? scfg)
	 (constructor (cfg-entry-node pcfg)
		      (pcfg-consequent-hooks pcfg)
		      (pcfg-alternative-hooks pcfg)))
	((pcfg-true? pcfg)
	 (constructor (cfg-entry-node scfg)
		      (scfg-next-hooks scfg)
		      (make-null-hooks)))
	((pcfg-false? pcfg)
	 (constructor (cfg-entry-node scfg)
		      (make-null-hooks)
		      (scfg-next-hooks scfg)))
	(else
	 (scfg-next-connect! scfg pcfg)
	 (constructor (cfg-entry-node scfg)
		      (pcfg-consequent-hooks pcfg)
		      (pcfg-alternative-hooks pcfg)))))

(define-export scfg*pcfg->pcfg!
  (scfg*pcfg->cfg! make-pcfg))

(define-export scfg*pcfg->scfg!
  (scfg*pcfg->cfg! make-scfg*))

)

(package (pcfg*scfg->pcfg! pcfg*scfg->scfg!)

(define ((pcfg*scfg->cfg! constructor) pcfg consequent alternative)
  (if (cfg-null? pcfg)
      (error "PCFG*SCFG->CFG!: Can't have null predicate"))
  (cond ((pcfg-true? pcfg)
	 (constructor (cfg-entry-node consequent)
		      (scfg-next-hooks consequent)
		      (make-null-hooks)))
	((pcfg-false? pcfg)
	 (constructor (cfg-entry-node alternative)
		      (make-null-hooks)
		      (scfg-next-hooks alternative)))
	(else
	 (constructor (cfg-entry-node pcfg)
		      (connect! (pcfg-consequent-hooks pcfg) consequent)
		      (connect! (pcfg-alternative-hooks pcfg) alternative)))))

(define (connect! hooks scfg)
  (if (or (hooks-null? hooks)
	  (scfg-null? scfg))
      hooks
      (begin
	(hooks-connect! hooks (cfg-entry-node scfg))
	(scfg-next-hooks scfg))))

(define-export pcfg*scfg->pcfg!
  (pcfg*scfg->cfg! make-pcfg))

(define-export pcfg*scfg->scfg!
  (pcfg*scfg->cfg! make-scfg*))

)

(package (pcfg*pcfg->pcfg! pcfg*pcfg->scfg!)

(define ((pcfg*pcfg->cfg! constructor) pcfg consequent alternative)
  (if (cfg-null? pcfg)
      (error "PCFG*PCFG->CFG!: Can't have null predicate"))
  (cond ((pcfg-true? pcfg)
	 (constructor (cfg-entry-node consequent)
		      (pcfg-consequent-hooks consequent)
		      (pcfg-alternative-hooks consequent)))
	((pcfg-false? pcfg)
	 (constructor (cfg-entry-node alternative)
		      (pcfg-consequent-hooks alternative)
		      (pcfg-alternative-hooks alternative)))
	(else
	 (connect! (pcfg-consequent-hooks pcfg)
		   consequent
		   consequent-select
	   (lambda (cchooks cahooks)
	     (connect! (pcfg-alternative-hooks pcfg)
		       alternative
		       alternative-select
	       (lambda (achooks aahooks)
		 (constructor (cfg-entry-node pcfg)
			      (hooks-union cchooks achooks)
			      (hooks-union cahooks aahooks)))))))))

(define (connect! hooks pcfg select receiver)
  (cond ((hooks-null? hooks) (receiver (make-null-hooks) (make-null-hooks)))
	((cfg-null? pcfg) (select receiver hooks))
	((pcfg-true? pcfg) (consequent-select receiver hooks))
	((pcfg-false? pcfg) (alternative-select receiver hooks))
	(else
	 (hooks-connect! hooks (cfg-entry-node pcfg))
	 (receiver (pcfg-consequent-hooks pcfg)
		   (pcfg-alternative-hooks pcfg)))))

(define-integrable (consequent-select receiver hooks)
  (receiver hooks (make-null-hooks)))

(define-integrable (alternative-select receiver hooks)
  (receiver (make-null-hooks) hooks))

(define-export pcfg*pcfg->pcfg!
  (pcfg*pcfg->cfg! make-pcfg))

(define-export pcfg*pcfg->scfg!
  (pcfg*pcfg->cfg! make-scfg*))

)