#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/cfg3.scm,v 1.4 1987/08/31 21:50:31 cph Exp $

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

(define (scfg*node->node! scfg next-node)
  (if (cfg-null? scfg)
      next-node
      (begin (if next-node
		 (hooks-connect! (scfg-next-hooks scfg) next-node))
	     (cfg-entry-node scfg))))

(define (pcfg*node->node! pcfg consequent-node alternative-node)
  (if (cfg-null? pcfg)
      (error "PCFG*NODE->NODE!: Can't have null predicate"))
  (if consequent-node
      (hooks-connect! (pcfg-consequent-hooks pcfg) consequent-node))
  (if alternative-node
      (hooks-connect! (pcfg-alternative-hooks pcfg) alternative-node))
  (cfg-entry-node pcfg))

(define (scfg-simple? scfg)
  (cfg-simple? scfg scfg-next-hooks))

(define (pcfg-simple? pcfg)
  (and (cfg-simple? pcfg pcfg-consequent-hooks)
       (cfg-simple? pcfg pcfg-alternative-hooks)))

(define-integrable (cfg-simple? cfg cfg-hooks)
  (and (not (null? (cfg-hooks cfg)))
       (null? (cdr (cfg-hooks cfg)))
       (eq? (cfg-entry-node cfg) (hook-node (car (cfg-hooks cfg))))))

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
	(else
	 (scfg-next-connect! scfg scfg*)
	 (make-scfg (cfg-entry-node scfg) (scfg-next-hooks scfg*)))))

(define (scfg-append! . scfgs)
  (scfg*->scfg! scfgs))

(define scfg*->scfg!
  (let ()
    (define (loop first second rest)
      (scfg-next-connect! first second)
      (if (null? rest)
	  second
	  (loop second (car rest) (find-non-null (cdr rest)))))

    (define (find-non-null scfgs)
      (if (or (null? scfgs)
	      (car scfgs))
	  scfgs
	  (find-non-null (cdr scfgs))))

    (named-lambda (scfg*->scfg! scfgs)
      (let ((first (find-non-null scfgs)))
	(and (not (null? first))
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
  (if (not pcfg)
      (error "SCFG*PCFG->CFG!: Can't have null predicate"))
  (constructor (if (not scfg)
		   (cfg-entry-node pcfg)
		   (begin (scfg-next-connect! scfg pcfg)
			  (cfg-entry-node scfg)))
	       (pcfg-consequent-hooks pcfg)
	       (pcfg-alternative-hooks pcfg)))

(define-export scfg*pcfg->pcfg!
  (scfg*pcfg->cfg! make-pcfg))

(define-export scfg*pcfg->scfg!
  (scfg*pcfg->cfg! make-scfg*))

)

(package (pcfg*scfg->pcfg! pcfg*scfg->scfg!)

(define ((pcfg*scfg->cfg! constructor) pcfg consequent alternative)
  (if (not pcfg)
      (error "PCFG*SCFG->CFG!: Can't have null predicate"))
  (constructor (cfg-entry-node pcfg)
	       (connect! (pcfg-consequent-hooks pcfg) consequent)
	       (connect! (pcfg-alternative-hooks pcfg) alternative)))

(define (connect! hooks scfg)
  (cond ((not scfg) hooks)
	((null? hooks) '())
	(else
	 (hooks-connect! hooks (cfg-entry-node scfg))
	 (scfg-next-hooks scfg))))

(define-export pcfg*scfg->pcfg!
  (pcfg*scfg->cfg! make-pcfg))

(define-export pcfg*scfg->scfg!
  (pcfg*scfg->cfg! make-scfg*))

)

(package (pcfg*pcfg->pcfg! pcfg*pcfg->scfg!)

(define ((pcfg*pcfg->cfg! constructor) pcfg consequent alternative)
  (if (not pcfg)
      (error "PCFG*PCFG->CFG!: Can't have null predicate"))
  (connect! (pcfg-consequent-hooks pcfg) consequent consequent-select
    (lambda (cchooks cahooks)
      (connect! (pcfg-alternative-hooks pcfg) alternative alternative-select
	(lambda (achooks aahooks)
	  (constructor (cfg-entry-node pcfg)
		       (hooks-union cchooks achooks)
		       (hooks-union cahooks aahooks)))))))

(define (connect! hooks pcfg select receiver)
  (cond ((not pcfg) (select receiver hooks))
	((null? hooks) (receiver '() '()))
	(else
	 (hooks-connect! hooks (cfg-entry-node pcfg))
	 (receiver (pcfg-consequent-hooks pcfg)
		   (pcfg-alternative-hooks pcfg)))))

(define (consequent-select receiver hooks)
  (receiver hooks '()))

(define (alternative-select receiver hooks)
  (receiver '() hooks))

(define-export pcfg*pcfg->pcfg!
  (pcfg*pcfg->cfg! make-pcfg))

(define-export pcfg*pcfg->scfg!
  (pcfg*pcfg->cfg! make-scfg*))

)