#| -*-Scheme-*-

$Id: rtlcfg.scm,v 4.11 2002/11/20 19:45:56 cph Exp $

Copyright (c) 1987-1989, 1999, 2002 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

;;;; RTL CFG Nodes

(declare (usual-integrations))

(define-snode sblock)
(define-pnode pblock)

(define-vector-slots bblock 6
  instructions
  live-at-entry
  live-at-exit
  (new-live-at-exit register-map)
  label
  continuations)

(define-vector-slots sblock 12
  continuation)

(define (make-sblock instructions)
  (make-pnode sblock-tag instructions false false false false '() false))

(define-vector-slots pblock 12
  consequent-lap-generator
  alternative-lap-generator)

(define (make-pblock instructions)
  (make-pnode pblock-tag instructions false false false false '() false false))

(define-integrable (statement->srtl statement)
  (snode->scfg (make-sblock (make-rtl-instruction statement))))

(define-integrable (predicate->prtl predicate)
  (pnode->pcfg (make-pblock (make-rtl-instruction predicate))))

(let ((bblock-describe
       (lambda (bblock)
	 (descriptor-list bblock
			  bblock
			  instructions
			  live-at-entry
			  live-at-exit
			  register-map
			  label
			  continuations))))
  (set-vector-tag-description!
   sblock-tag
   (lambda (sblock)
     (append! ((vector-tag-description snode-tag) sblock)
	      (bblock-describe sblock)
	      (descriptor-list sblock
			       sblock
			       continuation))))
  (set-vector-tag-description!
   pblock-tag
   (lambda (pblock)
     (append! ((vector-tag-description pnode-tag) pblock)
	      (bblock-describe pblock)
	      (descriptor-list pblock
			       pblock
			       consequent-lap-generator
			       alternative-lap-generator)))))

(define-integrable (bblock-reversed-instructions bblock)
  (rinst-reversed (bblock-instructions bblock)))

(define (bblock-compress! bblock limit-predicate)
  (let ((walk-next?
	 (if limit-predicate
	     (lambda (next) (and next (not (limit-predicate next))))
	     (lambda (next) next))))
    (let walk-bblock ((bblock bblock))
      (if (not (node-marked? bblock))
	  (begin
	    (node-mark! bblock)
	    (if (sblock? bblock)
		(let ((next (snode-next bblock)))
		  (if (walk-next? next)
		      (begin
			(if (null? (cdr (node-previous-edges next)))
			    (begin
			      (set-rinst-next!
			       (rinst-last (bblock-instructions bblock))
			       (bblock-instructions next))
			      (set-bblock-instructions!
			       next
			       (bblock-instructions bblock))
			      (snode-delete! bblock)))
			(walk-bblock next))))
		(begin
		  (let ((consequent (pnode-consequent bblock)))
		    (if (walk-next? consequent)
			(walk-bblock consequent)))
		  (let ((alternative (pnode-alternative bblock)))
		    (if (walk-next? alternative)
			(walk-bblock alternative))))))))))

(define (bblock-walk-forward bblock procedure)
  (let loop ((rinst (bblock-instructions bblock)))
    (procedure rinst)
    (if (rinst-next rinst) (loop (rinst-next rinst)))))

(define (bblock-walk-backward bblock procedure)
  (let loop ((rinst (bblock-instructions bblock)))
    (if (rinst-next rinst) (loop (rinst-next rinst)))
    (procedure rinst)))

(define (bblock-label! bblock)
  (or (bblock-label bblock)
      (let ((label (generate-label)))
	(set-bblock-label! bblock label)
	label)))

(define (bblock-perform-deletions! bblock)
  (define (loop rinst)
    (let ((next
	   (and (rinst-next rinst)
		(loop (rinst-next rinst)))))
      (if (rinst-rtl rinst)
	  (begin (set-rinst-next! rinst next)
		 rinst)
	  next)))
  (let ((instructions (loop (bblock-instructions bblock))))
    (if instructions
	(set-bblock-instructions! bblock instructions)
	(begin
	  (snode-delete! bblock)
	  (set-rgraph-bblocks! *current-rgraph*
			       (delq! bblock
				      (rgraph-bblocks *current-rgraph*)))))))

(define-integrable (pcfg/prefer-consequent! pcfg)
  (pcfg/prefer-branch! 'CONSEQUENT pcfg))

(define-integrable (pcfg/prefer-alternative! pcfg)
  (pcfg/prefer-branch! 'ALTERNATIVE pcfg))

(define (pcfg/prefer-branch! branch pcfg)
  (let loop ((bblock (cfg-entry-node pcfg)))
    (cond ((pblock? bblock)
	   (cfg-node-put! bblock cfg/prefer-branch/tag branch))
	  ((sblock? bblock)
	   (loop (snode-next bblock)))
	  (else
	   (error "PCFG/PREFER-BRANCH!: Unknown bblock type" bblock))))
  pcfg)

(define-integrable (pnode/preferred-branch pnode)
  (cfg-node-get pnode cfg/prefer-branch/tag))

(define cfg/prefer-branch/tag
  (intern "#[(compiler)cfg/prefer-branch]"))

;;;; RTL Instructions

(define-vector-slots rinst 0
  rtl
  dead-registers
  next)

(define (make-rtl-instruction rtl)
  (vector rtl '() false))

(define-integrable (rinst-dead-register? rinst register)
  (memq register (rinst-dead-registers rinst)))

(define (rinst-last rinst)
  (if (rinst-next rinst)
      (rinst-last (rinst-next rinst))
      rinst))

(define (rinst-disconnect-previous! bblock rinst)
  (let loop ((rinst* (bblock-instructions bblock)))
    (if (eq? rinst (rinst-next rinst*))
	(set-rinst-next! rinst* false)
	(loop (rinst-next rinst*)))))

(define (rinst-length rinst)
  (let loop ((rinst rinst) (length 0))
    (if rinst
	(loop (rinst-next rinst) (1+ length))
	length)))

(define (rinst-reversed rinst)
  (let loop ((rinst rinst) (result '()))
    (if rinst
	(loop (rinst-next rinst) (cons rinst result))
	result)))