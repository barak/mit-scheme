#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlbase/rtlcfg.scm,v 4.2 1987/12/30 07:07:18 cph Exp $

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

;;;; RTL CFG Nodes

(declare (usual-integrations))

(define-snode sblock)
(define-pnode pblock)

(define-vector-slots bblock 6
  instructions
  live-at-entry
  live-at-exit
  (new-live-at-exit register-map)
  label)

(define (make-sblock instructions)
  (make-pnode sblock-tag instructions false false false false))

(define-vector-slots pblock 11
  consequent-lap-generator
  alternative-lap-generator)

(define (make-pblock instructions)
  (make-pnode pblock-tag instructions false false false false false false))

(define-vector-slots rinst 0
  rtl
  dead-registers
  next)

(define (make-rtl-instruction rtl)
  (vector rtl '() false))

(define-integrable (statement->srtl statement)
  (snode->scfg (make-sblock (make-rtl-instruction statement))))

(define-integrable (predicate->prtl predicate)
  (pnode->pcfg (make-pblock (make-rtl-instruction predicate))))

(let ((bblock-describe
       (lambda (bblock)
	 (descriptor-list bblock
			  instructions
			  live-at-entry
			  live-at-exit
			  register-map
			  label))))
  (set-vector-tag-description!
   sblock-tag
   (lambda (sblock)
     (append! ((vector-tag-description snode-tag) sblock)
	      (bblock-describe sblock))))
  (set-vector-tag-description!
   pblock-tag
   (lambda (pblock)
     (append! ((vector-tag-description pnode-tag) pblock)
	      (bblock-describe pblock)
	      (descriptor-list pblock
			       consequent-lap-generator
			       alternative-lap-generator)))))

(define-integrable (rinst-dead-register? rinst register)
  (memq register (rinst-dead-registers rinst)))

(package (bblock-compress!)

(define-export (bblock-compress! bblock)
  (if (not (node-marked? bblock))
      (begin
	(node-mark! bblock)
	(if (sblock? bblock)
	    (let ((next (snode-next bblock)))
	      (if next
		  (begin
		    (if (node-previous=1? next)
			(begin
			  (set-rinst-next!
			   (rinst-last (bblock-instructions bblock))
			   (bblock-instructions next))
			  (set-bblock-instructions!
			   next
			   (bblock-instructions bblock))
			  (snode-delete! bblock)))
		    (bblock-compress! next))))
	    (begin (let ((consequent (pnode-consequent bblock)))
		     (if consequent
			 (bblock-compress! consequent)))
		   (let ((alternative (pnode-alternative bblock)))
		     (if alternative
			 (bblock-compress! alternative))))))))

(define (rinst-last rinst)
  (if (rinst-next rinst)
      (rinst-last (rinst-next rinst))
      rinst))

)

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

(define (make-linearizer map-inst bblock-linearize)
  (lambda (rgraphs)
    (with-new-node-marks
     (lambda ()
       (map-inst (lambda (rgraph)
		   (map-inst (lambda (edge)
			       (let ((bblock (edge-right-node edge)))
				 (if (node-marked? bblock)
				     '()
				     (bblock-linearize bblock))))
			     (rgraph-entry-edges rgraph)))
	       rgraphs)))))