#| -*-Scheme-*-

$Id: rtline.scm,v 4.12 2002/11/20 19:45:56 cph Exp $

Copyright (c) 1988, 1989, 1999 Massachusetts Institute of Technology

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

;;;; RTL linearizer

(declare (usual-integrations))

(define ((make-linearizer bblock-linearize
			  initial-value
			  instruction-append!
			  final-value)
	 root procedures continuations)
  continuations				;ignore
  (with-new-node-marks
   (lambda ()
     (let ((input-queue (make-queue))
	   (output (initial-value)))
       (let ((queue-continuations!
	      (lambda (bblock)
		(for-each (lambda (bblock)
			    (if (not (node-marked? bblock))
				(enqueue!/unsafe input-queue bblock)))
			  (bblock-continuations bblock)))))
	 (let ((process-bblock!
		(lambda (bblock)
		  (if (not (node-marked? bblock))
		      (set! output
			    (instruction-append!
			     output
			     (bblock-linearize bblock
					       queue-continuations!)))))))
	   (process-bblock!
	    (cond ((rtl-expr? root) (rtl-expr/entry-node root))
		  ((rtl-procedure? root) (rtl-procedure/entry-node root))
		  (else (error "Illegal linearization root" root))))
	   (queue-map!/unsafe input-queue process-bblock!)
	   (for-each (lambda (procedure)
		       (process-bblock! (rtl-procedure/entry-node procedure))
		       (queue-map!/unsafe input-queue process-bblock!))
		     procedures)
	   (final-value output)))))))

(define (setup-bblock-continuations! rgraphs)
  (for-each
   (lambda (rgraph)
     (for-each
      (lambda (bblock)
	(let ((continuations '()))
	  (bblock-walk-forward bblock
	    (lambda (rinst)
	      (let loop ((expression (cdr (rinst-rtl rinst))))
		(if (pair? expression)
		    (cond ((eq? (car expression) 'ENTRY:CONTINUATION)
			   ;; Because the average number of
			   ;; continuations per basic block is usually
			   ;; less than one, we optimize this case to
			   ;; speed up the accumulation.
			   (cond ((null? continuations)
				  (set! continuations
					(list (cadr expression))))
				 ((not (memq (cadr expression) continuations))
				  (set! continuations
					(cons (cadr expression)
					      continuations)))))
			  ((not (eq? (car expression) 'CONSTANT))
			   (for-each loop (cdr expression))))))))
	  (set-bblock-continuations!
	   bblock
	   (map (lambda (label)
		  (rtl-continuation/entry-node (label->object label)))
		continuations)))
	(if (sblock? bblock)
	    (let ((rtl (rinst-rtl (rinst-last (bblock-instructions bblock)))))
	      (if (rtl:invocation? rtl)
		  (let ((continuation (rtl:invocation-continuation rtl)))
		    (if continuation
			(set-sblock-continuation!
			 bblock
			 (rtl-continuation/entry-node
			  (label->object continuation)))))))))
      (rgraph-bblocks rgraph)))
   rgraphs))

;;; The linearizer attaches labels to nodes under two conditions.  The
;;; first is that the node in question has more than one previous
;;; neighboring node.  The other is when a conditional branch requires
;;; such a label.  It is assumed that if one encounters a node that
;;; has already been linearized, that it has a label, since this
;;; implies that it has more than one previous neighbor.

(define (bblock-linearize-rtl bblock queue-continuations!)
  (define (linearize-bblock bblock)
    (node-mark! bblock)
    (queue-continuations! bblock)
    (if (and (not (bblock-label bblock))
	     (node-previous>1? bblock))
	(bblock-label! bblock))
    (let ((kernel
	   (lambda ()
	     (let loop ((rinst (bblock-instructions bblock)))
	       (cond ((rinst-next rinst)
		      (cons (rinst-rtl rinst) (loop (rinst-next rinst))))
		     ((sblock? bblock)
		      (cons (rinst-rtl rinst)
			    (let ((next (snode-next bblock)))
			      (if next
				  (linearize-sblock-next next)
				  (let ((bblock (sblock-continuation bblock)))
				    (if (and bblock
					     (not (node-marked? bblock)))
					(linearize-bblock bblock)
					'()))))))
		     (else
		      (linearize-pblock bblock
					(rinst-rtl rinst)
					(pnode-consequent bblock)
					(pnode-alternative bblock))))))))
      (if (bblock-label bblock)
	  `(,(rtl:make-label-statement (bblock-label bblock)) ,@(kernel))
	  (kernel))))

  (define (linearize-sblock-next bblock)
    (if (node-marked? bblock)
	`(,(rtl:make-jump-statement (bblock-label bblock)))
	(linearize-bblock bblock)))

  (define (linearize-pblock pblock predicate cn an)
    (let ((heed-preference
	   (lambda (finish)
	     (if (eq? 'CONSEQUENT (pnode/preferred-branch pblock))
		 (finish (rtl:negate-predicate predicate) an cn)
		 (finish predicate cn an)))))
      (if (node-marked? cn)
	  (if (node-marked? an)
	      (heed-preference
	       (lambda (predicate cn an)
		 `(,(rtl:make-jumpc-statement predicate (bblock-label cn))
		   ,(rtl:make-jump-statement (bblock-label an)))))
	      `(,(rtl:make-jumpc-statement predicate (bblock-label cn))
		,@(linearize-bblock an)))
	  (if (node-marked? an)
	      `(,(rtl:make-jumpc-statement (rtl:negate-predicate predicate)
					   (bblock-label an))
		,@(linearize-bblock cn))
	      (heed-preference
	       (lambda (predicate cn an)
		 (let ((clabel (bblock-label! cn))
		       (alternative (linearize-bblock an)))
		   `(,(rtl:make-jumpc-statement predicate clabel)
		     ,@alternative
		     ,@(if (node-marked? cn) '() (linearize-bblock cn))))))))))

  (linearize-bblock bblock))

(define linearize-rtl
  (make-linearizer bblock-linearize-rtl
    (lambda () (let ((value (list false))) (cons value value)))
    (lambda (accumulator instructions)
      (set-cdr! (cdr accumulator) instructions)
      (set-cdr! accumulator (last-pair instructions))
      accumulator)
    cdar))