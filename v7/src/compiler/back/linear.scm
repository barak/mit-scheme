#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/back/linear.scm,v 4.9 1990/01/18 22:42:06 cph Exp $

Copyright (c) 1987, 1988, 1990 Massachusetts Institute of Technology

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

;;;; LAP linearizer

(declare (usual-integrations))

(define (bblock-linearize-lap bblock queue-continuations!)
  (define (linearize-bblock bblock)
    (LAP ,@(linearize-bblock-1 bblock)
	 ,@(linearize-next bblock)))

  (define (linearize-bblock-1 bblock)
    (node-mark! bblock)
    (queue-continuations! bblock)
    (if (and (not (bblock-label bblock))
	     (let loop ((bblock bblock))
	       (or (node-previous>1? bblock)
		   (and (node-previous=1? bblock)
			(let ((previous (node-previous-first bblock)))
			  (and (sblock? previous)
			       (null? (bblock-instructions previous))
			       (loop previous)))))))
	(bblock-label! bblock))
    (let ((kernel
	   (lambda ()
	     (bblock-instructions bblock))))
      (if (bblock-label bblock)
	  (LAP ,(lap:make-label-statement (bblock-label bblock)) ,@(kernel))
	  (kernel))))

  (define (linearize-next bblock)
    (if (sblock? bblock)
	(let ((next (find-next (snode-next bblock))))
	  (if next
	      (linearize-sblock-next next (bblock-label next))
	      (let ((bblock (sblock-continuation bblock)))
		(if (and bblock (not (node-marked? bblock)))
		    (linearize-bblock bblock)
		    (LAP)))))
	(linearize-pblock
	 bblock
	 (find-next (pnode-consequent bblock))
	 (find-next (pnode-alternative bblock)))))

  (define (linearize-sblock-next bblock label)
    (if (node-marked? bblock)
	(LAP ,(lap:make-unconditional-branch label))
	(linearize-bblock bblock)))

  (define (linearize-pblock pblock cn an)
    (if (node-marked? cn)
	(if (node-marked? an)
	    (heed-preference pblock cn an
	      (lambda (generator cn an)
		(LAP ,@(generator (bblock-label cn))
		     ,(lap:make-unconditional-branch (bblock-label an)))))
	    (LAP ,@((pblock-consequent-lap-generator pblock)
		    (bblock-label cn))
		 ,@(linearize-bblock an)))
	(if (node-marked? an)
	    (LAP ,@((pblock-alternative-lap-generator pblock)
		    (bblock-label an))
		 ,@(linearize-bblock cn))
	    (linearize-pblock-1 pblock cn an))))

  (define (linearize-pblock-1 pblock cn an)
    (let ((finish
	   (lambda (generator cn an)
	     (let ((clabel (bblock-label! cn))
		   (alternative (linearize-bblock an)))
	       (LAP ,@(generator clabel)
		    ,@alternative
		    ,@(if (node-marked? cn)
			  (LAP)
			  (linearize-bblock cn)))))))
      (let ((consequent-first
	     (lambda ()
	       (finish (pblock-alternative-lap-generator pblock) an cn)))
	    (alternative-first
	     (lambda ()
	       (finish (pblock-consequent-lap-generator pblock) cn an)))
	    (unspecial
	     (lambda ()
	       (heed-preference pblock cn an finish)))
	    (diamond
	     (lambda ()
	       (let ((jlabel (generate-label)))
		 (heed-preference pblock cn an
		   (lambda (generator cn an)
		     (let ((clabel (bblock-label! cn)))
		       (let ((consequent (linearize-bblock-1 cn))
			     (alternative (linearize-bblock-1 an)))
			 (LAP ,@(generator clabel)
			      ,@alternative
			      ,(lap:make-unconditional-branch jlabel)
			      ,@consequent
			      ,(lap:make-label-statement jlabel)
			      ,@(linearize-next cn))))))))))
	(cond ((sblock? cn)
	       (let ((cnn (find-next (snode-next cn))))
		 (cond ((eq? cnn an)
			(consequent-first))
		       ((sblock? an)
			(let ((ann (find-next (snode-next an))))
			  (cond ((eq? ann cn)
				 (alternative-first))
				((not cnn)
				 (if ann
				     (consequent-first)
				     (if (null? (bblock-continuations cn))
					 (if (null? (bblock-continuations an))
					     (unspecial)
					     (consequent-first))
					 (if (null? (bblock-continuations an))
					     (alternative-first)
					     (unspecial)))))
				((not ann)
				 (alternative-first))
				((eq? cnn ann)
				 (diamond))
				(else
				 (unspecial)))))
		       ((not cnn)
			(consequent-first))
		       (else
			(unspecial)))))
	      ((and (sblock? an)
		    (let ((ann (find-next (snode-next an))))
		      (or (not ann)
			  (eq? ann cn))))
	       (alternative-first))
	      (else
	       (unspecial))))))

  (define (heed-preference pblock cn an finish)
    (if (eq? 'CONSEQUENT (pnode/preferred-branch pblock))
	(finish (pblock-alternative-lap-generator pblock) an cn)
	(finish (pblock-consequent-lap-generator pblock) cn an)))

  (define (find-next bblock)
    (let loop ((bblock bblock) (previous false))
      (cond ((not bblock)
	     previous)
	    ((and (sblock? bblock)
		  (null? (bblock-instructions bblock)))
	     (loop (snode-next bblock) bblock))
	    (else
	     bblock))))

  (linearize-bblock bblock))

(define linearize-lap
  (make-linearizer bblock-linearize-lap
    (lambda () (LAP))
    (lambda (x y) (LAP ,@x ,@y))
    identity-procedure))