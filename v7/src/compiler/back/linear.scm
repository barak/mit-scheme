#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/back/linear.scm,v 4.7 1988/11/06 14:50:00 cph Rel $

Copyright (c) 1987, 1988 Massachusetts Institute of Technology

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

(define (bblock-linearize-bits bblock queue-continuations!)
  (define (linearize-bblock bblock)
    (node-mark! bblock)
    (queue-continuations! bblock)
    (if (and (not (bblock-label bblock))
	     (let ((edges (node-previous-edges bblock)))
	       (and (not (null? edges))
		    (not (null? (cdr edges))))))	(bblock-label! bblock))
    (let ((kernel
	   (lambda ()
	     (LAP ,@(bblock-instructions bblock)
		  ,@(if (sblock? bblock)
			(let ((next (snode-next bblock)))
			  (if next
			      (linearize-sblock-next next (bblock-label next))
			      (let ((bblock (sblock-continuation bblock)))
				(if (and bblock (not (node-marked? bblock)))
				    (linearize-bblock bblock)
				    (LAP)))))
			(linearize-pblock bblock
					  (pnode-consequent bblock)
					  (pnode-alternative bblock)))))))
      (if (bblock-label bblock)
	  (LAP ,(lap:make-label-statement (bblock-label bblock)) ,@(kernel))
	  (kernel))))

  (define (linearize-sblock-next bblock label)
    (if (node-marked? bblock)
	(LAP ,(lap:make-unconditional-branch label))
	(linearize-bblock bblock)))

  (define (linearize-pblock pblock cn an)
    (let ((heed-preference
	   (lambda (finish)
	     (if (eq? 'CONSEQUENT (pnode/preferred-branch pblock))
		 (finish (pblock-alternative-lap-generator pblock) an cn)
		 (finish (pblock-consequent-lap-generator pblock) cn an)))))
      (if (node-marked? cn)
	  (if (node-marked? an)
	      (heed-preference
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
	      (heed-preference
	       (lambda (generator cn an)
		 (let ((clabel (bblock-label! cn))
		       (alternative (linearize-bblock an)))
		   (LAP ,@(generator clabel)
			,@alternative
			,@(if (node-marked? cn)
			      (LAP)
			      (linearize-bblock cn))))))))))

  (linearize-bblock bblock))

(define linearize-bits
  (make-linearizer bblock-linearize-bits
    (lambda () (LAP))
    (lambda (x y) (LAP ,@x ,@y))
    identity-procedure))