#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/back/linear.scm,v 4.2 1988/06/14 08:10:23 cph Exp $

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

;;;; LAP linearizer

(declare (usual-integrations))

(define (bblock-linearize-bits bblock)
  (node-mark! bblock)
  (if (and (not (bblock-label bblock))
	   (node-previous>1? bblock))
      (bblock-label! bblock))
  (let ((kernel
	 (lambda ()
	   (LAP ,@(bblock-instructions bblock)
		,@(if (sblock? bblock)
		      (linearize-sblock-next (snode-next bblock))
		      (linearize-pblock bblock
					(pnode-consequent bblock)
					(pnode-alternative bblock)))))))
    (if (bblock-label bblock)
	(LAP ,(lap:make-label-statement (bblock-label bblock)) ,@(kernel))
	(kernel))))

(define (linearize-sblock-next bblock)
  (cond ((not bblock) (LAP))
	((node-marked? bblock)
	 (LAP ,(lap:make-unconditional-branch (bblock-label! bblock))))
	(else (bblock-linearize-bits bblock))))

(define (linearize-pblock pblock cn an)
  (if (node-marked? cn)
      (if (node-marked? an)
	  (LAP ,@((pblock-consequent-lap-generator pblock) (bblock-label! cn))
	       ,(lap:make-unconditional-branch (bblock-label! an)))
	  (LAP ,@((pblock-consequent-lap-generator pblock) (bblock-label! cn))
	       ,@(bblock-linearize-bits an)))
      (if (node-marked? an)
	  (LAP ,@((pblock-alternative-lap-generator pblock) (bblock-label! an))
	       ,@(bblock-linearize-bits cn))
	  (let ((label (bblock-label! cn))
		(alternative (bblock-linearize-bits an)))
	    (LAP ,@((pblock-consequent-lap-generator pblock) label)
		 ,@alternative
		 ,@(if (node-marked? cn)
		       (LAP)
		       (bblock-linearize-bits cn)))))))

(define (map-lap procedure objects)
  (let loop ((objects objects))
    (if (null? objects)
	(LAP)
	(LAP ,@(procedure (car objects))
	     ,@(loop (cdr objects))))))

(define linearize-bits
  (make-linearizer map-lap bblock-linearize-bits))