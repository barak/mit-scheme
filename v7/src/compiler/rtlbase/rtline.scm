#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlbase/rtline.scm,v 4.1 1987/12/04 20:18:04 cph Exp $

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

;;;; Linearizer for CFG

(declare (usual-integrations))

;;; The linearizer attaches labels to nodes under two conditions.  The
;;; first is that the node in question has more than one previous
;;; neighboring node.  The other is when a conditional branch requires
;;; such a label.  It is assumed that if one encounters a node that
;;; has already been linearized, that it has a label, since this
;;; implies that it has more than one previous neighbor.

;;;; RTL linearizer

(package (bblock-linearize-rtl)

(define-export (bblock-linearize-rtl bblock)
  (node-mark! bblock)
  (if (and (not (bblock-label bblock))
	   (node-previous>1? bblock))
      (bblock-label! bblock))
  (let ((kernel
	 (lambda ()
	   (let loop ((rinst (bblock-instructions bblock)))
	     (cond ((rinst-next rinst)
		    (cons (rinst-rtl rinst)
			  (loop (rinst-next rinst))))
		   ((sblock? bblock)
		    (cons (rinst-rtl rinst)
			  (linearize-sblock-next (snode-next bblock))))
		   (else
		    (linearize-pblock bblock
				      (rinst-rtl rinst)
				      (pnode-consequent bblock)
				      (pnode-alternative bblock))))))))
    (if (bblock-label bblock)
	`(,(rtl:make-label-statement (bblock-label bblock)) ,@(kernel))
	(kernel))))

(define (linearize-sblock-next bblock)
  (cond ((not bblock) '())
	((node-marked? bblock)
	 `(,(rtl:make-jump-statement (bblock-label! bblock))))
	(else (bblock-linearize-rtl bblock))))

(define (linearize-pblock pblock predicate cn an)
  (if (node-marked? cn)
      (if (node-marked? an)
	  `(,(rtl:make-jumpc-statement predicate (bblock-label! cn))
	    ,(rtl:make-jump-statement (bblock-label! an)))
	  `(,(rtl:make-jumpc-statement predicate (bblock-label! cn))
	    ,@(bblock-linearize-rtl an)))
      (if (node-marked? an)
	  `(,(rtl:make-jumpc-statement (rtl:negate-predicate predicate)
				       (bblock-label! an))
	    ,@(bblock-linearize-rtl cn))
	  (let ((label (bblock-label! cn))
		(alternative (bblock-linearize-rtl an)))
	    `(,(rtl:make-jumpc-statement predicate label)
	      ,@alternative
	      ,@(if (node-marked? cn)
		    '()
		    (bblock-linearize-rtl cn)))))))

)

;;;; Linearizers

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

(define linearize-rtl
  (make-linearizer mapcan bblock-linearize-rtl))