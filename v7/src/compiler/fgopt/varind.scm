#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/varind.scm,v 1.1 1989/10/26 07:40:21 cph Exp $

Copyright (c) 1989 Massachusetts Institute of Technology

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

;;;; Variable Indirections

(declare (usual-integrations))

(define (initialize-variable-indirections! lvalues)
  (with-new-lvalue-marks
   (lambda ()
     (for-each (lambda (lvalue)
		 (if (lvalue/variable? lvalue)		     (initialize-variable-indirection! lvalue)))
	       lvalues))))

(define (initialize-variable-indirection! variable)
  (if (not (lvalue-marked? variable))
      (begin
	(lvalue-mark! variable)
	(let ((block (variable-block variable)))
	  (and (not (lvalue-known-value variable))
	       (null? (variable-assignments variable))
	       (not (lvalue/source? variable))
	       (not (block-passed-out? block))
	       (let ((indirection
		      (let ((possibility
			     (let ((links
				    (lvalue-initial-backward-links variable)))
			       (and (not (null? links))
				    (null? (cdr links))
				    (car links)))))
			(and possibility
			     (lvalue/variable? possibility)
			     (null? (variable-assignments possibility))			     (let ((block* (variable-block possibility)))
			       (and (not (block-passed-out? block*))
				    (block-ancestor? block block*)))
			     (begin
			       (initialize-variable-indirection! possibility)
			       (or (variable-indirection possibility)
				   possibility))))))
		 (if indirection
		     (begin
		       (set-variable-indirection! variable indirection)
		       (let ((variables
			      (block-variables-nontransitively-free block)))
			 (if (not (memq indirection variables))
			     (set-block-variables-nontransitively-free!
			      block
			      (cons indirection variables))))
		       (let ((block* (variable-block indirection)))
			 (let loop ((block block))
			   (let ((variables (block-free-variables block)))
			     (if (not (memq indirection variables))
				 (begin
				   (set-block-free-variables!
				    block
				    (cons indirection variables))
				   (let ((parent (block-parent block)))
				     (if (not (eq? parent block*))
					 (loop parent))))))))))))))))