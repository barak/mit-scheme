#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/reteqv.scm,v 1.1 1989/10/26 07:40:09 cph Rel $

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

;;;; Return Equivalencing

(declare (usual-integrations))

(define (find-equivalent-returns! lvalues applications)
  (for-each (lambda (application)
	      (if (application/return? application)
		  (set-return/equivalence-class! application '())))
	    applications)
  (for-each
   (lambda (return-class)
     (for-each
      (lambda (return)
	(set-return/equivalence-class! return return-class))
      return-class))
   (append-map
    (lambda (source)
      (list-transform-positive
	  (node-equivalence-classes
	   (gmap
	    (eq-set-adjoin
	     source
	     (list-transform-positive (lvalue-forward-links source)
	       lvalue/unique-source))
	    lvalue-applications
	    eq-set-union)
	   return=?)
	(lambda (class)
	  (not (null? (cdr class))))))
    (gmap (list-transform-positive lvalues continuation-variable?)
      lvalue/unique-source
      (lambda (source sources)
	(if (and source (not (memq source sources)))
	    (cons source sources)
	    sources))))))

(define (gmap items procedure adjoin)
  (let loop ((items items))
    (if (null? items)
	'()
	(adjoin (procedure (car items))
		(loop (cdr items))))))

(define (node-equivalence-classes nodes node=?)
  (with-new-node-marks
   (lambda ()
     (let ((classes '()))
       (for-each (lambda (node)
		   (if (not (node-marked? node))
		       (begin
			 (node-mark! node)
			 (let ((class
				(list-search-positive classes
				  (lambda (class)
				    (node=? node (car class))))))
			   (if class
			       (set-cdr! class (cons node (cdr class)))
			       (begin
				 (set! classes (cons (list node) classes))
				 unspecific))))))
		 nodes)
       classes))))

(define (return=? x y)
  (and (eq? (node/subgraph-color x) (node/subgraph-color y))
       (let ((operator-x (rvalue-known-value (return/operator x)))
	     (operator-y (rvalue-known-value (return/operator y)))
	     (operand=?
	      (lambda ()
		(let ((operand-x (rvalue-known-value (return/operand x))))
		  (and operand-x
		       (eq? operand-x
			    (rvalue-known-value (return/operand y))))))))
	 (if operator-x
	     (and (eq? operator-x operator-y)
		  (or (eq? continuation-type/effect
			   (continuation/type operator-x))
		      (operand=?)))
	     (and (not operator-y)
		  (operand=?))))
       (let ((x (application-context x))
	     (y (application-context y)))
	 (or (eq? x y)
	     (let ((x (reference-context/block x))
		   (y (reference-context/block y)))
	       (let ((limit (block-popping-limit x)))
		 (and (eq? limit (block-popping-limit y))
		      (let ((dx (distance-to x limit))
			    (dy (distance-to y limit)))
			(and dx dy (= dx dy))))))))))

(define (distance-to x limit)
  (let loop ((x x))
    (if (eq? x limit)
	(block-frame-size x)
	(let ((parent (block-parent x)))
	  (and (eq? parent (block-stack-link x))
	       (let ((rest (loop parent)))
		 (and rest
		      (+ rest (block-frame-size x)))))))))