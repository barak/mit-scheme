#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

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