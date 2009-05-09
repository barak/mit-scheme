#| -*-Scheme-*-

$Id: varind.scm,v 1.10 2008/01/30 20:01:45 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; Variable Indirections
;;; package: (compiler fg-optimizer variable-indirection)

(declare (usual-integrations))

(define (initialize-variable-indirections! lvalues)
  (with-new-lvalue-marks
   (lambda ()
     (for-each (lambda (lvalue)
		 (if (and (lvalue/variable? lvalue)
			  (not (variable/continuation-variable? lvalue))
			  (not (variable/value-variable? lvalue)))
		     (initialize-variable-indirection! lvalue)))
	       lvalues))))

(define (initialize-variable-indirection! variable)
  (if (and (not (lvalue-marked? variable))
	   (not (variable-indirection variable)))
      (begin
	(lvalue-mark! variable)
	(let ((block (variable-block variable)))
	  (and (not (lvalue-known-value variable))
	       (null? (variable-assignments variable))
	       (not (variable-closed-over? variable))
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
			     (null? (variable-assignments possibility))
			     (not (variable-closed-over? possibility))
			     (let ((block* (variable-block possibility)))
			       (and (not (block-passed-out? block*))
				    (block-ancestor? block block*)))
			     (begin
			       (initialize-variable-indirection! possibility)
			       (or (variable-indirection possibility)
				   (cons possibility false)))))))
		 (if indirection
		     (let ((indirection-variable (car indirection)))
		       (set-variable-indirection! variable indirection)
		       (let ((variables
			      (block-variables-nontransitively-free block)))
			 (if (not (memq indirection-variable variables))
			     (set-block-variables-nontransitively-free!
			      block
			      (cons indirection-variable variables))))
		       (let ((block* (variable-block indirection-variable)))
			 (let loop ((block block))
			   (let ((variables (block-free-variables block)))
			     (if (not (memq indirection-variable variables))
				 (begin
				   (set-block-free-variables!
				    block
				    (cons indirection-variable variables))
				   (let ((parent (block-parent block)))
				     (if (not (eq? parent block*))
					 (loop parent))))))))))))))))