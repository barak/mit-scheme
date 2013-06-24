#| -*-Scheme-*-

$Id: boole.scm,v 14.4 2001/12/18 18:39:22 cph Exp $

Copyright (c) 1988, 1999, 2001 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.
|#

;;;; Boolean Operations
;;; package: (runtime boolean)

(declare (usual-integrations))

(define-primitives not (false? not))

(define false #f)
(define true #t)

(define (boolean? object)
  (or (eq? object #f)
      (eq? object #t)))

(define (boolean=? x y)
  (if x y (not y)))

(define (boolean/or . arguments)
  (let loop ((arguments arguments))
    (if (pair? arguments)
	(if (car arguments)
	    #t
	    (loop (cdr arguments)))
	#f)))

(define (boolean/and . arguments)
  (let loop ((arguments arguments))
    (if (pair? arguments)
	(if (car arguments)
	    (loop (cdr arguments))
	    #f)
	#t)))

(define (there-exists? items predicate)
  (let loop ((items items))
    (if (pair? items)
	(or (predicate (car items))
	    (loop (cdr items)))
	#f)))

(define (for-all? items predicate)
  (let loop ((items items))
    (if (pair? items)
	(if (predicate (car items))
	    (loop (cdr items))
	    #f)
	#t)))