#| -*-Scheme-*-

$Id: boole.scm,v 14.3 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1988, 1999 Massachusetts Institute of Technology

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
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; Boolean Operations
;;; package: ()

(declare (usual-integrations))

(define-primitives not (false? not))

(define false #F)
(define true #T)

(define (boolean? object)
  (or (eq? object #F)
      (eq? object #T)))

(define (boolean=? x y)
  (if x y (not y)))

(define (boolean/or . arguments)
  (let loop ((arguments arguments))
    (cond ((null? arguments) false)
	  ((car arguments) true)
	  (else (loop (cdr arguments))))))

(define (boolean/and . arguments)
  (let loop ((arguments arguments))
    (cond ((null? arguments) true)
	  ((car arguments) (loop (cdr arguments)))
	  (else false))))

(define (there-exists? items predicate)
  (let loop ((items items))
    (and (not (null? items))
	 (or (predicate (car items))
	     (loop (cdr items))))))

(define (for-all? items predicate)
  (let loop ((items items))
    (or (null? items)
	(and (predicate (car items))
	     (loop (cdr items))))))