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

;;;; Boolean Operations
;;; package: (runtime boolean)

(declare (usual-integrations))

(define-integrable (not object)
  ((ucode-primitive not) object))

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
  (let loop ((items* items))
    (if (pair? items*)
	(if (predicate (car items*))
	    #t
	    (loop (cdr items*)))
	(begin
	  (if (not (null? items*))
	      (error:not-list items 'THERE-EXISTS?))
	  #f))))

(define (for-all? items predicate)
  (let loop ((items* items))
    (if (pair? items*)
	(if (predicate (car items*))
	    (loop (cdr items*))
	    #f)
	(begin
	  (if (not (null? items*))
	      (error:not-list items 'FOR-ALL?))
	  #t))))