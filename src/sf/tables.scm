#| -*-Scheme-*-

$Id: 4b87568f70d3e1212459c1958888f6aca7a21ed8 $

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

;;;; SCode Optimizer: Tables

(declare (usual-integrations)
	 (integrate-external "object"))

;;;; Operations

(define (operations/make)
  (cons '() '()))

(define (operations/lookup operations variable if-found if-not)
  (let ((entry (assq variable (car operations))))
    (if entry
	(if (cdr entry)
	    (if-found (cadr entry) (cddr entry))
	    (if-not))
	(let ((entry (assq variable (cdr operations))))
	  (if entry
	      (if-found (cadr entry) (cddr entry))
	      (if-not))))))

(define (operations/shadow operations variables)
  (cons (map* (car operations)
	      (lambda (variable) (cons variable false))
	      variables)
	(cdr operations)))

(define (operations/bind operations operation variable value)
  (cons (cons (cons* variable operation value)
	      (car operations))
	(cdr operations)))

(define (operations/bind-global operations operation variable value)
  (cons (car operations)
	(cons (cons* variable operation value)
	      (cdr operations))))

(define (operations/map-external operations procedure)
  (let loop ((elements (car operations)))
    (cond ((null? elements)
	   '())
	  ((cdar elements)
	   (cons (procedure (cadar elements) (caar elements) (cddar elements))
		 (loop (cdr elements))))
	  (else
	   (loop (cdr elements))))))