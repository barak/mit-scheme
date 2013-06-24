#| -*-Scheme-*-

$Id: tables.scm,v 4.3 1999/01/02 06:19:10 cph Exp $

Copyright (c) 1987, 1993, 1999 Massachusetts Institute of Technology

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