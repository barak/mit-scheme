#| -*-Scheme-*-

$Id: display.scm,v 1.9 2003/02/14 18:25:20 cph Exp $

Copyright 1989-1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

;;;; Display-Type Abstraction
;;; package: (edwin display-type)

(declare (usual-integrations))

(define-structure (display-type
		   (conc-name display-type/)
		   (constructor %make-display-type)
		   (print-procedure
		    (unparser/standard-method 'DISPLAY-TYPE
		      (lambda (state display-type)
			(unparse-object state
					(display-type/name display-type))))))
  (name false read-only true)
  (multiple-screens? false read-only true)
  (operation/available? false read-only true)
  (operation/make-screen false read-only true)
  (operation/get-input-operations false read-only true)
  (operation/with-display-grabbed false read-only true)
  (operation/with-interrupts-enabled false read-only true)
  (operation/with-interrupts-disabled false read-only true))

(define (make-display-type name
			   multiple-screens?
			   available?
			   make-screen
			   get-input-operations
			   with-display-grabbed
			   with-interrupts-enabled
			   with-interrupts-disabled)
  (let ((display-type
	 (%make-display-type name
			     multiple-screens?
			     available?
			     make-screen
			     get-input-operations
			     with-display-grabbed
			     with-interrupts-enabled
			     with-interrupts-disabled)))
    (set! display-types (cons display-type display-types))
    display-type))

(define display-types '())

(define (display-type/available? display-type)
  ((display-type/operation/available? display-type)))

(define (display-type/make-screen display-type args)
  (apply (display-type/operation/make-screen display-type) args))

(define (display-type/get-input-operations display-type screen)
  ((display-type/operation/get-input-operations display-type) screen))

(define (display-type/with-display-grabbed display-type thunk)
  ((display-type/operation/with-display-grabbed display-type) thunk))

(define (display-type/with-interrupts-enabled display-type thunk)
  ((display-type/operation/with-interrupts-enabled display-type) thunk))

(define (display-type/with-interrupts-disabled display-type thunk)
  ((display-type/operation/with-interrupts-disabled display-type) thunk))

(define (editor-display-types)
  (list-transform-positive display-types display-type/available?))

(define (name->display-type name)
  (let ((display-type
	 (list-search-positive display-types
	   (lambda (display-type)
	     (eq? name (display-type/name display-type))))))
    display-type))