#| -*-Scheme-*-

$Id$

Copyright (c) 1988-1999 Massachusetts Institute of Technology

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

;;;; Very Simple Pattern Matcher: Lookup
;;; package: (compiler pattern-matcher/lookup)

(declare (usual-integrations))

(define pattern-variable-tag
  (intern "#[(compiler pattern-matcher/lookup)pattern-variable]"))

;;; PATTERN-LOOKUP returns either false or a pair whose car is the
;;; item matched and whose cdr is the list of variable values.  Use
;;; PATTERN-VARIABLES to get a list of names that is in the same order
;;; as the list of values.

(define (pattern-lookup entries instance)
  (define (lookup-loop entries)
    (and (not (null? entries))
	 (or ((cdar entries) instance)
	     (lookup-loop (cdr entries)))))
  (lookup-loop entries))

(define-integrable (pattern-lookup/bind binder values)
  (apply binder values))

(define (pattern-variables pattern)
  (let ((variables '()))
    (define (loop pattern)
      (if (pair? pattern)
	  (if (eq? (car pattern) pattern-variable-tag)
	      (if (not (memq (cdr pattern) variables))
		  (set! variables (cons (cdr pattern) variables)))
	      (begin (loop (car pattern))
		     (loop (cdr pattern))))))
    (loop pattern)
    variables))

(define-integrable (make-pattern-variable name)
  (cons pattern-variable-tag name))

(define (pattern-variable? object)
  (and (pair? object)
       (eq? (car object) pattern-variable-tag)))

(define-integrable (pattern-variable-name var)
  (cdr var))
