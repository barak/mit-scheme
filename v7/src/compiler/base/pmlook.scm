#| -*-Scheme-*-

$Id: pmlook.scm,v 1.11 2003/02/14 18:28:01 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

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
  (define (lookup-loop entries values bindings)
    (define (match pattern instance)
      (if (pair? pattern)
	  (if (eq? (car pattern) pattern-variable-tag)
	      (let ((entry (memq (cdr pattern) bindings)))
		(if (not entry)
		    (begin (set! bindings (cons (cdr pattern) bindings))
			   (set! values (cons instance values))
			   true)
		    (eqv? instance
			  (list-ref values (- (length bindings)
					      (length entry))))))
	      (and (pair? instance)
		   (match (car pattern) (car instance))
		   (match (cdr pattern) (cdr instance))))
	  (eqv? pattern instance)))

    (and (not (null? entries))
	 (or (and (match (caar entries) instance)
		  (pattern-lookup/bind (cdar entries) values))
	     (lookup-loop (cdr entries) '() '()))))
  (lookup-loop entries '() '()))

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