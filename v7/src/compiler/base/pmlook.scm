#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/pmlook.scm,v 1.7 1989/04/15 18:06:14 cph Rel $

Copyright (c) 1988, 1989 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; Very Simple Pattern Matcher: Lookup

(declare (usual-integrations))

(define pattern-variable-tag
  (intern "#[(compiler pattern-matcher/lookup)pattern-variable]"))

;;; PATTERN-LOOKUP returns either false or a pair whose car is the
;;; item matched and whose cdr is the list of variable values.  Use
;;; PATTERN-VARIABLES to get a list of names that is in the same order
;;; as the list of values.

(define (pattern-lookup entries instance)
  (define (lookup-loop entries values)
    (define (match pattern instance)
      (if (pair? pattern)
	  (if (eq? (car pattern) pattern-variable-tag)
	      (let ((entry (memq (cdr pattern) values)))
		(if entry
		    (eqv? (cdr entry) instance)
		    (begin (set! values (cons instance values))
			   true)))
	      (and (pair? instance)
		   (match (car pattern) (car instance))
		   (match (cdr pattern) (cdr instance))))
	  (eqv? pattern instance)))
    (and (not (null? entries))
	 (or (and (match (caar entries) instance)
		  (pattern-lookup/bind (cdar entries) values))
	     (lookup-loop (cdr entries) '()))))
  (lookup-loop entries '()))

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