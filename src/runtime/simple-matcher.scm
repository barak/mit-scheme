#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; Simple pattern matcher
;;; package: (runtime simple-matcher)

;;; Derived from the matcher in "Software Design for Flexibility", by Chris
;;; Hanson and Gerald Jay Sussman.

;;; This matcher implements traditional list-based pattern matching with element
;;; variables, segment variables, and backward reference maching.  It has some
;;; additional features, including: support for explicit-renaming macros using a
;;; rename procedure; element-variable restrictions; and anonymous variables.
;;;
;;; Element-variable syntax:
;;;   (?)			anonymous, no restriction
;;;   (? <symbol>)		named, no restriction
;;;   (? <predicate>)		anonymous, restriction
;;;   (? <symbol> <predicate>)	named, restriction
;;;
;;; Segment-variable syntax:
;;;   (??)			anonymous
;;;   (?? <symbol>)		named

(declare (usual-integrations))

(define (make-simple-matcher pattern #!optional rename)
  (let ((rename
	 (if (default-object? rename)
	     (lambda (id) id)
	     (guarantee unary-procedure? rename 'make-simple-matcher))))
    (%make-simple-matcher (compile-pattern pattern rename)
			  (simple-matcher-pattern->names pattern))))

(define (apply-simple-matcher matcher datum #!optional succeed)
  ((simple-matcher-procedure matcher)
   (list datum)
   (new-dict)
   (lambda (tail dict)
     (and (null? tail)
	  (let ((vals (reverse (map binding-value (dict-bindings dict)))))
	    (if (default-object? succeed)
		vals
		(succeed vals)))))))

(define-record-type <simple-matcher>
    (%make-simple-matcher procedure names)
    simple-matcher?
  (procedure simple-matcher-procedure)
  (names simple-matcher-names))

(define (compile-pattern pattern rename)
  (let loop ((pattern pattern))
    (cond ((element-var? pattern)
	   (element-matcher (var-type pattern)
			    (var-name pattern)
			    (element-var-restriction pattern)))
	  ((segment-var? pattern)
	   (segment-matcher (var-type pattern)
			    (var-name pattern)))
	  ((list? pattern)
	   (list-matcher (map loop pattern)))
	  ((identifier? pattern)
	   (constant-matcher (rename pattern)))
	  ((constant? pattern)
	   (constant-matcher pattern))
	  (else
	   (error "Ill-formed pattern:" pattern)))))

(define (simple-matcher-pattern->names pattern)
  (reverse
   (let loop ((pattern pattern) (names '()))
     (cond ((or (element-var? pattern)
		(segment-var? pattern))
            (let ((name (var-name pattern)))
	      (if (and name (not (memq name names)))
		  (cons name names)
                  names)))
           ((list? pattern) (fold loop names pattern))
	   ((identifier? pattern) names)
	   ((constant? pattern) names)
	   (else (error "Ill-formed pattern:" pattern))))))

;;;; Pattern syntax

(define (element-var? pattern)
  (and (pair? pattern)
       (eq? '? (car pattern))
       (or (null? (cdr pattern))
	   (and (pair? (cdr pattern))
		(if (null? (cddr pattern))
		    (or (symbol? (cadr pattern))
			(unary-procedure? (cadr pattern)))
		    (and (pair? (cddr pattern))
			 (unary-procedure? (caddr pattern))
			 (null? (cdddr pattern))))))))

(define (segment-var? pattern)
  (and (pair? pattern)
       (eq? '?? (car pattern))
       (or (null? (cdr pattern))
	   (and (pair? (cdr pattern))
		(symbol? (cadr pattern))
		(null? (cddr pattern))))))

(define (var-type var)
  (car var))

(define (var-name var)
  (and (pair? (cdr var))
       (symbol? (cadr var))
       (cadr var)))

(define (element-var-restriction var)
  (and (pair? (cdr var))
       (if (unary-procedure? (cadr var))
	   (cadr var)
	   (and (pair? (cddr var))
		(caddr var)))))

(define (constant? pattern)
  (or (number? pattern)
      (boolean? pattern)
      (char? pattern)))

;;;; Match combinators

(define (constant-matcher constant)
  (define (match-constant data dict succeed)
    (and (pair? data)
         (eqv? (car data) constant)
         (succeed (cdr data) dict)))
  match-constant)

(define (element-matcher type name restriction)
  (define (match-element data dict succeed)
    (and (pair? data)
	 (if name
	     (let ((binding (dict-binding type name dict)))
	       (if binding
		   (and (equal? (binding-value binding) (car data))
			(succeed (cdr data) dict))
		   (and (or (not restriction)
			    (restriction (car data)))
			(succeed (cdr data)
				 (extend-dict type name (car data) dict)))))
	     (and (or (not restriction)
		      (restriction (car data)))
		  (succeed (cdr data) dict)))))
  match-element)

(define (segment-matcher type name)
  (define (match-segment data dict succeed)
    (and (list? data)
	 (if name
	     (let ((binding (dict-binding type name dict)))
	       (if binding
		   (let loop ((elts (binding-value binding)) (elts* data))
		     (if (pair? elts)
			 (and (pair? elts*)
			      (equal? (car elts) (car elts*))
			      (loop (cdr elts) (cdr elts*)))
			 (succeed elts* dict)))
		   (let loop ((tail data) (head '()))
		     (or (succeed tail
				  (extend-dict type name (reverse head) dict))
			 (and (pair? tail)
			      (loop (cdr tail) (cons (car tail) head)))))))
	     (let loop ((tail data))
	       (or (succeed tail dict)
		   (and (pair? tail)
			(loop (cdr tail))))))))
  match-segment)

(define (list-matcher matchers)
  (define (match-list data dict succeed)
    (and (pair? data)
	 (let ((datum (car data)))
	   (and (list? datum)
		(let loop ((matchers matchers) (elts datum) (dict dict))
		  (if (pair? matchers)
		      ((car matchers) elts dict
			(lambda (elts* new-dict)
			  (loop (cdr matchers)
				elts*
				new-dict)))
		      (and (null? elts)
			   (succeed (cdr data) dict))))))))
  match-list)

;;;; Dictionaries

(define-record-type <dict>
    (make-dict bindings)
    dict?
  (bindings dict-bindings))

(define (new-dict)
  (make-dict '()))

(define (extend-dict type name value dict)
  (make-dict
   (cons (make-binding type name value)
	 (dict-bindings dict))))

(define (dict-binding type name dict)
  (let ((binding
	 (find (lambda (binding)
		 (eq? name (binding-name binding)))
	       (dict-bindings dict))))
    (if (and binding
	     (not (eq? type (binding-type binding))))
	(error "Can't mix variable types:" name type (binding-type binding)))
    binding))

(define (all-dict-values dict)
  (reverse (map binding-value (dict-bindings dict))))

(define-record-type <binding>
    (make-binding type name value)
    binding?
  (type binding-type)
  (name binding-name)
  (value binding-value))