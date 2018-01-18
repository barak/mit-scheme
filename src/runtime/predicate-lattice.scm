#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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

;;;; Predicates: lattice
;;; package: (runtime predicate-lattice)

(declare (usual-integrations))

(define (predicate<= predicate1 predicate2)
  (dispatch-tag<= (predicate->dispatch-tag predicate1)
		  (predicate->dispatch-tag predicate2)))

(define (predicate>= predicate1 predicate2)
  (predicate<= predicate2 predicate1))

(define (set-predicate<=! predicate superset)
  (set-dispatch-tag<=! (predicate->dispatch-tag predicate 'set-predicate<=!)
		       (predicate->dispatch-tag superset 'set-predicate<=!)))

(define (dispatch-tag= tag1 tag2)
  (guarantee dispatch-tag? tag1 'dispatch-tag=)
  (guarantee dispatch-tag? tag2 'dispatch-tag=)
  (eq? tag1 tag2))

(define (dispatch-tag<= tag1 tag2)
  (guarantee dispatch-tag? tag1 'dispatch-tag<=)
  (guarantee dispatch-tag? tag2 'dispatch-tag<=)
  (cached-dispatch-tag<= tag1 tag2))

(define (dispatch-tag>= tag1 tag2)
  (dispatch-tag<= tag2 tag1))

(define (cached-dispatch-tag<= tag1 tag2)
  (hash-table-intern! dispatch-tag<=-cache
		      (cons tag1 tag2)
		      (lambda () (uncached-dispatch-tag<= tag1 tag2))))

(define (uncached-dispatch-tag<= tag1 tag2)
  (or (eq? tag1 tag2)
      (dispatch-tag-is-bottom? tag1)
      (dispatch-tag-is-top? tag2)
      (and (not (dispatch-tag-is-top? tag1))
	   (not (dispatch-tag-is-bottom? tag2))
	   (let ((v
		  (find (lambda (v)
			  (and ((vector-ref v 0) tag1)
			       ((vector-ref v 1) tag2)))
			dispatch-tag<=-overrides)))
	     (if v
		 ((vector-ref v 2) tag1 tag2)
		 (any-dispatch-tag-superset (lambda (tag)
					      (cached-dispatch-tag<= tag tag2))
					    tag1))))))

(define (define-dispatch-tag<= test1 test2 handler)
  (set! dispatch-tag<=-overrides
	(cons (vector test1 test2 handler)
	      dispatch-tag<=-overrides))
  unspecific)

(define (any-object? object)
  (declare (ignore object))
  #t)

(define (no-object? object)
  (declare (ignore object))
  #f)

(define (top-dispatch-tag) the-top-dispatch-tag)
(define (bottom-dispatch-tag) the-bottom-dispatch-tag)

(define-integrable (dispatch-tag-is-top? tag)
  (eq? the-top-dispatch-tag tag))

(define-integrable (dispatch-tag-is-bottom? tag)
  (eq? the-bottom-dispatch-tag tag))

(define-deferred the-top-dispatch-tag
  (make-compound-tag any-object? 'conjoin '()))

(define-deferred the-bottom-dispatch-tag
  (make-compound-tag no-object? 'disjoin '()))

(define dispatch-tag<=-cache)
(define dispatch-tag<=-overrides)
(add-boot-init!
 (lambda ()
   ;; TODO(cph): should be a weak-key table, but we don't have tables that have
   ;; weak compound keys.
   (set! dispatch-tag<=-cache (make-equal-hash-table))
   (set! dispatch-tag<=-overrides '())
   (set! set-dispatch-tag<=!
	 (named-lambda (set-dispatch-tag<=! tag superset)
	   (if (not (add-dispatch-tag-superset tag superset))
	       (error "Tag already has this superset:" tag superset))
	   (if (dispatch-tag>= tag superset)
	       (error "Not allowed to create a superset loop:" tag superset))
	   (hash-table-clear! dispatch-tag<=-cache)))
   (run-deferred-boot-actions 'predicate-relations)))