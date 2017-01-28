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
  (tag<= (predicate->tag predicate1)
         (predicate->tag predicate2)))

(define (predicate>= predicate1 predicate2)
  (predicate<= predicate2 predicate1))

(define (tag= tag1 tag2)
  (guarantee tag? tag1 'tag=)
  (guarantee tag? tag2 'tag=)
  (eq? tag1 tag2))

(define (tag<= tag1 tag2)
  (guarantee tag? tag1 'tag<=)
  (guarantee tag? tag2 'tag<=)
  (cached-tag<= tag1 tag2))

(define (tag>= tag1 tag2)
  (tag<= tag2 tag1))

(define (cached-tag<= tag1 tag2)
  (hash-table-intern! tag<=-cache
		      (cons tag1 tag2)
		      (lambda () (uncached-tag<= tag1 tag2))))

(define (uncached-tag<= tag1 tag2)
  (or (eqv? tag1 tag2)
      ((get-override-handler tag1 tag2) tag1 tag2)
      (any (lambda (tag)
             (cached-tag<= tag tag2))
           (get-tag-supersets tag1))))

(define (metadata-event! operator tag . rest)
  (if (and (eq? operator 'set-tag<=!)
           (pair? rest))
      (let ((superset (car rest)))
        (if (tag<= tag superset)
            (error "Tag already has this superset:" tag superset))
        (if (tag>= tag superset)
            (error "Not allowed to create a superset loop:" tag superset))))
  (hash-table-clear! tag<=-cache))

(define (get-override-handler tag1 tag2)
  (let ((p
	 (find (lambda (p)
		 (and ((caar p) tag1)
		      ((cdar p) tag2)))
	       tag<=-overrides)))
    (if p
	(cdr p)
	false-tag<=)))

(define (define-tag<= predicate1 predicate2 handler)
  (let ((p
	 (find (lambda (p)
		 (and (eqv? (caar p) predicate1)
		      (eqv? (cdar p) predicate2)))
	       tag<=-overrides)))
    (if p
	(if (not (eqv? (cdr p) handler))
	    (error "Can't redefine tag<= override:" predicate1 predicate2))
	(begin
	  (set! tag<=-overrides
		(cons (cons (cons predicate1 predicate2) handler)
		      tag<=-overrides))
	  unspecific))))

(define (false-tag<= tag1 tag2) tag1 tag2 #f)
(define (true-tag<= tag1 tag2) tag1 tag2 #t)

(define tag<=-cache)
(define tag<=-overrides)
(add-boot-init!
 (lambda ()
   (set! tag<=-cache (make-equal-hash-table))
   (set! tag<=-overrides '())
   (add-event-receiver! event:predicate-metadata metadata-event!)

   (define-tag<= bottom-tag? tag? true-tag<=)
   (define-tag<= tag? top-tag? true-tag<=)

   (define-tag<= non-bottom-tag? bottom-tag? false-tag<=)
   (define-tag<= top-tag? non-top-tag? false-tag<=)))