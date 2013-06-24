#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

;;;; Populations
;;; package: (runtime population)

(declare (usual-integrations))

;;; A population is a collection of objects.  This collection has the
;;; property that if one of the objects in the collection is reclaimed
;;; as garbage, then it is no longer an element of the collection.

(define (initialize-package!)
  (set! population-of-populations (cons population-tag '()))
  (add-secondary-gc-daemon! gc-all-populations!))

(define (initialize-unparser!)
  (unparser/set-tagged-pair-method! population-tag
				    (standard-unparser-method 'POPULATION #f)))

(define bogus-false '(BOGUS-FALSE))
(define population-tag '(POPULATION))
(define-integrable weak-cons-type (ucode-type weak-cons))

(define-integrable (canonicalize object)
  (if (eq? object false) bogus-false object))

(define-integrable (uncanonicalize object)
  (if (eq? object bogus-false) false object))

(define (gc-population! population)
  (let loop ((l1 population) (l2 (cdr population)))
    (cond ((null? l2) true)
	  ((eq? (system-pair-car l2) false)
	   (system-pair-set-cdr! l1 (system-pair-cdr l2))
	   (loop l1 (system-pair-cdr l1)))
	  (else (loop l2 (system-pair-cdr l2))))))

(define (gc-all-populations!)
  (gc-population! population-of-populations)
  (map-over-population! population-of-populations gc-population!))

(define population-of-populations)

(define (make-population)
  (let ((population (cons population-tag '())))
    (add-to-population! population-of-populations population)
    population))

(define (population? object)
  (and (pair? object)
       (eq? (car object) population-tag)))

(define (add-to-population! population object)
  (let ((object (canonicalize object)))
    (let loop ((previous population) (this (cdr population)))
      (if (null? this)
	  (set-cdr! population
		    (system-pair-cons weak-cons-type
				      object
				      (cdr population)))
	  (let ((entry (system-pair-car this))
		(next (system-pair-cdr this)))
	    (cond ((not entry)
		   (system-pair-set-cdr! previous next)
		   (loop previous next))
		  ((not (eq? object entry))
		   (loop this next))))))))

(define (remove-from-population! population object)
  (let ((object (canonicalize object)))
    (let loop ((previous population) (this (cdr population)))
      (if (not (null? this))
	  (let ((entry (system-pair-car this))
		(next (system-pair-cdr this)))
	    (if (or (not entry) (eq? object entry))
		(begin (system-pair-set-cdr! previous next)
		       (loop previous next))
		(loop this next)))))))

;;;; Higher level operations

(define (map-over-population population procedure)
  (let loop ((l1 population) (l2 (cdr population)))
    (cond ((null? l2) '())
	  ((eq? (system-pair-car l2) false)
	   (system-pair-set-cdr! l1 (system-pair-cdr l2))
	   (loop l1 (system-pair-cdr l1)))
	  (else
	   (cons (procedure (uncanonicalize (system-pair-car l2)))
		 (loop l2 (system-pair-cdr l2)))))))

(define (map-over-population! population procedure)
  (let loop ((l1 population) (l2 (cdr population)))
    (cond ((null? l2) true)
	  ((eq? (system-pair-car l2) false)
	   (system-pair-set-cdr! l1 (system-pair-cdr l2))
	   (loop l1 (system-pair-cdr l1)))
	  (else
	   (procedure (uncanonicalize (system-pair-car l2)))
	   (loop l2 (system-pair-cdr l2))))))

(define (for-all-inhabitants? population predicate)
  (let loop ((l1 population) (l2 (cdr population)))
    (or (null? l2)
	(if (eq? (system-pair-car l2) false)
	    (begin (system-pair-set-cdr! l1 (system-pair-cdr l2))
		   (loop l1 (system-pair-cdr l1)))
	    (and (predicate (uncanonicalize (system-pair-car l2)))
		 (loop l2 (system-pair-cdr l2)))))))

(define (exists-an-inhabitant? population predicate)
  (let loop ((l1 population) (l2 (cdr population)))
    (and (not (null? l2))
	 (if (eq? (system-pair-car l2) false)
	     (begin (system-pair-set-cdr! l1 (system-pair-cdr l2))
		    (loop l1 (system-pair-cdr l1)))
	     (or (predicate (uncanonicalize (system-pair-car l2)))
		 (loop l2 (system-pair-cdr l2)))))))