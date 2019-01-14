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

;;;; Populations
;;; package: (runtime population)

(declare (usual-integrations))

;;; A population is a weak collection of objects.  A serial
;;; population is a population with a mutex to serialize its operations.

(define-deferred population-of-populations
  (list population-tag (make-thread-mutex)))

(add-boot-init!
 (lambda ()
   (add-secondary-gc-daemon!/unsafe clean-all-populations!)))

(define-integrable population-tag
  '|#[population]|)

(define-integrable bogus-false
  '|#[population false]|)

(define-integrable (canonicalize object)
  (if (eq? object false) bogus-false object))

(define-integrable (uncanonicalize object)
  (if (eq? object bogus-false) false object))

(define (clean-population! population)
  (if (cadr population)
      (with-thread-mutex-lock (cadr population)
	(lambda ()
	  (%clean-population! population)))
      (%clean-population! population)))

(define (%clean-population! population)
  (let loop ((l1 (cdr population)) (l2 (cddr population)))
    (cond ((null? l2) true)
	  ((eq? (system-pair-car l2) false)
	   (system-pair-set-cdr! l1 (system-pair-cdr l2))
	   (loop l1 (system-pair-cdr l2)))
	  (else (loop l2 (system-pair-cdr l2))))))

(define (clean-all-populations!)
  (clean-population! population-of-populations)
  (map-over-population! population-of-populations clean-population!))

(define (make-population)
  (let ((population (list population-tag #f)))
    (add-to-population! population-of-populations population)
    population))

(define (make-population/unsafe)
  (let ((population (list population-tag #f)))
    (add-to-population!/unsafe population-of-populations population)
    population))

(define (make-serial-population)
  (let ((population (list population-tag (make-thread-mutex))))
    (add-to-population! population-of-populations population)
    population))

(define (make-serial-population/unsafe)
  (let ((population (list population-tag (make-thread-mutex))))
    (add-to-population!/unsafe population-of-populations population)
    population))

(define (population? object)
  (and (pair? object)
       (eq? (car object) population-tag)))

(define-print-method population?
  (standard-print-method 'population))

(define-guarantee population "population")

(define (add-to-population! population object)
  (guarantee-population population 'add-to-population!)
  (if (cadr population)
      (with-thread-mutex-lock (cadr population)
	(lambda ()
	  (%add-to-population! population object)))
      (%add-to-population! population object)))

(define (%add-to-population! population object)
  (let ((object (canonicalize object)))
    (let loop ((previous (cdr population)) (this (cddr population)))
      (if (null? this)
	  (set-cdr! (cdr population)
		    (weak-cons object (cddr population)))
	  (let ((entry (system-pair-car this))
		(next (system-pair-cdr this)))
	    (cond ((not entry)
		   (system-pair-set-cdr! previous next)
		   (loop previous next))
		  ((not (eq? object entry))
		   (loop this next))))))))

(define (add-to-population!/unsafe population object)
  ;; No canonicalization, no uniquification, no locking.
  (set-cdr! (cdr population) (weak-cons object (cddr population))))

(define (remove-from-population! population object)
  (guarantee-population population 'remove-from-population!)
  (if (cadr population)
      (with-thread-mutex-lock (cadr population)
	(lambda ()
	  (%remove-from-population! population object)))
      (%remove-from-population! population object)))

(define (%remove-from-population! population object)
  (let ((object (canonicalize object)))
    (let loop ((previous (cdr population)) (this (cddr population)))
      (if (not (null? this))
	  (let ((entry (system-pair-car this))
		(next (system-pair-cdr this)))
	    (if (or (not entry) (eq? object entry))
		(begin (system-pair-set-cdr! previous next)
		       (loop previous next))
		(loop this next)))))))

(define (empty-population! population)
  (guarantee-population population 'empty-population!)
  (if (cadr population)
      (with-thread-mutex-lock (cadr population)
	(lambda ()
	  (%empty-population! population)))
      (%empty-population! population)))

(define (%empty-population! population)
  (set-cdr! (cdr population) '()))

;;;; Read-only operations

;;; These are safe without serialization.

(define (map-over-population population procedure)
  (let loop ((l2 (cddr population)))
    (cond ((null? l2) '())
	  ((eq? (system-pair-car l2) false)
	   (loop (system-pair-cdr l2)))
	  (else
	   (cons (procedure (uncanonicalize (system-pair-car l2)))
		 (loop (system-pair-cdr l2)))))))

(define (map-over-population! population procedure)
  (let loop ((l2 (cddr population)))
    (cond ((null? l2) true)
	  ((eq? (system-pair-car l2) false)
	   (loop (system-pair-cdr l2)))
	  (else
	   (procedure (uncanonicalize (system-pair-car l2)))
	   (loop (system-pair-cdr l2))))))

(define (for-all-inhabitants? population predicate)
  (let loop ((l2 (cddr population)))
    (or (null? l2)
	(if (eq? (system-pair-car l2) false)
	    (loop (system-pair-cdr l2))
	    (and (predicate (uncanonicalize (system-pair-car l2)))
		 (loop (system-pair-cdr l2)))))))

(define (exists-an-inhabitant? population predicate)
  (let loop ((l2 (cddr population)))
    (and (not (null? l2))
	 (if (eq? (system-pair-car l2) false)
	     (loop (system-pair-cdr l2))
	     (or (predicate (uncanonicalize (system-pair-car l2)))
		 (loop (system-pair-cdr l2)))))))