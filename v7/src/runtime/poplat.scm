#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/poplat.scm,v 14.2 1988/06/13 11:49:48 cph Rel $

Copyright (c) 1988 Massachusetts Institute of Technology

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
				    (unparser/standard-method 'POPULATION)))

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