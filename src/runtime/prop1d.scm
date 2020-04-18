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

;;;; One Dimensional Property Tables
;;; package: (runtime 1d-property)

(declare (usual-integrations))

(define population-of-1d-tables
  (make-serial-population/unsafe))

(define (clean-1d-tables!)
  (for-each-inhabitant population-of-1d-tables 1d-table/clean!))

(add-boot-init!
 (lambda ()
   (add-secondary-gc-daemon!/unsafe clean-1d-tables!)))

(define (make-1d-table)
  (let ((table (list 1d-table-tag)))
    (add-to-population! population-of-1d-tables table)
    table))

(define (make-1d-table/unsafe)
  (let ((table (list 1d-table-tag)))
    (add-to-population!/unsafe population-of-1d-tables table)
    table))

(define (1d-table? object)
  (and (pair? object)
       (eq? (car object) 1d-table-tag)))

(define-integrable 1d-table-tag
  '|#[1D table]|)

(define-print-method 1d-table?
  (standard-print-method '1d-table))

(define (get-table-alist table)
  (let loop ((alist (cdr table)))
    (if (and (pair? alist)
	     (gc-reclaimed-object? (weak-car (car alist))))
	(let ((next (cdr alist)))
	  (set-cdr! table next)
	  (loop next))
	alist)))

(define (1d-table/get table key default)
  (let ((p (weak-assv key (get-table-alist table))))
    (if p
	(weak-cdr p)
	default)))

(define (1d-table/lookup table key if-found if-not-found)
  (let ((p (weak-assv key (get-table-alist table))))
    (if p
	(if-found (weak-cdr p))
	(if-not-found))))

(define (1d-table/put! table key value)
  (let ((p (weak-assv key (get-table-alist table))))
    (if p
	(weak-set-cdr! p value)
	(set-cdr! table
		  (cons (weak-cons key value)
			(cdr table))))))

(define (1d-table/remove! table key)
  (set-cdr! table
	    (remove! (lambda (p)
		       (let ((key* (weak-car p)))
			 (or (gc-reclaimed-object? key*)
			     (eqv? key* key))))
		     (cdr table))))

(define (1d-table/clean! table)
  (set-cdr! table
	    (remove! (lambda (p)
		       (gc-reclaimed-object? (weak-car p)))
		     (cdr table))))

(define (1d-table/alist table)
  (fold (lambda (p acc)
	  (let ((key (weak-car p)))
	    (if (gc-reclaimed-object? key)
		acc
		(cons (cons key (weak-cdr p)) acc))))
	'()
	(cdr table)))

(define (1d-table/for-each proc table)
  (let loop ((alist (cdr table)) (prev table))
    (if (pair? alist)
	(let ((p (car alist))
	      (next (cdr alist)))
	  (let ((key (weak-car p)))
	    (if (gc-reclaimed-object? key)
		(begin
		  (set-cdr! prev next)
		  (loop next prev))
		(begin
		  (proc key (weak-cdr p))
		  (loop next alist))))))))