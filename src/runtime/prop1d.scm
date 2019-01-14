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

(define-deferred population-of-1d-tables
  (make-serial-population/unsafe))

(add-boot-init!
 (lambda ()
   (add-secondary-gc-daemon!/unsafe clean-1d-tables!)))

(define (clean-1d-tables!)
  (for-each-inhabitant population-of-1d-tables 1d-table/clean!))

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

(define-integrable false-key
  '|#[1D table false]|)

(define-print-method 1d-table?
  (standard-print-method '1d-table))

(define-integrable (weak-cons car cdr)
  (system-pair-cons (ucode-type weak-cons) car cdr))

(define (weak-assq key table)
  (let loop ((previous table) (alist (cdr table)))
    (and (not (null? alist))
	 (let ((entry (car alist))
	       (next (cdr alist)))
	   (let ((key* (system-pair-car entry)))
	     (cond ((not key*)
		    (set-cdr! previous next)
		    (loop previous next))
		   ((eq? key* key)
		    entry)
		   (else
		    (loop alist next))))))))

(define (1d-table/get table key default)
  (let ((entry (weak-assq (or key false-key) table)))
    (if entry
	(system-pair-cdr entry)
	default)))

(define (1d-table/lookup table key if-found if-not-found)
  (let ((entry (weak-assq (or key false-key) table)))
    (if entry
	(if-found (system-pair-cdr entry))
	(if-not-found))))

(define (1d-table/put! table key value)
  (let ((key (or key false-key)))
    (let ((entry (weak-assq key table)))
      (if entry
	  (system-pair-set-cdr! entry value)
	  (set-cdr! table
		    (cons (weak-cons key value)
			  (cdr table))))
      unspecific)))

(define (1d-table/remove! table key)
  (let ((key (or key false-key)))
    (let loop ((previous table) (alist (cdr table)))
      (if (not (null? alist))
	  (let ((key* (system-pair-car (car alist)))
		(next (cdr alist)))
	    (loop (if (or (not key*) (eq? key* key))
		      ;; Might as well clean whole list.
		      (begin
			(set-cdr! previous next)
			previous)
		      alist)
		  next))))))

(define (1d-table/clean! table)
  (let loop ((previous table) (alist (cdr table)))
    (if (not (null? alist))
	(let ((next (cdr alist)))
	  (loop (if (system-pair-car (car alist))
		    alist
		    (begin
		      (set-cdr! previous next)
		      previous))
		next)))))

(define (1d-table/alist table)
  (let loop ((previous table) (alist (cdr table)) (result '()))
    (if (null? alist)
	result
	(let ((entry (car alist))
	      (next (cdr alist)))
	  (let ((key (system-pair-car entry)))
	    (if (not key)
		(begin
		  (set-cdr! previous next)
		  (loop previous next result))
		(loop alist
		      next
		      (cons (cons (and (not (eq? key false-key)) key)
				  (system-pair-cdr entry))
			    result))))))))

(define (1d-table/for-each proc table)
  (let loop ((previous table) (alist (cdr table)))
    (if (not (null? alist))
	(let ((entry (car alist))
	      (next (cdr alist)))
	  (let ((key (system-pair-car entry)))
	    (if key
		(begin
		  (proc (and (not (eq? key false-key)) key)
			(system-pair-cdr entry))
		  (loop alist next))
		(begin
		  (set-cdr! previous next)
		  (loop previous next))))))))