#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020 Massachusetts Institute of Technology

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
  (let ((table (%make-1d-table)))
    (add-new-to-population! population-of-1d-tables table)
    table))

(define (make-1d-table/unsafe)
  (let ((table (%make-1d-table)))
    (add-new-to-population!/unsafe population-of-1d-tables table)
    table))

(define (%make-1d-table)
  (%record 1d-table-tag (weak-alist-table eqv?)))

(define (1d-table? object)
  (and (%record? object)
       (eq? 1d-table-tag (%record-ref object 0))))
(register-predicate! 1d-table? '1d-table '<= %record?)

(define-integrable 1d-table-tag
  '|#[1D table]|)

(define-integrable (%table-items table) (%record-ref table 1))

(define-print-method 1d-table?
  (standard-print-method '1d-table))

(define (1d-table/get table key default)
  (guarantee 1d-table? table '1d-table/get)
  (weak-alist-table-ref (%table-items table) key (lambda () default)))

(define dummy-value (list 'dummy-value))

(define (1d-table/lookup table key if-found if-not-found)
  (let ((value (1d-table/get table key dummy-value)))
    (if (eq? dummy-value value)
	(if-not-found)
	(if-found value))))

(define (1d-table/put! table key value)
  (guarantee 1d-table? table '1d-table/put!)
  (weak-alist-table-set! (%table-items table) key value))

(define (1d-table/remove! table key)
  (guarantee 1d-table? table '1d-table/remove!)
  (weak-alist-table-delete! (%table-items table) key))

(define (1d-table/clean! table)
  (guarantee 1d-table? table '1d-table/clean!)
  (weak-alist-table-clean! (%table-items table)))

(define (1d-table/alist table)
  (guarantee 1d-table? table '1d-table/alist)
  (weak-alist-table->alist (%table-items table)))

(define (1d-table/for-each proc table)
  (for-each (lambda (p) (proc (car p) (cdr p)))
	    (1d-table/alist table)))