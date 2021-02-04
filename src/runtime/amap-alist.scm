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

;;;; Association-list amap type
;;; package: (runtime amap alist)

(declare (usual-integrations))

(add-boot-deps! '(runtime amap impl))

(add-boot-init!
 (lambda ()
   (define-amap-implementation-selector 'alist
     '((mutability mutable)
       (kv-types (strong strong) (weak strong))
       (time-complexity linear))
     #f
     (lambda (comparator args)
       (declare (ignore comparator))
       (if (memq 'weak-keys args)
	   weak-impl
	   strong-impl)))))

(define-deferred strong-impl
  (make-amap-implementation 'strong-alist
    '((mutability mutable)
      (kv-types (strong strong))
      (time-complexity linear))
    #f
    `((->alist ,alist-table->alist)
      (clear! ,alist-table-clear!)
      (contains? ,alist-table-contains?)
      (delete-1! ,alist-table:delete-1!)
      (empty-copy ,alist-table:empty-copy)
      (empty? ,alist-table-empty?)
      (find ,alist-table:find)
      (fold ,alist-table-fold)
      (intern! ,alist-table-intern!)
      (keys ,alist-table-keys)
      (map! ,alist-table-map!)
      (new-state ,alist-table:new-state)
      (prune! ,alist-table-prune!)
      (ref ,alist-table-ref)
      (set-1! ,alist-table-set!)
      (size ,alist-table-size)
      (update! ,alist-table-update!)
      (values ,alist-table-values))))

(define (alist-table:new-state comparator args)
  (declare (ignore args))
  (alist-table (comparator-equality-predicate comparator)))

(define (alist-table:delete-1! table key)
  (let ((default (list 'default)))
    (not (eq? default (alist-table-delete! table key default)))))

(define (alist-table:empty-copy table)
  (alist-table (alist-table-key= table)))

(define (alist-table:find predicate table fail)
  (alist-table-search table predicate
		      (lambda (key value)
			(declare (ignore key))
			value)
		      fail))

(define-deferred weak-impl
  (make-amap-implementation 'weak-alist
    '((mutability mutable)
      (kv-types (weak strong))
      (time-complexity linear))
    #f
    `((->alist ,weak-alist-table->alist)
      (clean! ,weak-alist-table-clean!)
      (clear! ,weak-alist-table-clear!)
      (contains? ,weak-alist-table-contains?)
      (delete-1! ,weak-alist-table:delete-1!)
      (empty-copy ,weak-alist-table:empty-copy)
      (empty? ,weak-alist-table-empty?)
      (find ,weak-alist-table:find)
      (fold ,weak-alist-table-fold)
      (intern! ,weak-alist-table-intern!)
      (keys ,weak-alist-table-keys)
      (map! ,weak-alist-table-map!)
      (new-state ,weak-alist-table:new-state)
      (prune! ,weak-alist-table-prune!)
      (ref ,weak-alist-table-ref)
      (set-1! ,weak-alist-table-set!)
      (size ,weak-alist-table-size)
      (update! ,weak-alist-table-update!)
      (values ,weak-alist-table-values))))

(define (weak-alist-table:new-state comparator args)
  (declare (ignore args))
  (weak-alist-table (comparator-equality-predicate comparator)))

(define (weak-alist-table:delete-1! table key)
  (let ((default (list 'default)))
    (not (eq? default (weak-alist-table-delete! table key default)))))

(define (weak-alist-table:empty-copy table)
  (weak-alist-table (weak-alist-table-key= table)))

(define (weak-alist-table:find predicate table fail)
  (weak-alist-table-search table predicate
			   (lambda (key value)
			     (declare (ignore key))
			     value)
			   fail))