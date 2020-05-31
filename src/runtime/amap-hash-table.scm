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

;;;; Hash-table amap type
;;; package: (runtime amap hash-table)

(declare (usual-integrations))

(add-boot-deps! '(runtime hash-table)
		'(runtime amap impl))

(add-boot-init!
 (lambda ()
   (define-amap-implementation 'hash-table
     '((mutability mutable)
       (kv-types (strong strong)
		 (weak strong)
		 (strong weak)
		 (weak weak)
		 (ephemeral strong)
		 (strong ephemeral)
		 (ephemeral ephemeral))
       (time-complexity amortized-constant)
       (other initial-size))
     comparator-hashable?
     `((->alist ,hash-table->alist)
       (clean! ,hash-table-clean!)
       (clear! ,hash-table-clear!)
       (contains? ,hash-table-contains?)
       (copy ,hash-table-copy)
       (count ,hash-table-count)
       (delete! ,hash-table-delete!)
       (difference! ,hash-table-difference!)
       (empty-copy ,hash-table-empty-copy)
       (empty? ,hash-table-empty?)
       (entries ,hash-table-entries)
       (find ,hash-table-find)
       (fold ,hash-table-fold)
       (for-each ,hash-table-for-each)
       (intern! ,hash-table-intern!)
       (intersection! ,hash-table-intersection!)
       (keys ,hash-table-keys)
       (map ,hash-table:map)
       (map! ,hash-table-map!)
       (map->list ,hash-table-map->list)
       (mutable? ,hash-table-mutable?)
       (new-state ,hash-table:new-state)
       (pop! ,hash-table-pop!)
       (prune! ,hash-table-prune!)
       (ref ,hash-table-ref)
       (ref/default ,hash-table-ref/default)
       (set! ,hash-table-set!)
       (size ,hash-table-size)
       (union! ,hash-table-union!)
       (update! ,hash-table-update!)
       (update!/default ,hash-table-update!/default)
       (values ,hash-table-values)
       (xor! ,hash-table-xor!)))))

(define (hash-table:new-state comparator args)
  (apply make-hash-table comparator args))

(define (hash-table:map procedure comparator args table)
  (declare (ignore args))
  (hash-table-map procedure comparator table))