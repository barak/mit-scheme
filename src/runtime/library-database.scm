#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018 Massachusetts Institute of Technology

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

;;;; R7RS libraries: database abstraction
;;; package: (runtime library database)

(declare (usual-integrations))

(define (make-library-db)
  (let ((table (make-equal-hash-table)))

    (define (has? name)
      (hash-table-exists? table name))

    (define (get name #!optional default-value)
      (if (default-object? default-value)
	  (hash-table-ref table name)
	  (hash-table-ref/default table name default-value)))

    (define (put! name value)
      (hash-table-set! table name value))

    (define (delete! key)
      (hash-table-delete! table key))

    (define (get-alist)
      (hash-table->alist table))

    (define (put-alist! alist*)
      (for-each (lambda (p)
		  (put! (car p) (cdr p)))
		alist*))

    (bundle library-db? has? get put! delete! get-alist put-alist!)))

(define library-db?
  (make-bundle-predicate 'library-database))