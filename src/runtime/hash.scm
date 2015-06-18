#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 Massachusetts
    Institute of Technology

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

;;;; Object Hashing
;;; package: (runtime hash)

(declare (usual-integrations))

;;;; Object hashing

;;; How this works:

;;; There are two tables, the hash table and the unhash table:

;;; - The hash table associates objects to their hash numbers.  The
;;; entries are keyed according to the address (datum) of the object.

;;; - The unhash table associates the hash numbers with the
;;; corresponding objects.  It is keyed according to the numbers
;;; themselves.

;;; Both tables hold the objects weakly.  Thus the hash table holds
;;; its keys weakly, and the unhash table holds its values weakly.

(define default/hash-table-size 313)
(define default-hash-table)

(define (initialize-package!)
  (set! make-datum-weak-eq-hash-table
	(hash-table/constructor eq-hash-mod eq? #f
				hash-table-entry-type:datum-weak))
  (set! default-hash-table (hash-table/make)))

(define-structure (hash-table
		   (conc-name hash-table/)
		   (constructor %hash-table/make))
  (mutex)
  (next-number)
  (hash-table)
  (unhash-table))

(define make-datum-weak-eq-hash-table)

(define (hash-table/make #!optional size)
  (let ((size (if (default-object? size)
		  default/hash-table-size
		  size)))
    (%hash-table/make
     (make-thread-mutex)
     1
     (make-key-weak-eq-hash-table size)
     (make-datum-weak-eq-hash-table size))))

(define (hash x #!optional table)
  (if (eq? x #f)
      0
      (object-hash x
		   (if (default-object? table) default-hash-table table)
		   #t)))

(define (unhash n #!optional table)
  (if (= n 0)
      #f
      (let ((object
	     (object-unhash n
			    (if (default-object? table)
				default-hash-table
				table))))
	(if (not object)
	    (error:bad-range-argument n 'UNHASH))
	object)))

(define (valid-hash-number? n #!optional table)
  (or (= n 0)
      (object-unhash n (if (default-object? table) default-hash-table table))))

(define (object-hashed? x #!optional table)
  (or (eq? x #f)
      (object-hash x
		   (if (default-object? table) default-hash-table table)
		   #f)))

(define (object-hash object #!optional table insert?)
  (let ((table
	 (if (default-object? table)
	     default-hash-table
	     (begin
	       (if (not (hash-table? table))
		   (error:wrong-type-argument table
					      "object-hash table"
					      'OBJECT-HASH))
	       table)))
	(insert? (or (default-object? insert?) insert?)))
    (with-thread-mutex-lock (hash-table/mutex table)
      (lambda ()
	(let ((number (hash-table/get (hash-table/hash-table table) object #f)))
	  (if (not number)
	      (if insert?
		  (let ((hashtb (hash-table/hash-table table))
			(unhashtb (hash-table/unhash-table table))
			(next (hash-table/next-number table)))
		    (set-hash-table/next-number! table (1+ next))
		    (hash-table/put! unhashtb next object)
		    (hash-table/put! hashtb object next)
		    next)
		  number)
	      number))))))

(define (object-unhash number #!optional table)
  (let ((table
	 (if (default-object? table)
	     default-hash-table
	     (begin
	       (if (not (hash-table? table))
		   (error:wrong-type-argument table
					      "object-hash table"
					      'OBJECT-UNHASH))
	       table))))
    (with-thread-mutex-lock (hash-table/mutex table)
      (lambda ()
	(hash-table/get (hash-table/unhash-table table) number #f)))))