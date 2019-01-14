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

;;;; Object Hashing
;;; package: (runtime hash)

;;; How this works:

;;; There are two tables:

;;; * The hash table associates objects (as compared by eqv?) to their hash
;;;   numbers.

;;; * The unhash table associates the hash numbers back to the hashed objects.

;;; Both tables hold the objects weakly; the hash table holds its keys weakly,
;;; and the unhash table holds its values weakly.

(declare (usual-integrations))

(define (hash-object object #!optional hasher)
  ((->hasher hasher 'hash-object) 'hash-object object))

(define (object-hashed? object #!optional hasher)
  ((->hasher hasher 'object-hashed?) 'object-hashed? object))

(define (unhash-object hash #!optional hasher)
  ((->hasher hasher 'unhash-object) 'unhash-object hash))

(define (valid-object-hash? hash #!optional hasher)
  ((->hasher hasher 'valid-object-hash?) 'valid-object-hash? hash))

(define (->hasher hasher caller)
  (if (default-object? hasher)
      default-object-hasher
      (guarantee object-hasher? hasher caller)))

(define-deferred object-hasher? (make-bundle-predicate 'object-hasher))
(define-deferred default-object-hasher (make-object-hasher 313))

(define (make-object-hasher #!optional initial-size)
  (let ((mutex (make-thread-mutex))
	(next-hash 1)
	(hash-table (make-key-weak-eqv-hash-table initial-size))
	(unhash-table (make-datum-weak-eqv-hash-table initial-size)))

    (define (hash-object object)
      (if (eq? object #f)
	  0
	  (with-thread-mutex-lock mutex
	    (lambda ()
	      (hash-table-intern! hash-table object
				  (lambda ()
				    (let ((hash next-hash))
				      (set! next-hash (+ next-hash 1))
				      (hash-table-set! unhash-table hash object)
				      hash)))))))

    (define (object-hashed? object)
      (or (eq? object #f)
	  (with-thread-mutex-lock mutex
	    (lambda ()
	      (hash-table-exists? hash-table object)))))

    (define (unhash-object hash)
      (guarantee exact-nonnegative-integer? hash 'unhash-object)
      (if (= hash 0)
	  #f
	  (with-thread-mutex-lock mutex
	    (lambda ()
	      (hash-table-ref unhash-table hash)))))

    (define (valid-object-hash? hash)
      (guarantee exact-nonnegative-integer? hash 'valid-object-hash?)
      (or (= hash 0)
	  (with-thread-mutex-lock mutex
	    (lambda ()
	      (hash-table-exists? unhash-table hash)))))

    (bundle object-hasher?
	    hash-object object-hashed? unhash-object valid-object-hash?)))