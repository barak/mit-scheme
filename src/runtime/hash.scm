#| -*-Scheme-*-

$Id: fbc07e9bc1cdca5124653fdcf5149a15a0eb36ea $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;; The hashing code depends on weak conses supported by the
;;; microcode.  In particular, it depends on the fact that the car of
;;; a weak cons becomes #F if the object is garbage collected.

;;; Important: This code must be rewritten for a parallel processor,
;;; since two processors may be updating the data structures
;;; simultaneously.

;;; How this works:

;;; There are two tables, the hash table and the unhash table:

;;; - The hash table associates objects to their hash numbers.  The
;;; entries are keyed according to the address (datum) of the object,
;;; and thus must be recomputed after every relocation (ie. band
;;; loading, garbage collection, etc.).

;;; - The unhash table associates the hash numbers with the
;;; corresponding objects.  It is keyed according to the numbers
;;; themselves.

;;; In order to make the hash and unhash tables weakly hold the
;;; objects hashed, the following mechanism is used:

;;; The hash table, a vector, has a NMV header before all the
;;; buckets, and therefore the garbage collector will skip it and will
;;; not relocate its buckets.  It becomes invalid after a garbage
;;; collection and the first thing the daemon does is clear it.  Each
;;; bucket is a normal alist with the objects in the cars, and the
;;; numbers in the cdrs, thus assq can be used to find an object in
;;; the bucket.

;;; The unhash table, also a vector, holds the objects by means of
;;; weak conses.  These weak conses are the same as the pairs in the
;;; buckets in the hash table, but with their type codes changed.
;;; Each of the buckets in the unhash table is headed by an extra pair
;;; whose car is usually #T.  This pair is used by the splicing code.
;;; The daemon treats buckets headed by #F differently from buckets
;;; headed by #T.  A bucket headed by #T is compressed: Those pairs
;;; whose cars have disappeared are spliced out from the bucket.  On
;;; the other hand, buckets headed by #F are not compressed.  The
;;; intent is that while object-unhash is traversing a bucket, the
;;; bucket is locked so that the daemon will not splice it out behind
;;; object-unhash's back.  Then object-unhash does not need to be
;;; locked against garbage collection.

(define default/hash-table-size 313)
(define default-hash-table)
(define all-hash-tables)

(define (initialize-package!)
  (set! all-hash-tables (weak-cons 0 '()))
  (set! default-hash-table (hash-table/make))
  (add-event-receiver! event:after-restore (lambda () (gc-flip)))
  (add-primitive-gc-daemon! rehash-all-gc-daemon))

(define-structure (hash-table
		   (conc-name hash-table/)
		   (constructor %hash-table/make))
  (size)
  (next-number)
  (hash-table)
  (unhash-table))

(define (hash-table/make #!optional size)
  (let* ((size (if (default-object? size)
		   default/hash-table-size
		   size))
	 (table
	  (%hash-table/make
	   size
	   1
	   (let ((table (make-vector (+ size 1) '())))
	     (vector-set! table
			  0
			  ((ucode-primitive primitive-object-set-type)
			   (ucode-type manifest-nm-vector)
			   (make-non-pointer-object size)))
	     ((ucode-primitive primitive-object-set-type)
	      (ucode-type non-marked-vector)
	      table))
	   (let ((table (make-vector size '())))
	     (let loop ((n 0))
	       (if (fix:< n size)
		   (begin
		     (vector-set! table n (cons #t '()))
		     (loop (fix:+ n 1)))))
	     table))))
    (weak-set-cdr! all-hash-tables
		   (weak-cons table (weak-cdr all-hash-tables)))
    table))

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

;;; This can cons a bit when interpreted.

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
    (with-absolutely-no-interrupts
      (lambda ()
	(let* ((hash-index (fix:+ 1
				  (modulo (object-datum object)
					  (hash-table/size table))))
	       (the-hash-table
		((ucode-primitive primitive-object-set-type)
		 (ucode-type vector)
		 (hash-table/hash-table table)))
	       (bucket (vector-ref the-hash-table hash-index))
	       (association (assq object bucket)))
	  (cond (association (cdr association))
		((not insert?) #f)
		(else
		 (let ((result (hash-table/next-number table)))
		   (let ((pair (cons object result))
			 (unhash-bucket
			  (vector-ref (hash-table/unhash-table table)
				      (modulo result
					      (hash-table/size table)))))
		     (set-hash-table/next-number! table (+ result 1))
		     (vector-set! the-hash-table
				  hash-index
				  (cons pair bucket))
		     (set-cdr! unhash-bucket
			       (cons (object-new-type (ucode-type weak-cons)
						      pair)
				     (cdr unhash-bucket)))
		     result)))))))))

;;; This is safe because it locks the garbage collector out only for a
;;; little time, enough to tag the bucket being searched, so that the
;;; daemon will not splice that bucket.

(define (object-unhash number #!optional table)
  (let* ((table
	  (if (default-object? table)
	      default-hash-table
	      (begin
		(if (not (hash-table? table))
		    (error:wrong-type-argument table
					       "object-hash table"
					       'OBJECT-UNHASH))
		table)))
	 (index (modulo number (hash-table/size table))))
    (with-absolutely-no-interrupts
      (lambda ()
	(let ((bucket (vector-ref (hash-table/unhash-table table) index)))
	  (set-car! bucket #f)
	  (let ((result
		 (with-interrupt-mask interrupt-mask/gc-ok
		   (lambda (interrupt-mask)
		     interrupt-mask
		     (let loop ((l (cdr bucket)))
		       (cond ((null? l) #f)
			     ((= number (system-pair-cdr (car l)))
			      (system-pair-car (car l)))
			     (else (loop (cdr l)))))))))
	    (set-car! bucket #t)
	    result))))))

;;;; Rehash daemon

;;; The following is dangerous because of the (unnecessary) consing
;;; done by the interpreter while it executes the loops.  It runs with
;;; interrupts turned off.  The (necessary) consing done by rehash is
;;; not dangerous because at least that much storage was freed by the
;;; garbage collector.  To understand this, notice that the hash table
;;; has a SNMV header, so the garbage collector does not trace the
;;; hash table buckets, therefore freeing their storage.  The header
;;; is SNM rather than NM to make the buckets be relocated at band
;;; load/restore time.

;;; Until this code is compiled, and therefore safe, it is replaced by
;;; a primitive.  See the installation code below.
#|
(define (hash-table/rehash table)
  (let ((hash-table-size (hash-table/size table))
	(hash-table ((ucode-primitive primitive-object-set-type)
		     (ucode-type vector)
		     (hash-table/hash-table table)))
	(unhash-table (hash-table/unhash-table table)))

    (define (rehash weak-pair)
      (let ((index
	     (fix:+ 1 (modulo (object-datum (system-pair-car weak-pair))
			      hash-table-size))))
	(vector-set! hash-table
		     index
		     (cons (object-new-type (ucode-type pair) weak-pair)
			   (vector-ref hash-table index)))
	unspecific))

    (let cleanup ((n hash-table-size))
      (if (not (fix:= n 0))
	  (begin
	    (vector-set! hash-table n '())
	    (cleanup (fix:- n 1)))))

    (let outer ((n (fix:- hash-table-size 1)))
      (if (not (fix:< n 0))
	  (let ((bucket (vector-ref unhash-table n)))
	    (if (car bucket)
		(let inner1 ((l1 bucket) (l2 (cdr bucket)))
		  (cond ((null? l2)
			 (outer (fix:- n 1)))
			((eq? (system-pair-car (car l2)) #f)
			 (set-cdr! l1 (cdr l2))
			 (inner1 l1 (cdr l1)))
			(else
			 (rehash (car l2))
			 (inner1 l2 (cdr l2)))))
		(let inner2 ((l (cdr bucket)))
		  (cond ((null? l)
			 (outer (fix:- n 1)))
			((eq? (system-pair-car (car l)) #f)
			 (inner2 (cdr l)))
			(else
			 (rehash (car l))
			 (inner2 (cdr l)))))))))))
|#

(define-integrable (hash-table/rehash table)
  ((ucode-primitive rehash) (hash-table/unhash-table table)
			    (hash-table/hash-table table)))

(define (rehash-all-gc-daemon)
  (let loop ((l all-hash-tables)
	     (n (weak-cdr all-hash-tables)))
    (cond ((null? n)
	   (weak-set-cdr! l n))
	  ((not (weak-pair/car? n))
	   (loop l (weak-cdr n)))
	  (else
	   (weak-set-cdr! l n)
	   (hash-table/rehash (weak-car n))
	   (loop n (weak-cdr n))))))