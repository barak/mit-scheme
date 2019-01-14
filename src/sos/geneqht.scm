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

;;;; EQ?-Hash Tables for Generic Procedures

(declare (usual-integrations))

(define (make-eqht)
  (let ((table (%make-eqht)))
    (reset-table! table)
    (record-address-hash-table! table)
    table))

(define (eqht/get table key default)
  (let ((entries
	 (vector-ref (table-buckets table) (compute-key-hash table key))))
    (let loop ((entries entries))
      (cond ((null? entries)
	     default)
	    ((eq? (system-pair-car (car entries)) key)
	     (system-pair-cdr (car entries)))
	    (else
	     (loop (cdr entries)))))))

(define (eqht/put! table key datum)
  (let ((buckets (table-buckets table))
	(hash (compute-key-hash table key)))
    (let loop ((entries (vector-ref buckets hash)))
      (cond ((null? entries)
	     (without-interruption
	      (lambda ()
		(vector-set! buckets
			     hash
			     (cons (weak-cons key datum)
				   (vector-ref buckets hash)))
		(if (> (let ((count (fix:+ (table-count table) 1)))
			 (set-table-count! table count)
			 count)
		       (table-grow-size table))
		    (grow-table! table)))))
	    ((eq? (system-pair-car (car entries)) key)
	     (system-pair-set-cdr! (car entries) datum))
	    (else
	     (loop (cdr entries)))))))

(define (eqht/for-each table procedure)
  (for-each-vector-element (table-buckets table)
    (lambda (entries)
      (for-each (lambda (entry)
		  (if (system-pair-car entry)
		      (procedure (system-pair-car entry)
				 (system-pair-cdr entry))))
		entries))))

;;;; Address Hashing

(define (compute-key-hash table key)
  (let loop ()
    (let ((hash (eq-hash-mod key (vector-length (table-buckets table)))))
      (if (not (table-needs-rehash? table))
	  hash
	  (begin
	    (without-interruption (lambda () (rehash-table! table)))
	    (loop))))))

(define (record-address-hash-table! table)
  (add-to-population! address-hash-tables table))

(define (mark-address-hash-tables!)
  (for-each-inhabitant address-hash-tables
		       (lambda (table)
			 (set-table-needs-rehash?! table #t))))

(define address-hash-tables (make-serial-population))
(add-primitive-gc-daemon! mark-address-hash-tables!)

;;;; Resizing

(define (grow-table! table)
  (let loop ((size (table-grow-size table)))
    (if (> (table-count table) size)
	(loop (let ((size* (round->exact (* size 2.))))
		(if (> size* size)
		    size*
		    (+ size 1))))
	(new-size! table size))))

(define (shrink-table! table)
  (let loop ((size (table-grow-size table)))
    (cond ((<= size minimum-size)
	   (new-size! table minimum-size))
	  ((< (table-count table) (compute-shrink-size size))
	   (loop (decrement-size size)))
	  (else
	   (new-size! table size)))))

(define (new-size! table size)
  (set-table-grow-size! table size)
  (let ((old-buckets (table-buckets table)))
    (reset-table! table)
    (rehash-table-from-old-buckets! table old-buckets)))

(define (reset-table! table)
  (set-table-shrink-size! table (compute-shrink-size (table-grow-size table)))
  (let ((primes
	 (let ((size (round->exact (table-grow-size table))))
	   (let loop
	       ((primes
		 (if (< size (stream-car (table-primes table)))
		     prime-numbers-stream
		     (table-primes table))))
	     (if (<= size (stream-car primes))
		 primes
		 (loop (stream-cdr primes)))))))
    (set-table-primes! table primes)
    (set-table-buckets! table (make-vector (stream-car primes) '()))))

(define (compute-shrink-size size)
  (if (<= size minimum-size)
      0
      (max 0 (decrement-size (decrement-size size)))))

(define (decrement-size size)
  (let ((size* (round->exact (/ size 2.))))
    (if (< size* size)
	size*
	(- size 1))))

;;;; Rehashing

(define (rehash-table-from-old-buckets! table buckets)
  (let ((n-buckets (vector-length buckets)))
    (set-table-needs-rehash?! table #f)
    (do ((i 0 (fix:+ i 1)))
	((fix:= i n-buckets))
      (let ((entries (vector-ref buckets i)))
	(if (not (null? entries))
	    (rehash-table-entries! table entries)))))
  (maybe-shrink-table! table))

(define (rehash-table-entries! table entries)
  (let ((buckets (table-buckets table)))
    (let ((n-buckets (vector-length buckets)))
      (let loop ((entries entries))
	(if (not (null? entries))
	    (let ((rest (cdr entries)))
	      (if (system-pair-car (car entries))
		  (let ((hash
			 (eq-hash-mod (system-pair-car (car entries))
				      n-buckets)))
		    (set-cdr! entries (vector-ref buckets hash))
		    (vector-set! buckets hash entries))
		  (set-table-count! table (fix:- (table-count table) 1)))
	      (loop rest)))))))

(define (maybe-shrink-table! table)
  ;; Since the rehashing also deletes invalid entries, the count
  ;; might have been reduced.  So check to see if it's necessary to
  ;; shrink the table even further.
  (if (< (table-count table) (table-shrink-size table))
      (shrink-table! table)))

(define (rehash-table! table)
  (let ((entries (extract-table-entries! table)))
    (set-table-needs-rehash?! table #f)
    (rehash-table-entries! table entries))
  (maybe-shrink-table! table))

(define (extract-table-entries! table)
  (let ((buckets (table-buckets table)))
    (let ((n-buckets (vector-length buckets)))
      (let ((entries '()))
	(do ((i 0 (fix:+ i 1)))
	    ((fix:= i n-buckets))
	  (let ((bucket (vector-ref buckets i)))
	    (if (not (null? bucket))
		(begin
		  (let loop ((bucket bucket))
		    (if (null? (cdr bucket))
			(set-cdr! bucket entries)
			(loop (cdr bucket))))
		  (set! entries bucket)
		  (vector-set! buckets i '())))))
	entries))))

;;;; Miscellaneous

(define-structure (eqht (constructor %make-eqht ()) (conc-name table-))
  (count 0)
  (grow-size minimum-size)
  (shrink-size 0)
  buckets
  (primes prime-numbers-stream)
  (needs-rehash? #f))

(define-integrable minimum-size 4)