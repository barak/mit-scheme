#| -*-Scheme-*-

$Id: geneqht.scm,v 1.1 1996/04/23 20:37:27 cph Exp $

Copyright (c) 1990-96 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

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
	     (without-interrupts
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
	    (without-interrupts (lambda () (rehash-table! table)))
	    (loop))))))

(define-integrable (eq-hash-mod key modulus)
  (fix:remainder (let ((n
			((ucode-primitive primitive-object-set-type)
			 (ucode-type positive-fixnum)
			 key)))
		   (if (fix:< n 0)
		       (fix:not n)
		       n))
		 modulus))

(define (record-address-hash-table! table)
  (set! address-hash-tables (weak-cons table address-hash-tables))
  unspecific)

(define (mark-address-hash-tables!)
  (let loop ((previous #f) (tables address-hash-tables))
    (cond ((null? tables)
	   unspecific)
	  ((system-pair-car tables)
	   (set-table-needs-rehash?! (system-pair-car tables) #t)
	   (loop tables (system-pair-cdr tables)))
	  (else
	   (if previous
	       (system-pair-set-cdr! previous (system-pair-cdr tables))
	       (set! address-hash-tables (system-pair-cdr tables)))
	   (loop previous (system-pair-cdr tables))))))

(define address-hash-tables)

(define (initialize-address-hashing!)
  (set! address-hash-tables '())
  (add-primitive-gc-daemon! mark-address-hash-tables!))

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

(define-integrable (without-interrupts thunk)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (thunk)
    (set-interrupt-enables! interrupt-mask)
    unspecific))

(define-integrable (weak-cons car cdr)
  (system-pair-cons (ucode-type weak-cons) car cdr))