#| -*-Scheme-*-

$Id: hashtb.scm,v 1.26 2003/03/13 03:15:41 cph Exp $

Copyright 1990,1991,1993,1994,1995,2003 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; Hash Tables
;;; package: (runtime hash-table)

(declare (usual-integrations))

;;;; Hash Table Structure

(define-structure (hash-table
		   (type-descriptor <hash-table>)
		   (constructor make-hash-table
				(key-hash
				 key=?
				 make-entry
				 entry-valid?
				 entry-key
				 entry-datum
				 set-entry-datum!))
		   (conc-name table-))
  ;; Procedures describing keys and entries.
  (key-hash #f read-only #t)
  (key=? #f read-only #t)
  (make-entry #f read-only #t)
  (entry-valid? #f read-only #t)
  (entry-key #f read-only #t)
  (entry-datum #f read-only #t)
  (set-entry-datum! #f read-only #t)

  ;; Parameters of the hash table.
  (rehash-threshold default-rehash-threshold)
  (rehash-size default-rehash-size)

  ;; Internal state variables.
  (count 0)
  (grow-size minimum-size)
  (shrink-size 0)
  buckets
  (primes prime-numbers-stream)
  (flags 0))

(define-integrable (table-standard-accessors? table)
  (read-flag table 1))

(define-integrable (set-table-standard-accessors?! table value)
  (write-flag table 1 value))

(define-integrable (table-needs-rehash? table)
  (read-flag table 2))

(define-integrable (set-table-needs-rehash?! table value)
  (write-flag table 2 value))

(define-integrable (table-initial-size-in-effect? table)
  (read-flag table 4))

(define-integrable (set-table-initial-size-in-effect?! table value)
  (write-flag table 4 value))

(define-integrable (table-rehash-after-gc? table)
  (read-flag table 8))

(define-integrable (set-table-rehash-after-gc?! table value)
  (write-flag table 8 value))

(define-integrable (read-flag table flag)
  (fix:= (fix:and (table-flags table) flag) flag))

(define-integrable (write-flag table flag value)
  (if value
      (set-table-flags! table (fix:or (table-flags table) flag))
      (set-table-flags! table (fix:andc (table-flags table) flag))))

(define-integrable minimum-size 4)
(define-integrable default-rehash-threshold 1)
(define-integrable default-rehash-size 2.)

(define-integrable (guarantee-hash-table object procedure)
  (if (not (hash-table? object))
      (error:wrong-type-argument object "hash table" procedure)))

;;;; Constructors

(define (hash-table/constructor key-hash key=? make-entry entry-valid?
				entry-key entry-datum set-entry-datum!
				#!optional rehash-after-gc?)
  (let ((make-entry (if (eq? cons make-entry) strong-cons make-entry))
	(entry-valid? (if (eq? #t entry-valid?) strong-valid? entry-valid?))
	(entry-key (if (eq? car entry-key) strong-car entry-key))
	(entry-datum (if (eq? cdr entry-datum) strong-cdr entry-datum))
	(set-entry-datum!
	 (if (eq? set-cdr! set-entry-datum!)
	     strong-set-cdr!
	     set-entry-datum!))
	(rehash-after-gc?
	 (and (not (default-object? rehash-after-gc?))
	      rehash-after-gc?)))
    (lambda (#!optional initial-size)
      (let ((initial-size
	     (if (default-object? initial-size)
		 #f
		 (check-arg initial-size
			    #f
			    exact-nonnegative-integer?
			    "exact nonnegative integer"
			    #f))))
	(let ((table
	       (make-hash-table key-hash key=? make-entry entry-valid?
				entry-key entry-datum set-entry-datum!)))
	  (if (and initial-size (> initial-size minimum-size))
	      ;; If an initial size is given, it means that the table
	      ;; should be initialized with that usable size.  The
	      ;; table's usable size remains fixed at the initial size
	      ;; until the count exceeds the usable size, at which point
	      ;; normal table resizing takes over.
	      (begin
		(set-table-grow-size! table initial-size)
		(set-table-initial-size-in-effect?! table #t)))
	  (set-table-standard-accessors?!
	   table
	   (and (eq? eq? key=?)
		(or (eq? car entry-key)
		    (eq? strong-car entry-key)
		    (eq? weak-car entry-key))
		(or (eq? cdr entry-datum)
		    (eq? strong-cdr entry-datum)
		    (eq? weak-cdr entry-datum))
		(or (eq? set-cdr! set-entry-datum!)
		    (eq? strong-set-cdr! set-entry-datum!)
		    (eq? weak-set-cdr! set-entry-datum!))))
	  (set-table-rehash-after-gc?! table rehash-after-gc?)
	  (reset-table! table)
	  (if rehash-after-gc?
	      (set! address-hash-tables (weak-cons table address-hash-tables)))
	  table)))))

;;; Standard trick because known calls to these primitives compile
;;; more efficiently than unknown calls.
(define (strong-cons key datum) (cons key datum))
(define (strong-valid? entry) entry #t)
(define (strong-car entry) (car entry))
(define (strong-cdr entry) (cdr entry))
(define (strong-set-cdr! entry datum) (set-cdr! entry datum))

(define (strong-hash-table/constructor key-hash key=?
				       #!optional rehash-after-gc?)
  (hash-table/constructor key-hash key=? cons #t car cdr set-cdr!
			  (and (not (default-object? rehash-after-gc?))
			       rehash-after-gc?)))

(define (weak-hash-table/constructor key-hash key=?
				     #!optional rehash-after-gc?)
  (hash-table/constructor key-hash key=? weak-cons weak-pair/car?
			  weak-car weak-cdr weak-set-cdr!
			  (and (not (default-object? rehash-after-gc?))
			       rehash-after-gc?)))

;;;; Accessors

(define (hash-table/get table key default)
  (guarantee-hash-table table 'HASH-TABLE/GET)
  (let ((entries
	 (vector-ref (table-buckets table) (compute-key-hash table key))))
    (if (and key (table-standard-accessors? table))
	;; Optimize standard case: compiler makes this fast.
	(let loop ((entries entries))
	  (cond ((null? entries)
		 default)
		((eq? (system-pair-car (car entries)) key)
		 (system-pair-cdr (car entries)))
		(else
		 (loop (cdr entries)))))
	(let ((key=? (table-key=? table))
	      (entry-key (table-entry-key table)))
	  (let loop ((entries entries))
	    (cond ((null? entries)
		   default)
		  ((key=? (entry-key (car entries)) key)
		   ((table-entry-datum table) (car entries)))
		  (else
		   (loop (cdr entries)))))))))

;; This is useful when interning objects using a hash-table.
(define (hash-table/get-key table key default)
  (guarantee-hash-table table 'HASH-TABLE/GET)
  (let ((entries
	 (vector-ref (table-buckets table) (compute-key-hash table key))))
    (if (and key (table-standard-accessors? table))
	;; Optimize standard case: compiler makes this fast.
	(let loop ((entries entries))
	  (cond ((null? entries)
		 default)
		((eq? (system-pair-car (car entries)) key)
		 (system-pair-car (car entries)))
		(else
		 (loop (cdr entries)))))
	(let ((key=? (table-key=? table))
	      (entry-key (table-entry-key table)))
	  (let loop ((entries entries))
	    (cond ((null? entries)
		   default)
		  ((key=? (entry-key (car entries)) key)
		   (entry-key (car entries)))
		  (else
		   (loop (cdr entries)))))))))

(define hash-table/lookup
  (let ((default (list #f)))
    (lambda (table key if-found if-not-found)
      (let ((datum (hash-table/get table key default)))
	(if (eq? datum default)
	    (if-not-found)
	    (if-found datum))))))

;;;; Modifiers

(define (hash-table/put! table key datum)
  (guarantee-hash-table table 'HASH-TABLE/PUT!)
  (let ((buckets (table-buckets table))
	(hash (compute-key-hash table key)))
    (let ((add-bucket!
	   (lambda ()
	     (without-interrupts
	      (lambda ()
		(vector-set! buckets
			     hash
			     (cons ((table-make-entry table) key datum)
				   (vector-ref buckets hash)))
		(if (> (let ((count (fix:+ (table-count table) 1)))
			 (set-table-count! table count)
			 count)
		       (table-grow-size table))
		    (grow-table! table)))))))
      (if (and key (table-standard-accessors? table))
	  (let loop ((entries (vector-ref buckets hash)))
	    (cond ((null? entries)
		   (add-bucket!))
		  ((eq? (system-pair-car (car entries)) key)
		   (system-pair-set-cdr! (car entries) datum))
		  (else
		   (loop (cdr entries)))))
	  (let ((key=? (table-key=? table))
		(entry-key (table-entry-key table)))
	    (let loop ((entries (vector-ref buckets hash)))
	      (cond ((null? entries)
		     (add-bucket!))
		    ((key=? (entry-key (car entries)) key)
		     ((table-set-entry-datum! table) (car entries) datum))
		    (else
		     (loop (cdr entries))))))))))

(define (hash-table/remove! table key)
  (guarantee-hash-table table 'HASH-TABLE/REMOVE!)
  (let ((key=? (table-key=? table))
	(entry-key (table-entry-key table))
	(decrement-count
	 (lambda ()
	   (if (< (let ((count (fix:- (table-count table) 1)))
		    (set-table-count! table count)
		    count)
		  (table-shrink-size table))
	       (shrink-table! table)))))
    (let ((buckets (table-buckets table))
	  (hash (compute-key-hash table key)))
      (let ((entries (vector-ref buckets hash)))
	(if (not (null? entries))
	    (let ((next (cdr entries)))
	      (if (key=? (entry-key (car entries)) key)
		  (without-interrupts
		   (lambda ()
		     (vector-set! buckets hash next)
		     (decrement-count)))
		  (let loop ((previous entries) (entries next))
		    (if (not (null? entries))
			(let ((next (cdr entries)))
			  (if (key=? (entry-key (car entries)) key)
			      (without-interrupts
			       (lambda ()
				 (set-cdr! previous next)
				 (decrement-count)))
			      (loop entries next))))))))))))

;;;; Enumerators

(define (hash-table/for-each table procedure)
  ;; It's difficult to make this more efficient because PROCEDURE is
  ;; allowed to delete the entry from the table, and if the table is
  ;; resized while being examined we'll lose our place.
  (guarantee-hash-table table 'HASH-TABLE/FOR-EACH)
  (let ((entry-key (table-entry-key table))
	(entry-datum (table-entry-datum table)))
    (for-each (lambda (entry)
		(procedure (entry-key entry) (entry-datum entry)))
	      (hash-table/entries-list table))))

(define (hash-table/entries-vector table)
  (guarantee-hash-table table 'HASH-TABLE/ENTRIES-VECTOR)
  (let ((result (make-vector (table-count table))))
    (let* ((buckets (table-buckets table))
	   (n-buckets (vector-length buckets)))
      (let per-bucket ((n 0) (i 0))
	(if (fix:< n n-buckets)
	    (let per-entry ((entries (vector-ref buckets n)) (i i))
	      (if (null? entries)
		  (per-bucket (fix:+ n 1) i)
		  (begin
		    (vector-set! result i (car entries))
		    (per-entry (cdr entries) (fix:+ i 1))))))))
    result))

(define (hash-table/entries-list table)
  (guarantee-hash-table table 'HASH-TABLE/ENTRIES-LIST)
  (table->list table (lambda (entry) entry)))

(define (hash-table->alist table)
  (guarantee-hash-table table 'HASH-TABLE->ALIST)
  (table->list table
	       (let ((entry-key (table-entry-key table))
		     (entry-datum (table-entry-datum table)))
		 (lambda (entry)
		   (cons (entry-key entry) (entry-datum entry))))))

(define (hash-table/key-list table)
  (guarantee-hash-table table 'HASH-TABLE/KEY-LIST)
  (table->list table (table-entry-key table)))

(define (hash-table/datum-list table)
  (guarantee-hash-table table 'HASH-TABLE/DATUM-LIST)
  (table->list table (table-entry-datum table)))

(define (table->list table entry->element)
  (let ((buckets (table-buckets table))
	(cons-element
	 (let ((entry-valid? (table-entry-valid? table)))
	   (if (eq? strong-valid? entry-valid?)
	       (lambda (entry result)
		 (cons (entry->element entry) result))
	       (lambda (entry result)
		 (let ((element (entry->element entry)))
		   (if (entry-valid? entry)
		       (cons element result)
		       result)))))))
    (let ((n-buckets (vector-length buckets)))
      (let loop ((n 0) (result '()))
	(if (fix:< n n-buckets)
	    (loop (fix:+ n 1)
		  (let loop ((entries (vector-ref buckets n)) (result result))
		    (if (null? entries)
			result
			(loop (cdr entries)
			      (cons-element (car entries) result)))))
	    result)))))

;;;; Parameters

(define hash-table/key-hash
  (record-accessor <hash-table> 'KEY-HASH))

(define hash-table/key=?
  (record-accessor <hash-table> 'KEY=?))

(define hash-table/make-entry
  (record-accessor <hash-table> 'MAKE-ENTRY))

(define hash-table/entry-key
  (record-accessor <hash-table> 'ENTRY-KEY))

(define hash-table/entry-datum
  (record-accessor <hash-table> 'ENTRY-DATUM))

(define hash-table/set-entry-datum!
  (record-accessor <hash-table> 'SET-ENTRY-DATUM!))

(define hash-table/rehash-threshold
  (record-accessor <hash-table> 'REHASH-THRESHOLD))

(define hash-table/rehash-size
  (record-accessor <hash-table> 'REHASH-SIZE))

(define hash-table/count
  (record-accessor <hash-table> 'COUNT))

(define hash-table/size
  (record-accessor <hash-table> 'GROW-SIZE))

(define (set-hash-table/rehash-threshold! table threshold)
  (guarantee-hash-table table 'SET-HASH-TABLE/REHASH-THRESHOLD!)
  (let ((threshold
	 (check-arg threshold
		    default-rehash-threshold
		    (lambda (x)
		      (and (real? x)
			   (< 0 x)
			   (<= x 1)))
		    "real number between 0 (exclusive) and 1 (inclusive)"
		    'SET-HASH-TABLE/REHASH-THRESHOLD!)))
    (without-interrupts
     (lambda ()
       (set-table-rehash-threshold! table threshold)
       (new-size! table (table-grow-size table))))))

(define (set-hash-table/rehash-size! table size)
  (guarantee-hash-table table 'SET-HASH-TABLE/REHASH-SIZE!)
  (let ((size
	 (check-arg size
		    default-rehash-size
		    (lambda (x)
		      (cond ((exact-integer? x) (< 0 x))
			    ((real? x) (< 1 x))
			    (else #f)))
		    "real number < 1 or exact integer >= 1"
		    'SET-HASH-TABLE/REHASH-SIZE!)))
    (without-interrupts
     (lambda ()
       (set-table-rehash-size! table size)
       (reset-shrink-size! table)
       (if (< (table-count table) (table-shrink-size table))
	   (shrink-table! table))))))

;;;; Cleansing

(define (hash-table/clear! table)
  (guarantee-hash-table table 'HASH-TABLE/CLEAR!)
  (without-interrupts
   (lambda ()
     (if (not (table-initial-size-in-effect? table))
	 (set-table-grow-size! table minimum-size))
     (set-table-count! table 0)
     (reset-table! table))))

(define (hash-table/clean! table)
  (guarantee-hash-table table 'HASH-TABLE/CLEAN!)
  (if (not (eq? strong-valid? (table-entry-valid? table)))
      (without-interrupts
       (lambda ()
	 (clean-table! table)
	 (if (< (table-count table) (table-shrink-size table))
	     (shrink-table! table))))))

(define (clean-table! table)
  (let ((buckets (table-buckets table))
	(entry-valid? (table-entry-valid? table)))
    (let ((n-buckets (vector-length buckets)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i n-buckets))
	(letrec
	    ((scan-head
	      (lambda (entries)
		(cond ((null? entries)
		       (vector-set! buckets i entries))
		      ((entry-valid? (car entries))
		       (vector-set! buckets i entries)
		       (scan-tail entries (cdr entries)))
		      (else
		       (decrement-table-count! table)
		       (scan-head (cdr entries))))))
	     (scan-tail
	      (lambda (previous entries)
		(cond ((null? entries)
		       unspecific)
		      ((entry-valid? (car entries))
		       (scan-tail entries (cdr entries)))
		      (else
		       (decrement-table-count! table)
		       (let loop ((entries (cdr entries)))
			 (cond ((null? entries)
				(set-cdr! previous entries))
			       ((entry-valid? (car entries))
				(set-cdr! previous entries)
				(scan-tail entries (cdr entries)))
			       (else
				(decrement-table-count! table)
				(loop (cdr entries))))))))))
	  (let ((entries (vector-ref buckets i)))
	    (cond ((null? entries)
		   unspecific)
		  ((entry-valid? (car entries))
		   (scan-tail entries (cdr entries)))
		  (else
		   (decrement-table-count! table)
		   (scan-head (cdr entries))))))))))

(define-integrable (decrement-table-count! table)
  (set-table-count! table (fix:- (table-count table) 1)))

;;;; Resizing

(define (grow-table! table)
  (let loop ((size (table-grow-size table)))
    (if (> (table-count table) size)
	(loop (increment-size table size))
	(new-size! table size)))
  (set-table-initial-size-in-effect?! table #f))

(define (shrink-table! table)
  (if (not (table-initial-size-in-effect? table))
      (let loop ((size (table-grow-size table)))
	(cond ((<= size minimum-size)
	       (new-size! table minimum-size))
	      ((< (table-count table) (compute-shrink-size table size))
	       (loop (decrement-size table size)))
	      (else
	       (new-size! table size))))))

(define (new-size! table size)
  (set-table-grow-size! table size)
  (let ((old-buckets (table-buckets table)))
    (reset-table! table)
    (rehash-table-from-old-buckets! table old-buckets)))

(define (reset-table! table)
  (reset-shrink-size! table)
  (let ((primes
	 (let ((size
		(round->exact (/ (table-grow-size table)
				 (table-rehash-threshold table)))))
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

(define (reset-shrink-size! table)
  (set-table-shrink-size! table
			  (compute-shrink-size table (table-grow-size table))))

(define (compute-shrink-size table size)
  (if (<= size minimum-size)
      0
      (max 0 (decrement-size table (decrement-size table size)))))

(define (increment-size table size)
  (let ((rehash-size (table-rehash-size table)))
    (if (exact-integer? rehash-size)
	(+ size rehash-size)
	(let ((size* (round->exact (* size rehash-size))))
	  (if (> size* size)
	      size*
	      (+ size 1))))))

(define (decrement-size table size)
  (let ((rehash-size (table-rehash-size table)))
    (if (exact-integer? rehash-size)
	(- size rehash-size)
	(let ((size* (round->exact (/ size rehash-size))))
	  (if (< size* size)
	      size*
	      (- size 1))))))

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
  (let ((buckets (table-buckets table))
	(entry-valid? (table-entry-valid? table))
	(entry-key (table-entry-key table))
	(key-hash (table-key-hash table)))
    (let ((n-buckets (vector-length buckets)))
      (let loop ((entries entries))
	(if (not (null? entries))
	    (let ((rest (cdr entries)))
	      (if (entry-valid? (car entries))
		  (let ((hash
			 (key-hash (entry-key (car entries)) n-buckets)))
		    (set-cdr! entries (vector-ref buckets hash))
		    (vector-set! buckets hash entries))
		  (decrement-table-count! table))
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

;;;; Address-Hash Tables

;;; Address-hash tables compute their hash number from the address of
;;; the key.  Because the address is changed by the garbage collector,
;;; it is necessary to rehash the table after a garbage collection.

;;; Rehashing the table during the garbage collection is undesirable
;;; for these reasons:
;;; 1. The time required to rehash the table is proportional to the
;;;    number of items in the table, which can be quite large.  It's
;;;    undesirable for the garbage collection time to be extended this
;;;    way.
;;; 2. If the garbage collector rearranges the internals of the table,
;;;    then nearly every operation on the table must be locked to
;;;    prevent garbage collection from occurring while it runs.  This
;;;    means long periods with interrupts disabled, plus the overhead
;;;    of interrupt locking that is otherwise unnecessary.
;;; 3. If the table isn't used in between two garbage collections,
;;;    then the effort to rehash it during the first garbage
;;;    collection is wasted.

;;; For these reasons, rehashing of the table is performed lazily.
;;; When the garbage collector runs, it sets the table's NEEDS-REHASH?
;;; flag.  This flag is examined by all of the hash-table operations
;;; to see if it is necessary to rehash the table before performing
;;; the operation.  Since the only reason for rehashing the table is
;;; to ensure consistency between the table's contents and the result
;;; of the address hashing operation, it is sufficient to check this
;;; flag whenever the address hashing is performed.  This means that
;;; the rehashing of the table and the computing of the corresponding
;;; address hash must occur atomically with respect to the garbage
;;; collector.

;;; The only tricky part about this algorithm is that the garbage
;;; collector might run while the table is being resized.  If this
;;; occurs, part of the table might be hashed correctly, while the
;;; rest would be incorrect.  This is not a problem because resizing
;;; (with one exception) is always the last thing done by an
;;; operation.  If the garbage collection occurs during a resizing,
;;; the NEEDS-REHASH? flag will be true after the resizing is
;;; completed, and the next operation will rehash the table.

;;; The exception to this rule is COMPUTE-KEY-HASH, which might have
;;; to shrink the table due to keys which have been reclaimed by the
;;; garbage collector.  REHASH-TABLE! explicitly checks for this
;;; possibility, and rehashes the table again if necessary.

(define (compute-key-hash table key)
  (let ((key-hash (table-key-hash table)))
    (if (table-rehash-after-gc? table)
	(let loop ()
	  (let ((hash (key-hash key (vector-length (table-buckets table)))))
	    (if (not (table-needs-rehash? table))
		hash
		(begin
		  (without-interrupts (lambda () (rehash-table! table)))
		  (loop)))))
	(key-hash key (vector-length (table-buckets table))))))

(define-integrable (eq-hash-mod key modulus)
  (fix:remainder (eq-hash key) modulus))

(define-integrable (eq-hash object)
  (let ((n
	 ((ucode-primitive primitive-object-set-type)
	  (ucode-type positive-fixnum)
	  object)))
    (if (fix:< n 0)
	(fix:not n)
	n)))

(define (eqv-hash-mod key modulus)
  (int:remainder (eqv-hash key) modulus))

(define (eqv-hash key)
  (cond ((%bignum? key) (%bignum->nonneg-int key))
	((%ratnum? key) (%ratnum->nonneg-int key))
	((flo:flonum? key) (%flonum->nonneg-int key))
	((%recnum? key) (%recnum->nonneg-int key))
	(else (eq-hash key))))

(define (equal-hash-mod key modulus)
  (int:remainder (equal-hash key) modulus))

(define (equal-hash key)
  (cond ((pair? key)
	 (int:+ (equal-hash (car key))
		(equal-hash (cdr key))))
	((vector? key)
	 (let ((length (vector-length key)))
	   (do ((i 0 (fix:+ i 1))
		(accum 0
		       (int:+ accum
			      (equal-hash (vector-ref key i)))))
	       ((fix:= i length) accum))))
	((cell? key)
	 (equal-hash (cell-contents key)))
	((%bignum? key)
	 (%bignum->nonneg-int key))
	((%ratnum? key)
	 (%ratnum->nonneg-int key))
	((flo:flonum? key)
	 (%flonum->nonneg-int key))
	((%recnum? key)
	 (%recnum->nonneg-int key))
	((string? key)
	 (string-hash key))
	((bit-string? key)
	 (bit-string->unsigned-integer key))
	((pathname? key)
	 (string-hash (->namestring key)))
	(else
	 (eq-hash key))))

(define-integrable (%bignum? object)
  (object-type? (ucode-type big-fixnum) object))

(define-integrable (%ratnum? object)
  (object-type? (ucode-type ratnum) object))

(define-integrable (%recnum? object)
  (object-type? (ucode-type recnum) object))

(define-integrable (%bignum->nonneg-int bignum)
  (int:abs bignum))

(define-integrable (%ratnum->nonneg-int ratnum)
  (int:abs (int:+ (system-pair-car ratnum) (system-pair-cdr ratnum))))

(define-integrable (%flonum->nonneg-int flonum)
  (int:abs
   (flo:truncate->exact
    ((ucode-primitive flonum-denormalize 2)
     (car ((ucode-primitive flonum-normalize 1) flonum))
     microcode-id/floating-mantissa-bits))))

(define-integrable (%recnum->nonneg-int recnum)
  (let ((%real->nonneg-int
	 (lambda (real)
	   (cond ((%ratnum? real) (%ratnum->nonneg-int real))
		 ((flo:flonum? real) (%flonum->nonneg-int real))
		 (else (%bignum->nonneg-int real))))))
    (int:+ (%real->nonneg-int (system-pair-car recnum))
	   (%real->nonneg-int (system-pair-cdr recnum)))))

(declare (integrate-operator int:abs))
(define (int:abs n)
  (if (int:negative? n) (int:negate n) n))

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

;;;; Miscellany

(define address-hash-tables)
(define make-eq-hash-table)
(define make-eqv-hash-table)
(define make-equal-hash-table)
(define make-string-hash-table)

;; Define old names for compatibility:
(define hash-table/entry-value hash-table/entry-datum)
(define hash-table/set-entry-value! hash-table/set-entry-datum!)
(define make-symbol-hash-table)
(define make-object-hash-table)

(define (initialize-package!)
  (set! address-hash-tables '())
  (add-primitive-gc-daemon! mark-address-hash-tables!)
  (set! make-eq-hash-table (weak-hash-table/constructor eq-hash-mod eq? #t))
  ;; EQV? hash tables are weak except for numbers and #F.  It's
  ;; important to keep numbers in the table, and handling #F specially
  ;; makes it easier to deal with weak pairs.
  (set! make-eqv-hash-table
	(hash-table/constructor eqv-hash-mod
				eqv?
				(lambda (key datum)
				  (if (or (not key) (number? key))
				      (cons key datum)
				      (system-pair-cons (ucode-type weak-cons)
							key
							datum)))
				(lambda (entry)
				  (or (pair? entry)
				      (system-pair-car entry)))
				(lambda (entry)
				  (system-pair-car entry))
				(lambda (entry)
				  (system-pair-cdr entry))
				(lambda (entry datum)
				  (system-pair-set-cdr! entry datum))
				#t))
  (set! make-equal-hash-table
	(strong-hash-table/constructor equal-hash-mod equal? #t))
  (set! make-symbol-hash-table make-eq-hash-table)
  (set! make-object-hash-table make-eqv-hash-table)
  (set! make-string-hash-table
	(strong-hash-table/constructor string-hash-mod string=? #f))
  unspecific)

(define (check-arg object default predicate description procedure)
  (cond ((predicate object) object)
	((not object) default)
	(else (error:wrong-type-argument object description procedure))))

(define-integrable (without-interrupts thunk)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (thunk)
    (set-interrupt-enables! interrupt-mask)
    unspecific))