#| -*-Scheme-*-

$Id: hashtb.scm,v 1.7 1993/10/08 23:30:39 cph Exp $

Copyright (c) 1990-93 Massachusetts Institute of Technology

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

;;;; Hash Tables
;;; package: (runtime hash-table)

(declare (usual-integrations))

;;;; Hash Table Structure

(define-structure (hash-table
		   (constructor make-hash-table
				(key-hash
				 key=?
				 make-entry
				 entry-valid?
				 entry-key
				 entry-datum
				 set-entry-datum!
				 initial-size
				 rehash-threshold
				 rehash-size))
		   (conc-name table-))
  ;; Procedures describing keys and entries.
  (key-hash #f read-only #t)
  (key=? #f read-only #t)
  (make-entry #f read-only #t)
  (entry-valid? #f read-only #t)
  (entry-key #f read-only #t)
  (entry-datum #f read-only #t)
  (set-entry-datum! #f read-only #t)
  (standard-accessors? (and (eq? eq? key=?)
			    (or (and (eq? car entry-key)
				     (eq? cdr entry-datum)
				     (eq? set-cdr! set-entry-datum!))
				(and (eq? weak-car entry-key)
				     (eq? weak-cdr entry-datum)
				     (eq? weak-set-cdr! set-entry-datum!))))
		       read-only #t)

  ;; Parameters of the hash table.
  rehash-threshold
  rehash-size

  ;; Internal state variables.
  count
  size
  (initial-size #f read-only #t)
  grow-size
  shrink-size
  buckets
  primes
  (needs-rehash? #f))

(define (hash-table/constructor key-hash key=? make-entry entry-valid?
				entry-key entry-datum set-entry-datum!)
  (lambda (#!optional initial-size)
    (let ((initial-size
	   (if (default-object? initial-size)
	       default-size
	       (check-arg initial-size
			  default-size
			  exact-nonnegative-integer?
			  "exact nonnegative integer"
			  #f))))
      (let ((table
	     (make-hash-table key-hash
			      key=?
			      make-entry
			      entry-valid?
			      entry-key
			      entry-datum
			      set-entry-datum!
			      (max initial-size minimum-size)
			      default-rehash-threshold
			      default-rehash-size)))
	(clear-table! table)
	(if (address-hash? key-hash)
	    (set! address-hash-tables (weak-cons table address-hash-tables)))
	table))))

(define-integrable (guarantee-hash-table object procedure)
  (if (not (hash-table? object))
      (error:wrong-type-argument object "hash table" procedure)))

;;;; Parameters

(let-syntax
    ((define-export
       (macro (name)
	 (let ((export-name (symbol-append 'HASH-TABLE/ name)))
	   `(DEFINE (,export-name TABLE)
	      (GUARANTEE-HASH-TABLE TABLE ',export-name)
	      (,(symbol-append 'TABLE- name) TABLE))))))
  (define-export key-hash)
  (define-export key=?)
  (define-export make-entry)
  (define-export entry-key)
  (define-export entry-datum)
  (define-export set-entry-datum!)
  (define-export rehash-threshold)
  (define-export rehash-size)
  (define-export count))

(define (hash-table/size table)
  (guarantee-hash-table table 'HASH-TABLE/SIZE)
  (table-grow-size table))

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
       (let ((size (table-size table)))
	 (let ((shrink-size (compute-shrink-size table size))
	       (grow-size (compute-grow-size table size)))
	   (set-table-shrink-size! table shrink-size)
	   (set-table-grow-size! table grow-size)
	   (let ((count (table-count table)))
	     (cond ((< count shrink-size) (shrink-table! table))
		   ((> count grow-size) (grow-table! table))))))))))

(define (set-hash-table/rehash-size! table size)
  (guarantee-hash-table table 'SET-HASH-TABLE/REHASH-SIZE!)
  (set-table-rehash-size!
   table
   (check-arg size
	      default-rehash-size
	      (lambda (x)
		(cond ((exact-integer? x) (< 0 x))
		      ((real? x) (< 1 x))
		      (else #f)))
	      "real number < 1 or exact integer >= 1"
	      'SET-HASH-TABLE/REHASH-SIZE!)))

(define default-size 10)
(define minimum-size 4)
(define default-rehash-threshold 1)
(define default-rehash-size 2.)

(define (check-arg object default predicate description procedure)
  (cond ((predicate object) object)
	((not object) default)
	(else (error:wrong-type-argument object description procedure))))

(define-integrable (without-interrupts thunk)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (thunk)
    (set-interrupt-enables! interrupt-mask)
    unspecific))

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
  (let ((buckets (table-buckets table)))
    (let ((n-buckets (vector-length buckets)))
      (let loop ((n 0) (result '()))
	(if (fix:< n n-buckets)
	    (loop (fix:+ n 1)
		  (let loop ((entries (vector-ref buckets n)) (result result))
		    (if (null? entries)
			result
			(loop (cdr entries)
			      (cons (entry->element (car entries)) result)))))
	    result)))))

;;;; Cleansing

(define (hash-table/clear! table)
  (guarantee-hash-table table 'HASH-TABLE/CLEAR!)
  (without-interrupts (lambda () (clear-table! table))))

(define (clear-table! table)
  (set-table-count! table 0)
  (reset-table! table (table-initial-size table) #f #f #f))

(define (hash-table/clean! table)
  (guarantee-hash-table table 'HASH-TABLE/CLEAN!)
  ;; If `entry-valid?' is #t, then entries never become invalid.
  (if (not (eq? (table-entry-valid? table) #t))
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
		       (set-table-count! table (fix:- (table-count table) 1))
		       (scan-head (cdr entries))))))
	     (scan-tail
	      (lambda (previous entries)
		(cond ((null? entries)
		       unspecific)
		      ((entry-valid? (car entries))
		       (scan-tail entries (cdr entries)))
		      (else
		       (set-table-count! table (fix:- (table-count table) 1))
		       (let loop ((entries (cdr entries)))
			 (cond ((null? entries)
				(set-cdr! previous entries))
			       ((entry-valid? (car entries))
				(set-cdr! previous entries)
				(scan-tail entries (cdr entries)))
			       (else
				(set-table-count! table
						  (fix:- (table-count table)
							 1))
				(loop (cdr entries))))))))))
	  (let ((entries (vector-ref buckets i)))
	    (cond ((null? entries)
		   unspecific)
		  ((entry-valid? (car entries))
		   (scan-tail entries (cdr entries)))
		  (else
		   (set-table-count! table (fix:- (table-count table) 1))
		   (scan-head (cdr entries))))))))))

;;;; Resizing

(define (grow-table! table)
  (let ((count (table-count table))
	(rehash-size (table-rehash-size table)))
    (let loop ((size (table-size table)))
      (let ((grow-size (compute-grow-size table size)))
	(if (> count grow-size)
	    (loop (if (exact-integer? rehash-size)
		      (+ size rehash-size)
		      (let ((size* (round->exact (* size rehash-size))))
			(if (> size* size)
			    size*
			    (+ size 1)))))
	    (new-size! table size grow-size #f (table-primes table)))))))

(define (shrink-table! table)
  (let ((count (table-count table))
	(rehash-size (table-rehash-size table)))
    (let loop ((size (table-size table)))
      (let ((shrink-size (compute-shrink-size table size)))
	(if (< count shrink-size)
	    (loop (if (exact-integer? rehash-size)
		      (- size rehash-size)
		      (let ((size* (round->exact (/ size rehash-size))))
			(if (< size* size)
			    size*
			    (- size 1)))))
	    (new-size! table size #f shrink-size #f))))))

(define (new-size! table size grow-size shrink-size primes)
  (let ((old-buckets (table-buckets table)))
    (reset-table! table size grow-size shrink-size primes)
    (let ((buckets (table-buckets table))
	  (key-hash (table-key-hash table))
	  (entry-key (table-entry-key table)))
      (let ((old-n-buckets (vector-length old-buckets))
	    (n-buckets (vector-length buckets)))
	;; Clear NEEDS-REHASH? before starting the rehash; if it's set
	;; during the rehash that will tell us that GC occurred.
	(set-table-needs-rehash?! table #f)
	(do ((i 0 (fix:+ i 1)))
	    ((fix:= i old-n-buckets))
	  (let loop ((entries (vector-ref old-buckets i)))
	    (if (not (null? entries))
		(let ((next (cdr entries))
		      (hash (key-hash (entry-key (car entries)) n-buckets)))
		  (set-cdr! entries (vector-ref buckets hash))
		  (vector-set! buckets hash entries)
		  (loop next)))))))))

(define (reset-table! table size grow-size shrink-size primes)
  (let ((size (max size minimum-size)))
    (set-table-size! table size)
    (set-table-grow-size! table (or grow-size (compute-grow-size table size)))
    (set-table-shrink-size! table
			    (or shrink-size (compute-shrink-size table size)))
    (let ((primes
	   (let loop ((primes (or primes prime-numbers-stream)))
	     (if (<= size (stream-car primes))
		 primes
		 (loop (stream-cdr primes))))))
      (set-table-primes! table primes)
      (set-table-buckets! table (make-vector (stream-car primes) '())))))

(define (compute-grow-size table size)
  (round->exact (* (table-rehash-threshold table) size)))

(define (compute-shrink-size table size)
  (if (<= size minimum-size)
      0
      (round->exact (* (table-rehash-threshold table)
		       (let ((rehash-size (table-rehash-size table)))
			 (if (exact-integer? rehash-size)
			     (- size (+ rehash-size rehash-size))
			     (/ size (* rehash-size rehash-size))))))))

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
;;; of the address hashing operation, it is sufficient check this flag
;;; whenever the address hashing is performed.  This means that the
;;; rehashing of the table and the computing of the corresponding
;;; address hash must occur atomically with respect to the garbage
;;; collector.

;;; The only tricky part about this algorithm is that the garbage
;;; collector might run while the table is being resized.  If this
;;; occurs, part of the table might be hashed correctly, while the
;;; rest would be incorrect.  This is not a problem because resizing
;;; (with one exception) is always the last thing done by an
;;; operation.  If the garbage collection occurs during a resizing,
;;; the NEEDS-REHASH? flag will be set after the resizing is
;;; completed, and the next operation will rehash the table.

;;; The exception to this rule is COMPUTE-KEY-HASH, which might have
;;; to shrink the table due to keys which have been garbage collected.
;;; COMPUTE-KEY-HASH explicitly checks for this possibility, and
;;; rehashes the table again if necessary.

(define (compute-key-hash table key)
  (let ((key-hash (table-key-hash table)))
    (if (address-hash? key-hash)
	(let ((interrupt-mask (set-interrupt-enables! interrupt-mask/none)))
	  (let loop ()
	    (if (table-needs-rehash? table)
		(begin
		  (rehash-address-hash-table! table)
		  (if (< (table-count table) (table-shrink-size table))
		      (begin
			(set-interrupt-enables! interrupt-mask/gc-ok)
			(shrink-table! table)
			(set-interrupt-enables! interrupt-mask/none)
			(loop))
		      (set-table-needs-rehash?! table #f)))))
	  (let ((hash (key-hash key (vector-length (table-buckets table)))))
	    (set-interrupt-enables! interrupt-mask)
	    hash))
	(key-hash key (vector-length (table-buckets table))))))

(define-integrable (address-hash? key-hash)
  (or (eq? eq-hash key-hash)
      (eq? eqv-hash key-hash)))

(define (eq-hash key modulus)
  (fix:remainder (let ((n
			((ucode-primitive primitive-object-set-type)
			 (ucode-type fixnum)
			 key)))
		   (if (fix:< n 0)
		       (fix:not n)
		       n))
		 modulus))

(define (eqv-hash key modulus)
  (cond ((object-type? (ucode-type big-fixnum) key)
	 (modulo key modulus))
	((object-type? (ucode-type ratnum) key)
	 (modulo (+ (numerator key) (denominator key)) modulus))
	((object-type? (ucode-type big-flonum) key)
	 (modulo (+ (inexact->exact (numerator key))
		    (inexact->exact (denominator key)))
		 modulus))
	((object-type? (ucode-type recnum) key)
	 (modulo (let ((r (real-part key))
		       (i (imag-part key)))
		   (+ (inexact->exact (numerator r))
		      (inexact->exact (denominator r))
		      (inexact->exact (numerator i))
		      (inexact->exact (denominator i))))
		 modulus))
	(else
	 (eq-hash key modulus))))

(define (rehash-address-hash-table! table)
  (let ((buckets (table-buckets table))
	(key-hash (table-key-hash table))
	(entry-key (table-entry-key table)))
    (let ((n-buckets (vector-length buckets)))
      (let loop
	  ((entries
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
	      entries)))
	(if (not (null? entries))
	    (let ((rest (cdr entries)))
	      (if (entry-key (car entries))
		  (let ((hash (key-hash (entry-key (car entries)) n-buckets)))
		    (set-cdr! entries (vector-ref buckets hash))
		    (vector-set! buckets hash entries))
		  (set-table-count! table (fix:- (table-count table) 1)))
	      (loop rest)))))))

(define (mark-address-hash-tables!)
  (let loop ((previous #f) (tables address-hash-tables))
    (cond ((null? tables)
	   unspecific)
	  ((system-pair-car tables)
	   (set-table-needs-rehash?! (system-pair-car tables) #t)
	   (loop tables (system-pair-cdr tables)))
	  (else
	   (if previous
	       (set-cdr! previous (system-pair-cdr tables))
	       (set! address-hash-tables (system-pair-cdr tables)))
	   (loop previous (system-pair-cdr tables))))))

;;;; Initialization

(define make-eq-hash-table)
(define make-eqv-hash-table)
(define address-hash-tables)
(define make-string-hash-table)

;; Define old names for compatibility:
(define hash-table/entry-value hash-table/entry-datum)
(define hash-table/set-entry-value! hash-table/set-entry-datum!)
(define make-object-hash-table)
(define make-symbol-hash-table)

(define (initialize-package!)
  (set! address-hash-tables '())
  (add-primitive-gc-daemon! mark-address-hash-tables!)
  (set! make-eq-hash-table
	(hash-table/constructor eq-hash
				eq?
				weak-cons
				weak-pair/car?
				weak-car
				weak-cdr
				weak-set-cdr!))
  (set! make-eqv-hash-table
	(hash-table/constructor eqv-hash
				eqv?
				weak-cons
				weak-pair/car?
				weak-car
				weak-cdr
				weak-set-cdr!))
  (set! make-object-hash-table make-eqv-hash-table)
  (set! make-symbol-hash-table make-eq-hash-table)
  (set! make-string-hash-table
	(hash-table/constructor string-hash-mod
				string=?
				cons
				#t
				car
				cdr
				set-cdr!))
  unspecific)