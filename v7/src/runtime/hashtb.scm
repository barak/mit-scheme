#| -*-Scheme-*-

$Id: hashtb.scm,v 1.3 1993/10/07 04:30:34 cph Exp $

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
  primes)

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
			      initial-size
			      default-rehash-threshold
			      default-rehash-size)))
	(clear-table! table)
	table))))

(define (guarantee-hash-table object procedure)
  (if (not (hash-table? object))
      (error:wrong-type-argument object "hash table" procedure)))

(define (check-arg object default predicate description procedure)
  (cond ((predicate object) object)
	((not object) default)
	(else (error:wrong-type-argument object description procedure))))

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
  (define-export count)
  (define-export size))

;; Define old names for compatibility:
(define hash-table/entry-value hash-table/entry-datum)
(define hash-table/set-entry-value! hash-table/set-entry-datum!)

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
		    'SET-HASH-TABLE/REHASH-THRESHOLD!))
	(interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (set-table-rehash-threshold! table threshold)
    (let ((size (table-size table)))
      (let ((shrink-size (compute-shrink-size table size))
	    (grow-size (compute-grow-size table size)))
	(set-table-shrink-size! table shrink-size)
	(set-table-grow-size! table grow-size)
	(let ((count (table-count table)))
	  (cond ((< count shrink-size) (shrink-table! table))
		((> count grow-size) (grow-table! table))))))
    (set-interrupt-enables! interrupt-mask)
    unspecific))

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

;;;; Accessors

(define (hash-table/get table key default)
  (guarantee-hash-table table 'HASH-TABLE/GET)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (let ((result
	   (let ((entries
		  (let ((buckets (table-buckets table)))
		    (vector-ref
		     buckets
		     ((table-key-hash table) key (vector-length buckets))))))
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
		       (entry-key (table-entry-key table))
		       (entry-datum (table-entry-datum table)))
		   (let loop ((entries entries))
		     (cond ((null? entries)
			    default)
			   ((key=? (entry-key (car entries)) key)
			    (entry-datum (car entries)))
			   (else
			    (loop (cdr entries))))))))))
      (set-interrupt-enables! interrupt-mask)
      result)))

(define hash-table/lookup
  (let ((default (list #f)))
    (lambda (table key if-found if-not-found)
      (let ((value (hash-table/get table key default)))
	(if (eq? value default)
	    (if-not-found)
	    (if-found value))))))

;;;; Modifiers

(define (hash-table/put! table key value)
  (guarantee-hash-table table 'HASH-TABLE/PUT!)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (let ((buckets (table-buckets table)))
      (let ((hash ((table-key-hash table) key (vector-length buckets))))
	(let ((add-bucket!
	       (lambda ()
		 (let ((count (fix:+ (table-count table) 1)))
		   (set-table-count! table count)
		   (vector-set! buckets
				hash
				(cons ((table-make-entry table) key value)
				      (vector-ref buckets hash)))
		   (if (> count (table-grow-size table))
		       (grow-table! table))))))
	  (if (and key (table-standard-accessors? table))
	      (let loop ((entries (vector-ref buckets hash)))
		(cond ((null? entries)
		       (add-bucket!))
		      ((eq? (system-pair-car (car entries)) key)
		       (system-pair-set-cdr! (car entries) value))
		      (else
		       (loop (cdr entries)))))
	      (let ((key=? (table-key=? table))
		    (entry-key (table-entry-key table))
		    (set-entry-datum! (table-set-entry-datum! table)))
		(let loop ((entries (vector-ref buckets hash)))
		  (cond ((null? entries)
			 (add-bucket!))
			((key=? (entry-key (car entries)) key)
			 (set-entry-datum! (car entries) value))
			(else
			 (loop (cdr entries))))))))))
    (set-interrupt-enables! interrupt-mask)
    unspecific))

(define (hash-table/remove! table key)
  (guarantee-hash-table table 'HASH-TABLE/REMOVE!)
  (let ((key=? (table-key=? table))
	(entry-key (table-entry-key table))
	(interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok))
	(decrement-count
	 (lambda ()
	   (let ((count (fix:- (table-count table) 1)))
	     (set-table-count! table count)
	     (if (< count (table-shrink-size table))
		 (shrink-table! table))))))
    (let ((buckets (table-buckets table)))
      (let ((hash ((table-key-hash table) key (vector-length buckets))))
	(let ((entries (vector-ref buckets hash)))
	  (if (not (null? entries))
	      (let ((next (cdr entries)))
		(if (key=? (entry-key (car entries)) key)
		    (begin
		      (vector-set! buckets hash next)
		      (decrement-count))
		    (let loop ((previous entries) (entries next))
		      (if (not (null? entries))
			  (let ((next (cdr entries)))
			    (if (key=? (entry-key (car entries)) key)
				(begin
				  (set-cdr! previous next)
				  (decrement-count))
				(loop entries next)))))))))))
    (set-interrupt-enables! interrupt-mask)
    unspecific))

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

(define (hash-table/entries-list table)
  (guarantee-hash-table table 'HASH-TABLE/ENTRIES-LIST)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (let ((result
	   (let ((buckets (table-buckets table)))
	     (let ((n-buckets (vector-length buckets)))
	       (let loop ((n 0) (result '()))
		 (if (fix:< n n-buckets)
		     (loop (fix:+ n 1) (append (vector-ref buckets n) result))
		     result))))))
      (set-interrupt-enables! interrupt-mask)
      result)))

(define (hash-table/entries-vector table)
  (guarantee-hash-table table 'HASH-TABLE/ENTRIES-VECTOR)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
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
      (set-interrupt-enables! interrupt-mask)
      result)))

;;;; Cleansing

(define (hash-table/clear! table)
  (guarantee-hash-table table 'HASH-TABLE/CLEAR!)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (clear-table! table)
    (set-interrupt-enables! interrupt-mask)
    unspecific))

(define (clear-table! table)
  (set-table-count! table 0)
  (new-size! table (table-initial-size table) #f #f #f))

(define (hash-table/clean! table)
  (guarantee-hash-table table 'HASH-TABLE/CLEAN!)
  (let ((entry-valid? (table-entry-valid? table)))
    ;; If `entry-valid?' is #t, then entries never become invalid.
    (if (not (eq? entry-valid? #t))
	(let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
	  (let ((buckets (table-buckets table))
		(count (table-count table)))
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
			       (set! count (fix:- count 1))
			       (scan-head (cdr entries))))))
		     (scan-tail
		      (lambda (previous entries)
			(if (not (null? entries))
			    (if (entry-valid? (car entries))
				(scan-tail entries (cdr entries))
				(begin
				  (set! count (fix:- count 1))
				  (let loop ((entries (cdr entries)))
				    (cond ((null? entries)
					   (set-cdr! previous entries))
					  ((entry-valid? (car entries))
					   (set-cdr! previous entries)
					   (scan-tail entries (cdr entries)))
					  (else
					   (set! count (fix:- count 1))
					   (loop (cdr entries)))))))))))
		  (let ((entries (vector-ref buckets i)))
		    (if (not (null? entries))
			(if (entry-valid? (car entries))
			    (scan-tail entries (cdr entries))
			    (begin
			      (set! count (fix:- count 1))
			      (scan-head (cdr entries)))))))))
	    (set-table-count! table count)
	    (if (< count (table-shrink-size table))
		(shrink-table! table)))
	  (set-interrupt-enables! interrupt-mask)
	  unspecific))))

;;;; Resizing

(define (grow-table! table)
  (let ((old-buckets (table-buckets table)))
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
	      (new-size! table size grow-size #f (table-primes table))))))
    (rehash-buckets! table old-buckets)))

(define (compute-grow-size table size)
  (round->exact (* (table-rehash-threshold table) size)))

(define (shrink-table! table)
  (let ((old-buckets (table-buckets table)))
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
	      (new-size! table size #f shrink-size #f)))))
    (rehash-buckets! table old-buckets)))

(define (compute-shrink-size table size)
  (if (<= size minimum-size)
      0
      (round->exact (* (table-rehash-threshold table)
		       (let ((rehash-size (table-rehash-size table)))
			 (if (exact-integer? rehash-size)
			     (- size (+ rehash-size rehash-size))
			     (/ size (* rehash-size rehash-size))))))))

(define (new-size! table size grow-size shrink-size primes)
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

(define (rehash-buckets! table old-buckets)
  (let ((buckets (table-buckets table))
	(key-hash (table-key-hash table))
	(entry-key (table-entry-key table)))
    (let ((old-n-buckets (vector-length old-buckets))
	  (n-buckets (vector-length buckets)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i old-n-buckets))
	(let loop ((entries (vector-ref old-buckets i)))
	  (if (not (null? entries))
	      (let ((next (cdr entries))
		    (hash (key-hash (entry-key (car entries)) n-buckets)))
		(set-cdr! entries (vector-ref buckets hash))
		(vector-set! buckets hash entries)
		(loop next))))))))

;;;; Common Constructors

(define (make-object-hash-table #!optional initial-size)
  (let ((object-table (hash-table/make)))
    ((hash-table/constructor (lambda (object modulus)
			       (if object
				   (remainder (object-hash object
							   object-table
							   #t)
					      modulus)
				   0))
			     eq?
			     weak-cons
			     weak-pair/car?
			     weak-car
			     weak-cdr
			     weak-set-cdr!)
     (if (default-object? initial-size) #f initial-size))))

(define make-string-hash-table)
(define make-symbol-hash-table)

(define (initialize-package!)
  (set! make-string-hash-table
	(hash-table/constructor string-hash-mod
				string=?
				cons
				#t
				car
				cdr
				set-cdr!))
  (set! make-symbol-hash-table
	(hash-table/constructor symbol-hash-mod
				eq?
				cons
				#t
				car
				cdr
				set-cdr!))
  unspecific)