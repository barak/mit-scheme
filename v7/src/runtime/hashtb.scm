#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/hashtb.scm,v 1.2 1991/02/15 18:05:41 cph Exp $

Copyright (c) 1990-1 Massachusetts Institute of Technology

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
;;; This implementation is interrupt locked so that it is not possible
;;; to leave a hash table in an inconsistent state by aborting a
;;; computation.  However, the locking is not sufficient to permit a
;;; hash table to be shared between two concurrent processes.

(define type
  (make-record-type "hash-table"
    '(
      ;; Procedures describing keys and entries.
      KEY-HASH
      KEY=?
      MAKE-ENTRY
      ENTRY-VALID?
      ENTRY-KEY
      ENTRY-VALUE
      SET-ENTRY-VALUE!

      ;; Parameters of the hash table.
      REHASH-THRESHOLD
      REHASH-SIZE

      ;; Internal state variables.
      COUNT
      SIZE
      INITIAL-SIZE
      BUCKETS
      PRIMES
      )))

(define hash-table?                 (record-predicate type))
(define hash-table/key-hash         (record-accessor  type 'KEY-HASH))
(define hash-table/key=?            (record-accessor  type 'KEY=?))
(define hash-table/make-entry       (record-accessor  type 'MAKE-ENTRY))
(define hash-table/entry-valid?     (record-accessor  type 'ENTRY-VALID?))
(define hash-table/entry-key        (record-accessor  type 'ENTRY-KEY))
(define hash-table/entry-value      (record-accessor  type 'ENTRY-VALUE))
(define hash-table/set-entry-value! (record-accessor  type 'SET-ENTRY-VALUE!))
(define hash-table/rehash-threshold (record-accessor  type 'REHASH-THRESHOLD))
(define hash-table/rehash-size      (record-accessor  type 'REHASH-SIZE))
(define hash-table/count            (record-accessor  type 'COUNT))
(define set-hash-table/count!       (record-updater   type 'COUNT))
(define hash-table/size             (record-accessor  type 'SIZE))
(define set-hash-table/size!        (record-updater   type 'SIZE))
(define hash-table/buckets          (record-accessor  type 'BUCKETS))

;;;; Parameters

(define hash-table/constructor
  (let ((constructor
	 (record-constructor type
			     '(KEY-HASH
			       KEY=?
			       MAKE-ENTRY
			       ENTRY-VALID?
			       ENTRY-KEY
			       ENTRY-VALUE
			       SET-ENTRY-VALUE!
			       INITIAL-SIZE
			       REHASH-THRESHOLD
			       REHASH-SIZE))))
    (lambda (key-hash key=? make-entry entry-valid? entry-key entry-value
		      set-entry-value!)
      (lambda (#!optional initial-size)
	(let ((initial-size
	       (if (default-object? initial-size)
		   default-size
		   (check-arg initial-size
			      exact-nonnegative-integer?
			      default-size))))
	  (let ((table
		 (constructor key-hash
			      key=?
			      make-entry
			      entry-valid?
			      entry-key
			      entry-value
			      set-entry-value!
			      initial-size
			      default-threshold-factor
			      default-growth-factor)))
	    (clear-table! table)
	    table))))))

(define set-hash-table/rehash-threshold!
  (let ((updater (record-updater type 'REHASH-THRESHOLD)))
    (lambda (table factor)
      (let ((factor
	     (check-arg factor
			(lambda (x)
			  (and (real? x)
			       (positive? x)
			       (<= x 1)))
			default-threshold-factor)))
	(cond ((< factor
		  (/ (hash-table/size table)
		     (vector-length (hash-table/buckets table))))
	       (without-interrupts
		(lambda ()
		  (updater table factor)
		  (grow-table! table (hash-table/count table)))))
	      ((not (= factor (hash-table/rehash-threshold table)))
	       (updater table factor)))))))

(define set-hash-table/rehash-size!
  (let ((updater (record-updater type 'REHASH-SIZE)))
    (lambda (table factor)
      (updater table
	       (check-arg factor
			  (lambda (x)
			    (cond ((exact-integer? x) (positive? x))
				  ((real? x) (< 1 x))
				  (else false)))
			  default-growth-factor)))))

(define default-size 10)
(define default-threshold-factor 1)
(define default-growth-factor 2.)

;;;; Accessors and Updaters

(define (hash-table/get table key default)
  (let ((key=? (hash-table/key=? table))
	(entry-key (hash-table/entry-key table)))
    (let loop
	((entries
	  (let ((buckets (hash-table/buckets table)))
	    (vector-ref
	     buckets
	     ((hash-table/key-hash table) key (vector-length buckets))))))
      (cond ((null? entries)
	     default)
	    ((key=? (entry-key (car entries)) key)
	     ((hash-table/entry-value table) (car entries)))
	    (else
	     (loop (cdr entries)))))))

(define (hash-table/lookup table key if-found if-not-found)
  (let ((default '(default)))
    (let ((value (hash-table/get table key default)))
      (if (eq? value default)
	  (if-not-found)
	  (if-found value)))))

(define (hash-table/put! table key value)
  (let ((buckets (hash-table/buckets table))
	(key-hash (hash-table/key-hash table))
	(key=? (hash-table/key=? table))
	(entry-key (hash-table/entry-key table)))
    (let ((hash (key-hash key (vector-length buckets))))
      (let loop ((entries (vector-ref buckets hash)))
	(cond ((null? entries)
	       (let ((count (fix:1+ (hash-table/count table))))
		 (with-values
		     (lambda ()
		       (if (> count (hash-table/size table))
			   (begin
			     (without-interrupts
			      (lambda ()
				(grow-table! table count)))
			     (let ((buckets (hash-table/buckets table)))
			       (values buckets
				       (key-hash key
						 (vector-length buckets)))))
			   (values buckets hash)))
		   (lambda (buckets hash)
		     (without-interrupts
		      (lambda ()
			(set-hash-table/count! table count)
			(vector-set!
			 buckets
			 hash
			 (cons ((hash-table/make-entry table) key value)
			       (vector-ref buckets hash)))))))))
	      ((key=? (entry-key (car entries)) key)
	       ((hash-table/set-entry-value! table) (car entries) value))
	      (else
	       (loop (cdr entries))))))))

(define (hash-table/remove! table key)
  (let ((buckets (hash-table/buckets table))
	(key=? (hash-table/key=? table))
	(entry-key (hash-table/entry-key table)))
    (let ((hash ((hash-table/key-hash table) key (vector-length buckets))))
      (let ((entries (vector-ref buckets hash)))
	(if (not (null? entries))
	    (let ((next (cdr entries)))
	      (if (key=? (entry-key (car entries)) key)
		  (vector-set! buckets hash next)
		  (let loop ((previous entries) (entries next))
		    (if (not (null? entries))
			(let ((next (cdr entries)))
			  (if (key=? (entry-key (car entries)) key)
			      (set-cdr! previous next)
			      (loop entries next))))))))))))

;;;; Enumerators

(define (hash-table/for-each table procedure)
  (let ((buckets (hash-table/buckets table))
	(entry-key (hash-table/entry-key table))
	(entry-value (hash-table/entry-value table)))
    (let ((n-buckets (vector-length buckets)))
      (let loop ((n 0))
	(if (fix:< n n-buckets)
	    (begin
	      (let loop ((entries (vector-ref buckets n)))
		(if (not (null? entries))
		    (begin
		      ;; As in Common Lisp, the only alteration that
		      ;; `procedure' may make to `table' is to remove
		      ;; its argument entry.
		      (let ((entry (car entries)))
			(procedure (entry-key entry) (entry-value entry)))
		      (loop (cdr entries)))))
	      (loop (fix:1+ n))))))))

(define (hash-table/entries-list table)
  (let ((buckets (hash-table/buckets table)))
    (let ((n-buckets (vector-length buckets)))
      (let loop ((n 0) (result '()))
	(if (fix:< n n-buckets)
	    (loop (fix:1+ n) (append (vector-ref buckets n) result))
	    result)))))

(define (hash-table/entries-vector table)
  (let ((result (make-vector (hash-table/count table))))
    (let* ((buckets (hash-table/buckets table))
	   (n-buckets (vector-length buckets)))
      (let per-bucket ((n 0) (i 0))
	(if (fix:< n n-buckets)
	    (let per-entry ((entries (vector-ref buckets n)) (i i))
	      (if (null? entries)
		  (per-bucket (fix:1+ n) i)
		  (begin
		    (vector-set! result i (car entries))
		    (per-entry (cdr entries) (fix:1+ i))))))))
    result))

;;;; Cleansing

(define (hash-table/clear! table)
  (without-interrupts (lambda () (clear-table! table))))

(define (hash-table/clean! table)
  (let ((entry-valid? (hash-table/entry-valid? table)))
    ;; If `entry-valid?' is #t, then entries never become invalid.
    (if (not (eq? entry-valid? true))
	(without-interrupts
	 (lambda ()
	   (let ((buckets (hash-table/buckets table))
		 (count (hash-table/count table)))
	     (let ((n-buckets (vector-length buckets)))
	       (let per-bucket ((i 0))
		 (define (scan-head entries)
		   (cond ((null? entries)
			  (vector-set! buckets i entries))
			 ((entry-valid? (car entries))
			  (vector-set! buckets i entries)
			  (scan-tail entries (cdr entries)))
			 (else
			  (set! count (fix:-1+ count))
			  (scan-head (cdr entries)))))
		 (define (scan-tail previous entries)
		   (if (not (null? entries))
		       (if (entry-valid? (car entries))
			   (scan-tail entries (cdr entries))
			   (begin
			     (set! count (fix:-1+ count))
			     (let loop ((entries (cdr entries)))
			       (cond ((null? entries)
				      (set-cdr! previous entries))
				     ((entry-valid? (car entries))
				      (set-cdr! previous entries)
				      (scan-tail entries (cdr entries)))
				     (else
				      (set! count (fix:-1+ count))
				      (loop (cdr entries)))))))))
		 (if (fix:< i n-buckets)
		     (begin
		       (let ((entries (vector-ref buckets i)))
			 (if (not (null? entries))
			     (if (entry-valid? (car entries))
				 (scan-tail entries (cdr entries))
				 (begin
				   (set! count (fix:-1+ count))
				   (scan-head (cdr entries))))))
		       (per-bucket (fix:1+ i))))))
	     (set-hash-table/count! table count)))))))

;;;; Auxiliary Procedures

(define clear-table!
  (let ((initial-size (record-accessor type 'INITIAL-SIZE)))
    (lambda (table)
      (set-hash-table/count! table 0)
      (new-size! table (initial-size table) prime-numbers-stream))))

(define grow-table!
  (let ((get-primes (record-accessor type 'PRIMES)))
    (lambda (table count)
      (let ((old-buckets (hash-table/buckets table)))
	(new-size! table
		   (let ((size (hash-table/size table))
			 (growth-factor (hash-table/rehash-size table)))
		     (if (exact-integer? growth-factor)
			 (+ size
			    (* growth-factor
			       (integer-ceiling (- count size) growth-factor)))
			 (let loop ((size size))
			   (if (> count size)
			       (loop (* size growth-factor))
			       (round->exact size)))))
		   (get-primes table))
	(let ((buckets (hash-table/buckets table))
	      (key-hash (hash-table/key-hash table))
	      (entry-key (hash-table/entry-key table)))
	  (let ((old-n-buckets (vector-length old-buckets))
		(n-buckets (vector-length buckets)))
	    (let loop ((i 0))
	      (if (fix:< i old-n-buckets)
		  (begin
		    (let loop ((entries (vector-ref old-buckets i)))
		      (if (not (null? entries))
			  (let ((next (cdr entries))
				(hash (key-hash (entry-key (car entries))
						n-buckets)))
			    (set-cdr! entries (vector-ref buckets hash))
			    (vector-set! buckets hash entries)
			    (loop next))))
		    (loop (fix:1+ i)))))))))))

(define new-size!
  (let ((set-primes! (record-updater type 'PRIMES))
	(set-buckets! (record-updater type 'BUCKETS)))
    (lambda (table size primes)
      (set-hash-table/size! table size)
      (let ((primes
	     (let ((min-buckets
		    (ceiling->exact
		     (/ size (hash-table/rehash-threshold table)))))
	       (let loop ((primes primes))
		 (if (<= min-buckets (stream-car primes))
		     primes
		     (loop (stream-cdr primes)))))))
	(set-primes! table primes)
	(set-buckets! table (make-vector (stream-car primes) '()))))))

(define (check-arg object predicate default)
  (cond ((predicate object) object)
	((not object) default)
	(else (error:wrong-type-datum object false))))

;;;; Common Hash Table Constructors

(define (initialize-package!)
  (set! make-object-hash-table
	(hash-table/constructor (lambda (object modulus)
				  (modulo (hash object) modulus))
				eq?
				weak-cons
				weak-pair/car?
				weak-car
				weak-cdr
				weak-set-cdr!))
  (set! make-string-hash-table
	(hash-table/constructor string-hash-mod
				string=?
				cons
				true
				car
				cdr
				set-cdr!))
  (set! make-symbol-hash-table
	(hash-table/constructor symbol-hash-mod
				eq?
				cons
				true
				car
				cdr
				set-cdr!)))

(define make-object-hash-table)
(define make-string-hash-table)
(define make-symbol-hash-table)