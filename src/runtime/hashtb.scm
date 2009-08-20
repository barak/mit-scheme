#| -*-Scheme-*-

$Id$

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

;;;; Hash Tables
;;; package: (runtime hash-table)

(declare (usual-integrations))

;;;; Structures

(define-structure (hash-table-type
		   (type-descriptor <hash-table-type>)
		   (constructor make-table-type)
		   (conc-name table-type-))
  (key-hash #f read-only #t)
  (key=? #f read-only #t)
  (rehash-after-gc? #f read-only #t)
  (method:get #f read-only #t)
  (method:put! #f read-only #t)
  (method:modify! #f read-only #t)
  (method:remove! #f read-only #t)
  (method:clean! #f read-only #t)
  (method:rehash! #f read-only #t)
  (method:fold #f read-only #t)
  (method:copy-bucket #f read-only #t))

(define-guarantee hash-table-type "hash-table type")

(define-structure (hash-table
		   (type-descriptor <hash-table>)
		   (constructor make-table (type))
		   (conc-name table-)
		   (copier copy-table))
  (type #f read-only #t)

  ;; Parameters of the hash table.
  (rehash-threshold default-rehash-threshold)
  (rehash-size default-rehash-size)

  ;; Internal state variables.
  (count 0)
  (grow-size minimum-size)
  (shrink-size 0)
  buckets
  (primes prime-numbers-stream)
  (needs-rehash? #f)
  (initial-size-in-effect? #f))

(define-guarantee hash-table "hash table")

(define-integrable (increment-table-count! table)
  (set-table-count! table (fix:+ (table-count table) 1)))

(define-integrable (decrement-table-count! table)
  (set-table-count! table (fix:- (table-count table) 1)))

(define-integrable minimum-size 4)
(define-integrable default-rehash-threshold 1)
(define-integrable default-rehash-size 2.)

;;;; Table operations

(define ((hash-table-constructor type) #!optional initial-size)
  (%make-hash-table type initial-size))

(define (%make-hash-table type #!optional initial-size)
  (guarantee-hash-table-type type '%MAKE-HASH-TABLE)
  (let ((initial-size
	 (if (or (default-object? initial-size) (not initial-size))
	     #f
	     (begin
	       (guarantee-exact-nonnegative-integer initial-size
						    '%MAKE-HASH-TABLE)
	       initial-size))))
    (let ((table (make-table type)))
      (if (and initial-size (> initial-size minimum-size))
	  ;; If an initial size is given, it means that the table
	  ;; should be initialized with that usable size.  The
	  ;; table's usable size remains fixed at the initial size
	  ;; until the count exceeds the usable size, at which point
	  ;; normal table resizing takes over.
	  (begin
	    (set-table-grow-size! table initial-size)
	    (set-table-initial-size-in-effect?! table #t)))
      (reset-table! table)
      (if (table-type-rehash-after-gc? type)
	  (set! address-hash-tables (weak-cons table address-hash-tables)))
      table)))

(define (hash-table/type table)
  (guarantee-hash-table table 'HASH-TABLE/TYPE)
  (table-type table))

(define (hash-table/key-hash table)
  (guarantee-hash-table table 'HASH-TABLE/KEY-HASH)
  (table-type-key-hash (table-type table)))

(define (hash-table/key=? table)
  (guarantee-hash-table table 'HASH-TABLE/KEY=?)
  (table-type-key=? (table-type table)))

(define (hash-table/get table key default)
  (guarantee-hash-table table 'HASH-TABLE/GET)
  ((table-type-method:get (table-type table)) table key default))

(define (hash-table/lookup table key if-found if-not-found)
  (let ((datum (hash-table/get table key default-marker)))
    (if (eq? datum default-marker)
	(if-not-found)
	(if-found datum))))

(define (hash-table/put! table key datum)
  (guarantee-hash-table table 'HASH-TABLE/PUT!)
  ((table-type-method:put! (table-type table)) table key datum))

(define (hash-table/modify! table key procedure default)
  (guarantee-hash-table table 'HASH-TABLE/MODIFY!)
  ((table-type-method:modify! (table-type table)) table key procedure default))

(define (hash-table/intern! table key get-datum)
  (hash-table/modify! table
		      key
		      (lambda (datum)
			(if (eq? datum default-marker)
			    (get-datum)
			    datum))
		      default-marker))

(define (hash-table/remove! table key)
  (guarantee-hash-table table 'HASH-TABLE/REMOVE!)
  ((table-type-method:remove! (table-type table)) table key))

(define (hash-table/clean! table)
  (guarantee-hash-table table 'HASH-TABLE/CLEAN!)
  (with-table-locked! table
    (lambda ()
      ((table-type-method:clean! (table-type table)) table)
      (maybe-shrink-table! table))))

(define (hash-table/for-each table procedure)
  ;; It's difficult to make this more efficient because PROCEDURE is
  ;; allowed to delete the entry from the table, and if the table is
  ;; resized while being examined we'll lose our place.
  (for-each (lambda (p) (procedure (car p) (cdr p)))
	    (hash-table->alist table)))

(define (hash-table-fold table procedure initial-value)
  (guarantee-hash-table table 'HASH-TABLE-FOLD)
  ((table-type-method:fold (table-type table)) table procedure initial-value))

(define (hash-table->alist table)
  (hash-table-fold table
		   (lambda (key datum alist) (cons (cons key datum) alist))
		   '()))

(define (hash-table/key-list table)
  (hash-table-fold table
		   (lambda (key datum alist) datum (cons key alist))
		   '()))

(define (hash-table/datum-list table)
  (hash-table-fold table
		   (lambda (key datum alist) key (cons datum alist))
		   '()))

(define (hash-table/rehash-threshold table)
  (guarantee-hash-table table 'HASH-TABLE/REHASH-THRESHOLD)
  (table-rehash-threshold table))

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
    (with-table-locked! table
      (lambda ()
	(set-table-rehash-threshold! table threshold)
	(new-size! table (table-grow-size table))))))

(define (hash-table/rehash-size table)
  (guarantee-hash-table table 'HASH-TABLE/REHASH-SIZE)
  (table-rehash-size table))

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
    (with-table-locked! table
      (lambda ()
	(set-table-rehash-size! table size)
	(reset-shrink-size! table)
	(maybe-shrink-table! table)))))

(define (hash-table/count table)
  (guarantee-hash-table table 'HASH-TABLE/COUNT)
  (let loop ()
    (let ((count (table-count table)))
      (if (table-needs-rehash? table)
	  (begin
	    (rehash-table! table)
	    (loop))
	  count))))

(define (hash-table/size table)
  (guarantee-hash-table table 'HASH-TABLE/SIZE)
  (table-grow-size table))

(define (hash-table/clear! table)
  (guarantee-hash-table table 'HASH-TABLE/CLEAR!)
  (with-table-locked! table
    (lambda ()
      (if (not (table-initial-size-in-effect? table))
	  (set-table-grow-size! table minimum-size))
      (set-table-count! table 0)
      (reset-table! table))))

;;;; Weak table type

(define (weak-hash-table/constructor key-hash key=?
				     #!optional rehash-after-gc?)
  (hash-table-constructor
   (make-weak-hash-table-type key-hash key=?
			      (if (default-object? rehash-after-gc?)
				  #f
				  rehash-after-gc?))))

(define (make-weak-hash-table-type key-hash key=? rehash-after-gc?)
  (if rehash-after-gc?
      (make-weak-rehash-type key-hash key=?)
      (make-weak-no-rehash-type key-hash key=?)))

(define-integrable (make-weak-rehash-type key-hash key=?)
  (make-weak-type key-hash key=? #t (compute-address-hash key-hash)))

(define-integrable (make-weak-no-rehash-type key-hash key=?)
  (make-weak-type key-hash key=? #f (compute-non-address-hash key-hash)))

(define-integrable (make-weak-type key-hash key=? rehash-after-gc?
				   compute-hash!)
  (make-table-type key-hash key=? rehash-after-gc?
		   (make-method:get compute-hash! key=? %weak-entry-key
				    %weak-entry-datum)
		   (make-method:put! compute-hash! key=? %weak-make-entry
				     %weak-entry-key %weak-set-entry-datum!)
		   (make-method:modify! compute-hash! key=? %weak-make-entry
					%weak-entry-key %weak-entry-datum
					%weak-set-entry-datum!)
		   (make-method:remove! compute-hash! key=? %weak-entry-key)
		   weak-method:clean!
		   (make-method:rehash! key-hash %weak-entry-valid?
					%weak-entry-key)
		   (make-method:fold %weak-entry-valid? %weak-entry-key
				     %weak-entry-datum)
		   (make-method:copy-bucket %weak-entry-valid?
					    %weak-make-entry
					    %weak-entry-key
					    %weak-entry-datum)))

(define-integrable (%weak-make-entry key datum)
  (if (or (not key) (number? key))	;Keep numbers in table.
      (cons key datum)
      (system-pair-cons (ucode-type weak-cons) key datum)))

(define-integrable (%weak-entry-valid? entry)
  (or (pair? entry)
      (system-pair-car entry)))

(define-integrable %weak-entry-key system-pair-car)
(define-integrable %weak-entry-datum system-pair-cdr)
(define-integrable %weak-set-entry-datum! system-pair-set-cdr!)

(define (weak-method:clean! table)
  (let ((buckets (table-buckets table)))
    (let ((n-buckets (vector-length buckets)))
      (do ((i 0 (fix:+ i 1)))
	  ((not (fix:< i n-buckets)))
	(letrec
	    ((scan-head
	      (lambda (p)
		(if (pair? p)
		    (if (%weak-entry-key (car p))
			(begin
			  (vector-set! buckets i p)
			  (scan-tail (cdr p) p))
			(begin
			  (decrement-table-count! table)
			  (scan-head (cdr p))))
		    (vector-set! buckets i p))))
	     (scan-tail
	      (lambda (p q)
		(if (pair? p)
		    (if (%weak-entry-key (car p))
			(scan-tail (cdr p) p)
			(begin
			  (decrement-table-count! table)
			  (let loop ((p (cdr p)))
			    (if (pair? p)
				(if (%weak-entry-key (car p))
				    (begin
				      (set-cdr! q p)
				      (scan-tail (cdr p) p))
				    (begin
				      (decrement-table-count! table)
				      (loop (cdr p))))
				(set-cdr! q p)))))))))
	  (scan-head (vector-ref buckets i)))))))

;;;; Strong table type

(define (strong-hash-table/constructor key-hash key=?
				       #!optional rehash-after-gc?)
  (hash-table-constructor
   (make-strong-hash-table-type key-hash key=?
				(if (default-object? rehash-after-gc?)
				    #f
				    rehash-after-gc?))))

(define (make-strong-hash-table-type key-hash key=? rehash-after-gc?)
  (if rehash-after-gc?
      (make-strong-rehash-type key-hash key=?)
      (make-strong-no-rehash-type key-hash key=?)))

(define-integrable (make-strong-rehash-type key-hash key=?)
  (make-strong-type key-hash key=? #t (compute-address-hash key-hash)))

(define-integrable (make-strong-no-rehash-type key-hash key=?)
  (make-strong-type key-hash key=? #f (compute-non-address-hash key-hash)))

(define-integrable (make-strong-type key-hash key=? rehash-after-gc?
				     compute-hash!)
  (make-table-type key-hash key=? rehash-after-gc?
		   (make-method:get compute-hash! key=? %strong-entry-key
				    %strong-entry-datum)
		   (make-method:put! compute-hash! key=? %strong-make-entry
				     %strong-entry-key
				     %strong-set-entry-datum!)
		   (make-method:modify! compute-hash! key=?
					%strong-make-entry %strong-entry-key
					%strong-entry-datum
					%strong-set-entry-datum!)
		   (make-method:remove! compute-hash! key=?
					%strong-entry-key)
		   (lambda (table) table unspecific)
		   (make-method:rehash! key-hash %strong-entry-valid?
					%strong-entry-key)
		   (make-method:fold %strong-entry-valid?
				     %strong-entry-key
				     %strong-entry-datum)
		   (make-method:copy-bucket %strong-entry-valid?
					    %strong-make-entry
					    %strong-entry-key
					    %strong-entry-datum)))

(define-integrable %strong-make-entry cons)
(define-integrable (%strong-entry-valid? entry) entry #t)
(define-integrable %strong-entry-key car)
(define-integrable %strong-entry-datum cdr)
(define-integrable %strong-set-entry-datum! set-cdr!)

;;;; Methods

(define-integrable (make-method:get compute-hash! key=? entry-key entry-datum)
  (lambda (table key default)
    (let ((hash (compute-hash! table key)))
      (let loop ((p (vector-ref (table-buckets table) hash)))
	(if (pair? p)
	    (if (key=? (entry-key (car p)) key)
		(entry-datum (car p))
		(loop (cdr p)))
	    default)))))

(define-integrable (make-method:put! compute-hash! key=? make-entry entry-key
				     set-entry-datum!)
  (lambda (table key datum)
    (let ((hash (compute-hash! table key)))
      (let loop ((p (vector-ref (table-buckets table) hash)) (q #f))
	(if (pair? p)
	    (if (key=? (entry-key (car p)) key)
		(set-entry-datum! (car p) datum)
		(loop (cdr p) p))
	    (with-table-locked! table
	      (lambda ()
		(let ((r (cons (make-entry key datum) '())))
		  (if q
		      (set-cdr! q r)
		      (vector-set! (table-buckets table) hash r)))
		(increment-table-count! table)
		(maybe-grow-table! table))))))))

(define-integrable (make-method:modify! compute-hash! key=? make-entry
					entry-key entry-datum set-entry-datum!)
  (lambda (table key procedure default)
    (let ((hash (compute-hash! table key)))
      (let loop ((p (vector-ref (table-buckets table) hash)) (q #f))
	(if (pair? p)
	    (if (key=? (entry-key (car p)) key)
		(with-table-locked! table
		  (lambda ()
		    (let ((datum (procedure (entry-datum (car p)))))
		      (set-entry-datum! (car p) datum)
		      datum)))
		(loop (cdr p) p))
	    (let ((datum (procedure default)))
	      (with-table-locked! table
		(lambda ()
		  (let ((r (cons (make-entry key datum) '())))
		    (if q
			(set-cdr! q r)
			(vector-set! (table-buckets table) hash r)))
		  (increment-table-count! table)
		  (maybe-grow-table! table)))
	      datum))))))

(define-integrable (make-method:remove! compute-hash! key=? entry-key)
  (lambda (table key)
    (let ((hash (compute-hash! table key)))
      (let loop ((p (vector-ref (table-buckets table) hash)) (q #f))
	(if (pair? p)
	    (if (key=? (entry-key (car p)) key)
		(with-table-locked! table
		  (lambda ()
		    (if q
			(set-cdr! q (cdr p))
			(vector-set! (table-buckets table) hash (cdr p)))
		    (decrement-table-count! table)
		    (maybe-shrink-table! table)))
		(loop (cdr p) p)))))))

(define-integrable (make-method:rehash! key-hash entry-valid? entry-key)
  (lambda (table entries)
    (let ((buckets (table-buckets table)))
      (let ((n-buckets (vector-length buckets)))
	(let loop ((p entries))
	  (if (pair? p)
	      (let ((q (cdr p)))
		(if (entry-valid? (car p))
		    (let ((hash (key-hash (entry-key (car p)) n-buckets)))
		      (set-cdr! p (vector-ref buckets hash))
		      (vector-set! buckets hash p))
		    (decrement-table-count! table))
		(loop q))))))))

(define-integrable (make-method:fold entry-valid? entry-key entry-datum)
  (lambda (table procedure initial-value)
    (let ((buckets (table-buckets table)))
      (let ((n-buckets (vector-length buckets)))
	(let per-bucket ((i 0) (value initial-value))
	  (if (fix:< i n-buckets)
	      (let per-entry ((p (vector-ref buckets i)) (value value))
		(if (pair? p)
		    (per-entry (cdr p)
			       (if (entry-valid? (car p))
				   (procedure (entry-key (car p))
					      (entry-datum (car p))
					      value)
				   value))
		    (per-bucket (fix:+ i 1) value)))
	      value))))))

(define-integrable (make-method:copy-bucket entry-valid? make-entry
					    entry-key entry-datum)
  (lambda (bucket)
    (let find-head ((p bucket))
      (if (pair? p)
	  (if (entry-valid? (car p))
	      (let ((head
		     (cons (make-entry (entry-key (car p))
				       (entry-datum (car p)))
			   '())))
		(let loop ((p (cdr p)) (previous head))
		  (if (pair? p)
		      (loop (cdr p)
			    (if (entry-valid? (car p))
				(let ((p*
				       (cons (make-entry (entry-key (car p))
							 (entry-datum (car p)))
					     '())))
				  (set-cdr! previous p*)
				  p*)
				previous))))
		head)
	      (find-head (cdr p)))
	  p))))

;;;; Resizing

(define (maybe-grow-table! table)
  (if (> (table-count table) (table-grow-size table))
      (begin
	(let loop ((size (table-grow-size table)))
	  (if (> (table-count table) size)
	      (loop (increment-size table size))
	      (new-size! table size)))
	(set-table-initial-size-in-effect?! table #f))))

(define (maybe-shrink-table! table)
  (if (and (< (table-count table) (table-shrink-size table))
	   (not (table-initial-size-in-effect? table)))
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
    (let ((n-buckets (vector-length old-buckets))
	  (method (table-type-method:rehash! (table-type table))))
      (set-table-needs-rehash?! table #f)
      (do ((i 0 (fix:+ i 1)))
	  ((not (fix:< i n-buckets)))
	(method table (vector-ref old-buckets i))))
    (maybe-shrink-table! table)))

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

;;;; Address hashing

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

;;; The exception to this rule is COMPUTE-ADDRESS-HASH, which might
;;; have to shrink the table due to keys which have been reclaimed by
;;; the garbage collector.  REHASH-TABLE! explicitly checks for this
;;; possibility, and rehashes the table again if necessary.

(define-integrable (compute-non-address-hash key-hash)
  (lambda (table key)
    (key-hash key (vector-length (table-buckets table)))))

(define-integrable (compute-address-hash key-hash)
  (lambda (table key)
    (let loop ()
      (let ((hash (key-hash key (vector-length (table-buckets table)))))
	(if (table-needs-rehash? table)
	    (begin
	      (rehash-table! table)
	      (loop))
	    hash)))))

(define (rehash-table! table)
  (with-table-locked! table
    (lambda ()
      (let ((entries (extract-table-entries! table)))
	(set-table-needs-rehash?! table #f)
	((table-type-method:rehash! (table-type table)) table entries))
      (maybe-shrink-table! table))))

(define (extract-table-entries! table)
  (let ((buckets (table-buckets table)))
    (let ((n-buckets (vector-length buckets)))
      (do ((i 0 (fix:+ i 1))
	   (entries '()
		    (append! (let ((p (vector-ref buckets i)))
			       (vector-set! buckets i '())
			       p)
			     entries)))
	  ((not (fix:< i n-buckets)) entries)))))

;;;; EQ/EQV/EQUAL types

(declare (integrate eq-hash-mod))
(define (eq-hash-mod key modulus)
  (declare (integrate key modulus))
  (fix:remainder (eq-hash key) modulus))

(define-integrable (eq-hash object)
  (let ((n
	 ((ucode-primitive primitive-object-set-type)
	  (ucode-type positive-fixnum)
	  object)))
    (if (fix:< n 0)
	(fix:not n)
	n)))

(declare (integrate eqv-hash-mod))
(define-integrable (eqv-hash-mod key modulus)
  (declare (integrate key modulus))
  (int:remainder (eqv-hash key) modulus))

(define (eqv-hash key)
  (cond ((%bignum? key) (%bignum->nonneg-int key))
	((%ratnum? key) (%ratnum->nonneg-int key))
	((flo:flonum? key) (%flonum->nonneg-int key))
	((%recnum? key) (%recnum->nonneg-int key))
	(else (eq-hash key))))

(declare (integrate equal-hash-mod))
(define-integrable (equal-hash-mod key modulus)
  (declare (integrate key modulus))
  (int:remainder (equal-hash key) modulus))

(define (equal-hash key)
  (cond ((vector? key)
	 (let ((length (vector-length key)))
	   (do ((i 0 (fix:+ i 1))
		(accum 0 (int:+ accum (equal-hash (vector-ref key i)))))
	       ((not (fix:< i length)) accum))))
	((pair? key) (int:+ (equal-hash (car key)) (equal-hash (cdr key))))
	((cell? key) (equal-hash (cell-contents key)))
	((%bignum? key) (%bignum->nonneg-int key))
	((%ratnum? key) (%ratnum->nonneg-int key))
	((flo:flonum? key) (%flonum->nonneg-int key))
	((%recnum? key) (%recnum->nonneg-int key))
	((string? key) (string-hash key))
	((bit-string? key) (bit-string->unsigned-integer key))
	((pathname? key) (string-hash (->namestring key)))
	(else (eq-hash key))))

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

;;;; SRFI-69 compatability

(define (make-hash-table #!optional key=? key-hash initial-size)
  (%make-hash-table (custom-table-type
		     (if (default-object? key=?)
			 equal?
			 key=?)
		     (if (default-object? key-hash)
			 equal-hash-mod
			 key-hash))
		    initial-size))

(define (custom-table-type key=? key-hash)
  (cond ((and (eq? key=? eq?)
	      (or (eq? key-hash eq-hash-mod)
		  (eq? key-hash hash-by-identity)))
	 (make-weak-rehash-type eq-hash-mod eq?))
	((and (eq? key=? eqv?)
	      (eq? key-hash eqv-hash-mod))
	 (make-weak-rehash-type eqv-hash-mod eqv?))
	((and (eq? key=? equal?)
	      (or (eq? key-hash equal-hash-mod)
		  (eq? key-hash hash)))
	 (make-strong-rehash-type equal-hash-mod equal?))
	((and (or (eq? key=? string=?)
		  (eq? key=? string-ci=?))
	      (or (eq? key-hash string-hash-mod)
		  (eq? key-hash string-hash)
		  (eq? key-hash string-ci-hash)))
	 (make-strong-no-rehash-type (if (eq? key-hash string-hash)
					 string-hash-mod
					 key-hash)
				     key=?))
	(else
	 (make-strong-rehash-type key-hash key=?))))

(define (alist->hash-table alist #!optional key=? key-hash)
  (guarantee-alist alist 'ALIST->HASH-TABLE)
  (let ((table (make-hash-table key=? key-hash)))
    (for-each (lambda (p)
		(hash-table/put! table (car p) (cdr p)))
	      alist)
    table))

(define (hash key #!optional modulus)
  (if (default-object? modulus)
      (equal-hash key)
      (equal-hash-mod key modulus)))

(define (hash-by-identity key #!optional modulus)
  (if (default-object? modulus)
      (eq-hash key)
      (eq-hash-mod key modulus)))

(define (hash-table-exists? table key)
  (not (eq? (hash-table/get table key default-marker) default-marker)))

(define (hash-table-ref table key #!optional get-default)
  (let ((datum (hash-table/get table key default-marker)))
    (if (eq? datum default-marker)
	(begin
	  (if (default-object? get-default)
	      (error:bad-range-argument key 'HASH-TABLE-REF))
	  (get-default))
	datum)))

(define (hash-table-update! table key procedure #!optional get-default)
  (hash-table/modify! table
		      key
		      (if (default-object? get-default)
			  (lambda (datum)
			    (if (eq? datum default-marker)
				(error:bad-range-argument key
							  'HASH-TABLE-UPDATE!))
			    (procedure datum))
			  (lambda (datum)
			    (procedure (if (eq? datum default-marker)
					   (get-default)
					   datum))))
		      default-marker))

(define (hash-table-copy table)
  (guarantee-hash-table table 'HASH-TABLE-COPY)
  (with-table-locked! table
    (lambda ()
      (let ((table* (copy-table table))
	    (type (table-type table)))
	(set-table-buckets! table*
			    (vector-map (table-type-method:copy-bucket type)
					(table-buckets table)))
	(if (table-type-rehash-after-gc? type)
	    (set! address-hash-tables (weak-cons table* address-hash-tables)))
	table*))))

(define (hash-table-merge! table1 table2)
  (if (not (eq? table2 table1))
      (hash-table-fold table2
		       (lambda (key datum ignore)
			 ignore
			 (hash-table/put! table1 key datum))
		       unspecific))
  table1)

;;;; Miscellany

(define address-hash-tables)

(define eq-hash-table-type)
(define strong-eq-hash-table-type)
(define eqv-hash-table-type)
(define strong-eqv-hash-table-type)
(define equal-hash-table-type)
(define string-hash-table-type)

(define make-eq-hash-table)
(define make-strong-eq-hash-table)
(define make-eqv-hash-table)
(define make-strong-eqv-hash-table)
(define make-equal-hash-table)
(define make-string-hash-table)
(define make-symbol-hash-table)
(define make-object-hash-table)

(define (initialize-package!)
  (set! address-hash-tables '())
  (add-primitive-gc-daemon! mark-address-hash-tables!)
  (set! eq-hash-table-type
	(make-weak-rehash-type eq-hash-mod eq?))
  (set! strong-eq-hash-table-type
	(make-strong-rehash-type eq-hash-mod eq?))
  (set! eqv-hash-table-type
	(make-weak-rehash-type eqv-hash-mod eqv?))
  (set! strong-eqv-hash-table-type
	(make-strong-rehash-type eqv-hash-mod eqv?))
  (set! equal-hash-table-type
	(make-strong-rehash-type equal-hash-mod equal?))
  (set! string-hash-table-type
	(make-strong-no-rehash-type string-hash-mod string=?))
  (set! make-eq-hash-table
	(hash-table-constructor eq-hash-table-type))
  (set! make-strong-eq-hash-table
	(hash-table-constructor strong-eq-hash-table-type))
  (set! make-eqv-hash-table
	(hash-table-constructor eqv-hash-table-type))
  (set! make-strong-eqv-hash-table
	(hash-table-constructor strong-eqv-hash-table-type))
  (set! make-equal-hash-table
	(hash-table-constructor equal-hash-table-type))
  (set! make-string-hash-table
	(hash-table-constructor string-hash-table-type))
  ;; Define old names for compatibility:
  (set! make-symbol-hash-table make-eq-hash-table)
  (set! make-object-hash-table make-eqv-hash-table)
  unspecific)

(define (mark-address-hash-tables!)
  (let loop ((previous #f) (tables address-hash-tables))
    (if (system-pair? tables)
	(if (system-pair-car tables)
	    (begin
	      (set-table-needs-rehash?! (system-pair-car tables) #t)
	      (loop tables (system-pair-cdr tables)))
	    (begin
	      (if previous
		  (system-pair-set-cdr! previous (system-pair-cdr tables))
		  (set! address-hash-tables (system-pair-cdr tables)))
	      (loop previous (system-pair-cdr tables)))))))

(define (check-arg object default predicate description procedure)
  (cond ((predicate object) object)
	((not object) default)
	(else (error:wrong-type-argument object description procedure))))

(define-integrable (with-table-locked! table thunk)
  table
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (let ((value (thunk)))
      (set-interrupt-enables! interrupt-mask)
      value)))

(define default-marker
  (list 'DEFAULT-MARKER))