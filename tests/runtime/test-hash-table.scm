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

;;;; Tests of hash-table implementation

(declare (usual-integrations))

(define-structure (implementation (conc-name implementation/))
  (make #f read-only #t)
  (insert! #f read-only #t)
  (delete! #f read-only #t)
  (lookup #f read-only #t)
  (->alist #f read-only #t))

(define-structure (sequence-specification
		   (conc-name sequence-specification/))
  (length #f read-only #t)
  (key-radix #f read-only #t)
  (key-type #f read-only #t)
  (insert-fraction #f read-only #t)
  (delete-fraction #f read-only #t))

(define (run-test sequence-specification implementation)
  (run-sequence (sequence-specification->sequence sequence-specification)
		implementation))

(define (run-sequence s implementation)
  (let ((start-time (process-time-clock)))
    (run-test-sequence s implementation)
    (cons (- (process-time-clock) start-time)
	  (test-sequence-overhead s))))

(define (make-sequence . arguments)
  (sequence-specification->sequence
   (apply make-sequence-specification arguments)))

(define (sequence-specification->sequence sequence-specification)
  (generate-test-sequence (make-random-state #t)
			  sequence-specification))

(define (generate-test-sequence state spec)
  (let ((state (make-random-state state))
	(length (sequence-specification/length spec))
	(key-radix (sequence-specification/key-radix spec))
	(key-type (sequence-specification/key-type spec))
	(insert-fraction
	 (exact->inexact
	  (sequence-specification/insert-fraction spec)))
	(delete-fraction
	 (exact->inexact
	  (sequence-specification/delete-fraction spec))))

    (define random-int
      (random-source-make-integers state))

    (define (make-scalar-key)
      (random key-radix state))

    (define (make-list-key)
      (let loop ((n (random-int 10)) (key '()))
	(if (> n 0)
	    (loop (- n 1) (cons (make-scalar-key) key))
	    key)))

    (define (make-weak-list-key)
      (let loop ((n (random-int 10)) (key '()))
	(if (> n 0)
	    (loop (- n 1) (weak-cons (make-scalar-key) key))
	    key)))

    (let-values (((key= key<) (sequence-specification/predicates spec)))
      (let ((make-key
	     (case key-type
	       ((scalar) make-scalar-key)
	       ((list) make-list-key)
	       ((weak-list) make-weak-list-key)
	       (else (error "Unrecognized key type:" key-type))))
	    (delete-break (+ insert-fraction delete-fraction))
	    (tree (make-rb-tree key= key<)))
	(let loop ((n 0) (s '()))
	  (if (fix:= n length)
	      s
	      (loop (fix:+ n 1)
		    (cons (cons (let ((x (random 1. state)))
				  (cond ((< x insert-fraction) 'insert)
					((< x delete-break) 'delete)
					(else 'lookup)))
				(let ((key (make-key)))
				  (or (rb-tree/lookup tree key #f)
				      (let ((pointer (cons key '())))
					(rb-tree/insert! tree key pointer)
					pointer))))
			  s))))))))

(define (sequence-specification/predicates spec)

  (define (list= l1 l2)
    (if (null-list? l1)
	(null-list? l2)
	(and (not (null-list? l2))
	     (fix:= (car l1) (car l2))
	     (list= (cdr l1) (cdr l2)))))

  (define (list< l1 l2)
    (and (not (null-list? l2))
	 (or (null-list? l1)
	     (fix:< (car l1) (car l2))
	     (and (fix:= (car l1) (car l2))
		  (list< (cdr l1) (cdr l2))))))

  (define (weak-list= l1 l2)
    (if (null-weak-list? l1)
	(null-weak-list? l2)
	(and (not (null-weak-list? l2))
	     (fix:= (weak-car l1) (weak-car l2))
	     (weak-list= (weak-cdr l1) (weak-cdr l2)))))

  (define (weak-list< l1 l2)
    (and (not (null-weak-list? l2))
	 (or (null-weak-list? l1)
	     (fix:< (weak-car l1) (weak-car l2))
	     (and (fix:= (weak-car l1) (weak-car l2))
		  (weak-list< (weak-cdr l1) (weak-cdr l2))))))

  (case (sequence-specification/key-type spec)
    ((scalar) (values fix:= fix:<))
    ((list) (values list= list<))
    ((weak-list) (values weak-list= weak-list<))
    (else (error "Unrecognized key type:" key-type))))

(define (run-test-sequence s implementation)
  (let ((table ((implementation/make implementation)))
	(insert! (implementation/insert! implementation))
	(delete! (implementation/delete! implementation))
	(lookup (implementation/lookup implementation)))
    (for-each (lambda (step)
		(let ((operator (car step))
		      (key (cadr step)))
		  (case operator
		    ((insert) (insert! table key #f))
		    ((delete) (delete! table key))
		    (else (lookup table key #f)))))
	      s)
    table))

(define (test-sequence-overhead s)
  (let ((start-time (process-time-clock)))
    (run-test-sequence s dummy-implementation)
    (let ((end-time (process-time-clock)))
      (- end-time start-time))))

(define dummy-implementation
  (make-implementation
   (lambda () unspecific)
   (lambda (table key datum) table key datum unspecific)
   (lambda (table key) table key unspecific)
   (lambda (table key default) table key default unspecific)
   (lambda (table) table '())))

(load-option 'rb-tree)

(define (make-pointer-tree spec)
  (let-values (((key= key<) (sequence-specification/predicates spec)))
    (if (eq? 'scalar (sequence-specification/predicates spec))
	(make-rb-tree (lambda (x y) (key= (car x) (car y)))
		      (lambda (x y) (key< (car x) (car y))))
	(make-rb-tree key= key<))))

(load-option 'hash-table)

(define (make-hash-table-implementation constructor)
  (make-implementation constructor
		       hash-table-set!
		       hash-table-delete!
		       hash-table-ref/default
		       hash-table->alist))

(define (test-correctness spec implementation)
  (let-values (((key= key<) (sequence-specification/predicates spec)))
    (let ((table ((implementation/make implementation)))
	  (insert! (implementation/insert! implementation))
	  (delete! (implementation/delete! implementation))
	  (lookup (implementation/lookup implementation))
	  (tree
	   (if (eq? 'scalar (sequence-specification/key-type spec))
	       (make-rb-tree (lambda (x y) (key= (car x) (car y)))
			     (lambda (x y) (key< (car x) (car y))))
	       (make-rb-tree key= key<)))
	  (get-ptr
	   (if (eq? 'scalar (sequence-specification/key-type spec))
	       cdr
	       cadr)))
      (for-each (lambda (step)
		  (let ((operator (car step))
			(ptr (get-ptr step)))
		    (case operator
		      ((insert)
		       (rb-tree/insert! tree ptr #t)
		       (insert! table ptr #t))
		      ((delete)
		       (rb-tree/delete! tree ptr)
		       (delete! table ptr))
		      (else
		       (assert-equal (lookup table ptr #f)
				     (rb-tree/lookup tree ptr #f)
				     'expression `(lookup ,ptr))))))
		(sequence-specification->sequence spec))
      (assert-lset= (lambda (a b)
		      (and (eq? (car a) (car b))
			   (eq? (cdr a) (cdr b))))
		    ((implementation/->alist implementation) table)
		    (rb-tree->alist tree)))))

;;;; Correctness Tests

(define (check implementation key-type)
  (let ((n (if keep-it-fast!? #x100 #x1000)))
    (do ((i 0 (+ i 1))) ((= i (if keep-it-fast!? #x10 #x100)))
      (let* ((key-radix (+ 1 (random-integer n)))
	     (insert-fraction (random-real))
	     (delete-fraction (- 1 insert-fraction)))
	(test-correctness
	 (make-sequence-specification n key-radix key-type insert-fraction
				      delete-fraction)
	 implementation)))))

(define (integer-hash-mod integer modulus)
  (int:remainder (if (int:< integer 0) (int:- 0 integer) integer) modulus))

(let ((hash-parameters
       (list (list 'eq eq-hash-mod eq? #t)
	     (list 'eqv eqv-hash-mod eqv? #t)
	     (list 'equal equal-hash-mod equal? #t)
	     (list 'integer
		   (lambda (x modulus) (integer-hash-mod (car x) modulus))
		   (lambda (x y) (int:= (car x) (car y)))
		   #f)))
      (entry-types
       (list (list 'strong hash-table-entry-type:strong)
	     (list 'key-weak hash-table-entry-type:key-weak)
	     (list 'datum-weak hash-table-entry-type:datum-weak)
	     (list 'key&datum-weak hash-table-entry-type:key&datum-weak)
	     (list 'key-ephemeral hash-table-entry-type:key-ephemeral)
	     (list 'datum-ephemeral hash-table-entry-type:datum-ephemeral)
	     (list 'key&datum-ephemeral
		   hash-table-entry-type:key&datum-ephemeral))))
  (for-each (lambda (hash-parameters)
	      (for-each (lambda (entry-type)
			  (define-test
			    (symbol 'correctness-vs-rb:
				    (car entry-type)
				    '-
				    (car hash-parameters))
			    (lambda ()
			      (check
			       (make-hash-table-implementation
				(hash-table-constructor
				 (apply make-hash-table-type
					(append (cdr hash-parameters)
						(cdr entry-type)))))
			       'scalar))))
			entry-types))
	    hash-parameters))

(define (comparator->ht-impl comparator . args)
  (make-hash-table-implementation
   (apply hash-table-constructor comparator args)))

(define-test 'correctness-vs-rb:strong-list-eq-hash-table
  (lambda ()
    (check (comparator->ht-impl
	    (uniform-list-comparator (make-eq-comparator)))
	   'list)))

(define-test 'correctness-vs-rb:strong-list-eqv-hash-table
  (lambda ()
    (check (comparator->ht-impl
	    (uniform-list-comparator (make-eqv-comparator)))
	   'list)))

(define-test 'correctness-vs-rb:key-weak-list-eq-hash-table
  (lambda ()
    (check (comparator->ht-impl
	    (uniform-weak-list-comparator (make-eq-comparator)))
	   'weak-list)))

(define-test 'correctness-vs-rb:key-weak-list-eqv-hash-table
  (lambda ()
    (check (comparator->ht-impl
	    (uniform-weak-list-comparator (make-eqv-comparator)))
	   'weak-list)))

#|
(define-test 'correctness-vs-rb:strong-lset-eq-hash-table
  (lambda ()
    (check (comparator->ht-impl (lset-comparator (make-eq-comparator)))
	   'lset)))

(define-test 'correctness-vs-rb:strong-lset-eqv-hash-table
  (lambda ()
    (check (comparator->ht-impl (lset-comparator (make-eqv-comparator)))
	   'lset)))

(define-test 'correctness-vs-rb:key-weak-lset-eq-hash-table
  (lambda ()
    (check (comparator->ht-impl (weak-lset-comparator (make-eq-comparator)))
	   'weak-lset)))

(define-test 'correctness-vs-rb:key-weak-lset-eqv-hash-table
  (lambda ()
    (check (comparator->ht-impl (weak-lset-comparator (make-eqv-comparator)))
	   'weak-lset)))
|#

;;;; Regression Tests

;;; These are carefully tailored to the internal representation of
;;; the hash table.  This is simpler, but less robust, than writing a
;;; big, hairy, complicated statistical test that guarantees the
;;; desired behaviour with high probability.

(define (regression-make-table entry-type)
  ((hash-table-constructor
    (make-hash-table-type (lambda (k m) k m 0) eqv? #f entry-type))))

(define-test 'regression:false-key-of-broken-weak-entry
  (lambda ()
    (let ((hash-table (regression-make-table hash-table-entry-type:key-weak)))
      (hash-table-set! hash-table (cons 0 0) 'lose)
      (gc-flip)
      (assert-eqv (hash-table-ref/default hash-table #f 'win) 'win))))

(define-test 'regression:gc-reclaimed-key-of-broken-weak-entry
  (lambda ()
    (let ((hash-table (regression-make-table hash-table-entry-type:key-weak)))
      (hash-table-set! hash-table (cons 0 0) 'lose)
      (gc-flip)
      (assert-eqv (hash-table-ref/default hash-table (gc-reclaimed-object) 'win)
		  'win))))

(define-test 'regression:modification-during-srfi-69-update
  (lambda ()
    (let ((hash-table (regression-make-table hash-table-entry-type:strong)))
      (hash-table-set! hash-table 0 'lose-0)
      (hash-table-update! hash-table 0
	(lambda (datum)
	  (declare (ignore datum))
	  ;; Force consing a new entry.
	  (hash-table-delete! hash-table 0)
	  (hash-table-set! hash-table 0 'lose-1)
	  'win))
      (assert-eqv (hash-table-ref/default hash-table 0 'lose-2) 'win))))

(define-test 'regression:modification-during-srfi-69-update/default/0
  (lambda ()
    (let ((hash-table (regression-make-table hash-table-entry-type:strong)))
      (hash-table-set! hash-table 0 'lose-0)
      (hash-table-update!/default hash-table 0
        (lambda (datum)
          (declare (ignore datum))
          ;; Force consing a new entry.
          (hash-table-delete! hash-table 0)
          (hash-table-set! hash-table 0 'lose-1)
          'win)
        'lose-2)
      (assert-eqv (hash-table-ref/default hash-table 0 'lose-3) 'win))))

(define-test 'regression:modification-during-srfi-69-update/default/1
  (lambda ()
    (let ((hash-table (regression-make-table hash-table-entry-type:strong)))
      (hash-table-update!/default hash-table 0
	(lambda (datum)
	  (declare (ignore datum))
	  (hash-table-set! hash-table 1 'win-1)
	  'win-0)
        'lose-0a)
      (assert-eqv (hash-table-ref/default hash-table 0 'lose-0b) 'win-0)
      (assert-eqv (hash-table-ref/default hash-table 1 'lose-1) 'win-1))))

(define-test 'regression:modification-during-srfi-69-fold
  (lambda ()
    (let* ((index 1)
	   (hash-table (regression-make-table hash-table-entry-type:strong)))
      (hash-table-set! hash-table 0 0)
      (hash-table-set! hash-table 1 1)
      (assert-eqv (hash-table-fold hash-table
				   (lambda (key datum count)
				     key datum ;ignore
				     (set! index 0)
				     ;; Force a rehash.
				     (gc-flip)
				     (hash-table-ref/default hash-table 0 #f)
				     (+ count 1))
				   0)
		  2))))