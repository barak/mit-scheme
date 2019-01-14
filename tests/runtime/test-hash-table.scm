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

(define (generate-test-sequence state sequence-specification)
  (let ((state (make-random-state state))
	(length (sequence-specification/length sequence-specification))
	(key-radix (sequence-specification/key-radix sequence-specification))
	(insert-fraction
	 (exact->inexact
	  (sequence-specification/insert-fraction sequence-specification)))
	(delete-fraction
	 (exact->inexact
	  (sequence-specification/delete-fraction sequence-specification)))
	(tree (make-rb-tree fix:= fix:<)))
    (let ((delete-break (+ insert-fraction delete-fraction)))
      (let loop ((n 0) (s '()))
	(if (fix:= n length)
	    s
	    (loop (fix:+ n 1)
		  (cons (cons (let ((x (random 1. state)))
				(cond ((< x insert-fraction) 'insert)
				      ((< x delete-break) 'delete)
				      (else 'lookup)))
			      (let ((key (random key-radix state)))
				(or (rb-tree/lookup tree key #f)
				    (let ((pointer (cons key '())))
				      (rb-tree/insert! tree key pointer)
				      pointer))))
			s)))))))

(define (run-test-sequence s implementation)
  (let ((table ((implementation/make implementation)))
	(insert! (implementation/insert! implementation))
	(delete! (implementation/delete! implementation))
	(lookup (implementation/lookup implementation)))
    (do ((s s (cdr s)))
	((null? s))
      (cond ((eq? 'insert (caar s))
	     (insert! table (cdar s) #f))
	    ((eq? 'delete (caar s))
	     (delete! table (cdar s)))
	    (else
	     (lookup table (cdar s) #f))))
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

(define (make-pointer-tree)
  (make-rb-tree (lambda (x y) (fix:= (car x) (car y)))
		(lambda (x y) (fix:< (car x) (car y)))))

(define rbt
  (make-implementation make-pointer-tree
		       rb-tree/insert!
		       rb-tree/delete!
		       rb-tree/lookup
		       rb-tree->alist))

(load-option 'hash-table)

(define (make-hash-table-implementation constructor)
  (make-implementation constructor
		       hash-table-set!
		       hash-table-delete!
		       hash-table-ref/default
		       (lambda (table)
			 (sort (hash-table->alist table)
			       (lambda (x y) (fix:< (caar x) (caar y)))))))

(define (test-correctness s implementation)
  (let ((table ((implementation/make implementation)))
	(insert! (implementation/insert! implementation))
	(delete! (implementation/delete! implementation))
	(lookup (implementation/lookup implementation))
	(tree (make-pointer-tree)))
    (do ((s s (cdr s)))
	((null? s))
      (let ((operation (caar s))
	    (key (cdar s)))
	(cond ((eq? 'insert operation)
	       (rb-tree/insert! tree key #t)
	       (insert! table key #t))
	      ((eq? 'delete operation)
	       (rb-tree/delete! tree key)
	       (delete! table key))
	      (else
	       (let ((result (lookup table key #f)))
		 (if (not (eq? result (rb-tree/lookup tree key #f)))
		     (error "Incorrect lookup result:" result key)))))))
    (let loop
	((alist ((implementation/->alist implementation) table))
	 (check (rb-tree->alist tree)))
      (if (null? alist)
	  (if (not (null? check))
	      (error "Table has too few elements:" check))
	  (begin
	    (if (null? check)
		(error "Table has too many elements:" alist))
	    (if (not (and (eq? (caar alist) (caar check))
			  (eq? (cdar alist) (cdar check))))
		(error "Alist element incorrect:" (car alist) (car check)))
	    (loop (cdr alist) (cdr check)))))))

;;;; Correctness Tests

(define (check implementation)
  (let ((n (if keep-it-fast!? #x100 #x1000)))
    (do ((i 0 (+ i 1))) ((= i (if keep-it-fast!? #x10 #x100)))
      (let* ((key-radix (+ 1 (random-integer n)))
	     (insert-fraction (random-real))
	     (delete-fraction (- 1 insert-fraction)))
	(test-correctness
	 (make-sequence n key-radix insert-fraction delete-fraction)
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
						(cdr entry-type)))))))))
			entry-types))
	    hash-parameters))

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