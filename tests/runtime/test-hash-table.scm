#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020 Massachusetts Institute of Technology

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

;;;; From here on translated from the SRFI 125 tests

;;; Some of this is simple translation, other parts are complete rewrites.
;;; Since IANAL, I don't know what the copyright status of the end result.
;;; Original copyright:

;;; Copyright (C) William D Clinger 2015. All Rights Reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so,
;;; subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(define ht-default
  (make-hash-table (make-default-comparator)))

(define ht-default2
  (hash-table (make-default-comparator)
	      'foo 'bar
	      101.3 "fever"
	      '(x y z) '#()))

(define ht-eq
  (make-hash-table (make-eq-comparator)
		   'random-argument "another"))

(define ht-eq2
  (make-hash-table (make-eq-comparator)))

(define ht-eqv
  (make-hash-table (make-eqv-comparator)))

(define ht-eqv2
  (make-hash-table (make-eqv-comparator)))

(define ht-equal
  (make-hash-table (make-equal-comparator)))

(define ht-equal2
  (alist->hash-table '(((edward) . abbey)
                       ((dashiell) . hammett)
                       ((edward) . teach)
                       ((mark) . twain))
		     (make-equal-comparator)))

(define ht-string
  (make-hash-table (string-comparator)))

(define ht-string2
  (hash-table-unfold (lambda (s) (= 0 (string-length s)))
                     (lambda (s) (values s (string-length s)))
                     (lambda (s) (substring s 0 (- (string-length s) 1)))
                     "prefixes"
                     (string-comparator)
                     'ignored1 'ignored2 "ignored3" '#(ignored 4 5)))

(define ht-string-ci
  (make-hash-table (string-ci-comparator)))

(define ht-string-ci2
  (alist->hash-table '(("" . 0) ("Mary" . 4) ("Paul" . 4) ("Peter" . 5))
                     (string-ci-comparator)
                     "ignored1" 'ignored2))

(define ht-symbol
  (make-hash-table (symbol-comparator)))

(define ht-symbol2
  (alist->hash-table '((mary . travers) (noel . stookey) (peter .yarrow))
                     (symbol-comparator)))

(define ht-fixnum
  (make-hash-table (fixnum-comparator)))

(define ht-fixnum2
  (let ((table (make-hash-table (fixnum-comparator))))
    (for-each (lambda (i)
		(hash-table-set! table (* i i) i))
	      (iota 10))
    ;; Would be immutable if we supported this.
    (hash-table-copy table)))

(define test-tables
  (list ht-default ht-default2
	ht-eq ht-eq2
	ht-eqv ht-eqv2
	ht-equal ht-equal2
	ht-string ht-string2
	ht-string-ci ht-string-ci2
	ht-symbol ht-symbol2
	ht-fixnum ht-fixnum2))

(define test-data-1
  `((,ht-default foo err)
    (,ht-default2 101.3 "fever")
    (,ht-eq x err)
    (,ht-eq2 "y" err)
    (,ht-eqv (14 15) err)
    (,ht-eqv2 #\newline err)
    (,ht-equal (edward) err)
    (,ht-equal2 (mark) twain)
    (,ht-string "p" err)
    (,ht-string2 "pref" 4)
    (,ht-string-ci "mike" err)
    (,ht-string-ci2 "PAUL" 4)
    (,ht-symbol jane err)
    (,ht-symbol2 noel stookey)
    (,ht-fixnum 0 err)
    (,ht-fixnum2 4 2)))

(define (errval? object)
  (eq? 'err object))

(define (errval)
  'err)

;;;; Predicates

(define-test 'hash-table?
  (lambda ()
    (assert-false (hash-table? '#()))
    (assert-false (hash-table? (make-default-comparator)))
    (for-each (lambda (ht)
		(assert-true (hash-table? ht)))
	      test-tables)))

(define-test 'hash-table-contains?
  (lambda ()
    (for-each (lambda (e)
		((if (errval? (caddr e)) assert-false assert-true)
		 (hash-table-contains? (car e) (cadr e))))
	      test-data-1)
    (for-each (lambda (e)
		(assert-false (hash-table-contains? (car e) (cadr e))))
	      `((,ht-default #u8())
		(,ht-default2 47.9)
		(,ht-eq #())
		(,ht-eq2 ())
		(,ht-eqv foo)
		(,ht-eqv2 bar)
		(,ht-equal 19)
		(,ht-equal2 (henry))
		(,ht-string "p")
		(,ht-string2 "perp")
		(,ht-string-ci "mike")
		(,ht-string-ci2 "Noel")
		(,ht-symbol jane)
		(,ht-symbol2 paul)
		(,ht-fixnum 0)
		(,ht-fixnum2 5)))))

(define-test 'hash-table-empty?
  (lambda ()
    (for-each (lambda (e)
		((if (cadr e) assert-true assert-false)
		 (hash-table-empty? (car e))))
	      `((,ht-default #t)
		(,ht-default2 #f)
		(,ht-eq #t)
		(,ht-eq2 #t)
		(,ht-eqv #t)
		(,ht-eqv2 #t)
		(,ht-equal #t)
		(,ht-equal2 #f)
		(,ht-string #t)
		(,ht-string2 #f)
		(,ht-string-ci #t)
		(,ht-string-ci2 #f)
		(,ht-symbol #t)
		(,ht-symbol2 #f)
		(,ht-fixnum #t)
		(,ht-fixnum2 #f)))))

(define-test 'hash-table=?
  (lambda ()
    (for-each (lambda (ht1 ht2)
		(hash-table=? (make-default-comparator) ht1 ht2))
	      test-tables
	      test-tables)
    (for-each (lambda (e)
		((if (caddr e) assert-true assert-false)
		 (hash-table=? (make-default-comparator) (car e) (cadr e))))
	      `((,ht-default ,ht-default2 #f)
		(,ht-default2 ,ht-default #f)
		(,ht-eq ,ht-eq2 #t)
		(,ht-eq2 ,ht-eq #t)
		(,ht-eqv ,ht-eqv2 #t)
		(,ht-eqv2 ,ht-eqv #t)
		(,ht-equal ,ht-equal2 #f)
		(,ht-equal2 ,ht-equal #f)
		(,ht-string ,ht-string2 #f)
		(,ht-string2 ,ht-string #f)
		(,ht-string-ci ,ht-string-ci2 #f)
		(,ht-string-ci2 ,ht-string-ci #f)
		(,ht-symbol ,ht-symbol2 #f)
		(,ht-symbol2 ,ht-symbol #f)
		(,ht-fixnum ,ht-fixnum2 #f)
		(,ht-fixnum2 ,ht-fixnum #f)))))

(define-test 'hash-table-mutable?
  (lambda ()
    (for-each (lambda (ht)
		(assert-true (hash-table-mutable? ht)))
	      test-tables)
    (for-each (lambda (ht)
		;; Would be false if we had immutable tables.
		(assert-true (hash-table-mutable? (hash-table-copy ht))))
	      test-tables)
    (assert-true (hash-table-mutable? (hash-table-copy ht-fixnum2 #t)))))

;;;; Accessors

(define-test 'hash-table-ref
  (lambda ()
    (for-each (lambda (ht)
		(assert-error (lambda () (hash-table-ref ht 'not-a-key)))
		(assert-eq (guard (exn (else (errval)))
			     (hash-table-ref ht 'not-a-key errval))
			   (errval))
		(assert-eq (guard (exn (else (errval)))
			     (hash-table-ref ht 'not-a-key errval cons))
			   (errval))
		(assert-eq (guard (exn (else (errval)))
			     (hash-table-ref/default ht 'not-a-key (errval)))
			   (errval)))
	      test-tables)
    (for-each (lambda (e)
		(let ((ht (car e))
		      (key (cadr e))
		      (expected (caddr e)))
		  (if (errval? expected)
		      (assert-error (lambda () (hash-table-ref ht key)))
		      (assert-equal (hash-table-ref ht key) expected))
		  (assert-equal (hash-table-ref ht key errval)
				expected)
		  (assert-equal (hash-table-ref ht key errval list)
				(if (errval? expected)
				    expected
				    (list expected)))))
	      test-data-1)))

(define-test 'hash-table-ref/default
  (lambda ()
    (for-each (lambda (ht)
		(assert-eq (guard (exn (else (errval)))
			     (hash-table-ref/default ht 'not-a-key (errval)))
			   (errval)))
	      test-tables)
    (for-each (lambda (e)
		(let ((ht (car e))
		      (key (cadr e))
		      (expected (caddr e)))
		  (assert-equal (hash-table-ref/default ht key (errval))
				expected)))
	      test-data-1)))

(define-test 'delete-value
  (lambda ()
    (let ((ht (hash-table (number-comparator) 1 1 4 2 9 3 16 4 25 5 64 8)))
      (assert-= (hash-table-delete! ht) 0)
      (assert-= (hash-table-delete! ht 2 7 2000) 0)
      (assert-= (hash-table-delete! ht 1 2 4 7 64 2000) 3)
      (assert-= (hash-table-size ht) 3)
      (assert-= (length (hash-table-keys ht)) 3))))

(define (ht-tracker ht key=)
  (let ((kvs '()))

    (define (get-ht)
      ht)

    (define (apply-set! . sets)
      (set! kvs
	    (append (remove (lambda (kv)
			      (assoc (car kv) sets key=))
			    kvs)
		    sets))
      (apply hash-table-set! ht (append-map (lambda (x) x) kvs)))

    (define (apply-delete! . deletes)
      (set! kvs
	    (remove (lambda (kv)
		      (member (car kv) deletes key=))
		    kvs))
      (apply hash-table-delete! ht deletes))

    (define (apply-intern! . interns)
      (set! kvs
	    (fold (lambda (intern acc)
		    (if (assoc (car intern) acc key=)
			acc
			(cons intern acc)))
		  kvs
		  interns))
      (for-each (lambda (intern)
		  (hash-table-intern! ht
				      (car intern)
				      (lambda () (cadr intern))))
		interns))

    (define (apply-prune! predicate)
      (set! kvs
	    (remove (lambda (kv)
		      (predicate (car kv) (cadr kv)))
		    kvs))
      (hash-table-prune! predicate ht))

    (define (apply-update! key updater fail)
      (set! kvs
	    (if (assoc key kvs)
		(map (lambda (kv)
		       (if (key= key (car kv))
			   (list (car kv) (updater (cadr kv)))
			   kv))
		     kvs)
		(cons (list key (updater (fail)))
		      kvs)))
      (hash-table-update! ht key updater fail))

    (define (apply-update!/default key updater default)
      (set! kvs
	    (if (assoc key kvs)
		(map (lambda (kv)
		       (if (key= key (car kv))
			   (list (car kv) (updater (cadr kv)))
			   kv))
		     kvs)
		(cons (list key (updater default))
		      kvs)))
      (hash-table-update!/default ht key updater default))

    (define (expected-keys)
      (map car kvs))

    (define (expected-values)
      (map cadr kvs))

    (define (expected-alist)
      kvs)

    (define (check keys)
      (if (not (lset= key= (expected-keys) keys))
	  (error "ht-tracker error:" keys (expected-keys)))
      (assert-lset= key= (hash-table-keys ht) keys)
      (for-each (lambda (kv)
		  (assert-equal (hash-table-ref/default ht (car kv) (errval))
				(cadr kv)))
		kvs))

    (bundle #f
	    get-ht apply-set! apply-delete! apply-intern!
	    apply-prune! apply-update! apply-update!/default
	    expected-keys expected-values expected-alist
	    check)))

(define (setup-tracker comparator . args)
  (ht-tracker (apply make-hash-table comparator args)
	      (comparator-equality-predicate comparator)))

(define (test-seq-runner tracker steps)
  (let ((n (vector-length steps))
	(index 0)
	(keys '()))

    (define (run-step)
      (if (not (< index n))
	  (error "No more steps."))
      (let ((op (car (vector-ref steps index)))
	    (map-keys (cadr (vector-ref steps index))))
	(let ((keys* (map-keys keys (cdr op))))
	  (set! index (+ index 1))
	  (set! keys keys*)
	  (apply tracker op)
	  (tracker 'check keys*))))

    (define (run-steps k)
      (if (> k 0)
	  (begin
	    (run-step)
	    (run-steps (- k 1)))))

    (define (run-remaining-steps)
      (if (< index n)
	  (begin
	    (run-step)
	    (run-remaining-steps))))

    (bundle #f run-step run-steps run-remaining-steps)))

(define (map-keys:set! keys sets)
  (lset-union = keys (map car sets)))

(define (map-keys:delete! keys deletes)
  (lset-difference = keys deletes))

(define (map-keys:update! keys args)
  (lset-adjoin = keys (car args)))

(define-test 'test-sequence-one
  (lambda ()
    (let ((tracker (setup-tracker (fixnum-comparator))))
      (let ((runner (test-seq-runner tracker ts1))
	    (ht (tracker 'get-ht)))
	(runner 'run-steps 4)

	(runner 'run-step)
	(assert-lset= equal?
		      (hash-table-map->list vector ht)
		      (map vector
			   (tracker 'expected-keys)
			   (tracker 'expected-values)))
	(runner 'run-step)
	(assert-lset= equal?
		      (hash-table-map->list list ht)
		      (tracker 'expected-alist))

	(runner 'run-step)
	(assert-lset= equal?
		      (hash-table-map->list list ht)
		      (tracker 'expected-alist))

	(runner 'run-steps 5)
	(let ((ht* (hash-table-copy ht #t)))
	  (let-values (((key value) (hash-table-pop! ht*)))
	    (assert-= key (square value))
	    (assert-false (hash-table-contains? ht* key))
	    (assert-= (hash-table-size ht*) (- (hash-table-size ht) 1))))

	(runner 'run-remaining-steps)
	(assert-lset= equal? (hash-table-map->list list ht) ts1-result)
	;; Make sure that tracker is working correctly:
	(assert-lset= equal? (tracker 'expected-alist) ts1-result)))))

(define ts1
  (vector
   (list '(apply-set! (121 11) (144 12) (169 13))
	 map-keys:set!)
   (list '(apply-set! (0 0) (1 1) (4 2) (9 3) (16 4)
		      (25 5) (36 6) (49 7) (64 8) (81 9))
	 map-keys:set!)
   (list '(apply-delete! 1 9 25 49 81 200 121 169 81 1)
	 map-keys:delete!)
   (list '(apply-delete! 200 100 0 81 36)
	 map-keys:delete!)
   (list '(apply-intern! (169 13) (121 11) (0 0) (1 1) (1 99) (121 66))
	 map-keys:set!)
   (list `(apply-prune! ,(lambda (key value) (and (odd? key) (> value 10))))
	 (lambda (keys args)
	   (declare (ignore keys args))
	   '(0 1 4 16 64 144)))
   (list '(apply-intern! (169 13) (144 9999) (121 11))
	 map-keys:set!)
   (list `(apply-update! 9 ,length ,(lambda () '(a b c)))
	 map-keys:update!)
   (list `(apply-update! 16 ,- ,(default-object))
	 map-keys:update!)
   (list `(apply-update! 16 ,- ,abs)
	 map-keys:update!)
   (list `(apply-update!/default 25 ,- 5)
	 map-keys:update!)
   (list `(apply-update!/default 25 ,- 999)
	 map-keys:update!)
   (list `(apply-delete! 75)
	 map-keys:delete!)
   (list '(apply-set! (36 6) (81 9))
	 map-keys:set!)))

(define ts1-result
  '((0 0) (1 1) (4 2) (9 3) (16 4) (25 5)
    (36 6) (64 8) (81 9) (121 11) (144 12) (169 13)))

(define (reproduce-ts1)
  (let ((tracker (setup-tracker (fixnum-comparator))))
    (let ((runner (test-seq-runner tracker ts1)))
      (runner 'run-remaining-steps)
      (tracker 'get-ht))))

(define-test 'mapping-and-folding
  (lambda ()
    (let ((ht (reproduce-ts1)))
      (assert-equal (hash-table-find (lambda (key val)
				       (if (= 144 key (* val val))
					   (list key val)
					   #f))
				     ht
				     (lambda () 99))
		    '(144 12))
      (assert-equal (hash-table-find (lambda (key val)
				       (if (= 144 key val)
					   (list key val)
					   #f))
				     ht
				     (lambda () 99))
		    99)
      (assert-equal (hash-table-count <= ht)
		    2)
      (let ((ht*
	     (hash-table-map square
                             (make-eqv-comparator)
                             ht)))
	(assert-lset= equal?
		      (hash-table-map->list list ht*)
		      (map (lambda (kv)
			     (list (car kv)
				   (square (cadr kv))))
			   ts1-result)))
      (let ((keys (make-vector 15 -1))
            (vals (make-vector 15 -1)))
        (hash-table-for-each (lambda (key val)
			       (vector-set! keys val key)
			       (vector-set! vals val val))
                             ht)
	(assert-equal keys '#(0 1 4 9 16 25 36 -1 64 81 -1 121 144 169 -1))
	(assert-equal vals '#(0 1 2 3  4  5  6 -1  8  9 -1  11  12  13 -1))))))

(define (reproduce-ts2)
  (let ((ht (reproduce-ts1)))
    (hash-table-map! (lambda (key val)
		       (if (<= 10 key)
			   (- val)
			   val))
		     ht)
    ht))

(define-test 'copying
  (lambda ()
    (let ((ht (reproduce-ts2)))

      (define (test-=? ht1 ht2)
	(hash-table=? (number-comparator) ht1 ht2))

      ;; Not supported
      (assert-error (lambda () (hash-table-copy ht #f)))

      (assert-true (test-=? ht (hash-table-copy ht)))
      ;;(assert-true (test-=? ht (hash-table-copy ht #f)))
      (assert-true (test-=? ht (hash-table-copy ht #t)))
      (assert-true (hash-table-mutable? (hash-table-copy ht)))
      ;;(assert-false (hash-table-mutable? (hash-table-copy ht #f)))
      (assert-true (hash-table-mutable? (hash-table-copy ht #t)))

      (let ((kvs
	     '((0 0) (1 1) (4 2) (9 3) (16 -4) (25 -5)
	       (36 -6) (64 -8) (81 -9) (121 -11) (144 -12) (169 -13))))
	(assert-lset= equal? (hash-table-map->list list ht) kvs)
	(assert-lset= equal?
		      (hash-table->alist ht)
		      (map (lambda (kv)
			     (cons (car kv) (cadr kv)))
			   kvs))
	(assert-lset= =
		      (hash-table-fold (lambda (key val acc)
					 (declare (ignore val))
					 (cons key acc))
				       '()
				       ht)
		      (map car kvs))))))

(define-test 'sets
  (lambda ()
    (let ((ht (reproduce-ts2)))

      (hash-table-union! ht ht-fixnum2)
      (assert-lset= equal?
		    (hash-table->alist ht)
		    '((0 . 0) (1 . 1) (4 . 2) (9 . 3) (16 . -4) (25 . -5)
		      (36 . -6) (49 . 7) (64 . -8) (81 . -9) (121 . -11)
		      (144 . -12) (169 . -13)))

      (let ((ht* (hash-table-copy ht-fixnum2 #t)))
        (hash-table-union! ht* ht)
	(assert-lset= equal?
		      (hash-table->alist ht*)
		      '((0 . 0) (1 . 1) (4 . 2) (9 . 3) (16 . 4) (25 . 5)
			(36 . 6) (49 . 7) (64 . 8) (81 . 9) (121 . -11)
			(144 . -12) (169 . -13))))

      (hash-table-union! ht-eqv2 ht)
      (assert-true (hash-table=? (make-default-comparator) ht-eqv2 ht))

      (hash-table-intersection! ht-eqv2 ht)
      (assert-true (hash-table=? (make-default-comparator) ht-eqv2 ht))

      (hash-table-intersection! ht-eqv2 ht-eqv)
      (assert-true (hash-table-empty? ht-eqv2))

      (hash-table-intersection! ht ht-fixnum2)
      (assert-lset= equal?
		    (hash-table->alist ht)
		    '((0 . 0) (1 . 1) (4 . 2) (9 . 3) (16 . -4) (25 . -5)
		      (36 . -6) (49 . 7) (64 . -8) (81 . -9)))

      (hash-table-intersection! ht
	(alist->hash-table '((-1 . -1) (4 . 202) (25 . 205) (100 . 10))
			   (number-comparator)))
      (assert-lset= equal? (hash-table->alist ht) '((4 . 2) (25 . -5)))

      (let ((ht* (hash-table-copy ht-fixnum2 #t)))
	(hash-table-difference! ht*
	  (alist->hash-table '((-1 . -1) (4 . 202) (25 . 205) (100 . 10))
			     (number-comparator)))
	(assert-lset= equal?
		      (hash-table->alist ht*)
		      '((0 . 0) (1 . 1) (9 . 3) (16 . 4) (36 . 6) (49 . 7)
			(64 . 8) (81 . 9))))

      (let ((ht* (hash-table-copy ht-fixnum2 #t)))
	(hash-table-xor! ht*
	  (alist->hash-table '((-1 . -1) (4 . 202) (25 . 205) (100 . 10))
			     (number-comparator)))
	(assert-lset= equal?
		      (hash-table->alist ht*)
		      '((-1 . -1) (0 . 0) (1 . 1) (9 . 3) (16 . 4) (36 . 6)
			(49 . 7) (64 . 8) (81 . 9) (100 . 10)))))))

;;; Bugs reported on 5 January 2019 by Jéssica Milaré
;;; ( https://srfi-email.schemers.org/srfi-125/msg/10177551 )

(define-test 'reported-bugs
  (lambda ()

    ;; Spec says hash-table returns an immutable hash table (if that
    ;; is supported) and signal an error if there are duplicate keys,
    ;; but standard implementation returns a mutable hash table and
    ;; signals no error with duplicate keys.

    ;; Comment by Will Clinger: the spec says specifying a duplicate
    ;; key "is an error", so hash-table is not required to signal an
    ;; error when there are duplicate keys.  That part of the spec
    ;; was added on 8 May 2016, which is why it was not implemented
    ;; by the sample implementation of 2 May 2016.  Because a duplicate
    ;; key "is an error" rather than "signals an error", testing for
    ;; that situation is glass-box, as is testing for immutability.
    #|
    (assert-false
     (hash-table-mutable?
      (hash-table (number-comparator)
                  .25 .5 64 9999 81 9998 121 -11 144 -12)))
    |#
    ;; Spec says hash-table-set! must go left to right, but in
    ;; standard implementation it goes right to left.

    ;; Comment by Will Clinger: the left-to-right requirement was
    ;; added to the spec on 8 May 2016, which is why it was not
    ;; implemented by the sample implementation of 2 May 2016.

    (let ((ht (hash-table-empty-copy ht-eq)))
      (hash-table-set! ht 'foo 13 'bar 14 'foo 18)
      (assert-equal (hash-table-ref ht 'foo) 18))

    ;; Spec says hash-table-empty-copy returns a mutable hash table,
    ;; but in standard implementation it returns an immutable hash
    ;; table if the given hash table is immutable.
    #|
    (assert-false (hash-table-mutable? (hash-table (number-comparator))))
    (assert-true
     (hash-table-mutable?
      (hash-table-empty-copy
       (hash-table-copy (hash-table (number-comparator)) #f))))
    |#

    ;; hash-table-delete! seems to loop infinitely once it finds a key.

    ;; Comment by Will Clinger: that bug was added by
    ;; commit e17c15203a934ab741300e59619f880f363c2b2f
    ;; on 26 September 2018.  I do not understand the purpose of that
    ;; commit, as its one change appears to have had no substantive
    ;; effect apart from inserting this bug.

    (let ((ht (hash-table (make-default-comparator) 'foo 1 'bar 2 'baz 3)))
      (let ((ht* (hash-table-copy ht #t)))
	(hash-table-delete! ht* 'foo)
	(assert-equal (hash-table-size ht*) 2)))))