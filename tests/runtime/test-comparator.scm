#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

;;;; Tests of comparator implementation

(declare (usual-integrations))

(define (all-pairs-of items)
  (append-map (lambda (a)
		(map (lambda (b) (cons a b))
		     items))
	      items))

(define (all-weak-pairs-of items)
  (append-map (lambda (a)
		(map (lambda (b) (weak-cons a b))
		     items))
	      items))

(define (all-vectors-of items)
  (map list->vector (all-lists-of items)))

(define (all-lists-of items)
  (append-map all-permutations-of
	      (all-heads-of items)))

(define (all-permutations-of items)
  (let loop ((items items))
    (if (pair? items)
        (append-map (lambda (index)
                      (map (let ((head (list-ref items index)))
                             (lambda (tail)
                               (cons head tail)))
                           (loop (delete-item items index))))
                    (iota (length items)))
        '(()))))

(define (delete-item items index)
  (append (take items index)
          (cdr (drop items index))))

(define (all-heads-of items)
  (fold-right (lambda (i acc)
		(cons (take items i) acc))
	      (list items)
	      (iota (length items))))

(define (trim-large-items items n)
  (if (> (length items) n)
      (take items n)
      items))

;;; These shouldn't be longer than 4 or 5.
;;; The number of tests grows like this:
;;; (square
;;;  (fold (lambda (i sum)
;;;          (+ (fact i) sum))
;;;        (fact n)
;;;        (iota n)))
(define boolean-items '(#f #t))
(define small-fixnum-items '(2 3 5 7))

(define bytevector-items
  (map list->bytevector (all-lists-of small-fixnum-items)))

;;; These can be a hundred or so.
;;; The number of tests grows by (square n).
(define large-n-items 100)
(define char-items (map integer->char (iota large-n-items #x300)))
(define fixnum-items (iota large-n-items #x500))
(define flonum-items (map ->flonum fixnum-items))
(define string-items (map number->string fixnum-items))
(define symbol-items
  (map (lambda (string)
	 (symbol 'x- string))
       string-items))

(define-test 'predicates
  (lambda ()
    (assert-false (comparator? =))
    (let ((c (make-eq-comparator)))
      (assert-true (comparator? c))
      (assert-false (comparator-ordered? c))
      (assert-true (comparator-hashable? c))
      (assert-true (comparator-rehash-after-gc? c)))
    (let ((c (make-equal-comparator)))
      (assert-true (comparator? c))
      (assert-false (comparator-ordered? c))
      (assert-true (comparator-hashable? c))
      (assert-true (comparator-rehash-after-gc? c)))
    (let ((c (real-comparator)))
      (assert-true (comparator? c))
      (assert-true (comparator-ordered? c))
      (assert-true (comparator-hashable? c))
      (assert-false (comparator-rehash-after-gc? c)))
    (let ((c (make-default-comparator)))
      (assert-true (comparator? c))
      (assert-true (comparator-ordered? c))
      (assert-true (comparator-hashable? c))
      (assert-true (comparator-rehash-after-gc? c)))))

(define-test 'comparator-if
  (lambda ()
    (assert-eq (comparator-if<=> (real-comparator) 1 2 'less 'equal 'greater)
	       'less)
    (assert-eq (comparator-if<=> (real-comparator) 1 1 'less 'equal 'greater)
	       'equal)
    (assert-eq (comparator-if<=> (real-comparator) 2 1 'less 'equal 'greater)
	       'greater)
    (assert-eq (comparator-if<=> "1" "2" 'less 'equal 'greater)
	       'less)
    (assert-eq (comparator-if<=> "1" "1" 'less 'equal 'greater)
	       'equal)
    (assert-eq (comparator-if<=> "2" "1" 'less 'equal 'greater)
	       'greater)))

(define-test 'hash-bound/salt
  (lambda ()
    (assert-true (exact-nonnegative-integer? (hash-bound)))
    (assert-true (fix:fixnum? (hash-mask)))
    (assert-= (+ (hash-mask) 1) (hash-bound))
    (do ((i 0 (+ i 1)))
	((not (< i 1000)))
      (assert-< (hash-salt) (hash-bound)))))

(define-test 'basic
  (lambda ()
    (let ((c (boolean-comparator)))
      (basic-tests boolean-items c boolean=? boolean<? boolean-hash)
      (test-combinations basic-tests boolean-items c))
    (let ((c (fixnum-comparator)))
      (basic-tests small-fixnum-items c = < fixnum-hash)
      (test-combinations basic-tests small-fixnum-items c))))

(define (basic-tests items c expected= expected< expected-hash)

  (define (compare p name a b expected)
    (assert-eq (p c a b) expected
	       'expression `(,name ,a ,b)))

  (for-each (lambda (p)
	      (let ((a (car p))
		    (b (cdr p)))
		(assert-true (comparator-test-type c a))
		(assert-true (comparator-test-type c b))
		(compare =? '=? a b (expected= a b))
		(if (comparator-ordered? c)
		    (begin
		      (compare <? '<? a b (expected< a b))
		      (compare <=? '<=? a b (not (expected< b a)))
		      (compare >? '>? a b (expected< b a))
		      (compare >=? '>=? a b (not (expected< a b)))))))
	    (all-pairs-of items))
  (if (comparator-hashable? c)
      (for-each (lambda (item)
		  (let ((h (comparator-hash c item)))
		    (assert-true (exact-nonnegative-integer? h))
		    (assert-true (< h (hash-bound)))
		    (assert-= h (expected-hash item)
			      'expression `(comparator-hash ,item))))
		items)))

(define (comparison-tests items c1 c2)
  (lambda ()

    (define (check-type a)
      (assert-eq (comparator-test-type c1 a)
		 (comparator-test-type c2 a)
		 'expression
		 `(eq? (comparator-test-type ,c1 ,a)
		       (comparator-test-type ,c2 ,a))))

    (define (compare p name a b)
      (assert-eq (p c1 a b) (p c2 a b)
		 'expression `(eq (,name ,c1 ,a ,b) (,name ,c2 ,a ,b))))

    (for-each (lambda (p)
		(let ((a (car p))
		      (b (cdr p)))
		  (check-type a)
		  (check-type b)
		  (compare =? '=? a b)
		  (if (comparator-ordered? c1)
		      (begin
			(compare <? '<? a b)
			(compare <=? '<=? a b)
			(compare >? '>? a b)
			(compare >=? '>=? a b)))))
	      (all-pairs-of items))
    (if (comparator-hashable? c1)
	(for-each (lambda (item)
		    (assert-= (comparator-hash c1 item)
			      (comparator-hash c2 item)
			      'expression `(= (comparator-hash ,c1 ,item)
					      (comparator-hash ,c2 ,item))))
		  items))))

(define-test 'comparison
  (list
   (let ((dc (make-default-comparator)))
     (define (run-default items c1 c2)
       (list (lambda ()
	       (assert-eq (comparator-ordered? c1)
			  (comparator-ordered? c2)
			  'expression `(eq? (comparator-ordered? ,c1)
					    (comparator-ordered? ,c2)))
	       (assert-eq (comparator-hashable? c1)
			  (comparator-hashable? c2)
			  'expression `(eq? (comparator-hashable? ,c1)
					    (comparator-hashable? ,c2))))
	     (comparison-tests items c1 c2)
	     (comparison-tests items c1 dc)
	     (comparison-tests (all-pairs-of (trim-large-items items 4))
			       (make-pair-comparator c1 c1)
			       dc)
	     (comparison-tests (all-weak-pairs-of (trim-large-items items 4))
			       (make-weak-pair-comparator c1 c1)
			       dc)
	     (comparison-tests (all-vectors-of (trim-large-items items 4))
			       (uniform-vector-comparator c1)
			       dc)))

     (list
      (run-default boolean-items
		   (boolean-comparator)
		   (make-comparator boolean?
				    boolean=?
				    boolean<?
				    boolean-hash))
      (run-default bytevector-items
		   (bytevector-comparator)
		   (make-comparator bytevector?
				    bytevector=?
				    bytevector<?
				    bytevector-hash))
      (run-default char-items
		   (char-comparator)
		   (make-comparator char? char=? char<? char-hash))
      (run-default fixnum-items
		   (fixnum-comparator)
		   (make-comparator fix:fixnum? fix:= fix:< fixnum-hash))
      (run-default flonum-items
		   (real-comparator)
		   (make-comparator real? = < number-hash))
      (run-default string-items
		   (string-comparator)
		   (make-comparator string? string=? string<? string-hash))
      (run-default symbol-items
		   (symbol-comparator)
		   (make-comparator symbol? symbol=? symbol<? symbol-hash))))
    (comparison-tests char-items
		      (char-ci-comparator)
		      (make-comparator char?
				       char-ci=?
				       char-ci<?
				       char-ci-hash))
    (comparison-tests flonum-items
		      (flonum-comparator)
		      (make-comparator flo:flonum? flo:= flo:< number-hash))
    (comparison-tests flonum-items
		      (number-comparator)
		      (make-comparator number? = #f number-hash))
    (comparison-tests string-items
		      (string-ci-comparator)
		      (make-comparator string?
				       string-ci=?
				       string-ci<?
				       string-ci-hash))
    (comparison-tests symbol-items
		      (interned-symbol-comparator)
		      (make-comparator interned-symbol? eq? symbol<? eq-hash))))

(define (test-combinations test items c)
  (upair-test test items c)
  (klist-test test items c)
  (klset-test test items c)
  (kvector-test test items c))

(define (upair-test test items c)
  (test (all-pairs-of items)
	(make-pair-comparator c c)
	(upair= (comparator-equality-predicate c))
	(and (comparator-ordered? c)
	     (upair< (comparator-equality-predicate c)
		     (comparator-ordering-predicate c)))
	(and (comparator-hashable? c)
	     (upair-hash (comparator-hash-function c)))))

(define (upair= =)
  (lambda (a b)
    (and (= (car a) (car b))
	 (= (cdr a) (cdr b)))))

(define (upair< = <)
  (lambda (a b)
    (or (< (car a) (car b))
	(and (= (car a) (car b))
	     (< (cdr a) (cdr b))))))

(define (upair-hash hash)
  (lambda (a)
    (combine-hashes (hash (car a))
		    (hash (cdr a)))))

(define (klist-test test items celt)
  (let ((items (all-lists-of items)))

    (let ((c (make-list-comparator celt pair? null? car cdr)))
      (assert-false (uniform-klist-comparator? c))
      (assert-true (uniform-list-comparator? c))
      (assert-false (uniform-weak-list-comparator? c))

      (assert-eq (uniform-list-comparator-elt c) celt)
      (assert-eq (uniform-list-comparator celt) c) ;memoized

      (klist-test-1 test items c celt pair? null? car cdr))

    (let ((c (make-list-comparator celt weak-pair? null? weak-car weak-cdr)))
      (assert-false (uniform-klist-comparator? c))
      (assert-false (uniform-list-comparator? c))
      (assert-true (uniform-weak-list-comparator? c))

      (assert-eq (uniform-weak-list-comparator-elt c) celt)
      (assert-eq (uniform-weak-list-comparator celt) c) ;memoized

      (klist-test-1 test (map list->weak-list items) c celt
		    weak-pair? null? weak-car weak-cdr))

    (let ((c (make-list-comparator celt pair? null? cdr car)))
      (assert-true (uniform-klist-comparator? c))
      (assert-false (uniform-list-comparator? c))
      (assert-false (uniform-weak-list-comparator? c))

      (assert-eq (uniform-klist-comparator-elt c) celt)

      (klist-test-1 test
		    (map (lambda (list)
			   (fold-right xcons '() list))
			 items)
		    c celt
		    pair? null? cdr car))))

(define (klist-test-1 test items cklist celt kpair? knull? kar kdr)

  (define (klist= elt=)
    (define (loop a b)
      (if (knull-list? a)
	  (knull-list? b)
	  (and (not (knull-list? b))
	       (elt= (kar a) (kar b))
	       (loop (kdr a) (kdr b)))))
    loop)

  (define (klist< elt= elt<)
    (define (loop a b)
      (and (not (knull-list? b))
	   (or (knull-list? a)
	       (let ((ea (kar a))
		     (eb (kar b)))
		 (or (elt< ea eb)
		     (and (elt= ea eb)
			  (loop (kdr a) (kdr b))))))))
    loop)

  (define (klist-hash elt-hash)
    (lambda (a)
      (let loop ((a a) (result (initial-hash)))
	(if (knull-list? a)
	    result
	    (loop (kdr a)
		  (combine-hashes (elt-hash (kar a))
				  result))))))

  (define (knull-list? a)
    (cond ((knull? a) #t)
	  ((kpair? a) #f)
	  (else (error:wrong-type-argument "klist" a))))

  (test items
	cklist
	(klist= (comparator-equality-predicate celt))
	(and (comparator-ordered? celt)
	     (klist< (comparator-equality-predicate celt)
		     (comparator-ordering-predicate celt)))
	(and (comparator-hashable? celt)
	     (klist-hash (comparator-hash-function celt)))))

(define (klset-test test items celt)
  (let ((items (all-lists-of items)))
    (let ((elt= (comparator-equality-predicate celt))
	  (elt-hash (comparator-hash-function celt)))

      (define (= a b)
	(lset= elt= a b))

      (define (< a b)
	(and (lset<= elt= a b)
	     (not (lset= elt= a b))))

      (define (hash a)
	(let loop ((a a) (result (initial-hash)))
	  (if (null-list? a)
	      result
	      (loop (cdr a)
		    (combine-hashes (elt-hash (car a))
				    result)))))

      (let ((c (lset-comparator celt)))
	(assert-true (lset-comparator? c))
	(assert-false (weak-lset-comparator? c))

	(assert-eq (lset-comparator-elt c) celt)
	(assert-eq (lset-comparator celt) c) ;memoized

	(assert-true (comparator-ordered? c))
	(assert-true (comparator-hashable? c))

	(test items c = < hash))

      (let ((c (weak-lset-comparator celt)))
	(assert-false (lset-comparator? c))
	(assert-true (weak-lset-comparator? c))

	(assert-eq (weak-lset-comparator-elt c) celt)
	(assert-eq (weak-lset-comparator celt) c) ;memoized

	(assert-true (comparator-ordered? c))
	(assert-true (comparator-hashable? c))

	(test (map list->weak-list items)
	      c
	      (lambda (a b)
		(= (weak-list->list a) (weak-list->list b)))
	      (lambda (a b)
		(< (weak-list->list a) (weak-list->list b)))
	      (lambda (a)
		(hash (weak-list->list a))))))))

(define (kvector-test test items celt)
  (let ((items (all-lists-of items)))

    (let ((c (make-vector-comparator celt vector? vector-length vector-ref)))
      (assert-false (uniform-kvector-comparator? c))
      (assert-true (uniform-vector-comparator? c))

      (assert-eq (uniform-vector-comparator-elt c) celt)
      (assert-eq (uniform-vector-comparator celt) c) ;memoized

      (kvector-test-1 test (map list->vector items) c celt
		      vector-length vector-ref))

    (let ((c (make-vector-comparator celt list? length list-ref)))
      (assert-true (uniform-kvector-comparator? c))
      (assert-false (uniform-vector-comparator? c))

      (assert-eq (uniform-kvector-comparator-elt c) celt)

      (kvector-test-1 test items c celt length list-ref))))

(define (kvector-test-1 test items ckvector celt kvector-length kvector-ref)

  (define (kvector= elt=)
    (lambda (a b)
      (let ((n (kvector-length a)))
	(and (= n (kvector-length b))
	     (let loop ((i 0))
	       (if (< i n)
		   (and (elt= (kvector-ref a i) (kvector-ref b i))
			(loop (+ i 1)))
		   #t))))))

  (define (kvector< elt= elt<)
    (lambda (a b)
      (let ((na (kvector-length a))
	    (nb (kvector-length b)))
	(let ((n (min na nb)))
	  (let loop ((i 0))
	    (if (< i n)
		(let ((ea (kvector-ref a i))
		      (eb (kvector-ref b i)))
		  (or (elt< ea eb)
		      (and (elt= ea eb)
			   (loop (+ i 1)))))
		(< na nb)))))))

  (define (kvector-hash elt-hash)
    (lambda (a)
      (let ((n (kvector-length a)))
	(let loop ((i 0) (result (initial-hash)))
	  (if (< i n)
	      (loop (+ i 1)
		    (combine-hashes (elt-hash (kvector-ref a i))
				    result))
	      result)))))

  (test items
	ckvector
	(kvector= (comparator-equality-predicate celt))
	(and (comparator-ordered? celt)
	     (kvector< (comparator-equality-predicate celt)
		       (comparator-ordering-predicate celt)))
	(and (comparator-hashable? celt)
	     (kvector-hash (comparator-hash-function celt)))))