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

;;; These shouldn't be longer than 4 or 5.
;;; The number of tests grows like this:
;;; (square
;;;  (fold (lambda (i sum)
;;;          (+ (fact i) sum))
;;;        (fact n)
;;;        (iota n)))
(define boolean-items '(#f #t))
(define small-fixnum-items '(2 3 5 7))

(define (bytevector-items)
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
      (assert-false (comparator-rehash-after-gc? c)))))

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

(define-test 'comparison
  (lambda ()
    (comparison-tests boolean-items
		      (boolean-comparator)
		      (make-comparator boolean?
				       boolean=?
				       boolean<?
				       boolean-hash))
    (comparison-tests (bytevector-items)
		      (bytevector-comparator)
		      (make-comparator bytevector?
				       bytevector=?
				       bytevector<?
				       bytevector-hash))
    (comparison-tests char-items
		      (char-comparator)
		      (make-comparator char?
				       char=?
				       char<?
				       char-hash))
    (comparison-tests char-items
		      (char-ci-comparator)
		      (make-comparator char?
				       char-ci=?
				       char-ci<?
				       char-ci-hash))
    (comparison-tests fixnum-items
		      (fixnum-comparator)
		      (make-comparator fix:fixnum? fix:= fix:< fixnum-hash))
    (comparison-tests flonum-items
		      (flonum-comparator)
		      (make-comparator flo:flonum? flo:= flo:< number-hash))
    (comparison-tests flonum-items
		      (number-comparator)
		      (make-comparator number? = #f number-hash))
    (comparison-tests flonum-items
		      (real-comparator)
		      (make-comparator real? = < number-hash))
    (comparison-tests string-items
		      (string-comparator)
		      (make-comparator string? string=? string<? string-hash))
    (comparison-tests string-items
		      (string-ci-comparator)
		      (make-comparator string?
				       string-ci=?
				       string-ci<?
				       string-ci-hash))
    (comparison-tests symbol-items
		      (symbol-comparator)
		      (make-comparator symbol? symbol=? symbol<? symbol-hash))
    (comparison-tests symbol-items
		      (interned-symbol-comparator)
		      (make-comparator interned-symbol? eq? symbol<? eq-hash))))

(define (comparison-tests items c1 c2)

  (define (compare p name a b)
    (assert-eq (p c1 a b) (p c2 a b)
	       'expression `(eq (,name ,c1 ,a ,b) (,name ,c2 ,a ,b))))

  (assert-eq (comparator-ordered? c1)
	     (comparator-ordered? c2))
  (assert-eq (comparator-hashable? c1)
	     (comparator-hashable? c2))
  (for-each (lambda (p)
	      (let ((a (car p))
		    (b (cdr p)))
		(assert-eq (comparator-test-type c1 a)
			   (comparator-test-type c2 a))
		(assert-eq (comparator-test-type c1 b)
			   (comparator-test-type c2 b))
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
		items)))

(define (test-combinations test items c)
  (upair-test test items c)
  (klist-test test items c pair? null? car cdr)
  (kvector-test test items c vector-length vector-ref))

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

(define (klist-test test items celt kpair? knull? kar kdr)
  (test (all-lists-of items)
	(make-list-comparator celt kpair? knull? kar kdr)
	(klist= (comparator-equality-predicate celt)
		kpair? knull? kar kdr)
	(and (comparator-ordered? celt)
	     (klist< (comparator-equality-predicate celt)
		     (comparator-ordering-predicate celt)
		     kpair? knull? kar kdr))
	(and (comparator-hashable? celt)
	     (klist-hash (comparator-hash-function celt)
			 kpair? knull? kar kdr))))

(define (klist= elt= kpair? knull? kar kdr)

  (define (loop a b)
    (if (knull-list? a)
	(knull-list? b)
	(and (not (knull-list? b))
	     (elt= (kar a) (kar b))
	     (loop (kdr a) (kdr b)))))

  (define (knull-list? a)
    (cond ((knull? a) #t)
	  ((kpair? a) #f)
	  (else (error:wrong-type-argument "klist" a))))

  loop)

(define (klist< elt= elt< kpair? knull? kar kdr)

  (define (loop a b)
    (and (not (knull-list? b))
	 (or (knull-list? a)
	     (let ((ea (kar a))
		   (eb (kar b)))
	       (or (elt< ea eb)
		   (and (elt= ea eb)
			(loop (kdr a) (kdr b))))))))

  (define (knull-list? a)
    (cond ((knull? a) #t)
	  ((kpair? a) #f)
	  (else (error:wrong-type-argument "klist" a))))

  loop)

(define (klist-hash elt-hash kpair? knull? kar kdr)

  (define (knull-list? a)
    (cond ((knull? a) #t)
	  ((kpair? a) #f)
	  (else (error:wrong-type-argument "klist" a))))

  (lambda (a)
    (let loop ((a a) (result (initial-hash)))
      (if (knull-list? a)
	  result
	  (loop (kdr a)
		(combine-hashes (elt-hash (kar a))
				result))))))

(define (kvector-test test items celt kvector-length kvector-ref)
  (test (all-vectors-of items)
	(make-vector-comparator celt vector?
				kvector-length kvector-ref)
	(kvector= (comparator-equality-predicate celt)
		  kvector-length kvector-ref)
	(and (comparator-ordered? celt)
	     (kvector< (comparator-equality-predicate celt)
		       (comparator-ordering-predicate celt)
		       kvector-length kvector-ref))
	(and (comparator-hashable? celt)
	     (kvector-hash (comparator-hash-function celt)
			   kvector-length kvector-ref))))

(define (kvector= elt= kvector-length kvector-ref)
  (lambda (a b)
    (let ((n (kvector-length a)))
      (and (= n (kvector-length b))
	   (let loop ((i 0))
	     (if (< i n)
		 (and (elt= (kvector-ref a i) (kvector-ref b i))
		      (loop (+ i 1)))
		 #t))))))

(define (kvector< elt= elt< kvector-length kvector-ref)
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

(define (kvector-hash elt-hash kvector-length kvector-ref)
  (lambda (a)
    (let ((n (kvector-length a)))
      (let loop ((i 0) (result (initial-hash)))
	(if (< i n)
	    (loop (+ i 1)
		  (combine-hashes (elt-hash (kvector-ref a i))
				  result))
	    result)))))

(define (all-pairs-of items)
  (append-map (lambda (a)
		(map (lambda (b) (cons a b))
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