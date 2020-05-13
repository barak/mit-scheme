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

;;;; Comparators (compatible with SRFI 128)
;;; package: (runtime comparator)

(declare (usual-integrations))

(add-boot-deps! '(runtime compound-predicate)
		'(runtime hash-table))

(define (make-comparator ? = < hash #!optional rehash-after-gc?)
  (guarantee unary-procedure? ? 'make-comparator)
  (guarantee binary-procedure? = 'make-comparator)
  (if < (guarantee binary-procedure? < 'make-comparator))
  (if hash (guarantee unary-procedure? hash 'make-comparator))
  (%make-comparator ? = < hash (and hash rehash-after-gc? #t)))

(define-record-type <comparator>
    (%make-comparator ? = < hash rehash-after-gc?)
    comparator?
  (? %comparator-?)
  (= %comparator-=)
  (< %comparator-<)
  (hash %comparator-hash)
  (rehash-after-gc? comparator-rehash-after-gc?))

(define (comparator-ordered? comparator)
  (and (%comparator-< comparator) #t))

(define (comparator-hashable? comparator)
  (and (%comparator-hash comparator) #t))

(define (comparator-ordering-predicate comparator)
  (or (%comparator-< comparator) (default-< comparator)))

(define ((default-< comparator) a b)
  (declare (ignore a b))
  (error "Comparator not ordered:" comparator))

(define (comparator-hash-function comparator)
  (or (%comparator-hash comparator) (default-hash comparator)))

(define ((default-hash comparator) a)
  (declare (ignore a))
  (error "Comparator not hashable:" comparator))

(define (make-eq-comparator)
  the-eq-comparator)

(define-deferred the-eq-comparator
  (%make-comparator any-object? eq? #f eq-hash #t))

(define (make-eqv-comparator)
  the-eqv-comparator)

(define-deferred the-eqv-comparator
  (%make-comparator any-object? eqv? #f eqv-hash #t))

(define (make-equal-comparator)
  the-equal-comparator)

(define-deferred the-equal-comparator
  (%make-comparator any-object? equal? #f equal-hash #t))

(define (comparator-test-type comparator object)
  ((%comparator-? comparator) object))

(define (comparator-check-type comparator object)
  (guarantee (%comparator-? comparator) object 'comparator-check-type)
  #t)

(define (comparator-hash comparator object)
  ((comparator-hash-function comparator) object))

(define (=? comparator a b . rest)
  (let ((= (%comparator-= comparator)))
    (let loop ((a a) (b b) (rest rest))
      (if (pair? rest)
	  (and (= a b)
	       (loop b (car rest) (cdr rest)))
	  (= a b)))))

(define (<? comparator a b . rest)
  (let ((< (%comparator-< comparator)))
    (let loop ((a a) (b b) (rest rest))
      (if (pair? rest)
	  (and (< a b)
	       (loop b (car rest) (cdr rest)))
	  (< a b)))))

(define (<=? comparator a b . rest)
  (let ((< (%comparator-< comparator)))
    (let loop ((a a) (b b) (rest rest))
      (if (pair? rest)
	  (and (not (< b a))
	       (loop b (car rest) (cdr rest)))
	  (not (< b a))))))

(define (>? comparator a b . rest)
  (let ((< (%comparator-< comparator)))
    (let loop ((a a) (b b) (rest rest))
      (if (pair? rest)
	  (and (< b a)
	       (loop b (car rest) (cdr rest)))
	  (< b a)))))

(define (>=? comparator a b . rest)
  (let ((< (%comparator-< comparator)))
    (let loop ((a a) (b b) (rest rest))
      (if (pair? rest)
	  (and (not (< a b))
	       (loop b (car rest) (cdr rest)))
	  (not (< a b))))))

;;;; General combinators

(define (make-pair-comparator c1 c2)
  (%make-comparator (pair-predicate (%comparator-? c1)
				    (%comparator-? c2))
		    (make-pair= (%comparator-= c1)
				(%comparator-= c2))
		    (and (%comparator-< c1)
			 (%comparator-< c2)
			 (make-pair< (%comparator-= c1)
				     (%comparator-< c1)
				     (%comparator-< c2)))
		    (and (%comparator-hash c1)
			 (%comparator-hash c2)
			 (make-pair-hash (%comparator-hash c1)
					 (%comparator-hash c2)))
		    (or (comparator-rehash-after-gc? c1)
			(comparator-rehash-after-gc? c2))))

(define (make-pair= car= cdr=)
  (lambda (a b)
    (and (car= (car a) (car b))
	 (cdr= (cdr a) (cdr b)))))

(define (make-pair< car= car< cdr<)
  (lambda (a b)
    (or (car< (car a) (car b))
	(and (car= (car a) (car b))
	     (cdr< (cdr a) (cdr b))))))

(define (make-pair-hash car-hash cdr-hash)
  (lambda (a)
    (combine-hashes (car-hash (car a))
		    (cdr-hash (cdr a)))))

(define (make-list-comparator celt kpair? knull? kar kdr)
  (cond ((and (eqv? pair? kpair?)
	      (eqv? null? knull?)
	      (eqv? car kar)
	      (eqv? cdr kdr))
	 (uniform-list-comparator celt))
	((and (eqv? weak-pair? kpair?)
	      (eqv? null? knull?)
	      (eqv? weak-car kar)
	      (eqv? weak-cdr kdr))
	 (uniform-weak-list-comparator celt))
	(else
	 (%make-comparator
	  (make-klist? (%comparator-? celt)
		       kpair? knull? kar kdr)
	  (make-klist= (%comparator-= celt)
		       kpair? knull? kar kdr)
	  (and (%comparator-< celt)
	       (make-klist< (%comparator-= celt)
			    (%comparator-< celt)
			    kpair? knull? kar kdr))
	  (and (%comparator-hash celt)
	       (make-klist-hash (%comparator-hash celt)
				kpair? knull? kar kdr))
	  (comparator-rehash-after-gc? celt)))))

(define (make-klist? elt? kpair? knull? kar kdr)
  (lambda (a)
    (let loop ((scan1 a) (scan2 a))
      (if (kpair? scan1)
	  (and (elt? (kar scan1))
	       (let ((next (kdr scan1)))
		 (and (not (eq? next scan2))
		      (if (kpair? next)
			  (loop (kdr next) (kdr scan2))
			  (knull? next)))))
	  (knull? scan1)))))

(define (make-klist= elt= kpair? knull? kar kdr)
  (define (null-klist? object)
    (cond ((knull? object) #t)
	  ((kpair? object) #f)
	  (else (error:wrong-type-argument "list-ish" object))))
  (lambda (a b)
    (let loop ((scana a) (scanb b))
      (if (null-klist? scana)
	  (null-klist? scanb)
	  (and (not (null-klist? scanb))
	       (elt= (kar scana) (kar scanb))
	       (loop (kdr scana) (kdr scanb)))))))

(define (make-klist< elt= elt< kpair? knull? kar kdr)
  (define (null-klist? object)
    (cond ((knull? object) #t)
	  ((kpair? object) #f)
	  (else (error:wrong-type-argument "list-ish" object))))
  (lambda (a b)
    (let loop ((scana a) (scanb b))
      (and (not (null-klist? scanb))
	   (or (null-klist? scana)
	       (elt< (kar scana) (kar scanb))
	       (and (elt= (kar scana) (kar scanb))
		    (loop (kdr scana) (kdr scanb))))))))

(define (make-klist-hash elt-hash kpair? knull? kar kdr)
  (define (null-klist? object)
    (cond ((knull? object) #t)
	  ((kpair? object) #f)
	  (else (error:wrong-type-argument "list-ish" object))))
  (lambda (a)
    (let loop ((scan a) (result (initial-hash)))
      (if (null-klist? scan)
	  result
	  (loop (kdr scan)
		(combine-hashes (elt-hash (kar scan)) result))))))

;;;; Specific combinators

(define (memoized-constructor constructor)
  (let ((table (make-key-weak-eq-hash-table)))
    (lambda (celt)
      (hash-table-intern! table celt (lambda () (constructor celt))))))

(define-deferred uniform-list-comparator
  (memoized-constructor
   (lambda (celt)
     (%make-comparator
      (uniform-list-predicate (%comparator-? celt))
      (uniform-list-equality-predicate (%comparator-= celt))
      (and (%comparator-< celt)
	   (uniform-list-ordering-predicate (%comparator-= celt)
					    (%comparator-< celt)))
      (and (%comparator-hash celt)
	   (uniform-list-hash (%comparator-hash celt)))
      (comparator-rehash-after-gc? celt)))))

(define (uniform-list-equality-predicate elt=?)
  (cond ((eqv? eq? elt=?) uniform-eq-list=?)
	((eqv? eqv? elt=?) uniform-eqv-list=?)
	(else (%uniform-list-equality-predicate elt=?))))

(define-integrable (%uniform-list-equality-predicate elt=?)
  (lambda (l1 l2)
    (let loop ((l1 l1) (l2 l2))
      (if (and (pair? l1) (pair? l2))
	  (and (elt=? (car l1) (car l2))
	       (loop (cdr l1) (cdr l2)))
	  (and (null? l1) (null? l2))))))

(define uniform-eq-list=? (%uniform-list-equality-predicate eq?))
(define uniform-eqv-list=? (%uniform-list-equality-predicate eqv?))

(define (uniform-list-ordering-predicate elt=? elt<?)
  (lambda (a b)
    (let loop ((scana a) (scanb b))
      (and (not (null-list? scanb))
	   (or (null-list? scana)
	       (elt<? (car scana) (car scanb))
	       (and (elt=? (car scana) (car scanb))
		    (loop (cdr scana) (cdr scanb))))))))

(define-deferred lset-comparator
  (memoized-constructor
   (lambda (celt)
     (%make-comparator
      (uniform-list-predicate (%comparator-? celt))
      (lset-equality-predicate (%comparator-= celt))
      (lset-ordering-predicate (%comparator-= celt))
      (and (%comparator-hash celt)
	   (uniform-list-hash (%comparator-hash celt)))
      (comparator-rehash-after-gc? celt)))))

(define (lset-equality-predicate elt=?)
  (cond ((eqv? eq? elt=?) eq-lset=?)
	((eqv? eqv? elt=?) eqv-lset=?)
	(else (%lset-equality-predicate elt=?))))

(define-integrable (%lset-equality-predicate elt=?)
  (define-integrable (<=? l1 l2)
    (every (lambda (x)
	     (any (lambda (y) (elt=? x y)) l2))
	   l1))
  (lambda (l1 l2)
    (or (eq? l1 l2)
	(and (<=? l1 l2)
	     (<=? l2 l1)))))

(define eq-lset=? (%lset-equality-predicate eq?))
(define eqv-lset=? (%lset-equality-predicate eqv?))

(define (lset-ordering-predicate elt=?)
  (cond ((eqv? eq? elt=?) eq-lset<?)
	((eqv? eqv? elt=?) eqv-lset<?)
	(else (%lset-ordering-predicate elt=?))))

(define-integrable (%lset-ordering-predicate elt=?)
  (define-integrable (<=? l1 l2)
    (every (lambda (x)
	     (any (lambda (y) (elt=? x y)) l2))
	   l1))
  (lambda (l1 l2)
    (and (<=? l1 l2)
	 (not (<=? l2 l1)))))

(define eq-lset<? (%lset-ordering-predicate eq?))
(define eqv-lset<? (%lset-ordering-predicate eqv?))

(define (weak-list-constructor constructor)
  (memoized-constructor
   (lambda (celt)
     (let ((comparator (constructor celt)))
       (hash-table-set! weak-list-comparators comparator #t)
       comparator))))

(define (weak-list-comparator? object)
  (hash-table-exists? weak-list-comparators object))

(define-deferred weak-list-comparators
  (make-key-weak-eq-hash-table))

(define-deferred uniform-weak-list-comparator
  (weak-list-constructor
   (lambda (celt)
     (%make-comparator
      (uniform-weak-list-predicate (%comparator-? celt))
      (uniform-weak-list-equality-predicate (%comparator-= celt))
      (and (%comparator-< celt)
	   (uniform-weak-list-ordering-predicate (%comparator-= celt)
						 (%comparator-< celt)))
      (and (%comparator-hash celt)
	   (uniform-weak-list-hash (%comparator-hash celt)))
      (comparator-rehash-after-gc? celt)))))

(define (uniform-weak-list-equality-predicate elt=?)
  (cond ((eqv? eq? elt=?) uniform-eq-weak-list=?)
	((eqv? eqv? elt=?) uniform-eqv-weak-list=?)
	(else (%uniform-weak-list-equality-predicate elt=?))))

(define-integrable (%uniform-weak-list-equality-predicate elt=?)
  (lambda (l1 l2)
    (let loop ((l1 l1) (l2 l2))
      (if (and (weak-pair? l1) (weak-pair? l2))
	  (let ((key1 (weak-car l1)))
	    (and (not (gc-reclaimed-object? key1))
		 (elt=? key1 (weak-car l2))
		 (loop (weak-cdr l1) (weak-cdr l2))))
	  (and (null? l1) (null? l2))))))

(define uniform-eq-weak-list=? (%uniform-weak-list-equality-predicate eq?))
(define uniform-eqv-weak-list=? (%uniform-weak-list-equality-predicate eqv?))

(define (uniform-weak-list-ordering-predicate elt=? elt<?)
  (lambda (a b)
    (let loop ((scana a) (scanb b))
      (and (not (null-weak-list? scanb))
	   (or (null-weak-list? scana)
	       (let ((key1 (weak-car scana)))
		 (and (not (gc-reclaimed-object? key1))
		      (or (elt<? key1 (weak-car scanb))
			  (and (elt=? key1 (weak-car scanb))
			       (loop (weak-cdr scana) (weak-cdr scanb)))))))))))

(define-deferred weak-lset-comparator
  (weak-list-constructor
   (lambda (celt)
     (%make-comparator
      (uniform-weak-list-predicate (%comparator-? celt))
      (weak-lset-equality-predicate (%comparator-= celt))
      (weak-lset-ordering-predicate (%comparator-= celt))
      (and (%comparator-hash celt)
	   (uniform-weak-list-hash (%comparator-hash celt)))
      (comparator-rehash-after-gc? celt)))))

(define (weak-lset-equality-predicate elt=?)
  (cond ((eqv? eq? elt=?) eq-weak-lset=?)
	((eqv? eqv? elt=?) eqv-weak-lset=?)
	(else (%weak-lset-equality-predicate elt=?))))

(define-integrable (%weak-lset-equality-predicate elt=?)
  (define-integrable (<=? l1 l2)
    (weak-every (lambda (x)
		  (weak-any (lambda (y) (elt=? x y)) l2))
		l1))
  (lambda (l1 l2)
    (or (eq? l1 l2)
	(and (<=? l1 l2)
	     (<=? l2 l1)))))

(define eq-weak-lset=? (%weak-lset-equality-predicate eq?))
(define eqv-weak-lset=? (%weak-lset-equality-predicate eqv?))

(define (weak-lset-ordering-predicate elt=?)
  (cond ((eqv? eq? elt=?) eq-weak-lset<?)
	((eqv? eqv? elt=?) eqv-weak-lset<?)
	(else (%weak-lset-ordering-predicate elt=?))))

(define-integrable (%weak-lset-ordering-predicate elt=?)
  (define-integrable (<=? l1 l2)
    (weak-every (lambda (x)
		  (weak-any (lambda (y) (elt=? x y)) l2))
		l1))
  (lambda (l1 l2)
    (and (<=? l1 l2)
	 (not (<=? l2 l1)))))

(define eq-weak-lset<? (%weak-lset-ordering-predicate eq?))
(define eqv-weak-lset<? (%weak-lset-ordering-predicate eqv?))

;;;; Hash functions

;;; On 64-bit architectures:
;;;   hash functions return a 32-bit unsigned value
;;; On 32-bit architectures:
;;;   hash functions return a 25-bit unsigned value

(define-syntax hash-bound
  (syntax-rules ()
    ((_) (select-on-bytes-per-word #x01FFFFFF #xFFFFFFFF))))

(define ((protected-hash-function hash-fn) object)
  (check-hash (hash-fn object)))

(define-integrable (check-hash h)
  (guarantee index-fixnum? h)
  (if (not (fix:<= h (hash-bound)))
      (error:bad-range-argument h))
  h)

(define (combine-hashes h1 h2)
  (%combine-hashes (check-hash h1) (check-hash h2)))

(define-integrable (initial-hash)
  (select-on-bytes-per-word 25165813 3221225473))

(select-on-bytes-per-word
 (define-integrable (%combine-hashes h1 h2)
   (let ((sum (+ (* 31 h1) h2)))
     (if (fix:fixnum? sum)
	 (fix:and sum (hash-bound))
	 (bitwise-and sum (hash-bound)))))
 (define-integrable (%combine-hashes h1 h2)
   (fix:and (fix:+ (fix:* 31 h1) h2) (hash-bound))))

(define ((uniform-binary-hash hash-fn) object1 object2)
  (%combine-hashes (check-hash (hash-fn object1))
		   (check-hash (hash-fn object2))))

(define (uniform-list-hash elt-hash)
  (cond ((eqv? eq? elt-hash) uniform-eq-list-hash)
	((eqv? eqv? elt-hash) uniform-eqv-list-hash)
	(else
	 (let ()
	   (define-integrable (checked elt)
	     (check-hash (elt-hash elt)))
	   (%uniform-list-hash checked)))))

(define-integrable (%uniform-list-hash elt-hash)
  (lambda (object)
    (fold (lambda (elt result)
	    (%combine-hashes (elt-hash elt) result))
	  (initial-hash)
	  object)))

(define uniform-eq-list-hash (%uniform-list-hash eq-hash))
(define uniform-eqv-list-hash (%uniform-list-hash eqv-hash))

(define (uniform-weak-list-hash elt-hash)
  (cond ((eqv? eq? elt-hash) uniform-eq-weak-list-hash)
	((eqv? eqv? elt-hash) uniform-eqv-weak-list-hash)
	(else
	 (let ()
	   (define-integrable (checked elt)
	     (check-hash (elt-hash elt)))
	   (%uniform-weak-list-hash checked)))))

(define-integrable (%uniform-weak-list-hash elt-hash)
  (lambda (object)
    (weak-fold (lambda (elt result)
		 (%combine-hashes (elt-hash elt) result))
	       (initial-hash)
	       object)))

(define uniform-eq-weak-list-hash (%uniform-weak-list-hash eq-hash))
(define uniform-eqv-weak-list-hash (%uniform-weak-list-hash eqv-hash))