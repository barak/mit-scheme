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

;;;; Tests for bundles

(declare (usual-integrations))

(define-test 'simple
  (lambda ()
    (define (x) 10)
    (define (y) 20)
    (define (z) 40)

    (define (simple-tests b)
      (assert-true (bundle? b))
      (assert-eqv (bundle-ref b 'x) x)
      (assert-eqv (bundle-ref b 'y) y)
      (assert-eqv (bundle-ref b 'z) z)
      (assert-eqv (bundle-ref b 'w #f) #f)

      (assert-eqv (b 'x) (x))
      (assert-eqv (b 'y) (y))
      (assert-eqv (b 'z) (z))
      (assert-error (lambda () (b 'w))))

    (simple-tests (bundle #f x y z))

    (assert-true (bundle-predicate? bundle?))
    (simple-tests (bundle bundle? x y z))

    (let ((predicate (make-bundle-predicate 'foo)))
      (assert-true (bundle-predicate? predicate))
      (let ((b (bundle predicate x y z)))
	(assert-true (predicate b))
	(simple-tests b)))))

(define-test 'metadata-table
  (lambda ()

    (define metadata-table?
      (make-bundle-predicate 'metadata-table))

    (define foo
      (let ((alist '()))

	(define (has? key)
	  (if (assv key alist) #t #f))

	(define (get key #!optional default-value)
	  (let ((p (assv key alist)))
	    (if p
		(cdr p)
		(begin
		  (if (default-object? default-value)
		      (error "Object has no associated metadata:" key))
		  default-value))))

	(define (put! key metadata)
	  (let ((p (assv key alist)))
	    (if p
		(set-cdr! p metadata)
		(begin
		  (set! alist (cons (cons key metadata) alist))
		  unspecific))))

	(define (intern! key get-value)
	  (let ((p (assv key alist)))
	    (if p
		(cdr p)
		(let ((value (get-value)))
		  (set! alist (cons (cons key value) alist))
		  value))))

	(define (delete! key)
	  (set! alist
		(remove! (lambda (p)
			   (eqv? (car p) key))
			 alist))
	  unspecific)

	(define (get-alist)
	  alist)

	(define (put-alist! alist*)
	  (for-each (lambda (p)
		      (put! (car p) (cdr p)))
		    alist*))

	(bundle metadata-table?
		has?
		get
		put!
		intern!
		delete!
		get-alist
		put-alist!)))

    (assert-true (metadata-table? foo))

    (assert-false (foo 'has? 'x))
    (assert-false (foo 'has? 'y))
    (assert-error (lambda () (foo 'get 'x)))
    (assert-error (lambda () (foo 'get 'y)))
    (assert-eqv (foo 'get 'x 33) 33)
    (assert-eqv (foo 'get 'y 44) 44)
    (assert-equal (foo 'get-alist) '())

    (foo 'put! 'x 55)
    (assert-true (foo 'has? 'x))
    (assert-false (foo 'has? 'y))
    (assert-eqv (foo 'get 'x) 55)
    (assert-eqv (foo 'get 'x 33) 55)
    (assert-equal (foo 'get-alist) '((x . 55)))
    ))

(define-test 'delegation
  (lambda ()
    (define (x) 10)
    (define (y) 20)
    (define (z) 40)

    (let ((b1 (bundle #f x y z)))
      (let ((b2
	     (bundle-combine #f
			     bundle-combiner:first
			     (let ()
			       (define (y) 25)
			       (bundle #f y))
			     b1)))

	(assert-eqv (b1 'x) 10)
	(assert-eqv (b1 'y) 20)
	(assert-eqv (b1 'z) 40)
	(assert-error (lambda () (b1 'foo)))

	(assert-eqv (b2 'x) 10)
	(assert-eqv (b2 'y) 25)
	(assert-eqv (b2 'z) 40)
	(assert-error (lambda () (b2 'foo)))))))

(define-test 'lazy-delegation
  (lambda ()
    (define (x n) (+ 10 n))
    (define (y n) (+ 20 n))
    (define (z n) (+ 40 n))

    (let ((b1 (bundle #f x y z)))
      (assert-eqv (b1 'x 1) 11)
      (assert-eqv (b1 'x 2) 12)
      (assert-eqv (b1 'y 1) 21)
      (assert-eqv (b1 'y 2) 22)
      (assert-eqv (b1 'z 1) 41)
      (assert-eqv (b1 'z 2) 42)

      (let ((b2
	     (bundle-combine #f
			     bundle-combiner:first
			     (let ()
			       (define (x n)
				 (if (odd? n)
				     (b1 'y n)
				     (b1 'z n)))

			       (define (y n) (+ 25 n))

			       (bundle #f x y))
			     b1)))

	(assert-eqv (b2 'x 1) 21)
	(assert-eqv (b2 'x 2) 42)
	(assert-eqv (b2 'y 1) 26)
	(assert-eqv (b2 'y 2) 27)
	(assert-eqv (b2 'z 1) 41)
	(assert-eqv (b2 'z 2) 42)))))