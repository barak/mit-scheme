#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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
    (define-bundle-interface foo? make-foo capture-foo a b c)

    (assert-true (bundle-interface? foo?))
    (assert-equal (bundle-interface-element-names foo?)
		  '(a b c))
    (for-each (lambda (name)
		(assert-equal (bundle-interface-element-properties foo? name)
			      '()))
	      (bundle-interface-element-names foo?))

    (define bundle-a (bundle-accessor foo? 'a))
    (define bundle-b (bundle-accessor foo? 'b))
    (define bundle-c (bundle-accessor foo? 'c))
    (assert-error (lambda () (bundle-accessor foo 'd)))

    (define (test-bundle bundle av bv cv)
      (assert-true (foo? bundle))
      (assert-eqv (bundle-ref bundle 'a) av)
      (assert-eqv (bundle-ref bundle 'b) bv)
      (assert-eqv (bundle-ref bundle 'c) cv)
      (assert-eqv (bundle-ref bundle 'd #f) #f)
      (assert-error (lambda () (bundle-ref foo 'd)))
      (assert-eqv (bundle-a bundle) av)
      (assert-eqv (bundle-b bundle) bv)
      (assert-eqv (bundle-c bundle) cv))

    (let ((a 10)
	  (b 20)
	  (c 40))
      (test-bundle (make-foo a b c) a b c))

    (let ((a 0)
	  (b 1)
	  (c 3))
      (test-bundle (capture-foo) a b c))))

(define-test 'metadata-table
  (lambda ()

    (define-bundle-interface metadata-table?
      make-metadata-table
      capture-metadata-table
      has?
      get
      put!
      intern!
      delete!
      get-alist
      put-alist!)

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

	(capture-metadata-table)))

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