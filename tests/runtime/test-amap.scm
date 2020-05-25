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

;;;; Tests of associative-map implementation

(declare (usual-integrations))

(define test-alist
  '((2 . -11)
    (3 . -12)
    (5 . -13)
    (7 . -14)
    (11 . -15)))

(define test-reversed-alist
  '((2 . -15)
    (3 . -14)
    (5 . -13)
    (7 . -12)
    (11 . -11)))

(define test-keys
  (map car test-alist))

(define test-values
  (map cdr test-alist))

(define (test-value key)
  (cdr (assv key test-alist)))

(define (test-reversed-value key)
  (cdr (assv key test-reversed-alist)))

(define other-keys
  '(0 1))

(define test-key-comparator
  (fixnum-comparator))

(define test-value-comparator
  (fixnum-comparator))

(define (fail)
  1000)

(define (succeed value)
  (* value 3))

(define (test-pred key value)
  (eqv? value (test-value key)))

(define (test-kons key value acc)
  (cons* key (square value) acc))

(define test-knil
  '())

(define (test-map-proc value)
  (- value))

(define (test-map!-proc key value)
  (declare (ignore key))
  (- value))

(define amap-types
  '(hash-table alist red/black-tree))

(define (empty-nondestructive-tests impl-name)
  (lambda ()
    (let ((amap (make-amap test-key-comparator impl-name)))
      (assert-true (amap=? test-value-comparator amap amap))
      (assert-true (amap-mutable? amap))
      (assert-eqv (amap-find (lambda (key value)
			       (declare (ignore key value))
			       #t)
			     amap
			     (lambda () -1))
		  -1)
      (assert-true (amap-empty? (amap-map cons amap)))
      (amap-map! (lambda (key value)
		   (error "procedure called:" key value))
		 amap)
      (amap-prune! (lambda (key value)
		     (error "procedure called:" key value))
		   amap)
      (amap-assertions amap '())
      (amap-copy-assertions amap '()))))

(define-test 'empty-nondestructive-tests
  (map empty-nondestructive-tests amap-types))

(define (simple-tests impl-name)
  (lambda ()
    (let ((amap (make-amap test-key-comparator impl-name))
	  (alist))
      (for-each (lambda (p)
		  (amap-set! amap (car p) (cdr p)))
		test-alist)
      (set! alist test-alist)
      (amap-assertions amap alist)
      (amap-copy-assertions amap alist)

      (for-each (lambda (p)
		  (amap-set! amap (car p) (cdr p)))
		test-reversed-alist)
      (set! alist test-reversed-alist)
      (amap-assertions amap alist)

      (for-each (lambda (key)
		  (amap-update! amap key test-map-proc))
		test-keys)
      (set! alist
	    (map (lambda (p)
		   (cons (car p)
			 (test-map-proc (cdr p))))
		 alist))
      (amap-assertions amap alist)

      (amap-prune! test-pred amap)
      (set! alist
	    (remove (lambda (p)
		      (test-pred (car p) (cdr p)))
		    alist))
      (amap-assertions amap alist)

      (amap-map! test-map!-proc amap)
      (set! alist
	    (map (lambda (p)
		   (cons (car p)
			 (test-map!-proc (car p) (cdr p))))
		 alist))
      (amap-assertions amap alist))))

(define-test 'simple-tests
  (map simple-tests amap-types))

(define (amap-assertions amap alist)
  (assert-boolean= (amap-empty? amap) (null? alist))
  (assert-= (amap-size amap) (length alist))
  (assert-lset= equal? (amap->alist amap) alist)
  (assert-lset= eqv? (amap-keys amap) (map car alist))
  (assert-lset= eqv? (amap-values amap) (map cdr alist))
  (let-values (((keys values) (amap-entries amap)))
    (assert-lset= eqv? keys (map car alist))
    (assert-lset= eqv? values (map cdr alist)))
  (assert-= (amap-count test-pred amap)
	    (count (lambda (p)
		     (test-pred (car p) (cdr p)))
		   alist))
  (assert-lset= equal?
		(amap-fold test-kons test-knil amap)
		(alist-fold test-kons test-knil alist))
  (assert-lset= equal?
		(amap-map->list xcons amap)
		(map (lambda (p)
		       (xcons (car p) (cdr p)))
		     alist))
  (let ((n1 0)
	(n2 0))
    (amap-for-each (lambda (key value)
		     (declare (ignore value))
		     (set! n1 (+ key n1))
		     unspecific)
		   amap)
    (for-each (lambda (p)
		(set! n2 (+ (car p) n2))
		unspecific)
	      alist)
    (assert-= n1 n2))
  (for-each (lambda (key)
	      (let ((p (assv key alist)))
		(if p
		    (let ((value (cdr p)))
		      (assert-true (amap-contains? amap key))
		      (assert-eqv (amap-ref amap key) value)
		      (assert-eqv (amap-ref amap key fail) value)
		      (assert-eqv (amap-ref amap key fail succeed)
				  (succeed value))
		      (assert-eqv (amap-ref/default amap key (fail)) value))
		    (begin
		      (assert-false (amap-contains? amap key))
		      (assert-error (lambda () (amap-ref amap key)))
		      (assert-eqv (amap-ref amap key fail) (fail))
		      (assert-eqv (amap-ref amap key fail succeed) (fail))
		      (assert-eqv (amap-ref/default amap key (fail)) (fail))))))
	    test-keys))

(define (amap-copy-assertions amap alist)
  (let ((amap* (amap-copy amap)))
    (amap-assertions amap* alist)
    (assert-true (amap=? test-value-comparator amap amap*)))
  (let ((amap* (amap-empty-copy amap)))
    (amap-assertions amap* '())
    (assert-boolean= (amap=? test-value-comparator amap amap*)
		     (null? alist)))
  (let ((amap*
	 (alist->amap alist
		      test-key-comparator
		      (amap-implementation-name amap))))
    (amap-assertions amap* alist)
    (assert-true (amap=? test-value-comparator amap amap*)))
  (amap-assertions (amap-map test-map-proc amap)
		   (map (lambda (p)
			  (cons (car p)
				(test-map-proc (cdr p))))
			alist)))