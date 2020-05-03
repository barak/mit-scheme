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

;;;; Tests of simple trie

(declare (usual-integrations))

(define paths
  (list '(a b)
	'(a)
	'(c d)
	'(e a)
	'(d)
	'(d d)
	'(c b)))

(define (simple-example)
  (let* ((vals (iota (length paths)))
	 (alist (map cons paths vals)))
    (values vals alist (alist->trie alist))))

(define-test 'trie-empty
  (lambda ()
    (let ((trie (make-trie)))
      (assert-false (trie-has-value? trie))
      (assert-error (lambda () (trie-value trie)))
      (assert-equal (trie-find trie '()) trie)
      (assert-equal (trie-intern! trie '()) trie)
      (assert-equal (trie-values trie) '())
      (assert-equal (trie->alist trie) '())

      (for-each (lambda (path)
		  (assert-false (trie-find trie path)))
		paths)

      (set-trie-value! trie "win!")
      (assert-true (trie-has-value? trie))
      (assert-equal (trie-value trie) "win!")
      (assert-equal (trie-values trie) '("win!"))
      (assert-equal (trie->alist trie) '((() . "win!"))))))

(define-test 'trie-non-empty
  (lambda ()
    (let ((trie (make-trie))
	  (vals (iota (length paths))))
      (let ((vals* (map (lambda (val) (+ val 57)) vals))
	    (leaves
	     (map (lambda (path)
		    (let ((trie* (trie-intern! trie path)))
		      (assert-!equal trie* trie)
		      (assert-false (trie-has-value? trie*))
		      trie*))
		  paths)))

	;; Confirm that looking up again gets same node.
	(for-each (lambda (path leaf)
		    (assert-eq (trie-find trie path) leaf))
		  paths
		  leaves)

	;; Now set values on each node and check that they persist.
	(for-each set-trie-value!
		  leaves
		  vals)
	(for-each (lambda (path val)
		    (let ((leaf (trie-find trie path)))
		      (assert-true leaf)
		      (assert-true (trie-has-value? leaf))
		      (assert-equal (trie-value leaf) val)))
		  paths
		  vals)
	(assert-lset= equal? (trie-values trie) vals)
	(assert-lset= equal? (trie->alist trie) (map cons paths vals))

	;; Now modify the values and check that they persist.
	(for-each set-trie-value!
		  leaves
		  vals*)
	(for-each (lambda (path val)
		    (let ((leaf (trie-find trie path)))
		      (assert-true leaf)
		      (assert-true (trie-has-value? leaf))
		      (assert-!equal (trie-value leaf) val)))
		  paths
		  vals)
	(for-each (lambda (path val)
		    (let ((leaf (trie-find trie path)))
		      (assert-true leaf)
		      (assert-true (trie-has-value? leaf))
		      (assert-equal (trie-value leaf) val)))
		  paths
		  vals*)
	(assert-lset= equal? (trie-values trie) vals*)
	(assert-lset= equal? (trie->alist trie) (map cons paths vals*))))))

(define-test 'alist->trie
  (lambda ()
    (let-values (((vals alist trie) (simple-example)))
      (assert-lset= equal? (trie-values trie) vals)
      (assert-lset= equal? (trie->alist trie) alist))))

(define-test 'trie-fold
  (lambda ()
    (let-values (((vals alist trie) (simple-example)))
      (assert-lset= equal?
		    (trie-fold (lambda (path value acc)
				 (declare (ignore value))
				 (cons path acc))
			       '()
			       trie)
		    paths)
      (assert-lset= equal?
		    (trie-fold (lambda (path value acc)
				 (if (= 1 (length path))
				     (cons value acc)
				     acc))
			       '()
			       trie)
		    (filter-map (lambda (p)
				  (and (= 1 (length (car p)))
				       (cdr p)))
				alist)))))

(define-test 'trie-for-each
  (lambda ()
    (let-values (((vals alist trie) (simple-example)))
      (let ((paths* '()))
	(trie-for-each (lambda (path value)
			 (declare (ignore value))
			 (set! paths* (cons path paths*))
			 unspecific)
		       trie)
	(assert-lset= equal? paths* paths))
      (let ((vals* '()))
	(trie-for-each (lambda (path value)
			 (if (= 1 (length path))
			     (set! vals* (cons value vals*)))
			 unspecific)
		       trie)
	(assert-lset= equal?
		      vals*
		      (filter-map (lambda (p)
				    (and (= 1 (length (car p)))
					 (cdr p)))
				  alist))))))