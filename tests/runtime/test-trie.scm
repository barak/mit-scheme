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
  (list '()
	'(a b)
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
      (define (empty-assertions)
	(assert-false (trie-has-value? trie))
	(assert-error (lambda () (trie-value trie)))
	(assert-equal (find-subtrie trie '()) trie)
	(assert-equal (intern-subtrie! trie '()) trie)
	(assert-error (lambda () (trie-ref trie '())))
	(assert-equal (trie-paths trie) '())
	(assert-equal (trie-values trie) '())
	(assert-equal (trie->alist trie) '())
	(for-each (lambda (path)
		    (if (null? path)
			(assert-equal (find-subtrie trie path) trie)
			(assert-false (find-subtrie trie path)))
		    (assert-error (lambda () (trie-ref trie path))))
		  paths))

      (define (non-empty-assertions value)
	(assert-true (trie-has-value? trie))
	(assert-equal (trie-value trie) value)
	(assert-equal (trie-paths trie) '(()))
	(assert-equal (trie-values trie) `(,value))
	(assert-equal (trie->alist trie) `((() . ,value))))

      (empty-assertions)

      (set-trie-value! trie "win!")
      (non-empty-assertions "win!")

      (delete-trie-value! trie)
      (empty-assertions)

      (trie-set! trie '() "foo")
      (non-empty-assertions "foo")

      (trie-clear! trie)
      (empty-assertions))))

(define-test 'trie-non-empty
  (lambda ()
    (let ((trie (make-trie))
	  (vals (iota (length paths))))
      (let ((vals* (map (lambda (val) (+ val 57)) vals))
	    (leaves
	     (map (lambda (path)
		    (let ((trie* (intern-subtrie! trie path)))
		      (if (null? path)
			  (assert-equal trie* trie)
			  (assert-!equal trie* trie))
		      (assert-false (trie-has-value? trie*))
		      trie*))
		  paths)))

	;; Confirm that looking up again gets same node.
	(for-each (lambda (path leaf)
		    (assert-eq (find-subtrie trie path) leaf))
		  paths
		  leaves)

	;; Now set values on each node and check that they persist.
	(for-each set-trie-value!
		  leaves
		  vals)
	(for-each (lambda (path val)
		    (let ((leaf (find-subtrie trie path)))
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
		    (let ((leaf (find-subtrie trie path)))
		      (assert-true leaf)
		      (assert-true (trie-has-value? leaf))
		      (assert-!equal (trie-value leaf) val)))
		  paths
		  vals)
	(for-each (lambda (path val)
		    (let ((leaf (find-subtrie trie path)))
		      (assert-true leaf)
		      (assert-true (trie-has-value? leaf))
		      (assert-equal (trie-value leaf) val)))
		  paths
		  vals*)
	(assert-lset= equal? (trie-values trie) vals*)
	(assert-lset= equal? (trie->alist trie) (map cons paths vals*))))))

(define (edge-example)
  (let ((trie (make-trie))
	(paths '((a) (b) (c)))
	(vals '(2 3 5)))
    (for-each (lambda (path value)
		(trie-set! trie path value))
	      paths
	      vals)
    (values trie
	    (map (lambda (path)
		   (cons (car path)
			 (find-subtrie trie path)))
		 paths))))

(define-test 'trie-edge-find
  (lambda ()
    (let-values (((trie expected-alist) (edge-example)))
      (let ((alist '()))
	(assert-false
	 (trie-edge-find (lambda (key trie*)
			   (set! alist (alist-cons key trie* alist))
			   #f)
			 trie))
	(assert-lset= equal? alist expected-alist))
      (assert-true
       (trie-edge-find (lambda (key trie*)
			 (declare (ignore key))
			 (and (trie-has-value? trie*)
			      (even? (trie-value trie*))))
		       trie))
      (assert-true
       (trie-edge-find (lambda (key trie*)
			 (declare (ignore key))
			 (and (trie-has-value? trie*)
			      (odd? (trie-value trie*))))
		       trie))
      (assert-false
       (trie-edge-find (lambda (key trie*)
			 (declare (ignore key))
			 (and (trie-has-value? trie*)
			      (zero? (trie-value trie*))))
		       trie)))))

(define-test 'trie-edge-fold
  (lambda ()
    (let-values (((trie expected-alist) (edge-example)))
      (assert-lset= equal?
		    (trie-edge-fold (lambda (key trie* acc)
				      (alist-cons key trie* acc))
				    '()
				    trie)
		    expected-alist)
      (let ((expected
	     (fold (lambda (p acc)
		     (cons* (cdr p) (car p) acc))
		   '(the-end)
		   expected-alist))
	    (actual
	     (trie-edge-fold (lambda (key trie* acc)
			       (cons* trie* key acc))
			     '(the-end)
			     trie)))
	(assert-lset= equal? actual expected)
	(assert-equal (last actual) (last expected))))))

(define-test 'trie-edge-prune!
  (lambda ()

    (define (try predicate)
      (let-values (((trie expected-alist) (edge-example)))
	(trie-edge-prune! predicate trie)
	(assert-lset= equal?
		      (trie-edge-fold (lambda (key trie* acc)
					(alist-cons key trie* acc))
				      '()
				      trie)
		      (remove (lambda (p)
				(predicate (car p) (cdr p)))
			      expected-alist))))

    (try (lambda (key trie*)
	   (declare (ignore trie*))
	   (eq? 'a key)))
    (try (lambda (key trie*)
	   (declare (ignore trie*))
	   (eq? 'b key)))
    (try (lambda (key trie*)
	   (declare (ignore trie*))
	   (eq? 'c key)))
    (try (lambda (key trie*)
	   (declare (ignore trie*))
	   (or (eq? 'a key) (eq? 'c key))))))

(define-test 'alist->trie
  (lambda ()
    (let-values (((vals alist trie) (simple-example)))
      (assert-lset= equal? (trie-values trie) vals)
      (assert-lset= equal? (trie->alist trie) alist))))

(define-test 'trie-fold
  (lambda ()
    (let-values (((vals alist trie) (simple-example)))
      (declare (ignore vals))
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
      (declare (ignore vals))
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

(define-test 'trie-clean!
  (lambda ()
    (let-values (((vals alist trie) (simple-example)))
      (declare (ignore vals))
      (for-each (lambda (p)
		  (assert-equal (trie-ref trie (car p)) (cdr p)))
		alist)
      (trie-clear! trie)
      (for-each (lambda (p)
		  (assert-error (lambda () (trie-ref trie (car p)))))
		alist)
      )))