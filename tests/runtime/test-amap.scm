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
  fixnum-comparator)

(define test-value-comparator
  fixnum-comparator)

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
  (filter (lambda (name)
	    (and (amap-implementation-supports-comparator? name
							   test-key-comparator)
		 (amap-implementation-supports-args? name '())))
	  (amap-implementation-names)))

(define ordered-and-hashable-comparator fixnum-comparator)
(define hashable-comparator (make-eqv-comparator))
(define ordered-comparator (make-comparator symbol? eq? symbol<? #f))
(define neither-comparator (make-comparator any-object? eqv? #f #f))

(define base-key-comparators
  (list ordered-and-hashable-comparator
	hashable-comparator
	ordered-comparator
	neither-comparator))

(define list-key-comparators
  (map uniform-list-comparator base-key-comparators))

(define lset-key-comparators		;always ordered
  (map lset-comparator base-key-comparators))

(define test-key-comparators
  (append base-key-comparators
	  list-key-comparators
	  lset-key-comparators))

(define (at-least-one-of items)
  (delq '() (all-choices-of items)))

(define (at-most-one-of items)
  (cons '() (map list items)))

(define (all-choices-of items)

  (define (choose k items)
    (if (> k 0)
	(if (< k (length items))
	    (append (map (lambda (tail)
			   (cons (car items) tail))
			 (choose (- k 1) (cdr items)))
		    (choose k (cdr items)))
	    (list items))
	'(())))

  (let ((n (length items)))
    (let loop ((k 0))
      (if (< k n)
	  (append (choose k items)
		  (loop (+ k 1)))
	  (choose n items)))))

(define (good-comparator-pred name)
  (case name
    ((hash-table) comparator-hashable?)
    ((alist) any-object?)
    ((trie) uniform-list-comparator?)
    ((red/black-tree) comparator-ordered?)
    (else (error "Unknown implementation type:" name))))

(define (all-choices)
  (all-choices-of (subst-args (all-amap-args))))

(define (subst-args args)
  (map (lambda (arg)
	 (if (eqv? exact-nonnegative-integer? arg)
	     7
	     arg))
       args))

(define (good-choices name)
  (let ((supported (amap-implementation-supported-args name)))
    (append-choices (good-kv-choices supported)
		    (good-time-choices supported)
		    (good-other-choices supported))))

(define (good-kv-choices supported)
  (append '(())
	  (at-least-one-of (match-args supported kv-weak-args))
	  (at-least-one-of (match-args supported kv-ephemeral-args))))

(define kv-weak-args
  '(weak-keys weak-values))

(define kv-ephemeral-args
  '(ephemeral-keys ephemeral-values))

(define (good-time-choices supported)
  (at-most-one-of (match-args supported time-args)))

(define time-args
  '(amortized-constant-time log-time sublinear-time linear-time))

(define (good-other-choices supported)
  (all-choices-of (subst-args (match-args supported other-args))))

(define other-args
  `(thread-safe ordered-by-key ,exact-nonnegative-integer?))

(define (match-args a1 a2)
  (lset-intersection eqv? a1 a2))

(define (append-choices . choices-list)
  (reduce-right (lambda (prefixes suffixes)
		  (append-map (lambda (prefix)
				(map (lambda (suffix)
				       (append prefix suffix))
				     suffixes))
			      prefixes))
		'()
		choices-list))

(define (explicit-construction-tests name)
  (lambda ()

    (define (test-make comparator args)
      (if (and (good-comparator? comparator) (good-args? args))
	  (test-good comparator args)
	  (test-bad comparator args)))

    (define (test-good comparator args)
      (let ((amap (run-make comparator args)))
	(assert-true (amap? amap))
	(assert-equal (amap-implementation-name amap) name)
	(assert-eqv (amap-comparator amap) comparator)
	(assert-equal (amap-args amap) (massage-args args))))

    (define (test-bad comparator args)
      (assert-error (lambda () (run-make comparator args))
		    (default-object)
		    'expression `(make ,name ,comparator ,args)))

    (define (good-comparator? comparator)
      (amap-implementation-supports-comparator? name comparator))

    (define (good-args? args)
      (amap-implementation-supports-args? name args))

    (define (run-make comparator args)
      (apply make-amap comparator (massage-args args)))

    (define (massage-args args)
      (cons* name
	     'not-an-arg
	     '(not-an-arg)
	     "not-an-arg"
	     (subst-args args)))

    (define (test-good-args args expected)
      (assert-boolean= (good-args? (subst-args args)) expected
		       'expression `(good-args? ,name ,(subst-args args))))

    (let ((all-args (all-choices-of (amap-implementation-supported-args name))))
      (for-each (lambda (comparator)
		  (for-each (lambda (args)
			      (test-make comparator args))
			    all-args))
		test-key-comparators))
    (let-values (((good-comparators bad-comparators)
		  (partition (good-comparator-pred name)
			     test-key-comparators)))
      (let* ((good-args (good-choices name))
	     (bad-args
	      (lset-difference (lambda (a b)
				 (lset= equal? a b))
			       (all-choices)
			       good-args))
	     (bad-args (if keep-it-fast!? (take bad-args 256) bad-args)))
	(for-each (lambda (comparator)
		    (assert-true (good-comparator? comparator))
		    (for-each (lambda (args)
				(test-good-args args #t)
				(test-good comparator args))
			      good-args)
		    (for-each (lambda (args)
				(test-good-args args #f)
				(test-bad comparator args))
			      bad-args))
		  good-comparators)
	(for-each (lambda (comparator)
		    (assert-false (good-comparator? comparator))
		    (for-each (lambda (args)
				(test-good-args args #t)
				(test-bad comparator args))
			      good-args)
		    (for-each (lambda (args)
				(test-good-args args #f)
				(test-bad comparator args))
			      bad-args))
		  bad-comparators)))))

(define-test 'explicit-construction
  (map explicit-construction-tests (amap-implementation-names)))

(define-test 'implicit-construction
  (lambda ()

    (define (test-make comparator args impls)
	(if (null? impls)
	    (assert-error (lambda () (apply make-amap comparator args)))
	    (let ((impl (apply make-amap comparator args)))
	      (assert-memv (amap-implementation-name impl) impls
			   'expression `(make-amap ,comparator ,args)))))

    (define (supporting-impls comparator args)
      (filter (lambda (name)
		(and (amap-implementation-supports-comparator? name comparator)
		     (amap-implementation-supports-args? name args)))
              (amap-implementation-names)))

    (for-each
     (lambda (comp)
       (let ((max-expected
	      (let ((opt
		     (lambda (pred impl)
		       (if (pred comp) (list impl) '()))))
		`(alist ,@(opt comparator-hashable? 'hash-table)
			,@(opt comparator-ordered? 'red/black-tree)
			,@(opt uniform-list-comparator? 'trie)))))

	 (define (test-impls best . arg-lists)
	   (for-each (lambda (args)
		       (let ((impls (supporting-impls comp args)))
			 (assert-lset= eq?
				       impls
				       (lset-intersection eq? best max-expected)
				       'expression
				       `(supporting-impls ,comp ,args))
			 (test-make comp args impls)))
		     arg-lists))

	 (test-impls '(alist hash-table red/black-tree trie)
		     '())

	 (test-impls '(hash-table)
		     '(weak-values)
		     '(weak-keys weak-values)
		     '(ephemeral-keys)
		     '(ephemeral-values)
		     '(ephemeral-keys ephemeral-values)
		     '(7)
		     '(amortized-constant-time))

	 (test-impls '(red/black-tree)
		     '(log-time)
		     '(ordered-by-key))

	 (test-impls '(alist)
		     '(linear-time))

	 (test-impls '(alist hash-table) '(weak-keys))
	 (test-impls '(hash-table red/black-tree) '(sublinear-time))

	 ;; Illegal
	 (test-impls '()
		     '(weak-keys ephemeral-keys)
		     '(weak-values ephemeral-values))

	 ;; Not supported but not illegal
	 (test-impls '()
		     '(weak-keys ephemeral-values)
		     '(ephemeral-keys weak-values))))
     test-key-comparators)))

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