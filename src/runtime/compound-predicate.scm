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

;;;; Predicates: compound
;;; package: (runtime compound-predicate)

(declare (usual-integrations))

(add-boot-deps! '(runtime predicate)
		'(runtime hash-table))

(define compound-tag-metatag (make-dispatch-metatag 'compound-tag))
(define compound-tag? (dispatch-tag->predicate compound-tag-metatag))

(define %make-compound-tag
  (dispatch-metatag-constructor compound-tag-metatag 'make-compound-tag))

(define (make-compound-tag predicate key operands)
  (%make-compound-tag (cons (key-operator key) (map dispatch-tag-name operands))
		      predicate
		      key
		      operands))

(define-integrable (compound-tag-key tag)
  (dispatch-tag-extra-ref tag 0))

(define-integrable (%set-compound-tag-key! tag key)
  (%dispatch-tag-extra-set! tag 0 key))

(define-integrable (compound-tag-operands tag)
  (dispatch-tag-extra-ref tag 1))

(define (compound-predicate? object)
  (and (predicate? object)
       (compound-tag? (predicate->dispatch-tag object))))

(add-boot-init!
 (lambda ()
   (register-predicate! compound-tag? 'compound-tag '<= dispatch-tag?)
   (register-predicate! compound-predicate? 'compound-predicate
			'<= predicate?)))

(define (compound-predicate-operands predicate)
  (map dispatch-tag->predicate
       (compound-tag-operands (predicate->dispatch-tag predicate))))

;;;; Constructors

(define (compound-predicate-constructor operator make-datum-test
					make-predicates make-memoizer)
  (let-values (((constructor predicate-predicate key)
		(%constructor operator make-datum-test make-predicates
			      make-memoizer)))
    (declare (ignore key))
    (values constructor predicate-predicate)))

(define (%constructor operator make-datum-test make-operands make-memoizer)
  (let ((key (make-key operator)))

    (define (related-predicate? object)
      (and (predicate? object)
	   (keyed-tag? key (predicate->dispatch-tag object))))
    (add-boot-init!
     (lambda ()
       (register-predicate! related-predicate? (symbol operator '-predicate)
			    '<= compound-predicate?)))

    (values (lambda args
	      (let ((datum-test (apply make-datum-test args))
		    (operands (apply make-operands args))
		    (memoizer (make-memoizer key)))
		(guarantee-list-of unary-procedure? operands)
		(if (every predicate? operands)
		    (dispatch-tag->predicate
		     (memoizer datum-test
			       (map predicate->dispatch-tag operands)))
		    datum-test)))
	    related-predicate?
	    key)))

(define (tag-predicate key)
  (define (related-tag? object)
    (keyed-tag? key object))
  (register-predicate! related-tag? (symbol (key-operator key) '-tag)
		       '<= compound-tag?)
  related-tag?)

(define-record-type <key>
    (make-key operator)
    key?
  (operator key-operator))

(define (keyed-tag? key object)
  (and (compound-tag? object)
       (eq? key (compound-tag-key object))))

;;;; Memoizers

(define (single-predicate-memoizer key)
  (let ((table
	 (hash-table-intern! single-predicates-tables
			     key
			     make-key-weak-eqv-hash-table)))
    (lambda (datum-test tags)
      (hash-table-intern! table (car tags)
	(lambda ()
	  (make-compound-tag datum-test key tags))))))

(define-deferred single-predicates-tables
  (make-strong-eq-hash-table))

(define (ordered-predicates-memoizer key)
  (let ((table
	 (hash-table-intern! ordered-predicates-tables
			     key
			     (hash-table-constructor
			      (uniform-weak-list-comparator
			       (make-eqv-comparator))))))
    (lambda (datum-test tags)
      (hash-table-intern! table (list->weak-list tags)
	(lambda ()
	  (make-compound-tag datum-test key tags))))))

(define-deferred ordered-predicates-tables
  (make-strong-eq-hash-table))

(define (unordered-predicates-memoizer key)
  (let ((table (%get-unordered-predicates-table key)))
    (lambda (datum-test tags)
      (hash-table-intern! table (list->weak-list tags)
	(lambda ()
	  (make-compound-tag datum-test key tags))))))

(define (%get-unordered-predicates-table key)
  (hash-table-intern! unordered-predicates-tables
		      key
		      (hash-table-constructor
		       (weak-lset-comparator (make-eqv-comparator)))))

(define-deferred unordered-predicates-tables
  (make-strong-eq-hash-table))

;;;; Disjoin and conjoin

(define ((joinish-memoizer tag-is-limit?) key)
  (let ((memoizer (unordered-predicates-memoizer key)))
    (lambda (datum-test tags)
      (let ((tags*
	     (fold (lambda (tag tags)
		     (if (keyed-tag? key tag)
			 (lset-union eq? (compound-tag-operands tag) tags)
			 (lset-adjoin eq? tags tag)))
		   '()
		   tags)))
	(if (and (pair? tags*) (null? (cdr tags*)))
	    (car tags*)
	    (or (find tag-is-limit? tags*)
		(memoizer datum-test tags*)))))))

(define (disjoin . predicates)
  (disjoin* predicates))

(define-values (disjoin* disjoin? disjoin-key)
  (%constructor 'disjoin
    (lambda (predicates)
      (lambda (object)
	(any (lambda (predicate)
	       (predicate object))
	     predicates)))
    (lambda (predicates) predicates)
    (joinish-memoizer dispatch-tag-is-top?)))

(define-deferred disjoin-tag?
  (tag-predicate disjoin-key))

(define (conjoin . predicates)
  (conjoin* predicates))

(define-values (conjoin* conjoin? conjoin-key)
  (%constructor 'conjoin
    (lambda (predicates)
      (lambda (object)
	(every (lambda (predicate)
		 (predicate object))
	       predicates)))
    (lambda (predicates) predicates)
    (joinish-memoizer dispatch-tag-is-bottom?)))

(define-deferred conjoin-tag?
  (tag-predicate conjoin-key))

(add-boot-init!
 (lambda ()
   (define-dispatch-tag<= dispatch-tag? disjoin-tag?
     (lambda (tag1 tag2)
       (any (lambda (component2)
	      (dispatch-tag<= tag1 component2))
	    (compound-tag-operands tag2))))
   (define-dispatch-tag<= conjoin-tag? dispatch-tag?
     (lambda (tag1 tag2)
       (any (lambda (component1)
	      (dispatch-tag<= component1 tag2))
	    (compound-tag-operands tag1))))))

;;; Finish initializing top and bottom.
(let ((seq (boot-sequencer)))
  (seq 'add-before! (current-package-sequencer))
  (seq 'add-before! (package-name->sequencer '(runtime comparator)))
  (seq 'add-action!
    (lambda ()
      (%set-compound-tag-key! the-bottom-dispatch-tag disjoin-key)
      (hash-table-set! (%get-unordered-predicates-table disjoin-key)
		       '()
		       the-bottom-dispatch-tag)
      (%set-compound-tag-key! the-top-dispatch-tag conjoin-key)
      (hash-table-set! (%get-unordered-predicates-table conjoin-key)
		       '()
		       the-top-dispatch-tag))))

;;;; Other combinators

(define-values (complement complement?)
  (compound-predicate-constructor 'complement
    (lambda (predicate)
      (lambda (object)
	(not (predicate object))))
    list
    (lambda (key)
      (let ((memoizer (single-predicate-memoizer key)))
	(lambda (datum-test tags)
	  (if (keyed-tag? key (car tags))
	      (car (compound-tag-operands (car tags)))
	      (memoizer datum-test tags)))))))

(define-values (pair-predicate pair-predicate?)
  (compound-predicate-constructor 'pair
    (lambda (car-pred cdr-pred)
      (lambda (object)
	(and (pair? object)
	     (car-pred (car object))
	     (cdr-pred (cdr object)))))
    list
    ordered-predicates-memoizer))

(define-values (uniform-list-predicate uniform-list-predicate?)
  (compound-predicate-constructor 'uniform-list
    (lambda (elt-pred)
      (lambda (object)
	(list-of-type? object elt-pred)))
    list
    single-predicate-memoizer))

(define-values (uniform-weak-list-predicate uniform-weak-list-predicate?)
  (compound-predicate-constructor 'uniform-weak-list
    (lambda (elt-pred)
      (lambda (object)
	(weak-list-of-type? object elt-pred)))
    list
    single-predicate-memoizer))

(define-values (lset-predicate lset-predicate?)
  (compound-predicate-constructor 'lset
    (lambda (elt-pred)
      (lambda (object)
	(list-of-type? object elt-pred)))
    list
    single-predicate-memoizer))

(define-values (weak-lset-predicate weak-lset-predicate?)
  (compound-predicate-constructor 'weak-lset
    (lambda (elt-pred)
      (lambda (object)
	(weak-list-of-type? object elt-pred)))
    list
    single-predicate-memoizer))