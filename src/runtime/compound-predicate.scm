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

(define compound-tag-metatag (make-dispatch-metatag 'compound-tag))
(define compound-tag? (dispatch-tag->predicate compound-tag-metatag))

(define %make-compound-tag
  (dispatch-metatag-constructor compound-tag-metatag 'make-compound-tag))

(define (make-compound-tag predicate operator operands)
  (%make-compound-tag (cons operator (map dispatch-tag-name operands))
		      predicate
		      operator
		      operands))

(define-integrable (compound-tag-operator tag)
  (dispatch-tag-extra-ref tag 0))

(define-integrable (compound-tag-operands tag)
  (dispatch-tag-extra-ref tag 1))

(define (tag-is-disjoin? object)
  (and (compound-tag? object)
       (eq? 'disjoin (compound-tag-operator object))))

(define (tag-is-conjoin? object)
  (and (compound-tag? object)
       (eq? 'conjoin (compound-tag-operator object))))

(add-boot-init!
 (lambda ()

   (define-dispatch-tag<= dispatch-tag? tag-is-disjoin?
     (lambda (tag1 tag2)
       (any (lambda (component2)
	      (dispatch-tag<= tag1 component2))
	    (compound-tag-operands tag2))))

   (define-dispatch-tag<= tag-is-conjoin? dispatch-tag?
     (lambda (tag1 tag2)
       (any (lambda (component1)
	      (dispatch-tag<= component1 tag2))
	    (compound-tag-operands tag1))))))

(define (compound-predicate? object)
  (and (predicate? object)
       (compound-tag? (predicate->dispatch-tag object))))

(add-boot-init!
 (lambda ()
   (register-predicate! compound-predicate? 'compound-predicate
			'<= predicate?)))

(define (compound-predicate-operator predicate)
  (compound-tag-operator (predicate->dispatch-tag predicate)))

(define (compound-predicate-operands predicate)
  (map dispatch-tag->predicate
       (compound-tag-operands (predicate->dispatch-tag predicate))))

(define (disjoin . predicates)
  (disjoin* predicates))

(define (disjoin* predicates)
  (make-predicate (lambda (object)
		    (any (lambda (predicate)
			   (predicate object))
			 predicates))
		  'disjoin
		  predicates))

(define (conjoin . predicates)
  (conjoin* predicates))

(define (conjoin* predicates)
  (make-predicate (lambda (object)
		    (every (lambda (predicate)
			     (predicate object))
			   predicates))
		  'conjoin
		  predicates))

(define (make-predicate datum-test operator operands)
  (if (every predicate? operands)
      (dispatch-tag->predicate
       ((compound-operator-builder operator)
	datum-test
	operator
	(map predicate->dispatch-tag operands)))
      datum-test))

(define compound-operator-builder)
(define define-compound-operator)
(add-boot-init!
 (lambda ()
   (let ((table (make-alist-metadata-table)))
     (set! compound-operator-builder (bundle-ref table 'get))
     (set! define-compound-operator (bundle-ref table 'put!))
     unspecific)))

(add-boot-init!
 (lambda ()

   (define (make-joinish-memoizer tag-is-limit?)
     (let ((memoizer
	    (simple-lset-memoizer eq?
	      (lambda (datum-test operator tags)
		(declare (ignore datum-test operator))
		tags)
	      make-compound-tag)))
       (lambda (datum-test operator tags)
	 (let ((tags
		(delete-duplicates
		 (append-map
		  (lambda (tag)
		    (if (and (compound-tag? tag)
			     (eq? operator
				  (compound-tag-operator tag)))
			(compound-tag-operands tag)
			(list tag)))
		  tags)
		 eq?)))
	   (if (and (pair? tags) (null? (cdr tags)))
	       (car tags)
	       (or (find tag-is-limit? tags)
		   (memoizer datum-test operator tags)))))))

   (define-compound-operator 'disjoin
     (make-joinish-memoizer dispatch-tag-is-top?))

   (define-compound-operator 'conjoin
     (make-joinish-memoizer dispatch-tag-is-bottom?))))