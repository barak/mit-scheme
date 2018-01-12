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

;;;; Predicates: compound
;;; package: (runtime compound-predicate)

(declare (usual-integrations))

(define (make-compound-tag datum-test operator operands)
  (%make-compound-tag tagging-strategy:optional datum-test operator operands))

(define (%make-compound-tag tagging-strategy datum-test operator operands)
  (tagging-strategy datum-test
    (lambda (predicate tagger)
      (make-tag (cons operator (map tag-name operands))
		predicate tagger operator
		(make-compound-tag-extra operator operands)))))

(define (tag-is-compound? tag)
  (compound-tag-extra? (tag-extra tag)))

(define (compound-tag-operator tag)
  (compound-tag-extra-operator (tag-extra tag)))

(define (compound-tag-operands tag)
  (compound-tag-extra-operands (tag-extra tag)))

(define-record-type <compound-tag-extra>
    (make-compound-tag-extra operator operands)
    compound-tag-extra?
  (operator compound-tag-extra-operator)
  (operands compound-tag-extra-operands))

(define (tag-is-disjoin? object)
  (and (tag-is-compound? object)
       (eq? 'disjoin (compound-tag-operator object))))

(define (tag-is-conjoin? object)
  (and (tag-is-compound? object)
       (eq? 'conjoin (compound-tag-operator object))))

(add-boot-init!
 (lambda ()

   (define-tag<= tag? tag-is-disjoin?
     (lambda (tag1 tag2)
       (any (lambda (component2)
	      (tag<= tag1 component2))
	    (compound-tag-operands tag2))))

   (define-tag<= tag-is-conjoin? tag?
     (lambda (tag1 tag2)
       (any (lambda (component1)
	      (tag<= component1 tag2))
	    (compound-tag-operands tag1))))))

(define (compound-predicate? object)
  (and (predicate? object)
       (tag-is-compound? (predicate->tag object))))

(add-boot-init!
 (lambda ()
   (register-predicate! compound-predicate? 'compound-predicate
			'<= predicate?)))

(define (compound-predicate-operator predicate)
  (compound-tag-operator (predicate->tag predicate)))

(define (compound-predicate-operands predicate)
  (map tag->predicate (compound-tag-operands (predicate->tag predicate))))

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
      (tag->predicate
       ((compound-operator-builder operator)
	datum-test
	operator
	(map predicate->tag operands)))
      datum-test))

(define compound-operator?)
(define compound-operator-builder)
(define define-compound-operator)
(add-boot-init!
 (lambda ()
   (let ((table (make-hashed-metadata-table)))
     (set! compound-operator? (table 'has?))
     (set! compound-operator-builder (table 'get))
     (set! define-compound-operator (table 'put!)))
   (register-predicate! compound-operator? 'compound-predicate '<= symbol?)))

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
		    (if (and (tag-is-compound? tag)
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
     (make-joinish-memoizer tag-is-top?))

   (define-compound-operator 'conjoin
     (make-joinish-memoizer tag-is-bottom?))))