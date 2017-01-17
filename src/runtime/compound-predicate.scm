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
  (make-tag (cons operator (map tag-name operands))
            datum-test
	    predicate-tagging-strategy:optional
	    operator
            (make-compound-tag-extra operator operands)))

(define (compound-tag? object)
  (and (tag? object)
       (tag-is-compound? object)))

(add-boot-init!
 (lambda ()
   (register-predicate! compound-tag? 'compound-tag '<= tag?)))

(define (tag-is-compound? tag)
  (or (compound-tag-extra? (tag-extra tag))
      (top-tag? tag)
      (bottom-tag? tag)))

(define (compound-tag-operator tag)
  (cond ((compound-tag-extra? (tag-extra tag))
         (compound-tag-extra-operator (tag-extra tag)))
        ((top-tag? tag) 'conjoin)
        ((bottom-tag? tag) 'disjoin)
        (else (error:not-a compound-tag? tag 'compound-tag-operator))))

(define (compound-tag-operands tag)
  (cond ((compound-tag-extra? (tag-extra tag))
         (compound-tag-extra-operands (tag-extra tag)))
        ((top-tag? tag) '())
        ((bottom-tag? tag) '())
        (else (error:not-a compound-tag? tag 'compound-tag-operands))))

(define-record-type <compound-tag-extra>
    (make-compound-tag-extra operator operands)
    compound-tag-extra?
  (operator compound-tag-extra-operator)
  (operands compound-tag-extra-operands))

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

(define (compound-predicate-predicate operator)
  (define (predicate object)
    (and (predicate? object)
         (let ((tag (predicate->tag object)))
           (and (tag-is-compound? tag)
                (eq? operator (compound-tag-operator tag))))))
  (register-predicate! predicate `(compound-predicate-predicate ,operator)
                       '<= compound-predicate?)
  predicate)

(define (disjoin . predicates)
  (disjoin* predicates))

(define (unmemoized:disjoin* predicates)
  (lambda (object)
    (any (lambda (predicate)
           (predicate object))
         predicates)))

(define (conjoin . predicates)
  (conjoin* predicates))

(define (unmemoized:conjoin* predicates)
  (lambda (object)
    (every (lambda (predicate)
             (predicate object))
           predicates)))

(define (unmemoized:is-list-of predicate)
  (lambda (object)
    (and (list? object)
         (every predicate object))))

(define (unmemoized:is-non-empty-list-of predicate)
  (lambda (object)
    (and (non-empty-list? object)
         (every predicate object))))

(define (unmemoized:is-pair-of car-predicate cdr-predicate)
  (lambda (object)
    (and (pair? object)
         (car-predicate (car object))
         (cdr-predicate (cdr object)))))

(define (memoize-uniform-nary operator nullary procedure)
  (let ((memoizer
         (lset-memoizer eqv?
                        (lambda (predicates) predicates)
                        (lambda (predicates)
                          (make-predicate (lambda () (procedure predicates))
                                          operator
                                          predicates)))))
    (lambda (predicates)
      (guarantee list? predicates)
      (let ((predicates (delete-duplicates predicates eqv?)))
        (cond ((null? predicates)
               nullary)
              ((and (pair? predicates) (null? (cdr predicates)))
               (car predicates))
              (else
               (memoizer predicates)))))))

(define (memoize-unary operator procedure)
  (weak-eqv-memoizer (lambda (p1) p1)
                     (lambda (p1)
                       (make-predicate (lambda () (procedure p1))
                                       operator
                                       (list p1)))))

(define (memoize-binary operator procedure)
  (list-memoizer eqv?
                 (lambda (p1 p2) (list p1 p2))
                 (lambda (p1 p2)
                   (make-predicate (lambda () (procedure p1 p2))
                                   operator
                                   (list p1 p2)))))

(define (make-predicate get-predicate operator operands)
  (tag->predicate
   (let ((builder (get-compound-operator-builder operator #f))
         (operand-tags (map predicate->tag operands)))
     (if (not builder)
         (error:not-a compound-operator? operator 'make-predicate))
     (builder (lambda ()
                (make-compound-tag (get-predicate) operator operand-tags))
              operator
              operand-tags))))

(define compound-operator?)
(define get-compound-operator-builder)
(define set-compound-operator-builder!)
(add-boot-init!
 (lambda ()
   (let ((table (make-hashed-metadata-table)))
     (set! compound-operator? (table 'has?))
     (set! get-compound-operator-builder (table 'get-if-available))
     (set! set-compound-operator-builder! (table 'put!))
     unspecific)
   (register-predicate! compound-operator? 'compound-predicate '<= symbol?)))

(define (define-compound-operator operator builder)
  (guarantee symbol? operator 'define-compound-operator)
  (set-compound-operator-builder! operator builder)
  operator)

(add-boot-init!
 (lambda ()

   (define (builder:uniform-nary builder)
     (lambda (get-tag operator operand-tags)
       (let ((operand-tags
	      (append-map (lambda (tag)
			    (if (and (tag-is-compound? tag)
				     (eq? operator (compound-tag-operator tag)))
				(compound-tag-operands tag)
				(list tag)))
			  operand-tags)))
	 (if (and (pair? operand-tags) (null? (cdr operand-tags)))
	     (car operand-tags)
	     (builder get-tag operand-tags)))))

   (define-compound-operator 'disjoin
     (builder:uniform-nary
      (lambda (get-tag operand-tags)
	(if (any top-tag? operand-tags)
	    (top-tag)
	    (let ((tag (get-tag)))
	      (for-each (lambda (tag*)
			  (set-tag<=! tag* tag))
			operand-tags)
	      tag)))))

   (define-compound-operator 'conjoin
     (builder:uniform-nary
      (lambda (get-tag operand-tags)
	(if (any bottom-tag? operand-tags)
	    (bottom-tag)
	    (let ((tag (get-tag)))
	      (for-each (lambda (tag*)
			  (set-tag<=! tag tag*))
			operand-tags)
	      tag)))))))

(add-boot-init!
 (lambda ()

   (define (simple-nary superset)
     (let ((superset-tag (predicate->tag superset)))
       (lambda (get-tag operator operand-tags)
	 operator operand-tags
	 (let ((tag (get-tag)))
	   (set-tag<=! tag superset-tag)
	   tag))))

   (define-compound-operator 'is-list-of (simple-nary list?))
   (define-compound-operator 'is-non-empty-list-of
     (simple-nary non-empty-list?))
   (define-compound-operator 'is-pair-of (simple-nary pair?))))

(define disjoin*)
(define conjoin*)
(define is-list-of)
(define is-non-empty-list-of)
(define is-pair-of)
(add-boot-init!
 (lambda ()
   (set! disjoin*
	 (memoize-uniform-nary 'disjoin no-object? unmemoized:disjoin*))
   (set! conjoin*
	 (memoize-uniform-nary 'conjoin any-object? unmemoized:conjoin*))
   (set! is-list-of
	 (memoize-unary 'is-list-of unmemoized:is-list-of))
   (set! is-non-empty-list-of
	 (memoize-unary 'is-list-of unmemoized:is-non-empty-list-of))
   (set! is-pair-of
	 (memoize-binary 'is-pair-of unmemoized:is-pair-of))
   unspecific))