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

;;;; Types
;;; package: (runtime types)

(declare (usual-integrations)
	 (integrate-external "dispatch-low"))

(define %type-metatag
  (%make-dispatch-metatag 'type))

(define %type-tag?
  (%dispatch-tag-predicate %type-metatag))

(define-integrable (%type? object)
  (and (%apply-hook? object)
       (%type-tag? (%apply-hook-extra object))))

(define (make-type name parts test)
  (let ((predicate (%make-apply-hook test #f)))
    (%set-entity-extra!
     predicate
     (%make-tag %type-metatag
		(if (%pair? parts)
		    (cons name
			  ;; Cold load: equiv to (map type-name parts)
			  (let loop ((parts parts))
			    (if (pair? parts)
				(cons (type-name (car parts))
				      (loop (cdr parts)))
				'())))
		    name)
		predicate
		(list parts)))
    predicate))

(define-integrable (simple-type name test)
  (make-type name '() test))

(define type?
  (make-type 'type '() %type?))

(define (type-name type)
  (guarantee type? type 'type-name)
  (%dispatch-tag-name (%apply-hook-extra type)))

(define (type-parts type)
  (guarantee type? type 'type-parts)
  (%dispatch-tag-extra-ref (%apply-hook-extra type) 0))

(define (type-test type)
  (guarantee type? type 'type-test)
  (%apply-hook-procedure type))

(define (type-supersets type)
  (guarantee type? type 'type-supersets)
  (weak-list-set->list (%type-supersets type)))

(define-integrable (%type-supersets type)
  (%tag-supersets (%apply-hook-extra type)))

(define (restrict-type type restriction)
  (let ((subset
	 (make-type 'restriction
		    (list type)
		    (let ((test (type-test type)))
		      (lambda (object)
			(and (test object)
			     (restriction object)))))))
    (set-type<=! subset type)
    subset))

(define (disjoin-types . types)
  (cond ((null? types) no-object?)
	((null? (cdr types))
	 (guarantee type? (car types) 'disjoin-types)
	 (car types))
	(else
	 (let ((disjunction
		(make-type 'disjoin
			   types
			   (if (null? (cddr types))
			       (let ((test1 (type-test (car types)))
				     (test2 (type-test (cadr types))))
				 (lambda (object)
				   (or (test1 object)
				       (test2 object))))
			       (let ((tests (map type-test types)))
				 (lambda (object)
				   (any (lambda (test) (test object))
					tests)))))))
	   (do ((types types (cdr types)))
	       ((not (%pair? types)))
	     (set-type<=! (car types) disjunction))
	   disjunction))))

(define (conjoin-types . types)
  (cond ((null? types) any-object?)
	((null? (cdr types))
	 (guarantee type? (car types) 'conjoin-types)
	 (car types))
	(else
	 (let ((conjunction
		(make-type 'conjoin
			   types
			   (if (null? (cddr types))
			       (let ((test1 (type-test (car types)))
				     (test2 (type-test (cadr types))))
				 (lambda (object)
				   (and (test1 object)
				       (test2 object))))
			       (let ((tests (map type-test types)))
				 (lambda (object)
				   (every (lambda (test) (test object))
					  tests)))))))
	   (do ((types types (cdr types)))
	       ((not (%pair? types)))
	     (set-type<=! conjunction (car types)))
	   conjunction))))

(define (complement-type type)
  (make-type 'complement
	     (list type)
	     (let ((test (type-test type)))
	       (lambda (object)
		 (not (test object))))))

(define (type<= type1 type2)
  (guarantee type? type1 'type<=)
  (guarantee type? type2 'type<=)
  (%type<= type1 type2))

(define (%type<= type1 type2)
  (hash-table-intern! type<=-cache
		      (weak-list type1 type2)
    (lambda ()
      (or (eq? type1 type2)
	  (eq? type1 no-object?)
	  (eq? type2 any-object?)
	  (and (not (eq? type1 any-object?))
	       (not (eq? type2 no-object?))
	       (weak-list-set-any (lambda (type) (%type<= type type2))
				  (%type-supersets type1)))))))

(define (cold-load:set-type<=! subset superset)
  (set! deferred-relations
	(cons (cons subset superset) deferred-relations))
  unspecific)

(define after-cold-load:set-type<=!
  (named-lambda (set-type<=! subset superset)
    (guarantee type? subset 'set-type<=!)
    (guarantee type? superset 'set-type<=!)
    (if (%type<= superset subset)
	(error "Illegal type loop:" subset superset))
    (weak-list-set-add! superset (%type-supersets subset))
    (hash-table-clear! type<=-cache)))

(define set-type<=! cold-load:set-type<=!)

(define deferred-relations '())
(define type<=-cache)
(define (initialize-package!)
  (let ((seq (conjoin-boot-deps '(runtime comparator) '(runtime hash-table))))
    (seq 'add-action!
      (lambda ()
	(set! type<=-cache
	      (make-hash-table (uniform-weak-list-comparator eq-comparator)))
	(set! set-type<=! after-cold-load:set-type<=!)
	(for-each (lambda (e)
		    (set-type<=! (car e) (cdr e)))
		  deferred-relations)
	(set! deferred-relations)
	unspecific))))

;;;; Primitive types

(let-syntax
    ((define-primitive-types
       (er-macro-transformer
        (lambda (form r c)
          (declare (ignore c))
          `(,(r 'begin)
            ,@(map (lambda (name)
                     `(define ,(symbol name '?)
                        (,(r 'simple-type) ',name ,(symbol '% name '?))))
                   (cdr form)))))))
  (define-primitive-types
    any-object
    apply-hook
    bignum
    bit-string
    boolean
    broken-heart
    bytevector
    cell
    char
    compiled-code-block
    compiled-entry-address
    compiled-return-address
    constant
    control-point
    default-object
    delayed
    entity
    eof-object
    ephemeron
    extended-procedure
    fixnum
    flonum
    gc-non-pointer
    gc-pointer
    gc-reclaimed-object
    hunk3-a
    hunk3-b
    ic-environment
    interned-symbol
    interpreter-return-address
    legacy-string
    manifest-nm-vector
    no-object
    null
    pair
    primitive-procedure
    ratnum
    recnum
    %record
    scode-access
    scode-assignment
    scode-combination
    scode-comment
    scode-conditional
    scode-definition
    scode-delay
    scode-disjunction
    scode-extended-lambda
    scode-lexpr
    scode-quotation
    scode-sequence
    scode-simple-lambda
    scode-the-environment
    scode-variable
    simple-procedure
    stack-address
    system-cell
    system-pair
    system-quadruple
    system-triple
    system-vector
    %tagged-object
    unicode-string
    uninterned-symbol
    vector
    weak-pair))

(define dispatch-tag?
  (restrict-type %record?
    (lambda (record)
      (%dispatch-metatag? (%record-ref record 0)))))

(define dispatch-metatag?
  (restrict-type %record?
    (lambda (record)
      (eq? metatag-tag (%record-ref record 0)))))
(set-type<=! dispatch-metatag? dispatch-tag?)