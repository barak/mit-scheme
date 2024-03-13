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

(define (make-type name parts test)
  (let ((predicate (%make-apply-hook test #f)))
    (%set-entity-extra!
     predicate
     (%make-tag %type-metatag
		(if (pair? parts)
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
  (named-lambda (cold-load:type? object)
    (declare (ignore object))
    #t))

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
	       ((not (pair? types)))
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
	       ((not (pair? types)))
	     (set-type<=! conjunction (car types)))
	   conjunction))))

(define (complement-type type)
  (make-type 'complement
	     (list type)
	     (let ((test (type-test type)))
	       (lambda (object)
		 (not (test object))))))

(define (pair-type car-type cdr-type)
  (let ((type
	 (make-type 'pair
		    (list car-type cdr-type)
		    (let ((car-test (type-test car-type))
			  (cdr-test (type-test cdr-type)))
		      (lambda (object)
			(and (pair? object)
			     (car-test (car object))
			     (cdr-test (cdr object))))))))
    (set-type<=! type pair?)
    type))

(define (uniform-list-type elt-type)
  (let ((type
	 (make-type 'uniform-list
		    (list elt-type)
		    (let ((elt-test (type-test elt-type)))
		      (lambda (object)
			(list-of-type? object elt-test))))))
    (set-type<=! type list?)
    type))

(define (uniform-string-type char-type)
  (restrict-type string?
    (lambda (s)
      (string-every char-type s))))

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
    ((define-type
       (er-macro-transformer
        (lambda (form r c)
          (declare (ignore c))
	  (let ((name (cadr form))
		(code (caddr form)))
            `(define ,(symbol name '?)
	       (,(r 'simple-type) ',name
		(,(r 'lambda) (object)
		 (,(r 'object-type?) ,code object)))))))))
  (begin
    (define-type apply-hook apply-hook-type-code)
    (define-type bignum (ucode-type bignum))
    (define-type bit-string (ucode-type vector-1b))
    (define-type broken-heart (ucode-type broken-heart))
    (define-type bytevector (ucode-type bytevector))
    (define-type cell (ucode-type cell))
    (define-type char (ucode-type character))
    (define-type compiled-code-block (ucode-type compiled-code-block))
    (define-type compiled-entry-address (ucode-type compiled-entry))
    (define-type compiled-return-address (ucode-type compiled-return))
    (define-type control-point (ucode-type control-point))
    (define-type delayed (ucode-type delayed))
    (define-type entity (ucode-type entity))
    (define-type ephemeron (ucode-type ephemeron))
    (define-type extended-procedure (ucode-type extended-procedure))
    (define-type fixnum (ucode-type fixnum))
    (define-type flonum (ucode-type flonum))
    (define-type hunk3-a (ucode-type hunk3-a))
    (define-type hunk3-b (ucode-type hunk3-b))
    (define-type ic-environment (ucode-type environment))
    (define-type interned-symbol (ucode-type interned-symbol))
    (define-type interpreter-return-address (ucode-type return-address))
    (define-type legacy-string (ucode-type string))
    (define-type manifest-nm-vector (ucode-type manifest-nm-vector))
    (define-type misc-constant (ucode-type constant))
    (define-type misc-false (ucode-type false))
    (define-type $pair (ucode-type pair))
    (define-type primitive-procedure (ucode-type primitive))
    (define-type ratnum (ucode-type ratnum))
    (define-type recnum (ucode-type recnum))
    (define-type %record (ucode-type record))
    (define-type scode-access (ucode-type access))
    (define-type scode-assignment (ucode-type assignment))
    (define-type scode-combination (ucode-type combination))
    (define-type scode-comment (ucode-type comment))
    (define-type scode-conditional (ucode-type conditional))
    (define-type scode-definition (ucode-type definition))
    (define-type scode-delay (ucode-type delay))
    (define-type scode-disjunction (ucode-type disjunction))
    (define-type scode-extended-lambda (ucode-type extended-lambda))
    (define-type scode-lexpr (ucode-type lexpr))
    (define-type scode-quotation (ucode-type quotation))
    (define-type scode-sequence (ucode-type sequence))
    (define-type scode-simple-lambda (ucode-type lambda))
    (define-type scode-the-environment (ucode-type the-environment))
    (define-type scode-variable (ucode-type variable))
    (define-type simple-procedure (ucode-type procedure))
    (define-type stack-address (ucode-type stack-environment))
    (define-type %tagged-object (ucode-type tagged-object))
    (define-type unicode-string (ucode-type unicode-string))
    (define-type uninterned-symbol (ucode-type uninterned-symbol))
    (define-type vector (ucode-type vector))
    (define-type weak-pair (ucode-type weak-cons))))

(set! type?
      (restrict-type apply-hook?
	(lambda (hook)
	  (%type-tag? (%apply-hook-extra hook)))))

(define dispatch-tag?
  (restrict-type %record?
    (lambda (record)
      (%dispatch-metatag? (%record-ref record 0)))))

(define dispatch-metatag?
  (restrict-type %record?
    (lambda (record)
      (eq? metatag-tag (%record-ref record 0)))))
(set-type<=! dispatch-metatag? dispatch-tag?)

(define any-object?
  (simple-type 'any-object
    (lambda (object)
      (declare (ignore object))
      #t)))

(define no-object?
  (simple-type 'no-object
    (lambda (object)
      (declare (ignore object))
      #f)))

(define default-object?
  (restrict-type misc-constant?
    (lambda (object)
      (eq? #!default object))))

(define eof-object?
  (restrict-type misc-constant?
    (lambda (object)
      (eq? (eof-object) object))))

(define gc-reclaimed-object?
  (restrict-type misc-constant?
    (lambda (object)
      (eq? #!reclaimed object))))

(define symbol?
  (disjoin-types interned-symbol? uninterned-symbol?))

;;;; GC types

(define (code->gct-name code)
  (if (not (and (fixnum? code)
		(fix:>= code -4)
		(fix:<= code 5)))
      (error "Illegal GC type code:" code))
  (vector-ref gct-names (gct-code->index code)))

(define-integrable (name->gct-code name)
  (gct-index->code (name->gct-index name)))

(define (names->gct-mask names)
  (let loop ((names names) (mask 0))
    (if (pair? names)
	(loop (cdr names)
	      (fix:or (fix:lsh 1 (name->gct-index (car names))) mask))
	mask)))

(define (name->gct-index name)
  (let* ((v gct-names)
	 (n (vector-length v)))
    (let loop ((i 0))
      (if (not (fix:< i n))
	  (error "Illegal GC type name:" name))
      (if (eq? name (vector-ref v i))
	  i
	  (loop (fix:+ i 1))))))

(define-integrable (gct-code->index code)
  (fix:+ code 4))

(define-integrable (gct-index->code index)
  (fix:- index 4))

(define gct-names
  ;; Must match gc_type_t in microcode/gc.h.
  '#(compiled-entry vector gc-internal undefined non-pointer
		    cell pair triple quadruple compiled-return))

(define gc-non-pointer?
  (simple-type 'gc-non-pointer
    (lambda (object)
      (fix:= 0 ((ucode-primitive object-gc-type 1) object)))))

(define-integrable (gc-non-pointer-type-code? code)
  (fix:= 0 ((ucode-primitive type->gc-type 1) code)))

(define gc-pointer?
  (simple-type 'gc-pointer
    (lambda (object)
      (match-gct-mask gc-pointer-mask
		      ((ucode-primitive object-gc-type 1) object)))))

(define-integrable (gc-pointer-type-code? code)
  (match-gct-mask gc-pointer-mask ((ucode-primitive type->gc-type 1) code)))

(define gc-pointer-mask
  (names->gct-mask
   '(cell pair triple quadruple vector compiled-entry compiled-return)))

(define-integrable (match-gct-mask mask code)
  (not (fix:= 0 (fix:and mask (fix:lsh 1 (gct-code->index code))))))

(define system-cell?
  (simple-type 'system-cell
    (lambda (object)
      (fix:= 1 ((ucode-primitive object-gc-type 1) object)))))

(define (system-cell-type-code? code)
  (fix:= 1 ((ucode-primitive type->gc-type 1) code)))

(define system-pair?
  (simple-type 'system-pair
    (lambda (object)
      (fix:= 2 ((ucode-primitive object-gc-type 1) object)))))

(define (system-pair-type-code? code)
  (fix:= 2 ((ucode-primitive type->gc-type 1) code)))

(define system-triple?
  (simple-type 'system-triple
    (lambda (object)
      (fix:= 3 ((ucode-primitive object-gc-type 1) object)))))

(define (system-triple-type-code? code)
  (fix:= 3 ((ucode-primitive type->gc-type 1) code)))

(define system-quadruple?
  (simple-type 'system-quadruple
    (lambda (object)
      (fix:= 4 ((ucode-primitive object-gc-type 1) object)))))

(define (system-quadruple-type-code? code)
  (fix:= 4 ((ucode-primitive type->gc-type 1) code)))

(define system-vector?
  (simple-type 'system-vector
    (lambda (object)
      (fix:= -3 ((ucode-primitive object-gc-type 1) object)))))

(define (system-vector-type-code? code)
  (fix:= -3 ((ucode-primitive type->gc-type 1) code)))

(define (non-pointer-type-code? code)
  (or (gc-non-pointer-type-code? code)
      (fix:= (ucode-type manifest-nm-vector) code)))

(define (pointer-type-code? code)
  (or (gc-pointer-type-code? code)
      (fix:= (ucode-type broken-heart) code)))