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

(define (make-type name derivation test)
  (let ((predicate (%make-apply-hook test #f)))
    (%set-entity-extra! predicate
			(%make-tag %type-metatag
				   name
				   predicate
				   derivation))
    predicate))

(define-integrable (%type-dispatch-tag type)
  (%apply-hook-extra type))

(define-integrable (%type-test type)
  (%apply-hook-procedure type))

(define type?
  (named-lambda (cold-load:type? object)
    (declare (ignore object))
    #t))

(define (type-test type)
  (guarantee type? type 'type-test)
  (%type-test type))

(define (type-dispatch-tag type)
  (guarantee type? type 'type-dispatch-tag)
  (%type-dispatch-tag type))

(define-integrable (type-name type)
  (guarantee type? type 'type-name)
  (%dispatch-tag-name (%type-dispatch-tag type)))

(define (type-supersets type)
  (guarantee type? type 'type-supersets)
  (weak-list-set->list (%type-supersets type)))

(define-integrable (%type-supersets type)
  (%dispatch-tag-supersets (%type-dispatch-tag type)))

(define (type-subsets type)
  (guarantee type? type 'type-subsets)
  (weak-list-set->list (%type-subsets type)))

(define-integrable (%type-subsets type)
  (%dispatch-tag-subsets (%type-dispatch-tag type)))

(define (type-derivation type)
  (guarantee type? type 'type-derivation)
  (%type-derivation type))

(define-integrable (%type-derivation type)
  (%dispatch-tag-extra-ref (%type-dispatch-tag type) 0))

(define (%type-printer-name type)
  (or (%dispatch-tag-name (%type-dispatch-tag type))
      (%type-derivation type)))

(define (object->type object)
  (let ((code (object-type object)))
    (let ((type (vector-ref primitive-types code))
	  (method (vector-ref primitive-type-methods code)))
      (if method
	  (method object type)
	  type))))

;;;; Type constructor and combinators

(define (simple-type name test)
  (let ((type (make-type name #f test)))
    (set! simple-types (weak-cons type simple-types))
    type))

(define simple-types '())

(define (all-simple-types)
  (weak-list->list simple-types))

(define (all-types)
  (let ((ht (make-hash-table eq-comparator)))

    (define (do-type type)
      (if (not (hash-table-contains? ht type))
	  (begin
	    (hash-table-set! ht type #t)
	    (weak-list-set-for-each do-type (%type-subsets type))
	    (weak-list-set-for-each do-type (%type-supersets type)))))

    (weak-for-each do-type simple-types)
    (hash-table-keys ht)))

(define (refine-type name type refinement)
  (let ((subset
	 (make-type name (list 'refine type)
	   (let ((test (type-test type)))
	     (lambda (object)
	       (and (test object)
		    (refinement object)))))))
    (set-type<=! subset type)
    subset))

(define (disjoin-types name . types)
  (cond ((null? types) no-object?)
	((null? (cdr types))
	 (guarantee type? (car types) 'disjoin-types)
	 (car types))
	((null? (cddr types))
	 (let ((type1 (car types))
	       (type2 (cadr types)))
	   (let ((disjunction
		  (make-type name (list 'disjoin type1 type2)
		    (let ((test1 (type-test type1))
			  (test2 (type-test type2)))
		      (lambda (object)
			(or (test1 object)
			    (test2 object)))))))
	     (set-type<=! type1 disjunction)
	     (set-type<=! type2 disjunction)
	     disjunction)))
	(else
	 (let ((disjunction
		(make-type name (cons 'disjoin types)
		  (let ((tests (map type-test types)))
		    (lambda (object)
		      (any (lambda (test) (test object))
			   tests))))))
	   (for-each (lambda (type)
		       (set-type<=! type disjunction))
		     types)
	   disjunction))))

(define (conjoin-types name . types)
  (cond ((null? types) any-object?)
	((null? (cdr types))
	 (guarantee type? (car types) 'conjoin-types)
	 (car types))
	((null? (cddr types))
	 (let ((type1 (car types))
	       (type2 (cadr types)))
	   (let ((conjunction
		  (make-type name (list 'conjoin type1 type2)
		    (let ((test1 (type-test type1))
			  (test2 (type-test type2)))
		      (lambda (object)
			(and (test1 object)
			     (test2 object)))))))
	     (set-type<=! conjunction type1)
	     (set-type<=! conjunction type2)
	     conjunction)))
	(else
	 (let ((conjunction
		(make-type name (cons 'conjoin types)
		  (let ((tests (map type-test types)))
		    (lambda (object)
		      (every (lambda (test) (test object))
			     tests))))))
	   (for-each (lambda (type)
		       (set-type<=! conjunction type))
		     types)
	   conjunction))))

(define (complement-type name type)
  (make-type name (list 'complement type)
    (let ((test (type-test type)))
      (lambda (object)
	(not (test object))))))

(define (differ-types name type1 type2)
  (let ((type
	 (make-type name (list 'differ type1 type2)
	   (let ((test1 (type-test type1))
		 (test2 (type-test type2)))
	     (lambda (object)
	       (and (test1 object)
		    (not (test2 object))))))))
    (set-type<=! type type1)
    type))

(define (pair-type name car-type cdr-type)
  (refine-type name pair?
    (let ((car-test (type-test car-type))
	  (cdr-test (type-test cdr-type)))
      (lambda (object)
	(and (car-test (car object))
	     (cdr-test (cdr object)))))))

(define (uniform-list-type name elt-type)
  (let ((type
	 (make-type name (list 'uniform-list elt-type)
	   (let ((elt-test (type-test elt-type)))
	     (lambda (object)
	       (list-of-type? object elt-test))))))
    (set-type<=! type list?)
    type))

(define (uniform-string-type name char-type)
  (refine-type name string?
    (let ((char-test (type-test char-type)))
      (lambda (s)
	(string-every char-test s)))))

;;;; Primitive types

(let-syntax
    ((define-type
       (er-macro-transformer
        (lambda (form r c)
          (declare (ignore c))
	  (let ((name (cadr form))
		(code (caddr form)))
            `(define ,(symbol name '?)
	       (,(r 'simple-type)
		',(if (pair? (cdddr form))
		      (cadddr form)
		      name)
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
    (define-type $pair (ucode-type pair) pair)
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

(define primitive-type-bindings
  (list (cons apply-hook? apply-hook-type-code)
	(cons bignum? (ucode-type bignum))
	(cons bit-string? (ucode-type vector-1b))
	(cons broken-heart? (ucode-type broken-heart))
	(cons bytevector? (ucode-type bytevector))
	(cons cell? (ucode-type cell))
	(cons char? (ucode-type character))
	(cons compiled-code-block? (ucode-type compiled-code-block))
	(cons compiled-entry-address? (ucode-type compiled-entry))
	(cons compiled-return-address? (ucode-type compiled-return))
	(cons misc-constant? (ucode-type constant))
	(cons control-point? (ucode-type control-point))
	(cons delayed? (ucode-type delayed))
	(cons entity? (ucode-type entity))
	(cons ephemeron? (ucode-type ephemeron))
	(cons extended-procedure? (ucode-type extended-procedure))
	(cons misc-false? (ucode-type false))
	(cons fixnum? (ucode-type fixnum))
	(cons flonum? (ucode-type flonum))
	(cons hunk3-a? (ucode-type hunk3-a))
	(cons hunk3-b? (ucode-type hunk3-b))
	(cons ic-environment? (ucode-type environment))
	(cons interned-symbol? (ucode-type interned-symbol))
	(cons interpreter-return-address? (ucode-type return-address))
	(cons legacy-string? (ucode-type string))
	(cons manifest-nm-vector? (ucode-type manifest-nm-vector))
	(cons pair? (ucode-type pair))
	(cons primitive-procedure? (ucode-type primitive))
	(cons ratnum? (ucode-type ratnum))
	(cons recnum? (ucode-type recnum))
	(cons %record? (ucode-type record))
	(cons scode-access? (ucode-type access))
	(cons scode-assignment? (ucode-type assignment))
	(cons scode-combination? (ucode-type combination))
	(cons scode-comment? (ucode-type comment))
	(cons scode-conditional? (ucode-type conditional))
	(cons scode-definition? (ucode-type definition))
	(cons scode-delay? (ucode-type delay))
	(cons scode-disjunction? (ucode-type disjunction))
	(cons scode-extended-lambda? (ucode-type extended-lambda))
	(cons scode-lexpr? (ucode-type lexpr))
	(cons scode-quotation? (ucode-type quotation))
	(cons scode-sequence? (ucode-type sequence))
	(cons scode-simple-lambda? (ucode-type lambda))
	(cons scode-the-environment? (ucode-type the-environment))
	(cons scode-variable? (ucode-type variable))
	(cons simple-procedure? (ucode-type procedure))
	(cons stack-address? (ucode-type stack-environment))
	(cons %tagged-object? (ucode-type tagged-object))
	(cons unicode-string? (ucode-type unicode-string))
	(cons uninterned-symbol? (ucode-type uninterned-symbol))
	(cons vector? (ucode-type vector))
	(cons weak-pair? (ucode-type weak-cons))))

(define (install-type-methods!)
  (define (define-method code method)
    (vector-set! primitive-type-methods code method))

  (define (simple-alternative alternative)
    (lambda (object fallback)
      (if (alternative object)
	  alternative
	  fallback)))

  (define-method (ucode-type access)
    (simple-alternative scode-absolute-reference?))

  (define-method (ucode-type combination)
    (simple-alternative scode-unassigned??))

  (define-method (ucode-type comment)
    (simple-alternative scode-declaration?))

  (define-method (ucode-type compiled-entry)
    (lambda (entry fallback)
      (declare (ignore fallback))
      (case (system-triple-first
	     ((ucode-primitive compiled-entry-kind 1) entry))
	((0) compiled-procedure?)
	((1) compiled-return-address?)
	((2) compiled-expression?)
	(else compiled-code-address?))))

  (define-method (ucode-type false)
    (simple-alternative false?))

  (define-method (ucode-type constant)
    (let* ((constant-types
	    (vector true?
		    undefined-value?
		    undefined-value?
		    lambda-tag?
		    lambda-tag?
		    lambda-tag?
		    eof-object?
		    default-object?
		    lambda-tag?
		    null?
		    gc-reclaimed-object?))
	   (n-types (vector-length constant-types)))
      (lambda (object fallback)
	(let ((datum (object-datum object)))
	  (cond ((and (fixnum? datum) (fx<? datum n-types))
		 (vector-ref constant-types datum))
		((record-type-proxy-datum? datum) record-type-proxy?)
		(else fallback))))))

  (define-method (ucode-type entity)
    (simple-alternative arity-dispatched-procedure?))

  (define-method apply-hook-type-code
    (simple-alternative type?))

  (define-method (ucode-type record)
    (simple-alternative record?))

  (define-method (ucode-type sequence)
    (simple-alternative scode-open-block?)))

(define type<=
  (named-lambda (cold-load:type<= type1 type2)
    (let loop ((type1 type1))
      (%type<= loop type1 type2))))

(define after-cold-load:type<=
  (named-lambda (type<= type1 type2)
    (guarantee type? type1 'type<=)
    (guarantee type? type2 'type<=)
    (let loop ((type1 type1))
      (hash-table-intern! type<=-cache (weak-list type1 type2)
	(lambda ()
	  (%type<= loop type1 type2))))))

(define-integrable (%type<= loop type1 type2)
  (or (eq? type1 type2)
      (eq? type1 no-object?)
      (eq? type2 any-object?)
      (and (not (eq? type1 any-object?))
	   (not (eq? type2 no-object?))
	   (weak-list-set-any loop (%type-supersets type1)))))

(define set-type<=!
  (named-lambda (cold-load:set-type<=! subset superset)
    (set! deferred-relations
	  (cons (cons subset superset) deferred-relations))
    unspecific))

(define after-cold-load:set-type<=!
  (named-lambda (set-type<=! subset superset)
    (if (type<= superset subset)
	(error "Illegal type loop:" subset superset))
    (weak-list-set-add! superset (%type-supersets subset))
    (weak-list-set-add! subset (%type-subsets superset))
    (hash-table-clear! type<=-cache)))

(define deferred-relations '())
(define type<=-cache)
(define primitive-types)
(define primitive-type-methods)
(define (initialize-package!)
  (let ((seq (conjoin-boot-deps '(runtime comparator) '(runtime hash-table))))
    (seq 'add-action!
      (lambda ()
	(set! type<=-cache
	      (make-hash-table (uniform-weak-list-comparator eq-comparator)))
	(set! type<= after-cold-load:type<=)
	(set! set-type<=! after-cold-load:set-type<=!)
	(for-each (lambda (e)
		    (set-type<=! (car e) (cdr e)))
		  deferred-relations)
	(set! deferred-relations)
	unspecific)))
  (let ((seq (conjoin-boot-deps '(runtime microcode-tables))))
    (seq 'add-action!
      (lambda ()
	(let ((n-codes (microcode-type/code-limit)))
	  (set! primitive-types (make-vector n-codes no-object?))
	  (set! primitive-type-methods (make-vector n-codes #f)))
	(for-each (lambda (e)
		    (vector-set! primitive-types (cdr e) (car e)))
		  primitive-type-bindings)
	(install-type-methods!)))))

(set! type?
      (refine-type 'type apply-hook?
	(lambda (hook)
	  (%type-tag? (%apply-hook-extra hook)))))

(define-print-method type?
  (standard-print-method 'type
    (lambda (type)
      (list (or (type-name type) (type-derivation type))))))

(define-pp-describer type?
  (lambda (type)
    (list (list 'name (type-name type))
	  (list 'derivation (type-derivation type))
	  (list 'dispatch-tag (type-dispatch-tag type))
	  (list 'supersets (type-supersets type)))))

(define dispatch-tag?
  (refine-type 'dispatch-tag %record?
    (lambda (record)
      (%dispatch-metatag? (%record-ref record 0)))))

(define dispatch-metatag?
  (refine-type 'dispatch-metatag %record?
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
  (refine-type 'default-object misc-constant?
    (lambda (object)
      (eq? #!default object))))

(define eof-object?
  (refine-type 'eof-object misc-constant?
    (lambda (object)
      (eq? (eof-object) object))))

(define gc-reclaimed-object?
  (refine-type 'gc-reclaimed-object misc-constant?
    (lambda (object)
      (eq? #!reclaimed object))))

(define symbol?
  (disjoin-types 'symbol interned-symbol? uninterned-symbol?))

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