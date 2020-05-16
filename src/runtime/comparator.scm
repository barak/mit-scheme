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

;;;; SRFI 128: Comparators
;;; package: (runtime comparator)

(declare (usual-integrations))

(add-boot-deps! '(runtime microcode-tables)
		'(runtime compound-predicate)
		'(runtime predicate-dispatch)
		'(runtime hash-table))

(define (make-comparator ? = < hash #!optional rehash-after-gc?)
  (guarantee unary-procedure? ? 'make-comparator)
  (guarantee binary-procedure? = 'make-comparator)
  (if < (guarantee binary-procedure? < 'make-comparator))
  (if hash (guarantee unary-procedure? hash 'make-comparator))
  (%make-comparator ? = < hash (and hash rehash-after-gc? #t)))

(define-record-type <comparator>
    (%make-comparator ? = < hash rehash-after-gc?)
    comparator?
  (? %comparator-?)
  (= %comparator-=)
  (< %comparator-<)
  (hash %comparator-hash)
  (rehash-after-gc? comparator-rehash-after-gc?))

(define (comparator-ordered? comparator)
  (and (%comparator-< comparator) #t))

(define (comparator-hashable? comparator)
  (and (%comparator-hash comparator) #t))

(define (comparator-ordering-predicate comparator)
  (or (%comparator-< comparator)
      (lambda (a b)
	(declare (ignore a b))
	(error "Comparator not ordered:" comparator))))

(define (comparator-hash-function comparator)
  (or (%comparator-hash comparator)
      (lambda (a)
	(declare (ignore a))
	(error "Comparator not hashable:" comparator))))

(define (comparator-test-type comparator object)
  ((%comparator-? comparator) object))

(define (comparator-check-type comparator object)
  (guarantee (%comparator-? comparator) object 'comparator-check-type)
  #t)

(define (comparator-hash comparator object)
  ((comparator-hash-function comparator) object))

(define (=? comparator a b . rest)
  (let ((= (%comparator-= comparator)))
    (let loop ((a a) (b b) (rest rest))
      (if (pair? rest)
	  (and (= a b)
	       (loop b (car rest) (cdr rest)))
	  (= a b)))))

(define (<? comparator a b . rest)
  (let ((< (%comparator-< comparator)))
    (let loop ((a a) (b b) (rest rest))
      (if (pair? rest)
	  (and (< a b)
	       (loop b (car rest) (cdr rest)))
	  (< a b)))))

(define (<=? comparator a b . rest)
  (let ((< (%comparator-< comparator)))
    (let loop ((a a) (b b) (rest rest))
      (if (pair? rest)
	  (and (not (< b a))
	       (loop b (car rest) (cdr rest)))
	  (not (< b a))))))

(define (>? comparator a b . rest)
  (let ((< (%comparator-< comparator)))
    (let loop ((a a) (b b) (rest rest))
      (if (pair? rest)
	  (and (< b a)
	       (loop b (car rest) (cdr rest)))
	  (< b a)))))

(define (>=? comparator a b . rest)
  (let ((< (%comparator-< comparator)))
    (let loop ((a a) (b b) (rest rest))
      (if (pair? rest)
	  (and (not (< a b))
	       (loop b (car rest) (cdr rest)))
	  (not (< a b))))))

(define-syntax comparator-if<=>
  (syntax-rules ()
    ((_ comparator object1 object2 if< if= if>)
     (cond ((=? comparator object1 object2) if=)
	   ((<? comparator object1 object2) if<)
	   (else if>)))
    ((_ object1 object2 if< if= if>)
     (let ((comparator (make-default-comparator)))
       (cond ((=? comparator object1 object2) if=)
	     ((<? comparator object1 object2) if<)
	     (else if>))))))

;;;; Specific comparators

(define (make-eq-comparator)
  the-eq-comparator)

(define-deferred the-eq-comparator
  (%make-comparator any-object? eq? #f eq-hash #t))

(define (make-eqv-comparator)
  the-eqv-comparator)

(define-deferred the-eqv-comparator
  (%make-comparator any-object? eqv? #f eqv-hash #t))

(define (make-equal-comparator)
  the-equal-comparator)

(define-deferred the-equal-comparator
  (%make-comparator any-object? equal? #f equal-hash #t))

(define (boolean-comparator)
  the-boolean-comparator)

(define-deferred the-boolean-comparator
  (%make-comparator boolean? boolean=? boolean<? boolean-hash #f))

(define (char-comparator)
  the-char-comparator)

(define-deferred the-char-comparator
  (%make-comparator char? char=? char<? char-hash #f))

(define (char-ci-comparator)
  the-char-ci-comparator)

(define-deferred the-char-ci-comparator
  (%make-comparator char? char-ci=? char-ci<? char-ci-hash #f))

(define (string-comparator)
  the-string-comparator)

(define-deferred the-string-comparator
  (%make-comparator string? string=? string<? string-hash #f))

(define (string-ci-comparator)
  the-string-ci-comparator)

(define-deferred the-string-ci-comparator
  (%make-comparator string? string-ci=? string-ci<? string-ci-hash #f))

(define (symbol-comparator)
  the-symbol-comparator)

(define-deferred the-symbol-comparator
  (%make-comparator symbol? symbol=? symbol<? symbol-hash #f))

(define (interned-symbol-comparator)
  the-interned-symbol-comparator)

(define-deferred the-interned-symbol-comparator
  (%make-comparator interned-symbol? eq? symbol<? eq-hash #t))

(define (bytevector-comparator)
  the-bytevector-comparator)

(define-deferred the-bytevector-comparator
  (%make-comparator bytevector? bytevector=? bytevector<? bytevector-hash #f))

(define (number-comparator)
  the-number-comparator)

(define-deferred the-number-comparator
  (%make-comparator number? = #f number-hash #f))

(define (real-comparator)
  the-real-comparator)

(define-deferred the-real-comparator
  (%make-comparator real? = < number-hash #f))

(define (fixnum-comparator)
  the-fixnum-comparator)

(define-deferred the-fixnum-comparator
  (%make-comparator fix:fixnum? fix:= fix:< fixnum-hash #f))

(define (flonum-comparator)
  the-flonum-comparator)

(define-deferred the-flonum-comparator
  (%make-comparator flo:flonum? flo:= flo:< number-hash #f))

(define (char-set-comparator)
  the-char-set-comparator)

(define-deferred the-char-set-comparator
  (%make-comparator char-set? char-set= char-set< char-set-hash #f))

(define (exact-integer-comparator)
  the-exact-integer-comparator)

(define-deferred the-exact-integer-comparator
  (%make-comparator exact-integer? int:= int:< number-hash #f))

;;;; General combinators

(define-integrable (kpair-comparator-combinator kpair? kar-valid? kar kdr)

  (define-values (make-? is-?)
    (compound-predicate-constructor (predicate-name kpair?) kpair?
      (lambda (kar? kdr?)
	(lambda (object)
	  (and (kpair? object)
	       (let ((a (kar object)))
		 (and (kar-valid? a)
		      (kar? a)))
	       (kdr? (kdr object)))))
      list
      ordered-predicates-memoizer))

  (define (make-= kar= kdr=)
    (lambda (a b)
      (and (let ((aa (kar a))
		 (ab (kar b)))
	     (and (kar-valid? aa)
		  (kar-valid? ab)
		  (kar= aa ab)))
	   (kdr= (kdr a) (kdr b)))))

  (define (make-< kar= kar< kdr<)
    (lambda (a b)
      (let ((aa (kar a))
	    (ab (kar b)))
	(and (kar-valid? aa)
	     (kar-valid? ab)
	     (or (kar< aa ab)
		 (and (kar= aa ab)
		      (kdr< (kdr a) (kdr b))))))))

  (define (make-hash kar-hash kdr-hash)
    (lambda (a)
      (let ((aa (kar a)))
	(if (kar-valid? aa)
	    (combine-hashes (kar-hash aa)
			    (kdr-hash (kdr a)))
	    (kdr-hash (kdr a))))))

  (lambda (ac dc)
    (let ((a= (%comparator-= ac))
	  (a< (%comparator-< ac))
	  (ah (%comparator-hash ac))
	  (d< (%comparator-< dc))
	  (dh (%comparator-hash dc)))
      (%make-comparator (make-? (%comparator-? ac)
				(%comparator-? dc))
			(make-= a= (%comparator-= dc))
			(and a< d< (make-< a= a< d<))
			(and ah dh (make-hash ah dh))
			(or (comparator-rehash-after-gc? ac)
			    (comparator-rehash-after-gc? dc))))))

(define-deferred make-pair-comparator
  (kpair-comparator-combinator pair? car-valid? car cdr))

(define-integrable (car-valid? a)
  (declare (ignore a))
  #t)

(define-deferred make-weak-pair-comparator
  (kpair-comparator-combinator weak-pair? weak-car-valid? weak-car weak-cdr))

(define-integrable (weak-car-valid? a)
  (not (gc-reclaimed-object? a)))

(define-integrable (%unary-combinator make-? make-= make-< make-hash)
  (lambda (celt)
    (let ((c= (%comparator-= celt))
	  (c< (%comparator-< celt))
	  (ch (%comparator-hash celt)))
      (%make-comparator (make-? (%comparator-? celt))
			(make-= c=)
			(and c< (make-< c= c<))
			(and ch (make-hash ch))
			(comparator-rehash-after-gc? celt)))))

(define (make-list-comparator celt kpair? knull? kar kdr)
  (cond ((and (eqv? pair? kpair?)
	      (eqv? null? knull?)
	      (eqv? car kar)
	      (eqv? cdr kdr))
	 (uniform-list-comparator celt))
	((and (eqv? weak-pair? kpair?)
	      (eqv? null? knull?)
	      (eqv? weak-car kar)
	      (eqv? weak-cdr kdr))
	 (uniform-weak-list-comparator celt))
	(else
	 (let ()
	   (define (null-klist? object)
	     (cond ((knull? object) #t)
		   ((kpair? object) #f)
		   (else (error:wrong-type-argument "klist" object))))
	   (%unary-combinator (make-klist-pred kpair? knull? kar kdr)
			      (make-klist= null-klist? kar kdr)
			      (make-klist< null-klist? kar kdr)
			      (make-klist-hash null-klist? kar kdr))))))

(define ((make-klist-pred kpair? knull? kar kdr) elt?)
  (lambda (a)
    (let loop ((scan1 a) (scan2 a))
      (if (kpair? scan1)
	  (and (elt? (kar scan1))
	       (let ((next (kdr scan1)))
		 (and (not (eq? next scan2))
		      (if (kpair? next)
			  (loop (kdr next) (kdr scan2))
			  (knull? next)))))
	  (knull? scan1)))))

(define ((make-klist= null-klist? kar kdr) elt=)
  (lambda (a b)
    (let loop ((scana a) (scanb b))
      (if (null-klist? scana)
	  (null-klist? scanb)
	  (and (not (null-klist? scanb))
	       (elt= (kar scana) (kar scanb))
	       (loop (kdr scana) (kdr scanb)))))))

(define ((make-klist< null-klist? kar kdr) elt= elt<)
  (lambda (a b)
    (let loop ((scana a) (scanb b))
      (and (not (null-klist? scanb))
	   (or (null-klist? scana)
	       (elt< (kar scana) (kar scanb))
	       (and (elt= (kar scana) (kar scanb))
		    (loop (kdr scana) (kdr scanb))))))))

(define ((make-klist-hash null-klist? kar kdr) elt-hash)
  (lambda (a)
    (let loop ((scan a) (result (initial-hash)))
      (if (null-klist? scan)
	  result
	  (loop (kdr scan)
		(combine-hashes (elt-hash (kar scan)) result))))))

(define (make-vector-comparator celt kvector? kv-length kv-ref)
  (cond ((and (eqv? vector? kvector?)
	      (eqv? vector-length kv-length)
	      (eqv? vector-ref kv-ref))
	 (uniform-vector-comparator celt))
	(else
	 (%unary-combinator (make-kvector-pred kvector? kv-length kv-ref)
			    (make-kvector= kv-length kv-ref)
			    (make-kvector< kv-length kv-ref)
			    (make-kvector-hash kv-length kv-ref)))))

(define ((make-kvector-pred kvector? kv-length kv-ref) elt?)
  (if (eqv? any-object? elt?)
      kvector?
      (lambda (object)
	(and (kvector? object)
	     (let ((end (kv-length object)))
	       (let loop ((i 0))
		 (or (not (< i end))
		     (and (elt? (kv-ref object i))
			  (loop (+ i 1))))))))))

(define ((make-kvector= kv-length kv-ref) elt=)
  (lambda (kv1 kv2)
    (let ((end (kv-length kv1)))
      (and (= end (kv-length kv2))
	   (let loop ((i 0))
	     (or (not (< i end))
		 (and (elt= (kv-ref kv1 i) (kv-ref kv2 i))
		      (loop (+ i 1)))))))))

(define ((make-kvector< kv-length kv-ref) elt= elt<)
  (lambda (kv1 kv2)
    (let ((end1 (kv-length kv1))
	  (end2 (kv-length kv2)))
      (let ((end (min end1 end2)))
	(let loop ((i 0))
	  (if (< i end)
	      (let ((elt1 (kv-ref kv1 i))
		    (elt2 (kv-ref kv2 i)))
		(or (elt< elt1 elt2)
		    (and (elt= elt1 elt2)
			 (loop (+ i 1)))))
	      (< end1 end2)))))))

(define ((make-kvector-hash kv-length kv-ref) elt-hash)
  (lambda (kv)
    (let ((end (kv-length kv)))
      (let loop ((i 0) (result (initial-hash)))
	(if (< i end)
	    (loop (+ i 1)
		  (combine-hashes (elt-hash (kv-ref kv i)) result))
	    result)))))

;;;; Specialized combinators

(define (memoized-constructor constructor)
  (let ((table (make-key-weak-eq-hash-table)))
    (lambda (celt)
      (hash-table-intern! table celt (lambda () (constructor celt))))))

(define (weak-list-constructor constructor)
  (memoized-constructor
   (lambda (celt)
     (let ((comparator (constructor celt)))
       (hash-table-set! weak-list-comparators comparator #t)
       comparator))))

(define (weak-list-comparator? object)
  (hash-table-exists? weak-list-comparators object))

(define-deferred weak-list-comparators
  (make-key-weak-eq-hash-table))

(define-deferred uniform-list-comparator
  (memoized-constructor
   (%unary-combinator uniform-list-predicate
		      make-ulist=
		      make-ulist<
		      make-ulist-hash)))

(define-values-deferred (uniform-list-predicate uniform-list-predicate?)
  (compound-predicate-constructor 'uniform-list list?
    (lambda (elt-pred)
      (if (eqv? any-object? elt-pred)
	  list?
	  (lambda (object)
	    (list-of-type? object elt-pred))))
    list
    single-predicate-memoizer))

(define (make-ulist= elt=)
  (cond ((eqv? eq? elt=) eq-ulist=)
	((eqv? eqv? elt=) eqv-ulist=)
	(else (%make-ulist= elt=))))

(define-integrable (%make-ulist= elt=)
  (lambda (l1 l2)
    (let loop ((l1 l1) (l2 l2))
      (if (and (pair? l1) (pair? l2))
	  (and (elt= (car l1) (car l2))
	       (loop (cdr l1) (cdr l2)))
	  (and (null? l1) (null? l2))))))

(define eq-ulist= (%make-ulist= eq?))
(define eqv-ulist= (%make-ulist= eqv?))

(define (make-ulist< elt= elt<)
  (lambda (a b)
    (let loop ((scana a) (scanb b))
      (and (not (null-list? scanb))
	   (or (null-list? scana)
	       (elt< (car scana) (car scanb))
	       (and (elt= (car scana) (car scanb))
		    (loop (cdr scana) (cdr scanb))))))))

(define (make-ulist-hash elt-hash)
  (cond ((eqv? eq? elt-hash) eq-ulist-hash)
	((eqv? eqv? elt-hash) eqv-ulist-hash)
	(else
	 (let ()
	   (define-integrable (checked elt)
	     (check-hash (elt-hash elt)))
	   (%make-ulist-hash checked)))))

(define-integrable (%make-ulist-hash elt-hash)
  (lambda (object)
    (fold (lambda (elt result)
	    (%combine-hashes (elt-hash elt) result))
	  (initial-hash)
	  object)))

(define eq-ulist-hash (%make-ulist-hash eq-hash))
(define eqv-ulist-hash (%make-ulist-hash eqv-hash))

(define-deferred lset-comparator
  (memoized-constructor
   (%unary-combinator uniform-list-predicate
		      make-lset=
		      make-lset<
		      make-ulist-hash)))

(define (make-lset= elt=)
  (cond ((eqv? eq? elt=) eq-lset=)
	((eqv? eqv? elt=) eqv-lset=)
	(else (%make-lset= elt=))))

(define-integrable (%make-lset= elt=)
  (define-integrable (<=? l1 l2)
    (every (lambda (x)
	     (any (lambda (y) (elt= x y)) l2))
	   l1))
  (lambda (l1 l2)
    (or (eq? l1 l2)
	(and (<=? l1 l2)
	     (<=? l2 l1)))))

(define eq-lset= (%make-lset= eq?))
(define eqv-lset= (%make-lset= eqv?))

(define (make-lset< elt=)
  (cond ((eqv? eq? elt=) eq-lset<)
	((eqv? eqv? elt=) eqv-lset<)
	(else (%make-lset< elt=))))

(define-integrable (%make-lset< elt=)
  (define-integrable (<=? l1 l2)
    (every (lambda (x)
	     (any (lambda (y) (elt= x y)) l2))
	   l1))
  (lambda (l1 l2)
    (and (<=? l1 l2)
	 (not (<=? l2 l1)))))

(define eq-lset< (%make-lset< eq?))
(define eqv-lset< (%make-lset< eqv?))

(define-deferred uniform-weak-list-comparator
  (weak-list-constructor
   (%unary-combinator uniform-weak-list-predicate
		      make-uwlist=
		      make-uwlist<
		      make-uwlist-hash)))

(define-values-deferred (uniform-weak-list-predicate
			 uniform-weak-list-predicate?)
  (compound-predicate-constructor 'uniform-weak-list weak-list?
    (lambda (elt-pred)
      (if (eqv? any-object? elt-pred)
	  weak-list?
	  (lambda (object)
	    (weak-list-of-type? object elt-pred))))
    list
    single-predicate-memoizer))

(define (make-uwlist= elt=)
  (cond ((eqv? eq? elt=) eq-uwlist=)
	((eqv? eqv? elt=) eqv-uwlist=)
	(else (%make-uwlist= elt=))))

(define-integrable (%make-uwlist= elt=)
  (lambda (l1 l2)
    (let loop ((l1 l1) (l2 l2))
      (if (and (weak-pair? l1) (weak-pair? l2))
	  (let ((key1 (weak-car l1)))
	    (and (not (gc-reclaimed-object? key1))
		 (elt= key1 (weak-car l2))
		 (loop (weak-cdr l1) (weak-cdr l2))))
	  (and (null? l1) (null? l2))))))

(define eq-uwlist= (%make-uwlist= eq?))
(define eqv-uwlist= (%make-uwlist= eqv?))

(define (make-uwlist< elt= elt<)
  (lambda (a b)
    (let loop ((scana a) (scanb b))
      (and (not (null-weak-list? scanb))
	   (or (null-weak-list? scana)
	       (let ((key1 (weak-car scana)))
		 (and (not (gc-reclaimed-object? key1))
		      (or (elt< key1 (weak-car scanb))
			  (and (elt= key1 (weak-car scanb))
			       (loop (weak-cdr scana) (weak-cdr scanb)))))))))))

(define (make-uwlist-hash elt-hash)
  (cond ((eqv? eq? elt-hash) eq-uwlist-hash)
	((eqv? eqv? elt-hash) eqv-uwlist-hash)
	(else
	 (let ()
	   (define-integrable (checked elt)
	     (check-hash (elt-hash elt)))
	   (%make-uwlist-hash checked)))))

(define-integrable (%make-uwlist-hash elt-hash)
  (lambda (object)
    (weak-fold (lambda (elt result)
		 (%combine-hashes (elt-hash elt) result))
	       (initial-hash)
	       object)))

(define eq-uwlist-hash (%make-uwlist-hash eq-hash))
(define eqv-uwlist-hash (%make-uwlist-hash eqv-hash))

(define-deferred weak-lset-comparator
  (weak-list-constructor
   (%unary-combinator uniform-weak-list-predicate
		      make-wlset=
		      make-wlset<
		      make-uwlist-hash)))

(define (make-wlset= elt=)
  (cond ((eqv? eq? elt=) eq-wlset=)
	((eqv? eqv? elt=) eqv-wlset=)
	(else (%make-wlset= elt=))))

(define-integrable (%make-wlset= elt=)
  (define-integrable (<=? l1 l2)
    (weak-every (lambda (x)
		  (weak-any (lambda (y) (elt= x y)) l2))
		l1))
  (lambda (l1 l2)
    (or (eq? l1 l2)
	(and (<=? l1 l2)
	     (<=? l2 l1)))))

(define eq-wlset= (%make-wlset= eq?))
(define eqv-wlset= (%make-wlset= eqv?))

(define (make-wlset< elt=)
  (cond ((eqv? eq? elt=) eq-wlset<)
	((eqv? eqv? elt=) eqv-wlset<)
	(else (%make-wlset< elt=))))

(define-integrable (%make-wlset< elt=)
  (define-integrable (<=? l1 l2)
    (weak-every (lambda (x)
		  (weak-any (lambda (y) (elt= x y)) l2))
		l1))
  (lambda (l1 l2)
    (and (<=? l1 l2)
	 (not (<=? l2 l1)))))

(define eq-wlset< (%make-wlset< eq?))
(define eqv-wlset< (%make-wlset< eqv?))

(define-deferred uniform-vector-comparator
  (memoized-constructor
   (%unary-combinator uniform-vector-predicate
		      make-uvector=
		      make-uvector<
		      make-uvector-hash)))

(define-values-deferred (uniform-vector-predicate uniform-vector-predicate?)
  (compound-predicate-constructor 'uniform-vector vector?
    (lambda (elt-pred)
      (if (eqv? any-object? elt-pred)
	  vector?
	  (lambda (object)
	    (and (vector? object)
		 (vector-every elt-pred object)))))
    list
    single-predicate-memoizer))

(define (make-uvector= elt=)
  (lambda (v1 v2)
    (let ((end (vector-length v1)))
      (and (fix:= end (vector-length v2))
	   (let loop ((i 0))
	     (or (not (fix:< i end))
		 (and (elt= (vector-ref v1 i) (vector-ref v2 i))
		      (loop (fix:+ i 1)))))))))

(define (make-uvector< elt= elt<)
  (lambda (v1 v2)
    (let ((end1 (vector-length v1))
	  (end2 (vector-length v2)))
      (let ((end (fix:min end1 end2)))
	(let loop ((i 0))
	  (if (fix:< i end)
	      (let ((elt1 (vector-ref v1 i))
		    (elt2 (vector-ref v2 i)))
		(or (elt< elt1 elt2)
		    (and (elt= elt1 elt2)
			 (loop (fix:+ i 1)))))
	      (fix:< end1 end2)))))))

(define (make-uvector-hash elt-hash)
  (lambda (v)
    (let ((end (vector-length v)))
      (let loop ((i 0) (result (initial-hash)))
	(if (< i end)
	    (loop (+ i 1)
		  (combine-hashes (elt-hash (vector-ref v i)) result))
	    result)))))

;;;; Hash functions

;;; On 64-bit architectures:
;;;   hash functions return a 32-bit unsigned value
;;; On 32-bit architectures:
;;;   hash functions return a 25-bit unsigned value

(define-syntax hash-bound
  (syntax-rules ()
    ((_) (select-on-bytes-per-word #x02000000 #x100000000))))

(define-syntax hash-mask
  (syntax-rules ()
    ((_) (select-on-bytes-per-word #x01FFFFFF #xFFFFFFFF))))

(define-syntax hash-bits
  (syntax-rules ()
    ((_) (select-on-bytes-per-word 25 32))))

(define (hash-salt)
  (random-integer (hash-bound)))

(define ((protected-hash-function hash-fn) object)
  (check-hash (hash-fn object)))

(define-integrable (check-hash h)
  (guarantee index-fixnum? h)
  (if (not (fix:<= h (hash-mask)))
      (error:bad-range-argument h))
  h)

(define (combine-hashes h1 h2)
  (%combine-hashes (check-hash h1) (check-hash h2)))

(define-integrable (initial-hash)
  (select-on-bytes-per-word 25165813 3221225473))

(add-boot-init!
 (lambda ()
   (if (fixed-objects-item? 'initial-hash)
       (set-fixed-objects-item! 'initial-hash (initial-hash)))))

(select-on-bytes-per-word
 (define-integrable (%combine-hashes h1 h2)
   (let ((sum (+ (* 31 h1) h2)))
     (if (fix:fixnum? sum)
	 (fix:and sum (hash-mask))
	 (bitwise-and sum (hash-mask)))))
 (define-integrable (%combine-hashes h1 h2)
   (fix:and (fix:+ (fix:* 31 h1) h2) (hash-mask))))

(define (boolean-hash b)
  ;; Arbitrarily chosen primes:
  (%combine-hashes (if b 4111 5333) (initial-hash)))

(define (char-hash char)
  ;; CHAR->INTEGER always returns a non-negative fixnum.
  (%combine-hashes (char->integer char) (initial-hash)))

(define (char-ci-hash char)
  (char-hash (char-foldcase char)))

(define (fixnum-hash n)
  (%combine-hashes (fix:and (fix:xor (fix:lsh n (fix:- 0 (hash-bits))) n)
			    (hash-mask))
		   (initial-hash)))

;;;; Default comparator

(define (make-default-comparator)
  the-default-comparator)

(define (comparator-register-default! comparator)
  (let ((? (%comparator-? comparator)))
    (if (not (predicate? ?))
	(register-predicate! ? 'unknown))
    (define-default-type ?
      (%comparator-= comparator)
      (%comparator-< comparator)
      (%comparator-hash comparator))))

(define (define-default-type ? = < hash)
  (let ((index (hash-table-intern! index-table ? index-generator)))
    (define-predicate-dispatch-handler default-index (list ?)
      (lambda (object)
	(declare (ignore object))
	index)))
  (define-predicate-dispatch-handler %default= (list ? ?) =)
  (if < (define-predicate-dispatch-handler default< (list ? ?) <))
  (if hash (define-predicate-dispatch-handler default-hash (list ?) hash)))

(define-deferred index-table
  (make-strong-eqv-hash-table))

(define-deferred index-generator
  (make-range-generator 0))

(define-deferred default-index
  (standard-predicate-dispatcher 'default-index 1
    (lambda (object)
      (error "Don't know how to compare this object:" object))))

(define (default= x y)
  (or (eq? x y)
      (%default= x y)))

(define-deferred %default=
  (standard-predicate-dispatcher 'default= 2 eqv?))

(define-deferred default<
  (standard-predicate-dispatcher 'default< 2
    (lambda (x y)
      (< (default-index x) (default-index y)))))

(define-deferred default-hash
  (standard-predicate-dispatcher 'default-hash 1
    (lambda (object)
      (error "Don't know how to hash this object:" object))))

(define-deferred the-default-comparator
  (%make-comparator any-object?
		    default=
		    default<
		    default-hash
		    #t))

(add-boot-init!
 (lambda ()

   ;; SRFI 128 requires that null? come before pair?.
   (define-default-type null?
     (lambda (x y) (declare (ignore x y)) #t)
     (lambda (x y) (declare (ignore x y)) #f)
     (lambda (x) (declare (ignore x)) (%combine-hashes 2777 (initial-hash))))

   (comparator-register-default! (boolean-comparator))
   (comparator-register-default! (bytevector-comparator))
   (comparator-register-default! (char-comparator))
   (comparator-register-default! (char-set-comparator))
   (comparator-register-default! (fixnum-comparator))
   (comparator-register-default! (string-comparator))
   (comparator-register-default! (real-comparator))
   (comparator-register-default! (symbol-comparator))

   (define-default-type bit-string? bit-string=? #f eq-hash)
   (define-default-type cell? eq? #f eq-hash)
   (define-default-type pathname? pathname=? #f pathname-hash)

   (define-default-type complex?
     =
     (lambda (x y)
       (or (< (real-part x) (real-part y))
	   (< (imag-part x) (imag-part y))))
     eqv-hash)

   (let ((dc (make-default-comparator)))
     (comparator-register-default! (make-pair-comparator dc dc))
     (comparator-register-default! (make-weak-pair-comparator dc dc))
     (comparator-register-default! (uniform-vector-comparator dc)))))