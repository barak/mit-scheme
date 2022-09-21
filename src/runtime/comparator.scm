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
  (%make-comparator ? = < hash
		    (and hash
			 (if (default-object? rehash-after-gc?)
			     (hash-changes-after-gc? hash)
			     (and rehash-after-gc? #t)))
		    #f
		    #f))

(define-record-type <comparator>
    (%make-comparator ? = < hash rehash-after-gc? key elts)
    comparator?
  (? %comparator-?)
  (= %comparator-=)
  (< %comparator-<)
  (hash %comparator-hash)
  (rehash-after-gc? comparator-rehash-after-gc?)
  (key %comparator-key)
  (elts %comparator-elts))

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

(define (simple-comparator? object)
  (and (comparator? object)
       (not (%comparator-key object))))
(register-predicate! simple-comparator? 'simple-comparator '<= comparator?)

(define (hash-changes-after-gc? hash-fn)
  (hash-table-ref/default registered-hashes hash-fn #f))

(define (registered-hash? object)
  (hash-table-contains? registered-hashes object))

(define (register-hash! hash-fn changes-after-gc?)
  (guarantee unary-procedure? hash-fn 'register-hash!)
  (%register-hash! hash-fn changes-after-gc?))

(define (register-compound-hash! hash-fn . elts)
  (let ((v
	 (fold (lambda (elt acc)
		 (combine-cags (get-cag elt) acc))
	       #f
	       elts)))
    (if (not (eq? 'unknown v))
	(%register-hash! hash-fn v))))

(define (%register-hash! hash-fn changes-after-gc?)
  (hash-table-set! registered-hashes hash-fn (and changes-after-gc? #t)))

(define (get-cag hash-fn)
  (hash-table-ref/default registered-hashes hash-fn 'unknown))

(define (combine-cags a b)
  (cond ((or (eq? #t a) (eq? #t b)) #t)
	((or (eq? 'unknown a) (eq? 'unknown b)) 'unknown)
	(else #f)))

(define-deferred registered-hashes
  (make-key-weak-eqv-hash-table))

(add-boot-init!
 (lambda ()
   (register-hash! boolean-hash #f)
   (register-hash! bytevector-hash #f)
   (register-hash! char-ci-hash #f)
   (register-hash! char-hash #f)
   (register-hash! char-set-hash #f)
   (register-hash! eq-hash #t)
   (register-hash! equal-hash #t)
   (register-hash! eqv-hash #t)
   (register-hash! fixnum-hash #f)
   (register-hash! number-hash #f)
   (register-hash! string-ci-hash #f)
   (register-hash! string-hash #f)
   (register-hash! symbol-hash #f)))

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

(define (comparator-max-in-list comp list)
  (let ((< (comparator-ordering-predicate comp)))
    (let loop ((max (car list)) (list (cdr list)))
      (if (null? list)
          max
          (if (< max (car list))
              (loop (car list) (cdr list))
              (loop max (cdr list)))))))

(define (comparator-min-in-list comp list)
  (let ((< (comparator-ordering-predicate comp)))
    (let loop ((min (car list)) (list (cdr list)))
      (if (null? list)
          min
          (if (< min (car list))
              (loop min (cdr list))
              (loop (car list) (cdr list)))))))

(define (comparator-max comp . args)
  (comparator-max-in-list comp args))

(define (comparator-min comp . args)
  (comparator-min-in-list comp args))

;;;; Specific comparators

(define (make-eq-comparator)
  eq-comparator)

(define-deferred eq-comparator
  (make-comparator any-object? eq? #f eq-hash #t))

(define (make-eqv-comparator)
  eqv-comparator)

(define-deferred eqv-comparator
  (make-comparator any-object? eqv? #f eqv-hash #t))

(define (make-equal-comparator)
  equal-comparator)

(define-deferred equal-comparator
  (make-comparator any-object? equal? #f equal-hash #t))

(define-deferred boolean-comparator
  (make-comparator boolean? boolean=? boolean<? boolean-hash #f))

(define-deferred char-comparator
  (make-comparator char? char=? char<? char-hash #f))

(define-deferred char-ci-comparator
  (make-comparator char? char-ci=? char-ci<? char-ci-hash #f))

(define-deferred string-comparator
  (make-comparator string? string=? string<? string-hash #f))

(define-deferred string-ci-comparator
  (make-comparator string? string-ci=? string-ci<? string-ci-hash #f))

(define-deferred symbol-comparator
  (make-comparator symbol? symbol=? symbol<? symbol-hash #f))

(define-deferred interned-symbol-comparator
  (make-comparator interned-symbol? eq? symbol<? eq-hash #t))

(define-deferred bytevector-comparator
  (make-comparator bytevector? bytevector=? bytevector<? bytevector-hash #f))

(define-deferred number-comparator
  (make-comparator number? = #f number-hash #f))

(define-deferred real-comparator
  (make-comparator real? = < number-hash #f))

(define-deferred fixnum-comparator
  (make-comparator fix:fixnum? fix:= fix:< fixnum-hash #f))

(define-deferred flonum-comparator
  (make-comparator flo:flonum? flo:= flo:< number-hash #f))

(define-deferred char-set-comparator
  (make-comparator char-set? char-set= char-set< char-set-hash #f))

(define-deferred exact-integer-comparator
  (make-comparator exact-integer? int:= int:< number-hash #f))

;;;; Compound comparators

(define (compound-comparator-constructor name n-elts wrapper)
  (guarantee positive-fixnum? n-elts)
  (let ((key (make-key name)))
    (let ((predicate
	   (lambda (object)
	     (and (comparator? object)
		  (eq? key (%comparator-key object))))))
      (register-predicate! predicate name '<= compound-comparator?)
      (case n-elts
	((1)
	 (values (wrapper
		  (lambda (? = < hash rehash-after-gc? elt)
		    (%make-comparator ? = < hash rehash-after-gc?
				      key elt)))
		 predicate
		 (lambda (c)
		   (guarantee predicate c)
		   (%comparator-elts c))))
	((2)
	 (values (wrapper
		  (lambda (? = < hash rehash-after-gc? elt1 elt2)
		    (%make-comparator ? = < hash rehash-after-gc?
				      key (cons elt1 elt2))))
		 predicate
		 (lambda (c)
		   (guarantee predicate c)
		   (car (%comparator-elts c)))
		 (lambda (c)
		   (guarantee predicate c)
		   (cdr (%comparator-elts c)))))
	(else
	 (apply values
		(wrapper
		 (lambda (? = < hash rehash-after-gc? . elts)
		   (%make-comparator ? = < hash rehash-after-gc? key elts)))
		predicate
		(map (lambda (i)
		       (lambda (c)
			 (guarantee predicate c)
			 (list-ref (%comparator-elts c) i)))
		     (iota n-elts))))))))

(define (compound-comparator? object)
  (and (comparator? object)
       (key? (%comparator-key object))))
(register-predicate! compound-comparator? 'compound-comparator '<= comparator?)

(define-record-type <key>
    (make-key name)
    key?
  (name key-name))

(define (unary-combinator constructor make-? make-= make-< make-hash)
  (lambda (c1)
    (constructor (make-? (%comparator-? c1))
		 (make-= (%comparator-= c1))
		 (and (%comparator-< c1)
		      (make-< (%comparator-= c1)
			      (%comparator-< c1)))
		 (and (%comparator-hash c1)
		      (let ((hash (make-hash (%comparator-hash c1))))
			(register-compound-hash! hash
						 (%comparator-hash c1))
			hash))
		 (and (%comparator-hash c1)
		      (comparator-rehash-after-gc? c1))
		 c1)))

(define (binary-combinator constructor make-? make-= make-< make-hash)
  (lambda (c1 c2)
    (constructor (make-? (%comparator-? c1)
			 (%comparator-? c2))
		 (make-= (%comparator-= c1)
			 (%comparator-= c2))
		 (and (%comparator-< c1)
		      (%comparator-< c2)
		      (make-< (%comparator-= c1)
			      (%comparator-< c1)
			      (%comparator-= c2)
			      (%comparator-< c2)))
		 (and (%comparator-hash c1)
		      (%comparator-hash c2)
		      (let ((hash
			     (make-hash (%comparator-hash c1)
					(%comparator-hash c2))))
			(register-compound-hash! hash
						 (%comparator-hash c1)
						 (%comparator-hash c2))
			hash))
		 (and (%comparator-hash c1)
		      (%comparator-hash c2)
		      (or (comparator-rehash-after-gc? c1)
			  (comparator-rehash-after-gc? c2)))
		 c1 c2)))

;;;; General combinators

(define-integrable (kpair-wrapper kpair? kar-valid? kar kdr)

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

  (define (make-< kar= kar< kdr= kdr<)
    (declare (ignore kdr=))
    (lambda (a b)
      (let ((aa (kar a))
	    (ab (kar b)))
	(and (kar-valid? aa)
	     (kar-valid? ab)
	     (or (kar< aa ab)
		 (and (kar= aa ab)
		      (kdr< (kdr a) (kdr b))))))))

  (define (make-hash kar-hash kdr-hash)
    (let ((kar-hash (checked-hash kar-hash))
	  (kdr-hash (checked-hash kdr-hash)))
      (lambda (a)
	(let ((aa (kar a)))
	  (if (kar-valid? aa)
	      (%combine-hashes (kar-hash aa)
			       (kdr-hash (kdr a)))
	      (kdr-hash (kdr a)))))))

  (lambda (constructor)
    (binary-combinator constructor make-? make-= make-< make-hash)))

(define-values-deferred (make-pair-comparator
			 pair-comparator?
			 pair-comparator-car
			 pair-comparator-cdr)
  (compound-comparator-constructor 'pair-comparator 2
    (kpair-wrapper pair? car-valid? car cdr)))

(define-integrable (car-valid? a)
  (declare (ignore a))
  #t)

(define-values-deferred (make-weak-pair-comparator
			 weak-pair-comparator?
			 weak-pair-comparator-car
			 weak-pair-comparator-cdr)
  (compound-comparator-constructor 'weak-pair-comparator 2
    (kpair-wrapper weak-pair? weak-car-valid? weak-car weak-cdr)))

(define-integrable (weak-car-valid? a)
  (not (gc-reclaimed-object? a)))

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
	 (uniform-klist-comparator celt kpair? knull? kar kdr))))

(define ((uniform-klist-wrapper constructor) celt kpair? knull? kar kdr)

  (define (make-? elt?)
    (lambda (l1)
      (let loop ((scan1 l1) (scan2 l1))
	(if (kpair? scan1)
	    (and (elt? (kar scan1))
		 (let ((next (kdr scan1)))
		   (and (not (eq? next scan2))
			(if (kpair? next)
			    (loop (kdr next) (kdr scan2))
			    (knull? next)))))
	    (knull? scan1)))))

  (define (make-= elt=)
    (define (klist= l1 l2)
      (if (null-klist? l1)
	  (null-klist? l2)
	  (and (not (null-klist? l2))
	       (elt= (kar l1) (kar l2))
	       (klist= (kdr l1) (kdr l2)))))
    klist=)

  (define (make-< elt= elt<)
    (define (klist< l1 l2)
      (and (not (null-klist? l2))
	   (or (null-klist? l1)
	       (elt< (kar l1) (kar l2))
	       (and (elt= (kar l1) (kar l2))
		    (klist< (kdr l1) (kdr l2))))))
    klist<)

  (define (make-hash elt-hash)
    (let ((elt-hash (checked-hash elt-hash)))
      (lambda (kl)
	(let loop ((kl kl) (result (initial-hash)))
	  (if (null-klist? kl)
	      result
	      (loop (kdr kl)
		    (%combine-hashes (elt-hash (kar kl)) result)))))))

  (define (null-klist? object)
    (cond ((knull? object) #t)
	  ((kpair? object) #f)
	  (else (error:wrong-type-argument "klist" object))))

  ((unary-combinator constructor make-? make-= make-< make-hash) celt))

(define-values-deferred (uniform-klist-comparator
			 uniform-klist-comparator?
			 uniform-klist-comparator-elt)
  (compound-comparator-constructor 'uniform-klist-comparator 1
    uniform-klist-wrapper))

(define (make-vector-comparator celt kvector? kv-length kv-ref)
  (cond ((and (eqv? vector? kvector?)
	      (eqv? vector-length kv-length)
	      (eqv? vector-ref kv-ref))
	 (uniform-vector-comparator celt))
	(else
	 (uniform-kvector-comparator celt kvector? kv-length kv-ref))))

(define ((uniform-kvector-wrapper constructor) celt kvector? kv-length kv-ref)

  (define (make-? elt?)
    (if (eqv? any-object? elt?)
	kvector?
	(lambda (object)
	  (and (kvector? object)
	       (let ((end (kv-length object)))
		 (let loop ((i 0))
		   (or (not (< i end))
		       (and (elt? (kv-ref object i))
			    (loop (+ i 1))))))))))

  (define (make-= elt=)
    (lambda (v1 v2)
      (let ((end (kv-length v1)))
	(and (= end (kv-length v2))
	     (let loop ((i 0))
	       (or (not (< i end))
		   (and (elt= (kv-ref v1 i) (kv-ref v2 i))
			(loop (+ i 1)))))))))

  (define (make-< elt= elt<)
    (lambda (v1 v2)
      (let ((end1 (kv-length v1))
	    (end2 (kv-length v2)))
	(let ((end (min end1 end2)))
	  (let loop ((i 0))
	    (if (< i end)
		(let ((elt1 (kv-ref v1 i))
		      (elt2 (kv-ref v2 i)))
		  (or (elt< elt1 elt2)
		      (and (elt= elt1 elt2)
			   (loop (+ i 1)))))
		(< end1 end2)))))))

  (define (make-hash elt-hash)
    (let ((elt-hash (checked-hash elt-hash)))
      (lambda (v)
	(let ((end (kv-length v)))
	  (let loop ((i 0) (result (initial-hash)))
	    (if (< i end)
		(loop (+ i 1)
		      (%combine-hashes (elt-hash (kv-ref v i)) result))
		result))))))

  ((unary-combinator constructor make-? make-= make-< make-hash) celt))

(define-values-deferred (uniform-kvector-comparator
			 uniform-kvector-comparator?
			 uniform-kvector-comparator-elt)
  (compound-comparator-constructor 'uniform-kvector-comparator 1
    uniform-kvector-wrapper))

;;;; Specialized combinators

(define (memoized-constructor constructor)
  (let ((table (make-key-weak-eq-hash-table)))
    (lambda (celt)
      (hash-table-intern! table celt (lambda () (constructor celt))))))

(define-values-deferred (uniform-list-predicate uniform-list-predicate?)
  (compound-predicate-constructor 'uniform-list list?
    (lambda (elt-pred)
      (if (eqv? any-object? elt-pred)
	  list?
	  (lambda (object)
	    (list-of-type? object elt-pred))))
    list
    single-predicate-memoizer))

(define-values-deferred (uniform-list-comparator
			 uniform-list-comparator?
			 uniform-list-comparator-elt)
  (compound-comparator-constructor 'uniform-list-comparator 1
    (lambda (constructor)
      (memoized-constructor
       (unary-combinator constructor
			 uniform-list-predicate
			 make-ulist-=
			 make-ulist-<
			 make-ulist-hash)))))

(define (make-ulist-= elt=)
  (cond ((eqv? eq? elt=) eq-ulist=)
	((eqv? eqv? elt=) eqv-ulist=)
	(else (%make-ulist-= elt=))))

(define-integrable (%make-ulist-= elt=)
  (define (ulist= l1 l2)
    (if (and (pair? l1) (pair? l2))
	(and (elt= (car l1) (car l2))
	     (ulist= (cdr l1) (cdr l2)))
	(and (null? l1) (null? l2))))
  ulist=)

(define eq-ulist= (%make-ulist-= eq?))
(define eqv-ulist= (%make-ulist-= eqv?))

(define (make-ulist-< elt= elt<)
  (define (ulist< l1 l2)
    (and (not (null-list? l2))
	 (or (null-list? l1)
	     (elt< (car l1) (car l2))
	     (and (elt= (car l1) (car l2))
		  (ulist< (cdr l1) (cdr l2))))))
  ulist<)

(define (make-ulist-hash elt-hash)
  (cond ((eqv? eq-hash elt-hash) eq-ulist-hash)
	((eqv? eqv-hash elt-hash) eqv-ulist-hash)
	(else
	 (let ((elt-hash (checked-hash elt-hash)))
	   (%make-ulist-hash elt-hash)))))

(define-integrable (%make-ulist-hash elt-hash)
  (lambda (l)
    (fold (lambda (elt result)
	    (%combine-hashes (elt-hash elt) result))
	  (initial-hash)
	  l)))

(define eq-ulist-hash (%make-ulist-hash eq-hash))
(define eqv-ulist-hash (%make-ulist-hash eqv-hash))

(define-values-deferred (lset-comparator lset-comparator? lset-comparator-elt)
  (compound-comparator-constructor 'lset-comparator 1
    (lambda (constructor)
      (memoized-constructor
       (unary-combinator constructor
			 uniform-list-predicate
			 make-lset-=
			 make-lset-<
			 make-ulist-hash)))))

(define (make-lset-= elt=)
  (cond ((eqv? eq? elt=) eq-lset=)
	((eqv? eqv? elt=) eqv-lset=)
	(else (%make-lset-= elt=))))

(define-integrable (%make-lset-= elt=)
  (define-integrable (<=? l1 l2)
    (every (lambda (x)
	     (any (lambda (y) (elt= x y)) l2))
	   l1))
  (lambda (l1 l2)
    (or (eq? l1 l2)
	(and (<=? l1 l2)
	     (<=? l2 l1)))))

(define eq-lset= (%make-lset-= eq?))
(define eqv-lset= (%make-lset-= eqv?))

(define (make-lset-< elt= elt<)
  (declare (ignore elt<))
  (cond ((eqv? eq? elt=) eq-lset<)
	((eqv? eqv? elt=) eqv-lset<)
	(else (%make-lset-< elt=))))

(define-integrable (%make-lset-< elt=)
  (define-integrable (<=? l1 l2)
    (every (lambda (x)
	     (any (lambda (y) (elt= x y)) l2))
	   l1))
  (lambda (l1 l2)
    (and (<=? l1 l2)
	 (not (<=? l2 l1)))))

(define eq-lset< (%make-lset-< eq?))
(define eqv-lset< (%make-lset-< eqv?))

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

(define-values-deferred (uniform-weak-list-comparator
			 uniform-weak-list-comparator?
			 uniform-weak-list-comparator-elt)
  (compound-comparator-constructor 'uniform-weak-list-comparator 1
    (lambda (constructor)
      (memoized-constructor
       (unary-combinator constructor
			 uniform-weak-list-predicate
			 make-uwlist-=
			 make-uwlist-<
			 make-uwlist-hash)))))

(define (make-uwlist-= elt=)
  (cond ((eqv? eq? elt=) eq-uwlist=)
	((eqv? eqv? elt=) eqv-uwlist=)
	(else (%make-uwlist-= elt=))))

(define-integrable (%make-uwlist-= elt=)
  (define (uwlist= l1 l2)
    (if (and (weak-pair? l1) (weak-pair? l2))
	(and (let ((key1 (weak-car l1))
		   (key2 (weak-car l2)))
	       (and (not (gc-reclaimed-object? key1))
		    (not (gc-reclaimed-object? key2))
		    (elt= key1 key2)))
	     (uwlist= (weak-cdr l1) (weak-cdr l2)))
	(and (null? l1) (null? l2))))
  uwlist=)

(define eq-uwlist= (%make-uwlist-= eq?))
(define eqv-uwlist= (%make-uwlist-= eqv?))

(define (make-uwlist-< elt= elt<)
  (define (uwlist< l1 l2)
    (and (not (null-weak-list? l2))
	 (or (null-weak-list? l1)
	     (let ((key1 (weak-car l1))
		   (key2 (weak-car l2)))
	       (and (not (gc-reclaimed-object? key1))
		    (not (gc-reclaimed-object? key2))
		    (or (elt< key1 key2)
			(and (elt= key1 key2)
			     (uwlist< (weak-cdr l1) (weak-cdr l2)))))))))
  uwlist<)

(define (make-uwlist-hash elt-hash)
  (cond ((eqv? eq-hash elt-hash) eq-uwlist-hash)
	((eqv? eqv-hash elt-hash) eqv-uwlist-hash)
	(else
	 (let ((elt-hash (checked-hash elt-hash)))
	   (%make-uwlist-hash elt-hash)))))

(define-integrable (%make-uwlist-hash elt-hash)
  (lambda (uwlist)
    (weak-fold (lambda (elt result)
		 (%combine-hashes (elt-hash elt) result))
	       (initial-hash)
	       uwlist)))

(define eq-uwlist-hash (%make-uwlist-hash eq-hash))
(define eqv-uwlist-hash (%make-uwlist-hash eqv-hash))

(define-values-deferred (weak-lset-comparator
			 weak-lset-comparator?
			 weak-lset-comparator-elt)
  (compound-comparator-constructor 'weak-lset-comparator 1
    (lambda (constructor)
      (memoized-constructor
       (unary-combinator constructor
			 uniform-weak-list-predicate
			 make-wlset-=
			 make-wlset-<
			 make-uwlist-hash)))))

(define (make-wlset-= elt=)
  (cond ((eqv? eq? elt=) eq-wlset=)
	((eqv? eqv? elt=) eqv-wlset=)
	(else (%make-wlset-= elt=))))

(define-integrable (%make-wlset-= elt=)
  (define-integrable (<=? l1 l2)
    (weak-every (lambda (x)
		  (weak-any (lambda (y) (elt= x y)) l2))
		l1))
  (lambda (l1 l2)
    (or (eq? l1 l2)
	(and (<=? l1 l2)
	     (<=? l2 l1)))))

(define eq-wlset= (%make-wlset-= eq?))
(define eqv-wlset= (%make-wlset-= eqv?))

(define (make-wlset-< elt= elt<)
  (declare (ignore elt<))
  (cond ((eqv? eq? elt=) eq-wlset<)
	((eqv? eqv? elt=) eqv-wlset<)
	(else (%make-wlset-< elt=))))

(define-integrable (%make-wlset-< elt=)
  (define-integrable (<=? l1 l2)
    (weak-every (lambda (x)
		  (weak-any (lambda (y) (elt= x y)) l2))
		l1))
  (lambda (l1 l2)
    (and (<=? l1 l2)
	 (not (<=? l2 l1)))))

(define eq-wlset< (%make-wlset-< eq?))
(define eqv-wlset< (%make-wlset-< eqv?))

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

(define-values-deferred (uniform-vector-comparator
			 uniform-vector-comparator?
			 uniform-vector-comparator-elt)
  (compound-comparator-constructor 'uniform-vector-comparator 1
    (lambda (constructor)
      (memoized-constructor
       (unary-combinator constructor
			 uniform-vector-predicate
			 make-uvector-=
			 make-uvector-<
			 make-uvector-hash)))))

(define (make-uvector-= elt=)
  (lambda (v1 v2)
    (let ((end (vector-length v1)))
      (and (fix:= end (vector-length v2))
	   (let loop ((i 0))
	     (or (not (fix:< i end))
		 (and (elt= (vector-ref v1 i) (vector-ref v2 i))
		      (loop (fix:+ i 1)))))))))

(define (make-uvector-< elt= elt<)
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
  (let ((elt-hash (checked-hash elt-hash)))
    (lambda (v)
      (let ((end (vector-length v)))
	(let loop ((i 0) (result (initial-hash)))
	  (if (fix:< i end)
	      (loop (fix:+ i 1)
		    (%combine-hashes (elt-hash (vector-ref v i)) result))
	      result))))))

;;;; Hash functions

;;; Not a fixnum on 32-bit systems.
(define-syntax hash-bound
  (syntax-rules ()
    ((_) (select-on-bytes-per-word #x02000000 #x100000000))))

;;; Always a fixnum.
(define-syntax hash-mask
  (syntax-rules ()
    ((_) (select-on-bytes-per-word #x01FFFFFF #xFFFFFFFF))))

(define-syntax hash-bits
  (syntax-rules ()
    ((_) (select-on-bytes-per-word 25 32))))

;;; Always a fixnum.
(define (hash-salt)
  (random-integer (hash-bound)))

(define (checked-hash hash)
  (if (registered-hash? hash)
      hash
      (lambda (object)
	(check-hash (hash object)))))

;;; For hash tables only.
(define ((checked-hash-mod hash-mod) object modulus)
  (check-hash (hash-mod object modulus)))

(define (check-hash h)
  (%check-hash h))

(define-integrable (%check-hash h)
  (guarantee index-fixnum? h)
  (if (not (fix:<= h (hash-mask)))
      (error:bad-range-argument h))
  h)

(define-integrable (initial-hash)
  (select-on-bytes-per-word 25165813 3221225473))

(add-boot-init!
 (lambda ()
   (if (fixed-objects-item? 'initial-hash)
       (set-fixed-objects-item! 'initial-hash (initial-hash)))))

(define (combine-hashes h1 h2)
  (%combine-hashes (check-hash h1) (check-hash h2)))

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

(define-integrable (fixnum-hash n)
  (%combine-hashes (fix:and (fix:xor (fix:lsh n (fix:- 0 (hash-bits))) n)
			    (hash-mask))
		   (initial-hash)))

(define (number-hash z)
  (if (fix:fixnum? z)
      (fixnum-hash z)
      (primitive-object-hash z)))

;;;; Default comparator

(define (make-default-comparator)
  default-comparator)

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
(add-boot-init!
 (lambda ()
   (register-hash! default-hash #t)))

(define-deferred default-comparator
  (make-comparator any-object?
		   default=
		   default<
		   default-hash
		   #t))

(define-deferred pair-comparator
  (make-pair-comparator default-comparator default-comparator))

(define-deferred weak-pair-comparator
  (make-weak-pair-comparator default-comparator default-comparator))

(define-deferred list-comparator
  (uniform-list-comparator default-comparator))

(define-deferred vector-comparator
  (uniform-vector-comparator default-comparator))

(add-boot-init!
 (lambda ()

   ;; SRFI 128 requires that null? come before pair?.
   (define-default-type null?
     (lambda (x y) (declare (ignore x y)) #t)
     (lambda (x y) (declare (ignore x y)) #f)
     (lambda (x) (declare (ignore x)) (%combine-hashes 2777 (initial-hash))))

   (comparator-register-default! boolean-comparator)
   (comparator-register-default! bytevector-comparator)
   (comparator-register-default! char-comparator)
   (comparator-register-default! char-set-comparator)
   (comparator-register-default! fixnum-comparator)
   (comparator-register-default! string-comparator)
   (comparator-register-default! real-comparator)
   (comparator-register-default! symbol-comparator)
   (comparator-register-default! pair-comparator)
   (comparator-register-default! weak-pair-comparator)
   (comparator-register-default! vector-comparator)

   (define-default-type bit-string? bit-string=? #f eq-hash)
   (define-default-type cell? eq? #f eq-hash)
   (define-default-type pathname? pathname=? #f pathname-hash)

   (define-default-type complex?
     =
     (lambda (x y)
       (or (< (real-part x) (real-part y))
	   (< (imag-part x) (imag-part y))))
     eqv-hash)))