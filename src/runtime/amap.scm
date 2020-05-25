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

;;;; Associative maps (generalized from SRFI 125)
;;; package: (runtime amap)

(declare (usual-integrations))

;;; Constructors

(define (make-amap comparator . args)
  (guarantee comparator? comparator 'make-amap)
  (let-values (((impl impl-name) (select-impl comparator args)))
    (%make-amap comparator
		args
		impl-name
		impl
		((amap-impl:new-state impl) comparator args))))

(define (%amap-new-state amap state)
  (%make-amap (amap-comparator amap)
	      (amap-args amap)
	      (amap-implementation-name amap)
	      (amap-impl amap)
	      state))

(define-record-type <amap>
    (%make-amap comparator args impl-name impl state)
    amap?
  (comparator amap-comparator)
  (args amap-args)
  (impl-name amap-implementation-name)
  (impl amap-impl)
  (state amap-state))

(define-print-method amap?
  (standard-print-method 'amap
    (lambda (amap)
      (list (amap-implementation-name amap)))))

(define (amap-unfold stop? mapper successor seed comparator . args)
  (let ((result (apply make-amap comparator args)))
    (let loop ((seed seed))
      (if (stop? seed)
	  result
	  (let-values (((key value) (mapper seed)))
	    (amap-set! result key value)
	    (loop (successor seed)))))))

(define (alist->amap alist comparator . args)
  (let ((result (apply make-amap comparator args)))
    (alist-for-each (lambda (key value)
		      (amap-set! result key value))
		    alist)
    result))

;;; Predicates

(define (amap-contains? amap key)
  ((amap-impl:contains? (amap-impl amap)) (amap-state amap) key))

(define (amap-empty? amap)
  ((amap-impl:empty? (amap-impl amap)) (amap-state amap)))

(define (amap=? value-comparator amap1 amap2)
  (let ((key-comparator (amap-comparator amap1)))
    (if (not (eqv? key-comparator (amap-comparator amap2)))
	(error:bad-range-argument amap2 'amap=?))
    (let ((keys (amap-keys amap1)))
      (and (lset= (comparator-equality-predicate key-comparator)
		  keys
		  (amap-keys amap2))
	   (every (let ((=? (comparator-equality-predicate value-comparator)))
		    (lambda (key)
		      (=? (amap-ref amap1 key) (amap-ref amap2 key))))
		  keys)))))

(define (amap-mutable? amap)
  ((amap-impl:mutable? (amap-impl amap)) (amap-state amap)))

;;; Accessors

(define (amap-ref amap key #!optional fail succeed)
  ((amap-impl:ref (amap-impl amap)) (amap-state amap) key fail succeed))

(define (amap-ref/default amap key default)
  ((amap-impl:ref/default (amap-impl amap)) (amap-state amap) key default))

;;; Mutators

(define (amap-set! amap . plist)
  (apply (amap-impl:set! (amap-impl amap)) (amap-state amap) plist))

(define (amap-delete! amap . keys)
  (apply (amap-impl:delete! (amap-impl amap)) (amap-state amap) keys))

(define (amap-intern! amap key fail)
  ((amap-impl:intern! (amap-impl amap)) (amap-state amap) key fail))

(define (amap-update! amap key updater #!optional fail succeed)
  ((amap-impl:update! (amap-impl amap))
   (amap-state amap) key updater fail succeed))

(define (amap-update!/default amap key updater default)
  ((amap-impl:update!/default (amap-impl amap))
   (amap-state amap) key updater default))

(define (amap-pop! amap)
  ((amap-impl:pop! (amap-impl amap)) (amap-state amap)))

(define (amap-clear! amap)
  ((amap-impl:clear! (amap-impl amap)) (amap-state amap)))

(define (amap-clean! amap)
  ((amap-impl:clean! (amap-impl amap)) (amap-state amap)))

;;; The whole hash table

(define (amap-size amap)
  ((amap-impl:size (amap-impl amap)) (amap-state amap)))

(define (amap-keys amap)
  ((amap-impl:keys (amap-impl amap)) (amap-state amap)))

(define (amap-values amap)
  ((amap-impl:values (amap-impl amap)) (amap-state amap)))

(define (amap-entries amap)
  ((amap-impl:entries (amap-impl amap)) (amap-state amap)))

(define (amap-find procedure amap fail)
  ((amap-impl:find (amap-impl amap)) procedure (amap-state amap) fail))

(define (amap-count predicate amap)
  ((amap-impl:count (amap-impl amap)) predicate (amap-state amap)))

;;; Mapping and folding

(define (amap-map procedure amap #!optional comparator . args)
  (let ((comparator
	 (if (default-object? comparator)
	     (amap-comparator amap)
	     comparator))
	(args
	 (if (and (default-object? comparator) (null? args))
	     (amap-args amap)
	     args)))
    (%make-amap comparator args (amap-implementation-name amap) (amap-impl amap)
      ((amap-impl:map (amap-impl amap))
       procedure comparator args (amap-state amap)))))

(define (amap-for-each procedure amap)
  ((amap-impl:for-each (amap-impl amap)) procedure (amap-state amap)))

(define (amap-map! procedure amap)
  ((amap-impl:map! (amap-impl amap)) procedure (amap-state amap)))

(define (amap-map->list procedure amap)
  ((amap-impl:map->list (amap-impl amap)) procedure (amap-state amap)))

(define (amap-fold kons knil amap)
  ((amap-impl:fold (amap-impl amap)) kons knil (amap-state amap)))

(define (amap-prune! predicate amap)
  ((amap-impl:prune! (amap-impl amap)) predicate (amap-state amap)))

;;; Copying and conversion

(define (amap-copy amap #!optional mutable?)
  (%amap-new-state amap
    ((amap-impl:copy (amap-impl amap)) (amap-state amap) mutable?)))

(define (amap-empty-copy amap)
  (%amap-new-state amap
    ((amap-impl:empty-copy (amap-impl amap)) (amap-state amap))))

(define (amap->alist amap)
  ((amap-impl:->alist (amap-impl amap)) (amap-state amap)))

;;; Association maps as sets

(define (amap-union! amap1 amap2)
  ((amap-impl:union! (amap-impl amap1)) amap2)
  amap1)

(define (amap-intersection! amap1 amap2)
  ((amap-impl:intersection! (amap-impl amap1)) (amap-state amap1) amap2)
  amap1)

(define (amap-difference! amap1 amap2)
  ((amap-impl:difference! (amap-impl amap1)) (amap-state amap1) amap2)
  amap1)

(define (amap-xor! amap1 amap2)
  ((amap-impl:xor! (amap-impl amap1)) (amap-state amap1) amap2)
  amap1)

;;;; Implementation data structure

(define (%make-amap-impl metadata get-operation)
  (let* ((operators (all-operators))
	 (n-ops (length operators))
	 (impl
	  (%make-record %amap-impl-tag
			(fix:+ %amap-impl-op-offset n-ops))))
    (%record-set! impl 1 metadata)
    (for-each (lambda (operator index)
		(%record-set! impl index (get-operation operator)))
	      operators
	      (iota n-ops %amap-impl-op-offset))
    impl))

(define (amap-impl? object)
  (and (%record? object)
       (eq? %amap-impl-tag (%record-ref object 0))))
(register-predicate! amap-impl? 'amap-impl '<= %record?)

(define-integrable %amap-impl-tag '|(runtime amap)impl|)
(define-integrable %amap-impl-op-offset 2)

(define (amap-impl-metadata impl)
  (guarantee amap-impl? impl 'amap-impl-metadata)
  (%record-ref impl 1))

(define (operator? object)
  (and (assq object operator-property-alist) #t))

(define (all-operators)
  (filter-map (lambda (entry)
		(and (not (memq 'pseudo (cdr entry)))
		     (car entry)))
	      operator-property-alist))

(define (operator-properties operator)
  (cdr (assq operator operator-property-alist)))

(define (operator-mutates? operator)
  (memq 'mutates (operator-properties operator)))

(let-syntax
    ((op-defs
      (er-macro-transformer
       (lambda (form rename compare)
	 (syntax-check '(_ * (+ symbol)) form)
	 (let ((op-entries (cdr form)))
	   `(,(rename 'begin)
	     (,(rename 'define) operator-property-alist ',op-entries)
	     ,@(let ((normal
		      (remove (lambda (entry)
				(memq 'pseudo (cdr entry)))
			      op-entries)))
		 (map (lambda (entry index)
			`(,(rename 'define-integrable)
			  (,(symbol 'amap-impl: (car entry)) impl)
			  (,(rename 'declare)
			   (,(rename 'no-type-checks))
			   (,(rename 'no-range-checks)))
			  (,(rename '%record-ref) impl ,index)))
		      normal
		      (iota (length normal) 2)))))))))

  (op-defs (->alist)
	   (clean! mutates)
	   (clear! mutates)
	   (contains?)
	   (copy)
	   (count)
	   (delete! mutates)
	   (delete-1! mutates pseudo)
	   (difference! mutates)
	   (empty-copy)
	   (empty?)
	   (entries)
	   (find)
	   (fold)
	   (for-each)
	   (intern! mutates)
	   (intersection! mutates)
	   (keys)
	   (map)
	   (map! mutates)
	   (map->list)
	   (mutable?)
	   (new-state)
	   (pop! mutates)
	   (prune! mutates)
	   (ref)
	   (ref/default)
	   (set! mutates)
	   (set-1! mutates pseudo)
	   (size)
	   (union! mutates)
	   (update! mutates)
	   (update!/default mutates)
	   (values)
	   (xor! mutates)))