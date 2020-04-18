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

;;;; Weak pairs
;;; package: (runtime boot-definitions)

(declare (usual-integrations))

(define-primitives
  (weak-car 1)
  (weak-cdr 1)
  (weak-cons 2)
  (weak-pair? 1)
  (weak-set-car! 2)
  (weak-set-cdr! 2))

(define-integrable (weak-pair/car? pair)
  (not (gc-reclaimed-object? (weak-car pair))))

(define-integrable (gc-reclaimed-object)
  %gc-reclaimed)

(define-integrable (gc-reclaimed-object? object)
  (eq? %gc-reclaimed object))

(define-integrable %gc-reclaimed
  ((ucode-primitive object-set-type) (ucode-type constant) 10))

;;;; Weak lists

(define (weak-list? object)
  (let loop ((l1 object) (l2 object))
    (if (weak-pair? l1)
	(let ((l1 (weak-cdr l1)))
	  (and (not (eq? l1 l2))
	       (if (weak-pair? l1)
		   (loop (weak-cdr l1) (weak-cdr l2))
		   (null? l1))))
	(null? l1))))

(define (null-weak-list? object #!optional caller)
  (%null-weak-list? object caller))

(define-integrable (%null-weak-list? object caller)
  (cond ((null? object) #t)
	((weak-pair? object) #f)
	(else (error:not-a weak-list? object caller))))

(define (weak-list->list items)
  (%weak-fold-right cons '() items 'weak-list->list))

(define (list->weak-list items)
  (fold-right weak-cons '() items))

(define (weak-list->generator items)
  (%weak-list->generator items 'weak-list->generator))

(define (%weak-list->generator items caller)
  (let ((prev #f))
    (define (weak-list-generator)
      (if (%null-weak-list? items caller)
	  (eof-object)
	  (let ((item (weak-car items))
		(next (weak-cdr items)))
	    (if (gc-reclaimed-object? item)
		(begin
		  (if prev (weak-set-cdr! prev next))
		  (set! items next)
		  (weak-list-generator))
		(begin
		  (set! prev items)
		  (set! items next)
		  item)))))
    weak-list-generator))

(define (weak-memq item items)
  (define-integrable (predicate item*)
    (eq? item item*))
  (%member predicate items 'weak-memq))

(define (weak-memv item items)
  (define-integrable (predicate item*)
    (eq? item item*))
  (%member predicate items 'weak-memv))

(define (weak-member item items #!optional =)
  (let ((= (if (default-object? =) equal? =)))
    (define-integrable (predicate item*)
      (= item item*))
    (%member predicate items 'weak-member)))

(define (weak-find-tail predicate items)
  (%member predicate items 'weak-find-tail))

(define-integrable (%member predicate items caller)
  (let loop ((this items) (prev #f))
    (if (%null-weak-list? this caller)
	#f
	(let ((item (weak-car this))
	      (next (weak-cdr this)))
	  (cond ((gc-reclaimed-object? item)
		 (if prev (weak-set-cdr! prev next))
		 (loop next prev))
		((predicate item) this)
		(else (loop next this)))))))

(define (weak-reverse! items)
  (%weak-append-reverse! items '() 'weak-reverse!))

(define (weak-append-reverse! items tail)
  (%weak-append-reverse! items tail 'weak-append-reverse!))

(define (%weak-append-reverse! items tail caller)
  (let loop ((this items) (tail tail))
    (if (%null-weak-list? this caller)
	tail
	(let ((item (weak-car this))
	      (next (weak-cdr this)))
	  (if (gc-reclaimed-object? item)
	      (loop next tail)
	      (begin
		(weak-set-cdr! this tail)
		(loop next this)))))))

(define (weak-find predicate items)
  (let loop ((this items) (prev #f))
    (if (%null-weak-list? this 'weak-find)
	#f
	(let ((item (weak-car this))
	      (next (weak-cdr this)))
	  (cond ((gc-reclaimed-object? item)
		 (if prev (weak-set-cdr! prev next))
		 (loop next prev))
		((predicate item) item)
		(else (loop next this)))))))

(define (weak-drop-while predicate items)
  (let loop ((this items) (prev #f))
    (if (%null-weak-list? this 'weak-drop-while)
	'()
	(let ((item (weak-car this))
	      (next (weak-cdr this)))
	  (cond ((gc-reclaimed-object? item)
		 (if prev (weak-set-cdr! prev next))
		 (loop next prev))
		((predicate item) (loop next this))
		(else this))))))

(define (weak-take-while predicate items)
  (let loop ((this items) (prev #f))
    (if (%null-weak-list? this 'weak-take-while)
	'()
	(let ((item (weak-car this))
	      (next (weak-cdr this)))
	  (cond ((gc-reclaimed-object? item)
		 (if prev (weak-set-cdr! prev next))
		 (loop next prev))
		((predicate item)
		 (weak-cons item (loop next this)))
		(else
		 '()))))))

(define (weak-take-while! predicate items)
  (if (or (%null-weak-list? items 'weak-take-while!)
	  (not (predicate (weak-car items))))
      '()
      (let loop ((this (cdr items)) (prev items))
	(if (%null-weak-list? this 'weak-take-while!)
	    '()
	    (let ((item (weak-car this))
		  (next (weak-cdr this)))
	      (cond ((gc-reclaimed-object? item)
		     (weak-set-cdr! prev next)
		     (loop next prev))
		    ((predicate item)
		     (weak-cons item (loop next this)))
		    (else
		     '())))))))

(define (weak-delq! item items)
  (define-integrable (predicate item*)
    (eq? item item*))
  (%delete! predicate items 'weak-delq!))

(define (weak-delv! item items)
  (define-integrable (predicate item*)
    (eqv? item item*))
  (%delete! predicate items 'weak-delv!))

(define (weak-delete! item items #!optional =)
  (let ((= (if (default-object? =) equal? =)))
    (define-integrable (predicate item*)
      (= item item*))
    (%delete! predicate items 'weak-delete!)))

(define (weak-remove! predicate items)
  (%delete! predicate items 'weak-remove!))

(define (weak-filter! keep? items)
  (define-integrable (predicate item)
    (not (keep? item)))
  (%delete! predicate items 'weak-filter!))

(define (clean-weak-list! items)
  (define-integrable (predicate item)
    (declare (ignore item))
    #t)
  (%delete! predicate items 'clean-weak-list!))

(define-integrable (%delete! predicate items caller)

  (define (skip this)
    (if (%null-weak-list? this caller)
	this
	(let ((item (weak-car this))
	      (next (weak-cdr this)))
	  (if (or (gc-reclaimed-object? item) (predicate item))
	      (skip next)
	      (begin
		(scan next this)
		this)))))

  (define (scan this prev)
    (if (%null-weak-list? this caller)
	(weak-set-cdr! prev this)
	(let ((item (weak-car this))
	      (next (weak-cdr this)))
	  (if (or (gc-reclaimed-object? item) (predicate item))
	      (scan next prev)
	      (begin
		(weak-set-cdr! prev this)
		(scan next this))))))

  (skip items))

(define (weak-any predicate items)
  (generator-any predicate (%weak-list->generator items 'weak-any)))

(define (weak-every predicate items)
  (generator-every predicate (%weak-list->generator items 'weak-every)))

(define (weak-for-each procedure first . rest)
  (if (null? rest)
      (%weak-for-each procedure first 'weak-for-each)
      (apply generator-for-each
	     procedure
	     (weak-list->generator first)
	     (map weak-list->generator rest))))

(define (%weak-for-each procedure items caller)
  (let loop ((this items) (prev #f))
    (if (not (%null-weak-list? this caller))
	(let ((item (weak-car this))
	      (next (weak-cdr this)))
	  (if (gc-reclaimed-object? item)
	      (begin
		(if prev (weak-set-cdr! prev next))
		(loop next prev))
	      (begin
		(procedure item)
		(loop next this)))))))

(define (weak-fold kons knil first . rest)
  (if (null? rest)
      (%weak-fold kons knil first 'weak-fold)
      (apply generator-fold
	     kons
	     knil
	     (weak-list->generator first)
	     (map weak-list->generator rest))))

(define (%weak-fold kons knil items caller)
  (let loop ((this items) (prev #f) (acc knil))
    (if (%null-weak-list? this caller)
	acc
	(let ((item (weak-car this))
	      (next (weak-cdr this)))
	  (if (gc-reclaimed-object? item)
	      (begin
		(if prev (weak-set-cdr! prev next))
		(loop next prev acc))
	      (loop next this (kons item acc)))))))

(define (weak-fold-map kons knil procedure first . rest)
  (if (null? rest)
      (%weak-fold-map kons knil procedure first 'weak-fold-map)
      (apply generator-fold-map
	     kons
	     knil
	     procedure
	     (weak-list->generator first)
	     (map weak-list->generator rest))))

(define (%weak-fold-map kons knil procedure items caller)
  (%weak-fold (lambda (item acc)
		(kons (procedure item) acc))
	      knil
	      items
	      caller))

(define (weak-fold-right kons knil first . rest)
  (if (null? rest)
      (%weak-fold-right kons knil first 'weak-fold-right)
      (apply generator-fold-right
	     kons
	     knil
	     (weak-list->generator first)
	     (map weak-list->generator rest))))

(define (%weak-fold-right kons knil items caller)
  (let loop ((this items) (prev #f))
    (if (%null-weak-list? this caller)
	knil
	(let ((item (weak-car this))
	      (next (weak-cdr this)))
	  (if (gc-reclaimed-object? item)
	      (begin
		(if prev (weak-set-cdr! prev next))
		(loop next prev))
	      (kons item (loop next this)))))))

(define (weak-fold-right-map kons knil procedure first . rest)
  (if (null? rest)
      (%weak-fold-right-map kons knil procedure first 'weak-fold-right-map)
      (apply generator-fold-right-map
	     kons
	     knil
	     procedure
	     (weak-list->generator first)
	     (map weak-list->generator rest))))

(define (%weak-fold-right-map kons knil procedure items caller)
  (%weak-fold-right (lambda (item acc)
		      (kons (procedure item) acc))
		    knil
		    items
		    caller))

(define (weak-assq item items)
  (define-integrable (predicate item*)
    (eq? item item*))
  (%assoc predicate items 'weak-assq))

(define (weak-assv item items)
  (define-integrable (predicate item*)
    (eq? item item*))
  (%assoc predicate items 'weak-assv))

(define (weak-assoc item items #!optional =)
  (let ((= (if (default-object? =) equal? =)))
    (define-integrable (predicate item*)
      (= item item*))
    (%assoc predicate items 'weak-assoc)))

(define-integrable (%assoc predicate alist caller)
  (let loop ((this alist) (prev #f))
    (if (null-list? this caller)
	#f
	(let ((p (car this))
	      (next (cdr this)))
	  (let ((item (weak-car p)))
	    (cond ((gc-reclaimed-object? item)
		   (if prev (set-cdr! prev next))
		   (loop next prev))
		  ((predicate item) p)
		  (else (loop next this))))))))

(define (clean-weak-alist! alist)
  (remove! (lambda (p)
	     (gc-reclaimed-object? (weak-car p)))
	   alist))

;;;; Weak sets

(define (weak-set = . items)
  (%record weak-set-tag = (delete-duplicates items =)))

(define (weak-set? object)
  (and (%record? object)
       (eq? weak-set-tag (%record-ref object 0))))

(define-integrable weak-set-tag
  '|#[(runtime weak-pair)weak-set]|)

(define-integrable (%set-predicate set) (%record-ref set 1))

(define-integrable (%set-items set) (%record-ref set 2))
(define-integrable (%set-set-items! set items) (%record-set! set 2 items))

(define (add-to-weak-set! item set)
  (guarantee weak-set? set 'add-to-weak-set!)
  (let ((= (%set-predicate set))
	(items (%set-items set)))
    (let loop ((this items))
      (if (weak-pair? this)
	  (let ((item* (weak-car this))
		(next (weak-cdr this)))
	    (cond ((gc-reclaimed-object? item*)
		   (weak-set-car! this item)
		   #t)
		  ((= item item*)
		   #f)
		  (else
		   (loop next this))))
	  (begin
	    (%set-set-items! set (weak-cons item items))
	    #t)))))

(define (remove-from-weak-set! item set)
  (guarantee weak-set? set 'remove-from-weak-set!)
  (let ((= (%set-predicate set)))
    (let loop ((this (%set-items set)) (prev #f))
      (if (weak-pair? this)
	  (let ((item* (weak-car this))
		(next (weak-cdr this)))
	    (cond ((gc-reclaimed-object? item*)
		   (weak-set-cdr! prev next)
		   (loop next prev))
		  ((= item item*)
		   (if prev
		       (weak-set-cdr! prev next)
		       (%set-set-items! set next))
		   #t)
		  (else
		   (loop next this))))
	  #f))))

(define (weak-set-contains? item set)
  (guarantee weak-set? set 'weak-set-contains?)
  (weak-any (let ((= (%set-predicate set)))
	      (lambda (item*)
		(= item* item)))
	    (%trim-set-items set)))

(define (weak-set-fold kons knil set)
  (guarantee weak-set? set 'weak-set-fold)
  (weak-fold kons knil (%trim-set-items set)))

(define (weak-set-fold-right kons knil set)
  (guarantee weak-set? set 'weak-set-fold-right)
  (weak-fold-right kons knil (%trim-set-items set)))

(define (weak-set-any predicate set)
  (guarantee weak-set? set 'weak-set-any)
  (weak-any predicate (%trim-set-items set)))

(define (weak-set-every predicate set)
  (guarantee weak-set? set 'weak-set-every)
  (weak-every predicate (%trim-set-items set)))

(define (weak-set-for-each procedure set)
  (guarantee weak-set? set 'weak-set-for-each)
  (%weak-for-each procedure (%trim-set-items set) 'weak-set-for-each))

(define (%trim-set-items set)
  (let loop ((this (%set-items set)))
    (if (and (weak-pair? this)
	     (gc-reclaimed-object? (weak-car this)))
	(let ((next (weak-cdr this)))
	  (%set-set-items! set next)
	  (loop next))
	this)))

(define (weak-set->list set)
  (guarantee weak-set? set 'weak-set->list)
  (weak-list->list (%set-items set)))

(define (clean-weak-set! set)
  (guarantee weak-set? set 'clean-weak-set!)
  (let loop ((this (%set-items set)) (prev #f))
    (if (weak-pair? this)
	(let ((next (weak-cdr this)))
	  (if (gc-reclaimed-object? (weak-car this))
	      (begin
		(if prev
		    (weak-set-cdr! prev next)
		    (%set-set-items! set next))
		(loop next prev))
	      (loop next this))))))

(define (clear-weak-set! set)
  (guarantee weak-set? set 'clear-weak-set!)
  (%set-set-items! set '()))