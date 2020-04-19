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

(define (weak-list-empty? items)
  (if (%null-weak-list? items 'weak-list-empty?)
      #t
      (let ((next (weak-cdr items)))
	(if (gc-reclaimed-object? (weak-car items))
	    (weak-list-empty? next)
	    #t))))

(define (weak-length items)
  (let loop ((this items) (prev #f) (length 0))
    (if (%null-weak-list? this 'weak-length+)
	length
	(let ((next (weak-cdr this)))
	  (if (gc-reclaimed-object? (weak-car this))
	      (begin
		(if prev (weak-set-cdr! prev next))
		(loop next prev length))
	      (loop next this (fix:+ length 1)))))))

(define (weak-length+ items)

  (define (loop this past prev length)
    (let ((this+1 (get-next this prev)))
      (if this+1
	  (and (not (eq? this+1 past))
	       (let ((this+2 (get-next this+1 this)))
		 (if this+2
		     (loop this+2 (weak-cdr past) this+1 (fix:+ length 2))
		     (fix:+ length 1))))
	  length)))

  (define (get-next this prev)
    (if (%null-weak-list? this 'weak-length+)
	#f
	(let ((next (weak-cdr this)))
	  (if (gc-reclaimed-object? (weak-car this))
	      (begin
		(if prev (weak-set-cdr! prev next))
		(get-next next prev))
	      next))))

  (loop items items #f 0))

(define (weak-list-copy items)
  (let loop ((this items) (prev #f))
    (if (%null-weak-list? items 'weak-list-copy)
	'()
	(let ((item (weak-car this))
	      (next (weak-cdr this)))
	  (if (gc-reclaimed-object? item)
	      (begin
		(if prev (weak-set-cdr! prev next))
		(loop next prev))
	      (weak-cons item (loop next this)))))))

(define (weak-list->generator items)
  (let ((prev #f))
    (define (weak-list-generator)
      (if (%null-weak-list? items 'weak-list->generator)
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

(define (generator->weak-list gen)
  (let loop ()
    (let ((v (gen)))
      (if (eof-object? v)
	  '()
	  (weak-cons v (loop))))))

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

  (define (skip this)
    (if (%null-weak-list? this 'weak-take-while!)
	this
	(let ((item (weak-car this))
	      (next (weak-cdr this)))
	  (cond ((gc-reclaimed-object? item)
		 (skip next))
		((predicate item)
		 (scan next this)
		 this)
		(else '())))))

  (define (scan this prev)
    (if (not (%null-weak-list? this 'weak-take-while!))
	(let ((item (weak-car this))
	      (next (weak-cdr this)))
	  (cond ((gc-reclaimed-object? item)
		 (weak-set-cdr! prev next)
		 (scan next prev))
		((predicate item)
		 (scan next this))
		(else
		 (weak-set-cdr! prev '()))))))

  (skip items))

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

(define (weak-filter predicate items)
  (generator->weak-list (gfilter predicate (weak-list->generator items))))

(define (weak-remove predicate items)
  (generator->weak-list (gremove predicate (weak-list->generator items))))

(define (weak-any predicate items)
  (generator-any predicate (weak-list->generator items)))

(define (weak-every predicate items)
  (generator-every predicate (weak-list->generator items)))

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

;;;; Weak alists

(define (weak-alist? object)
  (list-of-type? weak-pair? object))

(define (weak-alist-copy alist)
  (let loop ((this alist) (prev #f))
    (if (null-list? this 'weak-alist-copy)
	'()
	(let ((p (car this))
	      (next (cdr this)))
	  (let ((key (weak-car p)))
	    (if (gc-reclaimed-object? key)
		(begin
		  (if prev (set-cdr! prev next))
		  (loop next prev))
		(cons (weak-cons key (weak-cdr p))
		      (loop next this))))))))

(define (weak-alist-filter predicate alist)
  (%weak-alist-fold-right (lambda (key datum acc)
			    (if (predicate key datum)
				(cons (weak-cons key datum) acc)
				acc))
			  '()
			  alist
			  'weak-alist-filter))

(define (weak-alist-remove predicate alist)
  (%weak-alist-fold-right (lambda (key datum acc)
			    (if (predicate key datum)
				acc
				(cons (weak-cons key datum) acc)))
			  '()
			  alist
			  'weak-alist-remove))

(define (weak-alist->generator alist)
  (let ((prev #f))
    (define (weak-alist-generator)
      (if (null-list? alist 'weak-alist->generator)
	  (eof-object)
	  (let ((p (car alist))
		(next (cdr alist)))
	    (let ((key (weak-car p)))
	      (if (gc-reclaimed-object? key)
		  (begin
		    (if prev (set-cdr! prev next))
		    (set! alist next)
		    (weak-alist-generator))
		  (begin
		    (set! prev alist)
		    (set! alist next)
		    p))))))
    weak-alist-generator))

(define (weak-alist-fold kons knil first . rest)
  (if (null? rest)
      (%weak-alist-fold kons knil first 'weak-alist-fold)
      (apply generator-fold
	     (lambda (p acc)
	       (let ((key (weak-car p)))
		 (if (gc-reclaimed-object? key)
		     acc
		     (kons key (weak-cdr p) acc))))
	     knil
	     (weak-alist->generator first)
	     (map weak-alist->generator rest))))

(define (%weak-alist-fold kons knil alist caller)
  (let loop ((this alist) (prev #f) (acc knil))
    (if (null-list? this caller)
	acc
	(let ((p (car this))
	      (next (cdr this)))
	  (let ((key (weak-car p)))
	    (if (gc-reclaimed-object? key)
		(begin
		  (if prev (set-cdr! prev next))
		  (loop next prev acc))
		(loop next this (kons key (weak-cdr p) acc))))))))

(define (weak-alist-fold-right kons knil first . rest)
  (if (null? rest)
      (%weak-alist-fold-right kons knil first 'weak-alist-fold-right)
      (apply generator-fold-right
	     (lambda (p acc)
	       (let ((key (weak-car p)))
		 (if (gc-reclaimed-object? key)
		     acc
		     (kons key (weak-cdr p) acc))))
	     knil
	     (weak-alist->generator first)
	     (map weak-alist->generator rest))))

(define (%weak-alist-fold-right kons knil alist caller)
  (let loop ((this alist) (prev #f))
    (if (null-list? this caller)
	knil
	(let ((p (car this))
	      (next (cdr this)))
	  (let ((key (weak-car p)))
	    (if (gc-reclaimed-object? key)
		(begin
		  (if prev (set-cdr! prev next))
		  (loop next prev))
		(kons key (weak-cdr p) (loop next this))))))))

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
	  (let ((key (weak-car p)))
	    (cond ((gc-reclaimed-object? key)
		   (if prev (set-cdr! prev next))
		   (loop next prev))
		  ((predicate key) p)
		  (else (loop next this))))))))

(define (weak-alist-find predicate alist)
  (%assoc predicate alist 'weak-alist-find))

(define (weak-del-assq key alist)
  (weak-alist-remove (lambda (key* datum)
		       (declare (ignore datum))
		       (eq? key key*))
		     alist))

(define (weak-del-assv key alist)
  (weak-alist-remove (lambda (key* datum)
		       (declare (ignore datum))
		       (eq? key key*))
		     alist))

(define (weak-del-assoc key alist #!optional =)
  (weak-alist-remove (let ((= (if (default-object? =) equal? =)))
		       (lambda (key* datum)
			 (declare (ignore datum))
			 (= key key*)))
		     alist))

(define (weak-del-assq! key alist)
  (define-integrable (predicate key* datum)
    (declare (ignore datum))
    (eq? key key*))
  (%weak-alist-remove! predicate alist 'weak-del-assq!))

(define (weak-del-assv! key alist)
  (define-integrable (predicate key* datum)
    (declare (ignore datum))
    (eqv? key key*))
  (%weak-alist-remove! predicate alist 'weak-del-assv!))

(define (weak-del-assoc! key alist #!optional =)
  (let ((= (if (default-object? =) equal? =)))
    (define-integrable (predicate key* datum)
      (declare (ignore datum))
      (= key key*))
    (%weak-alist-remove! predicate alist 'weak-del-assoc!)))

(define (weak-alist-remove! predicate alist)
  (%weak-alist-remove! predicate alist 'weak-alist-remove!))

(define (weak-alist-filter! keep? alist)
  (define-integrable (predicate key datum)
    (not (keep? key datum)))
  (%weak-alist-remove! predicate alist 'weak-alist-filter!))

(define-integrable (%weak-alist-remove! predicate alist caller)

  (define (skip this)
    (if (null-list? this caller)
	this
	(let ((p (car this))
	      (next (cdr this)))
	  (let ((key (weak-car p)))
	    (if (or (gc-reclaimed-object? key)
		    (predicate key (weak-cdr p)))
		(skip next)
		(begin
		  (scan next this)
		  this))))))

  (define (scan this prev)
    (if (null-list? this caller)
	(set-cdr! prev this)
	(let ((p (car this))
	      (next (cdr this)))
	  (let ((key (weak-car p)))
	    (if (or (gc-reclaimed-object? key)
		    (predicate key (weak-cdr p)))
		(scan next prev)
		(begin
		  (set-cdr! prev this)
		  (scan next this)))))))

  (skip alist))

(define (clean-weak-alist! alist)
  (remove! (lambda (p)
	     (gc-reclaimed-object? (weak-car p)))
	   alist))

(define ((weak-alist-adjoiner key= kons knil #!optional caller) key datum alist)
  (let loop ((this alist) (prev #f))
    (if (null-list? this caller)
	(list (weak-cons key (kons datum knil)))
	(let ((p (car this))
	      (next (cdr this)))
	  (let ((key* (weak-car p)))
	    (cond ((gc-reclaimed-object? key*)
		   (if prev (set-cdr! prev next))
		   (loop next prev))
		  ((key= key key*)
		   (cons (weak-cons key (kons datum (weak-cdr p)))
			 next))
		  (else
		   (cons (weak-cons key* (weak-cdr p))
			 (loop next this)))))))))

(define ((weak-alist-adjoiner! key= kons knil #!optional caller)
	 key datum alist)
  (let loop ((this alist) (prev #f))
    (if (null-list? this caller)
	(cons (weak-cons key (kons datum knil)) alist)
	(let ((p (car this))
	      (next (cdr this)))
	  (let ((key* (weak-car p)))
	    (cond ((gc-reclaimed-object? key*)
		   (if prev (set-cdr! prev next))
		   (loop next prev))
		  ((key= key key*)
		   (weak-set-cdr! p (kons datum (weak-cdr p)))
		   alist)
		  (else
		   (loop next this))))))))

(define (weak-alist-replace key= key datum alist)
  ((weak-alist-adjoiner key=
			(lambda (datum other)
			  (declare (ignore other))
			  datum)
			unspecific
			'weak-alist-replace)
   key datum alist))

(define (weak-alist-replace! key= key datum alist)
  ((weak-alist-adjoiner! key=
			 (lambda (datum other)
			   (declare (ignore other))
			   datum)
			 unspecific
			 'weak-alist-replace!)
   key datum alist))

;;;; Weak alist tables

(define (weak-alist-table key= #!optional finalizer)
  (%record weak-alist-table-tag
	   key=
	   (if (default-object? finalizer) #f finalizer)
	   '()))

(define (weak-alist-table? object)
  (and (%record? object)
       (eq? weak-alist-table-tag (%record-ref object 0))))

(define-integrable weak-alist-table-tag
  '|#[(runtime weak-pair)weak-alist-table]|)

(define-integrable (%table-key= table) (%record-ref table 1))
(define-integrable (%table-finalizer table) (%record-ref table 2))

(define-integrable (%table-alist table) (%record-ref table 3))
(define-integrable (%set-table-alist! table alist) (%record-set! table 3 alist))

(define (weak-alist-table-key= table)
  (guarantee weak-alist-table? table 'weak-alist-table-key=)
  (%table-key= table))

(define (weak-alist-table-finalizer table)
  (guarantee weak-alist-table? table 'weak-alist-table-finalizer)
  (%table-finalizer table))

(define (weak-alist-table-size table)
  (weak-alist-table-fold (lambda (key value acc)
			   (declare (ignore key value))
			   (fix:+ acc 1))
			 0
			 table))

(define (weak-alist-table-empty? table)
  (define-integrable (predicate key)
    (declare (ignore key))
    #t)
  (%weak-alist-table-search table predicate 'weak-alist-table-empty?
    (lambda (key p) (declare (ignore key p)) #f)
    (lambda () #t)))

(define (weak-alist-table-fold kons knil table)
  (guarantee weak-alist-table? table 'weak-alist-table-fold)
  (let ((finalizer (%table-finalizer table)))
    (let loop ((this (%table-alist table)) (prev #f) (acc knil))
      (if (pair? this)
	  (let ((p (car this))
		(next (cdr this)))
	    (let ((key (weak-car p)))
	      (if (gc-reclaimed-object? key)
		  (begin
		    (if finalizer
			(finalizer (weak-cdr p)))
		    (if prev
			(set-cdr! prev next)
			(%set-table-alist! table next))
		    (loop next prev acc))
		  (loop next this (kons key (weak-cdr p) acc)))))
	  acc))))

(define (weak-alist-table-fold-right kons knil table)
  (guarantee weak-alist-table? table 'weak-alist-table-fold-right)
  (let ((finalizer (%table-finalizer table)))
    (let loop ((this (%table-alist table)) (prev #f))
      (if (pair? this)
	  (let ((p (car this))
		(next (cdr this)))
	    (let ((key (weak-car p)))
	      (if (gc-reclaimed-object? key)
		  (begin
		    (if finalizer
			(finalizer (weak-cdr p)))
		    (if prev
			(set-cdr! prev next)
			(%set-table-alist! table next))
		    (loop next prev))
		  (kons key (weak-cdr p) (loop next this)))))
	  knil))))

(define (weak-alist-table-keys table)
  (weak-alist-table-fold (lambda (key value acc)
			   (declare (ignore value))
			   (cons key acc))
			 '()
			 table))

(define (weak-alist-table-values table)
  (weak-alist-table-fold (lambda (key value acc)
			   (declare (ignore key))
			   (cons value acc))
			 '()
			 table))

(define (weak-alist-table->alist table)
  (weak-alist-table-fold (lambda (key value acc)
			   (cons (cons key value) acc))
			 '()
			 table))

(define (weak-alist-table-exists? table key)
  (%operate-on-table table key 'weak-alist-table-exists?
    (lambda (p) (declare (ignore p)) #t)
    (lambda () #f)))

(define (weak-alist-table-ref table key #!optional get-default)
  (%operate-on-table table key 'weak-alist-table-ref
    weak-cdr
    (if (default-object? get-default)
	(lambda () (error:bad-range-argument key 'weak-alist-table-ref))
	get-default)))

(define (%weak-alist-table-get-pair table key)
  (%operate-on-table table key '%weak-alist-table-get-pair
    (lambda (p) p)
    (lambda () #f)))

(define (weak-alist-table-set! table key value)
  (%operate-on-table table key 'weak-alist-table-set!
    (lambda (p) (weak-set-cdr! p value))
    (lambda ()
      (%set-table-alist! table
			 (cons (weak-cons key value)
			       (%table-alist table))))))

(define (%weak-alist-table-add-pair! table p)
  (%set-table-alist! table (cons p (%table-alist table))))

(define (weak-alist-table-update! table key procedure #!optional get-default)
  (%operate-on-table table key 'weak-alist-table-update!
    (lambda (p) (weak-set-cdr! p (procedure (weak-cdr p))))
    (if (default-object? get-default)
	(lambda () (error:bad-range-argument key 'weak-alist-table-update!))
	(lambda ()
	  (%set-table-alist! table
			     (cons (weak-cons key (procedure (get-default)))
				   (%table-alist table)))))))

(define (weak-alist-table-intern! table key get-value)
  (%operate-on-table table key 'weak-alist-table-intern!
    weak-cdr
    (lambda ()
      (%set-table-alist! table
			 (cons (weak-cons key (get-value))
			       (%table-alist table))))))

(define-integrable (%operate-on-table table key caller if-found if-not-found)
  (let ((key= (%table-key= table)))
    (define-integrable (predicate key*)
      (key= key key*))
    (%weak-alist-table-search table predicate caller
      (lambda (key p) (declare (ignore key)) (if-found p))
      if-not-found)))

(define (weak-alist-table-search table predicate if-found if-not-found)
  (%weak-alist-table-search table predicate 'weak-alist-table-search
    (lambda (key p) (if-found key (weak-cdr p)))
    if-not-found))

(define (%weak-alist-table-search table predicate caller if-found if-not-found)
  (guarantee weak-alist-table? table caller)
  (let ((finalizer (%table-finalizer table)))
    (let loop ((this (%table-alist table)) (prev #f))
      (if (pair? this)
	  (let ((p (car this))
		(next (cdr this)))
	    (let ((key (weak-car p)))
	      (cond ((gc-reclaimed-object? key)
		     (if finalizer
			 (finalizer (weak-cdr p)))
		     (if prev
			 (set-cdr! prev next)
			 (%set-table-alist! table next))
		     (loop next prev))
		    ((predicate key) (if-found key p))
		    (else (loop next this)))))
	  (if-not-found)))))

(define (weak-alist-table-delete! table key #!optional default)
  (guarantee weak-alist-table? table 'weak-alist-table-delete!)
  (let ((key= (%table-key= table))
	(finalizer (%table-finalizer table)))
    (let loop ((this (%table-alist table)) (prev #f))
      (if (null-list? this 'weak-alist-table-delete!)
	  default
	  (let ((p (car this))
		(next (cdr this)))
	    (let ((key* (weak-car p)))
	      (cond ((gc-reclaimed-object? key*)
		     (if finalizer
			 (finalizer (weak-cdr p)))
		     (if prev
			 (set-cdr! prev next)
			 (%set-table-alist! table next))
		     (loop next prev))
		    ((key= key key*)
		     (if prev
			 (set-cdr! prev next)
			 (%set-table-alist! table next))
		     (weak-cdr p))
		    (else
		     (loop next this)))))))))

(define (weak-alist-table-delete-matching! table predicate)
  (guarantee weak-alist-table? table 'weak-alist-table-delete-matching!)
  (let ((finalizer (%table-finalizer table)))
    (let loop ((this (%table-alist table)) (prev #f))
      (if (not (null-list? this 'weak-alist-table-delete-matching!))
	  (let ((p (car this))
		(next (cdr this)))
	    (let ((key (weak-car p)))
	      (cond ((gc-reclaimed-object? key)
		     (if finalizer
			 (finalizer (weak-cdr p)))
		     (if prev
			 (set-cdr! prev next)
			 (%set-table-alist! table next))
		     (loop next prev))
		    ((predicate key (weak-cdr p))
		     (if prev
			 (set-cdr! prev next)
			 (%set-table-alist! table next))
		     (loop next prev))
		    (else
		     (loop next this)))))))))

(define (weak-alist-table-clean! table)
  (guarantee weak-alist-table? table 'weak-alist-table-clean!)
  (let ((finalizer (%table-finalizer table)))
    (let loop ((this (%table-alist table)) (prev #f))
      (if (not (null-list? this 'weak-alist-table-clean!))
	  (let ((p (car this))
		(next (cdr this)))
	    (if (gc-reclaimed-object? (weak-car p))
		(begin
		  (if finalizer
		      (finalizer (weak-cdr p)))
		  (if prev
		      (set-cdr! prev next)
		      (%set-table-alist! table next))
		  (loop next prev))
		(loop next this)))))))

(define (weak-alist-table-clear! table)
  (guarantee weak-alist-table? table 'weak-alist-table-clear!)
  (%set-table-alist! table '()))

;;;; Weak list sets

(define (weak-list-set = . items)
  (%record weak-list-set-tag = (delete-duplicates items =)))

(define (weak-list-set? object)
  (and (%record? object)
       (eq? weak-list-set-tag (%record-ref object 0))))
;
(define-integrable weak-list-set-tag
  '|#[(runtime weak-pair)weak-list-set]|)

(define-integrable (%set-predicate set) (%record-ref set 1))

(define-integrable (%set-items set) (%record-ref set 2))
(define-integrable (%set-set-items! set items) (%record-set! set 2 items))

(define (weak-list-set-predicate set)
  (guarantee weak-list-set? set 'weak-list-set-predicate)
  (%set-predicate set))

(define (weak-list-set-size set)
  (guarantee weak-list-set? set 'weak-list-set-size)
  (weak-length (%set-items set)))

(define (weak-list-set-empty? set)
  (guarantee weak-list-set? set 'weak-list-set-empty?)
  (weak-list-empty? (%set-items set)))

(define (weak-list-set-add! item set)
  (guarantee weak-list-set? set 'weak-list-set-add!)
  (let ((= (%set-predicate set))
	(items (%set-items set)))
    (let loop ((this items) (prev #f))
      (if (weak-pair? this)
	  (let ((item* (weak-car this))
		(next (weak-cdr this)))
	    (cond ((gc-reclaimed-object? item*)
		   (if prev
		       (weak-set-cdr! prev next)
		       (%set-set-items! set next))
		   (loop next prev))
		  ((= item item*)
		   #f)
		  (else
		   (loop next this))))
	  (begin
	    (%set-set-items! set (weak-cons item items))
	    #t)))))

(define (weak-list-set-delete! item set)
  (guarantee weak-list-set? set 'weak-list-set-delete!)
  (let ((= (%set-predicate set)))
    (let loop ((this (%set-items set)) (prev #f))
      (if (weak-pair? this)
	  (let ((item* (weak-car this))
		(next (weak-cdr this)))
	    (cond ((gc-reclaimed-object? item*)
		   (if prev
		       (weak-set-cdr! prev next)
		       (%set-set-items! set next))
		   (loop next prev))
		  ((= item item*)
		   (if prev
		       (weak-set-cdr! prev next)
		       (%set-set-items! set next))
		   #t)
		  (else
		   (loop next this))))
	  #f))))

(define (weak-list-set-delete-matching! set predicate)
  (guarantee weak-list-set? set 'weak-list-set-delete-matching!)
  (let loop ((this (%set-items set)) (prev #f))
    (if (weak-pair? this)
	(let ((item (weak-car this))
	      (next (weak-cdr this)))
	  (if (or (gc-reclaimed-object? item)
		  (predicate item))
	      (begin
		(if prev
		    (weak-set-cdr! prev next)
		    (%set-set-items! set next))
		(loop next prev))
	      (loop next this))))))

(define (weak-list-set-contains? item set)
  (guarantee weak-list-set? set)
  (generator-any (let ((= (%set-predicate set)))
		   (lambda (item*)
		     (= item item*)))
    (%weak-list-set->generator set 'weak-list-set-contains?)))

(define (weak-list-set-fold kons knil set)
  (generator-fold kons knil
    (%weak-list-set->generator set 'weak-list-set-fold)))

(define (weak-list-set-fold-right kons knil set)
  (generator-fold-right kons knil
    (%weak-list-set->generator set 'weak-list-set-fold-right)))

(define (weak-list-set-any predicate set)
  (generator-any predicate
    (%weak-list-set->generator set 'weak-list-set-any)))

(define (weak-list-set-every predicate set)
  (generator-every predicate
    (%weak-list-set->generator set 'weak-list-set-every)))

(define (weak-list-set->list set)
  (generator->list (%weak-list-set->generator set 'weak-list-set->list)))

(define (weak-list-set->generator set)
  (%weak-list-set->generator set 'weak-list-set->generator))

(define (%weak-list-set->generator set caller)
  (guarantee weak-list-set? set caller)
  (let ((this (%set-items set))
	(prev #f))
    (define (weak-list-set-generator)
      (if (weak-pair? this)
	  (let ((item (weak-car this))
		(next (weak-cdr this)))
	    (if (gc-reclaimed-object? item)
		(begin
		  (if prev
		      (set-cdr! prev next)
		      (%set-set-items! set next))
		  (set! this next)
		  (weak-list-set-generator))
		(begin
		  (set! prev this)
		  (set! this next)
		  item)))
	  (eof-object)))
    weak-list-set-generator))

(define (weak-list-set-for-each procedure set)
  (weak-list-set-delete-matching! set
    (lambda (item)
      (procedure item)
      #f)))

(define (weak-list-set-clean! set)
  (guarantee weak-list-set? set 'weak-list-set-clean!)
  (weak-list-set-delete-matching! set
    (lambda (item)
      (declare (ignore item))
      #f)))

(define (weak-list-set-clear! set)
  (guarantee weak-list-set? set 'weak-list-set-clear!)
  (%set-set-items! set '()))