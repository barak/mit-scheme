#| -*-Scheme-*-

$Id: list.scm,v 14.54 2007/01/05 21:19:28 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

;;;; List Operations
;;; package: (runtime list)

;;; Many list operations (like LIST-COPY and DELQ) have been replaced
;;; with iterative versions which are slightly longer than the
;;; recursive ones.  The iterative versions have the advantage that
;;; they are not limited by the stack size.  If you can execute
;;; (MAKE-LIST 100000) you should be able to process it.  Some
;;; machines have a problem with large stacks - Win32s has a max stack
;;; size of 128k.
;;;
;;; The disadvantage of the iterative versions is that side-effects are
;;; detectable in horrible ways with CALL-WITH-CURRENT-CONTINUATION.
;;; Due to this only those procedures which call procedures known NOT
;;; to use CALL-WITH-CURRENT-CONTINUATION can be written this way, so
;;; MAP is still recursive, but LIST-COPY is iterative.  The
;;; assumption is that any other way of grabbing the continuation
;;; (e.g. the threads package via a timer interrupt) will invoke the
;;; continuation at most once.
;;;
;;; We did some performance measurements.  The iterative versions were
;;; slightly faster.  These comparisons should be checked after major
;;; compiler work.
;;;
;;; Each interative version appears after the commented-out recursive
;;; version.  Please leave them in the file, we may want them in the
;;; future.  We have commented them out with ;; rather than block (i.e
;;; #||#) comments deliberately.  [Note from CPH: commented-out code
;;; deleted as it can always be recovered from version control.]
;;;
;;; -- Yael & Stephen

(declare (usual-integrations))

(define-primitives
  cons pair? null? car cdr set-car! set-cdr! general-car-cdr)

(define (list . items)
  items)

(define (cons* first-element . rest-elements)
  (let loop ((this-element first-element) (rest-elements rest-elements))
    (if (pair? rest-elements)
	(cons this-element
	      (loop (car rest-elements)
		    (cdr rest-elements)))
	this-element)))

(define (make-list length #!optional value)
  (guarantee-index-fixnum length 'MAKE-LIST)
  (let ((value (if (default-object? value) '() value)))
    (let loop ((n length) (result '()))
      (if (fix:zero? n)
	  result
	  (loop (fix:- n 1) (cons value result))))))

(define (circular-list . items)
  (if (pair? items)
      (let loop ((l items))
	(if (pair? (cdr l))
	    (loop (cdr l))
	    (set-cdr! l items))))
  items)

(define (make-circular-list length #!optional value)
  (guarantee-index-fixnum length 'MAKE-CIRCULAR-LIST)
  (if (fix:> length 0)
      (let ((value (if (default-object? value) '() value)))
	(let ((last (cons value '())))
	  (let loop ((n (fix:- length 1)) (result last))
	    (if (zero? n)
		(begin
		  (set-cdr! last result)
		  result)
		(loop (fix:- n 1) (cons value result))))))
      '()))

(define (make-initialized-list length initialization)
  (guarantee-index-fixnum length 'MAKE-INITIALIZED-LIST)
  (let loop ((index (fix:- length 1)) (result '()))
    (if (fix:< index 0)
	result
	(loop (fix:- index 1)
	      (cons (initialization index) result)))))

(define (xcons d a)
  (cons a d))

(define (iota count #!optional start step)
  (guarantee-index-fixnum count 'IOTA)
  (let ((start
	 (if (default-object? start)
	     0
	     (begin
	       (guarantee-number start 'IOTA)
	       start)))
	(step
	 (if (default-object? step)
	     1
	     (begin
	       (guarantee-number step 'IOTA)
	       step))))
    (let loop ((count count) (value start))
      (if (fix:> count 0)
	  (cons value (loop (fix:- count 1) (+ value step)))
	  '()))))

(define (list? object)
  (let loop ((l1 object) (l2 object))
    (if (pair? l1)
	(let ((l1 (cdr l1)))
	  (and (not (eq? l1 l2))
	       (if (pair? l1)
		   (loop (cdr l1) (cdr l2))
		   (null? l1))))
	(null? l1))))

(define (dotted-list? object)
  (let loop ((l1 object) (l2 object))
    (if (pair? l1)
	(let ((l1 (cdr l1)))
	  (and (not (eq? l1 l2))
	       (if (pair? l1)
		   (loop (cdr l1) (cdr l2))
		   (not (null? l1)))))
	(not (null? l1)))))

(define (circular-list? object)
  (let loop ((l1 object) (l2 object))
    (if (pair? l1)
	(let ((l1 (cdr l1)))
	  (if (eq? l1 l2)
	      #t
	      (if (pair? l1)
		  (loop (cdr l1) (cdr l2))
		  #f)))
	#f)))

(define-guarantee pair "pair")
(define-guarantee list "list")
(define-guarantee dotted-list "improper list")
(define-guarantee circular-list "circular list")

(define (list-of-type? object predicate)
  (let loop ((l1 object) (l2 object))
    (if (pair? l1)
	(and (predicate (car l1))
	     (let ((l1 (cdr l1)))
	       (and (not (eq? l1 l2))
		    (if (pair? l1)
			(and (predicate (car l1))
			     (loop (cdr l1) (cdr l2)))
			(null? l1)))))
	(null? l1))))

(define (guarantee-list-of-type object predicate description #!optional caller)
  (if (not (list-of-type? object predicate))
      (error:wrong-type-argument object
				 description
				 (if (default-object? caller) #f caller))))

(define (list?->length object)
  (let loop ((l1 object) (l2 object) (length 0))
    (if (pair? l1)
	(let ((l1 (cdr l1)))
	  (and (not (eq? l1 l2))
	       (if (pair? l1)
		   (loop (cdr l1) (cdr l2) (fix:+ length 2))
		   (and (null? l1)
			(fix:+ length 1)))))
	(and (null? l1)
	     length))))

(define (list-of-type?->length object predicate)
  (let loop ((l1 object) (l2 object) (length 0))
    (if (pair? l1)
	(and (predicate (car l1))
	     (let ((l1 (cdr l1)))
	       (and (not (eq? l1 l2))
		    (if (pair? l1)
			(and (predicate (car l1))
			     (loop (cdr l1) (cdr l2) (fix:+ length 2)))
			(and (null? l1)
			     (fix:+ length 1))))))
	(and (null? l1)
	     length))))

(define (guarantee-list->length object #!optional caller)
  (let ((n (list?->length object)))
    (if (not n)
	(error:not-list object caller))
    n))

(define (guarantee-list-of-type->length object predicate description
					#!optional caller)
  (let ((n (list-of-type?->length object predicate)))
    (if (not n)
	(error:wrong-type-argument object
				   description
				   (if (default-object? caller) #f caller)))
    n))

(define (length list)
  (guarantee-list->length list 'LENGTH))

(define (not-pair? x)
  (not (pair? x)))

(define (null-list? l #!optional caller)
  (cond ((pair? l) #f)
	((null? l) #t)
	(else (error:not-list l caller))))

(define (list= predicate . lists)

  (define (n-ary l1 l2 rest)
    (if (pair? rest)
	(and (binary l1 l2)
	     (n-ary l2 (car rest) (cdr rest)))
	(binary l1 l2)))

  (define (binary l1 l2)
    (cond ((pair? l1)
	   (cond ((eq? l1 l2) #t)
		 ((pair? l2)
		  (and (predicate (car l1) (car l2))
		       (binary (cdr l1) (cdr l2))))
		 ((null? l2) #f)
		 (else (lose))))
	  ((null? l1)
	   (cond ((null? l2) #t)
		 ((pair? l2) #f)
		 (else (lose))))
	  (else (lose))))

  (define (lose)
    (for-each (lambda (list)
		(guarantee-list list 'LIST=))
	      lists))

  (if (and (pair? lists)
	   (pair? (cdr lists)))
      (n-ary (car lists) (cadr lists) (cddr lists))
      #t))

(define (list-ref list index)
  (let ((tail (list-tail list index)))
    (if (not (pair? tail))
	(error:bad-range-argument index 'LIST-REF))
    (car tail)))

(define (list-tail list index)
  (guarantee-index-fixnum index 'LIST-TAIL)
  (let loop ((list list) (index* index))
    (if (fix:zero? index*)
	list
	(begin
	  (if (not (pair? list))
	      (error:bad-range-argument index 'LIST-TAIL))
	  (loop (cdr list) (fix:- index* 1))))))

(define (list-head list index)
  (guarantee-index-fixnum index 'LIST-HEAD)
  (let loop ((list list) (index* index))
    (if (fix:zero? index*)
	'()
	(begin
	  (if (not (pair? list))
	      (error:bad-range-argument index 'LIST-HEAD))
	  (cons (car list) (loop (cdr list) (fix:- index* 1)))))))

(define (sublist list start end)
  (list-head (list-tail list start) (- end start)))

(define (list-copy items)
  (let ((lose (lambda () (error:not-list items 'LIST-COPY))))
    (cond ((pair? items)
	   (let ((head (cons (car items) '())))
	     (let loop ((list (cdr items)) (previous head))
	       (cond ((pair? list)
		      (let ((new (cons (car list) '())))
			(set-cdr! previous new)
			(loop (cdr list) new)))
		     ((not (null? list)) (lose))))
	     head))
	  ((null? items) items)
	  (else (lose)))))

(define (tree-copy tree)
  (let walk ((tree tree))
    (if (pair? tree)
	(cons (walk (car tree)) (walk (cdr tree)))
	tree)))

(define (car+cdr pair)
  (values (car pair) (cdr pair)))

;;;; Weak Pairs

(define-integrable (weak-cons car cdr)
  (system-pair-cons (ucode-type weak-cons) (or car weak-pair/false) cdr))

(define-integrable (weak-pair? object)
  (object-type? (ucode-type weak-cons) object))

(define-integrable (weak-pair/car? weak-pair)
  (system-pair-car weak-pair))

(define (weak-car weak-pair)
  (let ((car (system-pair-car weak-pair)))
    (and (not (eq? car weak-pair/false))
	 car)))

(define-integrable (weak-set-car! weak-pair object)
  (system-pair-set-car! weak-pair (or object weak-pair/false)))

(define-integrable (weak-cdr weak-pair)
  (system-pair-cdr weak-pair))

(define-integrable (weak-set-cdr! weak-pair object)
  (system-pair-set-cdr! weak-pair object))

(define (weak-list->list items)
  (let loop ((items* items))
    (if (weak-pair? items*)
	(let ((car (system-pair-car items*)))
	  (if (not car)
	      (loop (system-pair-cdr items*))
	      (cons (if (eq? car weak-pair/false) #f car)
		    (loop (system-pair-cdr items*)))))
	(begin
	  (if (not (null? items*))
	      (error:not-weak-list items 'WEAK-LIST->LIST))
	  '()))))

(define (list->weak-list items)
  (let loop ((items* items))
    (if (pair? items*)
	(weak-cons (car items*) (loop (cdr items*)))
	(begin
	  (if (not (null? items*))
	      (error:not-list items 'LIST->WEAK-LIST))
	  '()))))

(define weak-pair/false
  "weak-pair/false")

(define (weak-list? object)
  (let loop ((l1 object) (l2 object))
    (if (weak-pair? l1)
	(let ((l1 (weak-cdr l1)))
	  (and (not (eq? l1 l2))
	       (if (weak-pair? l1)
		   (loop (weak-cdr l1) (weak-cdr l2))
		   (null? l1))))
	(null? l1))))

(define-guarantee weak-list "weak list")

(define (weak-memq object items)
  (let ((object (or object weak-pair/false)))
    (let loop ((items* items))
      (if (weak-pair? items*)
	  (if (eq? object (system-pair-car items*))
	      items*
	      (loop (system-pair-cdr items*)))
	  (begin
	    (if (not (null? items*))
		(error:not-weak-list items 'WEAK-MEMQ))
	    #f)))))

(define (weak-delq! item items)
  (letrec ((trim-initial-segment
	    (lambda (items*)
	      (if (weak-pair? items*)
		  (if (or (eq? item (system-pair-car items*))
			  (eq? #f (system-pair-car items*)))
		      (trim-initial-segment (system-pair-cdr items*))
		      (begin
			(locate-initial-segment items*
						(system-pair-cdr items*))
			items*))
		  (begin
		    (if (not (null? items*))
			(error:not-weak-list items 'WEAK-DELQ!))
		    '()))))
	   (locate-initial-segment
	    (lambda (last this)
	      (if (weak-pair? this)
		  (if (or (eq? item (system-pair-car this))
			  (eq? #f (system-pair-car this)))
		      (set-cdr! last
				(trim-initial-segment (system-pair-cdr this)))
		      (locate-initial-segment this (system-pair-cdr this)))
		  (if (not (null? this))
		      (error:not-weak-list items 'WEAK-DELQ!))))))
    (trim-initial-segment items)))

;;;; Standard Selectors

(declare (integrate-operator safe-car safe-cdr))

(define (safe-car x)
  (if (pair? x) (car x) (error:not-pair x 'SAFE-CAR)))

(define (safe-cdr x)
  (if (pair? x) (cdr x) (error:not-pair x 'SAFE-CDR)))

(define (caar x) (safe-car (safe-car x)))
(define (cadr x) (safe-car (safe-cdr x)))
(define (cdar x) (safe-cdr (safe-car x)))
(define (cddr x) (safe-cdr (safe-cdr x)))

(define (caaar x) (safe-car (safe-car (safe-car x))))
(define (caadr x) (safe-car (safe-car (safe-cdr x))))
(define (cadar x) (safe-car (safe-cdr (safe-car x))))
(define (caddr x) (safe-car (safe-cdr (safe-cdr x))))

(define (cdaar x) (safe-cdr (safe-car (safe-car x))))
(define (cdadr x) (safe-cdr (safe-car (safe-cdr x))))
(define (cddar x) (safe-cdr (safe-cdr (safe-car x))))
(define (cdddr x) (safe-cdr (safe-cdr (safe-cdr x))))

(define (caaaar x) (safe-car (safe-car (safe-car (safe-car x)))))
(define (caaadr x) (safe-car (safe-car (safe-car (safe-cdr x)))))
(define (caadar x) (safe-car (safe-car (safe-cdr (safe-car x)))))
(define (caaddr x) (safe-car (safe-car (safe-cdr (safe-cdr x)))))

(define (cadaar x) (safe-car (safe-cdr (safe-car (safe-car x)))))
(define (cadadr x) (safe-car (safe-cdr (safe-car (safe-cdr x)))))
(define (caddar x) (safe-car (safe-cdr (safe-cdr (safe-car x)))))
(define (cadddr x) (safe-car (safe-cdr (safe-cdr (safe-cdr x)))))

(define (cdaaar x) (safe-cdr (safe-car (safe-car (safe-car x)))))
(define (cdaadr x) (safe-cdr (safe-car (safe-car (safe-cdr x)))))
(define (cdadar x) (safe-cdr (safe-car (safe-cdr (safe-car x)))))
(define (cdaddr x) (safe-cdr (safe-car (safe-cdr (safe-cdr x)))))

(define (cddaar x) (safe-cdr (safe-cdr (safe-car (safe-car x)))))
(define (cddadr x) (safe-cdr (safe-cdr (safe-car (safe-cdr x)))))
(define (cdddar x) (safe-cdr (safe-cdr (safe-cdr (safe-car x)))))
(define (cddddr x) (safe-cdr (safe-cdr (safe-cdr (safe-cdr x)))))

(define (first x) (safe-car x))
(define (second x) (safe-car (safe-cdr x)))
(define (third x) (safe-car (safe-cdr (safe-cdr x))))
(define (fourth x) (safe-car (safe-cdr (safe-cdr (safe-cdr x)))))
(define (fifth x) (safe-car (safe-cdr (safe-cdr (safe-cdr (safe-cdr x))))))

(define (sixth x)
  (safe-car (safe-cdr (safe-cdr (safe-cdr (safe-cdr (safe-cdr x)))))))

(define (seventh x)
  (safe-car
   (safe-cdr (safe-cdr (safe-cdr (safe-cdr (safe-cdr (safe-cdr x))))))))

(define (eighth x)
  (safe-car
   (safe-cdr
    (safe-cdr (safe-cdr (safe-cdr (safe-cdr (safe-cdr (safe-cdr x)))))))))

(define (ninth x)
  (safe-car
   (safe-cdr
    (safe-cdr
     (safe-cdr (safe-cdr (safe-cdr (safe-cdr (safe-cdr (safe-cdr x))))))))))

(define (tenth x)
  (safe-car
   (safe-cdr
    (safe-cdr
     (safe-cdr
      (safe-cdr (safe-cdr (safe-cdr (safe-cdr (safe-cdr (safe-cdr x)))))))))))

;;;; Sequence Operations

;;; This algorithm uses a finite amount of stack and therefore half
;;; the memory of the simple recursive algorithm.  In addition, a
;;; clever compiler could optimize this into the obvious loop that
;;; everyone would write in assembly language.

(define (append . lists) (%append lists))
(define (append! . lists) (%append! lists))

(define (%append lists)
  (let ((lists (reverse! lists)))
    (if (pair? lists)
	(let loop ((accum (car lists)) (rest (cdr lists)))
	  (if (pair? rest)
	      (loop (let ((l1 (car rest)))
		      (cond ((pair? l1)
			     (let ((root (cons (car l1) #f)))
			       (let loop ((cell root) (next (cdr l1)))
				 (cond ((pair? next)
					(let ((cell* (cons (car next) #f)))
					  (set-cdr! cell cell*)
					  (loop cell* (cdr next))))
				       ((null? next)
					(set-cdr! cell accum))
				       (else
					(error:not-list (car rest) 'APPEND))))
			       root))
			    ((null? l1)
			     accum)
			    (else
			     (error:not-list (car rest) 'APPEND))))
		    (cdr rest))
	      accum))
	'())))

(define (%append! lists)
  (if (pair? lists)
      (let loop ((head (car lists)) (tail (cdr lists)))
	(cond ((not (pair? tail))
	       head)
	      ((pair? head)
	       (set-cdr! (last-pair head) (loop (car tail) (cdr tail)))
	       head)
	      (else
	       (if (not (null? head))
		   (error:not-list (car lists) 'APPEND!))
	       (loop (car tail) (cdr tail)))))
      '()))

(define (reverse l) (reverse* l '()))
(define (reverse! l) (reverse*! l '()))

(define (reverse* l tail)
  (let loop ((rest l) (so-far tail))
    (if (pair? rest)
	(loop (cdr rest) (cons (car rest) so-far))
	(begin
	  (if (not (null? rest))
	      (error:not-list l 'REVERSE*))
	  so-far))))

(define (reverse*! l tail)
  (let loop ((current l) (new-cdr tail))
    (if (pair? current)
	(let ((next (cdr current)))
	  (set-cdr! current new-cdr)
	  (loop next current))
	(begin
	  (if (not (null? current))
	      (error:not-list l 'REVERSE*!))
	  new-cdr))))

;;;; Mapping Procedures

(define (map procedure first . rest)

  (define (map-1 l)
    (if (pair? l)
	(let ((head (cons (procedure (car l)) '())))
	  (let loop ((l (cdr l)) (previous head))
	    (if (pair? l)
		(let ((new (cons (procedure (car l)) '())))
		  (set-cdr! previous new)
		  (loop (cdr l) new))
		(if (not (null? l))
		    (bad-end))))
	  head)
	(begin
	  (if (not (null? l))
	      (bad-end))
	  '())))

  (define (map-2 l1 l2)
    (if (and (pair? l1) (pair? l2))
	(let ((head (cons (procedure (car l1) (car l2)) '())))
	  (let loop ((l1 (cdr l1)) (l2 (cdr l2)) (previous head))
	    (if (and (pair? l1) (pair? l2))
		(let ((new (cons (procedure (car l1) (car l2)) '())))
		  (set-cdr! previous new)
		  (loop (cdr l1) (cdr l2) new))
		(if (not (and (or (null? l1) (pair? l1))
			      (or (null? l2) (pair? l2))))
		    (bad-end))))
	  head)
	(begin
	  (if (not (and (or (null? l1) (pair? l1))
			(or (null? l2) (pair? l2))))
	      (bad-end))
	  '())))

  (define (map-n lists)
    (let ((head (cons unspecific '())))
      (let loop ((lists lists) (previous head))
	(let split ((lists lists) (cars '()) (cdrs '()))
	  (if (pair? lists)
	      (if (pair? (car lists))
		  (split (cdr lists)
			 (cons (caar lists) cars)
			 (cons (cdar lists) cdrs))
		  (if (not (null? (car lists)))
		      (bad-end)))
	      (let ((new (cons (apply procedure (reverse! cars)) '())))
		(set-cdr! previous new)
		(loop (reverse! cdrs) new)))))
      (cdr head)))

  (define (bad-end)
    (mapper-error (cons first rest) 'MAP))

  (if (pair? rest)
      (if (pair? (cdr rest))
	  (map-n (cons first rest))
	  (map-2 first (car rest)))
      (map-1 first)))

(define (mapper-error lists caller)
  (for-each (lambda (list)
	      (if (dotted-list? list)
		  (error:not-list list caller)))
	    lists))

(define for-each)
(define map*)
(define append-map)
(define append-map*)
(define append-map!)
(define append-map*!)

(let-syntax
    ((mapper
      (rsc-macro-transformer
       (lambda (form environment)
	 environment
	 (let ((name (list-ref form 1))
	       (extra-vars (list-ref form 2))
	       (combiner (list-ref form 3))
	       (initial-value (list-ref form 4)))
	   `(SET! ,name
		  (NAMED-LAMBDA (,name ,@extra-vars PROCEDURE FIRST . REST)

		    (DEFINE (MAP-1 L)
		      (IF (PAIR? L)
			  (,combiner (PROCEDURE (CAR L))
				     (MAP-1 (CDR L)))
			  (BEGIN
			    (IF (NOT (NULL? L))
				(BAD-END))
			    ,initial-value)))

		    (DEFINE (MAP-2 L1 L2)
		      (IF (AND (PAIR? L1) (PAIR? L2))
			  (,combiner (PROCEDURE (CAR L1) (CAR L2))
				     (MAP-2 (CDR L1) (CDR L2)))
			  (BEGIN
			    (IF (NOT (AND (OR (NULL? L1) (PAIR? L1))
					  (OR (NULL? L2) (PAIR? L2))))
				(BAD-END))
			    ,initial-value)))

		    (DEFINE (MAP-N LISTS)
		      (LET SPLIT ((LISTS LISTS) (CARS '()) (CDRS '()))
			(IF (PAIR? LISTS)
			    (IF (PAIR? (CAR LISTS))
				(SPLIT (CDR LISTS)
				       (CONS (CAAR LISTS) CARS)
				       (CONS (CDAR LISTS) CDRS))
				(BEGIN
				  (IF (NOT (NULL? (CAR LISTS)))
				      (BAD-END))
				  ,initial-value))
			    (,combiner (APPLY PROCEDURE (REVERSE! CARS))
				       (MAP-N (REVERSE! CDRS))))))

		    (DEFINE (BAD-END)
		      (MAPPER-ERROR (CONS FIRST REST) ',name))

		    (IF (PAIR? REST)
			(IF (PAIR? (CDR REST))
			    (MAP-N (CONS FIRST REST))
			    (MAP-2 FIRST (CAR REST)))
			(MAP-1 FIRST)))))))))

  (mapper for-each () begin unspecific)
  (mapper map* (initial-value) cons initial-value)
  (mapper append-map () append '())
  (mapper append-map* (initial-value) append initial-value)
  (mapper append-map! () append! '())
  (mapper append-map*! (initial-value) append! initial-value))

(define (reduce procedure initial list)
  (if (pair? list)
      (%fold-1 procedure (car list) (cdr list) 'REDUCE)
      (begin
	(if (not (null? list))
	    (error:not-list list 'REDUCE))
	initial)))

(define (reduce-right procedure initial list)
  (if (pair? list)
      (let loop ((first (car list)) (rest (cdr list)))
	(if (pair? rest)
	    (procedure first (loop (car rest) (cdr rest)))
	    (begin
	      (if (not (null? rest))
		  (error:not-list list 'REDUCE-RIGHT))
	      first)))
      (begin
	(if (not (null? list))
	    (error:not-list list 'REDUCE-RIGHT))
	initial)))

(define (fold procedure initial first . rest)
  (if (pair? rest)
      (let loop ((lists (cons first rest)) (value initial))
	(let split ((lists lists) (cars '()) (cdrs '()))
	  (if (pair? lists)
	      (if (pair? (car lists))
		  (split (cdr lists)
			 (cons (caar lists) cars)
			 (cons (cdar lists) cdrs))
		  (begin
		    (if (not (null? (car lists)))
			(mapper-error (cons first rest) 'FOLD))
		    value))
	      (loop (reverse! cdrs)
		    (apply procedure (reverse! (cons value cars)))))))
      (%fold-1 procedure initial first 'FOLD)))

(define (%fold-1 procedure initial list caller)
  (let loop ((value initial) (list* list))
    (if (pair? list*)
	(loop (procedure (car list*) value)
	      (cdr list*))
	(begin
	  (if (not (null? list*))
	      (error:not-list list caller))
	  value))))

(define (fold-left procedure initial list)
  (%fold-1 (lambda (a b) (procedure b a)) initial list 'FOLD-LEFT))

(define (fold-right procedure initial first . rest)
  (if (pair? rest)
      (let loop ((lists (cons first rest)))
	(let split ((lists lists) (cars '()) (cdrs '()))
	  (if (pair? lists)
	      (if (pair? (car lists))
		  (split (cdr lists)
			 (cons (caar lists) cars)
			 (cons (cdar lists) cdrs))
		  (begin
		    (if (not (null? (car lists)))
			(mapper-error (cons first rest) 'FOLD-RIGHT))
		    initial))
	      (apply procedure
		     (reverse! (cons (loop (reverse! cdrs)) cars))))))
      (let loop ((list first))
	(if (pair? list)
	    (procedure (car list) (loop (cdr list)))
	    (begin
	      (if (not (null? list))
		  (error:not-list first 'FOLD-RIGHT))
	      initial)))))

;;;; Generalized list operations

(define (find-matching-item items predicate)
  (let loop ((items* items))
    (if (pair? items*)
	(if (predicate (car items*))
	    (car items*)
	    (loop (cdr items*)))
	(begin
	  (if (not (null? items*))
	      (error:not-list items 'FIND-MATCHING-ITEM))
	  #f))))

(define (find-non-matching-item items predicate)
  (let loop ((items* items))
    (if (pair? items*)
	(if (predicate (car items*))
	    (loop (cdr items*))
	    (car items*))
	(begin
	  (if (not (null? items*))
	      (error:not-list items 'FIND-MATCHING-ITEM))
	  #f))))

(define (find-unique-matching-item items predicate)
  (let loop ((items* items))
    (if (pair? items*)
	(if (predicate (car items*))
	    (if (there-exists? (cdr items*) predicate)
		#f
		(car items*))
	    (loop (cdr items*)))
	(begin
	  (if (not (null? items*))
	      (error:not-list items 'FIND-UNIQUE-MATCHING-ITEM))
	  #f))))

(define (find-unique-non-matching-item items predicate)
  (let loop ((items* items))
    (if (pair? items*)
	(if (predicate (car items*))
	    (loop (cdr items*))
	    (if (for-all? (cdr items*) predicate)
		(car items*)
		#f))
	(begin
	  (if (not (null? items*))
	      (error:not-list items 'FIND-UNIQUE-NON-MATCHING-ITEM))
	  #f))))

(define (count-matching-items items predicate)
  (do ((items* items (cdr items*))
       (n 0 (if (predicate (car items*)) (fix:+ n 1) n)))
      ((not (pair? items*))
       (if (not (null? items*))
	   (error:not-list items 'COUNT-MATCHING-ITEMS))
       n)))

(define (count-non-matching-items items predicate)
  (do ((items* items (cdr items*))
       (n 0 (if (predicate (car items*)) n (fix:+ n 1))))
      ((not (pair? items*))
       (if (not (null? items*))
	   (error:not-list items 'COUNT-NON-MATCHING-ITEMS))
       n)))

(define (keep-matching-items items predicate)
  (let ((lose (lambda () (error:not-list items 'KEEP-MATCHING-ITEMS))))
    (cond ((pair? items)
	   (let ((head (cons (car items) '())))
	     (let loop ((items* (cdr items)) (previous head))
	       (cond ((pair? items*)
		      (if (predicate (car items*))
			  (let ((new (cons (car items*) '())))
			    (set-cdr! previous new)
			    (loop (cdr items*) new))
			  (loop (cdr items*) previous)))
		     ((not (null? items*)) (lose))))
	     (if (predicate (car items))
		 head
		 (cdr head))))
	  ((null? items) items)
	  (else (lose)))))

(define (delete-matching-items items predicate)
  (let ((lose (lambda () (error:not-list items 'DELETE-MATCHING-ITEMS))))
    (cond ((pair? items)
	   (let ((head (cons (car items) '())))
	     (let loop ((items* (cdr items)) (previous head))
	       (cond ((pair? items*)
		      (if (predicate (car items*))
			  (loop (cdr items*) previous)
			  (let ((new (cons (car items*) '())))
			    (set-cdr! previous new)
			    (loop (cdr items*) new))))
		     ((not (null? items*)) (lose))))
	     (if (predicate (car items))
		 (cdr head)
		 head)))
	  ((null? items) items)
	  (else (lose)))))

(define (delete-matching-items! items predicate)
  (letrec
      ((trim-initial-segment
	(lambda (items*)
	  (if (pair? items*)
	      (if (predicate (car items*))
		  (trim-initial-segment (cdr items*))
		  (begin
		    (locate-initial-segment items* (cdr items*))
		    items*))
	      (begin
		(if (not (null? items*))
		    (lose))
		'()))))
       (locate-initial-segment
	(lambda (last this)
	  (if (pair? this)
	      (if (predicate (car this))
		  (set-cdr! last (trim-initial-segment (cdr this)))
		  (locate-initial-segment this (cdr this)))
	      (if (not (null? this))
		  (lose)))))
       (lose
	(lambda ()
	  (error:not-list items 'DELETE-MATCHING-ITEMS!))))
    (trim-initial-segment items)))

(define (keep-matching-items! items predicate)
  (letrec
      ((trim-initial-segment
	(lambda (items*)
	  (if (pair? items*)
	      (if (predicate (car items*))
		  (begin
		    (locate-initial-segment items* (cdr items*))
		    items*)
		  (trim-initial-segment (cdr items*)))
	      (begin
		(if (not (null? items*))
		    (lose))
		'()))))
       (locate-initial-segment
	(lambda (last this)
	  (if (pair? this)
	      (if (predicate (car this))
		  (locate-initial-segment this (cdr this))
		  (set-cdr! last (trim-initial-segment (cdr this))))
	      (if (not (null? this))
		  (lose)))))
       (lose
	(lambda ()
	  (error:not-list items 'KEEP-MATCHING-ITEMS!))))
    (trim-initial-segment items)))

(define ((list-deletor predicate) items)
  (delete-matching-items items predicate))

(define ((list-deletor! predicate) items)
  (delete-matching-items! items predicate))

;;;; Membership lists

(define (memq item items)
  (%member item items eq? 'MEMQ))

(define (memv item items)
  (%member item items eqv? 'MEMV))

(define (member item items #!optional =)
  (let ((= (if (default-object? =) equal? =)))
    (%member item items = 'MEMBER)))

(define (member-procedure = #!optional caller)
  (lambda (item items)
    (%member item items = caller)))

(define (add-member-procedure = #!optional caller)
  (lambda (item items)
    (if (%member item items = caller)
	items
	(cons item items))))

(define-integrable (%member item items = caller)
  (let ((lose (lambda () (error:not-list items caller))))
    (let loop ((items items))
      (if (pair? items)
	  (if (= (car items) item)
	      items
	      (loop (cdr items)))
	  (begin
	    (if (not (null? items))
		(lose))
	    #f)))))

(define ((delete-member-procedure deletor predicate) item items)
  ((deletor (lambda (match) (predicate match item))) items))

(define (delq item items)
  (%delete item items eq? 'DELQ))

(define (delv item items)
  (%delete item items eqv? 'DELQ))

(define (delete item items #!optional =)
  (let ((= (if (default-object? =) equal? =)))
    (%delete item items = 'DELETE)))

(define-integrable (%delete item items = caller)
  (let ((lose (lambda () (error:not-list items caller))))
    (if (pair? items)
	(let ((head (cons (car items) '())))
	  (let loop ((items (cdr items)) (previous head))
	    (cond ((pair? items)
		   (if (= (car items) item)
		       (loop (cdr items) previous)
		       (let ((new (cons (car items) '())))
			 (set-cdr! previous new)
			 (loop (cdr items) new))))
		  ((not (null? items))
		   (lose))))
	  (if (= (car items) item)
	      (cdr head)
	      head))
	(begin
	  (if (not (null? items))
	      (lose))
	  items))))

(define (delq! item items)
  (%delete! item items eq? 'DELQ!))

(define (delv! item items)
  (%delete! item items eqv? 'DELV!))

(define (delete! item items #!optional =)
  (let ((= (if (default-object? =) equal? =)))
    (%delete! item items = 'DELETE!)))

(define-integrable (%delete! item items = caller)
  (letrec
      ((trim-initial-segment
	(lambda (items)
	  (if (pair? items)
	      (if (= item (car items))
		  (trim-initial-segment (cdr items))
		  (begin
		    (locate-initial-segment items (cdr items))
		    items))
	      (begin
		(if (not (null? items))
		    (lose))
		'()))))
       (locate-initial-segment
	(lambda (last this)
	  (if (pair? this)
	      (if (= item (car this))
		  (set-cdr! last
			    (trim-initial-segment (cdr this)))
		  (locate-initial-segment this (cdr this)))
	      (if (not (null? this))
		  (error:not-list items caller)))))
       (lose
	(lambda ()
	  (error:not-list items caller))))
    (trim-initial-segment items)))

;;;; Association lists

(define (alist? object)
  (list-of-type? object pair?))

(define-guarantee alist "association list")

(define (alist-cons key datum alist)
  (cons (cons key datum) alist))

(define (alist-copy alist)
  (let ((lose (lambda () (error:not-alist alist 'ALIST-COPY))))
    (cond ((pair? alist)
	   (if (pair? (car alist))
	       (let ((head (cons (car alist) '())))
		 (let loop ((alist (cdr alist)) (previous head))
		   (cond ((pair? alist)
			  (if (pair? (car alist))
			      (let ((new
				     (cons (cons (caar alist) (cdar alist))
					   '())))
				(set-cdr! previous new)
				(loop (cdr alist) new))
			      (lose)))
			 ((not (null? alist)) (lose))))
		 head)
	       (lose)))
	  ((null? alist) alist)
	  (else (lose)))))

(define (association-procedure predicate selector #!optional caller)
  (lambda (key items)
    (let ((lose (lambda () (error:not-list items caller))))
      (let loop ((items items))
	(if (pair? items)
	    (if (predicate (selector (car items)) key)
		(car items)
		(loop (cdr items)))
	    (begin
	      (if (not (null? items))
		  (lose))
	      #f))))))

(define ((delete-association-procedure deletor predicate selector) key alist)
  ((deletor (lambda (entry) (predicate (selector entry) key))) alist))

(define (assq key alist)
  (%assoc key alist eq? 'ASSQ))

(define (assv key alist)
  (%assoc key alist eqv? 'ASSV))

(define (assoc key alist #!optional =)
  (let ((= (if (default-object? =) equal? =)))
    (%assoc key alist = 'ASSOC)))

(define-integrable (%assoc key alist = caller)
  (let ((lose (lambda () (error:not-alist alist caller))))
    (let loop ((alist alist))
      (if (pair? alist)
	  (begin
	    (if (not (pair? (car alist)))
		(lose))
	    (if (= (caar alist) key)
		(car alist)
		(loop (cdr alist))))
	  (begin
	    (if (not (null? alist))
		(lose))
	    #f)))))

(define (del-assq key alist)
  (%alist-delete key alist eq? 'DEL-ASSQ))

(define (del-assv key alist)
  (%alist-delete key alist eqv? 'DEL-ASSV))

(define (del-assoc key alist)
  (%alist-delete key alist equal? 'DEL-ASSOC))

(define (alist-delete key alist #!optional =)
  (let ((= (if (default-object? =) equal? =)))
    (%alist-delete key alist = 'ALIST-DELETE)))

(define-integrable (%alist-delete key alist = caller)
  (let ((lose (lambda () (error:not-alist alist caller))))
    (if (pair? alist)
	(begin
	  (if (not (pair? (car alist)))
	      (lose))
	  (let ((head (cons (car alist) '())))
	    (let loop ((alist (cdr alist)) (previous head))
	      (cond ((pair? alist)
		     (if (not (pair? (car alist)))
			 (lose))
		     (if (= (caar alist) key)
			 (loop (cdr alist) previous)
			 (let ((new (cons (car alist) '())))
			   (set-cdr! previous new)
			   (loop (cdr alist) new))))
		    ((not (null? alist))
		     (lose))))
	    (if (= (caar alist) key)
		(cdr head)
		head)))
	(begin
	  (if (not (null? alist))
	      (lose))
	  alist))))

(define (del-assq! key alist)
  (%alist-delete! key alist eq? 'DEL-ASSQ!))

(define (del-assv! key alist)
  (%alist-delete! key alist eqv? 'DEL-ASSV!))

(define (del-assoc! key alist)
  (%alist-delete! key alist equal? 'DEL-ASSOC!))

(define (alist-delete! key alist #!optional =)
  (let ((= (if (default-object? =) equal? =)))
    (%alist-delete! key alist = 'ALIST-DELETE!)))

(define-integrable (%alist-delete! item items = caller)
  (letrec
      ((trim-initial-segment
	(lambda (items)
	  (if (pair? items)
	      (begin
		(if (not (pair? (car items)))
		    (lose))
		(if (= (caar items) item)
		    (trim-initial-segment (cdr items))
		    (begin
		      (locate-initial-segment items (cdr items))
		      items)))
	      (begin
		(if (not (null? items))
		    (lose))
		'()))))
       (locate-initial-segment
	(lambda (last this)
	  (cond ((pair? this)
		 (if (not (pair? (car this)))
		     (lose))
		 (if (= (caar this) item)
		     (set-cdr!
		      last
		      (trim-initial-segment (cdr this)))
		     (locate-initial-segment this (cdr this))))
		((not (null? this))
		 (lose)))))
       (lose
	(lambda ()
	  (error:not-alist items caller))))
    (trim-initial-segment items)))

;;;; Keyword lists

(define (keyword-list? object)
  (let loop ((l1 object) (l2 object))
    (if (pair? l1)
	(and (symbol? (car l1))
	     (pair? (cdr l1))
	     (not (eq? (cdr l1) l2))
	     (loop (cdr (cdr l1)) (cdr l1)))
	(null? l1))))

(define-guarantee keyword-list "keyword list")

(define (restricted-keyword-list? object keywords)
  (let loop ((l1 object) (l2 object))
    (if (pair? l1)
	(and (memq (car l1) keywords)
	     (pair? (cdr l1))
	     (not (eq? (cdr l1) l2))
	     (loop (cdr (cdr l1)) (cdr l1)))
	(null? l1))))

(define (guarantee-restricted-keyword-list object keywords #!optional caller)
  (if (not (restricted-keyword-list? object keywords))
      (error:not-restricted-keyword-list object caller)))

(define (error:not-restricted-keyword-list object #!optional caller)
  (error:wrong-type-argument object
			     "restricted keyword list"
			     (if (default-object? caller) #f caller)))

(define (unique-keyword-list? object)
  (let loop ((l1 object) (l2 object) (symbols '()))
    (if (pair? l1)
	(and (symbol? (car l1))
	     (not (memq (car l1) symbols))
	     (pair? (cdr l1))
	     (not (eq? (cdr l1) l2))
	     (loop (cdr (cdr l1)) (cdr l1) (cons (car l1) symbols)))
	(null? l1))))

(define-guarantee unique-keyword-list "unique keyword list")

(define (get-keyword-value klist key)
  (let ((lose (lambda () (error:not-keyword-list klist 'GET-KEYWORD-VALUE))))
    (let loop ((klist klist))
      (if (pair? klist)
	  (begin
	    (if (not (pair? (cdr klist)))
		(lose))
	    (if (eq? (car klist) key)
		(cadr klist)
		(loop (cddr klist))))
	  (begin
	    (if (not (null? klist))
		(lose))
	    #!default)))))

(define (keyword-list->alist klist)
  (let loop ((klist klist))
    (if (pair? klist)
	(cons (cons (car klist) (cadr klist))
	      (loop (cddr klist)))
	'())))

(define (alist->keyword-list alist)
  (let loop ((alist alist))
    (if (pair? alist)
	(cons (caar alist)
	      (cons (cdar alist)
		    (loop (cdr alist))))
	'())))

;;;; Last pair

(define (last list)
  (car (last-pair list)))

(define (last-pair list)
  (guarantee-pair list 'LAST-PAIR)
  (let loop ((list list))
    (if (pair? (cdr list))
	(loop (cdr list))
	list)))

(define (except-last-pair list)
  (guarantee-pair list 'EXCEPT-LAST-PAIR)
  (if (not (pair? (cdr list)))
      '()
      (let ((head (cons (car list) '())))
	(let loop ((list (cdr list)) (previous head))
	  (if (pair? (cdr list))
	      (let ((new (cons (car list) '())))
		(set-cdr! previous new)
		(loop (cdr list) new))
	      head)))))

(define (except-last-pair! list)
  (guarantee-pair list 'EXCEPT-LAST-PAIR!)
  (if (pair? (cdr list))
      (begin
	(let loop ((list list))
	  (if (pair? (cdr (cdr list)))
	      (loop (cdr list))
	      (set-cdr! list '())))
	list)
      '()))