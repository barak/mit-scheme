#| -*-Scheme-*-

$Id: list.scm,v 14.37 2003/04/25 03:31:49 cph Exp $

Copyright 1986,1987,1988,1989,1990,1991 Massachusetts Institute of Technology
Copyright 1992,1993,1994,1995,1996,2000 Massachusetts Institute of Technology
Copyright 2001,2002,2003 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; List Operations
;;; package: (runtime list)

;;; Many list operations (like LIST-COPY and DELQ) have been replaced
;;; with iterative versions which are slightly longer than the
;;; recursive ones.  The iterative versions have the advantage that
;;; they are not limited by the stack size.  If you can execute
;;; (MAKE-LIST 100000) you should be able to process it.  Some
;;; machines have a problem with large stacks - Win32s as a max stack
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
  (if (not (fix:zero? length))
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
  (let loop ((index (- length 1)) (result '()))
    (if (negative? index)
	result
	(loop (- index 1)
	      (cons (initialization index) result)))))

(define (list? object)
  (let loop ((l1 object) (l2 object))
    (if (pair? l1)
	(let ((l1 (cdr l1)))
	  (and (not (eq? l1 l2))
	       (if (pair? l1)
		   (loop (cdr l1) (cdr l2))
		   (null? l1))))
	(null? l1))))

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

(define (guarantee-list object caller)
  (if (not (list? object))
      (error:wrong-type-argument object "list" caller)))

(define (guarantee-list-of-type object predicate description caller)
  (if (not (list-of-type? object predicate))
      (error:wrong-type-argument object description caller)))

(define (alist? object)
  (list-of-type? object pair?))

(define (guarantee-alist object caller)
  (guarantee-list-of-type object pair? "association list" caller))

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

(define (guarantee-list->length object caller)
  (let ((n (list?->length object)))
    (if (not n)
	(error:wrong-type-argument object "list" caller))
    n))

(define (guarantee-list-of-type->length object predicate description caller)
  (let ((n (list-of-type?->length object predicate)))
    (if (not n)
	(error:wrong-type-argument object description caller))
    n))

(define (length list)
  (guarantee-list->length list 'LENGTH))

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
  (let ((lose (lambda () (error:wrong-type-argument items "list" 'LIST-COPY))))
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

(define (alist-copy alist)
  (let ((lose
	 (lambda () (error:wrong-type-argument alist "alist" 'ALIST-COPY))))
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

(define (tree-copy tree)
  (let walk ((tree tree))
    (if (pair? tree)
	(cons (walk (car tree)) (walk (cdr tree)))
	tree)))

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
	      (error:wrong-type-argument items "weak list" 'WEAK-LIST->LIST))
	  '()))))

(define (list->weak-list items)
  (let loop ((items* items))
    (if (pair? items*)
	(weak-cons (car items*) (loop (cdr items*)))
	(begin
	  (if (not (null? items*))
	      (error:wrong-type-argument items "list" 'LIST->WEAK-LIST))
	  '()))))

(define weak-pair/false
  "weak-pair/false")

(define (weak-memq object items)
  (let ((object (or object weak-pair/false)))
    (let loop ((items* items))
      (if (weak-pair? items*)
	  (if (eq? object (system-pair-car items*))
	      items*
	      (loop (system-pair-cdr items*)))
	  (begin
	    (if (not (null? items*))
		(error:wrong-type-argument items "weak list" 'WEAK-MEMQ))
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
			(error:wrong-type-argument items "weak list"
						   'WEAK-MEMQ))
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
		      (error:wrong-type-argument items "weak list"
						 'WEAK-MEMQ))))))
    (trim-initial-segment items)))

;;;; Standard Selectors

(declare (integrate-operator safe-car safe-cdr))

(define (safe-car x)
  (if (pair? x) (car x) (error:not-a-pair x)))

(define (safe-cdr x)
  (if (pair? x) (cdr x) (error:not-a-pair x)))

(define (error:not-a-pair x)
  (error:wrong-type-argument x "pair" #f))

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

(define (append . lists)
  (%append lists))

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
					(error:wrong-type-argument (car rest)
								   "list"
								   'APPEND))))
			       root))
			    ((null? l1)
			     accum)
			    (else
			     (error:wrong-type-argument (car rest) "list"
							'APPEND))))
		    (cdr rest))
	      accum))
	'())))

(define (append! . lists)
  (%append! lists))

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
		   (error:wrong-type-argument (car lists) "list" 'APPEND!))
	       (loop (car tail) (cdr tail)))))
      '()))

(define (reverse l)
  (%reverse l '()))

(define (%reverse l tail)
  (let loop ((rest l) (so-far tail))
    (if (pair? rest)
	(loop (cdr rest) (cons (car rest) so-far))
	(begin
	  (if (not (null? rest))
	      (error:wrong-type-argument l "list" '%REVERSE))
	  so-far))))

(define (reverse! l)
  (let loop ((current l) (new-cdr '()))
    (if (pair? current)
	(let ((next (cdr current)))
	  (set-cdr! current new-cdr)
	  (loop next current))
	(begin
	  (if (not (null? current))
	      (error:wrong-type-argument l "list" 'REVERSE!))
	  new-cdr))))

;;;; Mapping Procedures

(define (map procedure first . rest)

  (define (map-1 l)
    (cond ((pair? l)
	   (let ((head (cons (procedure (car l)) '())))
	     (let loop ((l (cdr l)) (previous head))
	       (cond ((pair? l)
		      (let ((new (cons (procedure (car l)) '())))
			(set-cdr! previous new)
			(loop (cdr l) new)))
		     ((not (null? l))
		      (bad-end))))
	     head))
	  ((null? l) '())
	  (else (bad-end))))

  (define (map-2 l1 l2)
    (cond ((and (pair? l1) (pair? l2))
	   (let ((head (cons (procedure (car l1) (car l2)) '())))
	     (let loop ((l1 (cdr l1)) (l2 (cdr l2)) (previous head))
	       (cond ((and (pair? l1) (pair? l2))
		      (let ((new (cons (procedure (car l1) (car l2)) '())))
			(set-cdr! previous new)
			(loop (cdr l1) (cdr l2) new)))
		     ((not (and (null? l1) (null? l2)))
		      (bad-end))))
	     head))
	  ((and (null? l1) (null? l2)) '())
	  (else (bad-end))))

  (define (map-n lists)
    (let ((head (cons unspecific '())))
      (let loop ((lists lists) (previous head))
	(if (pair? (car lists))
	    (do ((lists lists (cdr lists))
		 (cars '() (cons (caar lists) cars))
		 (cdrs '() (cons (cdar lists) cdrs)))
		((not (pair? lists))
		 (let ((new (cons (apply procedure (reverse! cars)) '())))
		   (set-cdr! previous new)
		   (loop (reverse! cdrs) new)))
	      (if (not (pair? (car lists)))
		  (bad-end)))
	    (do ((lists lists (cdr lists)))
		((not (pair? lists)))
	      (if (not (null? (car lists)))
		  (bad-end)))))
      (cdr head)))

  (define (bad-end)
    (do ((lists (cons first rest) (cdr lists)))
	((not (pair? lists)))
      (if (not (list? (car lists)))
	  (error:wrong-type-argument (car lists) "list" 'MAP)))
    (let ((n (length first)))
      (do ((lists rest (cdr lists)))
	  ((not (pair? lists)))
	(if (not (fix:= n (length (car lists))))
	    (error:bad-range-argument (car lists) 'MAP)))))

  (if (pair? rest)
      (if (pair? (cdr rest))
	  (map-n (cons first rest))
	  (map-2 first (car rest)))
      (map-1 first)))

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
		      (COND ((PAIR? L)
			     (,combiner (PROCEDURE (CAR L))
					(MAP-1 (CDR L))))
			    ((NULL? L) ,initial-value)
			    (ELSE (BAD-END))))
		    (DEFINE (MAP-2 L1 L2)
		      (COND ((AND (PAIR? L1) (PAIR? L2))
			     (,combiner (PROCEDURE (CAR L1) (CAR L2))
					(MAP-2 (CDR L1) (CDR L2))))
			    ((AND (NULL? L1) (NULL? L2)) ,initial-value)
			    (ELSE (BAD-END))))
		    (DEFINE (MAP-N LISTS)
		      (LET N-LOOP ((LISTS LISTS))
			(IF (PAIR? (CAR LISTS))
			    (DO ((LISTS LISTS (CDR LISTS))
				 (CARS '() (CONS (CAAR LISTS) CARS))
				 (CDRS '() (CONS (CDAR LISTS) CDRS)))
				((NOT (PAIR? LISTS))
				 (,combiner (APPLY PROCEDURE (REVERSE! CARS))
					    (N-LOOP (REVERSE! CDRS))))
			      (IF (NOT (PAIR? (CAR LISTS)))
				  (BAD-END)))
			    (DO ((LISTS LISTS (CDR LISTS)))
				((NOT (PAIR? LISTS)) ,initial-value)
			      (IF (NOT (NULL? (CAR LISTS)))
				  (BAD-END))))))
		    (DEFINE (BAD-END)
		      (DO ((LISTS (CONS FIRST REST) (CDR LISTS)))
			  ((NOT (PAIR? LISTS)))
			(IF (NOT (LIST? (CAR LISTS)))
			    (ERROR:WRONG-TYPE-ARGUMENT (CAR LISTS) "list"
						       ',name)))
		      (LET ((N (LENGTH FIRST)))
			(DO ((LISTS REST (CDR LISTS)))
			    ((NOT (PAIR? LISTS)))
			  (IF (NOT (FIX:= N (LENGTH (CAR LISTS))))
			      (ERROR:BAD-RANGE-ARGUMENT (CAR LISTS) ',name)))))
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

(define mapcan append-map!)
(define mapcan* append-map*!)

(define (reduce procedure initial list)
  (if (pair? list)
      (let loop ((value (car list)) (l (cdr list)))
	(if (pair? l)
	    (loop (procedure value (car l)) (cdr l))
	    (begin
	      (if (not (null? l))
		  (error:wrong-type-argument list "list" 'REDUCE))
	      value)))
      (begin
	(if (not (null? list))
	    (error:wrong-type-argument list "list" 'REDUCE))
	initial)))

(define (reduce-right procedure initial list)
  (if (pair? list)
      (let loop ((value (car list)) (l (cdr list)))
	(if (pair? l)
	    (procedure value (loop (car l) (cdr l)))
	    (begin
	      (if (not (null? l))
		  (error:wrong-type-argument list "list" 'REDUCE-RIGHT))
	      value)))
      (begin
	(if (not (null? list))
	    (error:wrong-type-argument list "list" 'REDUCE-RIGHT))
	initial)))

(define (fold-left procedure initial-value a-list)
  (let fold ((initial-value initial-value)
	     (list a-list))
    (if (pair? list)
	(fold (procedure initial-value (car list))
	      (cdr list))
	(begin
	  (if (not (null? list))
	      (error:wrong-type-argument a-list "list" 'FOLD-LEFT))
	  initial-value))))

(define (fold-right procedure initial-value a-list)
  (let fold ((list a-list))
    (if (pair? list)
	(procedure (car list) (fold (cdr list)))
	(begin
	  (if (not (null? list))
	      (error:wrong-type-argument a-list "list" 'FOLD-RIGHT))
	  initial-value))))

;;;; Generalized List Operations

(define (keep-matching-items items predicate)
  (let ((lose
	 (lambda ()
	   (error:wrong-type-argument items "list" 'KEEP-MATCHING-ITEMS))))
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
  (let ((lose
	 (lambda ()
	   (error:wrong-type-argument items "list" 'DELETE-MATCHING-ITEMS))))
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

(define (find-matching-item items predicate)
  (let loop ((items* items))
    (if (pair? items*)
	(if (predicate (car items*))
	    (car items*)
	    (loop (cdr items*)))
	(begin
	  (if (not (null? items*))
	      (error:wrong-type-argument items "list" 'FIND-MATCHING-ITEM))
	  #f))))

(define list-transform-positive keep-matching-items)
(define list-transform-negative delete-matching-items)
(define list-search-positive find-matching-item)

(define (list-search-negative items predicate)
  (find-matching-item items
    (lambda (item)
      (not (predicate item)))))

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
	  (error:wrong-type-argument items "list" 'DELETE-MATCHING-ITEMS!))))
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
	  (error:wrong-type-argument items "list" 'KEEP-MATCHING-ITEMS!))))
    (trim-initial-segment items)))

(define ((list-deletor predicate) items)
  (delete-matching-items items predicate))

(define ((list-deletor! predicate) items)
  (delete-matching-items! items predicate))

;;;; Membership/Association Lists

(define (initialize-package!)
  (set! memv (member-procedure eqv?))
  (set! member (member-procedure equal?))
  (set! delv (delete-member-procedure list-deletor eqv?))
  (set! delete (delete-member-procedure list-deletor equal?))
  (set! delv! (delete-member-procedure list-deletor! eqv?))
  (set! delete! (delete-member-procedure list-deletor! equal?))
  (set! assv (association-procedure eqv? car))
  (set! assoc (association-procedure equal? car))
  (set! del-assq (delete-association-procedure list-deletor eq? car))
  (set! del-assv (delete-association-procedure list-deletor eqv? car))
  (set! del-assoc (delete-association-procedure list-deletor equal? car))
  (set! del-assq! (delete-association-procedure list-deletor! eq? car))
  (set! del-assv! (delete-association-procedure list-deletor! eqv? car))
  (set! del-assoc! (delete-association-procedure list-deletor! equal? car))
  unspecific)

(define memv)
(define member)
(define delv)
(define delete)
(define delv!)
(define delete!)
(define assv)
(define assoc)
(define del-assq)
(define del-assv)
(define del-assoc)
(define del-assq!)
(define del-assv!)
(define del-assoc!)

(define (member-procedure predicate)
  (lambda (item items)
    (let loop ((items* items))
      (if (pair? items*)
	  (if (predicate (car items*) item)
	      items*
	      (loop (cdr items*)))
	  (begin
	    (if (not (null? items*))
		(error:wrong-type-argument items "list" #f))
	    #f)))))

(define (add-member-procedure predicate)
  (let ((member (member-procedure predicate)))
    (lambda (item items)
      (if (member item items)
	  items
	  (cons item items)))))

(define ((delete-member-procedure deletor predicate) item items)
  ((deletor (lambda (match) (predicate match item))) items))

(define (association-procedure predicate selector)
  (lambda (key items)
    (let loop ((items* items))
      (if (pair? items*)
	  (if (predicate (selector (car items*)) key)
	      (car items*)
	      (loop (cdr items*)))
	  (begin
	    (if (not (null? items*))
		(error:wrong-type-argument items "list" #f))
	    #f)))))

(define ((delete-association-procedure deletor predicate selector) key alist)
  ((deletor (lambda (entry) (predicate (selector entry) key))) alist))

;;; The following could be defined using the generic procedures above,
;;; but the compiler produces better code for them this way.  The only
;;; reason to use these procedures is speed, so we crank them up.

(define (memq item items)
  (let loop ((items* items))
    (if (pair? items*)
	(if (eq? (car items*) item)
	    items*
	    (loop (cdr items*)))
	(begin
	  (if (not (null? items*))
	      (error:wrong-type-argument items "list" 'MEMQ))
	  #f))))

(define (assq key alist)
  (let loop ((alist* alist))
    (if (pair? alist*)
	(begin
	  (if (not (pair? (car alist*)))
	      (error:wrong-type-argument alist "alist" 'ASSQ))
	  (if (eq? (car (car alist*)) key)
	      (car alist*)
	      (loop (cdr alist*))))
	(begin
	  (if (not (null? alist*))
	      (error:wrong-type-argument alist "alist" 'ASSQ))
	  #f))))

(define (delq item items)
  (let ((lose (lambda () (error:wrong-type-argument items "list" 'DELQ))))
    (cond ((pair? items)
	   (let ((head (cons (car items) '())))
	     (let loop ((items (cdr items)) (previous head))
	       (cond ((pair? items)
		      (if (eq? item (car items))
			  (loop (cdr items) previous)
			  (let ((new (cons (car items) '())))
			    (set-cdr! previous new)
			    (loop (cdr items) new))))
		     ((not (null? items)) (lose))))
	     (if (eq? item (car items))
		 (cdr head)
		 head)))
	  ((null? items) items)
	  (else (lose)))))

(define (delq! item items)
  (letrec ((trim-initial-segment
	    (lambda (items*)
	      (if (pair? items*)
		  (if (eq? item (car items*))
		      (trim-initial-segment (cdr items*))
		      (begin
			(locate-initial-segment items* (cdr items*))
			items*))
		  (begin
		    (if (not (null? items*))
			(error:wrong-type-argument items "list" 'DELQ!))
		    '()))))
	   (locate-initial-segment
	    (lambda (last this)
	      (if (pair? this)
		  (if (eq? item (car this))
		      (set-cdr! last (trim-initial-segment (cdr this)))
		      (locate-initial-segment this (cdr this)))
		  (if (not (null? this))
		      (error:wrong-type-argument items "list" 'DELQ!))))))
    (trim-initial-segment items)))

;;;; Lastness and Segments

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
	(let loop ((list* (cdr list)) (previous head))
	  (if (pair? (cdr list*))
	      (let ((new (cons (car list*) '())))
		(set-cdr! previous new)
		(loop (cdr list*) new))
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

(define-integrable (guarantee-pair object procedure)
  (if (not (pair? object))
      (error:wrong-type-argument object "pair" procedure)))