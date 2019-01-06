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

;;; Note:  In this file, CAR and CDR refer to the ucode primitives,
;;; but composite operations, like CAAR, CDAR, CDADR, etc., refer to
;;; `safe' procedures that check the type of their arguments.
;;; Procedures such as %ASSOC, which are written with explicit type
;;; checks, use chains of CAR and CDR operations rather than the
;;; more concise versions in order to avoid unnecessary duplication
;;; of type checks and out-of-line calls. -- jrm

(declare (usual-integrations))

(define-primitives
  (car 1)
  (cdr 1)
  (cons 2)
  (general-car-cdr 2)
  (null? 1)
  (pair? 1)
  (set-car! 2)
  (set-cdr! 2))

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
  (guarantee index-fixnum? length 'make-list)
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
  (guarantee index-fixnum? length 'make-circular-list)
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
  (guarantee index-fixnum? length 'make-initialized-list)
  (let loop ((index (fix:- length 1)) (result '()))
    (if (fix:< index 0)
	result
	(loop (fix:- index 1)
	      (cons (initialization index) result)))))

(define (xcons d a)
  (cons a d))

(define (iota count #!optional start step)
  (guarantee index-fixnum? count 'iota)
  (let ((start
	 (if (default-object? start)
	     0
	     (begin
	       (guarantee number? start 'iota)
	       start)))
	(step
	 (if (default-object? step)
	     1
	     (begin
	       (guarantee number? step 'iota)
	       step))))
    (make-initialized-list count (lambda (index) (+ start (* index step))))))

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

(define (non-empty-list? object)
  (and (pair? object)
       (list? (cdr object))))

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
	(error:not-a list? object caller))
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
  (guarantee-list->length list 'length))

(define (length=? left right)
  (define (%length=? n list)
    (cond ((pair? list) (and (fix:positive? n)
			     (%length=? (fix:- n 1) (cdr list))))
	  ((null? list) (fix:zero? n))
	  (else (error:not-a list? list 'length=?))))

  (define (%same-length left right)
    (cond ((pair? left)
	   (cond ((pair? right) (%same-length (cdr left) (cdr right)))
		 ((null? right) #f)
		 (else (error:not-a list? right 'length=?))))
	  ((null? left)
	   (cond ((pair? right) #f)
		 ((null? right) #t)
		 (else (error:not-a list? right 'length=?))))
	  (else
	   (error:not-a list? left 'length=?))))

  ;; Take arguments in either order to make this easy to use.
  (cond ((pair? left)
	 (cond ((pair? right) (%same-length (cdr left) (cdr right)))
	       ((index-fixnum? right) (%length=? right left))
	       ((null? right) #f)
	       (else
		(error:wrong-type-argument right "index fixnum or list"
					   'length=?))))
	((index-fixnum? left)
	 (%length=? left right))
	((null? left)
	 (cond ((pair? right) #f)
	       ((index-fixnum? right) (fix:zero? right))
	       ((null? right) #t)
	       (else
		(error:wrong-type-argument right "index fixnum or list"
					   'length=?))))
	(else
	 (error:wrong-type-argument left "index fixnum or list" 'length=?))))

(define (not-pair? x)
  (not (pair? x)))

(define (null-list? l #!optional caller)
  (cond ((pair? l) #f)
	((null? l) #t)
	(else (error:not-a list? l caller))))

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
		(guarantee list? list 'list=))
	      lists))

  (if (and (pair? lists)
	   (pair? (cdr lists)))
      (n-ary (car lists) (car (cdr lists)) (cdr (cdr lists)))
      #t))

(define (list-ref list index)
  (let ((tail (list-tail list index)))
    (if (not (pair? tail))
	(error:bad-range-argument index 'list-ref))
    (car tail)))

(define (list-set! list index new-value)
  (let ((tail (list-tail list index)))
    (if (not (pair? tail))
	(error:bad-range-argument index 'list-set!))
    (set-car! tail new-value)))

(define (list-tail list index)
  (guarantee index-fixnum? index 'list-tail)
  (let loop ((list list) (index* index))
    (if (fix:zero? index*)
	list
	(begin
	  (if (not (pair? list))
	      (error:bad-range-argument index 'list-tail))
	  (loop (cdr list) (fix:- index* 1))))))

(define (list-head list index)
  (guarantee index-fixnum? index 'list-head)
  (let loop ((list list) (index* index))
    (if (fix:zero? index*)
	'()
	(begin
	  (if (not (pair? list))
	      (error:bad-range-argument index 'list-head))
	  (cons (car list) (loop (cdr list) (fix:- index* 1)))))))

(define (sublist list start end)
  (list-head (list-tail list start) (- end start)))

(define (list-copy items)
  (let ((lose (lambda () (error:not-a list? items 'list-copy))))
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

;;;; Weak lists

(define (weak-list->list items)
  (let loop ((items* items) (result '()))
    (if (weak-pair? items*)
	(loop (weak-cdr items*)
	      (let ((item (%weak-car items*)))
		(if item
		    (cons (%weak-false->false item) result)
		    result)))
	(begin
	  (if (not (null? items*))
	      (error:not-a weak-list? items 'weak-list->list))
	  (reverse! result)))))

(define (list->weak-list items)
  (let loop ((items* (reverse items)) (result '()))
    (if (pair? items*)
	(loop (cdr items*)
	      (weak-cons (car items*) result))
	(begin
	  (if (not (null? items*))
	      (error:not-a list? items 'list->weak-list))
	  result))))

(define (weak-list? object)
  (let loop ((l1 object) (l2 object))
    (if (weak-pair? l1)
	(let ((l1 (weak-cdr l1)))
	  (and (not (eq? l1 l2))
	       (if (weak-pair? l1)
		   (loop (weak-cdr l1) (weak-cdr l2))
		   (null? l1))))
	(null? l1))))

(define (weak-memq item items)
  (let ((item (%false->weak-false item)))
    (let loop ((items* items))
      (if (weak-pair? items*)
	  (if (eq? item (%weak-car items*))
	      items*
	      (loop (weak-cdr items*)))
	  (begin
	    (if (not (null? items*))
		(error:not-a weak-list? items 'weak-memq))
	    #f)))))

(define (weak-delq! item items)
  (let ((item (%false->weak-false item)))
    (letrec ((trim-initial-segment
	      (lambda (items*)
		(if (weak-pair? items*)
		    (if (or (eq? item (%weak-car items*))
			    (eq? #f (%weak-car items*)))
			(trim-initial-segment (weak-cdr items*))
			(begin
			  (locate-initial-segment items* (weak-cdr items*))
			  items*))
		    (begin
		      (if (not (null? items*))
			  (error:not-a weak-list? items 'weak-delq!))
		      '()))))
	     (locate-initial-segment
	      (lambda (last this)
		(if (weak-pair? this)
		    (if (or (eq? item (%weak-car this))
			    (eq? #f (%weak-car this)))
			(set-cdr! last (trim-initial-segment (weak-cdr this)))
			(locate-initial-segment this (weak-cdr this)))
		    (if (not (null? this))
			(error:not-a weak-list? items 'weak-delq!))))))
      (trim-initial-segment items))))

;;;; General CAR CDR

;;; Return a list of car and cdr symbols that the code
;;; represents.  Leftmost operation is outermost.
(define (decode-general-car-cdr code)
  (guarantee positive-fixnum? code)
  (do ((code code (fix:lsh code -1))
       (result '() (cons (if (even? code) 'cdr 'car) result)))
      ((= code 1) result)))

;;; Return the bit string that encode the operation-list.
;;; Operation list is encoded with leftmost outer.
(define (encode-general-car-cdr operation-list)
  (do ((code operation-list (cdr code))
       (answer 1 (+ (* answer 2)
		    (case (car code)
		      ((car) 1)
		      ((cdr) 0)
		      (else (error "encode-general-car-cdr: Invalid operation"
				    (car code)))))))
      ((not (pair? code))
       (if (not (fixnum? answer))
	   (error "encode-general-car-cdr: code too large" answer)
	   answer))))

;;;; Standard Selectors

(declare (integrate-operator safe-car safe-cdr))

(define (safe-car x)
  (if (pair? x) (car x) (error:not-a pair? x 'safe-car)))

(define (safe-cdr x)
  (if (pair? x) (cdr x) (error:not-a pair? x 'safe-cdr)))

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
					(error:not-a list? (car rest)
						     'append))))
			       root))
			    ((null? l1)
			     accum)
			    (else
			     (error:not-a list? (car rest) 'append))))
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
		   (error:not-a list? (car lists) 'append!))
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
	      (error:not-a list? l 'reverse*))
	  so-far))))

(define (reverse*! l tail)
  (let loop ((current l) (new-cdr tail))
    (if (pair? current)
	(let ((next (cdr current)))
	  (set-cdr! current new-cdr)
	  (loop next current))
	(begin
	  (if (not (null? current))
	      (error:not-a list? l 'reverse*!))
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
			 (cons (car (car lists)) cars)
			 (cons (cdr (car lists)) cdrs))
		  (if (not (null? (car lists)))
		      (bad-end)))
	      (let ((new (cons (apply procedure (reverse! cars)) '())))
		(set-cdr! previous new)
		(loop (reverse! cdrs) new)))))
      (cdr head)))

  (define (bad-end)
    (mapper-error (cons first rest) 'map))

  (if (pair? rest)
      (if (pair? (cdr rest))
	  (map-n (cons first rest))
	  (map-2 first (car rest)))
      (map-1 first)))

(define (mapper-error lists caller)
  (for-each (lambda (list)
	      (if (dotted-list? list)
		  (error:not-a list? list caller)))
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
	   `(set! ,name
		  (named-lambda (,name ,@extra-vars procedure first . rest)

		    (define (map-1 l)
		      (if (pair? l)
			  (,combiner (procedure (car l))
				     (map-1 (cdr l)))
			  (begin
			    (if (not (null? l))
				(bad-end))
			    ,initial-value)))

		    (define (map-2 l1 l2)
		      (if (and (pair? l1) (pair? l2))
			  (,combiner (procedure (car l1) (car l2))
				     (map-2 (cdr l1) (cdr l2)))
			  (begin
			    (if (not (and (or (null? l1) (pair? l1))
					  (or (null? l2) (pair? l2))))
				(bad-end))
			    ,initial-value)))

		    (define (map-n lists)
		      (let split ((lists lists) (cars '()) (cdrs '()))
			(if (pair? lists)
			    (if (pair? (car lists))
				(split (cdr lists)
				       (cons (car (car lists)) cars)
				       (cons (cdr (car lists)) cdrs))
				(begin
				  (if (not (null? (car lists)))
				      (bad-end))
				  ,initial-value))
			    (,combiner (apply procedure (reverse! cars))
				       (map-n (reverse! cdrs))))))

		    (define (bad-end)
		      (mapper-error (cons first rest) ',name))

		    (if (pair? rest)
			(if (pair? (cdr rest))
			    (map-n (cons first rest))
			    (map-2 first (car rest)))
			(map-1 first)))))))))

  (mapper for-each () begin unspecific)
  (mapper map* (initial-value) cons initial-value)
  (mapper append-map () append '())
  (mapper append-map* (initial-value) append initial-value)
  (mapper append-map! () append! '())
  (mapper append-map*! (initial-value) append! initial-value))

(declare (integrate-operator %fold-left))

(define (%fold-left caller procedure initial list)
  (declare (integrate caller procedure initial))
  (let %fold-left-step ((state initial) (remaining list))
    (if (pair? remaining)
	(%fold-left-step (procedure state (car remaining))
			 (cdr remaining))
	(begin
	  (if (not (null? remaining))
	      (error:not-a list? list caller))
	  state))))

;; N-ary version
;; Invokes (PROCEDURE state arg1 arg2 ...) on the all the lists in parallel.
;; State is returned as soon as any list is exhausted.
(define (%fold-left-lists caller procedure initial arglists)
  (let fold-left-step ((state initial) (lists arglists))
    (let collect-arguments ((arglists (reverse lists)) (cars '()) (cdrs '()))
      (if (pair? arglists)
	  (let ((first-list (car arglists)))
	    (if (pair? first-list)
		(collect-arguments (cdr arglists)
				   (cons (car first-list) cars)
				   (cons (cdr first-list) cdrs))
		(begin
		  (if (not (null? first-list))
		      (mapper-error arglists caller))
		  state)))
	  (begin
	    (if (not (null? arglists))
		(mapper-error arglists caller))
	    (fold-left-step (apply procedure state cars)
			    cdrs))))))

(define (fold-left procedure initial first . rest)
  (if (pair? rest)
      (%fold-left-lists 'fold-left procedure initial (cons first rest))
      (%fold-left 'fold-left procedure initial first)))

;;; Variants of FOLD-LEFT that should probably be avoided.

;; Like FOLD-LEFT, but
;;    PROCEDURE takes the arguments with the state at the right-hand end.
(define (fold procedure initial first . rest)
  (if (pair? rest)
      (%fold-left-lists 'fold
			(lambda (state . arguments)
			  (apply procedure (append arguments (list state))))
			initial
			(cons first rest))
      (%fold-left 'fold
		  (lambda (state item)
		    (declare (integrate state item))
		    (procedure item state))
		  initial
		  first)))

;; Like FOLD-LEFT, with four differences.
;;    1. Not n-ary
;;    2. INITIAL is first element in list.
;;    3. DEFAULT is only used if the list is empty
;;    4. PROCEDURE takes arguments in the wrong order.
(define (reduce procedure default list)
  (if (pair? list)
      (%fold-left 'reduce
		  (lambda (state item)
		    (declare (integrate state item))
		    (procedure item state))
		  (car list)
		  (cdr list))
      (begin
	(if (not (null? list))
	    (error:not-a list? list 'reduce))
	default)))

(define (reduce-left procedure initial list)
  (reduce (lambda (a b) (procedure b a)) initial list))

(define (reduce-right procedure initial list)
  (if (pair? list)
      (let loop ((first (car list)) (rest (cdr list)))
	(if (pair? rest)
	    (procedure first (loop (car rest) (cdr rest)))
	    (begin
	      (if (not (null? rest))
		  (error:not-a list? list 'reduce-right))
	      first)))
      (begin
	(if (not (null? list))
	    (error:not-a list? list 'reduce-right))
	initial)))

(define (fold-right procedure initial first . rest)
  (if (pair? rest)
      (let loop ((lists (cons first rest)))
	(let split ((lists lists) (cars '()) (cdrs '()))
	  (if (pair? lists)
	      (if (pair? (car lists))
		  (split (cdr lists)
			 (cons (car (car lists)) cars)
			 (cons (cdr (car lists)) cdrs))
		  (begin
		    (if (not (null? (car lists)))
			(mapper-error (cons first rest) 'fold-right))
		    initial))
	      (apply procedure
		     (reverse! (cons (loop (reverse! cdrs)) cars))))))
      (let loop ((list first))
	(if (pair? list)
	    (procedure (car list) (loop (cdr list)))
	    (begin
	      (if (not (null? list))
		  (error:not-a list? first 'fold-right))
	      initial)))))

;;;; Generalized list operations -- mostly deprecated in favor of SRFI-1

(define (find-matching-item items predicate)
  (find predicate items))

(define (find-non-matching-item items predicate)
  (find (lambda (item) (not (predicate item))) items))

(define (count-matching-items items predicate)
  (count predicate items))

(define (count-non-matching-items items predicate)
  (count (lambda (item)
	   (not (predicate item)))
	 items))

(define (keep-matching-items items predicate)
  (filter predicate items))

(define (delete-matching-items items predicate)
  (remove predicate items))

(define (delete-matching-items! items predicate)
  (remove! predicate items))

(define (keep-matching-items! items predicate)
  (filter! predicate items))

(define ((list-deletor predicate) items)
  (remove predicate items))

(define ((list-deletor! predicate) items)
  (remove! predicate items))

;;;; Membership lists

(define (memq item items)
  (%member item items eq? 'memq))

(define (memv item items)
  (%member item items eqv? 'memv))

(define (member item items #!optional =)
  (let ((= (if (default-object? =) equal? =)))
    (%member item items = 'member)))

(define (member-procedure = #!optional caller)
  (lambda (item items)
    (%member item items = caller)))

(define (add-member-procedure = #!optional caller)
  (lambda (item items)
    (if (%member item items = caller)
	items
	(cons item items))))

(define-integrable (%member item items = caller)
  (let ((lose (lambda () (error:not-a list? items caller))))
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
  (%delete item items eq? 'delq))

(define (delv item items)
  (%delete item items eqv? 'delv))

(define (delete item items #!optional =)
  (let ((= (if (default-object? =) equal? =)))
    (%delete item items = 'delete)))

(define-integrable (%delete item items = caller)
  (let ((lose (lambda () (error:not-a list? items caller))))
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
  (%delete! item items eq? 'delq!))

(define (delv! item items)
  (%delete! item items eqv? 'delv!))

(define (delete! item items #!optional =)
  (let ((= (if (default-object? =) equal? =)))
    (%delete! item items = 'delete!)))

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
		  (error:not-a list? items caller)))))
       (lose
	(lambda ()
	  (error:not-a list? items caller))))
    (trim-initial-segment items)))

;;;; Association lists

(define (alist? object)
  (list-of-type? object pair?))

(define-integrable (alist-cons key datum alist)
  (cons (cons key datum) alist))

(define (alist-copy alist)
  (let ((lose (lambda () (error:not-a alist? alist 'alist-copy))))
    (cond ((pair? alist)
	   (if (pair? (car alist))
	       (let ((head (cons (car alist) '())))
		 (let loop ((alist (cdr alist)) (previous head))
		   (cond ((pair? alist)
			  (if (pair? (car alist))
			      (let ((new
				     (alist-cons (car (car alist))
						 (cdr (car alist))
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
    (let ((lose (lambda () (error:not-a list? items caller))))
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
  (%assoc key alist eq? 'assq))

(define (assv key alist)
  (%assoc key alist eqv? 'assv))

(define (assoc key alist #!optional =)
  (let ((= (if (default-object? =) equal? =)))
    (%assoc key alist = 'assoc)))

(define-integrable (%assoc key alist = caller)
  (let ((lose (lambda () (error:not-a alist? alist caller))))
    (declare (no-type-checks))
    (let loop ((alist alist))
      (if (pair? alist)
	  (begin
	    (if (not (pair? (car alist)))
		(lose))
	    (if (= (car (car alist)) key)
		(car alist)
		(loop (cdr alist))))
	  (begin
	    (if (not (null? alist))
		(lose))
	    #f)))))

(define (del-assq key alist)
  (%alist-delete key alist eq? 'del-assq))

(define (del-assv key alist)
  (%alist-delete key alist eqv? 'del-assv))

(define (del-assoc key alist)
  (%alist-delete key alist equal? 'del-assoc))

(define (alist-delete key alist #!optional =)
  (let ((= (if (default-object? =) equal? =)))
    (%alist-delete key alist = 'alist-delete)))

(define-integrable (%alist-delete key alist = caller)
  (let ((lose (lambda () (error:not-a alist? alist caller))))
    (if (pair? alist)
	(begin
	  (if (not (pair? (car alist)))
	      (lose))
	  (let ((head (cons (car alist) '())))
	    (let loop ((alist (cdr alist)) (previous head))
	      (cond ((pair? alist)
		     (if (not (pair? (car alist)))
			 (lose))
		     (if (= (car (car alist)) key)
			 (loop (cdr alist) previous)
			 (let ((new (cons (car alist) '())))
			   (set-cdr! previous new)
			   (loop (cdr alist) new))))
		    ((not (null? alist))
		     (lose))))
	    (if (= (car (car alist)) key)
		(cdr head)
		head)))
	(begin
	  (if (not (null? alist))
	      (lose))
	  alist))))

(define (del-assq! key alist)
  (%alist-delete! key alist eq? 'del-assq!))

(define (del-assv! key alist)
  (%alist-delete! key alist eqv? 'del-assv!))

(define (del-assoc! key alist)
  (%alist-delete! key alist equal? 'del-assoc!))

(define (alist-delete! key alist #!optional =)
  (let ((= (if (default-object? =) equal? =)))
    (%alist-delete! key alist = 'alist-delete!)))

(define-integrable (%alist-delete! item items = caller)
  (letrec
      ((trim-initial-segment
	(lambda (items)
	  (if (pair? items)
	      (begin
		(if (not (pair? (car items)))
		    (lose))
		(if (= (car (car items)) item)
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
		 (if (= (car (car this)) item)
		     (set-cdr!
		      last
		      (trim-initial-segment (cdr this)))
		     (locate-initial-segment this (cdr this))))
		((not (null? this))
		 (lose)))))
       (lose
	(lambda ()
	  (error:not-a alist? items caller))))
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

(define (get-keyword-value klist key #!optional default-value)
  (let ((lose (lambda () (error:not-a keyword-list? klist 'get-keyword-value))))
    (let loop ((klist klist))
      (if (pair? klist)
	  (begin
	    (if (not (pair? (cdr klist)))
		(lose))
	    (if (eq? (car klist) key)
		(car (cdr klist))
		(loop (cdr (cdr klist)))))
	  (begin
	    (if (not (null? klist))
		(lose))
	    default-value)))))

(define (get-keyword-values klist key)
  (let ((lose
	 (lambda () (error:not-a keyword-list? klist 'get-keyword-values))))
    (let loop ((klist klist) (values '()))
      (if (pair? klist)
	  (begin
	    (if (not (pair? (cdr klist)))
		(lose))
	    (loop (cdr (cdr klist))
		  (if (eq? (car klist) key)
		      (cons (car (cdr klist)) values)
		      values)))
	  (begin
	    (if (not (null? klist))
		(lose))
	    (reverse! values))))))

(define (keyword-list->alist klist)
  (let loop ((klist klist))
    (if (pair? klist)
	(alist-cons (car klist) (car (cdr klist))
		    (loop (cdr (cdr klist))))
	'())))

(define (alist->keyword-list alist)
  (let loop ((alist alist))
    (if (pair? alist)
	(cons (car (car alist))
	      (cons (cdr (car alist))
		    (loop (cdr alist))))
	'())))

(define (keyword-option-parser keyword-option-specs)
  (guarantee-list-of keyword-option-spec? keyword-option-specs
		     'keyword-option-parser)
  (lambda (options caller)
    (guarantee keyword-list? options caller)
    (apply values
	   (map (lambda (spec)
		  (receive (name predicate get-default)
		      (keyword-option-spec-parts spec)
		    (let ((value (get-keyword-value options name)))
		      (if (default-object? value)
			  (begin
			    (if (default-object? get-default)
				(error (string "Missing required option '"
					       name
					       "':")
				       options))
			    (get-default))
			  (guarantee predicate value caller)))))
		keyword-option-specs))))

(define (keyword-option-spec? object)
  (and (list? object)
       (memv (length object) '(2 3))
       (interned-symbol? (car object))
       (or (and (unary-procedure? (cadr object))
		(or (null? (cddr object))
		    (thunk? (caddr object))))
	   (and (list-of-type? (cadr object) interned-symbol?)
		(or (null? (cddr object))
		    (memq (caddr object) (cadr object))
		    (thunk? (caddr object)))))))

(define (keyword-option-spec-parts spec)
  (values (car spec)
	  (if (pair? (cadr spec))
	      (lambda (object) (memq object (cadr spec)))
	      (cadr spec))
	  (cond ((null? (cddr spec)) (default-object))
		((interned-symbol? (caddr spec)) (lambda () (caddr spec)))
		(else (caddr spec)))))

;;;; Last pair

(define (last list)
  (car (last-pair list)))

(define (last-pair list)
  (if (not (pair? list))
      (error:not-a pair? list 'last-pair))
  (let loop ((list list))
    (if (pair? (cdr list))
	(loop (cdr list))
	list)))

(define (except-last-pair list)
  (if (not (pair? list))
      (error:not-a pair? list 'except-last-pair))
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
  (if (not (pair? list))
      (error:not-a pair? list 'except-last-pair!))
  (if (pair? (cdr list))
      (begin
	(let loop ((list list))
	  (if (pair? (cdr (cdr list)))
	      (loop (cdr list))
	      (set-cdr! list '())))
	list)
      '()))