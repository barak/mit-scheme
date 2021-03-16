#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020 Massachusetts Institute of Technology

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

;;;; Constructors

(define (list . items)
  items)

(define (cons* elt . elts)
  (let loop ((elt elt) (elts elts))
    (if (pair? elts)
	(cons elt (loop (car elts) (cdr elts)))
	elt)))

(define (make-list n #!optional fill)
  (guarantee index-fixnum? n 'make-list)
  (let loop ((n n) (result '()))
    (if (fix:> n 0)
	(loop (fix:- n 1) (cons fill result))
	result)))

(define (circular-list . elts)
  (if (pair? elts)
      (let loop ((l elts))
	(if (pair? (cdr l))
	    (loop (cdr l))
	    (set-cdr! l elts))))
  elts)

(define (make-circular-list n #!optional fill)
  (guarantee index-fixnum? n 'make-circular-list)
  (if (fix:> n 0)
      (let ((last (cons fill '())))
	(let loop ((n (fix:- n 1)) (result last))
	  (if (fix:> n 0)
	      (loop (fix:- n 1) (cons fill result))
	      (begin
		(set-cdr! last result)
		result))))
      '()))

(define (list-tabulate n init-proc)
  (guarantee index-fixnum? n 'list-tabulate)
  (if (fix:> n 0)
      (let loop ((index (fix:- n 1)) (result '()))
	(if (fix:> index 0)
	    (loop (fix:- index 1)
		  (cons (init-proc index) result))
	    (cons (init-proc index) result)))
      '()))

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
    (list-tabulate count (lambda (index) (+ start (* index step))))))

;;;; Predicates

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
    (and (pair? l1)
	 (let ((l1 (cdr l1)))
	   (or (eq? l1 l2)
	       (and (pair? l1)
		    (loop (cdr l1) (cdr l2))))))))

(define (null-list? object #!optional caller)
  (%null-list? object caller))

(define-integrable (%null-list? object caller)
  (cond ((null? object) #t)
	((pair? object) #f)
	(else (error:not-a list? object caller))))

(define (non-empty-list? object)
  (and (pair? object)
       (list? (cdr object))))

(define (not-pair? x)
  (not (pair? x)))

(define (list= elt= . lists)

  (define (n-ary l1 l2 rest)
    (if (pair? rest)
	(and (binary l1 l2)
	     (n-ary l2 (car rest) (cdr rest)))
	(binary l1 l2)))

  (define (binary l1 l2)
    (if (%null-list? l1 'list=)
	(%null-list? l2 'list=)
	(or (eq? l1 l2)
	    (and (not (%null-list? l2 'list=))
		 (elt= (car l1) (car l2))
		 (binary (cdr l1) (cdr l2))))))

  (if (and (pair? lists)
	   (pair? (cdr lists)))
      (n-ary (car lists) (cadr lists) (cddr lists))
      #t))

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

;;;; Length

(define (length list)
  (let ((n (list?->length list)))
    (if (not n)
	(error:not-a list? list 'length))
    n))

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

;;;; Simple accessors

(define (list-ref list index)
  (car (drop list index)))

(define (list-set! list index new-value)
  (set-car! (drop list index) new-value))

(define (sublist list start end)
  (take (drop list start) (- end start)))

(define (list-copy items)
  (if (pair? items)
      (let ((head (cons (car items) '())))
	(let loop ((list (cdr items)) (previous head))
	  (if (pair? list)
	      (let ((new (cons (car list) '())))
		(set-cdr! previous new)
		(loop (cdr list) new))
	      (set-cdr! previous list)))
	head)
      items))

(define (tree-copy tree)
  (let walk ((tree tree))
    (if (pair? tree)
	(cons (walk (car tree)) (walk (cdr tree)))
	tree)))

(define (car+cdr pair)
  (values (car pair) (cdr pair)))

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

(define-integrable (%append-2 l1 l2)
  (if (%null-list? l1 'append)
      l2
      (let ((root (cons (car l1) #f)))
	(let loop ((this (cdr l1)) (prev root))
	  (if (%null-list? this 'append)
	      (begin
		(set-cdr! prev l2)
		root)
	      (let ((prev* (cons (car this) #f)))
		(set-cdr! prev prev*)
		(loop (cdr this) prev*)))))))

(define append
  (make-arity-dispatched-procedure
   (named-lambda (append self . lists)
     self
     (if (pair? lists)
	 (let recur ((lists lists))
	   ;; Recursion limited by number of arguments.
	   (let ((list0 (car lists))
		 (lists (cdr lists)))
	     (if (pair? lists)
		 (%append-2 list0 (recur lists))
		 list0)))
	 '()))
   (lambda () '())
   (lambda (l) l)
   %append-2))

(define-integrable (%append-2! l1 l2)
  (if (%null-list? l1 'append!)
      l2
      (begin
	(set-cdr! (last-pair l1) l2)
	l1)))

(define append!
  (make-arity-dispatched-procedure
   (named-lambda (append! self . lists)
     self
     (if (pair? lists)
	 (let recur ((lists lists))
	   ;; Recursion limited by number of arguments.
	   (let ((list0 (car lists))
		 (lists (cdr lists)))
	     (if (pair? lists)
		 (%append-2! list0 (recur lists))
		 list0)))
	 '()))
   (lambda () '())
   (lambda (l) l)
   %append-2!))

;;;; Mapping Procedures

(define map
  (make-arity-dispatched-procedure

   (named-lambda (map self procedure first . rest)
     (declare (ignore self))
     (%reverse
      (let map-n ((lists (cons first rest)) (r '()))
	(let split ((lists lists) (cars '()) (cdrs '()))
	  (if (pair? lists)
	      (if (%null-list? (car lists) 'map)
		  r
		  (split (cdr lists)
			 (cons (car (car lists)) cars)
			 (cons (cdr (car lists)) cdrs)))
	      (map-n (reverse! cdrs)
		     (cons (apply procedure (reverse! cars)) r)))))))

   #f					;zero arguments
   #f					;one argument (procedure)

   (named-lambda (map procedure first)
     (%reverse
      (let map-1 ((l first) (r '()))
	(if (%null-list? l 'map)
	    r
	    (map-1 (cdr l)
		   (cons (procedure (car l)) r))))))

   (named-lambda (map procedure first second)
     (%reverse
      (let map-2 ((l1 first) (l2 second) (r '()))
	(if (or (%null-list? l1 'map)
		(%null-list? l2 'map))
	    r
	    (map-2 (cdr l1)
		   (cdr l2)
		   (cons (procedure (car l1) (car l2)) r))))))))

(define-integrable (%reverse list)
  (let rev ((list list) (reversed '()))
    (if (pair? list)
	(rev (cdr list) (cons (car list) reversed))
	reversed)))

(let-syntax
    ((mapper
      (rsc-macro-transformer
       (lambda (form environment)
	 (declare (ignore environment))
	 (let ((name (list-ref form 1))
	       (extra-vars (list-ref form 2))
	       (combiner (list-ref form 3))
	       (initial-value (list-ref form 4)))
	   `(define ,name
	      (make-arity-dispatched-procedure

	       (named-lambda (,name self ,@extra-vars procedure first . rest)
		 (declare (ignore self))
		 (let map-n ((lists (cons first rest)))
		   (let split ((lists lists) (cars '()) (cdrs '()))
		     (if (pair? lists)
			 (if (%null-list? (car lists) ',name)
			     ,initial-value
			     (split (cdr lists)
				    (cons (car (car lists)) cars)
				    (cons (cdr (car lists)) cdrs)))
			 (,combiner (apply procedure (reverse! cars))
				    (map-n (reverse! cdrs)))))))

	       ,@(make-list (+ (length extra-vars) 2) #f)

	       (named-lambda (,name ,@extra-vars procedure first)
		 (let map-1 ((l first))
		   (if (%null-list? l ',name)
		       ,initial-value
		       (,combiner (procedure (car l))
				  (map-1 (cdr l))))))

	       (named-lambda (,name ,@extra-vars procedure first second)
		 (let map-2 ((l1 first) (l2 second))
		   (if (or (%null-list? l1 ',name)
			   (%null-list? l2 ',name))
		       ,initial-value
		       (,combiner (procedure (car l1) (car l2))
				  (map-2 (cdr l1) (cdr l2)))))))))))))

  (mapper for-each () begin unspecific)
  (mapper map* (initial-value) cons initial-value)
  (mapper append-map () append '())
  (mapper append-map* (initial-value) append initial-value)
  (mapper append-map! () append! '())
  (mapper append-map*! (initial-value) append! initial-value))

;;;; Fold and reduce

(define (fold kons knil first . rest)
  (cond ((null? rest)
	 (%fold kons knil first 'fold))
	((null? (cdr rest))
	 (%fold-2 kons knil first (car rest) 'fold))
	(else
	 (apply generator-fold
		kons
		knil
		(list->generator first)
		(map list->generator rest)))))

(define (fold-map kons knil proc first . rest)
  (cond ((null? rest)
	 (%fold-map kons knil proc first 'fold-map))
	((null? (cdr rest))
	 (%fold-map-2 kons knil proc first (car rest) 'fold-map))
	(else
	 (apply generator-fold-map
		kons
		knil
		proc
		(list->generator first)
		(map list->generator rest)))))

(define (fold-right kons knil first . rest)
  (cond ((null? rest)
	 (%fold-right kons knil first 'fold-right))
	((null? (cdr rest))
	 (%fold-right-2 kons knil first (car rest) 'fold-right))
	(else
	 (apply generator-fold-right
		kons
		knil
		(list->generator first)
		(map list->generator rest)))))

(define (fold-right-map kons knil proc first . rest)
  (cond ((null? rest)
	 (%fold-right-map kons knil proc first 'fold-right-map))
	((null? (cdr rest))
	 (%fold-right-map-2 kons knil proc first (car rest) 'fold-right-map))
	(else
	 (apply generator-fold-right-map
		kons
		knil
		proc
		(list->generator first)
		(map list->generator rest)))))

(define-integrable (%fold kons knil elts caller)
  (let loop ((elts elts) (acc knil))
    (if (%null-list? elts caller)
	acc
	(loop (cdr elts) (kons (car elts) acc)))))

(define-integrable (%fold-map kons knil proc elts caller)
  (let loop ((elts elts) (acc knil))
    (if (%null-list? elts caller)
	acc
	(loop (cdr elts) (kons (proc (car elts)) acc)))))

(define-integrable (%fold-right kons knil elts caller)
  (let loop ((elts elts))
    (if (%null-list? elts caller)
	knil
	(kons (car elts) (loop (cdr elts))))))

(define-integrable (%fold-right-map kons knil proc elts caller)
  (let loop ((elts elts))
    (if (%null-list? elts caller)
	knil
	(kons (proc (car elts)) (loop (cdr elts))))))

(define-integrable (%fold-2 kons knil elts1 elts2 caller)
  (let loop ((elts1 elts1) (elts2 elts2) (acc knil))
    (if (%either-null-list? elts1 elts2 caller)
	acc
	(loop (cdr elts1)
	      (cdr elts2)
	      (kons (car elts1) (car elts2) acc)))))

(define-integrable (%fold-map-2 kons knil proc elts1 elts2 caller)
  (let loop ((elts1 elts1) (elts2 elts2) (acc knil))
    (if (%either-null-list? elts1 elts2 caller)
	acc
	(loop (cdr elts1)
	      (cdr elts2)
	      (kons (proc (car elts1) (car elts2)) acc)))))

(define-integrable (%fold-right-2 kons knil elts1 elts2 caller)
  (let loop ((elts1 elts1) (elts2 elts2))
    (if (%either-null-list? elts1 elts2 caller)
	knil
	(kons (car elts1)
	      (car elts2)
	      (loop (cdr elts1) (cdr elts2))))))

(define-integrable (%fold-right-map-2 kons knil proc elts1 elts2 caller)
  (let loop ((elts1 elts1) (elts2 elts2))
    (if (%either-null-list? elts1 elts2 caller)
	knil
	(kons (proc (car elts1) (car elts2))
	      (loop (cdr elts1) (cdr elts2))))))

(define-integrable (%either-null-list? a b caller)
  (let ((va (%null-list? a caller))
	(vb (%null-list? b caller)))
    (if va va vb)))

(define (reduce kons knil list)
  (if (%null-list? list 'reduce)
      knil
      (%fold kons (car list) (cdr list) 'reduce)))

(define (reduce-right kons knil list)
  (if (%null-list? list 'reduce-right)
      knil
      (let loop ((head (car list)) (tail (cdr list)))
	(if (null-list? tail 'reduce-right)
	    head
	    (kons head (loop (car tail) (cdr tail)))))))

;;;; Membership lists

(define (memq item items)
  (%member item items eq? 'memq))

(define (memv item items)
  (%member item items eqv? 'memv))

(define (member item items #!optional =)
  (if (default-object? =)
      (let ()
	(define-integrable (pred a b)
	  (or (eq? a b)
	      (equal? a b)))
	(%member item items pred 'member))
      (%member item items = 'member)))

(define-integrable (%member item items = caller)
  (let loop ((items items))
    (if (%null-list? items caller)
	#f
	(if (= item (car items))
	    items
	    (loop (cdr items))))))

(define (delq item items)
  (%delete item items eq? 'delq))

(define (delv item items)
  (%delete item items eqv? 'delv))

(define (delete item items #!optional =)
  (if (default-object? =)
      (let ()
	(define-integrable (pred a b)
	  (or (eq? a b)
	      (equal? a b)))
	(%delete item items pred 'delete))
      (%delete item items = 'delete)))

(define-integrable (%delete item items = caller)
  (define-integrable (delete? item*)
    (= item item*))
  (%remove delete? items caller))

(define-integrable (%remove delete? items caller)

  (define (scan items prev)
    ;; Set the cdr of prev to be a copy of items with the specified
    ;; elements deleted.  This implementation does _not_ share a common
    ;; tail -- it is a complete copy.
    (cond ((%null-list? items caller)
	   unspecific)
	  ((delete? (car items))
	   (scan (cdr items) prev))
	  (else
	   (let ((pair (cons (car items) '())))
	     (set-cdr! prev pair)
	     (scan (cdr items) pair)))))

  (let skip ((items items))
    ;; Skip an initial run of items to delete.
    (cond ((%null-list? items caller)
	   '())
	  ((delete? (car items))
	   (skip (cdr items)))
	  (else
	   (let ((head (cons (car items) '())))
	     (scan (cdr items) head)
	     head)))))

(define (delq! item items)
  (%delete! item items eq? 'delq!))

(define (delv! item items)
  (%delete! item items eqv? 'delv!))

(define (delete! item items #!optional =)
  (if (default-object? =)
      (let ()
	(define-integrable (pred a b)
	  (or (eq? a b)
	      (equal? a b)))
	(%delete! item items pred 'delete!))
      (%delete! item items = 'delete!)))

(define-integrable (%delete! item items = caller)
  (define-integrable (delete? item*)
    (= item item*))
  (%remove! delete? items caller))

(define-integrable (%remove! delete? items caller)

  (define (scan items)
    ;; Find the next run of items to delete and remember what
    ;; pair's cdr it started at.
    ;;
    ;; (assert (not (delete? (car items))))
    (let ((items (cdr items)) (prev items))
      ;; (assert (not (delete? (car prev))))
      (cond ((%null-list? items caller)
	     unspecific)
	    ((not (delete? (car items)))
	     (scan items))
	    (else
	     (let trim ((items items))
	       ;; Skip a run of items to delete, and set the
	       ;; cdr of prev to the first pair past it.
	       ;;
	       ;; (assert (delete? (car items)))
	       (let ((items (cdr items)))
		 (cond ((%null-list? items caller)
			(set-cdr! prev '()))
		       ((delete? (car items))
			(trim items))
		       (else
			(set-cdr! prev items)
			(scan items)))))))))

  (let skip ((items items))
    ;; Skip an initial run of items to delete and find the first pair
    ;; with an item not to delete, or return null if we're to delete
    ;; them all.
    (cond ((%null-list? items caller)
	   '())
	  ((delete? (car items))
	   (skip (cdr items)))
	  (else
	   ;; We have found the first pair without item, which is the
	   ;; one we will return.  Scrub the rest of the list in place.
	   (scan items)
	   items))))

;;;; Association lists

(define (alist? object)
  (list-of-type? object pair?))

(define (alist-cons key datum alist)
  (cons (cons key datum) alist))

(define-integrable (%null-alist? object caller)
  (cond ((null? object) #t)
	((and (pair? object) (pair? (car object))) #f)
	(else (error:not-a alist? object caller))))

(define (alist-fold kons knil alist #!optional caller)
  (let ((caller (if (default-object? caller) 'alist-fold caller)))
    (declare (no-type-checks))
    (let loop ((this alist) (acc knil))
      (if (%null-alist? this caller)
	  acc
	  (loop (cdr this)
		(kons (caar this)
		      (cdar this)
		      acc))))))

(define (alist-fold-right kons knil alist #!optional caller)
  (let ((caller (if (default-object? caller) 'alist-fold-right caller)))
    (declare (no-type-checks))
    (let loop ((this alist))
      (if (%null-alist? this caller)
	  knil
	  (kons (caar this)
		(cdar this)
		(loop (cdr this)))))))

(define (alist-copy alist)
  (alist-fold-right (lambda (key datum acc)
		      (cons (cons key datum) acc))
		    '()
		    alist
		    'alist-copy))

(define (alist-for-each procedure alist)
  (for-each (lambda (p)
	      (procedure (car p) (cdr p)))
	    alist))

(define (assq key alist)
  (%assoc key alist eq? 'assq))

(define (assv key alist)
  (define-integrable (pred a b)
    (or (eq? a b)
	(eqv? a b)))
  (%assoc key alist pred 'assv))

(define (assoc key alist #!optional =)
  (if (default-object? =)
      (let ()
	(define-integrable (pred a b)
	  (or (eq? a b)
	      (equal? a b)))
	(%assoc key alist pred 'assoc))
      (%assoc key alist = 'assoc)))

(define-integrable (%assoc key alist = caller)
  (declare (no-type-checks))
  (let loop ((alist alist))
    (and (not (%null-alist? alist caller))
	 (if (= key (caar alist))
	     (car alist)
	     (loop (cdr alist))))))

(define (del-assq key alist)
  (%alist-delete key alist eq? 'del-assq))

(define (del-assv key alist)
  (%alist-delete key alist eqv? 'del-assv))

(define (del-assoc key alist)
  (%alist-delete key alist equal? 'del-assoc))

(define (alist-delete key alist #!optional =)
  (let ((= (if (default-object? =) equal? =)))
    (define-integrable (pred a b)
      (or (eq? a b)
	  (= a b)))
    (%alist-delete key alist pred 'alist-delete)))

(define-integrable (%alist-delete key alist = caller)
  (define-integrable (delete? item)
    (if (pair? item)
	(= key (car item))
	(error:not-a alist? alist caller)))
  (%remove delete? alist caller))

(define (del-assq! key alist)
  (%alist-delete! key alist eq? 'del-assq!))

(define (del-assv! key alist)
  (%alist-delete! key alist eqv? 'del-assv!))

(define (del-assoc! key alist)
  (%alist-delete! key alist equal? 'del-assoc!))

(define (alist-delete! key alist #!optional =)
  (let ((= (if (default-object? =) equal? =)))
    (define-integrable (pred a b)
      (or (eq? a b)
	  (= a b)))
    (%alist-delete! key alist pred 'alist-delete!)))

(define-integrable (%alist-delete! key alist = caller)
  (define-integrable (delete? p)
    (if (pair? p)
	(= key (car p))
	(error:not-a alist? alist caller)))
  (%remove! delete? alist caller))

(define ((alist-adjoiner key= kons knil) key datum alist)
  (let loop ((alist alist))
    (if (null-list? alist)
	(list (cons key (kons datum knil)))
	(let ((p (car alist)))
	  (if (key= key (car p))
	      (cons (cons (car p) (kons datum (cdr p)))
		    (cdr alist))
	      (cons p (loop (cdr alist))))))))

(define ((alist-adjoiner! key= kons knil) key datum alist)
  (let ((p
	 (find (lambda (p)
		 (key= key (car p)))
	       alist)))
    (if p
	(begin
	  (set-cdr! p (kons datum (cdr p)))
	  alist)
	(cons (cons key (kons datum knil)) alist))))

;;;; Keyword lists

(define (keyword-list? object)
  (declare (no-type-checks))
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

(define-integrable (%null-keyword-list? object caller)
  (cond ((null? object) #t)
	((and (pair? object) (pair? (cdr object))) #f)
	(else (error:not-a keyword-list? object caller))))

(define (get-keyword-value klist key #!optional default-value)
  (declare (no-type-checks))
  (let loop ((this klist))
    (if (%null-keyword-list? this 'get-keyword-value)
	default-value
	(if (eq? (car this) key)
	    (car (cdr this))
	    (loop (cdr (cdr this)))))))

(define (get-keyword-values klist key)
  (declare (no-type-checks))
  (let loop ((this klist) (values '()))
    (if (%null-keyword-list? this 'get-keyword-values)
	(reverse! values)
	(loop (cdr (cdr this))
	      (if (eq? (car this) key)
		  (cons (car (cdr this)) values)
		  values)))))

(define (keyword-list-fold kons knil klist #!optional caller)
  (let ((caller (if (default-object? caller) 'keyword-list-fold caller)))
    (declare (no-type-checks))
    (let loop ((this klist) (acc knil))
      (if (%null-keyword-list? this caller)
	  acc
	  (loop (cddr this)
		(kons (car this) (cadr this) acc))))))

(define (keyword-list-fold-right kons knil klist #!optional caller)
  (let ((caller (if (default-object? caller) 'keyword-list-fold-right caller)))
    (declare (no-type-checks))
    (let loop ((this klist))
      (if (%null-keyword-list? this caller)
	  knil
	  (kons (car this)
		(cadr this)
		(loop (cddr this)))))))

(define (keyword-list->alist klist)
  (keyword-list-fold-right (lambda (key datum acc)
			     (cons (cons key datum) acc))
			   '()
			   klist
			   'keyword-list->alist))

(define (alist->keyword-list alist)
  (alist-fold-right (lambda (key datum acc)
		      (cons key (cons datum acc)))
		    '()
		    alist
		    'alist->keyword-list))

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
  (and (let ((n (list?->length object)))
	 (and n
	      (or (fix:= 2 n)
		  (fix:= 3 n))))
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
  (declare (no-type-checks))
  (if (not (pair? list))
      (error:not-a pair? list 'last-pair))
  (let loop ((list list))
    (if (pair? (cdr list))
	(loop (cdr list))
	list)))

(define (except-last-pair list)
  (declare (no-type-checks))
  (if (not (pair? list))
      (error:not-a pair? list 'except-last-pair))
  (if (pair? (cdr list))
      (let ((head (cons (car list) '())))
	(let loop ((list (cdr list)) (previous head))
	  (if (pair? (cdr list))
	      (let ((new (cons (car list) '())))
		(set-cdr! previous new)
		(loop (cdr list) new))))
	head)
      '()))

(define (except-last-pair! list)
  (declare (no-type-checks))
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

(define (cons-last item items)
  (declare (no-type-checks))
  (let ((new-last (list item)))
    (if (%null-list? items 'cons-last!)
	new-last
	(let ((head (cons (car items) new-last)))
	  (let loop ((items* (cdr items)) (prev head))
	    (if (not (%null-list? items* 'cons-last!))
		(let ((new (cons (car items*) new-last)))
		  (set-cdr! prev new)
		  (loop (cdr items*) new))))
	  head))))

(define (cons-last! item items)
  (declare (no-type-checks))
  (if (%null-list? items 'cons-last!)
      (list item)
      (begin
	(let loop ((items items))
	  (if (%null-list? (cdr items) 'cons-last!)
	      (set-cdr! items (list item))
	      (loop (cdr items))))
	items)))

;;;; Generalized list operations

(define ((list-deletor predicate) items)
  (remove predicate items))

(define ((list-deletor! predicate) items)
  (remove! predicate items))

(define (any-duplicates? items #!optional = get-key)
  (let ((= (if (default-object? =) equal? =)))
    (define-integrable (pred a b)
      (or (eq? a b)
	  (= a b)))
    (if (default-object? get-key)
	(let loop ((items items))
	  (and (pair? items)
	       (if (%member (car items) (cdr items) pred 'any-duplicates?)
		   #t
		   (loop (cdr items)))))
	(let loop ((items items))
	  (and (pair? items)
	       (or (any (let ((key (get-key (car items))))
			  (lambda (item)
			    (pred key (get-key item))))
			(cdr items))
		   (loop (cdr items))))))))

(define (list-of-unique-symbols? object)
  (and (list-of-type? object symbol?)
       (not (any-duplicates? object eq?))))

(define (member-procedure = #!optional caller)
  (define-integrable (pred a b)
    (or (eq? a b)
	(= a b)))
  (lambda (item items)
    (%member item items pred caller)))

(define (add-member-procedure = #!optional caller)
  (define-integrable (pred a b)
    (or (eq? a b)
	(= a b)))
  (lambda (item items)
    (if (%member item items pred caller)
	items
	(cons item items))))

(define ((delete-member-procedure deletor predicate) item items)
  ((deletor (lambda (match) (predicate match item))) items))

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

;;;; Alist tables

(define %make-alist-table)
(define alist-table?)
(define alist-table-key=)
(define %table-alist)
(define %set-table-alist!)
(seq:after-record 'add-action!
  (lambda ()
    (let ((rt (make-record-type '<alist-table> '(key= alist))))
      (set! %make-alist-table (record-constructor rt))
      (set! alist-table? (record-predicate rt))
      (set! alist-table-key= (record-accessor rt 'key=))
      (set! %table-alist (record-accessor rt 'alist))
      (set! %set-table-alist! (record-modifier rt 'alist))
      unspecific)))

(define (alist-table key=)
  (%make-alist-table key= '()))

(define (alist->alist-table key= alist)
  (%make-alist-table key= (alist-copy alist)))

(define (alist-table-size table)
  (length (%table-alist table)))

(define (alist-table-empty? table)
  (null? (%table-alist table)))

(define (alist-table-fold kons knil table)
  (alist-fold kons knil (%table-alist table)))

(define (alist-table-fold-right kons knil table)
  (alist-fold-right kons knil (%table-alist table)))

(define (alist-table-keys table)
  (map car (%table-alist table)))

(define (alist-table-values table)
  (map cdr (%table-alist table)))

(define (alist-table->alist table)
  (alist-copy (%table-alist table)))

(define (%find-pair table key)
  (let ((key= (alist-table-key= table)))
    (find (lambda (p)
	    (key= key (car p)))
	  (%table-alist table))))

(define (alist-table-contains? table key)
  (and (%find-pair table key) #t))

(define (alist-table-ref table key #!optional fail succeed)
  (let ((p (%find-pair table key)))
    (if p
	(if (default-object? succeed)
	    (cdr p)
	    (succeed (cdr p)))
	(begin
	  (if (default-object? fail)
	      (error:bad-range-argument key 'alist-table-ref))
	  (fail)))))

(define (alist-table-set! table key value)
  (let ((p (%find-pair table key)))
    (if p
	(set-cdr! p value)
	(%set-table-alist! table
			   (cons (cons key value)
				 (%table-alist table))))))

(define (alist-table-update! table key updater #!optional fail succeed)
  (let ((p (%find-pair table key)))
    (if p
	(set-cdr! p
		  (updater
		   (if (default-object? succeed)
		       (cdr p)
		       (succeed (cdr p)))))
	(begin
	  (if (default-object? fail)
	      (error:bad-range-argument key 'alist-table-update!))
	  (%set-table-alist! table
			     (cons (cons key (updater (fail)))
				   (%table-alist table)))))))

(define (alist-table-intern! table key get-value)
  (let ((p (%find-pair table key)))
    (if p
	(cdr p)
	(let ((value (get-value)))
	  (%set-table-alist! table
			     (cons (cons key value)
				   (%table-alist table)))
	  value))))

(define (alist-table-search table predicate if-found if-not-found)
  (let ((p
	 (find (lambda (p)
		 (predicate (car p) (cdr p)))
	       (%table-alist table))))
    (if p
	(if-found (car p) (cdr p))
	(if-not-found))))

(define (alist-table-delete! table key #!optional default)
  (let ((key= (alist-table-key= table)))
    (let loop ((this (%table-alist table)) (prev #f))
      (if (pair? this)
	  (if (key= key (caar this))
	      (begin
		(if prev
		    (set-cdr! prev (cdr this))
		    (%set-table-alist! table (cdr this)))
		(cdar this))
	      (loop (cdr this) this))
	  default))))

(define (alist-table-prune! predicate table)
  (let loop ((this (%table-alist table)) (prev #f))
    (if (pair? this)
	(loop (cdr this)
	      (if (predicate (caar this) (cdar this))
		  (begin
		    (if prev
			(set-cdr! prev (cdr this))
			(%set-table-alist! table (cdr this)))
		    prev)
		  this)))))

(define (alist-table-map! procedure table)
  (for-each (lambda (p)
	      (set-cdr! p (procedure (car p) (cdr p))))
	    (%table-alist table)))

(define (alist-table-clear! table)
  (%set-table-alist! table '()))

;;;; Deprecated

(define (fold-left proc knil first . rest)
  (apply fold
	 (lambda args
	   (apply proc (last args) (except-last-pair args)))
	 knil
	 first
	 rest))

(define (reduce-left proc knil list)
  (guarantee list? list 'reduce-left)
  (if (pair? list)
      (fold-left proc (car list) (cdr list))
      knil))

(define (guarantee-list-of-type object predicate description #!optional caller)
  (declare (ignore description))
  (guarantee-list-of predicate object caller))

(define (guarantee-list->length object #!optional caller)
  (let ((n (list?->length object)))
    (if (not n)
	(error:not-a list? object caller))
    n))

(define (guarantee-list-of-type->length object predicate description
					#!optional caller)
  (declare (ignore description))
  (let ((n (list-of-type?->length object predicate)))
    (if (not n)
	(error:not-a-list-of predicate object caller))
    n))