;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/list.scm,v 13.43 1988/05/03 18:55:13 jinx Exp $
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; List Operations

(declare (usual-integrations))

;;; This IN-PACKAGE is just a kludge to prevent the definitions of the
;;; primitives from shadowing the USUAL-INTEGRATIONS declaration.
#| Temporarily relocated to `boot.scm' to help compiler.
(in-package system-global-environment
(let-syntax ()
  (define-macro (define-primitives . names)
    `(BEGIN ,@(map (lambda (name)
		     `(DEFINE ,name ,(make-primitive-procedure name)))
		   names)))
  (define-primitives
   cons pair? null? length car cdr set-car! set-cdr!
   general-car-cdr memq assq)))|#

(define (list . elements)
  elements)

(define (list? frob)
  (cond ((pair? frob) (list? (cdr frob)))
	((null? frob) true)
	(else false)))

(define (cons* first-element . rest-elements)
  (define (loop this-element rest-elements)
    (if (null? rest-elements)
	this-element
	(cons this-element
	      (loop (car rest-elements)
		    (cdr rest-elements)))))
  (loop first-element rest-elements))

(define (make-list size #!optional value)
  (subvector->list (vector-cons size (if (unassigned? value) '() value))
		   0
		   size))

(define (list-copy elements)
  (apply list elements))

(define (list-ref l n)
  (cond ((not (pair? l)) (error "LIST-REF: Bad argument" l n))
	((zero? n) (car l))
	(else (list-ref (cdr l) (-1+ n)))))

(define (list-tail l n)
  (cond ((zero? n) l)
	((pair? l) (list-tail (cdr l) (-1+ n)))
	(else (error "LIST-TAIL: Bad argument" l))))

(define the-empty-stream '())
(define empty-stream? null?)
(define head car)

(define (tail stream)
  (force (cdr stream)))

;;;; Standard Selectors

(define (cddr x) (general-car-cdr x #o4))
(define (cdar x) (general-car-cdr x #o5))
(define (cadr x) (general-car-cdr x #o6))
(define (caar x) (general-car-cdr x #o7))

(define (cdddr x) (general-car-cdr x #o10))
(define (cddar x) (general-car-cdr x #o11))
(define (cdadr x) (general-car-cdr x #o12))
(define (cdaar x) (general-car-cdr x #o13))
(define (caddr x) (general-car-cdr x #o14))
(define (cadar x) (general-car-cdr x #o15))
(define (caadr x) (general-car-cdr x #o16))
(define (caaar x) (general-car-cdr x #o17))

(define (cddddr x) (general-car-cdr x #o20))
(define (cdddar x) (general-car-cdr x #o21))
(define (cddadr x) (general-car-cdr x #o22))
(define (cddaar x) (general-car-cdr x #o23))
(define (cdaddr x) (general-car-cdr x #o24))
(define (cdadar x) (general-car-cdr x #o25))
(define (cdaadr x) (general-car-cdr x #o26))
(define (cdaaar x) (general-car-cdr x #o27))
(define (cadddr x) (general-car-cdr x #o30))
(define (caddar x) (general-car-cdr x #o31))
(define (cadadr x) (general-car-cdr x #o32))
(define (cadaar x) (general-car-cdr x #o33))
(define (caaddr x) (general-car-cdr x #o34))
(define (caadar x) (general-car-cdr x #o35))
(define (caaadr x) (general-car-cdr x #o36))
(define (caaaar x) (general-car-cdr x #o37))

(define first car)
(define (second x) (general-car-cdr x #o6))
(define (third x) (general-car-cdr x #o14))
(define (fourth x) (general-car-cdr x #o30))
(define (fifth x) (general-car-cdr x #o60))
(define (sixth x) (general-car-cdr x #o140))
(define (seventh x) (general-car-cdr x #o300))
(define (eighth x) (general-car-cdr x #o600))

;;;; Sequence Operations

(define (append . lists)
  (define (outer current remaining)
    (define (inner list)
      (cond ((pair? list) (cons (car list) (inner (cdr list))))
	    ((null? list) (outer (car remaining) (cdr remaining)))
	    (else (error "APPEND: Argument not a list" current))))
    (if (null? remaining)
	current
	(inner current)))
  (if (null? lists)
      '()
      (outer (car lists) (cdr lists))))

(define (append! . lists)
  (define (loop head tail)
    (cond ((null? tail) head)
	  ((pair? head)
	   (set-cdr! (last-pair head) (loop (car tail) (cdr tail)))
	   head)
	  ((null? head) (loop (car tail) (cdr tail)))
	  (else (error "APPEND!: Argument not a list" head))))
  (if (null? lists)
      '()
      (loop (car lists) (cdr lists))))

(define (reverse l)
  (define (loop rest so-far)
    (cond ((pair? rest) (loop (cdr rest) (cons (car rest) so-far)))
	  ((null? rest) so-far)
	  (else (error "REVERSE: Argument not a list" l))))
  (loop l '()))

(define (reverse! l)
  (define (loop current new-cdr)
    (cond ((pair? current) (loop (set-cdr! current new-cdr) current))
	  ((null? current) new-cdr)
	  (else (error "REVERSE!: Argument not a list" l))))
  (loop l '()))

;;;; Mapping Procedures

(define (map f . lists)
  (cond ((null? lists)
	 (error "MAP: Too few arguments" f))
	((null? (cdr lists))
	 (let 1-loop ((list (car lists)))
	   (cond ((pair? list)
		  (cons (f (car list))
			(1-loop (cdr list))))
		 ((null? list)
		  '())
		 (else
		  (error "MAP: Argument not a list" (car lists))))))
	(else
	 (let n-loop ((lists lists))
	   (let parse-cars
	       ((lists lists)
		(receiver
		 (lambda (cars cdrs)
		   (cons (apply f cars)
			 (n-loop cdrs)))))
	     (cond ((null? lists)
		    (receiver '() '()))
		   ((null? (car lists))
		    '())
		   ((pair? (car lists))
		    (parse-cars (cdr lists)
				(lambda (cars cdrs)
				  (receiver (cons (car (car lists)) cars)
					    (cons (cdr (car lists)) cdrs)))))
		   (else
		    (error "MAP: Argument not a list" (car lists)))))))))

(define (map* initial-value f . lists)
  (cond ((null? lists)
	 (error "MAP*: Too few arguments" f))
	((null? (cdr lists))
	 (let 1-loop ((list (car lists)))
	   (cond ((pair? list)
		  (cons (f (car list))		  
			(1-loop (cdr list))))
		 ((null? list)
		  initial-value)
		 (else
		  (error "MAP*: Argument not a list" (car lists))))))
	(else
	 (let n-loop ((lists lists))
	   (let parse-cars
	       ((lists lists)
		(receiver
		 (lambda (cars cdrs)
		   (cons (apply f cars)
			 (n-loop cdrs)))))
	     (cond ((null? lists)
		    (receiver '() '()))
		   ((null? (car lists))
		    initial-value)
		   ((pair? (car lists))
		    (parse-cars (cdr lists)
				(lambda (cars cdrs)
				  (receiver (cons (car (car lists)) cars)
					    (cons (cdr (car lists)) cdrs)))))
		   (else
		    (error "MAP*: Argument not a list" (car lists)))))))))

(define (for-each f . lists)
  (cond ((null? lists)
	 (error "FOR-EACH: Too few arguments" f))
	((null? (cdr lists))
	 (let 1-loop ((list (car lists)))
	   (cond ((pair? list)
		  (f (car list))
		  (1-loop (cdr list)))
		 ((null? list)
		  *the-non-printing-object*)
		 (else
		  (error "FOR-EACH: Argument not a list" (car lists))))))
	(else
	 (let n-loop ((lists lists))
	   (let parse-cars
	       ((lists lists)
		(receiver
		 (lambda (cars cdrs)
		   (apply f cars)
		   (n-loop cdrs))))
	     (cond ((null? lists)
		    (receiver '() '()))
		   ((null? (car lists))
		    *the-non-printing-object*)
		   ((pair? (car lists))
		    (parse-cars (cdr lists)
				(lambda (cars cdrs)
				  (receiver (cons (car (car lists)) cars)
					    (cons (cdr (car lists)) cdrs)))))
		   (else
		    (error "FOR-EACH: Argument not a list" (car lists)))))))))

(define mapcar map)
(define mapcar* map*)
(define mapc for-each)

(define (reduce f initial list)
  (define (loop value l)
    (cond ((pair? l)
	   (loop (f value (car l))
		 (cdr l)))
	  ((null? l)
	   value)
	  (else
	   (error "REDUCE: Argument not a list" list))))
  (loop initial list))  
  
(define (there-exists? predicate)
  (define (loop objects)
    (and (pair? objects)
	 (or (predicate (car objects))
	     (loop (cdr objects)))))
  loop)

(define (for-all? predicate)
  (define (loop objects)
    (if (pair? objects)
	(and (predicate (car objects))
	     (loop (cdr objects)))
	true))
  loop)

;;;; Generalized List Operations

(define (positive-list-searcher predicate if-win if-lose)
  (define (list-searcher-loop list)
    (if (pair? list)
	(if (predicate list)
	    (if-win list)
	    (list-searcher-loop (cdr list)))
	(and if-lose (if-lose))))
  list-searcher-loop)

(define (negative-list-searcher predicate if-win if-lose)
  (define (list-searcher-loop list)
    (if (pair? list)
	(if (predicate list)
	    (list-searcher-loop (cdr list))
	    (if-win list))
	(and if-lose (if-lose))))
  list-searcher-loop)

(define (positive-list-transformer predicate tail)
  (define (list-transform-loop list)
    (if (pair? list)
	(if (predicate (car list))
	    (cons (car list)
		  (list-transform-loop (cdr list)))
	    (list-transform-loop (cdr list)))
	tail))
  list-transform-loop)

(define (negative-list-transformer predicate tail)
  (define (list-transform-loop list)
    (if (pair? list)
	(if (predicate (car list))
	    (list-transform-loop (cdr list))
	    (cons (car list)
		  (list-transform-loop (cdr list))))
	tail))
  list-transform-loop)

(define (list-deletor predicate)
  (define (list-deletor-loop list)
    (if (pair? list)
	(if (predicate (car list))
	    (list-deletor-loop (cdr list))
	    (cons (car list) (list-deletor-loop (cdr list))))
	'()))
  list-deletor-loop)

(define (list-deletor! predicate)
  (define (trim-initial-segment list)
    (if (pair? list)
	(if (predicate (car list))
	    (trim-initial-segment (cdr list))
	    (begin (locate-initial-segment list (cdr list))
		   list))
	list))
  (define (locate-initial-segment last this)
    (if (pair? this)
	(if (predicate (car this))
	    (set-cdr! last (trim-initial-segment (cdr this)))
	    (locate-initial-segment this (cdr this)))
	this))
  trim-initial-segment)

(define (list-transform-positive list predicate)
  (let loop ((list list))
    (if (pair? list)
	(if (predicate (car list))
	    (cons (car list) (loop (cdr list)))
	    (loop (cdr list)))
	'())))

(define (list-transform-negative list predicate)
  (let loop ((list list))
    (if (pair? list)
	(if (predicate (car list))
	    (loop (cdr list))
	    (cons (car list) (loop (cdr list))))
	'())))

(define (list-search-positive list predicate)
  (let loop ((list list))
    (and (pair? list)
	 (if (predicate (car list))
	     (car list)
	     (loop (cdr list))))))

(define (list-search-negative list predicate)
  (let loop ((list list))
    (and (pair? list)
	 (if (predicate (car list))
	     (loop (cdr list))
	     (car list)))))

;;;; Membership Lists

(define (member-procedure predicate)
  (lambda (element list)
    (let loop ((list list))
      (and (pair? list)
	   (if (predicate (car list) element)
	       list
	       (loop (cdr list)))))))

;(define memq (member-procedure eq?))
(define memv (member-procedure eqv?))
(define member (member-procedure equal?))

(define (delete-member-procedure deletor predicate)
  (lambda (element list)
    ((deletor (lambda (match)
		(predicate match element)))
     list)))

(define delq (delete-member-procedure list-deletor eq?))
(define delv (delete-member-procedure list-deletor eqv?))
(define delete (delete-member-procedure list-deletor equal?))

(define delq! (delete-member-procedure list-deletor! eq?))
(define delv! (delete-member-procedure list-deletor! eqv?))
(define delete! (delete-member-procedure list-deletor! equal?))

;;;; Association Lists

(define (association-procedure predicate selector)
  (lambda (key alist)
    (let loop ((alist alist))
      (and (pair? alist)
	   (if (predicate (selector (car alist)) key)
	       (car alist)
	       (loop (cdr alist)))))))

;(define assq (association-procedure eq? car))
(define assv (association-procedure eqv? car))
(define assoc (association-procedure equal? car))

(define ((delete-association-procedure deletor predicate selector) key alist)
  ((deletor (lambda (association)
	      (predicate (selector association) key)))
   alist))

(define del-assq (delete-association-procedure list-deletor eq? car))
(define del-assv (delete-association-procedure list-deletor eqv? car))
(define del-assoc (delete-association-procedure list-deletor equal? car))

(define del-assq! (delete-association-procedure list-deletor! eq? car))
(define del-assv! (delete-association-procedure list-deletor! eqv? car))
(define del-assoc! (delete-association-procedure list-deletor! equal? car))

;;;; Lastness

(define (last-pair l)
  (if (pair? l)
      (let loop ((l l))
	(if (pair? (cdr l))
	    (loop (cdr l))
	    l))
      (error "LAST-PAIR: Argument not a list" l)))

(define (except-last-pair l)
  (if (pair? l)
      (let loop ((l l))
	(if (pair? (cdr l))
	    (cons (car l)
		  (loop (cdr l)))
	    '()))
      (error "EXCEPT-LAST-PAIR: Argument not a list" l)))

(define (except-last-pair! l)
  (if (pair? l)
      (if (pair? (cdr l))
	  (begin (let loop ((l l))
		   (if (pair? (cddr l))
		       (loop (cdr l))
		       (set-cdr! l '())))
		 l)
	  '())
      (error "EXCEPT-LAST-PAIR!: Argument not a list" l)))