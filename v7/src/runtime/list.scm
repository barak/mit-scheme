;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1986 Massachusetts Institute of Technology
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

;;; $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/list.scm,v 13.40 1987/01/21 21:39:50 jinx Exp $

(declare (usual-integrations))

;;; This IN-PACKAGE is just a kludge to prevent the definitions of the
;;; primitives from shadowing the USUAL-INTEGRATIONS declaration.
(in-package system-global-environment
(let-syntax ()
  (define-macro (define-primitives . names)
    `(BEGIN ,@(map (lambda (name)
		     `(DEFINE ,name ,(make-primitive-procedure name)))
		   names)))
  (define-primitives
   cons pair? null? length car cdr set-car! set-cdr!
   general-car-cdr memq assq)))

(define (list . elements)
  elements)

(define (list? frob)
  (cond ((null? frob) true)
	((pair? frob) (list? (cdr frob)))
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
  (car (list-tail l n)))

(define (list-tail l n)
  (cond ((zero? n) l)
	((pair? l) (list-tail (cdr l) (-1+ n)))
	(else (error "LIST-TAIL: Argument not a list" l))))

(define the-empty-stream
  '())

(define empty-stream?
  null?)

(define head
  car)

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
	  ((null? head) (loop (car tail) (cdr tail)))
	  ((pair? head)
	   (set-cdr! (last-pair head) (loop (car tail) (cdr tail)))
	   head)
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

(define map)
(define map*)
(let ()

(define (inner-map f lists initial-value)
  (define (loop lists)
    (define (scan lists c)
      (if (null? lists)
	  (c '() '())
	  (let ((list (car lists)))
	    (cond ((null? list) initial-value)
		  ((pair? list)
		   (scan (cdr lists)
			 (lambda (cars cdrs)
			   (c (cons (car list) cars)
			      (cons (cdr list) cdrs)))))
		  (else (error "MAP: Argument not a list" list))))))
    (scan lists
	  (lambda (cars cdrs)
	    (cons (apply f cars) (loop cdrs)))))
  (loop lists))

(set! map
(named-lambda (map f . lists)
  (if (null? lists)
      (error "MAP: Too few arguments" f)
      (inner-map f lists '()))))

(set! map*
(named-lambda (map* initial-value f . lists)
  (if (null? lists)
      (error "MAP*: Too few arguments" initial-value f)
      (inner-map f lists initial-value))))

)

(define (for-each f . lists)
  (define (loop lists)
    (define (scan lists c)
      (if (null? lists)
	  (c '() '())
	  (let ((list (car lists)))
	    (cond ((null? list) '())
		  ((pair? list)
		   (scan (cdr lists)
			 (lambda (cars cdrs)
			   (c (cons (car list) cars)
			      (cons (cdr list) cdrs)))))
		  (else (error "FOR-EACH: Argument not a list" list))))))
    (scan lists
	  (lambda (cars cdrs)
	    (apply f cars)
	    (loop cdrs))))
  (if (null? lists)
      (error "FOR-EACH: Too few arguments" f)
      (loop lists))
  *the-non-printing-object*)

(define mapcar map)
(define mapcar* map*)
(define mapc for-each)

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

(define (positive-list-searcher pred if-win if-lose)
  (define (list-searcher-loop list)
    (if (pair? list)
	(if (pred list)
	    (if-win list)
	    (list-searcher-loop (cdr list)))
	(and if-lose (if-lose))))
  list-searcher-loop)

(define (negative-list-searcher pred if-win if-lose)
  (define (list-searcher-loop list)
    (if (pair? list)
	(if (pred list)
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

;;; Not so general, but useful.

(define (list-deletor pred)
  (negative-list-transformer pred '()))

(define (list-deletor! pred)
  (define (trim-initial-segment list)
    (if (pair? list)
	(if (pred (car list))
	    (trim-initial-segment (cdr list))
	    (begin (locate-initial-segment list (cdr list))
		   list))
	list))
  (define (locate-initial-segment last this)
    (if (pair? this)
	(if (pred (car this))
	    (set-cdr! last (trim-initial-segment (cdr this)))
	    (locate-initial-segment this (cdr this)))
	this))
  trim-initial-segment)

(define (list-transform-positive list predicate)
  ((positive-list-transformer predicate '()) list))

(define (list-transform-negative list predicate)
  ((negative-list-transformer predicate '()) list))

(define (list-search-positive list predicate)
  ((positive-list-searcher (lambda (items)
			     (predicate (car items)))
			   car
			   false)
   list))

(define (list-search-negative list predicate)
  ((negative-list-searcher (lambda (items)
			     (predicate (car items)))
			   car
			   false)
   list))

;;;; Membership Lists

(define ((member-procedure pred) element list)
  ((positive-list-searcher (lambda (sub-list)
			     (pred (car sub-list) element))
			   identity-procedure
			   false)
   list))

;(define memq (member-procedure eq?))
(define memv (member-procedure eqv?))
(define member (member-procedure equal?))

(define ((delete-member-procedure deletor pred) element list)
  ((deletor (lambda (match)
	      (pred match element)))
   list))

(define delq (delete-member-procedure list-deletor eq?))
(define delv (delete-member-procedure list-deletor eqv?))
(define delete (delete-member-procedure list-deletor equal?))

(define delq! (delete-member-procedure list-deletor! eq?))
(define delv! (delete-member-procedure list-deletor! eqv?))
(define delete! (delete-member-procedure list-deletor! equal?))

;;;; Association Lists

(define ((association-procedure pred selector) key alist)
  ((positive-list-searcher (lambda (sub-alist)
			     (pred (selector (car sub-alist)) key))
			   car
			   false)
   alist))

;(define assq (association-procedure eq? car))
(define assv (association-procedure eqv? car))
(define assoc (association-procedure equal? car))

(define ((delete-association-procedure deletor pred selector) key alist)
  ((deletor (lambda (association)
	      (pred (selector association) key)))
   alist))

(define del-assq (delete-association-procedure list-deletor eq? car))
(define del-assv (delete-association-procedure list-deletor eqv? car))
(define del-assoc (delete-association-procedure list-deletor equal? car))

(define del-assq! (delete-association-procedure list-deletor! eq? car))
(define del-assv! (delete-association-procedure list-deletor! eqv? car))
(define del-assoc! (delete-association-procedure list-deletor! equal? car))

;;;; Lastness

(define (last-pair l)
  (define (loop l)
    (if (pair? (cdr l))
	(loop (cdr l))
	l))
  (if (pair? l)
      (loop l)
      (error "LAST-PAIR: Argument not a list" l)))

(define (except-last-pair l)
  (define (loop l)
    (if (pair? (cdr l))
	(cons (car l)
	      (loop (cdr l)))
	'()))
  (if (pair? l)
      (loop l)
      (error "EXCEPT-LAST-PAIR: Argument not a list" l)))

(define (except-last-pair! l)
  (define (loop l)
    (if (pair? (cddr l))
	(loop (cdr l))
	(set-cdr! l '())))
  (if (pair? l)
      (if (pair? (cdr l))
	  (begin (loop l)
		 l)
	  '())
      (error "EXCEPT-LAST-PAIR!: Argument not a list" l)))