#| -*-Scheme-*-

$Id: class.scm,v 1.17 2003/07/22 02:35:06 cph Exp $

Copyright 1995,1997,2002,2002,2003 Massachusetts Institute of Technology

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

;;;; Classes

(declare (usual-integrations))

(define-structure (class (type-descriptor class-rtd)
			 (conc-name class/)
			 (constructor %make-class
				      (name direct-superclasses direct-slots))
			 (print-procedure
			  (standard-unparser-method 'CLASS
			    (lambda (class port)
			      (let ((name (class-name class)))
				(if name
				    (begin
				      (write-char #\space port)
				      (write name port))))))))
  (name #f read-only #t)
  (direct-superclasses #f read-only #t)
  (direct-slots #f read-only #t)
  precedence-list
  slots
  dispatch-tag)

(define (make-class name direct-superclasses direct-slots)
  (if (not (list-of-type? direct-superclasses class?))
      (error:wrong-type-argument direct-superclasses
				 "list of classes"
				 'MAKE-CLASS))
  (if (not (list? direct-slots))
      (error:wrong-type-argument direct-slots "list" 'MAKE-CLASS))
  (let ((class
	 (%make-class name
		      (if (null? direct-superclasses)
			  (list <instance>)
			  direct-superclasses)
		      (map (lambda (slot)
			     (canonicalize-slot-argument slot 'MAKE-CLASS))
			   direct-slots))))
    (set-class/precedence-list! class (compute-precedence-list class))
    (set-class/slots! class (compute-slots class))
    (set-class/dispatch-tag! class (make-dispatch-tag class))
    (install-slot-accessor-methods class)
    class))

(define (make-trivial-subclass superclass . superclasses)
  (make-class (class-name superclass) (cons superclass superclasses) '()))

(define <object>
  (let ((class (%make-class '<OBJECT> '() '())))
    (set-class/precedence-list! class (list class))
    (set-class/slots! class '())
    (set-class/dispatch-tag! class (make-dispatch-tag class))
    class))

(define (class-name class)
  (class/name (guarantee-class class 'CLASS-NAME)))

(define (class-direct-superclasses class)
  (class/direct-superclasses
   (guarantee-class class 'CLASS-DIRECT-SUPERCLASSES)))

(define (class-direct-slot-names class)
  (map car (class/direct-slots (guarantee-class class 'CLASS-DIRECT-SLOTS))))

(define (class-precedence-list class)
  (class/precedence-list (guarantee-class class 'CLASS-PRECEDENCE-LIST)))

(define (class-slots class)
  (class/slots (guarantee-class class 'CLASS-SLOTS)))

(define (class-slot class name error?)
  (or (list-search-positive (class/slots (guarantee-class class 'CLASS-SLOT))
	(lambda (slot)
	  (eq? name (slot-name slot))))
      (and error?
	   (class-slot class (error:no-such-slot class name) error?))))

(define (class->dispatch-tag class)
  (class/dispatch-tag (guarantee-class class 'CLASS->DISPATCH-TAG)))

(define (subclass? c s)
  (let ((pl (class-precedence-list c)))
    (and (there-exists? (specializer-classes s)
	   (lambda (s)
	     (memq s pl)))
	 #t)))

(define (guarantee-class class name)
  (cond ((class? class) class)
	((record-type? class) (record-type-class class))
	(else (error:wrong-type-argument class "class" name))))

(define (compute-precedence-list class)
  (let ((elements (build-transitive-closure class/direct-superclasses class)))
    (topological-sort
     elements
     (build-constraints class/direct-superclasses elements)
     (lambda (partial-cpl elements)
       (let loop ((partial-cpl (reverse partial-cpl)))
	 (if (not (pair? partial-cpl))
	     (error:bad-range-argument class 'COMPUTE-PRECEDENCE-LIST))
	 (let ((ds-of-ce (class/direct-superclasses (car partial-cpl))))
	   (let find-common ((elements elements))
	     (if (pair? elements)
		 (if (memq (car elements) ds-of-ce)
		     (car elements)
		     (find-common (cdr elements)))
		 (loop (cdr partial-cpl))))))))))

(define (compute-slots class)
  (let loop
      ((slots (append-map class/direct-slots (class/precedence-list class)))
       (index 1)
       (descriptors '()))
    (if (pair? slots)
	(let ((slot (car slots)))
	  (let ((name (car slot)))
	    (let inner ((slots (cdr slots)) (same '()) (diff '()))
	      (if (pair? slots)
		  (if (eq? name (caar slots))
		      (inner (cdr slots)
			     (cons (car slots) same)
			     diff)
		      (inner (cdr slots)
			     same
			     (cons (car slots) diff)))
		  (loop (reverse! diff)
			(+ index 1)
			(cons (compute-slot-descriptor
			       class
			       (cons slot (reverse! same))
			       index)
			      descriptors))))))
	(reverse! descriptors))))

;;;; Topological Sort

;;; Topologically sort a list of ELEMENTS.  CONSTRAINTS is the partial
;;; order, expressed as a list of pairs (X . Y) where X precedes Y.
;;; TIE-BREAKER is a procedure that is called when it is necessary to
;;; choose from multiple minimal elements; it is called with the
;;; partial result and the set of minimal elements as its arguments.

(define (topological-sort elements original-constraints tie-breaker)
  (let ((result (cons '() '())))
    (let ((add-to-result
	   (lambda (element)
	     (let ((tail (list element)))
	       (if (null? (car result))
		   (set-car! result tail)
		   (set-cdr! (cdr result) tail))
	       (set-cdr! result tail)))))
      (let loop
	  ((elements (list-copy elements))
	   (constraints (list-copy original-constraints)))
	(if (null? elements)
	    (car result)
	    (let ((minimal
		   (remove-if (lambda (element)
				(let loop ((constraints constraints))
				  (and (pair? constraints)
				       (or (eq? (cdar constraints) element)
					   (loop (cdr constraints))))))
			      elements)))
	      (if (null? minimal)
		  (error:bad-range-argument original-constraints
					    'TOPOLOGICAL-SORT))
	      (let ((elements
		     (remove-if! (lambda (element)
				   (memq element minimal))
				 elements))
		    (constraints
		     (remove-if! (lambda (constraint)
				   (or (memq (car constraint) minimal)
				       (memq (cdr constraint) minimal)))
				 constraints)))
		(let break-ties ((minimal minimal))
		  (if (null? (cdr minimal))
		      (let ((choice (car minimal)))
			(add-to-result choice)
			(loop elements constraints))
		      (let ((choice (tie-breaker (car result) minimal)))
			(add-to-result choice)
			(break-ties (remove-item! choice minimal))))))))))))

(define (build-transitive-closure get-follow-ons element)
  (let loop ((result '()) (pending (list element)))
    (if (pair? pending)
	(if (memq (car pending) result)
	    (loop result (cdr pending))
	    (loop (cons (car pending) result)
		  (append (get-follow-ons (car pending)) (cdr pending))))
	result)))

(define (build-constraints get-follow-ons elements)
  (let loop ((elements elements) (result '()))
    (if (pair? elements)
	(loop (cdr elements)
	      (let loop
		  ((element (car elements))
		   (follow-ons (get-follow-ons (car elements))))
		(if (pair? follow-ons)
		    (cons (cons element (car follow-ons))
			  (loop (car follow-ons) (cdr follow-ons)))
		    result)))
	result)))

(define (remove-if predicate items)
  (let loop ((items items))
    (if (pair? items)
	(if (predicate (car items))
	    (loop (cdr items))
	    (cons (car items) (loop (cdr items))))
	'())))

(define (remove-if! predicate items)
  (letrec ((trim-initial-segment
	    (lambda (items)
	      (if (pair? items)
		  (if (predicate (car items))
		      (trim-initial-segment (cdr items))
		      (begin
			(locate-initial-segment items (cdr items))
			items))
		  items)))
	   (locate-initial-segment
	    (lambda (last this)
	      (if (pair? this)
		  (if (predicate (car this))
		      (set-cdr! last (trim-initial-segment (cdr this)))
		      (locate-initial-segment this (cdr this)))
		  this))))
    (trim-initial-segment items)))

(define (remove-item! item items)
  (if (pair? items)
      (if (eq? item (car items))
	  (cdr items)
	  (begin
	    (let loop ((last items) (this (cdr items)))
	      (if (pair? this)
		  (if (eq? item (car this))
		      (set-cdr! last (cdr this))
		      (loop this (cdr this)))))
	    items))
      items))

;;;; Built-in Classes

(define <instance> (make-class '<INSTANCE> (list <object>) '()))

(define-syntax define-primitive-class
  (syntax-rules ()
    ((define-primitive-class name superclass ...)
     (define name
       (make-class 'name (list superclass ...) '())))))

(define-primitive-class <boolean> <object>)
(define-primitive-class <char> <object>)
(define-primitive-class <pair> <object>)
(define-primitive-class <%record> <object>)
(define-primitive-class <record> <%record>)
(define-primitive-class <dispatch-tag> <%record>)
(define-primitive-class <string> <object>)
(define-primitive-class <symbol> <object>)
(define-primitive-class <vector> <object>)

(define-primitive-class <number>)
(define-primitive-class <complex> <number>)
(define-primitive-class <real> <complex>)
(define-primitive-class <rational> <real>)
(define-primitive-class <integer> <rational>)

(define-primitive-class <exact> <number>)
(define-primitive-class <exact-complex> <complex> <exact>)
(define-primitive-class <exact-real> <real> <exact-complex>)
(define-primitive-class <exact-rational> <rational> <exact-real>)
(define-primitive-class <exact-integer> <integer> <exact-rational>)

(define-primitive-class <inexact> <number>)
(define-primitive-class <inexact-complex> <complex> <inexact>)
(define-primitive-class <inexact-real> <real> <inexact-complex>)
(define-primitive-class <inexact-rational> <rational> <inexact-real>)
(define-primitive-class <inexact-integer> <integer> <inexact-rational>)

(define-primitive-class <fixnum> <exact-integer>)
(define-primitive-class <bignum> <exact-integer>)
(define-primitive-class <ratnum> <exact-rational>)
(define-primitive-class <flonum> <inexact-rational>)
(define-primitive-class <flonum-vector> <flonum>)
(define-primitive-class <recnum> <complex>)

(define-primitive-class <procedure> <object>)
(define-primitive-class <generic-procedure> <procedure>)
(define-primitive-class <entity> <procedure>)

(define (object-class object)
  (dispatch-tag->class (dispatch-tag object)))

(define (record-type-class type)
  (dispatch-tag->class (record-type-dispatch-tag type)))

(define (record-class record)
  (record-type-class (record-type-descriptor record)))

(define (dispatch-tag->class tag)
  (let ((contents (dispatch-tag-contents tag)))
    (cond ((class? contents) contents)
	  ((hash-table/get built-in-class-table tag #f))
	  ((record-type? contents)
	   (let ((class (make-record-type-class contents)))
	     (hash-table/put! built-in-class-table tag class)
	     class))
	  (else <object>))))

(define (make-record-type-class type)
  (let ((class
	 (make-class (string->symbol
		      (string-append "<" (record-type-name type) ">"))
		     (list <record>)
		     (record-type-field-names type))))
    (set-class/dispatch-tag! class (record-type-dispatch-tag type))
    class))

(define built-in-class-table
  (make-eq-hash-table))

(let ((assign-type
       (lambda (name class)
	 (hash-table/put! built-in-class-table
			  (or (built-in-dispatch-tag name)
			      (built-in-dispatch-tag
			       (microcode-type/code->name
				(microcode-type/name->code name)))
			      (error "Unknown type name:" name))
			  class))))
  (assign-type 'BOOLEAN <boolean>)
  (assign-type 'CHARACTER <char>)
  (assign-type 'PAIR <pair>)
  (assign-type 'RECORD <%record>)
  (assign-type 'DISPATCH-TAG <dispatch-tag>)
  (assign-type 'STRING <string>)
  (assign-type 'INTERNED-SYMBOL <symbol>)
  (assign-type 'UNINTERNED-SYMBOL <symbol>)
  (assign-type 'VECTOR <vector>)

  (assign-type 'COMPILED-PROCEDURE <procedure>)
  (assign-type 'EXTENDED-PROCEDURE <procedure>)
  (assign-type 'PRIMITIVE <procedure>)
  (assign-type 'PROCEDURE <procedure>)
  (assign-type 'ENTITY <entity>)

  (if (fix:= (object-type 1) (object-type -1))
      (assign-type 'FIXNUM <fixnum>)
      (begin
	(assign-type 'POSITIVE-FIXNUM <fixnum>)
	(assign-type 'NEGATIVE-FIXNUM <fixnum>)))
  (assign-type 'BIGNUM <bignum>)
  (assign-type 'RATNUM <ratnum>)
  (assign-type 'FLONUM <flonum>)
  (assign-type 'FLONUM-VECTOR <flonum-vector>)
  (assign-type 'RECNUM <recnum>))

(hash-table/put! built-in-class-table
		 standard-generic-procedure-tag
		 <generic-procedure>)

(define <class> (object-class <object>))