#| -*-Scheme-*-

$Id: slot.scm,v 1.9 2003/02/14 18:25:21 cph Exp $

Copyright 1995-1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

;;;; Instance Slots

(declare (usual-integrations))

(define-structure (slot-descriptor (conc-name slot-descriptor/))
  (name #f read-only #t)
  (class #f read-only #t)
  (index #f read-only #t)
  (properties #f))

(define (slot-name slot)
  (guarantee-slot-descriptor slot 'SLOT-NAME)
  (slot-descriptor/name slot))

(define (slot-class slot)
  (guarantee-slot-descriptor slot 'SLOT-CLASS)
  (slot-descriptor/class slot))

(define (slot-index slot)
  (guarantee-slot-descriptor slot 'SLOT-INDEX)
  (slot-descriptor/index slot))

(define (slot-property slot key default)
  (let ((entry (assq key (slot-descriptor/properties slot))))
    (if entry
	(cdr entry)
	default)))

(define (slot-properties slot)
  (alist-copy (slot-descriptor/properties slot)))

(define (slot-initializer slot)
  (slot-property slot 'INITIALIZER #f))

(define (slot-initial-value slot)
  (slot-property slot 'INITIAL-VALUE record-slot-uninitialized))

(define (slot-initial-value? slot)
  (not (eq? record-slot-uninitialized (slot-initial-value slot))))

(define (guarantee-slot-descriptor slot name)
  (if (not (slot-descriptor? slot))
      (error:wrong-type-argument slot "slot descriptor" name)))

(add-generic-procedure-generator %record-slot-index
  (lambda (generic tags)
    generic
    (and (class? (dispatch-tag-contents (car tags)))
	 (lambda (instance name)
	   (let ((slot (class-slot (object-class instance) name #f)))
	     (and slot
		  (slot-index slot)))))))

(add-generic-procedure-generator %record-slot-names
  (lambda (generic tags)
    generic
    (and (class? (dispatch-tag-contents (car tags)))
	 (lambda (instance)
	   (map slot-name (class-slots (object-class instance)))))))

;;;; Slot Accessors

(define (method-constructor make-generator)
  (letrec
      ((constructor
	(lambda (class name)
	  (if (class-slot class name #f)
	      (make-computed-method (list class)
		(let ((generator (make-generator name)))
		  (lambda classes
		    (generator #f (map class->dispatch-tag classes)))))
	      (constructor class (error:no-such-slot class name))))))
    constructor))

(define slot-accessor-method (method-constructor %record-accessor-generator))
(define slot-modifier-method (method-constructor %record-modifier-generator))
(define slot-initpred-method (method-constructor %record-initpred-generator))

(define (accessor-constructor arity make-method)
  (lambda (class name)
    (let ((generic (make-generic-procedure arity)))
      (add-method generic (make-method class name))
      generic)))

(define slot-accessor (accessor-constructor 1 slot-accessor-method))
(define slot-modifier (accessor-constructor 2 slot-modifier-method))
(define slot-initpred (accessor-constructor 1 slot-initpred-method))

(define (install-slot-accessor-methods class)
  (for-each
   (lambda (name)
     (let* ((slot (class-slot class name #t))
	    (install
	     (lambda (keyword maker)
	       (let ((accessor (slot-property slot keyword #f)))
		 (if accessor
		     (begin
		       (add-method accessor (maker class name))
		       (set-slot-descriptor/properties!
			slot
			(del-assq! keyword
				   (slot-descriptor/properties slot)))))))))
       (install 'ACCESSOR slot-accessor-method)
       (install 'MODIFIER slot-modifier-method)
       (install 'INITPRED slot-initpred-method)))
   (class-direct-slot-names class)))

(define (slot-value object name)
  (%record-ref object (compute-slot-index object name)))

(define (set-slot-value! object name value)
  (%record-set! object (compute-slot-index object name) value))

(define (slot-initialized? object name)
  (not (eq? record-slot-uninitialized
	    (%record-ref object (compute-slot-index object name)))))

(define (compute-slot-index object name)
  (or (%record-slot-index object name)
      (error:no-such-slot (object-class object) name)))

;;;; Slot Arguments

(define (canonicalize-slot-argument argument caller)
  (cond ((symbol? argument)
	 (list argument))
	((and (pair? argument)
	      (symbol? (car argument))
	      (slot-argument-plist? (cdr argument)))
	 argument)
	(else
	 (error:bad-range-argument argument caller))))

(define (slot-argument-plist? object)
  (let loop ((l1 object) (l2 object))
    (if (pair? l1)
	(and (not (eq? (cdr l1) l2))
	     (symbol? (car l1))
	     (pair? (cdr l1))
	     (loop (cddr l1) (cdr l2)))
	(null? l1))))

(define (compute-slot-descriptor class slots index)
  (let ((slot (merge-slot-arguments slots)))
    (make-slot-descriptor (car slot) class index (cdr slot))))

(define (merge-slot-arguments slots)
  (let ((slots
	 (reverse!
	  (map (lambda (slot)
		 (cons (car slot)
		       (plist->alist (cdr slot))))
	       slots))))
    (let ((result (car slots)))
      (for-each
       (lambda (slot)
	 (for-each
	  (lambda (x)
	    (let ((names
		   (or (list-search-positive interacting-options
			 (lambda (names)
			   (memq (car x) names)))
		       (list (car x)))))
	      (let ((entry
		     (let loop ((names names))
		       (and (not (null? names))
			    (or (assq (car names) (cdr result))
				(loop (cdr names)))))))
		(if entry
		    (begin
		      (set-car! entry (car x))
		      (set-cdr! entry (cdr x)))
		    (set-cdr! result (cons x (cdr result)))))))
	  (cdr slot)))
       (cdr slots))
      result)))

(define interacting-options
  '((INITIAL-VALUE INITIALIZER)))

(define (plist->alist plist)
  (let loop ((plist plist) (alist '()))
    (if (null? plist)
	alist
	(loop (cddr plist)
	      (cons (cons (car plist) (cadr plist)) alist)))))