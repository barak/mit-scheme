;;; -*-Scheme-*-
;;;
;;; $Id: slot.scm,v 1.2 1997/06/17 08:10:41 cph Exp $
;;;
;;; Copyright (c) 1995-96 Massachusetts Institute of Technology
;;;
;;; This material was developed by the Scheme project at the
;;; Massachusetts Institute of Technology, Department of Electrical
;;; Engineering and Computer Science.  Permission to copy this
;;; software, to redistribute it, and to use it for any purpose is
;;; granted, subject to the following restrictions and understandings.
;;;
;;; 1. Any copy made of this software must include this copyright
;;; notice in full.
;;;
;;; 2. Users of this software agree to make their best efforts (a) to
;;; return to the MIT Scheme project any improvements or extensions
;;; that they make, so that these may be included in future releases;
;;; and (b) to inform MIT of noteworthy uses of this software.
;;;
;;; 3. All materials developed as a consequence of the use of this
;;; software shall duly acknowledge such use, in accordance with the
;;; usual standards of acknowledging credit in academic research.
;;;
;;; 4. MIT has made no warrantee or representation that the operation
;;; of this software will be error-free, and MIT is under no
;;; obligation to provide any services, by way of maintenance, update,
;;; or otherwise.
;;;
;;; 5. In conjunction with products arising from the use of this
;;; material, there shall be no use of the name of the Massachusetts
;;; Institute of Technology nor of any adaptation thereof in any
;;; advertising, promotional, or sales literature without prior
;;; written consent from MIT in each case.

;;;; Instance Slots

(declare (usual-integrations))

(define-structure (slot-descriptor (conc-name slot-descriptor/))
  (name #f read-only #t)
  (class #f read-only #t)
  (index #f read-only #t)
  (properties #f read-only #t))

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

(define (slot-allocation slot)
  (slot-property slot 'ALLOCATION 'INSTANCE))

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
  (lambda (class name)
    (make-computed-method (list class)
      (let ((generator (make-generator name)))
	(lambda classes
	  (generator #f (map class->dispatch-tag classes)))))))

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
		     (add-method accessor (maker class name)))))))
       (install 'ACCESSOR slot-accessor-method)
       (install 'MODIFIER slot-modifier-method)
       (install 'INITPRED slot-initpred-method)))
   (class-direct-slot-names class)))

(define (slot-value instance name)
  (%record-ref instance (compute-slot-index instance name 'SLOT-VALUE)))

(define (set-slot-value! instance name value)
  (%record-set! instance
		(compute-slot-index instance name 'SET-SLOT-VALUE!)
		value))

(define (slot-initialized? instance name)
  (not (eq? record-slot-uninitialized
	    (%record-ref instance
			 (compute-slot-index instance name
					     'SLOT-INITIALIZED?)))))

(define (compute-slot-index instance name error-name)
  (or (%record-slot-index instance name)
      (error:bad-range-argument name error-name)))

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
		       (list names))))
	      (let ((entry
		     (let loop ((names interaction))
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