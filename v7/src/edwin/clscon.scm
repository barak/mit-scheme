;;; -*-Scheme-*-
;;;
;;;	$Id: clscon.scm,v 1.6 1993/01/10 10:43:05 cph Exp $
;;;
;;;	Copyright (c) 1986-93 Massachusetts Institute of Technology
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
;;; NOTE: Parts of this program (Edwin) were created by translation
;;; from corresponding parts of GNU Emacs.  Users should be aware that
;;; the GNU GENERAL PUBLIC LICENSE may apply to these parts.  A copy
;;; of that license should have been included along with this file.
;;;

;;;; Class/Object System: Class Constructor

(declare (usual-integrations))

;;; ******************************************************************
;;; This software is intended for use in the Edwin window system only.
;;; Don't think about using it for anything else, since it is not, and
;;; likely will not ever, be supported as a part of the Scheme system.
;;; ******************************************************************

(define (make-class name superclass variables)
  (let ((entry (assq name class-descriptors))
	(object-size
	 (+ (length variables)
	    (if superclass (class-object-size superclass) 1)))
	(transforms (make-instance-transforms superclass variables)))
    (let ((make-class
	   (lambda ()
	     (let ((class
		    (%make-class name
				 superclass
				 object-size
				 transforms
				 (cons '()
				       (and superclass
					    (class-methods superclass))))))
	       (named-structure/set-tag-description!
		class
		(make-define-structure-type 'VECTOR
					    name
					    (map car transforms)
					    (map cdr transforms)
					    (unparser/standard-method name)))
	       class))))
      (if (not entry)
	  (let ((class (make-class)))
	    (set! class-descriptors (cons (cons name class) class-descriptors))
	    class)
	  (let ((class (cdr entry)))
	    (cond ((not (eq? (class-superclass class) superclass))
		   (let ((class (make-class)))
		     (set-cdr! entry class)
		     class))
		  ((and (= object-size (class-object-size class))
			(equal? transforms (class-instance-transforms class)))
		   class)
		  (else
		   (warn "Redefining class:" name)
		   (set-class-object-size! class object-size)
		   (set-class-instance-transforms! class transforms)
		   class)))))))

(define (make-instance-transforms superclass variables)
  (define (generate variables n tail)
    (if (null? variables)
	tail
	(cons (cons (car variables) n)
	      (generate (cdr variables) (1+ n) tail))))
  (if superclass
      (generate variables
		(class-object-size superclass)
		(class-instance-transforms superclass))
      (generate variables 1 '())))

(define (name->class name)
  (let ((entry (assq name class-descriptors)))
    (if (not entry)
	(error "Unknown class name:" name))
    (cdr entry)))

(define class-descriptors
  '())