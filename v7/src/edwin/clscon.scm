;;; -*-Scheme-*-
;;;
;;; $Id: clscon.scm,v 1.8 2002/02/03 03:38:54 cph Exp $
;;;
;;; Copyright (c) 1986-1999, 2002 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

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
  (define (generate variables n)
    (if (pair? variables)
	(cons (cons (car variables) n)
	      (generate (cdr variables) (+ n 1)))
	'()))
  (if superclass
      (append (class-instance-transforms superclass)
	      (generate variables (class-object-size superclass)))
      (generate variables 1)))

(define (name->class name)
  (let ((entry (assq name class-descriptors)))
    (if (not entry)
	(error "Unknown class name:" name))
    (cdr entry)))

(define class-descriptors
  '())