;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/clscon.scm,v 1.3 1989/05/01 21:11:34 cph Rel $
;;;
;;;	Copyright (c) 1986, 1989 Massachusetts Institute of Technology
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
	(object-size (if superclass
			 (+ (length variables) (class-object-size superclass))
			 (1+ (length variables))))
	(transforms (make-instance-transforms superclass variables)))
    (let ((make-class
	   (lambda ()
	     (let ((class
		    (vector class-tag
			    name
			    superclass
			    object-size
			    transforms
			    (cons '()
				  (and superclass
				       (class-methods superclass))))))
	       (unparser/set-tagged-vector-method!
		class
		(unparser/standard-method name))
	       (named-structure/set-tag-description! class object-description)
	       class))))
      (if (not entry)
	  (let ((class (make-class)))
	    (set! class-descriptors (cons (cons name class) class-descriptors))
	    class)
	  (let ((class (cdr entry)))
	    (if (eq? (class-superclass class) superclass)
		(begin
		  (with-output-to-port (cmdl/output-port (nearest-cmdl))
		    (lambda ()
		      (warn "Redefining class" name)))
		  (vector-set! class 3 object-size)
		  (vector-set! class 4 transforms)
		  class)
		(let ((class (make-class)))
		  (set-cdr! entry class)
		  class)))))))

(define (class? x)
  (and (vector? x)
       (not (zero? (vector-length x)))
       (eq? class-tag (vector-ref x 0))))

(define (name->class name)
  (cdr (or (assq name class-descriptors)
	   (error "unknown class name" name))))

(define class-tag "Class")

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

(unparser/set-tagged-vector-method! class-tag
				    (unparser/standard-method 'CLASS))

(define class-descriptors
  '())