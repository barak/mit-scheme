;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/class.scm,v 1.70 1989/05/01 21:10:16 cph Rel $
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

;;;; Class/Object System

(declare (usual-integrations))

;;; ******************************************************************
;;; This software is intended for use in the Edwin window system only.
;;; Don't think about using it for anything else, since it is not, and
;;; likely will not ever, be supported as a part of the Scheme system.
;;; ******************************************************************

(define-structure (class (type vector)
			 (constructor false)
			 (initial-offset 1))
  (name false read-only true)
  (superclass false read-only true)
  (object-size false read-only true)
  (instance-transforms false read-only true)
  (methods false read-only true))

(define (class-method class name)
  (class-methods/ref (class-methods class) name))

(define (class-methods/ref methods name)
  (or (method-lookup methods name) (error "unknown method" name)))

(define (method-lookup methods name)
  (let loop ((methods methods))
    (and methods
	 (let ((entry (assq name (car methods))))
	   (if entry
	       (cdr entry)
	       (loop (cdr methods)))))))

(define (class-method-define class name method)
  (let ((methods (class-methods class)))
    (let ((entry (assq name (car methods))))
      (if entry
	  (set-cdr! entry method)
	  (set-car! methods (cons (cons name method) (car methods))))))
  name)

(define-integrable (usual-method class name)
  (class-method (class-superclass class) name))

(define (subclass? class class*)
  (or (eq? class class*)
      (let loop ((class (class-superclass class)))
	(and class
	     (or (eq? class class*)
		 (loop (class-superclass class)))))))

(define (make-object class)
  (if (not (class? class))
      (error "not a class" class))
  (let ((object (make-vector (class-object-size class) false)))
    (vector-set! object 0 class)
    object))

(define (object? object)
  (and (vector? object)
       (not (zero? (vector-length object)))
       (class? (vector-ref object 0))))

(define (object-of-class? class object)
  (and (vector? object)
       (not (zero? (vector-length object)))
       (eq? class (vector-ref object 0))))

(define-integrable (object-class object)
  (vector-ref object 0))

(define-integrable (object-methods object)
  (class-methods (object-class object)))

(define-integrable (object-method object name)
  (class-method (object-class object) name))

(define (object-description object)
  (map (lambda (transform)
	 (list (car transform) (vector-ref object (cdr transform))))
       (class-instance-transforms (object-class object))))

(define (send object operation . args)
  (apply (object-method object operation) object args))

(define (send-if-handles object operation . args)
  (let ((method (method-lookup (object-methods object) operation)))
    (and method (apply method object args))))

(define (send-usual class object operation . args)
  (apply (usual-method class operation) object args))