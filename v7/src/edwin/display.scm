;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/display.scm,v 1.2 1990/10/09 16:23:54 cph Exp $
;;;
;;;	Copyright (c) 1989, 1990 Massachusetts Institute of Technology
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

;;;; Display-Type Abstraction
;;; package: (edwin display-type)

(declare (usual-integrations))

(define-structure (display-type
		   (conc-name display-type/)
		   (constructor %make-display-type)
		   (print-procedure
		    (unparser/standard-method 'DISPLAY-TYPE
		      (lambda (state display-type)
			(unparse-object state
					(display-type/name display-type))))))
  (name false read-only true)
  (multiple-screens? false read-only true)
  (operation/available? false read-only true)
  (operation/make-screen false read-only true)
  (operation/make-input-port false read-only true)
  (operation/with-interrupt-source false read-only true)
  (operation/with-interrupts-enabled false read-only true)
  (operation/with-interrupts-disabled false read-only true))

(define (make-display-type name
			   multiple-screens?
			   available?
			   make-screen
			   make-input-port
			   with-interrupt-source
			   with-interrupts-enabled
			   with-interrupts-disabled)
  (let ((display-type
	 (%make-display-type name
			     multiple-screens?
			     available?
			     make-screen
			     make-input-port
			     with-interrupt-source
			     with-interrupts-enabled
			     with-interrupts-disabled)))
    (set! display-types (cons display-type display-types))
    display-type))

(define display-types '())

(define (display-type/available? display-type)
  ((display-type/operation/available? display-type)))

(define (display-type/make-screen display-type args)
  (apply (display-type/operation/make-screen display-type) args))

(define (display-type/make-input-port display-type screen)
  ((display-type/operation/make-input-port display-type) screen))

(define (display-type/with-interrupt-source display-type thunk)
  ((display-type/operation/with-interrupt-source display-type) thunk))

(define (display-type/with-interrupts-enabled display-type thunk)
  ((display-type/operation/with-interrupts-enabled display-type) thunk))

(define (display-type/with-interrupts-disabled display-type thunk)
  ((display-type/operation/with-interrupts-disabled display-type) thunk))

(define (editor-display-types)
  (list-transform-positive display-types display-type/available?))

(define (name->display-type name)
  (let ((display-type
	 (list-search-positive display-types
	   (lambda (display-type)
	     (eq? name (display-type/name display-type))))))
    (if (not display-type)
	(error "Unknown display-type name" name))
    display-type))