;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/display.scm,v 1.1 1989/08/12 08:33:51 cph Exp $
;;;
;;;	Copyright (c) 1989 Massachusetts Institute of Technology
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
		   (constructor %make-display-type))
  (name false read-only true)
  (operation/available? false read-only true)
  (operation/make-screen false read-only true)
  (operation/make-input-port false read-only true)
  (operation/with-interrupt-source false read-only true)
  (operation/with-interrupts-enabled false read-only true)
  (operation/with-interrupts-disabled false read-only true))

(define (make-display-type name
			   available?
			   make-screen
			   make-input-port
			   with-interrupt-source
			   with-interrupts-enabled
			   with-interrupts-disabled)
  (let ((display-type
	 (%make-display-type name
			     available?
			     make-screen
			     make-input-port
			     with-interrupt-source
			     with-interrupts-enabled
			     with-interrupts-disabled)))
    (set! display-types (cons display-type display-types))
    display-type))

(define display-types '())
(define edwin-display-type false)

(define (display-type/available? display-type)
  ((display-type/operation/available? display-type)))

(define (make-editor-screen . args)
  (apply (display-type/operation/make-screen edwin-display-type) args))

(define (make-editor-input-port screen)
  ((display-type/operation/make-input-port edwin-display-type) screen))

(define (with-editor-interrupts thunk)
  ((display-type/operation/with-interrupt-source edwin-display-type) thunk))

(define (with-editor-interrupts-enabled thunk)
  ((display-type/operation/with-interrupts-enabled edwin-display-type) thunk))

(define (with-editor-interrupts-disabled thunk)
  ((display-type/operation/with-interrupts-disabled edwin-display-type) thunk))

(define (initialize-display-type!)
  (set! edwin-display-type
	(cond (edwin-display-type)
	      ((display-type/available? x-display-type) x-display-type)
	      ((list-search-positive display-types display-type/available?))
	      (else (error "No display available"))))
  unspecific)

(define (editor-display-types)
  (map display-type/name
       (list-transform-positive display-types display-type/available?)))

(define (editor-display-type)
  (and edwin-display-type (display-type/name edwin-display-type)))

(define (set-editor-display-type! type-name)
  (set! edwin-display-type
	(and type-name
	     (or (list-search-positive display-types
		   (lambda (display-type)
		     (eq? type-name (display-type/name display-type))))
		 (error "Unknown display-type name" type-name))))
  unspecific)