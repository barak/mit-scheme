;;; -*-Scheme-*-
;;;
;;;	$Id: utlwin.scm,v 1.58 1993/01/12 10:50:41 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989-93 Massachusetts Institute of Technology
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

;;;; Utility Windows

(declare (usual-integrations))

;;;; Blank Window

(define-class blank-window vanilla-window
  ())

(define (blank-window:update-display! window screen x-start y-start
				      xl xu yl yu display-style)
  window display-style			;ignore
  (screen-clear-rectangle screen
			  (fix:+ x-start xl) (fix:+ x-start xu)
			  (fix:+ y-start yl) (fix:+ y-start yu)
			  false)
  true)

(define-method blank-window :update-display!
  blank-window:update-display!)

;;;; Vertical Border Window

(define-class vertical-border-window vanilla-window
  ())

(define-method vertical-border-window (:initialize! window window*)
  (usual=> window :initialize! window*)
  (set! x-size 1))

(define-method vertical-border-window (:set-x-size! window x)
  window				;ignore
  (error "Can't change the x-size of a vertical border window" x))

(define-method vertical-border-window (:set-size! window x y)
  (if (not (fix:= x 1))
      (error "Can't change the x-size of a vertical border window" x))
  (set! x-size x)
  (set! y-size y)
  (setup-redisplay-flags! redisplay-flags))

(define (vertical-border-window:update-display! window screen x-start y-start
						xl xu yl yu display-style)
  display-style				;ignore
  (if (fix:< xl xu)
      (clip-window-region-1 yl yu (window-y-size window)
	(lambda (yl yu)
	  (let ((xl (fix:+ x-start xl))
		(yu (fix:+ y-start yu)))
	    (let loop ((y (fix:+ y-start yl)))
	      (if (fix:< y yu)
		  (begin
		    (screen-output-char screen xl y #\| false)
		    (loop (fix:+ y 1)))))))))
  true)

(define-method vertical-border-window :update-display!
  vertical-border-window:update-display!)

;;;; Cursor Window

(define-class cursor-window vanilla-window
  (enabled?))

(define-method cursor-window (:initialize! window window*)
  (usual=> window :initialize! window*)
  (set! x-size 1)
  (set! y-size 1)
  (set! enabled? false))

(define-method cursor-window (:set-x-size! window x)
  window				;ignore
  (error "Can't change the size of a cursor window" x))

(define-method cursor-window (:set-y-size! window y)
  window				;ignore
  (error "Can't change the size of a cursor window" y))

(define-method cursor-window (:set-size! window x y)
  window				;ignore
  (error "Can't change the size of a cursor window" x y))

(define (cursor-window:update-display! window screen x-start y-start
				       xl xu yl yu display-style)
  display-style				;ignore
  (if (and (with-instance-variables cursor-window window () enabled?)
	   (fix:< xl xu)
	   (fix:< yl yu))
      (screen-move-cursor screen x-start y-start))
  true)

(define-method cursor-window :update-display!
  cursor-window:update-display!)

(define-method cursor-window (:enable! window)
  (set! enabled? true)
  (setup-redisplay-flags! redisplay-flags))

(define-method cursor-window (:disable! window)
  (set! enabled? false)
  (set-car! redisplay-flags false))