;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/modwin.scm,v 1.36 1991/04/01 10:07:42 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989, 1990 Massachusetts Institute of Technology
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

;;;; Modeline Window

(declare (usual-integrations))

(define-class modeline-window vanilla-window ())

(define-method modeline-window (:initialize! window window*)
  (usual=> window :initialize! window*)
  (set! y-size 1))

(define (modeline-window:update-display! window screen x-start y-start
					 xl xu yl yu display-style)
  display-style				;ignore
  (if (and (fix:= yl 0) (fix:< yl yu))
      (let ((superior (window-superior window)))
	(modeline-string!
	 superior
	 (screen-get-output-line
	  screen
	  y-start
	  (fix:+ x-start xl)
	  (fix:+ x-start xu)
	  (variable-local-value (window-buffer superior)
				(ref-variable-object mode-line-inverse-video)))
	 xl xu)))
  true)

(define-method modeline-window :update-display!
  modeline-window:update-display!)

(define-variable mode-line-inverse-video
  "*True means use inverse video, or other suitable display mode, for the mode line."
  true)

(define-method modeline-window (:event! window type)
  type					;ignored
  (setup-redisplay-flags! redisplay-flags))