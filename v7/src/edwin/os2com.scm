;;; -*-Scheme-*-
;;;
;;;	$Id: os2com.scm,v 1.4 1996/05/03 06:55:22 cph Exp $
;;;
;;;	Copyright (c) 1994-96 Massachusetts Institute of Technology
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

;;;; OS/2 Presentation Manager Commands

(declare (usual-integrations))

(define-command set-foreground-color
  "Set foreground (text) color to COLOR."
  "sSet foreground color"
  (lambda (name)
    (let ((screen (selected-screen)))
      (os2-screen/set-foreground-color! screen (os2/find-color name))
      (update-screen! screen #t))))

(define-command set-background-color
  "Set background (text) color to COLOR."
  "sSet background color"
  (lambda (name)
    (let ((screen (selected-screen)))
      (os2-screen/set-background-color! screen (os2/find-color name))
      (update-screen! screen #t))))

(define-command define-color-name
  "Globally define COLOR-NAME to be COLOR.
This does not affect any colors on the screen,
but changes the meaning of COLOR-NAME when it is used in the future."
  "sDefine color\nsDefine color to"
  (lambda (name color)
    (os2/define-color name color)))

(define-command set-font
  "Set font to be used for drawing text."
  "sSet font"
  (lambda (font)
    (let ((screen (selected-screen)))
      (os2-screen/set-font! screen font)
      (update-screen! screen #t))))

(define-command set-frame-size
  "Set size of editor frame to WIDTH x HEIGHT."
  "nFrame width (chars)\nnFrame height (chars)"
  (lambda (width height)
    (os2-screen/set-size! (selected-screen) (max 2 width) (max 2 height))))

(define-command set-frame-position
  "Set position of editor frame to (X,Y)."
  "nFrame X position (pels)\nnFrame Y position (pels)"
  (lambda (x y)
    (os2-screen/set-position! (selected-screen) x y)))

(define-command show-frame-size
  "Show size of editor frame."
  ()
  (lambda ()
    (let ((screen (selected-screen)))
      (message "Frame is "
	       (screen-x-size screen)
	       " chars wide and "
	       (screen-y-size screen)
	       " chars high ("
	       (screen-pel-width screen)
	       "x"
	       (screen-pel-height screen)
	       " pels)"))))

(define-command show-frame-position
  "Show position of editor frame.
This is the position of the lower left-hand corner of the frame border
surrounding the frame, relative to the lower left-hand corner of the
desktop."
  ()
  (lambda ()
    (call-with-values (lambda () (os2-screen/get-position (selected-screen)))
      (lambda (x y)
	(message "Frame's lower left-hand corner is at (" x "," y ")")))))

;; For upwards compatibility
(define edwin-command$set-screen-size edwin-command$set-frame-size)
(define edwin-command$set-screen-position edwin-command$set-frame-position)
(define edwin-command$show-screen-size edwin-command$show-frame-size)
(define edwin-command$show-screen-position edwin-command$show-frame-position)

(define-command set-frame-name
  "Set name of selected frame to NAME.
Useful only if `frame-name-format' is false."
  "sSet frame name"
  (lambda (name) (os2-screen/set-title! (selected-screen) name)))

(define (update-os2-screen-names! screen)
  (let ((window
	 (if (and (selected-screen? screen) (within-typein-edit?))
	     (typein-edit-other-window)
	     (screen-selected-window screen))))
    (let ((buffer (window-buffer window)))
      (let ((format (ref-variable frame-name-format buffer))
	    (length (ref-variable frame-name-length buffer)))
	(if format
	    (os2-screen/set-title!
	     screen
	     (string-trim-right
	      (format-modeline-string window format length))))))))