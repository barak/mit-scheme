;;; -*-Scheme-*-
;;;
;;;	$Id: os2com.scm,v 1.1 1994/12/19 19:46:35 cph Exp $
;;;
;;;	Copyright (c) 1994 Massachusetts Institute of Technology
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
      (os2-screen/set-foreground-color! screen (name->color name))
      (update-screen! screen #t))))

(define-command set-background-color
  "Set background (text) color to COLOR."
  "sSet background color"
  (lambda (name)
    (let ((screen (selected-screen)))
      (os2-screen/set-background-color! screen (name->color name))
      (update-screen! screen #t))))

(define (name->color name)
  (let ((length (string-length name)))
    (if (and (not (fix:= 0 length))
	     (char=? #\# (string-ref name 0)))
	(let ((color
	       (and (fix:= 7 length)
		    (let ((color (substring->number name 1 length 16)))
		      (and color
			   (fix:>= color 0)
			   color)))))
	  (if (not color)
	      (editor-error "Ill-formed RGB color name:" name))
	  color)
	(editor-error "Unknown color name:" name))))

(define-command set-font
  "Set font to be used for drawing text."
  "sSet font"
  (lambda (font)
    (let ((screen (selected-screen)))
      (os2-screen/set-font! screen font)
      (update-screen! screen #t))))

(define-command set-screen-size
  "Set size of editor screen to WIDTH x HEIGHT."
  "nScreen width (chars)\nnScreen height (chars)"
  (lambda (width height)
    (os2-screen/set-size! (selected-screen) (max 2 width) (max 2 height))))

(define-command set-screen-position
  "Set position of editor screen to (X,Y)."
  "nX position (pels)\nnY position (pels)"
  (lambda (x y)
    (os2-screen/set-position! (selected-screen) x y)))

(define-command show-screen-size
  "Show size of editor screen."
  ()
  (lambda ()
    (let ((screen (selected-screen)))
      (message "Screen is "
	       (screen-x-size screen)
	       " chars wide and "
	       (screen-y-size screen)
	       " chars high ("
	       (screen-pel-width screen)
	       "x"
	       (screen-pel-height screen)
	       " pels)"))))

(define-command show-screen-position
  "Show position of editor screen.
This is the position of the lower left-hand corner of the frame border
surrounding the screen, relative to the lower left-hand corner of the
desktop."
  ()
  (lambda ()
    (call-with-values (lambda () (os2-screen/get-position (selected-screen)))
      (lambda (x y)
	(message "Screen's lower left-hand corner is at (" x "," y ")")))))