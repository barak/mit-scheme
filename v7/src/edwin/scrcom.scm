;;; -*-Scheme-*-
;;;
;;;	$Id: scrcom.scm,v 1.5 1996/04/23 23:07:48 cph Exp $
;;;
;;;	Copyright (c) 1990-96 Massachusetts Institute of Technology
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

;;;; Screen Commands

(declare (usual-integrations))

(define-command delete-frame
  "Delete the frame that point is in."
  ()
  (lambda ()
    (if (null? (cdr (screen-list)))
	(editor-error "Can't delete the only frame"))
    (delete-screen! (selected-screen))))

(define-command make-frame
  "Create a new frame, displaying the current buffer."
  ()
  (lambda () (select-buffer-other-screen (current-buffer))))

(define-command other-frame
  "Select the ARG'th different visible frame, and raise it.
All frames are arranged in a cyclic order.
This command selects the frame ARG steps away in that order.
A negative ARG moves in the opposite order."
  "p"
  (lambda (arg)
    (let ((screen (other-screen (selected-screen) arg #t)))
      (if (not screen)
	  (editor-error "No other frame"))
      (select-screen screen))))

(define-variable frame-name-format
  "If not false, template for displaying frame name.
Has same format as `mode-line-format'."
  'mode-line-buffer-identification)

(define-variable frame-name-length
  "Maximum length of frame name.
Used only if `frame-name-format' is non-false."
  64
  exact-nonnegative-integer?)

;; For upwards compatibility:
(define edwin-command$delete-screen edwin-command$delete-frame)
(define edwin-variable$x-screen-name-format edwin-variable$frame-name-format)
(define edwin-variable$x-screen-name-length edwin-variable$frame-name-length)

;;; This command is for Windows, and shouldn't really be here.
;;; It is for terminal screens only.
(define-command resize-screen
  "Resize the screen that point is in."
  ()
  (lambda () (resize-screen)))