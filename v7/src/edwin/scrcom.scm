;;; -*-Scheme-*-
;;;
;;; $Id: scrcom.scm,v 1.7 2000/12/01 05:24:42 cph Exp $
;;;
;;; Copyright (c) 1990-2000 Massachusetts Institute of Technology
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
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;;; Screen Commands

(declare (usual-integrations))

(define-command delete-frame
  "Delete the frame that point is in."
  ()
  (lambda ()
    (if (null? (cdr (screen-list)))
	(editor-error "Can't delete the only frame."))
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
    (let ((screen (other-screen (selected-screen) arg #f)))
      (if (not screen)
	  (editor-error "No other visible frame."))
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