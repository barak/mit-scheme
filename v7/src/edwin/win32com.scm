;;; -*-Scheme-*-
;;;
;;;	$Id: win32com.scm,v 1.3 1994/11/06 18:36:57 adams Exp $
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
;;;

;;; package (edwin win-commands)

;;; Win32 commands

(declare (usual-integrations))

(define (current-win32-window)
  (screen->handle (selected-screen)))

(define-command set-icon
  "Set the current window's icon to ICON.
ICON must be the (string) name of one of the known icons.
When called interactively, completion is available on the input."
  (lambda ()
    (list (prompt-for-alist-value "Set Icon"
				  (map (lambda (x) (cons x x))
				       (vector->list icon-names)))))
  (lambda (icon-name)
    (let  ((icon  (load-icon (get-handle 0) icon-name)))
      (if (zero? icon)
	  (error "Unknown icon name" icon-name)
	  ((ucode-primitive win32-screen-set-icon! 2)
	   (current-win32-window)
	   icon)))))

(define icon-names
  '#("shield3_icon"
     "shield4_icon"
     "shield2_icon"
     "shield1_icon"
     "lambda_icon"
     "lambda2_icon"
     "edwin_icon"
     "liar1_icon"
     "liar2_icon"
     "liar3_icon"
     "graphics_icon"
     "coffee_icon"
     "conses_icon"
     "environment_icon"
     "mincer_icon"
     "bch_ico"))



(define (update-win32-screen-name! screen)
  (let ((window
	 (if (and (selected-screen? screen) (within-typein-edit?))
	     (typein-edit-other-window)
	     (screen-selected-window screen))))
    (let ((buffer (window-buffer window))
	  (update-name
	   (lambda (set-name format length)
	     (if format
		 (set-name
		  screen
		  (string-trim-right
		   (format-modeline-string window format length)))))))
      (update-name win32-screen/set-name!
		   (ref-variable screen-name-format buffer)
		   (ref-variable screen-name-length buffer)))))

(define-variable screen-name-format
  "If not false, template for displaying window name.
Has same format as `mode-line-format'."
  'mode-line-buffer-identification)

(define-variable screen-name-length
  "Maximum length of window name.
Used only if `screen-name-format' is non-false."
  64
  exact-nonnegative-integer?)
