;;; -*-Scheme-*-
;;;
;;;	$Id: win32com.scm,v 1.4 1996/03/21 16:52:48 adams Exp $
;;;
;;;	Copyright (c) 1994-1996 Massachusetts Institute of Technology
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
	  (win32-screen/set-icon! (selected-screen) icon)))))

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


(define-command set-foreground-color
  "Set foreground (text) color to COLOR."
  "sSet foreground color"
  (lambda (name)
    (let ((screen (selected-screen)))
      (win32-screen/set-foreground-color! screen (win32/find-color name))
      (update-screen! screen #t))))
  
(define-command set-background-color
  "Set background (text) color to COLOR."
  "sSet background color"
  (lambda (name)
    (let ((screen (selected-screen)))
      (win32-screen/set-background-color! screen (win32/find-color name))
      (update-screen! screen #t))))

(define-command set-font
  "Set font to be used for drawing text."
  "sSet font"
  (lambda (font)
    (let ((screen (selected-screen)))
      (win32-screen/set-font! screen font)
      (update-screen! screen #t))))

(define-command set-default-font
  "Set font to be used for drawing text in new windows."
  "sSet default font"
  (lambda (font)
    ((ucode-primitive win32-screen-set-default-font! 1) font)))

;; Missing functionality in win32-screen.
;;(define-command set-screen-size
;;  "Set size of editor screen to WIDTH x HEIGHT."
;;  "nScreen width (chars)\nnScreen height (chars)"
;;  (lambda (width height)
;;    (win32-screen/set-size! (selected-screen) (max 2 width) (max 2 height))))

(define-command set-screen-position
  "Set position of editor screen to (X,Y)."
  "nX position (pels)\nnY position (pels)"
  (lambda (x y)
    (win32-screen/set-position! (selected-screen) x y)))

(define-command show-screen-size
  "Show size of editor screen."
  ()
  (lambda ()
    (let ((screen (selected-screen)))
      (call-with-values (lambda () (win32-screen/get-position screen))
	(lambda (x y r b)		
	  (message "Screen is "
		   (screen-x-size screen)
		   " chars wide and "
		   (screen-y-size screen)
		   " chars high ("
		   (- r x) "x" (- b y)
		   " pels)"))))))

(define-command show-screen-position
  "Show position of editor screen.
This is the position of the upper left-hand corner of the frame border
surrounding the screen, relative to the upper left-hand corner of the
desktop."
  ()
  (lambda ()
    (call-with-values (lambda () (win32-screen/get-position (selected-screen)))
      (lambda (x y r b)
	r b				; ignored
	(message "Screen's upper left-hand corner is at (" x "," y ")")))))



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
