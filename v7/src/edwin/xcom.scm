;;; -*-Scheme-*-
;;;
;;;	$Id: xcom.scm,v 1.15 1996/12/11 00:45:10 cph Exp $
;;;
;;;	Copyright (c) 1989-96 Massachusetts Institute of Technology
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

;;;; X Commands

(declare (usual-integrations))

(define-primitives
  (x-window-clear 1)
  (x-window-lower 1)
  (x-window-raise 1)
  (x-window-get-position 1)
  (x-window-get-size 1)
  (x-window-set-background-color 2)
  (x-window-set-border-color 2)
  (x-window-set-border-width 2)
  (x-window-set-cursor-color 2)
  (x-window-set-font 2)
  (x-window-set-foreground-color 2)
  (x-window-set-internal-border-width 2)
  (x-window-set-mouse-color 2)
  (x-window-set-mouse-shape 2)
  (x-window-set-position 3)
  (x-window-set-size 3)
  (x-window-x-size 1)
  (x-window-y-size 1)
  (xterm-x-size 1)
  (xterm-y-size 1)
  (xterm-set-size 3))

(define (current-xterm)
  (screen-xterm (selected-screen)))

(define-command set-foreground-color
  "Set foreground (text) color of selected frame to COLOR."
  "sSet foreground color"
  (lambda (color)
    (x-window-set-foreground-color (current-xterm) color)
    (update-screen! (selected-screen) true)))

(define-command set-background-color
  "Set background color of selected frame to COLOR."
  "sSet background color"
  (lambda (color)
    (let ((xterm (current-xterm)))
      (x-window-set-background-color xterm color)
      (x-window-clear xterm))
    (update-screen! (selected-screen) true)))

(define-command set-border-color
  "Set border color of selected frame to COLOR."
  "sSet border color"
  (lambda (color)
    (x-window-set-border-color (current-xterm) color)))

(define-command set-cursor-color
  "Set cursor color of selected frame to COLOR."
  "sSet cursor color"
  (lambda (color)
    (x-window-set-cursor-color (current-xterm) color)))

(define-command set-mouse-color
  "Set mouse color of selected frame to COLOR."
  "sSet mouse color"
  (lambda (color)
    (x-window-set-mouse-color (current-xterm) color)))

(define-command set-font
  "Set text font of selected frame to FONT."
  "sSet font"
  (lambda (font)
    (let ((xterm (current-xterm)))
      (let ((x-size (xterm-x-size xterm))
	    (y-size (xterm-y-size xterm)))
	(if (not (x-window-set-font xterm font))
	    (editor-error "Unknown font name: " font))
	(xterm-set-size xterm x-size y-size)))))

(define-command set-border-width
  "Set border width of selected frame to WIDTH."
  "nSet border width"
  (lambda (width)
    (x-window-set-border-width (current-xterm) (max 0 width))
    (update-screen! (selected-screen) true)))

(define-command set-internal-border-width
  "Set internal border width of selected frame to WIDTH."
  "nSet internal border width"
  (lambda (width)
    (x-window-set-internal-border-width (current-xterm) (max 0 width))))

(define-command show-frame-size
  "Show size of editor frame."
  ()
  (lambda ()
    (let ((screen (selected-screen)))
      (let ((w.h (x-window-get-size (screen-xterm screen))))
	(message "Frame is "
		 (screen-x-size screen)
		 " chars wide and "
		 (screen-y-size screen)
		 " chars high ("
		 (car w.h)
		 "x"
		 (cdr w.h)
		 " pixels)")))))

(define-command set-frame-size
  "Set size of selected frame to WIDTH x HEIGHT."
  "nFrame width (chars)\nnFrame height (chars)"
  (lambda (width height)
    (xterm-set-size (current-xterm) (max 2 width) (max 2 height))))

(define-command show-frame-position
  "Show position of editor frame.
This is the position of the upper left-hand corner of the frame border
surrounding the frame, relative to the upper left-hand corner of the
desktop."
  ()
  (lambda ()
    (let ((x.y (x-window-get-position (current-xterm))))
      (message "Frame's upper left-hand corner is at ("
	       (car x.y) "," (cdr x.y) ")"))))

(define-command set-frame-position
  "Set position of selected frame to (X,Y)."
  "nX position (pixels)\nnY position (pixels)"
  (lambda (x y)
    (x-window-set-position (current-xterm) x y)))

(define-command set-frame-name
  "Set name of selected frame to NAME.
Useful only if `frame-name-format' is false."
  "sSet frame name"
  (lambda (name) (xterm-screen/set-name (selected-screen) name)))

(define-command set-frame-icon-name
  "Set icon name of selected frame to NAME.
Useful only if `frame-icon-name-format' is false."
  "sSet frame icon name"
  (lambda (name) (xterm-screen/set-icon-name (selected-screen) name)))

(define (update-xterm-screen-names! screen)
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
      (update-name xterm-screen/set-name
		   (ref-variable frame-name-format buffer)
		   (ref-variable frame-name-length buffer))
      (update-name xterm-screen/set-icon-name
		   (ref-variable frame-icon-name-format buffer)
		   (ref-variable frame-icon-name-length buffer)))))

(define-variable frame-icon-name-format
  "If not false, template for displaying frame icon name.
Has same format as `mode-line-format'."
  "edwin")

(define-variable frame-icon-name-length
  "Maximum length of frame icon name.
Used only if `frame-icon-name-format' is non-false."
  32
  exact-nonnegative-integer?)

(define-command raise-frame
  "Raise the selected frame so that it is not obscured by other windows."
  ()
  (lambda () (x-window-raise (current-xterm))))

(define-command lower-frame
  "Lower the selected frame so that it does not obscure other windows."
  ()
  (lambda () (x-window-lower (current-xterm))))

(define-command auto-raise-mode
  "Toggle auto-raise mode.
With argument, turn auto-raise mode on if argument is positive.
When auto-raise mode is on, typing in a frame causes it to be raised."
  "P"
  (lambda (argument)
    (set! x-screen-auto-raise
	  (let ((argument (command-argument-value argument)))
	    (if argument
		(> argument 0)
		(not x-screen-auto-raise))))
    (message "Auto-raise " (if x-screen-auto-raise "enabled" "disabled"))))

(define-command set-mouse-shape
  "Set mouse cursor shape for selected frame to SHAPE.
SHAPE must be the (string) name of one of the known cursor shapes.
When called interactively, completion is available on the input."
  (lambda ()
    (list (prompt-for-alist-value "Set mouse shape"
				  (map (lambda (x) (cons x x))
				       (vector->list mouse-cursor-shapes)))))
  (lambda (shape)
    (x-window-set-mouse-shape
     (current-xterm)
     (let ((end (vector-length mouse-cursor-shapes)))
       (let loop ((index 0))
	 (cond ((>= index end)
		(error "Unknown shape name" shape))
	       ((string-ci=? (vector-ref mouse-cursor-shapes index) shape)
		index)
	       (else
		(loop (1+ index)))))))))

(define mouse-cursor-shapes
  '#("X-cursor"
     "arrow"
     "based-arrow-down"
     "based-arrow-up"
     "boat"
     "bogosity"
     "bottom-left-corner"
     "bottom-right-corner"
     "bottom-side"
     "bottom-tee"
     "box-spiral"
     "center-ptr"
     "circle"
     "clock"
     "coffee-mug"
     "cross"
     "cross-reverse"
     "crosshair"
     "diamond-cross"
     "dot"
     "dotbox"
     "double-arrow"
     "draft-large"
     "draft-small"
     "draped-box"
     "exchange"
     "fleur"
     "gobbler"
     "gumby"
     "hand1"
     "hand2"
     "heart"
     "icon"
     "iron-cross"
     "left-ptr"
     "left-side"
     "left-tee"
     "leftbutton"
     "ll-angle"
     "lr-angle"
     "man"
     "middlebutton"
     "mouse"
     "pencil"
     "pirate"
     "plus"
     "question-arrow"
     "right-ptr"
     "right-side"
     "right-tee"
     "rightbutton"
     "rtl-logo"
     "sailboat"
     "sb-down-arrow"
     "sb-h-double-arrow"
     "sb-left-arrow"
     "sb-right-arrow"
     "sb-up-arrow"
     "sb-v-double-arrow"
     "shuttle"
     "sizing"
     "spider"
     "spraycan"
     "star"
     "target"
     "tcross"
     "top-left-arrow"
     "top-left-corner"
     "top-right-corner"
     "top-side"
     "top-tee"
     "trek"
     "ul-angle"
     "umbrella"
     "ur-angle"
     "watch"
     "xterm"))


;;;; Mouse Commands
;;; (For compatibility with old code.)

(let-syntax
    ((copy
      (lambda (name)
	`(DEFINE ,(symbol-append 'EDWIN-COMMAND$X- name)
	   ,(symbol-append 'EDWIN-COMMAND$ name)))))
  (copy set-foreground-color)
  (copy set-background-color)
  (copy set-border-color)
  (copy set-cursor-color)
  (copy set-mouse-color)
  (copy set-font)
  (copy set-border-width)
  (copy set-internal-border-width)
  (copy auto-raise-mode)
  (copy set-mouse-shape)
  (copy mouse-select)
  (copy mouse-keep-one-window)
  (copy mouse-select-and-split)
  (copy mouse-set-point)
  (copy mouse-set-mark)
  (copy mouse-show-event)
  (copy mouse-ignore))

(define edwin-command$x-set-size edwin-command$set-frame-size)
(define edwin-command$x-set-position edwin-command$set-frame-position)
(define edwin-command$x-set-window-name edwin-command$set-frame-name)
(define edwin-command$x-set-icon-name edwin-command$set-frame-icon-name)
(define edwin-command$x-raise-screen edwin-command$raise-frame)
(define edwin-command$x-lower-screen edwin-command$lower-frame)

(let-syntax
    ((copy
      (lambda (name)
	`(DEFINE ,(symbol-append 'EDWIN-VARIABLE$X-SCREEN- name)
	   ,(symbol-append 'EDWIN-VARIABLE$FRAME- name)))))
  (copy icon-name-format)
  (copy icon-name-length))

(define x-button1-down button1-down)
(define x-button2-down button2-down)
(define x-button3-down button3-down)
(define x-button4-down button4-down)
(define x-button5-down button5-down)
(define x-button1-up button1-up)
(define x-button2-up button2-up)
(define x-button3-up button3-up)
(define x-button4-up button4-up)
(define x-button5-up button5-up)