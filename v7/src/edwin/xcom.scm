;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/xcom.scm,v 1.7 1992/03/13 23:59:41 cph Exp $
;;;
;;;	Copyright (c) 1989-92 Massachusetts Institute of Technology
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
  (x-window-set-background-color 2)
  (x-window-set-border-color 2)
  (x-window-set-border-width 2)
  (x-window-set-cursor-color 2)
  (x-window-set-font 2)
  (x-window-set-foreground-color 2)
  (x-window-set-icon-name 2)
  (x-window-set-internal-border-width 2)
  (x-window-set-mouse-color 2)
  (x-window-set-mouse-shape 2)
  (x-window-set-name 2)
  (x-window-set-position 3)
  (x-window-set-size 3)
  (xterm-x-size 1)
  (xterm-y-size 1)
  (xterm-set-size 3))

(define (current-xterm)
  (screen-xterm (selected-screen)))

(define-command x-set-foreground-color
  "Set foreground (text) color to COLOR."
  "sSet foreground color"
  (lambda (color)
    (x-window-set-foreground-color (current-xterm) color)
    (update-screen! (selected-screen) true)))

(define-command x-set-background-color
  "Set background color to COLOR."
  "sSet background color"
  (lambda (color)
    (let ((xterm (current-xterm)))
      (x-window-set-background-color xterm color)
      (x-window-clear xterm))
    (update-screen! (selected-screen) true)))

(define-command x-set-border-color
  "Set border color to COLOR."
  "sSet border color"
  (lambda (color)
    (x-window-set-border-color (current-xterm) color)))

(define-command x-set-cursor-color
  "Set cursor color to COLOR."
  "sSet cursor color"
  (lambda (color)
    (x-window-set-cursor-color (current-xterm) color)))

(define-command x-set-mouse-color
  "Set mouse color to COLOR."
  "sSet mouse color"
  (lambda (color)
    (x-window-set-mouse-color (current-xterm) color)))

(define-command x-set-font
  "Set font to be used for drawing text."
  "sSet font"
  (lambda (font)
    (let ((xterm (current-xterm)))
      (let ((x-size (xterm-x-size xterm))
	    (y-size (xterm-y-size xterm)))
	(if (not (x-window-set-font xterm font))
	    (editor-error "Unknown font name: " font))
	(xterm-set-size xterm x-size y-size)))))

(define-command x-raise-screen
  "Raise the editor screen so that it is not obscured by other X windows."
  ()
  (lambda () (x-window-raise (current-xterm))))

(define-command x-lower-screen
  "Lower the editor screen so that it does not obscure other X windows."
  ()
  (lambda () (x-window-lower (current-xterm))))

(define-command x-set-size
  "Set size of editor screen to WIDTH x HEIGHT."
  "nScreen width (chars)\nnScreen height (chars)"
  (lambda (width height)
    (xterm-set-size (current-xterm) (max 2 width) (max 2 height))))

(define-command x-set-position
  "Set position of editor screen to (X,Y)."
  "nX position (pixels)\nnY position (pixels)"
  (lambda (x y)
    (x-window-set-position (current-xterm) x y)))

(define-command x-set-border-width
  "Set width of border to WIDTH."
  "nSet border width"
  (lambda (width)
    (x-window-set-border-width (current-xterm) (max 0 width))
    (update-screen! (selected-screen) true)))

(define-command x-set-internal-border-width
  "Set width of internal border to WIDTH."
  "nSet internal border width"
  (lambda (width)
    (x-window-set-internal-border-width (current-xterm) (max 0 width))))

(define-command x-set-window-name
  "Set X window name to NAME.
Useful only if `x-screen-name-format' is false."
  "sSet X window name"
  (lambda (name)
    (x-window-set-name (current-xterm) name)))

(define-command x-set-icon-name
  "Set X window icon name to NAME.
Useful only if `x-screen-icon-name-format' is false."
  "sSet X window icon name"
  (lambda (name)
    (x-window-set-icon-name (current-xterm) name)))

(define-variable x-screen-name-format
  "If not false, template for displaying X window name.
Has same format as `mode-line-format'."
  'mode-line-buffer-identification)

(define-variable x-screen-icon-name-format
  "If not false, template for displaying X window icon name.
Has same format as `mode-line-format'."
  "edwin")

(define-variable x-screen-icon-name-length
  "Maximum length of X window icon name.
Used only if `x-screen-icon-name-format' is non-false."
  32)

(define (update-xterm-screen-names! screen)
  (let ((window
	 (if (and (selected-screen? screen) (within-typein-edit?))
	     (typein-edit-other-window)
	     (screen-selected-window screen)))
	(xterm (screen-xterm screen)))
    (let ((update-name
	   (lambda (set-name variable length)
	     (let ((format
		    (variable-local-value (window-buffer window) variable)))
	       (if format
		   (set-name
		    xterm
		    (string-trim-right
		     (format-modeline-string window format length))))))))
      (update-name x-window-set-name
		   (ref-variable-object x-screen-name-format)
		   (screen-x-size screen))
      (update-name x-window-set-icon-name
		   (ref-variable-object x-screen-icon-name-format)
		   (variable-local-value
		    (window-buffer window)
		    (ref-variable-object x-screen-icon-name-length))))))

(define-command x-set-mouse-shape
  "Set mouse cursor shape to SHAPE.
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

(define-command x-mouse-select
  "Select window the mouse is on."
  ()
  (lambda ()
    (select-window (button-event/window (current-button-event)))))

(define-command x-mouse-keep-one-window
  "Select window mouse is on, then kill all other windows."
  ()
  (lambda ()
    ((ref-command x-mouse-select))
    ((ref-command delete-other-windows))))

(define-command x-mouse-select-and-split
  "Select window mouse is on, then split it vertically in half."
  ()
  (lambda ()
    ((ref-command x-mouse-select))
    ((ref-command split-window-vertically) false)))

(define-command x-mouse-set-point
  "Select window mouse is on, and move point to mouse position."
  ()
  (lambda ()
    (let ((button-event (current-button-event)))
      (let ((window (button-event/window button-event)))
	(select-window window)
	(set-current-point!
	 (or (window-coordinates->mark window
				       (button-event/x button-event)
				       (button-event/y button-event))
	     (buffer-end (window-buffer window))))))))

(define-command x-mouse-set-mark
  "Select window mouse is on, and set mark at mouse position.
Display cursor at that position for a second."
  ()
  (lambda ()
    (let ((button-event (current-button-event)))
      (let ((window (button-event/window button-event)))
	(select-window window)
	(let ((mark
	       (or (window-coordinates->mark window
					     (button-event/x button-event)
					     (button-event/y button-event))
		   (buffer-end (window-buffer window)))))
	  (push-current-mark! mark)
	  (mark-flash mark))))))

(define-command x-mouse-show-event
  "Show the mouse position in the minibuffer."
  ()
  (lambda ()
    (let ((button-event (current-button-event)))
      (message "window: " (button-event/window button-event)
	       " x: " (button-event/x button-event)
	       " y: " (button-event/y button-event)))))

(define-command x-mouse-ignore
  "Don't do anything."
  ()
  (lambda () unspecific))

(define x-button1-down (make-down-button 0))
(define x-button2-down (make-down-button 1))
(define x-button3-down (make-down-button 2))
(define x-button4-down (make-down-button 3))
(define x-button5-down (make-down-button 4))
(define x-button1-up (make-up-button 0))
(define x-button2-up (make-up-button 1))
(define x-button3-up (make-up-button 2))
(define x-button4-up (make-up-button 3))
(define x-button5-up (make-up-button 4))

(define-key 'fundamental x-button1-down 'x-mouse-set-point)