;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/xcom.scm,v 1.1 1989/06/21 10:42:34 cph Rel $
;;;
;;;	Copyright (c) 1989 Massachusetts Institute of Technology
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
  (x-debug 1)
  (x-open-display 1)
  (x-close-display 1)
  (x-close-all-displays 0)
  (x-close-window 1)
  (x-window-x-size 1)
  (x-window-y-size 1)
  (x-window-set-size 3)
  (x-window-map 1)
  (x-window-unmap 1)
  (x-window-beep 1)
  (x-window-clear 1)
  (x-window-flush 1)
  (x-window-get-default 3)
  (x-window-set-foreground-color 2)
  (x-window-set-background-color 2)
  (x-window-set-border-color 2)
  (x-window-set-cursor-color 2)
  (x-window-set-mouse-color 2)
  (x-window-set-mouse-shape 2)
  (x-window-set-font 2)
  (x-window-set-border-width 2)
  (x-window-set-internal-border-width 2)
  (xterm-x-size 1)
  (xterm-y-size 1)
  (xterm-set-size 3))

(define (current-xterm)
  (screen-xterm (current-screen)))

(define-command x-set-foreground-color
  "Set foreground (text) color to COLOR."
  "sSet foreground color"
  (lambda (color)
    (x-window-set-foreground-color (current-xterm) color)
    (update-screen! (current-screen) true)))

(define-command x-set-background-color
  "Set background color to COLOR."
  "sSet background color"
  (lambda (color)
    (let ((xterm (current-xterm)))
      (x-window-set-background-color xterm color)
      (x-window-clear xterm))
    (update-screen! (current-screen) true)))

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

(define-command x-set-size
  "Set size of editor screen to (WIDTH, HEIGHT)."
  "nScreen width\nnScreen height"
  (lambda (width height)
    (xterm-set-size (current-xterm) (max 2 width) (max 2 height))))
(define-command x-set-border-width
  "Set width of border to WIDTH."
  "nSet border width"
  (lambda (width)
    (x-window-set-border-width (current-xterm) (max 0 width))
    (update-screen! (current-screen) true)))

(define-command x-set-internal-border-width
  "Set width of internal border to WIDTH."
  "nSet internal border width"
  (lambda (width)
    (x-window-set-internal-border-width (current-xterm) (max 0 width))))

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

(define (x-switch-to-window window x y)
  x y					;ignore
  (select-window window))

(define (x-move-to-coordinates window x y)
  (select-window window)
  (set-current-point!
   (or (window-coordinates->mark window x y)
       (buffer-end (window-buffer window)))))

(define-key 'fundamental button1-down x-move-to-coordinates)
(define-key 'fundamental button3-down x-switch-to-window)