#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

This file is part of an X11-screen plugin for MIT/GNU Scheme.

This plugin is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

This plugin is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this plugin; if not, write to the Free Software Foundation,
Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.

|#

;;;; X Commands

(declare (usual-integrations))

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

(define-command set-font
  "Set text font of selected frame to FONT."
  (lambda ()
    (list (prompt-for-x-font-name "Set font" #f)))
  (lambda (font)
    (let ((xterm (current-xterm)))
      (let ((x-size (xterm-x-size xterm))
	    (y-size (xterm-y-size xterm)))
	(if (not (x-window-set-font xterm font))
	    (editor-error "Unknown font name: " font))
	(xterm-reconfigure xterm x-size y-size)))))

(define-command set-default-font
  "Set text font to be used in new frames."
  (lambda ()
    (list (prompt-for-x-font-name "Set default font" #f)))
  (lambda (font)
    (x-set-default-font (screen-display (selected-screen)) font)))

(define-command font-apropos
  "Show all X fonts whose names match a given regular expression."
  "sFont apropos (regexp)"
  (lambda (regexp)
    (with-output-to-help-display
     (lambda ()
       (font-apropos regexp)))))

(define-command apropos-font
  (command-description (ref-command-object font-apropos))
  (command-interactive-specification (ref-command-object font-apropos))
  (command-procedure (ref-command-object font-apropos)))

(define (font-apropos regexp)
  (for-each (lambda (font)
              (write-string font)
              (newline))
            (string-table-apropos (x-font-name-table) regexp)))

(define (prompt-for-x-font-name prompt default . options)
  (apply prompt-for-string-table-name prompt default (x-font-name-table)
         options))

(define (x-font-name-table)
  (build-x-font-name-table (screen-display (selected-screen))
                           "*"
                           #f))

(define (build-x-font-name-table display pattern limit)
  (let ((font-name-vector (x-list-fonts display pattern limit))
        (font-name-table (make-string-table)))
    (do ((index 0 (fix:+ index 1)))
        ((fix:= index (vector-length font-name-vector)))
      (let ((font-name (vector-ref font-name-vector index)))
        (string-table-put! font-name-table font-name font-name)))
    font-name-table))

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

(define-command set-mouse-shape
  "Set mouse cursor shape for selected frame to SHAPE.
SHAPE must be the (string) name of one of the known cursor shapes.
When called interactively, completion is available on the input."
  (lambda ()
    (list (prompt-for-alist-value "Set mouse shape"
				  (map (lambda (x) (cons x x))
				       mouse-cursor-shapes))))
  (lambda (shape)
    (x-window-set-mouse-shape
     (current-xterm)
     (let loop ((shapes mouse-cursor-shapes) (index 0))
       (if (not (pair? shapes))
	   (error "Unknown shape name:" shape))
       (if (string-ci=? shape (car shapes))
	   index
	   (loop (cdr shapes) (fix:+ index 1)))))))

(define mouse-cursor-shapes
  '("X-cursor" "arrow" "based-arrow-down" "based-arrow-up" "boat" "bogosity"
	       "bottom-left-corner" "bottom-right-corner" "bottom-side"
	       "bottom-tee" "box-spiral" "center-ptr" "circle" "clock"
	       "coffee-mug" "cross" "cross-reverse" "crosshair" "diamond-cross"
	       "dot" "dotbox" "double-arrow" "draft-large" "draft-small"
	       "draped-box" "exchange" "fleur" "gobbler" "gumby" "hand1"
	       "hand2" "heart" "icon" "iron-cross" "left-ptr" "left-side"
	       "left-tee" "leftbutton" "ll-angle" "lr-angle" "man"
	       "middlebutton" "mouse" "pencil" "pirate" "plus" "question-arrow"
	       "right-ptr" "right-side" "right-tee" "rightbutton" "rtl-logo"
	       "sailboat" "sb-down-arrow" "sb-h-double-arrow" "sb-left-arrow"
	       "sb-right-arrow" "sb-up-arrow" "sb-v-double-arrow" "shuttle"
	       "sizing" "spider" "spraycan" "star" "target" "tcross"
	       "top-left-arrow" "top-left-corner" "top-right-corner"
	       "top-side" "top-tee" "trek" "ul-angle" "umbrella" "ur-angle"
	       "watch" "xterm"))

;;;; Mouse Commands
;;; (For compatibility with old code.)

(define-syntax define-old-mouse-command
  (sc-macro-transformer
   (lambda (form environment)
     (let ((name (cadr form)))
       `(DEFINE ,(symbol 'EDWIN-COMMAND$X- name)
	  ,(close-syntax (symbol 'EDWIN-COMMAND$ name)
			 environment))))))

(define-old-mouse-command set-foreground-color)
(define-old-mouse-command set-background-color)
(define-old-mouse-command set-border-color)
(define-old-mouse-command set-cursor-color)
(define-old-mouse-command set-mouse-color)
(define-old-mouse-command set-font)
(define-old-mouse-command set-border-width)
(define-old-mouse-command set-internal-border-width)
(define-old-mouse-command set-mouse-shape)
(define-old-mouse-command mouse-select)
(define-old-mouse-command mouse-keep-one-window)
(define-old-mouse-command mouse-select-and-split)
(define-old-mouse-command mouse-set-point)
(define-old-mouse-command mouse-set-mark)
(define-old-mouse-command mouse-show-event)
(define-old-mouse-command mouse-ignore)

(define edwin-command$x-set-size edwin-command$set-frame-size)
(define edwin-command$x-set-position edwin-command$set-frame-position)
(define edwin-command$x-set-window-name edwin-command$set-frame-name)
(define edwin-command$x-set-icon-name edwin-command$set-frame-icon-name)
(define edwin-command$x-raise-screen edwin-command$raise-frame)
(define edwin-command$x-lower-screen edwin-command$lower-frame)

(define edwin-variable$x-screen-name-format edwin-variable$frame-name-format)
(define edwin-variable$x-screen-name-length edwin-variable$frame-name-length)
(define edwin-variable$x-screen-length-format edwin-variable$frame-name-length)

(define edwin-variable$x-screen-icon-name-format
  edwin-variable$frame-icon-name-format)
(define edwin-variable$x-screen-icon-name-length
  edwin-variable$frame-icon-name-length)

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