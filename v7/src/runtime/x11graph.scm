#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/x11graph.scm,v 1.6 1990/10/02 22:44:20 cph Rel $

Copyright (c) 1989, 1990 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; X Graphics Interface
;;; package: (runtime X-graphics)

(declare (usual-integrations))

(define-primitives
  (x-debug 1)
  (x-open-display 1)
  (x-close-display 1)
  (x-close-all-displays 0)
  (x-close-window 1)
  (x-display-flush 1)
  (x-display-process-events 2)
  (x-window-x-size 1)
  (x-window-y-size 1)
  (x-window-map 1)
  (x-window-unmap 1)
  (x-window-beep 1)
  (x-window-clear 1)
  (x-window-display 1)
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
  (x-window-set-size 3)
  (x-window-set-position 3)
  (x-window-starbase-filename 1)
  (x-graphics-open-window 3)
  (x-graphics-vdc-extent 1)
  (x-graphics-set-vdc-extent 5)
  (x-graphics-reset-clip-rectangle 1)
  (x-graphics-set-clip-rectangle 5)
  (x-graphics-move-cursor 3)
  (x-graphics-drag-cursor 3)
  (x-graphics-draw-line 5)
  (x-graphics-draw-point 3)
  (x-graphics-draw-string 4)
  (x-graphics-set-function 2)
  (x-graphics-set-fill-style 2)
  (x-graphics-set-line-style 2)
  (x-graphics-set-dashes 3))

(define (initialize-package!)
  (set! x-graphics-device-type
	(make-graphics-device-type
	 `((available? ,operation/available?)
	   (clear ,operation/clear)
	   (close ,operation/close)
	   (coordinate-limits ,operation/coordinate-limits)
	   (device-coordinate-limits ,operation/device-coordinate-limits)
	   (drag-cursor ,operation/drag-cursor)
	   (draw-line ,operation/draw-line)
	   (draw-point ,operation/draw-point)
	   (draw-text ,operation/draw-text)
	   (flush ,operation/flush)
	   (get-default ,operation/get-default)
	   (map-window ,operation/map-window)
	   (move-cursor ,operation/move-cursor)
	   (move-window ,operation/move-window)
	   (open ,operation/open)
	   (reset-clip-rectangle ,operation/reset-clip-rectangle)
	   (resize-window ,operation/resize-window)
	   (set-background-color ,operation/set-background-color)
	   (set-border-color ,operation/set-border-color)
	   (set-border-width ,operation/set-border-width)
	   (set-clip-rectangle ,operation/set-clip-rectangle)
	   (set-coordinate-limits ,operation/set-coordinate-limits)
	   (set-drawing-mode ,operation/set-drawing-mode)
	   (set-font ,operation/set-font)
	   (set-foreground-color ,operation/set-foreground-color)
	   (set-internal-border-width ,operation/set-internal-border-width)
	   (set-line-style ,operation/set-line-style)
	   (set-mouse-color ,operation/set-mouse-color)
	   (set-mouse-shape ,operation/set-mouse-shape)
	   (starbase-filename ,operation/starbase-filename)
	   (unmap-window ,operation/unmap-window))))
  unspecific)

(define x-graphics-device-type)

(define (x-geometry-string x y width height)
  (string-append (if (and width height)
		     (string-append (number->string width)
				    "x"
				    (number->string height))
		     "")
		 (if (and x y)
		     (string-append (if (negative? x) "" "+")
				    (number->string x)
				    (if (negative? y) "" "+")
				    (number->string y))
		     "")))

(define-structure (x-graphics-device (conc-name x-graphics-device/))
  (window false read-only true)
  (display false read-only true))

(define (x-graphics-device/process-events! device)
  (let ((xd (x-graphics-device/display device)))
    (let loop ()
      (if (x-display-process-events xd 0)
	  (loop)))))

(define (operation/available?)
  (implemented-primitive-procedure? x-graphics-open-window))

(define (operation/clear device)
  (x-graphics-device/process-events! device)
  (x-window-clear (x-graphics-device/window device)))

(define (operation/close device)
  (x-graphics-device/process-events! device)
  (x-close-window (x-graphics-device/window device)))

(define (operation/coordinate-limits device)
  (x-graphics-device/process-events! device)
  (let ((limits (x-graphics-vdc-extent (x-graphics-device/window device))))
    (values (vector-ref limits 0)
	    (vector-ref limits 1)
	    (vector-ref limits 2)
	    (vector-ref limits 3))))

(define (operation/device-coordinate-limits device)
  (x-graphics-device/process-events! device)
  (let ((xw (x-graphics-device/window device)))
    (values 0 (-1+ (x-window-y-size xw)) (-1+ (x-window-x-size xw)) 0)))

(define (operation/drag-cursor device x y)
  (x-graphics-device/process-events! device)
  (x-graphics-drag-cursor (x-graphics-device/window device) x y))

(define (operation/draw-line device x-start y-start x-end y-end)
  (x-graphics-device/process-events! device)
  (x-graphics-draw-line (x-graphics-device/window device)
			x-start y-start x-end y-end))

(define (operation/draw-point device x y)
  (x-graphics-device/process-events! device)
  (x-graphics-draw-point (x-graphics-device/window device) x y))

(define (operation/draw-text device x y string)
  (x-graphics-device/process-events! device)
  (x-graphics-draw-string (x-graphics-device/window device) x y string))

(define (operation/flush device)
  (x-display-flush (x-graphics-device/display device))
  (x-graphics-device/process-events! device))

(define (operation/get-default device resource-name class-name)
  (x-graphics-device/process-events! device)
  (x-window-get-default (x-graphics-device/window device)
			resource-name class-name))

(define (operation/map-window device)
  (x-graphics-device/process-events! device)
  (x-window-map (x-graphics-device/window device)))

(define (operation/move-cursor device x y)
  (x-graphics-device/process-events! device)
  (x-graphics-move-cursor (x-graphics-device/window device) x y))

(define (operation/move-window device x y)
  (x-graphics-device/process-events! device)
  (x-window-set-position (x-graphics-device/window device) x y))

(define (operation/open display geometry #!optional suppress-map?)
  (let ((xw
	 (x-graphics-open-window
	  (if (or (not display) (string? display))
	      (let ((d (x-open-display display)))
		(if (not d)
		    (error "unable to open display" display))
		d)
	      display)
	  geometry
	  (and (not (default-object? suppress-map?))
	       suppress-map?))))
    (make-x-graphics-device xw (x-window-display xw))))

(define (operation/reset-clip-rectangle device)
  (x-graphics-device/process-events! device)
  (x-graphics-reset-clip-rectangle (x-graphics-device/window device)))

(define (operation/resize-window device width height)
  (x-graphics-device/process-events! device)
  (x-window-set-size (x-graphics-device/window device) width height))

(define (operation/set-background-color device color)
  (x-graphics-device/process-events! device)
  (x-window-set-background-color (x-graphics-device/window device) color))

(define (operation/set-border-color device color)
  (x-graphics-device/process-events! device)
  (x-window-set-border-color (x-graphics-device/window device) color))

(define (operation/set-border-width device width)
  (x-graphics-device/process-events! device)
  (x-window-set-border-width (x-graphics-device/window device) width))

(define (operation/set-coordinate-limits device x-left y-bottom x-right y-top)
  (x-graphics-device/process-events! device)
  (x-graphics-set-vdc-extent (x-graphics-device/window device)
			     x-left y-bottom x-right y-top))

(define (operation/set-clip-rectangle device x-left y-bottom x-right y-top)
  (x-graphics-device/process-events! device)
  (x-graphics-set-clip-rectangle (x-graphics-device/window device)
				 x-left y-bottom x-right y-top))

(define (operation/set-drawing-mode device mode)
  (x-graphics-device/process-events! device)
  (x-graphics-set-function (x-graphics-device/window device) mode))

(define (operation/set-font device font)
  (x-graphics-device/process-events! device)
  (x-window-set-font (x-graphics-device/window device) font))

(define (operation/set-foreground-color device color)
  (x-graphics-device/process-events! device)
  (x-window-set-foreground-color (x-graphics-device/window device) color))

(define (operation/set-internal-border-width device width)
  (x-graphics-device/process-events! device)
  (x-window-set-internal-border-width (x-graphics-device/window device) width))

(define (operation/set-line-style device line-style)
  (x-graphics-device/process-events! device)
  (if (not (and (exact-nonnegative-integer? line-style)
		(< line-style 8)))
      (error:illegal-datum line-style 'SET-LINE-STYLE))
  (let ((xw (x-graphics-device/window device)))
    (if (zero? line-style)
	(x-graphics-set-line-style xw 0)
	(begin
	  (x-graphics-set-line-style xw 2)
	  (x-graphics-set-dashes
	   xw
	   0
	   (vector-ref '#("\010\010"
			  "\001\001"
			  "\015\001\001\001"
			  "\013\001\001\001\001\001"
			  "\013\005"
			  "\014\001\002\001"
			  "\011\001\002\001\002\001")
		       (-1+ line-style)))))))

(define (operation/set-mouse-color device color)
  (x-graphics-device/process-events! device)
  (x-window-set-mouse-color (x-graphics-device/window device) color))

(define (operation/set-mouse-shape device shape)
  (x-graphics-device/process-events! device)
  (x-window-set-mouse-shape (x-graphics-device/window device) shape))

(define (operation/starbase-filename device)
  (x-graphics-device/process-events! device)
  (x-window-starbase-filename (x-graphics-device/window device)))

(define (operation/unmap-window device)
  (x-graphics-device/process-events! device)
  (x-window-unmap (x-graphics-device/window device)))