#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/x11graph.scm,v 1.3 1989/06/27 10:16:02 cph Rel $

Copyright (c) 1989 Massachusetts Institute of Technology

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
  (x-window-read-event-flags! 1)
  (x-window-x-size 1)
  (x-window-y-size 1)
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
  (x-graphics-set-dashes 3)
  (x-graphics-process-events 1))

(define (initialize-package!)
  (set! x-graphics-device-type
	(make-graphics-device-type
	 `((available? ,operation/available?)
	   (clear ,x-window-clear)
	   (close ,x-close-window)
	   (coordinate-limits ,operation/coordinate-limits)
	   (device-coordinate-limits ,operation/device-coordinate-limits)
	   (drag-cursor ,x-graphics-drag-cursor)
	   (draw-line ,x-graphics-draw-line)
	   (draw-point ,x-graphics-draw-point)
	   (draw-text ,x-graphics-draw-string)
	   (flush ,operation/flush)
	   (get-default ,x-window-get-default)
	   (map-window ,x-window-map)
	   (move-cursor ,x-graphics-move-cursor)
	   (move-window ,x-window-set-position)
	   (open ,operation/open)
	   (reset-clip-rectangle ,x-graphics-reset-clip-rectangle)
	   (resize-window ,x-window-set-size)
	   (set-background-color ,x-window-set-background-color)
	   (set-border-color ,x-window-set-border-color)
	   (set-border-width ,x-window-set-border-width)
	   (set-clip-rectangle ,x-graphics-set-clip-rectangle)
	   (set-coordinate-limits ,x-graphics-set-vdc-extent)
	   (set-drawing-mode ,x-graphics-set-function)
	   (set-font ,x-window-set-font)
	   (set-foreground-color ,x-window-set-foreground-color)
	   (set-internal-border-width ,x-window-set-internal-border-width)
	   (set-line-style ,operation/set-line-style)
	   (set-mouse-color ,x-window-set-mouse-color)
	   (set-mouse-shape ,x-window-set-mouse-shape)
	   (starbase-filename ,x-window-starbase-filename)
	   (unmap-window ,x-window-unmap))))
  (add-event-receiver! event:before-exit
    (lambda ()
      (if (implemented-primitive-procedure? x-close-all-displays)
	  (x-close-all-displays))))  unspecific)

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

(define (operation/available?)
  (implemented-primitive-procedure? x-graphics-open-window))

(define (operation/open display geometry #!optional suppress-map?)
  (x-graphics-open-window
   (if (or (not display) (string? display))
       (let ((d (x-open-display display)))
	 (if (not d)
	     (error "unable to open display" display))
	 d)
       display)
   geometry
   (and (not (default-object? suppress-map?))
	suppress-map?)))

(define (operation/flush xw)
  (x-window-flush xw)
  (x-graphics-process-events xw))

(define (operation/device-coordinate-limits xw)
  (x-graphics-process-events xw)
  (values 0 (-1+ (x-window-y-size xw)) (-1+ (x-window-x-size xw)) 0))

(define (operation/coordinate-limits xw)
  (let ((limits (x-graphics-vdc-extent xw)))
    (values (vector-ref limits 0)
	    (vector-ref limits 1)
	    (vector-ref limits 2)
	    (vector-ref limits 3))))

(define (operation/set-line-style xw line-style)
  (cond ((zero? line-style)
	 (x-graphics-set-line-style xw 0))
	((and (integer? line-style) (<= 1 line-style 7))
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
		      (-1+ line-style))))
	(else
	 (error "Illegal line style" line-style))))