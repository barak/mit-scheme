#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/starbase.scm,v 1.9 1992/02/04 23:59:26 cph Exp $

Copyright (c) 1989-92 Massachusetts Institute of Technology

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

;;;; Starbase Graphics Interface
;;; package: (runtime starbase-graphics)

(declare (usual-integrations))

(define-primitives
  (starbase-open-device 2)
  (starbase-close-device 1)
  (starbase-flush 1)
  (starbase-clear 1)
  (starbase-move-cursor 3)
  (starbase-drag-cursor 3)
  (starbase-draw-line 5)
  (starbase-draw-point 3)
  (starbase-set-line-style 2)
  (starbase-set-drawing-mode 2)
  (starbase-device-coordinates 1)
  (starbase-set-vdc-extent 5)
  (starbase-reset-clip-rectangle 1)
  (starbase-set-clip-rectangle 5)
  (starbase-draw-text 4)
  (starbase-set-text-height 2)
  (starbase-set-text-aspect 2)
  (starbase-set-text-slant 2)
  (starbase-set-text-rotation 2)
  (starbase-color-map-size 1)
  (starbase-define-color 5)
  (starbase-set-line-color 2)
  (starbase-write-image-file 3))

(define (initialize-package!)
  (set! starbase-graphics-device-type
	(make-graphics-device-type
	 `((available? ,operation/available?)
	   (clear ,operation/clear)
	   (close ,operation/close)
	   (color-map-size ,operation/color-map-size)
	   (coordinate-limits ,operation/coordinate-limits)
	   (define-color ,operation/define-color)
	   (device-coordinate-limits ,operation/device-coordinate-limits)
	   (drag-cursor ,operation/drag-cursor)
	   (draw-line ,operation/draw-line)
	   (draw-point ,operation/draw-point)
	   (draw-text ,operation/draw-text)
	   (flush ,operation/flush)
	   (move-cursor ,operation/move-cursor)
	   (open ,operation/open)
	   (reset-clip-rectangle ,operation/reset-clip-rectangle)
	   (set-clip-rectangle ,operation/set-clip-rectangle)
	   (set-coordinate-limits ,operation/set-coordinate-limits)
	   (set-drawing-mode ,operation/set-drawing-mode)
	   (set-line-color ,operation/set-line-color)
	   (set-line-style ,operation/set-line-style)
	   (set-text-aspect ,operation/set-text-aspect)
	   (set-text-height ,operation/set-text-height)
	   (set-text-rotation ,operation/set-text-rotation)
	   (set-text-slant ,operation/set-text-slant)
	   (text-aspect ,operation/text-aspect)
	   (text-height ,operation/text-height)
	   (text-rotation ,operation/text-rotation)
	   (text-slant ,operation/text-slant)
	   (write-image-file ,operation/write-image-file))))
  unspecific)

(define starbase-graphics-device-type)

(define-structure (starbase-graphics-descriptor
		   (conc-name starbase-graphics-descriptor/)
		   (constructor make-starbase-descriptor (identifier)))
  (identifier false read-only true)
  x-left
  y-bottom
  x-right
  y-top
  text-height
  text-aspect
  text-slant
  text-rotation)

(define (starbase-device/identifier device)
  (starbase-graphics-descriptor/identifier
   (graphics-device/descriptor device)))

(let-syntax
    ((define-accessors-and-mutators
       (macro (name)
	 `(BEGIN
	    (DEFINE (,(symbol-append 'STARBASE-DEVICE/ name) DEVICE)
	      (,(symbol-append 'STARBASE-GRAPHICS-DESCRIPTOR/ name)
	       (GRAPHICS-DEVICE/DESCRIPTOR DEVICE)))
	    (DEFINE (,(symbol-append 'SET-STARBASE-DEVICE/ name '!)
		     DEVICE VALUE)
	      (,(symbol-append 'SET-STARBASE-GRAPHICS-DESCRIPTOR/ name '!)
	       (GRAPHICS-DEVICE/DESCRIPTOR DEVICE)
	       VALUE))))))
  (define-accessors-and-mutators x-left)
  (define-accessors-and-mutators y-bottom)
  (define-accessors-and-mutators x-right)
  (define-accessors-and-mutators y-top)
  (define-accessors-and-mutators text-height)
  (define-accessors-and-mutators text-aspect)
  (define-accessors-and-mutators text-slant)
  (define-accessors-and-mutators text-rotation))

(define (operation/available?)
  (implemented-primitive-procedure? starbase-open-device))

(define (operation/open device-name driver-name)
  (let ((identifier (starbase-open-device device-name driver-name)))
    (and identifier
	 (let ((device (make-starbase-descriptor identifier)))
	   (operation/set-coordinate-limits device -1 -1 1 1)
	   (operation/set-text-height device 0.1)
	   (operation/set-text-aspect device 1)
	   (operation/set-text-slant device 0)
	   (operation/set-text-rotation device 0)
	   device))))

(define (operation/close device)
  (starbase-close-device (starbase-device/identifier device)))

(define (operation/flush device)
  (starbase-flush (starbase-device/identifier device)))

(define (operation/device-coordinate-limits device)
  (let ((limits
	 (starbase-device-coordinates
	  (starbase-device/identifier device))))
    (values (vector-ref limits 0)
	    (vector-ref limits 1)
	    (vector-ref limits 2)
	    (vector-ref limits 3))))

(define (operation/coordinate-limits device)
  (values (starbase-device/x-left device)
	  (starbase-device/y-bottom device)
	  (starbase-device/x-right device)
	  (starbase-device/y-top device)))

(define (operation/set-coordinate-limits device x-left y-bottom x-right y-top)
  (starbase-set-vdc-extent (starbase-device/identifier device)
			   x-left y-bottom x-right y-top)
  (set-starbase-device/x-left! device x-left)
  (set-starbase-device/y-bottom! device y-bottom)
  (set-starbase-device/x-right! device x-right)
  (set-starbase-device/y-top! device y-top))

(define (operation/reset-clip-rectangle device)
  (starbase-reset-clip-rectangle (starbase-device/identifier device)))

(define (operation/set-clip-rectangle device x-left y-bottom x-right y-top)
  (starbase-set-clip-rectangle (starbase-device/identifier device)
			       x-left y-bottom x-right y-top))

(define (operation/set-drawing-mode device drawing-mode)
  (starbase-set-drawing-mode (starbase-device/identifier device) drawing-mode))

(define (operation/set-line-style device line-style)
  (starbase-set-line-style (starbase-device/identifier device) line-style))

(define (operation/clear device)
  (starbase-clear (starbase-device/identifier device)))

(define (operation/draw-point device x y)
  (starbase-draw-point (starbase-device/identifier device) x y))

(define (operation/move-cursor device x y)
  (starbase-move-cursor (starbase-device/identifier device) x y))

(define (operation/drag-cursor device x y)
  (starbase-drag-cursor (starbase-device/identifier device) x y))

(define (operation/draw-line device x-start y-start x-end y-end)
  (starbase-draw-line (starbase-device/identifier device)
		      x-start y-start x-end y-end))

(define (operation/draw-text device x y text)
  (starbase-draw-text (starbase-device/identifier device) x y text))

;;; Custom Operations

(define (operation/write-image-file device filename invert?)
  (starbase-write-image-file (starbase-device/identifier device)
			     (->namestring (merge-pathnames filename))
			     invert?))

(define (operation/text-height device)
  (starbase-device/text-height device))

(define (operation/text-aspect device)
  (starbase-device/text-aspect device))

(define (operation/text-slant device)
  (starbase-device/text-slant device))

(define (operation/text-rotation device)
  (starbase-device/text-rotation device))

(define (operation/set-text-height device height)
  (starbase-set-text-height (starbase-device/identifier device) height)
  (set-starbase-device/text-height! device height))

(define (operation/set-text-aspect device aspect)
  (starbase-set-text-aspect (starbase-device/identifier device) aspect)
  (set-starbase-device/text-aspect! device aspect))

(define (operation/set-text-slant device slant)
  (starbase-set-text-slant (starbase-device/identifier device) slant)
  (set-starbase-device/text-slant! device slant))

(define (operation/set-text-rotation device rotation)
  (starbase-set-text-rotation (starbase-device/identifier device) rotation)
  (set-starbase-device/text-rotation! device rotation))

(define (operation/color-map-size device)
  (starbase-color-map-size (starbase-device/identifier device)))

(define (operation/define-color device color-index red green blue)
  (starbase-define-color (starbase-device/identifier device)
			 color-index red green blue))

(define (operation/set-line-color device color-index)
  (starbase-set-line-color (starbase-device/identifier device) color-index))