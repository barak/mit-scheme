#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; X Graphics Interface
;;; package: (runtime x-graphics)

(declare (usual-integrations))
(declare (integrate-external "graphics"))

(define-primitives
  (x-close-all-displays 0)
  (x-display-descriptor 1)
  (x-display-get-default 3)
  (x-display-process-events 2)
  (x-font-structure 2)
  (x-window-beep 1)
  (x-window-clear 1)
  (x-window-colormap 1)
  (x-window-depth 1)
  (x-window-event-mask 1)
  (x-window-flush 1)
  (x-window-iconify 1)
  (x-window-id 1)
  (x-window-lower 1)
  (x-window-map 1)
  (x-window-query-pointer 1)
  (x-window-raise 1)
  (x-window-set-background-color 2)
  (x-window-set-border-color 2)
  (x-window-set-border-width 2)
  (x-window-set-cursor-color 2)
  (x-window-set-event-mask 2)
  (x-window-set-font 2)
  (x-window-set-foreground-color 2)
  (x-window-set-icon-name 2)
  (x-window-set-input-hint 2)
  (x-window-set-internal-border-width 2)
  (x-window-set-mouse-color 2)
  (x-window-set-mouse-shape 2)
  (x-window-set-name 2)
  (x-window-set-position 3)
  (x-window-set-size 3)
  (x-window-starbase-filename 1)
  (x-window-visual 1)
  (x-window-withdraw 1)
  (x-window-x-size 1)
  (x-window-y-size 1)
  (x-graphics-copy-area 8)
  (x-graphics-drag-cursor 3)
  (x-graphics-draw-arc 8)
  (x-graphics-draw-line 5)
  (x-graphics-draw-lines 3)
  (x-graphics-draw-point 3)
  (x-graphics-draw-points 3)
  (x-graphics-draw-string 4)
  (x-graphics-draw-image-string 4)
  (x-graphics-fill-polygon 2)
  (x-graphics-map-x-coordinate 2)
  (x-graphics-map-y-coordinate 2)
  (x-graphics-move-cursor 3)
  (x-graphics-open-window 3)
  (x-graphics-reconfigure 3)
  (x-graphics-reset-clip-rectangle 1)
  (x-graphics-set-clip-rectangle 5)
  (x-graphics-set-dashes 3)
  (x-graphics-set-fill-style 2)
  (x-graphics-set-function 2)
  (x-graphics-set-line-style 2)
  (x-graphics-set-vdc-extent 5)
  (x-graphics-vdc-extent 1)
  (x-bytes-into-image 2)
  (x-create-image 3)
  (x-destroy-image 1)
  (x-display-image 8)
  (x-get-pixel-from-image 3)
  (x-set-pixel-in-image 4)
  (x-allocate-color 4)
  (x-create-colormap 3)
  (x-free-colormap 1)
  (x-query-color 2)
  (x-set-window-colormap 2)
  (x-store-color 5)
  (x-store-colors 2)
  (x-visual-deallocate 1))

;; These constants must match "microcode/x11base.c"
(define-integrable event-type:button-down 0)
(define-integrable event-type:button-up 1)
(define-integrable event-type:configure 2)
(define-integrable event-type:enter 3)
(define-integrable event-type:focus-in 4)
(define-integrable event-type:focus-out 5)
(define-integrable event-type:key-press 6)
(define-integrable event-type:leave 7)
(define-integrable event-type:motion 8)
(define-integrable event-type:expose 9)
(define-integrable event-type:delete-window 10)
(define-integrable event-type:map 11)
(define-integrable event-type:unmap 12)
(define-integrable event-type:take-focus 13)
(define-integrable event-type:visibility 14)
(define-integrable number-of-event-types 15)

;; This mask contains button-down, button-up,configure, enter,
;; focus-in, focus-out, key-press, leave, motion, delete-window, map,
;; unmap, and visibility.
(define-integrable event-mask:normal #x5dff)

;; This mask additionally contains take-focus.
(define-integrable event-mask:ignore-focus #x7dff)

;; This mask contains button-down.
(define-integrable user-event-mask:default #x0001)

;;;; X graphics device

(define (initialize-package!)
  (set! x-graphics-device-type
	(make-graphics-device-type
	 'X
	 `((available? ,x-graphics/available?)
	   (clear ,x-graphics/clear)
	   (close ,x-graphics/close-window)
	   (color? ,x-graphics/color?)
	   (coordinate-limits ,x-graphics/coordinate-limits)
	   (copy-area ,x-graphics/copy-area)
	   (create-colormap ,create-x-colormap)
	   (create-image ,x-graphics/create-image)
	   (device-coordinate-limits ,x-graphics/device-coordinate-limits)
	   (drag-cursor ,x-graphics/drag-cursor)
	   (draw-arc ,x-graphics/draw-arc)
	   (draw-circle ,x-graphics/draw-circle)
	   (draw-image ,image/draw)
	   (draw-line ,x-graphics/draw-line)
	   (draw-lines ,x-graphics/draw-lines)
	   (draw-point ,x-graphics/draw-point)
	   (draw-points ,x-graphics/draw-points)
	   (draw-subimage ,image/draw-subimage)
	   (draw-text ,x-graphics/draw-text)
	   (draw-text-opaque ,x-graphics/draw-text-opaque)
	   (fill-circle ,x-graphics/fill-circle)
	   (fill-polygon ,x-graphics/fill-polygon)
	   (flush ,x-graphics/flush)
	   (font-structure ,x-graphics/font-structure)
	   (get-colormap ,x-graphics/get-colormap)
	   (get-default ,x-graphics/get-default)
	   (iconify-window ,x-graphics/iconify-window)
	   (image-depth ,x-graphics/image-depth)
	   (lower-window ,x-graphics/lower-window)
	   (map-window ,x-graphics/map-window)
	   (move-cursor ,x-graphics/move-cursor)
	   (move-window ,x-graphics/move-window)
	   (open ,x-graphics/open)
	   (open? ,x-graphics/open-window?)
	   (query-pointer ,x-graphics/query-pointer)
	   (raise-window ,x-graphics/raise-window)
	   (reset-clip-rectangle ,x-graphics/reset-clip-rectangle)
	   (resize-window ,x-graphics/resize-window)
	   (set-background-color ,x-graphics/set-background-color)
	   (set-border-color ,x-graphics/set-border-color)
	   (set-border-width ,x-graphics/set-border-width)
	   (set-clip-rectangle ,x-graphics/set-clip-rectangle)
	   (set-colormap ,x-graphics/set-colormap)
	   (set-coordinate-limits ,x-graphics/set-coordinate-limits)
	   (set-drawing-mode ,x-graphics/set-drawing-mode)
	   (set-font ,x-graphics/set-font)
	   (set-foreground-color ,x-graphics/set-foreground-color)
	   (set-icon-name ,x-graphics/set-icon-name)
	   (set-input-hint ,x-graphics/set-input-hint)
	   (set-internal-border-width ,x-graphics/set-internal-border-width)
	   (set-line-style ,x-graphics/set-line-style)
	   (set-mouse-color ,x-graphics/set-mouse-color)
	   (set-mouse-shape ,x-graphics/set-mouse-shape)
	   (set-window-name ,x-graphics/set-window-name)
	   (starbase-filename ,x-graphics/starbase-filename)
	   (visual-info ,x-graphics/visual-info)
	   (withdraw-window ,x-graphics/withdraw-window))))
  (set! display-finalizer
	(make-gc-finalizer (ucode-primitive x-close-display 1)
			   x-display?
			   x-display/xd
			   set-x-display/xd!))
  (initialize-image-datatype)
  (initialize-colormap-datatype))

(define (x-graphics/available?)
  (implemented-primitive-procedure?
   (ucode-primitive x-graphics-open-window 3)))

(define x-graphics-device-type)

;;;; Open/Close Displays

(define display-finalizer)

(define-structure (x-display
		   (conc-name x-display/)
		   (constructor make-x-display (name xd))
		   (print-procedure
		    (standard-unparser-method 'X-DISPLAY
		      (lambda (display port)
			(write-char #\space port)
			(write (x-display/name display) port)))))
  (name #f read-only #t)
  xd
  (window-finalizer (make-gc-finalizer (ucode-primitive x-close-window 1)
				       x-window?
				       x-window/xw
				       set-x-window/xw!)
		    read-only #t)
  (event-queue (make-queue))
  (properties (make-1d-table) read-only #t))

(define (x-graphics/open-display name)
  (let ((name
	 (cond ((not name)
		(or x-graphics-default-display-name
		    (let ((name (get-environment-variable "DISPLAY")))
		      (if (not name)
			  (error "No DISPLAY environment variable."))
		      name)))
	       ((string? name)
		name)
	       (else
		(error:wrong-type-argument name
					   "string or #f"
					   x-graphics/open-display)))))
    (or (search-gc-finalizer display-finalizer
	  (lambda (display)
	    (string=? (x-display/name display) name)))
	(let ((xd ((ucode-primitive x-open-display 1) name)))
	  (if (not xd)
	      (error "Unable to open display:" name))
	  (let ((display (make-x-display name xd)))
	    (add-to-gc-finalizer! display-finalizer display)
	    (make-event-previewer display)
	    display)))))

(define (x-graphics/close-display display)
  (without-interrupts
   (lambda ()
     (if (x-display/xd display)
	 (begin
	   (remove-all-from-gc-finalizer! (x-display/window-finalizer display))
	   (remove-from-gc-finalizer! display-finalizer display))))))

(define (x-graphics/open-display? display)
  (if (x-display/xd display) #t #f))

(define (make-event-previewer display)
  (let ((registration))
    (set! registration
	  (permanently-register-io-thread-event
	   (x-display-descriptor (x-display/xd display))
	   'READ
	   (current-thread)
	   (lambda (mode)
	     mode
	     (call-with-current-continuation
	      (lambda (continuation)
		(bind-condition-handler
		    (list condition-type:bad-range-argument
			  condition-type:wrong-type-argument)
		    (lambda (condition)
		      ;; If X-DISPLAY-PROCESS-EVENTS or
		      ;; X-DISPLAY-DESCRIPTOR signals an argument error
		      ;; on its display argument, that means the
		      ;; display has been closed.
		      condition
		      (deregister-io-thread-event registration)
		      (continuation unspecific))
		  (lambda ()
		    (let ((event
			   (x-display-process-events (x-display/xd display)
						     2)))
		      (if event
			  (process-event display event))))))))))
    registration))

(define (read-event display)
  (letrec ((loop
	    (let ((queue (x-display/event-queue display)))
	      (lambda ()
		(if (queue-empty? queue)
		    (begin
		      (%read-and-process-event display)
		      (loop))
		    (dequeue! queue))))))
    (with-thread-events-blocked loop)))

(define (%read-and-process-event display)
  (let ((event
	 (and (eq? 'READ
		   (test-for-io-on-descriptor
		    (x-display-descriptor (x-display/xd display))
		    #t
		    'READ))
	      (x-display-process-events (x-display/xd display) 1))))
    (if event
	(process-event display event))))

(define (discard-events display)
  (letrec ((loop
	    (let ((queue (x-display/event-queue display)))
	      (lambda ()
		(cond ((not (queue-empty? queue))
		       (dequeue! queue)
		       (loop))
		      ((x-display-process-events (x-display/xd display) 2)
		       =>
		       (lambda (event)
			 (process-event display event)
			 (loop))))))))
    (with-thread-events-blocked loop)))

(define (process-event display event)
  (without-interrupts
   (lambda ()
     (let ((window
	    (search-gc-finalizer (x-display/window-finalizer display)
	      (let ((xw (vector-ref event 1)))
		(lambda (window)
		  (eq? (x-window/xw window) xw))))))
       (if window
	   (let ((type (vector-ref event 0)))
	     (let ((handler (vector-ref event-handlers type)))
	       (if handler
		   (handler window event)))
	     (if (or (fix:= event-type:delete-window type)
		     (not (fix:= 0
				 (fix:and (fix:lsh 1 type)
					  (x-window/user-event-mask window)))))
		 (begin
		   ;; This would prefer to be the graphics device, but
		   ;; that's not available from here.
		   (vector-set! event 1 window)
		   (enqueue!/unsafe (x-display/event-queue display)
				    event)))))))))

(define event-handlers
  (make-vector number-of-event-types #f))

(define-integrable (define-event-handler event-type handler)
  (vector-set! event-handlers event-type handler))

(define-event-handler event-type:configure
  (lambda (window event)
    window
    (x-graphics-reconfigure (vector-ref event 1)
			    (vector-ref event 2)
			    (vector-ref event 3))
    (if (eq? 'NEVER (x-window/mapped? window))
	(set-x-window/mapped?! window #t))))

(define-event-handler event-type:delete-window
  (lambda (window event)
    event
    (close-x-window window)))

(define-event-handler event-type:map
  (lambda (window event)
    event
    (set-x-window/mapped?! window #t)))

(define-event-handler event-type:unmap
  (lambda (window event)
    event
    (set-x-window/mapped?! window #f)))

(define-event-handler event-type:visibility
  (lambda (window event)
    (case (vector-ref event 2)
      ((0) (set-x-window/visibility! window 'UNOBSCURED))
      ((1) (set-x-window/visibility! window 'PARTIALLY-OBSCURED))
      ((2) (set-x-window/visibility! window 'OBSCURED)))))

(let ((mouse-event-handler
       (lambda (window event)
	 window
	 (let ((xw (vector-ref event 1)))
	   (vector-set! event 2
			(x-graphics-map-x-coordinate xw
						     (vector-ref event 2)))
	   (vector-set! event 3
			(x-graphics-map-y-coordinate xw
						     (vector-ref event 3)))))))
  ;; ENTER and LEAVE events should be modified to have X,Y coordinates.
  (define-event-handler event-type:button-down mouse-event-handler)
  (define-event-handler event-type:button-up mouse-event-handler)
  (define-event-handler event-type:motion mouse-event-handler))

;;;; Standard Operations

(define x-graphics:auto-raise? #f)

(define-structure (x-window (conc-name x-window/)
			    (constructor make-x-window (xw display)))
  xw
  (display #f read-only #t)
  (mapped? 'NEVER)
  (visibility #f)
  (user-event-mask user-event-mask:default))

(define-integrable (x-graphics-device/xw device)
  (x-window/xw (graphics-device/descriptor device)))

(define (x-graphics/display device)
  (x-window/display (graphics-device/descriptor device)))

(define-integrable (x-graphics-device/xd device)
  (x-display/xd (x-window/display (graphics-device/descriptor device))))

(define-integrable (x-graphics-device/mapped? device)
  (eq? #t (x-window/mapped? (graphics-device/descriptor device))))

(define-integrable (x-graphics-device/visibility device)
  (x-window/visibility (graphics-device/descriptor device)))

(define (x-graphics/open-window? device)
  (if (x-graphics-device/xw device) #t #f))

(define (x-graphics/close-window device)
  (without-interrupts
   (lambda ()
     (close-x-window (graphics-device/descriptor device)))))

(define (close-x-window window)
  (remove-from-gc-finalizer!
   (x-display/window-finalizer (x-window/display window))
   window))

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

(define x-graphics-default-geometry "512x512")
(define x-graphics-default-display-name #f)

(define (x-graphics/open descriptor->device
			 #!optional display geometry suppress-map?)
  (let ((display
	 (let ((display
		(and (not (default-object? display))
		     display)))
	   (if (x-display? display)
	       display
	       (x-graphics/open-display display)))))
    (call-with-values
	(lambda ()
	  (decode-suppress-map-arg (and (not (default-object? suppress-map?))
					suppress-map?)
				   'MAKE-GRAPHICS-DEVICE))
      (lambda (map? resource class)
	(let ((xw
	       (x-graphics-open-window
		 (x-display/xd display)
		 (if (default-object? geometry) 
		     x-graphics-default-geometry
		     geometry)
		 (vector #f resource class))))
	  (x-window-set-event-mask xw event-mask:normal)
	  (let ((window (make-x-window xw display)))
	    (add-to-gc-finalizer! (x-display/window-finalizer display) window)
	    (if map? (map-window window))
	    (descriptor->device window)))))))

(define (map-window window)
  (let ((xw (x-window/xw window)))
    (x-window-map xw)
    ;; If this is the first time that this window has been mapped, we
    ;; need to wait for a MAP event before continuing.
    (if (not (boolean? (x-window/mapped? window)))
	(begin
	  (x-window-flush xw)
	  (letrec ((loop
		    (let ((display (x-window/display window)))
		      (lambda ()
			(if (not (eq? #t (x-window/mapped? window)))
			    (begin
			      (%read-and-process-event display)
			      (loop)))))))
	    (with-thread-events-blocked loop))))))

(define (decode-suppress-map-arg suppress-map? procedure)
  (cond ((boolean? suppress-map?)
	 (values (not suppress-map?) "schemeGraphics" "SchemeGraphics"))
	((and (pair? suppress-map?)
	      (string? (car suppress-map?))
	      (string? (cdr suppress-map?)))
	 (values #f (car suppress-map?) (cdr suppress-map?)))
	((and (vector? suppress-map?)
	      (fix:= (vector-length suppress-map?) 3)
	      (boolean? (vector-ref suppress-map? 0))
	      (string? (vector-ref suppress-map? 1))
	      (string? (vector-ref suppress-map? 2)))
	 (values (vector-ref suppress-map? 0)
		 (vector-ref suppress-map? 1)
		 (vector-ref suppress-map? 2)))
	(else
	 (error:wrong-type-argument suppress-map?
				    "X suppress-map arg"
				    procedure))))

(define (x-graphics/clear device)
  (x-window-clear (x-graphics-device/xw device)))

(define (x-graphics/coordinate-limits device)
  (let ((limits (x-graphics-vdc-extent (x-graphics-device/xw device))))
    (values (vector-ref limits 0) (vector-ref limits 1)
	    (vector-ref limits 2) (vector-ref limits 3))))

(define (x-graphics/device-coordinate-limits device)
  (let ((xw (x-graphics-device/xw device)))
    (values 0 (- (x-window-y-size xw) 1) (- (x-window-x-size xw) 1) 0)))

(define (x-graphics/drag-cursor device x y)
  (x-graphics-drag-cursor (x-graphics-device/xw device)
			  (->flonum x)
			  (->flonum y)))

(define (x-graphics/draw-line device x-start y-start x-end y-end)
  (x-graphics-draw-line (x-graphics-device/xw device)
			(->flonum x-start)
			(->flonum y-start)
			(->flonum x-end)
			(->flonum y-end)))

(define (x-graphics/draw-lines device xv yv)
  (x-graphics-draw-lines (x-graphics-device/xw device) xv yv))

(define (x-graphics/draw-point device x y)
  (x-graphics-draw-point (x-graphics-device/xw device)
			 (->flonum x)
			 (->flonum y)))

(define (x-graphics/draw-points device xv yv)
  (x-graphics-draw-points (x-graphics-device/xw device) xv yv))

(define (x-graphics/draw-text device x y string)
  (x-graphics-draw-string (x-graphics-device/xw device)
			  (->flonum x)
			  (->flonum y)
			  string))

(define (x-graphics/draw-text-opaque device x y string)
  (x-graphics-draw-image-string (x-graphics-device/xw device)
				(->flonum x)
				(->flonum y)
				string))

(define (x-graphics/flush device)
  (if (and x-graphics:auto-raise?
	   (x-graphics-device/mapped? device)
	   (not (eq? 'UNOBSCURED (x-graphics-device/visibility device))))
      (x-graphics/raise-window device))
  ((ucode-primitive x-display-flush 1) (x-graphics-device/xd device)))

(define (x-graphics/move-cursor device x y)
  (x-graphics-move-cursor (x-graphics-device/xw device)
			  (->flonum x)
			  (->flonum y)))

(define (x-graphics/reset-clip-rectangle device)
  (x-graphics-reset-clip-rectangle (x-graphics-device/xw device)))

(define (x-graphics/set-clip-rectangle device x-left y-bottom x-right y-top)
  (x-graphics-set-clip-rectangle (x-graphics-device/xw device)
				 (->flonum x-left)
				 (->flonum y-bottom)
				 (->flonum x-right)
				 (->flonum y-top)))

(define (x-graphics/set-coordinate-limits device x-left y-bottom x-right y-top)
  (x-graphics-set-vdc-extent (x-graphics-device/xw device)
			     (->flonum x-left)
			     (->flonum y-bottom)
			     (->flonum x-right)
			     (->flonum y-top)))

(define (x-graphics/set-drawing-mode device mode)
  (x-graphics-set-function (x-graphics-device/xw device) mode))

(define (x-graphics/set-line-style device line-style)
  (if (not (and (exact-nonnegative-integer? line-style) (< line-style 8)))
      (error:wrong-type-argument line-style "graphics line style"
				 'SET-LINE-STYLE))
  (let ((xw (x-graphics-device/xw device)))
    (if (zero? line-style)
	(x-graphics-set-line-style xw 0)
	(begin
	  (x-graphics-set-line-style xw 2)
	  (x-graphics-set-dashes xw
				 0
				 (vector-ref '#("\010\010"
						"\001\001"
						"\015\001\001\001"
						"\013\001\001\001\001\001"
						"\013\005"
						"\014\001\002\001"
						"\011\001\002\001\002\001")
					     (- line-style 1)))))))

;;;; Appearance Operations

(define (x-graphics/set-background-color device color)
  (x-window-set-background-color (x-graphics-device/xw device) color))

(define (x-graphics/set-border-color device color)
  (x-window-set-border-color (x-graphics-device/xw device) color))

(define (x-graphics/set-border-width device width)
  (x-window-set-border-width (x-graphics-device/xw device) width))

(define (x-graphics/set-font device font)
  (x-window-set-font (x-graphics-device/xw device) font))

(define (x-graphics/set-foreground-color device color)
  (x-window-set-foreground-color (x-graphics-device/xw device) color))

(define (x-graphics/set-internal-border-width device width)
  (x-window-set-internal-border-width (x-graphics-device/xw device) width))

(define (x-graphics/set-mouse-color device color)
  (x-window-set-mouse-color (x-graphics-device/xw device) color))

(define (x-graphics/set-mouse-shape device shape)
  (x-window-set-mouse-shape (x-graphics-device/xw device) shape))

;;;; Miscellaneous Operations

(define (x-graphics/draw-arc device x y radius-x radius-y
			     angle-start angle-sweep fill?)
  (x-graphics-draw-arc (x-graphics-device/xw device)
		       (->flonum x)
		       (->flonum y)
		       (->flonum radius-x)
		       (->flonum radius-y)
		       (->flonum angle-start)
		       (->flonum angle-sweep)
		       fill?))
   
(define (x-graphics/draw-circle device x y radius)
  (x-graphics-draw-arc (x-graphics-device/xw device)
		       (->flonum x)
		       (->flonum y)
		       (->flonum radius)
		       (->flonum radius)
		       0.
		       360.
		       #f))
   
(define (x-graphics/fill-circle device x y radius)
  (x-graphics-draw-arc (x-graphics-device/xw device)
		       (->flonum x)
		       (->flonum y)
		       (->flonum radius)
		       (->flonum radius)
		       0.
		       360.
		       #t))
   
(define (x-graphics/fill-polygon device point-vector)
  (x-graphics-fill-polygon (x-graphics-device/xw device)
			   (vector-map ->flonum point-vector)))
   
(define (x-graphics/copy-area device source-x-left source-y-top width height
			      destination-x-left destination-y-top)
  (let ((xw (x-graphics-device/xw device)))
    (x-graphics-copy-area xw xw
			  (->flonum source-x-left)
			  (->flonum source-y-top)
			  (->flonum width)
			  (->flonum height)
			  (->flonum destination-x-left)
			  (->flonum destination-y-top))))

(define (x-graphics/get-default device resource-name class-name)
  (x-display-get-default (x-graphics-device/xd device)
			 resource-name class-name))

(define (x-graphics/starbase-filename device)
  (x-window-starbase-filename (x-graphics-device/xw device)))

(define (x-graphics/window-id device)
  (x-window-id (x-graphics-device/xw device)))

;;;; Event-Handling Operations

(define (x-graphics/set-input-hint device input?)
  (x-window-set-input-hint (x-graphics-device/xw device) input?))

(define (x-graphics/disable-keyboard-focus device)
  ;; Tell the window to participate in the TAKE-FOCUS protocol.  Since
  ;; there is no handler for this event, focus will never be given to
  ;; the window.
  (x-window-set-event-mask (x-graphics-device/xw device)
			   event-mask:ignore-focus))

(define (x-graphics/enable-keyboard-focus device)
  (x-window-set-event-mask (x-graphics-device/xw device) event-mask:normal))

(define (x-graphics/select-user-events device mask)
  (set-x-window/user-event-mask! (graphics-device/descriptor device) mask))

(define (x-graphics/query-pointer device)
  (let* ((window (x-graphics-device/xw device))
	 (result (x-window-query-pointer window)))
    (values (x-graphics-map-x-coordinate window (vector-ref result 2))
	    (x-graphics-map-y-coordinate window (vector-ref result 3))
	    (vector-ref result 4))))

(define (x-graphics/read-button device)
  (let ((event (read-event-of-type device event-type:button-down)))
    (values (vector-ref event 2)
	    (vector-ref event 3)
	    (vector-ref event 4))))

(define (read-event-of-type device event-type)
  (let ((window (graphics-device/descriptor device))
	(display (x-graphics/display device)))
  (let loop ()
    (let ((event (read-event display)))
      (if (eq? window (vector-ref event 1))
	  (begin
	    (if (fix:= (vector-ref event 0) event-type:delete-window)
		(error "Window closed while waiting to read event."))
	    (if (fix:= (vector-ref event 0) event-type)
		event
		(loop)))
	  (loop))))))

(define (x-graphics/read-user-event device)
  (read-event (x-graphics/display device)))

(define (x-graphics/discard-events device)
  (discard-events (x-graphics/display device)))

;;;; Font Operations

(define (x-graphics/font-structure device string)
  (x-font-structure (x-graphics-device/xd device) string))

(define-structure (x-font-structure (conc-name x-font-structure/)
				    (type vector))
  (name #f read-only #t)
  (direction #f read-only #t)
  (all-chars-exist? #f read-only #t)
  (default-char #f read-only #t)
  (min-bounds #f read-only #t)
  (max-bounds #f read-only #t)
  (start-index #f read-only #t)
  (character-bounds #f read-only #t)
  (max-ascent #f read-only #t)
  (max-descent #f read-only #t))

(define-structure (x-character-bounds (conc-name x-character-bounds/)
				      (type vector))
  (lbearing #f read-only #t)
  (rbearing #f read-only #t)
  (width #f read-only #t)
  (ascent #f read-only #t)
  (descent #f read-only #t))

;;;; Window Management Operations

(define (x-graphics/map-window device)
  (map-window (graphics-device/descriptor device)))

(define (x-graphics/withdraw-window device)
  (x-window-withdraw (x-graphics-device/xw device)))

(define (x-graphics/iconify-window device)
  (x-window-iconify (x-graphics-device/xw device)))

(define (x-graphics/raise-window device)
  (x-window-raise (x-graphics-device/xw device)))

(define (x-graphics/lower-window device)
  (x-window-lower (x-graphics-device/xw device)))

(define (x-graphics/set-icon-name device name)
  (x-window-set-icon-name (x-graphics-device/xw device) name))

(define (x-graphics/set-window-name device name)
  (x-window-set-name (x-graphics-device/xw device) name))

(define (x-graphics/move-window device x y)
  (x-window-set-position (x-graphics-device/xw device) x y))

(define (x-graphics/resize-window device width height)
  (x-window-set-size (x-graphics-device/xw device) width height))

;;;; Images

;; X-IMAGE is the descriptor of the generic images.

(define-structure (x-image (conc-name x-image/))
  descriptor
  window
  width
  height)

(define image-list)

(define (initialize-image-datatype)
  (1d-table/put!
   (graphics-type-properties x-graphics-device-type)
   'IMAGE-TYPE
   (make-image-type
    `((create ,create-x-image)
      (destroy ,x-graphics-image/destroy)
      (width ,x-graphics-image/width)
      (height ,x-graphics-image/height)
      (draw ,x-graphics-image/draw)
      (draw-subimage ,x-graphics-image/draw-subimage)
      (fill-from-byte-vector ,x-graphics-image/fill-from-byte-vector))))
  (set! image-list
	(make-gc-finalizer x-destroy-image
			   x-image?
			   x-image/descriptor
			   set-x-image/descriptor!))
  unspecific)

(define (create-x-image device width height)
  (let ((window (x-graphics-device/xw device)))
    (add-to-gc-finalizer! image-list
			  (make-x-image (x-create-image window width height)
					window width height))))

(define (x-image/destroy image)
  (remove-from-gc-finalizer! image-list image))

(define (x-image/get-pixel image x y)
  (x-get-pixel-from-image (x-image/descriptor image) x y))

(define (x-image/set-pixel image x y value)
  (x-set-pixel-in-image (x-image/descriptor image) x y value))

(define (x-image/draw image window-x window-y)
  (x-display-image (x-image/descriptor image)
		   0
		   0
		   (x-image/window image)
		   (->flonum window-x)
		   (->flonum window-y)
		   (x-image/width image)
		   (x-image/height image)))

(define (x-image/draw-subimage image x y width height window-x window-y)
  (x-display-image (x-image/descriptor image)
		   x
		   y
		   (x-image/window image)
		   (->flonum window-x)
		   (->flonum window-y)
		   width
		   height))

(define (x-image/fill-from-byte-vector image byte-vector)
  (x-bytes-into-image byte-vector (x-image/descriptor image)))

;; Abstraction layer for generic images

(define (x-graphics/create-image device width height)
  (image/create device width height))

;;(define x-graphics-image/create create-x-image)

(define (x-graphics-image/destroy image)
  (x-image/destroy (image/descriptor image)))

(define (x-graphics-image/width image)
  (x-image/width (image/descriptor image)))

(define (x-graphics-image/height image)
  (x-image/height (image/descriptor image)))

(define (x-graphics-image/draw device x y image)
  (let* ((x-image (image/descriptor image))
	 (w (x-image/width x-image))
	 (h (x-image/height x-image)))
    (x-display-image (x-image/descriptor x-image)
		     0
		     0
		     (x-graphics-device/xw device)
		     (->flonum x)
		     (->flonum y)
		     w
		     h)))

(define (x-graphics-image/draw-subimage device x y image im-x im-y w h)
  (let ((x-image  (image/descriptor image)))
    (x-display-image (x-image/descriptor x-image)
		     im-x
		     im-y
		     (x-graphics-device/xw device)
		     (->flonum x)
		     (->flonum y)
		     w
		     h)))

(define (x-graphics-image/fill-from-byte-vector image byte-vector)
  (x-image/fill-from-byte-vector (image/descriptor image) byte-vector))

;;;; Colormaps

(define-record-type <colormap>
    (%make-colormap descriptor)
    x-colormap?
  (descriptor colormap/descriptor set-colormap/descriptor!))

(define colormap-list)

(define (initialize-colormap-datatype)
  (set! colormap-list
	(make-gc-finalizer x-free-colormap
			   x-colormap?
			   colormap/descriptor
			   set-colormap/descriptor!))
  unspecific)

(define (make-colormap descriptor)
  (add-to-gc-finalizer! colormap-list (%make-colormap descriptor)))

(define (x-graphics/get-colormap device)
  (make-colormap (x-window-colormap (x-graphics-device/xw device))))

(define (x-graphics/set-colormap device colormap)
  (x-set-window-colormap (x-graphics-device/xw device)
			 (colormap/descriptor colormap)))

(define (create-x-colormap device writeable?)
  (let ((window (x-graphics-device/xw device)))
    (let ((visual (x-window-visual window)))
      (let ((descriptor (x-create-colormap window visual writeable?)))
	(x-visual-deallocate visual)
	(make-colormap descriptor)))))

(define (x-colormap/free colormap)
  (remove-from-gc-finalizer! colormap-list colormap))

(define (x-colormap/allocate-color colormap r g b)
  (x-allocate-color (colormap/descriptor colormap) r g b))

(define (x-colormap/query-color colormap position)
  (x-query-color (colormap/descriptor colormap) position))

(define (x-colormap/store-color colormap position r g b)
  (x-store-color (colormap/descriptor colormap) position r g b))

(define (x-colormap/store-colors colormap color-vector)
  (x-store-colors (colormap/descriptor colormap) color-vector))

(define (x-graphics/color? device)
  (let ((info (x-graphics/visual-info device)))
    (let ((n (vector-length info)))
      (let loop ((index 0))
	(and (not (fix:= index n))
	     (or (let ((class (x-visual-info/class (vector-ref info index))))
		   (or (eq? x-visual-class:static-color class)
		       (eq? x-visual-class:pseudo-color class)
		       (eq? x-visual-class:true-color class)
		       (eq? x-visual-class:direct-color class)))
		 (loop (fix:+ index 1))))))))

(define (x-graphics/image-depth device)
  (x-window-depth (x-graphics-device/xw device)))

(define (x-graphics/visual-info device)
  ((ucode-primitive x-get-visual-info 10) (x-graphics-device/xw device)
					  #f #f #f #f #f #f #f #f #f))

(define-structure (visual-info (type vector) (conc-name x-visual-info/))
  (visual #f read-only #t)
  (visual-id #f read-only #t)
  (screen #f read-only #t)
  (depth #f read-only #t)
  (class #f read-only #t)
  (red-mask #f read-only #t)
  (green-mask #f read-only #t)
  (blue-mask #f read-only #t)
  (colormap-size #f read-only #t)
  (bits-per-rgb #f read-only #t))

(define-integrable x-visual-class:static-gray 0)
(define-integrable x-visual-class:gray-scale 1)
(define-integrable x-visual-class:static-color 2)
(define-integrable x-visual-class:pseudo-color 3)
(define-integrable x-visual-class:true-color 4)
(define-integrable x-visual-class:direct-color 5)