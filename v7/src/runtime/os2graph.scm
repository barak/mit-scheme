#| -*-Scheme-*-

$Id: os2graph.scm,v 1.27 2007/01/05 21:19:28 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

;;;; OS/2 PM Graphics Interface
;;; package: (runtime os2-graphics)

(declare (usual-integrations))
(declare (integrate-external "graphics"))
(declare (integrate-external "os2winp"))

(define (initialize-package!)
  (set! os2-graphics-device-type
	(make-graphics-device-type
	 'OS/2
	 `((activate-window ,os2-graphics/activate-window)
	   (available? ,os2-graphics/available?)
	   (capture-image ,os2-graphics/capture-image)
	   (clear ,os2-graphics/clear)
	   (close ,os2-graphics/close)
	   (color? ,os2-graphics/color?)
	   (coordinate-limits ,os2-graphics/coordinate-limits)
	   (deactivate-window ,os2-graphics/deactivate-window)
	   (define-color ,os2-graphics/define-color)
	   (desktop-size ,os2-graphics/desktop-size)
	   (device-coordinate-limits ,os2-graphics/device-coordinate-limits)
	   (discard-events ,os2-graphics/discard-events)
	   (drag-cursor ,os2-graphics/drag-cursor)
	   (draw-line ,os2-graphics/draw-line)
	   (draw-lines ,os2-graphics/draw-lines)
	   (draw-point ,os2-graphics/draw-point)
	   (draw-text ,os2-graphics/draw-text)
	   (find-color ,os2-graphics/find-color)
	   (flush ,os2-graphics/flush)
	   (hide-window ,os2-graphics/hide-window)
	   (image-depth ,os2-graphics/image-depth)
	   (lower-window ,os2-graphics/lower-window)
	   (maximize-window ,os2-graphics/maximize-window)
	   (minimize-window ,os2-graphics/minimize-window)
	   (move-cursor ,os2-graphics/move-cursor)
	   (open ,os2-graphics/open)
	   (open? ,os2-graphics/open-window?)
	   (raise-window ,os2-graphics/raise-window)
	   (read-button ,os2-graphics/read-button)
	   (read-user-event ,os2-graphics/read-user-event)
	   (reset-clip-rectangle ,os2-graphics/reset-clip-rectangle)
	   (restore-window ,os2-graphics/restore-window)
	   (select-user-events ,os2-graphics/select-user-events)
	   (set-background-color ,os2-graphics/set-background-color)
	   (set-clip-rectangle ,os2-graphics/set-clip-rectangle)
	   (set-coordinate-limits ,os2-graphics/set-coordinate-limits)
	   (set-drawing-mode ,os2-graphics/set-drawing-mode)
	   (set-font ,os2-graphics/set-font)
	   (set-foreground-color ,os2-graphics/set-foreground-color)
	   (set-line-style ,os2-graphics/set-line-style)
	   (set-window-name ,os2-graphics/set-window-title)
	   (set-window-position ,os2-graphics/set-window-position)
	   (set-window-size ,os2-graphics/set-window-size)
	   (set-window-title ,os2-graphics/set-window-title)
	   (window-position ,os2-graphics/window-position)
	   (window-frame-size ,os2-graphics/window-frame-size)
	   (window-size ,os2-graphics/window-size))))
  (1d-table/put!
   (graphics-type-properties os2-graphics-device-type)
   'IMAGE-TYPE
   (make-image-type
    `((create ,os2-image/create)
      (destroy ,os2-image/destroy)
      (width ,os2-image/width)
      (height ,os2-image/height)
      (draw ,os2-image/draw)
      (draw-subimage ,os2-image/draw-subimage)
      (fill-from-byte-vector ,os2-image/fill-from-byte-vector))))
  (set! event-descriptor #f)
  (set! event-previewer-registration #f)
  (set! window-finalizer
	(make-gc-finalizer os2win-close window? window/wid set-window/wid!))
  (set! image-finalizer
	(make-gc-finalizer destroy-memory-ps image? image/ps set-image/ps!))
  (set! user-event-mask user-event-mask:default)
  (set! user-event-queue (make-queue))
  (initialize-color-table)
  (add-event-receiver! event:before-exit finalize-pm-state!))

(define os2-graphics-device-type)
(define event-descriptor)
(define event-previewer-registration)
(define window-finalizer)
(define image-finalizer)
(define user-event-mask)
(define user-event-queue)
(define graphics-window-icon)

;; This event mask contains just button events.
(define user-event-mask:default #x0001)

(define (finalize-pm-state!)
  (if event-descriptor
      (begin
	(os2win-destroy-pointer graphics-window-icon)
	(set! graphics-window-icon)
	(remove-all-from-gc-finalizer! window-finalizer)
	(remove-all-from-gc-finalizer! image-finalizer)
	(deregister-io-thread-event event-previewer-registration)
	(set! event-previewer-registration #f)
	(set! user-event-mask user-event-mask:default)
	(flush-queue! user-event-queue)
	(os2win-close-event-qid event-descriptor)
	(set! event-descriptor #f)
	unspecific)))

;;;; Window Abstraction

(define-structure (window
		   (conc-name window/)
		   (constructor %make-window
				(wid pel-width pel-height x-slope y-slope)))
  wid
  pel-width
  pel-height
  backing-image
  (changes #f)
  (x-gcursor 0)
  (y-gcursor 0)
  (x-left -1)
  (y-bottom -1)
  (x-right 1)
  (y-top 1)
  x-slope
  y-slope
  font-specifier
  font-metrics
  (foreground-color #xFFFFFF)
  (background-color #x000000)
  device)

(define (make-window wid width height)
  (let ((window
	 (%make-window wid width height
		       (exact->inexact (/ (- width 1) 2))
		       (exact->inexact (/ (- height 1) 2)))))
    (set-window/backing-image! window (create-image width height))
    (add-to-gc-finalizer! window-finalizer window)))

(define (close-window window)
  (if (window/wid window)
      (begin
	(destroy-image (window/backing-image window))
	(remove-from-gc-finalizer! window-finalizer window))))

(define-integrable (os2-graphics-device/wid device)
  (window/wid (graphics-device/descriptor device)))

(define-integrable (os2-graphics-device/psid device)
  (window/backing-store (graphics-device/descriptor device)))

(define-integrable (window/backing-store window)
  (image/ps (window/backing-image window)))

(define (compute-window-slopes! window)
  (set-window/x-slope!
   window
   (exact->inexact
    (/ (- (window/pel-width window) 1)
       (- (window/x-right window) (window/x-left window)))))
  (set-window/y-slope!
   window
   (exact->inexact
    (/ (- (window/pel-height window) 1)
       (- (window/y-top window) (window/y-bottom window))))))

(define (window/x->device window x)
  (round->exact (* (window/x-slope window) (- x (window/x-left window)))))

(define (window/y->device window y)
  (round->exact (* (window/y-slope window) (- y (window/y-bottom window)))))

(define (window/device->x window x)
  (+ (/ x (window/x-slope window)) (window/x-left window)))

(define (window/device->y window y)
  (+ (/ y (window/y-slope window)) (window/y-bottom window)))

;;;; Standard Operations

(define (os2-graphics/available?)
  (implemented-primitive-procedure? os2win-open))

(define (os2-graphics/open descriptor->device #!optional width height)
  (if (not event-descriptor)
      (begin
	(set! event-descriptor (os2win-open-event-qid))
	(set! event-previewer-registration
	      (permanently-register-io-thread-event
	       event-descriptor
	       'READ
	       (current-thread)
	       (lambda (mode)
		 mode
		 (read-and-process-event))))
	(set! graphics-window-icon
	      (os2win-load-pointer HWND_DESKTOP NULLHANDLE IDI_GRAPHICS))))
  (open-window descriptor->device
	       (if (default-object? width) 256 width)
	       (if (default-object? height) 256 height)))

(define (open-window descriptor->device width height)
  (let ((wid (os2win-open event-descriptor "Scheme Graphics")))
    (os2win-set-icon wid graphics-window-icon)
    (os2win-show-cursor wid #f)
    (os2win-show wid #t)
    (os2win-set-size wid width height)
    (pm-synchronize)
    (os2win-set-state wid window-state:deactivate)
    (os2win-set-state wid window-state:top)
    (let ((window (make-window wid width height)))
      (update-colors window)
      (set-window-font! window "4.System VIO")
      (let ((device (descriptor->device window)))
	(os2-graphics/clear device)
	(set-window/device! window device)
	device))))

(define (os2-graphics/open-window? device)
  (if (os2-graphics-device/wid device) #t #f))

(define (os2-graphics/close device)
  (let ((window (graphics-device/descriptor device)))
    (without-interrupts
     (lambda ()
       (close-window window)))))

(define (os2-graphics/clear device)
  (let ((window (graphics-device/descriptor device)))
    (without-interrupts
     (lambda ()
       (let ((width (window/pel-width window))
	     (height (window/pel-height window)))
	 (os2ps-clear (window/backing-store window) 0 width 0 height)
	 (invalidate-rectangle device 0 width 0 height))))))

(define (os2-graphics/coordinate-limits device)
  (let ((window (graphics-device/descriptor device)))
    (without-interrupts
     (lambda ()
       (values (window/x-left window)
	       (window/y-bottom window)
	       (window/x-right window)
	       (window/y-top window))))))

(define (os2-graphics/device-coordinate-limits device)
  (let ((window (graphics-device/descriptor device)))
    (without-interrupts
     (lambda ()
       (values 0
	       0
	       (- (window/pel-width window) 1)
	       (- (window/pel-height window) 1))))))

(define (os2-graphics/drag-cursor device x y)
  (let ((window (graphics-device/descriptor device)))
    (without-interrupts
     (lambda ()
       (let ((xs (window/x-gcursor window))
	     (ys (window/y-gcursor window))
	     (xe (window/x->device window x))
	     (ye (window/y->device window y)))
	 (let ((xl (if (fix:< xs xe) xs xe))
	       (yl (if (fix:< ys ye) ys ye))
	       (xh (fix:+ (if (fix:> xs xe) xs xe) 1))
	       (yh (fix:+ (if (fix:> ys ye) ys ye) 1)))
	   (os2ps-line (window/backing-store window) xe ye)
	   (set-window/x-gcursor! window xe)
	   (set-window/y-gcursor! window ye)
	   (invalidate-rectangle device xl xh yl yh)))))))

(define (os2-graphics/draw-line device x-start y-start x-end y-end)
  (os2-graphics/move-cursor device x-start y-start)
  (os2-graphics/drag-cursor device x-end y-end))

(define (os2-graphics/draw-lines device xv yv)
  (let ((window (graphics-device/descriptor device)))
    (without-interrupts
     (lambda ()
       (let ((xv (vector-map (lambda (x) (window/x->device window x)) xv))
	     (yv (vector-map (lambda (y) (window/y->device window y)) yv)))
	 (let ((xl (fix:vector-min xv))
	       (yl (fix:vector-min yv))
	       (xh (fix:+ (fix:vector-max xv) 1))
	       (yh (fix:+ (fix:vector-max yv) 1)))
	   (os2ps-poly-line-disjoint (window/backing-store window) xv yv)
	   (invalidate-rectangle device xl xh yl yh)))))))

(define (os2-graphics/draw-point device x y)
  ;; This sucks.  Implement a real point-drawing primitive.
  (let ((window (graphics-device/descriptor device)))
    (without-interrupts
     (lambda ()
       (let ((x (window/x->device window x))
	     (y (window/y->device window y)))
	 (os2ps-draw-point (window/backing-store window) x y)
	 (invalidate-rectangle device x (fix:+ x 1) y (fix:+ y 1)))))))

(define (os2-graphics/draw-text device x y string)
  (let ((window (graphics-device/descriptor device))
	(length (string-length string)))
    (without-interrupts
     (lambda ()
       (let ((psid (window/backing-store window))
	     (metrics (window/font-metrics window))
	     (x (window/x->device window x))
	     (y (window/y->device window y)))
	 (os2ps-write psid
		      x
		      (fix:+ y (font-metrics/descender metrics))
		      string
		      0
		      length)
	 (invalidate-rectangle device
			       x
			       (fix:+ x
				      (os2ps-text-width psid string 0 length))
			       y
			       (fix:+ y (font-metrics/height metrics))))))))

(define (os2-graphics/flush device)
  (let ((window (graphics-device/descriptor device)))
    (without-interrupts
     (lambda ()
       (let ((changes (window/changes window)))
	 (if changes
	     (begin
	       (os2win-invalidate (window/wid window)
				  (changes/x-left changes)
				  (changes/x-right changes)
				  (changes/y-bottom changes)
				  (changes/y-top changes))
	       (set-window/changes! window #f))))))))

(define (invalidate-rectangle device x-left x-right y-bottom y-top)
  (let ((window (graphics-device/descriptor device)))
    (if (graphics-device/buffer? device)
	(let ((changes (window/changes window)))
	  (if (not changes)
	      (set-window/changes! window
				   (make-changes x-left
						 x-right
						 y-bottom
						 y-top))
	      (begin
		(if (fix:< x-left (changes/x-left changes))
		    (set-changes/x-left! changes x-left))
		(if (fix:> x-right (changes/x-right changes))
		    (set-changes/x-right! changes x-right))
		(if (fix:< y-bottom (changes/y-bottom changes))
		    (set-changes/y-bottom! changes y-bottom))
		(if (fix:> y-top (changes/y-top changes))
		    (set-changes/y-top! changes y-top)))))
	(os2win-invalidate (window/wid window)
			   x-left x-right y-bottom y-top))))

(define-structure (changes (type vector)
			   (conc-name changes/)
			   (constructor make-changes))
  x-left
  x-right
  y-bottom
  y-top)

(define (os2-graphics/move-cursor device x y)
  (let ((window (graphics-device/descriptor device)))
    (without-interrupts
     (lambda ()
       (let ((x (window/x->device window x))
	     (y (window/y->device window y)))
	 (os2ps-move-graphics-cursor (window/backing-store window) x y)
	 (set-window/x-gcursor! window x)
	 (set-window/y-gcursor! window y))))))

(define (os2-graphics/reset-clip-rectangle device)
  (os2ps-reset-clip-rectangle (os2-graphics-device/psid device)))

(define (os2-graphics/set-clip-rectangle device x-left y-bottom x-right y-top)
  (let ((window (graphics-device/descriptor device)))
    (without-interrupts
     (lambda ()
       (os2ps-set-clip-rectangle (window/backing-store window)
				 (window/x->device window x-left)
				 (window/x->device window x-right)
				 (window/y->device window y-bottom)
				 (window/y->device window y-top))))))

(define (os2-graphics/set-coordinate-limits device
					    x-left y-bottom x-right y-top)
  (let ((window (graphics-device/descriptor device)))
    (without-interrupts
     (lambda ()
       (set-window/x-left! window x-left)
       (set-window/y-bottom! window y-bottom)
       (set-window/x-right! window x-right)
       (set-window/y-top! window y-top)
       (compute-window-slopes! window)))))

(define (os2-graphics/set-drawing-mode device mode)
  (os2ps-set-mix (os2-graphics-device/psid device)
		 (map-drawing-mode mode)))

(define (os2-graphics/set-line-style device style)
  (os2ps-set-line-type (os2-graphics-device/psid device)
		       (map-line-style style)))

;;;; Color Operations

(define (os2-graphics/color? device)
  (not (= 0 (os2ps-query-capability (os2-graphics-device/psid device)
				    CAPS_COLOR_TABLE_SUPPORT))))

(define (os2-graphics/define-color device name color)
  device
  (os2/define-color name color))

(define (os2-graphics/find-color device specification)
  device
  (os2/find-color specification))

(define (os2-graphics/set-background-color device color)
  (let ((window (graphics-device/descriptor device))
	(color (->color color 'SET-BACKGROUND-COLOR)))
    (without-interrupts
      (lambda ()
	(set-window/background-color! window color)
	(update-colors window)))))

(define (os2-graphics/set-foreground-color device color)
  (let ((window (graphics-device/descriptor device))
	(color (->color color 'SET-FOREGROUND-COLOR)))
    (without-interrupts
      (lambda ()
	(set-window/foreground-color! window color)
	(update-colors window)))))

(define (update-colors window)
  (os2ps-set-colors (window/backing-store window)
		    (window/foreground-color window)
		    (window/background-color window)))

(define (os2-graphics/image-depth device)
  (let ((bitcount
	 (os2ps-query-capability (os2-graphics-device/psid device)
				 CAPS_COLOR_BITCOUNT)))
    (if (<= 1 bitcount 8)
	bitcount
	8)))

;;;; Window Operations

(define (os2-graphics/window-size device)
  (let ((w.h (os2win-get-size (os2-graphics-device/wid device))))
    (values (car w.h)
	    (cdr w.h))))

(define (os2-graphics/set-window-size device width height)
  (os2win-set-size (os2-graphics-device/wid device) width height))

(define (os2-graphics/window-frame-size device)
  (let ((w.h (os2win-get-frame-size (os2-graphics-device/wid device))))
    (values (car w.h)
	    (cdr w.h))))

(define (os2-graphics/window-position device)
  (let ((x.y (os2win-get-pos (os2-graphics-device/wid device))))
    (values (car x.y)
	    (cdr x.y))))

(define (os2-graphics/set-window-position device x y)
  (os2win-set-pos (os2-graphics-device/wid device) x y))

(define (os2-graphics/set-window-title device title)
  (os2win-set-title (os2-graphics-device/wid device) title))

(define (os2-graphics/set-font device font-specifier)
  (set-window-font! (graphics-device/descriptor device) font-specifier))

(define (os2-graphics/hide-window device)
  (os2win-set-state (os2-graphics-device/wid device) window-state:hide))

(define (os2-graphics/minimize-window device)
  (os2win-set-state (os2-graphics-device/wid device) window-state:minimize))

(define (os2-graphics/maximize-window device)
  (os2win-set-state (os2-graphics-device/wid device) window-state:maximize))

(define (os2-graphics/restore-window device)
  (os2win-set-state (os2-graphics-device/wid device) window-state:restore))

(define (os2-graphics/raise-window device)
  (os2win-set-state (os2-graphics-device/wid device) window-state:top))

(define (os2-graphics/lower-window device)
  (os2win-set-state (os2-graphics-device/wid device) window-state:bottom))

(define (os2-graphics/activate-window device)
  (os2win-set-state (os2-graphics-device/wid device) window-state:activate))

(define (os2-graphics/deactivate-window device)
  (os2win-set-state (os2-graphics-device/wid device) window-state:deactivate))

(define (os2-graphics/desktop-size device)
  device
  (values (os2win-desktop-width) (os2win-desktop-height)))

;;;; Color Support

(define (os2/define-color name color)
  (if (not (and (color-name? name)
		(not (char=? #\# (string-ref name 0)))))
      (error:wrong-type-argument name "color name" 'OS2/DEFINE-COLOR))
  (let ((entry (lookup-color-name name))
	(color (->color color 'OS2/DEFINE-COLOR)))
    (if entry
	(set-cdr! entry color)
	(begin
	  (set! color-table (cons (cons name color) color-table))
	  unspecific))))

(define (os2/find-color specification)
  (->color specification 'OS2/FIND-COLOR))

(define (->color specification procedure)
  (cond ((color? specification)
	 specification)
	((color-triple? specification)
	 (triple->color specification))
	((color-name? specification)
	 (name->color specification procedure))
	(else
	 (error:wrong-type-argument specification
				    "color specification"
				    procedure))))

(define (color? object)
  (and (exact-nonnegative-integer? object)
       (< object #x1000000)))

(define (color-triple? object)
  (and (list? object)
       (= 3 (length object))
       (for-all? object
	 (lambda (element)
	   (and (exact-nonnegative-integer? element)
		(< element #x100))))))

(define (triple->color triple)
  (+ (* #x10000 (car triple))
     (* #x100 (cadr triple))
     (caddr triple)))

(define (color-name? object)
  (and (string? object)
       (not (string-null? object))))

(define (name->color name procedure)
  (if (char=? #\# (string-ref name 0))
      (let ((color (substring->number name 1 (string-length name) 16)))
	(if (not (color? color))
	    (error:bad-range-argument name procedure))
	color)
      (let ((entry (lookup-color-name name)))
	(if (not entry)
	    (error:bad-range-argument name procedure))
	(cdr entry))))

(define (lookup-color-name name)
  (let loop ((entries color-table))
    (and (not (null? entries))
	 (if (string-ci=? (caar entries) name)
	     (car entries)
	     (loop (cdr entries))))))

(define (initialize-color-table)
  (set! color-table '())
  (for-each (lambda (entry)
	      (os2/define-color (car entry) (cdr entry)))
	    initial-color-definitions))

(define color-table)

(define initial-color-definitions
  `(("red"          255   0   0)
    ("green"          0 255   0)
    ("blue"           0   0 255)
    ("cyan"           0 255 255)
    ("magenta"      255   0 255)
    ("yellow"       255 255   0)
    ("black"          0   0   0)
    ("dark gray"     63  63  63)
    ("dark grey"     63  63  63)
    ("gray"         127 127 127)
    ("grey"         127 127 127)
    ("light gray"   191 191 191)
    ("light grey"   191 191 191)
    ("white"        255 255 255)
    ("purple"	    127   0 127)
    ("dark green"     0 127   0)
    ("orange"       255 135   0)
    ("pink"         255 181 197)
    ("brown"        127  63   0)))

;;;; Console Window

;;; This and the color support really should be in a separate file.

(define (os2-console/color?)
  (not (= 0 (os2ps-query-capability (os2win-ps (os2win-console-wid))
				    CAPS_COLOR_TABLE_SUPPORT))))

(define (os2-console/get-font-metrics)
  (let ((metrics (os2ps-get-font-metrics (os2win-ps (os2win-console-wid)))))
    (values (font-metrics/width metrics)
	    (font-metrics/height metrics))))

(define (os2-console/set-font! font-name)
  (if (not (os2ps-set-font (os2win-ps (os2win-console-wid)) 1 font-name))
      (error:bad-range-argument font-name 'OS2-CONSOLE/SET-FONT!)))

(define (os2-console/set-colors! foreground background)
  (let ((wid (os2win-console-wid)))
    (os2ps-set-colors (os2win-ps wid)
		      (os2/find-color foreground)
		      (os2/find-color background))
    (let ((w.h (os2win-get-size wid)))
      (os2win-invalidate wid 0 (car w.h) 0 (cdr w.h)))))

(define (os2-console/get-pel-size)
  (let ((w.h (os2win-get-size (os2win-console-wid))))
    (values (car w.h)
	    (cdr w.h))))

(define (os2-console/set-pel-size! width height)
  (os2win-set-size (os2win-console-wid) width height))

(define (os2-console/get-size)
  (let ((wid (os2win-console-wid)))
    (let ((w.h (os2win-get-size wid))
	  (metrics (os2ps-get-font-metrics (os2win-ps wid))))
      (values (quotient (car w.h) (font-metrics/width metrics))
	      (quotient (cdr w.h) (font-metrics/height metrics))))))

(define (os2-console/set-size! width height)
  (let ((metrics (os2ps-get-font-metrics (os2win-ps (os2win-console-wid)))))
    (os2-console/set-pel-size! (* width (font-metrics/width metrics))
			       (* height (font-metrics/height metrics)))))

(define (os2-console/get-frame-size)
  (let ((w.h (os2win-get-frame-size (os2win-console-wid))))
    (values (car w.h)
	    (cdr w.h))))

(define (os2-console/get-frame-position)
  (let ((x.y (os2win-get-pos (os2win-console-wid))))
    (values (car x.y)
	    (cdr x.y))))

(define (os2-console/set-frame-position! x y)
  (os2win-set-pos (os2win-console-wid) x y))

;;;; Miscellaneous Support

(define (set-window-font! window font-specifier)
  (set-window/font-specifier! window font-specifier)
  (set-window/font-metrics!
   window
   (let ((metrics
	  (os2ps-set-font (window/backing-store window) 1 font-specifier)))
     (if (not metrics)
	 (error "Unknown font name:" font-specifier))
     metrics)))

(define (fix:vector-min v)
  (let ((length (vector-length v))
	(min (vector-ref v 0)))
    (do ((index 1 (fix:+ index 1)))
	((fix:= index length))
      (if (fix:< (vector-ref v index) min)
	  (set! min (vector-ref v index))))
    min))

(define (fix:vector-max v)
  (let ((length (vector-length v))
	(max (vector-ref v 0)))
    (do ((index 1 (fix:+ index 1)))
	((fix:= index length))
      (if (fix:> (vector-ref v index) max)
	  (set! max (vector-ref v index))))
    max))

(define map-drawing-mode
  (let ((modes
	 (vector FM_ZERO
		 FM_AND
		 FM_MASKSRCNOT
		 FM_OVERPAINT
		 FM_SUBTRACT
		 FM_LEAVEALONE
		 FM_XOR
		 FM_OR
		 FM_NOTMERGESRC
		 FM_NOTXORSRC
		 FM_INVERT
		 FM_MERGESRCNOT
		 FM_NOTCOPYSRC
		 FM_MERGENOTSRC
		 FM_NOTMASKSRC
		 FM_ONE)))
    (lambda (mode)
      (if (not (and (fix:fixnum? mode) (fix:<= 0 mode) (fix:< mode 16)))
	  (error:wrong-type-argument mode "graphics line style"
				     'MAP-DRAWING-MODE))
      (vector-ref modes mode))))

(define map-line-style
  (let ((styles
	 (vector LINETYPE_SOLID
		 LINETYPE_SHORTDASH
		 LINETYPE_DOT
		 LINETYPE_DASHDOT
		 LINETYPE_DASHDOUBLEDOT
		 LINETYPE_LONGDASH
		 LINETYPE_DOUBLEDOT
		 LINETYPE_ALTERNATE)))
    (lambda (style)
      (if (not (and (fix:fixnum? style) (fix:<= 0 style) (fix:< style 8)))
	  (error:wrong-type-argument style "graphics line style"
				     'MAP-LINE-STYLE))
      (vector-ref styles style))))

;;;; Events

(define (pm-synchronize)
  (os2pm-synchronize)
  (with-thread-events-blocked
    (lambda () (do () ((not (read-and-process-event)))))))

(define (read-and-process-event)
  (let ((event (os2win-get-event event-descriptor #f)))
    (and event
	 (begin (process-event event) #t))))

(define (process-event event)
  (without-interrupts
   (lambda ()
     (let ((window
	    (search-gc-finalizer window-finalizer
	      (let ((wid (event-wid event)))
		(lambda (window)
		  (eq? (window/wid window) wid))))))
       (if window
	   (begin
	     (let ((handler (vector-ref event-handlers (event-type event))))
	       (if handler
		   (handler window event)))
	     (maybe-queue-user-event window event)))))))

(define event-handlers (make-vector number-of-event-types #f))

(define-integrable (define-event-handler event-type handler)
  (vector-set! event-handlers event-type handler))

(define-event-handler event-type:button
  (lambda (window event)
    (if (and (eq? button-event-type:down (button-event/type event))
	     (not (os2win-focus? (window/wid window))))
	(os2win-activate (window/wid window)))))

(define-event-handler event-type:close
  (lambda (window event)
    event
    (close-window window)))

(define-event-handler event-type:paint
  (lambda (window event)
    (os2ps-bitblt (os2win-ps (window/wid window))
		  (window/backing-store window)
		  (let ((xl (paint-event/xl event)))
		    (vector xl (paint-event/xh event) xl))
		  (let ((yl (paint-event/yl event)))
		    (vector yl (paint-event/yh event) yl))
		  ROP_SRCCOPY
		  BBO_OR)))

(define-event-handler event-type:resize
  (lambda (window event)
    (let ((width (resize-event/width event))
	  (height (resize-event/height event)))
      (let ((old (window/backing-store window)))
	(let ((bitmap (os2ps-create-bitmap old width height)))
	  (let ((new (os2ps-create-memory-ps)))
	    (os2ps-set-bitmap new bitmap)
	    ;; I'm worried that this will fail because the new memory PS
	    ;; doesn't have the correct attributes.  Maybe this will
	    ;; only cause trouble once we start hacking color maps.
	    (os2ps-bitblt new
			  old
			  (vector 0 width 0 (window/pel-width window))
			  (vector 0 height 0 (window/pel-height window))
			  ROP_SRCCOPY
			  BBO_IGNORE)
	    (os2ps-set-bitmap new #f)
	    (os2ps-destroy-memory-ps new))
	  (os2ps-destroy-bitmap (os2ps-set-bitmap old bitmap))))
      (set-window/pel-width! window width)
      (set-window/pel-height! window height)
      (compute-window-slopes! window)
      (os2win-invalidate (window/wid window) 0 width 0 height)
      (set-window/changes! window #f))))

;;;; User Events

(define (maybe-queue-user-event window event)
  (if (not (fix:= 0 (fix:and (fix:lsh 1 (event-type event)) user-event-mask)))
      (begin
	(set-event-wid! event (window/device window))
	(enqueue!/unsafe user-event-queue event))))

(define (os2-graphics/select-user-events device mask)
  device
  (if (not (and (exact-nonnegative-integer? mask)
		(< mask (expt 2 number-of-event-types))))
      (error:bad-range-argument mask 'SELECT-USER-EVENTS))
  (set! user-event-mask mask)
  unspecific)

(define (os2-graphics/read-user-event device)
  device
  (with-thread-events-blocked
   (lambda ()
     (let loop ()
       (if (queue-empty? user-event-queue)
	   (begin
	     (if (eq? 'READ
		      (test-for-io-on-descriptor event-descriptor #t 'READ))
		 (read-and-process-event))
	     (loop))
	   (dequeue! user-event-queue))))))

(define (os2-graphics/read-button device)
  (let ((window (graphics-device/descriptor device))
	(event
	 (let loop ()
	   (let ((event (os2-graphics/read-user-event device)))
	     (if (and (eq? event-type:button (event-type event))
		      (eq? button-event-type:down (button-event/type event)))
		 event
		 (loop))))))
    (values (button-event/number event)
	    (window/device->x window (button-event/x event))
	    (window/device->y window (button-event/y event))
	    (event-wid event))))

(define (os2-graphics/discard-events device)
  device
  (with-thread-events-blocked
   (lambda ()
     (let loop ()
       (flush-queue! user-event-queue)
       (if (read-and-process-event)
	   (loop))))))

(define (flush-queue! queue)
  (without-interrupts
   (lambda ()
     (let loop ()
       (if (not (queue-empty? queue))
	   (begin
	     (dequeue!/unsafe queue)
	     (loop)))))))

;;;; Images

(define-structure (image (conc-name image/))
  ps
  (width #f read-only #t)
  (height #f read-only #t)
  colormap)

(define (os2-graphics/capture-image device x-left y-bottom x-right y-top)
  (let ((window (graphics-device/descriptor device)))
    (let ((x (window/x->device window x-left))
	  (y (window/y->device window y-bottom)))
      (let ((width (+ (- (window/x->device window x-right) x) 1))
	    (height (+ (- (window/y->device window y-top) y) 1)))
	(let ((image (image/create device width height)))
	  (os2ps-bitblt (image/ps (image/descriptor image))
			(window/backing-store window)
			(vector x (+ x width) 0)
			(vector y (+ y height) 0)
			ROP_SRCCOPY
			BBO_OR)
	  image)))))

(define (os2-image/create device width height)
  device
  (create-image width height))

(define (create-image width height)
  (let ((ps (os2ps-create-memory-ps)))
    (os2ps-set-bitmap ps (os2ps-create-bitmap ps width height))
    (add-to-gc-finalizer! image-finalizer (make-image ps width height #f))))

(define (os2-image/set-colormap image colormap)
  ;; Kludge: IMAGE/FILL-FROM-BYTE-VECTOR doesn't accept a colormap
  ;; argument to define how the bytes in the vector map into colors.
  ;; But OS/2 needs this information in order to transform those bytes
  ;; into a bitmap.  So this operation allows a colormap to be stored
  ;; in the image and retrieved later.
  (set-image/colormap! (image/descriptor image) colormap))

(define (os2-image/destroy image)
  (destroy-image (image/descriptor image)))

(define (destroy-image image)
  (remove-from-gc-finalizer! image-finalizer image))

(define (destroy-memory-ps ps)
  (let ((bitmap (os2ps-set-bitmap ps #f)))
    (os2ps-destroy-memory-ps ps)
    (if bitmap
	(os2ps-destroy-bitmap bitmap))))

(define (os2-image/width image)
  (image/width (image/descriptor image)))

(define (os2-image/height image)
  (image/height (image/descriptor image)))

(define (os2-image/fill-from-byte-vector image bytes)
  (let ((image (image/descriptor image)))
    (set-bitmap-bits
     (image/ps image)
     (let ((width (image/width image))
	   (height (image/height image)))
       (make-bitmap-info width height 8
			 (image/colormap image)
			 (convert-bitmap-data width height bytes))))))

(define (convert-bitmap-data width height bytes)
  ;; Convert Scheme bitmap data layout to OS/2 bitmap layout.  Scheme
  ;; layout is row-major with upper-left corner at index zero with no
  ;; padding.  OS/2 layout is row-major with lower-left corner at
  ;; index zero and rows padded to 32-bit boundaries.  This conversion
  ;; uses the OS/2 standard 8-bit-per-pixel bitmap format.
  (let ((row-size (* (ceiling (/ (* 8 width) 32)) 4)))
    (let ((copy (make-string (* row-size height))))
      (let loop ((from 0) (to (string-length copy)))
	(if (not (fix:= to 0))
	    (let ((from* (fix:+ from width))
		  (to (fix:- to row-size)))
	      (substring-move! bytes from from* copy to)
	      (loop from* to))))
      copy)))

(define (os2-image/draw device x y image)
  (let ((window (graphics-device/descriptor device))
	(image (image/descriptor image)))
    (draw-image device
		(window/x->device window x)
		(window/y->device window y)
		image
		0
		0
		(image/width image)
		(image/height image))))

(define (os2-image/draw-subimage device x y image
				 image-x image-y image-width image-height)
  (let ((window (graphics-device/descriptor device))
	(image (image/descriptor image)))
    (draw-image device
		(window/x->device window x)
		(window/y->device window y)
		image
		image-x
		;; IMAGE-Y must be inverted because Scheme images have
		;; origin in upper left and OS/2 bitmaps have origin
		;; in lower left.
		(- (image/height image) (+ image-y image-height))
		image-width
		image-height)))

(define (draw-image device x-left y-top
		    image image-x image-y image-width image-height)
  (let ((y-top (+ y-top 1)))
    (let ((x-right (+ x-left image-width))
	  (y-bottom (- y-top image-height)))
      (os2ps-bitblt (os2-graphics-device/psid device)
		    (image/ps image)
		    (vector x-left x-right image-x)
		    (vector y-bottom y-top image-y)
		    ROP_SRCCOPY
		    BBO_OR)
      (invalidate-rectangle device x-left x-right y-bottom y-top))))

;;;; Bitmap I/O

;;; This code uses the OS/2 C datatype modelling code to manipulate
;;; OS/2 C data types which are contained in Scheme character strings.

(define (get-bitmap-bits psid n-bits)
  (if (not (memv n-bits '(1 4 8 24)))
      (error:bad-range-argument n-bits 'GET-BITMAP-BITS))
  (maybe-initialize-bitmaps!)
  (call-with-values (lambda () (get-bitmap-dimensions (os2ps-get-bitmap psid)))
    (lambda (width height)
      (let ((info (make-bytes:bitmap-info-2 1 n-bits))
	    (data (make-bytes:bitmap-data width height 1 n-bits)))
	(let ((n (os2ps-get-bitmap-bits psid 0 height data info)))
	  (if (not (= height n))
	      (error "Only able to read part of bitmap data:" n height)))
	(bytes->bitmap-info info data)))))

(define (set-bitmap-bits psid info)
  (maybe-initialize-bitmaps!)
  (let ((height (bitmap-info/height info)))
    (call-with-values (lambda () (bitmap-info->bytes info))
      (lambda (info data)
	(let ((n (os2ps-set-bitmap-bits psid 0 height data info)))
	  (if (not (= height n))
	      (error "Only able to write part of bitmap data:" n height)))))))

(define bitmaps-initialized? #f)
(define (maybe-initialize-bitmaps!)
  (without-interrupts
   (lambda ()
     (if (not bitmaps-initialized?)
	 (begin
	   (initialize-c-types!)
	   (define-c-type "USHORT" "unsigned short")
	   (define-c-type "ULONG"  "unsigned long")
	   (define-c-type "BITMAPINFOHEADER"
	     '(struct ("ULONG"  "cbFix")
		      ("USHORT" "cx")
		      ("USHORT" "cy")
		      ("USHORT" "cPlanes")
		      ("USHORT" "cBitCount")))
	   (define-c-type "BITMAPINFO2"
	     '(struct ("ULONG"  "cbFix")
		      ("ULONG"  "cx")
		      ("ULONG"  "cy")
		      ("USHORT" "cPlanes")
		      ("USHORT" "cBitCount")
		      ("ULONG"  "ulCompression")
		      ("ULONG"  "cbImage")
		      ("ULONG"  "cxResolution")
		      ("ULONG"  "cyResolution")
		      ("ULONG"  "cclrUsed")
		      ("ULONG"  "cclrImportant")
		      ("USHORT" "usUnits")
		      ("USHORT" "usReserved")
		      ("USHORT" "usRecording")
		      ("USHORT" "usRendering")
		      ("ULONG"  "cSize1")
		      ("ULONG"  "cSize2")
		      ("ULONG"  "ulColorEncoding")
		      ("ULONG"  "ulIdentifier")
		      ((array "ULONG" 1) "argbColor")))
	   (set! get-bitmap-dimensions (make:get-bitmap-dimensions))
	   (set! bytes->bitmap-info (make:bytes->bitmap-info))
	   (set! bitmap-info->bytes (make:bitmap-info->bytes))
	   (set! make-bytes:bitmap-info-2 (make:make-bytes:bitmap-info-2))
	   (set! bitmaps-initialized? #t)
	   unspecific)))))

(define get-bitmap-dimensions)
(define (make:get-bitmap-dimensions)
  (let ((type (lookup-c-type "BITMAPINFOHEADER")))
    (let ((width (c-number-reader type 0 "cx"))
	  (height (c-number-reader type 0 "cy")))
      (lambda (bid)
	(let ((bytes (os2ps-get-bitmap-parameters bid)))
	  (values (width bytes) (height bytes)))))))

(define bytes->bitmap-info)
(define (make:bytes->bitmap-info)
  (let ((type (lookup-c-type "BITMAPINFO2")))
    (let ((width (c-number-reader type 0 "cx"))
	  (height (c-number-reader type 0 "cy"))
	  (n-bits (c-number-reader type 0 "cBitCount"))
	  (get-color (c-array-reader type 0 "argbColor")))
      (lambda (bytes data)
	(let ((n-bits (n-bits bytes)))
	  (make-bitmap-info (width bytes)
			    (height bytes)
			    n-bits
			    (if (= n-bits 24)
				#f
				(make-initialized-vector (expt 2 n-bits)
				  (lambda (index)
				    (get-color bytes index))))
			    data))))))

(define bitmap-info->bytes)
(define (make:bitmap-info->bytes)
  (let ((type (lookup-c-type "BITMAPINFO2")))
    (let ((set-width! (c-number-writer type 0 "cx"))
	  (set-height! (c-number-writer type 0 "cy"))
	  (set-color! (c-array-writer type 0 "argbColor")))
      (lambda (info)
	(let ((n-bits (bitmap-info/n-bits info)))
	  (let ((bytes (make-bytes:bitmap-info-2 1 n-bits)))
	    (set-width! bytes (bitmap-info/width info))
	    (set-height! bytes (bitmap-info/height info))
	    (if (not (= n-bits 24))
		(let ((n-colors (expt 2 n-bits))
		      (colormap (bitmap-info/colormap info)))
		  (do ((index 0 (fix:+ index 1)))
		      ((fix:= index n-colors))
		    (set-color! bytes index (vector-ref colormap index)))))
	    (values bytes (bitmap-info/data info))))))))

(define-structure (bitmap-info (conc-name bitmap-info/))
  (width #f read-only #t)
  (height #f read-only #t)
  (n-bits #f read-only #t)
  (colormap #f read-only #t)
  (data #f read-only #t))

(define (make-bytes:bitmap-data width height n-planes n-bits)
  (make-string (* (ceiling (/ (* n-bits width) 32)) 4 height n-planes)))

;;; OS2PS-GET-BITMAP-BITS and OS2PS-SET-BITMAP-BITS both require an
;;; argument of type BITMAPINFO2.  On input, this argument specifies
;;; the external format of the bitmap, which is just the size and
;;; depth of the information.  The colormap information is output from
;;; OS2PS-GET-BITMAP-BITS and input to OS2PS-SET-BITMAP-BITS.

(define make-bytes:bitmap-info-2)
(define (make:make-bytes:bitmap-info-2)
  (let ((type (lookup-c-type "BITMAPINFO2")))
    (call-with-values (lambda () (select-c-type type 0 '("argbColor")))
      (lambda (rgb-type size-base)
	(let ((size-increment (c-array-type/element-spacing rgb-type))
	      (set-struct-size! (c-number-writer type 0 "cbFix"))
	      (set-n-planes! (c-number-writer type 0 "cPlanes"))
	      (set-n-bits! (c-number-writer type 0 "cBitCount")))
	  (lambda (n-planes n-bits)
	    (let ((info
		   (make-string (+ size-base
				   (if (= n-bits 24)
				       0
				       (* size-increment (expt 2 n-bits))))
				(ascii->char 0))))
	      (set-struct-size! info size-base)
	      (set-n-planes! info n-planes)
	      (set-n-bits! info n-bits)
	      info)))))))