#| -*-Scheme-*-

$Id: os2graph.scm,v 1.5 1995/02/14 00:36:58 cph Exp $

Copyright (c) 1995 Massachusetts Institute of Technology

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

;;;; OS/2 PM Graphics Interface
;;; package: (runtime os2-graphics)

(declare (usual-integrations))
(declare (integrate-external "graphics"))
(declare (integrate-external "os2winp"))

(define os2-graphics-device-type)
(define event-descriptor)
(define event-previewer-registration)
(define window-list)
(define color-table)

(define (initialize-package!)
  (set! os2-graphics-device-type
	(make-graphics-device-type
	 `((activate-window ,os2-graphics/activate-window)
	   (available? ,os2-graphics/available?)
	   (clear ,os2-graphics/clear)
	   (close ,os2-graphics/close)
	   (color? ,os2-graphics/color?)
	   (coordinate-limits ,os2-graphics/coordinate-limits)
	   (deactivate-window ,os2-graphics/deactivate-window)
	   (define-color ,os2-graphics/define-color)
	   (desktop-size ,os2-graphics/desktop-size)
	   (device-coordinate-limits ,os2-graphics/device-coordinate-limits)
	   (drag-cursor ,os2-graphics/drag-cursor)
	   (draw-line ,os2-graphics/draw-line)
	   (draw-lines ,os2-graphics/draw-lines)
	   (draw-point ,os2-graphics/draw-point)
	   (draw-text ,os2-graphics/draw-text)
	   (find-color ,os2-graphics/find-color)
	   (flush ,os2-graphics/flush)
	   (hide-window ,os2-graphics/hide-window)
	   (lower-window ,os2-graphics/lower-window)
	   (maximize-window ,os2-graphics/maximize-window)
	   (minimize-window ,os2-graphics/minimize-window)
	   (move-cursor ,os2-graphics/move-cursor)
	   (open ,os2-graphics/open)
	   (raise-window ,os2-graphics/raise-window)
	   (reset-clip-rectangle ,os2-graphics/reset-clip-rectangle)
	   (restore-window ,os2-graphics/restore-window)
	   (set-background-color ,os2-graphics/set-background-color)
	   (set-clip-rectangle ,os2-graphics/set-clip-rectangle)
	   (set-coordinate-limits ,os2-graphics/set-coordinate-limits)
	   (set-drawing-mode ,os2-graphics/set-drawing-mode)
	   (set-font ,os2-graphics/set-font)
	   (set-foreground-color ,os2-graphics/set-foreground-color)
	   (set-line-style ,os2-graphics/set-line-style)
	   (set-window-position ,os2-graphics/set-window-position)
	   (set-window-size ,os2-graphics/set-window-size)
	   (set-window-title ,os2-graphics/set-window-title)
	   (window-position ,os2-graphics/window-position)
	   (window-size ,os2-graphics/window-size))))
  (register-graphics-device-type 'OS/2 os2-graphics-device-type)
  (set! event-descriptor #f)
  (set! event-previewer-registration #f)
  (set! window-list (make-protection-list))
  (set! color-table '())
  (for-each (lambda (entry)
	      (os2-graphics/define-color #f (car entry) (cdr entry)))
	    initial-color-definitions)
  (add-event-receiver! event:before-exit finalize-pm-state!)
  (add-gc-daemon! close-lost-objects-daemon))

(define (finalize-pm-state!)
  (if event-descriptor
      (begin
	(do ((windows (protection-list-elements window-list) (cdr windows)))
	    ((null? windows))
	  (close-window (car windows)))
	(deregister-input-thread-event event-previewer-registration)
	(set! event-previewer-registration #f)
	(os2win-close-event-qid event-descriptor)
	(set! event-descriptor #f)
	unspecific)))

(define (close-lost-objects-daemon)
  (clean-lost-protected-objects window-list os2win-close))

;;;; Window Abstraction

(define-structure (window
		   (conc-name window/)
		   (constructor %make-window
				(wid pel-width
				     pel-height
				     backing-store
				     backing-store-bitmap)))
  wid
  pel-width
  pel-height
  (backing-store #f read-only #t)
  backing-store-bitmap
  (changes #f)
  (x-gcursor 0)
  (y-gcursor 0)
  (x-left -1)
  (y-bottom -1)
  (x-right 1)
  (y-top 1)
  (x-slope (/ (- pel-width 1) 2))
  (y-slope (/ (- pel-height 1) 2))
  font-specifier
  font-metrics
  (foreground-color #xFFFFFF)
  (background-color #x000000))

(define (make-window wid)
  (let ((window
	 (let ((w.h (os2win-get-size wid))
	       (psid (os2ps-create-memory-ps)))
	   (let ((bid (os2ps-create-bitmap psid pel-width pel-height)))
	     (os2ps-set-bitmap psid bid)
	     (%make-window wid (car w.h) (cdr w.h) psid bid)))))
    (add-to-protection-list! window-list window wid)
    window))

(define (close-window window)
  (if (window/wid window)
      (begin
	(os2ps-destroy-memory-ps (window/backing-store window))
	(os2win-close (window/wid window))
	(set-window/wid! window #f)
	(remove-from-protection-list! window-list window))))

(define-integrable (os2-graphics-device/wid device)
  (window/wid (graphics-device/descriptor device)))

(define-integrable (os2-graphics-device/psid device)
  (window/backing-store (graphics-device/descriptor device)))

(define (compute-window-slopes! window)
  (set-window/x-slope! window
		       (/ (- (window/pel-width window) 1)
			  (- (window/x-right window) (window/x-left window))))
  (set-window/y-slope! window
		       (/ (- (window/pel-height window) 1)
			  (- (window/y-top window) (window/y-bottom window)))))

(define (set-window-font! window font-specifier)
  (set-window/font-specifier! window font-specifier)
  (set-window/font-metrics!
   window
   (let ((metrics
	  (os2ps-set-font (window/backing-store window) 1 font-specifier)))
     (if (not metrics)
	 (error "Unknown font name:" font-specifier))
     metrics)))

(define (window/x->device window x)
  (round->exact (* (window/x-slope window) (- x (window/x-left window)))))

(define (window/y->device window y)
  (round->exact (* (window/y-slope window) (- y (window/y-bottom window)))))

;;;; Standard Operations

(define (os2-graphics/available?)
  (implemented-primitive-procedure? os2win-open))

(define (os2-graphics/open descriptor->device)
  (if (not event-descriptor)
      (let ((descriptor (os2win-open-event-qid)))
	(set! event-previewer-registration (make-event-previewer descriptor))
	(set! event-descriptor descriptor)))
  (let ((wid (os2win-open event-descriptor "Scheme Graphics")))
    (os2win-show-cursor wid #f)
    (os2win-show wid #t)
    (os2win-set-state wid window-state:deactivate)
    (os2win-set-state wid window-state:top)
    (let ((window (make-window wid)))
      (update-colors window)
      (set-window-font! window "4.System VIO")
      (let ((device (descriptor->device window)))
	(os2-graphics/clear device)
	device))))

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
	   (invalidate-rectangle device xl yl xh yh)))))))

(define (os2-graphics/draw-line device x-start y-start x-end y-end)
  (os2-graphics/move-cursor device x-start y-start)
  (os2-graphics/drag-cursor device x-end y-end))

(define (os2-graphics/draw-lines device xv yv)
  (let ((window (graphics-device/descriptor device)))
    (without-interrupts
     (lambda ()
       (let ((xv (vector-map xv (lambda (x) (window/x->device window x))))
	     (yv (vector-map yv (lambda (y) (window/y->device window y)))))
	 (let ((xl (fix:vector-min xv))
	       (yl (fix:vector-min yv))
	       (xh (fix:+ (fix:vector-max xv) 1))
	       (yh (fix:+ (fix:vector-max yv) 1)))
	   (os2ps-poly-line-disjoint (window/backing-store window) xv yv)
	   (invalidate-rectangle device xl yl xh yh)))))))

(define (os2-graphics/draw-point device x y)
  ;; This sucks.  Implement a real point-drawing primitive.
  (let ((window (graphics-device/descriptor device)))
    (without-interrupts
     (lambda ()
       (let ((x (window/x->device window x))
	     (y (window/y->device window y)))
	 (os2ps-draw-point (window/backing-store window) x y)
	 (invalidate-rectangle device x y (fix:+ x 1) (fix:+ y 1)))))))

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
			       y
			       (fix:+ x
				      (os2ps-text-width psid string 0 length))
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
  (if (not (and (color-name? name)
		(not (char=? #\# (string-ref name 0)))))
      (error:wrong-type-argument name "color name" 'DEFINE-COLOR))
  (let ((entry (lookup-color-name name))
	(color (->color color 'DEFINE-COLOR)))
    (if entry
	(set-cdr! entry color)
	(begin
	  (set! color-table (cons (cons name color) color-table))
	  unspecific))))

(define (os2-graphics/find-color device specification)
  device
  (->color specification 'FIND-COLOR))

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

;;;; Window Operations

(define (os2-graphics/window-size device)
  (let ((w.h (os2win-get-size (os2-graphics-device/wid device))))
    (values (car w.h)
	    (cdr w.h))))

(define (os2-graphics/set-window-size device width height)
  (os2win-set-size (os2-graphics-device/wid device) width height))

(define (os2-graphics/window-frame-size device)
  (let ((w.h (os2win-get-size (os2-graphics-device/wid device))))
    (values (car w.h)
	    (cdr w.h))))

(define (os2-graphics/display-size device)
  device
  (values (os2win-desktop-width) (os2win-desktop-height)))

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
    ("brown"        127  63   0)))

;;;; Miscellaneous Support

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

(define (make-event-previewer descriptor)
  (permanently-register-input-thread-event
   descriptor
   (current-thread)
   (lambda ()
     (let ((event (os2win-get-event descriptor #f)))
       (if event
	   (process-event event))))))

(define (process-event event)
  (let ((window
	 (search-protection-list window-list
	   (let ((wid (event-wid event)))
	     (lambda (window)
	       (eq? (window/wid window) wid))))))
    (if window
	(let ((handler (vector-ref event-handlers (event-type event))))
	  (if handler
	      (handler window event))))))

(define event-handlers
  (make-vector number-of-event-types #f))

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
	  (os2ps-destroy-bitmap (os2ps-set-bitmap old bitmap))
	  (set-window/backing-store-bitmap! window bitmap)))
      (set-window/pel-width! window width)
      (set-window/pel-height! window height)
      (compute-window-slopes! window)
      (os2win-invalidate (window/wid window) 0 width 0 height)
      (set-window/changes! window #f))))

;;;; Protection lists

(define (make-protection-list)
  (list 'PROTECTION-LIST))

;; This is used after a disk-restore, to remove invalid information.

(define (drop-all-protected-objects list)
  (with-absolutely-no-interrupts
    (lambda ()
      (set-cdr! list '()))))

(define (add-to-protection-list! list scheme-object microcode-object)
  (with-absolutely-no-interrupts
   (lambda ()
     (set-cdr! list
	       (cons (weak-cons scheme-object microcode-object)
		     (cdr list))))))

(define (remove-from-protection-list! list scheme-object)
  (with-absolutely-no-interrupts
   (lambda ()
     (let loop ((associations (cdr list)) (previous list))
       (if (not (null? associations))
	   (if (eq? scheme-object (weak-pair/car? (car associations)))
	       (set-cdr! previous (cdr associations))
	       (loop (cdr associations) associations)))))))

(define (clean-lost-protected-objects list cleaner)
  (let loop ((associations (cdr list)) (previous list))
    (if (not (null? associations))
	(if (weak-pair/car? (car associations))
	    (loop (cdr associations) associations)
	    (begin
	      (cleaner (weak-cdr (car associations)))
	      (let ((next (cdr associations)))
		(set-cdr! previous next)
		(loop next previous)))))))

(define (search-protection-list list predicate)
  (let loop ((associations (cdr list)))
    (and (not (null? associations))
	 (let ((scheme-object (weak-car (car associations))))
	   (if (and scheme-object (predicate scheme-object))
	       scheme-object
	       (loop (cdr associations)))))))

(define (protection-list-elements list)
  (with-absolutely-no-interrupts
   (lambda ()
     (let loop ((associations (cdr list)))
       (cond ((null? associations)
	      '())
	     ((weak-pair/car? (car associations))
	      (cons (weak-car (car associations))
		    (loop (cdr associations))))
	     (else
	      (loop (cdr associations))))))))