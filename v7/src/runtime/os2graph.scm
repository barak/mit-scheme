#| -*-Scheme-*-

$Id: os2graph.scm,v 1.1 1995/01/06 00:50:16 cph Exp $

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
	 `((available? ,os2-graphics/available?)
	   (clear ,os2-graphics/clear)
	   (close ,os2-graphics/close)
	   (color? ,os2-graphics/color?)
	   (coordinate-limits ,os2-graphics/coordinate-limits)
	   (device-coordinate-limits ,os2-graphics/device-coordinate-limits)
	   (define-color ,os2-graphics/define-color)
	   (drag-cursor ,os2-graphics/drag-cursor)
	   (draw-line ,os2-graphics/draw-line)
	   (draw-lines ,os2-graphics/draw-lines)
	   (draw-point ,os2-graphics/draw-point)
	   (draw-text ,os2-graphics/draw-text)
	   (find-color ,os2-graphics/find-color)
	   (flush ,os2-graphics/flush)
	   (move-cursor ,os2-graphics/move-cursor)
	   (open ,os2-graphics/open)
	   (reset-clip-rectangle ,os2-graphics/reset-clip-rectangle)
	   (set-background-color ,os2-graphics/set-background-color)
	   (set-clip-rectangle ,os2-graphics/set-clip-rectangle)
	   (set-coordinate-limits ,os2-graphics/set-coordinate-limits)
	   (set-drawing-mode ,os2-graphics/set-drawing-mode)
	   (set-foreground-color ,os2-graphics/set-foreground-color)
	   (set-line-style ,os2-graphics/set-line-style))))
  (register-graphics-device-type 'OS/2 os2-graphics-device-type)
  (set! event-descriptor #f)
  (set! event-previewer-registration #f)
  (set! window-list (make-protection-list))
  (set! color-table '())
  (for-each (lambda (entry)
	      (os2-graphics/define-color #f (car entry) (cdr entry)))
	    initial-color-definitions)
  (add-event-receiver! event:before-exit finalize-pm-state!)
  (add-gc-daemon! close-lost-windows-daemon))

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

(define (close-lost-windows-daemon)
  (clean-lost-protected-objects window-list os2win-close))

(define (os2-graphics/available?)
  (implemented-primitive-procedure? os2win-open))

(define (os2-graphics/open descriptor->device)
  (if (not event-descriptor)
      (let ((descriptor (os2win-open-event-qid)))
	(set! event-previewer-registration (make-event-previewer descriptor))
	(set! event-descriptor descriptor)))
  (let ((wid (os2win-open-1 event-descriptor ws_savebits "Scheme Graphics"))
	(foreground-color #xFFFFFF)
	(background-color  #x000000))
    (os2win-set-colors wid foreground-color background-color)
    (os2win-show-cursor wid #f)
    (os2win-show wid #t)
    (os2win-set-state wid window-state:deactivate)
    (os2win-set-state wid window-state:top)
    (let ((window
	   (let ((w.h (os2win-get-size wid)))
	     (make-os2-window wid
			      (car w.h)
			      (cdr w.h)
			      (set-normal-font! wid "4.System VIO")
			      foreground-color
			      background-color))))
      (compute-window-slopes! window)
      (add-to-protection-list! window-list window wid)
      (descriptor->device window))))

(define (set-normal-font! wid font)
  (let ((metrics (os2win-set-font wid 1 font)))
    (if (not metrics)
	(error "Unknown font name:" font))
    (let ((width (font-metrics/width metrics))
	  (height (font-metrics/height metrics)))
      (os2win-set-grid wid width height)
      (os2win-shape-cursor wid width height
			   (fix:or CURSOR_SOLID CURSOR_FLASH)))
    metrics))

(define (compute-window-slopes! window)
  (set-os2-window/x-slope! window
			   (/ (- (os2-window/pel-width window) 1)
			      (- (os2-window/x-right window)
				 (os2-window/x-left window))))
  (set-os2-window/y-slope! window
			   (/ (- (os2-window/pel-height window) 1)
			      (- (os2-window/y-top window)
				 (os2-window/y-bottom window)))))

(define (os2-graphics/close device)
  (without-interrupts
   (lambda ()
     (close-window (graphics-device/descriptor device)))))

(define (close-window window)
  (if (os2-window/wid window)
      (begin
	(os2win-close (os2-window/wid window))
	(set-os2-window/wid! window #f)
	(remove-from-protection-list! window-list window))))

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
	       (eq? (os2-window/wid window) wid))))))
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
	     (not (os2win-focus? (os2-window/wid window))))
	(os2win-activate (os2-window/wid window)))))

(define-event-handler event-type:close
  (lambda (window event)
    event
    (close-window window)))

(define-event-handler event-type:paint
  (lambda (window event)
    event
    (clear-window window)
    (play-segment (os2-window/segment window))))

(define-event-handler event-type:resize
  (lambda (window event)
    (set-os2-window/pel-width! window (resize-event/width event))
    (set-os2-window/pel-height! window (resize-event/height event))
    (compute-window-slopes! window)))

(define (os2-graphics/clear device)
  (reset-segment (os2-graphics-device/segment device))
  (clear-window (graphics-device/descriptor device)))

(define (clear-window window)
  (os2win-clear (os2-window/wid window)
		0 (os2-window/pel-width window)
		0 (os2-window/pel-height window)))

(define (os2-graphics/coordinate-limits device)
  (let ((window (graphics-device/descriptor device)))
    (without-interrupts
     (lambda ()
       (values (os2-window/x-left window)
	       (os2-window/y-bottom window)
	       (os2-window/x-right window)
	       (os2-window/y-top window))))))

(define (os2-graphics/device-coordinate-limits device)
  (without-interrupts
   (lambda ()
     (values 0
	     0
	     (- (os2-graphics-device/pel-width device) 1)
	     (- (os2-graphics-device/pel-height device) 1)))))

(define (os2-graphics/drag-cursor device x y)
  (drawing-operation (os2-graphics-device/segment device)
    (lambda ()
      (os2win-line (os2-graphics-device/wid device)
		   (os2-graphics-device/x->device device x)
		   (os2-graphics-device/y->device device y)))))

(define (os2-graphics/draw-line device x-start y-start x-end y-end)
  (os2-graphics/move-cursor device x-start y-start)
  (os2-graphics/drag-cursor device x-end y-end))

(define (os2-graphics/draw-lines device xv yv)
  (drawing-operation (os2-graphics-device/segment device)
    (lambda ()
      (os2win-poly-line-disjoint
       (os2-graphics-device/wid device)
       (vector-map xv (lambda (x) (os2-graphics-device/x->device device x)))
       (vector-map yv
		   (lambda (y) (os2-graphics-device/y->device device y)))))))

(define (os2-graphics/draw-point device x y)
  (drawing-operation (os2-graphics-device/segment device)
    (lambda ()
      (let ((wid (os2-graphics-device/wid device))
	    (x (os2-graphics-device/x->device device x))
	    (y (os2-graphics-device/y->device device y))
	    (type))
	(dynamic-wind
	 (lambda ()
	   (set! type (map-line-style (graphics-device/line-style device)))
	   (os2win-set-line-type wid LINETYPE_SOLID))
	 (lambda ()
	   (os2win-move-graphics-cursor wid x y)
	   (os2win-line wid x y))
	 (lambda ()
	   (os2win-set-line-type wid type)))))))

(define (os2-graphics/draw-text device x y string)
  (drawing-operation (os2-graphics-device/segment device)
    (lambda ()
      (os2win-write (os2-graphics-device/wid device)
		    (os2-graphics-device/x->device device x)
		    (fix:+ (os2-graphics-device/y->device device y)
			   (os2-graphics-device/char-descender device))
		    string
		    0
		    (string-length string)))))

(define (os2-graphics/flush device)
  (flush-segment (os2-graphics-device/segment device)))

(define (os2-graphics/move-cursor device x y)
  (drawing-operation (os2-graphics-device/segment device)
    (lambda ()
      (os2win-move-graphics-cursor (os2-graphics-device/wid device)
				   (os2-graphics-device/x->device device x)
				   (os2-graphics-device/y->device device y)))))

(define (os2-graphics/reset-clip-rectangle device)
  device
  unspecific)

(define (os2-graphics/set-clip-rectangle device x-left y-bottom x-right y-top)
  device x-left y-bottom x-right y-top
  unspecific)

(define (os2-graphics/set-coordinate-limits device
					    x-left y-bottom x-right y-top)
  (drawing-operation (os2-graphics-device/segment device)
    (lambda ()
      (let ((window (graphics-device/descriptor device)))
	(set-os2-window/x-left! window x-left)
	(set-os2-window/y-bottom! window y-bottom)
	(set-os2-window/x-right! window x-right)
	(set-os2-window/y-top! window y-top)
	(compute-window-slopes! window)))))

(define (os2-graphics/set-drawing-mode device mode)
  (drawing-operation (os2-graphics-device/segment device)
    (lambda ()
      (os2win-set-mix (os2-graphics-device/wid device)
		      (map-drawing-mode mode)))))

(define (os2-graphics/set-line-style device style)
  (drawing-operation (os2-graphics-device/segment device)
    (lambda ()
      (os2win-set-line-type (os2-graphics-device/wid device)
			    (map-line-style style)))))

(define (os2-graphics/color? device)
  (not (= 0 (os2win-query-capability (os2-graphics-device/wid device)
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
  (drawing-operation (os2-graphics-device/segment device)
    (lambda ()
      (set-os2-graphics-device/background-color!
       device
       (->color color 'SET-BACKGROUND-COLOR))
      (update-colors (graphics-device/descriptor device)))))

(define (os2-graphics/set-foreground-color device color)
  (drawing-operation (os2-graphics-device/segment device)
    (lambda ()
      (set-os2-graphics-device/foreground-color!
       device
       (->color color 'SET-FOREGROUND-COLOR))
      (update-colors (graphics-device/descriptor device)))))

(define (update-colors window)
  (os2win-set-colors (os2-window/wid window)
		     (os2-window/foreground-color window)
		     (os2-window/background-color window)))

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

(define-structure (os2-window
		   (conc-name os2-window/)
		   (constructor make-os2-window
				(wid
				 pel-width
				 pel-height
				 font-metrics
				 foreground-color
				 background-color)))
  wid
  pel-width
  pel-height
  font-metrics
  foreground-color
  background-color
  (x-left -1)
  (y-bottom -1)
  (x-right 1)
  (y-top 1)
  x-slope
  y-slope
  (segment (make-segment) read-only #t))

(define-integrable (os2-graphics-device/wid device)
  (os2-window/wid (graphics-device/descriptor device)))

(define-integrable (os2-graphics-device/pel-width device)
  (os2-window/pel-width (graphics-device/descriptor device)))

(define-integrable (os2-graphics-device/pel-height device)
  (os2-window/pel-height (graphics-device/descriptor device)))

(define-integrable (os2-graphics-device/char-descender device)
  (font-metrics/descender
   (os2-window/font-metrics (graphics-device/descriptor device))))

(define-integrable (os2-graphics-device/x-left device)
  (os2-window/x-left (graphics-device/descriptor device)))

(define-integrable (os2-graphics-device/y-bottom device)
  (os2-window/y-bottom (graphics-device/descriptor device)))

(define-integrable (os2-graphics-device/x-right device)
  (os2-window/x-right (graphics-device/descriptor device)))

(define-integrable (os2-graphics-device/y-top device)
  (os2-window/y-top (graphics-device/descriptor device)))

(define-integrable (os2-graphics-device/x-slope device)
  (os2-window/x-slope (graphics-device/descriptor device)))

(define-integrable (os2-graphics-device/y-slope device)
  (os2-window/y-slope (graphics-device/descriptor device)))

(define-integrable (os2-graphics-device/segment device)
  (os2-window/segment (graphics-device/descriptor device)))

(define (os2-graphics-device/x->device device x)
  (round->exact (* (os2-graphics-device/x-slope device)
		   (- x (os2-graphics-device/x-left device)))))

(define (os2-graphics-device/y->device device y)
  (round->exact (* (os2-graphics-device/y-slope device)
		   (- y (os2-graphics-device/y-bottom device)))))

(define-integrable (os2-graphics-device/foreground-color device)
  (os2-window/foreground-color (graphics-device/descriptor device)))

(define-integrable (set-os2-graphics-device/foreground-color! device color)
  (set-os2-window/foreground-color! (graphics-device/descriptor device) color))

(define-integrable (os2-graphics-device/background-color device)
  (os2-window/background-color (graphics-device/descriptor device)))

(define-integrable (set-os2-graphics-device/background-color! device color)
  (set-os2-window/background-color! (graphics-device/descriptor device) color))

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

;;;; Drawing Segments

(define (make-segment)
  (cons (cons '() '())
	(cons '() '())))

(define (reset-segment segment)
  (without-interrupts
   (lambda ()
     (set-car! (car segment) '())
     (set-cdr! (car segment) '())
     (set-car! (cdr segment) '())
     (set-cdr! (cdr segment) '()))))

(define (flush-segment segment)
  (%play-segment
   (without-interrupts
    (lambda ()
      (let ((new-head (caar segment))
	    (new-tail (cdar segment)))
	(%enqueue-segment (cdr segment) new-head new-tail)
	(set-car! (car segment) '())
	(set-cdr! (car segment) '())
	new-head)))))

(define (drawing-operation segment thunk)
  (without-interrupts
   (lambda ()
     (let ((new (list thunk)))
       (%enqueue-segment (car segment) new new)))))

(define (play-segment segment)
  (%play-segment (cadr segment)))

(define (%enqueue-segment h.t new-head new-tail)
  (let ((old (cdr h.t)))
    (set-cdr! h.t new-tail)
    (if (null? old)
	(set-car! h.t new-head)
	(set-cdr! old new-head))))

(define (%play-segment thunks)
  (do ((thunks thunks (cdr thunks)))
      ((null? thunks))
    ((car thunks))))