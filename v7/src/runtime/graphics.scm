#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/graphics.scm,v 1.3 1990/01/22 23:39:37 cph Rel $

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

;;;; Graphics Operations
;;; package: (runtime graphics)

(declare (usual-integrations))

(define-structure (graphics-device-type
		   (conc-name graphics-device-type/)
		   (constructor
		    %make-graphics-device-type
		    (operation/available?
		     operation/clear
		     operation/close
		     operation/coordinate-limits
		     operation/device-coordinate-limits
		     operation/drag-cursor
		     operation/draw-line
		     operation/draw-point
		     operation/draw-text
		     operation/flush
		     operation/move-cursor
		     operation/open
		     operation/reset-clip-rectangle
		     operation/set-clip-rectangle
		     operation/set-coordinate-limits
		     operation/set-drawing-mode
		     operation/set-line-style
		     custom-operations)))
  (operation/available? false read-only true)
  (operation/clear false read-only true)
  (operation/close false read-only true)
  (operation/coordinate-limits false read-only true)
  (operation/device-coordinate-limits false read-only true)
  (operation/drag-cursor false read-only true)
  (operation/draw-line false read-only true)
  (operation/draw-point false read-only true)
  (operation/draw-text false read-only true)
  (operation/flush false read-only true)
  (operation/move-cursor false read-only true)
  (operation/open false read-only true)
  (operation/reset-clip-rectangle false read-only true)
  (operation/set-clip-rectangle false read-only true)
  (operation/set-coordinate-limits false read-only true)
  (operation/set-drawing-mode false read-only true)
  (operation/set-line-style false read-only true)
  (custom-operations false read-only true))

(define (make-graphics-device-type operations)
  (let ((operations
	 (map (lambda (entry)
		(if (not (and (pair? entry)
			      (symbol? (car entry))
			      (pair? (cdr entry))
			      (procedure? (cadr entry))
			      (null? (cddr entry))))
		    (error "Malformed operation alist entry" entry))
		(cons (car entry) (cadr entry)))
	      operations)))
    (let ((operation
	   (lambda (name)
	     (let ((entry (assq name operations)))
	       (if (not entry)
		   (error "Missing operation" name))
	       (set! operations (delq! entry operations))
	       (cdr entry)))))
      (let ((available? (operation 'available?))
	    (clear (operation 'clear))
	    (close (operation 'close))
	    (coordinate-limits (operation 'coordinate-limits))
	    (device-coordinate-limits (operation 'device-coordinate-limits))
	    (drag-cursor (operation 'drag-cursor))
	    (draw-line (operation 'draw-line))
	    (draw-point (operation 'draw-point))
	    (draw-text (operation 'draw-text))
	    (flush (operation 'flush))
	    (move-cursor (operation 'move-cursor))
	    (open (operation 'open))
	    (reset-clip-rectangle (operation 'reset-clip-rectangle))
	    (set-clip-rectangle (operation 'set-clip-rectangle))
	    (set-coordinate-limits (operation 'set-coordinate-limits))
	    (set-drawing-mode (operation 'set-drawing-mode))
	    (set-line-style (operation 'set-line-style)))
	(%make-graphics-device-type available?
				    clear
				    close
				    coordinate-limits
				    device-coordinate-limits
				    drag-cursor
				    draw-line
				    draw-point
				    draw-text
				    flush
				    move-cursor
				    open
				    reset-clip-rectangle
				    set-clip-rectangle
				    set-coordinate-limits
				    set-drawing-mode
				    set-line-style
				    operations)))))

(define (graphics-device-type/operation type name)
  (case name
    ((clear)
     (graphics-device-type/operation/clear type))
    ((close)
     (graphics-device-type/operation/close type))
    ((coordinate-limits)
     (graphics-device-type/operation/coordinate-limits type))
    ((device-coordinate-limits)
     (graphics-device-type/operation/device-coordinate-limits type))
    ((drag-cursor)
     (graphics-device-type/operation/drag-cursor type))
    ((draw-line)
     (graphics-device-type/operation/draw-line type))
    ((draw-point)
     (graphics-device-type/operation/draw-point type))
    ((draw-text)
     (graphics-device-type/operation/draw-text type))
    ((flush)
     (graphics-device-type/operation/flush type))
    ((move-cursor)
     (graphics-device-type/operation/move-cursor type))
    ((reset-clip-rectangle)
     (graphics-device-type/operation/reset-clip-rectangle type))
    ((set-clip-rectangle)
     (graphics-device-type/operation/set-clip-rectangle type))
    ((set-coordinate-limits)
     (graphics-device-type/operation/set-coordinate-limits type))
    ((set-drawing-mode)
     (graphics-device-type/operation/set-drawing-mode type))
    ((set-line-style)
     (graphics-device-type/operation/set-line-style type))
    (else
     (let ((entry (assq name (graphics-device-type/custom-operations type))))
       (if (not entry)
	   (error "Unknown graphics operation" name type))
       (cdr entry)))))

(define (graphics-type-available? type)
  ((graphics-device-type/operation/available? type)))

(define-structure (graphics-device
		   (conc-name graphics-device/)
		   (constructor %make-graphics-device (type descriptor)))
  (type false read-only true)
  descriptor
  (drawing-mode drawing-mode:dominant)
  (line-style line-style:solid)
  (buffer? false))

(define (make-graphics-device type . arguments)
  (let ((descriptor
	 (apply (graphics-device-type/operation/open type) arguments)))
    (and descriptor
	 (%make-graphics-device type descriptor))))

(let-syntax
    ((define-graphics-operation
       (macro (name)
	 `(DEFINE-INTEGRABLE
	    (,(symbol-append 'GRAPHICS-DEVICE/OPERATION/ name) DEVICE)
	    (,(symbol-append 'GRAPHICS-DEVICE-TYPE/OPERATION/ name)
	     (GRAPHICS-DEVICE/TYPE DEVICE))))))
  (define-graphics-operation clear)
  (define-graphics-operation close)
  (define-graphics-operation coordinate-limits)
  (define-graphics-operation device-coordinate-limits)
  (define-graphics-operation drag-cursor)
  (define-graphics-operation draw-line)
  (define-graphics-operation draw-point)
  (define-graphics-operation draw-text)
  (define-graphics-operation flush)
  (define-graphics-operation move-cursor)
  (define-graphics-operation reset-clip-rectangle)
  (define-graphics-operation set-clip-rectangle)
  (define-graphics-operation set-coordinate-limits)
  (define-graphics-operation set-drawing-mode)
  (define-graphics-operation set-line-style))

(define (graphics-operation device name . arguments)
  (let ((value
	 (apply (graphics-device-type/operation (graphics-device/type device)
						name)
		(graphics-device/descriptor device)
		arguments)))
    (maybe-flush device)
    value))

(define (graphics-enable-buffering device)
  (set-graphics-device/buffer?! device true))

(define (graphics-disable-buffering device)
  (if (graphics-device/buffer? device)
      (graphics-flush device))
  (set-graphics-device/buffer?! device false))

(define-integrable (maybe-flush device)
  (if (not (graphics-device/buffer? device))
      (graphics-flush device)))

(define (graphics-close device)
  ((graphics-device/operation/close device)
   (graphics-device/descriptor device)))

(define-integrable (graphics-flush device)
  ((graphics-device/operation/flush device)
   (graphics-device/descriptor device)))

(define (graphics-device-coordinate-limits device)
  ((graphics-device/operation/device-coordinate-limits device)
   (graphics-device/descriptor device)))

(define (graphics-coordinate-limits device)
  ((graphics-device/operation/coordinate-limits device)
   (graphics-device/descriptor device)))

(define (graphics-set-coordinate-limits device x-left y-bottom x-right y-top)
  ((graphics-device/operation/set-coordinate-limits device)
   (graphics-device/descriptor device)
   x-left y-bottom x-right y-top))

(define (graphics-set-clip-rectangle device x-left y-bottom x-right y-top)
  ((graphics-device/operation/set-clip-rectangle device)
   (graphics-device/descriptor device)
   x-left y-bottom x-right y-top))

(define (graphics-reset-clip-rectangle device)
  ((graphics-device/operation/reset-clip-rectangle device)
   (graphics-device/descriptor device)))

(define-integrable drawing-mode:erase 0)
(define-integrable drawing-mode:non-dominant 1)
(define-integrable drawing-mode:complement 2)
(define-integrable drawing-mode:dominant 3)

(define (graphics-bind-drawing-mode device drawing-mode thunk)
  (let ((old-mode (graphics-device/drawing-mode device)))
    (dynamic-wind
     (lambda ()
       (graphics-set-drawing-mode device drawing-mode))
     thunk
     (lambda ()
       (graphics-set-drawing-mode device old-mode)))))

(define (graphics-set-drawing-mode device drawing-mode)
  ((graphics-device/operation/set-drawing-mode device)
   (graphics-device/descriptor device)
   drawing-mode)
  (set-graphics-device/drawing-mode! device drawing-mode))

(define-integrable line-style:solid 0)
(define-integrable line-style:dash 1)
(define-integrable line-style:dot 2)
(define-integrable line-style:dash-dot 3)
(define-integrable line-style:dash-dot-dot 4)
(define-integrable line-style:long-dash 5)
(define-integrable line-style:center-dash 6)
(define-integrable line-style:center-dash-dash 7)

(define (graphics-bind-line-style device line-style thunk)
  (let ((old-style (graphics-device/line-style device)))
    (dynamic-wind
     (lambda ()
       (graphics-set-line-style device line-style))
     thunk
     (lambda ()
       (graphics-set-line-style device old-style)))))

(define (graphics-set-line-style device line-style)
  ((graphics-device/operation/set-line-style device)
   (graphics-device/descriptor device)
   line-style)
  (set-graphics-device/line-style! device line-style))

(define (graphics-clear device)
  ((graphics-device/operation/clear device)
   (graphics-device/descriptor device))
  (maybe-flush device))

(define (graphics-draw-point device x y)
  ((graphics-device/operation/draw-point device)
   (graphics-device/descriptor device)
   x y)
  (maybe-flush device))

(define (graphics-erase-point device x y)
  (graphics-bind-drawing-mode device drawing-mode:erase
    (lambda ()
      (graphics-draw-point device x y))))

(define (graphics-draw-text device x y text)
  ((graphics-device/operation/draw-text device)
   (graphics-device/descriptor device)
   x y text)
  (maybe-flush device))

(define (graphics-draw-line device x-start y-start x-end y-end)
  ((graphics-device/operation/draw-line device)
   (graphics-device/descriptor device)
   x-start y-start x-end y-end)
  (maybe-flush device))

(define (graphics-move-cursor device x y)
  ((graphics-device/operation/move-cursor device)
   (graphics-device/descriptor device)
   x y))

(define (graphics-drag-cursor device x y)
  ((graphics-device/operation/drag-cursor device)
   (graphics-device/descriptor device)
   x y)
  (maybe-flush device))