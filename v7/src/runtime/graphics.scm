#| -*-Scheme-*-

$Id: graphics.scm,v 1.21 2002/02/09 06:09:43 cph Exp $

Copyright (c) 1989-1999, 2001, 2002 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
|#

;;;; Graphics Operations
;;; package: (runtime graphics)

(declare (usual-integrations))

(define-structure (graphics-device-type
		   (conc-name graphics-device-type/)
		   (constructor
		    %make-graphics-device-type
		    (name
		     operation/available?
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
		     custom-operations))
		   (print-procedure
		    (standard-unparser-method 'GRAPHICS-TYPE
		      (lambda (type port)
			(write-char #\space port)
			(write (graphics-device-type/name type) port)))))
  (name false read-only true)
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
  (custom-operations false read-only true)
  (properties (make-1d-table) read-only true))

(define (make-graphics-device-type name operations)
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
	(let ((type
	       (%make-graphics-device-type name
					   available?
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
					   operations)))
	  (add-graphics-type type)
	  type)))))

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

(define graphics-types '())

(define (add-graphics-type type)
  (let ((name (graphics-device-type/name type)))
    (let loop ((types graphics-types))
      (cond ((null? types)
	     (set! graphics-types (cons type graphics-types))
	     unspecific)
	    ((eq? name (graphics-device-type/name (car types)))
	     (set-car! types type))
	    (else
	     (loop (cdr types)))))))

(define (graphics-type #!optional object error?)
  (let ((object (if (default-object? object) #f object))
	(error? (if (default-object? error?) #t error?)))
    (let ((test-type
	   (lambda (type)
	     (if (graphics-device-type/available? type)
		 type
		 (and error?
		      (error "Graphics type not supported:" type))))))
      (cond ((graphics-device-type? object)
	     (test-type object))
	    ((graphics-device? object)
	     (test-type (graphics-device/type object)))
	    ((not object)
	     (or (list-search-positive graphics-types
		   graphics-device-type/available?)
		 (and error?
		      (error "No graphics types supported."))))
	    (else
	     (let ((type
		    (list-search-positive graphics-types
		      (lambda (type)
			(eq? object (graphics-device-type/name type))))))
	       (if type
		   (test-type type)
		   (and error?
			(error "Graphics type unknown:" object)))))))))

(define (graphics-type-available? type)
  (graphics-type type #f))

(define (enumerate-graphics-types)
  (list-transform-positive graphics-types graphics-device-type/available?))

(define (graphics-device-type/available? type)
  ((graphics-device-type/operation/available? type)))

(define (graphics-type-name type)
  (guarantee-graphics-type type 'GRAPHICS-TYPE-NAME)
  (graphics-device-type/name type))

(define (graphics-type-properties type)
  (guarantee-graphics-type type 'GRAPHICS-TYPE-PROPERTIES)
  (graphics-device-type/properties type))

(define (guarantee-graphics-type type name)
  (if (not (graphics-device-type? type))
      (error:wrong-type-argument type "graphics type" name)))

(define-structure (graphics-device
		   (conc-name graphics-device/)
		   (constructor %make-graphics-device (type descriptor)))
  (type false read-only true)
  descriptor
  (drawing-mode drawing-mode:dominant)
  (line-style line-style:solid)
  (buffer? false)
  (properties (make-1d-table) read-only true))

(define (make-graphics-device #!optional type-name . arguments)
  (let ((type
	 (graphics-type (if (default-object? type-name) #f type-name))))
    (apply (graphics-device-type/operation/open type)
	   (lambda (descriptor)
	     (and descriptor
		  (%make-graphics-device type descriptor)))
	   arguments)))

(let-syntax
    ((define-graphics-operation
      (sc-macro-transformer
       (lambda (form environment)
	 (let ((name (cadr form)))
	   `(DEFINE-INTEGRABLE
	      (,(symbol-append 'GRAPHICS-DEVICE/OPERATION/ name) DEVICE)
	      (,(close-syntax (symbol-append 'GRAPHICS-DEVICE-TYPE/OPERATION/
					     name)
			      environment)
	       (GRAPHICS-DEVICE/TYPE DEVICE))))))))
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
		device
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
  ((graphics-device/operation/close device) device))

(define-integrable (graphics-flush device)
  ((graphics-device/operation/flush device) device))

(define (graphics-device-coordinate-limits device)
  ((graphics-device/operation/device-coordinate-limits device) device))

(define (graphics-coordinate-limits device)
  ((graphics-device/operation/coordinate-limits device) device))

(define (graphics-set-coordinate-limits device x-left y-bottom x-right y-top)
  ((graphics-device/operation/set-coordinate-limits device)
   device x-left y-bottom x-right y-top))

(define (graphics-set-clip-rectangle device x-left y-bottom x-right y-top)
  ((graphics-device/operation/set-clip-rectangle device)
   device x-left y-bottom x-right y-top))

(define (graphics-reset-clip-rectangle device)
  ((graphics-device/operation/reset-clip-rectangle device) device))

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
   device drawing-mode)
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
  ((graphics-device/operation/set-line-style device) device line-style)
  (set-graphics-device/line-style! device line-style))

(define (graphics-clear device)
  ((graphics-device/operation/clear device) device)
  (maybe-flush device))

(define (graphics-draw-point device x y)
  ((graphics-device/operation/draw-point device) device x y)
  (maybe-flush device))

(define (graphics-erase-point device x y)
  (graphics-bind-drawing-mode device drawing-mode:erase
    (lambda ()
      (graphics-draw-point device x y))))

(define (graphics-draw-text device x y text)
  ((graphics-device/operation/draw-text device) device x y text)
  (maybe-flush device))

(define (graphics-draw-line device x-start y-start x-end y-end)
  ((graphics-device/operation/draw-line device)
   device x-start y-start x-end y-end)
  (maybe-flush device))

(define (graphics-move-cursor device x y)
  ((graphics-device/operation/move-cursor device) device x y))

(define (graphics-drag-cursor device x y)
  ((graphics-device/operation/drag-cursor device) device x y)
  (maybe-flush device))

;;;; Images
;;; rectangular images that can be copied from and into the graphics
;;; device

(define-structure (image-device-type
		   (conc-name image-type/)
		   (constructor %make-image-type)
		   (predicate image-type?))
  (operation/create  false read-only true)
  (operation/destroy false read-only true)
  (operation/width   false read-only true)
  (operation/height  false read-only true)
  (operation/draw    false read-only true)
  (operation/draw-subimage    false read-only true)
  (operation/fill-from-byte-vector  false read-only true))

(define (image-type #!optional object error?)
  (let ((object (if (default-object? object) #f object))
	(error? (if (default-object? error?) #t error?)))
    (if (image-type? object)
	object
	(let ((type (graphics-type object error?)))
	  (and type
	       (or (1d-table/get (graphics-type-properties type)
				 'IMAGE-TYPE
				 #f)
		   (and error?
			(error "Graphics type has no associated image type:"
			       type))))))))

(define (make-image-type operations)
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
      (let ((create   (operation 'create))
	    (destroy  (operation 'destroy))
	    (width    (operation 'width))
	    (height   (operation 'height))
	    (draw     (operation 'draw))
	    (draw-subimage (operation 'draw-subimage))
	    (fill-from-byte-vector (operation 'fill-from-byte-vector)))
	(if (not (null? operations))
	    (error "Extra image type operations: " operations)
	    (%make-image-type create destroy 
			      width height 
			      draw draw-subimage fill-from-byte-vector))))))

(define-structure (image (conc-name image/) (constructor %make-image))
  type
  descriptor)

(define the-destroyed-image-type #f)

(define (image/create device width height)
  ;; operation/create returns a descriptor
  (let ((type (image-type device)))
    (%make-image type
		 ((image-type/operation/create type) device width height))))

(define (image/destroy image)
  ((image-type/operation/destroy (image/type image)) image)
  (set-image/type! image the-destroyed-image-type)
  (set-image/descriptor! image #f))

(define (image/width image)
  ((image-type/operation/width (image/type image)) image))

(define (image/height image)
  ((image-type/operation/height (image/type image)) image))

(define (image/draw device x y image)
  ((image-type/operation/draw (image/type image)) device x y image))

(define (image/draw-subimage device x y image im-x im-y width height)
  ((image-type/operation/draw-subimage (image/type image))
   device x y image im-x im-y width height))

(define (image/fill-from-byte-vector image byte-vector)
  ((image-type/operation/fill-from-byte-vector (image/type image))
   image byte-vector))