;;; -*- Scheme -*-

(declare (usual-integrations))

;;;; This is a very simple application written in Scheme, withous any
;;;; TK wigets.  It is a surface on which you cna sketch a path with
;;;; the mouse.

;;;Some geometric shape drawers:
;;;This might be generally useful -- it saves the higher level from
;;;worrying about the graphics context.

;;;this doesn't handle clipping.  should we create a line-drawer with a
;;;specified clip region??

(define (geometry-drawer-for-shape shape color drawproc)
  ;;also set line style, thickness, etc.
  (let* ((window #F)
	 (gc #F))
    (lambda args
      ;;need to init the window and GC and keep it up to date if the
      ;;window changes
      ;;is there some way to used cached info for the shape to speed
      ;;up these tests?
      (if (not (used-screen-area shape))
	  (error "attempt to draw on background with no screen area"
		 shape))
      (if (not (eq? window (get-uitkwindow shape)))
	  (begin (set! window (get-uitkwindow shape))
		 (set! gc (make-colored-graphics-context window color))))
      (apply drawproc (cons window (cons gc args))))))

(define (line-drawer-for-shape shape color)
  (geometry-drawer-for-shape
   shape color
   (lambda (window gc p1 p2)
     (drawline window gc 
	       (point.X p1) (point.Y p1)
	       (point.X p2) (point.Y p2)))))

(define (arc-drawer-for-shape shape color)
  (geometry-drawer-for-shape
   shape color
   (lambda (window gc p w h a1 a2)
     (drawarc window gc (point.X p) (point.Y p) w h a1 a2))))

(define (rectangle-drawer-for-shape shape color)
  (geometry-drawer-for-shape
   shape color
   (lambda (window gc p w h)
     (drawrectangle window gc (point.X p) (point.Y p) w h))))



(define (setup-doodle)
  (let ((d (make-doodle-surface 400 400 "light gray")))
    (add-child! (make-application "doodle") d)
    d))

(define (make-doodle-surface width height background-color)
  (let* ((line-color "black")
	 (background (make-rect width height background-color))
	 (draw-line (line-drawer-for-shape background line-color))
	 (path '()))
    (define (collect-points first-event while-grabbed)
      (shape-draw background)		;clear and erase path
      (set! path (list (event.offset first-event)))
      (while-grabbed
       (lambda (point)
	 (if (point-within? background point)
	     (begin (draw-line (car path) point)
		    (set! path (cons point path)))))
       (lambda () 'done)))		;nothing to do at end of grab
    (define (draw-path)
      (if (not (null? path))
	  (let loop ((path path))
	    (if (null? (cdr path))
		'done
		(begin (draw-line (car path) (cadr path))
		       (loop (cdr path)))))))
    (handle-exposure
     background
     (lambda (exposed-rectangle)
       (shape-draw background (rectangle->XRegion exposed-rectangle))
       ;;should clip to exposed rectangle.  What's a nice way to do this?
       (draw-path)))
    (handle-button-grab
     background ANYBUTTON
     (lambda (e while-grabbed)
       (collect-points e while-grabbed)))
    (on-geometry-change!
     background
     'ignore
     (lambda (old-screen-area new-screen-area)
       old-screen-area
       (if (UITKRectangle? new-screen-area)
	   (shape-draw background))))
    background))





