;;;;; -*- Scheme -*-
;;;;; Simple objects for UITK
;;;; derived from simple.sc,v 1.1 1993/02/16 14:04:09 jmiller Exp $

;;;; Simplest drawing object

;;;This is meant to be the external interface.  We need to implement these 
;;;in some standard way.


(define (with-clipping! shape user-fn Clip-to-XRegion)
  (let ((screen-area (Used-Screen-Area shape))
	(window (Get-UITKWindow shape))
	(gc (Shape%.graphics-context shape)))
    (if (and window screen-area)
	(let ((clip (rectangle->xregion screen-area)))
	  (define (intersect! xregion)
	    (if xregion
		(XIntersectRegion! xregion clip clip)))
	  (if (not (null? Clip-to-XRegion))
	      (intersect! (car Clip-to-XRegion)))
	  (intersect! (clip-region shape))
	  (SetClipXRegion window gc clip)
	  (user-fn window gc (UITKRectangle.Offset screen-area))
	  (XDestroyRegion clip)))))

(define (shape-draw shape . Clip-to-XRegion)
  (ensure-graphics-context shape)
  (with-clipping! shape (Shape%.x-draw shape) Clip-to-XRegion))

(define (shape-erase-maybe shape . Clip-to-XRegion)
  (ensure-graphics-context shape)
  (let ((e (Shape%.x-erase shape)))
    (and e
	 (begin (with-clipping! shape e Clip-to-XRegion)
		#T))))

(define (shape-copy shape)
  (let ((new (make-shape
	      (Shape%.x-draw shape)
	      (Shape%.calculate-used-screen-area shape)
	      (shape%.point-within? shape)
	      (Shape%.rectangle-overlaps? shape)
	      (%desired-size shape)
	      (Shape%.color shape))))
    (set-clip-region! new (clip-region shape))
    (assign-geometry! new
		      (drawing-surface shape)
		      (copy-rectangle (used-screen-area shape)))
    new))

(define (shape-set-color! shape color)
  (set-Shape%.color! shape color)
  (set-Shape%.graphics-context! shape false))

(define (shape-draw-function shape)
  (Shape%.x-draw shape))
    
(define (shape-set-gc-function! shape function-number)
  (set-Shape%.gc-function! shape function-number)
  (set-Shape%.graphics-context! shape false))

(define (shape-set-erase-function! shape fcn)
  (set-Shape%.x-erase! shape fcn))

(define (shape-point-within? Shape Offset)
  (if (not (vector? Offset))
      (error "SHAPE-POINT-WITHIN?: Bad offset" Offset))
  (let ((screen-area (used-screen-area shape)))
    (and screen-area
	 ((Shape%.point-within? shape)
	  (UITKRectangle.Offset screen-area)
	  Offset))))

(define (shape-rectangle-overlaps? Shape Offset W H)
  (if (not (vector? Offset))
      (error "SHAPE-RECTANGLE-OVERLAPS?: Bad offset" Offset))
  (if (not (number? W))
      (error "SHAPE-RECTANGLE-OVERLAPS?: Bad width" W))
  (if (not (number? H))
      (error "SHAPE-RECTANGLE-OVERLAPS?: Bad height" H))
  (let ((screen-area (used-screen-area shape)))
    (and screen-area
	 ((Shape%.rectangle-overlaps? shape)
	  (UITKRectangle.Offset screen-area)
	  offset w h))))

(define (generate-graphics-context! shape)
  ;; (define-primitives (set-debug-flags! 2))
  (let ((window (Get-UITKWindow shape)))
    (if window
	(begin
	  (set-Shape%.graphics-context!
	   shape
	   (make-colored-graphics-context window (Shape%.color shape)))
	  (let ((Xdisplay (UITKWindow.XDisplay
			   (DrawingSurface.UITKWindow
			    (Drawing-Surface shape)))))
	    (XSetFunction Xdisplay
			  (Shape%.graphics-context shape)
			  (Shape%.gc-function shape)))
	  'OK)
	#F)))

(define (ensure-graphics-context shape)
  (if (not (Shape%.graphics-context shape))
      (let ((gc (generate-graphics-context! shape)))
	(or gc
	    (begin (debug-print 'error)
		   (error "cannot make graphics context"))))))

(define (shape-assign-screen-area! shape screen-area)
  (cond ((vector? screen-area)
	 (let ((old (used-screen-area shape))
	       (used ((Shape%.calculate-used-screen-area shape)
		      screen-area)))	; Calculate bounding box, etc.
	   (set-used-screen-area! shape used)
	   (set-assigned-screen-area! shape screen-area)
	   (geometry-change! shape old used)
	   used))
	((not screen-area)
	 ;; Screen-Area is #F to retract the area.
	 ;; Just inform any objects interested and don't draw
	 ;; anything.
	 (let ((old (used-screen-area shape)))
	   (if old
	       (begin
		 (set-used-screen-area! shape #F)
		 (set-assigned-screen-area! shape #F)
		 (geometry-change! shape old #F)
		 #F))))
	(error "SHAPE-ASSIGN-SCREEN-AREA!: Bad screen-area" screen-area)))

;; Use default ASSIGN-DRAWING-SURFACE!

(define (shape-assign-glue! me)
  ;; infinitely stretchable
  (let* ((size (get-desired-size me))
	 (my-width (size.width size))
	 (my-height (size.height size)))
    (set-%hglue! me (make-fill-glue my-width 1))
    (set-%vglue! me (make-fill-glue my-height 1))))
  
;; Shape Maker
(define (shape-maker x-drawing-routine calculate-used-screen-area
		     point-within? rectangle-overlaps? color-string)
  (make-Shape%
   (make-UIObjInternals 'invalid
			'invalid
			UIObj-set-context! ; Defaults
			shape-assign-screen-area!
			UIObj-assign-drawing-surface!
			shape-point-within?
			shape-rectangle-overlaps?
			UIObj-handle-event
			UIObj-get-desired-size
			UIObj-assigned-screen-area
			UIObj-used-screen-area
			UIObj-set-assigned-screen-area!
			UIObj-set-used-screen-area!
			shape-assign-glue!)
   x-drawing-routine
   calculate-used-screen-area
   color-string
   point-within?
   rectangle-overlaps?
   GXCOPY))

(define (make-shape x-drawing-routine calculate-used-screen-area
		    point-within? rectangle-overlaps?
		    desired-size color-string)
  (let ((me (shape-maker x-drawing-routine
			 calculate-used-screen-area
			 point-within?
			 rectangle-overlaps?
			 color-string)))
    (set-%desired-size! me desired-size)
    me))

(define (make-rect width height color-string . filled?)
  ;;defaults to filled
  (let* ((width (round->exact width))
	 (height (round->exact height))
	 (Width-1 (- width 1))
	 (Height-1 (- height 1))
	 (Xdraw (if (or (null? filled?) (car filled?))
		    (lambda (uitkw gc x y)
		      (FillRectangle uitkw gc x y width height))
		    (lambda (uitkw gc x y)
		      ;;!!@%$^$^$$#@#@#!! X!!
		      (DrawRectangle uitkw gc x y Width-1 Height-1)))))
    (make-shape
     (lambda (UITKWindow graphics-context offset) ; Draw
       (XDraw UITKWindow graphics-context
	      (Point.X offset) (Point.Y offset)))
     (lambda (assigned-screen-area)	; Calculate-Used-Screen-Area
       (make-UITKRectangle (UITKRectangle.offset assigned-screen-area)
			   (make-size width height)))
     (lambda (My-Offset Offset)		; Point within?
       (point-in-rectangle? Offset My-Offset Width Height))
     (lambda (My-Offset Offset W H)	; Rectangle overlaps?
       (rectangle-overlaps-rectangle?
	My-Offset Width Height Offset W H))
     (make-size width height)		; Desired-Size
     color-string)))

(define (make-scaling-rect color-string . filled?)
  ;;defaults to filled
  (let* ((Width #F)
	 (Height #F)
	 (Width-1 #F)
	 (Height-1 #F)
	 (Xdraw (if (or (null? filled?) (car filled?))
		    (lambda (uitkw gc x y)
		      (FillRectangle uitkw gc x y width height))
		    (lambda (uitkw gc x y)
		      ;;!!@%$^$^$$#@#@#!! X!!
		      (DrawRectangle uitkw gc x y Width-1 Height-1)))))
    (make-shape
     (lambda (UITKWindow graphics-context offset) ; Draw
       (XDraw UITKWindow graphics-context
	      (Point.X offset) (Point.Y offset)))
     (lambda (assigned-screen-area)	; Calculate-Used-Screen-Area
       (set! height (UITKRectangle.Height assigned-screen-area))
       (set! width (UITKRectangle.Width assigned-screen-area))
       (set! height-1 (- height 1))
       (set! width-1 (- width 1))
       assigned-screen-area)
     (lambda (My-Offset Offset)		; Point within?
       (point-in-rectangle? Offset My-Offset Width Height))
     (lambda (My-Offset Offset W H)	; Rectangle overlaps?
       (rectangle-overlaps-rectangle?
	My-Offset Width Height Offset W H))
     (make-size width height)		; Desired-Size
     color-string)))

(define (make-filled-rectangle width height color-string)
  (make-rect width height color-string #T))

(define (make-unfilled-rectangle width height color-string)
  (make-rect width height color-string #F))

(define (make-oval width height color-string . filled?)
  (let* ((width (round->exact width))
	 (height (round->exact height))
	 (Width-1 (- width 1))
	 (Height-1 (- height 1))
	 (angle (* 360 64))		; X uses degrees/64
	 (Xdraw (if (or (null? filled?) (car filled?))
		    (lambda (uitkw gc x y)
		      (FillArc uitkw gc x y width height 0 angle))
		    (lambda (uitkw gc x y)
		      (DrawArc uitkw gc x y width-1 height-1 0 angle))))
	 (a (/ width 2.0))
	 (b (/ height 2.0))
	 (center (make-point a b)))
    (make-shape
     (lambda (UITKWindow graphics-context offset) ; Draw
       (Xdraw UITKWindow graphics-context
	      (Point.X offset) (Point.Y offset)))
     (lambda (assigned-screen-area)	; Calculate-Used-Screen-Area
       (make-UITKRectangle (UITKRectangle.Offset assigned-screen-area)
			   (make-size width height)))
     (lambda (My-Offset Offset)		; Point within?
       (and (point-in-rectangle? Offset My-Offset width height)
	    (let* ((dv (sub-vectors (add-vectors my-offset center) offset))
		   (dx/a (/ (point.x dv) a))
		   (dy/b (/ (point.y dv) b)))
	      (< (+ (* dx/a dx/a) (* dy/b dy/b)) 1.0))))
     (lambda (My-Offset Offset W H)	; Rectangle overlaps?
       (rectangle-overlaps-rectangle?
	My-Offset diameter diameter Offset W H))
     (make-size width height)		; Desired-Size
     color-string)))

(define (make-scaling-oval color-string . filled?)
  (let ((width #F)
	(height #F)
	(width-1 #F)
	(height-1 #F)
	(a #F)
	(b #F)
	(center #F))
    (let* ((angle (* 360 64))		; X uses degrees/64
	   (Xdraw (if (or (null? filled?) (car filled?))
		      (lambda (uitkw gc x y)
			(FillArc uitkw gc x y width height 0 angle))
		      (lambda (uitkw gc x y)
			(DrawArc uitkw gc x y width-1 height-1 0 angle)))))
      (make-shape
       (lambda (UITKWindow graphics-context offset) ; Draw
	 (Xdraw UITKWindow graphics-context
		(Point.X offset) (Point.Y offset)))
       (lambda (assigned-screen-area)	; Calculate-Used-Screen-Area
	 (set! width (UITKRectangle.Width assigned-screen-area))
	 (set! height (UITKRectangle.Height assigned-screen-area))
	 (set! width-1 (- width 1))
	 (set! height-1 (- height 1))
	 (set! a (/ width 2.0))
	 (set! b (/ height 2.0))
	 (set! center (make-point a b))
	 assigned-screen-area)
       (lambda (My-Offset Offset)	; Point within?
	 (and (point-in-rectangle? Offset My-Offset width height)
	      (let* ((dv (sub-vectors (add-vectors my-offset center) offset))
		     (dx/a (/ (point.x dv) a))
		     (dy/b (/ (point.y dv) b)))
		(< (+ (* dx/a dx/a) (* dy/b dy/b)) 1.0))))
       (lambda (My-Offset Offset W H)	; Rectangle overlaps?
	 (rectangle-overlaps-rectangle?
	  My-Offset width height Offset W H))
       (make-size width height)		; Desired-Size
       color-string))))

(define (make-filled-oval width height color-string)
  (make-oval width height color-string #T))

(define (make-unfilled-oval width height color-string)
  (make-oval width height color-string #F))

(define (make-filled-circle radius color-string)
  (make-filled-oval (* radius 2) (* radius 2) color-string))

(define (make-unfilled-circle radius color-string)
  (make-unfilled-oval (* radius 2) (* radius 2) color-string))

;;;not right?  do lines need to be oriented (from to)

(define (make-line width height color-string . filled?)
  filled?				;ignore
  (let ((width (round->exact width))
	(height (round->exact height)))
    (make-shape
     (lambda (UITKWindow graphics-context offset) ; Draw
       (DrawLine UITKWindow graphics-context
	      (Point.X offset) (Point.Y offset)
	      (+ width (Point.X offset)) (+ height (Point.Y offset))))
     (lambda (assigned-screen-area)	; Calculate-Used-Screen-Area
       (make-UITKRectangle (UITKRectangle.offset assigned-screen-area)
			   (make-size width height)))
     (lambda (My-Offset Offset)		; Point within?!!FIX
       (point-in-rectangle? Offset My-Offset Width Height))
     (lambda (My-Offset Offset W H)	; Rectangle overlaps?!!FIX
       (rectangle-overlaps-rectangle?
	My-Offset Width Height Offset W H))
     (make-size width height)		; Desired-Size
     color-string)))


;;;*******Still not right for rubber-banding (I think)

(define (make-scaling-line color-string . filled?)
  filled?				;ignore
  (let ((width #F)
	(height #F))
    (make-shape
     (lambda (UITKWindow graphics-context offset) ; Draw
       (DrawLine UITKWindow graphics-context
		 (Point.X offset) (Point.Y offset)
		 (+ width (Point.X offset)) (+ height (Point.Y offset))))
     (lambda (assigned-screen-area)	; Calculate-Used-Screen-Area
       (set! height (UITKRectangle.Height assigned-screen-area))
       (set! width (UITKRectangle.Width assigned-screen-area))
       assigned-screen-area)
    (lambda (My-Offset Offset)		; Point within?!!FIX
      (point-in-rectangle? Offset My-Offset Width Height))
    (lambda (My-Offset Offset W H)	; Rectangle overlaps?!!FIX
      (rectangle-overlaps-rectangle?
       My-Offset Width Height Offset W H))
    (make-size width height)		; Desired-Size
    color-string)))



#|
;;; points is a list of points.  The offset of a path is the upper left-most
;;; corner of the bounding box of the path.  Make-path comes born with
;;; a used screen area
(define (make-path points color)
  (if (null? points)
      (error "no points in path -- MAKE-PATH"))
  (let* ((xcors (map point.x points))
	 (ycors (map point.y points))
	 (xmin (apply min xcors))
	 (xmax (apply max xcors))
	 (ymin (apply min ycors))
	 (ymax (apply max ycors))
	 (width (- xmax xmin))
	 (height (- ymax ymin))
	 ;;adjust points so relative to upper-left
	 (xpoints (map (lambda (x) (- x xmin)) xcors))
	 (ypoints (map (lambda (y) (- y ymin)) ycors))
	 (shape
	  (make-shape
	   (lambda (UITKWindow graphics-context offset)
	     (let* ((ox (point.x offset))
		    (oy (point.y offset))
		    (xs (map (lambda (x) (+ x ox)) xpoints))
		    (ys (map (lambda (y) (+ y oy)) ypoints)))
	     (let loop ((fx (car xs))
			(fy (car ys))
			(restx (cdr xs))
			(resty (cdr ys)))
	       (if (null? restx)
		   'done
		   (begin (DrawLine UITKWindow graphics-context
				    fx fy (car restx) (car resty))
			  (loop (car restx) (car resty)
				(cdr restx) (cdr resty)))))))
	   (lambda (assigned-screen-area)
	     ??????)
	   (lambda (point) ())
	   (lambda (rect) ())
	   desired-size
	   color)))
    set up the used screen-area???
    shape))

FOO do just a line first
    

or maybe just do draw path and draw line given some shape for which to
use the uitkwindow.


|#


(define (self-paint! shape)
  (handle-exposure shape
   (lambda (exposed-rectangle)
     (shape-draw shape (Rectangle->XRegion exposed-rectangle))))
  (on-geometry-change! shape 'REASON
   (lambda (old-screen-area new-screen-area)
     old-screen-area new-screen-area		; Not used
     (if new-screen-area (shape-draw shape))))
  'done)

(define (make-self-painting-rectangle width height color)
  (let ((me (make-rect width height color)))
    (self-paint! me)
    me))

(define (make-self-painting-unfilled-rectangle width height color)
  (let ((me (make-unfilled-rectangle width height color)))
    (self-paint! me)
    me))

(define (make-self-painting-circle radius color)
  (let ((me (make-circle radius color)))
    (self-paint! me)
    me))


;;; This is a surface that generates rectangles or ovals when you click on it.
;;; Shapes grow with rubber banding.
;;; After a shape is generated, it can be moved around.
;;; You can choose either solid or outline mode for moving the shapes.
;;;This is a little ugly, because I want to make sure that
;;;make-rect is never called with negative width or height.

(define (make-shape-surface width height background-color new-shape-color)
  (let ((rubber-from #F)
	(move-offset #F)		;offset of moving shape relative to mouse
	(shadow-shape #F)
	(fill-shadow-shapes? #F)
	(shape-maker make-rect)
	(rubber-shape-maker make-scaling-rect)
	(shapes-and-areas '())
	(shape->maker-map '())
	(outer-rectangle (make-rect width height background-color)))

    (define (shape->shape-and-area shape)
      (cons shape (used-screen-area shape)))
    (define (shape-and-area->shape s-and-a) (car s-and-a))
    (define (shape-and-area->area s-and-a) (cdr s-and-a))
    (define (set-shape-and-area-area! s-and-a area)
      (let ((shape (shape-and-area->shape s-and-a)))
	(assign-screen-area! shape area)
	(set-cdr! s-and-a (used-screen-area shape))))

    (define (process-click-on-shape shape-and-area e while-grabbed)
      (let ((shape (shape-and-area->shape shape-and-area))
	    (area (shape-and-area->area shape-and-area)))
	(set! move-offset
	      (sub-vectors (Event.Offset e)
			   (UITKRectangle.offset area)))
      ;;;put shape at top
	(set! shapes-and-areas
	      (append (delq! shape-and-area shapes-and-areas)
		      (list shape-and-area)))
	(shape-draw shape)
	(set! shadow-shape (shape->moving-shape shape))
	(shape-erase shape)		;because shadow draws in XOR
	(shape-draw shadow-shape)
	(while-grabbed
	 (lambda (point)		; Motion procedure
	   (if (point-within? outer-rectangle point)
	       (begin
		 (shape-erase shadow-shape)
		 (assign-location! shadow-shape
				   (sub-vectors point move-offset))
		 (shape-draw shadow-shape))))
	 (lambda ()			;finalization procedure
	   (set-shape-and-area-area! shape-and-area
				     (used-screen-area shadow-shape))
	   (shape-erase shadow-shape)
	   (shape-draw shape)))))

    (define (process-click-on-background e while-grabbed)
      (set! rubber-from (Event.Offset e))
      (set! shadow-shape (make-rubber-shape))
      (shape-draw shadow-shape)
      (while-grabbed
       (lambda (point)			; Motion procedure
	 (if (point-within? outer-rectangle point)
	     (grow-shadow-shape point)))
       instantiate-shadow-shape))	;finalization procedure

    (define (instantiate-shadow-shape)
      (let ((new-shape (screen-area->shape
			shape-maker
			(used-screen-area shadow-shape)
			new-shape-color
			#T)))		;always fill
	(shape-erase shadow-shape)
	(shape-draw new-shape)
	(set! shapes-and-areas
	      (append shapes-and-areas
		      (list (shape->shape-and-area new-shape))))
	;;remember proc that shape was made with
	(set! shape->maker-map
	      (cons (cons new-shape shape-maker)
		    shape->maker-map))))

    (define (shape->moving-shape shape)
      (let ((entry (assq shape shape->maker-map)))
	(if (null? entry)
	    (error "shape not in shape->maker-map" shape))
	(let ((moving-shape
	       (screen-area->shape (cdr entry)
				   (used-screen-area shape)
				   background-color
				   fill-shadow-shapes?)))
	  (set-xor-draw! moving-shape)
	  moving-shape)))

    (define (screen-area->shape maker screen-area color fill?)
      (let ((new-shape (maker (UITKRectangle.Width screen-area)
			      (UITKRectangle.Height screen-area)
			      color
			      fill?)))
	(set-clip-region! new-shape
			  (rectangle->XRegion 
			   (used-screen-area outer-rectangle)))
	(assign-geometry! new-shape
			  (drawing-surface outer-rectangle)
			  (copy-rectangle screen-area))
	new-shape))

    (define (make-rubber-shape)
      (let ((new-shape
	     (rubber-shape-maker new-shape-color fill-shadow-shapes?)))
	(set-clip-region! new-shape
			  (rectangle->XRegion 
			   (used-screen-area outer-rectangle)))
	(set-shape-geometry! new-shape rubber-from rubber-from)
	(set-xor-draw! new-shape)
	new-shape))

    (define (set-shape-geometry! shape from to)
      (let ((x1 (point.x from))
	    (y1 (point.y from))
	    (x2 (point.x to))
	    (y2 (point.y to)))
	(let* ((dx (- x2 x1))
	       (dy (- y2 y1))
	       (x-left (if (>= dx 0) x1 x2))
	       (y-top (if (>= dy 0) y1 y2)))
	  (assign-geometry! shape
			    (drawing-surface outer-rectangle)
			    (make-UITKRectangle (make-point x-left y-top)
						(make-size (abs dx) (abs dy))))
	  'DONE)))

    (define (grow-shadow-shape to)
      (shape-erase shadow-shape)
      (set-shape-geometry! shadow-shape rubber-from to)
      (shape-draw shadow-shape))

    (define (find-shape-under-event event)
      (let loop ((more-shapes (reverse shapes-and-areas)))
	(cond ((null? more-shapes) #F)
	      ((event-within?
		(shape-and-area->shape (car more-shapes)) event)
	       (car more-shapes))
	      (else (loop (cdr more-shapes))))))

    (define (shape-erase shape)
      (or (shape-erase-maybe shape)
	  (redraw-surface-except
	   shape
	   (rectangle->Xregion (used-screen-area shape)))))

    (define (set-xor-draw! shape)
      (shape-set-gc-function! shape GXXOR)
      (shape-set-erase-function! shape (shape-draw-function shape))
      (shape-set-color! shape background-color))

    (define (redraw-surface-except shape clip-region)
	  (shape-draw outer-rectangle clip-region)
	  (for-each
	   (lambda (shape) (shape-draw shape clip-region))
	   (map shape-and-area->shape
		(list-transform-negative shapes-and-areas
		  (lambda (shape-and-area)
		    (eq? shape (shape-and-area->shape shape-and-area)))))))

    (define (redraw-surface clip-region)
      (shape-draw outer-rectangle clip-region)
      (for-each (lambda (shape) (shape-draw shape clip-region))
		(map shape-and-area->shape shapes-and-areas)))

    (handle-exposure outer-rectangle
		     (lambda (exposed-rectangle)
		       (redraw-surface (Rectangle->XRegion exposed-rectangle))))

    (handle-button-grab
     outer-rectangle ANYBUTTON
     (lambda (e while-grabbed)		; When the button goes down
       (let ((shape (find-shape-under-event e)))
	 (cond (shape (process-click-on-shape shape e while-grabbed))
	       ((event-within? outer-rectangle e)
		(process-click-on-background e while-grabbed))	     
	       (else
		(while-grabbed
		 (lambda (point) point 'OK) ; Nothing to do
		 (lambda () 'OK)))))))

    (on-geometry-change!
     outer-rectangle 'ignore
     (lambda (old-screen-area new-screen-area)
       (if (not (screen-area= old-screen-area new-screen-area))
	   (if (UITKRectangle? new-screen-area)
	       (let ((Clip (rectangle->XRegion new-screen-area)))
		 (for-each
		  (lambda (shape-and-area)
		    (let ((shape (shape-and-area->shape shape-and-area))
			  (area  (shape-and-area->area shape-and-area)))
		      (set-clip-region! shape clip)
		      (set-shape-and-area-area!
		       shape-and-area
		       (make-UITKRectangle
			(UITKRectangle.offset area)
			(UITKRectangle.Size new-screen-area)))))
		  shapes-and-areas)
		 (redraw-surface clip))
	       (for-each (lambda (shape-and-area)
			   (let ((shape
				  (shape-and-area->shape shape-and-area)))
			     (set-clip-region! shape #F)
			     (assign-screen-area! shape #F)))
			 shapes-and-areas)))))
    (lambda (message)
      (case message
	((the-surface) outer-rectangle)
	((set-color!)
	 (lambda (string)
	   (set! new-shape-color string)))
	((rectangles)
	 (set! rubber-shape-maker make-scaling-rect)
	 (set! shape-maker make-rect))
	((ovals)
	 (set! rubber-shape-maker make-scaling-oval)
	 (set! shape-maker make-oval))
	((lines)
	 (set! rubber-shape-maker make-scaling-line)
	 (set! shape-maker make-line))
	((outlined) (set! fill-shadow-shapes? #F))
	((filled) (set! fill-shadow-shapes? #T))
	((clear)
	 (begin (set! shapes-and-areas '())
		(set! shape->maker-map '())
		(redraw-surface
		 (rectangle->XRegion
		  (used-screen-area outer-rectangle)))))
	(else (error "unknown message"))))))




;;;;******This isn't working.  I don't understand how to manage the 
;;;;event queue

(define debug-surface)

(define (make-drop-rubber-rectangle-surface app width height color shape-color)
  (let ((rubber-from #F)		; X,Y of click relative to dragging shape
	(rubber-to #F)
	(active-shape #F)
	(shadow-shape #F)
	(fill-rubber-shapes? #F)
	(shape-maker make-rect)
	(shadow-shape-maker make-scaling-rect)
	(shapes '())
	(outer-rectangle (make-rect width height color)))
    (set! debug-surface (lambda () 'foo))
    (define (pick-random-shape)
      (let ((l (length shapes)))
	(list-ref shapes (random l))))
    (define (drop)
      (if (not (null? shapes))
	  (let* ((s (pick-random-shape)))
	      (redraw-surface-except s (rectangle->Xregion (used-screen-area s)))
	      (let* ((p (UITKRectangle.offset (used-screen-area s)))
		   (x (point.x p))
		   (y (point.y p)))
		(if (> y (+ (point.y (UITKRectangle.offset
				      (used-screen-area outer-rectangle)))
			  height))
		    (begin (set! shapes (delq! s shapes))
			 'OK)
		    (begin (assign-location! s (make-point x (+ y 5)))
			   (shape-draw s))))
	      (when-idle! app drop))))
    (define (find-shape event)
      (let loop ((more-shapes (reverse shapes)))
	(cond ((null? more-shapes) #F)
	      ((event-within? (car more-shapes) event)
	       (let ((this (car more-shapes)))
		 (set! shapes (append (delq! this shapes) (list this)))
		 this))
	      (else (loop (cdr more-shapes))))))
    (define (process-click-on-active-shape e while-grabbed)
      (let ((Click (Event.Offset e))
	    (screen-area (used-screen-area active-shape)))
	(set! rubber-from (sub-vectors Click
				       (UITKRectangle.offset screen-area)))
	;;bring active shape to top
	(shape-draw active-shape)
	(while-grabbed
	 (lambda (point)		; Motion procedure ...
	   (if (point-within? outer-rectangle point) ; maybe
	       (move-active-shape (sub-vectors point rubber-from))))
	 (lambda () (set! active-shape #F)) ;finalize
	 )))
    (define (process-click-on-background e while-grabbed)
      (let ((Click (Event.Offset e)))
	(initialize-rubber-rectangle Click)
	(while-grabbed
	 (lambda (point)		; Motion procedure ...
	   (if (point-within? outer-rectangle point) ; maybe
	       (grow-rubber-rectangle point)))
	 instantiate-rubber-rectangle))) ;finalize

    (define (instantiate-rubber-rectangle)
      (let* ((used (used-screen-area shadow-shape))
	     (new-shape (shape-maker (UITKRectangle.Width used)
				     (UITKRectangle.Height used)
				     new-shape-color)))
	(shape-erase shadow-shape)
	(set! shadow-shape #F)
	(set-clip-region! new-shape
			  (rectangle->XRegion 
			   (used-screen-area outer-rectangle)))
	(set-shape-geometry! new-shape)
	(shape-draw new-shape)
	(set! shapes (append shapes (list new-shape)))
	(when-idle! app drop)))

    (define (initialize-rubber-rectangle Click)
      (set! rubber-from Click)
      (make-rubber-rectangle Click))
	
    (define (make-rubber-rectangle to)
      (set! rubber-to to)
      (set! shadow-shape (shadow-shape-maker "white" fill-rubber-shapes?))
      (set-clip-region! shadow-shape
			(rectangle->XRegion 
			 (used-screen-area outer-rectangle)))
      (set-shape-geometry! shadow-shape)
      (shape-set-gc-function! shadow-shape GXXOR)
      (shape-set-erase-function! shadow-shape
				 (shape-draw-function shadow-shape))
      (shape-draw shadow-shape))

    (define (set-shape-geometry! shape)
      (let ((x1 (point.x rubber-from))
	    (y1 (point.y rubber-from))
	    (x2 (point.x rubber-to))
	    (y2 (point.y rubber-to)))
	(let* ((dx (- x2 x1))
	       (dy (- y2 y1))
	       (x-left (if (>= dx 0) x1 x2))
	       (y-top (if (>= dy 0) y1 y2)))
	  (assign-geometry! shape
			    (drawing-surface outer-rectangle)
			    (make-UITKRectangle (make-point x-left y-top)
						(make-size (abs dx) (abs dy))))
	  'DONE)))

    (define (grow-rubber-rectangle point)
      (set! rubber-to point)
      (shape-erase shadow-shape)	; Can't fail!
      (set-shape-geometry! shadow-shape)
      (shape-draw shadow-shape))

    (define (redraw-surface-except shape clip-region)
      (shape-draw outer-rectangle clip-region)
      (for-each (lambda (shape) (shape-draw shape clip-region))
		(delq shape shapes)))
    (define (redraw-surface clip-region)
      (shape-draw outer-rectangle clip-region)
      (for-each (lambda (shape) (shape-draw shape clip-region))
		shapes))
    (define (move-active-shape to-point)
      (let ((screen-area (used-screen-area active-shape)))
	(redraw-surface-except active-shape
			       (rectangle->XRegion screen-area))
	(assign-location! active-shape to-point)
	(shape-draw active-shape)))

    (handle-exposure outer-rectangle
		     (lambda (exposed-rectangle)
		       (redraw-surface (Rectangle->XRegion exposed-rectangle))))

    (handle-button-grab
     outer-rectangle ANYBUTTON
     (lambda (e while-grabbed)		; When the button goes down
       (set! active-shape (find-shape e))
       (cond (active-shape (process-click-on-active-shape e while-grabbed))
	     ((event-within? outer-rectangle e)
	      (process-click-on-background e while-grabbed))	     
	     (else
	      (while-grabbed
	       (lambda (point) point 'OK) ; Nothing to do
	       (lambda () (set! active-shape #F)))))))

    (on-geometry-change!
     outer-rectangle 'ignore
     (lambda (old-screen-area new-screen-area)
       old-screen-area
       (let ((ds (drawing-surface outer-rectangle)))
	 (if (UITKRectangle? new-screen-area)
	     (let ((Clip (rectangle->XRegion new-screen-area)))
	       (for-each
		(lambda (shape)
		  (set-clip-region! shape clip)
		  (assign-geometry! shape ds new-screen-area))
		shapes)
	       (redraw-surface clip))
	     (for-each (lambda (shape)
			 (set-clip-region! shape #F)
			 (assign-screen-area! shape #F))
		       shapes)))))
    (when-idle! app drop)
    (lambda (message)
      (case message
	((the-surface) outer-rectangle)
	((set-color!)
	 (lambda (new-shape-color-name)
	   (let* ((dsp (UITKWindow.XDisplay
			(DrawingSurface.UITKWindow
			 (Drawing-Surface outer-rectangle))))
		  (color ((string->color dsp) new-shape-color-name)))
	     (if color
		 (begin
		   (set! new-shape-color new-color-name)
		   'OK)
		 #F))))
	((rectangles)
	 (set! shadow-shape-maker make-scaling-rect)
	 (set! shape-maker make-rect))
	((ovals)
	 (set! shadow-shape-maker make-scaling-oval)
	 (set! shape-maker make-oval))
	((outlined) (set! fill-rubber-shapes? #F))
	((filled) (set! fill-rubber-shapes? #T))
	((clear)
	 (begin (set! shapes '())
		(redraw-surface
		 (rectangle->XRegion
		  (used-screen-area outer-rectangle)))))
	(else (error "unknown message"))))))
