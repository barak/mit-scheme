#| -*-Scheme-*-

$Id: picture.scm,v 1.22 1995/02/21 23:22:24 cph Exp $

Copyright (c) 1991-95 Massachusetts Institute of Technology

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

;;;; 6.001 Images

(declare (usual-integrations))

;;;; Miscellaneous Utilities

(define-primitives
  floating-vector-ref
  floating-vector-set!
  floating-vector-cons
  floating-vector-length)

(define (make-floating-vector length init)
  (let ((result (floating-vector-cons length)))
    (if (not (= init 0.))
	(do ((i 0 (fix:+ i 1)))
	    ((fix:= i length))
	  (floating-vector-set! result i init)))
    result))

(define (floating-vector-copy vector)
  (let* ((length (floating-vector-length vector))
	 (result (floating-vector-cons length)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i length))
      (floating-vector-set! result i (floating-vector-ref vector i)))
    result))

(define (side-effecting-iter n proc)
  (define (reverse-order-iter count)
    (if (fix:= count n)
	'done
	(begin
	  (proc count)
	  (reverse-order-iter (fix:+ 1 count)))))
  (reverse-order-iter 0))

(define (lo-bound interval-length)
  (fix:- 1 (quotient (fix:+ 1 interval-length) 2)))

(define (up-bound interval-length)
  (floor->exact (1+ (/ interval-length 2))))

(define (floating-vector->list vector)
  (generate-list (floating-vector-length vector)
    (lambda (i)
      (floating-vector-ref vector i))))

(define (generate-list n proc) ; ==> ( (proc 0) (proc 1) ... (proc n-1) )
  (let loop ((i (- n 1)) (list '()))
    (if (< i 0)
        list
        (loop (- i 1) (cons (proc i) list)))))

;;;; Graphics Windows

(define (make-window width height x y)
  (let ((window
	 (let ((name (graphics-type-name (graphics-type #f))))
	   (case name
	     ((X) (make-window/X11 width height x y))
	     ((WIN32) (make-window/win32 width height x y))
	     ((OS/2) (make-window/OS2 width height x y))
	     (else (error "Unsupported graphics type:" name))))))
    (graphics-set-coordinate-limits window 0 (- (- height 1)) (- width 1) 0)
    (restore-focus-to-editor)
    window))

(define (make-window/X11 width height x y)
  (let ((window
	 (make-graphics-device 'X
			       false
			       (x-geometry-string x y width height)
			       true)))
    ;; Prevent this window from receiving the keyboard focus.
    (x-graphics/disable-keyboard-focus window)
    ;; Inform the window manager that this window does not do any
    ;; keyboard input.
    (x-graphics/set-input-hint window false)
    ;; OK, now map the window onto the screen.
    (x-graphics/map-window window)
    (x-graphics/flush window)
    window))

(define (make-window/win32 width height x y)
  (let ((window (make-graphics-device 'WIN32 width height 'GRAYSCALE-128)))
    (graphics-operation window 'MOVE-WINDOW x y)
    window))

(define (make-window/OS2 width height x y)
  (let ((window (make-graphics-device 'OS/2 width height)))
    ;; X, Y specify the position of the upper-left corner of the
    ;; window, in coordinates relative to the upper-left corner of the
    ;; display with Y growing down; the OS/2 SET-WINDOW-POSITION
    ;; operation specifies the position of the lower-left corner of
    ;; the window, in coordinates relative to the lower left corner of
    ;; the display, with Y growing up.
    (call-with-values (lambda () (graphics-operation window 'DESKTOP-SIZE))
      (lambda (dx dy)
	dx
	(call-with-values
	    (lambda () (graphics-operation window 'WINDOW-FRAME-SIZE))
	  (lambda (fx fy)
	    fx
	    (graphics-operation window 'SET-WINDOW-POSITION
				x
				(- dy (+ y fy)))))))
    window))

(define (resize-window window width height)
  (let ((name (graphics-type-name (graphics-type window))))
    (case name
      ((X WIN32) (graphics-operation window 'RESIZE-WINDOW width height))
      ((OS/2) (graphics-operation window 'SET-WINDOW-SIZE width height))
      (else (error "Unsupported graphics type:" name)))))

(define (show-window-size window)
  (call-with-values (lambda () (graphics-device-coordinate-limits window))
    (lambda (x1 y1 x2 y2)
      (newline)
      (display `("width:" ,(+ (- x2 x1) 1) "  height:" ,(+ (- y1 y2) 1))))))

(define (n-gray-map window)
  (let ((name (graphics-type-name (graphics-type window))))
    (case name
      ((X) (n-gray-map/X11 window))
      ((WIN32 OS/2) (n-gray-map/win32 window))
      (else (error "Unsupported graphics type:" name)))))

(define (n-gray-map/X11 window)
  (let ((properties (x-display/properties (x-graphics/display window))))
    (or (1d-table/get properties '6001-GRAY-MAP #f)
	(let ((gm (allocate-grays window)))
	  (1d-table/put! properties '6001-GRAY-MAP gm)
	  gm))))

(define (allocate-grays window)
  (let ((w-cm (graphics-operation window 'get-colormap))
	(visual-info
	 ((ucode-primitive x-get-visual-info 10) (x-graphics-device/xw window)
						 #f #f #f #f #f #f #f #f #f)))
    (let ((find-info
	   (let ((length (vector-length visual-info)))
	     (if (= length 0)
		 (error "X-GET-VISUAL-INFO: no results"))
	     (lambda (class depth-min depth-max)
	       (let loop ((index 0))
		 (and (< index length)
		      (let ((info (vector-ref visual-info index)))
			(if (and (= class (vector-ref info 4))
				 ;; kludge, but X made us do it.
				 (<= depth-min (vector-ref info 8) depth-max))
			    info
			    (loop (+ index 1)))))))))
	  (make-gray-map
	   (lambda (n-levels)
	     (let ((gm (make-string n-levels))
		   (step (/ 65535 (- n-levels 1))))
	       (do ((index 0 (+ index 1)))
		   ((= index n-levels))
		 (vector-8b-set!
		  gm
		  index
		  (let ((intensity (round->exact (* step index))))
		    (x-colormap/allocate-color
		     w-cm
		     intensity intensity intensity))))
	       gm))))
      (cond ((find-info visual-class:static-gray 256 256)
	     (make-gray-map 256))
	    ((or (find-info visual-class:gray-scale 256 256)
		 (find-info visual-class:pseudo-color 250 256))
	     (make-gray-map 128))
	    ((find-info visual-class:static-gray 2 2)
	     (make-gray-map 2))
	    (else
	     (error "ALLOCATE-GRAYS: not known display type" window))))))

(define-integrable visual-class:static-gray 0)
(define-integrable visual-class:gray-scale 1)
(define-integrable visual-class:static-color 2)
(define-integrable visual-class:pseudo-color 3)
(define-integrable visual-class:true-color 4)
(define-integrable visual-class:direct-color 5)

(define (n-gray-map/win32 window)
  window
  (if (not 128->128-gray-map)
      (set! 128->128-gray-map
	    (let ((s (make-string 128)))
	      (let loop ((i 0))
		(if (fix:< i 128)
		    (begin
		      (vector-8b-set! s i i)
		      (loop (fix:+ i 1)))))
	      s)))
  128->128-gray-map)

(define 128->128-gray-map
  #f)

;;;; Pictures

(define (procedure->picture width height fn)
  (let ((new-pic (make-picture width height)))
    (picture-map! new-pic fn)
    new-pic))

(define (picture-map f . pic-list)
  (if (and (apply = (map (lambda (pic) (picture-width pic)) pic-list))
	   (apply = (map (lambda (pic) (picture-height pic)) pic-list)))
      (let* ((width (picture-width (car pic-list)))
	     (height (picture-height (car pic-list)))
	     (new-pic (make-picture width height))
	     (picdata (picture-data new-pic)))
	(cond ((null? pic-list)
	       (error "no pictures -- PICTURE-MAP"))
	      ((null? (cdr pic-list))
	       (let ((p1-data (picture-data (car pic-list))))
		 (let y-loop ((y 0))
		   (if (fix:< y height)
		       (let ((out-yth-row (vector-ref picdata y))
			     (in-yth-row (vector-ref p1-data y)))
			 (let x-loop ((x 0))
			   (if (fix:< x width)
			       (begin
				 (floating-vector-set!
				  out-yth-row x
				  (exact->inexact
				   (f (floating-vector-ref in-yth-row x))))
				 (x-loop (fix:+ 1 x)))
			       (y-loop (fix:+ 1 y)))))))))
	      ((null? (cddr pic-list))
	       (let ((p1-data (picture-data (car pic-list)))
		     (p2-data (picture-data (cadr pic-list))))
		 (let y-loop ((y 0))
		   (if (fix:< y height)
		       (let ((out-yth-row (vector-ref picdata y))
			     (in-yth-row1 (vector-ref p1-data y))
			     (in-yth-row2 (vector-ref p2-data y)))
			 (let x-loop ((x 0))
			   (if (fix:< x width)
			       (begin
				 (floating-vector-set!
				  out-yth-row x
				  (exact->inexact
				   (f (floating-vector-ref in-yth-row1 x)
				      (floating-vector-ref in-yth-row2 x))))
				 (x-loop (fix:+ 1 x)))
			       (y-loop (fix:+ 1 y)))))))))
	      (else
	       (let ((data-list
		      (map (lambda (pic) (picture-data pic)) pic-list)))
		 (let y-loop ((y 0))
		   (if (fix:< y height)
		       (let ((out-yth-row (vector-ref picdata y))
			     (in-yth-rows (map (lambda (data)
						 (vector-ref data y))
					       data-list)))
			 (let x-loop ((x 0))
			   (if (fix:< x width)
			       (begin
				 (floating-vector-set!
				  out-yth-row x
				  (exact->inexact
				   (apply f
					  (map (lambda (row)
						 (floating-vector-ref row x))
					       in-yth-rows))))
				 (x-loop (fix:+ 1 x)))
			       (y-loop (fix:+ 1 y))))))))))
	(picture-set-data! new-pic picdata)
	new-pic)
      (error "picture sizes do not match -- PICTURE-MAP")))

(define (picture-display window pic #!optional pic-min pic-max)
  (define (check-image pic window brick-wid brick-hgt)
    (if (image? (picture-image pic))
	(let ((image (picture-image pic)))
	  (and (1d-table/get (graphics-device/properties window) image #f)
	       (fix:= (fix:* (picture-width pic) brick-wid)
		      (image/width image))
	       (fix:= (fix:* (picture-height pic) brick-hgt)
		      (image/height image))))
	#f))

  (call-with-values
      (lambda ()
	(graphics-device-coordinate-limits window))
    (lambda (x1 y1 x2 y2)
      (set! *last-picture-displayed* pic)
      (graphics-set-coordinate-limits window 0 (- y2 y1) (- x2 x1) 0)
      (let* ((win-wid (fix:+ 1 (fix:- x2 x1)))
	     (win-hgt (fix:+ 1 (fix:- y1 y2)))
	     (len&margin (integer-divide win-wid (picture-width pic)))
	     (wid&margin (integer-divide win-hgt (picture-height pic)))
	     (h-margin (integer-divide-remainder len&margin))
	     (v-margin (integer-divide-remainder wid&margin))
	     (brick-wid (integer-divide-quotient len&margin))
	     (brick-hgt (integer-divide-quotient wid&margin))
	     (pic-min (if (default-object? pic-min)
			  (picture-min pic)
			  (exact->inexact pic-min)))
	     (pic-max (if (default-object? pic-max)
			  (picture-max pic)
			  (exact->inexact pic-max)))
	     (true-min-max? (and (= pic-min (picture-min pic))
				 (= pic-max (picture-max pic))))
	     (image-cached? (check-image pic window brick-wid brick-hgt)))
	(if (or (fix:< brick-wid 1) (fix:< brick-hgt 1))
	    (error "Window is too small to display" pic '--PICTURE-DISPLAY)
	    (let ((image (if (and image-cached? true-min-max?)
			     (picture-image pic)
			     (build-image pic window
					 brick-wid brick-hgt
					 pic-min pic-max))))
	      (graphics-clear window)
	      (graphics-operation window 'draw-image
			    (quotient h-margin 2)
			    (- (quotient v-margin 2))
			    image)
	      (if (and true-min-max? (not image-cached?))
		  (picture-set-image! pic image))))))))

(define (call-with-last-picture-file procedure)
  (if *last-picture-displayed*
      (call-with-temporary-filename
       (lambda (filename)
	 (picture->pgm-file *last-picture-displayed* filename)
	 (procedure filename)))
      (procedure false)))

(define *last-picture-displayed*
  false)

(define (picture-write picture filename)
  (let ((path-name  (->pathname filename)))
    (if (picture? picture)
	(begin
	  (picture-set-image! picture '())
	  (picture-min picture)  ; ignored - but saves cached min, max values
	  (if (not (pathname-type path-name))
	      (fasdump picture (pathname-new-type path-name "pic"))
	      (fasdump picture path-name)))
	(error:wrong-type-argument picture "picture" 'PICTURE-WRITE))))

(define (picture->pgm-file pic file)
  (let* ((width (picture-width pic))
	 (height (picture-height pic))
	 (data ( picture-data pic))
	 (pmin (picture-min pic))
	 (pmax (picture-max pic))
	 (char-function
	  (cond ((= pmin pmax)
		 (lambda (x) x (ascii->char 0)))
		(else
		 (let ((scale (/ 255. (- pmax pmin))))
		   (lambda (x)
		     (ascii->char (round->exact (* (- x pmin) scale)))))))))
    (call-with-output-file file
      (lambda (port)
	(let ((write-chars
	       (lambda (chars port)
		 (for-each (lambda (char) (write-char char port))
			   chars))))
	  ;;P5 is the magic type number for pgm.
	  (write-chars (string->list "P5") port)
	  (write-char #\Linefeed port)
	  (write-chars (string->list (number->string width)) port)
	  (write-char #\Space port)
	  (write-chars (string->list (number->string height)) port)
	  (write-char #\Linefeed port)
	  ;;write the number of gray levels
	  (write-chars (string->list (number->string 255)) port)
	  (write-char #\Linefeed port)
	  (let rowloop ((row (- height 1)))
	    (if (< row 0)
		'done
		(let ((rowvals
		       (map char-function
			    (floating-vector->list (vector-ref data row)))))
		  (begin
		    (write-string (list->string rowvals) port)
		    (rowloop (- row 1)))))))))))