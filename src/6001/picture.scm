#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

;;;; 6.001 Images

(declare (usual-integrations))

;;;; Miscellaneous Utilities

(define (flo:make-vector length init)
  (let ((result (flo:vector-cons length)))
    (if (not (= init 0.))
	(do ((i 0 (fix:+ i 1)))
	    ((fix:= i length))
	  (flo:vector-set! result i init)))
    result))

(define (flo:vector-copy vector)
  (let* ((length (flo:vector-length vector))
	 (result (flo:vector-cons length)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i length))
      (flo:vector-set! result i (flo:vector-ref vector i)))
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

(define (flo:vector->list vector)
  (generate-list (flo:vector-length vector)
    (lambda (i)
      (flo:vector-ref vector i))))

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

(define os2-image-colormap:gray-256
  (make-initialized-vector 256
    (lambda (index)
      (+ (* index #x10000)
	 (* index #x100)
	 index))))

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
      ((WIN32) (n-gray-map/win32 window))
      ((OS/2) (n-gray-map/os2 window))
      (else (error "Unsupported graphics type:" name)))))

(define n-gray-map/win32
  (let ((map (make-vector 128)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i 128))
      (vector-set! map i i))
    (lambda (window) window map)))

(define n-gray-map/os2
  (let ((map (make-vector 256)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i 256))
      (vector-set! map i i))
    (lambda (window) window map)))

(define (n-gray-map/X11 window)
  (let ((properties (x-display/properties (x-graphics/display window))))
    (or (1d-table/get properties '6001-GRAY-MAP #f)
	(let ((gm (allocate-grays window)))
	  (1d-table/put! properties '6001-GRAY-MAP gm)
	  gm))))

(define (allocate-grays window)
  (let ((w-cm (graphics-operation window 'get-colormap))
	(visual-info (vector->list (x-graphics/visual-info window))))
    (let ((find-class
	   (lambda (class)
	     (there-exists? visual-info
	       (lambda (info)
		 (eqv? class (x-visual-info/class info))))))
	  (find-range
	   (lambda (class depth-min depth-max)
	     (there-exists? visual-info
	       (lambda (info)
		 (and (eqv? class (x-visual-info/class info))
		      ;; kludge, but X made us do it.
		      (<= depth-min
			  (x-visual-info/colormap-size info)
			  depth-max))))))
	  (make-gray-map
	   (lambda (n-levels)
	     (let ((gm (make-vector n-levels))
		   (binner (linear-binner 0 (- n-levels 1) #x10000)))
	       (do ((index 0 (+ index 1)))
		   ((= index n-levels))
		 (vector-set! gm
			      index
			      (let ((intensity
				     (binner (exact->inexact index))))
				(x-colormap/allocate-color
				 w-cm intensity intensity intensity))))
	       gm)))
	  (make-color-map
	   (lambda (n-levels)
	     (make-spectrum-palette n-levels
	       (let ((binner (linear-binner 0 1 #x10000)))
		 (lambda (r g b)
		   (x-colormap/allocate-color w-cm
					      (binner r)
					      (binner g)
					      (binner b))))))))
      (cond ((or (find-class x-visual-class:true-color)
		 (find-class x-visual-class:direct-color))
	     (if use-color?
		 (make-color-map 256)
		 (make-gray-map 256)))
	    ((find-range x-visual-class:pseudo-color 250 256)
	     (if use-color?
		 (make-color-map 128)
		 (make-gray-map 128)))
	    ((find-range x-visual-class:static-gray 256 256)
	     (make-gray-map 256))
	    ((or (find-range x-visual-class:static-gray 128 255)
		 (find-range x-visual-class:gray-scale 256 256))
	     (make-gray-map 128))
	    ((find-range x-visual-class:static-gray 2 2)
	     (make-gray-map 2))
	    (else
	     (error "ALLOCATE-GRAYS: not known display type" window))))))

(define use-color? #f)
(define 2pi (* 8 (atan 1 1)))
(define color-saturation 1)
(define minimum-color-intensity .5)

(define (make-spectrum-palette n-levels encode-color)
  (make-initialized-vector n-levels
    (let ((step (/ (* 2/3 2pi) (- n-levels 2))))
      (lambda (index)
	(if (= 0 index)
	    (encode-color 0 0 0)
	    (call-with-values
		(lambda ()
		  (hsv->rgb (* step (- (- n-levels 2) index))
			    color-saturation
			    (+ minimum-color-intensity
			       (* (- 1 minimum-color-intensity)
				  (/ index (- n-levels 1))))))
	      encode-color))))))

(define (hsv->rgb h s v)
  ;; H is in radians, S and V are in [0, 1].
  ;; Returns three values, RGB, all in [0, 1].
  (if (= 0 s)
      (values v v v)
      (let ((h
	     (let ((2pi (* 8 (atan 1 1))))
	       (let ((h/2pi (/ h 2pi)))
		 (* (- h/2pi (floor h/2pi)) 6)))))
	(let ((i (floor->exact h)))
	  (let ((f (- h i)))
	    (let ((p (* v (- 1 s)))
		  (q (* v (- 1 (* s f))))
		  (t (* v (- 1 (* s (- 1 f))))))
	      (case i
		((0) (values v t p))
		((1) (values q v p))
		((2) (values p v t))
		((3) (values p q v))
		((4) (values t p v))
		((5) (values v p q)))))))))

(define (linear-binner min-value max-value n-bins)
  (let ((min-value (exact->inexact min-value))
	(scale (exact->inexact (/ n-bins (- max-value min-value)))))
    (lambda (value)
      (let ((bin
	     (flo:floor->exact (flo:* (flo:- (if (flo:flonum? value)
						 value
						 (exact->inexact value))
					     min-value)
				      scale))))
	(cond ((< bin 0) 0)
	      ((>= bin n-bins) (- n-bins 1))
	      (else bin))))))

(define (cutoff-binner cut-fraction min-value max-value n-bins)
  ;; Bin values with distinguished zero bin.  If the value would have
  ;; fallen in the low CUT-FRACTION of the zero bin for a linear
  ;; binning, then it goes in the zero bin here.  Otherwise, the value
  ;; is binned in the top N-1 bins.
  (let ((cut-value
	 (exact->inexact
	  (+ min-value (* cut-fraction (/ (- max-value min-value) n-bins)))))
	(binner (linear-binner min-value max-value (- n-bins 1))))
    (lambda (value)
      (if (flo:< value cut-value)
	  0
	  (fix:+ 1 (binner value))))))

(define-integrable visual-class:static-gray 0)
(define-integrable visual-class:gray-scale 1)
(define-integrable visual-class:static-color 2)
(define-integrable visual-class:pseudo-color 3)
(define-integrable visual-class:true-color 4)
(define-integrable visual-class:direct-color 5)

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
				 (flo:vector-set!
				  out-yth-row x
				  (exact->inexact
				   (f (flo:vector-ref in-yth-row x))))
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
				 (flo:vector-set!
				  out-yth-row x
				  (exact->inexact
				   (f (flo:vector-ref in-yth-row1 x)
				      (flo:vector-ref in-yth-row2 x))))
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
				 (flo:vector-set!
				  out-yth-row x
				  (exact->inexact
				   (apply f
					  (map (lambda (row)
						 (flo:vector-ref row x))
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
      (let* ((win-wid (+ 1 (abs (- x2 x1))))
	     (win-hgt (+ 1 (abs (- y1 y2))))
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
	      (image/draw window
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
	 (pmax (picture-max pic)))
    (call-with-output-file file
      (lambda (port)
	;;P5 is the magic type number for pgm.
	(write-string "P5" port)
	(write-char #\Linefeed port)
	(write width port)
	(write-char #\Space port)
	(write height port)
	(write-char #\Linefeed port)
	;;write the number of gray levels
	(write 255 port)
	(write-char #\Linefeed port)
	(let rowloop ((row (- height 1)))
	  (if (>= row 0)
	      (let ((rowvals
		     (map (cond ((= pmin pmax)
				 (lambda (x) x (ascii->char 0)))
				(else
				 (let ((scale (/ 255. (- pmax pmin))))
				   (lambda (x)
				     (ascii->char
				      (round->exact (* (- x pmin) scale)))))))
			  (flo:vector->list (vector-ref data row)))))
		(begin
		  (write-string (list->string rowvals) port)
		  (rowloop (- row 1))))))))))