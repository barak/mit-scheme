#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

This file is part of an x11 plugin for MIT/GNU Scheme.

This plugin is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

This plugin is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this plugin; if not, write to the Free Software Foundation,
Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.

|#

;;;; X11 interface
;;; package: (x11)
;;;
;;; These were once primitives created by x11base.c in umodule prx11.

(C-include "x11")

(define (x-graphics-set-vdc-extent window x-left y-bottom x-right y-top)
  ;; Set the virtual device coordinates to the given values.
  (C-call "x_graphics_set_vdc_extent" window x-left y-bottom x-right y-top))

(define (x-graphics-vdc-extent window)
  (let* ((limits (make-vector 4))
	 (floats (malloc (* 4 (C-sizeof "float")) 'float))
	 (scan (copy-alien floats)))
    (C-call "x_graphics_vdc_extent" window scan)
    (vector-set! limits 0 (C-> scan "float"))
    (alien-byte-increment! scan (C-sizeof "float"))
    (vector-set! limits 1 (C-> scan "float"))
    (alien-byte-increment! scan (C-sizeof "float"))
    (vector-set! limits 2 (C-> scan "float"))
    (alien-byte-increment! scan (C-sizeof "float"))
    (vector-set! limits 3 (C-> scan "float"))
    (free floats)
    limits))

(define (x-graphics-reset-clip-rectangle window)
  (C-call "x_graphics_reset_clip_rectangle" window))

(define (x-graphics-set-clip-rectangle window x-left y-bottom x-right y-top)
  ;; Set the clip rectangle to the given coordinates.
  (C-call "x_graphics_set_clip_rectangle" window
	  (->flonum x-left) (->flonum y-bottom)
	  (->flonum x-right) (->flonum y-top)))

(define (x-graphics-reconfigure window width height)
  (C-call "x_graphics_reconfigure" window width height))

(define (x-graphics-open-window display geometry suppress-map)
  ;; Open a window on DISPLAY using GEOMETRY.  If GEOMETRY is false
  ;; map window interactively.  If third argument SUPPRESS-MAP? is
  ;; true, do not map the window immediately.
  (receive (name class map?)
      (cond ((and (pair? suppress-map)
		  (string? (car suppress-map))
		  (string? (cdr suppress-map)))
	     (values (car suppress-map) (cdr suppress-map) #t))
	    ((and (vector? suppress-map)
		  (= 3 (vector-length suppress-map))
		  (boolean? (vector-ref suppress-map 0))
		  (string? (vector-ref suppress-map 1))
		  (string? (vector-ref suppress-map 2)))
	     (values (vector-ref suppress-map 1)
		     (vector-ref suppress-map 2)
		     (vector-ref suppress-map 0)))
	    ((eq? #f suppress-map)
	     (values #f #f #t))
	    (else
	     (values #f #f #f)))
    (let ((window
	   (c-call "x_graphics_open_window" (make-alien '(struct |xwindow|))
		   display
		   (->cstring geometry)
		   (->cstring name)
		   (->cstring class)
		   (if map? 1 0))))
      (if (alien-null? window)
	  (error "Could not open window:" geometry))
      window)))

(define (x-graphics-draw-line window x-start y-start x-end y-end)
  ;; Draw a line from the start coordinates to the end coordinates.
  ;; Subsequently move the graphics cursor to the end coordinates.
  (C-call "x_graphics_draw_line" window x-start y-start x-end y-end))

(define (x-graphics-move-cursor window x y)
  ;; Move the graphics cursor to the given coordinates.
  (C-call "x_graphics_move_cursor" window x y))

(define (x-graphics-drag-cursor window x y)
  ;; Draw a line from the graphics cursor to the given coordinates.
  ;; Subsequently move the graphics cursor to those coordinates.
  (C-call "x_graphics_drag_cursor" window x y))

(define (x-graphics-draw-point window x y)
  ;; Draw one point at the given coordinates.
  ;; Subsequently move the graphics cursor to those coordinates.
  (C-call "x_graphics_draw_point" window x y))

(define (x-graphics-draw-arc window x y radius-x radius-y
			     start-angle sweep-angle fill?)
  ;; Draw an arc at the given coordinates, with given X and Y radii.
  ;; START-ANGLE and SWEEP-ANGLE are in degrees, anti-clocwise.
  ;; START-ANGLE is from 3 o'clock, and SWEEP-ANGLE is relative to the
  ;; START-ANGLE.  If FILL? is true, the arc is filled.
  (C-call "x_graphics_draw_arc" window
	  x y radius-x radius-y start-angle sweep-angle (if fill? 1 0)))

(define (x-graphics-draw-string window x y string)
  ;; Draw characters in the current font at the given coordinates, with
  ;; transparent background.
  (C-call "x_graphics_draw_string" window x y (->cstring string)))

(define (x-graphics-draw-image-string window x y string)
  ;; Draw characters in the current font at the given coordinates, with
  ;; solid background.
  (C-call "x_graphics_draw_image_string" window x y (->cstring string)))

(define (x-graphics-set-function window function)
  (if (not (zero? (C-call "x_graphics_set_function" window function)))
      (error:bad-range-argument function 'x-graphics-set-function)))

(define (x-graphics-draw-points window x-vector y-vector)
  (let* ((n-points (flo:vector-length x-vector))
	 (points (malloc (* n-points (C-sizeof "XPoint")))))
    (if (not (= n-points (flo:vector-length y-vector)))
	(error:bad-range-argument y-vector 'x-graphics-draw-points))
    (C-call "x_graphics_draw_points" window x-vector y-vector n-points points)
    (free points)))

(define (x-graphics-draw-lines window x-vector y-vector)
  (let* ((n-points (flo:vector-length x-vector))
	 (points (malloc (* n-points (C-sizeof "XPoint")))))
    (if (not (= n-points (flo:vector-length y-vector)))
	(error:bad-range-argument y-vector 'x-graphics-draw-lines))
    (C-call "x_graphics_draw_lines" window x-vector y-vector n-points points)
    (free points)))

(define (x-graphics-set-fill-style window style)
  (if (zero? (C-call "x_graphics_set_fill_style" window style))
      (error:bad-range-argument style 'x-graphics-set-fill-style)))

(define (x-graphics-set-line-style window style)
  (if (zero? (C-call "x_graphics_set_line_style" window style))
      (error:bad-range-argument style 'x-graphics-set-line-style)))

(define (x-graphics-set-dashes window dash-offset dash-list)
  (if (zero? (C-call "x_graphics_set_dashes"
		     window dash-offset dash-list (string-length dash-list)))
      (error:bad-range-argument dash-offset 'x-graphics-set-dashes)))

(define (x-graphics-copy-area source-window destination-window
			      source-x-left source-y-top width height
			      destination-x-left destination-y-top)
  (if (zero? (C-call "x_graphics_copy_area"
		     source-window destination-window
		     source-x-left source-y-top width height
		     destination-x-left destination-y-top))
      (error "Source and destination are not the same.")))

(define (x-graphics-fill-polygon window vector)
  (let ((length (flo:vector-length vector)))
    (if (not (even? length))
	(error:bad-range-argument vector 'x-graphics-fill-polygon))
    (let ((points (malloc (* (/ length 2) (C-sizeof "XPoint")))))
      (C-call "x_graphics_fill_polygon" window vector length points)
      (free points))))

(define (x-create-image window width height)
  ;; Creates and returns an XImage object, of dimensions WIDTH by HEIGHT.
  ;; WINDOW is used to set the Display, Visual, and Depth characteristics.
  ;; The image is created by calling XCreateImage.
  (let ((result (C-call "x_create_image" (make-alien '(struct |xwindow|))
			window width height)))
    (if (alien-null? result)
	(error "Could not create image:" window)
	result)))

(define (x-bytes-into-image vector image)
  ;; VECTOR is a bytevector of pixel values stored in row-major order; it must
  ;; have the same number of pixels as IMAGE.  These pixels are written onto
  ;; IMAGE by repeated calls to XPutPixel.  This procedure is equivalent to
  ;; calling X-SET-PIXEL-IN-IMAGE for each pixel in VECTOR.
  (guarantee bytevector? vector 'x-bytes-into-image)
  (C-call "x_bytes_into_image" vector image))

(define (x-get-pixel-from-image image x y)
  (let ((pixel (C-call "x_get_pixel_from_image" image x y)))
    (if (negative? pixel)
	(error "Invalid arguments."))
    pixel))

(define (x-set-pixel-in-image image x y pixel)
  (if (zero? (C-call "x_set_pixel_in_image" image x y pixel))
      (error "Invalid arguments.")))

(define (x-destroy-image image)
  (C-call "x_destroy_image" image))

(define (x-display-image image x-offset y-offset
			 window window-xoff window-yoff width height)
  (if (zero? (C-call "x_display_image" image x-offset y-offset
		     window window-xoff window-yoff width height))
      (error "Invalid args.")))

(define (x-read-image image x-image-offset y-image-offset
		      window x-window-offset y-window-offset width height)
  (C-call "x_read_image" image x-image-offset y-image-offset
	  window x-window-offset y-window-offset width height))

(define (x-window-depth window)
  ;; Returns the pixel depth of WINDOW as an integer.
  (C-call "x_window_depth" window))

(define (x-graphics-map-x-coordinate window x)
  (C-call "x_graphics_map_x_coordinate" window x))

(define (x-graphics-map-y-coordinate window y)
  (C-call "x_graphics_map_y_coordinate" window y))