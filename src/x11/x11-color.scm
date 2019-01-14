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
;;; These were once primitives created by x11color.c in umodule prx11.

(C-include "x11")

;;; Visuals

(define (x-window-visual window)
  (let ((alien (C-call "x_window_visual" (make-alien '(struct |xvisual|))
		       window)))
    (if (alien-null? alien)
	(error "XGetWindowAttributes failed.")
	alien)))

(define (x-get-visual-info window/display visual-id screen-number depth class
			   red-mask green-mask blue-mask colormap-size
			   bits-per-rgb)
  ;; Returns a vector of vectors, each of which has the following format:
  ;;         Visual (Scheme format, for use in later calls)
  ;;         Visual-ID
  ;;         Screen number
  ;;         Depth
  ;;         Class
  ;;         Red-mask (integer)
  ;;         Green-mask (integer)
  ;;         Blue-mask (integer)
  ;;         Colormap size
  ;;         Bits per RGB
  (let ((display (if (not screen-number)
		     (C-call "x_window_display" window/display)
		     window/display))
	(screen-number (if (not screen-number)
			   (C-call "x_window_screen_number" window/display)
			   screen-number))
	(mask (C-enum "VisualNoMask"))
	(info (malloc (C-sizeof "XVisualInfo") '|XVisualInfo|))
	(items-return (make-alien '(* |XVisualInfo|)))
	(nitems-return (malloc (C-sizeof "int") 'int)))
    (C->= info "XVisualInfo screen" screen-number)
    (if visual-id (begin (set! mask (+ mask (C-enum "VisualIDMask")))
			 (C->= info "XVisualInfo visualid" visual-id)))
    (if depth (begin (set! mask (+ mask (C-enum "VisualDepthMask")))
		     (C->= info "XVisualInfo depth" depth)))
    (if class (begin (set! mask (+ mask (C-enum "VisualClassMask")))
		     (C->= info "XVisualInfo class" class)))
    (if red-mask (begin (set! mask (+ mask (C-enum "VisualRedMaskMask")))
			(C->= info "XVisualInfo red_mask" red-mask)))
    (if green-mask (begin (set! mask (+ mask (C-enum "VisualGreenMaskMask")))
			  (C->= info "XVisualInfo green_mask" green-mask)))
    (if blue-mask (begin (set! mask (+ mask (C-enum "VisualBlueMaskMask")))
			 (C->= info "XVisualInfo blue_mask" blue-mask)))
    (if colormap-size
	(begin (set! mask (+ mask (C-enum "VisualColormapSizeMask")))
	       (C->= info "XVisualInfo colormap_size" colormap-size)))
    (if bits-per-rgb
	(begin (set! mask (+ mask (C-enum "VisualBitsPerRGBMask")))
	       (C->= info "XVisualInfo bits_per_rgb" bits-per-rgb)))
    (add-alien-cleanup! items-return cleanup-visual-infos! init-visual-infos!)
    (C-call "x_get_visual_info" display mask info items-return nitems-return)
    (free info)
    (let ((nitems (C-> nitems-return "int"))
	  (items (C-> items-return "*" (make-alien '|XVisualInfo|))))
      (free nitems-return)
      (let loop ((i 0) (infos '()))
	(if (< i nitems)
	    (let ((info (vector (C-call "allocate_x_visual"
					(make-alien '(struct |xvisual|))
					(C-> items "XVisualInfo visual"))
				(C-> items "XVisualInfo visualid")
				(C-> items "XVisualInfo screen")
				(C-> items "XVisualInfo depth")
				(C-> items "XVisualInfo class")
				(C-> items "XVisualInfo red_mask")
				(C-> items "XVisualInfo green_mask")
				(C-> items "XVisualInfo blue_mask")
				(C-> items "XVisualInfo colormap_size")
				(C-> items "XVisualInfo bits_per_rgb"))))
	      (alien-byte-increment! items (C-sizeof "XVisualInfo"))
	      (loop (1+ i) (cons info infos)))
	    (begin
	      (cleanup-alien! items-return)
	      (list->vector (reverse! infos))))))))

(define (init-visual-infos! copy)
  ((ucode-primitive c-malloc 2) copy (C-sizeof "* XVisualInfo")))

(define (cleanup-visual-infos! copy)
  (if (not (alien-null? copy))
      (let ((items (C-> copy "* XVisualInfo")))
	(if (not (alien-null? items))
	    (C-call "XFree" items))
	((ucode-primitive c-free 1) copy)
	(alien-null! copy))))

;;; Colormap

(define (x-window-colormap window)
  (C-call "x_window_colormap" (make-alien '(struct |xcolormap|)) window))

(define (x-set-window-colormap window colormap)
  (C-call "x_set_window_colormap" window colormap))

(define (x-create-colormap window visual writable?)
  (C-call "x_create_colormap" (make-alien '(struct |xcolormap|))
	  window visual (if writable? 1 0)))

(define (x-free-colormap colormap)
  (C-call "x_free_colormap" colormap)
  (alien-null! colormap))

(define (x-allocate-color colormap red green blue)
  (let ((pixel (C-call "x_allocate_color" colormap red green blue)))
    (if (= -1 pixel)
	(error "Could to allocate color:" colormap))
    pixel))

(define (x-store-color colormap pixel red green blue)
  (let ((r (or red -1))
	(g (or green -1))
	(b (or blue -1)))
    (if (or (< r -1) (< 65536 r))
	(error:bad-range-argument r 'x-store-color))
    (if (or (< r -1) (< 65536 g))
	(error:bad-range-argument g 'x-store-color))
    (if (or (< r -1) (< 65536 b))
	(error:bad-range-argument b 'x-store-color))
    (C-call "x_store_color" colormap pixel r g b)))

(define (x-store-colors colormap array)
  ;; Input: colormap, vector of vectors, each of
  ;; which contains pixel, r, g, b (where r/g/b can be #f or integer).
  (let* ((length (vector-length array))
	 (ints (malloc (* (* 4 length) (C-sizeof "int")) 'int))
	 (scan (copy-alien ints)))
    (let loop ((i 0))
      (if (< i length)
	  (let ((prgb (vector-ref array i)))
	    (C->= scan "int" (vector-ref prgb 0))
	    (alien-byte-increment! scan (C-sizeof "int"))
	    (C->= scan "int" (vector-ref prgb 1))
	    (alien-byte-increment! scan (C-sizeof "int"))
	    (C->= scan "int" (vector-ref prgb 2))
	    (alien-byte-increment! scan (C-sizeof "int"))
	    (C->= scan "int" (vector-ref prgb 3))
	    (alien-byte-increment! scan (C-sizeof "int"))
	    (loop (1+ i)))))
    (C-call "x_store_colors" colormap ints length)
    (free ints)))

(define (x-query-color colormap pixel)
  (let ((vec (make-vector 3))
	(rgb (malloc (* 3 (C-sizeof "long")))))
    (C-call "x_query_color" colormap pixel rgb)
    (let ((scan (copy-alien rgb)))
      (vector-set! vec 0 (C-> scan "long"))
      (alien-byte-increment! (C-sizeof "long"))
      (vector-set! vec 1 (C-> scan "long"))
      (alien-byte-increment! (C-sizeof "long"))
      (vector-set! vec 2 (C-> scan "long")))
    (free rgb)
    vec))