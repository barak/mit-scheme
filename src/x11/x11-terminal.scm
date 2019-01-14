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

;;;; X11 Terminal interface
;;; package: (x11 terminal)
;;;
;;; These were once primitives created by x11term.c in umodule prx11.

(C-include "x11")

(define (xterm-erase-cursor window)
  (c-call "xterm_erase_cursor" window))

(define (xterm-draw-cursor window)
  (c-call "xterm_draw_cursor" window))

(define (xterm-dump-rectangle window x y width height)
  (c-call "xterm_dump_rectangle" window x y width height))

(define (xterm-reconfigure window x-csize y-csize)
  (c-call "xterm_reconfigure" window x-csize y-csize))

(define (xterm-map-x-coordinate window x)
  (c-call "xterm_map_x_coordinate" window x))

(define (xterm-map-y-coordinate window y)
  (c-call "xterm_map_y_coordinate" window y))

(define (xterm-map-x-size window width)
  (c-call "xterm_map_x_size" window width))

(define (xterm-map-y-size window height)
  (c-call "xterm_map_y_size" window height))

(define (xterm-open-window display geometry suppress-map)
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
	     (values 0 0 #t))
	    (else
	     (values 0 0 #f)))
    (let ((window
	   (c-call "xterm_open_window" (make-alien '(struct |xwindow|))
		   display
		   (->cstring geometry)
		   (->cstring name)
		   (->cstring class)
		   (if map? 1 0))))
      (if (alien-null? window)
	  (error "Could not open xterm:" geometry))
      window)))

(define (xterm-x-size xterm)
  (c-call "xterm_x_size" xterm))

(define (xterm-y-size xterm)
  (c-call "xterm_y_size" xterm))

(define (xterm-set-size xterm width height)
  (c-call "xterm_set_size" xterm width height))

(define (xterm-enable-cursor window enable?)
  (c-call "xterm_enable_cursor" window (if enable? 1 0)))

(define (xterm-write-cursor! xterm x y)
  (let ((code (c-call "xterm_write_cursor" xterm x y)))
    (case code
      ((1) (error:bad-range-argument x 'xterm-write-cursor!))
      ((2) (error:bad-range-argument y 'xterm-write-cursor!)))))

(define (xterm-write-char! xterm x y char highlight)
  (let ((code (c-call "xterm_write_char"
		      xterm x y (char->integer char) highlight)))
    (case code
      ((1) (error:bad-range-argument x 'xterm-write-char!))
      ((2) (error:bad-range-argument y 'xterm-write-char!))
      ((3) (error:bad-range-argument highlight 'xterm-write-char!)))))

(define (xterm-write-substring! xterm x y string start end highlight)
  (let ((code (if (bytevector? string)
		  (c-call "xterm_write_substring"
			  xterm x y string start end highlight)
		  (c-call "xterm_write_substring"
			  xterm x y
			  (string->iso8859-1 string start end)
			  0 (fix:- end start) highlight))))
    (case code
      ((1) (error:bad-range-argument x 'xterm-write-substring!))
      ((2) (error:bad-range-argument y 'xterm-write-substring!))
      ((3) (error:bad-range-argument start 'xterm-write-substring!))
      ((4) (error:bad-range-argument highlight 'xterm-write-substring!))
      ((5) (error:bad-range-argument end 'xterm-write-substring!)))))

(define (xterm-clear-rectangle! window x-start x-end y-start y-end highlight)
  (let ((code (c-call "xterm_clear_rectangle"
		      window x-start x-end y-start y-end highlight)))
    (case code
      ((1) (error:bad-range-argument x-end 'xterm-clear-rectangle))
      ((2) (error:bad-range-argument y-end 'xterm-clear-rectangle))
      ((3) (error:bad-range-argument x-start 'xterm-clear-rectangle))
      ((4) (error:bad-range-argument y-start 'xterm-clear-rectangle))
      ((5) (error:bad-range-argument highlight 'xterm-clear-rectangle)))))

(define (xterm-scroll-lines-up xterm x-start x-end y-start y-end lines)
  ;; Scroll the contents of the region up by LINES.
  (let ((code (c-call "xterm_scroll_lines_up"
		      xterm x-start x-end y-start y-end lines)))
    (case code
      ((1) (error:bad-range-argument x-end 'xterm-scroll-lines-up))
      ((2) (error:bad-range-argument y-end 'xterm-scroll-lines-up))
      ((3) (error:bad-range-argument x-start 'xterm-scroll-lines-up))
      ((4) (error:bad-range-argument y-start 'xterm-scroll-lines-up))
      ((5) (error:bad-range-argument lines 'xterm-scroll-lines-up)))))

(define (xterm-scroll-lines-down xterm x-start x-end y-start y-end lines)
  ;; Scroll the contents of the region down by LINES.
  (let ((code (c-call "xterm_scroll_lines_down"
		      xterm x-start x-end y-start y-end lines)))
    (case code
      ((1) (error:bad-range-argument x-end 'xterm-scroll-lines-down))
      ((2) (error:bad-range-argument y-end 'xterm-scroll-lines-down))
      ((3) (error:bad-range-argument x-start 'xterm-scroll-lines-down))
      ((4) (error:bad-range-argument y-start 'xterm-scroll-lines-down))
      ((5) (error:bad-range-argument lines 'xterm-scroll-lines-down)))))

(define (xterm-save-contents xterm x-start x-end y-start y-end)
  ;; Get the contents of the terminal screen rectangle as a bytevector.
  ;; The bytevector contains alternating (CHARACTER, HIGHLIGHT) pairs.
  ;; The pairs are organized in row-major order from (X-START, Y-START).
  (let* ((bytevector (make-bytevector (* 2
					 (- x-end x-start)
					 (- y-end y-start))))
	 (code (c-call "xterm_save_contents"
		       xterm x-start x-end y-start y-end bytevector)))
    (case code
      ((1) (error:bad-range-argument x-end 'xterm-save-contents))
      ((2) (error:bad-range-argument y-end 'xterm-save-contents))
      ((3) (error:bad-range-argument x-start 'xterm-save-contents))
      ((4) (error:bad-range-argument y-start 'xterm-save-contents)))
    bytevector))

(define (xterm-restore-contents xterm x-start x-end y-start y-end contents)
  ;; Replace the terminal screen rectangle with CONTENTS.
  ;; See `XTERM-SCREEN-CONTENTS' for the format of CONTENTS.
  (if (not (= (bytevector-length contents)
	      (* 2
		 (- x-end x-start)
		 (- y-end y-start))))
      (error:bad-range-argument contents 'xterm-restore-contents))
  (let ((code (c-call "xterm_restore_contents"
		      xterm x-start x-end y-start y-end contents)))
    (case code
      ((1) (error:bad-range-argument x-end 'xterm-restore-contents))
      ((2) (error:bad-range-argument y-end 'xterm-restore-contents))
      ((3) (error:bad-range-argument x-start 'xterm-restore-contents))
      ((4) (error:bad-range-argument y-start 'xterm-restore-contents)))))