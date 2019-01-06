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

;;;; Test the x11 interface.
;;; package: (x11)

(define (run-tests)
  (let ((x11dev (->environment '(x11 device))))
    (let ((x-window/display (access x-window/display x11dev))
	  (x-display/window-finalizer (access x-display/window-finalizer
					      x11dev))
	  (x-display/xd (access x-display/xd x11dev))
	  (x-window/xw (access x-window/xw x11dev)))
      (let* ((dev (make-graphics-device))
	     (x-window (graphics-device/descriptor dev))
	     (x-display (x-window/display x-window)))

	(if (not (eq? 'X11 (graphics-type-name (graphics-type dev))))
	    (error "The X11 graphics type is NOT the default."))

	(test-graphics dev)

	(test-properties (x-display/xd x-display)
			 (x-window-id (x-window/xw x-window)))

	(display "Waiting for windows to close...\n")
	(let wait ()
	  (sleep-current-thread 1000)
	  (if (not (null?
		    (gc-finalizer-elements
		     (x-display/window-finalizer x-display))))
	      (wait)))))))

(define (test-graphics dev)
  (display "Drawing...\n")
  (graphics-draw-point dev 0 .1)
  (graphics-draw-point dev 0 .2)
  (graphics-draw-point dev 0 .3)
  (graphics-erase-point dev 0 .2)
  (graphics-draw-text dev 0. .4 "Hello!")
  (graphics-draw-line dev -.5 -.5 .5 .5)
  (graphics-move-cursor dev -.5 .5)
  (graphics-drag-cursor dev .5 -.5))

(define (test-properties xd window-id)
  (display "Getting/putting properties...\n")

  ;; An atom type property.
  (let ((property (x-intern-atom xd "_NET_WM_ALLOWED_ACTIONS" #f))
	(type (x-intern-atom xd "ATOM" #f)))
    (let ((v (x-get-window-property xd window-id
				    property 0 0 #f type)))
      (let ((bytes-left (vector-ref v 2)))
	(let ((v (x-get-window-property xd window-id
					property 0 (quotient bytes-left 4)
					#f type)))
	  (let ((bytes-left (vector-ref v 2)))
	    (if (not (zero? bytes-left))
		(error "Incomplete property read.")))

	  (pp (vector-map (lambda (atom) (x-get-atom-name xd atom))
			  (vector-ref v 3)))))))

  ;; A short type property should be read and written.

  ;; A char type property should be read and written.

  ;; Each type should be read and written using data larger than
  ;; (property-quantum display).  (Move the multi-quanta reading [and
  ;; writing?] code, e.g. get-window-property, here from
  ;; x11-screen.scm?)
  )

(run-tests)