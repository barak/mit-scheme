#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/sicp/graphics.scm,v 1.3 1991/04/12 00:12:11 arthur Exp $

Copyright (c) 1987, 1988, 1989, 1990 Massachusetts Institute of Technology

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

;;;; Student graphics Interface
;;;; implemented for X Windows

(declare (usual-integrations))

(define clear-graphics)
(define clear-point)
(define draw-line-to)
(define draw-point)
(define graphics-available?)
(define graphics-text)
(define init-graphics)
(define position-pen)

(define graphics-package
  (make-environment

    (define graphics-device #F)

    (define (init-if-necessary)
      (if (not graphics-device)
	  (init-graphics)))

    (set! clear-graphics
	  (lambda ()
	    (init-if-necessary)
	    (graphics-clear graphics-device)
	    (graphics-move-cursor graphics-device 0 0)))

    (set! clear-point
	  (lambda (x y)
	    (init-if-necessary)
	    (graphics-erase-point graphics-device x y)))

    (set! draw-line-to
	  (lambda (x y)
	    (init-if-necessary)
	    (graphics-drag-cursor graphics-device x y)))

    (set! draw-point
	  (lambda (x y)
	    (init-if-necessary)
	    (graphics-draw-point graphics-device x y)))

    (set! graphics-available?
	  (lambda ()
	    (graphics-type-available? x-graphics-device-type)))

    (set! graphics-text
	  (lambda (text x y)
	    (init-if-necessary)
	    ;; Accepts different parameters on Chipmunks.
	    (graphics-draw-text graphics-device x y text)))

    (set! init-graphics
	  (lambda ()
	    (let ((display (x-open-display #f)))
	      (set! graphics-device
		    (make-graphics-device x-graphics-device-type
					  display "512x388" #f)))
	    (graphics-set-coordinate-limits graphics-device -256 -195 255 194)
	    (graphics-move-cursor graphics-device 0 0)))

    (set! position-pen
	  (lambda (x y)
	    (init-if-necessary)
	    (graphics-move-cursor graphics-device x y)))

))