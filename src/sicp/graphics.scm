#| -*-Scheme-*-

$Id: graphics.scm,v 1.7 2003/02/14 18:28:35 cph Exp $

Copyright (c) 1987-1999 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; Student graphics Interface
;;;; implemented for X Windows/ Win32 / OS2

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
	    (or (graphics-type-available? 'X)
		(graphics-type-available? 'WIN32)
		(graphics-type-available? 'OS/2))))

    (set! graphics-text
	  (lambda (text x y)
	    (init-if-necessary)
	    ;; Accepts different parameters on Chipmunks.
	    (graphics-draw-text graphics-device x y text)))

    (set! init-graphics
	  (lambda ()
	    (set! graphics-device
		  (cond ((graphics-type-available? 'X)
			 (make-graphics-device 'X #F "512x388"))
			((graphics-type-available? 'WIN32)
			 (make-graphics-device 'WIN32 512 388))
			((graphics-type-available? 'OS/2)
			 (make-graphics-device 'OS/2 512 388))
			(else
			 (error "Graphics is not available"))))
	    (graphics-set-coordinate-limits graphics-device -256 -195 255 194)
	    (graphics-move-cursor graphics-device 0 0)))

    (set! position-pen
	  (lambda (x y)
	    (init-if-necessary)
	    (graphics-move-cursor graphics-device x y)))

))