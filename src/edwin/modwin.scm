#| -*-Scheme-*-

$Id: modwin.scm,v 1.45 2003/02/14 18:28:12 cph Exp $

Copyright 1986, 1989-1999, 2002 Massachusetts Institute of Technology

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

;;;; Modeline Window

(declare (usual-integrations))

(define-class modeline-window vanilla-window
  (shows-buffer-modified?))

(define (modeline-window/shows-buffer-modified? window)
  (with-instance-variables modeline-window window ()
    shows-buffer-modified?))

(define (set-modeline-window/shows-buffer-modified?! window value)
  (with-instance-variables modeline-window window (value)
    (set! shows-buffer-modified? value)))

(define-method modeline-window (:initialize! window window*)
  (usual==> window :initialize! window*)
  (set! y-size 1)
  (set! shows-buffer-modified? #f))

(define (modeline-window:update-display! window screen x-start y-start
					 xl xu yl yu display-style)
  display-style				;ignore
  (if (and (fix:= yl 0) (fix:< yl yu))
      (let ((superior (window-superior window))
	    (xl (fix:+ x-start xl))
	    (xu (fix:+ x-start xu)))
	(let ((buffer (window-buffer superior)))
	  (modeline-string!
	   superior
	   (screen-get-output-line
	    screen y-start xl xu
	    (ref-variable mode-line-inverse-video buffer))
	   xl xu)
	  (set-modeline-window/shows-buffer-modified?!
	   window
	   (buffer-modified? buffer)))))
  #t)

(define-method modeline-window :update-display!
  modeline-window:update-display!)

(define-variable mode-line-inverse-video
  "True means use inverse video, or other suitable display mode, for the mode line."
  #t
  boolean?)

(define (modeline-window:event! window type)
  type					;ignored
  (window-needs-redisplay! window))

(define (modeline-window:notice-changes! window)
  (if (not (boolean=? (buffer-modified?
		       (window-buffer (window-superior window)))
		      (modeline-window/shows-buffer-modified? window)))
      (window-needs-redisplay! window)))