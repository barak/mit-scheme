;;; -*-Scheme-*-
;;;
;;;$Id: modwin.scm,v 1.40 1999/01/02 06:11:34 cph Exp $
;;;
;;; Copyright (c) 1986, 1989-1999 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;;; Modeline Window

(declare (usual-integrations))

(define-class modeline-window vanilla-window ())

(define-method modeline-window (:initialize! window window*)
  (usual=> window :initialize! window*)
  (set! y-size 1))

(define (modeline-window:update-display! window screen x-start y-start
					 xl xu yl yu display-style)
  display-style				;ignore
  (if (and (fix:= yl 0) (fix:< yl yu))
      (let ((superior (window-superior window))
	    (xl (fix:+ x-start xl))
	    (xu (fix:+ x-start xu)))
	(modeline-string!
	 superior
	 (screen-get-output-line
	  screen y-start xl xu
	  (variable-local-value (window-buffer superior)
				(ref-variable-object mode-line-inverse-video)))
	 xl xu)))
  true)

(define-method modeline-window :update-display!
  modeline-window:update-display!)

(define-variable mode-line-inverse-video
  "True means use inverse video, or other suitable display mode, for the mode line."
  true
  boolean?)

(define (modeline-window:event! window type)
  type					;ignored
  (with-instance-variables modeline-window window ()
    (setup-redisplay-flags! redisplay-flags)))