#| -*-Scheme-*-

$Id: winout.scm,v 1.16 2003/02/14 18:25:21 cph Exp $

Copyright 1986, 1989-2000 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

;;;; Buffer I/O Ports
;;; package: (edwin window-output-port)

(declare (usual-integrations))

(define (with-output-to-current-point thunk)
  (with-output-to-window-point (current-window) thunk))

(define (with-output-to-window-point window thunk)
  (with-output-to-port (window-output-port window) thunk))

(define (window-output-port window)
  (make-port window-output-port-type window))

(define (operation/fresh-line port)
  (if (not (line-start? (window-point (port/state port))))
      (operation/write-char port #\newline)))

(define (operation/write-char port char)
  (let ((window (port/state port)))
    (let ((buffer (window-buffer window))
	  (point (window-point window)))
      (if (and (null? (cdr (buffer-windows buffer)))
	       (line-end? point)
	       (buffer-auto-save-modified? buffer)
	       (or (not (window-needs-redisplay? window))
		   (window-direct-update! window #f)))
	  (cond ((and (group-end? point)
		      (char=? char #\newline)
		      (< (1+ (window-point-y window)) (window-y-size window)))
		 (window-direct-output-insert-newline! window))
		((and (not (char=? char #\newline))
		      (not (char=? char #\tab))
		      (let ((image (window-char->image window char)))
			(and (= (string-length image) 1)
			     (char=? (string-ref image 0) char)))
		      ;; above 3 expressions replace (char-graphic? char)
		      (< (1+ (window-point-x window)) (window-x-size window)))
		 (region-insert-char! point char)
		 (window-direct-output-insert-char! window char))
		(else
		 (region-insert-char! point char)))
	  (region-insert-char! point char)))))

(define (operation/write-substring port string start end)
  (let ((window (port/state port)))
    (let ((buffer (window-buffer window))
	  (point (window-point window)))
      (if (and (null? (cdr (buffer-windows buffer)))
	       (line-end? point)
	       (buffer-auto-save-modified? buffer)
	       (or (not (window-needs-redisplay? window))
		   (window-direct-update! window #f))
	       (let loop ((i (- end 1)))
		 (or (< i start)
		     (let ((char (string-ref string i)))
		       (and (not (char=? char #\newline))
			    (not (char=? char #\tab))
			    (let ((image (window-char->image window char)))
			      (and (= (string-length image) 1)
				   (char=? (string-ref image 0) char)
				   (loop (- i 1))))))))
	       (< (+ (- end start) (window-point-x window))
		  (window-x-size window)))
	  (window-direct-output-insert-substring! window string start end)
	  (region-insert-substring! point string start end)))))

(define (operation/flush-output port)
  (let ((window (port/state port)))
    (if (window-needs-redisplay? window)
	(window-direct-update! window #f))))

(define (operation/x-size port)
  (window-x-size (port/state port)))

(define (operation/print-self state port)
  (unparse-string state "to window ")
  (unparse-object state (port/state port)))

(define window-output-port-type
  (make-port-type `((FLUSH-OUTPUT ,operation/flush-output)
		    (FRESH-LINE ,operation/fresh-line)
		    (PRINT-SELF ,operation/print-self)
		    (WRITE-CHAR ,operation/write-char)
		    (WRITE-SUBSTRING ,operation/write-substring)
		    (X-SIZE ,operation/x-size))
		  #f))