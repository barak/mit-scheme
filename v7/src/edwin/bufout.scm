#| -*-Scheme-*-

$Id: bufout.scm,v 1.17 2004/02/16 05:42:55 cph Exp $

Copyright 1989,1991,1992,1993,1998,1999 Massachusetts Institute of Technology
Copyright 2004 Massachusetts Institute of Technology

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

;;;; Buffer Output Ports
;;; Package: (edwin buffer-output-port)

(declare (usual-integrations))

(define (with-output-to-mark mark thunk)
  (call-with-output-mark mark
    (lambda (port)
      (with-output-to-port port thunk))))

(define (call-with-output-mark mark procedure)
  (let ((port (mark->output-port mark)))
    (let ((value (procedure port)))
      (operation/close port)
      value)))

(define (mark->output-port mark #!optional buffer)
  (make-port mark-output-port-type
	     (cons (mark-left-inserting-copy mark)
		   (if (default-object? buffer)
		       #f
		       buffer))))

(define (output-port->mark port)
  (mark-temporary-copy (port/mark port)))

(define-integrable (port/mark port)
  (car (port/state port)))

(define-integrable (port/buffer port)
  (cdr (port/state port)))

(define (operation/flush-output port)
  (let ((mark (port/mark port))
	(buffer (port/buffer port)))
    (if buffer
	(for-each (if (mark= mark (buffer-point buffer))
		      (lambda (window)
			(set-window-point! window mark)
			(window-direct-update! window #f))
		      (lambda (window)
			(window-direct-update! window #f)))
		  (buffer-windows buffer)))))

(define (operation/write-self port output)
  (write-string " to buffer at " output)
  (write (port/mark port) output))

(define (operation/write-char port char)
  (guarantee-8-bit-char char)
  (region-insert-char! (port/mark port) char)
  1)

(define (operation/write-substring port string start end)
  (region-insert-substring! (port/mark port) string start end)
  (fix:- end start))

(define (operation/close port)
  (mark-temporary! (port/mark port)))

(define (operation/x-size port)
  (mark-x-size (port/mark port)))

(define mark-output-port-type
  (make-port-type `((CLOSE ,operation/close)
		    (FLUSH-OUTPUT ,operation/flush-output)
		    (WRITE-CHAR ,operation/write-char)
		    (WRITE-SELF ,operation/write-self)
		    (WRITE-SUBSTRING ,operation/write-substring)
		    (X-SIZE ,operation/x-size))
		  #f))