#| -*-Scheme-*-

$Id: output.scm,v 14.24 2002/11/20 19:46:21 cph Exp $

Copyright (c) 1988-2001 Massachusetts Institute of Technology

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

;;;; Output
;;; package: (runtime output-port)

(declare (usual-integrations))

;;;; Output Ports

(define (output-port/write-char port char)
  ((output-port/operation/write-char port) port char))

(define (output-port/write-string port string)
  (output-port/write-substring port string 0 (xstring-length string)))

(define (output-port/write-substring port string start end)
  ((output-port/operation/write-substring port) port string start end))

(define (output-port/write-object port object)
  (unparse-object/top-level object port #t (current-unparser-table)))

(define (output-port/fresh-line port)
  ((output-port/operation/fresh-line port) port))

(define (output-port/flush-output port)
  ((output-port/operation/flush-output port) port))

(define (output-port/discretionary-flush port)
  ((output-port/operation/discretionary-flush port) port))

(define (output-port/x-size port)
  (or (let ((operation (port/operation port 'X-SIZE)))
	(and operation
	     (operation port)))
      80))

(define (output-port/y-size port)
  (let ((operation (port/operation port 'Y-SIZE)))
    (and operation
	 (operation port))))

;;;; Output Procedures

(define (newline #!optional port)
  (let ((port
	 (if (default-object? port)
	     (current-output-port)
	     (guarantee-output-port port))))
    (output-port/write-char port #\newline)
    (output-port/discretionary-flush port)))

(define (fresh-line #!optional port)
  (let ((port
	 (if (default-object? port)
	     (current-output-port)
	     (guarantee-output-port port))))
    (output-port/fresh-line port)
    (output-port/discretionary-flush port)))

(define (write-char char #!optional port)
  (let ((port
	 (if (default-object? port)
	     (current-output-port)
	     (guarantee-output-port port))))
    (output-port/write-char port char)
    (output-port/discretionary-flush port)))

(define (write-string string #!optional port)
  (let ((port
	 (if (default-object? port)
	     (current-output-port)
	     (guarantee-output-port port))))
    (output-port/write-string port string)
    (output-port/discretionary-flush port)))

(define (write-substring string start end #!optional port)
  (let ((port
	 (if (default-object? port)
	     (current-output-port)
	     (guarantee-output-port port))))
    (output-port/write-substring port string start end)
    (output-port/discretionary-flush port)))

(define (wrap-custom-operation-0 operation-name)
  (lambda (#!optional port)
    (let ((port
	   (if (default-object? port)
	       (current-output-port)
	       (guarantee-output-port port))))
      (let ((operation (port/operation port operation-name)))
	(if operation
	    (begin
	      (operation port)
	      (output-port/discretionary-flush port)))))))

(define beep (wrap-custom-operation-0 'BEEP))
(define clear (wrap-custom-operation-0 'CLEAR))

(define (display object #!optional port unparser-table)
  (let ((port
	 (if (default-object? port)
	     (current-output-port)
	     (guarantee-output-port port)))
	(unparser-table
	 (if (default-object? unparser-table)
	     (current-unparser-table)
	     (guarantee-unparser-table unparser-table 'DISPLAY))))
    (if (string? object)
	(output-port/write-string port object)
	(unparse-object/top-level object port #f unparser-table))
    (output-port/discretionary-flush port)))

(define (write object #!optional port unparser-table)
  (let ((port
	 (if (default-object? port)
	     (current-output-port)
	     (guarantee-output-port port)))
	(unparser-table
	 (if (default-object? unparser-table)
	     (current-unparser-table)
	     (guarantee-unparser-table unparser-table 'WRITE))))
    (unparse-object/top-level object port #t unparser-table)
    (output-port/discretionary-flush port)))

(define (write-line object #!optional port unparser-table)
  (let ((port
	 (if (default-object? port)
	     (current-output-port)
	     (guarantee-output-port port)))
	(unparser-table
	 (if (default-object? unparser-table)
	     (current-unparser-table)
	     (guarantee-unparser-table unparser-table 'WRITE-LINE))))
    (unparse-object/top-level object port #t unparser-table)
    (output-port/write-char port #\newline)
    (output-port/discretionary-flush port)))

(define (flush-output #!optional port)
  (output-port/flush-output
   (if (default-object? port)
       (current-output-port)
       (guarantee-output-port port))))