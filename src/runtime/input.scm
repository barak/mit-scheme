#| -*-Scheme-*-

$Id: input.scm,v 14.20 1999/12/21 19:05:13 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; Input
;;; package: (runtime input-port)

(declare (usual-integrations))

;;;; Input Ports

(define (input-port/char-ready? port interval)
  ((input-port/operation/char-ready? port) port interval))

(define (input-port/peek-char port)
  ((input-port/operation/peek-char port) port))

(define (input-port/read-char port)
  ((input-port/operation/read-char port) port))

(define (input-port/discard-char port)
  ((input-port/operation/discard-char port) port))

(define (input-port/read-string port delimiters)
  ((input-port/operation/read-string port) port delimiters))

(define (input-port/discard-chars port delimiters)
  ((input-port/operation/discard-chars port) port delimiters))

(define (input-port/read-substring! port string start end)
  ((input-port/operation/read-substring port) port string start end))

(define (input-port/read-string! port string)
  (input-port/read-substring! port string 0 (string-length string)))

(define (input-port/read-line port)
  (let ((line (input-port/read-string port char-set:newline)))
    ;; Discard delimiter, if any -- this is a no-op at EOF.
    (input-port/discard-char port)
    line))

(define eof-object
  "EOF Object")

(define (eof-object? object)
  (eq? object eof-object))

(define (make-eof-object port)
  port
  eof-object)

;;;; Input Procedures

(define (char-ready? #!optional port interval)
  (input-port/char-ready? (if (default-object? port)
			      (current-input-port)
			      (guarantee-input-port port))
			  (if (default-object? interval)
			      0
			      (begin
				(if (not (exact-nonnegative-integer? interval))
				    (error:wrong-type-argument interval
							       false
							       'CHAR-READY?))
				interval))))

(define (peek-char #!optional port)
  (let ((port
	 (if (default-object? port)
	     (current-input-port)
	     (guarantee-input-port port))))
    (let loop ()
      (or (input-port/peek-char port)
	  (loop)))))

(define (read-char #!optional port)
  (let ((port
	 (if (default-object? port)
	     (current-input-port)
	     (guarantee-input-port port))))
    (let loop ()
      (or (input-port/read-char port)
	  (loop)))))

(define (read-char-no-hang #!optional port)
  (let ((port
	 (if (default-object? port)
	     (current-input-port)
	     (guarantee-input-port port))))
    (if (input-port/char-ready? port 0)
	(input-port/read-char port)
	(let ((eof? (port/operation port 'EOF?)))
	  (and eof?
	       (eof? port)
	       eof-object)))))

(define (read-string delimiters #!optional port)
  (input-port/read-string (if (default-object? port)
			      (current-input-port)
			      (guarantee-input-port port))
			  delimiters))

(define (read #!optional port parser-table)
  (parse-object (if (default-object? port)
		    (current-input-port)
		    (guarantee-input-port port))
		(if (default-object? parser-table)
		    (current-parser-table)
		    parser-table)))

(define (read-line #!optional port)
  (input-port/read-line (if (default-object? port)
			    (current-input-port)
			    (guarantee-input-port port))))

(define (read-string! string #!optional port)
  (input-port/read-string! (if (default-object? port)
			       (current-input-port)
			       (guarantee-input-port port))
			   string))

(define (read-substring! string start end #!optional port)
  (input-port/read-substring! (if (default-object? port)
				  (current-input-port)
				  (guarantee-input-port port))
			      string start end))