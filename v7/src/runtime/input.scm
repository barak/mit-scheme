#| -*-Scheme-*-

$Id: input.scm,v 14.24 2003/07/30 17:06:23 cph Exp $

Copyright 1986,1987,1988,1989,1990,1991 Massachusetts Institute of Technology
Copyright 1992,1993,1997,1999,2002,2003 Massachusetts Institute of Technology

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

(define <eof-object> (make-record-type '<EOF-OBJECT> '()))
(define eof-object? (record-predicate <eof-object>))
(define eof-object (make-eof-object))
(define (make-eof-object port) port eof-object)

;;;; Input Procedures

(define (char-ready? #!optional port interval)
  (input-port/char-ready? (if (default-object? port)
			      (current-input-port)
			      (guarantee-input-port port 'CHAR-READY?))
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
	     (guarantee-input-port port 'PEEK-CHAR))))
    (let loop ()
      (or (input-port/peek-char port)
	  (loop)))))

(define (read-char #!optional port)
  (let ((port
	 (if (default-object? port)
	     (current-input-port)
	     (guarantee-input-port port 'READ-CHAR))))
    (let loop ()
      (or (input-port/read-char port)
	  (loop)))))

(define (read-char-no-hang #!optional port)
  (let ((port
	 (if (default-object? port)
	     (current-input-port)
	     (guarantee-input-port port 'READ-CHAR-NO-HANG))))
    (if (input-port/char-ready? port 0)
	(input-port/read-char port)
	(let ((eof? (port/operation port 'EOF?)))
	  (and eof?
	       (eof? port)
	       eof-object)))))

(define (read-string delimiters #!optional port)
  (input-port/read-string (if (default-object? port)
			      (current-input-port)
			      (guarantee-input-port port 'READ-STRING))
			  delimiters))

(define (read #!optional port parser-table)
  (parse-object (if (default-object? port)
		    (current-input-port)
		    (guarantee-input-port port 'READ))
		(if (default-object? parser-table)
		    (current-parser-table)
		    parser-table)))

(define (read-line #!optional port)
  (input-port/read-line (if (default-object? port)
			    (current-input-port)
			    (guarantee-input-port port 'READ-LINE))))

(define (read-string! string #!optional port)
  (input-port/read-string! (if (default-object? port)
			       (current-input-port)
			       (guarantee-input-port port 'READ-STRING!))
			   string))

(define (read-substring! string start end #!optional port)
  (input-port/read-substring! (if (default-object? port)
				  (current-input-port)
				  (guarantee-input-port port 'READ-SUBSTRING!))
			      string start end))