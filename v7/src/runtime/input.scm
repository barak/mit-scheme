#| -*-Scheme-*-

$Id: input.scm,v 14.18 1997/02/21 06:08:15 cph Exp $

Copyright (c) 1988-97 Massachusetts Institute of Technology

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

(define (read-string! string #!optional start end port)
  (input-port/read-substring! (if (default-object? port)
				  (current-input-port)
				  (guarantee-input-port port))
			      string
			      (if (default-object? start)
				  0
				  start)
			      (if (default-object? end)
				  (string-length string)
				  end)))