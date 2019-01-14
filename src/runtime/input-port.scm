#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; Input
;;; package: (runtime input-port)

(declare (usual-integrations))

;;;; Low level

(define (input-port/char-ready? port)
  ((textual-port-operation/char-ready? port) port))

(define (input-port/read-char port)
  ((textual-port-operation/read-char port) port))

(define (input-port/unread-char port char)
  ((textual-port-operation/unread-char port) port char))

(define (input-port/peek-char port)
  ((textual-port-operation/peek-char port) port))

(define (input-port/read-string! port string)
  (input-port/read-substring! port string 0 (string-length string)))

(define (input-port/read-substring! port string start end)
  (if (< start end)
      ((textual-port-operation/read-substring port) port string start end)
      0))

(define (input-port/read-line port)
  (with-input-port-blocking-mode port 'blocking
    (lambda ()
      (let ((read-char (textual-port-operation/read-char port))
	    (builder (string-builder)))
	(let loop ()
	  (let ((char (read-char port)))
	    (cond ((eof-object? char)
		   (if (builder 'empty?)
		       char
		       (builder)))
		  ((char=? char #\newline)
		   (builder))
		  (else
		   (builder char)
		   (loop)))))))))

(define (input-port/read-string port delimiters)
  (with-input-port-blocking-mode port 'blocking
    (lambda ()
      (let ((read-char (textual-port-operation/read-char port))
	    (builder (string-builder)))
	(let loop ()
	  (let ((char (read-char port)))
	    (cond ((eof-object? char)
		   (if (builder 'empty?)
		       char
		       (builder)))
		  ((char-in-set? char delimiters)
		   (input-port/unread-char port char)
		   (builder))
		  (else
		   (builder char)
		   (loop)))))))))

(define (input-port/discard-chars port delimiters)
  (with-input-port-blocking-mode port 'blocking
    (lambda ()
      (let ((read-char (textual-port-operation/read-char port)))
	(let loop ()
	  (let ((char (read-char port)))
	    (cond ((eof-object? char)
		   unspecific)
		  ((char-in-set? char delimiters)
		   (input-port/unread-char port char))
		  (else
		   (loop)))))))))

(define-integrable (make-eof-object port)
  port
  (eof-object))

(define-integrable (eof-object)
  ((ucode-primitive primitive-object-set-type) (ucode-type constant) 6))

(define-integrable (eof-object? object)
  (eq? object (eof-object)))

(define (input-port/eof? port)
  (let ((eof? (textual-port-operation port 'eof?)))
    (and eof?
	 (eof? port))))

(define (input-port/line port)
  (let ((operation (textual-port-operation port 'input-line)))
    (and operation
	 (operation port))))

;;;; High level

(define (char-ready? #!optional port interval)
  (let ((port (optional-input-port port 'char-ready?))
	(interval
	 (if (default-object? interval)
	     0
	     (begin
	       (guarantee exact-nonnegative-integer? interval 'char-ready?)
	       interval))))
    (if (positive? interval)
	(let ((timeout (+ (real-time-clock) interval)))
	  (let loop ()
	    (cond ((input-port/char-ready? port) #t)
		  ((< (real-time-clock) timeout) (loop))
		  (else #f))))
	(input-port/char-ready? port))))

(define (read-char #!optional port)
  (let ((port (optional-input-port port 'read-char)))
    (let loop ()
      (or (input-port/read-char port)
	  (loop)))))

(define (unread-char char #!optional port)
  (guarantee char? char 'unread-char)
  (input-port/unread-char (optional-input-port port 'unread-char) char))

(define (peek-char #!optional port)
  (let ((port (optional-input-port port 'read-char)))
    (let loop ()
      (or (input-port/peek-char port)
	  (loop)))))

(define (read-char-no-hang #!optional port)
  (let ((port (optional-input-port port 'read-char-no-hang)))
    (and (input-port/char-ready? port)
	 (if (input-port/eof? port)
	     (eof-object)
	     (input-port/read-char port)))))

(define (read-string k #!optional port)
  (if (char-set? k)
      (read-delimited-string k port)
      (r7rs-read-string k port)))

(define (read-delimited-string delimiters #!optional port)
  (input-port/read-string (optional-input-port port 'read-string) delimiters))

(define (r7rs-read-string k #!optional port)
  (guarantee index-fixnum? k 'read-string)
  (let ((port (optional-input-port port 'read-string)))
    (if (fix:> k 0)
	(let ((string (make-string k)))
	  (let ((n (input-port/read-string! port string)))
	    (cond ((not n) n)
		  ((fix:> n 0) (if (fix:< n k) (string-head string n) string))
		  (else (eof-object)))))
	"")))

(define (read #!optional port)
  (read-top-level (optional-input-port port 'read)))

(define (read-file pathname)
  (call-with-input-file (pathname-default-version pathname 'newest)
    (lambda (port)
      (let loop ((sexps '()))
	(let ((sexp (read port)))
	  (if (eof-object? sexp)
	      (reverse! sexps)
	      (loop (cons sexp sexps))))))))

(define (read-line #!optional port)
  (input-port/read-line (optional-input-port port 'read-line)))

(define (read-string! string #!optional port start end)
  (let ((port (optional-input-port port 'read-string!))
	(end
	 (if (default-object? end)
	     (string-length string)
	     (begin
	       (guarantee index-fixnum? end 'read-string!)
	       (if (not (fix:<= end (string-length string)))
		   (error:bad-range-argument end 'read-string!))
	       end))))
    (let ((start
	   (if (default-object? start)
	       0
	       (begin
		 (guarantee index-fixnum? start 'read-string!)
		 (if (not (fix:<= start end))
		     (error:bad-range-argument start 'read-string!))
		 start))))
      (input-port/read-substring! port string start end))))


(define (read-substring! string start end #!optional port)
  (read-string! string port start end))

(define (optional-input-port port caller)
  (let ((port (if (default-object? port) (current-input-port) port)))
    (guarantee textual-input-port? port caller)
    (if (not (textual-input-port-open? port))
	(error:bad-range-argument port caller))
    port))