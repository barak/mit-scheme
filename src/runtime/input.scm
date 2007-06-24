#| -*-Scheme-*-

$Id: input.scm,v 14.35 2007/06/06 19:42:42 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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
  ((port/operation/char-ready? port) port))

(define (input-port/read-char port)
  ((port/operation/read-char port) port))

(define (input-port/unread-char port char)
  ((port/operation/unread-char port) port char))

(define (input-port/peek-char port)
  ((port/operation/peek-char port) port))

(define (input-port/read-string! port string)
  (input-port/read-substring! port string 0 (string-length string)))

(define (input-port/read-substring! port string start end)
  (if (fix:< start end)
      ((port/operation/read-substring port) port string start end)
      0))

(define (input-port/read-wide-string! port string)
  (input-port/read-wide-substring! port string 0 (wide-string-length string)))

(define (input-port/read-wide-substring! port string start end)
  (if (fix:< start end)
      ((port/operation/read-wide-substring port) port string start end)
      0))

(define (input-port/read-external-string! port string)
  (input-port/read-external-substring!
   port
   string
   0
   (external-string-length string)))

(define (input-port/read-external-substring! port string start end)
  (if (< start end)
      ((port/operation/read-external-substring port) port string start end)
      0))

(define (input-port/read-line port)
  (port/with-input-blocking-mode port 'BLOCKING
    (lambda ()
      (let loop ((a (make-accum 128)))
	(let ((char (input-port/read-char port)))
	  (cond ((eof-object? char)
		 (if (fix:> (accum-count a) 0)
		     (accum->string a)
		     char))
		((char=? char #\newline) (accum->string a))
		(else (loop (accum char a)))))))))

(define (input-port/read-string port delimiters)
  (port/with-input-blocking-mode port 'BLOCKING
    (lambda ()
      (let loop ((a (make-accum 128)))
	(let ((char (input-port/read-char port)))
	  (cond ((eof-object? char)
		 (if (fix:> (accum-count a) 0)
		     (accum->string a)
		     char))
		((char-set-member? delimiters char)
		 (input-port/unread-char port char)
		 (accum->string a))
		(else
		 (loop (accum char a)))))))))

(define (input-port/discard-chars port delimiters)
  (port/with-input-blocking-mode port 'BLOCKING
    (lambda ()
      (let loop ()
	(let ((char (input-port/read-char port)))
	  (cond ((eof-object? char)
		 unspecific)
		((char-set-member? delimiters char)
		 (input-port/unread-char port char))
		(else
		 (loop))))))))

(define-integrable (make-accum n)
  (cons (make-string n) 0))

(define (accum char a)
  (if (fix:= (cdr a) (string-length (car a)))
      (let ((s* (make-string (fix:* (cdr a) 2))))
	(substring-move! (car a) 0 (cdr a) s* 0)
	(set-car! a s*)))
  (string-set! (car a) (cdr a) char)
  (set-cdr! a (fix:+ (cdr a) 1))
  a)

(define-integrable (accum->string a)
  (set-string-maximum-length! (car a) (cdr a))
  (car a))

(define-integrable (accum-count a)
  (cdr a))

(define (make-eof-object port)
  port
  (object-new-type (ucode-type constant) 6))

(define (eof-object? object)
  (eq? object (object-new-type (ucode-type constant) 6)))

;;;; High level

(define (char-ready? #!optional port interval)
  (let ((port (optional-input-port port 'CHAR-READY?))
	(interval
	 (if (default-object? interval)
	     0
	     (begin
	       (guarantee-exact-nonnegative-integer interval 'CHAR-READY?)
	       interval))))
    (if (positive? interval)
	(let ((timeout (+ (real-time-clock) interval)))
	  (let loop ()
	    (cond ((input-port/char-ready? port) #t)
		  ((< (real-time-clock) timeout) (loop))
		  (else #f))))
	(input-port/char-ready? port))))

(define (read-char #!optional port)
  (let ((port (optional-input-port port 'READ-CHAR)))
    (let loop ()
      (or (input-port/read-char port)
	  (loop)))))

(define (unread-char char #!optional port)
  (guarantee-char char 'UNREAD-CHAR)
  (input-port/unread-char (optional-input-port port 'UNREAD-CHAR) char))

(define (peek-char #!optional port)
  (let ((port (optional-input-port port 'PEEK-CHAR)))
    (let loop ()
      (or (input-port/peek-char port)
	  (loop)))))

(define (read-char-no-hang #!optional port)
  (let ((port (optional-input-port port 'READ-CHAR-NO-HANG)))
    (if (input-port/char-ready? port)
	(input-port/read-char port)
	(let ((eof? (port/operation port 'EOF?)))
	  (and eof?
	       (eof? port)
	       (make-eof-object port))))))

(define (read-string delimiters #!optional port)
  (input-port/read-string (optional-input-port port 'READ-STRING) delimiters))

(define (read #!optional port environment)
  (parse-object (optional-input-port port 'READ) environment))

(define (read-file pathname #!optional environment)
  (call-with-input-file (pathname-default-version pathname 'NEWEST)
    (lambda (port)
      (let ((environment
	     (if (default-object? environment)
		 (nearest-repl/environment)
		 environment)))
	(let loop ((sexps '()))
	  (let ((sexp (read port environment)))
	    (if (eof-object? sexp)
		(reverse! sexps)
		(loop (cons sexp sexps)))))))))

(define (read-line #!optional port)
  (input-port/read-line (optional-input-port port 'READ-LINE)))

(define (read-string! string #!optional port)
  (let ((port (optional-input-port port 'READ-STRING!)))
    (cond ((string? string)
	   (input-port/read-string! port string))
	  ((wide-string? string)
	   (input-port/read-wide-string! port string))
	  ((external-string? string)
	   (input-port/read-external-string! port string))
	  (else
	   (error:wrong-type-argument string "string" 'READ-STRING!)))))

(define (read-substring! string start end #!optional port)
  (let ((port (optional-input-port port 'READ-STRING!)))
    (cond ((string? string)
	   (input-port/read-substring! port string start end))
	  ((wide-string? string)
	   (input-port/read-wide-substring! port string start end))
	  ((external-string? string)
	   (input-port/read-external-substring! port string start end))
	  (else
	   (error:wrong-type-argument string "string" 'READ-SUBSTRING!)))))

(define (optional-input-port port caller)
  (if (default-object? port)
      (current-input-port)
      (guarantee-input-port port caller)))