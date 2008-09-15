#| -*-Scheme-*-

$Id: http-syntax.scm,v 1.1 2008/09/15 05:15:12 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; HTTP syntax
;;; package: (runtime http-syntax)

(declare (usual-integrations))

;;;; Version

(define (http-version? object)
  (and (pair? object)
       (exact-nonnegative-integer? (car object))
       (exact-nonnegative-integer? (cdr object))))

(define-guarantee http-version "HTTP version")

(define (make-http-version major minor) (cons major minor))
(define (http-version-major v) (car v))
(define (http-version-minor v) (cdr v))

(define (http-version=? v1 v2)
  (and (= (car v1) (car v2))
       (= (cdr v1) (cdr v2))))

(define (http-version<? v1 v2)
  (or (< (car v1) (car v2))
      (and (= (car v1) (car v2))
	   (< (cdr v1) (cdr v2)))))

(define parse-http-version
  (*parser
   (encapsulate* make-http-version
     (seq "HTTP/"
	  (map string->number
	       (match (+ (char-set char-set:numeric))))
	  "."
	  (map string->number
	       (match (+ (char-set char-set:numeric))))))))

(define (write-http-version version port)
  (write-string "HTTP/" port)
  (write (car version) port)
  (write-string "." port)
  (write (cdr version) port))

;;;; Status

(define (http-status? object)
  (and (exact-nonnegative-integer? object)
       (< object 1000)))

(define-guarantee http-status "HTTP status code")

(define (http-status-major status)
  (modulo status 100))

(define parse-http-status
  (*parser
   (map string->number
	(match (seq (char-set char-set:numeric)
		    (char-set char-set:numeric)
		    (char-set char-set:numeric))))))

(define (write-http-status object port)
  (write-string (string-pad-left (number->string object) 3 #\0) port))

;;;; Header

(define-record-type <http-header>
    (%make-http-header name value)
    http-header?
  (name http-header-name)
  (value http-header-value))

(define-guarantee http-header "HTTP header field")

(define (make-http-header name value)
  (guarantee-http-token name 'MAKE-HTTP-HEADER)
  (guarantee-http-text value 'MAKE-HTTP-HEADER)
  (%make-http-header name value))

(define (convert-http-headers headers #!optional caller)
  (guarantee-list headers caller)
  (map (lambda (header)
	 (cond ((http-header? header)
		header)
	       ((and (pair? header)
		     (http-token? (car header))
		     (string? (cdr header)))
		(make-http-header (car header) (cdr header)))
	       ((and (pair? header)
		     (http-token? (car header))
		     (pair? (cdr header))
		     (string? (cadr header))
		     (null? (cddr header)))
		(make-http-header (car header) (cadr header)))
	       (else
		(error:not-http-header header caller))))
       headers))

(define (guarantee-http-headers object #!optional caller)
  (guarantee-list-of-type object http-header? "HTTP headers" caller))

(define (http-header name headers error?)
  (let ((h
	 (find (lambda (header)
		 (eq? (http-header-name header) name))
	       headers)))
    (if (and (not h) error?)
	(error:bad-range-argument name 'HTTP-HEADER))
    h))

(define (read-http-headers port)
  (map (lambda (h)
	 (make-http-header (rfc2822-header-name h)
			   (rfc2822-header-value h)))
       (read-rfc2822-headers port)))

(define (write-http-headers headers port)
  (guarantee-http-headers headers 'WRITE-HTTP-HEADERS)
  (write-rfc2822-headers (map (lambda (h)
				(make-rfc2822-header (http-header-name h)
						     (http-header-value h)))
			      headers)
			 port))

;;;; Token

(define (http-token? object)
  (and (interned-symbol? object)
       (string-is-http-token? (symbol-name object))))

(define-guarantee http-token "HTTP token")

(define (write-http-token token port)
  (write-string (symbol-name token) port))

(define (string-is-http-token? string)
  (*match-string match-http-token string))

(define parse-http-token
  (*parser (map intern (match match-http-token))))

(define match-http-token
  (*matcher (+ (char-set char-set:http-token))))

;;;; Text

(define (http-text? object)
  (string? object))

(define-guarantee http-text "HTTP text")

(define (write-text string port)
  (if (string-is-http-token? string)
      (write-string string port)
      (write-quoted-string string port)))

(define (write-quoted-string string port)
  (write-char #\" port)
  (%write-with-quotations string char-set:http-qdtext port)
  (write-char #\" port))

(define (write-comment string port)
  (write-char #\( port)
  (%write-with-quotations string char-set:http-ctext port)
  (write-char #\) port))

(define (%write-with-quotations string unquoted port)
  (let ((n (string-length string)))
    (do ((i 0 (fix:+ i 1)))
	((not (fix:< i n)))
      (let ((char (string-ref string i)))
	(if (not (char-set-member? unquoted char))
	    (write-char #\\ port))
	(write-char char port)))))


(define http-version:1.0)
(define http-version:1.1)

(define char-set:http-separators)
(define char-set:http-token)
(define char-set:http-text)
(define char-set:http-ctext)
(define char-set:http-qdtext)
(define char-set:alpha)
(define default-http-user-agent)

(define (initialize-package!)
  (set! http-version:1.0 (make-http-version 1 0))
  (set! http-version:1.1 (make-http-version 1 1))
  (set! char-set:http-separators
	(string->char-set "()<>@,;:\\\"/[]?={} \t"))
  (set! char-set:http-token
	(char-set-difference char-set:ascii
			     (char-set-union char-set:ctls
					     char-set:http-separators)))
  (set! char-set:http-text
	(char-set-invert char-set:ctls))
  (set! char-set:http-ctext
	(char-set-difference char-set:http-text
			     (char-set #\( #\))))
  (set! char-set:http-qdtext
	(char-set-difference char-set:http-text
			     (char-set #\")))
  (set! char-set:alpha
	(char-set-union (ascii-range->char-set #x41 #x5B)
			(ascii-range->char-set #x61 #x7B)))
  (set! default-http-user-agent
	(call-with-output-string
	  (lambda (output)
	    (write-string "MIT-GNU-Scheme/" output)
	    (let ((input
		   (open-input-string
		    (get-subsystem-version-string "release"))))
	      (let loop ()
		(let ((char (read-char input)))
		  (if (not (eof-object? char))
		      (begin
			(write-char (if (char-set-member? char-set:http-token
							  char)
					char
					#\_)
				    output)
			(loop)))))))))
  unspecific)