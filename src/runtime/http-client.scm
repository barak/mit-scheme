#| -*-Scheme-*-

$Id: eaaf6b4e52539752e8e31125bd6b024b895760b2 $

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
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; HTTP 1.0 client
;;; package: (runtime http-client)

(declare (usual-integrations))

(define (http-get uri headers)
  (http-client-exchange "GET" uri headers ""))

(define (http-head uri headers)
  (http-client-exchange "HEAD" uri headers ""))

(define (http-post uri headers body)
  (http-client-exchange "POST" uri headers body))

(define (http-client-exchange method uri headers body)
  (let ((request (http-client-request method uri headers body)))
    (call-with-http-client-socket uri
      (lambda (port)
	(write-http-request request port)
	(read-http-response request port)))))

(define (call-with-http-client-socket uri callee)
  (let ((port
	 (let ((authority (uri-authority uri)))
	   (open-tcp-stream-socket (uri-authority-host authority)
				   (or (uri-authority-port authority) 80)))))
    (let ((value (callee port)))
      (close-port port)
      value)))

(define (http-client-request method uri headers body)
  (guarantee-absolute-uri uri)
  (make-http-request method
		     (make-uri #f
			       #f
			       (uri-path uri)
			       (uri-query uri)
			       (uri-fragment uri))
		     http-version:1.1
		     (add-default-headers method uri headers)
		     body))

(define (add-default-headers method uri headers)
  (let loop
      ((ops default-header-ops)
       (headers (convert-http-headers headers)))
    (if (pair? ops)
	(loop (cdr ops)
	      ((car ops) method uri headers))
	headers)))

(define default-header-ops
  (let ()

    (define ((add name make-value) method uri headers)
      method uri
      (if (http-header name headers #f)
	  headers
	  (cons (make-http-header name (make-value))
		headers)))

    (define ((modify name modifier init-value) method uri headers)
      method uri
      (let ((h (http-header name headers #f)))
	(if h
	    (modifier (http-header-parsed-value h)
		      (lambda (value)
			(replace h
				 (make-http-header name value)
				 headers))
		      (lambda () headers))
	    (modifier init-value
		      (lambda (value)
			(cons (make-http-header name value) headers))
		      (lambda () headers)))))

    (define (replace h h* headers)
      (let loop ((headers headers))
	(if (pair? headers)
	    (if (eq? (car headers) h)
		(cons h* headers)
		(cons (car headers) (loop (cdr headers))))
	    '())))

    (list (add 'ACCEPT
	       (lambda ()
		 `((,(make-mime-type 'APPLICATION 'XHTML+XML))
		   (,(make-mime-type 'TEXT 'XHTML) (Q . "0.9"))
		   (,(make-mime-type 'TEXT 'PLAIN) (Q . "0.5"))
		   (TEXT (Q . "0.1")))))
	  (add 'ACCEPT-CHARSET (lambda () '((US-ASCII) (ISO-8859-1) (UTF-8))))
	  (add 'ACCEPT-ENCODING (lambda () '((IDENTITY))))
	  (add 'ACCEPT-LANGUAGE (lambda () `((EN-US) (EN (Q . "0.9")))))
	  (modify 'CONNECTION
		  (lambda (value change no-change)
		    (if (memq 'TE value)
			(no-change)
			(change (cons 'TE value))))
		  '())
	  (add 'DATE
	       (lambda ()
		 (universal-time->global-decoded-time (get-universal-time))))
	  (lambda (method uri headers)
	    method
	    (if (http-header 'HOST headers #f)
		headers
		(cons (make-http-header
		       'HOST
		       (let ((authority (uri-authority uri)))
			 (cons (uri-authority-host authority)
			       (uri-authority-port authority))))
		      headers)))
	  (modify 'TE
		  (lambda (value change no-change)
		    (if (assq 'TRAILERS value)
			(no-change)
			(change (cons (list 'TRAILERS) value))))
		  '())
	  (add 'USER-AGENT (lambda () default-http-user-agent)))))