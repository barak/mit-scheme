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

;;;; HTTP 1.0 client
;;; package: (runtime http-client)

(declare (usual-integrations))

(define (http-get uri headers)
  (http-client-exchange "GET" (->uri uri) headers #f))

(define (http-head uri headers)
  (http-client-exchange "HEAD" (->uri uri) headers #f))

(define (http-post uri headers body)
  (http-client-exchange "POST" (->uri uri) headers body))

(define (http-client-exchange method uri headers body)
  (let ((request (http-client-request method uri headers body)))
    (call-with-http-client-socket uri
      (lambda (port)
	(write-http-request request port)
	(read-http-response request port)))))

(define (call-with-http-client-socket uri callee)
  (let ((port
	 (let ((auth (uri-authority uri)))
	   (open-binary-tcp-stream-socket (uri-authority-host auth)
					  (or (uri-authority-port auth) 80)))))
    (let ((value (callee port)))
      (close-port port)
      value)))

(define (http-client-request method uri headers body)
  (guarantee absolute-uri? uri)
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

    (list (add 'accept
	       (lambda ()
		 `((,(make-mime-type 'application 'xhtml+xml))
		   (,(make-mime-type 'text 'xhtml) (q . "0.9"))
		   (,(make-mime-type 'text 'plain) (q . "0.5"))
		   (text (q . "0.1")))))
	  (add 'accept-charset (lambda () '((us-ascii) (iso-8859-1) (utf-8))))
	  (add 'accept-encoding (lambda () '((identity))))
	  (add 'accept-language (lambda () `((en-us) (en (q . "0.9")))))
	  (modify 'connection
		  (lambda (value change no-change)
		    (if (memq 'te value)
			(no-change)
			(change (cons 'te value))))
		  '())
	  (add 'date
	       (lambda ()
		 (universal-time->global-decoded-time (get-universal-time))))
	  (lambda (method uri headers)
	    method
	    (if (http-header 'host headers #f)
		headers
		(cons (make-http-header
		       'host
		       (let ((authority (uri-authority uri)))
			 (cons (uri-authority-host authority)
			       (uri-authority-port authority))))
		      headers)))
	  (modify 'te
		  (lambda (value change no-change)
		    (if (assq 'trailers value)
			(no-change)
			(change (cons (list 'trailers) value))))
		  '())
	  (add 'user-agent (lambda () default-http-user-agent)))))