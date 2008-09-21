#| -*-Scheme-*-

$Id: http-client.scm,v 14.9 2008/09/21 07:35:48 cph Exp $

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
    (let ((port
	   (let ((authority (uri-authority uri)))
	     (open-tcp-stream-socket (uri-authority-host authority)
				     (or (uri-authority-port authority) 80)))))
      (write-http-request request port)
      (let ((response (read-http-response request port)))
	(close-port port)
	response))))

(define (http-client-request method uri headers body)
  (guarantee-absolute-uri uri)
  (make-http-request method
		     (make-uri #f
			       #f
			       (uri-path uri)
			       (uri-query uri)
			       (uri-fragment uri))
		     http-version:1.1
		     (add-default-headers headers (uri-authority uri))
		     body))

(define (add-default-headers headers authority)
  (let ((headers (convert-http-headers headers)))
    (let ((optional
	   (lambda (name value)
	     (if (http-header name headers #f)
		 '()
		 (list (make-http-header name value))))))
      `(,(make-http-header 'DATE
			   (universal-time->global-decoded-time
			    (get-universal-time)))
	,@(optional 'ACCEPT
		    `((,(make-mime-type 'APPLICATION 'XHTML+XML))
		      (,(make-mime-type 'TEXT 'XHTML) (Q . "0.9"))
		      (,(make-mime-type 'TEXT 'PLAIN) (Q . "0.5"))
		      (TEXT (Q . "0.1"))))
	,@(optional 'ACCEPT-CHARSET '((US-ASCII) (ISO-8859-1) (UTF-8)))
	,@(optional 'ACCEPT-ENCODING '((IDENTITY)))
	,@(optional 'ACCEPT-LANGUAGE `((EN-US) (EN (Q . "0.9"))))
	,(make-http-header 'HOST
			   (cons (uri-authority-host authority)
				 (uri-authority-port authority)))
	,@(optional 'USER-AGENT default-http-user-agent)
	,@headers))))
