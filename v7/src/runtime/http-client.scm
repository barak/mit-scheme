#| -*-Scheme-*-

$Id: http-client.scm,v 14.7 2008/09/15 05:15:08 cph Exp $

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
  (run-client-method '|GET| uri headers ""))

(define (http-head uri headers)
  (run-client-method '|HEAD| uri headers ""))

(define (http-post uri headers body)
  (run-client-method '|POST| uri headers body))

(define (run-client-method method uri headers body)
  (guarantee-absolute-uri uri)
  (let* ((authority (uri-authority uri))
	 (port
	  (open-tcp-stream-socket (uri-authority-host authority)
				  (or (uri-authority-port authority) 80))))
    (let ((request
	   (make-http-request method
			      (make-uri #f
					#f
					(uri-path uri)
					(uri-query uri)
					(uri-fragment uri))
			      http-version:1.0
			      (add-default-headers headers authority)
			      body)))
      (write-http-request request port)
      (let ((response (read-http-response request port)))
	(close-port port)
	response))))

(define (add-default-headers headers authority)
  (let ((headers (convert-http-headers headers)))
    (cons (make-http-header 'HOST (host-string authority))
	  (if (http-header 'USER-AGENT headers #f)
	      headers
	      (cons (make-http-header 'USER-AGENT default-http-user-agent)
		    headers)))))

(define (host-string authority)
  (let ((host (uri-authority-host authority))
	(port (uri-authority-port authority)))
    (if port
	(string-append host ":" (number->string port))
	host)))