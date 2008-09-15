#| -*-Scheme-*-

$Id: httpio.scm,v 14.7 2008/09/15 05:15:17 cph Exp $

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

;;;; HTTP I/O
;;; package: (runtime http-i/o)

;;; Assumptions:
;;;   Transfer coding is assumed to always be "identity".

(declare (usual-integrations))

(define-record-type <http-request>
    (%make-http-request method uri version headers body)
    http-request?
  (method http-request-method)
  (uri http-request-uri)
  (version http-request-version)
  (headers http-request-headers)
  (body http-request-body))

(define-guarantee http-request "HTTP request")

(define (make-http-request method uri version headers body)
  (guarantee-http-token method 'MAKE-HTTP-REQUEST)
  (guarantee-http-request-uri uri 'MAKE-HTTP-REQUEST)
  (guarantee-http-version version 'MAKE-HTTP-REQUEST)
  (receive (headers body)
      (guarantee-headers&body headers body 'MAKE-HTTP-REQUEST)
    (%make-http-request method uri version headers body)))

(define-record-type <http-response>
    (%make-http-response version status reason headers body)
    http-response?
  (version http-response-version)
  (status http-response-status)
  (reason http-response-reason)
  (headers http-response-headers)
  (body http-response-body))

(define-guarantee http-response "HTTP response")

(define (make-http-response version status reason headers body)
  (guarantee-http-version version 'MAKE-HTTP-RESPONSE)
  (guarantee-http-status status 'MAKE-HTTP-RESPONSE)
  (guarantee-http-text reason 'MAKE-HTTP-RESPONSE)
  (receive (headers body)
      (guarantee-headers&body headers body 'MAKE-HTTP-RESPONSE)
    (%make-http-response version status reason headers body)))

(define (guarantee-headers&body headers body caller)
  (guarantee-http-headers headers caller)
  (if body
      (begin
	(guarantee-string body caller)
	(let ((n (%get-content-length headers))
	      (m (vector-8b-length body)))
	  (if n
	      (begin
		(if (not (= n m))
		    (error:bad-range-argument body caller))
		(values headers body))
	      (values (cons (make-http-header 'CONTENT-LENGTH
					      (number->string m))
			    headers)
		      body))))
      (values headers "")))

(define (simple-http-request? object)
  (and (http-request? object)
       (not (http-request-version object))))

(define-guarantee simple-http-request "simple HTTP request")

(define (make-simple-http-request uri)
  (guarantee-simple-http-request-uri uri 'MAKE-HTTP-REQUEST)
  (%make-http-request '|GET| uri #f '() ""))

(define (simple-http-response? object)
  (and (http-response? object)
       (not (http-response-version object))))

(define-guarantee simple-http-response "simple HTTP response")

(define (make-simple-http-response body)
  (guarantee-string body 'MAKE-SIMPLE-HTTP-RESPONSE)
  (%make-http-response #f 200 (http-status-description 200) '() body))

(define (http-message? object)
  (or (http-request? object)
      (http-response? object)))

(define-guarantee http-message "HTTP message")

(define (http-message-headers message)
  (cond ((http-request? message) (http-request-headers message))
	((http-response? message) (http-response-headers message))
	(else (error:not-http-message message 'HTTP-MESSAGE-HEADERS))))

(define (http-message-body message)
  (cond ((http-request? message) (http-request-body message))
	((http-response? message) (http-response-body message))
	(else (error:not-http-message message 'HTTP-MESSAGE-BODY))))

(define (http-request-uri? object)
  (or (simple-http-request-uri? object)
      (absolute-uri? object)
      (and (string? object)
	   (string=? object "*"))
      (uri-authority? object)))

(define-guarantee http-request-uri "HTTP URI")

(define (simple-http-request-uri? object)
  (and (uri? object)
       (not (uri-scheme object))
       (not (uri-authority object))
       (uri-path-absolute? (uri-path object))))

(define-guarantee simple-http-request-uri "simple HTTP URI")

;;;; Output

(define (%text-mode port)
  (port/set-coding port 'ISO-8859-1)
  (port/set-line-ending port 'CRLF))

(define (%binary-mode port)
  (port/set-coding port 'BINARY)
  (port/set-line-ending port 'BINARY))

(define (write-http-request request port)
  (%text-mode port)
  (write-http-token (http-request-method request) port)
  (write-string " " port)
  (write-uri (http-request-uri request) port)
  (if (http-request-version request)
      (begin
	(write-string " " port)
	(write-http-version (http-request-version request) port)
	(newline port)
	(write-http-headers (http-request-headers request) port)
	(%binary-mode port)
	(write-string (http-request-body request) port))
      (begin
	(newline port)))
  (flush-output port))

(define (write-http-response response port)
  (if (http-response-version response)
      (begin
	(%text-mode port)
	(write-http-version (http-response-version response) port)
	(write-string " " port)
	(write (http-response-status response) port)
	(write-string " " port)
	(write-string (http-response-reason response) port)
	(newline port)
	(write-http-headers (http-response-headers response) port)))
  (%binary-mode port)
  (write-string (http-response-body response) port)
  (flush-output port))

;;;; Input

(define (read-simple-http-request port)
  (%text-mode port)
  (let ((line (read-line port)))
    (if (eof-object? line)
	line
	(make-simple-http-request
	 (parse-line parse-simple-request line "simple HTTP request")))))

(define (read-simple-http-response port)
  (make-simple-http-response (%read-all port)))

(define (read-http-request port)
  (%text-mode port)
  (let ((line (read-line port)))
    (if (eof-object? line)
	line
	(receive (method uri version)
	    (parse-line parse-request-line line "HTTP request line")
	  (let ((headers (read-http-headers port)))
	    (make-http-request method uri version headers
			       (or (%read-delimited-body headers port)
				   (%no-read-body))))))))

(define (read-http-response request port)
  (%text-mode port)
  (let ((line (read-line port)))
    (if (eof-object? line)
	#f
	(receive (version status reason)
	    (parse-line parse-response-line line "HTTP response line")
	  (let ((headers (read-http-headers port)))
	    (make-http-response version status reason headers
				(if (or (non-body-status? status)
					(eq? (http-request-method request)
					     '|HEAD|))
				    #f
				    (or (%read-delimited-body headers port)
					(%read-terminal-body headers port)
					(%no-read-body)))))))))

(define (%read-all port)
  (%binary-mode port)
  (call-with-output-octets
   (lambda (output)
     (let ((buffer (make-vector-8b #x1000)))
       (let loop ()
	 (let ((n (read-string! buffer port)))
	   (if (> n 0)
	       (begin
		 (write-substring buffer 0 n output)
		 (loop)))))))))

(define (%read-delimited-body headers port)
  (let ((n (%get-content-length headers)))
    (and n
	 (begin
	   (%binary-mode port)
	   (call-with-output-octets
	    (lambda (output)
	      (let ((buffer (make-vector-8b #x1000)))
		(let loop ((n n))
		  (if (> n 0)
		      (let ((m (read-string! buffer port)))
			(if (= m 0)
			    (error "Premature EOF in HTTP message body."))
			(write-substring buffer 0 m output)
			(loop (- n m))))))))))))

(define (%read-terminal-body headers port)
  (and (let ((h (http-header 'CONNECTION headers #f)))
	 (and h
	      (any (lambda (token)
		     (string-ci=? token "close"))
		   (burst-string (http-header-value h) char-set:wsp #t))))
       (%read-all port)))

(define (%no-read-body)
  (error "Unable to determine HTTP message body length."))

;;;; Request and response lines

(define parse-request-line
  (*parser
   (seq (map string->symbol
	     (match (+ (char-set char-set:http-token))))
	" "
	(alt (match "*")
	     parse-absolute-uri
	     parse-uri-path-absolute
	     parse-uri-authority)
	" "
	parse-http-version)))

(define parse-response-line
  (*parser
   (seq parse-http-version
	" "
	parse-http-status
	" "
	(match (* (char-set char-set:http-text))))))

(define parse-simple-request
  (*parser
   (seq (map string->symbol (match "GET"))
	" "
	parse-uri-path-absolute)))

(define (parse-line parser line description)
  (let ((v (*parse-string parser line)))
    (if (not v)
	(error (string-append "Malformed " description ":") line))
    (if (fix:= (vector-length v) 1)
	(vector-ref v 0)
	(apply values (vector->list v)))))

;;;; Status descriptions

(define (http-status-description code)
  (guarantee-http-status code 'HTTP-STATUS-DESCRIPTION)
  (let loop ((low 0) (high (vector-length known-status-codes)))
    (if (< low high)
	(let ((index (quotient (+ low high) 2)))
	  (let ((p (vector-ref known-status-codes index)))
	    (cond ((< code (car p)) (loop low index))
		  ((> code (car p)) (loop (+ index 1) high))
		  (else (cdr p)))))
	"(Unknown)")))

(define known-status-codes
  '#((100 . "Continue")
     (101 . "Switching Protocols")
     (200 . "OK")
     (201 . "Created")
     (202 . "Accepted")
     (203 . "Non-Authoritative Information")
     (204 . "No Content")
     (205 . "Reset Content")
     (206 . "Partial Content")
     (300 . "Multiple Choices")
     (301 . "Moved Permanently")
     (302 . "Found")
     (303 . "See Other")
     (304 . "Not Modified")
     (305 . "Use Proxy")
     (306 . "(Unused)")
     (307 . "Temporary Redirect")
     (400 . "Bad Request")
     (401 . "Unauthorized")
     (402 . "Payment Required")
     (403 . "Forbidden")
     (404 . "Not Found")
     (405 . "Method Not Allowed")
     (406 . "Not Acceptable")
     (407 . "Proxy Authentication Required")
     (408 . "Request Timeout")
     (409 . "Conflict")
     (410 . "Gone")
     (411 . "Length Required")
     (412 . "Precondition Failed")
     (413 . "Request Entity Too Large")
     (414 . "Request-URI Too Long")
     (415 . "Unsupported Media Type")
     (416 . "Requested Range Not Satisfiable")
     (417 . "Expectation Failed")
     (500 . "Internal Server Error")
     (501 . "Not Implemented")
     (502 . "Bad Gateway")
     (503 . "Service Unavailable")
     (504 . "Gateway Timeout")
     (505 . "HTTP Version Not Supported")))

(define (non-body-status? status)
  (or (<= 100 status 199)
      (= status 204)
      (= status 304)))

(define (http-message-body-port message)
  (let ((port (open-input-octets (http-message-body message))))
    (receive (type coding) (%get-content-type message)
      (cond ((eq? (mime-type/top-level type) 'TEXT)
	     (port/set-coding port (or coding 'TEXT))
	     (port/set-line-ending port 'TEXT))
	    ((and (eq? (mime-type/top-level type) 'APPLICATION)
		  (let ((sub (mime-type/subtype type)))
		    (or (eq? sub 'XML)
			(string-suffix-ci? "+xml" (symbol-name sub)))))
	     (port/set-coding port (or coding 'UTF-8))
	     (port/set-line-ending port 'XML-1.0))
	    (coding
	     (port/set-coding port coding)
	     (port/set-line-ending port 'TEXT))
	    (else
	     (port/set-coding port 'BINARY)
	     (port/set-line-ending port 'BINARY))))
    port))

(define (%get-content-type message)
  (let ((h (http-message-header 'CONTENT-TYPE message #f)))
    (if h
	(let ((s (rfc2822-header-value h)))
	  (let ((v (*parse-string parser:http-content-type s)))
	    (if (not v)
		(error "Malformed content-type value:" s))
	    (values (vector-ref v 0)
		    (let ((p (assq 'CHARSET (vector-ref v 1))))
		      (and p
			   (let ((coding (intern (cdr p))))
			     (and (known-input-coding? coding)
				  coding)))))))
	(values (make-mime-type 'APPLICATION 'OCTET-STREAM)
		#f))))

(define (%get-content-length headers)
  (let ((h (http-header 'CONTENT-LENGTH headers #f)))
    (and h
	 (let ((s (http-header-value h)))
	   (let ((n (string->number s)))
	     (if (not (exact-nonnegative-integer? n))
		 (error "Malformed content-length value:" s))
	     n)))))

(define parser:http-content-type
  (let ((parse-parameter
	 (*parser
	  (encapsulate* cons
	    (seq ";"
		 (noise (* (char-set char-set:wsp)))
		 parser:mime-token
		 "="
		 (alt (match matcher:mime-token)
		      parser:rfc2822-quoted-string))))))
    (*parser
     (seq parser:mime-type
	  (encapsulate vector->list
		       (* parse-parameter))))))

(define (http-message-header name message error?)
  (http-header name (http-message-headers message) error?))