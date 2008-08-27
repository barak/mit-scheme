#| -*-Scheme-*-

$Id: httpio.scm,v 14.5 2008/08/27 03:59:47 cph Exp $

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
  (guarantee-http-uri uri 'MAKE-HTTP-REQUEST)
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
  (let ((headers (convert-http-headers headers caller)))
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
		(values (cons (make-rfc2822-header 'CONTENT-LENGTH
						   (number->string m))
			      headers)
			body))))
	(values headers ""))))

(define (convert-http-headers headers caller)
  (guarantee-list headers caller)
  (map (lambda (header)
	 (cond ((http-header? header)
		header)
	       ((and (pair? header)
		     (http-token? (car header))
		     (http-text? (cdr header)))
		(make-rfc2822-header (car header) (cdr header)))
	       ((and (pair? header)
		     (http-token? (car header))
		     (pair? (cdr header))
		     (http-text? (cadr header))
		     (null? (cddr header)))
		(make-rfc2822-header (car header) (cadr header)))
	       (else
		(error:not-http-header header caller))))
       headers))

(define (simple-http-request? object)
  (and (http-request? object)
       (not (http-request-version object))))

(define-guarantee simple-http-request "simple HTTP request")

(define (make-simple-http-request uri)
  (guarantee-simple-http-uri uri 'MAKE-HTTP-REQUEST)
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

(define (http-token? object)
  (and (interned-symbol? object)
       (not (eq? object '||))
       (string-in-char-set? (symbol-name object) char-set:http-token)))

(define-guarantee http-token "HTTP token")

(define (http-uri? object)
  (or (absolute-uri? object)
      (simple-http-uri? object)))

(define-guarantee http-uri "HTTP URI")

(define (simple-http-uri? object)
  (and (relative-uri? object)
       (not (uri-authority object))
       (uri-path-absolute? (uri-path object))))

(define-guarantee simple-http-uri "simple HTTP URI")

(define (http-version? object)
  (and (pair? object)
       (exact-nonnegative-integer? (car object))
       (exact-nonnegative-integer? (cdr object))))

(define-guarantee http-version "HTTP version")

(define (make-http-version major minor)
  (guarantee-exact-nonnegative-integer major 'MAKE-HTTP-VERSION)
  (guarantee-exact-nonnegative-integer minor 'MAKE-HTTP-VERSION)
  (cons major minor))

(define (http-version-major v)
  (guarantee-http-version v 'HTTP-VERSION-MAJOR)
  (car v))

(define (http-version-minor v)
  (guarantee-http-version v 'HTTP-VERSION-MINOR)
  (cdr v))

(define (http-version=? v1 v2)
  (guarantee-http-version v1 'HTTP-VERSION=?)
  (guarantee-http-version v2 'HTTP-VERSION=?)
  (and (= (car v1) (car v2))
       (= (cdr v1) (cdr v2))))

(define (http-version<? v1 v2)
  (guarantee-http-version v1 'HTTP-VERSION<?)
  (guarantee-http-version v2 'HTTP-VERSION<?)
  (if (< (car v1) (car v2))
      #t
      (and (= (car v1) (car v2))
	   (< (cdr v1) (cdr v2)))))

(define (http-status? object)
  (and (exact-nonnegative-integer? object)
       (< object 1000)))

(define-guarantee http-status "HTTP status code")

(define (http-header? object)
  (and (rfc2822-header? object)
       (http-token? (rfc2822-header-name object))
       (http-text? (rfc2822-header-value object))))

(define-guarantee http-header "HTTP header field")

(define (http-text? object)
  (and (string? object)
       (string-in-char-set? object char-set:http-text)))

(define-guarantee http-text "HTTP text")

;;;; Output

(define (%text-mode port)
  (port/set-coding port 'US-ASCII)
  (port/set-line-ending port 'CRLF))

(define (%binary-mode port)
  (port/set-coding port 'BINARY)
  (port/set-line-ending port 'BINARY))

(define (write-http-request request port)
  (%text-mode port)
  (write-token (http-request-method request) port)
  (write-string " " port)
  (write-uri (http-request-uri request) port)
  (if (http-request-version request)
      (begin
	(write-string " " port)
	(write-version (http-request-version request) port)
	(newline port)
	(write-rfc2822-headers (http-request-headers request) port)
	(%binary-mode port)
	(write-string (http-request-body request) port))
      (begin
	(newline port)))
  (flush-output port))

(define (write-http-response response port)
  (if (http-response-version response)
      (begin
	(%text-mode port)
	(write-version (http-response-version response) port)
	(write-string " " port)
	(write (http-response-status response) port)
	(write-string " " port)
	(write-string (http-response-reason response) port)
	(newline port)
	(write-rfc2822-headers (http-response-headers response) port)))
  (%binary-mode port)
  (write-string (http-response-body response) port)
  (flush-output port))

(define (write-token token port)
  (write-string (string-upcase (symbol->string token)) port))

(define (write-version version port)
  (write-string "HTTP/" port)
  (write (car version) port)
  (write-string "." port)
  (write (cdr version) port))

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
	  (let ((headers (read-rfc2822-headers port)))
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
	  (let ((headers (read-rfc2822-headers port)))
	    (make-http-response version status reason headers
				(if (or (non-body-status? status)
					(eq? (http-request-method request)
					     '|HEAD|))
				    #f
				    (or (%read-delimited-body headers port)
					(%read-terminal-body headers port)
					(%no-read-body)))))))))

(define (parse-line parser line description)
  (let ((v (*parse-string parser line)))
    (if (not v)
	(error (string-append "Malformed " description ":") line))
    (if (fix:= (vector-length v) 1)
	(vector-ref v 0)
	(apply values (vector->list v)))))

(define parse-simple-request
  (*parser
   (seq "GET"
	(noise match-wsp)
	parse-uri-no-authority)))

(define parse-request-line
  (*parser
   (seq (map string->symbol
	     (match (+ (char-set char-set:http-token))))
	(noise match-wsp)
	parse-uri-no-authority
	(noise match-wsp)
	parse-version)))

(define parse-response-line
  (*parser
   (seq parse-version
	(noise match-wsp)
	parse-status-code
	(noise match-wsp)
	(match (* (char-set char-set:http-text))))))

(define parse-version
  (*parser
   (encapsulate (lambda (v)
		  (make-http-version (vector-ref v 0)
				     (vector-ref v 1)))
     (seq "HTTP/"
	  (map string->number
	       (match (+ (char-set char-set:numeric))))
	  "."
	  (map string->number
	       (match (+ (char-set char-set:numeric))))))))

(define parse-status-code
  (*parser
   (map string->number
	(match (seq (char-set char-set:numeric)
		    (char-set char-set:numeric)
		    (char-set char-set:numeric))))))

(define match-wsp
  (*matcher (+ (char-set char-set:wsp))))

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

(define (%get-content-length headers)
  (let ((h (first-rfc2822-header 'CONTENT-LENGTH headers)))
    (and h
	 (let ((s (rfc2822-header-value h)))
	   (let ((n (string->number s)))
	     (if (not (exact-nonnegative-integer? n))
		 (error "Malformed content-length value:" s))
	     n)))))

(define (%read-terminal-body headers port)
  (and (let ((h (first-rfc2822-header 'CONNECTION headers)))
	 (and h
	      (any (lambda (token)
		     (string-ci=? token "close"))
		   (burst-string (rfc2822-header-value h) char-set:wsp #t))))
       (%read-all port)))

(define (%no-read-body)
  (error "Unable to determine HTTP message body length."))

;;;; Syntax

(define (string-in-char-set? string char-set)
  (let ((end (string-length string)))
    (let loop ((i 0))
      (if (fix:< i end)
	  (and (char-set-member? char-set (string-ref string i))
	       (loop (fix:+ i 1)))
	  #t))))

(define char-set:http-text)
(define char-set:http-token)
(define http-version:1.0)
(define http-version:1.1)

(define (initialize-package!)
  (set! char-set:http-text
	(char-set-difference char-set:ascii char-set:ctls))
  (set! char-set:http-token
	(char-set-difference char-set:http-text
			     (string->char-set "()<>@,;:\\\"/[]?={} \t")))
  (set! http-version:1.0 (make-http-version 1 0))
  (set! http-version:1.1 (make-http-version 1 1))
  unspecific)

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
    (receive (type coding) (http-content-type message)
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

(define (http-content-type message)
  (let ((h (first-http-header 'CONTENT-TYPE message)))
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

(define parser:http-content-type
  (let ((parse-parameter
	 (*parser
	  (encapsulate (lambda (v)
			 (cons (vector-ref v 0)
			       (vector-ref v 1)))
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

(define (http-content-length message)
  (%get-content-length (http-message-headers message)))

(define (first-http-header name message)
  (first-rfc2822-header name (http-message-headers message)))

(define (all-http-headers name message)
  (all-rfc2822-headers name (http-message-headers message)))