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

(define (make-http-request method uri version headers body)
  (guarantee http-token-string? method 'make-http-request)
  (guarantee http-request-uri? uri 'make-http-request)
  (guarantee http-version? version 'make-http-request)
  (receive (headers body)
      (guarantee-headers&body headers body 'make-http-request)
    (%make-http-request method uri version headers body)))

(define-print-method http-request?
  (standard-print-method 'http-request
    (lambda (request)
      (list (http-request-method request)
	    (uri->string (http-request-uri request))))))

(define-record-type <http-response>
    (%make-http-response version status reason headers body)
    http-response?
  (version http-response-version)
  (status http-response-status)
  (reason http-response-reason)
  (headers http-response-headers)
  (body http-response-body))

(define (make-http-response version status reason headers body)
  (guarantee http-version? version 'make-http-response)
  (guarantee http-status? status 'make-http-response)
  (guarantee http-text? reason 'make-http-response)
  (receive (headers body)
      (guarantee-headers&body headers body 'make-http-response)
    (%make-http-response version status reason headers body)))

(define-print-method http-response?
  (standard-print-method 'http-response
    (lambda (response)
      (list (http-response-status response)))))

(define (guarantee-headers&body headers body caller)
  (guarantee-list-of http-header? headers caller)
  (if body
      (begin
	(guarantee bytevector? body caller)
	(let ((n (%get-content-length headers))
	      (m (bytevector-length body)))
	  (if n
	      (begin
		(if (not (= n m))
		    (error:bad-range-argument body caller))
		(values headers body))
	      (values (cons (make-http-header 'content-length
					      (number->string m))
			    headers)
		      body))))
      (values headers (bytevector))))

(define (simple-http-request? object)
  (and (http-request? object)
       (not (http-request-version object))))

(define-guarantee simple-http-request "simple HTTP request")

(define (make-simple-http-request uri)
  (guarantee simple-http-request-uri? uri 'make-http-request)
  (%make-http-request '|GET| uri #f '() (bytevector)))

(define (simple-http-response? object)
  (and (http-response? object)
       (not (http-response-version object))))

(define-guarantee simple-http-response "simple HTTP response")

(define (make-simple-http-response body)
  (guarantee bytevector? body 'make-simple-http-response)
  (%make-http-response #f 200 (http-status-description 200) '() body))

(define (http-message? object)
  (or (http-request? object)
      (http-response? object)))

(define-guarantee http-message "HTTP message")

(define (http-message-headers message)
  (cond ((http-request? message) (http-request-headers message))
	((http-response? message) (http-response-headers message))
	(else (error:not-http-message message 'http-message-headers))))

(define (http-message-body message)
  (cond ((http-request? message) (http-request-body message))
	((http-response? message) (http-response-body message))
	(else (error:not-http-message message 'http-message-body))))

(define (http-request-uri? object)
  (or (simple-http-request-uri? object)
      (absolute-uri? object)
      (eq? object '*)
      (uri-authority? object)))

(define-guarantee http-request-uri "HTTP URI")

(define (simple-http-request-uri? object)
  (and (uri? object)
       (not (uri-scheme object))
       (not (uri-authority object))
       (uri-path-absolute? (uri-path object))))

(define-guarantee simple-http-request-uri "simple HTTP URI")

;;;; Output

(define (write-http-request request port)
  (write-ascii (http-request-method request) port)
  (write-u8 (char->integer #\space) port)
  (let ((uri (http-request-uri request)))
    (cond ((uri? uri)
	   (write-ascii (call-with-output-string
			 (lambda (out) (write-uri uri out)))
			port))
	  ((uri-authority? uri)
	   (write-ascii (call-with-output-string
			 (lambda (out) (write-uri-authority uri out)))
			port))
	  ((eq? uri '*)
	   (write-u8 (char->integer #\*) port))
	  (else
	   (error "Ill-formed HTTP request:" request))))
  (if (http-request-version request)
      (begin
	(write-u8 (char->integer #\space) port)
	(write-http-version (http-request-version request) port)
	(write-u8 (char->integer #\return) port)
	(write-u8 (char->integer #\linefeed) port)
	(write-http-headers (http-request-headers request) port)
	(write-bytevector (http-request-body request) port))
      (begin
	(newline port)))
  (flush-output-port port))

(define (write-ascii string port)
  (write-bytevector (string->utf8 string) port))

(define (write-http-response response port)
  (if (http-response-version response)
      (begin
	(write-http-version (http-response-version response) port)
	(write-u8 (char->integer #\space) port)
	(write-ascii (write-to-string (http-response-status response)) port)
	(write-u8 (char->integer #\space) port)
	(write-ascii (http-response-reason response) port)
	(newline port)
	(write-http-headers (http-response-headers response) port)))
  (write-bytevector (http-response-body response) port)
  (flush-output-port port))

;;;; Input

(define (read-simple-http-request port)
  (let ((line (read-ascii-line port)))
    (if (eof-object? line)
	line
	(make-simple-http-request
	 (parse-line parse-simple-request line "simple HTTP request")))))

(define (read-ascii-line port)
  (with-input-port-blocking-mode port 'blocking
    (lambda ()
      (let ((builder (string-builder)))
	(let loop ()
	  (let ((byte (read-u8 port)))
	    (cond ((eof-object? byte)
		   (if (builder 'empty?)
		       byte
		       (builder)))
		  ((fix:= 13 byte)
		   (let ((line (builder)))
		     (if (fix:= 10 (peek-u8 port))
			 (read-u8 port)
			 (warn "Invalid line ending in header line:" line))
		     line))
		  ((fix:= 10 byte)
		   (let ((line (builder)))
		     (warn "Invalid line ending in header line:" line)
		     line))
		  ((and (fix:<= 32 byte) (fix:<= byte 126))
		   (builder (integer->char byte))
		   (loop))
		  (else
		   (warn "Illegal character in header line:" byte (builder))
		   (loop)))))))))

(define (read-simple-http-response port)
  (make-simple-http-response (%read-all port)))

(define (read-http-request port)
  (let ((line (read-ascii-line port)))
    (if (eof-object? line)
	line
	(receive (method uri version)
	    (parse-line parse-request-line line "HTTP request line")
	  (let ((headers (read-http-headers port)))
	    (let ((b.t
		   (or (%read-chunked-body headers port)
		       (%read-delimited-body headers port)
		       (%no-read-body))))
	      (make-http-request method uri version
				 (append! headers (cdr b.t))
				 (car b.t))))))))

(define (read-http-response request port)
  (let ((line (read-ascii-line port)))
    (if (eof-object? line)
	#f
	(receive (version status reason)
	    (parse-line parse-response-line line "HTTP response line")
	  (let ((headers (read-http-headers port)))
	    (let ((b.t
		   (if (or (non-body-status? status)
			   (string=? (http-request-method request) "HEAD"))
		       (list #f)
		       (or (%read-chunked-body headers port)
			   (%read-delimited-body headers port)
			   (%read-terminal-body headers port)
			   (%no-read-body)))))
	      (make-http-response version status reason
				  (append! headers (cdr b.t))
				  (car b.t))))))))

(define (%read-chunked-body headers port)
  (let ((h (http-header 'transfer-encoding headers #f)))
    (and h
	 (let ((v (http-header-parsed-value h)))
	   (and (not (default-object? v))
		(assq 'chunked v)))
	 (let ((output (open-output-bytevector))
	       (buffer (make-bytevector #x1000)))
	   (let loop ()
	     (let ((n (%read-chunk-leader port)))
	       (if (> n 0)
		   (begin
		     (%read-chunk n buffer port output)
		     (let ((line (read-ascii-line port)))
		       (if (not (string-null? line))
			   (error "Missing CRLF after chunk data.")))
		     (loop)))))
	   (cons (get-output-bytevector output)
		 (read-http-headers port))))))

(define (%read-chunk-leader port)
  (let ((line (read-ascii-line port)))
    (if (eof-object? line)
	(error "Premature EOF in HTTP message body."))
    (let ((v (parse-http-chunk-leader line)))
      (if (not v)
	  (error "Ill-formed chunk in HTTP message body."))
      (car v))))

(define (%read-chunk n buffer port output)
  (let ((len (bytevector-length buffer)))
    (let loop ((n n))
      (if (> n 0)
	  (let ((m (read-bytevector! buffer port 0 (min n len))))
	    (if (not (exact-positive-integer? m))
		(error "Premature EOF in HTTP message body."))
	    (do ((i 0 (+ i 1)))
		((not (< i m)))
	      (write-u8 (bytevector-u8-ref buffer i) output))
	    (loop (- n m)))))))

(define (%read-delimited-body headers port)
  (let ((n (%get-content-length headers)))
    (and n
	 (list
	  (call-with-output-bytevector
	   (lambda (output)
	     (%read-chunk n (make-bytevector #x1000) port output)))))))

(define (%read-terminal-body headers port)
  (and (let ((h (http-header 'connection headers #f)))
	 (and h
	      (let ((v (http-header-parsed-value h)))
		(and (not (default-object? v))
		     (memq 'close v)))))
       (list (%read-all port))))

(define (%read-all port)
  (call-with-output-bytevector
   (lambda (output)
     (let ((buffer (make-bytevector #x1000)))
       (let loop ()
	 (let ((n (read-bytevector! buffer port)))
	   (if (exact-positive-integer? n)
	       (begin
		 (do ((i 0 (+ i 1)))
		     ((not (< i n)))
		   (write-u8 (bytevector-u8-ref buffer i) output))
		 (loop)))))))))

(define (%no-read-body)
  (error "Unable to determine HTTP message body length."))

;;;; Request and response lines

(define parse-request-line
  (*parser
   (seq (map string->symbol
	     (match (+ (char-set char-set:http-token))))
	" "
	(alt (map intern (match "*"))
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
  (guarantee http-status? code 'http-status-description)
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
     (102 . "Processing")
     (200 . "OK")
     (201 . "Created")
     (202 . "Accepted")
     (203 . "Non-Authoritative Information")
     (204 . "No Content")
     (205 . "Reset Content")
     (206 . "Partial Content")
     (207 . "Multi-Status")
     (226 . "IM Used")
     (300 . "Multiple Choices")
     (301 . "Moved Permanently")
     (302 . "Found")
     (303 . "See Other")
     (304 . "Not Modified")
     (305 . "Use Proxy")
     (306 . "Switch Proxy")
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
     (418 . "I'm a Teapot")
     (422 . "Unprocessable Entity")
     (423 . "Locked")
     (424 . "Failed Dependency")
     (425 . "Unordered Collection")
     (426 . "Upgrade Required")
     (449 . "Retry With")
     (500 . "Internal Server Error")
     (501 . "Not Implemented")
     (502 . "Bad Gateway")
     (503 . "Service Unavailable")
     (504 . "Gateway Timeout")
     (505 . "HTTP Version Not Supported")
     (506 . "Variant Also Negotiates")
     (507 . "Insufficient Storage")
     (509 . "Bandwidth Limit Exceeded")
     (510 . "Not Extended")))

(define (non-body-status? status)
  (or (<= 100 status 199)
      (= status 204)
      (= status 304)))

(define (http-message-body-port message)
  (let ((port
	 (binary->textual-port
	  (open-input-bytevector (http-message-body message)))))
    (receive (type coding) (%get-content-type message)
      (cond ((eq? (mime-type/top-level type) 'text)
	     (port/set-coding port (or coding 'text))
	     (port/set-line-ending port 'text))
	    ((and (eq? (mime-type/top-level type) 'application)
		  (let ((sub (mime-type/subtype type)))
		    (or (eq? sub 'xml)
			(string-suffix-ci? "+xml" (symbol->string sub)))))
	     (port/set-coding port (or coding 'utf-8))
	     (port/set-line-ending port 'xml-1.0))
	    (coding
	     (port/set-coding port coding)
	     (port/set-line-ending port 'text))
	    (else
	     (port/set-coding port 'binary)
	     (port/set-line-ending port 'binary))))
    port))

(define (%get-content-type message)
  (optional-header (http-message-header 'content-type message #f)
		   (lambda (v)
		     (values (car v)
			     (let ((p (assq 'charset (cdr v))))
			       (and p
				    (let ((coding (intern (cdr p))))
				      (and (known-input-port-coding? coding)
					   coding))))))
		   (lambda ()
		     (values (make-mime-type 'application 'octet-stream)
			     #f))))

(define (%get-content-length headers)
  (optional-header (http-header 'content-length headers #f)
		   (lambda (n) n)
		   (lambda () #f)))

(define (optional-header h win lose)
  (if h
      (let ((v (http-header-parsed-value h)))
	(if (default-object? v)
	    (lose)
	    (win v)))
      (lose)))

(define (http-message-header name message error?)
  (http-header name (http-message-headers message) error?))