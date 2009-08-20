#| -*-Scheme-*-

$Id$

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

;;;; MIT/GNU Scheme interface to Apache mod-lisp.

;;; Requires mod_lisp 2.41, or mod_lisp2 1.2.

(declare (usual-integrations))

(define (start-mod-lisp-server)
  (start-server-internal 3000 (host-address-loopback)))

(define (start-server-internal tcp-port tcp-host)
  (let ((socket (open-tcp-server-socket tcp-port tcp-host)))
    (dynamic-wind
     (lambda () unspecific)
     (lambda ()
       (do () ((channel-closed? socket))
	 (let ((port (tcp-server-connection-accept socket #t #f)))
	   (port/set-line-ending port 'NEWLINE)
	   (if trace-i/o-filename
	       (transcript-on trace-i/o-filename port))
	   (dynamic-wind
	    (lambda () unspecific)
	    (lambda ()
	      (write-response
	       (let ((generate-response
		      (lambda ()
			(handle-request (read-request port)))))
		 (if debug-internal-errors?
		     (generate-response)
		     (let ((response
			    (call-with-current-continuation
			     (lambda (k)
			       (bind-condition-handler
				   (list condition-type:error)
				   k
				 generate-response)))))
		       (if (condition? response)
			   (status-response 500 (condition->html response))
			   response))))
	       port)
	      (flush-output port))
	    (lambda ()
	      (transcript-off port)
	      (close-port port))))))
     (lambda () (channel-close socket)))))

(define trace-i/o-filename #f)
(define debug-internal-errors? #f)

(define (write-response message port)
  (if trace-requests?
      (pp `(RESPONSE ,@(map (lambda (p)
			      (list (car p) (cdr p)))
			    (http-message-headers message)))))
  (for-each (lambda (header)
	      (write-string (symbol-name (car header)) port)
	      (newline port)
	      (write-string (cdr header) port)
	      (newline port))
	    (http-message-headers message))
  (write-string "end" port)
  (newline port)
  (let ((entity (http-message-entity message)))
    (cond ((string? entity)
	   (write-string entity port))
	  ((pathname? entity)
	   (call-with-input-file entity
	     (lambda (input)
	       (port->port-copy input port))))
	  (else
	   (error "Illegal HTTP entity:" entity)))))

;;;; Request handler

(define (handle-request request)
  (let ((response (make-http-message))
	(pathname (http-message-pathname request)))
    (if trace-requests?
	(pp `(REQUEST (,(http-message-method request)
		       ,(http-message-url request)
		       ,@(http-message-url-parameters request))
		      (COOKIES ,@(http-message-cookies request))
		      ,@(map (lambda (p)
			       (list (car p) (cdr p)))
			     (http-message-headers request)))))
    (maybe-parse-post-variables request)
    (let ((expand
	   (lambda (default-type handler)
	     (set-status-header response 200)
	     (set-content-type-header response default-type)
	     (if handler
		 (mod-lisp-expander request response pathname handler)
		 (let ((pathname (->pathname pathname)))
		   (if (file-regular? pathname)
		       (maybe-set-entity request response pathname)
		       (status-response! response
					 404
					 (list "The document "
					       (http-message-url request)
					       " can't be found."))))))))
      (receive (handler default-type) (http-message-handler request)
	(if handler
	    (expand default-type handler)
	    (let ((type
		   (or (file-content-type pathname)
		       "application/octet-stream")))
	      (expand type
		      (get-mime-handler type))))))
    response))

(define (mod-lisp-expander request response pathname expander)
  (fluid-let ((*in-mod-lisp?* #t)
	      (*current-request* request)
	      (*current-response* response)
	      (*current-user-name* #f)
	      (expander-eval
	       (lambda (expression environment)
		 (with-repl-eval-boundary (nearest-repl)
		   (lambda ()
		     (eval expression environment))))))
    (run-hooks-in-list mod-lisp-before-expander-hooks request)
    (let ((value
	   (let ((user-name ((http-message-authenticator request))))
	     (cond ((or (string? user-name) (not user-name))
		    (set! *current-user-name* user-name)
		    (maybe-set-entity request response
				      (call-with-output-string
					(lambda (port)
					  (expander pathname port)))))
		   ((and (procedure? user-name)
			 (procedure-arity-valid? user-name 0))
		    (user-name))
		   ((eq? user-name 'UNAUTHENTICATED)
		    (http-response-unauthorized))
		   (else
		    (error "Illegal value from authenticator:" user-name))))))
      (run-hooks-in-list mod-lisp-after-expander-hooks request response)
      value)))

(define trace-requests? #f)
(define mod-lisp-before-expander-hooks (make-hook-list))
(define mod-lisp-after-expander-hooks (make-hook-list))

(define (in-mod-lisp?) *in-mod-lisp?*)

(define *in-mod-lisp?* #f)
(define *current-request*)
(define *current-response*)
(define *current-user-name*)

;;;; Read request

(define (read-request port)
  (let ((request (make-http-message)))
    (let loop ()
      (let ((keyword (read-line port)))
	(if (eof-object? keyword)
	    (error "EOF while reading headers."))
	(if (not (string-ci=? keyword "end"))
	    (let ((keyword (intern keyword))
		  (datum (read-line port)))
	      (if (eof-object? datum)
		  (error "Missing command datum:" keyword))
	      (if debug-request-headers?
		  (write-line (list keyword datum)))
	      (case keyword
		((METHOD)
		 (let ((method (intern datum)))
		   (if (not (memq method '(GET POST HEAD)))
		       (error "Unknown HTTP method:" method))
		   (set-http-message-method! request method)))
		((URL)
		 (receive (url parameters) (parse-url datum)
		   (set-http-message-url! request url)
		   (set-http-message-url-parameters! request parameters)))
		((CONTENT-LENGTH)
		 (set-http-message-entity!
		  request
		  (make-string
		   (or (string->number datum)
		       (error "Invalid Content-Length:" datum)))))
		((COOKIE)
		 (parse-cookie request datum))
		(else
		 (add-header request keyword datum)))
	      (loop)))))
    (let ((entity (http-message-entity request)))
      (if entity
	  (let ((end (string-length entity)))
	    (let loop ((start 0))
	      (if (fix:< start end)
		  (let ((n-read (read-substring! entity start end port)))
		    (cond ((not n-read)
			   (loop start))
			  ((> n-read 0)
			   (loop (+ start n-read)))
			  (else
			   (error "EOF while reading request entity.")))))))))
    request))

(define debug-request-headers? #f)

(define (parse-url url)
  (let ((q (string-find-next-char url #\?)))
    (if q
	(values (string-head url q)
		(parse-parameters (string-tail url (fix:+ q 1))))
	(values url '()))))

;;;; POST variables

(define (maybe-parse-post-variables request)
  (let ((entity (http-message-entity request)))
    (if (and entity (eq? 'POST (http-message-method request)))
	(begin
	  (set-http-message-post-parameters! request (parse-parameters entity))
	  (set-http-message-entity! request #f)
	  (if debug-post-variables?
	      (pp (http-message-post-parameters request)))))))

(define debug-post-variables? #f)

(define (parse-parameters string)
  (let loop ((parameters (burst-string string #\& #f)))
    (if (pair? parameters)
	(let ((parameter (car parameters))
	      (tail (loop (cdr parameters))))
	  (let ((e (string-find-next-char parameter #\=)))
	    (if e
		(cons (cons (string->symbol (string-head parameter e))
			    (decode-parameter-value parameter
						    (fix:+ e 1)
						    (string-length parameter)))
		      tail)
		tail)))
	'())))

(define (decode-parameter-value string start end)
  (call-with-output-string
    (lambda (port)
      (let loop ((start start))
	(receive (char start) (decode-parameter-char string start end)
	  (if char
	      (if (char=? char #\return)
		  (receive (char* start*)
		      (decode-parameter-char string start end)
		    (if (eqv? char* #\newline)
			(begin
			  (newline port)
			  (loop start*))
			(begin
			  (write-char char port)
			  (loop start))))
		  (begin
		    (write-char char port)
		    (loop start)))))))))

(define (decode-parameter-char string start end)
  (if (fix:< start end)
      (let ((char (string-ref string start)))
	(cond ((not (char=? char #\%))
	       (values (if (char=? char #\+) #\space char)
		       (fix:+ start 1)))
	      ((fix:<= (fix:+ start 3) end)
	       (let ((d1 (char->digit (string-ref string (fix:+ start 1)) 16))
		     (d2 (char->digit (string-ref string (fix:+ start 2)) 16)))
		 (if (and d1 d2)
		     (values (integer->char (+ (* 16 d1) d2))
			     (fix:+ start 3))
		     (values #f start))))
	       (else
		(values #f start))))
      (values #f #f)))

;;;; Cookie support

(define (parse-cookie message string)
  (let ((bindings
	 (map (lambda (binding)
		(let ((e (string-find-next-char binding #\=)))
		  (and e
		       (cons (intern (string-head binding e))
			     (string-tail binding (fix:+ e 1))))))
	      (map string-trim (burst-string string #\; #f)))))
    (if (memq #f bindings)
	(warn "Malformed cookie value:" string)
	(set-http-message-cookies! message
				   (append! (http-message-cookies message)
					    bindings)))))

(define (set-cookie message name value attrs)
  ;; Version 0 ("netscape") cookies.
  (add-header message
	      'SET-COOKIE
	      (let* ((%attr
		      (lambda (key name map-value)
			(let ((value (get-keyword-value attrs key)))
			  (if (default-object? value)
			      ""
			      (string-append "; "
					     (symbol-name name)
					     "="
					     (if map-value
						 (map-value value)
						 value))))))
		     (attr
		      (lambda (name map-value)
			(%attr name name map-value))))
		(string-append (symbol-name name) "=" value
			       (%attr 'max-age 'expires max-age->expires)
			       (attr 'domain #f)
			       (attr 'path #f)
			       (attr 'secure (lambda (v) v "secure"))))))

(define (max-age->expires n)
  (let ((dt (universal-time->global-decoded-time (+ (get-universal-time) n)))
	(d2 (lambda (n) (string-pad-left (number->string n) 2 #\0))))
    (string-append (let ((day (decoded-time/day-of-week dt)))
		     (if day
			 (string-append (day-of-week/short-string day) ", ")
			 ""))
		   (number->string (decoded-time/day dt))
		   "-"
		   (month/short-string (decoded-time/month dt))
		   "-"
		   (number->string (decoded-time/year dt))
		   " "
		   (d2 (decoded-time/hour dt))
		   ":"
		   (d2 (decoded-time/minute dt))
		   ":"
		   (d2 (decoded-time/second dt))
		   " GMT")))

;;;; Status messages

(define (condition->html condition)
  (list (html:p #f
		"\n"
		(call-with-output-string
		  (lambda (port)
		    (write-condition-report condition port)
		    (fresh-line port))))
	"\n"
	"\n"
	(html:pre #f
		  "\n"
		  (call-with-output-string
		    (lambda (port)
		      (stack-trace condition port)
		      (fresh-line port))))))

(define (status-response code extra)
  (let ((response (make-http-message)))
    (status-response! response code extra)
    response))

(define (status-response! response code extra)
  (set-status-header response code)
  (set-content-type-header response 'text/html)
  (set-entity response
	      (call-with-output-octets
	       (lambda (port)
		 (write-xml
		  (let ((message (status-message code)))
		    (html:html #f
			       "\n"
			       (html:head #f
					  "\n"
					  (html:title #f code " " message)
					  "\n")
			       "\n"
			       (html:body #f
					  "\n"
					  (html:h1 #f message)
					  "\n"
					  extra
					  "\n")
			       "\n"))
		  port)
		 (newline port)))))

(define (set-status-header message code)
  (set-header message
	      'STATUS
	      (call-with-output-string
		(lambda (port)
		  (write code port)
		  (write-char #\space port)
		  (write-string (status-message code) port)))))

(define (set-content-type-header message type)
  (set-header message 'CONTENT-TYPE (symbol-name type)))

(define (status-message code)
  (let loop ((low 0) (high (vector-length known-status-codes)))
    (if (not (fix:< low high))
	(error "Unknown status code:" code))
    (let ((index (fix:quotient (fix:+ low high) 2)))
      (let ((p (vector-ref known-status-codes index)))
	(cond ((< code (car p)) (loop low index))
	      ((> code (car p)) (loop (fix:+ index 1) high))
	      (else (cdr p)))))))

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

;;;; HTTP message datatype

(define-structure (http-message (constructor make-http-message ()))
  (headers '())
  (headers-tail '())
  (entity #f)
  (method #f)
  (url #f)
  (url-parameters '())
  (post-parameters '())
  (cookies '()))

(define (set-header message keyword datum)
  (let ((p (assq keyword (http-message-headers message))))
    (if p
	(set-cdr! p datum)
	(add-header message keyword datum))))

(define (add-header message keyword datum)
  (let ((new (list (cons keyword datum)))
	(tail (http-message-headers-tail message)))
    (if (pair? tail)
	(set-cdr! tail new)
	(set-http-message-headers! message new))
    (set-http-message-headers-tail! message new)))

(define (set-entity message entity)
  (set-header message
	      'CONTENT-LENGTH
	      (number->string
	       (cond ((string? entity)
		      (string-length entity))
		     ((pathname? entity)
		      (file-length entity))
		     (else
		      (error:wrong-type-argument entity
						 "string or pathname"
						 'SET-ENTITY)))))
  (set-http-message-entity! message entity))

(define (maybe-set-entity request response entity)
  (set-entity response
	      (if (eq? (http-message-method request) 'HEAD)
		  ""
		  entity)))

(define (message-keyword-proc accessor name)
  (lambda (message keyword #!optional error?)
    (let ((p (assq keyword (accessor message))))
      (if p
	  (cdr p)
	  (begin
	    (if (if (default-object? error?) #f error?)
		(error:bad-range-argument keyword name))
	    #f)))))

(define http-message-header
  (message-keyword-proc http-message-headers
			'HTTP-MESSAGE-HEADER))

(define http-message-url-parameter
  (message-keyword-proc http-message-url-parameters
			'HTTP-MESSAGE-URL-PARAMETER))

(define http-message-post-parameter
  (message-keyword-proc http-message-post-parameters
			'HTTP-MESSAGE-POST-PARAMETER))

(define http-message-cookie
  (message-keyword-proc http-message-cookies
			'HTTP-MESSAGE-COOKIE))

(define (http-message-pathname message)
  (http-message-header message 'script-filename #t))

;;;; Request/response accessors

(define (http-request-entity)
  (http-message-entity *current-request*))

(define (http-request-method)
  (http-message-method *current-request*))

(define (http-request-url)
  (http-message-url *current-request*))

(define (http-request-pathname)
  (http-message-pathname *current-request*))

(define (http-request-header-bindings)
  (http-message-headers *current-request*))

(define (http-request-url-parameter-bindings)
  (http-message-url-parameters *current-request*))

(define (http-request-post-parameter-bindings)
  (http-message-post-parameters *current-request*))

(define (http-request-cookies)
  (http-message-cookies *current-request*))

(define (keyword-proc accessor)
  (lambda (keyword #!optional error?)
    (accessor *current-request*
	      keyword
	      (if (default-object? error?) #f error?))))

(define http-request-header
  (keyword-proc http-message-header))

(define http-request-url-parameter
  (keyword-proc http-message-url-parameter))

(define http-request-post-parameter
  (keyword-proc http-message-post-parameter))

(define http-request-cookie
  (keyword-proc http-message-cookie))

(define (http-request-post-parameter-multiple keyword)
  (let loop
      ((bindings (http-message-post-parameters *current-request*))
       (strings '()))
    (if (pair? bindings)
	(loop (cdr bindings)
	      (if (eq? (caar bindings) keyword)
		  (cons (cdar bindings) strings)
		  strings))
	(reverse! strings))))

(define (http-response-header keyword datum #!optional overwrite?)
  (guarantee-symbol keyword 'HTTP-RESPONSE-HEADER)
  (guarantee-string datum 'HTTP-RESPONSE-HEADER)
  (if (memq keyword '(STATUS CONTENT-LENGTH))
      (error "Illegal header keyword:" keyword))
  (if (or (eq? keyword 'CONTENT-TYPE)
	  (if (default-object? overwrite?) #f overwrite?))
      (set-header *current-response* keyword datum)
      (add-header *current-response* keyword datum)))

(define (http-response-cookie name value . attrs)
  (set-cookie *current-response* name value attrs))

(define (http-response-entity entity)
  (maybe-set-entity *current-request* *current-response* entity))

(define (http-status-response code . extra)
  (guarantee-exact-nonnegative-integer code 'HTTP-STATUS-RESPONSE)
  (status-response! *current-response* code extra))

;;;; MIME stuff

(define (file-content-type pathname)
  (or (let ((extension (pathname-type pathname)))
	(and (string? extension)
	     (hash-table/get mime-extensions extension #f)))
      (let ((t (pathname-mime-type pathname)))
	(and t
	     (symbol (mime-type/top-level t)
		     '/
		     (mime-type/subtype t))))))

(define (get-mime-handler type)
  (hash-table/get mime-handlers type #f))

(define (define-mime-handler type handle-request)
  (cond ((symbol? type)
	 (hash-table/put! mime-handlers type handle-request))
	((and (pair? type)
	      (symbol? (car type))
	      (for-all? (cdr type) string?))
	 (hash-table/put! mime-handlers (car type) handle-request)
	 (for-each (lambda (extension)
		     (hash-table/put! mime-extensions extension (car type)))
		   (cdr type)))
	(else
	 (error:wrong-type-argument type "MIME type" 'DEFINE-MIME-HANDLER))))

(define mime-handlers (make-eq-hash-table))
(define mime-extensions (make-string-hash-table))

(define (html-content-type)
  (if (let ((type (http-browser-type)))
	(and (pair? type)
	     (eq? (car type) 'IE)))
      "text/html"
      "application/xhtml+xml"))

(define (http-browser-type)
  (let ((ua (http-request-header 'user-agent)))
    (and ua
	 (let loop ((p browser-type-alist))
	   (and (pair? p)
		(if (re-string-match (caar p) ua #t)
		    (cdar p)
		    (loop (cdr p))))))))

(define browser-type-alist
  '(("^Mozilla/5\\.0 (.*) Gecko/[0-9]+ Firefox/[0-9.]+" gecko firefox)
    ("^Mozilla/5\\.0 (.*) Gecko/[0-9]+ Epiphany/[0-9.]+" gecko epiphany)
    ("^Mozilla/5\\.0 (.*) Gecko/[0-9]+ Galeon/[0-9.]+" gecko galeon)
    ("^Mozilla/5\\.0 " gecko)
    ("^Mozilla/[0-9.]+ (compatible; MSIE [0-9.]+; Win.+)" ie windows)
    ("^Mozilla/[0-9.]+ (compatible; MSIE [0-9.]+; Mac.+)" ie mac)
    ("^Mozilla/[0-9.]+ (compatible; MSIE [0-9.]+; .+)" ie)
    ("^Mozilla/[0-9.]+ (compatible; Opera [0-9.]+; .+)" opera)
    ("W3C_Validator/[0-9.]+" validator)
    ("W3C_CSS_Validator_JFouffa/[0-9.]+" validator)
    ("WDG_Validator/[0-9.]+" validator)
    ("Page Valet/[0-9.]+" validator)
    ("CSE HTML Validator" validator)))

;;;; Authentication

(define (http-request-user-name)
  *current-user-name*)

(define (http-authenticator:basic)
  (let ((auth (http-request-header 'authorization)))
    (and auth
	 (cond ((string-prefix? "Basic " auth)
		(decode-basic-auth-header auth 6 (string-length auth)))
	       (else
		(error "Unknown authorization header format:" auth))))))

(define (decode-basic-auth-header string start end)
  (let ((auth
	 (call-with-output-string
	   (lambda (port)
	     (let ((ctx (decode-base64:initialize port #t)))
	       (decode-base64:update ctx string start end)
	       (decode-base64:finalize ctx))))))
    (let ((colon (string-find-next-char auth #\:)))
      (if (not colon)
	  (error "Malformed authorization string."))
      (string-head auth colon))))

(define (http-response-unauthorized)
  (http-status-response 401
			"You don't have authorization to view this document."))

(define (http-response-redirect url)
  (http-status-response 302
			"The document has moved "
			(html:href url "here")
			".")
  (http-response-header 'location url))

;;;; URL bindings

(define (http-message-authenticator message)
  (let ((authenticator
	 (url-binding-value (http-message-url message) 'authenticator)))
    (if (default-object? authenticator)
	(lambda () #f)
	authenticator)))

(define (http-message-handler message)
  (let ((url (http-message-url message)))
    (let ((handler (url-binding-value url 'handler)))
      (if (default-object? handler)
	  (values #f #f)
	  (values handler (url-binding-value url 'default-type #t))))))

(define (url-binding-value url name #!optional error?)
  (let loop ((bindings url-bindings) (binding #f))
    (cond ((pair? bindings)
	   (loop (cdr bindings)
		 (if (and (string-prefix? (caar bindings) url)
			  (assq name (cdar bindings))
			  (or (not binding)
			      (fix:> (string-length (caar bindings))
				     (string-length (car binding)))))
		     (car bindings)
		     binding)))
	  (binding
	   (cdr (assq name (cdr binding))))
	  (else
	   (if (if (default-object? error?) #f error?)
	       (error:bad-range-argument name 'url-binding-value))
	   #!default))))

(define (define-subtree-handler url default-type handler)
  (define-url-bindings url
    'default-type default-type
    'handler handler))

(define (define-url-bindings url . klist)
  (guarantee-keyword-list klist 'define-url-bindings)
  (let* ((binding
	  (find-matching-item url-bindings
	    (lambda (binding)
	      (string=? (car binding) url)))))
    (if binding
	(do ((klist klist (cddr klist)))
	    ((not (pair? klist)))
	  (let ((name (car klist))
		(value (cadr klist)))
	    (let ((p (assq name (cdr binding))))
	      (if p
		  (set-cdr! p value)
		  (set-cdr! binding
			    (cons (cons name value)
				  (cdr binding)))))))
	(begin
	  (set! url-bindings
		(cons (cons url (keyword-list->alist klist))
		      url-bindings))
	  unspecific))))

(define url-bindings '())

;;;; Utilities

(define (port->port-copy input output #!optional buffer-size)
  (let ((buffer
	 (make-string (if (default-object? buffer-size)
			  #x10000
			  buffer-size))))
    (let loop ()
      (let ((n (read-string! buffer input)))
	(cond ((not n)
	       (loop))
	      ((> n 0)
	       (write-substring buffer 0 n output)
	       (loop)))))))

(define (for-each-file-line pathname procedure)
  (call-with-input-file pathname
    (lambda (port)
      (for-each-port-line port procedure))))

(define (for-each-port-line port procedure)
  (let loop ()
    (let ((line (read-line port)))
      (if (not (eof-object? line))
	  (begin
	    (procedure line)
	    (loop))))))

;;;; Logging

(define (start-logging-requests pathname)
  (if (hook-in-list? mod-lisp-before-expander-hooks 'LOG-REQUESTS)
      (error "Logging already started."))
  (set! request-log-port (open-output-file pathname 'APPEND))
  (append-hook-to-list mod-lisp-before-expander-hooks
		       'LOG-REQUESTS
		       log-requests))

(define (stop-logging-requests)
  (remove-hook-from-list mod-lisp-before-expander-hooks 'LOG-REQUESTS)
  (let ((port request-log-port))
    (set! request-log-port #f)
    (if port
	(close-port port))))

(define (log-requests request)
  (if request-log-port
      (begin
	(write-line (list (get-universal-time)
			  (http-message-method request)
			  (http-message-url request)
			  (http-request-user-name)
			  (http-message-post-parameters request))
		    request-log-port)
	(flush-output request-log-port))))

(define request-log-port #f)