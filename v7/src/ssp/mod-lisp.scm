#| -*-Scheme-*-

$Id: mod-lisp.scm,v 1.8 2004/10/31 00:01:26 cph Exp $

Copyright 2003,2004 Massachusetts Institute of Technology

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

;;;; MIT/GNU Scheme interface to Apache mod-lisp.

(declare (usual-integrations))

(define (start-mod-lisp-server)
  (start-server-internal 3000
			 (host-address-loopback)
			 (cond ((file-directory? "/web/www/") "/web/www/")
			       ((file-directory? "/var/www/") "/var/www/")
			       (else (error "No server root?")))))

(define (start-server-internal tcp-port tcp-host server-root)
  (let ((socket (open-tcp-server-socket tcp-port tcp-host)))
    (dynamic-wind
     (lambda () unspecific)
     (lambda ()
       (do () ((channel-closed? socket))
	 (let ((port (tcp-server-connection-accept socket #t #f)))
	   (port/set-line-ending port 'NEWLINE)
	   (dynamic-wind
	    (lambda () unspecific)
	    (lambda ()
	      (write-response
	       (let ((generate-response
		      (lambda ()
			(handle-request (read-request port) server-root))))
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
	    (lambda () (close-port port))))))
     (lambda () (channel-close socket)))))

(define debug-internal-errors? #f)

(define (write-response message port)
  (for-each (lambda (header)
	      ;; Kludge: mod-lisp uses case-sensitive comparisons for
	      ;; these headers.
	      (write-string (case (car header)
			      ((CONTENT-LENGTH) "Content-Length")
			      ((CONTENT-TYPE) "Content-Type")
			      ((KEEP-SOCKET) "Keep-Socket")
			      ((LAST-MODIFIED) "Last-Modified")
			      ((LOCATION) "Location")
			      ((LOG) "Log")
			      ((LOG-ERROR) "Log-Error")
			      ((NOTE) "Note")
			      ((SET-COOKIE) "Set-Cookie")
			      ((STATUS) "Status")
			      (else (symbol-name (car header))))
			    port)
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

(define (condition->html condition)
  (call-with-output-string
    (lambda (port)
      (write-string "<p>" port)
      (newline port)
      (escape-output port
	(lambda (port)
	  (write-condition-report condition port)))
      (newline port)
      (write-string "</p>" port)
      (newline port)
      (newline port)
      (write-string "<pre>" port)
      (let ((dstate (make-initial-dstate condition)))
	(command/print-subproblem dstate port)
	(let loop ()
	  (if (let ((next
		     (stack-frame/next-subproblem (dstate/subproblem dstate))))
		(and next (not (stack-frame/repl-eval-boundary? next))))
	      (begin
		(newline port)
		(newline port)
		(escape-output port
		  (lambda (port)
		    (command/earlier-subproblem dstate port)))
		(loop)))))
      (write-string "</pre>" port)
      (newline port))))

(define (escape-output port generator)
  (write-escaped-string (call-with-output-string generator) port))

(define (write-escaped-string string port)
  (let ((end (string-length string)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i end))
      (write-escaped-char (string-ref string i) port))))

(define (write-escaped-char char port)
  (case char
    ((#\<) (write-string "&lt;" port))
    ((#\&) (write-string "&amp;" port))
    (else (write-char char port))))

;;;; Request handler

(define (handle-request request server-root)
  (let ((url (http-message-url request)))
    (if trace-requests?
	(pp `(REQUEST (,(http-message-method request)
		       ,url
		       ,@(http-message-url-parameters request))
		      ,@(map (lambda (p)
			       (list (car p) (cdr p)))
			     (http-message-headers request)))))
    (receive (root-dir relative) (url->relative url server-root)
      (fluid-let ((*root-dir* root-dir))
	(let ((response (make-http-message)))
	  (let ((expand
		 (lambda (pathname default-type handler)
		   (add-status-header response 200)
		   (add-content-type-header response default-type)
		   (set-entity response
			       (if handler
				   (mod-lisp-expander request
						      response
						      pathname
						      handler)
				   (->pathname pathname))))))
	    (receive (default-type handler) (get-subtree-handler relative)
	      (let ((pathname (merge-pathnames relative root-dir)))
		(if handler
		    (expand pathname default-type handler)
		    (begin
		      (maybe-parse-post-variables request)
		      (handle-request:default request
					      response
					      pathname
					      expand))))))
	  (if trace-requests?
	      (pp `(RESPONSE ,@(map (lambda (p)
				      (list (car p) (cdr p)))
				    (http-message-headers response)))))
	  response)))))

(define (url->relative url server-root)
  (cond ((rewrite-homedir url)
	 => (lambda (path)
	      (cond ((string-prefix? server-root path)
		     (values server-root
			     (string-tail path (string-length server-root))))
		    ((string-prefix? "/" path)
		     (values "/" (string-tail path 1)))
		    (else
		     (error "Unknown home path:" path)))))
	((string-prefix? "/" url)
	 (values server-root (string-tail url 1)))
	(else
	 (error "Unknown URL root:" url))))

(define (rewrite-homedir url)
  (let ((regs (re-string-match "^/~\\([^/]+\\)\\(.*\\)$" url)))
    (and regs
	 (rewrite-homedir-hook (re-match-extract url regs 1)
			       (let ((path (re-match-extract url regs 2)))
				 (if (string-prefix? "/" path)
				     (string-tail path 1)
				     path))))))

(define (rewrite-homedir-hook user-name path)
  (let ((dir
	 (ignore-errors
	  (lambda ()
	    (user-home-directory user-name)))))
    (and (not (condition? dir))
	 (string-append (->namestring dir)
			"public_html/"
			path))))

(define *root-dir*)
(define trace-requests? #f)

(define (handle-request:default request response pathname expand)
  (let ((page-found
	 (lambda (pathname)
	   (let ((type (file-content-type pathname)))
	     (expand pathname type (get-mime-handler type)))))
	(page-not-found
	 (lambda ()
	   (status-response! response 404 (http-message-url request)))))
    (case (file-type-indirect pathname)
      ((REGULAR)
       (page-found pathname))
      ((DIRECTORY)
       (let ((pathname (find-index-page pathname)))
	 (if pathname
	     (page-found pathname)
	     (page-not-found))))
      (else
       (page-not-found)))))

(define (get-subtree-handler relative)
  (let ((entry
	 (find-matching-item subtree-handlers
	   (lambda (entry)
	     (let loop
		 ((d1 (pathname-directory (vector-ref entry 0)))
		  (d2 (pathname-directory relative)))
	       (or (not (pair? d1))
		   (and (pair? d2)
			(equal? (car d1) (car d2))
			(loop (cdr d1) (cdr d2)))))))))
    (if entry
	(values (vector-ref entry 1) (vector-ref entry 2))
	(values #f #f))))

(define (define-subtree-handler pathname default-type handler)
  (let ((pathname (pathname-as-directory pathname)))
    (let ((entry
	   (find-matching-item subtree-handlers
	     (lambda (entry)
	       (pathname=? (vector-ref entry 0) pathname)))))
      (if entry
	  (begin
	    (vector-set! entry 1 default-type)
	    (vector-set! entry 2 handler))
	  (begin
	    (set! subtree-handlers
		  (cons (vector pathname default-type handler)
			subtree-handlers))
	    unspecific)))))

(define subtree-handlers '())

(define (find-index-page directory)
  (let ((directory (pathname-as-directory directory)))
    (let ((filename
	   (find-matching-item default-index-pages
	     (lambda (filename)
	       (file-exists? (merge-pathnames filename directory))))))
      (and filename
	   (merge-pathnames filename directory)))))

(define default-index-pages
  '("index.html" "index.xhtml" "index.ssp" "index.xml"))

(define (mod-lisp-expander request response pathname expander)
  (run-hooks-in-list mod-lisp-before-expander-hooks request)
  (call-with-output-string
    (lambda (port)
      (fluid-let ((*current-request* request)
		  (*current-response* response)
		  (*current-pathname* pathname)
		  (expander-eval
		   (lambda (expression environment)
		     (with-repl-eval-boundary (nearest-repl)
		       (lambda ()
			 (eval expression environment))))))
	(expander pathname port))
      (run-hooks-in-list mod-lisp-after-expander-hooks request response))))

(define mod-lisp-before-expander-hooks (make-hook-list))
(define mod-lisp-after-expander-hooks (make-hook-list))

(define *current-request*)
(define *current-response*)
(define *current-pathname*)

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
		     (let ((index
			    (->namestring
			     (pathname-new-type "index" extension))))
		       (if (not (member index default-index-pages))
			   (set! default-index-pages
				 (append default-index-pages
					 (list index)))))
		     (hash-table/put! mime-extensions extension (car type)))
		   (cdr type)))
	(else
	 (error:wrong-type-argument type "MIME type" 'DEFINE-MIME-HANDLER))))

(define mime-handlers (make-eq-hash-table))
(define mime-extensions (make-string-hash-table))

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
		   (if (not (memq method '(GET POST)))
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
		 (set-http-message-cookie-parameters!
		  request
		  (parse-parameters datum)))
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

;;;; HTTP message datatype

(define-structure (http-message (constructor make-http-message ()))
  (headers '())
  (headers-tail '())
  (entity #f)
  (method #f)
  (url #f)
  (url-parameters '())
  (post-parameters '())
  (cookie-parameters '()))

(define (add-header message keyword datum)
  (let ((p (assq keyword (http-message-headers message))))
    (if p
	(set-cdr! p datum)
	(let ((new (list (cons keyword datum)))
	      (tail (http-message-headers-tail message)))
	  (if tail
	      (set-cdr! tail new)
	      (set-http-message-headers! message new))
	  (set-http-message-headers-tail! message new)))))

(define (set-entity message entity)
  (add-header message
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

(define http-message-cookie-parameter
  (message-keyword-proc http-message-cookie-parameters
			'HTTP-MESSAGE-COOKIE-PARAMETER))

;;;; Status messages

(define (status-response code extra)
  (let ((response (make-http-message)))
    (status-response! response code extra)
    response))

(define (status-response! response code extra)
  (add-status-header response code)
  (add-content-type-header response 'text/html)
  (set-entity response
	      (call-with-output-string
		(lambda (port)
		  (let ((message (status-message code))
			(start
			 (lambda (name)
			   (write-char #\< port)
			   (write-string name port)
			   (write-char #\> port)
			   (newline port)))
			(end
			 (lambda (name)
			   (write-char #\< port)
			   (write-char #\/ port)
			   (write-string name port)
			   (write-char #\> port)
			   (newline port))))
		    (start "html")
		    (start "head")
		    (write-string "<title>" port)
		    (write-string message port)
		    (write-string "</title>" port)
		    (newline port)
		    (end "head")
		    (start "body")
		    (write-string "<h1>" port)
		    (write-string message port)
		    (write-string "</h1>" port)
		    (newline port)
		    (if extra
			(begin
			  (display extra port)
			  (newline port)))
		    (end "body")
		    (end "html"))))))

(define (status-message code)
  (case code
    ((200) "OK")
    ((404) "Not Found")
    ((500) "Internal Server Error")
    (else (error "Unknown status code:" code))))

(define (add-status-header message code)
  (add-header message
	      'STATUS
	      (call-with-output-string
		(lambda (port)
		  (write code port)
		  (write-char #\space port)
		  (write-string (status-message code) port)))))

(define (add-content-type-header message type)
  (add-header message 'CONTENT-TYPE (symbol-name type)))

;;;; Request/response accessors

(define (http-request-entity)
  (http-message-entity *current-request*))

(define (http-request-method)
  (http-message-method *current-request*))

(define (http-request-url)
  (http-message-url *current-request*))

(define (http-request-header-bindings)
  (http-message-headers *current-request*))

(define (http-request-url-parameter-bindings)
  (http-message-url-parameters *current-request*))

(define (http-request-post-parameter-bindings)
  (http-message-post-parameters *current-request*))

(define (http-request-cookie-parameter-bindings)
  (http-message-cookie-parameters *current-request*))

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

(define http-request-cookie-parameter
  (keyword-proc http-message-cookie-parameter))

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

(define (http-request-pathname)
  *current-pathname*)

(define (http-response-header keyword datum)
  (guarantee-symbol keyword 'HTTP-RESPONSE-HEADER)
  (guarantee-string datum 'HTTP-RESPONSE-HEADER)
  (if (memq keyword '(STATUS CONTENT-LENGTH))
      (error "Illegal header keyword:" keyword))
  (add-header *current-response* keyword datum))

(define (http-status-response code extra)
  (guarantee-exact-nonnegative-integer code 'HTTP-STATUS-RESPONSE)
  (guarantee-string extra 'HTTP-STATUS-RESPONSE)
  (status-response! *current-response* code extra))

(define (server-root-dir)
  *root-dir*)

(define (http-request-user-name)
  (http-message-user-name *current-request*))

(define (http-message-user-name message)
  (let ((auth (http-message-header message 'authorization)))
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

(define (html-content-type)
  (if (let ((type (http-browser-type)))
	(and (pair? type)
	     (eq? (car type) 'IE)))
      "text/xml"
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
      (write-line (list (get-universal-time)
			(http-message-method request)
			(http-message-url request)
			(http-message-user-name request)
			(http-message-post-parameters request))
		  request-log-port)))

(define request-log-port #f)