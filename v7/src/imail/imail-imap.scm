#| -*-Scheme-*-

$Id: imail-imap.scm,v 1.236 2008/09/25 15:16:09 riastradh Exp $

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

;;;; IMAIL mail reader: IMAP back end

(declare (usual-integrations))

;;;; URL

(define-class <imap-url> (<url>))
(define-url-protocol "imap" <imap-url>)

;; User name to connect as.
(define-generic imap-url-user-id (url))

;; Name or IP address of host to connect to.
(define-generic imap-url-host (url))

;; Port number to connect to.
(define-generic imap-url-port (url))

;; Name of mailbox to access.
(define-generic imap-url-mailbox (url))

(define-class <imap-folder-url> (<imap-url> <folder-url>)
  (user-id accessor imap-url-user-id)
  (host accessor imap-url-host)
  (port accessor imap-url-port)
  (mailbox accessor imap-url-mailbox)
  (list-time define standard initial-value #f)
  (exists? define standard)
  (selectable? define standard)
  (corresponding-container define standard))

(define-class <imap-container-url> (<imap-url> <container-url>)
  (corresponding-folder define accessor))

(let ((reflect-1
       (lambda (generic)
	 (define-method generic ((url <container-url>))
	   (generic (imap-container-url-corresponding-folder url))))))
  (reflect-1 imap-url-user-id)
  (reflect-1 imap-url-host)
  (reflect-1 imap-url-port)
  (reflect-1 url-exists?))

(define-method imap-url-mailbox ((url <container-url>))
  (string-append
   (imap-url-mailbox (imap-container-url-corresponding-folder url))
   "/"))

(define make-imap-url
  (let ((make-folder
	 (let ((constructor
		(instance-constructor <imap-folder-url>
				      '(USER-ID HOST PORT MAILBOX))))
	   (lambda (user-id host port mailbox)
	     (intern-url (constructor user-id host port mailbox)
			 imap-container-url))))
	(make-container
	 (let ((constructor
		(instance-constructor <imap-container-url>
				      '(CORRESPONDING-FOLDER))))
	   (lambda (folder)
	     (intern-url (constructor folder) imap-container-url)))))
    (lambda (user-id host port mailbox)
      (if (not mailbox)
	  (error:wrong-type-argument mailbox string 'MAKE-IMAP-URL))
      (let ((host (string-downcase host))
	    (mailbox (canonicalize-imap-mailbox mailbox)))
	(if (string-suffix? "/" mailbox)
	    (make-container
	     (make-folder user-id host port
			  (string-head mailbox
				       (fix:- (string-length mailbox) 1))))
	    (let ((folder (make-folder user-id host port mailbox)))
	      (if (string-null? mailbox)
		  (make-container folder)
		  folder)))))))

(define (imap-url-new-mailbox url mailbox)
  (make-imap-url (imap-url-user-id url)
		 (imap-url-host url)
		 (imap-url-port url)
		 mailbox))

(define-method url-body ((url <imap-url>))
  (make-imap-url-string url (imap-url-mailbox url)))

(define (make-imap-url-string url mailbox)
  (if (not mailbox)
      (error:wrong-type-argument mailbox string 'MAKE-IMAP-URL-STRING))
  (string-append "//"
		 (let ((user-id (imap-url-user-id url)))
		   (if (string=? user-id (current-user-name))
		       ""
		       (string-append (url:encode-string user-id) "@")))
		 (string-downcase (imap-url-host url))
		 (let ((port (imap-url-port url)))
		   (if (= port 143)
		       ""
		       (string-append ":" (number->string port))))
		 (if (or (string=? mailbox "")
			 (string=? mailbox "/"))
		     mailbox
		     (string-append
		      "/"
		      (url:encode-string
		       (canonicalize-imap-mailbox mailbox))))))

(define (canonicalize-imap-mailbox mailbox)
  (cond ((string-ci=? "inbox" mailbox) "inbox")
	((and (string-prefix-ci? "inbox/" mailbox)
	      (not (string-prefix? "inbox/" mailbox)))
	 (let ((mailbox (string-copy mailbox)))
	   (substring-downcase! mailbox 0 5)
	   mailbox))
	(else mailbox)))

(define (compatible-imap-urls? url1 url2)
  ;; Can URL1 and URL2 both be accessed from the same IMAP session?
  ;; E.g. can the IMAP COPY command work between them?
  (and (string=? (imap-url-user-id url1) (imap-url-user-id url2))
       (string=? (imap-url-host url1) (imap-url-host url2))
       (= (imap-url-port url1) (imap-url-port url2))))

(define-method url-exists? ((url <imap-folder-url>))
  (guarantee-imap-url-list-info url)
  (imap-folder-url-exists? url))

(define-method folder-url-is-selectable? ((url <imap-folder-url>))
  (guarantee-imap-url-list-info url)
  (imap-folder-url-selectable? url))

(define-method url-corresponding-container ((url <imap-folder-url>))
  (guarantee-imap-url-list-info url)
  (imap-folder-url-corresponding-container url))

(define (guarantee-imap-url-list-info url)
  (let ((t (get-universal-time))
	(list-time (imap-folder-url-list-time url)))
    (if (or (not list-time)
	    (> t (+ list-time imap-list-info-duration)))
	(if (null? (run-list-command url (imap-url-server-mailbox url)))
	    (begin
	      (set-imap-folder-url-list-time! url t)
	      (set-imap-folder-url-exists?! url #f)
	      (set-imap-folder-url-selectable?! url #f)
	      (set-imap-folder-url-corresponding-container! url #f))))))

(define (flush-imap-url-list-info url)
  (set-imap-folder-url-list-time!
   (if (imap-container-url? url)
       (imap-container-url-corresponding-folder url)
       url)
   #f))

;; Number of seconds for which LIST command info is assumed valid.
;; Info is automatically invalidated at times that IMAIL knows to do
;; so.  But other IMAP clients can invalidate this information without
;; notifying IMAIL, so we must periodically refresh the info from the
;; server.  (The protocol really ought to be fixed to provide
;; asynchronous updates to this information.)
(define imap-list-info-duration 60)

(define-method url-base-name ((url <imap-folder-url>))
  (let ((mailbox (imap-url-mailbox url)))
    (let ((index (imap-mailbox-container-slash mailbox)))
      (if index
	  (string-tail mailbox (fix:+ index 1))
	  mailbox))))

(define-method url-pass-phrase-key ((url <imap-url>))
  (make-url-string (url-protocol url) (make-imap-url-string url "")))

(define-method parse-url-body (string (default-url <imap-url>))
  (call-with-values (lambda () (parse-imap-url-body string default-url))
    (lambda (user-id host port mailbox)
      (if user-id
	  (make-imap-url user-id host port mailbox)
	  (error:bad-range-argument string 'PARSE-URL-BODY)))))

(define parse-imap-url-body
  (let ((parser
	 (let ((parse-server (imap:server-parser #f)))
	   (*parser
	    (alt (seq "//"
		      parse-server
		      (alt (seq "/" imap:parse:enc-mailbox)
			   imap:parse:enc-mailbox
			   (values #f)))
		 (seq (values #f #f #f)
		      (? "/")
		      imap:parse:enc-mailbox))))))
    (lambda (string default-url)
      (let ((v (parser (string->parser-buffer string))))
	(if v
	    (let ((user-id (vector-ref v 0))
		  (host (vector-ref v 1))
		  (port (vector-ref v 2))
		  (mailbox (vector-ref v 3)))
	      (values (or user-id
			  (imap-url-user-id default-url))
		      (or host
			  (imap-url-host default-url))
		      (or port
			  (if host 143 (imap-url-port default-url)))
		      (or mailbox
			  (imap-url-mailbox default-url))))
	    (values #f #f #f #f))))))

;;;; Container hierarchy

(define (imap-container-url url)
  (imap-url-new-mailbox url
			(or (imap-url-container-mailbox url)
			    "")))

(define-method container-url-for-prompt ((url <imap-url>))
  (imap-url-new-mailbox url
			(or (imap-url-container-mailbox url)
			    (get-personal-namespace url)
			    "")))

(define-method url-content-name ((url <imap-url>))
  (let* ((mailbox (imap-url-mailbox url))
	 (index (imap-mailbox-container-slash mailbox)))
    (if index
	(string-tail mailbox (fix:+ index 1))
	mailbox)))

(define-method make-content-url ((url <imap-container-url>) name)
  (imap-url-new-mailbox url (string-append (imap-url-mailbox url) name)))

(define (imap-url-container-mailbox url)
  (let ((mailbox (imap-url-mailbox url)))
    (let ((index (imap-mailbox-container-slash mailbox)))
      (and index
	   (string-head mailbox (fix:+ index 1))))))

(define (imap-mailbox-container-slash mailbox)
  (substring-find-previous-char mailbox
				0
				(let ((n (string-length mailbox)))
				  (if (string-suffix? "/" mailbox)
				      (fix:- n 1)
				      n))
				#\/))

(define (get-personal-namespace url)
  (let ((response
	 (let ((connection
		(search-imap-connections
		 (lambda (connection)
		   (and (compatible-imap-urls? (imap-connection-url connection)
					       url)
			(not (eq? (imap-connection-namespace connection)
				  'UNKNOWN))
			0)))))
	   (and connection
		(imap-connection-namespace connection)))))
    (and response
	 (let ((namespace (imap:response:namespace-personal response)))
	   (and (pair? namespace)
		(car namespace)
		(let ((prefix (imap:decode-mailbox-name (caar namespace)))
		      (delimiter (cadar namespace)))
		  (if delimiter
		      (if (string-ci=? "inbox/" prefix)
			  "inbox/"
			  (string-replace prefix (string-ref delimiter 0) #\/))
		      prefix)))))))

(define-method container-url-contents ((url <imap-container-url>))
  (%imap-mailbox-completions (imap-url-mailbox url) url))

;;;; Completion

(define-method %url-complete-string
    ((string <string>) (default-url <imap-url>)
		       if-unique if-not-unique if-not-found)
  (call-with-values (lambda () (imap-completion-args string default-url))
    (lambda (mailbox url)
      (if mailbox
	  (let ((convert
		 (lambda (mailbox) (make-imap-url-string url mailbox))))
	    (complete-imap-mailbox mailbox url
	      (lambda (mailbox)
		(if-unique (convert mailbox)))
	      (lambda (prefix get-mailboxes)
		(if-not-unique (convert prefix)
			       (lambda () (map convert (get-mailboxes)))))
	      if-not-found))
	  (if-not-found)))))

(define-method %url-string-completions
    ((string <string>) (default-url <imap-url>))
  (call-with-values (lambda () (imap-completion-args string default-url))
    (lambda (mailbox url)
      (if mailbox
	  (map (lambda (mailbox) (make-imap-url-string url mailbox))
	       (imap-mailbox-completions mailbox url))
	  '()))))

(define (imap-completion-args string default-url)
  (if (string-null? string)
      (values string default-url)
      (call-with-values (lambda () (parse-imap-url-body string default-url))
	(lambda (user-id host port mailbox)
	  (if user-id
	      (values mailbox (make-imap-url user-id host port "inbox"))
	      (values #f #f))))))

(define (complete-imap-mailbox mailbox url
			       if-unique if-not-unique if-not-found)
  (if (string-null? mailbox)
      (if-not-unique mailbox
		     (lambda () (imap-mailbox-completions mailbox url)))
      (let ((responses (imap-mailbox-completions mailbox url)))
	(cond ((not (pair? responses)) (if-not-found))
	      ((pair? (cdr responses))
	       (if-not-unique (string-greatest-common-prefix responses)
			      (lambda () responses)))
	      (else (if-unique (car responses)))))))

(define (imap-mailbox-completions prefix url)
  (map imap-url-mailbox (%imap-mailbox-completions prefix url)))

(define (%imap-mailbox-completions prefix url)
  (let loop
      ((urls
	(run-list-command
	 url
	 ;; Some IMAP servers don't like a mailbox of `/%' in LIST
	 ;; commands, and others simply returna uselessly empty
	 ;; result, so we have a special case for the root mailbox.
	 (if (string=? prefix "/")
	     "%"
	     (string-append (imap-mailbox/url->server url prefix) "%"))))
       (results '()))
    (if (pair? urls)
	(loop (cdr urls)
	      (cond ((imap-folder-url-selectable? (car urls))
		     (cons (car urls) results))
		    ((imap-folder-url-corresponding-container (car urls))
		     => (lambda (container-url)
			  ;; Some IMAP servers will return the
			  ;; container URL as an answer to the LIST
			  ;; command, but it is uninteresting here, so
			  ;; we filter it out.  (Should this filtering
			  ;; be done by RUN-LIST-COMMAND?)
			  (if (eq? container-url url)
			      results
			      (cons container-url results))))
		    (else results)))
	(reverse! results))))

(define (run-list-command url mailbox)
  (let ((t (get-universal-time)))
    (append-map (lambda (response)
		  (cond ((list-command-response-folder-url response url t)
			 => list)
			(else '())))
		(with-open-imap-connection url
		  (lambda (connection)
		    (imap:command:list connection "" mailbox))))))

(define (list-command-response-folder-url response url t)
  (let ((mailbox
	 (let ((delimiter (imap:response:list-delimiter response))
	       (mailbox
		(imap:decode-mailbox-name
		 (imap:response:list-mailbox response))))
	   (if delimiter
	       (string-replace mailbox (string-ref delimiter 0) #\/)
	       mailbox)))
	(flags (imap:response:list-flags response)))
    (let ((url (imap-url-new-mailbox url mailbox))
	  (noselect? (memq '\\NOSELECT flags))
	  (noinferiors? (memq '\\NOINFERIORS flags)))
      (if (and noselect? noinferiors?)
	  #f				;Completely uninteresting.
	  (receive (folder-url container-url)
	      (cond ((imap-folder-url? url)
		     (values url
			     (and (not noinferiors?)
				  (imap-url-new-mailbox url
							(string-append mailbox
								       "/")))))
		    ((imap-container-url? url)
		     (values (imap-container-url-corresponding-folder url)
			     (and (not noinferiors?) url)))
		    (else
		     (error "Bad IMAP URL returned by LIST:" url)))
	    (set-imap-folder-url-list-time! folder-url t)
	    (set-imap-folder-url-exists?! folder-url #t)
	    (set-imap-folder-url-selectable?! folder-url (not noselect?))
	    (set-imap-folder-url-corresponding-container! folder-url
							  container-url)
	    folder-url)))))

;;;; URL->server delimiter conversion

(define (imap-url-server-mailbox url)
  (imap-mailbox/url->server
   url
   (let ((mailbox (imap-url-mailbox url)))
     (if (string-suffix? "/" mailbox)
	 (string-head mailbox (fix:- (string-length mailbox) 1))
	 mailbox))))

(define (imap-mailbox/url->server url mailbox)
  (let ((delimiter (imap-mailbox-delimiter url mailbox)))
    (if (and delimiter (not (char=? delimiter #\/)))
	(string-replace mailbox #\/ delimiter)
	mailbox)))

(define (imap-mailbox-delimiter url mailbox)
  (let* ((slash (string-find-next-char mailbox #\/))
	 (root (if slash (string-head mailbox slash) mailbox))
	 (key (make-imap-url-string url root)))
    (hash-table/intern! imap-delimiters-table key
      (lambda ()
	(let ((delimiter
	       (imap:response:list-delimiter
		(with-open-imap-connection url
		  (lambda (connection)
		    (imap:command:get-delimiter connection root))))))
	  (and delimiter
	       (string-ref delimiter 0)))))))

(define imap-delimiters-table
  (make-equal-hash-table))

;;;; Server connection

(define-class <imap-connection> ()
  ;; The URL of the mailbox this connection has selected, if any.  If
  ;; it doesn't have a mailbox selected, the URL will have a null
  ;; string for its mailbox component.
  (url             define accessor)
  ;; If a folder has claimed this connection, it is stored here.
  (folder          define standard initial-value #f)
  (port            define standard initial-value #f)
  (greeting        define standard initial-value #f)
  (capabilities    define standard initial-value '())
  (namespace       define standard initial-value 'UNKNOWN)
  (sequence-number define standard initial-value 0)
  (response-queue  define accessor initializer (lambda () (cons '() '())))
  (reference-count define standard initial-value 0))

(define-method write-instance ((connection <imap-connection>) port)
  (write-instance-helper 'IMAP-CONNECTION connection port
    (lambda ()
      (write-char #\space port)
      (write (url-body (imap-connection-url connection)) port))))

(define (reset-imap-connection connection)
  (without-interrupts
   (lambda ()
     (set-imap-connection-url! connection #f)
     (set-imap-connection-greeting! connection #f)
     (set-imap-connection-capabilities! connection '())
     (set-imap-connection-sequence-number! connection 0)
     (let ((queue (imap-connection-response-queue connection)))
       (set-car! queue '())
       (set-cdr! queue '())))))

(define set-imap-connection-url!
  (let ((modifier (slot-modifier <imap-connection> 'URL)))
    (lambda (connection url)
      (modifier
       connection
       (or url (imap-url-new-mailbox (imap-connection-url connection) ""))))))

(define (next-imap-command-tag connection)
  (let ((n (imap-connection-sequence-number connection)))
    (set-imap-connection-sequence-number! connection (+ n 1))
    (nonnegative-integer->base26-string n 3)))

(define (nonnegative-integer->base26-string n min-length)
  (let ((s
	 (make-string (max (ceiling->exact (/ (log (+ n 1)) (log 26)))
			   min-length)
		      #\A)))
    (let loop ((n n) (i (fix:- (string-length s) 1)))
      (let ((q.r (integer-divide n 26)))
	(string-set! s i (string-ref "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (cdr q.r)))
	(if (not (= (car q.r) 0))
	    (loop (car q.r) (fix:- i 1)))))
    s))

(define (base26-string->nonnegative-integer s)
  (let ((end (string-length s)))
    (let loop ((start 0) (n 0))
      (if (fix:< start end)
	  (let ((digit (- (vector-8b-ref s start) (char->integer #\A))))
	    (if (not (<= 0 digit 25))
		(error:bad-range-argument s
					  'BASE26-STRING->NONNEGATIVE-INTEGER))
	    (loop (fix:+ start 1) (+ (* n 26) digit)))
	  n))))

(define (increment-connection-reference-count! connection)
  (set-imap-connection-reference-count!
   connection
   (+ (imap-connection-reference-count connection) 1)))

(define (decrement-connection-reference-count! connection)
  (set-imap-connection-reference-count!
   connection
   (- (imap-connection-reference-count connection) 1)))

(define (enqueue-imap-response connection response)
  (let ((queue (imap-connection-response-queue connection)))
    (let ((next (cons response '())))
      (without-interrupts
       (lambda ()
	 (if (pair? (cdr queue))
	     (set-cdr! (cdr queue) next)
	     (set-car! queue next))
	 (set-cdr! queue next))))))

(define (dequeue-imap-responses connection)
  (let ((queue (imap-connection-response-queue connection)))
    (without-interrupts
     (lambda ()
       (let ((responses (car queue)))
	 (set-car! queue '())
	 (set-cdr! queue '())
	 responses)))))

(define (get-folder-imap-connection url)
  (or (search-imap-connections
       (lambda (connection)
	 (if (eq? (imap-connection-url connection) url)
	     2
	     (and (compatible-imap-urls? (imap-connection-url connection) url)
		  (not (imap-connection-folder connection))
		  (if (test-imap-connection-open connection) 1 0)))))
      (make-imap-connection url)))

(define (get-compatible-imap-connection url)
  (or (search-imap-connections
       (lambda (connection)
	 (and (compatible-imap-urls? (imap-connection-url connection) url)
	      (if (test-imap-connection-open connection) 1 0))))
      (make-imap-connection url)))

(define (search-imap-connections assessor)
  (let loop ((connections memoized-imap-connections) (prev #f) (winner #f))
    (if (weak-pair? connections)
	(let ((connection (weak-car connections)))
	  (if connection
	      (loop (weak-cdr connections)
		    connections
		    (let ((value (assessor connection)))
		      (if (and value
			       (or (not winner)
				   (> value (cdr winner))))
			  (cons connection value)
			  winner)))
	      (let ((next (weak-cdr connections)))
		(if prev
		    (weak-set-cdr! prev next)
		    (set! memoized-imap-connections next))
		(loop next prev winner))))
	(and winner (car winner)))))

(define make-imap-connection
  (let ((constructor (instance-constructor <imap-connection> '(URL))))
    (lambda (url)
      (let ((connection (constructor (imap-url-new-mailbox url ""))))
	(without-interrupts
	 (lambda ()
	   (set! memoized-imap-connections
		 (weak-cons connection memoized-imap-connections))))
	connection))))

(define memoized-imap-connections '())

(define (guarantee-imap-connection-open connection)
  (stop-pending-connection-closure connection)
  (if (test-imap-connection-open connection)
      #f
      (let ((url (imap-connection-url connection)))
	(let ((port
	       ((imail-ui:message-wrapper "Connecting to " (imap-url-host url))
		(lambda ()
		  (open-tcp-stream-socket (imap-url-host url)
					  (or (imap-url-port url) "imap2"))))))
	  (port/set-line-ending port 'NEWLINE)
	  (let ((response
		 (imap:catch-no-response #f
		   (lambda ()
		     (let ((finished? #f))
		       (dynamic-wind
			(lambda () unspecific)
			(lambda ()
			  (reset-imap-connection connection)
			  (set-imap-connection-port! connection port)
			  (set-imap-connection-greeting!
			   connection
			   (let ((response (imap:read-server-response-1 port)))
			     (if (imap:response:ok? response)
				 (imap:response:response-text-string response)
				 response)))
			  (imap:command:capability connection)
			  (if (not (memq 'IMAP4REV1
					 (imap-connection-capabilities
					  connection)))
			      (error "Server doesn't support IMAP4rev1:" url))
			  (let ((response
				 (imail-ui:call-with-pass-phrase url
				   (lambda (pass-phrase)
				     (imap:command:login connection
							 (imap-url-user-id url)
							 pass-phrase)))))
			    (set! finished? #t)
			    response))
			(lambda ()
			  (if (not finished?)
			      (close-imap-connection connection)))))))))
	    (if (imap:response:no? response)
		(begin
		  (imail-ui:delete-stored-pass-phrase url)
		  (error "Unable to log in:"
			 (imap:response:response-text-string response))))))
	(if (eq? (imap-connection-namespace connection) 'UNKNOWN)
	    (set-imap-connection-namespace!
	     connection
	     (and (memq 'NAMESPACE (imap-connection-capabilities connection))
		  (imap:command:namespace connection))))
	#t)))

(define (with-open-imap-connection url receiver)
  (let ((connection (get-compatible-imap-connection url)))
    (dynamic-wind (lambda ()
		    (increment-connection-reference-count! connection))
		  (lambda ()
		    (guarantee-imap-connection-open connection)
		    (let ((v (receiver connection)))
		      (maybe-close-imap-connection connection 1 #f)
		      v))
		  (lambda ()
		    (decrement-connection-reference-count! connection)))))

(define (test-imap-connection-open connection)
  (let ((port (imap-connection-port connection)))
    (and port
	 (let ((lose
		(lambda ()
		  (process-queued-responses connection #f)
		  (close-imap-connection connection)
		  #f)))
	   (let loop ()
	     (cond ((not (char-ready? port))
		    (process-queued-responses connection #f)
		    #t)
		   ((eof-object? (peek-char port))
		    (lose))
		   (else
		    (let ((response
			   (ignore-errors
			    (lambda ()
			      (imap:read-server-response-1 port)))))
		      (if (or (condition? response)
			      (begin
				(enqueue-imap-response connection response)
				(imap:response:bye? response)))
			  (lose)
			  (loop))))))))))

(define (close-imap-connection-cleanly connection)
  (if (test-imap-connection-open connection)
      (imap:command:logout connection))
  (close-imap-connection connection))

(define (close-imap-connection connection)
  (let ((port
	 (without-interrupts
	  (lambda ()
	    (let ((port (imap-connection-port connection)))
	      (set-imap-connection-port! connection #f)
	      port)))))
    (if port
	(close-port port)))
  (reset-imap-connection connection))

(define (maybe-close-imap-connection connection min-count no-defer?)
  (if (= (imap-connection-reference-count connection) min-count)
      (if (or no-defer?
	      (search-imap-connections
	       (let ((url (imap-connection-url connection)))
		 (lambda (connection*)
		   (and (not (eq? connection* connection))
			(compatible-imap-urls?
			 (imap-connection-url connection*)
			 url)
			0)))))
	  (close-imap-connection-cleanly connection)
	  (defer-closing-of-connection connection))))

(define (defer-closing-of-connection connection)
  (without-interrupts
   (lambda ()
     (let ((entry (assq connection connections-awaiting-closure))
	   (t (+ (get-universal-time) connection-closure-delay)))
       (if entry
	   (set-cdr! entry t)
	   (set! connections-awaiting-closure
		 (cons (cons connection t)
		       connections-awaiting-closure))))
     (if (not connection-closure-thread-registration)
	 (begin
	   (set! connection-closure-thread-registration
		 (start-standard-polling-thread
		  connection-closure-thread-interval
		  connection-closure-output-processor))
	   unspecific)))))

(define (connection-closure-output-processor)
  (for-each close-imap-connection-cleanly
	    (without-interrupts
	     (lambda ()
	       (let ((t (get-universal-time)))
		 (let loop
		     ((this connections-awaiting-closure)
		      (prev #f)
		      (connections '()))
		   (if (pair? this)
		       (let ((next (cdr this)))
			 (if (>= t (cdar this))
			     (begin
			       (if prev
				   (set-cdr! prev next)
				   (set! connections-awaiting-closure next))
			       (loop next prev (cons (caar this) connections)))
			     (loop next this connections)))
		       (begin
			 (%maybe-stop-connection-closure-thread)
			 connections)))))))
  #f)

(define (stop-pending-connection-closure connection)
  (without-interrupts
   (lambda ()
     (set! connections-awaiting-closure
	   (del-assq! connection connections-awaiting-closure))
     (%maybe-stop-connection-closure-thread))))

(define (%maybe-stop-connection-closure-thread)
  ;; Interrupts are assumed off here.
  (if (and (null? connections-awaiting-closure)
	   connection-closure-thread-registration)
      (begin
	(stop-standard-polling-thread connection-closure-thread-registration)
	(set! connection-closure-thread-registration #f)
	unspecific)))

(define connections-awaiting-closure '())
(define connection-closure-delay 60)	;seconds
(define connection-closure-thread-interval (* 10 1000))	;milliseconds
(define connection-closure-thread-registration #f)

;;;; Folder and container datatypes

(define-class <imap-folder> (<folder>)
  (connection define standard
	      initial-value #f)
  (read-only? define standard)
  (allowed-flags define standard)
  (permanent-flags define standard)
  (permanent-keywords? define standard)
  (uidnext define standard)
  (uidvalidity define standard)
  (unseen define standard)
  ;; MESSAGES-SYNC-STATUS is the status of synchronization between our
  ;; folder and the server.  This can be
  ;;   #F,      meaning that we have lost all synchrony;
  ;;   LENGTH,  meaning that the folder's length agrees with the most
  ;;              recent EXISTS response from the server;
  ;;   UID,     meaning that our folder agrees with the server on the
  ;;              messages' UIDs, as of the last response from the
  ;;              server; or
  ;;   FLAGS,   meaning that our folder agrees with the esrver on the
  ;;              messages' flags, as of the last response from the
  ;;              server.
  ;; Each case implies all preceding cases, so that, for example, if
  ;; we agree on the flags, then we certainly agree on the length.
  ;; When updating the length as new messages are added, we may leave
  ;; the folder in a suboptimal state claiming that the length is
  ;; synchronized but the flags are not, while in fact all the
  ;; messages except the new ones have their flags synchronized.  This
  ;; approximation is much simpler to understand, however, and less
  ;; likely to fail in the case of user interrupts and other aborts.
  ;; If uninterrupted, UPDATE-IMAP-FOLDER-LENGTH! will return the
  ;; synchronization status to the most optimal value it can.
  (messages-sync-status define standard)
  (length accessor folder-length
	  define modifier
	  initial-value 0)
  (messages define standard
	    initial-value '#()))

(define-class (<imap-container> (constructor (locator))) (<container>)
  (connection define standard
	      initial-value #f))

(define make-imap-folder
  (let ((constructor (instance-constructor <imap-folder> '(LOCATOR))))
    (lambda (url)
      (let ((folder (constructor url)))
	(reset-imap-folder! folder)
	folder))))

(define (reset-imap-folder! folder)
  (without-interrupts
   (lambda ()
     (detach-all-messages! folder)
     (set-imap-folder-read-only?! folder #f)
     (set-imap-folder-allowed-flags! folder '())
     (set-imap-folder-permanent-flags! folder '())
     (set-imap-folder-permanent-keywords?! folder #f)
     (set-imap-folder-uidnext! folder #f)
     (set-imap-folder-uidvalidity! folder #f)
     (set-imap-folder-unseen! folder #f)
     (set-imap-folder-messages-sync-status! folder #f)
     (set-imap-folder-length! folder 0)
     (set-imap-folder-messages! folder (initial-messages)))))

(define (set-imap-folder-messages-synchronized?! folder status)
  (if (not (memq status '(#F LENGTH UID FLAGS)))
      (error:bad-range-argument status
				'SET-IMAP-FOLDER-MESSAGES-SYNCHRONIZED?!))
  (set-imap-folder-messages-sync-status! folder status))

(define (imap-folder-messages-synchronized? folder status)
  (let ((list (memq status '(#F LENGTH UID FLAGS))))
    (if (not list)
	(error:bad-range-argument status 'IMAP-FOLDER-MESSAGES-SYNCHRONIZED?))
    (and (memq (imap-folder-messages-sync-status folder) list))))

(define (guarantee-imap-folder-connection folder)
  (without-interrupts
   (lambda ()
     (or (imap-folder-connection folder)
	 (let ((connection
		(get-folder-imap-connection (resource-locator folder))))
	   (set-imap-connection-folder! connection folder)
	   (increment-connection-reference-count! connection)
	   (set-imap-folder-connection! folder connection)
	   connection)))))

(define (guarantee-imap-folder-open folder)
  (let ((connection (guarantee-imap-folder-connection folder))
	(url (resource-locator folder)))
    (if (or (guarantee-imap-connection-open connection)
	    (not (eq? (imap-connection-url connection) url))
	    (not (imap-folder-messages-synchronized? folder 'LENGTH)))
	(begin
	  (set-imap-folder-messages-synchronized?! folder #f)
	  (let ((selected? #f))
	    (dynamic-wind
	     (lambda ()
	       (set-imap-connection-url! connection url))
	     (lambda ()
	       (imap:command:select connection (imap-url-server-mailbox url))
	       (set! selected? #t)
	       unspecific)
	     (lambda ()
	       (if (not selected?)
		   (set-imap-connection-url! connection #f)))))
	  (object-modified! folder 'STATUS)))
    connection))

(define (new-imap-folder-uidvalidity! folder uidvalidity)
  (without-interrupts
   (lambda ()
     (detach-all-messages! folder)
     (fill-messages-vector! folder 0)
     ;; Downgrade the synchronization status to LENGTH.  Don't upgrade
     ;; it if we didn't have the length anyway, however.
     (if (imap-folder-messages-synchronized? folder 'LENGTH)
	 (set-imap-folder-messages-synchronized?! folder 'LENGTH))
     (if (imap-folder-uidvalidity folder)
	 (set-imap-folder-unseen! folder #f))
     (set-imap-folder-uidvalidity! folder uidvalidity)))
  (read-message-headers! folder 0)
  (set-imap-folder-messages-synchronized?! folder 'FLAGS)
  (clean-cache-directory folder))

(define (detach-all-messages! folder)
  (let ((v (imap-folder-messages folder))
	(n (folder-length folder)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i n))
      (detach-message! (vector-ref v i)))))

(define (fill-messages-vector! folder start)
  (let ((v (imap-folder-messages folder))
	(n (folder-length folder)))
    (do ((index start (fix:+ index 1)))
	((fix:= index n))
      (vector-set! v index (make-imap-message folder index)))))

(define (read-message-headers! folder start)
  (if (and (imap-folder-uidvalidity folder)
	   (> (folder-length folder) start))
      ((imail-ui:message-wrapper "Reading message UIDs")
       (lambda ()
	 (imap:command:fetch-range (imap-folder-connection folder)
				   start #f '(UID FLAGS))))))

(define (remove-imap-folder-message folder index)
  (let ((message (%get-message folder index)))
    (let ((unmapped-index (message-index message))
	  (key (message-order-key message)))
      (delete-cached-message message)
      (without-interrupts
       (lambda ()
	 (let ((v (imap-folder-messages folder))
	       (n (fix:- (folder-length folder) 1)))
	   (detach-message! (vector-ref v index))
	   (do ((i index (fix:+ i 1)))
	       ((fix:= i n))
	     (let ((m (vector-ref v (fix:+ i 1))))
	       (set-message-index! m i)
	       (vector-set! v i m)))
	   (vector-set! v n #f)
	   (set-imap-folder-length! folder n)
	   (set-imap-folder-unseen! folder #f)
	   (let ((new-length (compute-messages-length v n)))
	     (if new-length
		 (set-imap-folder-messages! folder
					    (vector-head v new-length))))
	   (object-modified! folder 'EXPUNGE
			     message index unmapped-index key)))))))

(define (initial-messages)
  (make-vector 64 #f))

(define (compute-messages-length v count)
  (let ((old-length (vector-length v))
	(min-length 64))
    (if (> count old-length)
	(let loop ((n (* old-length 2)))
	  (if (<= count n)
	      n
	      (loop (* n 2))))
	(and (> old-length min-length)
	     (<= count (quotient old-length 2))
	     (let loop ((n (quotient old-length 2)))
	       (let ((n/2 (quotient n 2)))
		 (if (or (> count n/2) (= n min-length))
		     n
		     (loop n/2))))))))

;;; UPDATE-IMAP-FOLDER-LENGTH! needs explanation.  There are two basic
;;; cases.

;;; In the first case, our folder is synchronized with the server,
;;; meaning that our folder has the same length and UIDs as the
;;; server's mailbox.  In that case, length changes can only be
;;; increases, and we know that no deletions occur except those
;;; reflected by EXPUNGE responses (both constraints required by the
;;; IMAP specification).

;;; In the second case, we have lost synchrony with the server,
;;; usually because the connection was closed and then reopened.  Here
;;; we must resynchronize, matching up messages by UID.  Our strategy
;;; is to detach all of the existing messages, create a new message
;;; set with empty messages, read in the UIDs for the new messages,
;;; then match up the old messages with the new.  Any old message that
;;; matches a new one replaces it in the folder, thus preserving
;;; message pointers where possible.  However, we stop if we find a
;;; message whose UID is not known, in case a previous attempt to read
;;; the UIDs was aborted in the middle.

;;; The reason for this complexity in the second case is that we can't
;;; be guaranteed that we will complete reading the UIDs for the new
;;; messages, either due to error or the user aborting the read.  So
;;; we must have everything in a consistent (if nonoptimal) state
;;; while reading.  If the read finishes, we can do the match/replace
;;; operation atomically.

(define (update-imap-folder-length! folder count)
  (with-interrupt-mask interrupt-mask/gc-ok
    (lambda (interrupt-mask)
      (cond ((or (imap-folder-messages-synchronized? folder 'FLAGS)
		 (zero? (folder-length folder)))
	     (increase-imap-folder-length! folder count interrupt-mask))
	    (else
	     (synchronize-imap-folder-length! folder count interrupt-mask)))))
  (clean-cache-directory folder))

(define (increase-imap-folder-length! folder count interrupt-mask)
  (let ((v (imap-folder-messages folder))
	(n (folder-length folder)))
    (cond ((> count n)
	   (let ((new-length (compute-messages-length v count)))
	     (if new-length
		 (set-imap-folder-messages!
		  folder
		  (vector-grow v new-length #f))))
	   (set-imap-folder-length! folder count)
	   (fill-messages-vector! folder n)
	   (set-imap-folder-messages-synchronized?! folder 'LENGTH)
	   (with-interrupt-mask interrupt-mask
	     (lambda (interrupt-mask)
	       interrupt-mask
	       (read-message-headers! folder n)))
	   (set-imap-folder-messages-synchronized?! folder 'FLAGS)
	   (object-modified! folder 'INCREASE-LENGTH n count))
	  ((= count n)
	   (set-imap-folder-messages-synchronized?! folder 'FLAGS))
	  (else
	   (error "EXISTS response decreased folder length:" folder)))))

(define (synchronize-imap-folder-length! folder count interrupt-mask)
  (detach-all-messages! folder)
  (let ((v (imap-folder-messages folder))
	(n (folder-length folder)))
    (set-imap-folder-length! folder count)
    (set-imap-folder-messages!
     folder
     (make-vector (or (compute-messages-length v count)
		      (vector-length v))
		  #f))
    (fill-messages-vector! folder 0)
    (set-imap-folder-messages-synchronized?! folder 'LENGTH)
    (if (> count 0)
	(with-interrupt-mask interrupt-mask
	  (lambda (interrupt-mask)
	    interrupt-mask
	    (read-message-headers! folder 0))))
    (set-imap-folder-messages-synchronized?! folder 'FLAGS)
    (let ((event (synchronize-imap-folder-messages! folder n v)))
      (if event
	  (object-modified! folder event n count)))))

;;; SYNCHRONIZE-IMAP-FOLDER-MESSAGES! returns the event that just
;;; occurred -- either SET-LENGTH, INCREASE-LENGTH, or #F.  It would
;;; be nice to avoid SET-LENGTH events altogether if a small number of
;;; messages have been expunged, and just to signal EXPUNGE events for
;;; each message missing from the server.  However, this is sketchy
;;; for two reasons: 
;;;
;;; 1. The EXPUNGE events would need to take a collection of messages,
;;;    rather than a single one, so that we can buffer them up and
;;;    avoid quadratic-time algorithms.  This is not hard, but it's
;;;    not clear at what point it is better just to signal a
;;;    SET-LENGTH event than to signal a lot of messages expunged.
;;;
;;; 2. EXPUNGE events include the message's original index and order
;;;    key, which is necessary for the ordering code to delete them
;;;    from the order tree.  If we had not previously determined the
;;;    message's order key, however, and if we don't have it cached on
;;;    disk (which is something we ought never to rely on), then we
;;;    might end up asking the server for it -- which won't work,
;;;    because the message has been expunged.
;;;
;;; So for now all we concern ourselves with is whether messages have
;;; been only added to the folder, which is the common case for a
;;; single client when it reconnects to the server for new mail.

(define (synchronize-imap-folder-messages! folder n v)
  (let ((v* (imap-folder-messages folder))
	(n* (folder-length folder)))
    (let loop ((i 0) (i* 0) (synchronized? #t))
      (if (and (fix:< i n)
	       (fix:< i* n*)
	       (%imap-message-uid-initialized? (vector-ref v i)))
	  (let ((m (vector-ref v i))
		(m* (vector-ref v* i*)))
	    (if (= (%imap-message-uid m) (%imap-message-uid m*))
		(begin
		  ;; Flags might have been updated while
		  ;; reading the UIDs.
		  (if (%message-flags-initialized? m*)
		      (%set-message-flags! m (message-flags m*)))
		  (detach-message! m*)
		  (attach-message! m folder i*)
		  (vector-set! v* i* m)
		  (loop (fix:+ i 1) (fix:+ i* 1) synchronized?))
		(begin
		  (if (> (imap-message-uid m)
			 (imap-message-uid m*))
		      (error "Message inserted into folder:" m*))
		  (loop (fix:+ i 1) i* #f))))
	  (if synchronized?
	      (and (< n n*) 'INCREASE-LENGTH)
	      'SET-LENGTH)))))

;;;; Message datatype

(define-class (<imap-message> (constructor (folder index))) (<message>)
  (uid)
  (length)
  (envelope)
  (bodystructure)
  (body-parts define standard initializer (lambda () (weak-cons #f '())))
  (cached-keywords define standard initial-value '()))

(define-generic imap-message-uid (message))
(define-generic imap-message-envelope (message))

(define-method set-message-flags! ((message <imap-message>) flags)
  (with-imap-message-open message
    (lambda (connection)
      (imap:command:uid-store-flags
       connection
       (imap-message-uid message)
       (map imail-flag->imap-flag
	    (let ((flags (flags-delete "recent" flags))
		  (folder (message-folder message)))
	      (if (imap-folder-permanent-keywords? folder)
		  flags
		  (list-transform-positive flags
		    (let ((allowed-flags (imap-folder-allowed-flags folder)))
		      (lambda (flag)
			(flags-member? flag allowed-flags)))))))))))

(define-method %message-permanent-flags ((message <imap-message>))
  ;; Perhaps this should intersect the flags with the folder's list of
  ;; permanent flags, if the folder does not allow permanent
  ;; user-defined flags, in order to preserve only those flags that
  ;; the IMAP folder would consider permanent.
  (flags-delete "recent" (message-flags message)))

(define (imap-flag->imail-flag flag)
  (let ((entry (assq flag standard-imap-flags)))
    (if entry
	(cdr entry)
	(symbol->string flag))))

(define (imail-flag->imap-flag flag)
  (let ((entry
	 (list-search-positive standard-imap-flags
	   (lambda (entry)
	     (string-ci=? flag (cdr entry))))))
    (if entry
	(car entry)
	(intern flag))))

(define standard-imap-flags
  (map (lambda (s)
	 (cons (intern (string-append "\\" s))
	       s))
       '("seen" "answered" "flagged" "deleted" "draft" "recent")))

(define-method message-internal-time ((message <imap-message>))
  (fetch-one-message-item message 'INTERNALDATE "internal date"))

(define (with-imap-message-open message receiver)
  (let ((folder (message-folder message)))
    (if folder
	(receiver (guarantee-imap-folder-open folder)))))

;;; These reflectors are needed to guarantee that we read the
;;; appropriate information from the server.  Some message slots are
;;; filled in by READ-MESSAGE-HEADERS!, but it's possible for
;;; READ-MESSAGE-HEADERS! to be interrupted, leaving unfilled slots.

(define %imap-message-uid (slot-accessor <imap-message> 'UID))
(define %imap-message-uid-initialized? (slot-initpred <imap-message> 'UID))

(define-method imap-message-uid ((message <imap-message>))
  (if (not (%imap-message-uid-initialized? message))
      (with-imap-message-open message
	(lambda (connection)
	  (let ((index (%message-index message)))
	    (let ((suffix
		   (string-append " UID for message "
				  (number->string (+ index 1)))))
	      ((imail-ui:message-wrapper "Reading" suffix)
	       (lambda ()
		 (imap:command:fetch connection index '(UID))
		 (if (not (%imap-message-uid message))
		     (begin
		       ;; Still don't have the goods.  Send a NOOP, in
		       ;; case the server is holding it back because it
		       ;; also needs to send an EXPUNGE.
		       (imap:command:noop connection)
		       (if (not (%imap-message-uid-initialized? message))
			   (error
			    (string-append "Unable to obtain"
					   suffix))))))))))))
  (%imap-message-uid message))

(define (guarantee-slot-initialized message initpred noun keywords)
  (if (not (initpred message))
      (let ((suffix
	     (string-append " " noun " for message "
			    (number->string (+ (%message-index message) 1)))))
	(fetch-message-items message keywords suffix)
	(if (not (initpred message))
	    (error (string-append "Unable to obtain" suffix))))))

(let ((reflector
       (lambda (generic-procedure slot-name noun keywords)
	 (let ((accessor (slot-accessor <imap-message> slot-name))
	       (initpred (slot-initpred <imap-message> slot-name)))
	   (define-method generic-procedure ((message <imap-message>))
	     (guarantee-slot-initialized message initpred noun keywords)
	     (accessor message))))))
  (reflector message-flags 'FLAGS "flags" '(FLAGS))
  (reflector message-length 'LENGTH "length" '(RFC822.SIZE))
  (reflector mime-entity-body-structure 'BODYSTRUCTURE "MIME structure"
	     '(BODYSTRUCTURE)))

;;; Some hair to keep weak references to header fields and envelopes,
;;; which we don't really care to keep around longer than we must.

(let ((reflector
       (lambda (generic-procedure slot-name noun keyword constructor)
	 (let ((accessor (slot-accessor <imap-message> slot-name))
	       (initpred (slot-initpred <imap-message> slot-name))
	       (modifier (slot-modifier <imap-message> slot-name)))
	   (define (fetch message store)
	     ((lambda (value)
		(store value)
		value)
	      (constructor (fetch-one-message-item message keyword noun))))
	   (define-method generic-procedure ((message <imap-message>))
	     (if (initpred message)
		 (let* ((pair (accessor message))
			(value (weak-car pair)))
		   (if (weak-pair/car? pair)
		       value
		       (fetch message
			      (lambda (value) (weak-set-car! pair value)))))
		 (fetch message
			(lambda (value)
			  (modifier message (weak-cons value '()))))))))))
  (reflector message-header-fields 'HEADER-FIELDS "header" 'RFC822.HEADER
    string->header-fields)
  (reflector imap-message-envelope 'ENVELOPE "envelope" 'ENVELOPE
    (lambda (envelope)
      (parse-mime-envelope envelope))))

(define (fetch-one-message-item message keyword noun)
  (imap:response:fetch-attribute
   (fetch-message-items message
			(list keyword)
			(string-append
			 " " noun " for message "
			 (number->string (+ (%message-index message) 1))))
   keyword))

;;;; Preloading Folder Outlines & Caching Folder Contents

;;; Keywords for summary buffers' message outlines.

(define imap-outline-keywords
  '(FLAGS INTERNALDATE RFC822.HEADER RFC822.SIZE))

;;; Keywords for displaying message content.

(define imap-content-keywords
  ;; What other keywords would be useful here?
  (append '(BODYSTRUCTURE) imap-outline-keywords))

;;; Keywords that are not to be written into the disk cache.

(define imap-dynamic-keywords
  '(FLAGS))

(define-method preload-folder-outlines
    ((folder <imap-folder>) #!optional messages)
  (fill-imap-message-cache folder imap-outline-keywords messages))

(define-method cache-folder-contents ((folder <imap-folder>) walk-mime-body)
  (fill-imap-message-cache folder imap-content-keywords)
  (let ((length (folder-length folder)))
    ((imail-ui:message-wrapper "Caching folder contents")
     (lambda ()
       (for-each-message folder
	 (lambda (index message)
	   (if (zero? (remainder index 10))
	       (imail-ui:progress-meter index length))
	   (cond ((mime-entity-body-structure message)
		  => (lambda (body-structure)
		       (walk-mime-body message body-structure
			 (lambda (body-part)
			   (fetch-message-body-part-to-cache
			    message
			    (imap-mime-body-section body-part))))))
		 (else
		  (fetch-message-body-part-to-cache message '(TEXT))))))))))

(define (for-each-message folder procedure)
  (let ((n (folder-length folder)))
    (do ((i 0 (+ i 1)))
	((= i n))
      (procedure i (%get-message folder i)))))

(define (fill-imap-message-cache folder keywords #!optional messages)
  (receive (message-sets total-count)
      (scan-imap-message-cache folder keywords messages)
    (if (positive? total-count)
	(let ((connection (guarantee-imap-folder-open folder))
	      (count 0))
	  ((imail-ui:message-wrapper "Reading message data")
	   (lambda ()
	     (hash-table/for-each message-sets
	       (lambda (keywords messages)
		 (imap:command:fetch-set/for-each
		  (lambda (response)
		    (if (zero? (remainder count 10))
			(imail-ui:progress-meter count total-count))
		    (set! count (+ count 1))
		    (cache-preload-response folder keywords response))
		  connection
		  (message-list->set (reverse! messages))
		  keywords)))))))))

(define (message-list->set messages)
  (let loop ((indexes (map %message-index messages)) (groups '()))
    (if (pair? indexes)
	(let ((start (car indexes)))
	  (let parse-group ((this start) (rest (cdr indexes)))
	    (if (and (pair? rest) (= (car rest) (+ this 1)))
		(parse-group (car rest) (cdr rest))
		(loop rest
		      (cons (if (= start this)
				(number->string (+ start 1))
				(string-append (number->string (+ start 1))
					       ":"
					       (number->string (+ this 1))))
			    groups)))))
	(decorated-string-append "" "," "" (reverse! groups)))))

(define (scan-imap-message-cache folder keywords #!optional messages)
  (let ((message-sets (make-equal-hash-table))
	(n (folder-length folder))
	(count 0))
    (with-folder-locked folder
      (lambda ()
	((imail-ui:message-wrapper "Scanning message cache")
	 (lambda ()
	   ((lambda (procedure)
	      (if (default-object? messages)
		  (for-each-message folder procedure)
		  (for-each procedure (iota (length messages)) messages)))
	    (lambda (index message)
	      (if (zero? (remainder index 10))
		  (imail-ui:progress-meter index n))
	      (let ((keywords (select-uncached-keywords message keywords)))
		(if (pair? keywords)
		    (begin
		      (hash-table/modify! message-sets keywords
			(lambda (messages) (cons message messages))
			'())
		      (set! count (+ count 1)))))))))))
    (values message-sets count)))

(define (imap-message-keyword-cached? message keyword)
  (let ((cached-keywords (imap-message-cached-keywords message)))
    (or (memq keyword cached-keywords)
	(memq keyword imap-dynamic-keywords)
	(and (file-exists? (message-item-pathname message keyword))
	     (begin
	       (set-imap-message-cached-keywords!
		message
		(cons keyword cached-keywords))
	       #t)))))

(define (select-uncached-keywords message keywords)
  (delete-matching-items keywords
    (lambda (keyword)
      (imap-message-keyword-cached? message keyword))))

;;;; MIME support

(define-class <imap-mime-body> ()
  (message define accessor)
  (section define accessor)
  (header-fields))

(let ((accessor (slot-accessor <imap-mime-body> 'HEADER-FIELDS))
      (modifier (slot-modifier <imap-mime-body> 'HEADER-FIELDS))
      (initpred (slot-initpred <imap-mime-body> 'HEADER-FIELDS)))
  (define (fetch body store)
    (let ((value
	   (lines->header-fields
	    (string->lines
	     (fetch-message-body-part
	      (imap-mime-body-message body)
	      (imap-mime-body-section/mime-header body))))))
      (store value)
      value))
  (define-method mime-body-header-fields ((body <imap-mime-body>))
    (if (initpred body)
	(let* ((pair (accessor body))
	       (header-fields (weak-car pair)))
	  (if (weak-pair/car? pair)
	      header-fields
	      (fetch body
		     (lambda (header-fields)
		       (weak-set-car! pair header-fields)))))
	(fetch body
	       (lambda (header-fields)
		 (modifier body (weak-cons header-fields '())))))))

(define-class (<imap-mime-body-basic>
	       (constructor (message
			     section
			     type subtype parameters id description encoding
			     n-octets
			     md5 disposition language)))
    (<mime-body-basic> <imap-mime-body>))

(define-class (<imap-mime-body-text>
	       (constructor (message
			     section
			     subtype parameters id description encoding
			     n-octets n-lines md5 disposition language)))
    (<mime-body-text> <imap-mime-body>))

(define-class (<imap-mime-body-message>
	       (constructor (message
			     section
			     parameters id description encoding n-octets
			     envelope body n-lines md5 disposition language)))
    (<mime-body-message> <imap-mime-body>))

(define-class (<imap-mime-body-multipart>
	       (constructor (message
			     section
			     subtype parameters parts disposition language)))
    (<mime-body-multipart> <imap-mime-body>))

(define-method write-message-body ((message <imap-message>) port)
  (write-imap-message-section message '(TEXT) (message-length message) port))

(define-method write-mime-body ((body <imap-mime-body>) port)
  (write-imap-message-section
   (imap-mime-body-message body)
   (imap-mime-body-section body)
   ;++ Kludge.  The IMAP includes the length in octets only for
   ;++ one-part bodies.
   (and (mime-body-one-part? body)
	(mime-body-one-part-n-octets body))
   port))

(define-method mime-body-message-header-fields ((body <mime-body-message>))
  (lines->header-fields
   (string->lines
    (call-with-output-string
      (lambda (port)
	(write-imap-message-section
	 (imap-mime-body-message body)
	 (imap-mime-body-section/message-header body)
	 #f
	 port))))))

(define (imap-mime-body-section/mime-header body)
  (let ((section (imap-mime-body-section body)))
    (if (pair? section)
	`(,@section MIME)
	'(HEADER))))

(define (imap-mime-body-section/message-header body)
  (let ((section (imap-mime-body-section body)))
    `(,@section HEADER)))

(define (write-imap-message-section message section length port)
  (cond ((search-imap-message-body-parts message section)
	 => (lambda (entry)
	      (write-string (cdr entry) port)))
	((and length
	      (let ((limit (imail-ui:body-cache-limit message)))
		(and limit
		     (if (and (exact-nonnegative-integer? length)
			      (exact-nonnegative-integer? limit))
			 (< length limit)
			 #t))))
	 (let ((part (fetch-message-body-part message section)))
	   (cache-imap-message-body-part message section part)
	   (write-string part port)))
	(else
	 (fetch-message-body-part-to-port message section port))))

(define (search-imap-message-body-parts message section)
  (define (scan-positive body-parts previous)
    (and (weak-pair? body-parts)
	 (let ((entry (weak-car body-parts)))
	   (if entry
	       (if (equal? section (car entry))
		   entry
		   (scan-positive (weak-cdr body-parts) body-parts))
	       (scan-negative (weak-cdr body-parts) previous)))))
  (define (scan-negative body-parts previous)
    (if (weak-pair? body-parts)
	(let ((entry (weak-car body-parts)))
	  (if entry
	      (begin
		(weak-set-cdr! previous body-parts)
		(if (equal? section (car entry))
		    entry
		    (scan-positive (weak-cdr body-parts) body-parts)))
	      (scan-negative (weak-cdr body-parts) previous)))
	(begin
	  (weak-set-cdr! previous '())
	  #f)))
  (let ((initial (imap-message-body-parts message)))
    (scan-positive (weak-cdr initial) initial)))

(define (cache-imap-message-body-part message section part)
  (let ((pair (imap-message-body-parts message)))
    (weak-set-cdr! pair (weak-cons (cons section part) (weak-cdr pair)))))

(define (parse-mime-body body message section)
  (cond ((not (and (pair? body) (list? body)))
	 (parse-mime-body:lose body message section))
	((string? (car body))
	 ;; The IMAP's indexing scheme treats every message body as
	 ;; multipart, so a `one-part' body is treated as part 1 of a
	 ;; multipart body.
	 (parse-mime-body:one-part body message `(,@section 1)))
	((pair? (car body))
	 (parse-mime-body:multi-part body message section))
	(else
	 (parse-mime-body:lose body message section))))

(define (parse-mime-body-part body message section index)
  (let ((section `(,@section ,index)))
    (cond ((not (and (pair? body) (list? body)))
	   (parse-mime-body:lose body message section))
	  ((string? (car body))
	   (parse-mime-body:one-part body message section))
	  ((pair? (car body))
	   (parse-mime-body:multi-part body message section))
	  (else
	   (parse-mime-body:lose body message section)))))

(define (parse-mime-body:multi-part body message section)
  (let loop ((tail body) (index 0))
    (if (not (pair? tail))
	(parse-mime-body:lose body))
    (if (string? (car tail))
	(let ((enclosed
	       (map
		(lambda (body index)
		  (parse-mime-body-part body message section index))
		(sublist body 0 index)
		(iota index 1)))
	      (extensions
	       (parse-mime-body:extensions (cdr tail))))
	  (let ((enclosure
		 (make-imap-mime-body-multipart message
						section
						(intern (car tail))
						(parse-mime-parameters
						 (car extensions))
						enclosed
						(cadr extensions)
						(caddr extensions))))
	    (for-each (lambda (enclosed)
			(set-mime-body-enclosure! enclosed enclosure))
		      enclosed)
	    enclosure))
	(loop (cdr tail) (fix:+ index 1)))))

(define (parse-mime-body:one-part body message section)
  (let ((n (length body)))
    (cond ((string-ci=? "text" (car body))
	   (if (not (fix:>= n 8))
	       (parse-mime-body:lose body message section))
	   (apply make-imap-mime-body-text message section
		  (intern (list-ref body 1))
		  (parse-mime-parameters (list-ref body 2))
		  (list-ref body 3)
		  (list-ref body 4)
		  (intern (list-ref body 5))
		  (list-ref body 6)
		  (list-ref body 7)
		  (parse-mime-body:extensions (list-tail body 8))))
	  ((and (string-ci=? "message" (car body))
		(string-ci=? "rfc822" (cadr body)))
	   (if (not (fix:>= n 10))
	       (parse-mime-body:lose body message section))
	   (let* ((enclosed
		   (parse-mime-body (list-ref body 8) message section))
		  (enclosure
		   (apply make-imap-mime-body-message message section
			  (parse-mime-parameters (list-ref body 2))
			  (list-ref body 3)
			  (list-ref body 4)
			  (intern (list-ref body 5))
			  (list-ref body 6)
			  (parse-mime-envelope (list-ref body 7))
			  enclosed
			  (list-ref body 9)
			  (parse-mime-body:extensions (list-tail body 10)))))
	     (set-mime-body-enclosure! enclosed enclosure)
	     enclosure))
	  (else
	   (if (not (fix:>= n 7))
	       (parse-mime-body:lose body message section))
	   (apply make-imap-mime-body-basic message section
		  (intern (list-ref body 0))
		  (intern (list-ref body 1))
		  (parse-mime-parameters (list-ref body 2))
		  (list-ref body 3)
		  (list-ref body 4)
		  (intern (list-ref body 5))
		  (list-ref body 6)
		  (parse-mime-body:extensions (list-tail body 7)))))))

(define (parse-mime-body:extensions tail)
  (if (pair? tail)
      (if (pair? (cdr tail))
	  (let ((disposition (parse-mime-disposition (cadr tail))))
	    (if (pair? (cddr tail))
		(list (car tail) disposition (caddr tail))
		(list (car tail) disposition #f)))
	  (list (car tail) #f #f))
      (list #f #f #f)))

(define (parse-mime-body:lose body message section)
  (error "Unrecognized MIME bodystructure:" body message section))

(define (parse-mime-parameters parameters)
  (if parameters
      (let ((lose
	     (lambda () (error "Malformed MIME parameters:" parameters))))
	(let loop ((parameters parameters) (alist '()))
	  (if (pair? parameters)
	      (if (pair? (cdr parameters))
		  (loop (cddr parameters)
			(cons (cons (intern (car parameters))
				    (cadr parameters))
			      alist))
		  (lose))
	      (if (null? parameters)
		  (reverse! alist)
		  (lose)))))
      '()))

(define (parse-mime-disposition disposition)
  (and disposition
       (begin
	 (if (not (and (pair? disposition)
		       (string? (car disposition))
		       (pair? (cdr disposition))
		       (null? (cddr disposition))))
	     (error "Malformed MIME disposition:" disposition))
	 (cons (intern (car disposition))
	       (parse-mime-parameters (cadr disposition))))))

(define (parse-mime-envelope envelope)
  (make-mime-envelope (list-ref envelope 0)
		      (list-ref envelope 1)
		      (parse-mime-addr-list (list-ref envelope 2))
		      (parse-mime-addr-list (list-ref envelope 3))
		      (parse-mime-addr-list (list-ref envelope 4))
		      (parse-mime-addr-list (list-ref envelope 5))
		      (parse-mime-addr-list (list-ref envelope 6))
		      (parse-mime-addr-list (list-ref envelope 7))
		      (list-ref envelope 8)
		      (list-ref envelope 9)))

(define (parse-mime-addr-list addr-list)
  (if addr-list
      (let ((lose
	     (lambda () (error "Malformed MIME address list:" addr-list))))
	(define (loop addr-list open-groups result)
	  (cond ((pair? addr-list)
		 (let ((a (car addr-list)))
		   (cond ((not (and (list? a) (fix:= 4 (length a))))
			  (lose))
			 ((and (or (not (car a)) (string? (car a)))
			       (or (not (cadr a)) (string? (cadr a)))
			       (string? (caddr a))
			       (string? (cadddr a)))
			  (loop (cdr addr-list)
				open-groups
				(cons (make-mime-address (car a)
							 (cadr a)
							 (caddr a)
							 (cadddr a))
				      result)))
			 ((and (not (car a))
			       (not (cadr a))
			       (string? (caddr a))
			       (not (cadddr a)))
			  (loop (cdr addr-list)
				(cons (cons (caddr a) result)
				      open-groups)
				'()))
			 ((and (not (car a))
			       (not (cadr a))
			       (not (caddr a))
			       (not (cadddr a))
			       (pair? open-groups))
			  (loop (cdr addr-list)
				(cdr open-groups)
				(cons (cons (caar open-groups)
					    (reverse! result))
				      (cdar open-groups))))
			 (else (lose)))))
		((and (null? addr-list) (null? open-groups)) (reverse! result))
		(else (lose))))
	(loop addr-list '() '()))
      '()))

;;;; IMAP disk cache

;; The disk cache has following structure:
;;
;; There is a root directory for the cache.  Under this directory,
;; there is one subdirectory for each server.  The server directory
;; name is a variant of the server information from the URL
;;
;; Under each server directory, there is one subdirectory for each
;; folder on that server.  The folder directory name is formed by
;; taking the folder's mailbox name and mapping the characters into a
;; safe subset.  The safe subset preserves all alphanumeric
;; characters, hypens, and underscores, converts "/" to ".", and
;; converts everything else to "=XX" form.
;;
;; Under each folder directory, there is a file called "uidvalidity"
;; that contains the UIDVALIDITY number, as a text string.  For each
;; message in the folder, there is a subdirectory whose name is the
;; UID of the message.  There is also a temporary directory called
;; "temporary" where files are written before being moved into the
;; other directories, and which has no important internal structure.
;; Files older than thirty-six hours are deleted from it occasionally.
;;
;; Under each message directory, there is a file called
;; "rfc822.header" that contains the header information.  There may
;; also be files called "envelope", "bodystructure", "rfc822.size",
;; "internaldate", "text", and "body[...]", all corresponding to the
;; IMAP FETCH keys.

(define (clean-cache-directory folder)
  (let ((temporary-directory (imap-folder-temporary-directory-pathname folder))
	(directory (imap-folder-cache-pathname folder))
	(uidvalidity (imap-folder-uidvalidity folder)))
    (clean-temporary-directory temporary-directory)
    (if uidvalidity
	(with-folder-locked folder
	  (lambda ()
	    (let ((up (merge-pathnames "uidvalidity" directory)))
	      (define (write-uidvalidity)
		(guarantee-init-file-directory temporary-directory)
		(simple-write-file uidvalidity up temporary-directory))
	      (if (file-directory? directory)
		  (let ((uidvalidity*
			 (ignore-errors (lambda () (simple-read-file up)))))
		    (if (and (file-regular? up)
			     (eqv? uidvalidity* uidvalidity))
			(remove-expunged-messages folder directory)
			(begin
			  (delete-directory-contents directory)
			  (write-uidvalidity))))
		  (begin
		    (delete-file-no-errors directory)
		    (guarantee-init-file-directory directory)
		    (write-uidvalidity)))))))))

(define temporary-file-expiration-time
  (* 60 60 36))

(define (clean-temporary-directory directory)
  (if (file-directory? directory)
      (for-each
       (let* ((now (get-universal-time))
	      (then (- now temporary-file-expiration-time)))
	 (lambda (pathname)
	   (catch-file-errors (lambda (condition) condition #f)
	     (lambda ()
	       (let ((ns (file-namestring pathname)))
		 (if (not (or (string=? ns ".")
			      (string=? ns "..")
			      (let ((t (file-modification-time pathname)))
				(and t (> t then)))))
		     (delete-file pathname)))))))
       (directory-read directory #f))))

(define (remove-expunged-messages folder directory)
  (if (imap-folder-messages-synchronized? folder 'UID)
      (for-each (lambda (pathname)
		  (let ((ns (file-namestring pathname)))
		    (if (not (or (string=? ns ".")
				 (string=? ns "..")
				 (string=? ns "uidvalidity")
				 (string=? ns "temporary")
				 (let ((uid (string->number ns 10)))
				   (and uid
					(get-imap-message-by-uid folder uid)
					(file-directory? pathname)))))
			(delete-file-recursively pathname))))
		(directory-read directory #f))))

(define (get-imap-message-by-uid folder uid)
  (let loop ((low 0) (high (folder-length folder)))
    (if (fix:< low high)
	(let ((index (fix:quotient (fix:+ low high) 2)))
	  (let ((message (%get-message folder index)))
	    (let ((uid* (imap-message-uid message)))
	      (cond ((< uid uid*) (loop low index))
		    ((> uid uid*) (loop (fix:+ index 1) high))
		    (else message)))))
	#f)))

(define (fetch-message-items message keywords suffix)
  (if (lset= eq? keywords imap-dynamic-keywords)
      (fetch-message-items-1 message keywords suffix)
      (with-folder-locked (message-folder message)
	(lambda ()
	  (let ((alist
		 (map (lambda (keyword)
			(cons keyword
			      (if (memq keyword imap-dynamic-keywords)
				  '()
				  (let ((pathname
					 (message-item-pathname message
								keyword)))
				    (if (file-exists? pathname)
					(list
					 (read-cached-message-item message
								   keyword
								   pathname))
					'())))))
		      keywords)))
	    (let ((uncached
		   (list-transform-positive alist
		     (lambda (entry)
		       (null? (cdr entry))))))
	      (if (pair? uncached)
		  (let ((response
			 (fetch-message-items-1 message
						(map car uncached)
						suffix)))
		    (cache-fetch-response message response
		      (lambda (keyword)
			(assq keyword alist))
		      (lambda (keyword item)
			(set-cdr! (assq keyword alist) (list item)))))))
	    `(FETCH ,(+ (%message-index message) 1) ,@alist)))
	(lambda ()
	  (fetch-message-items-1 message keywords suffix)))))

(define (cache-fetch-response message response keyword-predicate save-item)
  (for-each (lambda (keyword)
	      (if (and (not (memq keyword imap-dynamic-keywords))
		       (keyword-predicate keyword))
		  (let ((item (imap:response:fetch-attribute response keyword))
			(pathname (message-item-pathname message keyword))
			(temporary-directory
			 (imap-message-temporary-directory-pathname message)))
		    (guarantee-init-file-directory pathname)
		    (guarantee-init-file-directory temporary-directory)
		    (if (memq keyword message-items-cached-as-string)
			(string->file item pathname temporary-directory)
			(simple-write-file item pathname temporary-directory))
		    (let ((keywords (imap-message-cached-keywords message)))
		      (if (not (memq keyword keywords))
			  (set-imap-message-cached-keywords!
			   message
			   (cons keyword keywords))))
		    (save-item keyword item))))
	    (imap:response:fetch-attribute-keywords response)))

(define message-items-cached-as-string
  '(RFC822.HEADER))

(define (fetch-message-items-1 message keywords suffix)
  ((imail-ui:message-wrapper "Reading" suffix)
   (lambda ()
     (imap:read-literal-progress-hook imail-ui:progress-meter
       (lambda ()
	 (with-imap-message-open message
	   (lambda (connection)
	     (imap:command:uid-fetch connection
				     (imap-message-uid message)
				     keywords))))))))

(define (fetch-message-body-part-to-cache message section)
  (let ((cache-keyword (imap-body-section->keyword section))
	(imap-keyword (imap-body-section->keyword/peek section)))
    (with-folder-locked (message-folder message)
      (lambda ()
	(let ((pathname (message-item-pathname message cache-keyword)))
	  (if (not (file-exists? pathname))
	      (let ((temporary-directory
		     (imap-message-temporary-directory-pathname message)))
		(guarantee-init-file-directory pathname)
		(guarantee-init-file-directory temporary-directory)
		(call-with-temporary-output-file pathname temporary-directory
		  (lambda (output-port)
		    (imap:bind-fetch-body-part-port output-port
		      (lambda ()
			(fetch-message-body-part-1 message
						   section
						   imap-keyword))))))))))))

(define (fetch-message-body-part-to-port message section port)
  (let ((keyword (imap-body-section->keyword section)))
    (let ((fetch-to-port
	   (lambda (port)
	     (imap:bind-fetch-body-part-port port
	       (lambda ()
		 (fetch-message-body-part-1 message section keyword))))))
      (with-folder-locked (message-folder message)
	(lambda ()
	  (let ((pathname (message-item-pathname message keyword)))
	    (if (not (file-exists? pathname))
		(let ((temporary-directory
		       (imap-message-temporary-directory-pathname message)))
		  (guarantee-init-file-directory pathname)
		  (guarantee-init-file-directory temporary-directory)
		  (call-with-temporary-output-file pathname temporary-directory
		    fetch-to-port)))
	    (file->port pathname port)))
	(lambda ()
	  (fetch-to-port port))))))

(define (fetch-message-body-part message section)
  (let ((keyword (imap-body-section->keyword section)))
    (with-folder-locked (message-folder message)
      (lambda ()
	(let ((pathname (message-item-pathname message keyword)))
	  (if (file-exists? pathname)
	      (file->string pathname)
	      (let ((part (fetch-message-body-part-1 message section keyword))
		    (temporary-directory
		     (imap-message-temporary-directory-pathname message)))
		(guarantee-init-file-directory pathname)
		(guarantee-init-file-directory temporary-directory)
		(string->file part pathname temporary-directory)
		part))))
      (lambda ()
	(fetch-message-body-part-1 message section keyword)))))

(define (fetch-message-body-part-1 message section keyword)
  (imap:response:fetch-body-part
   (let ((suffix 
	  (string-append " body"
			 (if (equal? section '(TEXT)) "" " part")
			 " for message "
			 (number->string (+ (%message-index message) 1)))))
     ((imail-ui:message-wrapper "Reading" suffix)
      (lambda ()
	(imap:read-literal-progress-hook imail-ui:progress-meter
	  (lambda ()
	    (with-imap-message-open message
	      (lambda (connection)
		(imap:command:uid-fetch connection
					(imap-message-uid message)
					`(',keyword)))))))))
   section
   #f))

(define (imap-body-section->keyword section)
  (%imap-body-section->keyword section "body"))

(define (imap-body-section->keyword/peek section)
  (%imap-body-section->keyword section "body.peek"))

(define (%imap-body-section->keyword section prefix)
  (string-append prefix
		 "["
		 (decorated-string-append
		  "" "." ""
		  (map (lambda (x)
			 (if (exact-nonnegative-integer? x)
			     (number->string x)
			     (symbol-name x)))
		       section))
		 "]"))

(define (cache-preload-response folder keywords response)
  (with-folder-locked folder
    (lambda ()
      (let ((message
	     (%get-message folder
			   (- (imap:response:fetch-index response)
			      1))))
	(cache-fetch-response message response
	  (lambda (keyword) (memq keyword keywords))
	  (lambda (keyword item) keyword item unspecific))))))

(define (delete-cached-message message)
  (with-folder-locked (message-folder message)
    (lambda ()
      (delete-file-recursively (imap-message-cache-pathname message)))))

(define (with-folder-locked folder if-locked #!optional if-not-locked)
  (let ((if-not-locked (if (default-object? if-not-locked) #f if-not-locked))
	(pathname (imap-folder-lock-pathname folder))
	(locked? #f))
    (guarantee-init-file-directory pathname)
    (dynamic-wind
     (lambda () unspecific)
     (lambda ()
       (let loop ((i 0))
	 (without-interrupts
	  (lambda ()
	    (set! locked? (allocate-temporary-file pathname))
	    unspecific))
	 (cond (locked?
		(if (> i 0)
		    (imail-ui:clear-message))
		(remove-property! folder 'IMAP-CACHE-LOCK-FAILURE)
		(if-locked))
	       ((get-property folder 'IMAP-CACHE-LOCK-FAILURE #f)
		(if if-not-locked (if-not-locked)))
	       ((= i 2)
		(imail-ui:clear-message)
		(store-property! folder 'IMAP-CACHE-LOCK-FAILURE #t)
		(if if-not-locked (if-not-locked)))
	       (else
		(imail-ui:message "Waiting for folder lock..." i)
		(imail-ui:sit-for 1000)
		(loop (+ i 1))))))
     (lambda ()
       (if locked?
	   (deallocate-temporary-file pathname))))))

(define (clear-lock-state-on-folder-close folder)
  (remove-property! folder 'IMAP-CACHE-LOCK-FAILURE))

(define (message-item-pathname message keyword)
  (init-file-specifier->pathname
   `(,@(imap-message-cache-specifier message)
     ,(encode-cache-namestring
       (if (symbol? keyword)
	   (symbol-name keyword)
	   keyword)))))

(define (imap-message-cache-pathname message)
  (pathname-as-directory
   (init-file-specifier->pathname (imap-message-cache-specifier message))))

(define (imap-message-cache-specifier message)
  `(,@(imap-folder-cache-specifier (message-folder message))
    ,(write-to-string (imap-message-uid message))))

(define (imap-message-temporary-directory-pathname message)
  (imap-folder-temporary-directory-pathname (message-folder message)))

(define (imap-folder-temporary-directory-pathname folder)
  (merge-pathnames (pathname-as-directory "temporary")
		   (imap-folder-cache-pathname folder)))

(define (imap-folder-lock-pathname folder)
  (let ((spec (imap-folder-cache-specifier folder)))
    (let ((p (last-pair spec)))
      (set-car! p (string-append (car p) "!lock")))
    (init-file-specifier->pathname spec)))

(define (imap-folder-cache-pathname folder)
  (pathname-as-directory
   (init-file-specifier->pathname (imap-folder-cache-specifier folder))))

(define (imap-folder-cache-specifier folder)
  (let ((url (resource-locator folder)))
    (list "imail-cache"
	  (string-append (encode-cache-namestring (imap-url-user-id url))
			 "_"
			 (string-downcase (imap-url-host url))
			 "_"
			 (number->string (imap-url-port url)))
	  (encode-cache-namestring (imap-url-mailbox url)))))

(define (encode-cache-namestring string)
  (call-with-output-string
   (lambda (port)
     (let ((n (string-length string)))
       (do ((i 0 (fix:+ i 1)))
	   ((fix:= i n))
	 (let ((char (string-ref string i)))
	   (cond ((char-set-member? char-set:cache-namestring-safe char)
		  (write-char char port))
		 ((char=? char #\/)
		  (write-char #\# port))
		 (else
		  (write-char #\% port)
		  (let ((n (char->integer char)))
		    (if (fix:< n #x10)
			(write-char #\0 port))
		    (write-string (number->string n 16) port))))))))))

(define char-set:cache-namestring-safe
  (char-set-union char-set:alphanumeric (string->char-set "-_.")))

(define (read-cached-message-item message keyword pathname)
  (let ((item
	 (if (memq keyword message-items-cached-as-string)
	     (file->string pathname)
	     (simple-read-file pathname))))
    (process-fetch-attribute message keyword item)
    item))

(define (simple-read-file pathname)
  (call-with-input-file pathname read))

(define (simple-write-file object pathname #!optional temporary-directory)
  (call-with-temporary-output-file pathname temporary-directory
    (lambda (port)
      (write object port)
      (newline port))))

(define (string->file string pathname #!optional temporary-directory)
  (call-with-temporary-output-file pathname temporary-directory
    (lambda (port)
      (write-string string port))))

(define (call-with-temporary-output-file pathname temporary-directory receiver)
  (if (or (not temporary-directory)
	  (default-object? temporary-directory))
      (call-with-output-file temporary-directory receiver)
      (let ((temporary-pathname (temporary-file-pathname temporary-directory))
	    (done? #f))
	(dynamic-wind
	 (lambda ()
	   (if done?
	       (error "Re-entry prohibited into temporary file creation.")))
	 (lambda ()
	   (let ((result (call-with-output-file temporary-pathname receiver)))
	     (rename-file temporary-pathname pathname)
	     result))
	 (lambda ()
	   (set! done? #t)
	   (deallocate-temporary-file temporary-pathname))))))

(define (file->string pathname)
  (call-with-output-string
    (lambda (port)
      (file->port pathname port))))

(define (file->port pathname output-port)
  (call-with-input-file pathname
    (lambda (input-port)
      (let ((buffer (make-string #x1000)))
	(let loop ()
	  (let ((n (read-string! buffer input-port)))
	    (if (fix:> n 0)
		(begin
		  (write-substring buffer 0 n output-port)
		  (loop)))))))))

(define (delete-file-recursively pathname)
  (if (file-directory? pathname)
      (begin
	(delete-directory-contents (pathname-as-directory pathname))
	(delete-directory pathname))
      (delete-file-no-errors pathname)))

(define (delete-directory-contents directory)
  (for-each (lambda (pathname)
	      (if (not (let ((ns (file-namestring pathname)))
			 (or (string=? ns ".")
			     (string=? ns ".."))))
		  (delete-file-recursively pathname)))
	    (directory-read directory #f)))

;;;; Server operations

(define-method %create-resource ((url <imap-url>))
  (let ((resource
	 (with-open-imap-connection url
	   (lambda (connection)
	     (imap:command:create connection (imap-url-server-mailbox url))))))
    (flush-imap-url-list-info url)
    resource))

(define-method %delete-resource ((url <imap-url>))
  (with-open-imap-connection url
    (lambda (connection)
      (imap:command:delete connection (imap-url-server-mailbox url))))
  (flush-imap-url-list-info url))

(define-method %rename-resource ((url <imap-url>) (new-url <imap-url>))
  (if (compatible-imap-urls? url new-url)
      (with-open-imap-connection url
	(lambda (connection)
	  (imap:command:rename connection
			       (imap-url-server-mailbox url)
			       (imap-url-server-mailbox new-url))))
      (error "Unable to perform rename between different IMAP accounts:"
	     url new-url))
  (flush-imap-url-list-info url)
  (flush-imap-url-list-info new-url))

(define-method %append-message ((message <message>) (url <imap-folder-url>))
  (let ((folder (message-folder message))
	(maybe-create
	 (lambda (connection thunk)
	   (if (imap:catch-no-response
		(lambda (response)
		  (let ((code (imap:response:response-text-code response)))
		    (and code
			 (imap:response-code:trycreate? code))))
		(lambda ()
		  (thunk)
		  #f))
	       (begin
		 (imap:command:create connection (imap-url-server-mailbox url))
		 (thunk)
		 #t)))))
    (if (let ((url* (resource-locator folder)))
	  (and (imap-url? url*)
	       (compatible-imap-urls? url url*)))
	(let ((connection (guarantee-imap-folder-open folder)))
	  (maybe-create connection
	    (lambda ()
	      (imap:command:uid-copy connection
				     (imap-message-uid message)
				     (imap-url-server-mailbox url)))))
	(with-open-imap-connection url
	  (lambda (connection)
	    (maybe-create connection
	      (lambda ()
		(imap:command:append connection
				     (imap-url-server-mailbox url)
				     (map imail-flag->imap-flag
					  (message-permanent-flags message))
				     (message-internal-time message)
				     (message->string message)))))))))

(define-method with-open-connection ((url <imap-url>) thunk)
  (with-open-imap-connection url
    (lambda (connection)
      connection
      (thunk))))

;;;; Folder operations

(define-method open-resource ((url <imap-folder-url>))
  (let ((folder (maybe-make-resource url make-imap-folder)))
    (guarantee-imap-folder-open folder)
    folder))

(define-method close-resource ((folder <imap-folder>) no-defer?)
  (close-imap-folder folder no-defer?))

(define (close-imap-folder folder no-defer?)
  (let ((connection
	 (without-interrupts
	  (lambda ()
	    (let ((connection (imap-folder-connection folder)))
	      (if connection
		  (begin
		    (set-imap-folder-connection! folder #f)
		    (set-imap-connection-folder! connection #f)
		    (decrement-connection-reference-count! connection)))
	      connection)))))
    (if connection
	(begin
	  (maybe-close-imap-connection connection 0 no-defer?)
	  (clear-lock-state-on-folder-close folder)
	  (object-modified! folder 'STATUS)))))

(define-method %get-message ((folder <imap-folder>) index)
  (vector-ref (imap-folder-messages folder) index))

(define-method first-unseen-message-index ((folder <imap-folder>))
  (or (imap-folder-unseen folder) 0))

(define-method expunge-deleted-messages ((folder <imap-folder>))
  (imap:command:expunge (guarantee-imap-folder-open folder)))

(define-method %search-folder ((folder <imap-folder>) criteria)
  (map (lambda (index) (- index 1))
       (imap:response:search-indices
	(let ((connection (guarantee-imap-folder-open folder)))
	  (cond ((string? criteria)
		 (imap:command:search connection 'TEXT criteria))
		(else
		 (error:wrong-type-argument criteria
					    "search criteria"
					    'SEARCH-FOLDER)))))))

(define-method folder-sync-status ((folder <imap-folder>))
  ;; Changes are always written through.
  folder
  'SYNCHRONIZED)

(define-method save-resource ((folder <imap-folder>))
  ;; Changes are always written through.
  folder
  #f)

(define-method discard-folder-cache ((folder <imap-folder>))
  (close-resource folder #f)
  (reset-imap-folder! folder))

(define-method probe-folder ((folder <imap-folder>))
  (imap:command:noop (guarantee-imap-folder-open folder)))

(define-method folder-connection-status ((folder <imap-folder>))
  (if (let ((connection (imap-folder-connection folder)))
	(and connection
	     (test-imap-connection-open connection)))
      'ONLINE
      'OFFLINE))

(define-method disconnect-folder ((folder <imap-folder>))
  (close-resource folder #t))

;;;; Container operations

(define-method open-resource ((url <imap-container-url>))
  (let ((container (maybe-make-resource url make-imap-container)))
    (guarantee-imap-connection-open
     (without-interrupts
      (lambda ()
	(or (imap-container-connection container)
	    (let ((connection (get-compatible-imap-connection url)))
	      (set-imap-container-connection! container connection)
	      (increment-connection-reference-count! connection)
	      connection)))))
    (object-modified! container 'STATUS)
    container))

(define-method close-resource ((container <imap-container>) no-defer?)
  (let ((connection
	 (without-interrupts
	  (lambda ()
	    (let ((connection (imap-container-connection container)))
	      (if connection
		  (begin
		    (set-imap-container-connection! container #f)
		    (decrement-connection-reference-count! connection)))
	      connection)))))
    (if connection
	(begin
	  (maybe-close-imap-connection connection 0 no-defer?)
	  (object-modified! container 'STATUS)))))

(define-method save-resource ((container <imap-container>))
  container
  #f)

;;;; IMAP command invocation

(define (imap:command:capability connection)
  (imap:command:no-response connection 'CAPABILITY))

(define (imap:command:namespace connection)
  (imap:command:single-response imap:response:namespace? connection
				'NAMESPACE))

(define (imap:command:login connection user-id pass-phrase)
  ((imail-ui:message-wrapper "Logging in as " user-id)
   (lambda ()
     (imap:command:no-response connection 'LOGIN user-id pass-phrase))))

(define (imap:command:select connection mailbox)
  ((imail-ui:message-wrapper "Selecting mailbox")
   (lambda ()
     (imap:command:no-response connection 'SELECT
			       (imap:encode-mailbox-name mailbox)))))

(define (imap:command:status connection mailbox items)
  (imap:command:single-response imap:response:status? connection 'STATUS
				(imap:encode-mailbox-name mailbox)
				items))

(define (imap:command:fetch connection index items)
  (imap:command:fetch-response connection 'FETCH (list (+ index 1) items)))

(define (imap:command:uid-fetch connection uid items)
  (imap:command:fetch-response connection 'UID (list 'FETCH uid items)))

(define (imap:command:fetch-response connection command arguments)
  (let ((responses (apply imap:command connection command arguments)))
    (if (and (pair? (cdr responses))
	     (for-all? (cdr responses) imap:response:fetch?))
	(if (null? (cddr responses))
	    (cadr responses)
	    ;; Some servers, notably UW IMAP, sometimes return
	    ;; multiple FETCH responses.  This can happen even if only
	    ;; one item is fetched.  Since the caller expects a single
	    ;; response, synthesize one from the available responses.
	    (cons* (caadr responses)
		   (cadadr responses)
		   (append-map cddr (cdr responses))))
	(error "Malformed response from IMAP server:" responses))))

(define (imap:command:fetch-range connection start end items)
  (imap:command:fetch-set connection (imap-range->set start end) items))

(define (imap:command:fetch-range/for-each procedure
					   connection start end items)
  (imap:command:fetch-set/for-each procedure
				   connection
				   (imap-range->set start end)
				   items))

(define (imap-range->set start end)
  (string-append (number->string (+ start 1))
		 ":"
		 (if end (number->string end) "*")))

(define (imap:command:fetch-set connection set items)
  (imap:command:multiple-response imap:response:fetch? connection
				  'FETCH `',set items))

(define (imap:command:fetch-set/for-each procedure connection set items)
  (imap:command:for-each-response procedure connection 'FETCH `',set items))

(define (imap:command:uid-store-flags connection uid flags)
  (imap:command:no-response connection 'UID 'STORE uid 'FLAGS flags))

(define (imap:command:expunge connection)
  ((imail-ui:message-wrapper "Expunging messages")
   (lambda ()
     (imap:command:no-response connection 'EXPUNGE))))

(define (imap:command:noop connection)
  (imap:command:no-response connection 'NOOP))

(define (imap:command:logout connection)
  (imap:command:no-response connection 'LOGOUT))

(define (imap:command:create connection mailbox)
  (imap:command:no-response connection 'CREATE
			    (imap:encode-mailbox-name mailbox)))

(define (imap:command:delete connection mailbox)
  (imap:command:no-response connection 'DELETE
			    (imap:encode-mailbox-name mailbox)))

(define (imap:command:rename connection from to)
  (imap:command:no-response connection 'RENAME
			    (imap:encode-mailbox-name from)
			    (imap:encode-mailbox-name to)))

(define (imap:command:uid-copy connection uid mailbox)
  (imap:command:no-response connection 'UID 'COPY
			    uid (imap:encode-mailbox-name mailbox)))

(define (imap:command:append connection mailbox flags time text)
  (imap:command:no-response connection 'APPEND
			    (imap:encode-mailbox-name mailbox)
			    (and (pair? flags) flags)
			    (and time (imap:universal-time->date-time time))
			    (cons 'LITERAL text)))

(define (imap:command:search connection . key-plist)
  (apply imap:command:single-response imap:response:search? connection
	 'SEARCH key-plist))

(define (imap:command:list connection reference pattern)
  (imap:command:multiple-response imap:response:list? connection 'LIST
				  (imap:encode-mailbox-name reference)
				  (imap:encode-mailbox-name pattern)))

(define (imap:command:get-delimiter connection reference)
  (imap:command:single-response imap:response:list? connection 'LIST
				(imap:encode-mailbox-name reference)
				(imap:encode-mailbox-name "")))

(define (imap:command:no-response connection command . arguments)
  (let ((responses (apply imap:command connection command arguments)))
    (if (not (null? (cdr responses)))
	(error "Malformed response from IMAP server:" responses))
    (car responses)))

(define (imap:command:single-response predicate connection command . arguments)
  (let ((responses (apply imap:command connection command arguments)))
    (if (and (pair? (cdr responses))
	     (predicate (cadr responses))
	     (null? (cddr responses)))
	(cadr responses)
	(error "Malformed response from IMAP server:" responses))))

(define (imap:command:multiple-response predicate
					connection command . arguments)
  (let ((responses (apply imap:command connection command arguments)))
    (if (for-all? (cdr responses) predicate)
	(cdr responses)
	(error "Malformed response from IMAP server:" responses))))

(define (imap:command:for-each-response procedure
					connection command . arguments)
  (apply imap:command*
	 (lambda (response) (procedure response) #f)
	 connection command arguments)
  unspecific)

(define condition-type:imap-server-error
  (make-condition-type 'IMAP-SERVER-ERROR condition-type:error '(RESPONSE)
    (lambda (condition port)
      (let ((response (imap:server-error:response condition)))
	(write-string "Server signalled a command error: " port)
	(write-string (imap:response:response-text-string response) port)
	(let ((code (imap:response:response-text-code response)))
	  (if code
	      (begin
		(write-char #\space port)
		(write code port))))))))

(define imap:server-error
  (condition-signaller condition-type:imap-server-error
		       '(RESPONSE)
		       standard-error-handler))

(define imap:server-error:response
  (condition-accessor condition-type:imap-server-error 'RESPONSE))

(define (imap:command* filter connection command . arguments)
  (bind-condition-handler '()
      (lambda (condition)
	(if (not (eq? (condition/type condition)
		      condition-type:imap-server-error))
	    (begin
	      (close-imap-connection connection)
	      (if (broken-pipe? condition)
		  (error
		   "Connection to IMAP server broken; please try again.")))))
    (lambda ()
      (imap:wait-for-tagged-response
       connection
       (imap:send-command connection command arguments)
       (if (eq? command 'UID) (car arguments) command)
       filter))))

(define (imap:command connection command . arguments)
  (apply imap:command*
	 (lambda (response) response #t)
	 connection command arguments))

(define (start-imap-trace pathname)
  (stop-imap-trace)
  (set! imap-trace-port (open-output-file pathname))
  unspecific)

(define (stop-imap-trace)
  (if imap-trace-port
      (begin
	(close-port imap-trace-port)
	(set! imap-trace-port #f)
	unspecific)))

(define imap-trace-port #f)

(define (imap:send-command connection command arguments)
  (let ((tag (next-imap-command-tag connection))
	(port (imap-connection-port connection)))
    (if imap-trace-port
	(begin
	  (write-line (cons* 'SEND tag command
			     (if (eq? command 'LOGIN)
				 (cons* (car arguments)
					"password"
					(cddr arguments))
				 arguments))
		      imap-trace-port)
	  (flush-output imap-trace-port)))
    (imap-transcript-write-string tag port)
    (imap-transcript-write-char #\space port)
    (imap-transcript-write-string (symbol-name command) port)
    (for-each (lambda (argument)
		(if argument
		    (begin
		      (imap-transcript-write-char #\space port)
		      (imap:send-command-argument connection tag argument))))
	      arguments)
    (imap-transcript-write-char #\return port)
    (imap-transcript-write-char #\linefeed port)
    (imap-transcript-flush-output port)
    tag))

(define (imap:send-command-argument connection tag argument)
  (let ((port (imap-connection-port connection)))
    (let loop ((argument argument))
      (cond ((exact-nonnegative-integer? argument)
	     (imap-transcript-write argument port))
	    ((symbol? argument)
	     (imap-transcript-write-string (symbol-name argument) port))
	    ((and (pair? argument)
		  (eq? (car argument) 'QUOTE)
		  (pair? (cdr argument))
		  (string? (cadr argument))
		  (null? (cddr argument)))
	     (imap-transcript-write-string (cadr argument) port))
	    ((and (pair? argument)
		  (eq? (car argument) 'LITERAL)
		  (string? (cdr argument)))
	     (imap:write-literal-string connection tag (cdr argument)))
	    ((string? argument)
	     (if (imap:string-may-be-quoted? argument)
		 (imap:write-quoted-string argument port)
		 (imap:write-literal-string connection tag argument)))
	    ((list? argument)
	     (imap-transcript-write-char #\( port)
	     (if (pair? argument)
		 (begin
		   (loop (car argument))
		   (for-each (lambda (object)
			       (imap-transcript-write-char #\space port)
			       (loop object))
			     (cdr argument))))
	     (imap-transcript-write-char #\) port))
	    (else (error "Illegal IMAP syntax:" argument))))))

(define (imap:write-literal-string connection tag string)
  (let ((port (imap-connection-port connection)))
    (imap:write-literal-string-header string port)
    (imap-transcript-flush-output port)
    (let loop ()
      (let ((response (imap:read-server-response-1 port)))
	(cond ((imap:response:continue? response)
	       (imap:write-literal-string-body string port))
	      ((and (imap:response:tag response)
		    (string-ci=? tag (imap:response:tag response)))
	       (imap:server-error response))
	      (else
	       (enqueue-imap-response connection response)
	       (loop)))))))

(define (imap:wait-for-tagged-response connection tag command filter)
  (let ((port (imap-connection-port connection)))
    (let loop ()
      (let ((response (imap:read-server-response-1 port)))
	(let ((tag* (imap:response:tag response)))
	  (if tag*
	      (let ((responses (process-queued-responses connection command)))
		(if (string-ci=? tag tag*)
		    (if (imap:response:ok? response)
			(cons response responses)
			(imap:server-error response))
		    (if (< (base26-string->nonnegative-integer tag*)
			   (base26-string->nonnegative-integer tag))
			;; If this is an old tag, ignore it and move on.
			(loop)
			(error "Out-of-sequence tag:" tag* tag))))
	      (begin
		(if (filter response)
		    (enqueue-imap-response connection response))
		(loop))))))))

(define (imap:read-server-response-1 port)
  (let ((response (imap:read-server-response port)))
    (if imap-trace-port
	(begin
	  (write-line (list 'RECEIVE response) imap-trace-port)
	  (flush-output imap-trace-port)))
    response))

(define (imap:catch-no-response predicate thunk)
  (call-with-current-continuation
   (lambda (k)
     (bind-condition-handler
	 (list condition-type:imap-server-error)
	 (lambda (condition)
	   (let ((response (imap:server-error:response condition)))
	     (if (and (imap:response:no? response)
		      (or (not predicate) (predicate response)))
		 (k response))))
       thunk))))

(define (process-queued-responses connection command)
  (with-modification-events-deferred
    (lambda ()
      (let loop ((responses (dequeue-imap-responses connection)))
	(if (pair? responses)
	    (if (process-response connection command (car responses))
		(cons (car responses) (loop (cdr responses)))
		(loop (cdr responses)))
	    '())))))

(define (process-response connection command response)
  (cond ((imap:response:status-response? response)
	 (let ((code (imap:response:response-text-code response))
	       (text (imap:response:response-text-string response)))
	   (if code
	       (process-response-text connection command code text))
	   (if (and (imap:response:bye? response)
		    (not (memq command '(LOGOUT #F))))
	       (begin
		 (close-imap-connection connection)
		 (error "Server shut down connection:" text)))
	   (if (or (imap:response:no? response)
		   (imap:response:bad? response))
	       (imail-ui:present-user-alert
		(lambda (port)
		  (write-string "Notice from IMAP server:" port)
		  (newline port)
		  (display text port)
		  (newline port)))))
	 (imap:response:preauth? response))
	((imap:response:exists? response)
	 (with-imap-connection-folder connection
	   (lambda (folder)
	     (update-imap-folder-length!
	      folder
	      (imap:response:exists-count response))))
	 #f)
	((imap:response:expunge? response)
	 (with-imap-connection-folder connection
	   (lambda (folder)
	     (remove-imap-folder-message
	      folder
	      (- (imap:response:expunge-index response) 1))))
	 #f)
	((imap:response:flags? response)
	 (with-imap-connection-folder connection
	   (lambda (folder)
	     (set-imap-folder-allowed-flags!
	      folder
	      (map imap-flag->imail-flag (imap:response:flags response)))))
	 #f)
	((imap:response:recent? response)
	 #f)
	((imap:response:capability? response)
	 (set-imap-connection-capabilities!
	  connection
	  (imap:response:capabilities response))
	 #f)
	((imap:response:namespace? response)
	 #t)
	((imap:response:list? response)
	 (eq? command 'LIST))
	((imap:response:lsub? response)
	 (eq? command 'LSUB))
	((imap:response:search? response)
	 (eq? command 'SEARCH))
	((imap:response:status? response)
	 (eq? command 'STATUS))
	((imap:response:fetch? response)
	 (with-imap-connection-folder connection
	   (lambda (folder)
	     (process-fetch-attributes
	      (%get-message folder
			    (- (imap:response:fetch-index response) 1))
	      response)))
	 (eq? command 'FETCH))
	(else
	 (error "Illegal server response:" response))))

(define (process-response-text connection command code text)
  command
  (cond ((imap:response-code:alert? code)
	 (imail-ui:present-user-alert
	  (lambda (port)
	    (write-string "Alert from IMAP server:" port)
	    (newline port)
	    (display text port)
	    (newline port))))
	((imap:response-code:permanentflags? code)
	 (with-imap-connection-folder connection
	   (lambda (folder)
	     (let ((pflags (imap:response-code:permanentflags code)))
	       (set-imap-folder-permanent-keywords?!
		folder
		(if (memq '\\* pflags) #t #f))
	       (set-imap-folder-permanent-flags!
		folder
		(map imap-flag->imail-flag (delq '\\* pflags)))))))
	((imap:response-code:read-only? code)
	 (with-imap-connection-folder connection
	   (lambda (folder)
	     (set-imap-folder-read-only?! folder #t))))
	((imap:response-code:read-write? code)
	 (with-imap-connection-folder connection
	   (lambda (folder)
	     (set-imap-folder-read-only?! folder #f))))
	((imap:response-code:uidnext? code)
	 (with-imap-connection-folder connection
	   (lambda (folder)
	     (set-imap-folder-uidnext! folder
				       (imap:response-code:uidnext code)))))
	((imap:response-code:uidvalidity? code)
	 (with-imap-connection-folder connection
	   (lambda (folder)
	     (let ((uidvalidity (imap:response-code:uidvalidity code)))
	       (if (not (eqv? uidvalidity (imap-folder-uidvalidity folder)))
		   (new-imap-folder-uidvalidity! folder uidvalidity))))))
	((imap:response-code:unseen? code)
	 (with-imap-connection-folder connection
	   (lambda (folder)
	     (set-imap-folder-unseen!
	      folder
	      (- (imap:response-code:unseen code) 1)))))
	#|
	((or (imap:response-code:badcharset? code)
	     (imap:response-code:newname? code)
	     (imap:response-code:parse? code)
	     (imap:response-code:trycreate? code))
	 unspecific)
	|#
	))

(define (process-fetch-attributes message response)
  (for-each
   (lambda (keyword)
     (process-fetch-attribute message
			      keyword
			      (imap:response:fetch-attribute response
							     keyword)))
   (imap:response:fetch-attribute-keywords response)))

(define (process-fetch-attribute message keyword datum)
  (case keyword
    ((BODYSTRUCTURE)
     (%set-imap-message-bodystructure! message
				       (parse-mime-body datum message '())))
    ((FLAGS)
     (%set-message-flags! message (map imap-flag->imail-flag datum)))
    ((RFC822.SIZE)
     (%set-imap-message-length! message datum))
    ((UID)
     (%set-imap-message-uid! message datum))))

(define (with-imap-connection-folder connection receiver)
  (let ((folder (imap-connection-folder connection)))
    (if folder
	(receiver folder))))

(define %message-flags-initialized?
  (slot-initpred <imap-message> 'FLAGS))

(define %set-imap-message-uid!
  (slot-modifier <imap-message> 'UID))

(define %set-imap-message-length!
  (slot-modifier <imap-message> 'LENGTH))

(define %set-imap-message-bodystructure!
  (slot-modifier <imap-message> 'BODYSTRUCTURE))