;;; -*-Scheme-*-
;;;
;;; $Id: imail-imap.scm,v 1.150 2001/05/13 03:46:01 cph Exp $
;;;
;;; Copyright (c) 1999-2001 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

;;;; IMAIL mail reader: IMAP back end

(declare (usual-integrations))

;;;; URL

(define-class <imap-url> (<folder-url> <container-url>)
  ;; User name to connect as.
  (user-id define accessor)
  ;; Name or IP address of host to connect to.
  (host define accessor)
  ;; Port number to connect to.
  (port define accessor)
  ;; Name of mailbox to access.
  (mailbox define accessor))

(define-url-protocol "imap" <imap-url>)

(define-method url-exists? ((url <imap-url>))
  (and (imap-url-info url) #t))

(define-method url-is-selectable? ((url <imap-url>))
  (let ((response (imap-url-info url)))
    (and response
	 (not (memq '\NOSELECT (imap:response:list-flags response))))))

(define (imap-url-info url)
  (let ((responses
	 (with-open-imap-connection url
	   (lambda (connection)
	     (imap:command:list connection
				""
				(imap-url-server-mailbox url))))))
    (and (pair? responses)
	 (null? (cdr responses))
	 (car responses))))

(define make-imap-url
  (let ((constructor
	 (instance-constructor <imap-url> '(USER-ID HOST PORT MAILBOX))))
    (lambda (user-id host port mailbox)
      (intern-url
       (constructor user-id
		    (string-downcase host)
		    port
		    (canonicalize-imap-mailbox mailbox))))))

(define (make-imap-url-string url mailbox)
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
		 (if mailbox
		     (string-append
		      "/"
		      (url:encode-string (canonicalize-imap-mailbox mailbox)))
		     "")))

(define (canonicalize-imap-mailbox mailbox)
  (cond ((string-ci=? "inbox" mailbox) "inbox")
	((and (string-prefix-ci? "inbox/" mailbox)
	      (not (string-prefix? "inbox/" mailbox)))
	 (let ((mailbox (string-copy mailbox)))
	   (substring-downcase! mailbox 0 5)
	   mailbox))
	(else mailbox)))

(define-method url-body ((url <imap-url>))
  (make-imap-url-string url (imap-url-mailbox url)))

(define-method url-presentation-name ((url <imap-url>))
  (url-base-name url))

(define (compatible-imap-urls? url1 url2)
  ;; Can URL1 and URL2 both be accessed from the same IMAP session?
  ;; E.g. can the IMAP COPY command work between them?
  (and (string=? (imap-url-user-id url1) (imap-url-user-id url2))
       (string=? (imap-url-host url1) (imap-url-host url2))
       (= (imap-url-port url1) (imap-url-port url2))))

(define-method url-pass-phrase-key ((url <imap-url>))
  (make-url-string (url-protocol url) (make-imap-url-string url #f)))

(define-method url-base-name ((url <imap-url>))
  (let ((mailbox (imap-url-mailbox url)))
    (let ((index (string-search-backward "/" mailbox)))
      (if index
	  (string-tail mailbox index)
	  mailbox))))

(define (imap-url-new-mailbox url mailbox)
  (make-imap-url (imap-url-user-id url)
		 (imap-url-host url)
		 (imap-url-port url)
		 mailbox))

(define-method make-peer-url ((url <imap-url>) base-name)
  (imap-url-new-mailbox
   url
   (string-append (imap-mailbox-container-string url (imap-url-mailbox url))
		  base-name)))

(define-method url-container ((url <imap-url>))
  (imap-url-new-mailbox
   url
   (let ((mailbox (imap-mailbox-container-string url (imap-url-mailbox url))))
     (if (string-suffix? "/" mailbox)
	 (string-head mailbox (fix:- (string-length mailbox) 1))
	 mailbox))))

(define (imap-mailbox-container-string url mailbox)
  (let ((index (string-search-backward "/" mailbox)))
    (if index
	(string-head mailbox index)
	(or (let ((response
		   (let ((connection
			  (search-imap-connections
			   (lambda (connection)
			     (and (compatible-imap-urls?
				   (imap-connection-url connection)
				   url)
				  (not
				   (eq? (imap-connection-delimiter connection)
					'UNKNOWN)))))))
		     (and connection
			  (imap-connection-namespace connection)))))
	      (and response
		   (let ((namespace
			  (imap:response:namespace-personal response)))
		     (and (pair? namespace)
			  (car namespace)
			  (let ((prefix
				 (imap:decode-mailbox-name (caar namespace)))
				(delimiter (cadar namespace)))
			    (cond ((not delimiter)
				   prefix)
				  ((and (fix:= (string-length prefix) 6)
					(string-prefix-ci? "inbox" prefix)
					(string-suffix? delimiter prefix))
				   "inbox/")
				  (else
				   (string-replace prefix
						   (string-ref delimiter 0)
						   #\/))))))))
	    ""))))

(define-method parse-url-body (string (default-url <imap-url>))
  (call-with-values (lambda () (parse-imap-url-body string default-url))
    (lambda (user-id host port mailbox)
      (if user-id
	  (make-imap-url user-id host port mailbox)
	  (error:bad-range-argument string 'PARSE-URL-BODY)))))

(define parse-imap-url-body
  (let ((parser
	 (let ((//server
		(sequence-parser (noise-parser (string-matcher "//"))
				 (imap:server-parser #f)))
	       (/mbox
		(sequence-parser (noise-parser (string-matcher "/"))
				 (optional-parser imap:parse:enc-mailbox))))
	   (alternatives-parser
	    (sequence-parser //server (optional-parser /mbox))
	    /mbox
	    imap:parse:enc-mailbox))))
    (lambda (string default-url)
      (let ((pv (parse-string parser string)))
	(if pv
	    (values (or (parser-token pv 'USER-ID)
			(imap-url-user-id default-url))
		    (or (parser-token pv 'HOST)
			(imap-url-host default-url))
		    (cond ((parser-token pv 'PORT) => string->number)
			  ((parser-token pv 'HOST) 143)
			  (else (imap-url-port default-url)))
		    (or (parser-token pv 'MAILBOX)
			(imap-url-mailbox default-url)))
	    (values #f #f #f #f))))))

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
  (with-open-imap-connection url
    (lambda (connection)
      (let ((get-list
	     (lambda (prefix)
	       (imap:command:list connection "" (string-append prefix "%")))))
	(append-map!
	 (lambda (response)
	   (let ((flags (imap:response:list-flags response))
		 (delimiter (imap:response:list-delimiter response))
		 (mailbox
		  (imap:decode-mailbox-name
		   (imap:response:list-mailbox response))))
	     (let ((mailbox*
		    (if delimiter
			(string-replace mailbox (string-ref delimiter 0) #\/)
			mailbox)))
	       (let ((tail
		      (if (and delimiter
			       (or (memq '\NOSELECT flags)
				   (and (not (memq '\NOINFERIORS flags))
					(pair?
					 (get-list
					  (string-append mailbox
							 delimiter))))))
			  (list (string-append mailbox* "/"))
			  '())))
		 (if (memq '\NOSELECT flags)
		     tail
		     (cons mailbox* tail))))))
	 (get-list (imap-mailbox/url->server url prefix)))))))

;;;; URL/server delimiter conversion

(define (imap-url-server-mailbox url)
  (imap-mailbox/url->server url (imap-url-mailbox url)))

(define (imap-mailbox/url->server url mailbox)
  (let ((delimiter (imap-mailbox-delimiter url mailbox)))
    (if (and delimiter (not (char=? delimiter #\/)))
	(string-replace mailbox #\/ delimiter)
	mailbox)))

(define (imap-mailbox/server->url url mailbox)
  (let ((delimiter (imap-mailbox-delimiter url mailbox)))
    (if (and delimiter (not (char=? delimiter #\/)))
	(string-replace mailbox delimiter #\/)
	mailbox)))

(define (imap-mailbox-delimiter url mailbox)
  (or (let ((entry (find-imap-namespace-entry url mailbox)))
	(and entry
	     (cadr entry)))
      (let ((delimiter (imap-url-delimiter url)))
	(and delimiter
	     (string-ref delimiter 0)))))

(define (find-imap-namespace-entry url mailbox)
  (let ((response (imap-url-namespace url)))
    (and response
	 (let ((try
		(lambda (namespace)
		  (let loop ((entries namespace))
		    (and (pair? entries)
			 (or (let ((prefix
				    (imap:decode-mailbox-name (caar entries)))
				   (delimiter (cadar entries)))
			       (if (and delimiter
					(fix:= (string-length prefix) 6)
					(string-prefix-ci? "inbox" prefix)
					(string-suffix? delimiter prefix))
				   (and (string-prefix-ci? prefix mailbox)
					(list (string-append "inbox" delimiter)
					      (string-ref delimiter 0)))
				   (and (string-prefix? prefix mailbox)
					(list prefix
					      (and delimiter
						   (string-ref delimiter
							       0))))))
			     (loop (cdr entries))))))))
	   (or (try (imap:response:namespace-personal response))
	       (try (imap:response:namespace-shared response))
	       (try (imap:response:namespace-other response)))))))

;;;; Server connection

(define-class <imap-connection> ()
  (url             define accessor)
  (port            define standard initial-value #f)
  (greeting        define standard initial-value #f)
  (capabilities    define standard initial-value '())
  (delimiter       define standard initial-value 'UNKNOWN)
  (namespace       define standard initial-value #f)
  (sequence-number define standard initial-value 0)
  (response-queue  define accessor initializer (lambda () (cons '() '())))
  (folder          define standard initial-value #f)
  (reference-count define standard initial-value 0))

(define-method write-instance ((connection <imap-connection>) port)
  (write-instance-helper 'IMAP-CONNECTION connection port
    (lambda ()
      (write-char #\space port)
      (write (url-body (imap-connection-url connection)) port))))

(define (reset-imap-connection connection)
  (without-interrupts
   (lambda ()
     (set-imap-connection-greeting! connection #f)
     (set-imap-connection-capabilities! connection '())
     (set-imap-connection-sequence-number! connection 0)
     (let ((queue (imap-connection-response-queue connection)))
       (set-car! queue '())
       (set-cdr! queue '()))
     (set-imap-connection-folder! connection #f))))

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

(define (get-imap-connection url)
  (or (or (search-imap-connections
	   (lambda (connection)
	     (and (compatible-imap-urls? (imap-connection-url connection)
					 url)
		  (test-imap-connection-open connection))))
	  (search-imap-connections
	   (lambda (connection)
	     (compatible-imap-urls? (imap-connection-url connection)
				    url))))
      (make-imap-connection url)))

(define (search-imap-connections predicate)
  (let loop ((connections memoized-imap-connections) (prev #f))
    (and (weak-pair? connections)
	 (let ((connection (weak-car connections)))
	   (if connection
	       (if (predicate connection)
		   connection
		   (loop (weak-cdr connections) connections))
	       (let ((next (weak-cdr connections)))
		 (if prev
		     (weak-set-cdr! prev next)
		     (set! memoized-imap-connections next))
		 (loop next prev)))))))

(define make-imap-connection
  (let ((constructor (instance-constructor <imap-connection> '(URL))))
    (lambda (url)
      (let ((connection (constructor url)))
	(without-interrupts
	 (lambda ()
	   (set! memoized-imap-connections
		 (weak-cons connection memoized-imap-connections))))
	connection))))

(define memoized-imap-connections '())

(define (test-imap-connection-open connection)
  (let ((port (imap-connection-port connection)))
    (and port
	 (let* ((process
		 (lambda ()
		   (process-responses connection #f
				      (dequeue-imap-responses connection))))
		(lose
		 (lambda ()
		   (process)
		   (close-imap-connection connection)
		   #f)))
	   (let loop ()
	     (cond ((not (char-ready? port))
		    (process)
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

(define (guarantee-imap-connection-open connection)
  (if (test-imap-connection-open connection)
      #f
      (let ((url (imap-connection-url connection)))
	(let ((port
	       (open-tcp-stream-socket (imap-url-host url)
				       (or (imap-url-port url) "imap2")
				       #f
				       "\n")))
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
	(if (eq? (imap-connection-delimiter connection) 'UNKNOWN)
	    (begin
	      (set-imap-connection-delimiter!
	       connection
	       (imap:response:list-delimiter
		(car (imap:command:list connection "" "inbox"))))
	      (if (memq 'NAMESPACE (imap-connection-capabilities connection))
		  (set-imap-connection-namespace!
		   connection
		   (imap:command:namespace connection)))))
	#t)))

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

(define (with-open-imap-connection url receiver)
  (let ((connection (get-imap-connection url)))
    (dynamic-wind (lambda ()
		    (set-imap-connection-reference-count!
		     connection
		     (+ (imap-connection-reference-count connection) 1)))
		  (lambda ()
		    (guarantee-imap-connection-open connection)
		    (let ((v (receiver connection)))
		      (maybe-close-imap-connection connection)
		      v))
		  (lambda ()
		    (set-imap-connection-reference-count!
		     connection
		     (- (imap-connection-reference-count connection) 1))))))

(define (maybe-close-imap-connection connection)
  (if (= (imap-connection-reference-count connection)
	 (if (imap-connection-folder connection) 0 1))
      (begin
	(if (imap-connection-port connection)
	    (imap:command:logout connection))
	(close-imap-connection connection))))

(define (imap-url-delimiter url)
  (let ((connection (get-imap-connection url)))
    (let ((delimiter (imap-connection-delimiter connection)))
      (if (eq? delimiter 'UNKNOWN)
	  (with-open-imap-connection url imap-connection-delimiter)
	  delimiter))))

(define (imap-url-namespace url)
  (let ((connection (get-imap-connection url)))
    (if (eq? (imap-connection-delimiter connection) 'UNKNOWN)
	(with-open-imap-connection url imap-connection-namespace)
	(imap-connection-namespace connection))))

;;;; Folder datatype

(define-class (<imap-folder> (constructor (url connection))) (<folder>)
  (connection define accessor)
  (read-only? define standard)
  (allowed-flags define standard)
  (permanent-flags define standard)
  (permanent-keywords? define standard)
  (uidnext define standard)
  (uidvalidity define standard)
  (unseen define standard)
  (messages-synchronized? define standard)
  (n-messages define standard initial-value 0)
  (messages define standard initial-value '#()))

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
     (set-imap-folder-messages-synchronized?! folder #f)
     (set-imap-folder-n-messages! folder 0)
     (set-imap-folder-messages! folder (initial-messages)))))

(define (guarantee-imap-folder-open folder)
  (let ((connection (imap-folder-connection folder)))
    (if (or (guarantee-imap-connection-open connection)
	    (not (eq? folder (imap-connection-folder connection))))
	(begin
	  (set-imap-folder-messages-synchronized?! folder #f)
	  (let ((selected? #f))
	    (dynamic-wind
	     (lambda ()
	       (set-imap-connection-folder! connection folder))
	     (lambda ()
	       (imap:command:select
		connection
		(imap-url-server-mailbox (folder-url folder)))
	       (set! selected? #t)
	       unspecific)
	     (lambda ()
	       (if (not selected?)
		   (set-imap-connection-folder! connection #f)))))
	  (folder-modified! folder 'STATUS)
	  #t))))

(define (new-imap-folder-uidvalidity! folder uidvalidity)
  (without-interrupts
   (lambda ()
     (detach-all-messages! folder)
     (fill-messages-vector! folder 0)
     (if (imap-folder-uidvalidity folder)
	 (set-imap-folder-unseen! folder #f))
     (set-imap-folder-uidvalidity! folder uidvalidity)))
  (read-message-headers! folder 0))

(define (detach-all-messages! folder)
  (let ((v (imap-folder-messages folder))
	(n (imap-folder-n-messages folder)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i n))
      (detach-message! (vector-ref v i)))))

(define (fill-messages-vector! folder start)
  (let ((v (imap-folder-messages folder))
	(n (imap-folder-n-messages folder)))
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
  (without-interrupts
   (lambda ()
     (let ((v (imap-folder-messages folder))
	   (n (fix:- (imap-folder-n-messages folder) 1)))
       (detach-message! (vector-ref v index))
       (do ((i index (fix:+ i 1)))
	   ((fix:= i n))
	 (let ((m (vector-ref v (fix:+ i 1))))
	   (set-message-index! m i)
	   (vector-set! v i m)))
       (vector-set! v n #f)
       (set-imap-folder-n-messages! folder n)
       (set-imap-folder-unseen! folder #f)
       (let ((new-length (compute-messages-length v n)))
	 (if new-length
	     (set-imap-folder-messages! folder
					(vector-head v new-length))))
       (folder-modified! folder 'EXPUNGE index)))))

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

;;; SET-IMAP-FOLDER-LENGTH! needs explanation.  There are two basic
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
;;; message pointers where possible.

;;; The reason for this complexity in the second case is that we can't
;;; be guaranteed that we will complete reading the UIDs for the new
;;; messages, either due to error or the user aborting the read.  So
;;; we must have everything in a consistent (if nonoptimal) state
;;; while reading.  If the read finishes, we can do the match/replace
;;; operation atomically.

(define (set-imap-folder-length! folder count)
  (with-interrupt-mask interrupt-mask/gc-ok
    (lambda (interrupt-mask)
      (if (or (imap-folder-messages-synchronized? folder)
	      (= 0 (imap-folder-n-messages folder)))
	  (let ((v (imap-folder-messages folder))
		(n (imap-folder-n-messages folder)))
	    (cond ((> count n)
		   (let ((new-length (compute-messages-length v count)))
		     (if new-length
			 (set-imap-folder-messages!
			  folder
			  (vector-grow v new-length #f))))
		   (set-imap-folder-n-messages! folder count)
		   (fill-messages-vector! folder n)
		   (set-imap-folder-messages-synchronized?! folder #t)
		   (with-interrupt-mask interrupt-mask
		     (lambda (interrupt-mask)
		       interrupt-mask
		       (read-message-headers! folder n)))
		   (folder-modified! folder 'INCREASE-LENGTH n count))
		  ((= count n)
		   (set-imap-folder-messages-synchronized?! folder #t))
		  (else
		   (error "EXISTS response decreased folder length:"
			  folder))))
	  (begin
	    (detach-all-messages! folder)
	    (let ((v (imap-folder-messages folder))
		  (n (imap-folder-n-messages folder)))
	      (set-imap-folder-n-messages! folder count)
	      (set-imap-folder-messages!
	       folder
	       (make-vector (or (compute-messages-length v count)
				(vector-length v))
			    #f))
	      (fill-messages-vector! folder 0)
	      (set-imap-folder-messages-synchronized?! folder #t)
	      (if (> count 0)
		  (with-interrupt-mask interrupt-mask
		    (lambda (interrupt-mask)
		      interrupt-mask
		      ((imail-ui:message-wrapper "Reading message UIDs")
		       (lambda ()
			 (imap:command:fetch-range
			  (imap-folder-connection folder)
			  0 #f '(UID)))))))
	      (let ((v* (imap-folder-messages folder))
		    (n* (imap-folder-n-messages folder)))
		(let loop ((i 0) (i* 0))
		  (if (and (fix:< i n) (fix:< i* n*))
		      (let ((m (vector-ref v i))
			    (m* (vector-ref v* i*)))
			(if (= (imap-message-uid m) (imap-message-uid m*))
			    (begin
			      ;; Flags might have been updated while
			      ;; reading the UIDs.
			      (if (%message-flags-initialized? m*)
				  (%set-message-flags! m (message-flags m*)))
			      (detach-message! m*)
			      (attach-message! m folder i*)
			      (vector-set! v* i* m)
			      (loop (fix:+ i 1) (fix:+ i* 1)))
			    (begin
			      (if (> (imap-message-uid m)
				     (imap-message-uid m*))
				  (error "Message inserted into folder:" m*))
			      (loop (fix:+ i 1) i*)))))))
	      (folder-modified! folder 'SET-LENGTH n count)))))))

;;;; Message datatype

(define-class (<imap-message> (constructor (folder index))) (<message>)
  (uid)
  (length)
  (envelope)
  (bodystructure)
  (body-parts define standard initial-value '()))

(define-generic imap-message-uid (message))
(define-generic imap-message-length (message))
(define-generic imap-message-envelope (message))
(define-generic imap-message-bodystructure (message))

(define-method set-message-flags! ((message <imap-message>) flags)
  (with-imap-message-open message
    (lambda (connection)
      (imap:command:uid-store-flags connection
				    (imap-message-uid message)
				    (map imail-flag->imap-flag
					 (flags-delete "recent" flags))))))

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
	 (cons s (string-tail (symbol->string s) 1)))
       '(\SEEN \ANSWERED \FLAGGED \DELETED \DRAFT \RECENT)))

(define-method message-internal-time ((message <imap-message>))
  (with-imap-message-open message
    (lambda (connection)
      (imap:response:fetch-attribute
       (imap:command:uid-fetch connection
			       (imap-message-uid message)
			       '(INTERNALDATE))
       'INTERNALDATE))))

(define-method message-length ((message <imap-message>))
  (with-imap-message-open message
    (lambda (connection)
      connection
      (imap-message-length message))))

(define (with-imap-message-open message receiver)
  (let ((folder (message-folder message)))
    (if folder
	(begin
	  (guarantee-imap-folder-open folder)
	  (receiver (imap-folder-connection folder))))))

;;; These reflectors are needed to guarantee that we read the
;;; appropriate information from the server.  Some message slots are
;;; filled in by READ-MESSAGE-HEADERS!, but it's possible for
;;; READ-MESSAGE-HEADERS! to be interrupted, leaving unfilled slots.

(let ((accessor (slot-accessor <imap-message> 'UID))
      (initpred (slot-initpred <imap-message> 'UID)))
  (define-method imap-message-uid ((message <imap-message>))
    (if (not (initpred message))
	(with-imap-message-open message
	  (lambda (connection)
	    (let ((index (message-index message)))
	      (let ((suffix
		     (string-append " UID for message "
				    (number->string (+ index 1)))))
		((imail-ui:message-wrapper "Reading" suffix)
		 (lambda ()
		   (imap:command:fetch connection index '(UID))
		   (if (not (initpred message))
		       (begin
			 ;; Still don't have the goods.  Send a NOOP, in
			 ;; case the server is holding it back because it
			 ;; also needs to send an EXPUNGE.
			 (imap:command:noop connection)
			 (if (not (initpred message))
			     (error
			      (string-append "Unable to obtain"
					     suffix))))))))))))
    (accessor message)))

(define (guarantee-slot-initialized message initpred noun keywords)
  (if (not (initpred message))
      (with-imap-message-open message
	(lambda (connection)
	  (let ((uid (imap-message-uid message)))
	    (let ((suffix
		   (string-append
		    " " noun " for message "
		    (number->string (+ (message-index message) 1)))))
	      ((imail-ui:message-wrapper "Reading" suffix)
	       (lambda ()
		 (imap:read-literal-progress-hook imail-ui:progress-meter
		   (lambda ()
		     (imap:command:uid-fetch connection uid keywords)
		     (if (not (initpred message))
			 (error
			  (string-append "Unable to obtain" suffix)))))))))))))

(let ((reflector
       (lambda (generic-procedure slot-name guarantee)
	 (let ((initpred (slot-initpred <imap-message> slot-name)))
	   (define-method generic-procedure ((message <imap-message>))
	     (guarantee message initpred)
	     (call-next-method message))))))
  (reflector message-header-fields 'HEADER-FIELDS
    (lambda (message initpred)
      (guarantee-slot-initialized message initpred "header" '(RFC822.HEADER))))
  (reflector message-flags 'FLAGS
    (lambda (message initpred)
      (guarantee-slot-initialized message initpred "flags" '(FLAGS)))))

(let ((reflector
       (lambda (generic-procedure slot-name guarantee)
	 (let ((accessor (slot-accessor <imap-message> slot-name))
	       (initpred (slot-initpred <imap-message> slot-name)))
	   (define-method generic-procedure ((message <imap-message>))
	     (guarantee message initpred)
	     (accessor message))))))
  (reflector imap-message-length 'LENGTH
    (lambda (message initpred)
      (guarantee-slot-initialized message initpred "length" '(RFC822.SIZE))))
  (reflector imap-message-envelope 'ENVELOPE
    (lambda (message initpred)
      (guarantee-slot-initialized message initpred "envelope" '(ENVELOPE))))
  (reflector imap-message-bodystructure 'BODYSTRUCTURE
    (lambda (message initpred)
      (guarantee-slot-initialized message initpred "MIME structure"
				  '(BODYSTRUCTURE)))))

(define-method preload-folder-outlines ((folder <imap-folder>))
  (guarantee-imap-folder-open folder)
  (let ((messages
	 (messages-satisfying folder
	   (lambda (message)
	     (not (and (imap-message-header-fields-initialized? message)
		       (imap-message-length-initialized? message)))))))
    (if (pair? messages)
	((imail-ui:message-wrapper "Reading message headers")
	 (lambda ()
	   (imap:command:fetch-set (imap-folder-connection folder)
				   (message-list->set messages)
				   '(RFC822.HEADER RFC822.SIZE)))))))
	

(define imap-message-header-fields-initialized?
  (slot-initpred <imap-message> 'HEADER-FIELDS))

(define imap-message-length-initialized?
  (slot-initpred <imap-message> 'LENGTH))

(define (messages-satisfying folder predicate)
  (let ((n (folder-length folder)))
    (let loop ((i 0) (messages '()))
      (if (< i n)
	  (loop (+ i 1)
		(let ((message (get-message folder i)))
		  (if (predicate message)
		      (cons message messages)
		      messages)))
	  (reverse! messages)))))

(define (message-list->set messages)
  (let loop ((indexes (map message-index messages)) (groups '()))
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

;;;; MIME support

(define-method mime-message-body-structure ((message <imap-message>))
  (imap-message-bodystructure message))

(define-method write-message-body ((message <imap-message>) port)
  (write-mime-message-body-part
   message '(TEXT) (imap-message-length message) port))

(define-method write-mime-message-body-part
    ((message <imap-message>) selector cache? port)
  (let ((section
	 (map (lambda (x)
		(if (exact-nonnegative-integer? x)
		    (+ x 1)
		    x))
	      selector)))
    (let ((entry
	   (list-search-positive (imap-message-body-parts message)
	     (lambda (entry)
	       (equal? (car entry) section)))))
      (cond (entry
	     (write-string (cdr entry) port))
	    ((and cache?
		  (let ((limit (imail-ui:body-cache-limit message)))
		    (and limit
			 (if (and (exact-nonnegative-integer? cache?)
				  (exact-nonnegative-integer? limit))
			     (< cache? limit)
			     #t))))
	     (let ((part (%imap-message-body-part message section)))
	       (set-imap-message-body-parts!
		message
		(cons (cons section part)
		      (imap-message-body-parts message)))
	       (write-string part port)))
	    (else
	     (imap:bind-fetch-body-part-port port
	       (lambda ()
		 (%imap-message-body-part message section))))))))

(define (%imap-message-body-part message section)
  (imap:response:fetch-body-part
   (let ((suffix 
	  (string-append " body"
			 (if (equal? section '(TEXT)) "" " part")
			 " for message "
			 (number->string (+ (message-index message) 1)))))
     ((imail-ui:message-wrapper "Reading" suffix)
      (lambda ()
	(imap:read-literal-progress-hook imail-ui:progress-meter
	  (lambda ()
	    (with-imap-message-open message
	      (lambda (connection)
		(imap:command:uid-fetch
		 connection
		 (imap-message-uid message)
		 `(',(string-append "body["
				    (decorated-string-append
				     "" "." ""
				     (map (lambda (x)
					    (if (exact-nonnegative-integer? x)
						(number->string x)
						(symbol->string x)))
					  section))
				    "]"))))))))))
   section
   #f))

(define (parse-mime-body body)
  (cond ((not (and (pair? body) (list? body))) (parse-mime-body:lose body))
	((string? (car body)) (parse-mime-body:one-part body))
	((pair? (car body)) (parse-mime-body:multi-part body))
	(else (parse-mime-body:lose body))))

(define (parse-mime-body:one-part body)
  (let ((n (length body)))
    (cond ((string-ci=? "text" (car body))
	   (if (not (fix:>= n 8))
	       (parse-mime-body:lose body))
	   (apply make-mime-body-text
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
	       (parse-mime-body:lose body))
	   (let* ((enclosed (parse-mime-body (list-ref body 8)))
		  (enclosure
		   (apply make-mime-body-message
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
	       (parse-mime-body:lose body))
	   (apply make-mime-body-basic
		  (intern (list-ref body 0))
		  (intern (list-ref body 1))
		  (parse-mime-parameters (list-ref body 2))
		  (list-ref body 3)
		  (list-ref body 4)
		  (intern (list-ref body 5))
		  (list-ref body 6)
		  (parse-mime-body:extensions (list-tail body 7)))))))

(define (parse-mime-body:multi-part body)
  (let loop ((tail body) (index 0))
    (if (not (pair? tail))
	(parse-mime-body:lose body))
    (if (string? (car tail))
	(let ((enclosed (map parse-mime-body (sublist body 0 index)))
	      (extensions (parse-mime-body:extensions (cdr tail))))
	  (let ((enclosure
		 (make-mime-body-multipart (intern (car tail))
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

(define (parse-mime-body:extensions tail)
  (if (pair? tail)
      (if (pair? (cdr tail))
	  (let ((disposition (parse-mime-disposition (cadr tail))))
	    (if (pair? (cddr tail))
		(list (car tail) disposition (caddr tail))
		(list (car tail) disposition #f)))
	  (list (car tail) #f #f))
      (list #f #f #f)))

(define (parse-mime-body:lose body)
  (error "Unrecognized MIME bodystructure:" body))

(define (parse-mime-parameters parameters)
  (let ((lose (lambda () (error "Malformed MIME parameters:" parameters))))
    (let loop ((parameters parameters) (alist '()))
      (if (pair? parameters)
	  (if (pair? (cdr parameters))
	      (loop (cddr parameters)
		    (cons (cons (intern (car parameters)) (cadr parameters))
			  alist))
	      (lose))
	  (if (null? parameters)
	      (reverse! alist)
	      (lose))))))

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

;;;; Server operations

(define-method %create-folder ((url <imap-url>))
  (with-open-imap-connection url
    (lambda (connection)
      (imap:command:create connection (imap-url-server-mailbox url)))))

(define-method %delete-folder ((url <imap-url>))
  (with-open-imap-connection url
    (lambda (connection)
      (imap:command:delete connection (imap-url-server-mailbox url)))))

(define-method %rename-folder ((url <imap-url>) (new-url <imap-url>))
  (if (compatible-imap-urls? url new-url)
      (with-open-imap-connection url
	(lambda (connection)
	  (imap:command:rename connection
			       (imap-url-server-mailbox url)
			       (imap-url-server-mailbox new-url))))
      (error "Unable to perform rename between different IMAP accounts:"
	     url new-url)))

(define-method %append-message ((message <message>) (url <imap-url>))
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
		 (thunk))))))
    (if (let ((url* (folder-url folder)))
	  (and (imap-url? url*)
	       (compatible-imap-urls? url url*)))
	(begin
	  (guarantee-imap-folder-open folder)
	  (let ((connection (imap-folder-connection folder)))
	    (maybe-create connection
	      (lambda ()
		(imap:command:uid-copy connection
				       (imap-message-uid message)
				       (imap-url-server-mailbox url))))))
	(with-open-imap-connection url
	  (lambda (connection)
	    (maybe-create connection
	      (lambda ()
		(imap:command:append connection
				     (imap-url-server-mailbox url)
				     (map imail-flag->imap-flag
					  (flags-delete
					   "recent"
					   (message-flags message)))
				     (message-internal-time message)
				     (message->string message)))))))))

(define-method with-open-connection ((url <imap-url>) thunk)
  (with-open-imap-connection url
    (lambda (connection)
      connection
      (thunk))))

;;;; Folder operations

(define-method %open-folder ((url <imap-url>))
  (let ((folder
	 (make-imap-folder url
			   (or (search-imap-connections
				(lambda (connection)
				  (eq? (imap-connection-url connection) url)))
			       (make-imap-connection url)))))
    (reset-imap-folder! folder)
    (guarantee-imap-folder-open folder)
    folder))

(define-method %close-folder ((folder <imap-folder>))
  (let ((connection (imap-folder-connection folder)))
    (maybe-close-imap-connection connection)
    (set-imap-connection-folder! connection #f))
  (folder-modified! folder 'STATUS))

(define-method folder-length ((folder <imap-folder>))
  (imap-folder-n-messages folder))

(define-method %get-message ((folder <imap-folder>) index)
  (vector-ref (imap-folder-messages folder) index))

(define-method first-unseen-message-index ((folder <imap-folder>))
  (or (imap-folder-unseen folder) 0))

(define-method expunge-deleted-messages ((folder <imap-folder>))
  (guarantee-imap-folder-open folder)
  (imap:command:expunge (imap-folder-connection folder)))

(define-method search-folder ((folder <imap-folder>) criteria)
  (guarantee-imap-folder-open folder)
  (map (lambda (index) (- index 1))
       (imap:response:search-indices
	(let ((connection (imap-folder-connection folder)))
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

(define-method save-folder ((folder <imap-folder>))
  ;; Changes are always written through.
  folder
  #f)

(define-method discard-folder-cache ((folder <imap-folder>))
  (close-folder folder)
  (reset-imap-folder! folder))

(define-method probe-folder ((folder <imap-folder>))
  (guarantee-imap-folder-open folder)
  (imap:command:noop (imap-folder-connection folder)))

(define-method folder-connection-status ((folder <imap-folder>))
  (if (test-imap-connection-open (imap-folder-connection folder))
      'ONLINE
      'OFFLINE))

(define-method disconnect-folder ((folder <imap-folder>))
  (close-folder folder))

(define-method folder-supports-mime? ((folder <imap-folder>))
  folder
  #t)

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
  ((imail-ui:message-wrapper "Select mailbox " mailbox)
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
  (imap:command:fetch-set connection
			  (string-append (number->string (+ start 1))
					 ":"
					 (if end (number->string end) "*"))
			  items))

(define (imap:command:fetch-set connection set items)
  (imap:command:multiple-response imap:response:fetch? connection
				  'FETCH `',set items))

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
  (imap:command:multiple-response imap:response:list? connection
				  'LIST reference pattern))

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

(define (imap:command connection command . arguments)
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
       (if (eq? command 'UID)
	   (car arguments)
	   command)))))

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
    (imap-transcript-write command port)
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
      (cond ((or (symbol? argument)
		 (exact-nonnegative-integer? argument))
	     (imap-transcript-write argument port))
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

(define (imap:wait-for-tagged-response connection tag command)
  (let ((port (imap-connection-port connection)))
    (let loop ()
      (let ((response (imap:read-server-response-1 port)))
	(let ((tag* (imap:response:tag response)))
	  (if tag*
	      (let ((responses
		     (process-responses
		      connection command
		      (dequeue-imap-responses connection))))
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
		(enqueue-imap-response connection response)
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

(define (process-responses connection command responses)
  (with-folder-events-deferred
    (lambda ()
      (if (pair? responses)
	  (if (process-response connection command (car responses))
	      (cons (car responses)
		    (process-responses connection command (cdr responses)))
	      (process-responses connection command (cdr responses)))
	  '()))))

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
	     (set-imap-folder-length! folder
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
	      (get-message folder
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
		(if (memq '\* pflags) #t #f))
	       (set-imap-folder-permanent-flags!
		folder
		(map imap-flag->imail-flag (delq '\* pflags)))))))
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
     (%set-imap-message-bodystructure! message (parse-mime-body datum))
     #t)
    ((ENVELOPE)
     (%set-imap-message-envelope! message datum)
     #t)
    ((FLAGS)
     (%set-message-flags! message (map imap-flag->imail-flag datum))
     #t)
    ((RFC822.HEADER)
     (%set-message-header-fields! message (string->header-fields datum))
     #t)
    ((RFC822.SIZE)
     (%set-imap-message-length! message datum)
     #t)
    ((UID)
     (%set-imap-message-uid! message datum)
     #t)
    (else #f)))

(define (with-imap-connection-folder connection receiver)
  (let ((folder (imap-connection-folder connection)))
    (if folder
	(receiver folder))))

(define %set-message-header-fields!
  (slot-modifier <imap-message> 'HEADER-FIELDS))

(define %message-flags-initialized?
  (slot-initpred <imap-message> 'FLAGS))

(define %set-imap-message-uid!
  (slot-modifier <imap-message> 'UID))

(define %set-imap-message-length!
  (slot-modifier <imap-message> 'LENGTH))

(define %set-imap-message-envelope!
  (slot-modifier <imap-message> 'ENVELOPE))

(define %set-imap-message-bodystructure!
  (slot-modifier <imap-message> 'BODYSTRUCTURE))