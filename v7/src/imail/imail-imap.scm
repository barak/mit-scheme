;;; -*-Scheme-*-
;;;
;;; $Id: imail-imap.scm,v 1.10 2000/05/02 21:42:08 cph Exp $
;;;
;;; Copyright (c) 1999-2000 Massachusetts Institute of Technology
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
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;;; IMAIL mail reader: IMAP back end

(declare (usual-integrations))

;;;; URL

(define-class (<imap-url>
	       (constructor (user-id auth-type host port mailbox uid)))
    (<url>)
  ;; User name to connect as.
  (user-id accessor url-user-id)
  ;; Type of authentication to use.  Ignored.
  (auth-type define accessor)
  ;; Name or IP address of host to connect to.
  (host define accessor)
  ;; Port number to connect to.
  (port define accessor)
  ;; Name of mailbox to access.
  (mailbox define accessor)
  ;; Unique ID specifying a message.  Ignored.
  (uid define accessor))

(define-url-protocol "imap" <imap-url>
  (lambda (string)
    (let ((lose (lambda () (error:bad-range-argument string #f))))
      (if (not (string-prefix? "//" string))
	  (lose))
      (let ((end (string-length string)))
	(let ((slash (substring-find-next-char string 2 end #\/)))
	  (if (not slash)
	      (lose))
	  (let ((pv1 (parse-substring imap:parse:server string 2 slash)))
	    (if (not pv1)
		(lose))
	    (let ((pv2
		   (parse-substring imap:parse:simple-message
				    string (fix:+ slash 1) end)))
	      (if (not pv2)
		  (lose))
	      (make-imap-url (parser-token pv1 'USER-ID)
			     (parser-token pv1 'AUTH-TYPE)
			     (parser-token pv1 'HOST)
			     (parser-token pv1 'PORT)
			     (parser-token pv2 'MAILBOX)
			     (parser-token pv2 'UID)))))))))

(define-method url-body ((url <imap-url>))
  (string-append
   "//"
   (let ((user-id (url-user-id url))
	 (auth-type (imap-url-auth-type url)))
     (if (or user-id auth-type)
	 (string-append (if user-id
			    (url:encode-string user-id)
			    "")
			(if auth-type
			    (string-append ";auth="
					   (if (string=? auth-type "*")
					       auth-type
					       (url:encode-string auth-type)))
			    "")
			"@")
	 ""))
   (imap-url-host url)
   (let ((port (imap-url-port url)))
     (if port
	 (string-append ":" port)
	 ""))
   "/"
   (url:encode-string (imap-url-mailbox url))
   (let ((uid (imap-url-uid url)))
     (if uid
	 (string-append "/;uid=" uid)
	 ""))))

;;;; Server connection

(define-class (<imap-connection> (constructor (host ip-port user-id))) ()
  (host define accessor)
  (ip-port define accessor)
  (user-id define accessor)
  (port define standard
	initial-value #f)
  (sequence-number define standard
		   initial-value 0)
  (response-queue define accessor
		  initializer (lambda () (cons '() '())))
  (folder define standard
	  accessor selected-imap-folder
	  modifier select-imap-folder
	  initial-value #f))

(define (reset-imap-connection connection)
  (without-interrupts
   (lambda ()
     (set-imap-connection-sequence-number! connection 0)
     (let ((queue (imap-connection-response-queue connection)))
       (set-car! queue '())
       (set-cdr! queue '()))
     (select-imap-folder connection #f))))

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
  (let ((host (imap-url-host url))
	(ip-port (imap-url-port url))
	(user-id (or (url-user-id url) (imail-default-user-id))))
    (let loop ((connections memoized-imap-connections) (prev #f))
      (if (weak-pair? connections)
	  (let ((connection (weak-car connections)))
	    (if connection
		(if (and (string-ci=? (imap-connection-host connection) host)
			 (eqv? (imap-connection-ip-port connection) ip-port)
			 (string=? (imap-connection-user-id connection)
				   user-id))
		    (begin
		      (guarantee-imap-connection-open connection)
		      connection)
		    (loop (weak-cdr connections) alist))
		(let ((next (weak-cdr connections)))
		  (if prev
		      (weak-set-cdr! prev next)
		      (set! memoized-imap-connections next))
		  (loop next prev))))
	  (let ((connection (make-imap-connection host ip-port user-id)))
	    (set! memoized-imap-connections
		  (weak-cons connection memoized-imap-connections))
	    (guarantee-imap-connection-open connection)
	    connection)))))

(define memoized-imap-connections '())

(define (guarantee-imap-connection-open connection)
  (if (not (imap-connection-port connection))
      (let ((host (imap-connection-host connection))
	    (ip-port (imap-connection-ip-port connection))
	    (user-id (imap-connection-user-id connection)))
	(let ((port
	       (open-tcp-stream-socket host (or ip-port "imap2") #f "\n")))
	  (read-line port)	;discard server announcement
	  (set-imap-connection-port! connection port)
	  (reset-imap-connection connection)
	  (let ((response
		 (authenticate host user-id
		   (lambda (passphrase)
		     (imap:command:login connection user-id passphrase)))))
	    (if (imap:response:no? response)
		(begin
		  (close-imap-connection connection)
		  (error "Unable to log in:" response))))
	  (if (not (memq 'IMAP4REV1 (imap:command:capability connection)))
	      (begin
		(close-imap-connection connection)
		(error "Server doesn't support IMAP4rev1:" host)))))))

(define (close-imap-connection connection)
  (let ((port (imap-connection-port connection)))
    (if port
	(begin
	  (close-port port)
	  (set-imap-connection-port! connection #f))))
  (reset-imap-connection connection))

(define (imap-connection-open? connection)
  (imap-connection-port connection))

;;;; Folder datatype

(define-class (<imap-folder> (constructor (url connection))) (<folder>)
  (connection define accessor)
  (allowed-flags define standard)
  (permanent-flags define standard)
  (uidvalidity define standard
	       initial-value #f)
  (first-unseen define standard
		initial-value #f)
  (messages define standard
	    initializer (lambda () (make-vector 0))))

(define-class (<imap-message>
	       (constructor (uid flags length envelope)))
    ()
  (uid define accessor)
  (flags define standard)
  (length define accessor)
  (envelope define accessor)
  (external define standard
	    initial-value #f))

(define (set-imap-folder-length! folder count)
  (let ((v (imap-folder-messages folder))
	(v* (make-vector count #f))
	(connection (imap-folder-connection folder)))
    (let ((end (vector-length v)))
      (fill-messages-vector connection v*)
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i count))
	(let ((uid (imap-message-uid (vector-ref v* i))))
	  (let loop ((j 0))
	    (if (fix:< j end)
		(if (and (vector-ref v j)
			 (= uid (imap-message-uid (vector-ref v j))))
		    (begin
		      (vector-set! v* i (vector-ref v j))
		      (vector-set! v j #f))
		    (loop (fix:+ j 1)))))))
      (detach-external-messages v))
    (set-imap-folder-messages! folder v*))
  (folder-modified! folder))

(define (forget-imap-folder-messages! folder)
  (let ((v (imap-folder-messages folder)))
    (detach-external-messages v)
    (fill-messages-vector (imap-folder-connection folder) v))
  (folder-modified! folder))

(define (fill-messages-vector connection messages)
  (let ((end (vector-length messages)))
    (do ((responses
	  (imap:command:fetch-range connection 0 end
				    '(UID FLAGS RFC822.SIZE ENVELOPE))
	  (cdr responses))
	 (index 0 (fix:+ index 1)))
	((fix:= index end))
      (vector-set! messages index (apply make-imap-message (car responses))))))

(define (detach-external-messages v)
  (for-each-vector-element v
    (lambda (m)
      (if (and m (imap-message-external m))
	  (detach-message (imap-message-external m))))))

(define (remove-imap-folder-message folder index)
  (let ((v (imap-folder-messages folder)))
    (let ((m (vector-ref v index)))
      (if (and m (imap-message-external m))
	  (detach-message (imap-message-external m))))
    (let ((end (vector-length v)))
      (let ((v* (make-vector (fix:- end 1))))
	(subvector-move-left! v 0 index v* 0)
	(subvector-move-left! v (fix:+ index 1) end v* index)
	(set-imap-folder-messages! folder v*))))
  (folder-modified! folder))

;;;; Server operations

(define-method %new-folder ((url <imap-url>))
  ???)

(define-method %delete-folder ((url <imap-url>))
  ???)

(define-method %move-folder ((url <imap-url>) (new-url <imap-url>))
  ???)

(define-method %copy-folder ((url <imap-url>) (new-url <imap-url>))
  ???)

(define-method available-folder-names ((url <imap-url>))
  ???)

;;;; Folder operations

(define-method %open-folder ((url <imap-url>))
  (let ((connection (get-imap-connection url)))
    (let ((folder (make-imap-folder url connection)))
      (select-imap-folder connection folder)
      (if (not (imap:command:select connection (imap-url-mailbox url)))
	  (select-imap-folder connection #f))
      folder)))

(define-method %close-folder ((folder <imap-folder>))
  (close-imap-connection (imap-folder-connection folder)))

(define-method %folder-valid? ((folder <imap-folder>))
  folder
  #t)

(define-method folder-length ((folder <imap-folder>))
  (vector-length (imap-folder-messages folder)))

(define-method %get-message ((folder <imap-folder>) index)
  (let ((messages (imap-folder-messages folder))
	(connection (imap-folder-connection folder)))
    (let ((message
	   (or (vector-ref messages index)
	       (let ((message
		      (apply make-imap-message
			     (imap:command:fetch connection
						 index
						 '(UID FLAGS RFC822.SIZE
						       ENVELOPE)))))
		 (vector-set! messages index message)
		 message))))
      (or (imap-message-external message)
	  (let ((external
		 (let ((items
			(imap:command:fetch connection
					    index
					    '(RFC822.HEADER RFC822.TEXT))))
		   (make-attached-message
		    folder
		    (lines->header-fields
		     (except-last-pair!
		      (string->lines
		       (translate-string-line-endings (car items)))))
		    (translate-string-line-endings (cadr items))))))
	    (set-message-index! external index)
	    (set-imap-message-external! message external)
	    external)))))

(define-method first-unseen-message ((folder <imap-folder>))
  (let ((unseen (imap-folder-first-unseen folder)))
    (and unseen
	 (get-message folder unseen))))

(define-method %append-message ((folder <imap-folder>) message)
  ???)

(define-method expunge-deleted-messages ((folder <imap-folder>))
  ???)

(define-method search-folder ((folder <imap-folder>) criteria)
  ???)

(define-method poll-folder ((folder <imap-folder>))
  (imap:command:noop (imap-folder-connection folder))
  #f)

(define-method synchronize-folder ((folder <imap-folder>))
  ???)

(define-method %save-folder ((folder <imap-folder>))
  ???)

(define-method %maybe-revert-folder ((folder <imap-folder>) resolve-conflict)
  ???)

(define-method %revert-folder ((folder <imap-folder>))
  ???)

(define-method %write-folder ((folder <folder>) (url <imap-url>))
  ???)

;;;; IMAP command invocation

(define (imap:command:capability connection)
  (imap:response:capabilities
   (imap:command:single-response imap:response:capability?
				 connection 'CAPABILITY)))

(define (imap:command:login connection user-id passphrase)
  (imap:command:no-response connection 'LOGIN user-id passphrase))

(define (imap:command:select connection mailbox)
  (imap:response:ok? (imap:command:no-response connection 'SELECT mailbox)))

(define (imap:command:fetch connection index items)
  (let ((response
	 (imap:command:single-response imap:response:fetch?
				       connection 'FETCH (+ index 1) items)))
    (map (lambda (item)
	   (imap:response:fetch-attribute response item))
	 items)))

(define (imap:command:fetch-range connection start end items)
  (if (fix:< start end)
      (map (lambda (response)
	     (map (lambda (item)
		    (imap:response:fetch-attribute response item))
		  items))
	   (imap:command:multiple-response imap:response:fetch?
					   connection 'FETCH
					   (cons 'ATOM
						 (string-append
						  (number->string (+ start 1))
						  ":"
						  (number->string end)))
					   items))
      '()))

(define (imap:command:noop connection)
  (imap:command:no-response connection 'NOOP))

(define (imap:command:no-response connection command . arguments)
  (let ((responses (apply imap:command connection command arguments)))
    (if (not (null? (cdr responses)))
	(error "Malformed response from IMAP server:" responses))
    (car responses)))

(define (imap:command:single-response predicate connection command . arguments)
  (let ((responses (apply imap:command connection command arguments)))
    (if (imap:response:ok? (car responses))
	(if (and (pair? (cdr responses))
		 (predicate (cadr responses))
		 (null? (cddr responses)))
	    (cadr responses)
	    (error "Malformed response from IMAP server:" responses))
	(error "Server signalled a command error:" (car responses)))))

(define (imap:command:multiple-response predicate
					connection command . arguments)
  (let ((responses (apply imap:command connection command arguments)))
    (if (imap:response:ok? (car responses))
	(if (for-all? (cdr responses) predicate)
	    (cdr responses)
	    (error "Malformed response from IMAP server:" responses))
	(error "Server signalled a command error:" (car responses)))))

(define (imap:command connection command . arguments)
  (imap:wait-for-tagged-response connection
				 (imap:send-command connection
						    command arguments)
				 command))

(define (imap:send-command connection command arguments)
  (let ((tag (next-imap-command-tag connection))
	(port (imap-connection-port connection)))
    (write-string tag port)
    (write-char #\space port)
    (write command port)
    (for-each (lambda (argument)
		(write-char #\space port)
		(imap:send-command-argument connection tag command argument))
	      arguments)
    (write-char #\return port)
    (write-char #\linefeed port)
    (flush-output port)
    tag))

(define (imap:send-command-argument connection tag command argument)
  (let ((port (imap-connection-port connection)))
    (let loop ((argument argument))
      (cond ((or (symbol? argument)
		 (exact-nonnegative-integer? argument))
	     (write argument port))
	    ((and (pair? argument)
		  (eq? (car argument) 'ATOM)
		  (string? (cdr argument)))
	     (write-string (cdr argument) port))
	    ((string? argument)
	     (if (imap:string-may-be-quoted? argument)
		 (imap:write-quoted-string argument port)
		 (imap:write-literal-string connection tag argument)))
	    ((list? argument)
	     (write-char #\( port)
	     (if (pair? argument)
		 (begin
		   (loop (car argument))
		   (for-each (lambda (object)
			       (write-char #\space port)
			       (loop object))
			     (cdr argument))))
	     (write-char #\) port))
	    (else (error "Illegal IMAP syntax:" argument))))))

(define (imap:write-literal-string connection tag string)
  (let ((port (imap-connection-port connection)))
    (imap:write-literal-string-header string port)
    (flush-output port)
    (let loop ()
      (let ((response (imap:read-server-response port)))
	(cond ((imap:response:continue? response)
	       (imap:write-literal-string-body string port))
	      ((and (imap:response:tag response)
		    (string-ci=? tag (imap:response:tag response)))
	       (error "Unable to finish continued command:" response))
	      (else
	       (enqueue-imap-response connection response)
	       (loop)))))))

(define (imap:wait-for-tagged-response connection tag command)
  (let ((port (imap-connection-port connection)))
    (let loop ()
      (let ((response (imap:read-server-response port)))
	(if (imap:response:tag response)
	    (let ((responses
		   (process-responses
		    connection command
		    (dequeue-imap-responses connection))))
	      (cond ((not (string-ci=? tag (imap:response:tag response)))
		     (error "Out-of-sequence tag:"
			    (imap:response:tag response) tag))
		    ((or (imap:response:ok? response)
			 (imap:response:no? response))
		     (cons response responses))
		    (else
		     (error "IMAP protocol error:" response))))
	    (begin
	      (enqueue-imap-response connection response)
	      (loop)))))))

(define (process-responses connection command responses)
  (if (pair? responses)
      (if (process-response connection command (car responses))
	  (cons (car responses)
		(process-responses connection command (cdr responses)))
	  (process-responses connection command (cdr responses)))
      '()))

(define (process-response connection command response)
  (cond ((imap:response:status-response? response)
	 (let ((code (imap:response:response-text-code response))
	       (string (imap:response:response-text-string response)))
	   (if code
	       (process-response-text connection code string))
	   (if (and (imap:response:bye? response)
		    (not (eq? command 'LOGOUT)))
	       (begin
		 (close-imap-connection connection)
		 (error "Server shut down connection:" string))))
	 (imap:response:preauth? response))
	((imap:response:exists? response)
	 (let ((count (imap:response:exists-count response))
	       (folder (selected-imap-folder connection)))
	   (if (not (= count (folder-length folder)))
	       (set-imap-folder-length! folder count)))
	 #f)
	((imap:response:expunge? response)
	 (let ((folder (selected-imap-folder connection)))
	   (remove-imap-folder-message folder
				       (imap:response:expunge-index response))
	   (folder-modified! folder))
	 #f)
	((imap:response:flags? response)
	 (let ((folder (selected-imap-folder connection)))
	   (set-imap-folder-allowed-flags! folder
					   (imap:response:flags response))
	   (folder-modified! folder))
	 #f)
	((imap:response:recent? response)
	 #f)
	((or (imap:response:capability? response)
	     (imap:response:fetch? response)
	     (imap:response:list? response)
	     (imap:response:lsub? response)
	     (imap:response:search? response)
	     (imap:response:status? response))
	 #t)
	(else
	 (error "Illegal server response:" response))))

(define (process-response-text connection code text)
  (cond ((imap:response-code:uidvalidity? code)
	 (let ((folder (selected-imap-folder connection))
	       (uidvalidity (imap:response-code:uidvalidity code)))
	   (if (let ((uidvalidity* (imap-folder-uidvalidity folder)))
		 (or (not uidvalidity*)
		     (> uidvalidity uidvalidity*)))
	       (forget-imap-folder-messages! folder))
	   (set-imap-folder-uidvalidity! folder uidvalidity)
	   (folder-modified! folder)))
	((imap:response-code:unseen? code)
	 (let ((folder (selected-imap-folder connection)))
	   (set-imap-folder-first-unseen!
	    folder
	    (- (imap:response-code:unseen code) 1))
	   (folder-modified! folder)))
	((imap:response-code:permanentflags? code)
	 (let ((folder (selected-imap-folder connection)))
	   (set-imap-folder-permanent-flags!
	    folder
	    (imap:response-code:permanentflags code))
	   (folder-modified! folder)))
	((imap:response-code:alert? code)
	 (imail-present-user-alert
	  (lambda (port)
	    (write-string "Alert from IMAP server:" port)
	    (newline port)
	    (display text port)
	    (newline port))))
	#|
	((or (imap:response-code:newname? code)
	     (imap:response-code:parse? code)
	     (imap:response-code:read-only? code)
	     (imap:response-code:read-write? code)
	     (imap:response-code:trycreate? code))
	 unspecific)
	|#
	))