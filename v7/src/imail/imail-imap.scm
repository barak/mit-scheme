;;; -*-Scheme-*-
;;;
;;; $Id: imail-imap.scm,v 1.6 2000/04/28 16:49:10 cph Exp $
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
  (user-id accessor url-user-id)
  (auth-type define accessor)
  (host define accessor)
  (port define accessor)
  (mailbox define accessor)
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

;;;; Server operations

(define-class (<imap-connection> (constructor (user-id host port))) ()
  (host define accessor)
  (user-id define accessor)
  (port define standard)
  (sequence-number define standard
		   initial-value 0)
  (response-queue define accessor
		  initializer (lambda () (cons '() '())))
  (folder define standard
	  accessor selected-imap-folder
	  modifier select-imap-folder
	  initial-value #f))

(define-class (<imap-folder> (constructor (connection url))) (<folder>)
  (connection define accessor)
  (url accessor folder-url)
  (allowed-flags define standard)
  (permanent-flags define standard)
  (uidvalidity define standard
	       initial-value #f)
  (first-unseen define standard)
  (length define standard
	  initial-value 0)
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

(define (forget-imap-folder-messages! folder)
  (let ((v (imap-folder-messages folder)))
    (let ((n (vector-length v)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i n))
	(let ((m (vector-ref v i)))
	  (if (and m (imap-message-external m))
	      (detach-message (imap-message-external m)))
	  (vector-set! v i #f)))))
  (set-imap-folder-messages! folder
			     (make-vector (imap-folder-length folder) #f)))

(define (open-imap-connection url)
  (let ((host (imap-url-host url))
	(user-id (or (url-user-id url) (imail-default-user-id))))
    (let loop ((alist associated-imap-connections) (prev #f))
      (if (pair? alist)
	  (let ((connection (weak-car (car alist))))
	    (if connection
		(if (let ((h.u (weak-cdr (car alist))))
		      (and (string-ci=? (car h.u) host)
			   (string=? (cdr h.u) user-id)))
		    connection
		    (loop (cdr alist) alist))
		(let ((next (cdr alist)))
		  (if prev
		      (set-cdr! prev next)
		      (set! associated-imap-connections next))
		  (loop next prev))))
	  (let ((connection
		 (make-imap-connection
		  host user-id
		  (let ((port (open-tcp-stream-socket host "imap2" #f "\n")))
		    (read-line port)	;discard server announcement
		    port))))
	    (set! associated-imap-connections
		  (cons (weak-cons connection (cons host user-id))
			associated-imap-connections))
	    (if (not (memq 'IMAP4REV1
			   (imap:command:capability connection)))
		(begin
		  (close-imap-connection connection)
		  (error "Server doesn't support IMAP4rev1:" host)))
	    (let ((response
		   (authenticate url user-id
		     (lambda (passphrase)
		       (imap:command:login connection user-id passphrase)))))
	      (if (imap:response:no? response)
		  (begin
		    (close-imap-connection connection)
		    (error "Unable to log in:" response))))
	    connection)))))

(define (close-imap-connection connection)
  (let ((port (imap-connection-port connection)))
    (if port
	(begin
	  (close-port port)
	  (set-imap-connection-port! connection port))))
  (let ((host (imap-connection-host connection))
	(user-id (imap-connection-user-id connection)))
    (let loop ((alist associated-imap-connections) (prev #f))
      (if (pair? alist)
	  (let ((connection* (weak-car (car alist))))
	    (if (or (not connection*) (eq? connection* connection))
		(let ((next (cdr alist)))
		  (if prev
		      (set-cdr! prev next)
		      (set! associated-imap-connections next))
		  (if connection*
		      (loop next prev)))
		(loop (cdr alist) alist)))))))

(define associated-imap-connections '())

(define (imap-connection/enqueue-response! connection response)
  (let ((queue (imap-connection-response-queue connection)))
    (let ((next (cons response '())))
      (if (pair? (cdr queue))
	  (set-cdr! (cdr queue) next)
	  (set-car! queue next))
      (set-cdr! queue next))))

(define (imap-connection/dequeue-responses! connection)
  (let ((queue (imap-connection-response-queue connection)))
    (let ((responses (car queue)))
      (set-car! queue '())
      (set-cdr! queue '())
      responses)))

(define (expunge-imap-folder-message folder index)
  ???)

(define-method %open-folder ((url <imap-url>))
  (let ((connection (open-imap-connection url)))
    (let ((folder (make-imap-folder connection url)))
      (select-imap-folder connection folder)
      (if (not (imap:command:select connection (imap-url-mailbox url)))
	  (select-imap-folder connection #f))
      folder)))

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

(define-method subscribed-folder-names ((url <imap-url>))
  ???)

(define-method %folder-valid? ((folder <imap-folder>))
  ???)

(define-method folder-length ((folder <imap-folder>))
  (imap-folder-length folder))

(define-method %get-message ((folder <imap-folder>) index)
  (let ((messages (imap-folder-messages folder)))
    (let ((message
	   (or (vector-ref messages index)
	       (apply make-imap-message
		      (imap:command:fetch (imap-folder-connection folder)
					  index
					  '(UID FLAGS RFC822.SIZE
						ENVELOPE))))))
      (or (imap-message-external message)
	  (let ((external
		 (let ((items
			(imap:command:fetch (imap-folder-connection folder)
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

(define (translate-string-line-endings string)
  (translate-substring-line-endings string 0 (string-length string)))

(define (translate-substring-line-endings string start end)
  (let ((indexes (substring-search-all "\r\n" string start end)))
    (let ((s (make-string (fix:- (fix:- end start) (length indexes)))))
      (let loop ((indexes indexes) (i start) (j 0))
	(if (pair? indexes)
	    (let ((j (substring-move! string i (car indexes) s j)))
	      (string-set! s j #\newline)
	      (loop (cdr indexes) (fix:+ (car indexes) 2) (fix:+ j 1)))
	    (substring-move! string i end s j)))
      s)))

(define-method %append-message ((folder <imap-folder>) message)
  ???)

(define-method expunge-deleted-messages ((folder <imap-folder>))
  ???)

(define-method search-folder ((folder <imap-folder>) criteria)
  ???)

(define-method poll-folder ((folder <imap-folder>))
  ???)

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

(define-method subscribe-folder ((folder <imap-folder>))
  folder
  (error "Unimplemented operation:" 'SUBSCRIBE-FOLDER))

(define-method unsubscribe-folder ((folder <imap-folder>))
  folder
  (error "Unimplemented operation:" 'UNSUBSCRIBE-FOLDER))

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
      (imap:command:multiple-response imap:response:fetch?
				      connection 'FETCH
				      (string-append (number->string
						      (+ start 1))
						     ":"
						     (number->string end))
				      items)
      '()))

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

(define (next-imap-command-tag connection)
  (let ((n (imap-connection-sequence-number connection)))
    (set-imap-connection-sequence-number! connection (+ n 1))
    (string-append "A" (string-pad-left (number->string n) 4 #\0))))

(define (imap:send-command-argument connection tag command argument)
  (let ((port (imap-connection-port connection)))
    (let loop ((argument argument))
      (cond ((or (symbol? argument)
		 (exact-nonnegative-integer? argument))
	     (write argument port))
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
	       (imap-connection/enqueue-response! connection response)
	       (loop)))))))

(define (imap:wait-for-tagged-response connection tag command)
  (let ((port (imap-connection-port connection)))
    (let loop ()
      (let ((response (imap:read-server-response port)))
	(if (imap:response:tag response)
	    (let ((responses
		   (process-responses
		    connection command
		    (imap-connection/dequeue-responses! connection))))
	      (cond ((not (string-ci=? tag (imap:response:tag response)))
		     (error "Out-of-sequence tag:"
			    (imap:response:tag response) tag))
		    ((or (imap:response:ok? response)
			 (imap:response:no? response))
		     (cons response responses))
		    (else
		     (error "IMAP protocol error:" response))))
	    (begin
	      (imap-connection/enqueue-response! connection response)
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
	   (if (not (= count (imap-folder-length folder)))
	       (begin
		 (set-imap-folder-length! folder count)
		 (forget-imap-folder-messages! folder))))
	 #f)
	((imap:response:expunge? response)
	 (expunge-imap-folder-message (selected-imap-folder connection)
				      (imap:response:expunge-index response))
	 #f)
	((imap:response:flags? response)
	 (set-imap-folder-allowed-flags! (selected-imap-folder connection)
					 (imap:response:flags response))
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
	   (set-imap-folder-uidvalidity! folder uidvalidity)))
	((imap:response-code:unseen? code)
	 (set-imap-folder-first-unseen! (selected-imap-folder connection)
					(imap:response-code:unseen code)))
	((imap:response-code:permanentflags? code)
	 (set-imap-folder-permanent-flags!
	  (selected-imap-folder connection)
	  (imap:response-code:permanentflags code)))
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