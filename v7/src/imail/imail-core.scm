;;; -*-Scheme-*-
;;;
;;; $Id: imail-core.scm,v 1.118 2001/03/19 19:29:48 cph Exp $
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

;;;; IMAIL mail reader: core definitions

(declare (usual-integrations))

;;;; Base object type

(define-class <imail-object> ()
  (properties define accessor
	      initializer make-1d-table))

(define (get-property object key default)
  (1d-table/get (imail-object-properties object) key default))

(define (store-property! object key datum)
  (1d-table/put! (imail-object-properties object) key datum))

(define (remove-property! object key)
  (1d-table/remove! (imail-object-properties object) key))

;;;; URL type

(define-class <url> (<imail-object>))

(define (guarantee-url url procedure)
  (if (not (url? url))
      (error:wrong-type-argument url "IMAIL URL" procedure)))

;; Return the canonical name of URL's protocol as a string.
(define-generic url-protocol (url))

;; Return the body of URL as a string.
(define-generic url-body (url))

(define (make-url-string protocol body)
  (string-append protocol ":" body))

(define (url->string url)
  (make-url-string (url-protocol url) (url-body url)))

(define-method write-instance ((url <url>) port)
  (write-instance-helper 'URL url port
    (lambda ()
      (write-char #\space port)
      (write (url->string url) port))))

;; Return a string that concisely identifies URL, for use in the
;; presentation layer.
(define-generic url-presentation-name (url))

;; Return a string that represents the object containing URL's folder.
;; E.g. the container of "imap://localhost/inbox" is
;; "imap://localhost/" (except that for IMAP folders, the result may
;; be affected by the NAMESPACE prefix information).
(define (url-container-string url)
  (make-url-string (url-protocol url)
		   (url-body-container-string url)))

(define-generic url-body-container-string (url))

;; Return the base name of URL.  This is the PATHNAME-NAME of a
;; file-based folder, and for IMAP it's the part of the mailbox name
;; following the rightmost delimiter.
(define-generic url-base-name (url))

;; Return a URL that has the same container as URL, but with base name
;; NAME.  This is roughly equivalent to appending NAME to the
;; container string of URL.
(define-generic make-peer-url (url name))

;; Return #T if URL represents an existing folder.
(define-generic url-exists? (url))

;; Return a string that uniquely identifies the server and account for
;; URL.  E.g. for IMAP this could be the URL string without the
;; mailbox information.  This string will be included in the
;; pass-phrase prompt, and also used as a key for memoization.
(define-generic url-pass-phrase-key (url))

;; Convert STRING to a URL.  GET-DEFAULT-URL is a procedure of one
;; argument that returns a URL that is used to fill in defaults if
;; STRING is a specification for a partial URL.  GET-DEFAULT-URL is
;; called with #F as its first argument to return a default URL to be
;; used if STRING doesn't explicitly specify a protocol.  Otherwise,
;; it is called with a protocol name as its first argument to return a
;; protocol-specific default.
(define (parse-url-string string get-default-url)
  (let ((colon (string-find-next-char string #\:)))
    (if colon
	(parse-url-body (string-tail string (fix:+ colon 1))
			(get-default-url (string-head string colon)))
	(parse-url-body string (get-default-url #f)))))

;; Protocol-specific parsing.  Dispatch on the class of DEFAULT-URL.
;; Each method is responsible for calling INTERN-URL on the result of
;; the parse, and returning the interned URL.  Illegal syntax in
;; STRING must cause an error to be signalled.
(define-generic parse-url-body (string default-url))

(define (intern-url url)
  (let ((string (url->string url)))
    (or (hash-table/get interned-urls string #f)
	(begin
	  (hash-table/put! interned-urls string url)
	  url))))

(define interned-urls
  (make-string-hash-table))

(define (define-url-protocol name class)
  (define-method url-protocol ((url class)) url name)
  (hash-table/put! url-protocols (string-downcase name) class))

(define (url-protocol-name? name)
  (hash-table/get url-protocols (string-downcase name) #f))

(define url-protocols
  (make-string-hash-table))

;; Do completion on URL-STRING, which is a partially-specified URL.
;; Tail-recursively calls one of the three procedure arguments, as
;; follows.  If URL-STRING has a unique completion, IF-UNIQUE is
;; called with that completion.  If URL-STRING has more than one
;; completion, IF-NOT-UNIQUE is called with two arguments: the first
;; argument is a prefix string that all of the completions share, and
;; the second argument is a thunk that returns a list of the
;; completions.  If URL-STRING has no completions, IF-NOT-FOUND is
;; called with no arguments.

;; See PARSE-URL-STRING for a description of GET-DEFAULT-URL.

(define (url-complete-string string get-default-url
			     if-unique if-not-unique if-not-found)
  (call-with-values (lambda () (url-completion-args string get-default-url))
    (lambda (body default-url prepend)
      (if default-url
	  (%url-complete-string body default-url
	    (lambda (body)
	      (if-unique (prepend body)))
	    (lambda (prefix get-completions)
	      (if-not-unique (prepend prefix)
			     (lambda () (map prepend (get-completions)))))
	    if-not-found)
	  (if-not-found)))))

(define-generic %url-complete-string
    (string default-url if-unique if-not-unique if-not-found))

;; Return a list of the completions for STRING.
;; See PARSE-URL-STRING for a description of GET-DEFAULT-URL.

(define (url-string-completions string get-default-url)
  (call-with-values (lambda () (url-completion-args string get-default-url))
    (lambda (body default-url prepend)
      (map prepend
	   (if default-url
	       (%url-string-completions body default-url)
	       '())))))

(define-generic %url-string-completions (string default-url))

(define (url-completion-args string get-default-url)
  (let ((colon (string-find-next-char string #\:))
	(make-prepend
	 (lambda (protocol)
	   (lambda (body)
	     (make-url-string protocol body)))))
    (if colon
	(let ((protocol (string-head string colon)))
	  (values (string-tail string (fix:+ colon 1))
		  (and (url-protocol-name? protocol)
		       (get-default-url protocol))
		  (make-prepend protocol)))
	(let ((url (get-default-url #f)))
	  (values string url (make-prepend (url-protocol url)))))))

;;;; Server operations

;; -------------------------------------------------------------------
;; Create a new folder named URL.  Signal an error if the folder
;; already exists or can't be created.

(define (create-folder url)
  (%create-folder url))

(define-generic %create-folder (url))

;; -------------------------------------------------------------------
;; Delete the folder named URL.  Signal an error if the folder doesn't
;; exist or if it can't be deleted.

(define (delete-folder url)
  (let ((folder (get-memoized-folder url)))
    (if folder
	(close-folder folder)))
  (unmemoize-folder url)
  (%delete-folder url))

(define-generic %delete-folder (url))

;; -------------------------------------------------------------------
;; Rename the folder named URL to NEW-URL.  Signal an error if the
;; folder doesn't exist, if NEW-URL already refers to a folder, or if
;; the rename can't be performed for some reason.  This operation does
;; NOT do format conversion, or move a folder from one place to
;; another.  It only allows changing the name of an existing folder.

(define (rename-folder url new-url)
  (let ((folder (get-memoized-folder url)))
    (if folder
	(close-folder folder)))
  (unmemoize-folder url)
  (%rename-folder url new-url))

(define-generic %rename-folder (url new-url))

;; -------------------------------------------------------------------
;; Insert a copy of MESSAGE in FOLDER at the end of the existing
;; messages.  Unspecified result.

(define (append-message message url)
  (%append-message message url))

(define-generic %append-message (message url))

;; -------------------------------------------------------------------
;; Keep a connection open to the server referenced by URL for the
;; dynamic extent of THUNK.

(define-generic with-open-connection (url thunk))

;;;; Folder type

(define-class <folder> (<imail-object>)
  (url define accessor)
  (modification-count define standard
		      initial-value 0)
  (modification-event define accessor
		      initializer make-event-distributor))

(define-method write-instance ((folder <folder>) port)
  (write-instance-helper 'FOLDER folder port 
    (lambda ()
      (write-char #\space port)
      (write (url-presentation-name (folder-url folder)) port))))

(define (guarantee-folder folder procedure)
  (if (not (folder? folder))
      (error:wrong-type-argument folder "IMAIL folder" procedure)))

(define (folder-modified! folder type . parameters)
  (without-interrupts
   (lambda ()
     (set-folder-modification-count!
      folder
      (+ (folder-modification-count folder) 1))))
  (apply folder-event folder type parameters))

(define (folder-event folder type . parameters)
  (if *deferred-folder-events*
      (set-cdr! *deferred-folder-events*
		(cons (cons* folder type parameters)
		      (cdr *deferred-folder-events*)))
      (begin
	(if (and imap-trace-port (imap-folder? folder))
	    (begin
	      (write-line (cons* 'FOLDER-EVENT folder type parameters)
			  imap-trace-port)
	      (flush-output imap-trace-port)))
	(event-distributor/invoke! (folder-modification-event folder)
				   folder
				   type
				   parameters))))

(define (with-folder-events-deferred thunk)
  (let ((events (list 'EVENTS)))
    (let ((v
	   (fluid-let ((*deferred-folder-events* events))
	     (thunk))))
      (for-each (lambda (event) (apply folder-event event))
		(reverse! (cdr events)))
      v)))

(define *deferred-folder-events* #f)

(define (get-memoized-folder url)
  (let ((folder (hash-table/get memoized-folders url #f)))
    (and folder
	 (let ((folder (weak-car folder)))
	   (if (and folder (url-exists? url))
	       folder
	       (begin
		 (unmemoize-folder url)
		 #f))))))

(define (memoize-folder folder)
  (hash-table/put! memoized-folders (folder-url folder) (weak-cons folder #f))
  folder)

(define (unmemoize-folder url)
  (hash-table/remove! memoized-folders url))

(define memoized-folders
  (make-eq-hash-table))

;;;; Folder operations

;; -------------------------------------------------------------------
;; Open the folder named URL.

(define (open-folder url)
  (or (get-memoized-folder url)
      (memoize-folder (%open-folder url))))

(define-generic %open-folder (url))

;; -------------------------------------------------------------------
;; Close FOLDER, freeing up connections, memory, etc.  Subsequent use
;; of the folder must work, but may incur a significant time or space
;; penalty.

(define (close-folder folder)
  (save-folder folder)
  (%close-folder folder))

(define-generic %close-folder (folder))

;; -------------------------------------------------------------------
;; Return the number of messages in FOLDER.

(define-generic folder-length (folder))

;; -------------------------------------------------------------------
;; Get the INDEX'th message in FOLDER and return it.  Signal an
;; error for invalid INDEX.

(define (get-message folder index)
  (guarantee-index index 'GET-MESSAGE)
  (if (not (< index (folder-length folder)))
      (error:bad-range-argument index 'GET-MESSAGE))
  (%get-message folder index))

(define-generic %get-message (folder index))

;; -------------------------------------------------------------------
;; Remove all messages in FOLDER that are marked for deletion.
;; Unspecified result.

(define-generic expunge-deleted-messages (folder))

;; -------------------------------------------------------------------
;; Search FOLDER for messages matching CRITERIA.  At present, CRITERIA
;; may be a string.  Returns a list of messages.

(define-generic search-folder (folder criteria))

;; -------------------------------------------------------------------
;; Compare FOLDER's cache with the persistent folder and return a
;; symbol indicating whether they are synchronized, as follows:
;; SYNCHRONIZED CACHE-MODIFIED PERSISTENT-MODIFIED BOTH-MODIFIED
;; PERSISTENT-DELETED UNSYNCHRONIZED

(define-generic folder-sync-status (folder))

;; -------------------------------------------------------------------
;; Save any cached changes made to FOLDER.  Returns a boolean
;; indicating whether anything was saved.

(define-generic save-folder (folder))

;; -------------------------------------------------------------------
;; Discard cached contents of FOLDER.  Subsequent use of FOLDER will
;; reload contents from the persistent folder.

(define-generic discard-folder-cache (folder))

;; -------------------------------------------------------------------
;; Probe FOLDER's server for changes.  Useful as a check for new mail.

(define-generic probe-folder (folder))

;; -------------------------------------------------------------------
;; Return a symbol representing FOLDER's connection status.  The
;; returned value is one of the following symbols:
;; ONLINE	Open connection to the server.
;; OFFLINE	No connection to the server.
;; NO-SERVER	Folder is not server-based.

(define-generic folder-connection-status (folder))

;; -------------------------------------------------------------------
;; Disconnect FOLDER from its associated server.  The folder will
;; automatically reconnect as needed.

(define-generic disconnect-folder (folder))

;; -------------------------------------------------------------------
;; Return #T if FOLDER supports MIME parsing.

(define-generic folder-supports-mime? (folder))

;; -------------------------------------------------------------------
;; Preload outline information about each message in the folder.
;; Normally used prior to generating a folder summary, to accelerate
;; the downloading of this information from the server.  This
;; operation need not be implemented, as it is just a performance
;; enhancement.

(define-generic preload-folder-outlines (folder))

;;;; Message type

(define-class <message> (<imail-object>)
  (header-fields define accessor)
  (flags define accessor)
  (folder define standard
	  initial-value #f)
  (index define standard
	 initial-value #f))

(define-method write-instance ((message <message>) port)
  (write-instance-helper 'MESSAGE message port 
    (lambda ()
      (write-char #\space port)
      (write (message-folder message) port)
      (write-char #\space port)
      (write (message-index message) port))))

(define (guarantee-message message procedure)
  (if (not (message? message))
      (error:wrong-type-argument message "IMAIL message" procedure)))

(define-generic write-message-body (message port))
(define-generic set-message-flags! (message flags))
(define-generic message-internal-time (message))
(define-generic message-length (message))

(define %set-message-flags!
  (let ((modifier (slot-modifier <message> 'FLAGS)))
    (lambda (message flags)
      (modifier message flags)
      (let ((folder (message-folder message)))
	(if folder
	    (folder-modified! folder 'FLAGS message))))))

(define (message-attached? message #!optional folder)
  (let ((folder (if (default-object? folder) #f folder)))
    (if folder
	(eq? folder (message-folder message))
	(message-folder message))))

(define (message-detached? message)
  (not (message-folder message)))

(define (attach-message! message folder index)
  (guarantee-folder folder 'ATTACH-MESSAGE!)
  (without-interrupts
   (lambda ()
     (set-message-folder! message folder)
     (set-message-index! message index))))

(define (detach-message! message)
  (set-message-folder! message #f))

(define (message->string message)
  (with-string-output-port
    (lambda (port)
      (write-header-fields (message-header-fields message) port)
      (write-message-body message port))))

(define (message-time message)
  (let ((date (get-first-header-field-value message "date" #f)))
    (and date
	 (parse-header-field-date date))))

;;;; Message Navigation

(define (first-unseen-message folder)
  (let ((end (folder-length folder)))
    (let loop ((start (first-unseen-message-index folder)))
      (and (< start end)
	   (let ((message (get-message folder start)))
	     (if (message-seen? message)
		 (loop (+ start 1))
		 message))))))

(define-generic first-unseen-message-index (folder))

(define (first-message folder)
  (and (> (folder-length folder) 0)
       (get-message folder 0)))

(define (last-message folder)
  (let ((n (folder-length folder)))
    (and (> n 0)
	 (get-message folder (- n 1)))))

(define (previous-message message #!optional predicate)
  (let ((predicate
	 (if (or (default-object? predicate) (not predicate))
	     (lambda (message) message #t)
	     predicate))
	(folder (message-folder message)))
    (let loop ((index (message-index message)))
      (and (> index 0)
	   (let ((index (- index 1)))
	     (let ((message (get-message folder index)))
	       (if (predicate message)
		   message
		   (loop index))))))))

(define (next-message message #!optional predicate)
  (let ((predicate
	 (if (or (default-object? predicate) (not predicate))
	     (lambda (message) message #t)
	     predicate))
	(folder (message-folder message)))
    (let ((n (folder-length folder)))
      (let loop ((index (message-index message)))
	(let ((index (+ index 1)))
	  (and (< index n)
	       (let ((message (get-message folder index)))
		 (if (predicate message)
		     message
		     (loop index)))))))))

;;;; Message flags

;;; Flags are markers that can be attached to messages.  They indicate
;;; state about the message, such as whether it has been deleted,
;;; seen, etc.  A flag is represented by a string.

(define (message-flagged? message flag)
  (guarantee-message-flag flag 'MESSAGE-FLAGGED?)
  (flags-member? flag (message-flags message)))

(define (set-message-flag message flag)
  (guarantee-message-flag flag 'SET-MESSAGE-FLAG)
  (without-interrupts
   (lambda ()
     (let ((flags (message-flags message)))
       (if (not (flags-member? flag flags))
	   (set-message-flags! message (cons flag flags)))))))

(define (clear-message-flag message flag)
  (guarantee-message-flag flag 'SET-MESSAGE-FLAG)
  (without-interrupts
   (lambda ()
     (let ((flags (message-flags message)))
       (if (flags-member? flag flags)
	   (set-message-flags! message (flags-delete! flag flags)))))))

(define (folder-flags folder)
  (let ((n (folder-length folder)))
    (do ((index 0 (+ index 1))
	 (flags '() (append (message-flags (get-message folder index)) flags)))
	((= index n) (remove-duplicates flags string-ci=?)))))

(define flags-member? (member-procedure string-ci=?))
(define flags-add (add-member-procedure string-ci=?))
(define flags-delete (delete-member-procedure list-deletor string-ci=?))
(define flags-delete! (delete-member-procedure list-deletor! string-ci=?))

(define (message-flag? object)
  (header-field-name? object))

(define (guarantee-message-flag object procedure)
  (if (not (message-flag? object))
      (error:wrong-type-argument object "message flag" procedure)))

(define standard-message-flags
  '("answered" "deleted" "filed" "forwarded" "resent" "seen"))

(define (message-flags->header-field flags)
  (make-header-field message-flags:name
		     (decorated-string-append "" " " "" flags)))

(define (header-field->message-flags header)
  (and (string-ci=? message-flags:name (header-field-name header))
       ;; Extra pair needed to distinguish #F from ().
       (cons #f
	     (burst-string (header-field-value header)
			   char-set:whitespace
			   #t))))

(define message-flags:name "X-IMAIL-FLAGS")

(define (parse-imail-header-fields headers)
  (let loop ((headers headers) (headers* '()) (flags '()))
    (cond ((not (pair? headers))
	   (values (reverse! headers*)
		   (remove-duplicates! (reverse! flags) string-ci=?)))
	  ((header-field->message-flags (car headers))
	   => (lambda (flags*)
		(loop (cdr headers)
		      headers*
		      (append! (reverse! (cdr flags*)) flags))))
	  (else
	   (loop (cdr headers)
		 (cons (car headers) headers*)
		 flags)))))

(define (message-deleted? msg) (message-flagged? msg "deleted"))
(define (message-undeleted? msg) (not (message-flagged? msg "deleted")))
(define (delete-message msg) (set-message-flag msg "deleted"))
(define (undelete-message msg) (clear-message-flag msg "deleted"))

(define (message-answered? msg) (message-flagged? msg "answered"))
(define (message-unanswered? msg) (not (message-flagged? msg "answered")))
(define (message-answered msg) (set-message-flag msg "answered"))
(define (message-not-answered msg) (clear-message-flag msg "answered"))

(define (message-seen? msg) (message-flagged? msg "seen"))
(define (message-unseen? msg) (not (message-flagged? msg "seen")))
(define (message-seen msg) (set-message-flag msg "seen"))
(define (message-not-seen msg) (clear-message-flag msg "seen"))

(define (message-filed? msg) (message-flagged? msg "filed"))
(define (message-unfiled? msg) (not (message-flagged? msg "filed")))
(define (message-filed msg) (set-message-flag msg "filed"))
(define (message-not-filed msg) (clear-message-flag msg "filed"))

(define (message-forwarded? msg) (message-flagged? msg "forwarded"))
(define (message-not-forwarded? msg) (not (message-flagged? msg "forwarded")))
(define (message-forwarded msg) (set-message-flag msg "forwarded"))
(define (message-not-forwarded msg) (clear-message-flag msg "forwarded"))

(define (message-resent? msg) (message-flagged? msg "resent"))
(define (message-not-resent? msg) (not (message-flagged? msg "resent")))
(define (message-resent msg) (set-message-flag msg "resent"))
(define (message-not-resent msg) (clear-message-flag msg "resent"))

;;;; Header fields

(define-structure (header-field
		   (type-descriptor header-field-rtd)
		   (safe-accessors #t)
		   (constructor #f)
		   (print-procedure
		    (standard-unparser-method 'HEADER-FIELD
		      (lambda (header port)
			(write-char #\space port)
			(write (header-field-name header) port)))))
  (name #f read-only #t)
  (value #f read-only #t))

(define make-header-field
  (let ((constructor (record-constructor header-field-rtd)))
    (lambda (name value)
      (guarantee-header-field-name name 'MAKE-HEADER-FIELD)
      (constructor name value))))

(define (guarantee-header-field-name object procedure)
  (if (not (header-field-name? object))
      (error:wrong-type-argument object "header-field name" procedure)))

(define (header-field-name? object)
  (and (string? object)
       (rfc822:header-field-name? object 0 (string-length object))))

(define (copy-header-field header)
  (record-copy header))

(define (->header-fields object)
  (cond ((or (pair? object) (null? object)) object)
	((message? object) (message-header-fields object))
	((string? object) (string->header-fields object))
	(else (error:wrong-type-argument object "header fields" #f))))

(define (encode-header-fields headers receiver)
  (for-each (lambda (header) (encode-header-field header receiver)) headers)
  (receiver "\n" 0 1))

(define (encode-header-field header receiver)
  (let ((name (header-field-name header)))
    (receiver name 0 (string-length name)))
  (receiver ": " 0 2)
  (encode-header-field-value (header-field-value header) receiver)
  (receiver "\n" 0 1))

(define (encode-header-field-value value receiver)
  (let ((end (string-length value)))
    (let loop ((start 0))
      (let ((index (substring-find-next-char value start end #\newline)))
	(if index
	    (let ((index (fix:+ index 1)))
	      (receiver value start index)
	      (receiver "\t" 0 1)
	      (loop index))
	    (receiver value start end))))))

(define (header-field-length header)
  (let ((value (header-field-value header)))
    (+ (string-length (header-field-name header))
       (string-length value)
       (string-n-newlines value)
       3)))

(define (write-header-fields headers port)
  (encode-header-fields headers
    (lambda (string start end)
      (write-substring string start end port))))

(define (write-header-field header port)
  (encode-header-field header
    (lambda (string start end)
      (write-substring string start end port))))

(define (header-fields->string headers)
  (with-string-output-port
    (lambda (port)
      (write-header-fields headers port))))

(define (header-field->string header)
  (with-string-output-port
    (lambda (port)
      (write-header-field header port))))

(define (header-field-value->string value)
  (with-string-output-port
    (lambda (port)
      (encode-header-field-value value
	(lambda (string start end)
	  (write-substring string start end port))))))

(define (get-first-header-field headers name error?)
  (let loop ((headers (->header-fields headers)))
    (cond ((pair? headers)
	   (if (string-ci=? name (header-field-name (car headers)))
	       (car headers)
	       (loop (cdr headers))))
	  (error? (error:bad-range-argument name 'GET-FIRST-HEADER-FIELD))
	  (else #f))))

(define (get-last-header-field headers name error?)
  (let loop ((headers (->header-fields headers)) (winner #f))
    (cond ((pair? headers)
	   (loop (cdr headers)
		 (if (string-ci=? name (header-field-name (car headers)))
		     (car headers)
		     winner)))
	  ((and (not winner) error?)
	   (error:bad-range-argument name 'GET-LAST-HEADER-FIELD))
	  (else winner))))

(define (get-all-header-fields headers name)
  (list-transform-positive (->header-fields headers)
    (lambda (header)
      (string-ci=? name (header-field-name header)))))

(define (get-first-header-field-value headers name error?)
  (let ((header (get-first-header-field headers name error?)))
    (and header
	 (header-field-value header))))

(define (get-last-header-field-value headers name error?)
  (let ((header (get-last-header-field headers name error?)))
    (and header
	 (header-field-value header))))

(define (get-all-header-field-values headers name)
  (map header-field-value (get-all-header-fields headers name)))

(define (string->header-fields string)
  (lines->header-fields (string->lines string)))

(define (lines->header-fields lines)
  (let find-initial ((lines lines) (headers '()))
    (cond ((or (not (pair? lines))
	       (string-null? (car lines)))
	   (reverse! headers))
	  ((header-field-initial-line? (car lines))
	   (let collect-group ((lines (cdr lines)) (group (list (car lines))))
	     (if (or (not (pair? lines))
		     (string-null? (car lines))
		     (header-field-initial-line? (car lines)))
		 (find-initial
		  lines
		  (cons
		   (let ((lines (reverse! group)))
		     (let ((colon
			    (and (pair? lines)
				 (string-find-next-char (car lines) #\:))))
		       (if (not colon)
			   (error "Malformed header-field lines:" lines))
		       (make-header-field
			(string-head (car lines) colon)
			(decorated-string-append
			 "" "\n" ""
			 (map string-trim
			      (cons (string-tail (car lines) (fix:+ colon 1))
				    (cdr lines)))))))
		   headers))
		 (collect-group (cdr lines) (cons (car lines) group)))))
	  (else
	   (find-initial (cdr lines) headers)))))

(define (header-field-initial-line? line)
  (let ((colon (string-find-next-char line #\:)))
    (and colon
	 (rfc822:header-field-name? line 0 colon))))

;;;; MIME structure

(define-generic mime-message-body-structure (message))
(define-generic write-mime-message-body-part (message selector cache? port))

(define-class <mime-body> (<imail-object>)
  (parameters define accessor)
  (disposition define accessor)
  (language define accessor)
  (enclosure define standard initial-value #f))

(define-generic mime-body-type (body))
(define-generic mime-body-subtype (body))

(define (mime-body-type-string body)
  (string-append (symbol->string (mime-body-type body))
		 "/"
		 (symbol->string (mime-body-subtype body))))

(define (mime-body-parameter body key default)
  (let ((entry (assq key (mime-body-parameters body))))
    (if entry
	(cdr entry)
	default)))

(define (mime-body-disposition-filename body)
  (let ((disposition (mime-body-disposition body)))
    (and disposition
	 (let ((entry (assq 'FILENAME (cdr disposition))))
	   (and entry
		(cdr entry))))))

(define-method write-instance ((body <mime-body>) port)
  (write-instance-helper 'MIME-BODY body port 
    (lambda ()
      (write-char #\space port)
      (write-string (mime-body-type-string body) port))))

(define (mime-body-enclosed? b1 b2)
  (or (eq? b1 b2)
      (let ((enclosure (mime-body-enclosure b1)))
	(and enclosure
	     (mime-body-enclosed? enclosure b2)))))

(define-class <mime-body-one-part> (<mime-body>)
  (id define accessor)
  (description define accessor)
  (encoding define accessor)
  (n-octets define accessor)
  (md5 define accessor))

(define-class (<mime-body-message>
	       (constructor (parameters id description encoding n-octets
					envelope body n-lines
					md5 disposition language)))
    (<mime-body-one-part>)
  (envelope define accessor)		;<mime-envelope> instance
  (body define accessor)		;<mime-body> instance
  (n-lines define accessor))

(define-method mime-body-type ((body <mime-body-message>)) body 'MESSAGE)
(define-method mime-body-subtype ((body <mime-body-message>)) body 'RFC822)

(define-class (<mime-body-text>
	       (constructor (subtype parameters id description encoding
				     n-octets n-lines
				     md5 disposition language)))
    (<mime-body-one-part>)
  (subtype accessor mime-body-subtype)
  (n-lines define accessor))

(define-method mime-body-type ((body <mime-body-text>)) body 'TEXT)

(define-class (<mime-body-basic>
	       (constructor (type subtype parameters id description encoding
				  n-octets md5 disposition language)))
    (<mime-body-one-part>)
  (type accessor mime-body-type)
  (subtype accessor mime-body-subtype))

(define-class (<mime-body-multipart>
	       (constructor (subtype parameters parts disposition language)))
    (<mime-body>)
  (subtype accessor mime-body-subtype)
  (parts define accessor))

(define-method mime-body-type ((body <mime-body-multipart>)) body 'MULTIPART)

(define-class (<mime-envelope>
	       (constructor (date subject from sender reply-to to cc bcc
				  in-reply-to message-id)))
    ()
  (date define accessor)
  (subject define accessor)
  (from define accessor)
  (sender define accessor)
  (reply-to define accessor)
  (to define accessor)
  (cc define accessor)
  (bcc define accessor)
  (in-reply-to define accessor)
  (message-id define accessor))

(define-class (<mime-address> (constructor (name source-route mailbox host)))
    ()
  (name define accessor)
  (source-route define accessor)
  (mailbox define accessor)
  (host define accessor))