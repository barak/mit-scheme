;;; -*-Scheme-*-
;;;
;;; $Id: imail-core.scm,v 1.33 2000/04/28 18:43:53 cph Exp $
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

;;;; IMAIL mail reader: core definitions

;;; **** Implement file backup.

(declare (usual-integrations))

;;;; URL type

(define-class <url> ())

;; Return the canonical name of URL's protocol as a string.
(define-generic url-protocol (url))

;; Return the body of URL as a string.
(define-generic url-body (url))

(define (guarantee-url url procedure)
  (if (not (url? url))
      (error:wrong-type-argument url "IMAIL url" procedure)))

(define-method write-instance ((url <url>) port)
  (write-instance-helper 'URL url port
    (lambda ()
      (write-char #\space port)
      (write (url->string url) port))))

(define (make-url protocol body)
  (string->url (string-append protocol ":" body)))

(define-generic ->url (object))
(define-method ->url ((url <url>)) url)
(define-method ->url ((string <string>)) (string->url string))

(define (string->url string)
  (or (hash-table/get saved-urls string #f)
      (let ((url
	     (let ((colon (string-find-next-char string #\:)))
	       (if (not colon)
		   (error "Malformed URL string:" string))
	       ((get-url-protocol-parser (string-head string colon))
		(string-tail string (fix:+ colon 1))))))
	(hash-table/put! saved-urls string url)
	url)))

(define saved-urls
  (make-string-hash-table))

(define (url->string url)
  (string-append (url-protocol url) ":" (url-body url)))

(define (define-url-protocol name class parser)
  (define-method url-protocol ((url class)) url name)
  (hash-table/put! url-protocol-parsers (string-downcase name) parser))

(define (get-url-protocol-parser name)
  (let ((parser
	 (hash-table/get url-protocol-parsers (string-downcase name) #f)))
    (if (not parser)
	(error:bad-range-argument name 'GET-URL-PROTOCOL-PARSER))
    parser))

(define url-protocol-parsers
  (make-string-hash-table))

(define-generic url-user-id (url))
(define-method url-user-id ((url <url>)) url #f)

(define (get-memoized-folder url)
  (let ((folder (hash-table/get memoized-folders url #f)))
    (and folder
	 (let ((folder (weak-car folder)))
	   (if (and folder (%folder-valid? folder))
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

;;;; Server operations

;;; In "online" mode, these server operations directly modify the
;;; server's state.

;;; In "disconnected" mode, server operations don't interact with the
;;; server, but instead manipulate locally-cached copies of folders
;;; that reside on the server.  The operations are recorded and saved
;;; in the file system, then played back when SYNCHRONIZE-FOLDER is
;;; called.  In this mode, SYNCHRONIZE-FOLDER and POLL-FOLDER are the
;;; only operations that interact with the server.

;;; [**** Note that SYNCHRONIZE-FOLDER is insufficient to properly
;;; implement "disconnected" mode.  The client must also know how to
;;; enumerate the server's folder set, so that it can tell whether a
;;; given cached folder has been deleted or renamed on the server.
;;; Similarly, the SYNCHRONIZE-FOLDER operation must be able to tell
;;; the client that the folder being synchronized has been deleted or
;;; renamed, so that the client can take appropriate action.]

;; Open the folder named URL.
(define (open-folder url)
  (let ((url (->url url)))
    (or (get-memoized-folder url)
	(memoize-folder (%open-folder url)))))

(define-generic %open-folder (url))

;; Create a new folder named URL.  Signal an error if the folder
;; already exists or can't be created.
(define (new-folder url)
  (let ((url (->url url)))
    (if (get-memoized-folder url)
	(error "Folder already exists:" url)
	(memoize-folder (%new-folder url)))))

(define-generic %new-folder (url))

;; Delete the folder named URL.  Signal an error if the folder doesn't
;; exist or if it can't be deleted.
(define (delete-folder url)
  (let ((url (->url url)))
    (unmemoize-folder url)
    (%delete-folder url)))

(define-generic %delete-folder (url))

;; Move the folder named URL to NEW-URL.  Signal an error if the
;; folder doesn't exist, if NEW-URL already refers to a folder, or if
;; the move can't be performed for some reason.  This operation can be
;; also be used to convert between protocols, e.g. to move a folder
;; from a server to a file.
(define (move-folder url new-url)
  (let ((url (->url url))
	(new-url (->url new-url)))
    (unmemoize-folder url)
    (%move-folder url new-url)))

(define-generic %move-folder (url new-url))

(define-method %move-folder ((url <url>) (new-url <url>))
  (%copy-folder url new-url)
  (%delete-folder url))

;; Copy the folder named URL to be NEW-URL.  Signal an error if the
;; folder doesn't exist, if NEW-URL already refers to a folder, or if
;; the copy can't be performed for some reason.
(define (copy-folder url new-url)
  (%copy-folder (->url url) (->url new-url)))

(define-generic %copy-folder (url new-url))

(define-method %copy-folder ((url <url>) (new-url <url>))
  (%write-folder (open-folder url) new-url))

;; Return a list of URLs for folders that match URL-PATTERN.
;; URL-PATTERN can contain wildcards.
(define-generic available-folder-names (url-pattern))

;; [This is an IMAP command that appears to be designed to support
;; delivery of usenet news.]
(define-generic subscribed-folder-names (url-pattern))

;; Define AUTHENTICATOR to be the authenticator to use in the dynamic
;; extent of THUNK.

;; AUTHENTICATOR is a procedure that performs authentication, for
;; protocols that require it.  AUTHENTICATOR is called with URL as its
;; only argument and must return the authentication information,
;; usually a username/password, as multiple values.

;; For protocols that don't require authentication, AUTHENTICATOR is
;; not called, and BIND-AUTHENTICATOR need not be used.

;; [AUTHENTICATOR can be called at a variety of times; these will be
;; made more explicit when known.]

(define (bind-authenticator authenticator thunk)
  (fluid-let ((authenticate authenticator))
    (thunk)))

(define authenticate)

;;;; Folder type

(define-class <folder> ()
  (modified? define standard
	     initial-value #t)
  (modification-event define accessor
		      initial-value (make-event-distributor))
  (properties define standard
	      initializer make-1d-table))

(define-method write-instance ((folder <folder>) port)
  (write-instance-helper 'FOLDER folder port 
    (lambda ()
      (write-char #\space port)
      (write (url->string (folder-url folder)) port))))

(define (guarantee-folder folder procedure)
  (if (not (folder? folder))
      (error:wrong-type-argument folder "IMAIL folder" procedure)))

(define (folder-get folder key default)
  (1d-table/get (folder-properties folder) key default))

(define (folder-put! folder key datum)
  (1d-table/put! (folder-properties folder) key datum))

(define (folder-remove! folder key)
  (1d-table/remove! (folder-properties folder) key))

;; Return the URL of FOLDER.
(define-generic folder-url (folder))

(define-method ->url ((folder <folder>))
  (folder-url folder))

;; Return #T if FOLDER represents a real folder, i.e. has a
;; corresponding file or server entry.
(define (folder-valid? folder)
  (eq? folder (get-memoized-folder (folder-url folder))))

(define-generic %folder-valid? (folder))

;; Return the number of messages in FOLDER.
(define-generic folder-length (folder))

(define (folder-modified! folder)
  (if (not (folder-modified? folder))
      (begin
	(set-folder-modified?! folder #t)
	(event-distributor/invoke! (folder-modification-event folder)
				   folder))))

(define (folder-not-modified! folder)
  (if (folder-modified? folder)
      (begin
	(let ((count (folder-length folder)))
	  (do ((index 0 (+ index 1)))
	      ((= index count))
	    (message-not-modified! (get-message folder index))))
	(set-folder-modified?! folder #f)
	(event-distributor/invoke! (folder-modification-event folder)
				   folder))))

;; Get the INDEX'th message in FOLDER and return it.  Signal an
;; error for invalid INDEX.
(define (get-message folder index)
  (guarantee-index index 'GET-MESSAGE)
  (if (not (< index (folder-length folder)))
      (error:bad-range-argument index 'GET-MESSAGE))
  (%get-message folder index))

(define-generic %get-message (folder index))

;;; Insert a copy of MESSAGE in FOLDER at the end of the existing
;;; messages.  Unspecified result.
(define (append-message folder message)
  (guarantee-message message 'APPEND-MESSAGE)
  (%append-message folder message))

(define-generic %append-message (folder message))

;; Remove all messages in FOLDER that are marked for deletion.
;; Unspecified result.
(define-generic expunge-deleted-messages (folder))

;; Search FOLDER for messages matching CRITERIA, returning them in a
;; list.  [Possible values for CRITERIA not yet defined.]  Returns a
;; list of messages.
(define-generic search-folder (folder criteria))

;; Poll the inbox associated with FOLDER to see if there is new mail.
;; If so, the mail is appended to FOLDER.  Return the number of new
;; messages.  Return #F if FOLDER has no associated inbox.
(define-generic poll-folder (folder))

;; Synchronize the local copy of FOLDER with the server's copy.
;; Unspecified result.
(define-generic synchronize-folder (folder))

;; Save any changes made to FOLDER.  This permits the use of caches
;; for improved performance.
(define (save-folder folder)
  (%save-folder folder))

(define-generic %save-folder (folder))

(define-method %save-folder ((folder <folder>))
  (%write-folder folder (folder-url folder)))

;; Check to see if the persistent copy of FOLDER has changed since it
;; was copied into memory, and update the memory copy if so.  Return
;; #t if the memory copy is updated, #f if it is not.  If both
;; the memory copy and the persistent copy have changed, the procedure
;; RESOLVE-CONFLICT is called with the folder as an argument.
;; RESOLVE-CONFLICT must return a boolean which if true indicates that
;; the folder should be reverted.
(define (maybe-revert-folder folder resolve-conflict)
  (%maybe-revert-folder folder resolve-conflict))

(define-generic %maybe-revert-folder (folder resolve-conflict))
(define-generic %revert-folder (folder))

;; Write the contents of FOLDER to URL.
(define (write-folder folder url)
  (%write-folder folder (->url url)))

(define-generic %write-folder (folder url))

;; [These are IMAP commands that appear to be designed to support
;; delivery of usenet news.]
(define-generic subscribe-folder (folder))
(define-generic unsubscribe-folder (folder))

;;;; Message type

(define-class (<message> (constructor (header-fields body flags properties)))
    ()
  (header-fields define accessor)
  (body define accessor)
  (flags define standard)
  (modified? define standard
	     initial-value #t)
  (properties define standard)
  (folder define standard
	  initial-value #f)
  (index define standard))

(define-method write-instance ((message <message>) port)
  (write-instance-helper 'MESSAGE message port 
    (lambda ()
      (if (message-folder message)
	  (begin
	    (write-char #\space port)
	    (write (message-folder message) port))))))

(define (guarantee-message message procedure)
  (if (not (message? message))
      (error:wrong-type-argument message "IMAIL message" procedure)))

(define (make-detached-message headers body)
  (let loop ((headers headers) (headers* '()) (flags '()) (properties '()))
    (cond ((not (pair? headers))
	   (make-message (reverse! headers*)
			 body
			 (reverse! flags)
			 (reverse! properties)))
	  ((header-field->message-flags (car headers))
	   => (lambda (flags*)
		(loop (cdr headers)
		      headers*
		      (append! (reverse! (cdr flags*)) flags)
		      properties)))
	  ((header-field->message-property (car headers))
	   => (lambda (property)
		(loop (cdr headers)
		      headers*
		      flags
		      (cons property properties))))
	  (else
	   (loop (cdr headers)
		 (cons (car headers) headers*)
		 flags
		 properties)))))

(define (attach-message message folder)
  (guarantee-folder folder 'ATTACH-MESSAGE)
  (let ((message
	 (make-message (map copy-header-field (message-header-fields message))
		       (message-body message)
		       (list-copy (message-flags message))
		       (alist-copy (message-properties message)))))
    (set-message-folder! message folder)
    message))

(define (detach-message message)
  (set-message-folder! message #f)
  (set-message-index! message #f))

(define (make-attached-message folder headers body)
  (let ((message (make-detached-message headers body)))
    (set-message-folder! message folder)
    message))

(define (message-modified! message)
  (without-interrupts
   (lambda ()
     (set-message-modified?! message #t)
     (let ((folder (message-folder message)))
       (if folder
	   (folder-modified! folder))))))

(define (message-not-modified! message)
  (set-message-modified?! message #f))

(define (maybe-strip-imail-headers strip? headers)
  (if strip?
      (list-transform-negative headers
	(lambda (header)
	  (or (header-field->message-flags header)
	      (header-field->message-property header))))
      headers))

(define (message->string message)
  (string-append (header-fields->string (message-header-fields message))
		 "\n"
		 (message-body message)))

;;;; Message Navigation

(define-generic first-unseen-message (folder))
(define-method first-unseen-message ((folder <folder>))
  (let ((message (first-message folder)))
    (and message
	 (let loop ((message message))
	   (let ((next (next-message message)))
	     (cond ((not next) message)
		   ((message-seen? next) (loop next))
		   (else next)))))))

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
  (let ((flags (message-flags message)))
    (if (not (flags-member? flag flags))
	(set-message-flags! message (cons flag flags))))
  (message-modified! message))

(define (clear-message-flag message flag)
  (guarantee-message-flag flag 'SET-MESSAGE-FLAG)
  (flags-delete! flag (message-flags message))
  (message-modified! message))

(define (folder-flags folder)
  (let ((n (folder-length folder)))
    (do ((index 0 (+ index 1))
	 (flags '() (append (message-flags (get-message folder index)) flags)))
	((= index n)
	 (remove-duplicates flags string-ci=?)))))

(define flags-member? (member-procedure string-ci=?))
(define flags-delete! (delete-member-procedure list-deletor! string-ci=?))

(define (message-flag? object)
  (header-field-name? object))

(define (guarantee-message-flag object procedure)
  (if (not (message-flag? object))
      (error:wrong-type-argument object "message flag" procedure)))

(define standard-message-flags
  '("answered" "deleted" "edited" "filed" "forwarded" "resent" "seen"))

(define (message-flags->header-field flags)
  (make-header-field message-flags:name
		     (decorated-string-append "" " " "" flags)))

(define (header-field->message-flags header)
  (and (string-ci=? message-flags:name (header-field-name header))
       ;; Extra pair needed to distinguish #F from ().
       (cons #f (burst-string (header-field-value header) char-set:lwsp #t))))

(define message-flags:name "X-IMAIL-FLAGS")

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

(define (message-edited? msg) (message-flagged? msg "edited"))
(define (message-unedited? msg) (not (message-flagged? msg "edited")))
(define (message-edited msg) (set-message-flag msg "edited"))
(define (message-not-edited msg) (clear-message-flag msg "edited"))

(define (message-resent? msg) (message-flagged? msg "resent"))
(define (message-not-resent? msg) (not (message-flagged? msg "resent")))
(define (message-resent msg) (set-message-flag msg "resent"))
(define (message-not-resent msg) (clear-message-flag msg "resent"))

;;;; Message properties

;;; Properties are used to associate information with a message.  A
;;; property is a distinguished header field that carries information
;;; intended for the mail reader rather than the user.

(define (get-message-property message name default)
  (guarantee-message-property-name name 'GET-MESSAGE-PROPERTY)
  (let loop ((headers (message-properties message)))
    (if (pair? headers)
	(if (string-ci=? name (caar headers))
	    (cdar headers)
	    (loop (cdr headers)))
	default)))

(define (set-message-property message name value)
  (guarantee-message-property-name name 'SET-MESSAGE-PROPERTY)
  (guarantee-message-property-value value 'SET-MESSAGE-PROPERTY)
  (let ((alist (message-properties message)))
    (let loop ((alist* alist))
      (if (pair? alist*)
	  (if (string-ci=? name (caar alist*))
	      (set-cdr! (car alist*) value)
	      (loop (cdr alist*)))
	  (set-message-properties! message
				   (cons (cons name value) alist)))))
  (message-modified! message))

(define (message-property-name? object)
  (header-field-name? object))

(define (message-property-value? object)
  (or (header-field-value? object)
      (and (list? object)
	   (for-all? object header-field?))))

(define (guarantee-message-property-name name procedure)
  (if (not (message-property-name? name))
      (error:wrong-type-argument name "message-property name" procedure)))

(define (guarantee-message-property-value value procedure)
  (if (not (message-property-value? value))
      (error:wrong-type-argument value "message-property value" procedure)))

(define (message-property->header-field name value)
  (make-header-field
   (string-append message-property:prefix name)
   (if (header-field-value? value)
       (string-append message-property:string-marker value)
       (apply string-append
	      message-property:headers-marker
	      (map (lambda (line)
		     (string-append "\n" line))
		   (quote-lines
		    (append-map (lambda (header)
				  (header-field->lines header))
				value)))))))

(define (header-field->message-property header)
  (and (string-prefix-ci? message-property:prefix (header-field-name header))
       (cons (string-tail (header-field-name header)
			  (string-length message-property:prefix))
	     (let ((value (header-field-value header)))
	       (cond ((string-prefix? message-property:string-marker value)
		      (string-tail
		       value
		       (string-length message-property:string-marker)))
		     ((string-prefix? message-property:headers-marker value)
		      (lines->header-fields
		       (unquote-lines
			(cdr (burst-string value #\newline #f)))))
		     (else
		      (error "Malformed message-property value:" value)))))))

(define message-property:prefix "X-IMAIL-PROPERTY-")
(define message-property:string-marker "[string]")
(define message-property:headers-marker "[headers]")

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
      (guarantee-header-field-value value 'MAKE-HEADER-FIELD)
      (constructor name value))))

(define (copy-header-field header)
  (record-copy header))

(define (->header-fields object)
  (cond ((or (pair? object) (null? object)) object)
	((message? object) (message-header-fields object))
	(else (error:wrong-type-argument object "header fields" #f))))

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
	 (string-trim (header-field-value header)))))

(define (get-last-header-field-value headers name error?)
  (let ((header (get-last-header-field headers name error?)))
    (and header
	 (string-trim (header-field-value header)))))

(define (get-all-header-field-values headers name)
  (let ((headers (get-all-header-fields headers name)))
    (and (pair? headers)
	 (decorated-string-append
	  "" ", " ""
	  (map (lambda (header)
		 (string-trim (header-field-value header)))
	       headers)))))

(define (header-field-name? object)
  (and (string? object)
       (%header-field-name? object 0 (string-length object))))

(define %header-field-name?
  (let ((excluded-chars
	 (char-set-invert
	  (char-set-difference (ascii-range->char-set 33 127)
			       (char-set #\:)))))
    (lambda (string start end)
      (and (fix:< start end)
	   (not (substring-find-next-char-in-set string start end
						 excluded-chars))))))

(define (header-field-value? object)
  (and (string? object)
       (let ((end (string-length object)))
	 (let loop ((index 0))
	   (let ((nl (substring-find-next-char object index end #\newline)))
	     (or (not nl)
		 (and (fix:< (fix:+ nl 1) end)
		      (char-lwsp? (string-ref object (fix:+ nl 1)))
		      (loop (fix:+ nl 2)))))))))

(define (guarantee-header-field-name object procedure)
  (if (not (header-field-name? object))
      (error:wrong-type-argument object "header-field name" procedure)))

(define (guarantee-header-field-value object procedure)
  (if (not (header-field-value? object))
      (error:wrong-type-argument object "header-field value" procedure)))

(define (header-field->lines header)
  (let ((lines (string->lines (header-field-value header))))
    (cons (string-append (header-field-name header) ":" (car lines))
	  (cdr lines))))

(define (lines->header-field lines)
  (let ((colon
	 (and (pair? lines)
	      (string-find-next-char (car lines) #\:))))
    (if (not colon)
	(error "Malformed header-field lines:" lines))
    (make-header-field (string-head (car lines) colon)
		       (apply string-append
			      (string-tail (car lines) (fix:+ colon 1))
			      (map (lambda (line)
				     (string-append "\n" line))
				   (cdr lines))))))

(define (header-fields->lines headers)
  (append-map! header-field->lines headers))

(define (lines->header-fields lines)
  (map lines->header-field (burst-list lines header-field-initial-line?)))

(define (header-field-initial-line? line)
  (let ((colon (string-find-next-char line #\:)))
    (and colon
	 (%header-field-name? line 0 colon))))

(define (header-field-continuation-line? line)
  (and (not (string-null? line))
       (char-lwsp? (string-ref line 0))))

(define (string->header-fields string)
  (lines->header-fields (string->lines string)))

(define (header-fields->string headers)
  (lines->string (header-fields->lines headers)))