;;; -*-Scheme-*-
;;;
;;; $Id: imail-core.scm,v 1.72 2000/05/19 03:20:46 cph Exp $
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

;; Return the canonical name of URL's protocol as a string.
(define-generic url-protocol (url))

;; Return the body of URL as a string.
(define-generic url-body (url))

(define (guarantee-url url procedure)
  (if (not (url? url))
      (error:wrong-type-argument url "IMAIL URL" procedure)))

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
		   (error:bad-range-argument string 'STRING->URL))
	       ((get-url-protocol-parser (string-head string colon))
		(string-tail string (fix:+ colon 1))))))
	(hash-table/put! saved-urls string url)
	url)))

(define (save-url url)
  (let ((string (url->string url)))
    (or (hash-table/get saved-urls string #f)
	(begin
	  (hash-table/put! saved-urls string url)
	  url))))

(define saved-urls
  (make-string-hash-table))

(define (url->string url)
  (string-append (url-protocol url) ":" (url-body url)))

(define (define-url-protocol name class parser)
  (define-method url-protocol ((url class)) url name)
  (hash-table/put! url-protocol-parsers (string-downcase name) parser))

(define (get-url-protocol-parser name)
  (or (hash-table/get url-protocol-parsers (string-downcase name) #f)
      (error:bad-range-argument name 'GET-URL-PROTOCOL-PARSER)))

(define url-protocol-parsers
  (make-string-hash-table))

;; Return a string that concisely identifies URL, for use in the
;; presentation layer.
(define-generic url-presentation-name (url))

;;;; Server operations

;; -------------------------------------------------------------------
;; Create a new folder named URL.  Signal an error if the folder
;; already exists or can't be created.

(define (create-folder url)
  (%create-folder (->url url)))

(define-generic %create-folder (url))

;; -------------------------------------------------------------------
;; Delete the folder named URL.  Signal an error if the folder doesn't
;; exist or if it can't be deleted.

(define (delete-folder url)
  (let ((url (->url url)))
    (let ((folder (get-memoized-folder url)))
      (if folder
	  (close-folder folder)))
    (unmemoize-folder url)
    (%delete-folder url)))

(define-generic %delete-folder (url))

;; -------------------------------------------------------------------
;; Rename the folder named URL to NEW-URL.  Signal an error if the
;; folder doesn't exist, if NEW-URL already refers to a folder, or if
;; the rename can't be performed for some reason.  This operation does
;; NOT do format conversion, or move a folder from one place to
;; another.  It only allows changing the name of an existing folder.

(define (rename-folder url new-url)
  (let ((url (->url url))
	(new-url (->url new-url)))
    (let ((folder (get-memoized-folder url)))
      (if folder
	  (close-folder folder)))
    (unmemoize-folder url)
    (%rename-folder url new-url)))

(define-generic %rename-folder (url new-url))

;; -------------------------------------------------------------------
;; Insert a copy of MESSAGE in FOLDER at the end of the existing
;; messages.  Unspecified result.

(define (append-message message url)
  (%append-message message (->url url)))

(define-generic %append-message (message url))

;; -------------------------------------------------------------------
;; Return a list of URLs for folders that match URL-PATTERN.
;; URL-PATTERN can contain wildcards.

(define-generic available-folder-names (url-pattern))

;; -------------------------------------------------------------------
;; Define AUTHENTICATOR to be the authenticator to use in the dynamic
;; extent of THUNK.

;; AUTHENTICATOR is a procedure that performs authentication, for
;; protocols that require it.  AUTHENTICATOR is called with a host
;; name, a user ID, and a procedure as its arguments.  It invokes the
;; procedure on a single argument, the password.  The AUTHENTICATOR
;; may wipe the password string on the procedure's return, if desired.

;; For protocols that don't require authentication, AUTHENTICATOR is
;; not called, and BIND-AUTHENTICATOR need not be used.

;; [AUTHENTICATOR can be called at a variety of times; these will be
;; made more explicit when known.]

(define (bind-authenticator authenticator thunk)
  (fluid-let ((authenticate authenticator))
    (thunk)))

(define authenticate)

;;;; Folder type

(define-class <folder> (<imail-object>)
  (url define accessor)
  (modification-count define standard
		      initial-value 0)
  (modification-event define accessor
		      initial-value (make-event-distributor)))

(define-method write-instance ((folder <folder>) port)
  (write-instance-helper 'FOLDER folder port 
    (lambda ()
      (write-char #\space port)
      (write (url-presentation-name (folder-url folder)) port))))

(define (guarantee-folder folder procedure)
  (if (not (folder? folder))
      (error:wrong-type-argument folder "IMAIL folder" procedure)))

(define-method ->url ((folder <folder>))
  (folder-url folder))

(define (folder-modified! folder type . parameters)
  (without-interrupts
   (lambda ()
     (set-folder-modification-count!
      folder
      (+ (folder-modification-count folder) 1))))
  (apply folder-event folder type parameters))

(define (folder-event folder type . parameters)
  (event-distributor/invoke! (folder-modification-event folder)
			     folder
			     type
			     parameters))

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

;;;; Folder operations

;; -------------------------------------------------------------------
;; Open the folder named URL.

(define (open-folder url)
  (let ((url (->url url)))
    (or (get-memoized-folder url)
	(memoize-folder (%open-folder url)))))

(define-generic %open-folder (url))

;; -------------------------------------------------------------------
;; Close FOLDER, freeing up connections, memory, etc.  Subsequent use
;; of the folder must work, but may incur a significant time or space
;; penalty.

(define-generic close-folder (folder))

;; -------------------------------------------------------------------
;; Return #T if FOLDER represents a real folder, i.e. has a
;; corresponding file or server entry.

(define (folder-valid? folder)
  (eq? folder (get-memoized-folder (folder-url folder))))

(define-generic %folder-valid? (folder))

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
;; Search FOLDER for messages matching CRITERIA.  [Possible values for
;; CRITERIA not yet defined.]  Returns a list of messages.

(define-generic search-folder (folder criteria))

;; -------------------------------------------------------------------
;; Compare FOLDER's cache with the persistent folder and return a
;; symbol indicating whether they are synchronized, as follows:
;; SYNCHRONIZED FOLDER-MODIFIED PERSISTENT-MODIFIED BOTH-MODIFIED
;; PERSISTENT-DELETED UNSYNCHRONIZED

(define-generic folder-sync-status (folder))

;; -------------------------------------------------------------------
;; Save any cached changes made to FOLDER.

(define-generic save-folder (folder))

;; -------------------------------------------------------------------
;; Discard cached contents of FOLDER.  Subsequent use of FOLDER will
;; reload contents from the persistent folder.

(define-generic discard-folder-cache (folder))

;; -------------------------------------------------------------------
;; Probe FOLDER's server for changes.  Useful as a check for new mail.

(define-generic probe-folder (folder))

;;;; Message type

(define-class (<message> (constructor (header-fields body flags)))
    (<imail-object>)
  (header-fields define accessor)
  (body define accessor)
  (flags define standard
	 modifier %set-message-flags!)
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
  (string-append (header-fields->string (message-header-fields message))
		 "\n"
		 (message-body message)))

(define-generic message-internal-time (message))
(define-method message-internal-time ((message <message>))
  (let loop ((headers (get-all-header-fields message "received")) (winner #f))
    (if (pair? headers)
	(call-with-values
	    (lambda ()
	      (rfc822:received-header-components
	       (header-field-value (car headers))))
	  (lambda (from by via with id for time)
	    from by via with id for	;ignored
	    (loop (cdr headers)
		  (if (or (not winner) (< time winner)) time winner))))
	(or winner
	    (message-time message)))))

(define (message-time message)
  (let ((date (get-first-header-field-value message "date" #f)))
    (and date
	 (let ((t
		(ignore-errors
		 (lambda ()
		   (string->universal-time
		    (rfc822:tokens->string
		     (rfc822:strip-comments (rfc822:string->tokens date))))))))
	   (and (not (condition? t))
		t)))))

(define-generic message-length (message))
(define-method message-length ((message <message>))
  (+ (reduce (lambda (header)
	       (+ (string-length (header-field-name header))
		  (string-length (header-field-value header))
		  2))
	     1
	     (message-header-fields message))
     (string-length (message-body message))))

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
(define-method first-unseen-message-index ((folder <folder>))
  folder
  0)

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

(define (set-message-flags! message flags)
  (%set-message-flags! message flags)
  (let ((folder (message-folder message)))
    (if folder
	(folder-modified! folder 'FLAGS message))))

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
  '("answered" "deleted" "edited" "filed" "forwarded" "resent" "seen"))

(define (message-flags->header-field flags)
  (make-header-field message-flags:name
		     (decorated-string-append "" " " "" flags)))

(define (header-field->message-flags header)
  (and (string-ci=? message-flags:name (header-field-name header))
       ;; Extra pair needed to distinguish #F from ().
       (cons #f (burst-string (header-field-value header) char-set:lwsp #t))))

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

(define (message-edited? msg) (message-flagged? msg "edited"))
(define (message-unedited? msg) (not (message-flagged? msg "edited")))
(define (message-edited msg) (set-message-flag msg "edited"))
(define (message-not-edited msg) (clear-message-flag msg "edited"))

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
       (rfc822:header-field-name? object 0 (string-length object))))

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
  (let loop ((lines lines) (headers '()))
    (if (and (pair? lines)
	     (not (string-null? (car lines))))
	(let collect-group ((lines (cdr lines)) (group (list (car lines))))
	  (if (or (not (pair? lines))
		  (string-null? (car lines))
		  (header-field-initial-line? (car lines)))
	      (loop lines
		    (cons (lines->header-field (reverse! group)) headers))
	      (collect-group (cdr lines) (cons (car lines) group))))
	(reverse! headers))))

(define (header-field-initial-line? line)
  (let ((colon (string-find-next-char line #\:)))
    (and colon
	 (rfc822:header-field-name? line 0 colon))))

(define (header-field-continuation-line? line)
  (and (not (string-null? line))
       (char-lwsp? (string-ref line 0))))

(define (string->header-fields string)
  (lines->header-fields (string->lines string)))

(define (header-fields->string headers)
  (lines->string (header-fields->lines headers)))