;;; -*-Scheme-*-
;;;
;;; $Id: imail-core.scm,v 1.6 2000/01/14 18:10:08 cph Exp $
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
;;; **** Strip IMAIL headers when importing or exporting messages.
;;;      (What does that mean, precisely?)

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

(define-class <folder> ())

(define (guarantee-folder folder procedure)
  (if (not (folder? folder))
      (error:wrong-type-argument folder "IMAIL folder" procedure)))

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
(define-generic count-messages (folder))

;; Get the INDEX'th message in FOLDER and return it.  Signal an
;; error for invalid INDEX.
(define (get-message folder index)
  (guarantee-index index 'GET-MESSAGE)
  (if (not (fix:< index (count-messages folder)))
      (error:bad-range-argument index 'GET-MESSAGE))
  (%get-message folder index))

(define-generic %get-message (folder index))

;; Insert a copy of MESSAGE in FOLDER at INDEX; pre-existing messages
;; with indices of INDEX or higher have their indices incremented.
;; Unspecified result.
(define (insert-message folder index message)
  (guarantee-index index 'INSERT-MESSAGE)
  (if (not (fix:<= index (length (count-messages folder))))
      (error:bad-range-argument index 'INSERT-MESSAGE))
  (guarantee-message message 'INSERT-MESSAGE)
  (%insert-message folder index message))

(define-generic %insert-message (folder index message))

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

;; Write the contents of FOLDER to URL.
(define (write-folder folder url)
  (%write-folder folder (->url url)))

(define-generic %write-folder (folder url))

;; [These are IMAP commands that appear to be designed to support
;; delivery of usenet news.]
(define-generic subscribe-folder (folder))
(define-generic unsubscribe-folder (folder))

;;;; Message type

(define-structure (message (type-descriptor message-rtd)
			   (safe-accessors #t))
  header-fields
  body
  flags
  properties)

(define (guarantee-message message procedure)
  (if (not (message? message))
      (error:wrong-type-argument message "IMAIL message" procedure)))

(define-generic header-fields (object))

(define-method header-fields ((message message-rtd))
  (message-header-fields message))

(define (copy-message message)
  (make-message (map copy-header-field (message-header-fields message))
		(message-body message)
		(list-copy (message-flags message))
		(alist-copy (message-properties message))))

(define (make-standard-message headers body)
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

(define (maybe-strip-imail-headers strip? headers)
  (if strip?
      (list-transform-negative headers
	(lambda (header)
	  (or (header-field->message-flags header)
	      (header-field->message-property header))))
      headers))

;;;; Message flags

;;; Flags are markers that can be attached to messages.  They indicate
;;; state about the message, such as whether it has been deleted,
;;; seen, etc.  A flag is represented by a symbol or a string; symbols
;;; represent standard flags with predefined meanings, while strings
;;; represent user-defined flags.

(define (message-flagged? message flag)
  (guarantee-message-flag flag 'MESSAGE-FLAGGED?)
  (if (member flag (message-flags message)) #t #f))

(define (set-message-flag message flag)
  (guarantee-message-flag flag 'SET-MESSAGE-FLAG)
  (let ((flags (message-flags message)))
    (if (not (member flag flags))
	(set-message-flags! message (cons flag flags)))))

(define (clear-message-flag message flag)
  (set-message-flags! message (delete flag (message-flags message))))

(define (message-flag? object)
  (or (memq object standard-message-flags)
      (header-field-name? object)))

(define (guarantee-message-flag object procedure)
  (if (not (message-flag? object))
      (error:wrong-type-argument object "message flag" procedure)))

(define standard-message-flags
  '(DELETED ANSWERED SEEN FILED FORWARDED EDITED RESENT))

(define (message-flags->header-field flags)
  (make-header-field
   message-flags:name
   (apply string-append
	  (map (lambda (flag)
		 (if (symbol? flag)
		     (string-append " :" (symbol->string flag))
		     (string-append " " flag)))
	       flags))))

(define (header-field->message-flags header)
  (and (string-ci=? message-flags:name (header-field-name header))
       ;; Extra pair needed to distinguish #F from ().
       (cons 'YUK
	     (map (lambda (token)
		    (if (char=? #\: (string-ref token 0))
			(intern (string-tail token 1))
			token))
		  (burst-string (header-field-value header)
				char-set:lwsp
				#t)))))

(define message-flags:name "X-IMAIL-FLAGS")

(define (message-deleted? msg) (message-flagged? msg 'DELETED))
(define (delete-message msg) (set-message-flag msg 'DELETED))
(define (undelete-message msg) (clear-message-flag msg 'DELETED))

(define (message-answered? msg) (message-flagged? msg 'ANSWERED))
(define (message-answered msg) (set-message-flag msg 'ANSWERED))
(define (message-not-answered msg) (clear-message-flag msg 'ANSWERED))

(define (message-seen? msg) (message-flagged? msg 'SEEN))
(define (message-seen msg) (set-message-flag msg 'SEEN))
(define (message-not-seen msg) (clear-message-flag msg 'SEEN))

(define (message-filed? msg) (message-flagged? msg 'FILED))
(define (message-filed msg) (set-message-flag msg 'FILED))
(define (message-not-filed msg) (clear-message-flag msg 'FILED))

(define (message-forwarded? msg) (message-flagged? msg 'FORWARDED))
(define (message-forwarded msg) (set-message-flag msg 'FORWARDED))
(define (message-not-forwarded msg) (clear-message-flag msg 'FORWARDED))

(define (message-edited? msg) (message-flagged? msg 'EDITED))
(define (message-edited msg) (set-message-flag msg 'EDITED))
(define (message-not-edited msg) (clear-message-flag msg 'EDITED))

(define (message-resent? msg) (message-flagged? msg 'RESENT))
(define (message-resent msg) (set-message-flag msg 'RESENT))
(define (message-not-resent msg) (clear-message-flag msg 'RESENT))

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
				   (cons (cons name value) alist))))))

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

(define (get-first-header-field headers name error?)
  (let loop
      ((headers
	(if (or (pair? headers) (null? headers))
	    headers
	    (header-fields headers))))
    (cond ((pair? headers)
	   (if (string-ci=? name (header-field-name (car headers)))
	       (car headers)
	       (loop (cdr headers))))
	  (error? (error:bad-range-argument name 'GET-FIRST-HEADER-FIELD))
	  (else #f))))

(define (get-last-header-field headers name error?)
  (let loop
      ((headers
	(if (or (pair? headers) (null? headers))
	    headers
	    (header-fields headers)))
       (winner #f))
    (cond ((pair? headers)
	   (loop (cdr headers)
		 (if (string-ci=? name (header-field-name (car headers)))
		     (car headers)
		     winner)))
	  ((and (not winner) error?)
	   (error:bad-range-argument name 'GET-LAST-HEADER-FIELD))
	  (else winner))))

(define (get-all-header-fields headers name)
  (list-transform-positive headers
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

(define (lines->header-fields lines)
  (map lines->header-field
       (burst-list lines header-field-initial-line?)))

(define (header-field-initial-line? line)
  (let ((colon (string-find-next-char line #\:)))
    (and colon
	 (%header-field-name? line 0 colon))))

(define (header-field-continuation-line? line)
  (and (not (string-null? line))
       (char-lwsp? (string-ref line 0))))