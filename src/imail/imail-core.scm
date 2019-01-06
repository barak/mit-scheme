#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; IMAIL mail reader: core definitions

(declare (usual-integrations))

;;;; URL type

(define-class <url> (<property-mixin>)
  (container initial-value 'UNKNOWN))

(define-class <folder-url> (<url>))
(define-class <container-url> (<url>))

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

;; Return #T iff URL represents an existing folder.
(define-generic url-exists? (url))

;; Return #T iff FOLDER-URL both exists and can be opened.
(define-generic folder-url-is-selectable? (folder-url))

;; If URL can potentially contain other resources, return a container
;; URL for the same resource.  Otherwise return #F.  The result is
;; undefined if the URL doesn't represent an existing folder.
(define-generic url-corresponding-container (url))
(define-method url-corresponding-container ((url <container-url>)) url)

;; Return a locator for the container of URL.  E.g. the container URL
;; of "imap://localhost/inbox/foo" is "imap://localhost/inbox/".
(define-generic container-url (url))
(add-method container-url (slot-accessor-method <url> 'CONTAINER))

;; Like CONTAINER-URL except that the returned container URL is
;; allowed to be different from the true container URL when this
;; results in a better prompt.
;;
;; For example, when URL is "imap://localhost/inbox" and the IMAP
;; server is Cyrus, this will return "imap://localhost/inbox/".
(define-generic container-url-for-prompt (url))

;; Return the content name of a URL.  The content name of a URL is the
;; suffix of the URL that uniquely identifies the resource with
;; respect to its container.
;;
;; Here are some examples:
;;
;; URL					content name
;; ---------------------------		------------
;; imap://localhost/inbox/foo		foo
;; imap://localhost/inbox/foo/		foo/
;; file:/usr/home/cph/foo.mail		foo.mail
(define-generic url-content-name (url))

;; Return a URL that refers to the content NAME of the container
;; referred to by CONTAINER-URL.
(define-generic make-content-url (container-url name))

;; Return the base name of FOLDER-URL.  This is the content name of
;; FOLDER-URL, but presented in a type-independent way.  For example,
;; if the content name of a file URL is "foo.mail", the base name is
;; just "foo".
(define-generic url-base-name (folder-url))

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

(define intern-url
  (let ((modifier (slot-modifier <url> 'CONTAINER)))
    (lambda (url compute-container)
      (let ((string (url->string url)))
	(or (hash-table-ref/default interned-urls string #f)
	    (begin
	      (let ((finished? #f))
		(dynamic-wind
		 (lambda ()
		   (hash-table-set! interned-urls string url))
		 (lambda ()
		   (modifier url (compute-container url))
		   (set! finished? #t)
		   unspecific)
		 (lambda ()
		   (if (not finished?)
		       (hash-table-delete! interned-urls string)))))
	      url))))))

(define interned-urls
  (make-string-hash-table))

(define (define-url-protocol name class)
  (define-method url-protocol ((url class)) url name)
  (hash-table-set! url-protocols (string-downcase name) class))

(define (url-protocol-name? name)
  (hash-table-ref/default url-protocols (string-downcase name) #f))

(define url-protocols
  (make-string-hash-table))

(define (url-presentation-name url)
  (let ((name (url-content-name url)))
    (if (string-suffix? "/" name)
	(string-head name (fix:- (string-length name) 1))
	name)))

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
;; Create a new resource named URL.  Signal an error if the resource
;; already exists or can't be created.

(define (create-resource url)
  (let ((folder (%create-resource url)))
    (container-modified! url 'CREATE-RESOURCE)
    folder))

(define-generic %create-resource (url))

;; -------------------------------------------------------------------
;; Delete the resource named URL.  Signal an error if the resource
;; doesn't exist or if it can't be deleted.

(define (delete-resource url)
  (%delete-resource url)
  (unmemoize-resource url)
  (container-modified! url 'DELETE-RESOURCE))

(define-generic %delete-resource (url))

;; -------------------------------------------------------------------
;; Rename the resource named URL to NEW-URL.  Signal an error if the
;; resource doesn't exist, if NEW-URL already refers to a resource, or if
;; the rename can't be performed for some reason.  This operation does
;; NOT do format conversion, or move a resource from one place to
;; another.  It only allows changing the name of an existing resource.

(define (rename-resource url new-url)
  (%rename-resource url new-url)
  (unmemoize-resource url)
  (container-modified! url 'DELETE-RESOURCE)
  (container-modified! new-url 'CREATE-RESOURCE))

(define-generic %rename-resource (url new-url))

;; -------------------------------------------------------------------
;; Insert a copy of MESSAGE in the folder referenced by URL at the end
;; of the existing messages.  Unspecified result.

(define (append-message message url)
  (if (%append-message message url)
      (container-modified! url 'CREATE-RESOURCE)))

(define-generic %append-message (message url))

;; -------------------------------------------------------------------
;; Keep a connection open to the server referenced by URL for the
;; dynamic extent of THUNK.

(define-generic with-open-connection (url thunk))

(define (container-modified! url type . arguments)
  (let ((container (get-memoized-resource (container-url url))))
    (if container
	(apply object-modified! container type url arguments))))

;; -------------------------------------------------------------------
;; Return a list of URLs referring to the contents of CONTAINER-URL.
;; The result can contain both folder and container URLs.
;; The result is not sorted.

(define-generic container-url-contents (container-url))

;;;; Resources

(define-class <resource> (<property-mixin> <modification-event-mixin>)
  (locator define accessor))

(define-method write-instance ((r <resource>) port)
  (write-instance-helper (resource-type-name r) r port
    (lambda ()
      (write-char #\space port)
      (write (url-content-name (resource-locator r)) port))))

(define-generic resource-type-name (resource))
(define-method resource-type-name ((r <resource>)) r 'RESOURCE)

(define-method url-protocol ((resource <resource>))
  (url-protocol (resource-locator resource)))

(define-method url-body ((resource <resource>))
  (url-body (resource-locator resource)))

(define-method container-url ((resource <resource>))
  (container-url (resource-locator resource)))

(define-method container-url-for-prompt ((resource <resource>))
  (container-url-for-prompt (resource-locator resource)))

(define-method url-content-name ((resource <resource>))
  (url-content-name (resource-locator resource)))

(define-method url-base-name ((resource <resource>))
  (url-base-name (resource-locator resource)))

(define-class <folder> (<resource>)
  (order define accessor
	 initial-value #f))

(define set-folder-order!
  (let ((modifier (slot-modifier <folder> 'ORDER)))
    (lambda (folder order)
      (if order (memoize-folder-order order folder))
      (let ((original-order (folder-order folder)))
        (modifier folder order)
        (cond ((and (not original-order) order)
               (receive-modification-events folder update-folder-order))
              ((and original-order (not order))
               (ignore-modification-events folder update-folder-order))))
      (object-modified! folder 'REORDERED))))

(define-class <container> (<resource>))

(define-method resource-type-name ((r <folder>)) r 'FOLDER)
(define-method resource-type-name ((r <container>)) r 'CONTAINER)

(define-method %append-message (message (folder <folder>))
  (%append-message message (resource-locator folder)))

(define-method make-content-url ((container <container>) name)
  (make-content-url (resource-locator container) name))

(define-method container-url-contents ((container <container>))
  (container-url-contents (resource-locator container)))

(define (guarantee-folder folder procedure)
  (if (not (folder? folder))
      (error:wrong-type-argument folder "IMAIL folder" procedure)))

(define (guarantee-container container procedure)
  (if (not (container? container))
      (error:wrong-type-argument container "IMAIL container" procedure)))

(define (maybe-make-resource url constructor)
  (or (get-memoized-resource url)
      (memoize-resource (constructor url))))

(define (get-memoized-resource url #!optional error?)
  (or (let ((resource (hash-table-ref/default memoized-resources url #f)))
	(and resource
	     (let ((resource (weak-car resource)))
	       ;; Delete memoization _only_ if URL-EXISTS?
	       ;; unambiguously states non-existence.  An error is
	       ;; often transitory.
	       (if (and resource (ignore-errors (lambda () (url-exists? url))))
		   resource
		   (begin
		     (hash-table-delete! memoized-resources url)
		     #f)))))
      (and (if (default-object? error?) #f error?)
	   (error "URL has no associated resource:" url))))

(define (memoize-resource resource)
  (hash-table-set! memoized-resources
		   (resource-locator resource)
		   (weak-cons resource
			      (lambda (resource)
				(close-resource resource #t))))
  resource)

(define (unmemoize-resource url)
  (let ((r.c (hash-table-ref/default memoized-resources url #f)))
    (if r.c
	(let ((resource (weak-car r.c)))
	  (if resource
	      (begin
		(let ((close (weak-cdr r.c)))
		  (if close
		      (close resource)))
		(hash-table-delete! memoized-resources url)))))))

(define (%unmemoize-resource url)
  (hash-table-delete! memoized-resources url))

(define memoized-resources
  (make-key-weak-eq-hash-table))

;;;; Folder operations

;; -------------------------------------------------------------------
;; Open the resource named URL.

(define-generic open-resource (url))

(define (with-open-resource url procedure)
  (let ((resource #f))
    (dynamic-wind (lambda ()
		    (set! resource (open-resource url))
		    unspecific)
		  (lambda () (procedure resource))
		  (lambda ()
		    (let ((r resource))
		      (if r
			  (begin
			    (set! resource #f)
			    (close-resource r #f))))))))

;; -------------------------------------------------------------------
;; Close RESOURCE, freeing up connections, memory, etc.  Subsequent
;; use of the resource must work, but may incur a significant time or
;; space penalty.  NO-DEFER? means that the resource must be closed
;; immediately, and not deferred.

(define-generic close-resource (resource no-defer?))

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
  (%get-message folder (map-folder-index folder index)))

(define-generic %get-message (folder index))

;; -------------------------------------------------------------------
;; Remove all messages in FOLDER that are marked for deletion.
;; Unspecified result.

(define-generic expunge-deleted-messages (folder))

;; -------------------------------------------------------------------
;; Search FOLDER for messages matching CRITERIA.  At present, CRITERIA
;; may be a string.  Returns a list of message indices.

(define (search-folder folder criteria)
  (map (lambda (index)
         (unmap-folder-index folder index))
       (%search-folder folder criteria)))

(define-generic %search-folder (folder criteria))

;; -------------------------------------------------------------------
;; Compare FOLDER's cache with the persistent folder and return a
;; symbol indicating whether they are synchronized, as follows:
;; SYNCHRONIZED CACHE-MODIFIED PERSISTENT-MODIFIED BOTH-MODIFIED
;; PERSISTENT-DELETED UNSYNCHRONIZED

(define-generic folder-sync-status (folder))

;; -------------------------------------------------------------------
;; Save any cached changes made to RESOURCE.  Returns a boolean
;; indicating whether anything was saved.

(define-generic save-resource (resource))

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
;; Preload outline information about each message in the folder.
;; Normally used prior to generating a folder summary, to accelerate
;; the downloading of this information from the server.  This
;; operation need not be implemented, as it is just a performance
;; enhancement.  With an optional list of messages, it preloads
;; outlines only for those messages.

(define-generic preload-folder-outlines (folder #!optional messages))

(define-method preload-folder-outlines
    ((folder <folder>) #!optional messages)
  folder messages			;ignore
  unspecific)

;; -------------------------------------------------------------------
;; Cache the entire contents of the folder locally, including the
;; outline and body text.  For messages that have MIME body
;; structures, CACHE-FOLDER-CONTENTS passes the message, its body
;; structure and a procedure to WALK-MIME-BODY, which should apply the
;; procedure to each section of the message that should be cached.
;;
;; This is like PRELOAD-FOLDER-OUTLINES, and also need not be
;; implemented.  CACHE-FOLDER-CONTENTS should fetch at least what
;; PRELOAD-FOLDER-OUTLINES fetches, so that subsequent calls to
;; PRELOAD-FOLDER-OUTLINES will be fast.

(define-generic cache-folder-contents (folder walk-mime-body))

(define-method cache-folder-contents ((folder <folder>) walk-mime-body)
  folder walk-mime-body                 ;ignore
  unspecific)

;;;; Message type

(define-class <message> (<property-mixin>)
  (header-fields define accessor)
  (flags define accessor)
  (folder define standard
	  initial-value #f)
  (index define standard
	 accessor %message-index
	 initial-value #f))

(define-method write-instance ((message <message>) port)
  (write-instance-helper 'MESSAGE message port
    (lambda ()
      (write-char #\space port)
      (write (message-folder message) port)
      (write-char #\space port)
      (write (%message-index message) port))))

(define (guarantee-message message procedure)
  (if (not (message? message))
      (error:wrong-type-argument message "IMAIL message" procedure)))

(define-generic write-message-body (message port))
(define-generic set-message-flags! (message flags))
(define-generic message-internal-time (message))
(define-generic message-length (message))

(define (message-index message)
  (let ((index (%message-index message))
	(folder (message-folder message)))
    (if folder
	(unmap-folder-index folder index)
	index)))

;;; Messages are MIME entities.

(define-method mime-entity? ((message <message>))
  message                               ;ignore
  #t)

(define-method mime-entity-header-fields ((message <message>))
  (message-header-fields message))

(define-method write-mime-entity-body ((message <message>) port)
  (write-message-body message port))

(define %set-message-flags!
  (let ((modifier (slot-modifier <message> 'FLAGS)))
    (lambda (message flags)
      (modifier message flags)
      (let ((folder (message-folder message)))
	(if folder
	    (object-modified! folder 'FLAGS message))))))

(define (message-permanent-flags message)
  (flags-delete "deleted" (%message-permanent-flags message)))

(define-generic %message-permanent-flags (message))

(define-method %message-permanent-flags ((message <message>))
  (message-flags message))

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
  (call-with-output-string
   (lambda (port)
     (write-header-fields (message-header-fields message) port)
     (write-message-body message port))))

(define (message-time message)
  (intern-property! message '|#[(edwin imail)message-time]|
    (lambda (message)
      (let ((date (get-first-header-field-value message "date" #f)))
	(and date
	     (parse-header-field-date date))))))

(define (message-subject message)
  (intern-property! message '|#[(edwin imail)message-subject]|
    (lambda (message)
      (cond ((get-first-header-field-value message "subject" #f)
	     => strip-subject-re)
	    (else "")))))

(define (strip-subject-re subject)
  (let ((end (string-length subject)))
    (let loop ((start 0))
      (if (and (<= 3 (- end start))
	       (substring-prefix-ci? "Re:" 0 3 subject start end))
	  (cond ((substring-find-next-char-in-set subject (+ start 3) end
						  char-set:subject-content)
		 => loop)
		(else ""))
	  (string-tail subject start)))))

(define char-set:subject-content (char-set-invert (char-set #\space #\tab)))

(define (message-author message)
  (intern-property! message '|#[(edwin imail)message-author]|
    (lambda (message)
      (or (get-first-header-field-address message "from" #f)
	  (get-first-header-field-address message "sender" #f)
	  ""))))

(define (message-recipient message)
  (intern-property! message '|#[(edwin imail)message-recipient]|
    (lambda ()
      (or (get-first-header-field-address message "to" #f)
	  (get-first-header-field-address message "apparently-to" #f)
	  ""))))

(define (get-first-header-field-address message name error?)
  (let ((v (get-first-header-field-value message name error?)))
    (and v
	 (rfc822:first-address v))))

;;;; Message Navigation

(define (first-unseen-message folder)
  (let ((end (folder-length folder)))
    (let loop
	((start
	  (if (folder-order folder)
	      0
	      (first-unseen-message-index folder))))
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

;;;; Folder orders

(define-structure (folder-order
		   (type-descriptor <folder-order>)
		   (constructor %make-folder-order (predicate selector cache)))
  (predicate #f read-only #t)
  (selector #f read-only #t)
  (cache #f read-only #t)
  (tree #f))

(define (make-folder-order predicate selector #!optional cache?)
  (%make-folder-order predicate
		      selector
		      (and (or (default-object? cache?) cache?)
			   (make-integer-hash-table))))

(define (reset-folder-order! order)
  (set-folder-order-tree! order #f)
  (let ((cache (folder-order-cache order)))
    (if cache (hash-table-clear! cache))))

(define (map-folder-index folder index)
  (let ((order (folder-order folder)))
    (if order
	(begin
	  (memoize-folder-order order folder)
          (let ((tree (folder-order-tree order)))
            (if (< index (wt-tree/size tree))
                (%message-index (wt-tree/index-datum tree index))
                index)))
	index)))

(define (unmap-folder-index folder index)
  (let ((order (folder-order folder)))
    (if order
	(begin
	  (memoize-folder-order order folder)
          (or (wt-tree/rank (folder-order-tree order)
                            (index-order-key folder order index))
              index))
	index)))

(define (make-message-wt-tree key<?)
  (make-wt-tree
   (make-wt-tree-type
    (lambda (a b)
      (or (key<? (car a) (car b))
          (and (not (key<? (car b) (car a)))
               (< (cdr a) (cdr b))))))))

(define make-integer-hash-table
  (hash-table-constructor
   (make-hash-table-type int:remainder int:= #f hash-table-entry-type:strong)))

(define (%message-order-key message order index)
  (let ((compute-key
	 (lambda () (cons ((folder-order-selector order) message) index)))
	(cache (folder-order-cache order)))
    (if cache
	(hash-table-intern! cache index compute-key)
	(compute-key))))

(define (index-order-key folder order index)
  (%message-order-key (%get-message folder index) order index))

(define (message-order-key message)
  (let ((folder (message-folder message)))
    (if (not folder)
        (error "Message has no ordering key:" message))
    (let ((order (folder-order folder)))
      (if (not order)
          #f
          (%message-order-key message order (%message-index message))))))

(define (memoize-folder-order order folder)
  (let loop ((modification-count (object-modification-count folder)))
    (if (not (folder-order-tree order))
	(begin
	  (set-folder-order-tree!
	   order
	   (build-folder-order-tree order folder))
	  (let ((modification-count* (object-modification-count folder)))
	    (if (not (= modification-count modification-count*))
		(begin
		  (imail-ui:message "Folder changed; re-sorting")
		  (reset-folder-order! order)
		  (loop modification-count*))))))))

(define (build-folder-order-tree order folder)
  (preload-folder-outlines folder)
  ((imail-ui:message-wrapper "Sorting folder")
   (lambda ()
     (let ((length (folder-length folder))
	   (tree (make-message-wt-tree (folder-order-predicate order)))
	   (selector (folder-order-selector order))
	   (cache (folder-order-cache order)))
       (let ((compute-key
	      (if cache
		  (lambda (message index)
		    (hash-table-intern! cache index
		      (lambda () (cons (selector message) index))))
		  (lambda (message index)
		    (cons (selector message) index)))))
	 (do ((index 0 (+ index 1)))
	     ((= index length))
	   (if (zero? (remainder index 10))
	       (imail-ui:progress-meter index length))
	   (let ((message (%get-message folder index)))
	     (wt-tree/add! tree (compute-key message index) message))))
       tree))))

(define (update-folder-order folder modification-type arguments)
  (without-interrupts
   (lambda ()
     (let ((order (folder-order folder)))
       (if order
	   (case modification-type
	     ((SET-LENGTH)
	      (reset-folder-order! order))
	     ((INCREASE-LENGTH)
	      (let ((tree (folder-order-tree order)))
		(if tree
		    (let ((index (car arguments))
			  (count (cadr arguments)))
		      (do ((index index (+ index 1)))
			  ((= index count))
			(let ((message (%get-message folder index)))
			  (wt-tree/add!
			   tree
			   (%message-order-key message order index)
			   message)))))))
	     ((EXPUNGE)
	      ;; Expunging a message may change the indices of
	      ;; existing messages, which invalidates our data
	      ;; structures, which were designed with UIDs, not
	      ;; sequence numbers, in mind.
	      (reset-folder-order! order))))))))

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
  (make-internal-header-field "FLAGS"
			      (decorated-string-append "" " " "" flags)))

(define (header-fields->message-flags headers)
  (delete-duplicates
   (append-map (lambda (header)
                 (burst-string (header-field-value header)
                               char-set:whitespace
                               #t))
               (filter (internal-header-field-predicate "FLAGS")
                       headers))
   string-ci=?))

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
		   (type-descriptor <header-field>)
		   (safe-accessors #t)
		   (constructor #f)
		   (print-procedure
		    (standard-print-method 'HEADER-FIELD
		      (lambda (header)
			(list (header-field-name header))))))
  (name #f read-only #t)
  (value #f read-only #t))

(define make-header-field
  (let ((constructor (record-constructor <header-field>)))
    (lambda (name value)
      (guarantee-header-field-name name 'MAKE-HEADER-FIELD)
      (constructor name value))))

(define (guarantee-header-field-name object procedure)
  (if (not (header-field-name? object))
      (error:wrong-type-argument object "header-field name" procedure)))

(define (header-field-name? object)
  (and (string? object)
       (rfc822:header-field-name? object 0 (string-length object))))

(define copy-header-field
  copy-record)

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
      (write-string string port start end))))

(define (write-header-field header port)
  (encode-header-field header
    (lambda (string start end)
      (write-string string port start end))))

(define (header-fields->string headers)
  (call-with-output-string
   (lambda (port)
     (write-header-fields headers port))))

(define (header-field->string header)
  (call-with-output-string
   (lambda (port)
     (write-header-field header port))))

(define (header-field-value->string value)
  (call-with-output-string
   (lambda (port)
     (encode-header-field-value value
       (lambda (string start end)
	 (write-string string port start end))))))

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
  (filter (lambda (header)
	    (string-ci=? name (header-field-name header)))
	  (->header-fields headers)))

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

(define (parse-first-named-header headers name default parser)
  (cond ((get-first-header-field-value headers name #f)
         => (header-parser parser name default))
        (else default)))

(define (parse-last-named-header headers name default parser)
  (cond ((get-last-header-field-value headers name #f)
         => (header-parser parser name default))
        (else default)))

(define (parse-all-named-headers headers name default parser)
  (map (header-parser parser name default)
       (get-all-header-field-values headers name)))

(define-integrable (header-parser parser name default)
  (lambda (value)
    (or (parser value)
        (begin
          (warn (string-append "Malformed " name " field value:")
                value)
          default))))

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

(define (internal-header-field? header)
  (string-prefix-ci? internal-header-field-prefix (header-field-name header)))

(define (make-internal-header-field name value)
  (make-header-field (string-append internal-header-field-prefix name)
		     value))

(define (internal-header-field-name header)
  (string-tail (header-field-name header)
	       internal-header-field-prefix-length))

(define (internal-header-field-predicate name)
  (lambda (header)
    (and (internal-header-field? header)
	 (string-ci=? (internal-header-field-name header) name))))

(define internal-header-field-prefix
  "X-IMAIL-")

(define internal-header-field-prefix-length
  (string-length internal-header-field-prefix))
