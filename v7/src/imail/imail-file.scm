#| -*-Scheme-*-

$Id: imail-file.scm,v 1.95 2008/07/03 20:08:09 cph Exp $

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

;;;; IMAIL mail reader: file-based folder support

(declare (usual-integrations))

;;;; File-folder types

(define-class <file-folder-type> ()
  (name define accessor)
  (predicate define accessor))

(define (define-file-folder-type class name predicate)
  (hash-table/put! file-folder-types
		   class
		   ((instance-constructor class '(NAME PREDICATE))
		    name predicate)))

(define file-folder-types
  (make-eq-hash-table))

(define (prompt-for-file-folder-type url)
  (imail-ui:prompt-for-alist-value
   (string-append "File type for " (url->string url))
   (map (lambda (type)
	  (cons (file-folder-type-name type) type))
	(hash-table/datum-list file-folder-types))))

(define (url-file-folder-type url)
  (or (file-folder-type (pathname-url-pathname url))
      (error "Unknown folder type:" url)))

(define (file-folder-type pathname)
  (let loop ((types (hash-table/datum-list file-folder-types)))
    (and (pair? types)
	 (if ((file-folder-type-predicate (car types)) pathname)
	     (car types)
	     (loop (cdr types))))))

;;;; URL

(define-class <pathname-url> (<url>)
  (pathname define accessor))

(define-url-protocol "file" <pathname-url>)

(define (make-pathname-url pathname)
  (case (file-type-indirect pathname)
    ((REGULAR) (make-file-url pathname))
    ((DIRECTORY) (make-directory-url pathname))
    ((#F)
     (if (directory-pathname? pathname)
	 (make-directory-url pathname)
	 (make-file-url pathname)))
    (else (error "Pathname refers to illegal file type:" pathname))))

(define (pathname-container-url url)
  (make-directory-url (pathname-container (pathname-url-pathname url))))

(define-method container-url-for-prompt ((url <pathname-url>))
  (make-directory-url (pathname-container (pathname-url-pathname url))))

(define-method url-content-name ((url <pathname-url>))
  (let ((pathname (pathname-url-pathname url)))
    (enough-namestring pathname (pathname-container pathname))))

(define (pathname-container pathname)
  (directory-pathname (directory-pathname-as-file pathname)))

(define-method parse-url-body ((string <string>) (default-url <pathname-url>))
  (make-pathname-url
   (parse-pathname-url-body string (pathname-url-pathname default-url))))

(define (parse-pathname-url-body string default-pathname)
  (let ((finish
	 (lambda (string)
	   (merge-pathnames
	    (let ((s
		   (decorated-string-append
		    "" "/" ""
		    (map url:decode-string (burst-string string #\/ #f)))))
	      (if (and (eq? (host/type-name (pathname-host default-pathname))
			    'DOS)
		       (re-string-match "/[a-z]:" s #t))
		  (string-tail s 1)
		  s))
	    (directory-pathname default-pathname)))))
    (cond ((string-prefix? "//localhost/" string)
	   (finish (string-tail string (string-length "//localhost"))))
	  ((string-prefix? "///" string)
	   (finish (string-tail string (string-length "//"))))
	  ((string-prefix? "//" string)
	   (error:bad-range-argument string 'PARSE-PATHNAME-URL-BODY))
	  (else
	   (finish string)))))

(define-method url-body ((url <pathname-url>))
  (pathname->url-body (pathname-url-pathname url)))

(define (pathname->url-body pathname)
  (string-append (let ((device (pathname-device pathname)))
		   (if (string? device)
		       (string-append "/" device ":")
		       ""))
		 (let ((directory (pathname-directory pathname)))
		   (if (pair? directory)
		       (string-append
			(if (eq? (car directory) 'ABSOLUTE) "/" "")
			(decorated-string-append
			 "" "" "/"
			 (map (lambda (string)
				(url:encode-string
				 (if (eq? string 'UP) ".." string)))
			      (cdr directory))))
		       ""))
		 (url:encode-string (file-namestring pathname))))

;;;; File URLs

(define-class <file-url> (<folder-url> <pathname-url>))

(define make-file-url
  (let ((constructor (instance-constructor <file-url> '(PATHNAME))))
    (lambda (pathname)
      (intern-url (constructor (merge-pathnames pathname))
		  pathname-container-url))))

(define-method url-exists? ((url <file-url>))
  (file-exists? (pathname-url-pathname url)))

(define-method folder-url-is-selectable? ((url <file-url>))
  (and (url-exists? url)
       (file-folder-type (pathname-url-pathname url))
       #t))

(define-method url-corresponding-container ((url <file-url>))
  url
  #f)

(define-method url-base-name ((url <file-url>))
  (pathname-name (pathname-url-pathname url)))

;;;; Directory URLs

(define-class <directory-url> (<container-url> <pathname-url>))

(define make-directory-url
  (let ((constructor (instance-constructor <directory-url> '(PATHNAME))))
    (lambda (pathname)
      (intern-url
       (constructor (pathname-as-directory (merge-pathnames pathname)))
       pathname-container-url))))

(define-method url-exists? ((url <directory-url>))
  (file-directory? (pathname-url-pathname url)))

(define-method make-content-url ((url <directory-url>) name)
  (make-pathname-url (merge-pathnames name (pathname-url-pathname url))))

(define-method container-url-contents ((url <directory-url>))
  (simple-directory-read (pathname-url-pathname url)
    (lambda (name directory result)
      (if (or (string=? name ".") (string=? name ".."))
	  result
	  (cons (make-pathname-url
		 (parse-namestring (string-append directory name) #f #f))
		result)))))

;;;; Server operations

(define-method %url-complete-string
    ((string <string>) (default-url <pathname-url>)
		       if-unique if-not-unique if-not-found)
  (pathname-complete-string
   (parse-pathname-url-body
    string
    (directory-pathname (pathname-url-pathname default-url)))
   (lambda (pathname) pathname #t)
   (lambda (string)
     (if-unique (pathname->url-body string)))
   (lambda (prefix get-completions)
     (if-not-unique (pathname->url-body prefix)
		    (lambda () (map pathname->url-body (get-completions)))))
   if-not-found))

(define-method %url-string-completions
    ((string <string>) (default-url <pathname-url>))
  (map pathname->url-body
       (pathname-completions-list
	(parse-pathname-url-body
	 string
	 (directory-pathname (pathname-url-pathname default-url)))
	(lambda (pathname) pathname #t))))

(define-method %create-resource ((url <file-url>))
  (let ((pathname (pathname-url-pathname url)))
    (if (file-exists? pathname)
	(error:bad-range-argument url 'CREATE-RESOURCE))
    (create-file-folder-file url (prompt-for-file-folder-type url))
    (open-resource url)))

(define-generic create-file-folder-file (url type))

(define-method %create-resource ((url <directory-url>))
  (make-directory (pathname-url-pathname url))
  (maybe-make-resource url make-file-container))

(define-method %delete-resource ((url <file-url>))
  (delete-file (pathname-url-pathname url)))

(define-method %delete-resource ((url <directory-url>))
  (delete-directory (pathname-url-pathname url)))

(define-method %rename-resource ((url <pathname-url>) (new-url <pathname-url>))
  (rename-file (pathname-url-pathname url) (pathname-url-pathname new-url)))

(define-method with-open-connection ((url <file-url>) thunk)
  url
  (thunk))

;;;; Folder

(define-class (<file-folder> (predicate)) (<folder>)
  (messages define standard
	    accessor %file-folder-messages
	    initial-value 'UNKNOWN)
  (file-modification-time define standard
			  initial-value #f)
  (file-modification-count define standard
			   initial-value #f)
  (xstring define standard))

(define (file-folder-messages folder)
  (if (eq? 'UNKNOWN (%file-folder-messages folder))
      (revert-file-folder folder))
  (%file-folder-messages folder))

(define-generic revert-file-folder (folder))

(define (file-folder-pathname folder)
  (pathname-url-pathname (resource-locator folder)))

(define-method open-resource ((url <pathname-url>))
  (or (and (url-exists? url)
	   (%open-file-resource url (url-file-folder-type url)))
      (begin
	(unmemoize-resource url)
	(error:bad-range-argument url 'OPEN-RESOURCE))))

(define-generic %open-file-resource (url folder-type))

(define-method close-resource ((folder <file-folder>) no-defer?)
  no-defer?
  (save-resource folder)
  (discard-file-folder-messages folder)
  (discard-file-folder-xstring folder))

(define (discard-file-folder-messages folder)
  (without-interrupts
   (lambda ()
     (let ((messages (%file-folder-messages folder)))
       (if (not (eq? 'UNKNOWN messages))
	   (begin
	     (set-file-folder-messages! folder 'UNKNOWN)
	     (for-each-vector-element messages detach-message!)))))))

(define (discard-file-folder-xstring folder)
  (without-interrupts
   (lambda ()
     (set-file-folder-xstring! folder #f)
     (set-file-folder-file-modification-time! folder #f)
     (set-file-folder-file-modification-count! folder #f))))

(define-method folder-length ((folder <file-folder>))
  (vector-length (file-folder-messages folder)))

(define-method %get-message ((folder <file-folder>) index)
  (vector-ref (file-folder-messages folder) index))

(define-method %append-message ((message <message>) (url <file-url>))
  (let ((exists? (url-exists? url)))
    (let ((folder (get-memoized-resource url)))
      (if folder
	  (let ((message (make-message-copy message folder)))
	    (without-interrupts
	     (lambda ()
	       (set-file-folder-messages!
		folder
		(let ((messages (file-folder-messages folder)))
		  (let ((n (vector-length messages)))
		    (let ((messages (vector-grow messages (fix:+ n 1))))
		      (attach-message! message folder n)
		      (vector-set! messages n message)
		      messages)))))))
	  (let ((type
		 (if exists?
		     (url-file-folder-type url)
		     (prompt-for-file-folder-type url))))
	    (if (not exists?)
		(create-file-folder-file url type))
	    (append-message-to-file message url type))))
    (not exists?)))

(define-generic make-message-copy (message folder))
(define-generic append-message-to-file (message url type))

(define-method expunge-deleted-messages ((folder <file-folder>))
  (without-interrupts
   (lambda ()
     (let ((messages (file-folder-messages folder)))
       (let ((n (vector-length messages)))
	 (let ((n-deleted
		(let loop ((i 0) (n-deleted 0))
		  (if (fix:< i n)
		      (loop (fix:+ i 1)
			    (if (message-deleted? (vector-ref messages i))
				(fix:+ n-deleted 1)
				n-deleted))
		      n-deleted))))
	   (if (fix:> n-deleted 0)
	       (let ((messages* (make-vector (- n n-deleted))))
		 (let loop ((i 0) (i* 0))
		   (if (fix:< i n)
		       (let ((m (vector-ref messages i)))
			 (if (message-deleted? m)
			     (begin
			       (let ((key (message-order-key m)))
                                 (detach-message! m)
                                 (object-modified! folder 'EXPUNGE i* key))
			       (loop (fix:+ i 1) i*))
			     (begin
			       (set-message-index! m i*)
			       (vector-set! messages* i* m)
			       (loop (fix:+ i 1) (fix:+ i* 1)))))))
		 (set-file-folder-messages! folder messages*)))))))))

(define-method %search-folder ((folder <file-folder>) criteria)
  (cond ((string? criteria)
	 (let ((n (folder-length folder)))
	   (let loop ((index 0) (winners '()))
	     (if (< index n)
		 (loop (+ index 1)
		       (if (let ((message (%get-message folder index)))
			     (or (string-search-forward
				  criteria
				  (header-fields->string
				   (message-header-fields message)))
				 (string-search-forward
				  criteria
				  (file-message-body message))))
			   (cons index winners)
			   winners))
		 (reverse! winners)))))
	(else
	 (error:wrong-type-argument criteria
				    "search criteria"
				    'SEARCH-FOLDER))))

(define-method folder-sync-status ((folder <file-folder>))
  (let ((sync-time (file-folder-file-modification-time folder))
	(sync-count (file-folder-file-modification-count folder))
	(current-count (object-modification-count folder))
	(current-time (file-modification-time (file-folder-pathname folder))))
    (if (and sync-time sync-count)
	(if current-time
	    (if (= sync-time current-time)
		(if (= sync-count current-count)
		    'SYNCHRONIZED
		    'CACHE-MODIFIED)
		(if (= sync-count current-count)
		    'PERSISTENT-MODIFIED
		    'BOTH-MODIFIED))
	    'PERSISTENT-DELETED)
	'UNSYNCHRONIZED)))

(define-method save-resource ((folder <file-folder>))
  (and (let ((status (folder-sync-status folder)))
	 (or (memq status '(CACHE-MODIFIED PERSISTENT-DELETED))
	     (and (eq? status 'BOTH-MODIFIED)
		  (imail-ui:prompt-for-yes-or-no?
		   "Disk file has changed since last read.  Save anyway"))))
       (call-with-current-continuation
	 (lambda (k)
	   (bind-condition-handler (list condition-type:error)
	       (lambda (condition)
		 ;; Can this be done in a pop-up buffer?  It doesn't
		 ;; work just to use IMAIL-UI:PRESENT-USER-ALERT
		 ;; because that futzes with the kill-buffer hooks.
		 (imail-ui:message
		  (call-with-output-string
		    (lambda (output-port)
		      (write-condition-report condition output-port))))
		 (k #f))
	     (lambda ()
	       (synchronize-file-folder-write folder write-file-folder)
	       #t))))))

(define-generic write-file-folder (folder pathname))

(define (synchronize-file-folder-write folder writer)
  (let ((pathname (file-folder-pathname folder)))
    (let loop ()
      (let ((count (object-modification-count folder)))
	(writer folder pathname)
	(let ((t (file-modification-time pathname)))
	  (if (and t (= count (object-modification-count folder)))
	      (begin
		(set-file-folder-file-modification-count! folder count)
		(set-file-folder-file-modification-time! folder t))
	      (loop)))))))

(define (read-file-folder-contents folder reader)
  (discard-file-folder-messages folder)
  (let ((t (file-folder-file-modification-time folder))
	(pathname (file-folder-pathname folder)))
    (if (not (and t (= t (file-modification-time pathname))))
	(begin
	  (if t (discard-file-folder-xstring folder))
	  (let loop ()
	    (let ((t (file-modification-time pathname)))
	      ((imail-ui:message-wrapper "Reading file "
					 (->namestring pathname))
	       (lambda ()
		 (set-file-folder-xstring! folder
					   (read-file-into-xstring pathname))))
	      (if (= t (file-modification-time pathname))
		  (begin
		    (set-file-folder-file-modification-time! folder t)
		    (set-file-folder-file-modification-count!
		     folder
		     (object-modification-count folder)))
		  (loop)))))))
  (set-file-folder-messages!
   folder
   ((imail-ui:message-wrapper "Parsing messages")
    (lambda ()
      (call-with-input-xstring (file-folder-xstring folder) 0 reader)))))

(define-method discard-folder-cache ((folder <file-folder>))
  (close-resource folder #f))

(define-method probe-folder ((folder <file-folder>))
  folder
  unspecific)

(define-method folder-connection-status ((folder <file-folder>))
  folder
  'NO-SERVER)

(define-method disconnect-folder ((folder <file-folder>))
  folder
  unspecific)

(define-method preload-folder-outlines ((folder <file-folder>))
  folder
  unspecific)

(define-method first-unseen-message-index ((folder <file-folder>))
  folder
  0)

;;;; Container

(define-class (<file-container> (constructor (locator))) (<container>))

(define-method open-resource ((url <directory-url>))
  (maybe-make-resource url make-file-container))

(define-method close-resource ((container <file-container>) no-defer?)
  container no-defer?
  unspecific)

(define-method save-resource ((container <file-container>))
  container
  #f)

;;;; Message

(define-class <file-message> (<message>)
  body)

(define (file-message-xstring message)
  (file-folder-xstring (message-folder message)))

(define (file-external-ref? object)
  (and (pair? object)
       (exact-nonnegative-integer? (car object))
       (exact-nonnegative-integer? (cdr object))))

(define (make-file-external-ref start end) (cons start end))
(define (file-external-ref/start ref) (car ref))
(define (file-external-ref/end ref) (cdr ref))

(define (define-file-external-message-method procedure class slot operator)
  (let ((accessor (slot-accessor class slot)))
    (define-method procedure ((message class))
      (let ((item (accessor message)))
	(if (file-external-ref? item)
	    (operator
	     (xsubstring (file-message-xstring message)
			 (file-external-ref/start item)
			 (file-external-ref/end item)))
            item)))))

(define-file-external-message-method message-header-fields
  <file-message>
  'HEADER-FIELDS
  (lambda (s)
    (remove! internal-header-field? (string->header-fields s))))

(define-generic file-message-body (message))

(define-file-external-message-method file-message-body
  <file-message>
  'BODY
  (lambda (s) s))

(define-method file-message-body ((message <message>))
  (call-with-output-string
   (lambda (port)
     (write-message-body message port))))

(define-method write-message-body ((message <file-message>) port)
  (write-string (file-message-body message) port))

(define-method set-message-flags! ((message <file-message>) flags)
  (%set-message-flags! message flags))

(let ((get-header-fields (slot-accessor <file-message> 'HEADER-FIELDS))
      (get-body (slot-accessor <file-message> 'BODY)))
  (define-method message-length ((message <file-message>))
    (+ (let ((headers (get-header-fields message)))
	 (if (file-external-ref? headers)
	     (- (file-external-ref/end headers)
		(file-external-ref/start headers))
	     (apply +
		    (map header-field-length
			 (message-header-fields message)))))
       1
       (let ((body (get-body message)))
	 (if (file-external-ref? body)
	     (- (file-external-ref/end body)
		(file-external-ref/start body))
	     (string-length (file-message-body message)))))))

(define-method message-internal-time ((message <file-message>))
  (or (let loop
	  ((headers (get-all-header-fields message "received")) (winner #f))
	(if (pair? headers)
	    (loop (cdr headers)
		  (let ((time
			 (ignore-errors
			  (lambda ()
			    (call-with-values
				(lambda ()
				  (rfc822:received-header-components
				   (header-field-value (car headers))))
			      (lambda (from by via with id for time)
				from by via with id for	;ignored
				time))))))
		    (if (and time
			     (not (condition? time))
			     (or (not winner) (< time winner)))
			time
			winner)))
	    winner))
      (message-time message)
      (file-folder-modification-time (message-folder message))))

(define (file-folder-modification-time folder)
  (or (let ((t
	     (or (file-folder-file-modification-time folder)
		 (file-modification-time (file-folder-pathname folder)))))
	(and t
	     (file-time->universal-time t)))
      (get-universal-time)))

(define (file-folder-internal-headers folder ref)
  (filter! internal-header-field?
	   (string->header-fields
	    (xsubstring (file-folder-xstring folder)
			(file-external-ref/start ref)
			(file-external-ref/end ref)))))