;;; -*-Scheme-*-
;;;
;;; $Id: imail-file.scm,v 1.76 2001/06/03 01:22:54 cph Exp $
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

;;;; IMAIL mail reader: file-based folder support

(declare (usual-integrations))

;;;; URL

(define-class <pathname-url> (<url>)
  (pathname define accessor))

(define-url-protocol "file" <pathname-url>)

(define (pathname-url-constructor class)
  (let ((procedure
	 (let ((constructor (instance-constructor class '(PATHNAME))))
	   (lambda (pathname)
	     (intern-url (constructor (merge-pathnames pathname))
			 pathname-container-url)))))
    (register-pathname-url-constructor class procedure)
    procedure))

(define (register-pathname-url-constructor class constructor)
  (hash-table/put! pathname-url-constructors class constructor))

(define (get-pathname-url-constructor class)
  (or (hash-table/get pathname-url-constructors class #f)
      (error "Unknown pathname-url class:" class)))

(define pathname-url-constructors
  (make-eq-hash-table))

(define (pathname-container-url url)
  (make-directory-url (pathname-container (pathname-url-pathname url))))

(define-method container-url-for-prompt ((url <pathname-url>))
  (make-directory-url (pathname-container (pathname-url-pathname url))))

(define-method url-content-name ((url <pathname-url>))
  (let ((pathname (pathname-url-pathname url)))
    (enough-namestring pathname (pathname-container pathname))))

(define (pathname-container pathname)
  (directory-pathname (directory-pathname-as-file pathname)))

(define (define-pathname-url-predicates class
	  file-predicate
	  directory-predicate
	  pathname-predicate)
  (let ((constructor (get-pathname-url-constructor class)))
    (let loop ((entries pathname-url-predicates))
      (if (pair? entries)
	  (if (eq? class (vector-ref (car entries) 0))
	      (begin
		(vector-set! (car entries) 1 file-predicate)
		(vector-set! (car entries) 2 directory-predicate)
		(vector-set! (car entries) 3 pathname-predicate)
		(vector-set! (car entries) 4 constructor))
	      (loop (cdr entries)))
	  (begin
	    (set! pathname-url-predicates
		  (cons (vector class
				file-predicate
				directory-predicate
				pathname-predicate
				constructor)
			pathname-url-predicates))
	    unspecific)))))

(define (find-pathname-url-constructor pathname must-exist? if-not-found)
  (let ((type (file-type-indirect pathname))
	(search
	 (lambda (index)
	   (let loop ((entries pathname-url-predicates))
	     (and (pair? entries)
		  (if ((vector-ref (car entries) index) pathname)
		      (vector-ref (car entries) 4)
		      (loop (cdr entries))))))))
    (or (case type
	  ((REGULAR) (search 1))
	  ((DIRECTORY) (search 2))
	  ((#F) (and (not must-exist?) (search 3)))
	  (else #f))
	(and if-not-found
	     (if-not-found pathname type)))))

(define pathname-url-predicates '())

(define-method parse-url-body ((string <string>) (default-url <pathname-url>))
  (let ((pathname
	 (parse-pathname-url-body string (pathname-url-pathname default-url))))
    ((standard-pathname-url-constructor pathname) pathname)))

(define (standard-pathname-url-constructor pathname)
  (find-pathname-url-constructor pathname #f
    (lambda (pathname type)
      (case type
	((REGULAR) make-file-url)
	((DIRECTORY) make-directory-url)
	((#F)
	 (if (directory-pathname? pathname)
	     make-directory-url
	     ;; Default for non-existent files:
	     make-umail-url))
	(else
	 (error "Pathname refers to illegal file type:" pathname))))))

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
(define make-file-url (pathname-url-constructor <file-url>))

(define-method url-exists? ((url <file-url>))
  (file-exists? (pathname-url-pathname url)))

(define-method folder-url-is-selectable? ((url <file-url>))
  (and (find-pathname-url-constructor (pathname-url-pathname url) #t #f) #t))

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

(register-pathname-url-constructor <directory-url> make-directory-url)

(define-method url-exists? ((url <directory-url>))
  (file-directory? (pathname-url-pathname url)))

(define-method make-content-url ((url <directory-url>) name)
  (let ((pathname (merge-pathnames name (pathname-url-pathname url))))
    ((standard-pathname-url-constructor pathname) pathname)))

(define-method container-url-contents ((url <directory-url>))
  (simple-directory-read (pathname-url-pathname url)
    (lambda (name directory result)
      (if (or (string=? name ".") (string=? name ".."))
	  result
	  (let* ((pathname
		  (parse-namestring (string-append directory name) #f #f))
		 (constructor (pathname-url-filter pathname)))
	    (if constructor
		(cons (constructor pathname) result)
		result))))))

;;;; Server operations

(define-method %url-complete-string
    ((string <string>) (default-url <pathname-url>)
		       if-unique if-not-unique if-not-found)
  (pathname-complete-string
   (parse-pathname-url-body
    string
    (directory-pathname (pathname-url-pathname default-url)))
   pathname-url-filter
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
	pathname-url-filter)))

(define (pathname-url-filter pathname)
  (find-pathname-url-constructor pathname #t
    (lambda (pathname type)
      pathname
      (and (eq? type 'DIRECTORY)
	   make-directory-url))))

(define-method %create-resource ((url <directory-url>))
  (make-directory (pathname-url-pathname url)))

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

(define-method %close-resource ((folder <file-folder>) no-defer?)
  no-defer?
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
  (let ((folder (get-memoized-resource url)))
    (if folder
	(let ((message (make-message-copy message folder))
	      (exists?
	       (or (file-folder-file-modification-time folder)
		   (file-exists? (file-folder-pathname folder)))))
	  (without-interrupts
	   (lambda ()
	     (set-file-folder-messages!
	      folder
	      (let ((messages (file-folder-messages folder)))
		(let ((n (vector-length messages)))
		  (let ((messages (vector-grow messages (fix:+ n 1))))
		    (attach-message! message folder n)
		    (vector-set! messages n message)
		    messages))))))
	  (not exists?))
	(append-message-to-file message url))))

(define-generic make-message-copy (message folder))
(define-generic append-message-to-file (message url))

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
			       (detach-message! m)
			       (object-modified! folder 'EXPUNGE i*)
			       (loop (fix:+ i 1) i*))
			     (begin
			       (set-message-index! m i*)
			       (vector-set! messages* i* m)
			       (loop (fix:+ i 1) (fix:+ i* 1)))))))
		 (set-file-folder-messages! folder messages*)))))))))

(define-method search-folder ((folder <file-folder>) criteria)
  (cond ((string? criteria)
	 (let ((n (folder-length folder)))
	   (let loop ((index 0) (winners '()))
	     (if (< index n)
		 (loop (+ index 1)
		       (if (let ((message (get-message folder index)))
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
	 (or (memq status '(FOLDER-MODIFIED PERSISTENT-DELETED))
	     (and (eq? status 'BOTH-MODIFIED)
		  (imail-ui:prompt-for-yes-or-no?
		   "Disk file has changed since last read.  Save anyway"))))
       (begin
	 (synchronize-file-folder-write folder write-file-folder)
	 #t)))

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
  (close-resource folder))

(define-method probe-folder ((folder <file-folder>))
  folder
  unspecific)

(define-method folder-connection-status ((folder <file-folder>))
  folder
  'NO-SERVER)

(define-method disconnect-folder ((folder <file-folder>))
  folder
  unspecific)

(define-method folder-supports-mime? ((folder <file-folder>))
  folder
  #f)

(define-method preload-folder-outlines ((folder <file-folder>))
  folder
  unspecific)

(define-method first-unseen-message-index ((folder <file-folder>))
  folder
  0)

;;;; Container

(define-class (<file-container> (constructor (locator))) (<container>))

(define-method %open-resource ((url <directory-url>))
  (make-file-container url))

(define-method %close-resource ((container <file-container>) no-defer?)
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
	    (call-next-method message))))))

(define-file-external-message-method message-header-fields
  <file-message>
  'HEADER-FIELDS
  string->header-fields)

(define-generic file-message-body (message))

(define-file-external-message-method file-message-body
  <file-message>
  'BODY
  (lambda (s) s))

(define-method file-message-body ((message <message>))
  (with-string-output-port
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

(define (file-folder-strip-internal-headers folder ref)
  (call-with-input-xstring (file-folder-xstring folder)
			   (file-external-ref/start ref)
    (lambda (port)
      (let loop ((header-lines '()))
	(let ((line (read-line port))
	      (finish
	       (lambda (offset)
		 (values (make-file-external-ref
			  (- (xstring-port/position port)
			     offset)
			  (file-external-ref/end ref))
			 (lines->header-fields (reverse! header-lines))))))
	  (cond ((eof-object? line)
		 (finish 0))
		((re-string-match "X-IMAIL-[^:]+:\\|[ \t]" line)
		 (loop (cons line header-lines)))
		(else
		 (finish (+ (string-length line) 1)))))))))