;;; -*-Scheme-*-
;;;
;;; $Id: imail-rmail.scm,v 1.18 2000/04/14 01:45:37 cph Exp $
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

;;;; IMAIL mail reader: RMAIL back end

(declare (usual-integrations))

;;;; URL

(define-class <rmail-url> (<file-url>))

(define make-rmail-url
  (let ((constructor (instance-constructor <rmail-url> '(PATHNAME))))
    (lambda (pathname)
      (constructor (merge-pathnames pathname)))))

(define-url-protocol "rmail" <rmail-url>
  (lambda (string)
    (make-rmail-url (short-name->pathname string))))

;;;; Server operations

(define-method %open-folder ((url <rmail-url>))
  (read-rmail-file (file-url-pathname url)))

(define-method %new-folder ((url <rmail-url>))
  (let ((folder (make-rmail-folder url)))
    (set-header-fields! folder (compute-rmail-folder-header-fields folder))
    (save-folder folder)
    folder))

;;;; Folder

(define-class (<rmail-folder> (constructor (url))) (<file-folder>))

(define-method header-fields ((folder <rmail-folder>))
  (folder-get folder 'RMAIL-HEADER-FIELDS '()))

(define-method set-header-fields! ((folder <rmail-folder>) headers)
  (folder-put! folder 'RMAIL-HEADER-FIELDS headers))

(define-method %write-folder ((folder <folder>) (url <rmail-url>))
  (write-rmail-file folder (file-url-pathname url))
  (if (eq? url (folder-url folder))
      (update-file-folder-modification-time! folder)))

(define-method poll-folder ((folder <rmail-folder>))
  (rmail-get-new-mail folder))

(define-method header-fields ((folder <folder>))
  (compute-rmail-folder-header-fields folder))

(define (compute-rmail-folder-header-fields folder)
  (list (make-header-field "Version" " 5")
	(make-header-field "Labels"
			   (decorated-string-append
			    "" "," ""
			    (flags->rmail-labels (folder-flags folder))))
	(make-header-field "Note" "   This is the header of an rmail file.")
	(make-header-field "Note" "   If you are seeing it in rmail,")
	(make-header-field "Note"
			   "    it means the file has no messages in it.")))

;;;; Read RMAIL file

(define (read-rmail-file pathname)
  (let ((folder (make-rmail-folder (make-rmail-url pathname))))
    (%revert-folder folder)
    folder))

(define-method %revert-folder ((folder <rmail-folder>))
  (call-with-binary-input-file (file-folder-pathname folder)
    (lambda (port)
      (set-header-fields! folder (read-rmail-prolog port))
      (let loop ()
	(let ((message (read-rmail-message port)))
	  (if message
	      (begin
		(append-message folder message)
		(loop)))))))
  (update-file-folder-modification-time! folder))

(define (read-rmail-prolog port)
  (if (not (string-prefix? "BABYL OPTIONS:" (read-required-line port)))
      (error "Not an RMAIL file:" port))
  (lines->header-fields (read-lines-to-eom port)))

(define (read-rmail-message port)
  ;; **** This must be generalized to recognize an RMAIL file that has
  ;; unix-mail format messages appended to it.
  (let ((line (read-line port)))
    (cond ((eof-object? line)
	   #f)
	  ((and (fix:= 1 (string-length line))
		(char=? rmail-message:start-char (string-ref line 0)))
	   (read-rmail-message-1 port))
	  (else
	   (error "Malformed RMAIL file:" port)))))

(define (read-rmail-message-1 port)
  (call-with-values
      (lambda () (parse-attributes-line (read-required-line port)))
    (lambda (formatted? flags)
      (let* ((headers (read-rmail-header-fields port))
	     (displayed-headers
	      (lines->header-fields (read-header-lines port)))
	     (body (read-to-eom port))
	     (finish
	      (lambda (headers)
		(let ((message (make-detached-message headers body)))
		  (for-each (lambda (flag)
			      (set-message-flag message flag))
			    flags)
		  (let ((headers (header-fields message)))
		    (if (and (pair? headers)
			     (string-ci=? "summary-line"
					  (header-field-name (car headers))))
			(begin
			  (set-message-property
			   message
			   (header-field-name (car headers))
			   (header-field-value (car headers)))
			  (set-header-fields! message (cdr headers)))))
		  message))))
	(if formatted?
	    (let ((message (finish headers)))
	      (set-message-property message
				    "displayed-header-fields"
				    displayed-headers)
	      message)
	    (finish displayed-headers))))))

(define (parse-attributes-line line)
  (let ((parts (map string-trim (burst-string line #\, #f))))
    (if (not (and (fix:= 2 (count-matching-items parts string-null?))
		  (or (string=? "0" (car parts))
		      (string=? "1" (car parts)))
		  (string-null? (car (last-pair parts)))))
	(error "Malformed RMAIL message-attributes line:" line))
    (call-with-values
	(lambda () (cut-list! (except-last-pair (cdr parts)) string-null?))
      (lambda (attributes labels)
	(values (string=? "1" (car parts))
		(rmail-markers->flags attributes
				      (if (pair? labels)
					  (cdr labels)
					  labels)))))))

(define (read-rmail-header-fields port)
  (lines->header-fields
   (source->list
    (lambda ()
      (let ((line (read-required-line port)))
	(cond ((string-null? line)
	       (if (not (string=? rmail-message:headers-separator
				  (read-required-line port)))
		   (error "Missing RMAIL message-header separator string:"
			  port))
	       (make-eof-object port))
	      ((string=? rmail-message:headers-separator line)
	       (make-eof-object port))
	      (else line)))))))

;;;; Write RMAIL file

(define (write-rmail-file folder pathname)
  ;; **** Do backup of file here.
  (call-with-binary-output-file pathname
    (lambda (port)
      (write-string "BABYL OPTIONS: -*- rmail -*-" port)
      (newline port)
      (write-header-fields (header-fields folder) port)
      (write-char rmail-message:end-char port)
      (for-each (lambda (message) (write-rmail-message message port))
		(file-folder-messages folder)))))

(define (write-rmail-message message port)
  (write-char rmail-message:start-char port)
  (newline port)
  (let ((headers (header-fields message))
	(displayed-headers
	 (get-message-property message "displayed-header-fields" 'NONE)))
    (write-rmail-attributes-line message displayed-headers port)
    (if (not (eq? 'NONE displayed-headers))
	(begin
	  (write-rmail-properties message port)
	  (write-header-fields headers port)
	  (newline port)))
    (write-string rmail-message:headers-separator port)
    (newline port)
    (if (eq? 'NONE displayed-headers)
	(begin
	  (write-rmail-properties message port)
	  (write-header-fields headers port))
	(write-header-fields displayed-headers port))
    (newline port)
    (write-string (message-body message) port)
    (fresh-line port)
    (write-char rmail-message:end-char port)))

(define (write-rmail-attributes-line message formatted? port)
  (write-char (if formatted? #\1 #\0) port)
  (write-char #\, port)
  (call-with-values (lambda () (flags->rmail-markers (message-flags message)))
    (lambda (attributes labels)
      (let ((write-markers
	     (lambda (markers)
	       (for-each (lambda (marker)
			   (write-char #\space port)
			   (write-string marker port)
			   (write-char #\, port))
			 markers))))
	(write-markers attributes)
	(write-char #\, port)
	(write-markers labels))))
  (newline port))

(define (write-rmail-properties message port)
  (let ((alist (message-properties message)))
    (let ((summary-line
	   (list-search-positive alist
	     (lambda (n.v)
	       (string-ci=? "summary-line" (car n.v))))))
      (if summary-line
	  (%write-header-field (car summary-line) (cdr summary-line) port)))
    (for-each
     (lambda (n.v)
       (if (not (or (string-ci=? "summary-line" (car n.v))
		    (string-ci=? "displayed-header-fields" (car n.v))))
	   (write-header-field
	    (message-property->header-field (car n.v) (cdr n.v))
	    port)))
     alist)))

;;;; Get new mail

(define (rmail-get-new-mail folder)
  (let ((pathnames (rmail-folder-inbox-list folder)))
    (if (null? pathnames)
	#f
	(let ((initial-count (folder-length folder)))
	  (guarantee-rmail-variables-initialized)
	  (let ((inbox-folders
		 (map (lambda (pathname)
			(let ((inbox (read-rmail-inbox folder pathname #t)))
			  (let ((n (folder-length inbox)))
			    (do ((index 0 (+ index 1)))
				((= index n))
			      (append-message folder
					      (get-message inbox index))))
			  inbox))
		      pathnames)))
	    (save-folder folder)
	    (for-each (lambda (folder)
			(if folder
			    (delete-folder folder)))
		      inbox-folders))
	  (- (folder-length folder) initial-count)))))

(define (rmail-folder-inbox-list folder)
  (let ((inboxes (get-first-header-field-value folder "mail" #f)))
    (cond (inboxes
	   (map (let ((directory
		       (directory-pathname (file-folder-pathname folder))))
		  (lambda (filename)
		    (merge-pathnames (string-trim filename) directory)))
		(burst-string inboxes #\, #f)))
	  ((pathname=? (edwin-variable-value 'RMAIL-FILE-NAME)
		       (url-body (folder-url folder)))
	   (edwin-variable-value 'RMAIL-PRIMARY-INBOX-LIST))
	  (else '()))))

(define (read-rmail-inbox folder pathname rename?)
  (let ((pathname
	 (cond ((not rename?)
		pathname)
	       ((pathname=? rmail-spool-directory
			    (directory-pathname pathname))
		(rename-inbox-using-movemail
		 pathname
		 (directory-pathname (file-folder-pathname folder))))
	       (else
		(rename-inbox-using-rename pathname)))))
    (and (file-exists? pathname)
	 (read-umail-file pathname))))

(define (rename-inbox-using-movemail pathname directory)
  (let ((pathname
	 ;; On some systems, /usr/spool/mail/foo is a directory and
	 ;; the actual inbox is /usr/spool/mail/foo/foo.
	 (if (file-directory? pathname)
	     (merge-pathnames (file-pathname pathname)
			      (pathname-as-directory pathname))
	     pathname))
	(target (merge-pathnames ".newmail" directory)))
    (if (and (file-exists? pathname)
	     (not (file-exists? target)))
	(let ((port (make-accumulator-output-port)))
	  (let ((result
		 (run-shell-command
		  (string-append "movemail "
				 (->namestring pathname)
				 " "
				 (->namestring target))
		  'OUTPUT port)))
	    (if (not (= 0 result))
		(error "Movemail failure:"
		       (get-output-from-accumulator port))))))
    target))

(define (rename-inbox-using-rename pathname)
  (let ((target
	 (merge-pathnames (string-append (file-namestring pathname) "+")
			  (directory-pathname pathname))))
    (if (and (file-exists? pathname)
	     (not (file-exists? target)))
	(rename-file pathname target))
    target))

;;;; Attributes and labels

(define (rmail-markers->flags attributes labels)
  (let loop ((strings (append attributes labels)) (flags '()))
    (if (pair? strings)
	(loop (cdr strings) (cons (car strings) flags))
	(reverse!
	 (if (flags-member? "unseen" flags)
	     (flags-delete! "unseen" flags)
	     (cons "seen" flags))))))

(define (flags->rmail-markers flags)
  (let loop
      ((flags
	(if (flags-member? "seen" flags)
	    (flags-delete! "seen" flags)
	    (cons "unseen" flags)))
       (attributes '())
       (labels '()))
    (if (pair? flags)
	(if (member (car flags) rmail-attributes)
	    (loop (cdr flags) (cons (car flags) attributes) labels)
	    (loop (cdr flags) attributes (cons (car flags) labels)))
	(values (reverse! attributes) (reverse! labels)))))

(define (flags->rmail-labels flags)
  (call-with-values (lambda () (flags->rmail-markers flags))
    (lambda (attributes labels)
      attributes
      labels)))

;;;; Syntactic Markers

(define rmail-message:headers-separator
  "*** EOOH ***")

(define rmail-message:start-char
  #\page)

(define rmail-message:end-char
  (integer->char #x1f))

(define rmail-message:end-char-set
  (char-set rmail-message:end-char))

(define rmail-attributes
  '("deleted" "answered" "unseen" "filed" "forwarded" "edited" "resent"))

;;;; Utilities

(define (read-lines-to-eom port)
  (source->list
   (lambda ()
     (if (eqv? rmail-message:end-char (peek-char port))
	 (begin
	   (read-char port)		;discard
	   (make-eof-object port))
	 (read-required-line port)))))

(define (read-to-eom port)
  (let ((string (read-string rmail-message:end-char-set port)))
    (if (or (eof-object? string)
	    (eof-object? (read-char port)))
	(error "EOF while reading RMAIL message body:" port))
    string))