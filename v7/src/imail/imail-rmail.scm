;;; -*-Scheme-*-
;;;
;;; $Id: imail-rmail.scm,v 1.2 2000/01/07 23:09:17 cph Exp $
;;;
;;; Copyright (c) 1999 Massachusetts Institute of Technology
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

(define-class (<rmail-url> (constructor (pathname))) (<file-url>))

(define-url-protocol "rmail" <rmail-url>
  (lambda (string)
    (make-rmail-url (short-name->pathname string))))

;;;; Server operations

(define-method %open-folder ((url <rmail-url>))
  (read-rmail-file url))

(define-method %new-folder ((url <rmail-url>))
  (let ((folder (make-rmail-folder url 'COMPUTE '())))
    (save-folder folder)
    folder))

;;;; Folder

(define-class (<rmail-folder> (constructor (url header-fields messages)))
    (<file-folder>)
  (header-fields accessor header-fields define modifier))

(define-method %write-folder ((folder <folder>) (url <rmail-url>))
  (write-rmail-file folder url))

(define-method poll-folder ((folder <rmail-folder>))
  (rmail-get-new-mail folder))

(define-method initialize-instance ((folder <rmail-folder>))
  (if (eq? 'COMPUTE (header-fields folder))
      (set-rmail-folder-header-fields!
       folder
       (compute-rmail-folder-header-fields folder))))

(define-method header-fields ((folder <folder>))
  (compute-rmail-folder-header-fields folder))

(define (compute-rmail-folder-header-fields folder)
  (list (make-header-field "Version" " 5")
	(make-header-field
	 "Labels"
	 (let ((labels (compute-rmail-folder-labels folder)))
	   (if (pair? labels)
	       (apply string-append
		      (car labels)
		      (map (lambda (label) (string-append "," label))
			   (cdr labels)))
	       "")))
	(make-header-field "Note" "   This is the header of an rmail file.")
	(make-header-field "Note" "   If you are seeing it in rmail,")
	(make-header-field "Note"
			   "    it means the file has no messages in it.")))

(define (compute-rmail-folder-labels folder)
  (flags->rmail-labels
   (let ((n (count-messages folder)))
     (let loop ((index 0) (flags '()))
       (if (fix:< index n)
	   (loop (fix:+ index 1)
		 (union-of-lists (message-flags (get-message folder index))
				 flags))
	   flags)))))

;;;; Read RMAIL file

(define (read-rmail-file url)
  (let* ((pathname (file-url-pathname url))
	 (namestring (->namestring pathname)))
    (call-with-input-file pathname
      (lambda (port)
	(let ((folder-headers (read-rmail-prolog port)))
	  (make-rmail-folder url
			     folder-headers
			     (read-rmail-messages port)))))))

(define (read-rmail-prolog port)
  (if (not (string-prefix? "BABYL OPTIONS:" (read-required-line port)))
      (error "Not an RMAIL file:" port))
  (lines->header-fields (read-lines-to-eom port)))

(define (read-rmail-messages port)
  (source->list (lambda () (read-rmail-message port))))

(define (read-rmail-message port)
  ;; **** This must be generalized to recognize an RMAIL file that has
  ;; unix-mail format messages appended to it.
  (let ((line (read-line port)))
    (cond ((eof-object? line)
	   line)
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
		(let ((message (make-standard-message headers body)))
		  (for-each (lambda (flag)
			      (set-message-flag message flag))
			    flags)
		  (let ((headers (message-header-fields message)))
		    (if (and (pair? headers)
			     (string-ci=? "summary-line"
					  (header-field-name (car headers))))
			(begin
			  (set-message-property
			   message
			   (header-field-name (car headers))
			   (header-field-value (car headers)))
			  (set-message-header-fields! message (cdr headers)))))
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

(define (write-rmail-file folder url)
  ;; **** Do backup of file here.
  (call-with-output-file (file-url-pathname url)
    (lambda (port)
      (write-rmail-prolog (header-fields folder) port)
      (write-rmail-messages (file-folder-messages folder) port))))

(define (write-rmail-prolog header-fields port)
  (write-string "BABYL OPTIONS: -*- rmail -*-" port)
  (newline port)
  (write-header-fields header-fields port)
  (write-char rmail-message:end-char port))

(define (write-rmail-messages messages port)
  (for-each (lambda (message) (write-rmail-message message port)) messages))

(define (write-rmail-message message port)
  (write-char rmail-message:start-char port)
  (newline port)
  (let ((headers (message-header-fields message))
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
	(let ((initial-count (count-messages folder)))
	  (let ((inbox-folders
		 (map (lambda (pathname)
			(let ((inbox (read-rmail-inbox folder pathname #t)))
			  (let ((n (count-messages inbox)))
			    (do ((index 0 (fix:+ index 1)))
				((fix:= i n))
			      (append-message folder
					      (get-message inbox index))))
			  inbox))
		      pathnames)))
	    (save-folder folder)
	    (for-each (lambda (folder)
			(if folder
			    (delete-folder (folder-url folder))))
		      inbox-folders))
	  (fix:- (count-messages folder) initial-count)))))

(define (rmail-folder-inbox-list folder)
  (let ((url (folder-url folder))
	(inboxes (get-first-header-field-value folder "mail" #f)))
    (cond (inboxes
	   (map (let ((directory
		       (directory-pathname (file-url-pathname url))))
		  (lambda (filename)
		    (merge-pathnames (string-trim filename) directory)))
		(burst-string inboxes #\, #f)))
	  ((pathname=? (rmail-primary-folder-name) (url-body url))
	   (rmail-primary-inbox-list))
	  (else '()))))

(define (rmail-primary-folder-name)
  "RMAIL")

(define (rmail-primary-inbox-list)
  (error "Unimplemented procedure:" 'RMAIL-PRIMARY-INBOX-LIST))

(define (rmail-spool-directory)
  (error "Unimplemented procedure:" 'RMAIL-SPOOL-DIRECTORY))

(define (read-rmail-inbox folder pathname rename?)
  (let ((pathname
	 (cond ((not rename?)
		pathname)
	       ((pathname=? (rmail-spool-directory)
			    (directory-pathname pathname))
		(rename-inbox-using-movemail
		 pathname
		 (directory-pathname
		  (file-url-pathname (folder-url folder)))))
	       (else
		(rename-inbox-using-rename pathname)))))
    (and (file-exists? pathname)
	 (open-folder (make-url "umail" (pathname->short-name pathname))))))

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
  (let loop
      ((strings (remove-equal-duplicates (append attributes labels)))
       (flags '()))
    (if (pair? strings)
	(loop (cdr strings)
	      (cons (if (list-search-positive rmail-attributes
			  (lambda (attribute)
			    (string-ci=? attribute (car strings))))
			(rmail-attribute->flag (car strings))
			(rmail-label->flag (car strings)))
		    flags))
	(reverse! flags))))

(define (flags->rmail-markers flags)
  (let loop ((flags flags) (attributes '()) (labels '()))
    (if (pair? flags)
	(if (flag-is-rmail-attribute? (car flags))
	    (loop (cdr flags)
		  (cons (flag->rmail-attribute (car flags)) attributes)
		  labels)
	    (loop (cdr flags)
		  attributes
		  (cons (flag->rmail-label (car flags)) labels)))
	(values (reverse! attributes) (reverse! labels)))))

(define (flags->rmail-labels flags)
  (call-with-values (lambda () (flags->rmail-markers flags))
    (lambda (attributes labels)
      attributes
      labels)))

(define (flag-is-rmail-attribute? flag)
  (memq flag rmail-attribute-flags))

(define (flag->rmail-attribute flag)
  (symbol->string flag))

(define (rmail-attribute->flag attribute)
  (intern attribute))

(define (flag->rmail-label flag)
  (if (symbol? flag)
      (string-append "standard:" (symbol->string flag))
      flag))

(define (rmail-label->flag label)
  (if (string-prefix? "standard:" label)
      (intern (string-tail label 9))
      label))

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

(define rmail-attribute-flags
  (map intern rmail-attributes))

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