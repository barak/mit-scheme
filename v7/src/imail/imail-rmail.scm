;;; -*-Scheme-*-
;;;
;;; $Id: imail-rmail.scm,v 1.59 2001/03/19 22:26:01 cph Exp $
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

;;;; IMAIL mail reader: RMAIL back end

(declare (usual-integrations))

;;;; URL

(define-class <rmail-url> (<file-url>))
(define-url-protocol "rmail" <rmail-url>)

(define make-rmail-url
  (let ((constructor (instance-constructor <rmail-url> '(PATHNAME))))
    (lambda (pathname)
      (intern-url (constructor (merge-pathnames pathname))))))

(define-method parse-url-body ((string <string>) (default-url <rmail-url>))
  (make-rmail-url
   (parse-file-url-body string (file-url-pathname default-url))))

(define-method make-peer-url ((url <rmail-url>) name)
  (make-rmail-url
   (merge-pathnames (pathname-default-type name "rmail")
		    (directory-pathname (file-url-pathname url)))))

;;;; Server operations

(define-method %open-folder ((url <rmail-url>))
  (if (not (file-readable? (file-url-pathname url)))
      (error:bad-range-argument url 'OPEN-FOLDER))
  (make-rmail-folder url))

(define-method %create-folder ((url <rmail-url>))
  (if (file-exists? (file-url-pathname url))
      (error:bad-range-argument url 'CREATE-FOLDER))
  (let ((folder (make-rmail-folder url)))
    (set-file-folder-messages! folder '())
    (set-rmail-folder-header-fields!
     folder
     (compute-rmail-folder-header-fields folder))
    (set-file-folder-file-modification-time! folder (get-universal-time))
    (set-file-folder-file-modification-count!
     folder
     (folder-modification-count folder))
    (save-folder folder)))

;;;; Folder

(define-class (<rmail-folder> (constructor (url))) (<file-folder>)
  (header-fields define standard))

(define-method rmail-folder-header-fields ((folder <folder>))
  (compute-rmail-folder-header-fields folder))

(define (compute-rmail-folder-header-fields folder)
  (make-rmail-folder-header-fields (folder-flags folder)))

(define (make-rmail-folder-header-fields flags)
  (list (make-header-field "Version" "5")
	(make-header-field "Labels"
			   (decorated-string-append
			    "" "," ""
			    (flags->rmail-labels flags)))
	(make-header-field "Note" "This is the header of an rmail file.")
	(make-header-field "Note" "If you are seeing it in rmail,")
	(make-header-field "Note" "it means the file has no messages in it.")))

;;;; Message

(define-class (<rmail-message>
	       (constructor (header-fields body flags
					   displayed-header-fields
					   internal-time)))
    (<file-message>)
  displayed-header-fields
  internal-time)

(define-generic rmail-message-displayed-header-fields (message))

(define-file-external-message-method rmail-message-displayed-header-fields
  <rmail-message>
  'DISPLAYED-HEADER-FIELDS
  string->header-fields)

(define-method rmail-message-displayed-header-fields ((message <message>))
  message
  'UNDEFINED)

(let ((accessor (slot-accessor <rmail-message> 'INTERNAL-TIME)))
  (define-method message-internal-time ((message <rmail-message>))
    (or (accessor message)
	(call-next-method message))))

(define-method make-message-copy ((message <message>) (folder <rmail-folder>))
  folder
  (make-rmail-message (message-header-fields message)
		      (file-message-body message)
		      (list-copy (message-flags message))
		      (rmail-message-displayed-header-fields message)
		      (message-internal-time message)))

;;;; Read RMAIL file

(define-method revert-file-folder ((folder <rmail-folder>))
  (read-file-folder-contents folder
    (lambda (port)
      (set-rmail-folder-header-fields! folder (read-rmail-prolog port))
      (let loop ((line #f) (index 0) (messages '()))
	(if (= 0 (remainder index 100))
	    (imail-ui:progress-meter index #f))
	(call-with-values (lambda () (read-rmail-message folder port line))
	  (lambda (message line)
	    (if message
		(begin
		  (attach-message! message folder index)
		  (loop line (+ index 1) (cons message messages)))
		(reverse! messages))))))))

(define (read-rmail-prolog port)
  (if (not (rmail-prolog-start-line? (read-required-line port)))
      (error "Not an RMAIL file:" port))
  (lines->header-fields (read-lines-to-eom port)))

(define (read-rmail-message folder port read-ahead-line)
  (let ((line (or read-ahead-line (read-line port))))
    (cond ((eof-object? line)
	   (values #f #f))
	  ((rmail-prolog-start-line? line)
	   (discard-to-eom port)
	   (read-rmail-message folder port #f))
	  ((rmail-message-start-line? line)
	   (values (read-rmail-message-1 folder port) #f))
	  ((umail-delimiter? line)
	   (read-umail-message folder line port
	     (lambda (line)
	       (or (rmail-prolog-start-line? line)
		   (rmail-message-start-line? line)
		   (umail-delimiter? line)))))
	  (else
	   (error "Malformed RMAIL file:" port)))))

(define (read-rmail-message-1 folder port)
  (call-with-values (lambda () (read-rmail-attributes-line port))
    (lambda (formatted? flags)
      (let* ((headers (read-rmail-alternate-headers port))
	     (displayed-headers (read-rmail-displayed-headers port))
	     (body (read-rmail-body port))
	     (finish
	      (lambda (headers displayed-headers)
		(call-with-values
		    (lambda ()
		      (parse-rmail-internal-time-header folder headers))
		  (lambda (headers time)
		    (make-rmail-message headers
					body
					flags
					displayed-headers
					time))))))
	(if formatted?
	    (finish headers displayed-headers)
	    (finish displayed-headers 'UNDEFINED))))))

(define (read-rmail-attributes-line port)
  (let ((line (read-required-line port)))
    (let ((parts (map string-trim (burst-string line #\, #f))))
      (if (not (and (fix:= 2 (count-matching-items parts string-null?))
		    (or (string=? "0" (car parts))
			(string=? "1" (car parts)))
		    (string-null? (car (last-pair parts)))))
	  (error "Malformed RMAIL message-attributes line:" line))
      (call-with-values
	  (lambda () (cut-list! (except-last-pair (cdr parts)) string-null?))
	(lambda (attributes labels)
	  (values
	   (string=? "1" (car parts))
	   (rmail-markers->flags attributes
				 (if (pair? labels) (cdr labels) labels))))))))

(define (read-rmail-alternate-headers port)
  (let ((start (xstring-port/position port)))
    (make-file-external-ref
     start
     (let loop ()
       (let ((line (read-required-line port)))
	 (cond ((string-null? line)
		(let ((end (- (xstring-port/position port) 1)))
		  (if (not (string=? rmail-message:headers-separator
				     (read-required-line port)))
		      (error "Missing RMAIL headers-separator string:" port))
		  end))
	       ((string=? line rmail-message:headers-separator)
		(- (xstring-port/position port)
		   (+ (string-length line) 1)))
	       (else
		(loop))))))))

(define (read-rmail-displayed-headers port)
  (let ((start (xstring-port/position port)))
    (skip-past-blank-line port)
    (make-file-external-ref start (- (xstring-port/position port) 1))))

(define (read-rmail-body port)
  (let ((start (xstring-port/position port)))
    (input-port/discard-chars port rmail-message:end-char-set)
    (input-port/discard-char port)
    (make-file-external-ref start (- (xstring-port/position port) 1))))

(define (parse-rmail-internal-time-header folder headers)
  (call-with-values
      (lambda () (file-folder-strip-internal-headers folder headers))
    (lambda (headers internal-headers)
      (values headers
	      (let ((v
		     (get-first-header-field internal-headers
					     "X-IMAIL-INTERNAL-TIME"
					     #f)))
		(and v
		     (parse-header-field-date v)))))))

;;;; Write RMAIL file

(define-method write-file-folder ((folder <rmail-folder>) pathname)
  (call-with-binary-output-file pathname
    (lambda (port)
      (write-rmail-file-header (rmail-folder-header-fields folder) port)
      (for-each (lambda (message) (write-rmail-message message port))
		(file-folder-messages folder)))))

(define-method append-message-to-file ((message <message>) (url <rmail-url>))
  (let ((pathname (file-url-pathname url)))
    (if (file-exists? pathname)
	(let ((port (open-binary-output-file pathname #t)))
	  (write-rmail-message message port)
	  (close-port port))
	(call-with-binary-output-file pathname
	  (lambda (port)
	    (write-rmail-file-header (make-rmail-folder-header-fields '())
				     port)
	    (write-rmail-message message port))))))

(define (write-rmail-file-header header-fields port)
  (write-string "BABYL OPTIONS: -*- rmail -*-" port)
  (newline port)
  (write-header-fields header-fields port)
  (write-char rmail-message:end-char port))

(define (write-rmail-message message port)
  (write-char rmail-message:start-char port)
  (newline port)
  (let ((headers
	 (let ((headers (message-header-fields message))
	       (time (message-internal-time message)))
	   (if time
	       (cons (make-header-field "X-IMAIL-INTERNAL-TIME"
					(universal-time->string time))
		     headers)
	       headers)))
	(displayed-headers (rmail-message-displayed-header-fields message)))
    (let ((formatted? (not (eq? 'UNDEFINED displayed-headers))))
      (write-rmail-attributes-line message formatted? port)
      (if formatted? (write-header-fields headers port))
      (write-string rmail-message:headers-separator port)
      (newline port)
      (write-header-fields (if formatted? displayed-headers headers) port)
      (write-message-body message port)
      (fresh-line port)
      (write-char rmail-message:end-char port))))

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
	    (flags-delete "seen" flags)
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

(define (rmail-prolog-start-line? line)
  (string-prefix? "BABYL OPTIONS:" line))

(define (rmail-prolog-end-line? line)
  (string-prefix? "\037" line))

(define (rmail-message-start-line? line)
  (string=? "\f" line))

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

(define (discard-to-eom port)
  (input-port/discard-chars port rmail-message:end-char-set)
  (input-port/discard-char port))