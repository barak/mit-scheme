;;; -*-Scheme-*-
;;;
;;; $Id: imail-umail.scm,v 1.43 2001/05/13 03:46:17 cph Exp $
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

(define-class <umail-url> (<file-url>))
(define make-umail-url (pathname-url-constructor <umail-url>))

(define-pathname-url-predicate <umail-url>
  (lambda (pathname)
    (case (file-type-indirect pathname)
      ((REGULAR)
       (let* ((magic "From ")
	      (n-to-read (string-length magic))
	      (buffer (make-string n-to-read))
	      (n-read
	       (call-with-input-file pathname
		 (lambda (port)
		   (read-string! buffer port)))))
	 (and (fix:= n-to-read n-read)
	      (string=? buffer magic))))
      ((#F) (string=? (pathname-type pathname) "mail"))
      (else #f))))

(define-method make-peer-url ((url <umail-url>) name)
  (make-umail-url
   (merge-pathnames (pathname-default-type name "mail")
		    (directory-pathname (pathname-url-pathname url)))))

;;;; Server operations

(define-method %open-folder ((url <umail-url>))
  (if (not (file-readable? (pathname-url-pathname url)))
      (error:bad-range-argument url 'OPEN-FOLDER))
  (make-umail-folder url))

(define-method %create-folder ((url <umail-url>))
  (if (file-exists? (pathname-url-pathname url))
      (error:bad-range-argument url 'CREATE-FOLDER))
  (let ((folder (make-umail-folder url)))
    (set-file-folder-messages! folder '#())
    (set-file-folder-file-modification-time! folder (get-universal-time))
    (set-file-folder-file-modification-count!
     folder
     (folder-modification-count folder))
    (save-folder folder)))

;;;; Folder

(define-class (<umail-folder> (constructor (url))) (<file-folder>))

;;;; Message

(define-class (<umail-message>
	       (constructor (header-fields body flags from-line)))
    (<file-message>)
  (from-line define accessor))

(define-method umail-message-from-line ((message <message>))
  (string-append "From "
		 (or (let ((from
			    (get-first-header-field-value message "from" #f)))
		       (and from
			    (rfc822:first-address from)))
		     "unknown")
		 " "
		 (universal-time->local-ctime-string
		  (message-internal-time message))))

(define-method make-message-copy ((message <message>) (folder <umail-folder>))
  folder
  (make-umail-message (message-header-fields message)
		      (file-message-body message)
		      (list-copy (message-flags message))
		      (umail-message-from-line message)))

(define-method message-internal-time ((message <umail-message>))
  (or (extract-umail-from-time (umail-message-from-line message))
      (call-next-method message)))

;;;; Read unix mail file

(define-method revert-file-folder ((folder <umail-folder>))
  (read-file-folder-contents folder
    (lambda (port)
      (let ((from-line (read-line port)))
	(if (eof-object? from-line)
	    '#()
	    (begin
	      (if (not (umail-delimiter? from-line))
		  (error "Malformed unix mail file:" port))
	      (let loop ((from-line from-line) (index 0) (messages '()))
		(if (= 0 (remainder index 10))
		    (imail-ui:progress-meter index #f))
		(call-with-values
		    (lambda ()
		      (read-umail-message folder
					  from-line
					  port
					  umail-delimiter?))
		  (lambda (message from-line)
		    (attach-message! message folder index)
		    (let ((messages (cons message messages)))
		      (if from-line
			  (loop from-line (+ index 1) messages)
			  (list->vector (reverse! messages)))))))))))))

(define (read-umail-message folder from-line port delimiter?)
  (let ((h-start (xstring-port/position port)))
    (skip-past-blank-line port)
    (let ((b-start (xstring-port/position port)))
      (let ((finish
	     (lambda (b-end line)
	       (values
		(read-umail-message-1
		 folder
		 from-line
		 (make-file-external-ref h-start (- b-start 1))
		 (make-file-external-ref b-start b-end))
		line))))
	(let loop ()
	  (let ((line (read-line port)))
	    (cond ((eof-object? line)
		   (finish (xstring-port/position port) #f))
		  ((delimiter? line)
		   (finish (- (xstring-port/position port)
			      (+ (string-length line) 1))
			   line))
		  (else
		   (loop)))))))))

(define (read-umail-message-1 folder from-line headers body)
  (call-with-values
      (lambda () (file-folder-strip-internal-headers folder headers))
    (lambda (headers internal-headers)
      (call-with-values
	  (lambda ()
	    (parse-imail-header-fields internal-headers))
	(lambda (internal-headers flags)
	  internal-headers
	  (make-umail-message headers body flags from-line))))))

(define (umail-delimiter? line)
  (re-string-match unix-mail-delimiter line))

;;;; Write unix mail file

(define-method write-file-folder ((folder <umail-folder>) pathname)
  (call-with-binary-output-file pathname
    (lambda (port)
      (for-each-vector-element (file-folder-messages folder)
	(lambda (message)
	  (write-umail-message message #t port))))))

(define-method append-message-to-file ((message <message>) (url <umail-url>))
  (let ((port (open-binary-output-file (pathname-url-pathname url) #t)))
    (write-umail-message message #t port)
    (close-port port)))

(define (write-umail-message message output-flags? port)
  (write-string (umail-message-from-line message) port)
  (newline port)
  (if output-flags?
      (write-header-field (message-flags->header-field (message-flags message))
			  port))
  (write-header-fields (message-header-fields message) port)
  (for-each (lambda (line)
	      (if (string-prefix-ci? "From " line)
		  (write-string ">" port))
	      (write-string line port)
	      (newline port))
	    (string->lines (file-message-body message))))

;;;; Detection of unix "from" lines.

(define (extract-umail-from-time string)
  (let ((regs (re-string-search-forward unix-from-time-regexp string)))
    (and regs
	 (let ((t
		(ignore-errors
		 (lambda ()
		   (ctime-string->universal-time
		    (string-append
		     (re-match-extract string regs 1)
		     " "
		     (re-match-extract string regs 2)
		     " "
		     (re-match-extract string regs 3)
		     " "
		     (re-match-extract string regs 4)
		     " "
		     (re-match-extract string regs 8))
		    (let ((tz1 (re-match-extract string regs 6))
			  (tz2 (re-match-extract string regs 9)))
		      (cond ((not (string-null? tz1)) (string->time-zone tz1))
			    ((not (string-null? tz2)) (string->time-zone tz2))
			    (else #f))))))))
	   (and (not (condition? t))
		t)))))

(define unix-from-time-regexp
  ;; This very complex regular expression taken from Emacs 20.
  (let ((time-zone-regexp
	 (string-append
	  (regexp-group "[A-Z]?[A-Z]?[A-Z][A-Z]\\( DST\\)?"
			"[-+]?[0-9][0-9][0-9][0-9]"
			"")
	  " *")))
    (string-append
     ;; The time the message was sent.
     "\\([^\000-\r \177]+\\) +"				; day of the week
     "\\([^\000-\r \177]+\\) +"				; month
     "\\([0-3]?[0-9]\\) +"				; day of month
     "\\([0-2][0-9]:[0-5][0-9]\\(:[0-6][0-9]\\)?\\) *"	; time of day

     ;; Perhaps a time zone, specified by an abbreviation, or by a
     ;; numeric offset.
     time-zone-regexp

     ;; The year.
     " \\([0-9][0-9]+\\) *"

     ;; On some systems the time zone can appear after the year, too.
     time-zone-regexp

     ;; Old uucp cruft.
     "\\(remote from .*\\)?"

     "$")))

(define unix-mail-delimiter
  ;; This very complex regular expression taken from Emacs 20.
  ;; Many things can happen to an RFC 822 mailbox before it is put into
  ;; a `From' line.  The leading phrase can be stripped, e.g.
  ;; `Joe <@w.x:joe@y.z>' -> `<@w.x:joe@y.z>'.  The <> can be stripped, e.g.
  ;; `<@x.y:joe@y.z>' -> `@x.y:joe@y.z'.  Everything starting with a CRLF
  ;; can be removed, e.g.
  ;;		From: joe@y.z (Joe	K
  ;;			User)
  ;; can yield `From joe@y.z (Joe 	K Fri Mar 22 08:11:15 1996', and
  ;;		From: Joe User
  ;;			<joe@y.z>
  ;; can yield `From Joe User Fri Mar 22 08:11:15 1996'.
  ;; The mailbox can be removed or be replaced by white space, e.g.
  ;;		From: "Joe User"{space}{tab}
  ;;			<joe@y.z>
  ;; can yield `From {space}{tab} Fri Mar 22 08:11:15 1996',
  ;; where {space} and {tab} represent the Ascii space and tab characters.
  ;; We want to match the results of any of these manglings.
  ;; The following regexp rejects names whose first characters are
  ;; obviously bogus, but after that anything goes.
  (string-append "^From \\([^\000-\b\n-\r\177].*\\)? " unix-from-time-regexp))