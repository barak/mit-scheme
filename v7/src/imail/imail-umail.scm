;;; -*-Scheme-*-
;;;
;;; $Id: imail-umail.scm,v 1.2 2000/01/07 23:10:02 cph Exp $
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

(define-class (<umail-url> (constructor (pathname))) (<file-url>))

(define-url-protocol "umail" <umail-url>
  (lambda (string)
    (make-umail-url (short-name->pathname string))))

;;;; Server operations

(define-method %open-folder ((url <umail-url>))
  (read-umail-file url))

(define-method %new-folder ((url <umail-url>))
  (let ((folder (make-umail-folder url '())))
    (save-folder folder)
    folder))

;;;; Folder

(define-class (<umail-folder> (constructor (url messages))) (<file-folder>))

(define-method %write-folder ((folder <folder>) (url <umail-url>))
  (write-umail-file folder url))

(define-method poll-folder ((folder <umail-folder>))
  folder
  #f)

;;;; Read unix mail file

(define (read-umail-file url)
  (let* ((pathname (file-url-pathname url)))
    (call-with-input-file pathname
      (lambda (port)
	(make-umail-folder url (read-umail-messages port))))))

(define (read-umail-messages port)
  (map parse-umail-message
       (burst-list (read-lines port)
		   (lambda (line)
		     (re-string-match unix-mail-delimiter line)))))

(define (parse-umail-message lines)
  (let ((message
	 (let loop ((ls (cdr lines)) (header-lines '()))
	   (if (pair? ls)
	       (if (string-null? (car ls))
		   (make-standard-message
		    (lines->header-fields (reverse! header-lines))
		    (lines->string
		     (map (lambda (line)
			    (if (string-prefix-ci? ">From " line)
				(string-tail line 1)
				line))
			  (cdr ls))))
		   (loop (cdr ls) (cons (car ls) header-lines)))
	       (make-standard-message (reverse! header-lines) "")))))
    (set-message-property message "umail-from-line" (car lines))
    message))

;;;; Write unix mail file

(define (write-umail-file folder url)
  (call-with-output-file (file-url-pathname url)
    (lambda (port)
      (write-umail-messages (file-folder-messages folder) port))))

(define (write-umail-messages messages port)
  (for-each (lambda (message) (write-umail-message message port)) messages))

(define (write-umail-message message port)
  (let ((from-line (get-message-property message "umail-from-line" #f)))
    (if from-line
	(write-string from-line port)
	(begin
	  (write-string "From " port)
	  (write-string (or (let ((from
				   (get-first-header-field-value
				    message "from" #f)))
			      (and from
				   (rfc822-first-address from)))
			    "unknown")
			port)
	  (write-string " " port)
	  (write-string (universal-time->unix-ctime (get-universal-time))
			port))))
  (newline port)
  (write-header-field (message-flags->header-field (message-flags message))
		      port)
  (for-each (lambda (n.v)
	      (if (not (string-ci=? "umail-from-line" (car n.v)))
		  (write-header-field
		   (message-property->header-field (car n.v) (cdr n.v))
		   port)))
	    (message-properties message))
  (write-header-fields (message-header-fields message) port)
  (newline port)
  (for-each (lambda (line)
	      (if (string-prefix-ci? "From " line)
		  (write-string ">" port))
	      (write-string line port)
	      (newline port))
	    (string->lines (message-body message))))

(define (universal-time->unix-ctime time)
  (decoded-time->unix-ctime (universal-time->local-decoded-time time)))

(define (decoded-time->unix-ctime dt)
  (string-append
   (day-of-week/short-string (decoded-time/day-of-week dt))
   " "
   (month/short-string (decoded-time/month dt))
   " "
   (string-pad-left (number->string (decoded-time/day dt)) 2)
   " "
   (string-pad-left (number->string (decoded-time/hour dt)) 2 #\0)
   ":"
   (string-pad-left (number->string (decoded-time/minute dt)) 2 #\0)
   ":"
   (string-pad-left (number->string (decoded-time/second dt)) 2 #\0)
   " "
   (number->string (decoded-time/year dt))))

;;;; Detection of unix "from" lines.

(define unix-mail-delimiter
  ;; This very complex regular expression taken from Emacs 20.
  (let ((time-zone-regexp
	 (string-append
	  (regexp-group "[A-Z]?[A-Z]?[A-Z][A-Z]\\( DST\\)?"
			"[-+]?[0-9][0-9][0-9][0-9]"
			"")
	  " *")))
    (string-append
     "^From "

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
     "\\([^\000-\b\n-\r\177].*\\)? "

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