;;; -*-Scheme-*-
;;;
;;; $Id: imail-imap.scm,v 1.1 2000/04/18 21:44:48 cph Exp $
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

;;;; IMAIL mail reader: IMAP back end

(declare (usual-integrations))

;;;; URL

(define-class (<imap-url>
	       (constructor (user-id auth-type host port mailbox uid)))
    (<url>)
  (user-id define accessor)
  (auth-type define accessor)
  (host define accessor)
  (port define accessor)
  (mailbox define accessor)
  (uid define accessor))

(define-url-protocol "imap" <imap-url>
  (lambda (string)
    (let ((lose (lambda () (error:bad-range-argument string #f))))
      (if (not (string-prefix? "//" string))
	  (lose))
      (let ((end (string-length string)))
	(let ((slash (substring-find-next-char string 2 end)))
	  (if (not slash)
	      (lose))
	  (let ((pv1 (imap:parse:server string 0 slash)))
	    (if (not (and pv1 (fix:= (car pv1) slash)))
		(lose))
	    (let ((pv2 (imap:parse:simple-message string (fix:+ slash 1) end)))
	      (if (not (and pv2 (fix:= (car pv2) end)))
		  (lose))
	      (make-imap-url (parser-token pv1 'USER-ID)
			     (parser-token pv1 'AUTH-TYPE)
			     (parser-token pv1 'HOST)
			     (parser-token pv1 'PORT)
			     (parser-token pv2 'MAILBOX)
			     (parser-token pv2 'UID)))))))))

;;;; Server operations

(define-method %open-folder ((url <imap-url>))
  )

(define-method %new-folder ((url <imap-url>))
  )

(define-method %delete-folder ((url <imap-url>))
  )

(define-method %move-folder ((url <imap-url>) (new-url <imap-url>))
  )

(define-method %copy-folder ((url <imap-url>) (new-url <imap-url>))
  )

(define-method available-folder-names ((url <imap-url>))
  )

(define-method subscribed-folder-names ((url <imap-url>))
  )

;;;; Folder

(define-class (<imap-folder> (constructor (url))) (<folder>)
  (url accessor folder-url)
  )