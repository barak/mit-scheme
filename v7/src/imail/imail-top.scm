;;; -*-Scheme-*-
;;;
;;; $Id: imail-top.scm,v 1.1 2000/01/04 22:51:05 cph Exp $
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

;;;; IMAIL mail reader: top level

(declare (usual-integrations))

(define-command imail
  "Read and edit incoming mail."
  ()
  (lambda ()
    (let ((connection
	   (let ((backend (get-backend (ref-variable imail-backend-type))))
	     (open-connection
	      backend
	      (and (backend-requires-hostname? backend)
		   (ref-variable imail-backend-server))))))
      (authenticate-connection connection
			       (or (ref-variable imail-user-name)
				   (current-user-name))
	(lambda (prompt-string)
	  (call-with-pass-phrase prompt-string string-copy)))
      (let ((name (ref-variable imail-primary-folder)))
	(let ((folder (get-folder connection name)))
	  (let ((buffer
		 (or (imail-folder->buffer folder)
		     (let ((buffer
			    (new-buffer
			     (imail-folder-name->buffer-name name))))
		       (buffer-put! buffer 'IMAIL-FOLDER folder)
		       (select-message
			buffer
			(let ((count (count-messages folder)))
			  (if (= 0 count)
			      count
			      (- count 1))))
		       buffer))))
	    (select-buffer buffer)))))
    ((ref-command imail-get-new-mail) #f)))

(define (imail-folder->buffer folder)
  )

(define (imail-folder-name->buffer-name folder)
  )

(define-command imail-get-new-mail
  "Get new mail from this folder's inbox."
  ()
  (lambda ()
    (let ((buffer (current-buffer)))
      (rmail-find-file-revert buffer)
      (let ((n-messages
	     (let ((memo (buffer-msg-memo buffer)))
	       (if (msg-memo? memo)
		   (msg-memo/number (msg-memo/last memo))
		   0))))
	(with-buffer-open buffer
	  (lambda ()
	    (with-buffer-undo-disabled buffer
	      (lambda ()
		(get-new-mail buffer
			      (ref-variable rmail-inbox-list)
			      #t)))))
	(show-message
	 buffer
	 (let ((memo (buffer-msg-memo buffer)))
	   (cond ((not (msg-memo? memo)) 0)
		 ((> (msg-memo/number (msg-memo/last memo)) n-messages)
		  (+ n-messages 1))
		 (else (msg-memo/number memo)))))
	(event-distributor/invoke! (ref-variable rmail-new-mail-hook))))))