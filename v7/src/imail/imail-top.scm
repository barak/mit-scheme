;;; -*-Scheme-*-
;;;
;;; $Id: imail-top.scm,v 1.2 2000/01/14 22:43:01 cph Exp $
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

;;;; IMAIL mail reader: top level

(declare (usual-integrations))

(define-variable imail-last-output-url
  "Last URL used by \\[imail-output]."
  "umail:xmail"
  string?)

(define-command imail
  "Read and edit incoming mail.
May be called with an imail folder URL as argument;
 then performs imail editing on that folder,
 but does not copy any new mail into the folder."
  (lambda ()
    (list (and (command-argument)
	       (prompt-for-string "Run imail on folder" #f))))
  (lambda (url-string)
    (bind-authenticator imail-authenticator
      (lambda ()
	(let* ((url
		(->url (or url-string (ref-variable imail-primary-folder))))
	       (folder (open-folder url)))
	  (select-buffer
	   (or (imail-folder->buffer folder)
	       (let ((buffer (new-buffer (imail-url->buffer-name url))))
		 (buffer-put! buffer 'IMAIL-FOLDER folder)
		 (select-message buffer (first-unseen-message-index folder))
		 buffer))))))
    (if (not url-string)
	((ref-command imail-get-new-mail) #f))))

(define (imail-authenticator url)
  (let ((user-name
	 (or (ref-variable imail-user-name)
	     (current-user-name))))
    (values user-name
	    (call-with-pass-phrase
	     (string-append "Password for user "
			    user-name
			    " to access imail folder "
			    (url->string url))
	     string-copy))))

(define (imail-folder->buffer folder)
  (list-search-positive (buffer-list)
    (lambda (buffer)
      (eq? folder (buffer-get buffer 'IMAIL-FOLDER #f)))))

(define (imail-buffer->folder buffer error?)
  (or (buffer-get buffer 'IMAIL-FOLDER #f)
      (and error? (error:bad-range-argument buffer 'IMAIL-BUFFER->FOLDER))))

(define (imail-url->buffer-name url)
  (url-body url))

(define (first-unseen-message-index folder)
  (let ((n (count-messages folder)))
    (let loop ((i 0))
      (if (or (>= i n)
	      (not (message-seen? (get-message folder i))))
	  i
	  (loop (+ i 1))))))

(define-command imail-get-new-mail
  "Get new mail from this folder's inbox."
  ()
  (lambda ()
    (let ((buffer (selected-buffer)))
      (let ((folder (imail-buffer->folder buffer #t)))
	(maybe-revert-folder folder
	  (lambda (folder)
	    (prompt-for-yes-or-no?
	     (string-append
	      "Persistent copy of folder has changed since last read.  "
	      (if (folder-modified? folder)
		  "Discard your changes"
		  "Re-read folder")))))
	(let ((n-new (poll-folder folder)))
	  (cond ((not n-new)
		 (message "(This folder has no associated inbox.)"))
		((= 0 n-new)
		 (message "(No new mail has arrived.)"))
		(else
		 (select-message buffer (- (count-messages folder) n-new))
		 (event-distributor/invoke! (ref-variable imail-new-mail-hook))
		 (message n-new
			  " new message"
			  (if (= n-new 1) "" "s")
			  " read"))))))))

(define-variable imail-new-mail-hook
  "An event distributor that is invoked when IMAIL incorporates new mail."
  (make-event-distributor))

(define (select-message buffer index)
  (if (not (exact-nonnegative-integer? index))
      (error:wrong-type-argument index "exact non-negative integer"
				 'SELECT-MESSAGE))
  (let ((folder (imail-buffer->folder buffer #t)))
    (let ((count (count-messages folder)))
      (let ((index
	     (cond ((< index count) index)
		   ((< 0 count) (- count 1))
		   (else 0))))
	(buffer-reset! buffer)
	(buffer-put! buffer 'IMAIL-INDEX index)
	(let ((mark (mark-left-inserting-copy (buffer-start buffer))))
	  (if (< index count)
	      (let ((message (get-message folder index)))
		(for-each (lambda (line)
			    (insert-string line mark)
			    (insert-newline mark))
			  (let ((displayed
				 (get-message-property
				  message
				  "displayed-header-fields"
				  '())))
			    (if (eq? '() displayed)
				(message-header-fields message)
				displayed)))
		(insert-newline mark)
		(insert-string (message-body message) mark))
	      (insert-string "[This folder has no messages in it.]" mark))
	  (guarantee-newline mark)
	  (mark-temporary! mark))
	(set-buffer-major-mode! buffer (ref-mode-object imail))))))

(define-major-mode imail read-only "IMAIL"
  "IMAIL Mode is used by \\[imail] for editing IMAIL files.
All normal editing commands are turned off.
Instead, these commands are available:

.	Move point to front of this message (same as \\[beginning-of-buffer]).
SPC	Scroll to next screen of this message.
DEL	Scroll to previous screen of this message.
\\[imail-next-undeleted-message]	Move to next non-deleted message.
\\[imail-previous-undeleted-message]	Move to previous non-deleted message.
\\[imail-next-message]	Move to next message whether deleted or not.
\\[imail-previous-message]	Move to previous message whether deleted or not.
\\[imail-last-message]	Move to the last message in folder.
\\[imail-select-message]	Jump to message specified by numeric position in file.
\\[imail-search]	Search for string and show message it is found in.

\\[imail-delete-forward]	Delete this message, move to next nondeleted.
\\[imail-delete-backward]	Delete this message, move to previous nondeleted.
\\[imail-undelete-previous-message]	Undelete message.  Tries current message, then earlier messages
	until a deleted message is found.
\\[imail-expunge]	Expunge deleted messages.
\\[imail-synchronize]	Synchonize the folder with the server.
	For file folders, synchronizes with the file.

\\[imail-quit]       Quit imail: save, then switch to another buffer.

\\[imail-get-new-mail]	Read any new mail from the associated inbox into this folder.

\\[imail-mail]	Mail a message (same as \\[mail-other-window]).
\\[imail-reply]	Reply to this message.  Like \\[imail-mail] but initializes some fields.
\\[imail-forward]	Forward this message to another user.
\\[imail-continue]	Continue composing outgoing message started before.

\\[imail-output]       Output this message to a specified folder (append it).
\\[imail-input]	Append messages from a specified folder.

\\[imail-add-label]	Add label to message.  It will be displayed in the mode line.
\\[imail-kill-label]	Remove a label from current message.
\\[imail-next-labeled-message]	Move to next message with specified label
          (label defaults to last one specified).
          Standard labels:
	    answered, deleted, edited, filed, forwarded, resent, seen.
          Any other label is present only if you add it with `\\[imail-add-label]'.
\\[imail-previous-labeled-message]   Move to previous message with specified label.

\\[imail-summary]	Show headers buffer, with a one line summary of each message.
\\[imail-summary-by-labels]	Like \\[imail-summary] only just messages with particular label(s) are summarized.
\\[imail-summary-by-recipients]   Like \\[imail-summary] only just messages with particular recipient(s) are summarized.

\\[imail-toggle-header]	Toggle between full headers and reduced headers.
	  Normally only reduced headers are shown.
\\[imail-edit-current-message]	Edit the current message.  C-c C-c to return to Rmail."
  (lambda (buffer)
    (local-set-variable! mode-line-modified "--- " buffer)
    (local-set-variable! imail-last-output-url
			 (ref-variable imail-last-output-url buffer)
			 buffer)
    (buffer-put! buffer 'REVERT-BUFFER-METHOD imail-revert-buffer)
    (add-kill-buffer-hook buffer imail-kill-buffer)
    (set-buffer-read-only! buffer)
    (disable-group-undo! (buffer-group buffer))
    (event-distributor/invoke! (ref-variable imail-mode-hook buffer) buffer)))

(define-variable imail-mode-hook
  "An event distributor that is invoked when entering IMAIL mode."
  (make-event-distributor))

(define-key 'imail #\.		'beginning-of-buffer)
(define-key 'imail #\space	'scroll-up)
(define-key 'imail #\rubout	'scroll-down)
(define-key 'imail #\n		'imail-next-undeleted-message)
(define-key 'imail #\p		'imail-previous-undeleted-message)
(define-key 'imail #\m-n	'imail-next-message)
(define-key 'imail #\m-p	'imail-previous-message)
(define-key 'imail #\j		'imail-select-message)
(define-key 'imail #\>		'imail-last-message)

(define-key 'imail #\a		'imail-add-label)
(define-key 'imail #\k		'imail-kill-label)
(define-key 'imail #\c-m-n	'imail-next-labeled-message)
(define-key 'imail #\c-m-p	'imail-previous-labeled-message)

(define-key 'imail #\d		'imail-delete-forward)
(define-key 'imail #\c-d	'imail-delete-backward)
(define-key 'imail #\u		'imail-undelete-previous-message)
(define-key 'imail #\x		'imail-expunge)

(define-key 'imail #\s		'imail-synchronize)
(define-key 'imail #\g		'imail-get-new-mail)

(define-key 'imail #\c-m-h	'imail-summary)
(define-key 'imail #\c-m-l	'imail-summary-by-labels)
(define-key 'imail #\c-m-r	'imail-summary-by-recipients)

(define-key 'imail #\m		'imail-mail)
(define-key 'imail #\r		'imail-reply)
(define-key 'imail #\c		'imail-continue)
(define-key 'imail #\f		'imail-forward)

(define-key 'imail #\t		'imail-toggle-header)
(define-key 'imail #\m-s	'imail-search)
(define-key 'imail #\o		'imail-output)
(define-key 'imail #\i		'imail-input)
(define-key 'imail #\q		'imail-quit)
(define-key 'imail #\?		'describe-mode)
(define-key 'imail #\w		'imail-edit-current-message)

(define-key 'imail-edit '(#\c-c #\c-c)	'imail-cease-edit)
(define-key 'imail-edit '(#\c-c #\c-])	'imail-abort-edit)

(define (imail-revert-buffer buffer dont-use-auto-save? dont-confirm?)
  )

(define (imail-kill-buffer buffer)
  )

(define-command imail-input
  "Append messages to this folder from a specified folder."
  "sInput from imail folder"
  (lambda (url-string)
    ))

(define-command imail-quit
  )

(define-command imail-synchronize
  "Synchronize the current folder with the master copy on the server.
Currently meaningless for file-based folders."
  ()
  (lambda ()
    (synchronize-folder (imail-buffer->folder (selected-buffer) #t))))

;;; Edwin Variables:
;;; scheme-environment: '(edwin)
;;; scheme-syntax-table: edwin-syntax-table
;;; End:
