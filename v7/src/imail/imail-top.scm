;;; -*-Scheme-*-
;;;
;;; $Id: imail-top.scm,v 1.185 2000/06/23 19:05:38 cph Exp $
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

(define-variable imail-dont-reply-to-names
  "A regular expression specifying names to prune in replying to messages.
#f means don't reply to yourself."
  #f
  string-or-false?)

(define-variable imail-default-dont-reply-to-names
  "A regular expression specifying part of the value of the default value of
the variable `imail-dont-reply-to-names', for when the user does not set
`imail-dont-reply-to-names' explicitly.  (The other part of the default
value is the user's name.)
It is useful to set this variable in the site customisation file."
  "info-"
  string?)

(define-variable imail-kept-headers
  "A list of regular expressions matching header fields one wants to see.
Headers matching these regexps are shown in the given order,
 and other headers are hidden.
This variable overrides imail-ignored-headers;
 to use imail-ignored-headers, set imail-kept-headers to '()."
  (map (lambda (name) (string-append "^" name "$"))
       '("date" "from" "to" "cc" "subject"))
  (lambda (object) (list-of-type? object string?)))

(define-variable imail-ignored-headers
  "A regular expression matching header fields one would rather not see."
  (regexp-group "via" "mail-from" "origin" "status" "received"
		"[a-z-]*message-id" "summary-line" "errors-to")
  string-or-false?)

(define-variable imail-message-filter
  "If not #f, is a filter procedure for new headers in IMAIL.
The procedure is called with one argument, a list of headers,
 and is expected to return another list of headers.
 Each list element is a pair of two strings, the name and value."
  #f
  (lambda (object) (or (not object) (procedure? object))))

(define-variable imail-delete-after-output
  "True means automatically delete a message that is copied to a file."
  #f
  boolean?)

(define-variable imail-expunge-confirmation
  "Control what kind of confirmation is required for expunging messages.
This variable's value is a list of symbols, as follows:
BRIEF		Use \"y or n\"-style confirmation.
VERBOSE		Use \"yes or no\"-style confirmation.
SHOW-MESSAGES	Pop up window with messages to be expunged."
  '(VERBOSE SHOW-MESSAGES)
  (lambda (x)
    (list-of-type? x (lambda (x) (memq x '(BRIEF VERBOSE SHOW-MESSAGES))))))

(define-variable imail-reply-with-re
  "True means prepend subject with Re: in replies."
  #f
  boolean?)

(define-variable imail-body-cache-limit
  "Size limit for caching of message bodies.
Message bodies (or inline MIME message parts) less than this size are cached.
This variable can also be #T or #F meaning cache/don't cache unconditionally."
  65536
  (lambda (x) (or (boolean? x) (exact-nonnegative-integer? x))))

(define-variable imail-primary-folder
  "URL for the primary folder that you read your mail from."
  #f
  string-or-false?)

(define-variable imail-default-imap-server
  "The hostname of an IMAP server to connect to if none is otherwise specified.
May contain an optional port suffix \":<port>\".
May be overridden by an explicit hostname in imail-primary-folder."
  "localhost"
  string?)

(define-variable imail-default-user-id
  "A user id to use when authenticating to a mail server.
#F means use the id of the user running Edwin.
May be overridden by an explicit user id in imail-primary-folder."
  #f
  string-or-false?)

(define-variable imail-default-imap-mailbox
  "The name of the default mailbox to connect to on an IMAP server,
if none is otherwise specified.
May be overridden by an explicit mailbox in imail-primary-folder."
  "inbox"
  string?)

(define-variable imail-pass-phrase-retention-time
  "The amount of time, in minutes, that IMAIL retains pass phrases.
The pass phrase is deleted if unused for this long.
Set this to zero if you don't want pass-phrase retention."
  30
  exact-nonnegative-integer?)

(define-variable imail-update-interval
  "How often to update a folder's contents, in seconds.
IMAIL will periodically poll the mail server for changes at this interval.
The polls will only occur when there is an open connection to the server;
  it will not reestablish a connection when there is none.
This has no effect on file-based folders.
Set this variable to #F to disable updating."
  600
  (lambda (x) (or (not x) (and (exact-integer? x) (positive? x)))))

(define-variable imail-auto-wrap
  "If true, messages will have their lines wrapped at the right margin.
If set to 'FILL, the paragraphs are filled rather than wrapped.
Otherwise, the text is left as is."
  #t
  (lambda (x) (or (boolean? x) (eq? x 'FILL))))

(define-variable imail-forward-all-headers
  "If true, forwarded email messages will contain all header fields.
Otherwise, only the header fields normally shown by IMAIL are sent.
If value is 'MIME-ONLY, full header fields are sent only when
 imail-forward-using-mime is also true."
  #f
  (lambda (x) (or (boolean? x) (eq? x 'MIME-ONLY))))

(define-variable imail-forward-using-mime
  "If true, forwarded email messages are sent as MIME attachments.
Otherwise, they are inserted into the message body."
  #t
  boolean?)

(define-variable imail-known-mime-charsets
  "List of regular expressions matching character-set names.
Text messages using these character sets are displayed inline;
 when other character sets are used, the text is treated as an attachment."
  (list "us-ascii" "iso-8859-[0-9]+" "windows-[0-9]+")
  list-of-strings?)

(define-variable imail-inline-mime-text-subtypes
  "List of MIME text subtypes that should be shown inline.
The value of this variable is a list of symbols.
A text entity that appears at the top level of a message
 is always shown inline, regardless of its subtype.
Likewise, a text/plain entity is always shown inline.
Note that this variable does not affect subparts of multipart/alternative."
  '(HTML ENRICHED)
  list-of-strings?)

(define-variable imail-use-original-mime-boundaries
  "If true, multipart message parts are separated with MIME boundary strings.
Otherwise, simple dashed-line separators are used."
  #f
  boolean?)

(define-variable imail-mime-attachment-directory
  "Default directory in which to store MIME attachments.
Either #F or a pathname."
  #f
  (lambda (x) (or (not x) (string? x) (pathname? x))))

(define-command imail
  "Read and edit incoming mail.
Given a prefix argument, it prompts for an IMAIL URL,
 then visits the mail folder at that URL.
IMAIL URLs take one of the following forms.

imap://[<user-name>@]<host-name>[:<port>]/<folder-name>
    Specifies a folder on an IMAP server.  The portions in brackets
    are optional and are filled in automatically if omitted.

rmail:<pathname>
    Specifies an RMAIL file.

umail:<pathname>
    Specifies a unix mail file.

You may simultaneously open multiple mail folders.  If you revisit a
folder that is already in a buffer, that buffer is selected.  Messages
may be freely copied from one mail folder to another, regardless of
the type of folder.  Likewise, the available commands are the same
regardless of the folder type."
  (lambda ()
    (list (and (command-argument)
	       (prompt-for-imail-url-string "Run IMAIL on folder" #f
					    'HISTORY 'IMAIL
					    'REQUIRE-MATCH? #t))))
  (lambda (url-string)
    (let ((folder
	   (open-folder
	    (if url-string
		(imail-parse-partial-url url-string)
		(imail-default-url)))))
      (let ((buffer (imail-folder->buffer folder #f)))
	(if buffer
	    (begin
	      (select-buffer buffer)
	      ((ref-command imail-get-new-mail) #f))
	    (begin
	      (let ((buffer
		     (new-buffer
		      (url-presentation-name (folder-url folder)))))
		(associate-imail-with-buffer buffer folder #f)
		(select-buffer buffer))
	      (select-message folder
			      (or (first-unseen-message folder)
				  (selected-message #f))
			      #t)))))))

(define-major-mode imail read-only "IMAIL"
  (lambda ()
    (with-string-output-port
      (lambda (port)
	(write-string imail-mode-description port)
	(newline port)
	(newline port)
	(write-string (make-string 70 #\-) port)
	(newline port)
	(write-string "These variables customize the behavior of IMAIL:" port)
	(newline port)
	(newline port)
	(for-each
	 (let ((buffer (selected-buffer)))
	   (lambda (variable)
	     (let ((name (variable-name-string variable)))
	       (if (not (string-prefix-ci? "imail-summary-" name))
		   (begin
		     (write-string name port)
		     (newline port)
		     (write-string "  " port)
		     (write-description
		      (description-first-line (variable-description variable))
		      port)
		     (newline port)
		     (write-string "  Value: " port)
		     (write (variable-local-value buffer variable) port)
		     (newline port)
		     (newline port))))))
	 (string-table-apropos editor-variables "^imail-"))
	(write-string (make-string 70 #\-) port)
	(newline port)
	(write-string "These are all the key bindings for IMAIL mode:" port)
	(newline port)
	(newline port)
	(write-string "\\{imail}" port))))
  (lambda (buffer)
    (buffer-put! buffer 'REVERT-BUFFER-METHOD imail-revert-buffer)
    (add-kill-buffer-hook buffer imail-kill-buffer)
    (buffer-put! buffer 'MAIL-YANK-ORIGINAL-METHOD imail-yank-original)
    (local-set-variable! mode-line-modified "--- " buffer)
    (imail-adjust-adaptive-fill buffer)
    (standard-alternate-paragraph-style! buffer)
    (set-buffer-read-only! buffer)
    (disable-group-undo! (buffer-group buffer))
    (event-distributor/invoke! (ref-variable imail-mode-hook buffer) buffer)))

(define-variable imail-mode-hook
  "An event distributor that is invoked when entering IMAIL mode."
  (make-event-distributor))

(define (imail-adjust-adaptive-fill buffer)
  (add-adaptive-fill-regexp! "[ \t]*[-a-zA-Z0-9]*>+[ \t]*" buffer))

(define (add-adaptive-fill-regexp! regexp buffer)
  (local-set-variable! adaptive-fill-regexp
		       (string-append regexp "\\|"
				      (ref-variable adaptive-fill-regexp #f))
		       buffer)
  (local-set-variable!
   adaptive-fill-first-line-regexp
   (string-append regexp "\\|"
		  (ref-variable adaptive-fill-first-line-regexp #f))
   buffer))

(define imail-mode-description
  "IMAIL mode is used by \\[imail] for editing mail folders.
All normal editing commands are turned off.
Instead, these commands are available:

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
\\[imail-save-folder]	Save the current folder.

\\[imail-input]	Visit a specified folder in its own buffer.
\\[imail-get-new-mail]	Poll the server for changes.
\\[imail-disconnect]	Disconnect from the server.
\\[imail-quit]       Quit IMAIL: disconnect from server, then switch to another buffer.

\\[imail-mail]	Mail a message (same as \\[mail-other-window]).
\\[imail-reply]	Reply to this message.  Like \\[imail-mail] but initializes some fields.
\\[imail-forward]	Forward this message to another user.
\\[imail-continue]	Continue composing outgoing message started before.

\\[imail-output]       Append this message to a specified folder.
\\[imail-save-attachment]	Save a MIME attachment to a file.

\\[imail-add-flag]	Add flag to message.  It will be displayed in the mode line.
\\[imail-kill-flag]	Remove flag from message.
\\[imail-next-flagged-message]	Move to next message with specified flag
          (flag defaults to last one specified).
          Standard flags:
	    answered, deleted, filed, forwarded, resent, seen.
          Any other flag is present only if you add it with `\\[imail-add-flag]'.
\\[imail-previous-flagged-message]   Move to previous message with specified flag.

\\[imail-create-folder]	Create a new folder.  (Normally not needed as output commands
	  create folders automatically.)
\\[imail-delete-folder]	Delete an existing folder and all its messages.
\\[imail-rename-folder]	Rename a folder.
\\[imail-copy-folder]	Copy all messages from one folder to another.

\\[imail-summary]	Show headers buffer, with a one line summary of each message.
\\[imail-summary-by-flags]	Like \\[imail-summary] only just messages with particular flag(s).
\\[imail-summary-by-recipients]   Like \\[imail-summary] only just messages with particular recipient(s).

\\[imail-toggle-header]	Toggle between full headers and reduced headers.
\\[imail-toggle-message]	Toggle between standard and raw message formats.")

(define (imail-revert-buffer buffer dont-use-auto-save? dont-confirm?)
  dont-use-auto-save?
  (let ((folder (selected-folder #t buffer)))
    (if (let ((status (folder-sync-status folder)))
	  (case status
	    ((UNSYNCHRONIZED)
	     #t)
	    ((SYNCHRONIZED PERSISTENT-MODIFIED)
	     (or dont-confirm?
		 (prompt-for-yes-or-no? "Revert buffer from folder")))
	    ((CACHE-MODIFIED)
	     (prompt-for-yes-or-no? "Discard your changes to folder"))
	    ((BOTH-MODIFIED)
	     (prompt-for-yes-or-no?
	      "Persistent copy of folder changed; discard your changes"))
	    ((PERSISTENT-DELETED)
	     (editor-error "Persistent copy of folder deleted."))
	    (else
	     (error "Unknown folder-sync status:" status))))
	(begin
	  (discard-folder-cache folder)
	  (select-message
	   folder
	   (or (selected-message #f buffer)
	       (first-unseen-message folder))
	   #t)))))

(define (imail-kill-buffer buffer)
  (let ((folder (selected-folder #f buffer)))
    (if folder
	(begin
	  (close-folder folder)
	  (unmemoize-folder (folder-url folder))))))

(define-key 'imail #\a		'imail-add-flag)
(define-key 'imail #\b		'imail-bury)
(define-key 'imail #\c		'imail-continue)
(define-key 'imail #\d		'imail-delete-forward)
(define-key 'imail #\c-d	'imail-delete-backward)
(define-key 'imail #\m-d	'imail-disconnect)
(define-key 'imail #\f		'imail-forward)
(define-key 'imail #\g		'imail-get-new-mail)
(define-key 'imail #\h		'imail-summary)
(define-key 'imail #\i		'imail-input)
(define-key 'imail #\j		'imail-select-message)
(define-key 'imail #\k		'imail-kill-flag)
(define-key 'imail #\l		'imail-summary-by-flags)
(define-key 'imail #\m		'imail-mail)
(define-key 'imail #\n		'imail-next-undeleted-message)
(define-key 'imail #\m-n	'imail-next-message)
(define-key 'imail #\c-m-n	'imail-next-flagged-message)
(define-key 'imail #\o		'imail-output)
(define-key 'imail #\c-o	'imail-save-attachment)
(define-key 'imail #\p		'imail-previous-undeleted-message)
(define-key 'imail #\m-p	'imail-previous-message)
(define-key 'imail #\c-m-p	'imail-previous-flagged-message)
(define-key 'imail #\q		'imail-quit)
(define-key 'imail #\r		'imail-reply)
(define-key 'imail #\s		'imail-save-folder)
(define-key 'imail #\m-s	'imail-search)
(define-key 'imail #\t		'imail-toggle-header)
(define-key 'imail #\c-t	'imail-toggle-message)
(define-key 'imail #\u		'imail-undelete-previous-message)
(define-key 'imail #\m-u	'imail-first-unseen-message)
(define-key 'imail #\x		'imail-expunge)
(define-key 'imail #\.		'beginning-of-buffer)
(define-key 'imail #\<		'imail-first-message)
(define-key 'imail #\>		'imail-last-message)
(define-key 'imail #\space	'scroll-up)
(define-key 'imail #\rubout	'scroll-down)
(define-key 'imail #\?		'describe-mode)
(define-key 'imail '(#\c-c #\c-n)	'imail-next-same-subject)
(define-key 'imail '(#\c-c #\c-p)	'imail-previous-same-subject)

;; Putting these after the group above exploits behavior in the comtab
;; abstraction that makes these bindings the ones that show up during
;; command substitution.
(define-key 'imail #\c-m-f	'imail-summary-by-flags)
(define-key 'imail #\c-m-h	'imail-summary)
(define-key 'imail #\c-m-r	'imail-summary-by-recipients)
(define-key 'imail #\c-m-s	'imail-summary-by-regexp)
(define-key 'imail #\c-m-t	'imail-summary-by-topic)

;; These commands have no equivalent in RMAIL.
(define-key 'imail #\C		'imail-copy-folder)
(define-key 'imail #\D		'imail-delete-folder)
(define-key 'imail #\R		'imail-rename-folder)
(define-key 'imail #\+		'imail-create-folder)

;; These commands not yet implemented.
;;(define-key 'imail #\m-m	'imail-retry-failure)
;;(define-key 'imail #\w		'imail-output-body-to-file)
;;(define-key 'imail '(#\c-c #\c-s #\c-d)	'imail-sort-by-date)
;;(define-key 'imail '(#\c-c #\c-s #\c-s)	'imail-sort-by-subject)
;;(define-key 'imail '(#\c-c #\c-s #\c-a)	'imail-sort-by-author)
;;(define-key 'imail '(#\c-c #\c-s #\c-r)	'imail-sort-by-recipient)
;;(define-key 'imail '(#\c-c #\c-s #\c-c)	'imail-sort-by-correspondent)
;;(define-key 'imail '(#\c-c #\c-s #\c-l)	'imail-sort-by-lines)
;;(define-key 'imail '(#\c-c #\c-s #\c-k)	'imail-sort-by-keywords)

;;;; Navigation

(define-command imail-select-message
  "Show message number N (prefix argument), counting from start of folder."
  "p"
  (lambda (index)
    (let ((folder (selected-folder)))
      (if (not (<= 1 index (folder-length folder)))
	  (editor-error "Message index out of bounds:" index))
      (select-message folder (- index 1)))))

(define-command imail-first-message
  "Show first message in folder."
  ()
  (lambda ()
    (let ((folder (selected-folder)))
      (select-message folder (navigator/first-message folder)))))

(define-command imail-last-message
  "Show last message in folder."
  ()
  (lambda ()
    (let ((folder (selected-folder)))
      (select-message folder (navigator/last-message folder)))))

(define-command imail-first-unseen-message
  "Show first unseen message in folder."
  ()
  (lambda ()
    (let ((folder (selected-folder)))
      (let ((m (navigator/first-unseen-message folder)))
	(if m
	    (select-message folder m)
	    (message "No unseen messages"))))))

(define-command imail-next-message
  "Show following message whether deleted or not.
With prefix argument N, moves forward N messages,
or backward if N is negative."
  "p"
  (lambda (delta)
    (move-relative-any delta #f)))

(define-command imail-previous-message
  "Show previous message whether deleted or not.
With prefix argument N, moves backward N messages,
or forward if N is negative."
  "p"
  (lambda (delta)
    ((ref-command imail-next-message) (- delta))))

(define-command imail-next-undeleted-message
  "Show following non-deleted message.
With prefix argument N, moves forward N non-deleted messages,
or backward if N is negative."
  "p"
  (lambda (delta)
    (move-relative-undeleted delta #f)))

(define-command imail-previous-undeleted-message
  "Show previous non-deleted message.
With prefix argument N, moves backward N non-deleted messages,
or forward if N is negative."
  "p"
  (lambda (delta)
    ((ref-command imail-next-undeleted-message) (- delta))))

(define-command imail-next-same-subject
  "Go to the next mail message having the same subject header.
With prefix argument N, do this N times.
If N is negative, go backwards instead."
  "p"
  (lambda (delta)
    (let ((get-subject
	   (lambda (m)
	     (let ((subject (get-first-header-field-value m "subject" #f)))
	       (and subject
		    (strip-subject-re (string-trim subject)))))))
      (let ((subject (get-subject (selected-message))))
	(if (not subject)
	    (editor-error "Selected message has no subject header."))
	(move-relative delta
		       (lambda (m)
			 (let ((subject* (get-subject m)))
			   (and subject*
				(string-ci=? subject subject*))))
		       "message with same subject"
		       #f)))))

(define-command imail-previous-same-subject
  "Go to the previous mail message having the same subject header.
With prefix argument N, do this N times.
If N is negative, go forwards instead."
  "p"
  (lambda (delta)
    ((ref-command imail-next-same-subject) (- delta))))

(define-command imail-next-flagged-message
  "Show next message with one of the flags FLAGS.
FLAGS should be a comma-separated list of flag names.
If FLAGS is empty, the last set of flags specified is used.
With prefix argument N moves forward N messages with these flags."
  (lambda ()
    (list (command-argument-numeric-value (command-argument))
	  (imail-prompt-for-flags "Move to next message with flags")))
  (lambda (delta flags)
    (let ((flags (burst-comma-list-string flags)))
      (if (null? flags)
	  (editor-error "No flags have been specified."))
      (for-each (lambda (flag)
		  (if (not (message-flag? flag))
		      (error "Invalid flag name:" flag)))
		flags)
      (move-relative delta
		     (lambda (message)
		       (there-exists? flags
			 (lambda (flag)
			   (message-flagged? message flag))))
		     (string-append "message with flag"
				    (if (= 1 (length flags)) "" "s")
				    " "
				    (decorated-string-append "" ", " "" flags))
		     #f))))

(define-command imail-previous-flagged-message
  "Show previous message with one of the flags FLAGS.
FLAGS should be a comma-separated list of flag names.
If FLAGS is empty, the last set of flags specified is used.
With prefix argument N moves backward N messages with these flags."
  (lambda ()
    (list (command-argument-numeric-value (command-argument))
	  (imail-prompt-for-flags "Move to previous message with flags")))
  (lambda (delta flags)
    ((ref-command imail-next-flagged-message) (- delta) flags)))

(define (imail-prompt-for-flags prompt)
  (prompt-for-string prompt
		     #f
		     'DEFAULT-TYPE 'INSERTED-DEFAULT
		     'HISTORY 'IMAIL-PROMPT-FOR-FLAGS
		     'HISTORY-INDEX 0))

;;;; Message deletion

(define-command imail-delete-message
  "Delete this message and stay on it."
  ()
  (lambda ()
    (delete-message (selected-message))))

(define-command imail-delete-forward
  "Delete this message and move to next nondeleted one.
With prefix argument N, deletes forward N messages,
 or backward if N is negative.
Deleted messages stay in the file until the \\[imail-expunge] command is given."
  "p"
  (lambda (delta)
    (move-relative-undeleted delta delete-message)))

(define-command imail-delete-backward
  "Delete this message and move to previous nondeleted one.
With prefix argument N, deletes backward N messages,
 or forward if N is negative.
Deleted messages stay in the file until the \\[imail-expunge] command is given."
  "p"
  (lambda (delta)
    ((ref-command imail-delete-forward) (- delta))))

(define-command imail-undelete-previous-message
  "Back up to deleted message, select it, and undelete it."
  ()
  (lambda ()
    (let ((message (selected-message)))
      (if (message-deleted? message)
	  (undelete-message message)
	  (let ((message
		 (navigator/previous-message message message-deleted?)))
	    (if (not message)
		(editor-error "No previous deleted message."))
	    (undelete-message message)
	    (select-message (message-folder message) message))))))

(define-command imail-undelete-forward
  "Undelete this message and move to next one.
With prefix argument N, undeletes forward N messages,
 or backward if N is negative."
  "p"
  (lambda (delta) (move-relative-any delta undelete-message)))

(define-command imail-undelete-backward
  "Undelete this message and move to previous one.
With prefix argument N, undeletes backward N messages,
 or forward if N is negative."
  "p"
  (lambda (delta) ((ref-command imail-undelete-forward) (- delta))))

(define-command imail-expunge
  "Actually erase all deleted messages in the folder."
  ()
  (lambda ()
    (let ((folder (selected-folder)))
      (let ((n (count-messages folder message-deleted?)))
	(cond ((= n 0)
	       (message "No messages to expunge"))
	      ((let ((confirmation (ref-variable imail-expunge-confirmation)))
		 (or (null? confirmation)
		     (let ((prompt
			    (string-append "Expunge "
					   (number->string n)
					   " message"
					   (if (> n 1) "s" "")
					   " marked for deletion")))
		       (let ((do-prompt
			      (lambda ()
				(if (memq 'BRIEF confirmation)
				    (prompt-for-confirmation? prompt)
				    (prompt-for-yes-or-no? prompt)))))
			 (if (memq 'SHOW-MESSAGES confirmation)
			     (cleanup-pop-up-buffers
			      (lambda ()
				(imail-expunge-pop-up-messages folder)
				(do-prompt)))
			     (do-prompt))))))
	       (let ((message (selected-message)))
		 (if (message-deleted? message)
		     (select-message
		      folder
		      (or (next-message message message-undeleted?)
			  (previous-message message message-undeleted?)
			  (next-message message)
			  (previous-message message)))))
	       (expunge-deleted-messages folder))
	      (else
	       (message "Messages not expunged")))))))

(define (count-messages folder predicate)
  (let ((n (folder-length folder)))
    (do ((i 0 (+ i 1))
	 (k 0 (if (predicate (get-message folder i)) (+ k 1) k)))
	((= i n) k))))

(define (imail-expunge-pop-up-messages folder)
  (pop-up-temporary-buffer " *imail-message*" '(READ-ONLY SHRINK-WINDOW)
    (lambda (buffer window)
      window
      (local-set-variable! truncate-lines #t buffer)
      (let ((mark (mark-left-inserting-copy (buffer-point buffer)))
	    (n (folder-length folder)))
	(let ((index-digits (exact-nonnegative-integer-digits (- n 1))))
	  (do ((i 0 (+ i 1)))
	      ((= i n))
	    (let ((m (get-message folder i)))
	      (if (message-deleted? m)
		  (write-imail-summary-line! m index-digits mark)))))))))

;;;; Message flags

(define-command imail-add-flag
  "Add FLAG to flags associated with current IMAIL message.
Completion is performed over known flags when reading.
With prefix argument N, adds FLAG to next N messages,
 or previous -N if N is negative."
  (lambda ()
    (list (command-argument)
	  (imail-read-flag "Add flag" #f)))
  (lambda (argument flag)
    (move-relative-any argument
		       (lambda (message) (set-message-flag message flag)))))

(define-command imail-kill-flag
  "Remove FLAG from flags associated with current IMAIL message.
Completion is performed over known flags when reading.
With prefix argument N, removes FLAG from next N messages,
 or previous -N if N is negative."
  (lambda ()
    (list (command-argument)
	  (imail-read-flag "Remove flag" #t)))
  (lambda (argument flag)
    (move-relative-any argument
		       (lambda (message) (clear-message-flag message flag)))))

(define (imail-read-flag prompt require-match?)
  (prompt-for-string-table-name
   prompt #f
   (alist->string-table
    (map list
	 (remove-duplicates (append standard-message-flags
				    (folder-flags (selected-folder)))
			    string=?)))
   'DEFAULT-TYPE 'INSERTED-DEFAULT
   'HISTORY 'IMAIL-READ-FLAG
   'REQUIRE-MATCH? require-match?))

;;;; Message I/O

(define-command imail-input-from-folder
  "Append messages to this folder from a specified folder."
  (lambda ()
    (list (prompt-for-imail-url-string "Get messages from folder" #f
				       'HISTORY 'IMAIL-INPUT-FROM-FOLDER
				       'HISTORY-INDEX 0
				       'REQUIRE-MATCH? #t)))
  (lambda (url-string)
    (let ((url (imail-parse-partial-url url-string)))
      (copy-folder (open-folder url)
		   (folder-url (selected-folder))
		   (lambda () ((ref-command imail-get-new-mail) #f))
		   (string-append "from " (url->string url))))))

(define-command imail-output
  "Append this message to a specified folder."
  (lambda ()
    (list (prompt-for-imail-url-string "Output to folder" #f
				       'HISTORY 'IMAIL-OUTPUT
				       'HISTORY-INDEX 0)
	  (command-argument)))
  (lambda (url-string argument)
    (let ((url (imail-parse-partial-url url-string))
	  (delete? (ref-variable imail-delete-after-output)))
      (move-relative-undeleted (or argument (and delete? 1))
	(lambda (message)
	  (append-message message url)
	  (message-filed message)
	  (if delete? (delete-message message))))
      (let ((n (if argument (command-argument-numeric-value argument) 1)))
	(message (number->string n)
		 " message"
		 (if (= n 1) "" "s")
		 " written to "
		 (url->string url))))))

;;;; Attachments

(define-command imail-save-attachment
  "Save the attachment at point.
If point is not on an attachment, prompts for the attachment to save.
With prefix argument, prompt even when point is on an attachment."
  "P"
  (lambda (always-prompt?)
    (let ((attachment
	   (maybe-prompt-for-mime-attachment (current-point) always-prompt?)))
      (save-mime-attachment (car attachment)
			    (cdr attachment)
			    (selected-message)
			    (selected-buffer)))))

(define (maybe-prompt-for-mime-attachment mark always-prompt?)
  (let ((attachment (mark-mime-attachment mark)))
    (if (and attachment (not always-prompt?))
	attachment
	(let ((attachments (buffer-mime-attachments (mark-buffer mark))))
	  (if (null? attachments)
	      (editor-error "This message has no attachments."))
	  (let ((alist
		 (uniquify-mime-attachment-names
		  (map (lambda (b.s)
			 (cons (mime-attachment-name (car b.s) (cdr b.s) #t)
			       b.s))
		       attachments))))
	    (prompt-for-alist-value "Save attachment"
				    alist
				    (and attachment
					 (let ((entry
						(list-search-positive alist
						  (lambda (entry)
						    (eq? (cdr entry)
							 attachment)))))
					   (and entry
						(car entry))))
				    #f))))))

(define (uniquify-mime-attachment-names alist)
  (let loop ((alist alist) (converted '()))
    (if (pair? alist)
	(loop (cdr alist)
	      (cons (cons (let ((name (caar alist)))
			    (let loop ((name* name) (n 1))
			      (if (there-exists? converted
				    (lambda (entry)
				      (string=? (car entry) name*)))
				  (loop (string-append
					 name "<" (number->string n) ">")
					(+ n 1))
				  name*)))
			  (cdar alist))
		    converted))
	(reverse! converted))))

(define (save-mime-attachment body selector message buffer)
  (let ((filename
	 (let ((history 'IMAIL-SAVE-ATTACHMENT))
	   (prompt-for-file
	    "Save attachment as"
	    (let ((filename
		   (let ((filename (mime-body-disposition-filename body)))
		     (and filename
			  (filter-mime-attachment-filename filename)))))
	      (and filename
		   (list
		    (merge-pathnames
		     filename
		     (let ((pathname
			    (ref-variable imail-mime-attachment-directory
					  buffer)))
		       (if pathname
			   (pathname-as-directory pathname)
			   (let ((filenames (prompt-history-strings history)))
			     (if (pair? filenames)
				 (directory-pathname (car filenames))
				 (buffer-default-directory buffer)))))))))
	    'HISTORY history)))
	(text?
	 (let ((type (mime-body-type body)))
	   (or (eq? type 'TEXT)
	       (eq? type 'MESSAGE)))))
    (if (or (not (file-exists? filename))
	    (prompt-for-yes-or-no? "File already exists; overwrite"))
	((if text? call-with-output-file call-with-binary-output-file)
	 filename
	 (lambda (port)
	   (call-with-mime-decoding-output-port
	    (mime-body-one-part-encoding body)
	    port
	    text?
	    (lambda (port)
	      (write-mime-message-body-part message selector #f port))))))))

(define (filter-mime-attachment-filename filename)
  (let ((filename
	 (let ((index
		(string-find-previous-char-in-set
		 filename
		 char-set:mime-attachment-filename-delimiters)))
	   (if index
	       (string-tail filename (+ index 1))
	       filename))))
    (and (not (string-find-next-char-in-set
	       filename
	       char-set:rejected-mime-attachment-filename))
	 (if (eq? microcode-id/operating-system 'UNIX)
	     (string-replace filename #\space #\_)
	     filename))))

(define char-set:mime-attachment-filename-delimiters
  (char-set #\/ #\\ #\:))

(define char-set:rejected-mime-attachment-filename
  (char-set-invert
   (char-set-difference char-set:graphic
			char-set:mime-attachment-filename-delimiters)))

;;;; Sending mail

(define-command imail-mail
  "Send mail in another window.
While composing the message, use \\[mail-yank-original] to yank the
original message into it."
  ()
  (lambda ()
    (make-mail-buffer '(("To" "") ("Subject" ""))
		      (chase-imail-buffer (selected-buffer))
		      (lambda (mail-buffer)
			(initialize-imail-mail-buffer mail-buffer)
			(select-buffer-other-window mail-buffer)))))

(define-command imail-reply
  "Reply to the current message.
Normally include CC: to all other recipients of original message;
 prefix argument means ignore them.
While composing the reply, use \\[mail-yank-original] to yank the
 original message into it."
  "P"
  (lambda (just-sender?)
    (let ((message (selected-message)))
      (make-mail-buffer (imail-reply-headers message (not just-sender?))
			(chase-imail-buffer (selected-buffer))
			(lambda (mail-buffer)
			  (initialize-imail-mail-buffer mail-buffer)
			  (message-answered message)
			  (select-buffer-other-window mail-buffer))))))

(define-command imail-continue
  "Continue composing outgoing message previously being composed."
  ()
  (lambda () ((ref-command mail-other-window) #t)))

;; This procedure is invoked by M-x mail-yank-original in Mail mode.
(define (imail-yank-original buffer left-margin mark)
  (insert-message (selected-message #t buffer) #t left-margin mark))

(define (initialize-imail-mail-buffer buffer)
  (imail-adjust-adaptive-fill buffer)
  (buffer-put! buffer 'MAILER-VERSION-STRING imail-mailer-version-string))

(define (imail-mailer-version-string generic)
  (string-append "IMAIL/" (get-subsystem-version-string "IMAIL")
		 "; " generic))

(define-command imail-forward
  "Forward the current message to another user.
With prefix argument, \"resend\" the message instead of forwarding it;
see the documentation of `imail-resend'."
  "P"
  (lambda (resend?)
    (if resend?
	(dispatch-on-command (ref-command-object imail-resend))
	(imail-forward))))

(define (imail-forward)
  (let ((message (selected-message)))
    (make-mail-buffer
     `(("To" "")
       ("Subject"
	,(string-append
	  "["
	  (let ((from (get-first-header-field-value message "from" #f)))
	    (if from
		(rfc822:canonicalize-address-string from)
		""))
	  ": "
	  (message-subject message)
	  "]")))
     #f
     (lambda (mail-buffer)
       (initialize-imail-mail-buffer mail-buffer)
       (let ((raw? (ref-variable imail-forward-all-headers mail-buffer)))
	 (if (ref-variable imail-forward-using-mime mail-buffer)
	     (add-buffer-mime-attachment!
	      mail-buffer
	      'MESSAGE 'RFC822 '() '(INLINE)
	      (map header-field->mail-header
		   (let ((headers (message-header-fields message)))
		     (if raw?
			 headers
			 (maybe-reformat-headers headers mail-buffer))))
	      (lambda (port) (write-message-body message port)))
	     (let ((mark (mark-left-inserting-copy (buffer-end mail-buffer))))
	       (with-buffer-point-preserved mail-buffer
		 (lambda ()
		   (insert-header-fields message (eq? raw? #t) mark)
		   (insert-message-body message mark)))
	       (mark-temporary! mark))))
       (if (window-has-no-neighbors? (current-window))
	   (select-buffer mail-buffer)
	   (select-buffer-other-window mail-buffer))
       (message-forwarded message)))))

(define-command imail-resend
  "Resend current message to ADDRESSES.
ADDRESSES is a string consisting of several addresses separated by commas."
  "sResend to"
  (lambda (addresses)
    (let ((buffer (selected-buffer))
	  (message (selected-message)))
      (make-mail-buffer
       `(("Resent-From" ,(mail-from-string buffer))
	 ("Resent-Date" ,(universal-time->string (get-universal-time)))
	 ("Resent-To" ,addresses)
	 ,@(if (ref-variable mail-self-blind buffer)
	       `(("Resent-Bcc" ,(mail-from-string buffer)))
	       '())
	 ,@(map header-field->mail-header
		(list-transform-negative (message-header-fields message)
		  (lambda (header)
		    (string-ci=? (header-field-name header) "sender")))))
       #f
       (lambda (mail-buffer)
	 (initialize-imail-mail-buffer mail-buffer)
	 (with-buffer-point-preserved mail-buffer
	   (lambda ()
	     (insert-message-body message (buffer-end mail-buffer))))
	 (disable-buffer-mime-processing! mail-buffer)
	 (if (window-has-no-neighbors? (current-window))
	     (select-buffer mail-buffer)
	     (select-buffer-other-window mail-buffer))
	 (message-resent message))))))

(define (imail-reply-headers message cc?)
  (let ((resent-reply-to
	 (get-last-header-field-value message "resent-reply-to" #f))
	(from (get-first-header-field-value message "from" #f))
	(concat
	 (lambda (strings)
	   (and (pair? strings)
		(decorated-string-append "" ", " "" strings)))))
    `(("To"
       ,(rfc822:canonicalize-address-string
	 (or resent-reply-to
	     (concat (get-all-header-field-values message "reply-to"))
	     from)))
      ("CC"
       ,(and cc?
	     (let ((to
		    (if resent-reply-to
			(get-last-header-field-value message "resent-to" #f)
			(concat (get-all-header-field-values message "to"))))
		   (cc
		    (if resent-reply-to
			(get-last-header-field-value message "resent-cc" #f)
			(concat (get-all-header-field-values message "cc")))))
	       (let ((cc
		      (if (and to cc)
			  (string-append to ", " cc)
			  (or to cc))))
		 (and cc
		      (let ((addresses
			     (imail-dont-reply-to
			      (rfc822:string->addresses cc))))
			(and (pair? addresses)
			     (rfc822:addresses->string addresses))))))))
      ("In-reply-to"
       ,(if resent-reply-to
	    (make-in-reply-to-field
	     from
	     (get-last-header-field-value message "resent-date" #f)
	     (get-last-header-field-value message "resent-message-id" #f))
	    (make-in-reply-to-field
	     from
	     (get-first-header-field-value message "date" #f)
	     (get-first-header-field-value message "message-id" #f))))
      ("Subject"
       ,(let ((subject
	       (or (and resent-reply-to
			(let ((subject
			       (get-last-header-field-value message
							    "resent-subject"
							    #f)))
			  (and subject
			       (strip-subject-re subject))))
		   (message-subject message))))
	  (if (ref-variable imail-reply-with-re)
	      (string-append "Re: " subject)
	      subject))))))

(define (imail-dont-reply-to addresses)
  (let ((pattern
	 (re-compile-pattern
	  (string-append (regexp-group ".*!" "")
			 (regexp-group (imail-dont-reply-to-names)))
	  #t)))
    (let loop ((addresses addresses))
      (if (pair? addresses)
	  (if (re-string-match pattern (car addresses))
	      (loop (cdr addresses))
	      (cons (car addresses) (loop (cdr addresses))))
	  '()))))

(define (imail-dont-reply-to-names)
  (or (ref-variable imail-dont-reply-to-names #f)
      (let ((regexp
	     (string-append
	      (let ((r (ref-variable imail-default-dont-reply-to-names #f)))
		(if r
		    (string-append r "\\|")
		    ""))
	      (re-quote-string (current-user-name))
	      "\\>")))
	(set-variable! imail-dont-reply-to-names regexp #f)
	regexp)))

(define (message-subject message)
  (let ((subject (get-first-header-field-value message "subject" #f)))
    (if subject
	(strip-subject-re subject)
	"")))

(define (strip-subject-re subject)
  (if (string-prefix-ci? "re:" subject)
      (strip-subject-re (string-trim-left (string-tail subject 3)))
      subject))

(define (header-field->mail-header header)
  (list (header-field-name header)
	(let ((v (header-field-value header)))
	  (if (string-prefix? " " v)
	      (string-tail v 1)
	      v))))

(define (with-buffer-point-preserved buffer thunk)
  (let ((point (mark-right-inserting-copy (buffer-point buffer))))
    (let ((value (thunk)))
      (set-buffer-point! buffer point)
      (mark-temporary! point)
      value)))

;;;; Folder Operations

(define-command imail-create-folder
  "Create a new folder with the specified name.
An error if signalled if the folder already exists."
  (lambda ()
    (list (prompt-for-imail-url-string "Create folder" #f
				       'HISTORY 'IMAIL-CREATE-FOLDER)))
  (lambda (url-string)
    (let ((url (imail-parse-partial-url url-string)))
      (create-folder url)
      (message "Created folder " (url->string url)))))

(define-command imail-delete-folder
  "Delete a specified folder and all its messages."
  (lambda ()
    (list (prompt-for-imail-url-string "Delete folder" #f
				       'HISTORY 'IMAIL-DELETE-FOLDER
				       'REQUIRE-MATCH? #t)))
  (lambda (url-string)
    (let ((url (imail-parse-partial-url url-string)))
      (if (prompt-for-yes-or-no?
	   (string-append "Delete folder " (url->string url)))
	  (begin
	    (delete-folder url)
	    (message "Deleted folder " (url->string url)))
	  (message "Folder not deleted")))))

(define-command imail-rename-folder
  "Rename a folder.
May only rename a folder to a new name on the same server or file system.
The folder's type may not be changed."
  (lambda ()
    (let ((from
	   (prompt-for-imail-url-string "Rename folder" #f
					'HISTORY 'IMAIL-RENAME-FOLDER-SOURCE
					'HISTORY-INDEX 0
					'REQUIRE-MATCH? #t)))
      (list from
	    (prompt-for-imail-url-string
	     "Rename folder to"
	     (url-container-string (imail-parse-partial-url from))
	     'HISTORY 'IMAIL-RENAME-FOLDER-TARGET))))
  (lambda (from to)
    (let ((from (imail-parse-partial-url from))
	  (to (imail-parse-partial-url to)))
      (rename-folder from to)
      (message "Folder renamed to " (url->string to)))))

(define-command imail-copy-folder
  "Copy all messages from a specified folder to another folder.
If the target folder exists, the messages are appended to it.
If it doesn't exist, it is created first."
  (lambda ()
    (let ((from
	   (prompt-for-imail-url-string "Copy folder" #f
					'HISTORY 'IMAIL-COPY-FOLDER-SOURCE
					'HISTORY-INDEX 0
					'REQUIRE-MATCH? #t)))
      (list from
	    (prompt-for-imail-url-string
	     "Copy messages to folder"
	     (make-peer-url
	      (let ((history
		     (prompt-history-strings 'IMAIL-COPY-FOLDER-TARGET)))
		(if (pair? history)
		    (imail-parse-partial-url (car history))
		    (imail-default-url)))
	      (url-base-name (imail-parse-partial-url from)))
	     'HISTORY 'IMAIL-COPY-FOLDER-TARGET))))
  (lambda (from to)
    (let ((folder (open-folder (imail-parse-partial-url from)))
	  (to (imail-parse-partial-url to)))
      (with-open-connection to
	(lambda ()
	  (copy-folder folder to #f
		       (string-append "to " (url->string to))))))))

(define (copy-folder folder to refresh reference-string)
  (let ((n (folder-length folder)))
    (do ((i 0 (+ i 1)))
	((= i n))
      ((message-wrapper #f
			"Copying message "
			(number->string (+ i 1))
			"/"
			(number->string n))
       (lambda () (append-message (get-message folder i) to))))
    (if refresh (refresh))
    (message (number->string n)
	     " message"
	     (if (= n 1) "" "s")
	     " copied "
	     reference-string)))

;;;; Miscellany

(define-command imail-quit
  "Quit out of IMAIL."
  ()
  (lambda ()
    (let ((folder (selected-folder)))
      (close-folder folder)
      (imail-bury folder))))

(define-command imail-bury
  "Bury current IMAIL buffer and its summary buffer."
  ()
  (lambda ()
    (imail-bury (selected-folder))))

(define (imail-bury folder)
  (let ((folder-buffer (imail-folder->buffer folder #t)))
    (for-each
     (lambda (buffer)
       (if (buffer-alive? buffer)
	   (let ((buffer* (other-buffer buffer)))
	     (for-each (lambda (window)
			 (if (window-has-no-neighbors? window)
			     (if buffer*
				 (select-buffer-in-window buffer* window #f))
			     (window-delete! window)))
		       (buffer-windows buffer))
	     (bury-buffer buffer))))
     (buffer-get folder-buffer 'IMAIL-ASSOCIATED-BUFFERS '()))
    (let ((buffer (other-buffer folder-buffer)))
      (if buffer
	  (for-each (lambda (window)
		      (select-buffer-in-window buffer window #f))
		    (buffer-windows folder-buffer))))
    (bury-buffer folder-buffer)))

(define-command imail-input
  "Run IMAIL on a specified folder."
  (lambda ()
    (list (prompt-for-imail-url-string "Run IMAIL on folder" #f
				       'HISTORY 'IMAIL
				       'REQUIRE-MATCH? #t)))
  (lambda (url-string)
    ((ref-command imail) url-string)))

(define-command imail-save-folder
  "Save the currently selected IMAIL folder."
  ()
  (lambda ()
    (message
     (if (save-folder (selected-folder))
	 "Folder saved"
	 "No changes need to be saved."))))

(define-command imail-toggle-header
  "Show full message headers if pruned headers currently shown, or vice versa."
  ()
  (lambda ()
    (let ((message (selected-message)))
      (select-message (selected-folder)
		      message
		      #t
		      (if (get-property message 'RAW? #f) #f 'FULL-HEADERS)))))

(define-command imail-toggle-message
  "Toggle between standard and raw formats for message."
  ()
  (lambda ()
    (let ((message (selected-message)))
      (select-message (selected-folder)
		      message
		      #t
		      (not (get-property message 'RAW? #f))))))

(define-command imail-get-new-mail
  "Probe the mail server for new mail.
Selects the first new message if any new mail.
 (Currently useful only for IMAP folders.)

You can also specify another folder to get mail from.
A prefix argument says to prompt for a URL and append all messages
 from that folder to the current one."
  (lambda ()
    (list (and (command-argument)
	       (prompt-for-imail-url-string "Get messages from folder" #f
					    'HISTORY 'IMAIL-INPUT
					    'HISTORY-INDEX 0
					    'REQUIRE-MATCH? #t))))
  (lambda (url-string)
    (if url-string
	((ref-command imail-input-from-folder) url-string)
	(let* ((folder (selected-folder))
	       (count (folder-modification-count folder)))
	  (probe-folder folder)
	  (cond ((<= (folder-modification-count folder) count)
		 (message "No changes to mail folder"))
		((navigator/first-unseen-message folder)
		 => (lambda (unseen) (select-message folder unseen)))
		((selected-message #f)
		 (message "No unseen messages"))
		((navigator/last-message folder)
		 => (lambda (first) (select-message folder first)))
		(else
		 (message "No changes to mail folder")))))))

(define-command imail-disconnect
  "Disconnect the selected IMAIL folder from its server.
Has no effect on non-server-based folders."
  ()
  (lambda ()
    (disconnect-folder (selected-folder))))

(define-command imail-search
  "Show message containing next match for given string.
Negative argument means search in reverse."
  (lambda ()
    (let ((reverse? (< (command-argument-numeric-value (command-argument)) 0)))
      (list (prompt-for-string (string-append (if reverse? "Reverse " "")
					      "IMAIL search")
			       #f
			       'DEFAULT-TYPE 'INSERTED-DEFAULT
			       'HISTORY 'IMAIL-SEARCH
			       'HISTORY-INDEX 0)
	    reverse?)))
  (lambda (pattern reverse?)
    (let ((folder (selected-folder))
	  (msg
	   (string-append (if reverse? "Reverse " "")
			  "IMAIL search for " pattern "...")))
      (message msg)
      (let ((index
	     (let ((index (message-index (selected-message))))
	       (let loop
		   ((indexes
		     (let ((indexes (search-folder folder pattern)))
		       (if reverse?
			   (reverse indexes)
			   indexes))))
		 (and (pair? indexes)
		      (if (if reverse?
			      (< (car indexes) index)
			      (> (car indexes) index))
			  (car indexes)
			  (loop (cdr indexes))))))))
	(if index
	    (begin
	      (select-message folder index)
	      (message msg "done"))
	    (editor-failure "Search failed: " pattern))))))

;;;; URLs

(define (imail-default-url)
  (let ((primary-folder (ref-variable imail-primary-folder #f)))
    (if primary-folder
	(imail-parse-partial-url primary-folder)
	(imail-get-default-url #f))))

(define (imail-parse-partial-url string)
  (parse-url-string string imail-get-default-url))

(define (imail-get-default-url protocol)
  (cond ((not protocol)
	 (let ((folder
		(buffer-get (chase-imail-buffer (selected-buffer))
			    'IMAIL-FOLDER
			    #f)))
	   (if folder
	       (folder-url folder)
	       (imail-get-default-url "imap"))))
	((string-ci=? protocol "imap")
	 (call-with-values
	     (lambda ()
	       (let ((server (ref-variable imail-default-imap-server #f)))
		 (let ((colon (string-find-next-char server #\:)))
		   (if colon
		       (values
			(string-head server colon)
			(or (string->number (string-tail server (+ colon 1)))
			    (error "Invalid port specification:" server)))
		       (values server 143)))))
	   (lambda (host port)
	     (make-imap-url (or (ref-variable imail-default-user-id #f)
				(current-user-name))
			    host
			    port
			    (ref-variable imail-default-imap-mailbox
					  #f)))))
	((string-ci=? protocol "rmail") (make-rmail-url "~/RMAIL"))
	((string-ci=? protocol "umail") (make-umail-url "~/inbox.mail"))
	(else (error:bad-range-argument protocol))))

(define (prompt-for-imail-url-string prompt default . options)
  (let ((get-option
	 (lambda (key)
	   (let loop ((options options))
	     (and (pair? options)
		  (pair? (cdr options))
		  (if (eq? (car options) key)
		      (cadr options)
		      (loop (cddr options)))))))
	(default
	  (cond ((string? default) default)
		((url? default) (url->string default))
		((not default) (url-container-string (imail-default-url)))
		(else (error "Illegal default:" default)))))
    (let ((history (get-option 'HISTORY)))
      (if (null? (prompt-history-strings history))
	  (set-prompt-history-strings! history (list default))))
    (apply prompt-for-completed-string
	   prompt
	   (if (= (or (get-option 'HISTORY-INDEX) -1) -1) default #f)
	   (lambda (string if-unique if-not-unique if-not-found)
	     (url-complete-string string imail-get-default-url
				  if-unique if-not-unique if-not-found))
	   (lambda (string)
	     (url-string-completions string imail-get-default-url))
	   (lambda (string)
	     (let ((url
		    (ignore-errors
		     (lambda ()
		       (imail-parse-partial-url string)))))
	       (and (url? url)
		    (url-exists? url))))
	   'DEFAULT-TYPE 'INSERTED-DEFAULT
	   options)))

;;;; Core interface to front end

;;; The mailer core abstraction, which otherwise doesn't know about
;;; the presentation layer, occasionally needs some presentation
;;; services.  The hooks in this section provide them.

(define (imail-ui:present-user-alert procedure)
  (call-with-output-to-temporary-buffer " *IMAP alert*"
					'(READ-ONLY SHRINK-WINDOW
						    FLUSH-ON-SPACE)
					procedure))

(define (imail-ui:message-wrapper . arguments)
  (let ((prefix (string-append (message-args->string arguments) "...")))
    (lambda (thunk)
      (fluid-let ((*imail-message-wrapper-prefix* prefix))
	(message prefix)
	(let ((v (thunk)))
	  (message prefix "done")
	  v)))))

(define (imail-ui:progress-meter current total)
  (if (and *imail-message-wrapper-prefix* (< 0 current total))
      (message *imail-message-wrapper-prefix*
	       (string-pad-left
		(number->string (round->exact (* (/ current total) 100)))
		3)
	       "% (of "
	       (number->string total)
	       ")")))

(define *imail-message-wrapper-prefix* #f)

(define imail-ui:message message)
(define imail-ui:prompt-for-yes-or-no? prompt-for-yes-or-no?)

(define (imail-ui:body-cache-limit message)
  (ref-variable imail-body-cache-limit
		(let ((folder (message-folder message)))
		  (and folder
		       (imail-folder->buffer folder #f)))))

(define (imail-ui:call-with-pass-phrase url receiver)
  (let ((key (url-pass-phrase-key url))
	(retention-time (ref-variable imail-pass-phrase-retention-time #f)))
    (let ((entry (hash-table/get memoized-pass-phrases key #f)))
      (if entry
	  (begin
	    (without-interrupts
	     (lambda ()
	       (deregister-timer-event (vector-ref entry 1))
	       (set-up-pass-phrase-timer! entry key retention-time)))
	    (call-with-unobscured-pass-phrase (vector-ref entry 0) receiver))
	  (call-with-pass-phrase
	   (string-append "Pass phrase for " key)
	   (lambda (pass-phrase)
	     (if (> retention-time 0)
		 (hash-table/put!
		  memoized-pass-phrases
		  key
		  (let ((entry
			 (vector (obscure-pass-phrase pass-phrase) #f #f)))
		    (set-up-pass-phrase-timer! entry key retention-time)
		    entry)))
	     (receiver pass-phrase)))))))

(define (imail-ui:delete-stored-pass-phrase url)
  (hash-table/remove! memoized-pass-phrases (url-pass-phrase-key url)))

(define (set-up-pass-phrase-timer! entry key retention-time)
  ;; A race condition can occur when the timer event is re-registered.
  ;; If the previous timer event is queued but not executed before
  ;; being deregistered, then it will run after the re-registration
  ;; and try to delete the record.  By matching on ID, the previous
  ;; event sees that it has been superseded and does nothing.
  (let ((id (list 'ID)))
    (vector-set! entry 2 id)
    (vector-set! entry 1
      (register-timer-event (* retention-time 60000)
	(lambda ()
	  (without-interrupts
	   (lambda ()
	     (let ((entry (hash-table/get memoized-pass-phrases key #f)))
	       (if (and entry (eq? (vector-ref entry 2) id))
		   (hash-table/remove! memoized-pass-phrases key))))))))))

(define memoized-pass-phrases
  (make-string-hash-table))

(define (obscure-pass-phrase clear-text)
  (let ((n (string-length clear-text)))
    (let ((noise (random-byte-vector n)))
      (let ((obscured-text (make-string (* 2 n))))
	(string-move! noise obscured-text 0)
	(do ((i 0 (fix:+ i 1)))
	    ((fix:= i n))
	  (vector-8b-set! obscured-text (fix:+ i n)
			  (fix:xor (vector-8b-ref clear-text i)
				   (vector-8b-ref noise i))))
	obscured-text))))

(define (call-with-unobscured-pass-phrase obscured-text receiver)
  (let ((n (quotient (string-length obscured-text) 2))
	(clear-text))
    (dynamic-wind
     (lambda ()
       (set! clear-text (make-string n))
       unspecific)
     (lambda ()
       (do ((i 0 (fix:+ i 1)))
	   ((fix:= i n))
	 (vector-8b-set! clear-text i
			 (fix:xor (vector-8b-ref obscured-text i)
				  (vector-8b-ref obscured-text (fix:+ i n)))))
       (receiver clear-text))
     (lambda ()
       (string-fill! clear-text #\NUL)
       (set! clear-text)
       unspecific))))

;;;; Navigation aids

(define (move-relative-any argument operation)
  (move-relative argument #f "message" operation))

(define (move-relative-undeleted argument operation)
  (move-relative argument message-undeleted? "undeleted message" operation))

(define (move-relative argument predicate noun operation)
  (if argument
      (let ((delta (command-argument-numeric-value argument)))
	(if (not (= 0 delta))
	    (call-with-values
		(lambda ()
		  (if (< delta 0)
		      (values (- delta) navigator/previous-message "previous")
		      (values delta navigator/next-message "next")))
	      (lambda (n step direction)
		(let ((folder (selected-folder))
		      (msg (selected-message)))
		  (let loop ((n n) (msg msg) (winner #f))
		    (if operation (operation msg))
		    (let ((next (step msg predicate)))
		      (cond ((not next)
			     (if winner (select-message folder winner))
			     (message "No " direction " " noun))
			    ((= n 1)
			     (select-message folder next))
			    (else
			     (loop (- n 1) next next))))))))))
      (if operation (operation (selected-message)))))

;;;; Message selection

(define (select-message folder selector #!optional force? raw?)
  (let ((buffer (imail-folder->buffer folder #t))
	(message
	 (cond ((message? selector)
		(if (message-attached? selector folder)
		    selector
		    (let ((index (message-index selector)))
		      (if (and index (< index (folder-length folder)))
			  (get-message folder index)
			  (last-message folder)))))
	       ((not selector)
		(last-message folder))
	       ((and (exact-integer? selector)
		     (<= 0 selector)
		     (< selector (folder-length folder)))
		(get-message folder selector))
	       (else
		(error:wrong-type-argument selector "message selector"
					   'SELECT-MESSAGE))))
	(raw? (if (default-object? raw?) #f raw?)))
    (if (or (if (default-object? force?) #f force?)
	    (not (eq? message (buffer-get buffer 'IMAIL-MESSAGE 'UNKNOWN))))
	(begin
	  (set-buffer-writeable! buffer)
	  (buffer-widen! buffer)
	  (region-delete! (buffer-region buffer))
	  (associate-imail-with-buffer buffer folder message)
	  (set-buffer-major-mode! buffer (ref-mode-object imail))
	  (let ((mark (mark-left-inserting-copy (buffer-start buffer))))
	    (with-read-only-defeated mark
	      (lambda ()
		(if message
		    (begin
		      (store-property! message 'RAW? raw?)
		      (insert-message message #f 0 mark))
		    (insert-string "[This folder has no messages in it.]"
				   mark))))
	    (mark-temporary! mark))
	  (set-buffer-point! buffer (buffer-start buffer))
	  (buffer-not-modified! buffer)))
    (if message
	(message-seen message))
    (folder-event folder 'SELECT-MESSAGE message)))

(define (selected-folder #!optional error? buffer)
  (or (let ((buffer
	     (chase-imail-buffer
	      (if (or (default-object? buffer) (not buffer))
		  (selected-buffer)
		  buffer))))
	(let ((folder (buffer-get buffer 'IMAIL-FOLDER 'UNKNOWN)))
	  (if (eq? 'UNKNOWN folder)
	      (error "IMAIL-FOLDER property not bound:" buffer))
	  folder))
      (and (if (default-object? error?) #t error?)
	   (error "No selected IMAIL folder."))))

(define (selected-message #!optional error? buffer)
  (or (let ((buffer
	     (if (or (default-object? buffer) (not buffer))
		 (selected-buffer)
		 buffer)))
	(let ((method (navigator/selected-message buffer)))
	  (if method
	      (method buffer)
	      (let ((buffer (chase-imail-buffer buffer)))
		(let ((message (buffer-get buffer 'IMAIL-MESSAGE 'UNKNOWN)))
		  (if (eq? message 'UNKNOWN)
		      (error "IMAIL-MESSAGE property not bound:" buffer))
		  (and message
		       (let ((folder (selected-folder #f buffer)))
			 (if (message-attached? message folder)
			     message
			     (begin
			       (buffer-put! buffer 'IMAIL-MESSAGE #f)
			       #f)))))))))
      (and (if (default-object? error?) #t error?)
	   (error "No selected IMAIL message."))))

;;;; Buffer associations

(define (associate-imail-with-buffer buffer folder message)
  (without-interrupts
   (lambda ()
     (buffer-put! buffer 'IMAIL-FOLDER folder)
     (buffer-put! buffer 'IMAIL-MESSAGE message)
     (store-property! folder 'BUFFER buffer)
     (set-buffer-default-directory!
      buffer
      (if (file-folder? folder)
	  (directory-pathname (file-folder-pathname folder))
	  (user-homedir-pathname)))
     (add-event-receiver! (folder-modification-event folder)
			  notice-folder-event)
     (add-kill-buffer-hook buffer delete-associated-buffers)
     (add-kill-buffer-hook buffer stop-probe-folder-thread)
     (start-probe-folder-thread buffer))))

(define (delete-associated-buffers folder-buffer)
  (for-each (lambda (buffer)
	      (if (buffer-alive? buffer)
		  (kill-buffer buffer)))
	    (buffer-get folder-buffer 'IMAIL-ASSOCIATED-BUFFERS '())))

(define (imail-folder->buffer folder error?)
  (or (let ((buffer (get-property folder 'BUFFER #f)))
	(and buffer
	     (if (buffer-alive? buffer)
		 buffer
		 (begin
		   (remove-property! folder 'BUFFER)
		   #f))))
      (and error? (error:bad-range-argument folder 'IMAIL-FOLDER->BUFFER))))

(define (imail-message->buffer message error?)
  (or (list-search-positive (buffer-list)
	(lambda (buffer)
	  (eq? (buffer-get buffer 'IMAIL-MESSAGE #f) message)))
      (and error? (error:bad-range-argument message 'IMAIL-MESSAGE->BUFFER))))

(define (associate-buffer-with-imail-buffer folder-buffer buffer)
  (without-interrupts
   (lambda ()
     (buffer-put! buffer 'IMAIL-FOLDER-BUFFER folder-buffer)
     (let ((buffers (buffer-get folder-buffer 'IMAIL-ASSOCIATED-BUFFERS '())))
       (if (not (memq buffer buffers))
	   (buffer-put! folder-buffer 'IMAIL-ASSOCIATED-BUFFERS
			(cons buffer buffers))))
     (add-kill-buffer-hook buffer dissociate-buffer-from-imail-buffer))))

(define (dissociate-buffer-from-imail-buffer buffer)
  (without-interrupts
   (lambda ()
     (let ((folder-buffer (buffer-get buffer 'IMAIL-FOLDER-BUFFER #f)))
       (if folder-buffer
	   (begin
	     (buffer-remove! buffer 'IMAIL-FOLDER-BUFFER)
	     (buffer-put! folder-buffer 'IMAIL-ASSOCIATED-BUFFERS
			  (delq! buffer
				 (buffer-get folder-buffer
					     'IMAIL-ASSOCIATED-BUFFERS
					     '()))))))
     (remove-kill-buffer-hook buffer dissociate-buffer-from-imail-buffer))))

(define (chase-imail-buffer buffer)
  (or (buffer-get buffer 'IMAIL-FOLDER-BUFFER #f)
      buffer))

;;;; Mode-line updates

(define (notice-folder-event folder type parameters)
  (case type
    ((EXPUNGE)
     (maybe-add-command-suffix! notice-message-expunge folder
				(car parameters)))
    ((INCREASE-LENGTH SET-LENGTH)
     (maybe-add-command-suffix! notice-folder-length-change folder
				(car parameters) (cadr parameters)))
    (else
     (maybe-add-command-suffix! notice-folder-modifications folder))))

(define (notice-message-expunge folder index)
  (let ((buffer (imail-folder->buffer folder #f)))
    (if buffer
	(let ((m (selected-message #f buffer)))
	  (if (or (not m)
		  (message-detached? m))
	      (select-message folder
			      (and index
				   (< index (folder-length folder))
				   index)
			      #t)))))
  (notice-folder-modifications folder))

(define (notice-folder-length-change folder old new)
  (let ((buffer (imail-folder->buffer folder #f)))
    (if buffer
	(cond ((> new old)
	       (select-message folder old #t))
	      ((let ((m (selected-message #f buffer)))
		 (or (not m)
		     (message-detached? m)))
	       (select-message folder #f #t)))))
  (notice-folder-modifications folder))

(define (notice-folder-modifications folder)
  (let ((buffer (imail-folder->buffer folder #f)))
    (if buffer
	(begin
	  (local-set-variable! mode-line-process
			       (imail-mode-line-summary-string buffer)
			       buffer)
	  (buffer-modeline-event! buffer 'PROCESS-STATUS)))))

(define (imail-mode-line-summary-string buffer)
  (let ((folder (selected-folder #f buffer))
	(message (selected-message #f buffer)))
    (and folder
	 (let ((n (folder-length folder)))
	   (string-append
	    (let ((status (folder-connection-status folder)))
	      (if (eq? status 'NO-SERVER)
		  ""
		  (string-append " " (symbol->string status))))
	    " "
	    (let ((index (and message (message-index message))))
	      (cond (index (number->string (+ 1 index)))
		    ((> n 0) "??")
		    (else "0")))
	    "/"
	    (number->string n)
	    (let loop ((i 0) (unseen 0))
	      (cond ((< i n)
		     (loop (+ i 1)
			   (if (message-unseen? (get-message folder i))
			       (+ unseen 1)
			       unseen)))
		    ((> unseen 0)
		     (string-append " (" (number->string unseen) " unseen)"))
		    (else "")))
	    (let ((flags
		   (if message
		       (flags-delete "seen" (message-flags message))
		       '())))
	      (if (pair? flags)
		  (string-append " " (decorated-string-append "" "," "" flags))
		  "")))))))

;;;; Probe-folder thread

(define (start-probe-folder-thread buffer)
  (stop-probe-folder-thread buffer)
  (let ((folder (buffer-get buffer 'IMAIL-FOLDER #f))
	(interval (ref-variable imail-update-interval #f)))
    (if (and folder interval
	     (not (get-property folder 'PROBE-REGISTRATION #f)))
	(let ((holder (list #f)))
	  (set-car! holder
		    (register-inferior-thread!
		     (let ((thread
			    (create-thread
			     editor-thread-root-continuation
			     (probe-folder-thread holder
						  (* 1000 interval)))))
		       (detach-thread thread)
		       thread)
		     (probe-folder-output-processor
		      (weak-cons folder unspecific))))
	  (store-property! folder 'PROBE-REGISTRATION holder)))))

(define ((probe-folder-thread holder interval))
  (do () (#f)
    (let ((registration (car holder)))
      (cond ((eq? registration 'KILL-THREAD) (exit-current-thread unspecific))
	    (registration (inferior-thread-output! registration))))
    (sleep-current-thread interval)))

(define ((probe-folder-output-processor folder))
  (let ((folder (weak-car folder)))
    (and folder
	 (eq? (folder-connection-status folder) 'ONLINE)
	 (begin
	   (probe-folder folder)
	   #t))))

(define (stop-probe-folder-thread buffer)
  (without-interrupts
   (lambda ()
     (let ((folder (buffer-get buffer 'IMAIL-FOLDER #f)))
       (if folder
	   (begin
	     (let ((holder (get-property folder 'PROBE-REGISTRATION #f)))
	       (if holder
		   (begin
		     (let ((registration (car holder)))
		       (if (and registration
				(not (eq? registration 'KILL-THREAD)))
			   (deregister-inferior-thread! registration)))
		     (set-car! holder 'KILL-THREAD))))
	     (remove-property! folder 'PROBE-REGISTRATION)))))))

;;;; Message insertion procedures

(define (insert-message message inline-only? left-margin mark)
  (let ((raw? (get-property message 'RAW? #f)))
    (insert-header-fields message raw? mark)
    (cond ((and raw? (not (eq? raw? 'FULL-HEADERS)))
	   (insert-message-body message mark))
	  ((folder-supports-mime? (message-folder message))
	   (insert-mime-message-body message mark inline-only? left-margin))
	  (else
	   (call-with-auto-wrapped-output-mark mark left-margin
	     (lambda (port)
	       (write-message-body message port)))))))

(define (insert-header-fields headers raw? mark)
  (for-each (lambda (header)
	      (insert-string (header-field-name header) mark)
	      (insert-char #\: mark)
	      (insert-string (header-field-value header) mark)
	      (insert-newline mark))
	    (let ((headers (->header-fields headers)))
	      (if raw?
		  headers
		  (maybe-reformat-headers
		   headers
		   (or (and (message? headers)
			    (imail-message->buffer headers #f))
		       mark)))))
  (insert-newline mark))

(define (maybe-reformat-headers headers buffer)
  (let ((headers
	 (cond ((ref-variable imail-kept-headers buffer)
		=> (lambda (regexps)
		     (append-map!
		      (lambda (regexp)
			(list-transform-positive headers
			  (lambda (header)
			    (re-string-match regexp
					     (header-field-name header)
					     #t))))
		      regexps)))
	       ((ref-variable imail-ignored-headers buffer)
		=> (lambda (regexp)
		     (list-transform-negative headers
		       (lambda (header)
			 (re-string-match regexp
					  (header-field-name header)
					  #t)))))
	       (else headers)))
	(filter (ref-variable imail-message-filter buffer)))
    (if filter
	(map (lambda (n.v)
	       (make-header-field (car n.v) (cdr n.v)))
	     (filter (map (lambda (header)
			    (cons (header-field-name header)
				  (header-field-value header)))
			  headers)))
	headers)))

(define (insert-message-body message mark)
  (call-with-output-mark mark
    (lambda (port)
      (write-message-body message port))))

;;;; MIME message formatting

(define (insert-mime-message-body message mark inline-only? left-margin)
  (insert-mime-message-part
   message
   (mime-message-body-structure message)
   '()
   (make-insert-mime-context inline-only? left-margin #f '())
   mark))

(define-structure insert-mime-context
  (inline-only? #f read-only #t)
  (left-margin #f read-only #t)
  (enclosure #f read-only #t)
  (boundaries #f read-only #t))

(define (make-insert-mime-subcontext context enclosure boundary)
  (make-insert-mime-context (insert-mime-context-inline-only? context)
			    (insert-mime-context-left-margin context)
			    enclosure
			    (cons (cons boundary (not boundary))
				  (insert-mime-context-boundaries context))))

(define (maybe-insert-mime-boundary context mark)
  (let ((boundary
	 (let loop ((boundaries (insert-mime-context-boundaries context)))
	   (and (pair? boundaries)
		(if (cdar boundaries)
		    (caar boundaries)
		    (loop (cdr boundaries)))))))
    (let loop ((boundaries (insert-mime-context-boundaries context)))
      (if (and (pair? boundaries)
	       (not (cdar boundaries)))
	  (begin
	    (set-cdr! (car boundaries) #t)
	    (loop (cdr boundaries)))))
    (if boundary
	(begin
	  (insert-newline mark)
	  (if (eq? boundary 'SIMPLE)
	      (insert-chars #\- (- (mark-x-size mark) 1) mark)
	      (begin
		(insert-string "--" mark)
		(insert-string boundary mark)))
	  (insert-newline mark)
	  (insert-newline mark)))))

(define-generic insert-mime-message-part (message body selector context mark))

(define-method insert-mime-message-part
    (message (body <mime-body>) selector context mark)
  message
  (insert-mime-message-attachment 'ATTACHMENT body selector context mark))

(define-method insert-mime-message-part
    (message (body <mime-body-multipart>) selector context mark)
  (let ((context
	 (make-insert-mime-subcontext
	  context
	  body
	  (if (ref-variable imail-use-original-mime-boundaries mark)
	      (mime-body-parameter body 'BOUNDARY "----------")
	      'SIMPLE))))
    (do ((parts (mime-body-multipart-parts body) (cdr parts))
	 (i 0 (fix:+ i 1)))
	((null? parts))
      (let ((part (car parts))
	    (selector `(,@selector ,i)))
	(if (and (fix:> i 0)
		 (eq? (mime-body-subtype body) 'ALTERNATIVE))
	    (insert-mime-message-attachment 'ALTERNATIVE part selector context
					    mark)
	    (insert-mime-message-part message part selector context mark))))))

(define-method insert-mime-message-part
    (message (body <mime-body-message>) selector context mark)
  (maybe-insert-mime-boundary context mark)
  (insert-header-fields (with-string-output-port
			  (lambda (port)
			    (write-mime-message-body-part message
							  `(,@selector HEADER)
							  #t
							  port)))
			#f
			mark)
  (insert-mime-message-part message
			    (mime-body-message-body body)
			    selector
			    (make-insert-mime-subcontext context body #f)
			    mark))

(define-method insert-mime-message-part
    (message (body <mime-body-text>) selector context mark)
  (let* ((enclosure (insert-mime-context-enclosure context))
	 (message-enclosure?
	  (and enclosure
	       (eq? (mime-body-type enclosure) 'MESSAGE)
	       (eq? (mime-body-subtype enclosure) 'RFC822)))
	 (encoding
	  (let ((encoding
		 (and message-enclosure?
		      (mime-body-one-part-encoding enclosure))))
	    (if (and encoding (not (memq encoding '(7BIT 8BIT BINARY))))
		;; This is illegal, but Netscape does it.
		encoding
		(mime-body-one-part-encoding body)))))
    (if (and (or (not enclosure)
		 (let ((disposition (mime-body-disposition body)))
		   (and disposition
			(eq? (car disposition) 'INLINE)))
		 (let ((subtype (mime-body-subtype body)))
		   (or (eq? subtype 'PLAIN)
		       (memq subtype
			     (ref-variable imail-inline-mime-text-subtypes
					   mark)))))
	     (known-mime-encoding? encoding)
	     (re-string-match
	      (string-append "\\`"
			     (apply regexp-group
				    (ref-variable imail-known-mime-charsets
						  mark))
			     "\\'")
	      (mime-body-parameter body 'CHARSET "us-ascii")
	      #t))
	(begin
	  (maybe-insert-mime-boundary context mark)
	  (call-with-auto-wrapped-output-mark
	   mark
	   (insert-mime-context-left-margin context)
	   (lambda (port)
	     (call-with-mime-decoding-output-port encoding port #t
	       (lambda (port)
		 (write-mime-message-body-part message
					       (if (or (not enclosure)
						       message-enclosure?)
						   `(,@selector TEXT)
						   selector)
					       #t
					       port))))))
	(insert-mime-message-attachment 'ATTACHMENT body selector context
					mark))))

(define (insert-mime-message-attachment class body selector context mark)
  (if (not (insert-mime-context-inline-only? context))
      (begin
	(maybe-insert-mime-boundary context mark)
	(let ((start (mark-right-inserting-copy mark)))
	  (insert-string "<IMAIL-" mark)
	  (insert-string (string-upcase (symbol->string class)) mark)
	  (insert-string " " mark)
	  (let ((column (mark-column mark)))
	    (let ((name (mime-attachment-name body selector #f)))
	      (if name
		  (begin
		    (insert-string "name=" mark)
		    (insert name mark)
		    (insert-newline mark)
		    (change-column column mark))))
	    (insert-string "type=" mark)
	    (insert (mime-body-type body) mark)
	    (insert-string "/" mark)
	    (insert (mime-body-subtype body) mark)
	    (insert-newline mark)
	    (if (eq? (mime-body-type body) 'TEXT)
		(begin
		  (change-column column mark)
		  (insert-string "charset=" mark)
		  (insert (mime-body-parameter body 'CHARSET "us-ascii") mark)
		  (insert-newline mark)))
	    (let ((encoding (mime-body-one-part-encoding body)))
	      (if (not (known-mime-encoding? encoding))
		  (begin
		    (change-column column mark)
		    (insert-string "encoding=" mark)
		    (insert encoding mark)
		    (insert-newline mark))))
	    (change-column column mark)
	    (insert-string "length=" mark)
	    (insert (mime-body-one-part-n-octets body) mark))
	  (insert-string ">" mark)
	  (region-put! start mark 'IMAIL-MIME-ATTACHMENT (cons body selector))
	  (mark-temporary! start))
	(insert-newline mark))))

(define (known-mime-encoding? encoding)
  (memq encoding '(7BIT 8BIT BINARY QUOTED-PRINTABLE BASE64)))

(define (mime-attachment-name body selector provide-default?)
  (or (mime-body-parameter body 'NAME #f)
      (and provide-default?
	   (string-append "unnamed-attachment-"
			  (if (null? selector)
			      "0"
			      (decorated-string-append
			       "" "." ""
			       (map (lambda (n) (number->string (+ n 1)))
				    selector)))))))

(define (mark-mime-attachment mark)
  (region-get mark 'IMAIL-MIME-ATTACHMENT #f))

(define (buffer-mime-attachments buffer)
  (let ((end (buffer-end buffer)))
    (let loop ((start (buffer-start buffer)) (attachments '()))
      (let ((index
	     (next-specific-property-change (mark-group start)
					    (mark-index start)
					    (mark-index end)
					    'IMAIL-MIME-ATTACHMENT))
	    (attachments
	     (let ((attachment (region-get start 'IMAIL-MIME-ATTACHMENT #f)))
	       (if attachment
		   (cons attachment attachments)
		   attachments))))
	(if index
	    (loop (make-mark (mark-group start) index) attachments)
	    (reverse! attachments))))))

(define (call-with-mime-decoding-output-port encoding port text? generator)
  (case encoding
    ((QUOTED-PRINTABLE)
     (call-with-decode-quoted-printable-output-port port text? generator))
    ((BASE64)
     (call-with-decode-base64-output-port port text? generator))
    (else
     (generator port))))

(define (call-with-decode-quoted-printable-output-port port text? generator)
  (let ((port
	 (make-port decode-quoted-printable-port-type
		    (decode-quoted-printable:initialize port text?))))
    (let ((v (generator port)))
      (close-output-port port)
      v)))

(define decode-quoted-printable-port-type
  (make-port-type
   `((WRITE-SUBSTRING
      ,(lambda (port string start end)
	 (decode-quoted-printable:update (port/state port) string start end)))
     (CLOSE-OUTPUT
      ,(lambda (port)
	 (decode-quoted-printable:finalize (port/state port)))))
   #f))

(define (call-with-decode-base64-output-port port text? generator)
  (let ((port
	 (make-port decode-base64-port-type
		    (decode-base64:initialize port text?))))
    (let ((v (generator port)))
      (close-output-port port)
      v)))

(define decode-base64-port-type
  (make-port-type
   `((WRITE-SUBSTRING
      ,(lambda (port string start end)
	 (decode-base64:update (port/state port) string start end)))
     (CLOSE-OUTPUT
      ,(lambda (port)
	 (decode-base64:finalize (port/state port)))))
   #f))

;;;; Automatic wrap/fill

(define (call-with-auto-wrapped-output-mark mark left-margin generator)
  (let ((auto-wrap (ref-variable imail-auto-wrap mark)))
    (if auto-wrap
	(let ((start (mark-right-inserting-copy mark))
	      (end (mark-left-inserting-copy mark)))
	  (call-with-output-mark mark generator)
	  (if (eq? auto-wrap 'FILL)
	      (fill-individual-paragraphs start end
					  (- (ref-variable fill-column start)
					     left-margin)
					  #f #f)
	      (wrap-individual-paragraphs start end
					  (- (- (mark-x-size mark) 1)
					     left-margin)
					  #f))
	  (mark-temporary! start)
	  (mark-temporary! end))
	(call-with-output-mark mark generator))))

;;;; Navigation hooks

(define (navigator/first-unseen-message folder)
  ((or (imail-navigator imail-navigators/first-unseen-message)
       first-unseen-message)
   folder))

(define (navigator/first-message folder)
  ((or (imail-navigator imail-navigators/first-message)
       first-message)
   folder))

(define (navigator/last-message folder)
  ((or (imail-navigator imail-navigators/last-message)
       last-message)
   folder))

(define (navigator/next-message message #!optional predicate)
  ((or (imail-navigator imail-navigators/next-message)
       next-message)
   message
   (if (default-object? predicate) #f predicate)))

(define (navigator/previous-message message #!optional predicate)
  ((or (imail-navigator imail-navigators/previous-message)
       previous-message)
   message
   (if (default-object? predicate) #f predicate)))

(define (imail-navigator accessor)
  (let ((navigators (buffer-get (selected-buffer) 'IMAIL-NAVIGATORS #f)))
    (and navigators
	 (accessor navigators))))

(define (navigator/selected-message buffer)
  (let ((navigators (buffer-get buffer 'IMAIL-NAVIGATORS #f)))
    (and navigators
	 (imail-navigators/selected-message navigators))))

(define-structure (imail-navigators safe-accessors
				    (conc-name imail-navigators/))
  (first-unseen-message #f read-only #t)
  (first-message #f read-only #t)
  (last-message #f read-only #t)
  (next-message #f read-only #t)
  (previous-message #f read-only #t)
  (selected-message #f read-only #t))