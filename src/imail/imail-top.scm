#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

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
It is useful to set this variable in the site customization file."
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

(define-variable imail-output-default
  "If not #f, the default URL for M-x imail-output."
  #f
  string-or-false?)

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
Otherwise, only the header fields normally shown by IMAIL are sent."
  'ASK
  (lambda (x) (or (boolean? x) (eq? x 'ASK))))

(define-variable imail-forward-using-mime
  "If true, forwarded email messages are sent as MIME attachments.
Otherwise, they are inserted into the message body."
  #t
  boolean?)

(define-variable imail-known-mime-charsets
  "List of regular expressions matching character-set names.
Text messages using these character sets are displayed inline;
 when other character sets are used, the text is treated as an attachment."
  (list "us-ascii" "iso-?8859-[0-9]+" "utf-?[78]"
	"unicode-[0-9]+-[0-9]+-utf-[78]" ; RFC 1641
	"windows-[0-9]+" "unknown-8bit")
  list-of-strings?)

(define-variable imail-inline-mime-text-subtypes
  "List of MIME text subtypes that should be shown inline.
The value of this variable is a list of symbols.
A text body that appears at the top level of a message
 is always shown inline, regardless of its subtype.
Likewise, a text/plain body is always shown inline.
Note that this variable does not affect subparts of multipart/alternative."
  '(HTML ENRICHED)
  list-of-strings?)

(define-variable imail-inline-mime-text-limit
  "Size limit in octets for showing MIME text message parts inline.
MIME text message parts less than this size are shown in-line by default.
This variable can also be #F; then all parts will be shown inline."
  65536
  (lambda (x) (or (boolean? x) (exact-nonnegative-integer? x))))

(define-variable imail-mime-attachment-directory
  "Default directory in which to store MIME attachments.
Either #F or a pathname."
  #f
  (lambda (x) (or (not x) (string? x) (pathname? x))))

(define-variable imail-mime-show-alternatives
  "If true, all parts of a multipart/alternative message are shown.
 (Only one of the parts will be shown in-line; the others as attachments.)
Otherwise, only one of the parts is shown."
  #f
  boolean?)

(define-variable imail-mime-collapse-digest
  "If true, component messages of a MIME digest are shown as attachments."
  #f
  boolean?)

(define-variable imail-mime-boundary-style
  "Specifies style of separators between parts of multipart MIME message.
'SIMPLE means use a simple dashed line.
'SGML is like 'SIMPLE except the line is bracketed with <!-- -->.
'ORIGINAL means use the original MIME boundary strings."
  'SIMPLE
  (lambda (x) (memq x '(SIMPLE SGML ORIGINAL))))

(define-variable imail-mime-show-headers
  "If true, show MIME headers in expanded body parts.
Headers are shown only for parts that are displayed out-of-line by
  default."
  #f
  boolean?)

(define-variable imail-global-mail-notification
  "If true, all buffer modelines say if there is unseen mail.
 (This checks only for unseen mail in the primary folder.)
Otherwise, only the IMAIL buffer for that folder has an indicator."
  #t
  boolean?)

(define-command imail
  "Read and edit incoming mail.
Given a prefix argument, it prompts for an IMAIL URL,
 then visits the mail folder at that URL.
IMAIL URLs take one of the following forms.

imap://[<user-name>@]<host-name>[:<port>]/<folder-name>
    Specifies a folder on an IMAP server.  The portions in brackets
    are optional and are filled in automatically if omitted.

file:<pathname>
    Specifies a file-based folder, e.g. RMAIL.

You may simultaneously open multiple mail folders.  If you revisit a
folder that is already in a buffer, that buffer is selected.  Messages
may be freely copied from one mail folder to another, regardless of
the type of folder.  Likewise, the available commands are the same
regardless of the folder type."
  (lambda ()
    (list (and (command-argument)
	       (prompt-for-selectable-folder "Run IMAIL on folder" #f
					     'HISTORY 'IMAIL
					     'REQUIRE-MATCH? #t))))
  (lambda (url-string)
    (let ((folder
	   (open-resource
	    (if url-string
		(imail-parse-partial-url url-string)
		(imail-primary-url #f)))))
      (let ((buffer (imail-folder->buffer folder #f)))
	(if buffer
	    (begin
	      (select-buffer buffer)
	      (if (eq? (folder-connection-status folder) 'OFFLINE)
		  ((ref-command imail-get-new-mail) #f)
		  (let ((message (first-unseen-message folder)))
		    (if message
			(select-message folder message #t)))))
	    (begin
	      (let ((buffer
		     (new-buffer
		      (url-presentation-name (resource-locator folder)))))
		(associate-imail-with-buffer buffer folder #f)
		(select-buffer buffer))
	      (select-message folder
			      (or (first-unseen-message folder)
				  (selected-message #f))
			      #t)))))))

(define-major-mode imail read-only "IMAIL"
  (lambda ()
    (call-with-output-string
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
    (local-set-variable! mode-line-process
			 imail-mode-line-summary-string
			 buffer)
    (imail-adjust-adaptive-fill buffer)
    (standard-alternate-paragraph-style! buffer)
    (set-buffer-read-only! buffer)
    (disable-group-undo! (buffer-group buffer))
    (event-distributor/invoke! (ref-variable imail-mode-hook buffer) buffer)))

(define-variable imail-mode-hook
  "An event distributor that is invoked when entering IMAIL mode."
  (make-event-distributor))

(define (imail-adjust-adaptive-fill buffer)
  (add-adaptive-fill-regexp! "[ \t]*[-a-zA-Z0-9]*>\\([ \t]*>\\)*[ \t]*"
			     buffer))

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

(define (imail-mode-line-summary-string window)
  (let* ((buffer (window-buffer window))
	 (folder (selected-folder #f buffer))
	 (message (selected-message #f buffer)))
    (if folder
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
	   (let ((unseen (count-unseen-messages folder)))
	     (if (> unseen 0)
		 (string-append " (" (number->string unseen) " unseen)")
		 ""))
	   (let ((flags
		  (if message
		      (flags-delete "seen" (message-flags message))
		      '())))
	     (if (pair? flags)
		 (string-append " " (decorated-string-append "" "," "" flags))
		 ""))))
	"")))

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
\\[imail-quit]	Quit IMAIL: disconnect from server, then switch to another buffer.

\\[imail-mail]	Mail a message (same as \\[mail-other-window]).
\\[imail-reply]	Reply to this message.  Like \\[imail-mail] but initializes some fields.
\\[imail-forward]	Forward this message to another user.
\\[imail-continue]	Continue composing outgoing message started before.

\\[imail-output]	Append this message to a specified folder.
\\[imail-file-message]	Append this message to a specified file.
	  (The message is written in a human-readable format.)
\\[imail-save-attachment]	Save a MIME attachment to a file.
\\[imail-save-mime-body]	Save an arbitrary MIME body to a file.

\\[imail-add-flag]	Add flag to message.  It will be displayed in the mode line.
\\[imail-kill-flag]	Remove flag from message.
\\[imail-next-flagged-message]	Move to next message with specified flag
	  (flag defaults to last one specified).
	  Standard flags:
	    answered, deleted, filed, forwarded, resent, seen.
	  Any other flag is present only if you add it with `\\[imail-add-flag]'.
\\[imail-previous-flagged-message]	Move to previous message with specified flag.

\\[imail-summary]	Show headers buffer, with a one line summary of each message.
\\[imail-summary-by-flags]	Like \\[imail-summary] only just messages with particular flag(s).
\\[imail-summary-by-recipients]	Like \\[imail-summary] only just messages with particular recipient(s).
\\[imail-summary-by-topic]	Like \\[imail-summary] only just messages with particular topic(s).
\\[imail-summary-by-regexp]	Like \\[imail-summary] only just messages matching regular expression.

\\[imail-toggle-header]		Toggle between full headers and reduced headers.
\\[imail-toggle-mime-body]	Toggle MIME body between expanded and collapsed formats.
\\[imail-toggle-message]	Toggle between standard and raw message formats.

\\[imail-create-folder]	Create a new folder.  (Normally not needed as output commands
	  create folders automatically.)
\\[imail-delete-folder]	Delete an existing folder and all its messages.
\\[imail-rename-folder]	Rename a folder.
\\[imail-copy-folder]	Copy all messages from one folder to another.

\\[imail-cache]	Fill any local cache associated with the selected folder.")

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
	  (buffer-remove! buffer 'IMAIL-MIME-EXPANSIONS)
	  (select-message
	   folder
	   (or (selected-message #f buffer)
	       (first-unseen-message folder))
	   #t)))))

(define (imail-kill-buffer buffer)
  (let ((folder (selected-folder #f buffer)))
    (if folder
	(unmemoize-resource (resource-locator folder))))
  (notifier:set-mail-string! #f))

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
(define-key 'imail #\u		'imail-undelete-previous-message)
(define-key 'imail #\m-u	'imail-first-unseen-message)
(define-key 'imail #\w		'imail-save-mime-body)
(define-key 'imail #\x		'imail-expunge)
(define-key 'imail #\.		'beginning-of-buffer)
(define-key 'imail #\<		'imail-first-message)
(define-key 'imail #\>		'imail-last-message)
(define-key 'imail #\space	'scroll-up)
(define-key 'imail #\rubout	'scroll-down)
(define-key 'imail #\?		'describe-mode)
(define-key 'imail #\^		'imail-browser-view-container)
(define-key 'imail '(#\c-c #\c-n)	'imail-next-same-subject)
(define-key 'imail '(#\c-c #\c-p)	'imail-previous-same-subject)
(define-key 'imail '(#\c-c #\c-s #\c-a)	'imail-sort-by-author)
(define-key 'imail '(#\c-c #\c-s #\c-c)	'imail-sort-by-correspondent)
(define-key 'imail '(#\c-c #\c-s #\c-d)	'imail-sort-by-date)
(define-key 'imail '(#\c-c #\c-s #\c-r)	'imail-sort-by-recipient)
(define-key 'imail '(#\c-c #\c-s #\c-s)	'imail-sort-by-subject)
(define-key 'imail '(#\c-c #\c-s #\c-v)	'imail-sort-by-arrival)
(define-key 'imail '(#\c-c #\c-t #\c-e)	'imail-toggle-mime-body)
(define-key 'imail '(#\c-c #\c-t #\c-h)	'imail-toggle-header)
(define-key 'imail '(#\c-c #\c-t #\c-m)	'imail-toggle-message)
(define-key 'imail '(#\c-c #\c-t #\c-w)	'imail-toggle-wrap-body)
(define-key 'imail #\M-o	'imail-file-message)

;; Putting these after the group above exploits behavior in the comtab
;; abstraction that makes these bindings the ones that show up during
;; command substitution.
(define-key 'imail #\t		'imail-toggle-header)
(define-key 'imail #\c-m-h	'imail-summary)
(define-key 'imail #\c-m-l	'imail-summary-by-flags)
(define-key 'imail #\c-m-r	'imail-summary-by-recipients)
(define-key 'imail #\c-m-s	'imail-summary-by-regexp)
(define-key 'imail #\c-m-t	'imail-summary-by-topic)

;; These commands have no equivalent in RMAIL.
(define-key 'imail #\C		'imail-copy-folder)
(define-key 'imail #\D		'imail-delete-folder)
(define-key 'imail #\R		'imail-rename-folder)
(define-key 'imail #\+		'imail-create-folder)
(define-key 'imail button3-down 'imail-mouse-save-mime-body)

;; These commands not yet implemented.
;;(define-key 'imail #\m-m	'imail-retry-failure)
;;(define-key 'imail #\w		'imail-output-body-to-file)
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
		       (any (lambda (flag)
			      (message-flagged? message flag))
			    flags))
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
  "Delete this message and stay on it.
With prefix argument N, deletes forward N messages,
 or backward if N is negative.
Deleted messages stay in the file until the \\[imail-expunge] command is given."
  "P"
  (lambda (argument)
    (if argument
	(move-relative-undeleted (command-argument-numeric-value argument)
				 delete-message)
	(delete-message (selected-message)))))

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
      (let ((messages (list-deleted-messages folder)))
	(cond ((not (pair? messages))
	       (message "No messages to expunge"))
	      ((let ((confirmation (ref-variable imail-expunge-confirmation)))
		 (or (null? confirmation)
		     (let ((prompt
			    (string-append "Expunge "
					   (number->string (length messages))
					   " message"
					   (if (pair? (cdr messages)) "s" "")
					   " marked for deletion")))
		       (let ((do-prompt
			      (lambda ()
				(cond ((memq 'BRIEF confirmation)
				       (prompt-for-confirmation? prompt))
				      ((memq 'VERBOSE confirmation)
				       (prompt-for-yes-or-no? prompt))
				      (else #t)))))
			 (if (memq 'SHOW-MESSAGES confirmation)
			     (cleanup-pop-up-buffers
			      (lambda ()
				(imail-expunge-pop-up-messages folder messages)
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

(define (imail-expunge-pop-up-messages folder messages)
  (pop-up-temporary-buffer " *imail-message*" '(READ-ONLY SHRINK-WINDOW)
    (lambda (buffer window)
      window
      (local-set-variable! truncate-lines #t buffer)
      (preload-folder-outlines folder messages)
      (let ((mark (mark-left-inserting-copy (buffer-point buffer)))
	    (index-digits
	     (exact-nonnegative-integer-digits
	      (- (folder-length folder) 1))))
	(for-each (lambda (m)
		    (if (message-deleted? m)
			(write-imail-summary-line! m index-digits mark)))
		  messages)))))

(define (list-deleted-messages folder)
  (list-messages folder message-deleted?))

(define (list-messages folder predicate)
  (let ((n (folder-length folder)))
    (do ((i 0 (+ i 1))
	 (messages '()
		   (let ((m (get-message folder i)))
		     (if (predicate m)
			 (cons m messages)
			 messages))))
	((= i n) messages))))

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
   'DEFAULT-TYPE 'VISIBLE-DEFAULT
   'HISTORY 'IMAIL-READ-FLAG
   'HISTORY-INDEX 0
   'REQUIRE-MATCH? require-match?))

;;;; Message I/O

(define-command imail-input-from-folder
  "Append messages to this folder from a specified folder."
  (lambda ()
    (list (prompt-for-selectable-folder "Get messages from folder" #f
					'HISTORY 'IMAIL-INPUT-FROM-FOLDER
					'HISTORY-INDEX 0
					'REQUIRE-MATCH? #t)))
  (lambda (url-string)
    (let ((url (imail-parse-partial-url url-string)))
      (copy-folder url
		   (resource-locator (selected-folder))
		   (string-append "from " (url->string url))
		   (lambda () ((ref-command imail-get-new-mail) #f))))))

(define-command imail-output
  "Append this message to a specified folder.
With prefix argument, appends next several messages."
  (lambda ()
    (list (prompt-for-folder "Output to folder"
			     (ref-variable imail-output-default #f)
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

(define-command imail-file-message
  "Append this message to a text file.
With prefix argument, appends next several messages.
This command writes the message to the output file in human-readable format,
 unlike the \\[imail-output] command which writes in computer format."
  (lambda ()
    (list (prompt-for-file "Append message to file" #f
			   'HISTORY 'IMAIL-FILE-MESSAGE
			   'HISTORY-INDEX 0)
	  (command-argument)))
  (lambda (pathname argument)
    (let ((exists? (file-exists? pathname)))
      (if (and exists? (file-folder-type pathname))
	  ((ref-command imail-output)
	   (url->string (make-pathname-url pathname))
	   argument)
	  (call-with-temporary-buffer " *imail-file-message*"
	    (lambda (buffer)
	      (let ((mark (mark-left-inserting-copy (buffer-start buffer))))
		(move-relative-undeleted argument
		  (lambda (message)
		    (if exists?
			(begin
			  (insert-newline mark)
			  (insert-chars #\= 79 mark)
			  (insert-newlines 2 mark))
			(set! exists? #t))
		    (insert-message message #f 0 mark)))
		(mark-temporary! mark))
	      (append-to-file (buffer-region buffer) pathname #t
			      'DEFAULT)))))))

;;;; Attachments

(define-command imail-save-attachment
  "Save the attachment at point.
If point is not on an attachment, prompts for the attachment to save.
With prefix argument, prompt even when point is on an attachment."
  "P"
  (lambda (always-prompt?)
    (let ((buffer (imail-folder->buffer (selected-folder) #t)))
      (save-mime-body (car (maybe-prompt-for-mime-info "Save attachment"
						       (buffer-point buffer)
						       always-prompt?
						       mime-attachment?))
		      buffer))))

(define-command imail-mouse-save-mime-body
  "Save the MIME body that mouse is on."
  ()
  (lambda ()
    (let ((button-event (current-button-event)))
      (let ((window (button-event/window button-event)))
	(let ((buffer (window-buffer window)))
	  (save-mime-body
	   (let ((info
		  (mark-mime-info
		   (or (window-coordinates->mark
			window
			(button-event/x button-event)
			(button-event/y button-event))
		       (buffer-end buffer)))))
	     (if (not info)
		 (editor-error "Mouse not on a MIME body."))
	     info)
	   buffer))))))

(define-command imail-save-mime-body
  "Save the MIME body at point."
  ()
  (lambda ()
    (save-mime-body (car (current-mime-body)) (selected-buffer))))

(define-command imail-toggle-mime-body
  "Expand or collapse the MIME body at point."
  ()
  (lambda ()
    (let ((i.m (current-mime-body)))
      (let ((info (car i.m))
	    (mark (cdr i.m)))
	(set-mime-info-expanded?! info
				  mark
				  (not (mime-info-expanded? info mark)))
	(re-render-mime-body info mark)))))

(define-command imail-toggle-wrap-body
  "Toggle auto-wrap on or off for the MIME body at point."
  ()
  (lambda ()
    (let ((i.m (current-mime-body)))
      (let ((info (car i.m))
	    (mark (cdr i.m)))
	(mime-body-wrapped! (mime-info-body info)
			    (not (mime-body-wrapped? (mime-info-body info))))
	(re-render-mime-body info mark)))))

(define (mime-body-wrapped? body)
  (get-property body 'WRAP? #t))

(define (mime-body-wrapped! body value)
  (if (eq? value #t)
      (remove-property! body 'WRAP?)
      (store-property! body 'WRAP? value)))

(define (re-render-mime-body info mark)
  (let ((region (mime-body-region mark))
	(buffer (mark-buffer mark)))
    (if (not region)
	(error "No MIME body at mark:" mark))
    (let ((point (mark-right-inserting-copy (buffer-point buffer))))
      (with-read-only-defeated mark
	(lambda ()
	  (region-delete! region)
	  (let ((mark (mark-left-inserting-copy (region-start region))))
	    (insert-mime-info info mark)
	    (mark-temporary! mark))))
      (mark-temporary! point)
      (set-buffer-point! buffer point))
    (buffer-not-modified! buffer)))

(define (maybe-prompt-for-mime-info prompt mark always-prompt? predicate)
  (let ((info (mark-mime-info mark)))
    (if (and info (not always-prompt?) (predicate info))
	(cons info mark)
	(let ((alist
	       (uniquify-mime-attachment-names
		(map (lambda (i.m)
		       (cons (mime-attachment-name (car i.m) #t)
			     i.m))
		     (filter (lambda (i.m)
			       (predicate (car i.m)))
			     (buffer-mime-info (mark-buffer mark)))))))
	  (if (pair? alist)
	      (if (or (pair? (cdr alist)) always-prompt?)
		  (prompt-for-alist-value
		   prompt
		   alist
		   (and info
			(let loop ((alist alist))
			  (and (pair? alist)
			       (if (eq? (cadar alist) info)
				   (caar alist)
				   (loop (cdr alist))))))
		   #f)
		  (cdar alist))
	      (editor-error "This message has no attachments."))))))

(define (uniquify-mime-attachment-names alist)
  (let loop ((alist alist) (converted '()))
    (if (pair? alist)
	(loop (cdr alist)
	      (cons (cons (let ((name (caar alist)))
			    (let loop ((name* name) (n 1))
			      (if (any (lambda (entry)
					 (string=? (car entry) name*))
				       converted)
				  (loop (string-append
					 name "<" (number->string n) ">")
					(+ n 1))
				  name*)))
			  (cdar alist))
		    converted))
	(reverse! converted))))

(define (current-mime-body)
  (let ((point (current-point)))
    (let ((info (mark-mime-info point)))
      (if (not info)
	  (editor-error "Point not on a MIME body."))
      (cons info point))))

(define (save-mime-body info buffer)
  (let ((body (mime-info-body info)))
    (let ((filename
	   (let ((history 'IMAIL-SAVE-ATTACHMENT))
	     (prompt-for-file
	      (string-append "Save "
			     (if (mime-attachment? info)
				 "attachment"
				 "MIME body")
			     " as")
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
			     (let ((filenames
				    (prompt-history-strings history)))
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
	  (call-with-output-file filename
	    (lambda (port)
	      (if (not text?)
		  (begin
		    (port/set-coding port 'binary)
		    (port/set-line-ending port 'binary)))
	      (call-with-mime-decoding-output-port
	       (let ((encoding (mime-body-one-part-encoding body)))
		 (if (and (mime-type? body 'APPLICATION 'MAC-BINHEX40)
			  (eq? encoding '7BIT))
		     'BINHEX40
		     encoding))
	       port
	       text?
	       (lambda (port)
		 (with-mime-best-effort
		  (lambda ()
		    (write-mime-body body port)))))))))))

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
    (make-initialized-mail-buffer '(("To" "") ("Subject" ""))
				  (chase-imail-buffer (selected-buffer))
				  initialize-imail-mail-buffer
				  select-buffer-other-window)))

(define-command imail-reply
  "Reply to the current message.
Normally include CC: to all other recipients of original message;
 prefix argument means ignore them.
While composing the reply, use \\[mail-yank-original] to yank the
 original message into it."
  "P"
  (lambda (just-sender?)
    (let ((message (selected-message)))
      (make-initialized-mail-buffer
       (imail-reply-headers message (not just-sender?))
       (chase-imail-buffer (selected-buffer))
       (lambda (mail-buffer)
	 (initialize-imail-mail-buffer mail-buffer)
	 (message-answered message))
       select-buffer-other-window))))

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

(if (not global-mailer-version-string)
    (set! global-mailer-version-string imail-mailer-version-string))

(define-command imail-forward
  "Forward the current message to another user.
With single \\[universal-argument], \"resend\" the message instead of forwarding it;
 see the documentation of `imail-resend'.
With negative argument, forward the message with all headers;
 otherwise headers are trimmed according to imail-forward-all-headers."
  "P"
  (lambda (argument)
    (if (command-argument-multiplier-only? argument)
	(dispatch-on-command (ref-command-object imail-resend))
	(imail-forward argument))))

(define (imail-forward argument)
  (let ((message (selected-message)))
    (make-initialized-mail-buffer
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
       (let ((raw?
	      (if (< (command-argument-numeric-value argument) 0)
		  #t
		  (let ((all?
			 (ref-variable imail-forward-all-headers mail-buffer)))
		    (if (eq? all? 'ASK)
			(prompt-for-confirmation? "Forward all header fields")
			all?)))))
	 (if (ref-variable imail-forward-using-mime mail-buffer)
	     (add-buffer-mime-attachment!
	      mail-buffer
	      (make-mime-type 'MESSAGE 'RFC822)
	      '()
	      '(INLINE)
	      (map header-field->mail-header
		   (let ((headers (message-header-fields message)))
		     (if raw?
			 headers
			 (maybe-reformat-headers
			  headers
			  (or (imail-message->buffer message #f)
			      mail-buffer)
			  #t))))
	      (lambda (port) (write-message-body message port)))
	     (let ((mark (mark-left-inserting-copy (buffer-end mail-buffer))))
	       (with-buffer-point-preserved mail-buffer
		 (lambda ()
		   (insert-header-fields message raw? mark)
		   (insert-message-body message mark)))
	       (mark-temporary! mark))))
       (message-forwarded message))
     (if (window-has-no-neighbors? (current-window))
	 select-buffer
	 select-buffer-other-window))))

(define-command imail-resend
  "Resend current message to ADDRESSES.
ADDRESSES is a string consisting of several addresses separated by commas."
  "sResend to"
  (lambda (addresses)
    (let ((buffer (selected-buffer))
	  (message (selected-message)))
      (make-initialized-mail-buffer
       `(("Resent-From" ,(mail-from-string buffer))
	 ("Resent-Date" ,(universal-time->string (get-universal-time)))
	 ("Resent-To" ,addresses)
	 ,@(if (ref-variable mail-self-blind buffer)
	       `(("Resent-Bcc" ,(mail-from-string buffer)))
	       '())
	 ,@(map header-field->mail-header
		(remove (lambda (header)
			  (string-ci=? (header-field-name header) "sender"))
			(message-header-fields message))))
       #f
       (lambda (mail-buffer)
	 (initialize-imail-mail-buffer mail-buffer)
	 (with-buffer-point-preserved mail-buffer
	   (lambda ()
	     (insert-message-body message (buffer-end mail-buffer))))
	 (disable-buffer-mime-processing! mail-buffer)
	 (message-resent message))
       (if (window-has-no-neighbors? (current-window))
	   select-buffer
	   select-buffer-other-window)))))

(define (imail-reply-headers message cc?)
  (let ((resent-reply-to
	 (get-last-header-field-value message "resent-reply-to" #f))
	(from (get-first-header-field-value message "from" #f))
	(concat
	 (lambda (strings)
	   (and (pair? strings)
		(decorated-string-append "" ", " "" strings)))))
    `(("To"
       ,(rfc822:canonicalize-named-address-string
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
			      (rfc822:string->named-addresses cc))))
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
	  (if (re-string-match pattern
			       (rfc822:canonicalize-address-string
				(car addresses)))
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

(define (header-field->mail-header header)
  (list (header-field-name header)
	(header-field-value->string (header-field-value header))))

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
    (list (prompt-for-folder "Create folder" #f
			     'HISTORY 'IMAIL-CREATE-FOLDER)))
  (lambda (url-string)
    (let ((url (imail-parse-partial-url url-string)))
      (create-resource url)
      (message "Created folder " (url->string url)))))

(define-command imail-delete-folder
  "Delete a specified folder and all its messages."
  (lambda ()
    (list (maybe-prompt-for-folder "Delete folder"
				   'HISTORY 'IMAIL-DELETE-FOLDER
				   'REQUIRE-MATCH? #t)))
  (lambda (url-string)
    (let ((url (imail-parse-partial-url url-string)))
      (if (prompt-for-yes-or-no?
	   (string-append "Delete folder " (url->string url)))
	  (begin
	    (delete-resource url)
	    (message "Deleted folder " (url->string url)))
	  (message "Folder not deleted")))))

(define-command imail-rename-folder
  "Rename a folder.
May only rename a folder to a new name on the same server or file system.
The folder's type may not be changed."
  (lambda ()
    (let ((from
	   (maybe-prompt-for-folder "Rename folder"
				    'HISTORY 'IMAIL-RENAME-FOLDER-SOURCE
				    'REQUIRE-MATCH? #t)))
      (list from
	    (prompt-for-url
	     "Rename folder to"
	     (container-url-for-prompt (imail-parse-partial-url from))
	     'HISTORY 'IMAIL-RENAME-FOLDER-TARGET))))
  (lambda (from to)
    (let* ((from (imail-parse-partial-url from))
	   (to
	    (let ((to (imail-parse-partial-url to)))
	      (if (container-url? to)
		  (make-content-url to (url-content-name from))
		  to))))
      (rename-resource from to)
      (message "Folder renamed to " (url->string to)))))

(define-command imail-copy-folder
  "Copy all messages from a specified folder to another folder.
If the target folder exists, the messages are appended to it.
If it doesn't exist, it is created first."
  (lambda ()
    (let ((from
	   (maybe-prompt-for-selectable-folder
	    "Copy folder"
	    'HISTORY 'IMAIL-COPY-FOLDER-SOURCE
	    'REQUIRE-MATCH? #t)))
      (list from
	    (prompt-for-url
	     "Copy messages to folder"
	     (make-content-url
	      (or (let ((history
			 (prompt-history-strings 'IMAIL-COPY-FOLDER-TARGET)))
		    (and (pair? history)
			 (let ((url
				(ignore-errors
				 (lambda ()
				   (imail-parse-partial-url (car history))))))
			   (and (url? url)
				(container-url-for-prompt url)))))
		  (imail-default-container))
	      (url-base-name (imail-parse-partial-url from)))
	     'HISTORY 'IMAIL-COPY-FOLDER-TARGET))))
  (lambda (from to)
    (let ((from (imail-parse-partial-url from))
	  (to (imail-parse-partial-url to)))
      (copy-folder from
		   (if (container-url? to)
		       (make-content-url to (url-content-name from))
		       to)))))

(define (copy-folder url new-url #!optional reference-string refresh)
  (if (eq? url new-url)
      (editor-error "Can't copy folder to itself:" url))
  (with-open-resource url
    (lambda (folder)
      (with-open-connection new-url
	(lambda ()
	  (let ((n (folder-length folder)))
	    (do ((i 0 (+ i 1)))
		((= i n))
	      ((message-wrapper #f
				"Copying message "
				(number->string (+ i 1))
				"/"
				(number->string n))
	       (lambda () (append-message (get-message folder i) new-url))))
	    (if (if (default-object? refresh) #f refresh)
		(refresh))
	    (message (number->string n)
		     " message"
		     (if (= n 1) "" "s")
		     " copied "
		     (if (or (default-object? reference-string)
			     (not reference-string))
			 (string-append "to " (url->string new-url))
			 reference-string))))))))

;;;; Sorting

(define-command imail-sort-by-arrival
  "Sort messages of current folder by arrival time.
This is the default order if no sorting has been done."
  ()
  (lambda ()
    (set-folder-order! (selected-folder) #f)))

(define-command imail-sort-by-date
  "Sort messages of current folder by date.
With prefix argument, sort them in reverse order."
  "P"
  (lambda (reverse?)
    (sort-selected-folder (if reverse? > <)
			  (lambda (m)
			    (or (message-time m)
				(message-internal-time m))))))

(define-command imail-sort-by-subject
  "Sort messages of current folder by subject.
With prefix argument, sort them in reverse order."
  "P"
  (lambda (reverse?)
    (sort-selected-folder (if reverse? string-ci>? string-ci<?)
			  message-subject)))

(define-command imail-sort-by-author
  "Sort messages of current folder by author.
With prefix argument, sort them in reverse order."
  "P"
  (lambda (reverse?)
    (sort-selected-folder (if reverse? string-ci>? string-ci<?)
			  message-author)))

(define-command imail-sort-by-recipient
  "Sort messages of current folder by recipient.
With prefix argument, sort them in reverse order."
  "P"
  (lambda (reverse?)
    (sort-selected-folder (if reverse? string-ci>? string-ci<?)
			  message-recipient)))

(define-command imail-sort-by-correspondent
  "Sort messages of current folder by other correspondent.
With prefix argument, sort them in reverse order."
  "P"
  (lambda (reverse?)
    (sort-selected-folder (if reverse? string-ci>? string-ci<?)
			  message-correspondent)))

(define (message-correspondent message)
  (let loop ((names '("from" "sender" "to" "apparently-to")))
    (if (pair? names)
	(or (let ((v (get-first-header-field-value message (car names) #f)))
	      (and v
		   (let ((addresses
			  (imail-dont-reply-to (rfc822:string->addresses v))))
		     (and (pair? addresses)
			  (car addresses)))))
	    (loop (cdr names)))
	"")))

(define (sort-selected-folder < message-key)
  (set-folder-order! (selected-folder) (make-folder-order < message-key)))

;;;; Miscellany

(define-command imail-quit
  "Quit out of IMAIL.
Closes all IMAIL folders and buries their buffers.
With prefix argument, closes and buries only selected IMAIL folder."
  "P"
  (lambda (selected-only?)
    (let ((quit
	   (lambda (folder)
	     (close-resource folder #t)
	     (imail-bury folder))))
      (if selected-only?
	  (quit (selected-folder))
	  (for-each quit (folder-list))))))

(define (folder-list)
  (let loop ((buffers (buffer-list)) (folders '()))
    (if (pair? buffers)
	(loop (cdr buffers)
	      (let ((folder (buffer-get (car buffers) 'IMAIL-FOLDER #f)))
		(if folder
		    (cons folder folders)
		    folders)))
	(reverse! folders))))

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
			     (if buffer* (select-buffer buffer* window))
			     (window-delete! window)))
		       (buffer-windows buffer))
	     (bury-buffer buffer))))
     (buffer-get folder-buffer 'IMAIL-ASSOCIATED-BUFFERS '()))
    (let ((buffer (other-buffer folder-buffer)))
      (if buffer
	  (for-each (lambda (window)
		      (select-buffer buffer window))
		    (buffer-windows folder-buffer))))
    (bury-buffer folder-buffer)))

(define-command imail-input
  "Run IMAIL on a specified folder."
  (lambda ()
    (list (prompt-for-selectable-folder "Run IMAIL on folder" #f
					'HISTORY 'IMAIL
					'REQUIRE-MATCH? #t)))
  (lambda (url-string)
    ((ref-command imail) url-string)))

(define-command imail-save-folder
  "Save the currently selected IMAIL folder."
  ()
  (lambda ()
    (message
     (if (save-resource (selected-folder))
	 "Folder saved"
	 "No changes need to be saved."))))

(define-command imail-toggle-header
  "Show full message headers if pruned headers currently shown, or vice versa."
  ()
  (lambda ()
    (let ((message (selected-message)))
      (message-raw! message
		    (case (message-raw? message)
		      ((#f) 'HEADERS-ONLY)
		      ((HEADERS-ONLY) #f)
		      ((BODY-ONLY) #t)
		      (else 'BODY-ONLY)))
      (select-message (selected-folder) message #t))))

(define-command imail-toggle-message
  "Toggle between standard and raw formats for message."
  ()
  (lambda ()
    (let ((message (selected-message)))
      (message-raw! message
		    (case (message-raw? message)
		      ((#f HEADERS-ONLY) #t)
		      (else #f)))
      (select-message (selected-folder) message #t))))

(define (message-raw? message)
  (get-property message 'RAW? #f))

(define (message-raw! message value)
  (if value
      (store-property! message 'RAW? value)
      (remove-property! message 'RAW?)))

(define-command imail-get-new-mail
  "Probe the mail server for new mail.
Selects the first new message if any new mail.
 (Currently useful only for IMAP folders.)

You can also specify another folder to get mail from.
A prefix argument says to prompt for a URL and append all messages
 from that folder to the current one."
  (lambda ()
    (list (and (command-argument)
	       (prompt-for-selectable-folder "Get messages from folder" #f
					     'HISTORY 'IMAIL-INPUT
					     'HISTORY-INDEX 0
					     'REQUIRE-MATCH? #t))))
  (lambda (url-string)
    (if url-string
	((ref-command imail-input-from-folder) url-string)
	(let* ((folder (selected-folder))
	       (count (object-modification-count folder)))
	  (probe-folder-noisily folder)
	  (cond ((navigator/first-unseen-message folder)
		 => (lambda (unseen) (select-message folder unseen)))
		((<= (object-modification-count folder) count)
		 (message "No changes to mail folder"))
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

(define-command imail-cache
  "Fill any local cache associated with the selected folder.
By default, fetch only parts that would ordinarily be displayed by
  default in-line.
With a prefix argument, fetch every part of every message, whether or
  not it would ordinarily be displayed in-line.
WARNING: With a prefix argument, this command may take a very long
  time to complete if there are many immense attachments in the
  folder."
  "P"
  (lambda (argument)
    (cache-folder-contents
     (selected-folder)
     (let ((buffer (selected-buffer)))
       (lambda (message body-structure cache-procedure)
	 (define (cache entity body selector context buffer)
	   entity selector context buffer
	   (cache-procedure body))
	 (define (ignore entity body selector context buffer)
	   entity body selector context buffer
	   unspecific)
	 (walk-mime-body message
			 body-structure
			 '()
			 (make-walk-mime-context #f 0 #f '())
			 buffer
			 cache
			 (if argument cache ignore)))))))

;;;; URLs

(define (imail-primary-url protocol)
  (let ((url-string (ref-variable imail-primary-folder #f)))
    (if url-string
	(imail-parse-partial-url url-string)
	(imail-default-url protocol))))

(define (imail-parse-partial-url string)
  (parse-url-string string imail-default-url))

(define (imail-default-url protocol)
  (cond ((not protocol)
	 (or (selected-url #f)
	     (let ((folder (selected-folder #f)))
	       (and folder
		    (resource-locator folder)))
	     (imail-default-url "imap")))
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
	((string-ci=? protocol "file") (make-pathname-url "~/RMAIL"))
	(else (error:bad-range-argument protocol))))

(define (imail-default-container)
  (let ((url (selected-url #f)))
    (if url
	(container-url url)
	(let ((container (selected-container #f)))
	  (if container
	      (resource-locator container)
	      (container-url-for-prompt (imail-default-url #f)))))))

(define (maybe-prompt-for-folder prompt . options)
  (or (selected-url-string #f)
      (apply prompt-for-folder prompt #f options)))

(define (maybe-prompt-for-selectable-folder prompt . options)
  (or (selected-url-string #f)
      (apply prompt-for-selectable-folder prompt #f options)))

(define (maybe-prompt-for-container prompt . options)
  (or (selected-url-string #f)
      (apply prompt-for-container prompt #f options)))

(define (prompt-for-url prompt default . options)
  (%prompt-for-url prompt default options #f))

(define (prompt-for-folder prompt default . options)
  (%prompt-for-url prompt default options
		   (lambda (url)
		     (and (folder-url? url)
			  (url-exists? url)))))

(define (prompt-for-selectable-folder prompt default . options)
  (%prompt-for-url prompt default options
		   (lambda (url)
		     (and (folder-url? url)
			  (folder-url-is-selectable? url)))))

(define (prompt-for-container prompt default . options)
  (%prompt-for-url prompt default options
		   (lambda (url)
		     (and (container-url? url)
			  (url-exists? url)))))

(define (%prompt-for-url prompt default options predicate)
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
	       ((not default) (url->string (imail-default-container)))
	       (else (error "Illegal default:" default)))))
    (let ((history (get-option 'HISTORY)))
      (if (null? (prompt-history-strings history))
	  (set-prompt-history-strings! history (list default))))
    (apply prompt-for-completed-string
	   prompt
	   (if (= (or (get-option 'HISTORY-INDEX) -1) -1) default #f)
	   (lambda (string if-unique if-not-unique if-not-found)
	     (url-complete-string string imail-default-url
				  if-unique if-not-unique if-not-found))
	   (lambda (string)
	     (url-string-completions string imail-default-url))
	   (and predicate
		(lambda (string)
		  (predicate (imail-parse-partial-url string))))
	   'DEFAULT-TYPE 'INSERTED-DEFAULT
	   options)))

;;;; Core interface to front end

;;; The mailer core abstraction, which otherwise doesn't know about
;;; the presentation layer, occasionally needs some presentation
;;; services.  The hooks in this section provide them.

(define (imail-ui:present-user-alert procedure)
  (call-with-output-to-temporary-buffer " *IMAIL alert*"
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
  (if (and *imail-message-wrapper-prefix* (< 0 current))
      (if total
	  (if (< current total)
	      (message *imail-message-wrapper-prefix*
		       (string-pad-left
			(number->string
			 (round->exact (* (/ current total) 100)))
			3)
		       "% (of "
		       (number->string total)
		       ")"))
	  (message *imail-message-wrapper-prefix*
		   (number->string current)))))

(define *imail-message-wrapper-prefix* #f)

(define imail-ui:message message)
(define imail-ui:clear-message clear-message)
(define imail-ui:sit-for sit-for)
(define imail-ui:prompt-for-alist-value prompt-for-alist-value)
(define imail-ui:prompt-for-yes-or-no? prompt-for-yes-or-no?)

(define (imail-ui:body-cache-limit message)
  (ref-variable imail-body-cache-limit
		(let ((folder (message-folder message)))
		  (and folder
		       (imail-folder->buffer folder #f)))))

(define (imail-ui:call-with-pass-phrase url receiver)
  (call-with-stored-pass-phrase (url-pass-phrase-key url) receiver))

(define (imail-ui:delete-stored-pass-phrase url)
  (delete-stored-pass-phrase (url-pass-phrase-key url)))

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

(define (select-message folder selector #!optional force?)
  (let ((buffer (imail-folder->buffer folder #t))
	(message
	 (cond ((message? selector)
		(if (message-attached? selector folder)
		    selector
		    (let ((index (message-index selector)))
		      (if (and index (< index (folder-length folder)))
			  (get-message folder index)
			  (last-message folder)))))
	       ((exact-nonnegative-integer? selector)
		(if (< selector (folder-length folder))
		    (get-message folder selector)
		    (error:bad-range-argument selector 'SELECT-MESSAGE)))
	       ((not selector)
		(last-message folder))
	       (else
		(error:wrong-type-argument selector "message selector"
					   'SELECT-MESSAGE)))))
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
		    (insert-message message #f 0 mark)
		    (insert-string "[This folder has no messages in it.]"
				   mark))))
	    (mark-temporary! mark))
	  (set-buffer-point! buffer (buffer-start buffer))
	  (buffer-not-modified! buffer)))
    (if message (message-seen message))
    (signal-modification-event folder 'SELECT-MESSAGE message)))

(define (selected-message #!optional error? buffer)
  (or (let ((buffer
	     (if (or (default-object? buffer) (not buffer))
		 (selected-buffer)
		 buffer)))
	(let ((method (navigator/selected-message buffer)))
	  (if method
	      (method buffer)
	      (let ((buffer (chase-imail-buffer buffer)))
		(let ((message (buffer-get buffer 'IMAIL-MESSAGE #f)))
		  (and message
		       (let ((folder (selected-folder #f buffer)))
			 (if (and folder (message-attached? message folder))
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
     (receive-modification-events folder notice-folder-event)
     (add-kill-buffer-hook buffer disassociate-buffer-from-folder)
     (add-kill-buffer-hook buffer delete-associated-buffers)
     (if (eq? (folder-connection-status folder) 'ONLINE)
	 (start-probe-folder-thread folder)))))

(define (disassociate-buffer-from-folder buffer)
  (without-interrupts
   (lambda ()
     (let ((folder (buffer-get buffer 'IMAIL-FOLDER #f)))
       (if folder
	   (begin
	     (ignore-modification-events folder notice-folder-event)
	     (stop-probe-folder-thread folder)
	     (remove-property! folder 'BUFFER)))))))

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
  (or (find (lambda (buffer)
	      (eq? (buffer-get buffer 'IMAIL-MESSAGE #f) message))
	    (buffer-list))
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

(define (selected-folder #!optional error? buffer)
  (or (buffer-get (chase-imail-buffer
		   (if (or (default-object? buffer) (not buffer))
		       (selected-buffer)
		       buffer))
		  'IMAIL-FOLDER
		  #f)
      (and (if (default-object? error?) #t error?)
	   (error "No selected IMAIL folder."))))

(define (selected-container #!optional error? buffer)
  (let ((buffer
	 (if (or (default-object? buffer) (not buffer))
	     (selected-buffer)
	     buffer)))
    (or (buffer-get buffer 'IMAIL-CONTAINER #f)
	(and (if (default-object? error?) #t error?)
	     (error "Buffer has no IMAIL container:" buffer)))))

(define (set-buffer-imail-container! buffer container)
  (buffer-put! buffer 'IMAIL-CONTAINER container))

(define (selected-container-url #!optional error? buffer)
  (let ((container
	 (selected-container (if (default-object? error?) #t error?)
			     (if (default-object? buffer) #f buffer))))
    (and container
	 (resource-locator container))))

(define (selected-url-string #!optional error? mark)
  (let ((url
	 (selected-url (if (default-object? error?) #t error?)
		       (if (default-object? mark) #f mark))))
    (and url
	 (url->string url))))

(define (selected-url #!optional error? mark)
  (let ((mark
	 (if (or (default-object? mark) (not mark))
	     (current-point)
	     mark)))
    (or (let ((selector
	       (buffer-get (mark-buffer mark) 'IMAIL-URL-SELECTOR #f)))
	  (and selector
	       (selector mark)))
	(and (if (default-object? error?) #t error?)
	     (error "No URL at this location.")))))

(define (set-buffer-imail-url-selector! buffer selector)
  (buffer-put! buffer 'IMAIL-URL-SELECTOR selector))

;;;; Folder-event handling

(define (notice-folder-event folder type parameters)
  parameters
  (if (eq? type 'STATUS)
      (case (folder-connection-status folder)
	((ONLINE) (start-probe-folder-thread folder))
	((OFFLINE) (stop-probe-folder-thread folder))))
  (maybe-add-command-suffix! notice-folder-modifications folder))

(define (notice-folder-modifications folder)
  (let ((buffer (imail-folder->buffer folder #f)))
    (if buffer
	(begin
	  (let ((m (buffer-get buffer 'IMAIL-MESSAGE #f)))
	    (cond ((not m)
		   (let ((m (last-message folder)))
		     (if m
			 (select-message folder m #t))))
		  ((not (message-attached? m folder))
		   (select-message folder
				   (or (first-unseen-message folder)
				       (let ((index (message-index m)))
					 (and (< index (folder-length folder))
					      index)))
				   #t))))
	  (if (and (ref-variable imail-global-mail-notification buffer)
		   (eq? (resource-locator folder) (imail-primary-url "imap")))
	      (notifier:set-mail-string!
	       (if (> (count-unseen-messages folder) 0)
		   "[New Mail]"
		   "")))
	  (buffer-modeline-event! buffer 'PROCESS-STATUS)))))

(define (count-unseen-messages folder)
  (let ((count (get-property folder 'COUNT-UNSEEN-MESSAGES #f))
	(mod-count (object-modification-count folder)))
    (if (and count (= (cdr count) mod-count))
	(car count)
	(let ((n (folder-length folder)))
	  (do ((i (first-unseen-message-index folder) (+ i 1))
	       (unseen 0
		       (if (let loop
			       ((flags
				 (message-flags (%get-message folder i))))
			     (and (pair? flags)
				  (or (string-ci=? "seen" (car flags))
				      (string-ci=? "deleted" (car flags))
				      (loop (cdr flags)))))
			   unseen
			   (+ unseen 1))))
	      ((= i n)
	       (store-property! folder
				'COUNT-UNSEEN-MESSAGES
				(cons unseen mod-count))
	       unseen))))))

;;;; Probe-folder thread

(define (start-probe-folder-thread folder)
  (if (not (get-property folder 'PROBE-REGISTRATION #f))
      (begin
	(stop-probe-folder-thread folder)
	(without-interrupts
	 (lambda ()
	   (let ((interval (ref-variable imail-update-interval #f)))
	     (if interval
		 (store-property! folder
				  'PROBE-REGISTRATION
				  (start-standard-polling-thread
				   (* 1000 interval)
				   (probe-folder-output-processor
				    (weak-cons folder unspecific))
				   (list 'probe-folder
					 (url-presentation-name
					  (resource-locator folder))))))))))))

(define ((probe-folder-output-processor folder))
  (let ((folder (weak-car folder)))
    (and folder
	 (if (and (imail-folder->buffer folder #f)
		  (eq? (folder-connection-status folder) 'ONLINE))
	     (begin
	       (override-next-command!
		(lambda ()
		  (probe-folder-noisily folder)))
	       'FORCE-RETURN)
	     (begin
	       (stop-probe-folder-thread folder)
	       #f)))))

(define (stop-probe-folder-thread folder)
  (without-interrupts
   (lambda ()
     (let ((holder (get-property folder 'PROBE-REGISTRATION #f)))
       (if holder
	   (begin
	     (stop-standard-polling-thread holder)
	     (remove-property! folder 'PROBE-REGISTRATION)))))))

(define (probe-folder-noisily folder)
  (temporary-message "Probing folder "
		     (url-presentation-name (resource-locator folder))
		     "...")
  (probe-folder folder))

;;;; Message insertion procedures

(define (insert-message message inline-only? left-margin mark)
  (let ((raw? (message-raw? message)))
    (insert-header-fields message (and raw? (not (eq? raw? 'BODY-ONLY))) mark)
    (cond ((and raw? (not (eq? raw? 'HEADERS-ONLY)))
	   (insert-message-body message mark))
	  ((mime-entity-body-structure message)
	   => (lambda (body-structure)
		(insert-mime-body message body-structure
				  mark inline-only? left-margin)))
	  (else
	   (call-with-auto-wrapped-output-mark mark left-margin message
	     (lambda (port)
	       (write-message-body message port)))))))

(define (insert-header-fields headers raw? mark)
  (encode-header-fields (let ((headers* (->header-fields headers)))
			  (if raw?
			      headers*
			      (maybe-reformat-headers
			       headers*
			       (or (and (message? headers)
					(imail-message->buffer headers #f))
				   mark)
			       #f)))
			(lambda (string start end)
			  (insert-substring string start end mark))))

(define (maybe-reformat-headers headers context keep-mime?)
  (let ((headers
	 (let ((mime-headers
		(lambda ()
		  (if keep-mime?
		      (filter (lambda (header)
				(re-string-match
				 "^\\(mime-version$\\|content-\\)"
				 (header-field-name header)
				 #t))
			      headers)
		      '()))))
	   (cond ((ref-variable imail-kept-headers context)
		  => (lambda (regexps)
		       (remove-duplicates!
			(append-map*!
			 (mime-headers)
			 (lambda (regexp)
			   (filter (lambda (header)
				     (re-string-match regexp
						      (header-field-name header)
						      #t))
				   headers))
			 regexps)
			(lambda (a b) (eq? a b)))))
		 ((ref-variable imail-ignored-headers context)
		  => (lambda (regexp)
		       (remove-duplicates!
			(append!
			 (remove (lambda (header)
				   (re-string-match regexp
						    (header-field-name header)
						    #t))
				 headers)
			 (mime-headers))
			(lambda (a b) (eq? a b)))))
		 (else headers))))
	(filter (ref-variable imail-message-filter context)))
    (if filter
	(map (lambda (n.v) (make-header-field (car n.v) (cdr n.v)))
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

(define (insert-mime-body message body-structure mark inline-only? left-margin)
  (walk-mime-body
   message
   body-structure
   '()
   (make-walk-mime-context inline-only? left-margin #f '())
   mark
   insert-mime-body-inline
   insert-mime-body-outline))

(define-structure walk-mime-context
  (inline-only? #f read-only #t)
  (left-margin #f read-only #t)
  (enclosure #f read-only #t)
  (boundaries #f read-only #t))

(define (make-walk-mime-subcontext context enclosure boundary)
  (make-walk-mime-context (walk-mime-context-inline-only? context)
			  (walk-mime-context-left-margin context)
			  enclosure
			  (cons (cons boundary (not boundary))
				(walk-mime-context-boundaries context))))

(define (mime-enclosure-type? context type #!optional subtype)
  (let ((enclosure (walk-mime-context-enclosure context)))
    (and enclosure
	 (mime-type? enclosure type subtype))))

(define (mime-type? body type #!optional subtype)
  (and (eq? (mime-body-type body) type)
       (or (default-object? subtype)
	   (not subtype)
	   (eq? (mime-body-subtype body) subtype))))

(define (maybe-insert-mime-boundary context selector mark)
  (let ((boundary
	 (let loop ((boundaries (walk-mime-context-boundaries context)))
	   (and (pair? boundaries)
		(if (cdar boundaries)
		    (caar boundaries)
		    (loop (cdr boundaries)))))))
    (let loop ((boundaries (walk-mime-context-boundaries context)))
      (if (and (pair? boundaries)
	       (not (cdar boundaries)))
	  (begin
	    (set-cdr! (car boundaries) #t)
	    (loop (cdr boundaries)))))
    (if boundary
	(begin
	  (if (not (and (mime-enclosure-type? context 'MULTIPART)
			(mime-type? (walk-mime-context-enclosure context)
				    'MULTIPART)
			(zero? (last selector))))
	      (begin
		(insert-newline mark)
		(insert-newline mark)))
	  (cond ((string? boundary)
		 (insert-string "--" mark)
		 (insert-string boundary mark))
		((eq? 'SGML boundary)
		 (insert-string "<!-- " mark)
		 (insert-chars #\- (- (mark-x-size mark) 10) mark)
		 (insert-string " -->" mark))
		(else
		 (insert-chars #\- (- (mark-x-size mark) 1) mark)))
	  (insert-newline mark)
	  (insert-newline mark)))))

(define (mime-part-encoding context body)
  (let ((encoding
	 (and (mime-enclosure-type? context 'MESSAGE 'RFC822)
	      (mime-body-one-part-encoding
	       (walk-mime-context-enclosure context)))))
    (if (and encoding (not (memq encoding '(7BIT 8BIT BINARY))))
	;; This is illegal, but Netscape does it.
	encoding
	(mime-body-one-part-encoding body))))

(define-generic walk-mime-body
  (entity body selector context mark if-inline if-outline))
(define-generic inline-mime-part? (body context mark))

(define-method walk-mime-body
    (entity (body <mime-body>) selector context mark if-inline if-outline)
  ((if (inline-mime-part? body context mark) if-inline if-outline)
   entity body selector context mark))

(define-method inline-mime-part? ((body <mime-body>) context mark)
  context mark
  (mime-type? body 'MESSAGE 'DELIVERY-STATUS))

(define-method inline-mime-part? ((body <mime-body-message>) context mark)
  body
  (not (and (mime-enclosure-type? context 'MULTIPART 'DIGEST)
	    (ref-variable imail-mime-collapse-digest mark))))

(define-method inline-mime-part? ((body <mime-body-text>) context mark)
  (and (let ((disposition (mime-body-disposition body)))
	 (if disposition
	     (eq? (car disposition) 'INLINE)
	     (or (not (walk-mime-context-enclosure context))
		 (let ((subtype (mime-body-subtype body)))
		   (or (eq? subtype 'PLAIN)
		       (memq subtype
			     (ref-variable imail-inline-mime-text-subtypes
					   mark)))))))
       (known-mime-encoding? (mime-part-encoding context body))
       (re-string-match
	(string-append "\\`"
		       (apply regexp-group
			      (ref-variable imail-known-mime-charsets
					    mark))
		       "\\'")
	(mime-body-parameter body 'CHARSET "us-ascii")
	#t)
       (let ((limit (ref-variable imail-inline-mime-text-limit mark)))
	 (or (not limit)
	     (< (mime-body-one-part-n-octets body) limit)))))

(define-method walk-mime-body
    (entity (body <mime-body-multipart>) selector context mark
	    if-inline if-outline)
  (let ((context
	 (make-walk-mime-subcontext
	  context
	  body
	  (let ((style (ref-variable imail-mime-boundary-style mark)))
	    (if (eq? 'ORIGINAL style)
		(mime-body-parameter body 'BOUNDARY "----------")
		style))))
	(parts (mime-body-multipart-parts body)))
    (if (eq? (mime-body-subtype body) 'ALTERNATIVE)
	(if (pair? parts)
	    (begin
	      (walk-mime-body entity (car parts) `(,@selector 0)
			      context mark if-inline if-outline)
	      (if (ref-variable imail-mime-show-alternatives mark)
		  (do ((parts (cdr parts) (cdr parts))
		       (i 1 (fix:+ i 1)))
		      ((null? parts))
		    (if-outline entity (car parts) `(,@selector ,i) context
				mark)))))
	(do ((parts parts (cdr parts))
	     (i 0 (fix:+ i 1)))
	    ((null? parts))
	  (walk-mime-body entity (car parts) `(,@selector ,i)
			  context mark if-inline if-outline)))))

(define (insert-mime-body-inline entity body selector context mark)
  (maybe-insert-mime-boundary context selector mark)
  (insert-mime-info (make-mime-info #t entity body selector context) mark))

(define (insert-mime-body-outline entity body selector context mark)
  (if (not (walk-mime-context-inline-only? context))
      (begin
	(maybe-insert-mime-boundary context selector mark)
	(insert-mime-info (make-mime-info #f entity body selector context)
			  mark))))

(define (insert-mime-info info mark)
  (let ((start (mark-right-inserting-copy mark))
	(entity (mime-info-entity info))
	(body (mime-info-body info))
	(selector (mime-info-selector info))
	(context (mime-info-context info)))
    (if (mime-info-expanded? info mark)
	(begin
	  (if (and (ref-variable imail-mime-show-headers mark)
		   (not (inline-mime-part? body context mark))
		   (mime-enclosure-type? context 'MULTIPART))
	      (insert-header-fields (mime-body-header-fields body) #t mark))
	  (insert-mime-body-inline* entity body selector context mark))
	(insert-mime-outline
	 (compute-mime-body-outline body
				    (mime-attachment-name info #f)
				    context)
	 mark))
    (attach-mime-info start mark info)
    (mark-temporary! start)))

(define (insert-mime-outline parameters mark)
  (let ((indentation "    "))
    (insert-string "<imail-part" mark)
    (insert-newline mark)
    (for-each (lambda (n.v)
		(if n.v
		    (begin
		      (insert-string indentation mark)
		      (insert-string (car n.v) mark)
		      (insert-string "=" mark)
		      (insert (let ((value (cdr n.v)))
				(if (string? value)
				    value
				    (write-to-string value)))
			      mark)
		      (insert-newline mark))))
	      parameters)
    (insert-string indentation mark)
    (insert-string "/>" mark)))

(define-generic insert-mime-body-inline* (entity body selector context mark))

(define-method insert-mime-body-inline*
    (entity (body <mime-body>) selector context mark)
  entity selector			;ignore
  (call-with-auto-wrapped-output-mark
   mark
   (walk-mime-context-left-margin context)
   body
   (lambda (port)
     (write-mime-body body port))))

(define-method insert-mime-body-inline*
    (entity (body <mime-body-one-part>) selector context mark)
  entity selector			;ignore
  (call-with-auto-wrapped-output-mark
   mark
   (walk-mime-context-left-margin context)
   body
   (lambda (port)
     (call-with-mime-decoding-output-port
      (mime-part-encoding context body)
      port
      #t
      (lambda (port)
	(with-mime-best-effort
	 (lambda ()
	   (write-mime-body body port))))))))

(define-method insert-mime-body-inline*
    (entity (body <mime-body-message>) selector context mark)
  (insert-header-fields (mime-body-message-header-fields body) #f mark)
  (walk-mime-body entity
		  (mime-body-message-body body)
		  `(,@selector BODY)
		  (make-walk-mime-subcontext context body #f)
		  mark
		  insert-mime-body-inline
		  insert-mime-body-outline))

(define-method insert-mime-body-inline*
    (entity (body <mime-body-multipart>) selector context mark)
  (walk-mime-body entity body selector context mark
		  insert-mime-body-inline
		  insert-mime-body-outline))

(define-generic compute-mime-body-outline (body name context))

(define-method compute-mime-body-outline ((body <mime-body>) name context)
  context
  (list (and name (cons "name" name))
	(cons "type" (mime-body-type-string body))
	(and (eq? (mime-body-type body) 'TEXT)
	     (cons "charset" (mime-body-parameter body 'CHARSET "us-ascii")))))

(define-method compute-mime-body-outline
    ((body <mime-body-one-part>) name context)
  (append (call-next-method body name context)
	  (list (let ((encoding (mime-body-one-part-encoding body)))
		  (and (not (known-mime-encoding? encoding))
		       (cons "encoding" encoding)))
		(cons "length" (mime-body-one-part-n-octets body)))))

(define-method compute-mime-body-outline
    ((body <mime-body-message>) name context)
  name
  (let ((envelope (mime-body-message-envelope body)))
    (list (and (not (mime-enclosure-type? context 'MULTIPART 'DIGEST))
	       (cons "type" (mime-body-type-string body)))
	  (let ((from (mime-envelope-from envelope)))
	    (and (pair? from)
		 (cons
		  "from"
		  (or (mime-address-name (car from))
		      (string-append (mime-address-mailbox (car from))
				     "@"
				     (mime-address-host (car from)))))))
	  (let ((subject (mime-envelope-subject envelope)))
	    (and subject
		 (cons "subject" subject)))
	  (cons "length" (mime-body-one-part-n-octets body)))))

(define (mime-attachment-name info provide-default?)
  (or (mime-body-parameter (mime-info-body info) 'NAME #f)
      (mime-body-disposition-filename (mime-info-body info))
      (and provide-default?
	   (string-append (if (mime-info-inline? info)
			      "inline-"
			      "unnamed-attachment-")
			  (let ((selector (mime-info-selector info)))
			    (if (null? selector)
				"0"
				(decorated-string-append
				 "" "." ""
				 (map (lambda (n) (number->string (+ n 1)))
				      selector))))))))

(define (attach-mime-info start end info)
  ;; Scan forward for each change in the IMAIL-MIME-INFO property, and
  ;; for any region in which it is not set (between inferior MIME
  ;; entities) we set it.  What we really want is some way to layer
  ;; text properties `under' existing ones, but the text property
  ;; facility doesn't support that.
  (define (attach start end)
    (if (not (region-get start 'IMAIL-MIME-INFO #f))
	(region-put! start end 'IMAIL-MIME-INFO info #t)))
  (let loop ((mark start))
    (cond ((find-next-specific-property-change mark end 'IMAIL-MIME-INFO)
	   => (lambda (mark*)
		(attach mark mark*)
		(loop mark*)))
	  (else
	   (attach mark end)))))

(define (mark-mime-info mark)
  (region-get mark 'IMAIL-MIME-INFO #f))

(define (buffer-mime-info buffer)
  (let ((end (buffer-end buffer)))
    (let loop ((start (buffer-start buffer)) (attachments '()))
      (let ((mark
	     (find-next-specific-property-change start end 'IMAIL-MIME-INFO))
	    (attachments
	     (let ((attachment (region-get start 'IMAIL-MIME-INFO #f)))
	       (if attachment
		   (cons (cons attachment start) attachments)
		   attachments))))
	(if mark
	    (loop mark attachments)
	    (reverse! attachments))))))

(define (mime-body-region mark)
  (specific-property-region mark 'IMAIL-MIME-INFO
    (lambda (i1 i2)
      (mime-body-enclosed? (mime-info-body i1) (mime-info-body i2)))))

(define (mime-attachment? info)
  (not (mime-info-inline? info)))

(define-structure mime-info
  (inline? #f)
  (entity #f read-only #t)
  (body #f read-only #t)
  (selector #f read-only #t)
  (context #f read-only #t))

(define (mime-info-expanded? info mark)
  (let ((expansions (buffer-get (->buffer mark) 'IMAIL-MIME-EXPANSIONS #f))
	(key (cons (mime-info-entity info) (mime-info-selector info)))
	(inline? (mime-info-inline? info)))
    (if expansions
	(hash-table-ref/default expansions key inline?)
	inline?)))

(define (set-mime-info-expanded?! info mark expanded?)
  (let ((buffer (->buffer mark))
	(key (cons (mime-info-entity info) (mime-info-selector info))))
    (if (if (mime-info-inline? info) expanded? (not expanded?))
	(cond ((buffer-get buffer 'IMAIL-MIME-EXPANSIONS #f)
	       => (lambda (expansions)
		    (hash-table-delete! expansions key)
		    (if (zero? (hash-table-size expansions))
			(buffer-remove! buffer 'IMAIL-MIME-EXPANSIONS)))))
	(hash-table-set!
	 (or (buffer-get buffer 'IMAIL-MIME-EXPANSIONS #f)
	     (let ((expansions (make-equal-hash-table)))
	       (buffer-put! buffer 'IMAIL-MIME-EXPANSIONS expansions)
	       expansions))
	 key
	 expanded?))))

;;;; Automatic wrap/fill

(define (call-with-auto-wrapped-output-mark mark left-margin object generator)
  (let ((auto-wrap (ref-variable imail-auto-wrap mark)))
    (if (and auto-wrap (mime-body-wrapped? object))
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