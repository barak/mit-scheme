;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/rmail.scm,v 1.10 1991/10/26 21:08:26 cph Exp $
;;;
;;;	Copyright (c) 1991 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;
;;; NOTE: Parts of this program (Edwin) were created by translation
;;; from corresponding parts of GNU Emacs.  Users should be aware that
;;; the GNU GENERAL PUBLIC LICENSE may apply to these parts.  A copy
;;; of that license should have been included along with this file.
;;;

;;;; RMAIL Mail Reader

(declare (usual-integrations))

(define rmail-spool-directory
  "/usr/mail/")

(define-variable rmail-file-name
  ""
  "~/RMAIL"
  string?)

(define-variable rmail-last-file
  "Last file used by \\[rmail-output]."
  "~/xmail"
  string?)

(define-variable rmail-last-rmail-file
  "Last file used by \\[rmail-output-to-rmail-file]."
  "~/XMAIL"
  string?)

(define-variable rmail-inbox-list
  ""
  '()
  list-of-strings?)

(define-variable rmail-primary-inbox-list
  "List of files which are inboxes for user's primary mail file ~/RMAIL.
Empty list means the default, which is (\"~/mbox\" \"/usr/spool/mail/$USER\")
\(the second name varies depending on the operating system)."
  '()
  list-of-strings?)

(define-variable rmail-dont-reply-to-names
  "A regular expression specifying names to prune in replying to messages.
#f means don't reply to yourself."
  false
  string-or-false?)

(define-variable rmail-default-dont-reply-to-names
  "A regular expression specifying part of the value of the default value of
the variable `rmail-dont-reply-to-names', for when the user does not set
`rmail-dont-reply-to-names' explicitly.  (The other part of the default
value is the user's name.)
It is useful to set this variable in the site customisation file."
  "info-"
  string?)

(define-variable rmail-ignored-headers
  "Gubbish header fields one would rather not see."
  "^via:\\|^mail-from:\\|^origin:\\|^status:\\|^received:\\|^[a-z-]*message-id:\\|^summary-line:\\|^errors-to:"
  string-or-false?)

(define-variable rmail-message-filter
  "If not #f, is a filter procedure for new headers in RMAIL.
Called with the start and end marks of the header as arguments."
  false
  (lambda (object) (or (not object) (procedure? object))))

(define-variable rmail-delete-after-output
  "True means automatically delete a message that is copied to a file."
  false
  boolean?)

(define-variable rmail-reply-with-re
  "True means prepend subject with Re: in replies."
  false
  boolean?)

(define-variable rmail-mode-hook
  "An event distributor that is invoked when entering RMAIL mode."
  (make-event-distributor))

(define-major-mode rmail read-only "RMAIL"
  "Rmail Mode is used by \\[rmail] for editing Rmail files.
All normal editing commands are turned off.
Instead, these commands are available:

.	Move point to front of this message (same as \\[beginning-of-buffer]).
SPC	Scroll to next screen of this message.
DEL	Scroll to previous screen of this message.
n	Move to Next non-deleted message.
p	Move to Previous non-deleted message.
M-n	Move to Next message whether deleted or not.
M-p	Move to Previous message whether deleted or not.
>	Move to the last message in Rmail file.
j	Jump to message specified by numeric position in file.
M-s	Search for string and show message it is found in.
d	Delete this message, move to next nondeleted.
C-d	Delete this message, move to previous nondeleted.
u	Undelete message.  Tries current message, then earlier messages
	till a deleted message is found.
e	Expunge deleted messages.
s	Expunge and save the file.
q       Quit Rmail: expunge, save, then switch to another buffer.
C-x C-s Save without expunging.
g	Move new mail from system spool directory or mbox into this file.
m	Mail a message (same as \\[mail-other-window]).
c	Continue composing outgoing message started before.
r	Reply to this message.  Like m but initializes some fields.
f	Forward this message to another user.
o       Output this message to an Rmail file (append it).
C-o	Output this message to a Unix-format mail file (append it).
i	Input Rmail file.  Run Rmail on that file.
a	Add label to message.  It will be displayed in the mode line.
k	Kill label.  Remove a label from current message.
C-M-n   Move to Next message with specified label
          (label defaults to last one specified).
          Standard labels: filed, unseen, answered, forwarded, deleted.
          Any other label is present only if you add it with `a'.
C-M-p   Move to Previous message with specified label
C-M-h	Show headers buffer, with a one line summary of each message.
C-M-l	Like h only just messages with particular label(s) are summarized.
C-M-r   Like h only just messages with particular recipient(s) are summarized.
t	Toggle header, show Rmail header if unformatted or vice versa.
w	Edit the current message.  C-c C-c to return to Rmail."
  (guarantee-variables-initialized)
  (let ((buffer (current-buffer)))
    (local-set-variable! mode-line-modified "--- ")
    (local-set-variable! version-control 'NEVER)
    (local-set-variable! file-precious-flag true)
    (local-set-variable! require-final-newline false)
    (local-set-variable! rmail-last-file (ref-variable rmail-last-file))
    (local-set-variable!
     rmail-inbox-list
     (let ((inboxes (parse-file-inboxes buffer)))
       (if (and (null? inboxes)
		(pathname=? (buffer-pathname buffer)
			    (->pathname (ref-variable rmail-file-name))))
	   (ref-variable rmail-primary-inbox-list)
	   inboxes)))
    (buffer-put! buffer 'REVERT-BUFFER-METHOD rmail-revert-buffer)
    (memoize-buffer buffer)
    (set-buffer-read-only! buffer))
  (event-distributor/invoke! (ref-variable rmail-mode-hook)))

(define-major-mode rmail-edit text "RMAIL Edit"
  "Major mode for editing the contents of an RMAIL message.
The editing commands are the same as in Text mode,
together with two commands to return to regular RMAIL:
  * \\[rmail-abort-edit] cancels the changes you have made and returns to RMAIL
  * \\[rmail-cease-edit] makes them permanent.")

(define (guarantee-variables-initialized)
  (if (null? (ref-variable rmail-primary-inbox-list))
      (set-variable! rmail-primary-inbox-list
		     (list "~/mbox"
			   (string-append rmail-spool-directory
					  (unix/current-user-name)))))
  (if (not (ref-variable rmail-dont-reply-to-names))
      (set-variable!
       rmail-dont-reply-to-names
       (string-append
	(let ((rmail-default-dont-reply-to-names
	       (ref-variable rmail-default-dont-reply-to-names)))
	  (if rmail-default-dont-reply-to-names
	      (string-append rmail-default-dont-reply-to-names "\\|")
	      ""))
	(re-quote-string (unix/current-user-name))
	"\\>")))
  (if (not umail-message-end-regexp)
      (set! umail-message-end-regexp
	    (string-append "\\("
			   umail-message-start-regexp
			   "\\|"
			   mmdf-message-start-regexp
			   "\\|"
			   babyl-header-start-regexp
			   "\\|^[\037]?"
			   babyl-message-start-regexp
			   "\\)"))))

(define (parse-file-inboxes buffer)
  (let ((start (buffer-start buffer))
	(end (buffer-end buffer)))
    (if (re-match-forward babyl-header-start-regexp start end false)
	(let ((end
	       (if (re-search-forward babyl-header-end-regexp start end false)
		   (re-match-start 0)
		   end)))
	  (let ((start (search-forward "\nMail:" start end true)))
	    (if start
		(parse-comma-list start end)
		'())))
	'())))

(define (parse-comma-list start end)
  (let loop ((start start))
    (let ((start (skip-chars-forward " " start end)))
      (let ((m (skip-chars-forward "^," start end)))
	(cons (extract-string start (skip-chars-backward " " m start))
	      (if (mark< m end)
		  (loop (mark1+ m))
		  '()))))))

(define-key 'rmail #\.		'beginning-of-buffer)
(define-key 'rmail #\space	'scroll-up)
(define-key 'rmail #\rubout	'scroll-down)
(define-key 'rmail #\n		'rmail-next-undeleted-message)
(define-key 'rmail #\p		'rmail-previous-undeleted-message)
(define-key 'rmail #\m-n	'rmail-next-message)
(define-key 'rmail #\m-p	'rmail-previous-message)
(define-key 'rmail #\c-m-n	'rmail-next-labeled-message)
(define-key 'rmail #\c-m-p	'rmail-previous-labeled-message)
(define-key 'rmail #\a		'rmail-add-label)
(define-key 'rmail #\k		'rmail-kill-label)
(define-key 'rmail #\d		'rmail-delete-forward)
(define-key 'rmail #\u		'rmail-undelete-previous-message)
(define-key 'rmail #\e		'rmail-expunge)
(define-key 'rmail #\x		'rmail-expunge)
(define-key 'rmail #\s		'rmail-expunge-and-save)
(define-key 'rmail #\g		'rmail-get-new-mail)
(define-key 'rmail #\h		'rmail-summary)
(define-key 'rmail #\c-m-h	'rmail-summary)
(define-key 'rmail #\l		'rmail-summary-by-labels)
(define-key 'rmail #\c-m-l	'rmail-summary-by-labels)
(define-key 'rmail #\c-m-r	'rmail-summary-by-recipients)
(define-key 'rmail #\t		'rmail-toggle-header)
(define-key 'rmail #\m		'rmail-mail)
(define-key 'rmail #\r		'rmail-reply)
(define-key 'rmail #\c		'rmail-continue)
(define-key 'rmail #\f		'rmail-forward)
(define-key 'rmail #\m-s	'rmail-search)
(define-key 'rmail #\j		'rmail-show-message)
(define-key 'rmail #\o		'rmail-output-to-rmail-file)
(define-key 'rmail #\c-o	'rmail-output)
(define-key 'rmail #\i		'rmail-input)
(define-key 'rmail #\q		'rmail-quit)
(define-key 'rmail #\>		'rmail-last-message)
(define-key 'rmail #\?		'describe-mode)
(define-key 'rmail #\w		'rmail-edit-current-message)
(define-key 'rmail #\c-d	'rmail-delete-backward)

(define-command rmail
  "Read and edit incoming mail.
Moves messages into file named by  rmail-file-name  (a babyl format file)
 and edits that file in RMAIL Mode.
Type \\[describe-mode] once editing that file, for a list of RMAIL commands.

May be called with filename as argument;
then performs rmail editing on that file,
but does not copy any new mail into the file."
  (lambda ()
    (list (and (command-argument)
	       (pathname->string
		(prompt-for-input-truename "Run rmail on RMAIL file"
					   false)))))
  (lambda (filename)
    (rmail-find-file (or filename (ref-variable rmail-file-name)))
    (let ((mode (current-major-mode)))
      (cond ((eq? mode (ref-mode-object rmail-edit))
	     (editor-error "Exit rmail-edit mode before getting new mail"))
	    ((not (eq? mode (ref-mode-object rmail)))
	     (set-current-major-mode! (ref-mode-object rmail)))))
    ((ref-command rmail-get-new-mail) false)))

(define-command rmail-input
  "Run RMAIL on file FILENAME."
  "FRun rmail on RMAIL file"
  (ref-command rmail))

(define (rmail-find-file filename)
  (fluid-let ((after-find-file rmail-after-find-file))
    (find-file filename)))

(define (rmail-find-file-revert buffer)
  (fluid-let ((after-find-file rmail-after-find-file))
    (find-file-revert buffer)))

(define (rmail-revert-buffer buffer dont-use-auto-save? dont-confirm?)
  (let ((n
	 (let ((memo (buffer-msg-memo buffer)))
	   (and (msg-memo? memo)
		(msg-memo/number memo)))))
    (fluid-let ((after-find-file rmail-after-find-file))
      (revert-buffer-default buffer dont-use-auto-save? dont-confirm?))
    (show-message buffer
		  (and n
		       (let ((memo (buffer-msg-memo buffer)))
			 (and (msg-memo? memo)
			      (<= n (msg-memo/number (msg-memo/last memo)))
			      n))))))

(define (rmail-after-find-file buffer pathname)
  pathname
  ;; No need to auto save RMAIL files.
  (disable-buffer-auto-save! buffer)
  (convert-buffer-to-babyl-format buffer)
  (set-buffer-major-mode! buffer (ref-mode-object rmail)))

(define-command rmail-quit
  "Quit out of RMAIL."
  ()
  (lambda ()
    ((ref-command rmail-expunge-and-save))
    ((ref-command bury-buffer))))

(define-command rmail-expunge-and-save
  "Expunge and save RMAIL file."
  ()
  (lambda ()
    ((ref-command rmail-expunge))
    ((ref-command save-buffer) false)))

;;;; Mail input

(define-command rmail-get-new-mail
  "Move any new mail from this RMAIL file's inbox files.
The inbox files can be specified with the file's Mail: option.
The variable rmail-primary-inbox-list specifies the inboxes for
your primary RMAIL file if it has no Mail: option.
These are normally your ~/mbox and your /usr/spool/mail/$USER.

You can also specify the file to get new mail from.  In this
case, the file of new mail is not changed or deleted.
Noninteractively, you can pass the inbox file name as an argument.
Interactively, a prefix argument causes us to read a file name
and use that file as the inbox."
  (lambda ()
    (list (and (command-argument)
	       (pathname->string
		(prompt-for-input-truename "Get new mail from file"
					   false)))))
  (lambda (filename)
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
		(if filename
		    (get-new-mail buffer (list filename) false)
		    (get-new-mail buffer
				  (ref-variable rmail-inbox-list)
				  true))))))
	(show-message
	 buffer
	 (let ((memo (buffer-msg-memo buffer)))
	   (cond ((not (msg-memo? memo)) 0)
		 ((> (msg-memo/number (msg-memo/last memo)) n-messages)
		  (+ n-messages 1))
		 (else (msg-memo/number memo)))))))))

(define (get-new-mail buffer inbox-list delete-inboxes?)
  (let ((start (mark-right-inserting-copy (buffer-end buffer)))
	(end (mark-left-inserting-copy (buffer-end buffer)))
	(modified? (buffer-modified? buffer)))
    (delete-string (skip-chars-backward " \t\n" end) end)
    (let ((inserted-inboxes
	   (let loop ((filenames inbox-list) (result '()))
	     (if (null? filenames)
		 result
		 (loop (cdr filenames)
		       (let ((pathname
			      (insert-inbox-text buffer
						 end
						 (car filenames)
						 delete-inboxes?)))
			 (if pathname
			     (cons pathname result)
			     result)))))))
      (let ((new-messages (convert-region-to-babyl-format start end)))
	(if (> new-messages 0)
	    (begin
	      (memoize-messages buffer start end)
	      (save-buffer buffer
			   ;; If buffer has not changed yet, and has
			   ;; not been saved yet, don't replace the
			   ;; old backup file now.
			   (if (and (ref-variable make-backup-files buffer)
				    modified?)
			       false
			       'NO-BACKUP))))
	(if delete-inboxes?
	    (for-each (lambda (pathname)
			(catch-file-errors (lambda () unspecific)
					   (lambda () (delete-file pathname))))
		      inserted-inboxes))
	(cond ((> new-messages 0)
	       (message new-messages
			" new message"
			(if (= new-messages 1) "" "s")
			" read"))
	      ((not (null? inbox-list))
	       (message "(No new mail has arrived)")))
	(mark-temporary! end)
	(mark-temporary! start)
	new-messages))))

(define (insert-inbox-text buffer mark filename rename?)
  (let ((insert
	 (lambda (pathname)
	   (and (file-exists? pathname)
		(let ((mark (mark-left-inserting-copy mark)))
		  (insert-file mark pathname)
		  (if (let ((char (mark-left-char mark)))
			(and char
			     (not (char=? char #\newline))))
		      (insert-newline mark))
		  (mark-temporary! mark)
		  pathname)))))
    (let ((source (->pathname filename)))
      (cond ((not rename?)
	     (insert source))
	    ((string=? rmail-spool-directory
		       (pathname-directory-string source))
	     (rename-inbox-using-movemail source
					  insert
					  (buffer-default-directory buffer)))
	    (else
	     (rename-inbox-using-rename source insert))))))

(define (rename-inbox-using-rename source insert)
  (let ((target
	 (string->pathname (string-append (pathname->string source) "~"))))
    (let ((msg
	   (string-append "Getting mail from "
			  (pathname->string source)
			  "...")))
      (message msg)
      (if (and (file-exists? source) (not (file-exists? target)))
	  (rename-file source target))
      (let ((value (insert target)))
	(message msg "done")
	value))))

(define (rename-inbox-using-movemail source insert directory)
  (let ((source
	 ;; On some systems, /usr/spool/mail/foo is a directory and
	 ;; the actual inbox is /usr/spool/mail/foo/foo.
	 (if (file-directory? source)
	     (merge-pathnames (string->pathname (pathname-name source))
			      (pathname-as-directory source))
	     source))
	(target
	 (merge-pathnames (string->pathname ".newmail")
			  (->pathname directory))))
    (let ((msg
	   (string-append "Getting mail from "
			  (pathname->string source)
			  "...")))
      (message msg)
      (if (and (file-exists? source)
	       (not (file-exists? target)))
	  (let ((error-buffer (temporary-buffer " movemail errors")))
	    (let ((start (buffer-start error-buffer))
		  (end (buffer-end error-buffer)))
	      (run-synchronous-process false start false false
				       (pathname->string
					(edwin-etc-pathname "movemail"))
				       (pathname->string source)
				       (pathname->string target))
	      (if (mark< start end)
		  (error
		   (let ((m
			  (or (match-forward "movemail: " start end false)
			      start)))
		     (string-append
		      "movemail: "
		      (extract-string
		       m
		       (skip-chars-backward " \t" (line-end m 0) m)))))))
	    (kill-buffer error-buffer)))
      (let ((value (insert target)))
	(message msg "done")
	value))))

;;;; Moving around

(define-command rmail-next-message
  "Show following message whether deleted or not.
With prefix argument N, moves forward N messages,
or backward if N is negative."
  "p"
  (lambda (n)
    (cond ((> n 0)
	   (let loop ((n n) (memo (current-msg-memo)) (winner false))
	     (let ((next (msg-memo/next memo)))
	       (cond ((not next)
		      (if winner (set-current-msg-memo! winner))
		      (message "No following message"))
		     ((= n 1)
		      (set-current-msg-memo! next))
		     (else
		      (loop (- n 1) next next))))))
	  ((< n 0)
	   ((ref-command rmail-previous-message) (- n))))))

(define-command rmail-previous-message
  "Show previous message whether deleted or not.
With prefix argument N, moves backward N messages,
or forward if N is negative."
  "p"
  (lambda (n)
    (cond ((> n 0)
	   (let loop ((n n) (memo (current-msg-memo)) (winner false))
	     (let ((previous (msg-memo/previous memo)))
	       (cond ((not previous)
		      (if winner (set-current-msg-memo! winner))
		      (message "No previous message"))
		     ((= n 1)
		      (set-current-msg-memo! previous))
		     (else
		      (loop (- n 1) previous previous))))))
	  ((< n 0)
	   ((ref-command rmail-next-message) (- n))))))

(define-command rmail-next-undeleted-message
  "Show following non-deleted message.
With prefix argument N, moves forward N non-deleted messages,
or backward if N is negative."
  "p"
  (lambda (n)
    (cond ((> n 0)
	   (let loop ((n n) (memo (current-msg-memo)) (winner false))
	     (let ((next (msg-memo/next-undeleted memo)))
	       (cond ((not next)
		      (if winner (set-current-msg-memo! winner))
		      (message "No following undeleted message"))
		     ((= n 1)
		      (set-current-msg-memo! next))
		     (else
		      (loop (- n 1) next next))))))
	  ((< n 0)
	   ((ref-command rmail-previous-undeleted-message) (- n))))))

(define-command rmail-previous-undeleted-message
  "Show previous non-deleted message.
With prefix argument N, moves backward N non-deleted messages,
or forward if N is negative."
  "p"
  (lambda (n)
    (cond ((> n 0)
	   (let loop ((n n) (memo (current-msg-memo)) (winner false))
	     (let ((previous (msg-memo/previous-undeleted memo)))
	       (cond ((not previous)
		      (if winner (set-current-msg-memo! winner))
		      (message "No previous undeleted message"))
		     ((= n 1)
		      (set-current-msg-memo! previous))
		     (else
		      (loop (- n 1) previous previous))))))
	  ((< n 0)
	   ((ref-command rmail-next-undeleted-message) (- n))))))

(define-command rmail-show-message
  "Show message number N (prefix argument), counting from start of file."
  "p"
  (lambda (n)
    (show-message (current-buffer) n)))

(define-command rmail-last-message
  "Show last message in file."
  ()
  (lambda ()
    (set-current-msg-memo! (last-msg-memo))))

(define-command rmail-search
  "Show message containing next match for REGEXP.
Search in reverse (earlier messages) with 2nd arg REVERSEP true.
Interactively, empty argument means use same regexp used last time,
and reverse search is specified by a negative numeric arg."
  (lambda ()
    (let ((reverse? (< (command-argument-numeric-value (command-argument)) 0)))
      (let ((regexp
	     (prompt-for-string (string-append (if reverse? "Reverse " "")
					       "Rmail search (regexp)")
				search-last-regexp)))
	(set! search-last-regexp regexp)
	(list regexp reverse?))))
  (lambda (regexp reverse?)
    (let ((buffer (current-buffer))
	  (memo (current-msg-memo))
	  (msg
	   (string-append (if reverse? "Reverse " "")
			  "Rmail search for "
			  regexp
			  "...")))
      (message msg)
      (with-values
	  (lambda ()
	    (without-clipping buffer
	      (lambda ()
		(if reverse?
		    (let loop ((memo memo))
		      (let ((memo (msg-memo/previous memo)))
			(cond ((not memo)
			       (values false false))
			      ((re-search-backward regexp
						   (msg-memo/end-body memo)
						   (msg-memo/start-body memo))
			       =>
			       (lambda (mark) (values memo mark)))
			      (else
			       (loop memo)))))
		    (let loop ((memo memo))
		      (let ((memo (msg-memo/next memo)))
			(cond ((not memo)
			       (values false false))
			      ((re-search-forward regexp
						  (msg-memo/start-body memo)
						  (msg-memo/end-body memo))
			       =>
			       (lambda (mark) (values memo mark)))
			      (else
			       (loop memo)))))))))
	(lambda (memo mark)
	  (if memo
	      (begin
		(select-message buffer memo)
		(set-current-point! mark)
		(message msg "done"))
	      (editor-failure "Search failed: " regexp)))))))

(define search-last-regexp
  false)

(define (show-message buffer n)
  (let ((memo (buffer-msg-memo buffer)))
    (if (not (msg-memo? memo))
	(begin
	  (let ((start (buffer-start buffer)))
	    (let ((m
		   (re-search-backward babyl-header-end-regexp
				       (buffer-end buffer)
				       start
				       false)))
	      (if m
		  (narrow-to-region start (mark1+ m))))
	    (set-buffer-point! buffer start))
	  (if (current-buffer? buffer)
	      (begin (update-mode-line! buffer)
		     (message "No messages"))))
	(let ((last (msg-memo/last memo)))
	  (cond ((not n)
		 (select-message buffer last))
		((<= 1 n (msg-memo/number last))
		 (select-message buffer (msg-memo/nth memo n)))
		((current-buffer? buffer)
		 (message "No such message")))))))

(define (current-msg-memo)
  (let ((memo (buffer-msg-memo (current-buffer))))
    (if (not (msg-memo? memo))
	(editor-error "No messages"))
    memo))

(define (last-msg-memo)
  (msg-memo/last (current-msg-memo)))

(define (set-current-msg-memo! memo)
  (select-message (mark-buffer (msg-memo/start memo)) memo))

(define (select-message buffer memo)
  (let ((start (msg-memo/start memo)))
    (set-buffer-msg-memo! buffer memo)
    (widen start)
    (let ((end (msg-memo/end memo)))
      (if (match-forward "\f\n0" start end false)
	  (with-read-only-defeated start
	    (lambda ()
	      (reformat-message start end))))
      (clear-attribute! memo 'UNSEEN)
      (update-mode-line! buffer)
      (let ((start (re-search-forward babyl-eooh-regexp start end false)))
	(narrow-to-region start (mark-1+ end))
	(set-buffer-point! buffer start)))))

(define (update-mode-line! buffer)
  (define-variable-local-value! buffer (ref-variable-object mode-line-process)
    (mode-line-summary-string buffer))
  (buffer-modeline-event! buffer 'PROCESS-STATUS))

(define (mode-line-summary-string buffer)
  (let ((memo (buffer-msg-memo buffer)))
    (and (msg-memo? memo)
	 (apply string-append
		" "
		(number->string (msg-memo/number memo))
		"/"
		(number->string (msg-memo/number (msg-memo/last memo)))
		(append-map!
		 (lambda (label) (list "," label))
		 (append! (map symbol->string (msg-memo/attributes memo))
			  (parse-labels (msg-memo/start memo))))))))

;;;; Message deletion

(define-command rmail-delete-message
  "Delete this message and stay on it."
  ()
  (lambda () (set-attribute! (current-msg-memo) 'DELETED)))

(define-command rmail-undelete-previous-message
  "Back up to deleted message, select it, and undelete it."
  ()
  (lambda ()
    (let ((memo (current-msg-memo)))
      (if (msg-memo/deleted? memo)
	  (clear-attribute! memo 'DELETED)
	  (let ((memo (msg-memo/previous-deleted memo)))
	    (if (not memo) (editor-error "No previous deleted message"))
	    (clear-attribute! memo 'DELETED)
	    (set-current-msg-memo! memo))))))

(define-command rmail-delete-forward
  "Delete this message and move to next nondeleted one.
Deleted messages stay in the file until the \\[rmail-expunge] command is given.
With prefix argument, delete and move backward."
  "P"
  (lambda (backward?)
    (set-attribute! (current-msg-memo) 'DELETED)
    ((ref-command rmail-next-undeleted-message) (if backward? -1 1))))

(define-command rmail-delete-backward
  "Delete this message and move to previous nondeleted one.
Deleted messages stay in the file until the \\[rmail-expunge] command is given."
  ()
  (lambda () ((ref-command rmail-delete-forward) true)))

(define-command rmail-expunge
  "Actually erase all deleted messages in the file."
  ()
  (lambda ()
    (let ((buffer (current-buffer)))
      (let ((memo (buffer-msg-memo buffer)))
	(if (msg-memo? memo)
	    (show-message
	     buffer
	     (with-buffer-open buffer (lambda () (expunge buffer memo)))))))))

(define (expunge buffer memo)
  (let ((old-n (msg-memo/number memo)))
    (let loop ((memo (msg-memo/first memo)) (n 1))
      (let ((next (msg-memo/next memo)))
	(cond ((not (msg-memo/deleted? memo))
	       (set-msg-memo/number! memo n)
	       (if (or (= n old-n) (and (not next) (< n old-n)))
		   (set-buffer-msg-memo! buffer memo))
	       (if next
		   (loop next (+ n 1))
		   (min n old-n)))
	      (next
	       (let ((start (msg-memo/start memo)))
		 (delete-string start (msg-memo/start next))
		 (mark-temporary! start))
	       (let ((previous (msg-memo/previous memo)))
		 (if previous (set-msg-memo/next! previous next))
		 (set-msg-memo/previous! next previous))
	       (loop next n))
	      (else
	       (let ((start (msg-memo/start memo))
		     (end (buffer-last-msg-end buffer)))
		 (set-buffer-last-msg-end! buffer start)
		 (delete-string start end)
		 (mark-temporary! end))
	       (let ((previous (msg-memo/previous memo)))
		 (if previous
		     (begin
		       (set-msg-memo/next! previous false)
		       (if (<= n old-n) (set-buffer-msg-memo! buffer previous))
		       (min (- n 1) old-n))
		     (begin
		       (set-buffer-msg-memo! buffer true)
		       false)))))))))

;;;; Mailing commands

(define-command rmail-mail
  "Send mail in another window.
While composing the message, use \\[mail-yank-original] to yank the
original message into it."
  ()
  (lambda ()
    (make-mail-buffer false select-buffer-other-window
		      false false false false (current-buffer))))

(define-command rmail-continue
  "Continue composing outgoing message previously being composed."
  ()
  (lambda ()
    ((ref-command mail-other-window) true)))

(define-command rmail-forward
  "Forward the current message to another user."
  ()
  (lambda ()
    (let ((buffer (current-buffer))
	  (memo (current-msg-memo)))
      (set-attribute! memo 'FORWARDED)
      (make-mail-buffer
       false
       (if (window-has-no-neighbors? (current-window))
	   select-buffer
	   select-buffer-other-window)
       false
       (without-clipping buffer
	 (lambda ()
	   (with-values (lambda () (original-header-limits memo))
	     (lambda (start end)
	       (string-append
		"["
		(let ((from (fetch-first-field "from" start end)))
		  (if from
		      (addresses->string (strip-quoted-names from))
		      ""))
		": "
		(or (fetch-first-field "subject" start end) "")
		"]")))))
       false
       false
       false)
      (let ((mark (buffer-end (current-buffer))))
	(insert-newline mark)
	(insert-region (buffer-start buffer) (buffer-end buffer) mark)))))

(define-command rmail-reply
  "Reply to the current message.
Normally include CC: to all other recipients of original message;
prefix argument means ignore them.
While composing the reply, use \\[mail-yank-original] to yank the
original message into it."
  "P"
  (lambda (just-sender?)
    (let ((buffer (current-buffer))
	  (memo (current-msg-memo)))
      (set-attribute! memo 'ANSWERED)
      (without-clipping buffer
	(lambda ()
	  (with-values (lambda () (original-header-limits memo))
	    (lambda (start end)
	      (let ((resent-reply-to
		     (fetch-last-field "resent-reply-to" start end))
		    (from (fetch-first-field "from" start end)))
		(make-mail-buffer
		 false
		 select-buffer-other-window
		 (addresses->string
		  (strip-quoted-names
		   (or resent-reply-to
		       (fetch-all-fields "reply-to" start end)
		       from)))
		 (let ((subject
			(or (and resent-reply-to
				 (fetch-last-field "resent-subject" start end))
			    (fetch-first-field "subject" start end))))
		   (if (ref-variable rmail-reply-with-re)
		       (if (and subject (not (string-prefix-ci? "re: " subject)))
			   (string-append "Re: " subject)
			   subject)
		       (if (and subject (string-prefix-ci? "re: " subject))
			   (string-tail subject 4)
			   subject)))
		 (if resent-reply-to
		     (make-in-reply-to-field
		      from
		      (fetch-last-field "resent-date" start end)
		      (fetch-last-field "resent-message-id" start end))
		     (make-in-reply-to-field
		      from
		      (fetch-first-field "date" start end)
		      (fetch-first-field "message-id" start end)))
		 (and (not just-sender?)
		      (let ((to
			     (if resent-reply-to
				 (fetch-last-field "resent-to" start end)
				 (fetch-all-fields "to" start end)))
			    (cc
			     (if resent-reply-to
				 (fetch-last-field "resent-cc" start end)
				 (fetch-all-fields "cc" start end))))
			(let ((cc
			       (if (and to cc)
				   (string-append to ", " cc)
				   (or to cc))))
			  (and cc
			       (addresses->string
				(dont-reply-to (strip-quoted-names cc)))))))
		 buffer)))))))))

(define (original-header-limits memo)
  (let ((start (msg-memo/start memo))
	(end (msg-memo/end memo)))
    (if (match-forward "\f\n0" start end false)
	(begin
	  (if (not (re-search-forward babyl-eooh-regexp start end false))
	      (editor-error))
	  (let ((hstart (re-match-end 0)))
	    (values hstart (header-end hstart end))))
	(values
	 (let ((start (line-start start 2 'ERROR)))
	   (if (match-forward "Summary-line:" start end true)
	       (line-start start 1 'ERROR)
	       start))
	 (begin
	   (if (not (re-search-forward babyl-eooh-regexp start end false))
	       (editor-error))
	   (re-match-start 0))))))

(define (fetch-first-field field start end)
  (let ((fs (re-search-forward (field-name->regexp field) start end true)))
    (and fs
	 (extract-field fs end))))

(define (fetch-last-field field start end)
  (and (re-search-backward (field-name->regexp field) end start true)
       (extract-field (re-match-end 0) end)))

(define (fetch-all-fields field start end)
  (let ((strings
	 (let ((regexp (field-name->regexp field)))
	   (let loop ((start start))
	     (let ((fs (re-search-forward regexp start end true)))
	       (if fs
		   (let ((string (extract-field fs end))
			 (strings (loop fs)))
		     (if string
			 (cons string
			       (if strings
				   (cons ", " strings)
				   '()))
			 strings))
		   '()))))))
    (and (not (null? strings))
	 (apply string-append strings))))

(define (extract-field fs end)
  (let ((fe
	 (skip-chars-backward " \t\n"
			      (if (re-search-forward "^[^ \t]" fs end false)
				  (re-match-start 0)
				  end)
			      fs)))
    (and (mark< fs fe)
	 (extract-string fs fe))))

(define (field-name->regexp field)
  (string-append "^" (re-quote-string field) "[ \t]*:[ \t]*"))

(define (header-end start end)
  (or (search-forward "\n\n" start end false) end))

(define (strip-quoted-names string)
  (let ((address-list (strip-quoted-names-1 (string->rfc822-tokens string))))
    (if (and address-list (null? (cdr address-list)))
	(car address-list)
	(let ((end (string-length string)))
	  (let loop ((start 0))
	    (let ((index (substring-find-next-char string start end #\,)))
	      (if index
		  (cons (string-trim (substring string start index))
			(loop (+ index 1)))
		  (list (string-trim (substring string start end))))))))))

(define (dont-reply-to addresses)
  (let ((pattern
	 (re-compile-pattern
	  (string-append "\\(.*!\\|\\)\\("
			 (ref-variable rmail-dont-reply-to-names)
			 "\\)")
	  true)))
    (let loop ((addresses addresses))
      (cond ((null? addresses)
	     '())
	    ((re-match-string-forward pattern true false (car addresses))
	     (loop (cdr addresses)))
	    (else
	     (cons (car addresses) (loop (cdr addresses))))))))

(define (addresses->string addresses)
  (and (not (null? addresses))
       (separated-append addresses ", ")))

(define (separated-append tokens separator)
  (if (null? (cdr tokens))
      (car tokens)
      (apply string-append
	     (let loop ((tokens tokens))
	       (if (null? (cdr tokens))
		   (list (car tokens))
		   (cons* (car tokens) separator (loop (cdr tokens))))))))

(define (make-in-reply-to-field from date message-id)
  message-id
  (and (or from date)
       (string-append "Msg"
		      (if date
			  (string-append " of " date)
			  "")
		      (if from
			  (string-append " from " from)
			  ""))))

;;;; Address Extraction

(define (strip-quoted-names-1 tokens)
  (define (parse-addr-spec tokens)
    (let ((local-part (parse-list tokens parse-word #\.)))
      (and local-part
	   (not (null? (cdr local-part)))
	   (eqv? #\@ (cadr local-part))
	   (let ((domain (parse-domain (cddr local-part))))
	     (and domain
		  (cons (string-append (separated-append (car local-part) ".")
				       "@"
				       (separated-append (car domain) "."))
			(cdr domain)))))))
  (define (parse-domain tokens)
    (parse-list tokens
		(lambda (tokens)
		  (and (not (null? tokens))
		       (string? (car tokens))
		       (not (eqv? #\" (string-ref (car tokens) 0)))
		       tokens))
		#\.))
  (define (parse-list tokens parse-element separator)
    (let ((first (parse-element tokens)))
      (and first
	   (let loop ((tokens (cdr first)) (words (list (car first))))
	     (let ((next
		    (and (not (null? tokens))
			 (eqv? separator (car tokens))
			 (parse-element (cdr tokens)))))
	       (if next
		   (loop (cdr next) (cons (car next) words))
		   (cons (reverse! words) tokens)))))))
  (define (parse-word tokens)
    (and (not (null? tokens))
	 (string? (car tokens))
	 (not (eqv? #\[ (string-ref (car tokens) 0)))
	 tokens))
  (parse-list
   tokens
   (lambda (tokens)
     (or (parse-addr-spec tokens)
	 (let ((word (parse-word tokens)))
	   (and word
		(let ((tokens
		       (let loop ((tokens (cdr word)))
			 (let ((word (parse-word tokens)))
			   (if word
			       (loop (cdr word))
			       tokens)))))
		  (and (not (null? tokens))
		       (eqv? #\< (car tokens))
		       (let ((addr-spec
			      (parse-addr-spec
			       (let ((domains
				      (parse-list
				       (cdr tokens)
				       (lambda (tokens)
					 (and (not (null? tokens))
					      (eqv? #\@ (car tokens))
					      (parse-domain (cdr tokens))))
				       #\,)))
				 (if (and domains
					  (not (null? (cdr domains)))
					  (eqv? #\: (cadr domains)))
				     (cddr domains)
				     (cdr tokens))))))
			 (and addr-spec
			      (not (null? (cdr addr-spec)))
			      (eqv? #\> (cadr addr-spec))
			      (cons (car addr-spec) (cddr addr-spec))))))))))
   #\,))

;;;; RFC 822 parser

(define (string->rfc822-tokens string)
  (rfc822-clean-tokens (rfc822-read-tokens (string->input-port string))))

(define (rfc822-clean-tokens tokens)
  (let loop ((tokens tokens))
    (if (null? tokens)
	'()
	(let ((rest (loop (cdr tokens))))
	  (if (cond ((char? (car tokens))
		     (eqv? #\space (car tokens)))
		    ((string? (car tokens))
		     (char=? #\( (string-ref (car tokens) 0)))
		    (else true))
	      rest
	      (cons (car tokens) rest))))))

(define rfc822-read-tokens
  (let* ((special-chars
	  (char-set #\( #\) #\[ #\] #\< #\> #\@ #\, #\; #\: #\\ #\" #\.))
	 (atom-chars
	  (char-set-difference (ascii-range->char-set #x21 #x7F)
			       special-chars)))
    (lambda (port)
      (let ((special-char?
	     (lambda (char) (char-set-member? special-chars char)))
	    (atom-char? (lambda (char) (char-set-member? atom-chars char)))
	    (lwsp?
	     (lambda (char) (or (char=? #\space char) (char=? #\tab char))))
	    (loser
	     (lambda (chars)
	       (list (cons 'UNTERMINATED (apply string (reverse! chars)))))))
	(let dispatch ()
	  (let ((char (input-port/read-char port)))
	    (cond ((eof-object? char)
		   '())
		  ((lwsp? char)
		   (do ()
		       ((not (lwsp? (input-port/peek-char port))))
		     (input-port/discard-char port))
		   (cons #\space (dispatch)))
		  ((atom-char? char)
		   ;; atom
		   (let loop ((chars (list char)))
		     (let ((char (input-port/peek-char port)))
		       (if (and (not (eof-object? char))
				(atom-char? char))
			   (begin
			     (input-port/discard-char port)
			     (loop (cons char chars)))
			   (cons (apply string (reverse! chars))
				 (dispatch))))))
		  ((char=? #\" char)
		   ;; quoted string
		   (let loop ((chars (list char)))
		     (let ((char (input-port/read-char port)))
		       (cond ((eof-object? char)
			      (loser chars))
			     ((char=? #\" char)
			      (cons (apply string (reverse! (cons char chars)))
				    (dispatch)))
			     ((char=? #\\ char)
			      (let ((char (input-port/read-char port))
				    (chars (cons char chars)))
				(if (eof-object? char)
				    (loser chars)
				    (loop (cons char chars)))))
			     ((char=? #\newline char)
			      (let ((char (input-port/peek-char port)))
				(if (lwsp? char)
				    (begin
				      (input-port/discard-char port)
				      (loop (cons char chars)))
				    (loser chars))))
			     (else
			      (loop (cons char chars)))))))
		  ((char=? #\( char)
		   ;; comment
		   (let loop ((level 1) (chars (list char)))
		     (let ((char (input-port/read-char port)))
		       (cond ((eof-object? char)
			      (loser chars))
			     ((char=? #\( char)
			      (loop (+ level 1) (cons char chars)))
			     ((char=? #\) char)
			      (let ((chars (cons char chars)))
				(if (= level 1)
				    (cons (apply string (reverse! chars))
					  (dispatch))
				    (loop (- level 1) chars))))
			     ((char=? #\\ char)
			      (let ((char (input-port/read-char port))
				    (chars (cons char chars)))
				(if (eof-object? char)
				    (loser chars)
				    (loop level (cons char chars)))))
			     ((char=? #\newline char)
			      (let ((char (input-port/peek-char port)))
				(if (lwsp? char)
				    (begin
				      (input-port/discard-char port)
				      (loop level (cons char chars)))
				    (loser chars))))
			     (else
			      (loop level (cons char chars)))))))
		  ((char=? #\[ char)
		   ;; domain literal
		   (let loop ((chars (list char)))
		     (let ((char (input-port/peek-char port)))
		       (cond ((or (eof-object? char)
				  (char=? #\[ char))
			      (loser chars))
			     ((char=? #\] char)
			      (input-port/discard-char port)
			      (cons (apply string (reverse! (cons char chars)))
				    (dispatch)))
			     ((char=? #\\ char)
			      (input-port/discard-char port)
			      (let ((char (input-port/read-char port))
				    (chars (cons char chars)))
				(if (eof-object? char)
				    (loser chars)
				    (loop (cons char chars)))))
			     ((char=? #\newline char)
			      (input-port/discard-char port)
			      (let ((char (input-port/peek-char port)))
				(if (lwsp? char)
				    (begin
				      (input-port/discard-char port)
				      (loop (cons char chars)))
				    (loser chars))))
			     (else
			      (input-port/discard-char port)
			      (loop (cons char chars)))))))
		  ((char=? #\newline char)
		   (let ((char (input-port/peek-char port)))
		     (if (and (not (eof-object? char))
			      (lwsp? char))
			 (dispatch)
			 '())))
		  (else
		   (cons (if (special-char? char)
			     char
			     (cons 'ILLEGAL char))
			 (dispatch))))))))))

;;;; Mail output

(define-command rmail-output-to-rmail-file
  "Append the current message to an Rmail file named FILE-NAME.
If the file does not exist, ask if it should be created.
If file is being visited, the message is appended to the
buffer visiting that file."
  (lambda ()
    (list
     (pathname->string
      (get-rmail-output-pathname "Output message to Rmail file"
				 (ref-variable rmail-last-rmail-file)))))
  (lambda (filename)
    (let* ((pathname (->pathname filename))
	   (filename (pathname->string pathname)))
      (set-variable! rmail-last-rmail-file filename)
      (let* ((memo (current-msg-memo))
	     (message
	      (without-clipping (current-buffer)
		(lambda ()
		  (extract-string (msg-memo/start memo)
				  (msg-memo/end memo))))))
	(cond ((pathname->buffer pathname)
	       =>
	       (lambda (buffer)
		 (if (current-buffer? buffer)
		     (editor-error
		      "Can't output message to same file it's already in"))
		 (with-buffer-open buffer
		   (lambda ()
		     (let ((memo (buffer-msg-memo buffer))
			   (end (buffer-end buffer)))
		       (let ((start (mark-right-inserting-copy end))
			     (end (mark-left-inserting-copy end)))
			 (if memo
			     (delete-string (skip-chars-backward " \t\n" end)
					    end))
			 (insert-string message end)
			 (if memo
			     (begin
			       (memoize-messages buffer start end)
			       (select-message buffer memo)))
			 (mark-temporary! start)
			 (mark-temporary! end)))))))
	      ((file-exists? pathname)
	       (let ((port (open-output-file pathname true)))
		 (write-string message port)
		 (close-output-port port)))
	      ((prompt-for-yes-or-no?
		(string-append "\"" filename "\" does not exist, create it"))
	       (call-with-output-file pathname
		 (lambda (port)
		   (write-string babyl-initial-header port)
		   (write-string message port))))
	      (else
	       (editor-error "Output file does not exist")))
	(set-attribute! memo 'FILED)
	(if (ref-variable rmail-delete-after-output)
	    ((ref-command rmail-delete-forward) false))))))

(define-command rmail-output
  "Append this message to Unix mail file named FILE-NAME."
  (lambda ()
    (list
     (pathname->string
      (get-rmail-output-pathname "Output message to Unix mail file"
				 (ref-variable rmail-last-file)))))
  (lambda (filename)
    (let* ((pathname (->pathname filename)))
      (set-variable! rmail-last-file (pathname->string pathname))
      (let ((memo (current-msg-memo)))
	(let ((buffer (temporary-buffer " rmail output")))
	  (let ((end (mark-left-inserting-copy (buffer-end buffer))))
	    (let ((buffer (current-buffer)))
	      (insert-region (buffer-start buffer) (buffer-end buffer) end))
	    (insert-newline end)
	    (let loop ((start (buffer-start buffer)))
	      (if (re-search-forward "^From " start end true)
		  (loop (replace-match ">\\&"))))
	    (mark-temporary! end)
	    (let ((start (buffer-start buffer)))
	      (insert-string
	       (string-append
		"From "
		(or (first-address
		     (fetch-first-field "from" start (header-end start end)))
		    "unknown")
		" "
		(unix/file-time->string (unix/current-file-time))
		"\n")
	       start)))
	  (append-to-file (buffer-region buffer) pathname false)
	  (kill-buffer buffer))
	(set-attribute! memo 'FILED)
	(if (ref-variable rmail-delete-after-output)
	    ((ref-command rmail-delete-forward) false))))))

(define (get-rmail-output-pathname prompt default)
  (let ((default (->pathname default)))
    (let ((name (pathname-name-path default)))
      (let ((pathname
	     (prompt-for-pathname
	      (string-append prompt " (default " (pathname->string name) ")")
	      (pathname-directory-path default)
	      false)))
	(if (file-directory? pathname)
	    (merge-pathnames name (pathname-as-directory pathname))
	    pathname)))))

(define (first-address field)
  (and field
       (let ((addresses (strip-quoted-names field)))
	 (and (not (null? addresses))
	      (car addresses)))))

;;;; Undigestifier

(define-command undigestify-rmail-message
  "Break up a digest message into its constituent messages.
Leaves original message, deleted, before the undigestified messages."
  ()
  (lambda ()
    (let ((buffer (current-buffer))
	  (memo (current-msg-memo)))
      (let ((temp (temporary-buffer " rmail undigestify")))
	(let ((start (buffer-start temp))
	      (end (mark-left-inserting-copy (buffer-end temp))))
	  (insert-string babyl-initial-message-start end)
	  (insert-region (buffer-start buffer) (buffer-end buffer) end)
	  (delete-string (skip-chars-backward " \t\n" end start) end)
	  (insert-string "\n\037" end)
	  (let ((digest-name
		 (first-address
		  (let ((hend (header-end start end)))
		    (or (fetch-first-field "Reply-To" start hend)
			(fetch-first-field "To" start hend)
			(fetch-first-field "Apparently-To" start hend))))))
	    (if (not (and digest-name
			  (let ((m (mark- end 2)))
			    (re-search-backward digest-end-regexp
						m
						(line-start m -10 'LIMIT)
						true))))
		(editor-error "Message is not a digest"))
	    (let ((start
		   (mark-left-inserting-copy (digest-summary-end start end))))
	      (if (not (fetch-first-field "To" start (header-end start end)))
		  (begin
		    (insert-string "To: " start)
		    (insert-string digest-name start)
		    (insert-newline start)))
	      (let loop ()
		(let ((m (digest-message-end start end)))
		  (if m
		      (begin
			(move-mark-to! start m)
			(if (or (match-forward "End " start end true)
				(not
				 (fetch-first-field "To"
						    start
						    (header-end start end))))
			    (begin
			      (insert-string "To: " start)
			      (insert-string digest-name start)
			      (insert-string "\n\n" start)))
			(loop)))))
	      (mark-temporary! start)))
	  (mark-temporary! end))
	(message "Message successfully undigestified")
	(with-buffer-open buffer
	  (lambda ()
	    (let* ((end (msg-memo/end memo))
		   (start (mark-right-inserting-copy end)))
	      (insert-region (buffer-start temp)
			     (buffer-end temp)
			     end)
	      (kill-buffer temp)
	      (memoize-messages-insert buffer start end memo)
	      (mark-temporary! start)))))
      (show-message buffer (msg-memo/number memo))
      ((ref-command rmail-delete-forward) false))))

(define (digest-summary-end start end)
  (if (not (re-search-forward digest-summary-separator-regexp start end false))
      (editor-error "Missing summary separator"))
  (replace-match digest-separator-replacement))

(define (digest-message-end start end)
  (and (re-search-forward digest-message-separator-regexp start end false)
       (replace-match digest-separator-replacement)))

;;;; Message memoization

(define (memoize-buffer buffer)
  (let ((end (buffer-end buffer)))
    (let ((m
	   (re-match-forward babyl-header-start-regexp
			     (buffer-start buffer)
			     end
			     false)))
      (if m
	  (let ((m (re-search-forward babyl-header-end-regexp m end false)))
	    (if m
		(memoize-messages buffer m end)))))))

(define (memoize-messages buffer start end)
  (let ((memo (buffer-msg-memo buffer)))
    (with-values
	(lambda ()
	  (memoize-messages* start
			     end
			     (and (msg-memo? memo) (msg-memo/last memo))))
      (lambda (start tail)
	(if (not (msg-memo? memo))
	    (set-buffer-msg-memo! buffer (or tail true)))
	(let ((old-end (buffer-last-msg-end buffer)))
	  (if old-end
	      (mark-temporary! old-end)))
	(set-buffer-last-msg-end! buffer start)))))

(define (memoize-messages-insert buffer start end memo)
  (let ((next (msg-memo/next memo)))
    (if (not next)
	(memoize-messages buffer start end)
	(with-values (lambda () (memoize-messages* start end memo))
	  (lambda (start tail)
	    (mark-temporary! start)
	    (set-msg-memo/next! tail next)
	    (set-msg-memo/previous! next tail)
	    (let loop ((memo next) (n (+ (msg-memo/number tail) 1)))
	      (set-msg-memo/number! memo n)
	      (if (msg-memo/next memo)
		  (loop (msg-memo/next memo) (+ n 1)))))))))

(define (memoize-messages* start end tail)
  (message "Counting messages...")
  (let loop ((start (mark-left-inserting-copy start)) (tail tail) (n 1))
    (let ((mend (search-forward babyl-message-end-regexp start end false)))
      (if mend
	  (let ((mend (mark-left-inserting-copy mend)))
	    (canonicalize-message-attributes start)
	    (let ((memo
		   (make-msg-memo tail
				  false
				  start
				  (if tail (+ (msg-memo/number tail) 1) 1)
				  (parse-attributes start))))
	      (if tail
		  (set-msg-memo/next! tail memo))
	      (if (zero? (remainder n 20))
		  (message "Counting messages..." n))
	      (loop mend memo (+ n 1))))
	  (begin
	    (message "Counting messages...done")
	    (values start tail))))))

(define-integrable (buffer-msg-memo buffer)
  (buffer-get buffer 'RMAIL-MSG-MEMO))

(define-integrable (set-buffer-msg-memo! buffer memo)
  (buffer-put! buffer 'RMAIL-MSG-MEMO memo))

(define-integrable (buffer-last-msg-end buffer)
  (buffer-get buffer 'RMAIL-LAST-MSG-END))

(define-integrable (set-buffer-last-msg-end! buffer memo)
  (buffer-put! buffer 'RMAIL-LAST-MSG-END memo))

(define-structure (msg-memo (conc-name msg-memo/))
  previous
  next
  (start false read-only true)
  number
  attributes)

(define (msg-memo/end memo)
  (let ((next (msg-memo/next memo)))
    (if next
	(msg-memo/start next)
	(buffer-last-msg-end (mark-buffer (msg-memo/start memo))))))

(define (msg-memo/start-body memo)
  (let ((start (msg-memo/start memo)))
    (or (re-search-forward babyl-eooh-regexp start (msg-memo/end memo) false)
	start)))

(define (msg-memo/end-body memo)
  (mark-1+ (msg-memo/end memo)))

(define (msg-memo/first memo)
  (let loop ((memo memo))
    (let ((previous (msg-memo/previous memo)))
      (if previous
	  (loop previous)
	  memo))))

(define (msg-memo/last memo)
  (let loop ((memo memo))
    (let ((next (msg-memo/next memo)))
      (if next
	  (loop next)
	  memo))))

(define (msg-memo/nth memo n)
  (if (= n (msg-memo/number memo))
      memo
      (let ((msg-memo/next
	     (if (< n (msg-memo/number memo))
		 msg-memo/previous
		 msg-memo/next)))
	(let loop ((memo memo))
	  (let ((next (msg-memo/next memo)))
	    (cond ((not next) memo)
		  ((= n (msg-memo/number next)) next)
		  (else (loop next))))))))

(define-integrable (msg-memo/deleted? memo)
  (memq 'DELETED (msg-memo/attributes memo)))

(define (msg-memo/next-undeleted memo)
  (let ((next (msg-memo/next memo)))
    (and next
	 (if (msg-memo/deleted? next)
	     (msg-memo/next-undeleted next)
	     next))))

(define (msg-memo/previous-undeleted memo)
  (let ((previous (msg-memo/previous memo)))
    (and previous
	 (if (msg-memo/deleted? previous)
	     (msg-memo/previous-undeleted previous)
	     previous))))

(define (msg-memo/next-deleted memo)
  (let ((next (msg-memo/next memo)))
    (and next
	 (if (msg-memo/deleted? next)
	     next
	     (msg-memo/next-deleted next)))))

(define (msg-memo/previous-deleted memo)
  (let ((previous (msg-memo/previous memo)))
    (and previous
	 (if (msg-memo/deleted? previous)
	     previous
	     (msg-memo/previous-deleted previous)))))

;;;; Attributes and Labels

(define (canonicalize-message-attributes mstart)
  (let ((start (attributes-start-mark mstart)))
    (let ((end (line-end start 0)))
      (let loop ((m start) (in-labels? false))
	(cond ((re-match-forward " [^ ,]+," m end false)
	       (loop (re-match-end 0) in-labels?))
	      ((and (not in-labels?) (match-forward "," m end false))
	       => (lambda (m) (loop m true)))
	      ((and in-labels? (mark= m end))
	       unspecific)
	      ((re-match-forward " *\\([^ ,]+\\)," m end false)
	       (loop (replace-match " \\1,") in-labels?))
	      ((and (not in-labels?) (re-match-forward " +," m end false))
	       (loop (replace-match ",") true))
	      ((and in-labels? (re-match-forward " +$" m end false))
	       (delete-match))
	      (else
	       (editor-error "Malformed message attributes: "
			     (extract-string start end))))))))

(define (set-attribute! memo attribute)
  (if (not (memq attribute (msg-memo/attributes memo)))
      (begin
	(set-msg-memo/attributes! memo
				  (cons attribute
					(msg-memo/attributes memo)))
	(let ((start (msg-memo/start memo)))
	  (with-group-open (mark-group start)
	    (lambda ()
	      (insert-string (attribute->string attribute)
			     (attributes-end-mark start))
	      (update-mode-line! (mark-buffer start))))))))

(define (clear-attribute! memo attribute)
  (if (memq attribute (msg-memo/attributes memo))
      (begin
	(set-msg-memo/attributes! memo
				  (delq! attribute
					 (msg-memo/attributes memo)))
	(let ((start (msg-memo/start memo)))
	  (with-group-open (mark-group start)
	    (lambda ()
	      (if (search-forward (attribute->string attribute)
				  (attributes-start-mark start)
				  (attributes-end-mark start)
				  true)
		  (delete-match))
	      (update-mode-line! (mark-buffer start))))))))

(define (attribute->string attribute)
  (string-append " " (string-downcase (symbol->string attribute)) ","))

(define (attributes-start-mark mstart)
  (let ((m
	 (re-match-forward babyl-message-start-regexp
			   mstart
			   (group-end mstart)
			   false)))
    (if (not m)
	(editor-error "Mark not at message start: " mstart))
    m))

(define (attributes-end-mark mstart)
  (mark-1+ (labels-start-mark mstart)))

(define (labels-start-mark mstart)
  (let ((m
	 (let ((lstart (line-start mstart 1 'ERROR)))
	   (search-forward ",," lstart (line-end lstart 0) false))))
    (if (not m)
	(editor-error "Can't find attributes/labels separator"))
    m))

(define (labels-end-mark mstart)
  (line-end mstart 1 'ERROR))

(define (parse-attributes mstart)
  (map intern
       (parse-label-list (attributes-start-mark mstart)
			 (attributes-end-mark mstart))))

(define (parse-labels mstart)
  (parse-label-list (labels-start-mark mstart)
		    (labels-end-mark mstart)))

(define (parse-label-list start end)
  (let loop ((m start))
    (if (mark< m end)
	(let ((aend (char-search-forward #\, m end false)))
	  (cons (string-downcase (extract-string (mark1+ m) (mark-1+ aend)))
		(loop aend)))
	'())))

(define-command rmail-toggle-header
  "Show original message header if pruned header currently shown, or vice versa."
  ()
  (lambda ()
    (let ((buffer (current-buffer)))
      (let ((memo (current-msg-memo)))
	(with-buffer-open buffer
	  (lambda ()
	    (let ((start (msg-memo/start memo))
		  (end (msg-memo/end memo)))
	      (cond ((match-forward "\f\n0" start end false)
		     (reformat-message start end))
		    ((match-forward "\f\n1" start end false)
		     (unformat-message start end)))))))
      (set-current-point! (buffer-start buffer)))))

(define (reformat-message start end)
  (let ((m (mark+ start 2)))
    (delete-right-char m)
    (insert-char #\1 m))
  (if (not (re-search-forward babyl-eooh-regexp start end false))
      (editor-error))
  (let ((eooh (re-match-start 0)))
    (let ((hstart (mark-right-inserting-copy (line-start eooh 1 'ERROR))))
      (let ((hend
	     (let ((m (search-forward "\n\n" hstart end false)))
	       (if m
		   (mark-left-inserting-copy m)
		   (let ((m (mark-left-inserting-copy end)))
		     (if (char-match-backward #\newline m)
			 (insert-newline m)
			 (insert-newlines 2 m))
		     m)))))
	(insert-string (extract-string hstart hend) eooh)
	(let ((regexp (ref-variable rmail-ignored-headers)))
	  (if regexp
	      (do ()
		  ((not (re-search-forward regexp hstart hend true)))
		(let ((m (line-start (re-match-start 0) 0)))
		  (delete-string
		   m
		   (mark-1+ (re-search-forward "\n[^ \t]" m hend false)))))))
	(let ((filter (ref-variable rmail-message-filter)))
	  (if filter
	      (filter hstart hend)))
	(mark-temporary! hend)
	(mark-temporary! hstart)))))

(define (unformat-message start end)
  (let ((m (mark+ start 2)))
    (delete-right-char m)
    (insert-char #\0 m))
  (let ((start
	 (let ((start (line-start start 2 'ERROR)))
	   (if (match-forward "Summary-line:" start end true)
	       (line-start start 1 'ERROR)
	       start))))
    (if (not (re-search-forward babyl-eooh-regexp start end false))
	(editor-error))
    (let ((header (extract-and-delete-string start (re-match-start 0))))
      (let ((hstart (line-start start 1)))
	(delete-string hstart (header-end hstart end))
	(insert-string header hstart)))))

;;;; Mail conversion

(define (convert-region-to-babyl-format start end)
  (define (loop point count)
    (text-clip point end)
    (cond ((mark= point end)
	   count)
	  ((re-match-forward babyl-header-start-regexp point end false)
	   (delete-string
	    point
	    (or (search-forward babyl-header-end-regexp point end false) end))
	   (loop point count))
	  ((re-match-forward babyl-message-start-regexp point end false)
	   (let ((m
		  (or (search-forward babyl-message-end-regexp point end false)
		      (missing-end end "Babyl"))))
	     (delete-string m (skip-chars-forward " \t\n" m end))
	     (loop m (+ count 1))))
	  ((re-match-forward umail-message-start-regexp point end false)
	   (let ((point (mark-left-inserting-copy point)))
	     (insert-string babyl-initial-message-start point)
	     (nuke-pinhead-header point end)
	     (mark-temporary! point)
	     (process-message-body
	      point
	      count
	      (if (re-search-forward umail-message-end-regexp point end false)
		  (re-match-start 0)
		  end))))
	  ((re-match-forward mmdf-message-start-regexp point end true)
	   (let ((start (replace-match babyl-initial-message-start)))
	     (process-message-body
	      start
	      count
	      (if (re-search-forward mmdf-message-end-regexp start end true)
		  (mark-1+ (replace-match "\037"))
		  (missing-end end "MMDF")))))
	  (else
	   (editor-error "error converting to Babyl format")
	   true)))

  (define (process-message-body point count mend)
    (let ((mend (mark-left-inserting-copy mend)))
      (do ((m point (replace-match "\n^_")))
	  ((not (search-forward "\n\037" m mend false))))
      (let ((m (match-forward "\037" mend end false)))
	(if m
	    (move-mark-to! mend m)
	    (insert-string "\037" mend)))
      (mark-temporary! mend)
      (loop mend (+ count 1))))

  (define (missing-end end type)
    (message "Invalid " type " format in inbox!")
    (sit-for 1)
    end)

  (with-text-clipped start end (lambda () (loop start 0))))

(define (convert-buffer-to-babyl-format buffer)
  (with-buffer-open buffer
    (lambda ()
      (let ((start (buffer-start buffer))
	    (end (buffer-end buffer)))
	(if (not (re-match-forward babyl-header-start-regexp start end false))
	    (insert-string babyl-initial-header start))
	(search-backward "\n\037" end start false)
	(let ((start (re-match-end 0)))
	  (let ((m (skip-chars-forward "\n" start end)))
	    (cond ((and (mark= m end)
			(mark< start m))
		   (delete-string start m))
		  ((re-match-forward umail-message-start-regexp m end false)
		   (delete-string start m)
		   (message "Converting to Babyl format...")
		   (convert-region-to-babyl-format start end)
		   (message "Converting to Babyl format...done")))))))))

(define (nuke-pinhead-header start end)
  (let ((hend
	 (or (search-forward "\n\n" start end false)
	     (begin
	       (insert-string "\n\n" end)
	       end))))
    (let ((has-from (search-forward "\nFrom:" start hend true))
	  (has-date (search-forward "\nDate:" start hend true)))
      (if (and has-from has-date)
	  (delete-string start (line-start start 1))
	  (begin
	    (re-match-forward umail-message-start-regexp start hend false)
	    (replace-match
	     (let ((from "From: \\1")
		   (date
		    (if (mark< (re-match-start 7) (re-match-end 7))
			"Date: \\3, \\5 \\4 \\8 \\6\\7"
			"Date: \\3, \\5 \\4 \\8 \\6 EST")))
	       (cond (has-from date)
		     (has-date from)
		     (else (string-append date "\n" from))))))))))

;;;; Utilities

(define (without-clipping buffer thunk)
  (let ((group (buffer-group buffer)))
    (with-group-text-clipped! group 0 (group-length group) thunk)))

(define-integrable (with-buffer-open buffer thunk)
  (with-group-open (buffer-group buffer) thunk))

(define-integrable (with-buffer-undo-disabled buffer thunk)
  (with-group-undo-disabled (buffer-group buffer) thunk))

(define (with-group-open group thunk)
  (let ((outside-ro)
	(inside-ro false)
	(outside-start)
	(outside-end)
	(inside-start (mark-permanent! (group-absolute-start group)))
	(inside-end (mark-permanent! (group-absolute-end group))))
    (dynamic-wind (lambda ()
		    (set! outside-ro (group-read-only? group))
		    (set! outside-start (group-start-mark group))
		    (set! outside-end (group-end-mark group))
		    (vector-set! group group-index:read-only? inside-ro)
		    (vector-set! group group-index:start-mark inside-start)
		    (vector-set! group group-index:end-mark inside-end))
		  thunk
		  (lambda ()
		    (set! inside-ro (group-read-only? group))
		    (set! inside-start (group-start-mark group))
		    (set! inside-end (group-end-mark group))
		    (vector-set! group group-index:read-only? outside-ro)
		    (vector-set! group group-index:start-mark outside-start)
		    (vector-set! group group-index:end-mark outside-end)))))

;;;; Constants

(define umail-message-start-regexp
  "^From \\([^ \n]*\\(\\|\".*\"[^ \n]*\\)\\)  ?\\([^ \n]*\\) \\([^ \n]*\\) *\\([0-9]*\\) \\([0-9:]*\\)\\( ?[A-Z]?[A-Z][A-Z]T\\| ?[-+]?[0-9][0-9][0-9][0-9]\\|\\) 19\\([0-9]*\\) *\\(remote from .*\\)?$")

(define umail-message-end-regexp
  false)

(define mmdf-message-start-regexp
  "^\001\001\001\001\n")

(define mmdf-message-end-regexp
  "^\001\001\001\001\n")

(define babyl-header-start-regexp
  "^BABYL OPTIONS:")

(define babyl-header-end-regexp
  "\n\037")

(define babyl-initial-header
  "BABYL OPTIONS:
Version: 5
Labels:
Note:   This is the header of an rmail file.
Note:   If you are seeing it in rmail,
Note:    it means the file has no messages in it.\n\037")

(define babyl-message-start-regexp
  "\f\n[01],")

(define babyl-message-end-regexp
  "\n\037")

(define babyl-eooh-string
  "*** EOOH ***\n")

(define babyl-eooh-regexp
  (string-append "^" (re-quote-string babyl-eooh-string)))

(define babyl-initial-message-start
  (string-append "\f\n0, unseen,,\n" babyl-eooh-string))

(define-integrable digest-end-regexp
  "^End of.*Digest.*\n\\*\\*\\*\\*\\*\\*\\*\\*\\**\\(\n------*\\)*")

(define-integrable digest-summary-separator-regexp
  "\n*\n--------------------------------------------------------*\n*")

(define-integrable digest-message-separator-regexp
  "\n*\n\n----------------------------*\n*")

(define digest-separator-replacement
  (string-append "\n\037" babyl-initial-message-start))
