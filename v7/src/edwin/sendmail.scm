;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/sendmail.scm,v 1.13 1992/01/24 00:34:28 cph Exp $
;;;
;;;	Copyright (c) 1991-92 Massachusetts Institute of Technology
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

;;;; Mail Sending

(declare (usual-integrations))

(define-variable mail-reply-buffer
  ""
  false
  (lambda (object) (or (false? object) (buffer? object))))

(define-variable mail-default-reply-to
  "Address to insert as default Reply-to field of outgoing messages."
  false
  string-or-false?)

(define-variable mail-self-blind
  "True means insert BCC to self in messages to be sent.
This is done when the message is initialized,
so you can remove or alter the BCC field to override the default."
  false
  boolean?)

(define-variable mail-archive-file-name
  "Name of file to write all outgoing messages in, or false for none."
  false
  string-or-false?)

(define-variable mail-header-function
  "A function of one argument, POINT (the current point), which
inserts additional header lines into a mail message.  By default,
this function inserts the header line \"X-Scheme-Mailer: Edwin\"
followed by the version number of Edwin.  The function is called
immediately after the Reply-to: header is inserted, if any.  If this
variable is false, it is ignored."  
  (lambda (point)
    (insert-string "X-Scheme-Mailer:" point)
    (for-each-system!
     (lambda (system)
       (if (string=? "Edwin"
		     (system/name system))
	   (begin
	     (insert-string " " point)
	     (insert-string 
	      (system/identification-string system)
	      point)))))
    (insert-newline point)))

(define-variable mail-header-separator
  "Line used to separate headers from text in messages being composed."
  "--text follows this line--"
  string?)

(define-variable mail-interactive
  "True means when sending a message wait for and display errors.
False means let mailer mail back a message to report errors."
  false
  boolean?)

(define-variable sendmail-program
  "Filename of sendmail program."
  (if (file-exists? "/usr/lib/sendmail")
      "/usr/lib/sendmail"
      "fakemail")
  string?)

(define-variable mail-yank-ignored-headers
  "Delete these headers from old message when it's inserted in a reply."
  "^via:\\|^mail-from:\\|^origin:\\|^status:\\|^remailed\\|^received:\\|^[a-z-]*message-id:\\|^summary-line:\\|^to:\\|^cc:\\|^subject:\\|^in-reply-to:\\|^return-path:")

(define-variable send-mail-procedure
  "Procedure to call to send the current buffer as mail.
The headers are delimited by a string found in mail-header-separator."
  (lambda () (sendmail-send-it)))

(define-variable mail-setup-hook
  "An event distributor invoked immediately after a mail buffer initialized."
  (make-event-distributor))

(define-command mail
  "Edit a message to be sent.  Argument means resume editing (don't erase).
Returns with message buffer selected.
While editing message, type C-c C-c to send the message and exit.

Separate names of recipients with commas.

Various special commands starting with C-c are available in sendmail mode
to move to message header fields.

If mail-self-blind is non-false, a BCC to yourself is inserted
when the message is initialized.

If mail-default-reply-to is non-false, it should be an address (a string);
a Reply-to: field with that address is inserted.

If mail-archive-file-name is non-false, an FCC field with that file name
is inserted."
  "P"
  (lambda (no-erase?)
    (make-mail-buffer no-erase? select-buffer false false false false false)))

(define-command mail-other-window
  "Like `mail' command, but display mail buffer in another window."
  "P"
  (lambda (no-erase?)
    (make-mail-buffer no-erase? select-buffer-other-window
		      false false false false false)))

(define (make-mail-buffer no-erase? select-buffer
			  to subject in-reply-to cc reply-buffer)
  (let ((buffer (find-or-create-buffer "*mail*")))
    (select-buffer buffer)
    (if (and (not no-erase?)
	     (or (not (buffer-modified? buffer))
		 (prompt-for-confirmation?
		  "Unsent message being composed; erase it")))
	(begin
	  (set-buffer-default-directory! buffer (->pathname "~/"))
	  (setup-buffer-auto-save! buffer)
	  (region-delete! (buffer-unclipped-region buffer))
	  (mail-setup buffer to subject in-reply-to cc reply-buffer)))))

(define (mail-setup buffer to subject in-reply-to cc reply-buffer)
  (guarantee-mail-aliases)
  (set-buffer-major-mode! buffer (ref-mode-object mail))
  (local-set-variable! mail-reply-buffer reply-buffer)
  (let ((point (mark-left-inserting-copy (buffer-start buffer)))
	(fill
	 (lambda (start end)
	   (fill-region-as-paragraph start end
				     "\t" (ref-variable fill-column)
				     false))))
    (insert-string "To: " point)
    (if to
	(begin
	  (insert-string to point)
	  (fill (buffer-start buffer) point)))
    (insert-newline point)
    (if cc
	(let ((start (mark-right-inserting point)))
	  (insert-string "CC: " point)
	  (insert-string cc point)
	  (fill start point)
	  (insert-newline point)))
    (if in-reply-to
	(begin
	  (insert-string "In-reply-to: " point)
	  (insert-string in-reply-to point)
	  (insert-newline point)))
    (insert-string "Subject: " point)
    (if subject
	(insert-string subject point))
    (insert-newline point)
    (let ((mail-default-reply-to (ref-variable mail-default-reply-to)))
      (if mail-default-reply-to
	  (begin
	    (insert-string "Reply-to: " point)
	    (insert-string mail-default-reply-to point)
	    (insert-newline point))))
    (let ((mail-header-function (ref-variable mail-header-function)))
      (if mail-header-function
	  (mail-header-function point)))
    (if (ref-variable mail-self-blind)
	(begin
	  (insert-string "BCC: " point)
	  (insert-string (unix/current-user-name) point)
	  (insert-newline point)))
    (let ((mail-archive-file-name (ref-variable mail-archive-file-name)))
      (if mail-archive-file-name
	  (begin
	    (insert-string "FCC: " point)
	    (insert-string mail-archive-file-name point)
	    (insert-newline point))))
    (insert-string (ref-variable mail-header-separator) point)
    (insert-newline point)
    (mark-temporary! point))
  (set-buffer-point! buffer
		     (if to
			 (buffer-end buffer)
			 (mail-position-on-field buffer "To")))
  (if (not (or to subject in-reply-to))
      (buffer-not-modified! buffer))
  (event-distributor/invoke! (ref-variable mail-setup-hook)))

(define-major-mode mail text "Mail"
  "Major mode for editing mail to be sent.
Separate names of recipients (in To: and CC: fields) with commas.
Like Text Mode but with these additional commands:
C-c C-s  mail-send (send the message)    C-c C-c  mail-send-and-exit
C-c C-f  move to a header field (and create it if there isn't):
	 C-c C-f C-t  move to To:	C-c C-f C-s  move to Subj:
	 C-c C-f C-b  move to BCC:	C-c C-f C-c  move to CC:
C-c C-w  mail-signature (insert ~/.signature at end).
C-c C-y  mail-yank-original (insert current message, in Rmail).
C-c C-q  mail-fill-yanked-message (fill what was yanked)."
  (local-set-variable!
   paragraph-start
   (string-append "^"
		  (re-quote-string (ref-variable mail-header-separator))
		  "$\\|^[ \t]*[-_][-_][-_]+$\\|"
		  (ref-variable paragraph-start)))
  (local-set-variable!
   paragraph-separate
   (string-append "^"
		  (re-quote-string (ref-variable mail-header-separator))
		  "$\\|^[ \t]*[-_][-_][-_]+$\\|"
		  (ref-variable paragraph-separate)))
  (event-distributor/invoke! (ref-variable mail-mode-hook)))

(define-variable mail-mode-hook
  "An event distributor that is invoked when entering Mail mode."
  (make-event-distributor))

(define-key 'mail '(#\C-c #\?) 'describe-mode)
(define-key 'mail '(#\C-c #\C-f #\C-t) 'mail-to)
(define-key 'mail '(#\C-c #\C-f #\C-b) 'mail-bcc)
(define-key 'mail '(#\C-c #\C-f #\C-c) 'mail-cc)
(define-key 'mail '(#\C-c #\C-f #\C-s) 'mail-subject)
(define-key 'mail '(#\C-c #\C-w) 'mail-signature)
(define-key 'mail '(#\C-c #\C-y) 'mail-yank-original)
(define-key 'mail '(#\C-c #\C-q) 'mail-fill-yanked-message)
(define-key 'mail '(#\C-c #\C-c) 'mail-send-and-exit)
(define-key 'mail '(#\C-c #\C-s) 'mail-send)

(define-command mail-send-and-exit
  "Send message like mail-send, then, if no errors, exit from mail buffer.
Prefix arg means don't delete this window."
  "P"
  (lambda (argument)
    ((ref-command mail-send))
    (bury-buffer (current-buffer))
    (if (and (not argument)
	     (not (window-has-no-neighbors? (current-window)))
	     (eq? (ref-mode-object rmail)
		  (buffer-major-mode (window-buffer (other-window)))))
	(window-delete! (current-window))
	(select-buffer (previous-buffer)))))

(define-command mail-send
  "Send the message in the current buffer.
If  mail-interactive  is non-false, wait for success indication
or error messages, and inform user.
Otherwise any failure is reported in a message back to
the user from the mailer."
  ()
  (lambda ()
    (message "Sending...")
    ((ref-variable send-mail-procedure))
    (buffer-not-modified! (current-buffer))
    (delete-auto-save-file! (current-buffer))
    (message "Sending...done")))

(define-command mail-to
  "Move point to end of To field."
  ()
  (lambda ()
    (set-current-point! (mail-position-on-field (current-buffer) "To"))))

(define-command mail-subject
  "Move point to end of Subject field."
  ()
  (lambda ()
    (set-current-point! (mail-position-on-field (current-buffer) "Subject"))))

(define-command mail-cc
  "Move point to end of CC field."
  ()
  (lambda ()
    (set-current-point! (mail-position-on-cc-field (current-buffer) "CC"))))

(define-command mail-bcc
  "Move point to end of BCC field."
  ()
  (lambda ()
    (set-current-point! (mail-position-on-cc-field (current-buffer) "BCC"))))

(define (mail-position-on-field buffer field)
  (let ((start (buffer-start buffer)))
    (mail-field-end! start (mail-header-end start (buffer-end buffer)) field)))

(define (mail-position-on-cc-field buffer field)
  (let ((start (buffer-start buffer)))
    (let ((end (mail-header-end start (buffer-end buffer))))
      (or (mail-field-end start end field)
	  (mail-insert-field (mail-field-end! start end "To") field)))))

(define (mail-header-end start end)
  (mail-match-header-separator start end)
  (skip-chars-backward "\n" (re-match-start 0) start))

(define (mail-match-header-separator start end)
  (if (not (re-search-forward
	    (string-append
	     "^" (re-quote-string (ref-variable mail-header-separator)) "$")
	    start end false))
      (editor-error "Can't find mail-header-separator")))

(define (mail-field-end! start end field)
  (or (mail-field-end start end field)
      (mail-insert-field end field)))

(define (mail-field-end start end field)
  (and (re-search-forward (string-append "^" field ":[ \t]*") start end true)
       (let ((field-start (re-match-end 0)))
	 (if (re-search-forward "^[^ \t]" field-start end false)
	     (skip-chars-backward "\n" (re-match-start 0) field-start)
	     end))))

(define (mail-insert-field end field)
  (let ((end (mark-left-inserting-copy end)))
    (guarantee-newline end)
    (insert-string field end)
    (insert-string ": " end)
    (if (line-end? end)
	(begin
	  (mark-temporary! end)
	  end)
	(begin
	  (insert-newline end)
	  (mark-temporary! end)
	  (mark-1+ end)))))

(define-command mail-signature
  "Sign letter with contents of ~/.signature file."
  ()
  (lambda ()
    (insert-file (buffer-end (current-buffer)) "~/.signature")))

(define-command mail-yank-original
  "Insert the message being replied to, if any (in rmail).
Puts point after the text and mark before.
Indents each nonblank line ARG spaces (default 3).
Just \\[universal-argument] as argument means don't indent
and don't delete any header fields."
  "P"
  (lambda (argument)
    (let ((mail-reply-buffer (ref-variable mail-reply-buffer)))
      (if mail-reply-buffer
	  (begin
	    (for-each (lambda (window)
			(if (not (window-has-no-neighbors? window))
			    (window-delete! window)))
		      (buffer-windows mail-reply-buffer))
	    (let ((end (mark-left-inserting-copy (current-point))))
	      (let ((start (mark-right-inserting-copy end)))
		(insert-region (buffer-start mail-reply-buffer)
			       (buffer-end mail-reply-buffer)
			       start)
		(if (not (line-end? end))
		    (insert-newline end))
		(if (not (command-argument-multiplier-only? argument))
		    (begin
		      (mail-yank-clear-headers start end)
		      (indent-rigidly start end
				      (or (command-argument-value argument)
					  3))))
		(mark-temporary! start)
		(mark-temporary! end)
		(push-current-mark! start)
		(set-current-point! end))))))))

(define (mail-yank-clear-headers start end)
  (let ((start (mark-left-inserting-copy start))
	(end
	 (mark-left-inserting-copy
	  (if (re-search-forward "\n\n" start end false)
	      (mark1+ (re-match-start 0))
	      end)))
	(mail-yank-ignored-headers (ref-variable mail-yank-ignored-headers)))
    (with-text-clipped start end
      (lambda ()
	(do ()
	    ((not
	      (re-search-forward mail-yank-ignored-headers start end true)))
	  (move-mark-to! start (re-match-start 0))
	  (delete-string
	   start
	   (if (re-search-forward "^[^ \t]" (line-end start 0) end false)
	       (re-match-start 0)
	       end)))))
    (mark-temporary! start)
    (mark-temporary! end)))

(define-command mail-fill-yanked-message
  "Fill the paragraphs of a message yanked into this one.
Numeric argument means justify as well."
  "P"
  (lambda (justify?)
    (let ((buffer (current-buffer)))
      (mail-match-header-separator (buffer-start buffer) (buffer-end buffer))
      (fill-individual-paragraphs (re-match-end 0)
				  (buffer-end buffer)
				  (ref-variable fill-column)
				  justify?
				  true))))

(define (sendmail-send-it)
  (let ((error-buffer
	 (and (ref-variable mail-interactive)
	      (temporary-buffer " sendmail errors")))
	(temp-buffer (temporary-buffer " sendmail temp"))
	(mail-buffer (current-buffer))
	(user-name (unix/current-user-name)))
    (let ((start (buffer-start temp-buffer))
	  (end (buffer-end temp-buffer)))
      (insert-region (buffer-start mail-buffer)
		     (buffer-end mail-buffer)
		     start)
      (if (not (line-start? end))
	  (insert-char #\newline end))
      (mail-match-header-separator start end)
      (let ((header-end (mark-left-inserting-copy (delete-match))))
	;; Delete any blank lines in the header.
	(do ((start start (replace-match "\n")))
	    ((not (re-search-forward "\n\n+" start header-end false))))
	(expand-mail-aliases start header-end)
	(if (re-search-forward "^FCC:" start header-end true)
	    (mail-do-fcc temp-buffer header-end))
	;; If there is a From and no Sender, put in a Sender.
	(if (and (re-search-forward "^From:" start header-end true)
		 (not
		  (re-search-forward "^Sender:" start header-end true)))
	    (begin
	      (insert-string "\nSender: " header-end)
	      (insert-string user-name header-end)))
	;; Don't send out a blank subject line.
	(if (re-search-forward "^Subject:[ \t]*\n" start header-end true)
	    (delete-match)))
      (let ((program (ref-variable sendmail-program)))
	(if error-buffer
	    (begin
	      (run-synchronous-process (make-region start end)
				       (buffer-end error-buffer)
				       false
				       false
				       program
				       "-oi" "-t"
				       ;; Always specify who from,
				       ;; since some systems have
				       ;; broken sendmails.
				       "-f" user-name)
	      (let ((end (buffer-end error-buffer)))
		(do ((start (buffer-start error-buffer) (replace-match "; ")))
		    ((not (re-search-forward "\n+ *" start end false))))))
	    ;; If we aren't going to look at the errors, run the
	    ;; program in the background so control returns to the
	    ;; user as soon as possible.
	    (let ((process
		   (start-pipe-subprocess
		    program
		    (vector (os/filename-non-directory program)
			    "-oi" "-t"
			    (string-append "-f" user-name)
			    ;; These mean "report errors by mail" and
			    ;; "deliver in background".
			    "-oem" "-odb")
		    false)))
	      (channel-write-string-block (subprocess-output-channel process)
					  (extract-string start end))
	      (subprocess-delete process)))))
    (kill-buffer temp-buffer)
    (if error-buffer
	(let ((errors
	       (extract-string (buffer-start error-buffer)
			       (buffer-end error-buffer))))
	  (kill-buffer error-buffer)
	  (if (not (string-null? errors))
	      (editor-error "Sending...failed to " errors))))))

(define (mail-do-fcc mail-buffer header-end)
  (let ((pathnames (digest-fcc-headers (buffer-start mail-buffer) header-end))
	(temp-buffer (temporary-buffer " rmail output")))
    (let ((start (buffer-start temp-buffer))
	  (end (buffer-end temp-buffer)))
      (insert-newline end)
      (insert-string "From " end)
      (insert-string (unix/current-user-name) end)
      (insert-string " " end)
      (insert-string (unix/file-time->string (unix/current-file-time)) end)
      (insert-newline end)
      (insert-region (buffer-start mail-buffer)
		     (buffer-end mail-buffer)
		     end)
      (insert-newline end)
      ;; ``Quote'' "^From " as ">From "
      ;;  (note that this isn't really quoting, as there is no requirement
      ;;   that "^[>]+From " be quoted in the same transparent way.)
      (let ((m (mark-right-inserting-copy (mark+ start 2))))
	(do ()
	    ((not (re-search-forward "^From " m end false)))
	  (move-mark-to! m (re-match-end 0))
	  (insert-string ">" (re-match-start 0)))
	(mark-temporary! m))
      (for-each (lambda (pathname)
		  (let ((buffer (pathname->buffer pathname)))
		    (if buffer
			(insert-region start end (buffer-end buffer))
			(append-to-file (make-region start end)
					pathname
					true))))
		pathnames)
      (kill-buffer temp-buffer))))

(define (digest-fcc-headers start header-end)
  (let ((m (mark-right-inserting-copy start)))
    (let loop ((pathnames '()))
      (if (re-search-forward "^FCC:[ \t]*\\([^ \t\n]+\\)" m header-end true)
	  (let ((filename
		 (extract-string (re-match-start 1) (re-match-end 1))))
	    (move-mark-to! m (line-start (re-match-start 0) 0))
	    (delete-string m (line-start m 1))
	    (loop (cons (->pathname filename) pathnames)))
	  (begin
	    (mark-temporary! m)
	    pathnames)))))