;;; -*-Scheme-*-
;;;
;;; $Id: sendmail.scm,v 1.49 2000/06/08 18:52:59 cph Exp $
;;;
;;; Copyright (c) 1991-2000 Massachusetts Institute of Technology
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

;;;; Mail Sending

(declare (usual-integrations))

(define-variable user-mail-address
  "Full mailing address of this user.
This is initialized based on `mail-host-address',
after your init file is read, in case it sets `mail-host-address'."
  #f
  string-or-false?)

(define-variable mail-host-address
  "Name of this machine, for purposes of naming users."
  #f
  string-or-false?)

(define-variable mail-full-name
  "Your full name.
Appears in the From: field of mail and news messages, following the address.
If set to the null string, From: field contains only the email address."
  ""
  string?)

(define-variable mail-from-style
  "Specifies how \"From:\" fields look.
One of the following values:
'PARENS	king@grassland.com (Elvis Parsley)
'ANGLES	Elvis Parsley <king@grassland.com>
#F	king@grassland.com"
  'ANGLES
  (lambda (object) (memq object '(PARENS ANGLES #F))))

(define-variable mail-organization
  "The name of your organization.
Appears in the Organization: field of mail and news messages.
If set to the null string, no Organization: field is generated."
  ""
  string?)

(define-variable mail-identify-reader
  "Switch controlling generation of X-Mailer headers in messages."
  #t
  boolean?)

(define-variable mail-default-reply-to
  "Address to insert as default Reply-to field of outgoing messages."
  #f
  (lambda (object)
    (or (not object)
	(string? object)
	(and (procedure? object)
	     (procedure-arity-valid? object 0)))))

(define-variable mail-self-blind
  "True means insert BCC to self in messages to be sent.
This is done when the message is initialized,
so you can remove or alter the BCC field to override the default."
  #f
  boolean?)

(define-variable mail-archive-file-name
  "Name of file to write all outgoing messages in, or #f for none."
  #f
  string-or-false?)

(define-variable mail-relay-host
  "Name of host to which all outgoing mail should be sent.
Can be a host name (a string) or #F.
If #F, mail is passed to sendmail for handling.
Otherwise, mail is sent directly to the named host using SMTP."
  #f
  string-or-false?)

(define-variable smtp-trace
  "If true, direct SMTP transmissions are traced in a buffer."
  #f
  boolean?)

(define-variable smtp-require-valid-recipients
  "If true, all SMTP recipients must be valid before a message is sent.
Otherwise, only one valid recipient is required."
  #t
  boolean?)

(define-variable mail-yank-ignored-headers
  "Delete these headers from old message when it's inserted in a reply."
  (reduce (lambda (x y) (string-append x "\\|" y))
	  ""
	  '("^via:"
	    "^mail-from:"
	    "^origin:"
	    "^status:"
	    "^remailed"
	    "^received:"
	    "^[a-z-]*message-id:"
	    "^summary-line:"
	    "^to:"
	    "^cc:"
	    "^subject:"
	    "^in-reply-to:"
	    "^return-path:"))
  string?)

(define-variable mail-interactive
  "True means when sending a message wait for and display errors.
#F means let mailer mail back a message to report errors."
  #f
  boolean?)

(define-variable mail-header-separator
  "Line used to separate headers from text in messages being composed."
  "--text follows this line--"
  string?)

(define-variable mail-header-function
  "A function of one argument, POINT (the current point), which inserts
additional header lines into a mail message.  The function is called
after all other headers are inserted.  If this variable is #f, it
is ignored."
  #f
  (lambda (object)
    (or (false? object)
	(and (procedure? object)
	     (procedure-arity-valid? object 1)))))

(define-variable sendmail-program
  "Filename of sendmail program."
  (os/sendmail-program)
  string?)

(define-variable send-mail-procedure
  "Procedure to call to send the current buffer as mail.
The headers are delimited by a string found in mail-header-separator."
  (lambda () (sendmail-send-it))
  (lambda (object)
    (and (procedure? object)
	 (procedure-arity-valid? object 0))))
(variable-permanent-local! (ref-variable-object send-mail-procedure))

(define-variable mail-reply-buffer
  ""
  #f
  (lambda (object) (or (false? object) (buffer? object))))
(variable-permanent-local! (ref-variable-object mail-reply-buffer))

(define-command mail
  "Edit a message to be sent.  Argument means resume editing (don't erase).
While editing message, type C-c C-c to send the message and exit.

Separate names of recipients with commas.

Various special commands starting with C-c are available in sendmail mode
to move to message header fields.

If mail-self-blind is true, a BCC: to yourself is inserted when the
message is initialized.

If mail-default-reply-to is a string, a Reply-to: field containing
that string is inserted.

If mail-archive-file-name is true, an FCC: field with that file name
is inserted."
  "P"
  (lambda (no-erase?) (mail-command no-erase? select-buffer)))

(define-command mail-other-window
  "Like \\[mail], but display mail buffer in another window."
  "P"
  (lambda (no-erase?) (mail-command no-erase? select-buffer-other-window)))

(define-command mail-other-frame
  "Like \\[mail], but display mail buffer in another frame."
  "P"
  (lambda (no-erase?) (mail-command no-erase? select-buffer-other-screen)))

(define (mail-command no-erase? select-buffer)
  (make-mail-buffer '(("To" "") ("Subject" "")) #f select-buffer
		    (if no-erase?
			'KEEP-PREVIOUS-MAIL
			'QUERY-DISCARD-PREVIOUS-MAIL)))

(define (make-mail-buffer headers reply-buffer #!optional
			  selector handle-previous buffer-name mode)
  (let ((selector (if (default-object? selector) #f selector))
	(handle-previous
	 (if (default-object? handle-previous)
	     'QUERY-DISCARD-PREVIOUS-MAIL
	     handle-previous))
	(buffer-name
	 (if (or (default-object? buffer-name) (not buffer-name))
	     "*mail*"
	     buffer-name))
	(mode (if (default-object? mode) #f mode)))
    (let ((buffer (find-buffer buffer-name))
	  (continue
	   (lambda (select?)
	     (let ((buffer (find-or-create-buffer buffer-name)))
	       (buffer-reset! buffer)
	       (set-buffer-default-directory! buffer
					      (default-homedir-pathname))
	       (setup-buffer-auto-save! buffer)
	       (mail-setup buffer headers reply-buffer mode)
	       (if (and select? selector) (selector buffer))
	       buffer))))
      (cond ((not buffer)
	     (continue #t))
	    ((eq? handle-previous 'KEEP-PREVIOUS-MAIL)
	     (if selector (selector buffer))
	     #f)
	    ((or (not (buffer-modified? buffer))
		 (eq? handle-previous 'DISCARD-PREVIOUS-MAIL))
	     (continue #t))
	    ((eq? handle-previous 'QUERY-DISCARD-PREVIOUS-MAIL)
	     (if selector (selector buffer))
	     (if (cleanup-pop-up-buffers
		  (lambda ()
		    (if (not selector) (pop-up-buffer buffer))
		    (prompt-for-confirmation?
		     "Unsent message being composed; erase it")))
		 (continue #f)
		 #f))
	    (else
	     (error:bad-range-argument handle-previous 'MAKE-MAIL-BUFFER))))))

(define (mail-setup buffer headers reply-buffer #!optional mode)
  (guarantee-mail-aliases)
  (set-buffer-major-mode! buffer
			  (or (and (not (default-object? mode)) mode)
			      (ref-mode-object mail)))
  (local-set-variable! mail-reply-buffer reply-buffer buffer)
  (let ((headers (add-standard-headers headers buffer))
	(point (mark-left-inserting-copy (buffer-start buffer)))
	(fill
	 (lambda (start end)
	   (fill-region-as-paragraph start end
				     "\t" (ref-variable fill-column buffer)
				     #f))))
    (let ((start (mark-right-inserting-copy point)))
      (for-each (lambda (header)
		  (let ((key (car header))
			(value (cadr header)))
		    (if value
			(begin
			  (move-mark-to! start point)
			  (insert-string key point)
			  (insert-string ": " point)
			  (insert-string value point)
			  (if (and (not (string-null? value))
				   (if (null? (cddr header))
				       (or (string-ci=? key "to")
					   (string-ci=? key "cc"))
				       (caddr header)))
			      (fill start point))
			  (insert-newline point)))))
		headers)
      (mark-temporary! start))
    (let ((mail-header-function (ref-variable mail-header-function buffer)))
      (if mail-header-function
	  (mail-header-function point)))
    (insert-string (ref-variable mail-header-separator buffer) point)
    (insert-newline point)
    (mark-temporary! point)
    (let ((given-header?
	   (lambda (name null-true?)
	     (let ((header
		    (list-search-positive headers
		      (lambda (header)
			(string-ci=? (car header) name)))))
	       (and header
		    (cadr header)
		    (if null-true?
			(string-null? (cadr header))
			(not (string-null? (cadr header)))))))))
      (set-buffer-point! buffer
			 (if (given-header? "To" #t)
			     (mail-position-on-field buffer "To")
			     (buffer-end buffer)))
      (if (not (or (given-header? "To" #f)
		   (given-header? "Subject" #f)
		   (given-header? "In-reply-to" #f)))
	  (buffer-not-modified! buffer))))
  (event-distributor/invoke! (ref-variable mail-setup-hook buffer) buffer))

(define (add-standard-headers headers buffer)
  (let ((add
	 (lambda (key value)
	   (if (string? value)
	       (list (list key value #f))
	       '()))))
    (let ((add-unique
	   (lambda (key value)
	     (add key
		  (and (not (list-search-positive headers
			      (lambda (header)
				(string-ci=? (car header) key))))
		       value)))))
      (append headers
	      (add "Reply-to"
		   (let ((mail-default-reply-to
			  (ref-variable mail-default-reply-to buffer)))
		     (if (procedure? mail-default-reply-to)
			 (mail-default-reply-to)
			 mail-default-reply-to)))
	      (add "BCC"
		   (and (ref-variable mail-self-blind buffer)
			(mail-from-string buffer)))
	      (add "FCC" (ref-variable mail-archive-file-name buffer))
	      (add-unique "Organization" (mail-organization-string buffer))
	      (add-unique "X-Mailer" (mailer-version-string buffer))))))

(define (mail-from-string buffer)
  (let ((address
	 (or (ref-variable user-mail-address buffer)
	     (string-append (current-user-name)
			    "@"
			    (or (ref-variable mail-host-address buffer)
				(os/hostname)))))
	(full-name (ref-variable mail-full-name buffer)))
    (if (string-null? full-name)
	address
	(case (ref-variable mail-from-style buffer)
	  ((PARENS)
	   (string-append address " (" full-name ")"))
	  ((ANGLES)
	   (string-append (rfc822:quote-string full-name)
			  " <" address ">"))
	  (else address)))))

(define (mail-organization-string buffer)
  (let ((organization (ref-variable mail-organization buffer)))
    (and (not (string-null? organization))
	 organization)))

(define (mailer-version-string buffer)
  (and (ref-variable mail-identify-reader buffer)
       (string-append "Edwin [version "
		      (get-subsystem-version-string "edwin")
		      ", MIT Scheme Release "
		      (get-subsystem-version-string "release")
		      "]")))

(define-variable mail-setup-hook
  "An event distributor invoked immediately after a mail buffer is initialized.
The mail buffer is passed as an argument; it is not necessarily selected."
  (make-event-distributor))

(define-major-mode mail text "Mail"
  "Major mode for editing mail to be sent.
Like Text Mode but with these additional commands:
\\[mail-send]  mail-send (send the message)    \\[mail-send-and-exit]  mail-send-and-exit
Here are commands that move to a header field (and create it if there isn't):
	 \\[mail-to]  move to To:	\\[mail-subject]  move to Subject:
	 \\[mail-cc]  move to CC:	\\[mail-bcc]  move to BCC:
\\[mail-signature]  mail-signature (insert ~/.signature file).
\\[mail-yank-original]  mail-yank-original (insert current message, in Rmail).
\\[mail-fill-yanked-message]  mail-fill-yanked-message (fill what was yanked)."
  (lambda (buffer)
    (define-variable-local-value! buffer (ref-variable-object paragraph-start)
      (string-append "^"
		     (re-quote-string (ref-variable mail-header-separator))
		     "$\\|^[ \t]*[-_][-_][-_]+$\\|"
		     (ref-variable paragraph-start buffer)))
    (define-variable-local-value! buffer
	(ref-variable-object paragraph-separate)
      (string-append "^"
		     (re-quote-string (ref-variable mail-header-separator))
		     "$\\|^[ \t]*[-_][-_][-_]+$\\|"
		     (ref-variable paragraph-separate buffer)))
    (event-distributor/invoke! (ref-variable mail-mode-hook buffer) buffer)))

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

(define ((field-mover field))
  (set-current-point! (mail-position-on-field (current-buffer) field)))

(define ((cc-field-mover field))
  (set-current-point! (mail-position-on-cc-field (current-buffer) field)))

(define-command mail-to
  "Move point to end of To field."
  ()
  (field-mover "To"))

(define-command mail-subject
  "Move point to end of Subject field."
  ()
  (field-mover "Subject"))

(define-command mail-cc
  "Move point to end of CC field."
  ()
  (cc-field-mover "CC"))

(define-command mail-bcc
  "Move point to end of BCC field."
  ()
  (cc-field-mover "BCC"))

(define (mail-position-on-field buffer field)
  (let ((start (buffer-start buffer)))
    (mail-field-end! start
		     (mail-match-header-separator start (buffer-end buffer))
		     field)))

(define (mail-position-on-cc-field buffer field)
  (let ((start (buffer-start buffer)))
    (let ((end (mail-match-header-separator start (buffer-end buffer))))
      (or (mail-field-end start end field)
	  (mail-insert-field (or (mail-field-end start end "CC")
				 (mail-field-end start end "To")
				 (mail-insert-field end "To"))
			     field)))))

(define (mail-match-header-separator start end)
  (if (not (re-search-forward
	    (string-append
	     "^"
	     (re-quote-string (ref-variable mail-header-separator start))
	     "$")
	    start end #f))
      (editor-error "Can't find mail-header-separator."))
  (re-match-start 0))

(define (mail-header-end start #!optional end error?)
  (let ((mark
	 (search-forward "\n\n"
			 start
			 (if (or (default-object? end) (not end))
			     (group-end start)
			     end)
			 #f)))
    (if (and (not mark) (or (default-object? error?) error?))
	(error "Unable to locate mail header end:" start))
    (and mark
	 (mark-1+ mark))))

(define (mail-field-start header-start header-end field)
  (re-search-forward (string-append "^" field ":[ \t]*")
		     header-start
		     header-end
		     #t))

(define (mail-field-end header-start header-end field)
  (let ((field-start (mail-field-start header-start header-end field)))
    (and field-start
	 (%mail-field-end field-start header-end))))

(define (mail-field-region header-start header-end field)
  (let ((field-start (mail-field-start header-start header-end field)))
    (and field-start
	 (make-region field-start (%mail-field-end field-start header-end)))))

(define (%mail-field-end field-start header-end)
  (if (re-search-forward "^[^ \t]" field-start header-end #f)
      (mark-1+ (re-match-start 0))
      header-end))

(define (mail-insert-field mark field)
  (let ((mark (mark-left-inserting-copy mark)))
    (if (not (line-start? mark))
	(let ((ls (line-start mark 1 #f)))
	  (if ls
	      (move-mark-to! mark ls)
	      (begin
		(move-mark-to! mark (line-end mark 0))
		(insert-newline mark)))))
    (insert-string field mark)
    (insert-string ": " mark)
    (insert-newline mark)
    (mark-temporary! mark)
    (mark-1+ mark)))

(define (mail-field-end! header-start header-end field)
  (or (mail-field-end header-start header-end field)
      (mail-insert-field header-end field)))

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
		(let ((method
		       (buffer-get mail-reply-buffer
				   'MAIL-YANK-ORIGINAL-METHOD
				   #f)))
		  (if method
		      (method mail-reply-buffer end)
		      (insert-region (buffer-start mail-reply-buffer)
				     (buffer-end mail-reply-buffer)
				     start)))
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
	  (if (re-search-forward "\n\n" start end #f)
	      (mark1+ (re-match-start 0))
	      end)))
	(mail-yank-ignored-headers (ref-variable mail-yank-ignored-headers)))
    (with-text-clipped start end
      (lambda ()
	(do ()
	    ((not
	      (re-search-forward mail-yank-ignored-headers start end #t)))
	  (move-mark-to! start (re-match-start 0))
	  (delete-string
	   start
	   (if (re-search-forward "^[^ \t]" (line-end start 0) end #f)
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
				  #t))))

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
If  mail-interactive  is true, wait for success indication
or error messages, and inform user.
Otherwise any failure is reported in a message back to
the user from the mailer."
  ()
  (lambda ()
    ((ref-variable send-mail-procedure))
    (buffer-not-modified! (current-buffer))
    (delete-auto-save-file! (current-buffer))))

(define (sendmail-send-it)
  (let ((mail-buffer (current-buffer)))
    (let ((temp-buffer
	   (prepare-mail-buffer-for-sending mail-buffer
	     (lambda (start end)
	       ;; Don't send out a blank subject line.
	       (if (re-search-forward "^Subject:[ \t]*\n" start end #t)
		   (delete-match))))))
      (dynamic-wind
       (lambda () unspecific)
       (lambda ()
	 (if (ref-variable mail-relay-host mail-buffer)
	     (smtp-mail-buffer temp-buffer mail-buffer)
	     (let ((errors (send-mail-buffer temp-buffer mail-buffer)))
	       (if errors
		   (editor-error errors)))))
       (lambda () (kill-buffer temp-buffer))))))

(define (prepare-mail-buffer-for-sending mail-buffer process-header)
  (let ((temp-buffer (temporary-buffer " sendmail temp")))
    (let ((start (mark-right-inserting-copy (buffer-start temp-buffer)))
	  (end (mark-left-inserting-copy (buffer-end temp-buffer))))
      (insert-region (buffer-start mail-buffer)
		     (buffer-end mail-buffer)
		     start)
      (guarantee-newline end)
      (mail-match-header-separator start end)
      (let ((header-end (mark-left-inserting-copy (delete-match))))
	;; Delete any blank lines in the header.
	(do ((start start (replace-match "\n")))
	    ((not (re-search-forward "\n\n+" start header-end #f))))
	(expand-mail-aliases start header-end)
	(if (re-search-forward "^FCC:" start header-end #t)
	    (mail-do-fcc temp-buffer header-end))
	;; If there is a From and no Sender, put in a Sender.
	(if (and (re-search-forward "^From:" start header-end #t)
		 (not (re-search-forward "^Sender:" start header-end #t)))
	    (begin
	      (insert-string "Sender: " header-end)
	      (insert-string (current-user-name) header-end)
	      (insert-string "\n" header-end)))
	(process-header start header-end)
	(mark-temporary! header-end))
      (mark-temporary! end)
      (mark-temporary! start))
    temp-buffer))

(define (send-mail-buffer mail-buffer lookup-buffer)
  (let ((error-buffer
	 (and (ref-variable mail-interactive lookup-buffer)
	      (temporary-buffer " sendmail errors")))
	(msg "Sending..."))
    (message msg)
    (let ((program (ref-variable sendmail-program lookup-buffer)))
      (if error-buffer
	  (begin
	    (run-synchronous-process (buffer-region mail-buffer)
				     (buffer-end error-buffer)
				     #f #f program "-oi" "-t"
				     ;; Always specify who from,
				     ;; since some systems have
				     ;; broken sendmails.
				     "-f" (current-user-name))
	    (let ((end (buffer-end error-buffer)))
	      (do ((start (buffer-start error-buffer) (replace-match "; ")))
		  ((not (re-search-forward "\n+ *" start end #f))))))
	  ;; If we aren't going to look at the errors, run the
	  ;; program in the background so control returns to the
	  ;; user as soon as possible.
	  (let ((process
		 (start-pipe-subprocess
		  (os/find-program program #f (ref-variable exec-path))
		  (vector (file-namestring program) "-oi" "-t"
			  (string-append "-f" (current-user-name))
			  ;; These mean "report errors by mail" and
			  ;; "deliver in background".
			  "-oem" "-odb")
		  #f)))
	    (channel-write-string-block (subprocess-output-channel process)
					(buffer-string mail-buffer))
	    (subprocess-delete process))))
    (let ((errors
	   (and error-buffer
		(let ((errors (buffer-string error-buffer)))
		  (kill-buffer error-buffer)
		  (and (not (string-null? errors))
		       (string-append  "Sending...failed to " errors))))))
      (if (not errors)
	  (message msg "done"))
      errors)))

(define (mail-do-fcc mail-buffer header-end)
  (let ((pathnames (digest-fcc-headers (buffer-start mail-buffer) header-end))
	(temp-buffer (temporary-buffer " rmail output")))
    (let ((start (buffer-start temp-buffer))
	  (end (buffer-end temp-buffer)))
      (insert-newline end)
      (insert-string "From " end)
      (insert-string (current-user-name) end)
      (insert-string " " end)
      (insert-string (universal-time->string (get-universal-time)) end)
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
	    ((not (re-search-forward "^From " m end #f)))
	  (move-mark-to! m (re-match-end 0))
	  (insert-string ">" (re-match-start 0)))
	(mark-temporary! m))
      (for-each (lambda (pathname)
		  (let ((buffer (pathname->buffer pathname)))
		    (if buffer
			(insert-region start end (buffer-end buffer))
			(append-to-file (make-region start end)
					pathname
					#t
					#t))))
		pathnames)
      (kill-buffer temp-buffer))))

(define (digest-fcc-headers start header-end)
  (let ((m (mark-right-inserting-copy start)))
    (let loop ((pathnames '()))
      (if (re-search-forward "^FCC:[ \t]*\\([^ \t\n]+\\)" m header-end #t)
	  (let ((filename
		 (extract-string (re-match-start 1) (re-match-end 1))))
	    (move-mark-to! m (line-start (re-match-start 0) 0))
	    (delete-string m (line-start m 1))
	    (loop (cons (->pathname filename) pathnames)))
	  (begin
	    (mark-temporary! m)
	    pathnames)))))

;;;; Direct SMTP

(define (smtp-mail-buffer mail-buffer lookup-buffer)
  (let ((msg "Sending..."))
    (message msg)
    (let ((from
	   (rfc822:canonicalize-address-string
	    (mail-from-string lookup-buffer)))
	  (rcpts (mail-deduce-address-list mail-buffer))
	  (trace-buffer
	   (and (ref-variable smtp-trace lookup-buffer)
		(temporary-buffer "*SMTP-trace*")))
	  (require-valid?
	   (ref-variable smtp-require-valid-recipients lookup-buffer))
	  (valid-response?
	   (lambda (response) (= 250 (smtp-response-number response)))))
      (if (null? rcpts)
	  (editor-error "No recipients specified for mail."))
      (mail-delete-bcc-lines mail-buffer)
      (let ((responses
	     (call-with-smtp-socket (ref-variable mail-relay-host
						  lookup-buffer)
				    trace-buffer
	       (lambda (port banner)
		 banner
		 (smtp-command/helo port)
		 (smtp-command/mail port from)
		 (let ((responses
			(map (lambda (rcpt)
			       (smtp-command/rcpt port rcpt))
			     rcpts)))
		   (if (if require-valid?
			   (for-all? responses valid-response?)
			   (there-exists? responses valid-response?))
		       (smtp-command/data port mail-buffer)
		       (smtp-command/rset port))
		   (smtp-command/quit port)
		   responses)))))
	(cond ((not (for-all? responses valid-response?))
	       (pop-up-temporary-buffer "*SMTP-invalid*"
					'(READ-ONLY FLUSH-ON-SPACE)
		 (lambda (buffer window)
		   window
		   (let ((m (mark-left-inserting-copy (buffer-start buffer))))
		     (for-each (lambda (rcpt response)
				 (if (not (valid-response? response))
				     (begin
				       (insert-string rcpt m)
				       (insert-char #\tab m)
				       (insert-string response m)
				       (insert-newline m))))
			       rcpts responses)
		     (mark-temporary! m)))))
	      (trace-buffer
	       (set-buffer-point! trace-buffer (buffer-start trace-buffer))
	       (buffer-not-modified! trace-buffer)
	       (pop-up-buffer trace-buffer #f)))
	(message msg
		 (if (if require-valid?
			 (for-all? responses valid-response?)
			 (there-exists? responses valid-response?))
		     "done"
		     "aborted"))))))

(define (mail-deduce-address-list mail-buffer)
  (let* ((header-start (buffer-start mail-buffer))
	 (header-end (mail-header-end header-start))
	 (regexp
	  (if (mail-field-start header-start header-end "resent-to")
	      "^\\(resent-to:\\|resent-cc:\\|resent-bcc:\\)[ \t]*"
	      "^\\(to:\\|cc:\\|bcc:\\)[ \t]*")))
    (let loop ((start header-start) (addresses '()))
      (let ((field-start (re-search-forward regexp start header-end #t)))
	(if field-start
	    (let ((field-end (%mail-field-end field-start header-end)))
	      (loop field-end
		    (cons (rfc822:string->addresses
			   (extract-string field-start field-end))
			  addresses)))
	    (apply append (reverse! addresses)))))))

(define (mail-delete-bcc-lines mail-buffer)
  (let* ((header-start (buffer-start mail-buffer))
	 (header-end (mail-header-end header-start)))
    (let loop ((start header-start))
      (let ((fs (mail-field-start start header-end "bcc")))
	(if fs
	    (let ((ls (line-start fs 0)))
	      (delete-string ls
			     (let ((fe (%mail-field-end fs header-end)))
			       (if (mark< fe header-end) (mark1+ fe) fe)))
	      (loop ls)))))))

(define (call-with-smtp-socket host-name trace-buffer receiver)
  (let ((port #f))
    (dynamic-wind
     (lambda ()
       (set! port
	     (make-smtp-port (open-tcp-stream-socket host-name "smtp")
			     trace-buffer))
       unspecific)
     (lambda ()
       (receiver port (smtp-read-response port 220)))
     (lambda ()
       (if port
	   (begin
	     (close-port (smtp-port-port port))
	     (set! port #f)
	     unspecific))))))

(define-structure smtp-port
  (port #f read-only #t)
  (trace-buffer #f read-only #t))

(define (smtp-read-line port)
  (let ((line (read-line (smtp-port-port port))))
    (smtp-trace-write-string line port)
    (smtp-trace-newline port)
    line))

(define (smtp-write-line port . strings)
  (for-each (lambda (string)
	      (smtp-trace-write-string string port)
	      (write-string string (smtp-port-port port)))
	    strings)
  (smtp-trace-newline port)
  (newline (smtp-port-port port)))

(define (smtp-drain-output port)
  (flush-output (smtp-port-port port)))

(define (smtp-trace-write-string string port)
  (let ((trace-buffer (smtp-port-trace-buffer port)))
    (if trace-buffer
	(insert-string string (buffer-end trace-buffer)))))

(define (smtp-trace-newline port)
  (let ((trace-buffer (smtp-port-trace-buffer port)))
    (if trace-buffer
	(insert-newline (buffer-end trace-buffer)))))

(define (smtp-command/helo port)
  (smtp-write-line port "HELO " (os/hostname))
  (smtp-read-response port 250))

(define (smtp-command/mail port from)
  (smtp-write-line port "MAIL FROM:<" from ">")
  (smtp-read-response port 250))

(define (smtp-command/rcpt port rcpt)
  (smtp-write-line port "RCPT TO:<" rcpt ">")
  (smtp-read-response port 250 550))

(define (smtp-command/data port mail-buffer)
  (smtp-write-line port "DATA")
  (smtp-read-response port 354)
  (let loop ((start (buffer-start mail-buffer)))
    (if (not (group-end? start))
	(let ((le (line-end start 0)))
	  (let ((line (extract-string start le)))
	    (if (and (fix:> 0 (string-length line))
		     (char=? #\. (string-ref line 0)))
		(smtp-write-line port "." line)
		(smtp-write-line port line)))
	  (if (not (group-end? le))
	      (loop (mark1+ le))))))
  (smtp-write-line port ".")
  (smtp-read-response port 250))

(define (smtp-command/rset port)
  (smtp-write-line port "RSET")
  (smtp-read-response port 250))

(define (smtp-command/quit port)
  (smtp-write-line port "QUIT")
  (smtp-read-response port 221))

(define (smtp-read-response port . numbers)
  (smtp-drain-output port)
  (let ((response (smtp-read-line port)))
    (let ((n (smtp-response-number response)))
      (if (not (there-exists? numbers (lambda (n*) (= n n*))))
	  (editor-error response))
      (if (smtp-response-continued? response)
	  (let loop ((responses (list response)))
	    (let ((response (smtp-read-line port)))
	      (if (not (= n (smtp-response-number response)))
		  (error "Mismatched codes in multiline response:" n response))
	      (let ((responses (cons response responses)))
		(if (smtp-response-continued? response)
		    (loop responses)
		    (convert-smtp-multiline-response (reverse! responses))))))
	  response))))

(define (smtp-response-number line)
  (or (and (fix:>= (string-length line) 3)
	   (substring->nonnegative-integer line 0 3))
      (error "Malformed SMTP response:" line)))

(define (smtp-response-continued? line)
  (and (fix:>= (string-length line) 4)
       (char=? #\- (string-ref line 3))))

(define (convert-smtp-multiline-response responses)
  (apply string-append
	 (cons* (string-head (car responses) 3)
		" "
		(let ((lines
		       (map (lambda (response) (string-tail response 4))
			    responses)))
		  (cons (car lines)
			(append-map (lambda (line) (list "\n" line))
				    lines))))))