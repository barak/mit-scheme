;;; -*-Scheme-*-
;;;
;;;	$Id: sendmail.scm,v 1.31 1995/05/10 20:48:39 cph Exp $
;;;
;;;	Copyright (c) 1991-95 Massachusetts Institute of Technology
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

(define-variable mail-default-reply-to
  "Address to insert as default Reply-to field of outgoing messages."
  false
  (lambda (object)
    (or (not object)
	(string? object)
	(and (procedure? object)
	     (procedure-arity-valid? object 0)))))

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
False means let mailer mail back a message to report errors."
  false
  boolean?)

(define-variable mail-header-separator
  "Line used to separate headers from text in messages being composed."
  "--text follows this line--"
  string?)

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
    (insert-newline point))
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
  false
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
  (lambda (no-erase?)
    (make-mail-buffer '(("To" "") ("Subject" ""))
		      #f
		      select-buffer
		      (if no-erase?
			  'KEEP-PREVIOUS-MAIL
			  'QUERY-DISCARD-PREVIOUS-MAIL))))

(define-command mail-other-window
  "Like `mail' command, but display mail buffer in another window."
  "P"
  (lambda (no-erase?)
    (make-mail-buffer '(("To" "") ("Subject" ""))
		      #f
		      select-buffer-other-window
		      (if no-erase?
			  'KEEP-PREVIOUS-MAIL
			  'QUERY-DISCARD-PREVIOUS-MAIL))))

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
  (let ((point (mark-left-inserting-copy (buffer-start buffer)))
	(fill
	 (lambda (start end)
	   (fill-region-as-paragraph start end
				     "\t" (ref-variable fill-column buffer)
				     false))))
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
    (let ((mail-default-reply-to (ref-variable mail-default-reply-to buffer)))
      (let ((mail-default-reply-to
	     (if (procedure? mail-default-reply-to)
		 (mail-default-reply-to)
		 mail-default-reply-to)))
	(if (string? mail-default-reply-to)
	    (begin
	      (insert-string "Reply-to: " point)
	      (insert-string mail-default-reply-to point)
	      (insert-newline point)))))
    (let ((mail-header-function (ref-variable mail-header-function buffer)))
      (if mail-header-function
	  (mail-header-function point)))
    (if (ref-variable mail-self-blind buffer)
	(begin
	  (insert-string "BCC: " point)
	  (insert-string (current-user-name) point)
	  (insert-newline point)))
    (let ((mail-archive-file-name
	   (ref-variable mail-archive-file-name buffer)))
      (if mail-archive-file-name
	  (begin
	    (insert-string "FCC: " point)
	    (insert-string mail-archive-file-name point)
	    (insert-newline point))))
    (insert-string (ref-variable mail-header-separator buffer) point)
    (insert-newline point)
    (mark-temporary! point))
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
	(buffer-not-modified! buffer)))
  (event-distributor/invoke! (ref-variable mail-setup-hook buffer) buffer))

(define-variable mail-setup-hook
  "An event distributor invoked immediately after a mail buffer is initialized.
The mail buffer is passed as an argument; it is not necessarily selected."
  (make-event-distributor))

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
	  (mail-insert-field (mail-field-end! start end "To") field)))))

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
      (let ((errors (send-mail-buffer temp-buffer mail-buffer)))
	(kill-buffer temp-buffer)
	(if errors (editor-error errors))))))

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
	      (insert-string "\nSender: " header-end)
	      (insert-string (current-user-name) header-end)))
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
		  (os/find-program program #f)
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
      (insert-string (file-time->string (current-file-time)) end)
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