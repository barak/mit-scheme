;;; -*-Scheme-*-
;;;
;;; $Id: imail-summary.scm,v 1.46 2001/09/20 18:13:01 cph Exp $
;;;
;;; Copyright (c) 2000-2001 Massachusetts Institute of Technology
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
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

;;;; IMAIL mail reader: summary buffer

(declare (usual-integrations))

(define-variable imail-summary-pop-up-message
  "If true, selecting a message in the IMAIL summary buffer pops up the
 message buffer in a separate window.
If false, the message buffer is updated but not popped up."
  #t
  boolean?)

(define-variable imail-summary-auto-select
  "If true, some cursor motion commands cause automatic message selection.
If false, these commands move the cursor but don't select messages.
The commands affected are:
    \\[imail-summary-next-message]
    \\[imail-summary-previous-message]
    \\[imail-summary-first-message]
    \\[imail-summary-last-message]"
  #t
  boolean?)

(define-variable imail-summary-highlight-message
  "If true, the selected message is highlighted in the summary buffer."
  #t
  boolean?)

(define-variable imail-summary-show-date
  "If true, an abbreviated date field is shown."
  #f
  boolean?)

(define-variable imail-summary-subject-width
  "Width of the subject field, in characters."
  35
  exact-nonnegative-integer?)

(define-variable imail-summary-height
  "Height of the summary window, either in lines or as a fraction.
An exact positive integer means a fixed number of lines.
A real number between 0 and 1 exclusive means a fraction of the screen height."
  1/4
  (lambda (x)
    (or (and (exact-integer? x) (positive? x))
	(and (real? x) (< 0 x 1)))))

(define-variable imail-summary-fixed-layout
  "If true, summary buffer is linked to folder buffer in fixed layout.
Selecting either buffer causes both to be selected,
 in a standard window configuration.
Once selected, selecting another buffer causes the window configuration
 to be restored to a single window."
  #f
  boolean?)

(define-command imail-summary
  "Display a summary of the selected folder, one line per message."
  ()
  (lambda () (imail-summary "All" #f)))

(define-command imail-summary-by-flags
  "Display a summary of all messages with one or more FLAGS.
FLAGS is a string containing the desired labels, separated by commas."
  (lambda ()
    (list (imail-prompt-for-flags "Flags to summarize by")))
  (lambda (flags-string)
    (imail-summary (string-append "Flags " flags-string)
		   (let ((flags (burst-comma-list-string flags-string)))
		     (lambda (m)
		       (there-exists? (message-flags m)
			 (lambda (flag)
			   (flags-member? flag flags))))))))

(define-command imail-summary-by-recipients
  "Display a summary of all messages with the given RECIPIENTS.
Normally checks the To, From and Cc fields of headers;
but if prefix arg is given, only look in the To and From fields.
RECIPIENTS is a string of regexps separated by commas."
  "sRecipients to summarize by\nP"
  (lambda (recipients-string primary-only?)
    (imail-summary
     (string-append "Recipients " recipients-string)
     (let ((regexp
	    (apply regexp-group (burst-comma-list-string recipients-string))))
       (let ((try
	      (lambda (s)
		(and s
		     (re-string-search-forward regexp s #t)))))
	 (lambda (m)
	   (or (try (get-first-header-field-value m "from" #f))
	       (try (get-first-header-field-value m "to" #f))
	       (and (not primary-only?)
		    (try (get-first-header-field-value m "cc" #f))))))))))

(define-command imail-summary-by-regexp
  "Display a summary of all messages according to regexp REGEXP.
If the regular expression is found in the header of the message
\(including in the date and other lines, as well as the subject line),
Edwin will list the header line in the summary."
  "sRegexp to summarize by"
  (lambda (regexp)
    (imail-summary
     (string-append "Regular expression " regexp)
     (let ((case-fold? (ref-variable case-fold-search)))
       (lambda (m)
	 (re-string-search-forward regexp
				   (header-fields->string
				    (message-header-fields m))
				   case-fold?))))))

(define-command imail-summary-by-topic
  "Display a summary of all messages with the given SUBJECT.
Checks the Subject field of headers.
SUBJECT is a string of regexps separated by commas."
  "sTopics to summarize by"
  (lambda (regexps-string)
    (imail-summary
     (string-append "About " regexps-string)
     (let ((regexp
	    (apply regexp-group (burst-comma-list-string regexps-string)))
	   (case-fold? (ref-variable case-fold-search)))
       (lambda (m)
	 (let ((s (get-first-header-field-value m "subject" #f)))
	   (and s
		(re-string-search-forward regexp s case-fold?))))))))

(define (imail-summary description predicate)
  (let* ((folder (selected-folder))
	 (folder-buffer (imail-folder->buffer folder #t))
	 (buffer
	  (let ((buffer (buffer-get folder-buffer 'IMAIL-SUMMARY-BUFFER #f)))
	    (or (and buffer
		     (if (buffer-alive? buffer)
			 buffer
			 (begin
			   (buffer-remove! folder-buffer 'IMAIL-SUMMARY-BUFFER)
			   #f)))
		(let ((buffer
		       (new-buffer
			(string-append (buffer-name folder-buffer)
				       "-summary"))))
		  (without-interrupts
		   (lambda ()
		     (add-kill-buffer-hook buffer imail-summary-detach)
		     (receive-modification-events
		      folder
		      imail-summary-modification-event)
		     (buffer-put! folder-buffer 'IMAIL-SUMMARY-BUFFER buffer)
		     (associate-buffer-with-imail-buffer folder-buffer buffer)
		     (buffer-put! buffer 'IMAIL-NAVIGATORS
				  (imail-summary-navigators buffer))
		     (if (ref-variable imail-summary-fixed-layout buffer)
			 (create-buffer-layout imail-summary-layout-selector
					       (list buffer folder-buffer)))))
		  buffer)))))
    (buffer-put! buffer 'IMAIL-SUMMARY-DESCRIPTION description)
    (buffer-put! buffer 'IMAIL-SUMMARY-PREDICATE predicate)
    (if (not (selected-buffer? buffer))
	(let ((windows (buffer-windows buffer)))
	  (if (pair? windows)
	      (select-window (car windows))
	      (select-buffer buffer))))
    (preload-folder-outlines folder)
    (rebuild-imail-summary-buffer buffer)))

(define (imail-summary-detach buffer)
  (let ((folder-buffer (buffer-get buffer 'IMAIL-FOLDER-BUFFER #f)))
    (if folder-buffer
	(begin
	  (buffer-remove! folder-buffer 'IMAIL-SUMMARY-BUFFER)
	  (let ((folder (buffer-get folder-buffer 'IMAIL-FOLDER #f)))
	    (if folder
		(ignore-modification-events
		 folder
		 imail-summary-modification-event)))))))

(define (imail-folder->summary-buffer folder error?)
  (or (let ((buffer (imail-folder->buffer folder error?)))
	(and buffer
	     (buffer-get buffer 'IMAIL-SUMMARY-BUFFER #f)))
      (and error?
	   (error:bad-range-argument folder 'IMAIL-FOLDER->SUMMARY-BUFFER))))

(define (imail-summary-buffer->folder buffer error?)
  (or (let ((folder-buffer (buffer-get buffer 'IMAIL-FOLDER-BUFFER #f)))
	(and folder-buffer
	     (buffer-get folder-buffer 'IMAIL-FOLDER #f)))
      (and error?
	   (error:bad-range-argument buffer 'IMAIL-SUMMARY-BUFFER->FOLDER))))

(define (imail-summary-layout-selector window buffers)
  (let ((summary-buffer (car buffers))
	(folder-buffer (cadr buffers)))
    (select-buffer summary-buffer window)
    (let ((w (window-split-vertically! window (imail-summary-height window))))
      (if w
	  (select-buffer folder-buffer w)))))

(define (imail-summary-modification-event folder type parameters)
  (let ((buffer (imail-folder->summary-buffer folder #f)))
    (if buffer
	(case type
	  ((FLAGS)
	   (let ((message (car parameters)))
	     (call-with-values
		 (lambda () (imail-summary-find-message buffer message))
	       (lambda (mark approximate?)
		 (if (and mark (not approximate?))
		     (begin
		       (let ((mark (mark+ mark 1 'ERROR)))
			 (with-read-only-defeated mark
			   (lambda ()
			     (group-replace-string!
			      (mark-group mark)
			      (mark-index mark)
			      (message-flag-markers message)))))
		       (buffer-not-modified! buffer)))))))
	  ((SELECT-MESSAGE)
	   (let ((message (car parameters)))
	     (if message
		 (imail-summary-select-message buffer message))))
	  ((EXPUNGE INCREASE-LENGTH SET-LENGTH REORDERED)
	   (maybe-add-command-suffix! rebuild-imail-summary-buffer buffer))))))

;;;; Summary content generation

(define (rebuild-imail-summary-buffer buffer)
  (let ((folder (selected-folder #f buffer)))
    (if folder
	(begin
	  (buffer-widen! buffer)
	  (with-read-only-defeated (buffer-start buffer)
	    (lambda ()
	      (region-delete! (buffer-region buffer))
	      (fill-imail-summary-buffer! buffer
					  folder
					  (buffer-get buffer
						      'IMAIL-SUMMARY-PREDICATE
						      #f))))
	  (set-buffer-major-mode! buffer (ref-mode-object imail-summary))
	  (buffer-not-modified! buffer)
	  (set-buffer-point! buffer (imail-summary-first-line buffer))
	  (let ((message
		 (selected-message #f
				   (buffer-get buffer
					       'IMAIL-FOLDER-BUFFER #f))))
	    (if message
		(imail-summary-select-message buffer message)))))))

(define (fill-imail-summary-buffer! buffer folder predicate)
  (let ((end (folder-length folder)))
    (let ((messages
	   (let loop ((i 0) (messages '()))
	     (if (< i end)
		 (loop (+ i 1) (cons (get-message folder i) messages))
		 (reverse! messages))))
	  (index-digits (exact-nonnegative-integer-digits end))
	  (show-date? (ref-variable imail-summary-show-date buffer))
	  (subject-width (imail-summary-subject-width buffer)))
      (let ((mark (mark-left-inserting-copy (buffer-start buffer))))
	(insert-string " Flags" mark)
	(insert-string " " mark)
	(insert-chars #\# index-digits mark)
	(insert-string " Length" mark)
	(if show-date? (insert-string "  Date " mark))
	(insert-string "  " mark)
	(insert-string-pad-right "Subject" subject-width #\space mark)
	(insert-string "  " mark)
	(insert-string "From" mark)
	(insert-newline mark)
	(insert-string " -----" mark)
	(insert-string " " mark)
	(insert-chars #\- index-digits mark)
	(insert-string " ------" mark)
	(if show-date? (insert-string " ------" mark))
	(insert-string "  " mark)
	(insert-chars #\- subject-width mark)
	(insert-string "  " mark)
	(insert-chars #\-
		      (max 4 (- (mark-x-size mark) (+ (mark-column mark) 1)))
		      mark)
	(insert-newline mark)
	(for-each (lambda (message)
		    (if (or (not predicate) (predicate message))
			(write-imail-summary-line! message index-digits mark)))
		  messages)
	(mark-temporary! mark)))))

(define (write-imail-summary-line! message index-digits mark)
  (let ((m (get-property message 'IMAIL-SUMMARY-MARK #f)))
    (if m
	(mark-temporary! m)))
  (store-property! message
		   'IMAIL-SUMMARY-MARK
		   (mark-right-inserting-copy mark))
  (insert-char #\space mark)
  (insert-string (message-flag-markers message) mark)
  (insert-char #\space mark)
  (insert-string-pad-left (number->string (+ (message-index message) 1))
			  index-digits #\space mark)
  (insert-string "  " mark)
  (insert-string (message-summary-length-string message) mark)
  (if (ref-variable imail-summary-show-date mark)
      (begin
	(insert-string " " mark)
	(insert-string (message-summary-date-string message) mark)))
  (insert-string "  " mark)
  (let ((target-column
	 (+ (mark-column mark) (imail-summary-subject-width mark))))
    (insert-string (message-summary-subject-string message) mark)
    (if (> (mark-column mark) target-column)
	(delete-string (move-to-column mark target-column) mark))
    (if (< (mark-column mark) target-column)
	(insert-chars #\space (- target-column (mark-column mark)) mark)))
  (insert-string "  " mark)
  (insert-string (message-summary-from-string message) mark)
  (insert-newline mark))

(define (imail-summary-subject-width mark)
  (max (ref-variable imail-summary-subject-width mark)
       (string-length "Subject")))

(define (message-flag-markers message)
  (let ((s (make-string 5 #\space)))
    (let ((do-flag
	   (lambda (index char boolean)
	     (if boolean
		 (string-set! s index char)))))
      (do-flag 0 #\D (message-deleted? message))
      (do-flag 1 #\U (message-unseen? message))
      (do-flag 2 #\A (message-answered? message))
      (do-flag 3 #\R
	       (or (message-resent? message)
		   (message-forwarded? message)))
      (do-flag 4 #\F (message-filed? message)))
    s))

(define (message-summary-length-string message)
  (abbreviate-exact-nonnegative-integer (message-length message) 5))

(define (message-summary-date-string message)
  (let ((t (message-time message)))
    (if t
	(let ((dt (universal-time->local-decoded-time t)))
	  (string-append
	   (string-pad-left (number->string (decoded-time/day dt)) 2)
	   " "
	   (month/short-string (decoded-time/month dt))))
	(make-string 6 #\space))))

(define (message-summary-from-string message)
  (let* ((s
	  (decorated-string-append
	   "" " " ""
	   (map string-trim
		(string->lines
		 (or (get-first-header-field-value message "from" #f) "")))))
	 (field (lambda (n) (lambda (regs) (re-match-extract s regs n)))))
    (cond ((re-string-search-forward "[ \t\"]*\\<\\(.*\\)\\>[\" \t]*<.*>" s)
	   => (field 1))
	  ;; Chris VanHaren (Athena User Consultant) <vanharen>
	  ((re-string-search-forward "[ \t\"]*\\<\\(.*\\)\\>.*(.*).*<.*>.*" s)
	   => (field 1))
	  ((re-string-search-forward ".*(\\(.*\\))" s)
	   => (field 1))
	  ((re-string-search-forward ".*<\\(.*\\)>.*" s)
	   => (field 1))
	  ((re-string-search-forward " *\\<\\(.*\\)\\> *" s)
	   => (field 1))
	  (else s))))

(define (message-summary-subject-string message)
  (let ((s
	 (let ((s (or (get-first-header-field-value message "subject" #f) "")))
	   (let ((regs (re-string-match "\\(re:[ \t]*\\)+" s #t)))
	     (if regs
		 (string-tail s (re-match-end-index 0 regs))
		 s)))))
    (let ((i (string-find-next-char s #\newline)))
      (if i
	  (string-head s i)
	  s))))

;;;; Navigation

(define (imail-summary-navigators buffer)

  (define (first-unseen-message folder)
    (let loop ((message (first-message folder)))
      (and message
	   (if (message-unseen? message)
	       message
	       (loop (next-message message #f))))))

  (define (first-message folder)
    (imail-summary-navigator/edge buffer folder
				  (imail-summary-first-line buffer)))

  (define (last-message folder)
    (imail-summary-navigator/edge buffer folder
				  (imail-summary-last-line buffer)))

  (define (next-message message predicate)
    (imail-summary-navigator/delta buffer message predicate 1))

  (define (previous-message message predicate)
    (imail-summary-navigator/delta buffer message predicate -1))

  (make-imail-navigators first-unseen-message
			 first-message
			 last-message
			 next-message
			 previous-message
			 imail-summary-navigator/selected-message))

(define (imail-summary-navigator/edge buffer folder mark)
  (and folder
       (eq? folder (imail-summary-buffer->folder buffer #f))
       (let ((index (imail-summary-selected-message-index mark)))
	 (and index
	      (< index (folder-length folder))
	      (get-message folder index)))))

(define (imail-summary-navigator/delta buffer message predicate delta)
  (let ((folder (message-folder message)))
    (and folder
	 (eq? folder (imail-summary-buffer->folder buffer #f))
	 (let loop
	     ((m
	       (call-with-values
		   (lambda () (imail-summary-find-message buffer message))
		 (lambda (m approximate?)
		   (if (and approximate?
			    ((if (< delta 0) < >)
			     (imail-summary-selected-message-index m)
			     (message-index message)))
		       m
		       (and m (line-start m delta #f)))))))
	   (and m
		(let ((index (imail-summary-selected-message-index m)))
		  (and index
		       (< index (folder-length folder))
		       (let ((message (get-message folder index)))
			 (if (or (not predicate) (predicate message))
			     message
			     (loop (line-start m delta #f)))))))))))

(define (imail-summary-navigator/selected-message buffer)
  (or (let ((index
	     (let ((point (buffer-point buffer)))
	       (let loop ((offset 0))
		 (let ((next (line-start point offset #f))
		       (prev (line-start point (- offset) #f)))
		   (or (and next (imail-summary-selected-message-index next))
		       (and prev (imail-summary-selected-message-index prev))
		       (and (or next prev)
			    (loop (+ offset 1)))))))))
	(and index
	     (let ((folder (imail-summary-buffer->folder buffer #t)))
	       (and (< index (folder-length folder))
		    (get-message folder index)))))
      (selected-message #f (buffer-get buffer 'IMAIL-FOLDER-BUFFER #f))))

(define (imail-summary-selected-message-index mark)
  (and (imail-summary-match-line mark)
       (- (string->number
	   (extract-string (re-match-start 1) (re-match-end 1)))
	  1)))

(define (imail-summary-match-line mark)
  (re-match-forward
   (string-append
    "[* ][D ][U ][A ][R ][F ] +\\([0-9]+\\)  +\\([0-9.]+[a-zA-Z ]\\)"
    (if (ref-variable imail-summary-show-date mark)
	" \\([ 123][0-9] [a-zA-Z]+  \\| +\\)"
	"  "))
   (line-start mark 0)
   (line-end mark 0)
   #f))

(define (imail-summary-select-message buffer message)
  (highlight-region (buffer-unclipped-region buffer) #f)
  (call-with-values (lambda () (imail-summary-find-message buffer message))
    (lambda (mark approximate?)
      (if mark
	  (begin
	    (set-buffer-point! buffer mark)
	    (if (and (not approximate?)
		     (ref-variable imail-summary-highlight-message buffer))
		(begin
		  (highlight-region
		   (make-region (if (imail-summary-match-line mark)
				    (or (re-match-start 3)
					(re-match-end 0))
				    mark)
				(line-end mark 0))
		   #t)
		  (buffer-not-modified! buffer)))))))
  (if (ref-variable imail-summary-pop-up-message buffer)
      (imail-summary-pop-up-message-buffer buffer)))

(define (imail-summary-pop-up-message-buffer summary-buffer)
  (let ((folder-buffer (buffer-get summary-buffer 'IMAIL-FOLDER-BUFFER #f)))
    (if (and folder-buffer
	     (not (buffer-visible? folder-buffer))
	     (selected-buffer? summary-buffer))
	(pop-up-buffer
	 folder-buffer #f
	 `((HEIGHT ,(imail-summary-height (selected-window))))))))

(define (imail-summary-height window)
  (let ((height (ref-variable imail-summary-height window)))
    (if (exact-integer? height)
	height
	(round->exact (* (window-y-size window) height)))))

(define (imail-summary-find-message buffer message)
  (let ((mark (get-property message 'IMAIL-SUMMARY-MARK #f)))
    (if (and mark
	     (eqv? (imail-summary-selected-message-index mark)
		   (message-index message)))
	(values mark #f)
	(let ((index (message-index message)))
	  (if index
	      (let ((m (imail-summary-first-line buffer)))
		(let ((index* (imail-summary-selected-message-index m)))
		   (cond ((not index*)
			  (values #f #f))
			 ((< index* index)
			  (let loop ((last m))
			    (let ((m (line-start last 1 #f)))
			      (if m
				  (let ((index*
					 (imail-summary-selected-message-index
					  m)))
				     (cond ((or (not index*)
						(> index* index))
					    (values last #t))
					   ((= index index*)
					    (values m #f))
					   (else
					    (loop m))))
				  (values last #t)))))
			 (else
			  (values m (> index* index))))))
	      (values #f #f))))))

(define (imail-summary-first-line buffer)
  (line-start (buffer-start buffer) 2 'LIMIT))

(define (imail-summary-last-line buffer)
  (let ((end (buffer-end buffer)))
    (let ((last (line-start end -1 #f)))
      (if (and last
	       (mark>= last (imail-summary-first-line buffer)))
	  last
	  end))))

;;;; IMAIL Summary mode

(define-major-mode imail-summary imail "IMAIL Summary"
  "Major mode in effect in IMAIL summary buffer.
Each line summarizes a single mail message.
The columns describing the message are, left to right:

1. Several flag characters, each indicating whether the message is
   marked with the corresponding flag.  The characters are, in order,
   `D' (deleted), `U' (unseen), `A' (answered), `R' (re-sent or
   forwarded), and `F' (filed).

2. The message index number.

3. The approximate length of the message in bytes.  Large messages are
   abbreviated using the standard metric suffixes (`k'=1,000,
   `M'=1,000,000, etc.)  The length includes all of the header fields,
   including those that aren't normally shown.  (In IMAP folders, the
   length is slightly higher because the server counts line endings as
   two characters whereas Edwin counts them as one.)

4. The date the message was sent, abbreviated by the day and month.
   The date field is optional; see imail-summary-show-date.

5. The subject line from the message, truncated if it is too long to
   fit in the available space.  The width of the subject area is
   controlled by the variable imail-summary-subject-width.

6. The sender of the message, from the message's `From:' header.

Additional variables controlling this mode:

imail-summary-pop-up-message       keep message buffer visible
imail-summary-highlight-message    highlight line for current message
imail-summary-show-date            show date message sent
imail-summary-subject-width        width of subject field

The commands in this buffer are mostly the same as those for IMAIL
mode (the mode used by the buffer that shows the message contents),
with some additions to make navigation more natural.

\\{imail-summary}"
  (lambda (buffer)
    (buffer-put! buffer 'REVERT-BUFFER-METHOD imail-summary-revert-buffer)
    (remove-kill-buffer-hook buffer imail-kill-buffer)
    (local-set-variable! truncate-lines #t buffer)
    (local-set-variable! mode-line-process
			 imail-summary-mode-line-summary-string
			 buffer)
    (event-distributor/invoke! (ref-variable imail-summary-mode-hook buffer)
			       buffer)))

(define-variable imail-summary-mode-hook
  "An event distributor that is invoked when entering IMAIL Summary mode."
  (make-event-distributor))

(define (imail-summary-revert-buffer buffer dont-use-auto-save? dont-confirm?)
  dont-use-auto-save?
  (if (or dont-confirm? (prompt-for-yes-or-no? "Revert summary buffer"))
      (rebuild-imail-summary-buffer buffer)))

(define (imail-summary-mode-line-summary-string window)
  (let* ((buffer (window-buffer window))
	 (folder (selected-folder #f buffer)))
    (if folder
	(string-append
	 (let ((status (folder-connection-status folder)))
	   (if (eq? status 'NO-SERVER)
	       ""
	       (string-append " " (symbol->string status))))
	 ": "
	 (buffer-get buffer 'IMAIL-SUMMARY-DESCRIPTION "All"))
	"")))

(define-key 'imail-summary #\space	'imail-summary-scroll-msg-up)
(define-key 'imail-summary #\rubout	'imail-summary-scroll-msg-down)
(define-key 'imail-summary #\c-n	'imail-summary-next-message)
(define-key 'imail-summary #\c-p	'imail-summary-previous-message)
(define-key 'imail-summary #\.		'imail-summary-beginning-of-buffer)
(define-key 'imail-summary #\e		'imail-summary-select-message)
(define-key 'imail-summary #\u		'imail-undelete-forward)
(define-key 'imail-summary #\m-<	'imail-summary-first-message)
(define-key 'imail-summary #\m->	'imail-summary-last-message)

(define-key 'imail-summary (make-special-key 'down 0) '(imail-summary . #\c-n))
(define-key 'imail-summary (make-special-key 'up 0) '(imail-summary . #\c-p))

(define-key 'imail-summary button1-down 'imail-summary-mouse-select-message)
(define-key 'imail-summary button4-down '(imail-summary . #\c-p))
(define-key 'imail-summary button5-down '(imail-summary . #\c-n))

(define-command imail-summary-select-message
  "Select the message that point is on and show it in another window."
  ()
  (lambda ()
    (select-message (selected-folder)
		    (or (selected-message #f)
			(editor-error "No message on this line."))
		    #t)
    (imail-summary-pop-up-message-buffer (selected-buffer))))

(define-command imail-summary-mouse-select-message
  "Select the message that mouse is on and show it in another window."
  ()
  (lambda ()
    (let ((button-event (current-button-event)))
      (let ((window (button-event/window button-event)))
	(select-window window)
	(set-current-point!
	 (line-start (or (window-coordinates->mark
			  window
			  (button-event/x button-event)
			  (button-event/y button-event))
			 (buffer-end (window-buffer window)))
		     0))))
    ((ref-command imail-summary-select-message))))

(define-command imail-summary-beginning-of-message
  "Show current message from the beginning."
  ()
  (lambda ()
    (let ((buffer (imail-folder->buffer (selected-folder) #t)))
      (set-buffer-point! buffer (buffer-start buffer))
      (imail-summary-pop-up-message-buffer (selected-buffer)))))

(define-command imail-summary-scroll-msg-up
  "Scroll the IMAIL window forward.
If the IMAIL window is displaying the end of a message,
advance to the next message."
  "P"
  (lambda (argument)
    (if (command-argument-negative-only? argument)
	((ref-command imail-summary-scroll-msg-down) #f)
	(let ((buffer (imail-folder->buffer (selected-folder) #t)))
	  (if (eq? (selected-message #f) (selected-message #f buffer))
	      (let ((windows (buffer-windows buffer)))
		(if (pair? windows)
		    (let ((window (car windows)))
		      (if (window-mark-visible? window (buffer-end buffer))
			  ((ref-command imail-next-message)
			   (if argument
			       (command-argument-numeric-value argument)
			       1))
			  (scroll-window
			   window
			   (standard-scroll-window-argument window
							    argument
							    1))))
		    ((ref-command imail-summary-beginning-of-message))))
	      ((ref-command imail-summary-select-message)))))))

(define-command imail-summary-scroll-msg-down
  "Scroll the IMAIL window backward.
If the IMAIL window is displaying the beginning of a message,
advance to the previous message."
  "P"
  (lambda (argument)
    (if (command-argument-negative-only? argument)
	((ref-command imail-summary-scroll-msg-up) #f)
	(let ((buffer (imail-folder->buffer (selected-folder) #t)))
	  (if (eq? (selected-message #f) (selected-message #f buffer))
	      (let ((windows (buffer-windows buffer)))
		(if (pair? windows)
		    (let ((window (car windows)))
		      (if (window-mark-visible? window (buffer-start buffer))
			  ((ref-command imail-previous-message)
			   (if argument
			       (command-argument-numeric-value argument)
			       1))
			  (scroll-window
			   window
			   (standard-scroll-window-argument window
							    argument
							    -1))))
		    ((ref-command imail-summary-beginning-of-message))))
	      ((ref-command imail-summary-select-message)))))))

(define-command imail-summary-next-message
  (lambda ()
    (command-description
     (if (ref-variable imail-summary-auto-select)
	 (ref-command-object imail-next-message)
	 (ref-command-object next-line))))
  "p"
  (lambda (delta)
    ((if (ref-variable imail-summary-auto-select)
	 (ref-command imail-next-message)
	 (ref-command next-line))
     delta)))

(define-command imail-summary-previous-message
  (lambda ()
    (command-description
     (if (ref-variable imail-summary-auto-select)
	 (ref-command-object imail-previous-message)
	 (ref-command-object previous-line))))
  "p"
  (lambda (delta)
    ((if (ref-variable imail-summary-auto-select)
	 (ref-command imail-previous-message)
	 (ref-command previous-line))
     delta)))

(define-command imail-summary-first-message
  (lambda ()
    (command-description
     (if (ref-variable imail-summary-auto-select)
	 (ref-command-object imail-first-message)
	 (ref-command-object beginning-of-buffer))))
  ()
  (lambda ()
    (if (ref-variable imail-summary-auto-select)
	((ref-command imail-first-message))
	((ref-command beginning-of-buffer) #f))))

(define-command imail-summary-last-message
  (lambda ()
    (command-description
     (if (ref-variable imail-summary-auto-select)
	 (ref-command-object imail-last-message)
	 (ref-command-object end-of-buffer))))
  ()
  (lambda ()
    (if (ref-variable imail-summary-auto-select)
	((ref-command imail-last-message))
	((ref-command end-of-buffer) #f))))