;;; -*-Scheme-*-
;;;
;;; $Id: imail-summary.scm,v 1.11 2000/05/19 17:50:30 cph Exp $
;;;
;;; Copyright (c) 2000 Massachusetts Institute of Technology
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

;;;; IMAIL mail reader: summary buffer

(declare (usual-integrations))

(define-variable imail-summary-pop-up-message
  "If true, selecting a message in the IMAIL summary buffer pops up the
 message buffer in a separate window.
If false, the message buffer is updated but not popped up."
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

(define-command imail-summary
  "Display a summary of the selected folder, one line per message."
  ()
  (lambda () (imail-summary "All" #f)))

(define-command imail-summary-by-flags
  "Display a summary of the selected folder, one line per message.
Only messages marked with one of the given flags are shown.
The flags are specified as a comma-separated list of names."
  (lambda ()
    (list (imail-prompt-for-flags "Flags to summarize by")))
  (lambda (flags-string)
    (imail-summary (string-append "Flags " flags-string)
		   (let ((flags (parse-comma-list-string flags-string)))
		     (lambda (m)
		       (there-exists? (message-flags m)
			 (lambda (flag)
			   (flags-member? flag flags))))))))

(define-command imail-summary-by-recipients
  "Display a summary of the selected folder, one line per message.
Only messages addressed to one of the given recipients are shown.
Normally checks the To, From and CC fields of headers;
 but if prefix arg given, only look in the To and From fields.
The recipients are specified as a comma-separated list of names."
  "sRecipients to summarize by\nP"
  (lambda (recipients-string primary-only?)
    (imail-summary
     (string-append "Recipients " recipients-string)
     (let ((regexp
	    (apply regexp-group
		   (map re-quote-string
			(parse-comma-list-string recipients-string)))))
       (let ((try
	      (lambda (s)
		(and s
		     (re-string-search-forward regexp s #t)))))
	 (lambda (m)
	   (or (try (get-first-header-field-value m "from" #f))
	       (try (get-first-header-field-value m "to" #f))
	       (and (not primary-only?)
		    (try (get-first-header-field-value m "cc" #f))))))))))

(define (parse-comma-list-string string)
  (list-transform-negative (map string-trim (burst-string string #\, #f))
    string-null?))

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
		     (add-event-receiver! (folder-modification-event folder)
					  imail-summary-modification-event)
		     (buffer-put! folder-buffer 'IMAIL-SUMMARY-BUFFER buffer)
		     (associate-buffer-with-imail-buffer folder-buffer buffer)
		     (buffer-put! buffer 'IMAIL-MESSAGE-METHOD
				  imail-summary-selected-message)))
		  buffer)))))
    (buffer-put! buffer 'IMAIL-SUMMARY-DESCRIPTION description)
    (buffer-put! buffer 'IMAIL-SUMMARY-PREDICATE predicate)
    (rebuild-imail-summary-buffer buffer)
    (if (not (selected-buffer? buffer))
	(let ((windows (buffer-windows buffer)))
	  (if (pair? windows)
	      (select-window (car windows))
	      (select-buffer buffer))))))

(define (imail-summary-detach buffer)
  (let ((folder-buffer (buffer-get buffer 'IMAIL-FOLDER-BUFFER #f)))
    (if folder-buffer
	(begin
	  (buffer-remove! folder-buffer 'IMAIL-SUMMARY-BUFFER)
	  (let ((folder (buffer-get folder-buffer 'IMAIL-FOLDER #f)))
	    (if folder
		(add-event-receiver! (folder-modification-event folder)
				     imail-summary-modification-event)))))))

(define (imail-summary-modification-event folder type parameters)
  (let ((buffer (imail-folder->summary-buffer folder #f)))
    (if buffer
	(case type
	  ((FLAGS)
	   (let ((mark (imail-summary-message-mark buffer (car parameters))))
	     (if mark
		 (begin
		   (with-read-only-defeated mark
		     (lambda ()
		       (group-replace-string!
			(mark-group mark)
			(mark-index mark)
			(message-flag-markers (car parameters)))))
		   (buffer-not-modified! buffer)))))
	  ((SELECT-MESSAGE)
	   (imail-summary-select-message buffer (car parameters)))
	  ((EXPUNGE INCREASE-LENGTH SET-LENGTH)
	   (maybe-add-command-suffix! rebuild-imail-summary-buffer buffer))))))

(define (imail-folder->summary-buffer folder error?)
  (let ((buffer (imail-folder->buffer folder error?)))
    (and buffer
	 (or (buffer-get buffer 'IMAIL-SUMMARY-BUFFER #f)
	     (and error?
		  (error:bad-range-argument folder
					    'IMAIL-FOLDER->SUMMARY-BUFFER))))))

;;;; Navigation

(define (imail-summary-selected-message buffer)
  (let ((folder (selected-folder #f buffer))
	(start (imail-summary-first-line buffer))
	(here (line-start (buffer-point buffer) 0)))
    (and folder
	 (mark<= start here)
	 (let ((index (count-lines start here)))
	   (and (< index (folder-length folder))
		(get-message folder index))))))

(define (imail-summary-select-message buffer message)
  (highlight-region (buffer-unclipped-region buffer) #f)
  (let ((mark (imail-summary-message-mark buffer message)))
    (if mark
	(begin
	  (set-buffer-point! buffer mark)
	  (if (ref-variable imail-summary-highlight-message buffer)
	      (begin
		(highlight-region (make-region mark (line-start mark 1 'LIMIT))
				  #t))))))
  (if (ref-variable imail-summary-pop-up-message buffer)
      (imail-summary-pop-up-message-buffer buffer)))

(define (imail-summary-message-mark buffer message)
  (let ((index (message-index message)))
    (and index
	 (line-start (imail-summary-first-line buffer) index))))

(define (imail-summary-pop-up-message-buffer buffer)
  (let ((folder-buffer (buffer-get buffer 'IMAIL-FOLDER-BUFFER #f)))
    (if (and folder-buffer (selected-buffer? buffer))
	(pop-up-buffer folder-buffer))))

(define (imail-summary-first-line buffer)
  (line-start (buffer-start buffer) 2 'LIMIT))

;;;; Summary content generation

(define (rebuild-imail-summary-buffer buffer)
  (buffer-widen! buffer)
  (with-read-only-defeated (buffer-start buffer)
    (lambda ()
      (region-delete! (buffer-region buffer))
      (fill-imail-summary-buffer! buffer
				  (selected-folder #f buffer)
				  (buffer-get buffer
					      'IMAIL-SUMMARY-PREDICATE
					      #f))))
  (set-buffer-major-mode! buffer (ref-mode-object imail-summary))
  (buffer-not-modified! buffer)
  (set-buffer-point! buffer (imail-summary-first-line buffer))
  (let ((message
	 (selected-message #f (buffer-get buffer 'IMAIL-FOLDER-BUFFER))))
    (if message
	(imail-summary-select-message buffer message))))

(define (fill-imail-summary-buffer! buffer folder predicate)
  (let ((end (folder-length folder)))
    (let ((messages
	   (let loop ((i 0) (messages '()))
	     (if (< i end)
		 (loop (+ i 1) (cons (get-message folder i) messages))
		 (reverse! messages))))
	  (index-digits
	   (let loop ((n 1) (k 10))
	     (if (< end k)
		 n
		 (loop (+ n 1) (* k 10)))))
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

;;;; IMAIL Summary mode

(define-major-mode imail-summary imail "IMAIL Summary"
  "Major mode in effect in IMAIL summary buffer.
Each line summarizes a single mail message.
The columns describing the message are, left to right:

1. Several flag characters, each indicating whether the message is
   marked with the corresponding flag.  The characters are, in order,
   `D' (deleted), `U' (not seen), `A' (answered), `R' (resent or
   forwarded), and `F' (filed).

2. The message index number.

3. The approximate length of the message in bytes.  Large messages are
   abbreviated using the standard metric suffixes (`k'=1,000,
   `M'=1,000,000, etc.)  The length includes all of the header fields,
   including those that aren't normally shown.  (In IMAP folders, the
   length is slightly higher because it counts line endings as two
   characters whereas Edwin counts them as one.)

4. The date the message was sent, abbreviated by the day and month.

5. The subject line from the message, truncated if it is too long to
   fit in the available space.

6. The sender of the message, from the message's `From:' header.

The commands in this buffer are mostly the same as those for IMAIL
mode (the mode used by the buffer that shows the message contents),
with some additions to make navigation more natural.

\\{imail-summary}"
  (lambda (buffer)
    (buffer-put! buffer 'REVERT-BUFFER-METHOD imail-summary-revert-buffer)
    (remove-kill-buffer-hook buffer imail-kill-buffer)
    (local-set-variable! truncate-lines #t buffer)
    (local-set-variable! mode-line-process
			 (list ": "
			       (buffer-get buffer
					   'IMAIL-SUMMARY-DESCRIPTION
					   "All"))
			 buffer)
    (event-distributor/invoke! (ref-variable imail-summary-mode-hook buffer)
			       buffer)))

(define-variable imail-summary-mode-hook
  "An event distributor that is invoked when entering IMAIL Summary mode."
  (make-event-distributor))

(define (imail-summary-revert-buffer buffer dont-use-auto-save? dont-confirm?)
  dont-use-auto-save? dont-confirm?
  (if (or dont-confirm?
	  (prompt-for-yes-or-no? "Revert summary buffer"))
      (rebuild-imail-summary-buffer buffer)))

(define-key 'imail-summary #\space	'imail-summary-select-message)
(define-key 'imail-summary #\rubout	'imail-undelete-previous-message)
(define-key 'imail-summary #\c-n	'imail-summary-next-message)
(define-key 'imail-summary #\c-p	'imail-summary-previous-message)
(define-key 'imail-summary #\.		'undefined)
(define-key 'imail-summary #\q		'imail-summary-quit)
(define-key 'imail-summary #\u		'imail-summary-undelete-forward)
(define-key 'imail-summary #\m-<	'imail-select-message)
(define-key 'imail-summary #\m->	'imail-last-message)

(define-command imail-summary-select-message
  "Select the message that point is on and show it in another window."
  ()
  (lambda ()
    (select-message (selected-folder) (selected-message) #t)
    (imail-summary-pop-up-message-buffer (selected-buffer))))

(define-command imail-summary-next-message
  "Show following message whether deleted or not.
With prefix argument N, moves forward N messages,
or backward if N is negative."
  "p"
  (lambda (delta)
    (if (selected-message #f)
	((ref-command imail-next-message) delta)
	(begin
	  ((ref-command next-line) delta)
	  (let ((message (selected-message #f)))
	    (if message
		(select-message (selected-folder) message)))))))

(define-command imail-summary-previous-message
  "Show previous message whether deleted or not.
With prefix argument N, moves backward N messages,
or forward if N is negative."
  "p"
  (lambda (delta)
    ((ref-command imail-summary-next-message) (- delta))))

(define-command imail-summary-undelete-forward
  "Undelete following message whether deleted or not.
With prefix argument N, undeletes forward N messages,
or backward if N is negative."
  "p"
  (lambda (delta)
    (move-relative delta #f "message" undelete-message)))

(define-command imail-summary-quit
  "Quit out of IMAIL."
  ()
  (lambda ()
    (let ((folder-buffer
	   (buffer-get (selected-buffer) 'IMAIL-FOLDER-BUFFER #f)))
      (if folder-buffer
	  (for-each window-delete! (buffer-windows folder-buffer))))
    ((ref-command imail-quit))
    ((ref-command bury-buffer))))