;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/rmailsum.scm,v 1.4 1991/08/06 22:58:51 bal Exp $
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

;;;; RMAIL Summary Mode

(declare (usual-integrations))

(define rmail-buffer false)

(define rmail-summary-buffer false)

(define rmail-summary-vector false)

;;; (define-variable rmail-last-multi-labels
;;;   ""
;;;   ""
;;;   list-of-strings?)

(define-command rmail-summary
  "Display a summary of all messages, one line per message."
  '()
  (lambda ()
    (rmail-new-summary "All" false)))

;;;(define rmail-summary-by-labels
;;;   "Display a summary of all messages with one or more LABELS.
;;; LABELS should be a string containing the desired labels, separated by commas."
;;;   "sLabels to summarize by: "
;;;   (lambda (labels)
;;;     (if (string=? labels "")
;;; 	(set! labels (or rmail-last-multi-labels
;;; 			 (error "No label specified"))))
;;;     (set! rmail-last-multi-labels labels)
;;;     (rmail-new-summary (string-append "labels " labels)
;;; 		       rmail-message-labels?
;;; 		       (string-append ", \\(" (mail-comma-list-regexp labels) "\\),"))))
;;; 
;;; (define rmail-summary-by-recipients 
;;;   "Display a summary of all messages with the given RECIPIENTS.
;;; Normally checks the To, From and Cc fields of headers;
;;; but if PRIMARY-ONLY is non-nil (prefix arg given),
;;;  only look in the To and From fields.
;;; RECIPIENTS is a string of names separated by commas."
;;;   (interactive "sRecipients to summarize by: \nP")
;;;   (lambda (recipients primary-only)
;;;     (rmail-new-summary
;;;      (string-append "recipients " recipients)
;;;      rmail-message-recipients?
;;;      (mail-comma-list-regexp recipients) primary-only)))
;;; 
;;; ***HERE***
;;; (define (rmail-message-recipients? msg recipients primary-only)
;;;   (let ((the-current-point (current-point)))
;;;     (set
;;;   (goto-char (rmail-msgbeg msg))
;;;     (search-forward "\n*** EOOH ***\n")
;;;     (narrow-to-region (point) (progn (search-forward "\n\n") (point)))
;;;     (or (string-match recipients (or (mail-fetch-field "To") ""))
;;; 	(string-match recipients (or (mail-fetch-field "From") ""))
;;; 	(if (not primary-only)
;;; 	    (string-match recipients (or (mail-fetch-field "Cc") ""))))))

(define rmail-new-summary 
  (lambda (description function . args)
    (guarantee-rmail-summary-variables)
    (message "Computing summary lines...")
    (if (not rmail-summary-buffer)
	(set! rmail-summary-buffer
	      (temporary-buffer (string-append (buffer-name (current-buffer)) "-summary"))))
    (let ((summary-msgs ())
	  (the-current-message-number (msg-memo/number (current-msg-memo)))
	  (new-summary-line-count 0))
      (let loop ((the-memo (msg-memo/first (current-msg-memo))))
	(let ((next-memo (msg-memo/next the-memo)))
	  (if (or (not function)
		  (apply function (cons the-memo args)))
	      (set! summary-msgs
		    (cons (rmail-make-summary-line the-memo)
			  summary-msgs)))
	  (if next-memo
	      (loop next-memo))))
      (select-buffer-other-window rmail-summary-buffer)
      (set-buffer-writeable! (current-buffer))
      (set-current-point! (buffer-start (current-buffer)))
      (kill-string (buffer-start (current-buffer))
		   (buffer-end (current-buffer)))
      (let loop ((the-summary-list (reverse summary-msgs)))
	(if (not (null? the-summary-list))
	    (begin
	      (insert-string (car the-summary-list))
	      (loop (cdr the-summary-list)))))
      (set-buffer-read-only! (current-buffer))
      (set-current-point! (buffer-start (current-buffer)))
      (set-current-major-mode! (ref-mode-object rmail-summary))
;      ((ref-command make-local-variable) 'minor-mode-alist)
;      (set-variable! minor-mode-alist (list ": " description))
      (set-current-point! 
       (line-start
	(re-search-forward 
	 (string-append "^[ ]*" (number->string the-current-message-number))
	 (buffer-start (current-buffer))
	 (buffer-end (current-buffer)))
	0))
      (rmail-summary-goto-message-current-line)
      (message "Computing summary lines...done"))))

(define (rmail-make-summary-line memo)
  (let ((new-summary-line-count 0))
    (let ((line (or (vector-ref rmail-summary-vector (-1+ (msg-memo/number memo)))
		    (begin
		      (set! new-summary-line-count
			    (1+ new-summary-line-count))
		      (if (= 0 (modulo new-summary-line-count 10))
			  (message "Computing summary lines..."
				   new-summary-line-count))
		      (rmail-make-summary-line-1 memo)
		      (vector-ref rmail-summary-vector (-1+ (msg-memo/number memo)))
		      ))))
      ;; Fix up the part of the summary that says "deleted" or "unseen".
      (string-set! line 4
		   (if (msg-memo/deleted? memo) #\D
		       (if (char=? #\0 (string-ref (extract-string (msg-memo/start memo)
								   (msg-memo/end memo))
						   2))
			   #\- #\space)))
      line)))

(define (rmail-make-summary-line-1 memo)
  (with-buffer-open 
   (current-buffer)
   (lambda ()
     (let ((old-point (current-point))
	   (start (msg-memo/start memo))
	   (end (msg-memo/end memo)))
       (let ((lim
	      (begin
		(set-current-point! start)
		((ref-command next-line) 2)
		(current-point)))
	     (pos)
	     (labels
	      (begin
		(set-current-point! start)
		(move-thing mark+ 3)
		(if (and (search-forward ",," start end)
			 (line-end? (current-point)))
		    (let ((point (current-point)))
		      (string-append 
		       "{"
		       (extract-string point (line-end point 0))
		       "} "))
		    "")))
	     (line
	      (begin
		(set-current-point! start)
		((ref-command next-line) 1)
		(let ((point (current-point)))
		  (if (string-prefix? 
		       "Summary-line: "
		       (extract-string point (line-end point 0)))
		      (begin
			(string-tail (extract-string point
						     (begin
						       ((ref-command next-line) 1)
						       (current-point)))
				     14))
		      false)))))
	 ;; If we didn't get a valid status line from the message,
	 ;; make a new one and put it in the message.
	 (or line
	     (let ((inner-begin
		    (let ((foo (search-forward "\n*** EOOH ***\n" start end)))
		      (if foo
			  foo
			  (begin
			    ((ref-command next-line) 1)
			    (current-point))))))
	       (set! line (rmail-make-basic-summary-line inner-begin))
	       (insert-string (string-append "Summary-line: " line)
			      (line-start start 2))))
	 (set! pos (string-find-next-char line #\#))
	 (let ((num (msg-memo/number memo)))
	   (vector-set! rmail-summary-vector (-1+ num)
			(string-append
			 (string-pad-left (number->string num) 4)
			 "  "
			 (string-head line pos)
			 labels
			 (string-tail line (1+ pos))))))
       (set-current-point! old-point)))))

(define (rmail-make-basic-summary-line the-begin)
  (string-append
   (let ((the-mark
	  (re-search-forward "^Date:" the-begin (group-end the-begin))))
     (if (not the-mark)
	 "      "
	 (let ((the-end-of-line (line-end the-mark 0)))
	   (cond
	    ((re-search-forward "\\([^0-9:]\\)\\([0-3]?[0-9]\\)\\([- \t_]+\\)\\([adfjmnos][aceopu][bcglnprtvy]\\)" the-mark the-end-of-line)
	     (string-append
	      (string-pad-left (extract-string (re-match-start 2) (re-match-end 2)) 2)
	      "-"
	      (extract-string (re-match-start 4) (re-match-end 4))))
	    ((re-search-forward "\\([^a-z]\\)\\([adfjmnos][acepou][bcglnprtvy]\\)\\([-a-z \t_]*\\)\\([0-9][0-9]?\\)" the-mark the-end-of-line)
	     (string-append
	      (string-pad-left (extract-string (re-match-start 4) (re-match-end 4)) 2)
	      "-"
	      (extract-string (re-match-start 2) (re-match-end 2))))
	    (else
	     "??????")))))
   "  "
   (let ((the-mark
	  (re-search-forward "^From:[ \t]*" the-begin (group-end the-begin))))
     (if (not the-mark)
	 "                         "
	 (let* ((from
		 (mail-extract-real-name
		  (skip-chars-forward " \t" the-mark)
		  (skip-chars-backward " " (line-end the-mark 0))))
		(len (string-length from))
		(mch (string-find-next-char-in-set from (char-set #\@ #\%))))
	   (substring
	    (string-append
	     (if (or (not mch) (<= len 25))
		 (string-tail from (max 0 (- len 25)))
		 (let ((lo
			(cond ((< (- mch 9) 0) 0)
			      ((< len (+ mch 16))
			       (- len 25))
			      (else
			       (- mch 9)))))
		   (substring from lo (min len (+ lo 25)))))
	     "                         ")
	    0 25))))
   "  #"
   (let ((the-mark
	  (re-search-forward "^Subject:" the-begin (group-end the-begin))))
     (if the-mark
	 (let ((the-start (skip-chars-forward " \t" the-mark)))
	   (extract-string the-start (line-end the-start 0)))
	 (let ((the-start (re-search-forward "[\n][\n]+" the-begin (group-end the-begin))))
	   (extract-string the-start (line-end the-start 0)))))
   "\n"))

(define (mail-extract-real-name address-start address-end)
  (cond ((re-search-forward "[ \t\"]*\\<\\(.*\\)\\>[\" \t]*<.*>" address-start address-end)
	 (extract-string (re-match-start 1) (re-match-end 1)))
	;; Chris VanHaren (Athena User Consultant) <vanharen>
	((re-search-forward "[ \t\"]*\\<\\(.*\\)\\>.*(.*).*<.*>.*" address-start address-end)
	 (extract-string (re-match-start 1) (re-match-end 1)))
	((re-search-forward ".*(\\(.*\\))" address-start address-end)
	 (extract-string (re-match-start 1) (re-match-end 1)))
	((re-search-forward ".*<\\(.*\\)>.*" address-start address-end)
	 (extract-string (re-match-start 1) (re-match-end 1)))
	((re-search-forward " *\\<\\(.*\\)\\> *" address-start address-end)
	 (extract-string (re-match-start 1) (re-match-end 1)))
	(else
	 address)))

(define-major-mode rmail-summary read-only "RMAIL Summary"
  "Major mode in effect in Rmail summary buffer.
A subset of the Rmail mode commands are supported in this mode. 
As commands are issued in the summary buffer the corresponding
mail message is displayed in the rmail buffer.

n       Move to next undeleted message, or arg messages.
p       Move to previous undeleted message, or arg messages.
C-n	Move to next, or forward arg messages.
C-p	Move to previous, or previous arg messages.
j       Jump to the message at the cursor location.
d       Delete the message at the cursor location and move to next message.
u	Undelete this or previous deleted message.
q	Quit Rmail.
x	Exit and kill the summary window.
space   Scroll message in other window forward.
delete  Scroll message backward.

Entering this mode calls value of hook variable rmail-summary-mode-hook."
  (let ((buffer (current-buffer)))
    (set-buffer-read-only! buffer))
  (event-distributor/invoke! (ref-variable rmail-summary-mode-hook)))

(define (guarantee-rmail-summary-variables)
  (let ((number-of-messages (msg-memo/number (msg-memo/last (current-msg-memo)))))
    (set! rmail-buffer (current-buffer))
    (set! rmail-summary-vector (make-vector number-of-messages #F))))

(define-key 'rmail-summary #\j		'rmail-summary-show-message)
(define-key 'rmail-summary #\n		'rmail-summary-next-undeleted-message)
(define-key 'rmail-summary #\p		'rmail-summary-previous-undeleted-message)
(define-key 'rmail-summary #\m-n	'rmail-summary-next-message)
(define-key 'rmail-summary #\m-p	'rmail-summary-previous-message)
(define-key 'rmail-summary #\c-m-n	'rmail-summary-next-labeled-message)
(define-key 'rmail-summary #\c-m-p	'rmail-summary-previous-labeled-message)
(define-key 'rmail-summary #\space	'rmail-summary-scroll-message-up)
(define-key 'rmail-summary #\rubout	'rmail-summary-scroll-message-down)
(define-key 'rmail-summary #\d		'rmail-summary-delete-message-forward)
(define-key 'rmail-summary #\D    	'rmail-summary-delete-message-backward)
(define-key 'rmail-summary #\M-d        'rmail-summary-delete-message)
(define-key 'rmail-summary #\u		'rmail-summary-undelete-message-backward)
(define-key 'rmail-summary #\U   	'rmail-summary-undelete-message-forward)
(define-key 'rmail-summary #\M-u	'rmail-summary-undelete-message)
(define-key 'rmail-summary #\q		'rmail-summary-quit)
(define-key 'rmail-summary #\x		'rmail-summary-exit)

;;; (define-key 'rmail #\.		'beginning-of-buffer)
;;; (define-key 'rmail #\a		'rmail-add-label)
;;; (define-key 'rmail #\k		'rmail-kill-label)
;;; (define-key 'rmail #\e		'rmail-expunge)
;;; (define-key 'rmail #\x		'rmail-expunge)
;;; (define-key 'rmail #\s		'rmail-expunge-and-save)
;;; (define-key 'rmail #\g		'rmail-get-new-mail)
;;; (define-key 'rmail #\c-m-h	'rmail-summary)
;;; (define-key 'rmail #\l		'rmail-summary-by-labels)
;;; (define-key 'rmail #\c-m-l	'rmail-summary-by-labels)
;;; (define-key 'rmail #\c-m-r	'rmail-summary-by-recipients)
;;; (define-key 'rmail #\t		'rmail-toggle-header)
;;; (define-key 'rmail #\m		'rmail-mail)
;;; (define-key 'rmail #\r		'rmail-reply)
;;; (define-key 'rmail #\c		'rmail-continue)
;;; (define-key 'rmail #\f		'rmail-forward)
;;; (define-key 'rmail #\m-s	'rmail-search)
;;; (define-key 'rmail #\o		'rmail-output-to-rmail-file)
;;; (define-key 'rmail #\c-o	'rmail-output)
;;; (define-key 'rmail #\i		'rmail-input)
;;; (define-key 'rmail #\q		'rmail-quit)
;;; (define-key 'rmail #\>		'rmail-last-message)
;;; (define-key 'rmail #\?		'describe-mode)
;;; (define-key 'rmail #\w		'rmail-edit-current-message)

(define (make-rmail-summary-handler-prefix-arg key)
  (lambda (arg)
    (select-buffer-other-window rmail-buffer)
    ((command-procedure (comtab-entry (mode-comtabs (current-major-mode)) key)) arg)
    (select-buffer-other-window rmail-summary-buffer)))

(define-command rmail-summary-show-message
  ""
  "P"
  (lambda (arg)
    (if arg
	(let ((the-new-mark
	       (re-search-forward 
		(string-append "^[ ]*" (number->string arg))
		(buffer-start (current-buffer))
		(buffer-end (current-buffer)))))
	  (if the-new-mark
	      (begin
		(set-current-point! (line-start the-new-mark 0))
		(rmail-summary-goto-message-current-line))
	      (message (string-append "Message "
				      (number->string arg)
				      " not found."))))
	(rmail-summary-goto-message-current-line))))

(define (rmail-summary-goto-message-current-line)
  (let ((start (line-start (current-point) 0)))
    (let ((end (mark+ start 4)))
      (if end
	  (let ((the-message-number
		 (string->number (string-trim (extract-string start end)))))
	    (if (not (null? the-message-number))
		(begin
		  (select-buffer-other-window rmail-buffer)
		  ((command-procedure (comtab-entry (mode-comtabs (current-major-mode)) #\j)) the-message-number)
		  (select-buffer-other-window rmail-summary-buffer))))))))

(define-command rmail-summary-next-message
  "Goto ARGth previous message."
  "p"
  (lambda (arg)
    (set-current-point! (line-start (current-point) arg))
    (rmail-summary-goto-message-current-line)))

(define-command rmail-summary-previous-message
  "Goto ARGth next message."
  "p"
  (lambda (arg)
    (set-current-point! (line-start (current-point) (- arg)))
    (rmail-summary-goto-message-current-line)))

(define-command rmail-summary-next-undeleted-message 
  "Goto ARGth next undeleted message."
  "p"
  (lambda (arg)
    (let ((the-buf-end (buffer-end (current-buffer))))
      (let loop ((count arg)
		 (the-mark (line-end (current-point) 0)))
	(if (> count 0)
	    (let ((the-new-mark
		   (re-search-forward "^....[^D]" the-mark the-buf-end)))
	      (if the-new-mark
		  (loop (-1+ count) the-new-mark)
		  (begin
		    (set-current-point! (line-start the-mark 0))
		    (rmail-summary-goto-message-current-line))))
	    (begin
	      (set-current-point! (line-start the-mark 0))
	      (rmail-summary-goto-message-current-line)))))))

(define-command rmail-summary-previous-undeleted-message 
  "Goto ARGth previous undeleted message."
  "p"
  (lambda (arg)
    (let ((the-buf-start (buffer-start (current-buffer))))
      (let loop ((count arg)
		 (the-mark (line-start (current-point) 0)))
	(if (> count 0)
	    (let ((the-new-mark
		   (re-search-backward "^....[^D]" the-mark the-buf-start)))
	      (if the-new-mark
		  (loop (-1+ count) the-new-mark)
		  (begin
		    (set-current-point! (line-start the-mark 0))
		    (rmail-summary-goto-message-current-line))))
	    (begin
	      (set-current-point! (line-start the-mark 0))
	      (rmail-summary-goto-message-current-line)))))))

(define-command rmail-summary-scroll-message-up
  "Scroll RMAIL window up."
  "P"
  (lambda (arg)
    (select-buffer-other-window rmail-buffer)
    (let ((window (current-window)))
      (scroll-window window
		     (standard-scroll-window-argument window arg 1)
		     (lambda () true)))
    (select-buffer-other-window rmail-summary-buffer)))
 
(define-command rmail-summary-scroll-message-down
  "Scroll RMAIL window down."
  "P"
  (lambda (arg)
    (select-buffer-other-window rmail-buffer)
    (let ((window (current-window)))
      (scroll-window window
		     (standard-scroll-window-argument window arg -1)
		     (lambda () true)))
    (select-buffer-other-window rmail-summary-buffer)))

(define-command rmail-summary-delete-message
  "Delete this message and stay on it."
  '()
  (lambda ()
    (let ((the-memo (buffer-msg-memo rmail-buffer)))
      (set-attribute! the-memo 'DELETED))
    (let ((the-mark1
	   (skip-chars-forward " " (line-start (current-point) 0))))
      (let ((the-mark
	     (skip-chars-forward "[0-9]" the-mark1)))
	(set-buffer-writeable! (current-buffer))
	(delete-string the-mark (mark1+ the-mark))
	(insert-string "D" the-mark)
	(set-buffer-read-only! (current-buffer))))))

(define-command rmail-summary-delete-message-forward
  "Delete this message and move to next undeleted message."
  '()
  (lambda ()
    ((ref-command rmail-summary-delete-message))
    ((ref-command rmail-summary-next-undeleted-message) 1)))

(define-command rmail-summary-delete-message-backward
  "Delete this message and move to previous undeleted message."
  '()
  (lambda ()
    ((ref-command rmail-summary-delete-message))
    ((ref-command rmail-summary-previous-undeleted-message) 1)))
  
(define-command rmail-summary-undelete-message
  "Undelete this message and stay here."
  '()
  (lambda ()
    (let ((the-memo (buffer-msg-memo rmail-buffer)))
      (if (msg-memo/deleted? the-memo)
	  (clear-attribute! the-memo 'DELETED))
      (let ((the-mark1
	     (skip-chars-forward " " (line-start (current-point) 0))))
	(let ((the-mark
	       (skip-chars-forward "[0-9]" the-mark1)))
	  (set-buffer-writeable! (current-buffer))
	  (delete-string the-mark (mark1+ the-mark))
	  (insert-string " " the-mark)
	  (set-buffer-read-only! (current-buffer)))))))

(define-command rmail-summary-undelete-message-backward
  "Search backwards from current message for first deleted message,
and undelete it."
  '()
  (lambda ()
    (let ((the-mark
	   (re-search-backward "^....D" (line-end (current-point) 0) (buffer-start (current-buffer)))))
      (if the-mark
	  (begin
	    (set-current-point! (line-start the-mark 0))
	    (rmail-summary-goto-message-current-line)
	    ((ref-command rmail-summary-undelete-message)))))))

(define-command rmail-summary-undelete-message-forward
  "Search forward from current message for first deleted message,
and undelete it."
  '()
  (lambda ()
    (let ((the-mark
	   (re-search-forward "^....D" (line-start (current-point) 0) (buffer-end (current-buffer)))))
      (if the-mark
	  (begin
	    (set-current-point! (line-start the-mark 0))
	    (rmail-summary-goto-message-current-line)
	    ((ref-command rmail-summary-undelete-message)))))))

(define-command rmail-summary-exit
  "Exit RMAIL Summary mode, remaining within RMAIL."
  '()
  (lambda ()
    (bury-buffer (current-buffer))
    ((ref-command delete-window))))

(define-command rmail-summary-quit
  "Exit RMAIL Summary mode and RMAIL mode."
  '()
  (lambda ()
    ((ref-command rmail-summary-exit))
    ((ref-command rmail-quit))))
