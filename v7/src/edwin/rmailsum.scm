;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/rmailsum.scm,v 1.1 1991/08/05 16:39:45 bal Exp $
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

(define rmail-summary-buffer false)

(define-variable rmail-last-multi-labels
  ""
  ""
  list-of-strings?)

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
    (message "Computing summary lines...")
    (if (not rmail-summary-buffer)
	(set! rmail-summary-buffer
	      (temporary-buffer (string-append (buffer-name (current-buffer)) "-summary"))))
    (let ((summary-msgs ())
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
      (select-buffer rmail-summary-buffer)
      (set-buffer-writeable! (current-buffer))
      (set-current-point! (buffer-start (current-buffer)))
      (kill-string (buffer-start (current-buffer))
		   (buffer-end (current-buffer)))
      (let loop ((the-summary-list (reverse summary-msgs)))
	(if (not (null? the-summary-list))
	    (begin
	      (insert-string (car the-summary-list))
	      (loop (cdr the-summary-list)))))
;;;	       (subst-char-in-region 1 2 ?\( ?\ )
      (set-buffer-read-only! (current-buffer))
      (set-current-point! (buffer-start (current-buffer)))) 
;      (rmail-summary-mode)
;      ((ref-command make-local-variable) 'minor-mode-alist)
;      (set-variable! minor-mode-alist (list ": " description))
;      (rmail-summary-goto-msg mesg true)
      (message "Computing summary lines...done")))

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
		 (mail-strip-quoted-names
		  (extract-string
		   the-mark
		   (skip-chars-backward " \t" (line-end the-mark 0)))))
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

(defun rmail-summary-next-all (&optional number)
  (interactive "p")
  (forward-line (if number number 1))
  (rmail-summary-goto-msg))

(defun rmail-summary-previous-all (&optional number)
  (interactive "p")
  (forward-line (- (if number number 1)))
  (rmail-summary-goto-msg))

(defun rmail-summary-next-msg (&optional number)
  (interactive "p")
  (forward-line 0)
  (and (> number 0) (forward-line 1))
  (let ((count (if (< number 0) (- number) number))
	(search (if (> number 0) 're-search-forward 're-search-backward))
	end)
    (while (and (> count 0) (funcall search "^.....[^D]" nil t))
      (setq count (1- count)))
    (rmail-summary-goto-msg)))

(defun rmail-summary-previous-msg (&optional number)
  (interactive "p")
  (rmail-summary-next-msg (- (if number number 1))))

(defun rmail-summary-delete-forward ()
  (interactive)
  (let (end)
    (rmail-summary-goto-msg)
    (pop-to-buffer rmail-buffer)
    (rmail-delete-message)
    (pop-to-buffer rmail-summary-buffer)
    (let ((buffer-read-only nil))
      (skip-chars-forward " ")
      (skip-chars-forward "[0-9]")
      (delete-char 1)
      (insert "D"))
    (rmail-summary-next-msg 1)))

(defun rmail-summary-undelete ()
  (interactive)
  (let ((buffer-read-only nil))
    (end-of-line)
    (cond ((re-search-backward "\\(^ *[0-9]*\\)\\(D\\)" nil t)
	   (replace-match "\\1 ")
	   (rmail-summary-goto-msg)
	   (pop-to-buffer rmail-buffer)
	   (and (rmail-message-deleted-p rmail-current-message)
		(rmail-undelete-previous-message))
	   (pop-to-buffer rmail-summary-buffer))
	  (t
	   (rmail-summary-goto-msg)))))

;; Rmail Summary mode is suitable only for specially formatted data.
(put 'rmail-summary-mode 'mode-class 'special)

(defun rmail-summary-mode ()
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
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'rmail-buffer)
  (make-local-variable 'rmail-total-messages)
  (setq major-mode 'rmail-summary-mode)
  (setq mode-name "RMAIL Summary")
  (use-local-map rmail-summary-mode-map)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (set-syntax-table text-mode-syntax-table)
  (run-hooks 'rmail-summary-mode-hook))

(defun rmail-summary-goto-msg (&optional n nowarn)
  (interactive "P")
  (if (consp n) (setq n (prefix-numeric-value n)))
  (if (eobp) (forward-line -1))
  (beginning-of-line)
  (let ((buf rmail-buffer)
	(cur (point))
	(curmsg (string-to-int
		 (buffer-substring (point)
				   (min (point-max) (+ 5 (point)))))))
    (if (not n)
	(setq n curmsg)
      (if (< n 1)
	  (progn (message "No preceding message")
		 (setq n 1)))
      (if (> n rmail-total-messages)
	  (progn (message "No following message")
		 (goto-char (point-max))
		 (rmail-summary-goto-msg)))
      (goto-char (point-min))
      (if (not (re-search-forward (concat "^ *" (int-to-string n)) nil t))
	  (progn (or nowarn (message "Message %d not found" n))
		 (setq n curmsg)
		 (goto-char cur))))
    (beginning-of-line)
    (skip-chars-forward " ")
    (skip-chars-forward "0-9")
    (save-excursion (if (= (following-char) ?-)
			(let ((buffer-read-only nil))
			  (delete-char 1)
			  (insert " "))))
    (beginning-of-line)
    (pop-to-buffer buf)
    (rmail-show-message n)
    (pop-to-buffer rmail-summary-buffer)))

(defvar rmail-summary-mode-map nil)

(if rmail-summary-mode-map
    nil
  (setq rmail-summary-mode-map (make-keymap))
  (suppress-keymap rmail-summary-mode-map)
  (define-key rmail-summary-mode-map "j" 'rmail-summary-goto-msg)
  (define-key rmail-summary-mode-map "n" 'rmail-summary-next-msg)
  (define-key rmail-summary-mode-map "p" 'rmail-summary-previous-msg)
  (define-key rmail-summary-mode-map "\C-n" 'rmail-summary-next-all)
  (define-key rmail-summary-mode-map "\C-p" 'rmail-summary-previous-all)
  (define-key rmail-summary-mode-map " " 'rmail-summary-scroll-msg-up)
  (define-key rmail-summary-mode-map "q" 'rmail-summary-quit)
  (define-key rmail-summary-mode-map "u" 'rmail-summary-undelete)
  (define-key rmail-summary-mode-map "x" 'rmail-summary-exit)
  (define-key rmail-summary-mode-map "\177" 'rmail-summary-scroll-msg-down)
  (define-key rmail-summary-mode-map "d" 'rmail-summary-delete-forward))

(defun rmail-summary-scroll-msg-up (&optional dist)
  "Scroll other window forward."
  (interactive "P")
  (let ((window (selected-window))
	(new-window (display-buffer rmail-buffer)))
    (unwind-protect
	(progn
	  (select-window new-window)
	  (scroll-up dist))
      (select-window window))))

(defun rmail-summary-scroll-msg-down (&optional dist)
  "Scroll other window backward."
  (interactive "P")
  (let ((window (selected-window))
	(new-window (display-buffer rmail-buffer)))
    (unwind-protect
	(progn
	  (select-window new-window)
	  (scroll-down dist))
      (select-window window))))

(defun rmail-summary-quit ()
  "Quit out of rmail and rmail summary."
  (interactive)
  (rmail-summary-exit)
  (rmail-quit))

(defun rmail-summary-exit ()
  "Exit rmail summary, remaining within rmail."
  (interactive)
  (bury-buffer (current-buffer))
  (if (get-buffer-window rmail-buffer)
      ;; Select the window with rmail in it, then delete this window.
      (select-window (prog1
			 (get-buffer-window rmail-buffer)
		       (delete-window (selected-window))))
    ;; Switch to the rmail buffer in this window.
    (switch-to-buffer rmail-buffer)))
