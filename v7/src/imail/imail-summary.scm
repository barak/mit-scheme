;;; -*-Scheme-*-
;;;
;;; $Id: imail-summary.scm,v 1.3 2000/05/18 05:19:00 cph Exp $
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

(define-command imail-summary
  "Display a summary of the selected folder, one line per message."
  ()
  (lambda () (imail-summary "All" #f)))

(define-command imail-summary-by-flags
  "Display a summary of the selected folder, one line per message.
Only messages marked with one of the given flags are shown.
The flags are specified as a comma-separated list of names."
  "sFlags to summarize by"
  (lambda (flags-string)
    (imail-summary
     (string-append "Flags " flags-string)
     (let ((flags (parse-comma-list-string flags-string)))
       (lambda (m)
	 (flags-intersect? (message-flags m) flags))))))

(define (flags-intersect? f1 f2)
  (there-exists? f1
    (lambda (flag)
      (flags-member? flag f2))))

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
		     (add-event-receiver! (folder-modification-event folder)
					  imail-summary-modification-event)
		     (associate-buffer-with-imail-buffer folder-buffer buffer)
		     (buffer-put! buffer 'IMAIL-MESSAGE-METHOD
				  imail-summary-selected-message)
		     (buffer-put! folder-buffer 'IMAIL-SUMMARY-BUFFER buffer)
		     (add-kill-buffer-hook buffer imail-summary-detach)))
		  buffer)))))
    (buffer-put! buffer 'IMAIL-SUMMARY-DESCRIPTION description)
    (buffer-put! buffer 'IMAIL-SUMMARY-PREDICATE predicate)
    (rebuild-imail-summary-buffer buffer)
    (select-buffer buffer)))

(define (imail-summary-detach buffer)
  (let ((folder-buffer (buffer-get buffer 'IMAIL-FOLDER-BUFFER #f)))
    (if folder-buffer
	(buffer-remove! folder-buffer 'IMAIL-SUMMARY-BUFFER))))

(define (imail-summary-modification-event folder type parameters)
  (let ((buffer (imail-folder->summary-buffer folder #f)))
    (if buffer
	(case type
	  ((FLAGS)
	   (let ((mark (imail-summary-message-mark buffer (car parameters))))
	     (if mark
		 (with-read-only-defeated mark
		   (lambda ()
		     (group-replace-char!
		      (mark-group mark)
		      (mark-index mark)
		      (if (message-deleted? (car parameters))
			  #\D
			  #\space)))))))
	  ((SELECT-MESSAGE)
	   (let ((mark (imail-summary-message-mark buffer (car parameters))))
	     (if mark
		 (set-buffer-point! buffer mark))))
	  ((EXPUNGE)
	   (maybe-add-command-suffix! rebuild-imail-summary-buffer buffer))
	  ((INCREASE-LENGTH SET-LENGTH)
	   (rebuild-imail-summary-buffer buffer))))))

(define (imail-folder->summary-buffer folder error?)
  (let ((buffer (imail-folder->buffer folder error?)))
    (and buffer
	 (or (buffer-get buffer 'IMAIL-SUMMARY-BUFFER #f)
	     (and error?
		  (error:bad-range-argument folder
					    'IMAIL-FOLDER->SUMMARY-BUFFER))))))

(define (imail-summary-message-mark buffer message)
  (let ((index (message-index message)))
    (and index
	 (line-start (buffer-start buffer) index))))

(define (rebuild-imail-summary-buffer buffer)
  (set-buffer-writeable! buffer)
  (buffer-widen! buffer)
  (region-delete! (buffer-region buffer))
  (fill-imail-summary-buffer! buffer
			      (selected-folder #f buffer)
			      (buffer-get buffer
					  'IMAIL-SUMMARY-PREDICATE
					  #f))
  (set-buffer-major-mode! buffer (ref-mode-object imail))
  (buffer-put! buffer 'REVERT-BUFFER-METHOD imail-summary-revert-buffer)
  (remove-kill-buffer-hook buffer imail-kill-buffer)
  (buffer-not-modified! buffer)
  (set-buffer-point! buffer (buffer-start buffer))
  (local-set-variable! truncate-lines #t buffer)
  (local-set-variable! mode-line-process
		       (list ": "
			     (buffer-get buffer
					 'IMAIL-SUMMARY-DESCRIPTION
					 "All"))
		       buffer)
  (let ((message
	 (selected-message #f (buffer-get buffer 'IMAIL-FOLDER-BUFFER))))
    (if message
	(imail-summary-select-message buffer message))))

(define (imail-summary-revert-buffer buffer dont-use-auto-save? dont-confirm?)
  dont-use-auto-save? dont-confirm?
  (if (or dont-confirm?
	  (prompt-for-yes-or-no? "Revert summary buffer"))
      (rebuild-imail-summary-buffer buffer)))

(define (imail-summary-selected-message buffer)
  (let ((folder (selected-folder #f buffer))
	(index
	 (count-lines (buffer-start buffer)
		      (line-start (buffer-point buffer) 0))))
    (and folder
	 (< index (folder-length folder))
	 (get-message folder index))))

(define (imail-summary-select-message buffer message)
  (let ((mark (line-start (buffer-start buffer) (message-index message))))
    (if mark
	(set-buffer-point! buffer mark))))

(define (fill-imail-summary-buffer! buffer folder predicate)
  (let ((messages
	 (let ((end (folder-length folder)))
	   (let loop ((i 0) (messages '()))
	     (if (< i end)
		 (loop (+ i 1) (cons (get-message folder i) messages))
		 (reverse! messages))))))
    (let ((mark (mark-left-inserting-copy (buffer-start buffer))))
      (for-each (lambda (message)
		  (if (or (not predicate) (predicate message))
		      (write-imail-summary-line! message mark)))
		messages)
      (mark-temporary! mark))))

(define (write-imail-summary-line! message mark)
  (insert-char (if (message-deleted? message) #\D #\space) mark)
  (insert-string-pad-left (number->string (+ (message-index message) 1))
			  4 #\space mark)
  (insert-string "  " mark)
  (insert-string-pad-right (message-summary-date-string message)
			   11 #\space mark)
  (insert-string "  " mark)
  (insert-string-pad-right (let ((s (message-summary-from-string message)))
			     (if (> (string-length s) 24)
				 (string-head s 24)
				 s))
			   24 #\space mark)
  (insert-string " " mark)
  (insert-string (message-summary-subject-string message) mark)
  (insert-newline mark))

(define (message-summary-date-string message)
  (let ((t (message-time message)))
    (if t
	(let ((dt (universal-time->local-decoded-time t)))
	  (string-append
	   (string-pad-left (number->string (decoded-time/day dt)) 2)
	   " "
	   (month/short-string (decoded-time/month dt))
	   " "
	   (number->string (decoded-time/year dt))))
	"")))

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
  (let ((s (or (get-first-header-field-value message "subject" #f) "")))
    (let ((i (string-find-next-char s #\newline)))
      (if i
	  (string-head s i)
	  s))))