;;; -*-Scheme-*-
;;;
;;; $Id: vc.scm,v 1.35 2000/03/23 22:49:05 cph Exp $
;;;
;;; Copyright (c) 1994-2000 Massachusetts Institute of Technology
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

;;;; Version Control
;;;  Translated from "vc.el" in Emacs 19.22.

(declare (usual-integrations))

;;;; Editor Variables

(define-variable vc-make-backup-files
  "If true, backups of registered files are made as with other files.
If false (the default), files covered by version control don't get backups."
  #f
  boolean?)

(define-variable-per-buffer vc-mode-line-status
  "A mode line string showing the version control status of the buffer.
Bound to #F if the buffer is not under version control."
  #f
  string-or-false?)
(let ((variable (ref-variable-object vc-mode-line-status)))
  (variable-permanent-local! variable)
  (set-variable! minor-mode-alist
		 (cons (list variable variable)
		       (ref-variable minor-mode-alist))))

(define-variable vc-suppress-confirm
  "If true, treat user as expert; suppress yes-no prompts on some things."
  #f
  boolean?)

(define-variable vc-keep-workfiles
  "If true, don't delete working files after registering changes.
If the back-end is CVS, workfiles are always kept, regardless of the
value of this flag."
  #t
  boolean?)

(define-variable vc-initial-comment
  "Prompt for initial comment when a file is registered."
  #f
  boolean?)

(define-variable vc-command-messages
  "If true, display run messages from back-end commands."
  #f
  boolean?)

(define-variable diff-switches
  "A list of strings specifying switches to be be passed to diff."
  '("-c")
  list-of-strings?)

(define-variable vc-checkin-hooks
  "An event distributor that is invoked after a checkin is done."
  (make-event-distributor))

(define-variable vc-checkout-carefully
  "True means be extra-careful in checkout.
Verify that the file really is not locked
and that its contents match what the master file says."
  ;; Default is to be extra careful for super-user.
  (lambda () (= (unix/current-uid) 0))
  (lambda (object)
    (or (boolean? object)
	(and (procedure? object)
	     (procedure-arity-valid? object 0)))))

(define-variable vc-log-mode-hook
  "An event distributor that is invoked when entering VC-log mode."
  (make-event-distributor))

(define-variable vc-display-status
  "If true, display revision number and lock status in modeline.
Otherwise, not displayed."
  #t
  boolean?)

(define-variable vc-rcs-preserve-mod-times
  "If true, files checked out from RCS use checkin time for mod time.
Otherwise, the mod time of the file is the checkout time."
  #t
  boolean?)

;;;; Editor Hooks

(define (vc-find-file-hook buffer)
  (buffer-remove! buffer 'VC-MASTER)
  (let ((master (buffer-vc-master buffer)))
    (vc-mode-line master buffer)
    (if (and master (not (ref-variable vc-make-backup-files buffer)))
	(define-variable-local-value! buffer
	    (ref-variable-object make-backup-files)
	  #f))))
(add-event-receiver! (ref-variable find-file-hooks) vc-find-file-hook)

(define (vc-file-not-found-hook buffer)
  (let ((master (buffer-vc-master buffer)))
    (and master
	 (begin
	   (load-edwin-library 'VC)
	   (call-with-current-continuation
	    (lambda (k)
	      (bind-condition-handler (list condition-type:error)
		  (lambda (condition)
		    condition
		    (k #f))
		(lambda ()
		  (vc-checkout master #f)
		  #t))))))))
(let ((hooks (ref-variable find-file-not-found-hooks)))
  (if (not (memq vc-file-not-found-hook hooks))
      (set-variable! find-file-not-found-hooks
		     (append! hooks (list vc-file-not-found-hook)))))

(define (vc-after-save buffer)
  (let ((master (buffer-vc-master buffer)))
    (if master
	(vc-mode-line master buffer))))

(define (vc-mode-line master buffer)
  (let ((buffer (or buffer (vc-workfile-buffer master))))
    (let ((variable (ref-variable-object vc-mode-line-status)))
      (if master
	  (set-variable-local-value!
	   buffer
	   variable 
	   (string-append " "
			  (vc-type-display-name (vc-master-type master))
			  (if (ref-variable vc-display-status buffer)
			      (vc-mode-line-status master)
			      "")))
	  (undefine-variable-local-value! buffer variable)))
    (buffer-modeline-event! buffer 'VC-MODE-LINE-STATUS)
    (if (and master
	     (buffer-writeable? buffer)
	     (eq? buffer (vc-workfile-buffer master))
	     ;; If the file is locked by some other user, make the
	     ;; buffer read-only.  Like this, even root cannot modify a
	     ;; file that someone else has locked.
	     (or (let ((locking-user (vc-locking-user master #f)))
		   (and locking-user
			(not (string=? locking-user (current-user-name)))))
		 ;; If the user is root, and the file is not
		 ;; owner-writeable, then pretend that we can't write it
		 ;; even though we can (because root can write
		 ;; anything).  This way, even root cannot modify a file
		 ;; that isn't locked.
		 (and (= 0 (unix/current-uid))
		      (fix:= 0
			     (fix:and #o200
				      (file-modes
				       (vc-workfile-pathname master)))))))
	(set-buffer-read-only! buffer))))

(define (vc-mode-line-status master)
  (let ((revision
	 (or (vc-workfile-version master)
	     (vc-default-version master #f))))
    (if revision
	(let ((locker (vc-locking-user master revision)))
	  (string-append (cond ((not locker) "-")
			       ((string=? locker (current-user-name)) ":")
			       (else (string-append ":" locker ":")))
			 revision))
	" @@")))

;;;; Primary Commands

(define-command vc-toggle-read-only
  "Change read-only status of current buffer, perhaps via version control.
If the buffer is visiting a file registered with version control,
then check the file in or out.  Otherwise, just change the read-only flag
of the buffer."
  ()
  (lambda ()
    (if (buffer-vc-master (current-buffer))
	((ref-command vc-next-action) #f)
	((ref-command toggle-read-only)))))

(define-command vc-next-action
  "Do the next logical checkin or checkout operation on the current file.
   If the file is not already registered, this registers it for version
control and then retrieves a writeable, locked copy for editing.
   If the file is registered and not locked by anyone, this checks out
a writeable and locked file ready for editing.
   If the file is checked out and locked by the calling user, this
first checks to see if the file has changed since checkout.  If not,
it performs a revert.
   If the file has been changed, this pops up a buffer for entry
of a log message; when the message has been entered, it checks in the
resulting changes along with the log message as change commentary.  If
the variable `vc-keep-workfiles' is true (which is its default), a
read-only copy of the changed file is left in place afterwards.
   If the file is registered and locked by someone else, you are given
the option to steal the lock.
   If you call this from within a VC dired buffer with no files marked,
it will operate on the file in the current line.
   If you call this from within a VC dired buffer, and one or more
files are marked, it will accept a log message and then operate on
each one.  The log message will be used as a comment for any register
or checkin operations, but ignored when doing checkouts.  Attempted
lock steals will raise an error.

   For checkin, a prefix argument lets you specify the version number to use."
  "P"
  (lambda (revision?)
    (if (not (eq? (current-major-mode) (ref-mode-object dired)))
	(let ((workfile (buffer-pathname (current-buffer))))
	  (if (not workfile)
	      (vc-registration-error #f))
	  (vc-next-action-on-file workfile revision? #f))
	(let ((files
	       (let ((files (dired-marked-files)))
		 (if (null? files)
		     (dired-next-files 1)
		     files))))
	  (cond ((null? files)
		 unspecific)
		((null? (cdr files))
		 (vc-next-action-on-file (caar files) revision? #f))
		(else
		 (vc-start-entry #f
				 "Enter a change comment for the marked files."
				 #f
				 (vc-next-action-dired (current-buffer))
				 #f)))))))

(define-command vc-register
  "Register the current file into your version-control system."
  "P"
  (lambda (revision?)
    (let ((workfile (buffer-pathname (current-buffer))))
      (if (not workfile)
	  (vc-registration-error #f))
      (if (file-vc-master workfile)
	  (editor-error "This file is already registered."))
      (vc-register workfile revision? #f #f))))

(define (vc-next-action-on-file workfile revision comment)
  (let ((master (file-vc-master workfile)))
    (if (not master)
	(vc-register workfile revision comment 'LOCK)
	(let* ((revision (vc-get-version revision "Version level to act on")))
	  (let ((owner (vc-locking-user master revision)))
	    (cond ((not owner)
		   (vc-checkout master revision))
		  ((string=? owner (current-user-name))
		   (if (or (let ((buffer (vc-workfile-buffer workfile)))
			     (and buffer
				  (buffer-modified? buffer)))
			   (vc-workfile-modified? master))
		       (vc-checkin master revision comment)
		       (vc-revert master revision)))
		  (else
		   (vc-steal-lock master revision comment owner))))))))

(define (vc-next-action-dired buffer)
  (lambda (comment)
    (for-each-dired-mark buffer
      (lambda (file)
	(let ((msg (string-append "Processing " (->namestring file) "...")))
	  (message msg)
	  (vc-next-action-on-file file #f comment)
	  (message msg "done"))))))

(define (vc-register workfile revision comment keep?)
  (let ((revision
	 (vc-get-version revision
			 (string-append "Initial version level for "
					(vc-workfile-string workfile)))))
    (let ((buffer (vc-workfile-buffer workfile)))
      ;; Watch out for new buffers of size 0: the corresponding file
      ;; does not exist yet, even though buffer-modified? is false.
      (if (and buffer
	       (not (buffer-modified? buffer))
	       (= 0 (buffer-length buffer))
	       (not (file-exists? workfile)))
	  (buffer-modified! buffer)))
    (vc-save-workfile-buffer workfile)
    (vc-start-entry workfile
		    "Enter initial comment."
		    (or comment
			(if (ref-variable vc-initial-comment
					  (vc-workfile-buffer workfile))
			    #f
			    ""))
		    (let ((keep? (or keep? (vc-keep-workfiles? workfile))))
		      (lambda (comment)
			(vc-backend-register workfile revision comment keep?)
			(vc-update-workfile-buffer workfile keep?)))
		    #f)))

(define (vc-checkout master revision)
  (let ((revision
	 (or (vc-get-version revision "Version level to check out")
	     (vc-workfile-version master))))
    (let ((do-it
	   (lambda ()
	     (vc-backend-checkout master revision #t #f)
	     (vc-revert-workfile-buffer master #t))))
      (cond ((or (not (let ((value (ref-variable vc-checkout-carefully)))
			(if (boolean? value)
			    value
			    (value))))
		 (not (vc-workfile-modified? master))
		 (= 0 (vc-backend-diff master #f #f #t)))
	     (do-it))
	    ((cleanup-pop-up-buffers
	      (lambda ()
		(vc-backend-diff master #f #f #f)
		(let ((diff-buffer (get-vc-command-buffer)))
		  (insert-string
		   (string-append "Changes to "
				  (vc-workfile-string master)
				  " since last lock:\n\n")
		   (buffer-start diff-buffer))
		  (set-buffer-point! diff-buffer (buffer-start diff-buffer))
		  (pop-up-buffer diff-buffer #f)
		  (editor-beep)
		  (prompt-for-yes-or-no?
		   (string-append "File has unlocked changes, "
				  "claim lock retaining changes")))))
	     (guarantee-vc-master-valid master)
	     (vc-backend-steal master revision)
	     (let ((buffer (vc-workfile-buffer master)))
	       (if buffer
		   (vc-mode-line master buffer))))
	    ((prompt-for-yes-or-no? "Revert to checked-in version, instead")
	     (do-it))
	    (else
	     (editor-error "Checkout aborted."))))))

(define (vc-checkin master revision comment)
  (let ((revision (vc-get-version revision "New version level")))
    (vc-save-workfile-buffer master)
    (vc-start-entry master
		    "Enter a change comment."
		    comment
		    (let ((keep? (vc-keep-workfiles? master)))
		      (lambda (comment)
			(vc-backend-checkin master revision
					    (if (blank-string? comment)
						"*** empty log message ***"
						comment)
					    keep?)
			(vc-update-workfile-buffer master keep?)))
		    (lambda ()
		      (event-distributor/invoke!
		       (ref-variable vc-checkin-hooks
				     (vc-workfile-buffer master))
		       master)))))

(define (blank-string? string)
  (not (string-find-next-char-in-set string char-set:not-whitespace)))

(define (vc-revert master revision)
  (let ((revision
	 (or (vc-get-version revision "Version level to revert")
	     (vc-workfile-version master))))
    (vc-save-workfile-buffer master)
    (vc-backend-revert master revision)
    (vc-revert-workfile-buffer master #f)))

(define (vc-steal-lock master revision comment owner)
  (let ((filename (vc-workfile-string master)))
    (if comment
	(editor-error "Sorry, you can't steal the lock on "
		      filename
		      " this way."))
    (let ((revision
	   (or (vc-get-version revision "Version level to steal")
	       (vc-workfile-version master))))
      (let ((file:rev
	     (if revision
		 (string-append filename ":" revision)
		 filename)))
	(if (not (prompt-for-confirmation?
		  (string-append "Take the lock on " file:rev " from " owner)))
	    (editor-error "Steal cancelled."))
	(make-mail-buffer `(("To" ,owner) ("Subject" ,file:rev))
			  #f
			  select-buffer-other-window
			  'DISCARD-PREVIOUS-MAIL)
	(let ((mail-buffer (current-buffer)))
	  (let ((time (get-decoded-time)))
	    (insert-string (string-append "I stole the lock on "
					  file:rev
					  ", "
					  (decoded-time/date-string time)
					  " at "
					  (decoded-time/time-string time)
					  ".\n")
			   (buffer-end mail-buffer)))
	  (set-buffer-point! mail-buffer (buffer-end mail-buffer))
	  (let ((variable (ref-variable-object send-mail-procedure)))
	    (define-variable-local-value! mail-buffer variable
	      (lambda ()
		(guarantee-vc-master-valid master)
		(vc-backend-steal master revision)
		(vc-revert-workfile-buffer master #t)
		;; Send the mail after the steal has completed
		;; successfully.
		((variable-default-value variable)))))))))
  (message "Please explain why you are stealing the lock."
	   "  Type C-c C-c when done."))

;;;; Auxiliary Commands

(define-command vc-diff
  "Display diffs between file versions.
Normally this compares the current file and buffer with the most recent 
checked in version of that file.  This uses no arguments.
With a prefix argument, it reads the file name to use
and two version designators specifying which versions to compare."
  "P"
  (lambda (revisions?)
    (if revisions?
	(dispatch-on-command (ref-command-object vc-version-diff))
	(vc-diff (current-vc-master #t) #f #f))))

(define-command vc-version-diff
  "For FILE, report diffs between two stored versions REV1 and REV2 of it.
If FILE is a directory, generate diffs between versions for all registered
files in or below it."
  "FFile or directory to diff\nsOlder version\nsNewer version"
  (lambda (workfile rev1 rev2)
    (if (file-directory? workfile)
	(editor-error "Directory diffs not yet supported.")
	(vc-diff (file-vc-master workfile #t) rev1 rev2))))

(define (vc-diff master rev1 rev2)
  (vc-save-workfile-buffer master)
  (let ((rev1 (vc-normalize-version rev1))
	(rev2 (vc-normalize-version rev2)))
    (let ((rev1 (if (or rev1 rev2) rev1 (vc-workfile-version master))))
      (if (and (or rev1 rev2 (not (vc-workfile-modified? master)))
	       (= 0 (vc-backend-diff master rev1 rev2 #t)))
	  (begin
	    (message "No changes to "
		     (vc-workfile-string master)
		     (if (and rev1 rev2)
			 (string-append " between " rev1 " and " rev2)
			 (string-append " since "
					(or rev1 rev2 "latest version")))
		     ".")
	    #t)
	  (begin
	    (vc-backend-diff master rev1 rev2 #f)
	    (pop-up-vc-command-buffer #t)
	    #f)))))

(define-command vc-version-other-window
  "Visit version REV of the current buffer in another window.
If the current buffer is named `F', the version is named `F.~REV~'.
If `F.~REV~' already exists, it is used instead of being re-created."
  "sVersion to visit (default is latest version)"
  (lambda (revision)
    (let ((master (current-vc-master #t)))
      (let ((revision
	     (or (vc-normalize-version revision)
		 (vc-default-version master #t))))
	(let ((workfile
	       (string-append (->namestring (vc-master-workfile master))
			      ".~"
			      revision
			      "~")))
	  (if (not (file-exists? workfile))
	      (vc-backend-checkout master revision #f workfile))
	  (find-file-other-window workfile))))))

(define-command vc-insert-headers
  "Insert headers in a file for use with your version-control system.
Headers are inserted at the start of the buffer."
  ()
  (lambda ()
    (let ((master (current-vc-master #t)))
      (let ((buffer
	     (or (vc-workfile-buffer master)
		 (find-file-other-window (vc-workfile-pathname master)))))
	(without-group-clipped! (buffer-group buffer)
	  (lambda ()
	    (if (or (not (vc-backend-check-headers master buffer))
		    (prompt-for-confirmation?
		     "Version headers already exist.  Insert another set"))
		(insert-string
		 (string-append
		  (or (ref-variable comment-start buffer) "#")
		  "\t"
		  (vc-type-header-keyword (vc-master-type master))
		  (let ((end (or (ref-variable comment-end buffer) "")))
		    (if (string-null? end)
			end
			(string-append "\t" end)))
		  "\n")
		 (buffer-start buffer)))))))))

(define-command vc-print-log
  "List the change log of the current buffer in a window."
  ()
  (lambda ()
    (vc-backend-print-log (current-vc-master #t))
    (pop-up-vc-command-buffer #f)))

(define-command vc-revert-buffer
  "Revert the current buffer's file back to the latest checked-in version.
This asks for confirmation if the buffer contents are not identical
to that version."
  ()
  (lambda ()
    (let ((buffer (current-buffer)))
      (let ((master (buffer-vc-master buffer)))
	(if (cleanup-pop-up-buffers
	     (lambda ()
	       (or (not (vc-diff master #f #f))
		   (ref-variable vc-suppress-confirm)
		   (prompt-for-yes-or-no? "Discard changes"))))
	    (begin
	      (vc-backend-revert master #f)
	      (vc-revert-buffer buffer #t))
	    (editor-error "Revert cancelled."))))))

(define-command vc-cancel-version
  "Get rid of most recently checked in version of this file.
A prefix argument means do not revert the buffer afterwards."
  "P"
  (lambda (no-revert?)
    no-revert?
    (editor-error "VC-CANCEL-VERSION not implemented.")))

;;;; VC Dired

(define-command vc-directory
  "Show version-control status of files under a directory.
Normally shows only locked files; prefix arg says to show all files."
  "P"
  (lambda (all-files?)
    (let ((directory (buffer-default-directory (current-buffer))))
      (let ((buffer (vc-dired directory all-files?)))
	(if (> (buffer-length buffer) 0)
	    (pop-up-buffer buffer #t)
	    (begin
	      (if (not (buffer-visible? buffer))
		  (kill-buffer buffer))
	      (message "No files are currently "
		       (if all-files? "registered" "locked")
		       " under "
		       (->namestring directory))))))))

(define-command vc-dired
  "Show version-control status of files under a directory.
Normally shows only locked files; prefix arg says to show all files."
  "DVC-Dired (directory)\nP"
  (lambda (directory all-files?)
    (select-buffer (vc-dired directory all-files?))))

(define (vc-dired directory all-files?)
  (let ((buffer (get-vc-dired-buffer directory)))
    (fill-vc-dired-buffer! buffer directory all-files?)
    buffer))

(define (get-vc-dired-buffer directory)
  (or (list-search-positive (buffer-list)
	(lambda (buffer)
	  (let ((spec (buffer-get buffer 'VC-DIRECTORY-SPEC)))
	    (and spec
		 (pathname=? (car spec) directory)))))
      (new-buffer (pathname->buffer-name directory))))

(define (fill-vc-dired-buffer! buffer directory all-files?)
  (let ((msg
	 (string-append "Reading directory " (->namestring directory) "...")))
    (buffer-reset! buffer)
    (set-buffer-major-mode! buffer (ref-mode-object dired))
    (define-variable-local-value! buffer (ref-variable-object mode-name)
      "VC-Dired")
    (set-buffer-default-directory! buffer (directory-pathname directory))
    (buffer-put! buffer 'VC-DIRECTORY-SPEC (cons directory all-files?))
    (buffer-put! buffer 'REVERT-BUFFER-METHOD revert-vc-dired-buffer)
    (message msg)
    (let ((mark (mark-left-inserting-copy (buffer-start buffer))))
      (insert-string (string-append "  Files currently "
				    (if all-files? "registered" "locked")
				    " under "
				    (->namestring directory)
				    ":\n")
		     mark)
      (generate-vc-dired-lines directory all-files? mark)
      (mark-temporary! mark))
    (message msg "done"))
  (set-buffer-point! buffer (buffer-start buffer))
  (buffer-not-modified! buffer)
  (set-buffer-read-only! buffer))

(define (revert-vc-dired-buffer buffer dont-use-auto-save? dont-confirm?)
  (let ((spec (buffer-get buffer 'VC-DIRECTORY-SPEC)))
    (if spec
	(fill-vc-dired-buffer! buffer (car spec) (cdr spec))
	(revert-buffer-default buffer dont-use-auto-save? dont-confirm?))))

(define (generate-vc-dired-lines directory all-files? mark)
  (for-each (lambda (file)
	      (let ((attr (file-attributes-direct file)))
		(if (and attr (not (file-attributes/type attr)))
		    (let ((master (file-vc-master file)))
		      (if master
			  (let ((locker (vc-locking-user master #f)))
			    (if (or locker all-files?)
				(generate-vc-dired-line file
							attr
							locker
							mark))))))))
	    (directory-read directory)))

(define (generate-vc-dired-line file attr locker mark)
  (insert-string
   (string-append
    "  "
    (file-attributes/mode-string attr)
    " "
    (pad-on-left-to (number->string (file-attributes/n-links attr)) 3)
    " "
    (pad-on-right-to (or locker "") 10)
    " "
    (pad-on-left-to (number->string (file-attributes/length attr)) 8)
    " "
    (file-time->ls-string (file-attributes/modification-time attr))
    " "
    (file-namestring file)
    "\n")
   mark))

;;;; Log Entries

(define (vc-start-entry master msg comment finish-entry after)
  (if comment
      (begin
	(finish-entry comment)
	(if after (after)))
      (let ((log-buffer (new-buffer "*VC-log*")))
	(set-buffer-major-mode! log-buffer (ref-mode-object vc-log))
	(if (vc-master? master)
	    (vc-mode-line master log-buffer))
	(buffer-put! log-buffer 'VC-PARENT-BUFFER
		     (and master (vc-workfile-buffer master)))
	(let ((window (current-window)))
	  (let ((log-window (pop-up-buffer log-buffer #t)))
	    (buffer-put! log-buffer
			 'VC-LOG-FINISH-ENTRY
			 (vc-finish-entry master
					  finish-entry
					  after
					  (weak-cons log-window #f)
					  (weak-cons window #f)))))
	(message msg "  Type C-c C-c when done."))))

(define (vc-finish-entry master finish-entry after log-window window)
  (lambda (log-buffer)
    ;; If a new window was created to hold the log buffer, and the
    ;; log buffer is still selected in that window, delete it.
    (let ((log-window (weak-car log-window)))
      (if (and log-window
	       (window-live? log-window)
	       (eq? log-buffer (window-buffer log-window))
	       (not (window-has-no-neighbors? log-window)))
	  (window-delete! log-window)))
    (let ((window (weak-car window)))
      (if (and window
	       (window-live? window))
	  (select-window window)))
    (guarantee-newline (buffer-end log-buffer))
    (if (vc-master? master)
	(guarantee-vc-master-valid master))
    ;; Signal error if log entry too long.
    (if (vc-master? master)
	(vc-backend-logentry-check master log-buffer))
    (let ((comment (buffer-string log-buffer)))
      ;; Enter the comment in the comment ring.
      (comint-record-input vc-comment-ring comment)
      ;; We're finished with the log buffer now.
      (kill-buffer log-buffer)
      ;; Perform the log operation.
      (finish-entry comment))
    (if after (after))))

(define vc-comment-ring
  (make-ring 32))

(define-major-mode vc-log text "VC-Log"
  "Major mode for entering a version-control change log message.
In this mode, the following additional bindings will be in effect.

\\[vc-finish-logentry]	proceed with check in, ending log message entry

Whenever you do a checkin, your log comment is added to a ring of
saved comments.  These can be recalled as follows:

\\[comint-previous-input]	replace region with previous message in comment ring
\\[comint-next-input]	replace region with next message in comment ring
\\[comint-history-search-reverse]	search backward for regexp in the comment ring
\\[comint-history-search-forward]	search forward for regexp in the comment ring

Entry to the vc-log submode calls the value of text-mode-hook, then
the value of vc-log-mode-hook."
  (lambda (buffer)
    (define-variable-local-value! buffer
	(ref-variable-object comint-input-ring)
      vc-comment-ring)
    (define-variable-local-value! buffer
	(ref-variable-object comint-last-input-match)
      false)
    (event-distributor/invoke! (ref-variable vc-log-mode-hook buffer) buffer)))

(define-key 'vc-log '(#\C-c #\C-c) 'vc-finish-logentry)
(define-key 'vc-log #\M-p 'comint-previous-input)
(define-key 'vc-log #\M-n 'comint-next-input)
(define-key 'vc-log #\M-r 'comint-history-search-backward)
(define-key 'vc-log #\M-s 'comint-history-search-forward)

(define-command vc-finish-logentry
  "Complete the operation implied by the current log entry."
  ()
  (lambda ()
    (let ((buffer (current-buffer)))
      (let ((finish-entry (buffer-get buffer 'VC-LOG-FINISH-ENTRY)))
	(if (not finish-entry)
	    (error "No log operation is pending."))
	(finish-entry buffer)))))

;;;; VC-Master Association

(define (file-vc-master workfile #!optional require-master?)
  (let ((require-master?
	 (if (default-object? require-master?)
	     #f
	     require-master?))
	(buffer (pathname->buffer workfile)))
    (if buffer
	(buffer-vc-master buffer require-master?)
	(%file-vc-master workfile require-master?))))

(define (current-vc-master #!optional require-master?)
  (let ((buffer (current-buffer))
	(require-master?
	 (if (default-object? require-master?)
	     #f
	     require-master?)))
    (if (eq? (buffer-major-mode buffer) (ref-mode-object dired))
	(let ((file (dired-this-file)))
	  (if file
	      (file-vc-master (car file) require-master?)
	      (begin
		(if require-master? (vc-registration-error #f))
		#f)))
	(buffer-vc-master (or (buffer-get buffer 'VC-PARENT-BUFFER) buffer)
			  require-master?))))

(define (buffer-vc-master buffer #!optional require-master?)
  (let ((require-master?
	 (if (default-object? require-master?)
	     #f
	     require-master?))
	(workfile (buffer-pathname buffer)))
    (if workfile
	(let ((master (buffer-get buffer 'VC-MASTER)))
	  (if (and master
		   (pathname=? workfile (vc-master-workfile master))
		   (vc-master-valid? master))
	      master
	      (let ((master (%file-vc-master workfile require-master?)))
		(buffer-put! buffer 'VC-MASTER master)
		master)))
	(begin
	  (buffer-put! buffer 'VC-MASTER #f)
	  (if require-master? (vc-registration-error buffer))
	  #f))))

(define (%file-vc-master workfile require-master?)
  (let ((workfile (->pathname workfile)))
    (let loop ((templates vc-master-templates))
      (if (null? templates)
	  (begin
	    (if require-master? (vc-registration-error workfile))
	    #f)
	  (let ((master ((car templates) workfile)))
	    (if (and master (vc-master-valid? master))
		master
		(loop (cdr templates))))))))

(define (guarantee-vc-master-valid master)
  (if (not (vc-master-valid? master))
      (error "VC master file disappeared:" (vc-master-workfile master))))

(define (vc-registration-error object)
  (if (or (buffer? object) (not object))
      (editor-error "Buffer "
		    (buffer-name (or object (current-buffer)))
		    " is not associated with a file.")
      (editor-error "File "
		    (vc-workfile-string object)
		    " is not under version control.")))

;;;; VC-Master Datatype

(define-structure (vc-master
		   (constructor make-vc-master (type pathname workfile)))
  (type #f read-only #t)
  (pathname #f read-only #t)
  (workfile #f read-only #t)
  (checkout-time #f)
  (properties (make-1d-table) read-only #t))

(define (vc-master-get master key default)
  (1d-table/get (vc-master-properties master) key default))

(define (vc-master-put! master key value)
  (1d-table/put! (vc-master-properties master) key value))

(define (vc-master-remove! master key)
  (1d-table/remove! (vc-master-properties master) key))

(define (sync-checkout-time! master unchanged?)
  (set-vc-master-checkout-time!
   master
   (and unchanged?
	(file-modification-time-indirect (vc-workfile-pathname master))))
  (vc-mode-line master #f))

(define (vc-master-read-cached-value master key read-value)
  (let ((pathname (vc-master-pathname master)))
    (let loop ()
      (let ((time (file-modification-time-indirect pathname)))
	(or (and (eqv? time (vc-master-get master 'MASTER-TIME #f))
		 (vc-master-get master key #f))
	    (begin
	      (vc-master-put! master 'MASTER-TIME time)
	      (vc-master-put! master key (read-value))
	      (loop)))))))

(define-structure (vc-type (constructor %make-vc-type
					(name display-name header-keyword)))
  (name #f read-only #t)
  (display-name #f read-only #t)
  (header-keyword #f read-only #t)
  (operations '())
  (properties (make-1d-table) read-only #t))

(define (vc-type-get type key default)
  (1d-table/get (vc-type-properties type) key default))

(define (vc-type-put! type key value)
  (1d-table/put! (vc-type-properties type) key value))

(define (vc-type-remove! type key)
  (1d-table/remove! (vc-type-properties type) key))

(define (make-vc-type name display-name header-keyword)
  (let ((type (%make-vc-type name display-name header-keyword))
	(entry (assq name vc-types)))
    (if entry
	(set-cdr! entry type)
	(set! vc-types (cons (cons name type) vc-types)))
    type))

(define vc-types
  '())

(define (define-vc-master-template pathname-map)
  (set! vc-master-templates (cons pathname-map vc-master-templates))
  unspecific)

(define vc-master-templates
  '())

(define (vc-release? type release)
  (let ((release* (vc-release type)))
    (and release*
	 (release<=? release release*))))

(define (vc-release type)
  (let ((release (vc-type-get type 'RELEASE 'UNKNOWN)))
    (if (eq? 'UNKNOWN release)
	(let ((release ((vc-type-operation type 'RELEASE))))
	  (vc-type-put! type 'RELEASE release)
	  release)
	release)))

(define (release<=? r1 r2)
  ;; Compare release numbers, represented as strings.
  ;; Release components are assumed cardinal numbers, not decimal
  ;; fractions (5.10 is a higher release than 5.9).  Omitted fields
  ;; are considered lower (5.6.7 is earlier than 5.6.7.1).
  ;; Comparison runs till the end of the string is found, or a
  ;; non-numeric component shows up (5.6.7 is earlier than "5.6.7 beta",
  ;; which is probably not what you want in some cases).
  ;;   This code is suitable for existing RCS release numbers.  
  ;; CVS releases are handled reasonably, too (1.3 < 1.4* < 1.5).
  (let ((t1 (burst-string r1 #\space #t))
	(t2 (burst-string r2 #\space #t)))
    (let loop
	((ns1 (burst-string (car t1) #\. #f))
	 (ns2 (burst-string (car t2) #\. #f)))
      (if (pair? ns1)
	  (and (pair? ns2)
	       (let ((n1 (string->number (car ns1)))
		     (n2 (string->number (car ns2))))
		 (or (< n1 n2)
		     (and (= n1 n2)
			  (loop (cdr ns1) (cdr ns2))))))
	  (or (pair? ns2)
	      (not (pair? (cdr t1)))
	      (pair? (cdr t2)))))))

(define (trunk-revision? revision)
  (re-string-match "\\`[0-9]+\\.[0-9]+\\'" revision))

(define (define-vc-type-operation name type procedure)
  (let ((entry (assq name (vc-type-operations type))))
    (if entry
	(set-cdr! entry procedure)
	(set-vc-type-operations! type
				 (cons (cons name procedure)
				       (vc-type-operations type))))))

(define (vc-type-operation type name)
  (let ((entry (assq name (vc-type-operations type))))
    (if (not entry)
	(error:bad-range-argument name 'VC-TYPE-OPERATION))
    (cdr entry)))

(define (vc-call name master . arguments)
  (apply (vc-type-operation (vc-master-type master) name) master arguments))

;;;; Back-End Calls

(define (vc-backend-register workfile revision comment keep?)
  ((vc-type-operation
    (if (and (not (null? vc-types))
	     (null? (cdr vc-types)))
	(cdar vc-types)
	(let ((likely-types
	       (list-transform-positive vc-types
		 (lambda (entry)
		   ((vc-type-operation (cdr entry) 'LIKELY-CONTROL-TYPE?)
		    workfile)))))
	  (if (and (not (null? likely-types))
		   (null? (cdr likely-types)))
	      (cdar likely-types)
	      (cleanup-pop-up-buffers
	       (lambda ()
		 (call-with-output-to-temporary-buffer " *VC-types*"
						       '(SHRINK-WINDOW)
		   (lambda (port)
		     (for-each (lambda (entry)
				 (write-string (car entry) port)
				 (newline port))
			       vc-types)))
		 (prompt-for-alist-value "Version control type"
					 vc-types
					 #f
					 #f))))))
    'REGISTER)
   workfile revision comment keep?))

(define (vc-backend-checkout master revision lock? workfile)
  (vc-call 'CHECKOUT master revision lock?
	   (and workfile
		(not (pathname=? workfile (vc-workfile-pathname master)))
		workfile)))

(define (vc-backend-revert master revision)
  (vc-call 'REVERT master revision))

(define (vc-backend-checkin master revision comment keep?)
  (vc-call 'CHECKIN master revision comment keep?))

(define (vc-backend-steal master revision)
  (vc-call 'STEAL master revision))

(define (vc-backend-logentry-check master log-buffer)
  (vc-call 'LOGENTRY-CHECK master log-buffer))

(define (vc-backend-diff master rev1 rev2 simple?)
  (let ((result (vc-call 'DIFF master rev1 rev2 simple?)))
    (if (and (or (not rev1) (equal? rev1 (vc-workfile-version master)))
	     (not rev2))
	(sync-checkout-time! master (= 0 result)))
    result))

(define (vc-backend-print-log master)
  (vc-call 'PRINT-LOG master))

(define (vc-default-version master error?)
  (vc-call 'DEFAULT-VERSION master error?))

(define (vc-workfile-version master)
  (vc-call 'WORKFILE-VERSION master))

(define (vc-locking-user master revision)
  (vc-call 'LOCKING-USER master revision))

(define (vc-backend-check-headers master buffer)
  (vc-call 'CHECK-HEADERS master buffer))

(define (vc-master-valid? master)
  (vc-call 'VALID? master))

;;;; RCS Commands

(define vc-type:rcs
  ;; Splitting up string constant prevents RCS from expanding this
  ;; keyword.
  (make-vc-type 'RCS "RCS" (string-append "$" "Id" "$")))

(define (rcs-directory workfile)
  (let ((directory (directory-pathname workfile)))
    (pathname-new-directory directory
			    (append (pathname-directory directory)
				    '("RCS")))))

(let ((rcs-template
       (lambda (transform)
	 (define-vc-master-template
	   (lambda (workfile)
	     (make-vc-master vc-type:rcs (transform workfile) workfile)))))
      (in-rcs-directory
       (lambda (pathname)
	 (merge-pathnames (file-pathname pathname)
			  (rcs-directory pathname))))
      (rcs-file
       (lambda (pathname)
	 (merge-pathnames (string-append (file-namestring pathname) ",v")
			  (directory-pathname pathname)))))
  (rcs-template (lambda (workfile) (rcs-file (in-rcs-directory workfile))))
  (rcs-template in-rcs-directory)
  (rcs-template rcs-file))

(define-vc-type-operation 'VALID? vc-type:rcs
  (lambda (master)
    ;; FILE-EQ? yields #f if either file doesn't exist.
    (let ((pathname (vc-master-pathname master)))
      (and (file-exists? pathname)
	   (not (file-eq? (vc-master-workfile master) pathname))))))

(define-vc-type-operation 'LOCKING-USER vc-type:rcs
  (lambda (master revision)
    (let ((admin (get-rcs-admin master)))
      (let ((delta (rcs-find-delta admin revision #f)))
	(if delta
	    (let loop ((locks (rcs-admin/locks admin)))
	      (and (not (null? locks))
		   (if (eq? delta (cdar locks))
		       (caar locks)
		       (loop (cdr locks)))))
	    ;; Kludge: this causes the next action to be a checkin.
	    (current-user-name))))))

(define (get-rcs-admin master)
  (vc-master-read-cached-value master 'RCS-ADMIN
    (lambda ()
      (parse-rcs-admin (vc-master-pathname master)))))

(define-vc-type-operation 'CHECK-HEADERS vc-type:rcs
  (lambda (master buffer)
    master
    (check-rcs-headers buffer)))

(define (check-rcs-headers buffer)
  (re-search-forward (string-append "\\$[A-Za-z\300-\326\330-\366\370-\377]+"
				    "\\(: [\t -#%-\176\240-\377]*\\)?\\$")
		     (buffer-start buffer)
		     (buffer-end buffer)))

(define-vc-type-operation 'LIKELY-CONTROL-TYPE? vc-type:rcs
  (lambda (workfile)
    (file-directory? (rcs-directory workfile))))

(define-vc-type-operation 'REGISTER vc-type:rcs
  (lambda (workfile revision comment keep?)
    (with-vc-command-message workfile "Registering"
      (lambda ()
	(vc-run-command workfile '() "ci"
			(and (vc-release? vc-type:rcs "5.6.4") "-i")
			(rcs-rev-switch (cond ((not keep?) "-r")
					      ((eq? 'LOCK keep?) "-l")
					      (else "-u"))
					revision)
			(string-append "-t-" comment)
			(vc-workfile-pathname workfile))))))

(define-vc-type-operation 'RELEASE vc-type:rcs
  (lambda ()
    (and (= 0 (vc-run-command #f '() "rcs" "-V"))
	 (re-search-forward "^RCS version \\([0-9.]+ *.*\\)"
			    (buffer-start (get-vc-command-buffer)))
	 (extract-string (re-match-start 1) (re-match-end 1)))))

(define-vc-type-operation 'CHECKOUT vc-type:rcs
  (lambda (master revision lock? workfile)
    (with-vc-command-message master "Checking out"
      (lambda ()
	(if workfile
	    ;; RCS makes it difficult to check a file out into anything
	    ;; but the working file.
	    (begin
	      (delete-file-no-errors workfile)
	      (vc-run-shell-command master '() "co"
				    (rcs-rev-switch "-p" revision)
				    (vc-workfile-pathname master)
				    ">"
				    workfile)
	      (set-file-modes! workfile (if lock? #o644 #o444)))
	    (vc-run-command master '() "co"
			    (rcs-rev-switch (if lock? "-l" "-r") revision)
			    (rcs-mtime-switch master)
			    (vc-workfile-pathname master)))))
    (if (and (not revision) (not workfile))
	(sync-checkout-time! master #t))))

(define-vc-type-operation 'REVERT vc-type:rcs
  (lambda (master revision)
    (with-vc-command-message master "Reverting"
      (lambda ()
	(vc-run-command master '() "co"
			"-f"
			(rcs-rev-switch "-u" revision)
			(rcs-mtime-switch master)
			(vc-workfile-pathname master))))))

(define-vc-type-operation 'CHECKIN vc-type:rcs
  (lambda (master revision comment keep?)
    (with-vc-command-message master "Checking in"
      (lambda ()
	(vc-run-command master '() "ci"
			;; If available, use the secure check-in option.
			(and (vc-release? vc-type:rcs "5.6.4") "-j")
			(rcs-rev-switch (if keep? "-u" "-r") revision)
			(string-append "-m" comment)
			(vc-workfile-pathname master))))))

(define-vc-type-operation 'STEAL vc-type:rcs
  (lambda (master revision)
    (if (not (vc-release? vc-type:rcs "5.6.2"))
	(error "Unable to steal locks with this version of RCS."))
    (with-vc-command-message master "Stealing lock on"
      (lambda ()
	(vc-run-command master '() "rcs"
			"-M"
			(rcs-rev-switch "-u" revision)
			(rcs-rev-switch "-l" revision)
			(vc-workfile-pathname master))))))

(define (rcs-rev-switch switch revision)
  (if revision
      (string-append switch revision)
      switch))

(define (rcs-mtime-switch master)
  (and (ref-variable vc-rcs-preserve-mod-times (vc-workfile-buffer master))
       "-M"))

(define-vc-type-operation 'LOGENTRY-CHECK vc-type:rcs
  (lambda (master log-buffer)
    master log-buffer
    unspecific))

(define-vc-type-operation 'DIFF vc-type:rcs
  (lambda (master rev1 rev2 simple?)
    (let ((type (vc-master-type master))
	  (run-diff
	   (lambda (status brief?)
	     (vc-run-command master
			     `((STATUS ,status)
			       ,@(if simple? `((BUFFER " *vc-diff*")) '()))
			     "rcsdiff"
			     (and brief? "--brief")
			     "-q"
			     (and rev1 (string-append "-r" rev1))
			     (and rev2 (string-append "-r" rev2))
			     (if simple?
				 '()
				 (ref-variable diff-switches
					       (vc-workfile-buffer master)))
			     (vc-workfile-pathname master)))))
      (if (or (not simple?) (vc-type-get type 'RCSDIFF-NO-BRIEF? #f))
	  (run-diff 1 #f)
	  (let ((status (run-diff 2 #t)))
	    (if (= 2 status)
		(begin
		  (vc-type-put! type 'RCSDIFF-NO-BRIEF? #t)
		  (run-diff 1 #f))
		status))))))

(define-vc-type-operation 'PRINT-LOG vc-type:rcs
  (lambda (master)
    (vc-run-command master '() "rlog" (vc-workfile-pathname master))))

(define-vc-type-operation 'DEFAULT-VERSION vc-type:rcs
  (lambda (master error?)
    (let ((delta (rcs-find-delta (get-rcs-admin master) #f error?)))
      (and delta
	   (rcs-delta/number delta)))))

(define-vc-type-operation 'WORKFILE-VERSION vc-type:rcs
  (lambda (master)
    (let ((parse-buffer
	   (lambda (buffer)
	     (let ((start (buffer-start buffer))
		   (end (buffer-end buffer)))
	       (let ((find-keyword
		      (lambda (keyword)
			(let ((mark
			       (search-forward (string-append "$" keyword ":")
					       start end #f)))
			  (and mark
			       (skip-chars-forward " " mark end #f)))))
		     (get-version
		      (lambda (start)
			(let ((end (skip-chars-forward "0-9." start end)))
			  (and (mark< start end)
			       (let ((revision (extract-string start end)))
				 (let ((length (rcs-number-length revision)))
				   (and (> length 2)
					(even? length)
					(rcs-number-head revision
							 (- length 1)
							 #f)))))))))
		 (cond ((or (find-keyword "Id") (find-keyword "Header"))
			=> (lambda (mark)
			     (get-version
			      (skip-chars-forward
			       " "
			       (skip-chars-forward "^ " mark end)
			       end))))
		       ((find-keyword "Revision") => get-version)
		       (else #f)))))))
      (let ((pathname (vc-workfile-pathname master)))
	(let ((buffer (pathname->buffer pathname)))
	  (if buffer
	      (parse-buffer buffer)
	      (call-with-temporary-buffer " *VC-temp*"
		(lambda (buffer)
		  (catch-file-errors (lambda () #f)
		    (lambda ()
		      (read-buffer buffer pathname #f)
		      (parse-buffer buffer)))))))))))

;;;; CVS Commands

(define vc-type:cvs
  (make-vc-type 'CVS "CVS" (string-append "$" "Id" "$")))

(define-vc-master-template
  (lambda (workfile)
    (find-cvs-master workfile)))

(define (find-cvs-master workfile)
  (let* ((entries-file (merge-pathnames "Entries" (cvs-directory workfile)))
	 (master (make-vc-master vc-type:cvs entries-file workfile))
	 (time (file-modification-time-indirect entries-file))
	 (tokens (find-cvs-entry master)))
    (and tokens
	 (begin
	   (vc-master-put! master 'MASTER-TIME time)
	   (vc-master-put! master 'CVS-WORKFILE-VERSION (cadr tokens))
	   (let ((mtime (file-modification-time-indirect workfile)))
	     (if (string=? (file-time->global-ctime-string mtime)
			   (caddr tokens))
		 (set-vc-master-checkout-time! master mtime)
		 (vc-backend-diff master #f #f #t)))
	   master))))

(define (cvs-directory workfile)
  (let ((directory (directory-pathname workfile)))
    (pathname-new-directory directory
			    (append (pathname-directory directory)
				    '("CVS")))))

(define (get-cvs-workfile-version master error?)
  (vc-master-read-cached-value master 'CVS-WORKFILE-VERSION
    (lambda ()
      (let ((tokens (find-cvs-entry master)))
	(if tokens
	    (cadr tokens)
	    (and error?
		 (error "Workfile has no version:"
			(vc-master-workfile master))))))))

(define (find-cvs-entry master)
  (let ((pathname (vc-master-pathname master))
	(name (file-namestring (vc-master-workfile master))))
    (and (file-readable? pathname)
	 (call-with-input-file pathname
	   (lambda (port)
	     (let ((prefix (string-append "/" name "/")))
	       (let loop ()
		 (let ((line (read-line port)))
		   (and (not (eof-object? line))
			(if (string-prefix? prefix line)
			    (let ((tokens (cdr (burst-string line #\/ #f))))
			      (if (fix:= 5 (length tokens))
				  tokens
				  (loop)))
			    (loop)))))))))))

(define (get-cvs-status master)
  (let ((pathname (vc-workfile-pathname master)))
    (vc-run-command master
		    `((DIRECTORY ,(directory-pathname pathname))
		      (BUFFER " *vc-status*"))
		    "cvs" "status" (file-pathname pathname)))
  (let ((m (buffer-start (get-vc-command-buffer))))
    (let ((status
	   (if (re-search-forward "^File: [^ \t]+[ \t]+Status: \\(.*\\)" m)
	       (convert-cvs-status
		(extract-string (re-match-start 1) (re-match-end 1)))
	       'UNKNOWN)))
      (if (eq? 'UP-TO-DATE status)
	  (sync-checkout-time! master #t))
      (values
       status
       (if (re-search-forward
	    "\\(RCS Version\\|RCS Revision\\|Repository revision\\):[ \t]+\\([0-9.]+\\)"
	    m)
	   (extract-string (re-match-start 2) (re-match-end 2))
	   #f)))))

(define (convert-cvs-status status)
  (cond ((string-ci=? status "Up-to-date")
	 'UP-TO-DATE)
	((string-ci=? status "Locally Modified")
	 'LOCALLY-MODIFIED)
	((string-ci=? status "Needs Merge")
	 'NEEDS-MERGE)
	((or (string-ci=? status "Needs Checkout")
	     (string-ci=? status "Needs Patch"))
	 'NEEDS-CHECKOUT)
	((or (string-ci=? status "Unresolved Conflict")
	     (string-ci=? status "File had conflicts on merge"))
	 'UNRESOLVED-CONFLICT)
	((or (string-ci=? status "Locally Added")
	     (string-ci=? status "New file!"))
	 'LOCALLY-ADDED)
	(else
	 'UNKNOWN)))

(define (cvs-rev-switch revision)
  (and revision
       (list "-r" revision)))

(define-vc-type-operation 'VALID? vc-type:cvs
  (lambda (master)
    (get-cvs-workfile-version master #f)))

(define-vc-type-operation 'RELEASE vc-type:cvs
  (lambda ()
    (and (= 0 (vc-run-command #f '() "cvs" "-v"))
	 (re-search-forward "^Concurrent Versions System (CVS) \\([0-9.]+\\)"
			    (buffer-start (get-vc-command-buffer)))
	 (extract-string (re-match-start 1) (re-match-end 1)))))

(define-vc-type-operation 'LOCKING-USER vc-type:cvs
  (lambda (master revision)
    revision
    (let ((workfile (vc-workfile-pathname master)))
      (let ((mtime (file-modification-time-indirect workfile)))
	(and mtime
	     (not (eqv? mtime (vc-master-checkout-time master)))
	     (let ((attr (file-attributes workfile)))
	       (and attr
		    (unix/uid->string (file-attributes/uid attr)))))))))

(define-vc-type-operation 'DEFAULT-VERSION vc-type:cvs
  (lambda (master error?)
    (or (call-with-values (lambda () (get-cvs-status master))
	  (lambda (status revision)
	    status
	    revision))
	(and error?
	     (error "Unable to determine default CVS version:"
		    (vc-workfile-pathname master))))))

(define-vc-type-operation 'WORKFILE-VERSION vc-type:cvs
  (lambda (master)
    (get-cvs-workfile-version master #f)))

(define-vc-type-operation 'CHECK-HEADERS vc-type:cvs
  (lambda (master buffer)
    master
    (check-rcs-headers buffer)))

(define-vc-type-operation 'LIKELY-CONTROL-TYPE? vc-type:cvs
  (lambda (workfile)
    (file-directory? (cvs-directory workfile))))

(define-vc-type-operation 'REGISTER vc-type:cvs
  (lambda (workfile revision comment keep?)
    revision keep?			;always keep file.
    (with-vc-command-message workfile "Registering"
      (lambda ()
	(vc-run-command workfile '() "cvs" "add"
			"-m" comment
			(vc-workfile-pathname workfile))))))

(define-vc-type-operation 'CHECKOUT vc-type:cvs
  (lambda (master revision lock? workfile)
    lock?				;locking not used with CVS
    (cond (workfile
	   ;; CVS makes it difficult to check a file out into anything
	   ;; but the working file.
	   (delete-file-no-errors workfile)
	   (vc-run-shell-command master '() "cvs" "update" "-p"
				 (cvs-rev-switch revision)
				 (vc-workfile-pathname master)
				 ">"
				 workfile))
	  (revision
	   ;; Checkout only necessary for given revision.
	   (vc-run-command master '() "cvs" "update"
			   (cvs-rev-switch revision)
			   (vc-workfile-pathname master))
	   (sync-checkout-time! master #t)))))

(define-vc-type-operation 'REVERT vc-type:cvs
  (lambda (master revision)
     ;; Check out via standard output, so that no sticky tag is set.
    (vc-backend-checkout master revision #f (vc-workfile-pathname master))))

(define-vc-type-operation 'CHECKIN vc-type:cvs
  (lambda (master revision comment keep?)
    keep?
    (bind-condition-handler (list condition-type:editor-error)
	(lambda (condition)
	  condition
	  (if (eq? 'NEEDS-MERGE
		   (call-with-values (lambda () (get-cvs-status master))
		     (lambda (status revision)
		       revision
		       status)))
	      (error "Type C-x 0 C-x C-q to merge in changes.")))
      (lambda ()
	(if (and revision
		 (not (equal? revision (vc-workfile-version master)))
		 (trunk-revision? revision))
	    (vc-run-command master '() "cvs" "commit"
			    "-m" "#intermediate"
			    (vc-workfile-pathname master)))
	(vc-run-command master '() "cvs" "commit"
			(cvs-rev-switch revision)
			"-m" comment
			(vc-workfile-pathname master))))
    ;; If this was an explicit check-in, remove the sticky tag.
    (vc-run-command master '() "cvs" "update" "-A"
		    (vc-workfile-pathname master))))

(define-vc-type-operation 'STEAL vc-type:cvs
  (lambda (master revision)
    master revision
    (error "You cannot steal a CVS lock; there are no CVS locks to steal.")))

(define-vc-type-operation 'LOGENTRY-CHECK vc-type:cvs
  (lambda (master log-buffer)
    master log-buffer
    unspecific))

(define-vc-type-operation 'DIFF vc-type:cvs
  (lambda (master rev1 rev2 simple?)
    (let ((options
	   `((STATUS 1)
	     ,@(if simple? `((BUFFER " *vc-diff*")) '()))))
      (if (equal? "0" (vc-workfile-version master))
	  ;; This file is added but not yet committed; there is no
	  ;; master file.
	  (begin
	    (if (or rev1 rev2)
		(error "No revisions exist:" (vc-workfile-pathname master)))
	    (if simple?
		;; File is added but not committed; we regard this as
		;; "changed".
		1
		;; Diff against /dev/null.
		(vc-run-command master options "diff"
				(ref-variable diff-switches
					      (vc-workfile-buffer master))
				"/dev/null"
				(vc-workfile-pathname master))))
	  (vc-run-command master options "cvs" "diff"
			  (and rev1 (string-append "-r" rev1))
			  (and rev2 (string-append "-r" rev2))
			  (if simple?
			      '()
			      (ref-variable diff-switches
					    (vc-workfile-buffer master)))
			  (vc-workfile-pathname master))))))

(define-vc-type-operation 'PRINT-LOG vc-type:cvs
  (lambda (master)
    (vc-run-command master '() "cvs" "log" (vc-workfile-pathname master))))

;;;; Command Execution

(define (vc-run-command master options command . arguments)
  (let ((option
	 (lambda (name default)
	   (let ((option (assq name options)))
	     (if option
		 (cadr option)
		 (default))))))
  (let ((command-messages?
	 (ref-variable vc-command-messages
		       (and master (vc-workfile-buffer master))))
	(msg
	 (string-append "Running " command
			(if master
			    (string-append " on " (vc-workfile-string master))
			    "")
			"..."))
	(status-limit (option 'STATUS (lambda () 0)))
	(directory (option 'DIRECTORY working-directory-pathname))
	(command-buffer
	 (let ((buffer (option 'BUFFER get-vc-command-buffer)))
	   (cond ((string? buffer) (find-or-create-buffer buffer))
		 ((buffer? buffer) buffer)
		 (else (error "Illegal buffer:" buffer))))))
    (if command-messages? (message msg))
    (buffer-reset! command-buffer)
    (bury-buffer command-buffer)
    (set-buffer-default-directory! command-buffer directory)
    (let ((result
	   (apply run-synchronous-process
		  #f
		  (buffer-end command-buffer)
		  directory
		  #f
		  (os/find-program command directory
				   (ref-variable exec-path command-buffer))
		  (vc-command-arguments arguments))))
      (if (and (eq? 'EXITED (car result))
	       (<= 0 (cdr result) status-limit))
	  (begin
	    (if command-messages? (message msg "done"))
	    (cdr result))
	  (begin
	    (pop-up-vc-command-buffer #f)
	    (editor-error "Running " command "...FAILED "
			  (list (car result) (cdr result)))))))))

(define (vc-command-arguments arguments)
  (append-map (lambda (argument)
		(cond ((not argument) '())
		      ((string? argument) (list argument))
		      ((pathname? argument) (list (->namestring argument)))
		      ((list? argument) (vc-command-arguments argument))
		      (else (error "Ill-formed command argument:" argument))))
	      arguments))

(define (vc-run-shell-command master options command . arguments)
  (vc-run-command master options "/bin/sh" "-c"
		  (reduce string-append-separated
			  ""
			  (vc-command-arguments (cons command arguments)))))

(define (pop-up-vc-command-buffer select?)
  (let ((command-buffer (get-vc-command-buffer)))
    (set-buffer-point! command-buffer (buffer-start command-buffer))
    (pop-up-buffer command-buffer select?)))

(define (get-vc-command-buffer)
  (find-or-create-buffer "*vc*"))

(define (with-vc-command-message master operation thunk)
  (let ((msg (string-append operation " " (vc-workfile-string master) "...")))
    (message msg)
    (thunk)
    (message msg "done")))

;;;; Workfile Utilities

(define (vc-keep-workfiles? master)
  (or (eq? vc-type:cvs (vc-master-type master))
      (ref-variable vc-keep-workfiles (vc-workfile-buffer master))))

(define (vc-update-workfile-buffer master keep?)
  ;; Depending on VC-KEEP-WORKFILES, either revert the workfile
  ;; buffer to show the updated workfile, or kill the buffer.
  (let ((buffer (vc-workfile-buffer master)))
    (if buffer
	(if keep?
	    (vc-revert-buffer buffer #t)
	    (kill-buffer buffer)))))

(define (vc-get-version revision prompt)
  (vc-normalize-version (if (or (not revision) (string? revision))
			    revision
			    (prompt-for-string prompt #f))))

(define (vc-normalize-version revision)
  (and revision
       (not (string-null? revision))
       revision))

(define (vc-workfile-buffer master)
  (pathname->buffer (vc-workfile-pathname master)))

(define (vc-workfile-string master)
  (->namestring (vc-workfile-pathname master)))

(define (vc-workfile-pathname master)
  (if (vc-master? master)
      (vc-master-workfile master)
      master))

(define (vc-workfile-modified? master)
  (let ((mod-time
	 (file-modification-time-indirect (vc-workfile-pathname master))))
    (cond ((not mod-time) #f)
	  ((eqv? (vc-master-checkout-time master) mod-time) #f)
	  (else (not (= 0 (vc-backend-diff master #f #f #t)))))))

(define (vc-save-workfile-buffer master)
  (let ((buffer (vc-workfile-buffer master)))
    (if buffer
	(vc-save-buffer buffer))))

(define (vc-save-buffer buffer)
  (if (buffer-modified? buffer)
      (begin
	(if (not (or (ref-variable vc-suppress-confirm buffer)
		     (prompt-for-confirmation?
		      (string-append "Buffer "
				     (buffer-name buffer)
				     " modified; save it"))))
	    (editor-error "Aborted"))
	(save-buffer buffer #f))))

(define (vc-revert-workfile-buffer master dont-confirm?)
  (let ((buffer (vc-workfile-buffer master)))
    (if buffer
	(vc-revert-buffer buffer dont-confirm?))))

(define (vc-revert-buffer buffer dont-confirm?)
  ;; Revert BUFFER, try to keep point and mark where user expects them
  ;; in spite of changes due to expanded version-control keywords.
  (let ((point-contexts
	 (map (lambda (window)
		(list window
		      (vc-mark-context (window-point window))
		      (vc-mark-context (window-start-mark window))))
	      (buffer-windows buffer)))
	(point-context (vc-mark-context (buffer-point buffer)))
	(mark-context (vc-mark-context (buffer-mark buffer))))
    (revert-buffer buffer #t dont-confirm?)
    (update-screens! '(IGNORE-INPUT NO-SCREEN-OUTPUT))
    (if (null? point-contexts)
	(let ((m (vc-find-context buffer point-context)))
	  (if m
	      (set-buffer-point! buffer m)))
	(for-each (lambda (entry)
		    (let ((window (car entry)))
		      (if (and (window-live? window)
			       (eq? buffer (window-buffer window)))
			  (begin
			    (let ((m (vc-find-context buffer (caddr entry))))
			      (if m
				  (set-window-start-mark! window m #t)))
			    (let ((m (vc-find-context buffer (cadr entry))))
			      (if m
				  (set-window-point! window m)))))))
		  point-contexts))
    (let ((m (vc-find-context buffer mark-context)))
      (if m
	  (set-buffer-mark! buffer m)))))

(define (vc-mark-context mark)
  (let ((group (mark-group mark))
	(index (mark-index mark)))
    (let ((length (group-length group)))
      (vector index
	      length
	      (group-extract-string group index (min length (+ index 100)))))))

(define (vc-find-context buffer context)
  (let ((group (buffer-group buffer))
	(index (vector-ref context 0))
	(string (vector-ref context 2)))
    (let ((length (group-length group)))
      (if (string-null? string)
	  (group-end-mark group)
	  (and (or (and (< index length)
			(search-forward string
					(make-mark group index)
					(make-mark group length)))
		   (let ((index
			  (- index
			     (abs (- (vector-ref context 1) length))
			     (string-length string))))
		     (and (<= 0 index length)
			  (search-forward string
					  (make-mark group index)
					  (make-mark group length)))))
	       (let ((mark (re-match-start 0)))
		 (cond ((mark< mark (group-start-mark group))
			(group-start-mark group))
		       ((mark> mark (group-end-mark group))
			(group-end-mark group))
		       (else mark))))))))