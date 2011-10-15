#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; Version Control

;;; Adapted from "vc.el" in Emacs 19.22.
;;; Updated March 2000 from "vc.el" in Emacs 20.6.

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
  ;;(variable-permanent-local! variable)
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

(define-variable vc-delete-logbuf-window
  "If true, delete the *VC-log* buffer and window after each logical action.
If false, bury that buffer instead.
This is most useful if you have multiple windows on a frame and would like to
preserve the setting."
  #t
  boolean?)

(define-variable vc-initial-comment
  "Prompt for initial comment when a file is registered."
  #f
  boolean?)

(define-variable vc-default-init-version
  "A string used as the default version number when a new file is registered.
This can be overriden by giving a prefix argument to \\[vc-register]."
  "1.1"
  string?)

(define-variable vc-command-messages
  "If true, display run messages from back-end commands."
  #f
  boolean?)

(define-variable vc-checkin-hooks
  "An event distributor that is invoked after a checkin is done."
  (make-event-distributor))

(define (user-is-root?)
  (and (eq? microcode-id/operating-system 'unix)
       (= (unix/current-uid) 0)))

(define-variable vc-checkout-carefully
  "True means be extra-careful in checkout.
Verify that the file really is not locked
and that its contents match what the master file says."
  ;; Default is to be extra careful for super-user.
  user-is-root?
  (lambda (object)
    (or (boolean? object)
	(and (procedure? object)
	     (procedure-arity-valid? object 0)))))

(define-variable vc-log-mode-hook
  "An event distributor that is invoked when entering VC-log mode."
  (make-event-distributor))

(define-variable vc-follow-symlinks
  "Indicates what to do if you visit a symbolic link to a file
that is under version control.  Editing such a file through the
link bypasses the version control system, which is dangerous and
probably not what you want.  
  If this variable is #t, VC follows the link and visits the real file,
telling you about it in the echo area.  If it is `ask', VC asks for
confirmation whether it should follow the link.  If #f, the link is
visited and a warning displayed."
  'ASK
  (lambda (object) (or (boolean? object) (eq? 'ASK object))))

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

(define-variable vc-cvs-stay-local
  "If true, use only the CVS timestamp to tell if a file has been modified.
Otherwise, VC will compare the file to the copy in the repository."
  #t
  boolean?)

;;;; VC-TYPE datatype

(define-structure (vc-type (constructor %make-vc-type
					(name display-name header-keyword))
			   safe-accessors)
  (name #f read-only #t)		;a symbol
  (display-name #f read-only #t)	;a string
  (header-keyword #f read-only #t)	;a string
  (operations (make-1d-table) read-only #t)
  (properties (make-1d-table) read-only #t))

(define (vc-type-get type key default)
  (1d-table/get (vc-type-properties type) key default))

(define (vc-type-put! type key value)
  (1d-table/put! (vc-type-properties type) key value))

(define (vc-type-remove! type key)
  (1d-table/remove! (vc-type-properties type) key))

(define (make-vc-type name display-name header-keyword)
  (let ((type (%make-vc-type name display-name header-keyword)))
    (let loop ((types vc-types))
      (if (pair? types)
	  (if (eq? name (vc-type-name (car types)))
	      (set-car! types type)
	      (loop (cdr types)))
	  (set! vc-types (cons type vc-types))))
    type))

(define vc-types '())

(define (define-vc-type-operation name type procedure)
  (1d-table/put! (vc-type-operations type) name procedure))

(define (vc-type-operation type name #!optional error?)
  (or (1d-table/get (vc-type-operations type) name #f)
      (begin
	(if error?
	    (error:bad-range-argument name 'VC-TYPE-OPERATION))
	#f)))

(define (vc-call name master . arguments)
  (apply (vc-type-operation (vc-master-type master) name) master arguments))

;;;; VC-MASTER datatype

(define-structure (vc-master (constructor make-vc-master
					  (type pathname workfile))
			     safe-accessors)
  (type #f read-only #t)		;a VC-TYPE object
  (pathname #f read-only #t)		;a PATHNAME object
  (workfile #f read-only #t)		;a PATHNAME object
  (properties (make-1d-table) read-only #t))

(define (vc-master-get master key default)
  (1d-table/get (vc-master-properties master) key default))

(define (vc-master-put! master key value)
  (1d-table/put! (vc-master-properties master) key value))

(define (vc-master-remove! master key)
  (1d-table/remove! (vc-master-properties master) key))

(define (read-cached-value-1 master key pathname read-value)
  (let loop ()
    (let ((v.t (vc-master-get master key #f))
	  (time (file-modification-time pathname)))
      (if (and v.t (eqv? time (cdr v.t)))
	  (car v.t)
	  (begin
	    (vc-master-put! master key (cons (read-value time) time))
	    (loop))))))
#|
(define (cache-value-1! master key pathname read-value)
  (let ((time (file-modification-time pathname)))
    (let ((value (read-value)))
      (vc-master-put! master key (cons value time))
      value)))
|#
(define (read-cached-value-2 master key p1 p2 read-value)
  (let loop ()
    (let ((vtt (vc-master-get master key #f))
	  (t1 (file-modification-time p1))
	  (t2 (file-modification-time p2)))
      (if (and vtt
	       (eqv? t1 (vector-ref vtt 1))
	       (eqv? t2 (vector-ref vtt 2)))
	  (vector-ref vtt 0)
	  (begin
	    (vc-master-put! master key (vector (read-value t1 t2) t1 t2))
	    (loop))))))

(define (cache-value-2! master key p1 p2 read-value)
  (let ((t1 (file-modification-time p1))
	(t2 (file-modification-time p2)))
    (let ((value (read-value t1 t2)))
      (vc-master-put! master key (vector value t1 t2))
      value)))

;;;; Editor Hooks

(set-variable! find-file-hooks
	       (append! (ref-variable find-file-hooks)
			(list (lambda (buffer) (vc-hook:find-file buffer)))))

(define (vc-hook:find-file buffer)
  (cond ((buffer-vc-master buffer #f)
	 => (lambda (master)
	      (vc-mode-line master buffer)
	      (if (not (ref-variable vc-make-backup-files buffer))
		  (local-set-variable! make-backup-files #f buffer))
	      buffer))
	((let ((pathname (buffer-pathname buffer)))
	   (and (file-symbolic-link? pathname)
		(file-vc-master (file-chase-links pathname) #f)))
	 => (lambda (master)
	      (let ((workfile (vc-master-workfile master))
		    (type (vc-type-display-name (vc-master-type master))))
		(let ((follow
		       (lambda ()
			 (kill-buffer buffer)
			 (let ((buffer*
				(or (pathname->buffer workfile)
				    (find-file-noselect workfile #f))))
			   (message "Followed link to "
				    (->namestring workfile))
			   buffer*))))
		  (case (ref-variable vc-follow-symlinks buffer)
		    ((#F)
		     (message "Warning: symbolic link to "
			      type
			      "-controlled source file"))
		    ((ASK)
		     (if (or (pathname->buffer workfile)
			     (prompt-for-yes-or-no?
			      (string-append
			       "Symbolic link to "
			       type
			       "-controlled source file; follow link")))
			 (follow)
			 (begin
			   (message
			    "Warning: editing through the link bypasses version control.")
			   buffer)))
		    (else (follow)))))))
	(else buffer)))

(set-variable!
 find-file-not-found-hooks
 (append! (ref-variable find-file-not-found-hooks)
	  (list (lambda (buffer) (vc-hook:find-file-not-found buffer)))))

(define (vc-hook:find-file-not-found buffer)
  (let ((master (buffer-vc-master buffer #f)))
    (and master
	 (call-with-current-continuation
	  (lambda (k)
	    (bind-condition-handler (list condition-type:error)
		(lambda (condition) condition (k #f))
	      (lambda ()
		(vc-checkout master #f)
		#t)))))))

(add-event-receiver! event:after-buffer-save
		     (lambda (buffer) (vc-hook:after-buffer-save buffer)))

(define (vc-hook:after-buffer-save buffer)
  (let ((master (buffer-vc-master buffer #f)))
    (if master
	(vc-mode-line master buffer))))

(add-event-receiver! event:set-buffer-pathname
		     (lambda (buffer) (vc-hook:set-buffer-pathname buffer)))

(define (vc-hook:set-buffer-pathname buffer)
  (buffer-remove! buffer 'VC-MASTER))

(add-event-receiver! event:set-buffer-major-mode
		     (lambda (buffer) (vc-hook:set-buffer-major-mode buffer)))

(define (vc-hook:set-buffer-major-mode buffer)
  (let ((master (buffer-vc-master buffer #f)))
    (if master
	(begin
	  (vc-mode-line master buffer)
	  (if (not (ref-variable vc-make-backup-files buffer))
	      (local-set-variable! make-backup-files #f buffer))))))

;;;; Mode line

(define (vc-mode-line master buffer)
  (let ((buffer (or buffer (vc-workfile-buffer master #f))))
    (set-variable! vc-mode-line-status
		   (vc-backend-mode-line-status master buffer)
		   buffer)
    (buffer-modeline-event! buffer 'VC-MODE-LINE-STATUS)))

(define (%default-mode-line-status master buffer)
  (string-append
   " "
   (vc-type-display-name (vc-master-type master))
   (if (ref-variable vc-display-status buffer)
       (let ((revision
	      (or (vc-backend-workfile-revision master)
		  (vc-backend-default-revision master))))
	 (let ((locker (vc-backend-locking-user master revision))
	       (user-name (current-user-name)))
	   (if revision
	       (string-append
		(cond ((not locker) "-")
		      ((string=? locker user-name) ":")
		      (else (string-append ":" locker ":")))
		revision)
	       " @@")))
       "")))

;;;; VC-MASTER association

(define (current-vc-master error?)
  (buffer-vc-master (selected-buffer) error?))

(define (buffer-vc-master buffer error?)
  (let ((buffer (chase-parent-buffer buffer)))
    (let ((master (buffer-get buffer 'VC-MASTER #f)))
      (if (and master (vc-backend-master-valid? master))
	  master
	  (begin
	    (buffer-remove! buffer 'VC-MASTER)
	    (if (vc-dired-buffer? buffer)
		(let ((workfile (dired-this-file buffer error?)))
		  (and workfile
		       (file-vc-master workfile error?)))
		(let ((workfile (buffer-pathname buffer)))
		  (if workfile
		      (let ((master (%file-vc-master workfile error?)))
			(if master (buffer-put! buffer 'VC-MASTER master))
			master)
		      (and error? (vc-registration-error buffer))))))))))

(define (chase-parent-buffer buffer)
  (let loop ((buffer buffer))
    (let ((buffer* (buffer-get buffer 'VC-PARENT-BUFFER #f)))
      (if buffer*
	  (loop buffer*)
	  buffer))))

(define (file-vc-master workfile error?)
  (let ((workfile (->pathname workfile)))
    (let ((buffer (pathname->buffer workfile)))
      (if buffer
	  (buffer-vc-master buffer error?)
	  (%file-vc-master workfile error?)))))

(define (%file-vc-master workfile error?)
  (let ((workfile (->pathname workfile)))
    (or (vc-backend-find-master workfile)
	(and error? (vc-registration-error workfile)))))

(define (guarantee-vc-master-valid master)
  (if (not (vc-backend-master-valid? master))
      (error "VC master file disappeared:" (vc-master-pathname master))))

(define (vc-registration-error object)
  (if (buffer? object)
      (editor-error "Buffer " (buffer-name object)
		    " is not associated with a file.")
      (editor-error "File " (->namestring object)
		    " is not under version control.")))

;;;; Primary Commands

(define-command vc-toggle-read-only
  "Change read-only status of current buffer, perhaps via version control.
If the buffer is visiting a file registered with version control,
then check the file in or out.  Otherwise, just change the read-only flag
of the buffer."
  ()
  (lambda ()
    (if (current-vc-master #f)
	((ref-command vc-next-action) #f)
	((ref-command toggle-read-only)))))

(define-command vc-next-action
  "Do the next logical checkin or checkout operation on the current file.
   If you call this from within a VC dired buffer with no files marked,
it will operate on the file in the current line.
   If you call this from within a VC dired buffer, and one or more
files are marked, it will accept a log message and then operate on
each one.  The log message will be used as a comment for any register
or checkin operations, but ignored when doing checkouts.  Attempted
lock steals will raise an error.
   A prefix argument lets you specify the version number to use.

For RCS files:
   If the file is not already registered, this registers it for version
control.
   If the file is registered and not locked by anyone, this checks out
a writable and locked file ready for editing.
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

For CVS files:
   If the file is not already registered, this registers it for version
control.  This does a \"cvs add\", but no \"cvs commit\".
   If the file is added but not committed, it is committed.
   If your working file is changed, but the repository file is
unchanged, this pops up a buffer for entry of a log message; when the
message has been entered, it checks in the resulting changes along
with the logmessage as change commentary.  A writable file is retained.
   If the repository file is changed, you are asked if you want to
merge in the changes into your working copy."
  "P"
  (lambda (revision?)
    (let ((buffer (selected-buffer)))
      (if (vc-dired-buffer? buffer)
	  (vc-next-action-dired buffer)
	  (vc-next-action-on-file (or (buffer-pathname buffer)
				      (vc-registration-error buffer))
				  #f revision? #f)))))

(define-command vc-register
  "Register the current file into your version-control system."
  "P"
  (lambda (revision?)
    (let ((workfile
	   (let ((buffer (selected-buffer)))
	     (or (if (vc-dired-buffer? buffer)
		     (dired-this-file buffer #t)
		     (buffer-pathname buffer))
		 (vc-registration-error buffer)))))
      (if (file-vc-master workfile #f)
	  (editor-error "This file is already registered."))
      (vc-register workfile revision? #f #f))))

(define (vc-next-action-on-file workfile from-dired? revision? comment)
  (let ((master (file-vc-master workfile #f)))
    (if master
	(let ((do-checkin
	       (lambda ()
		 (let* ((buffer
			 (let ((buffer (pathname->buffer workfile)))
			   (and buffer
				(find-file-revert buffer))))
			(shown? #f)
			(show
			 (lambda ()
			   (if (not shown?)
			       (begin
				 (if from-dired?
				     (pop-up-buffer buffer #f
						    '(NOT-CURRENT-WINDOW))
				     (select-buffer buffer))
				 (set! shown? #t))))))
		   ;; If the file on disk is newer, then the user just
		   ;; said no to rereading it.  So the user probably
		   ;; wishes to overwrite the file with the buffer's
		   ;; contents, and check that in.
		   (cond ((not buffer) unspecific)
			 ((verify-visited-file-modification-time? buffer)
			  (vc-save-buffer buffer #t))
			 ((begin
			    (show)
			    (prompt-for-yes-or-no?
			     "Replace file on disk with buffer contents"))
			  (save-buffer buffer #f))
			 (else (editor-error "Aborted")))
		   ;; Revert if file is unchanged and buffer is too.
		   ;; If buffer is modified, that means the user just
		   ;; said no to saving it; in that case, don't
		   ;; revert, because the user might intend to save
		   ;; after finishing the log entry.
		   (cond ((or (and buffer (buffer-modified? buffer))
			      (vc-backend-workfile-modified? master))
			  (vc-checkin master revision? comment))
			 ;; DO NOT revert the file without asking the
			 ;; user!
			 ((prompt-for-yes-or-no?
			   (if buffer
			       (begin (show) "Revert to master version")
			       (string-append "Revert "
					      (file-namestring workfile)
					      " to master version")))
			  (vc-backend-revert master)
			  (if buffer (vc-revert-buffer buffer #t)))))))
	      (do-checkout
	       (lambda ()
		 (vc-save-workfile-buffer workfile)
		 (vc-checkout master revision?))))
	  (let ((keyword (vc-backend-next-action master)))
	    (case keyword
	      ((CHECKIN)
	       (do-checkin))
	      ((CHECKOUT)
	       (do-checkout))
	      ((UNMODIFIED)
	       (cond (revision?
		      (do-checkout))
		     ((not from-dired?)
		      (message (buffer-name (vc-workfile-buffer master #f))
			       " is up to date."))))
	      ((MERGE)
	       (vc-next-action-merge master from-dired?))
	      ((PENDING-MERGE)
	       (message (->namestring workfile) " has a pending merge."))
	      ((RESOLVE-CONFLICT)
	       (message (->namestring workfile)
			" has an unresolved conflict."))
	      ((STEAL-LOCK)
	       (vc-steal-lock master revision? comment
			      (vc-backend-locking-user master #f)))
	      (else
	       (error "Unknown next action keyword:" keyword)))))
	(vc-register workfile revision? comment 'LOCK))))

(define (vc-next-action-dired buffer)
  (let ((files
	 (let ((files (dired-marked-files buffer)))
	   (if (pair? files)
	       files
	       (dired-next-files 1 buffer)))))
    (if (pair? files)
	(if (pair? (cdr files))
	    (vc-start-entry
	     buffer
	     "Enter a change comment for the marked files."
	     (if (there-exists? files
		   (lambda (file)
		     (let ((master (file-vc-master (car file) #f)))
		       (and master
			    (eq? (vc-backend-next-action master) 'CHECKIN)))))
		 #f
		 "")
	     (lambda (comment)
	       (for-each-dired-mark buffer
		 (lambda (file)
		   (let ((msg
			  (string-append "Processing "
					 (->namestring file)
					 "...")))
		     (message msg)
		     (vc-next-action-on-file file #t #f comment)
		     (message msg "done")))))
	     #f)
	    (vc-next-action-on-file (caar files) #t #f #f)))))

(define (vc-register workfile revision? comment keep?)
  (let ((buffer (pathname->buffer workfile)))
    (let ((revision
	   (or (vc-get-revision revision?
				(string-append "Initial version level for "
					       (->namestring workfile)))
	       (ref-variable vc-default-init-version buffer))))
      ;; Watch out for new buffers of size 0: the corresponding file
      ;; does not exist yet, even though buffer-modified? is false.
      (if (and buffer
	       (not (buffer-modified? buffer))
	       (= 0 (buffer-length buffer))
	       (not (file-exists? workfile)))
	  (buffer-modified! buffer))
      (vc-save-workfile-buffer workfile)
      (vc-start-entry workfile "Enter initial comment."
		      (or comment
			  (if (ref-variable vc-initial-comment buffer) #f ""))
		      (let ((keep?
			     (or keep?
				 (ref-variable vc-keep-workfiles buffer))))
			(lambda (comment)
			  (vc-backend-register workfile revision comment keep?)
			  (vc-resync-workfile-buffer workfile keep?)))
		      #f))))

(define (vc-checkout master revision?)
  (let ((revision (vc-get-revision revision? "Branch or version to move to")))
    (let ((do-it
	   (lambda ()
	     (vc-backend-checkout master revision #t #f)
	     (vc-revert-workfile-buffer master #t))))
      (cond ((not (and (let ((value (ref-variable vc-checkout-carefully)))
			 (if (boolean? value) value (value)))
		       (vc-backend-workfile-modified? master)))
	     (do-it))
	    ((cleanup-pop-up-buffers
	      (lambda ()
		(run-diff master #f #f)
		(insert-string
		 (string-append "Changes to "
				(vc-workfile-string master)
				" since last lock:\n\n")
		 (buffer-start (get-vc-diff-buffer #f)))
		(pop-up-vc-diff-buffer #f)
		(editor-beep)
		(prompt-for-yes-or-no?
		 "File has unlocked changes, claim lock retaining changes")))
	     (guarantee-vc-master-valid master)
	     (vc-backend-steal master revision)
	     (let ((buffer (vc-workfile-buffer master #f)))
	       (if buffer
		   (vc-mode-line master buffer))))
	    ((prompt-for-yes-or-no? "Revert to checked-in version, instead")
	     (do-it))
	    (else
	     (editor-error "Checkout aborted."))))))

(define (vc-checkin master revision? comment)
  (let ((revision (vc-get-revision revision? "New version level")))
    (vc-save-workfile-buffer (vc-master-workfile master))
    (vc-start-entry master "Enter a change comment." comment
		    (let ((keep? (vc-backend-keep-workfiles? master)))
		      (lambda (comment)
			(vc-backend-checkin master revision
					    (if (blank-string? comment)
						"*** empty log message ***"
						comment)
					    keep?)
			(vc-resync-workfile-buffer (vc-master-workfile master)
						   keep?)))
		    (lambda ()
		      (event-distributor/invoke!
		       (ref-variable vc-checkin-hooks
				     (vc-workfile-buffer master #f))
		       master)))))

(define (vc-steal-lock master revision? comment owner)
  (if (and (rcs-master? master)
	   (not (vc-release? vc-type:rcs "5.6.2")))
      ;; Can't steal locks with old RCS versions.
      (editor-error "File is locked by " owner "."))
  (let ((filename (vc-workfile-string master)))
    (if comment
	(editor-error "Sorry, you can't steal the lock on "
		      filename
		      " this way."))
    (let ((revision (vc-get-revision revision? "Version level to steal")))
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
	(let ((mail-buffer (selected-buffer)))
	  (insert-string
	   (string-append "I stole the lock on " file:rev ", "
			  (universal-time->string (get-universal-time))
			  ".\n")
	   (buffer-end mail-buffer))
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

(define (vc-next-action-merge master from-dired?)
  (let ((buffer (vc-workfile-buffer master #f)))
    ;; (NOT FROM-DIRED?) implies (NOT (NOT BUFFER)).
    (if (or from-dired?
	    (prompt-for-yes-or-no?
	     (string-append
	      (buffer-name buffer)
	      " is not up-to-date.  Merge in changes now")))
	(begin
	  (if (and buffer (buffer-modified? buffer))
	      (begin
		(if from-dired?
		    (select-buffer-other-window buffer)
		    (select-buffer buffer))
		(vc-save-buffer buffer #f)))
	  (if (and buffer
		   (buffer-modified? buffer)
		   (not
		    (prompt-for-yes-or-no?
		     (string-append
		      "Buffer "
		      (buffer-name buffer)
		      " modified; merge file on disc anyhow"))))
	      (editor-error "Merge aborted"))
	  (let ((conflicts? (cvs-backend-merge-news master)))
	    (if buffer
		(vc-revert-buffer buffer #t))
	    (if (and conflicts?
		     (prompt-for-confirmation?
		      "Conflicts detected.  Resolve them now"))
		(find-file (vc-master-workfile master)))))
	(editor-error (buffer-name buffer) " needs update."))))

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
  "Report diffs between two stored versions REV1 and REV2 of a file."
  (lambda ()
    (let* ((workfile
	   (prompt-for-existing-file
	    "File to diff"
	    (let ((pathname (buffer-pathname (selected-buffer))))
	      (and pathname
		   (list pathname)))))
	   (master (file-vc-master workfile #t))
	   (revision (vc-backend-workfile-revision master)))
      (call-with-values
	  (lambda ()
	    (let ((previous
		   (and (not (vc-backend-workfile-modified? master))
			(previous-revision revision))))
	      (if previous
		  (values previous revision)
		  (values revision #f))))
	(lambda (default1 default2)
	  (let* ((rev1 (prompt-for-string "Older version" default1))
		 (rev2
		  (prompt-for-string "Newer version" default2
				     'DEFAULT-TYPE 'NULL-DEFAULT)))
	    (list workfile rev1 rev2))))))
  (lambda (workfile rev1 rev2)
    (if (file-directory? workfile)
	(editor-error "Directory diffs not yet supported.")
	(vc-diff (file-vc-master workfile #t) rev1 rev2))))

(define (vc-diff master rev1 rev2)
  (if (not (and rev1 rev2))
      (vc-save-workfile-buffer (vc-master-workfile master)))
  (let ((rev1 (vc-normalize-revision rev1))
	(rev2 (vc-normalize-revision rev2)))
    (if (and (or rev1 rev2 (vc-backend-workfile-modified? master))
	     (run-diff master rev1 rev2))
	(begin
	  (pop-up-vc-diff-buffer #t)
	  #f)
	(begin
	  (message "No changes to "
		   (vc-workfile-string master)
		   (if (and rev1 rev2)
		       (string-append " between " rev1 " and " rev2)
		       (string-append " since "
				      (or rev1 rev2 "latest version")))
		   ".")
	  #t))))

(define (run-diff master rev1 rev2)
  (if (and (not rev1) (not rev2))
      (cache-value-2! master 'MODIFIED?
		      (vc-master-pathname master)
		      (vc-master-workfile master)
	(lambda (tm tw)
	  (let ((modified? (vc-backend-diff master rev1 rev2 #f)))
	    (if (cvs-master? master)
		(set-vc-cvs-workfile-mtime-string! master tm tw modified?))
	    modified?)))
      (vc-backend-diff master rev1 rev2 #f)))

(define-command vc-version-other-window
  "Visit version REV of the current buffer in another window.
If the current buffer is named `F', the version is named `F.~REV~'.
If `F.~REV~' already exists, it is used instead of being re-created."
  "sVersion to visit (default is latest version)"
  (lambda (revision)
    (let* ((master (current-vc-master #t))
	   (revision
	    (or (vc-normalize-revision revision)
		(vc-backend-workfile-revision master)
		(vc-backend-default-revision master)))
	   (workfile
	    (string-append (vc-workfile-string master) ".~" revision "~")))
      (if (not (file-exists? workfile))
	  (vc-backend-checkout master revision #f workfile))
      (find-file-other-window workfile))))

(define-command vc-insert-headers
  "Insert headers in a file for use with your version-control system.
Headers are inserted at the start of the buffer."
  ()
  (lambda ()
    (let* ((master (current-vc-master #t))
	   (buffer (vc-workfile-buffer master #t)))
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
	       (buffer-start buffer))))))))

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
    (let* ((master (current-vc-master #t))
	   (buffer (vc-workfile-buffer master #t)))
      (if (or (and (vc-backend-workfile-modified? master)
		   (or (ref-variable vc-suppress-confirm)
		       (cleanup-pop-up-buffers
			(lambda ()
			  (run-diff master #f #f)
			  (pop-up-vc-diff-buffer #f)
			  (prompt-for-yes-or-no? "Discard changes")))))
	      (and (cvs-master? master)
		   (cvs-file-edited? master)))
	  (begin
	    (vc-backend-revert master)
	    (vc-revert-buffer buffer #t))
	  (editor-error "Revert cancelled.")))))

;;;; VC Dired

(define-command vc-directory
  "Show version-control status of files under a directory.
Normally shows only locked files; prefix arg says to show all files."
  "DDired under VC (directory)\nP"
  (lambda (directory all-files?)
    (let ((buffer (vc-dired directory all-files?)))
      (if (group-end? (line-start (buffer-start buffer) 1 'LIMIT))
	  (begin
	    (if (not (buffer-visible? buffer))
		(kill-buffer buffer))
	    (message "No files are currently "
		     (if all-files? "registered" "locked")
		     " in "
		     (->namestring directory)))
	  (pop-up-buffer buffer #t)))))

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
	  (let ((spec (buffer-get buffer 'VC-DIRECTORY-SPEC #f)))
	    (and spec
		 (pathname=? (car spec) directory)))))
      (new-buffer (pathname->buffer-name directory))))

(define (fill-vc-dired-buffer! buffer directory all-files?)
  (let ((msg
	 (string-append "Reading directory " (->namestring directory) "...")))
    (buffer-reset! buffer)
    (set-buffer-major-mode! buffer (ref-mode-object vc-dired))
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
  (let ((spec (buffer-get buffer 'VC-DIRECTORY-SPEC #f)))
    (if spec
	(begin
	  (fill-vc-dired-buffer! buffer (car spec) (cdr spec))
	  buffer)
	(revert-buffer-default buffer dont-use-auto-save? dont-confirm?))))

(define (generate-vc-dired-lines directory all-files? mark)
  (for-each
   (lambda (file)
     (let ((attr (file-attributes-direct file)))
       (if (and attr (not (file-attributes/type attr)))
	   (let ((status
		  (let ((master (file-vc-master file #f)))
		    (and master
			 (vc-backend-workfile-status-string master)))))
	     (if (or status all-files?)
		 (generate-vc-dired-line file attr status mark))))))
   (directory-read directory)))

(define (generate-vc-dired-line file attr status mark)
  (insert-string
   (string-append
    "  "
    (file-attributes/mode-string attr)
    " "
    (pad-on-right-to (if status (string-append "(" status ")") "") 10)
    " "
    (file-time->ls-string (file-attributes/modification-time attr))
    " "
    (file-namestring file)
    "\n")
   mark))

(define-major-mode vc-dired dired "VC-Dired"
  "The major mode used in VC directory buffers.  It works like Dired,
but lists only files under version control, with the current VC state of 
each file being indicated in the place of the file's link count, owner, 
group and size.  Subdirectories are also listed, and you may insert them 
into the buffer as desired, as in Dired.
  All Dired commands operate normally, with the exception of `v', which
is redefined as the version control prefix, so that you can type 
`vl', `v=' etc. to invoke `vc-print-log', `vc-diff', and the like on
the file named in the current Dired buffer line.  `vv' invokes
`vc-next-action' on this file, or on all files currently marked.
There is a special command, `*l', to mark all files currently locked.

\\{vc-dired}"
  (lambda (buffer)
    buffer
    unspecific))

(define (vc-dired-buffer? buffer)
  (eq? (ref-mode-object vc-dired) (buffer-major-mode buffer)))

(define-key 'vc-dired '(#\v #\h) 'vc-insert-headers)
(define-key 'vc-dired '(#\v #\i) 'vc-register)
(define-key 'vc-dired '(#\v #\l) 'vc-print-log)
;;(define-key 'vc-dired '(#\v #\m) 'vc-merge)
;;(define-key 'vc-dired '(#\v #\r) 'vc-retrieve-snapshot)
;;(define-key 'vc-dired '(#\v #\s) 'vc-create-snapshot)
(define-key 'vc-dired '(#\v #\u) 'vc-revert-buffer)
(define-key 'vc-dired '(#\v #\v) 'vc-next-action)
(define-key 'vc-dired '(#\v #\=) 'vc-diff)
(define-key 'vc-dired '(#\v #\~) 'vc-version-other-window)
(define-key 'vc-dired '(#\* #\l) 'vc-dired-mark-locked)

(define-command vc-dired-mark-locked
  "Mark all files currently locked."
  ()
  (lambda ()
    (dired-mark-files! (selected-buffer)
      (lambda (file)
	(let ((master (file-vc-master file #f)))
	  (and master
	       (vc-backend-locking-user master #f)))))))

;;;; Log Entries

(define (vc-start-entry reference msg comment finish-entry after)
  (if comment
      (begin
	(finish-entry comment)
	(if after (after)))
      (let ((log-buffer (new-buffer "*VC-log*")))
	(set-buffer-major-mode! log-buffer (ref-mode-object vc-log))
	(if (vc-master? reference)
	    (vc-mode-line reference log-buffer))
	(let ((buffer
	       (and reference
		    (if (buffer? reference)
			reference
			(pathname->buffer (->workfile reference))))))
	  (if buffer
	      (buffer-put! log-buffer 'VC-PARENT-BUFFER buffer)
	      (buffer-remove! log-buffer 'VC-PARENT-BUFFER)))
	(let ((window (selected-window))
	      (buffer (selected-buffer)))
	  (let ((log-window (pop-up-buffer log-buffer #t)))
	    (buffer-put! log-buffer
			 'VC-LOG-FINISH-ENTRY
			 (vc-finish-entry reference
					  finish-entry
					  after
					  (weak-cons log-window #f)
					  (weak-cons window #f)
					  (weak-cons buffer #f)))))
	(message msg "  Type C-c C-c when done."))))

(define (vc-finish-entry reference finish-entry after log-window window buffer)
  (lambda (log-buffer)
    (if (vc-master? reference)
	(begin
	  (guarantee-vc-master-valid reference)
	  (vc-backend-check-log-entry reference log-buffer)))
    (guarantee-newline (buffer-end log-buffer))
    (let ((comment (buffer-string log-buffer))
	  (parent-buffer (chase-parent-buffer log-buffer)))
      (comint-record-input vc-comment-ring comment)
      ;; Save any changes the user might have made while editing the
      ;; comment.
      (if (and (not (eq? parent-buffer log-buffer))
	       (buffer-alive? parent-buffer)
	       (not (vc-dired-buffer? parent-buffer)))
	  (vc-save-buffer parent-buffer #t))
      ;; If a new window was created to hold the log buffer, and the log
      ;; buffer is still selected in that window, delete it.
      (let ((log-window (weak-car log-window)))
	(if (and log-window
		 (window-live? log-window)
		 (eq? log-buffer (window-buffer log-window))
		 (not (window-has-no-neighbors? log-window)))
	    (window-delete! log-window)))
      ;; Either kill or bury the log buffer.
      (if (buffer-alive? log-buffer)
	  (if (ref-variable vc-delete-logbuf-window log-buffer)
	      (kill-buffer log-buffer)
	      (begin
		(make-buffer-invisible log-buffer)
		(bury-buffer log-buffer))))
      (let ((window (weak-car window))
	    (buffer (weak-car buffer)))
	(if (and window (window-live? window))
	    (select-window window))
	(if (and buffer (buffer-alive? buffer))
	    (if (and window (window-live? window))
		(select-buffer-no-record buffer window)
		(select-buffer buffer))))
      ;; Do the log operation.
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
    (local-set-variable! comint-input-ring vc-comment-ring buffer)
    (local-set-variable! comint-last-input-match #f buffer)
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
    (let ((buffer (selected-buffer)))
      (let ((finish-entry (buffer-get buffer 'VC-LOG-FINISH-ENTRY #f)))
	(if (not finish-entry)
	    (error "No log operation is pending."))
	(finish-entry buffer)))))

;;;; Back-End Calls

;;; In what follows, a "revision string" has the following definition:
;;; A revision string of #F usually refers to the head of the
;;;   branch on which the workfile resides, but in some cases it has a
;;;   different meaning.
;;; A revision string with an odd number of elements specifies a branch,
;;;   and the string refers to the head of the specified branch.
;;; A revision string with an even number of elements specifies a
;;;   particular revision.  When checking in, this revision must not
;;;   exist, and must be greater than any existing revision on the
;;;   associated trunk or branch.  When checking out, this revision
;;;   must exist.
;;; A revision string may be symbolic, in which case it is treated as
;;;   the numeric string that it is bound to.

(define (vc-backend-release type)
  ;; TYPE is a VC-TYPE object.
  ;; The return value is either a release string or #F.
  ;; A release string matches "[0-9.]+ *.*".
  (let ((release (vc-type-get type 'RELEASE 'UNKNOWN)))
    (if (eq? 'UNKNOWN release)
	(let ((release ((vc-type-operation type 'RELEASE))))
	  (vc-type-put! type 'RELEASE release)
	  release)
	release)))

(define (vc-backend-find-master workfile)
  (let loop ((ps (vc-control-directories workfile)))
    (and (pair? ps)
	 (or ((vc-type-operation (caar ps) 'FIND-MASTER) workfile (cdar ps))
	     (loop (cdr ps))))))

(define (vc-backend-master-valid? master)
  ;; MASTER is a VC-MASTER object.
  ;; The return value is a boolean indicating that MASTER is valid.
  (vc-call 'VALID? master))

(define (vc-backend-default-revision master)
  ;; MASTER is a valid VC-MASTER object.
  ;; The default revision (usually the head of the trunk) is returned.
  ;; If there is no such revision, #F is returned.
  (vc-call 'DEFAULT-REVISION master))

(define (vc-backend-workfile-revision master)
  ;; MASTER is a valid VC-MASTER object.
  ;; The last checked-in revision of the file is returned.
  ;; If this can't be determined, #F is returned.
  (vc-call 'WORKFILE-REVISION master))

(define (vc-backend-locking-user master revision)
  ;; MASTER is a valid VC-MASTER object.
  ;; REVISION is a revision string or #F.
  ;;   A REVISION of #F refers to the last checked-in revision of the
  ;;   workfile.
  ;; The user holding the lock on that revision is returned.  If there
  ;;   is no lock, or if the lock cannot be determined, #F is returned.
  (vc-call 'LOCKING-USER master revision))

(define (vc-backend-workfile-modified? master)
  (vc-call 'WORKFILE-MODIFIED? master))

(define (vc-backend-next-action master)
  (vc-call 'NEXT-ACTION master))

(define (vc-backend-keep-workfiles? master)
  (vc-call 'KEEP-WORKFILES? master))

(define (vc-backend-workfile-status-string master)
  (vc-call 'WORKFILE-STATUS-STRING master))

(define (vc-backend-register workfile revision comment keep?)
  ;; WORKFILE is an absolute pathname to an existing file.
  ;; REVISION is either a revision string or #F.
  ;; COMMENT is a comment string.
  ;; KEEP? is either #F, #T, or LOCK.
  ;;   #F means don't keep a copy of WORKFILE after registration.
  ;;   #T means keep an unlocked copy.
  ;;   LOCK means keep a locked copy.
  ;; On return, WORKFILE must be registered.
  ((vc-type-operation
    (if (and (pair? vc-types)
	     (null? (cdr vc-types)))
	(car vc-types)
	(let ((likely-types (map car (vc-control-directories workfile))))
	  (if (and (pair? likely-types)
		   (null? (cdr likely-types)))
	      (car likely-types)
	      (cleanup-pop-up-buffers
	       (lambda ()
		 (call-with-output-to-temporary-buffer " *VC-types*"
						       '(SHRINK-WINDOW)
		   (lambda (port)
		     (for-each
		      (lambda (type)
			(write-string (vc-type-display-name type) port)
			(newline port))
		      vc-types)))
		 (prompt-for-alist-value
		  "Version control type"
		  (map (lambda (type)
			 (cons (vc-type-display-name type)
			       type))
		       vc-types)
		  #f
		  #f))))))
    'REGISTER)
   workfile revision comment keep?))

(define (vc-backend-checkout master revision lock? workfile)
  ;; MASTER is a valid VC-MASTER object.
  ;; REVISION is either a revision string or #F.
  ;; LOCK? is a boolean saying whether to lock the specified revision.
  ;;   This has effect only with backends that do locking.
  ;; WORKFILE is either an absolute pathname or #F.
  ;;   If #F, the file is checked out into the workfile pathname of MASTER.
  ;;   Otherwise, the file is checked out into WORKFILE.
  (vc-call 'CHECKOUT master revision lock?
	   (and workfile
		(not (pathname=? workfile (vc-master-workfile master)))
		workfile)))

(define (vc-backend-checkin master revision comment keep?)
  ;; MASTER is a valid VC-MASTER object.
  ;; REVISION is either a revision string or #F.
  ;; COMMENT is a comment string.
  ;; KEEP? is a boolean specifying that the workfile should be kept
  ;;   after checking in.  If #F, the workfile is deleted.
  (vc-call 'CHECKIN master revision comment keep?))

(define (vc-backend-revert master)
  ;; MASTER is a valid VC-MASTER object.
  ;; The workfile is checked out, discarding the existing workfile.
  (vc-call 'REVERT master))

(define (vc-backend-steal master revision)
  ;; MASTER is a valid VC-MASTER object.
  ;; REVISION is either a revision string or #F.
  ;; The lock is stolen from the owner without notification.
  (vc-call 'STEAL master revision))

(define (vc-backend-diff master rev1 rev2 simple?)
  ;; MASTER is a valid VC-MASTER object.
  ;; REV1 is either a revision string or #F.
  ;; REV2 is either a revision string or #F.
  ;;   If REV1 and REV2 are both #F, the workfile is compared to its
  ;;     most recent checked-in revision.
  ;;   If REV1 nor REV2 is #F, the specified revisions are compared.
  ;;   Otherwise, the workfile is compared to the specified revision.
  ;; SIMPLE? is a boolean specifying how the comparison is performed.
  ;;   If #T, only the result of the comparison is interesting.
  ;;   If #F, the differences are to be shown to the user.
  (if (equal? "0" (vc-backend-workfile-revision master))
      ;; This file is added but not yet committed; there is no
      ;; master file.
      (begin
	(if (or rev1 rev2)
	    (error "No revisions exist:" (vc-master-workfile master)))
	(if simple?
	    ;; File is added but not committed; we regard this as
	    ;; "changed".
	    #t
	    ;; Diff against /dev/null.
	    (= 1
	       (vc-run-command master
			       (get-vc-diff-options simple?)
			       "diff"
			       (gc-vc-diff-switches master)
			       "/dev/null"
			       (file-pathname
				(vc-master-workfile master))))))
      (vc-call 'DIFF master rev1 rev2 simple?)))

(define (vc-backend-print-log master)
  ;; MASTER is a valid VC-MASTER object.
  ;; The log associated with that file is popped up in another buffer.
  (vc-call 'PRINT-LOG master))

(define (vc-backend-check-log-entry master log-buffer)
  ;; MASTER is a valid VC-MASTER object.
  ;; LOG-BUFFER is a buffer containing a log message.
  ;; The buffer's contents is checked for compatibility with the
  ;;   backend, and an error is signalled if it is incompatible.
  (vc-call 'CHECK-LOG-ENTRY master log-buffer))

(define (vc-backend-check-headers master buffer)
  ;; MASTER is a valid VC-MASTER object.
  ;; BUFFER is the workfile buffer.
  ;; Examines the buffer contents to determine if they contain
  ;; appropriate revision-control header strings.  Returns #t iff the
  ;; header strings are present.
  (vc-call 'CHECK-HEADERS master buffer))

(define (vc-backend-mode-line-status master buffer)
  (let ((operation
	 (vc-type-operation (vc-master-type master) 'MODE-LINE-STATUS #f)))
    (if operation
	(operation master buffer)
	(%default-mode-line-status master buffer))))

(define (vc-control-directories workfile)
  (let ((start (merge-pathnames (directory-pathname workfile))))
    (let loop ((path (pathname-directory start)) (possible vc-types))
      (let ((directory (pathname-new-directory start path)))
	(receive (good maybe) (local-control-directories directory)
	  (or (let ((good*
		     (filter (lambda (p) (memq (car p) possible))
			     good)))
		(and (pair? good*)
		     good*))
	      (if (and (null? good)
		       (pair? (cdr path)))
		  (let ((maybe (lset-intersection eqv? maybe possible)))
		    (if (pair? maybe)
			(loop (except-last-pair path) maybe)
			'()))
		  '())))))))

(define (local-control-directories directory)
  (let loop ((types vc-types) (good '()) (maybe '()))
    (if (pair? types)
	(let ((control-dir
	       ((vc-type-operation (car types) 'CONTROL-DIRECTORY)
		directory)))
	  (cond ((not control-dir)
		 (loop (cdr types)
		       good
		       maybe))
		((eq? control-dir 'SEARCH-PARENT)
		 (loop (cdr types)
		       good
		       (cons (car types) maybe)))
		(else
		 (loop (cdr types)
		       (cons (cons (car types) control-dir) good)
		       maybe))))
	(values good maybe))))

;;;; Command Execution

(define (vc-run-command master options command . arguments)
  (let ((workfile (and master (->workfile master)))
	(option
	 (lambda (name default)
	   (let ((option (assq name options)))
	     (if option
		 (cadr option)
		 (default))))))
    (let ((command-messages?
	   (ref-variable vc-command-messages
			 (and workfile (pathname->buffer workfile))))
	  (msg
	   (string-append "Running " command
			  (if master
			      (string-append " on " (->namestring workfile))
			      "")
			  "..."))
	  (status-limit (option 'STATUS (lambda () 0)))
	  (directory
	   (option 'DIRECTORY
		   (if workfile
		       (lambda () (directory-pathname workfile))
		       working-directory-pathname)))
	  (command-buffer
	   (let ((buffer (option 'BUFFER get-vc-command-buffer)))
	     (cond ((buffer? buffer) buffer)
		   ((string? buffer) (find-or-create-buffer buffer))
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
	      (editor-error msg "...FAILED "
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
		  (reduce-right string-append-separated
				""
				(vc-command-arguments
				 (cons command arguments)))))

(define (pop-up-vc-command-buffer select?)
  (let ((buffer (get-vc-command-buffer)))
    (set-buffer-point! buffer (buffer-start buffer))
    (pop-up-buffer buffer select?)))

(define (get-vc-command-buffer)
  (find-or-create-buffer "*vc*"))

(define (pop-up-vc-diff-buffer select?)
  (let ((buffer (get-vc-diff-buffer #f)))
    (set-buffer-point! buffer (buffer-start buffer))
    (pop-up-buffer buffer select?)))

(define (get-vc-diff-options simple?)
  `((STATUS 1)
    (BUFFER ,(get-vc-diff-buffer simple?))))

(define (get-vc-diff-buffer simple?)
  (find-or-create-buffer (if simple? " *vc-diff*" "*vc-diff*")))

(define (gc-vc-diff-switches master)
  (ref-variable diff-switches (vc-workfile-buffer master #f)))

(define (with-vc-command-message master operation thunk)
  (let ((msg
	 (string-append operation " " (->namestring (->workfile master))
			"...")))
    (message msg)
    (thunk)
    (message msg "done")))

;;;; Release/Revision numbers

(define (vc-release? type release)
  (let ((release* (vc-backend-release type)))
    (and release*
	 (release<=? release release*))))

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

(define (branch-revision? revision)
  (re-string-match "\\`[0-9]+\\(\\.[0-9]+\\.[0-9]+\\)*\\'" revision))

(define (revision-branch-part revision)
  (let ((regs (re-string-search-forward "\\.[0-9]+\\'" revision)))
    (if (not regs)
	(error:bad-range-argument revision 'BRANCH-PART))
    (string-head revision (re-match-start-index 0 regs))))

(define (revision-minor-part revision)
  (let ((regs (re-string-search-forward "[0-9]+\\'" revision)))
    (if (not regs)
	(error:bad-range-argument revision 'BRANCH-PART))
    (substring revision
	       (re-match-start-index 0 regs)
	       (re-match-end-index 0 regs))))

(define (previous-revision revision)
  (let ((branch (revision-branch-part revision))
	(minor (string->number (revision-minor-part revision))))
    (if (> minor 1)
	(string-append branch "." (number->string (- minor 1)))
	;; At the first minor number.  If on trunk, no obvious answer.
	(and (branch-revision? revision)
	     (revision-branch-part branch)))))

(define (vc-get-revision revision? prompt)
  (and revision?
       (vc-normalize-revision (prompt-for-string prompt #f))))

(define (vc-normalize-revision revision)
  (and revision
       (not (string-null? revision))
       revision))

;;;; Utilities

(define (blank-string? string)
  (not (string-find-next-char-in-set string char-set:not-whitespace)))

(define (subdirectory-pathname pathname name)
  (let ((directory (directory-pathname pathname)))
    (pathname-new-directory directory
			    (append (pathname-directory directory)
				    (list name)))))

(define (->workfile object)
  (cond ((vc-master? object) (vc-master-workfile object))
	((pathname? object) object)
	(else (error:wrong-type-argument object "workfile" '->WORKFILE))))

(define (vc-workfile-buffer master find?)
  (let ((pathname (vc-master-workfile master)))
    (if find?
	(find-file-noselect pathname #f)
	(pathname->buffer pathname))))

(define (vc-workfile-buffer-modified? master)
  (let ((buffer (vc-workfile-buffer master #f)))
    (and buffer
	 (buffer-modified? buffer))))

(define (vc-workfile-string master)
  (->namestring (vc-master-workfile master)))

(define (vc-save-workfile-buffer workfile)
  (let ((buffer (pathname->buffer workfile)))
    (if buffer
	(vc-save-buffer buffer #t))))

(define (vc-save-buffer buffer error?)
  (if (buffer-modified? buffer)
      (if (or (ref-variable vc-suppress-confirm buffer)
	      (prompt-for-confirmation?
	       (string-append "Buffer " (buffer-name buffer)
			      " modified; save it")))
	  (save-buffer buffer #f)
	  (if error? (editor-error "Aborted")))))

(define (vc-resync-workfile-buffer workfile keep?)
  (let ((buffer (pathname->buffer workfile)))
    (if buffer
	(if keep?
	    (vc-revert-buffer buffer #t)
	    (kill-buffer buffer)))))

(define diff-brief-available?
  (let ((result 'UNKNOWN))
    (lambda ()
      (if (eq? result 'UNKNOWN)
	  (set! result
		(= 0
		   (run-synchronous-subprocess
		    "diff" '("--brief" "/dev/null" "/dev/null")
		    'OUTPUT #F))))
      result)))

(define (vc-revert-workfile-buffer master dont-confirm?)
  (let ((buffer (vc-workfile-buffer master #f)))
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
    (let ((buffer (revert-buffer buffer #t dont-confirm?)))
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
	    (set-buffer-mark! buffer m)))
      buffer)))

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