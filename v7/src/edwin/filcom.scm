;;; -*-Scheme-*-
;;;
;;;	$Id: filcom.scm,v 1.197 1998/12/09 02:51:39 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989-98 Massachusetts Institute of Technology
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

;;;; File Commands

(declare (usual-integrations))

(define (find-file filename)
  (select-buffer (find-file-noselect filename true)))

(define-command find-file
  "Visit a file in its own buffer.
If the file is already in some buffer, select that buffer.
Otherwise, visit the file in a buffer named after the file."
  "FFind file"
  find-file)

(define (find-file-other-window filename)
  (select-buffer-other-window (find-file-noselect filename true)))

(define-command find-file-other-window
  "Visit a file in another window.
May create a window, or reuse one."
  "FFind file in other window"
  find-file-other-window)

(define (find-file-other-screen filename)
  (select-buffer-other-screen (find-file-noselect filename true)))

(define-command find-file-other-frame
  "Visit a file in another frame."
  "FFind file in other frame"
  find-file-other-screen)
(define edwin-command$find-file-other-screen
  edwin-command$find-file-other-frame)

(define-command find-alternate-file
  "Find file FILENAME, select its buffer, kill previous buffer.
If the current buffer now contains an empty file that you just visited
\(presumably by mistake), use this command to visit the file you really want."
  "FFind alternate file"
  (lambda (filename)
    (let ((buffer (current-buffer)))
      (let ((do-it
	     (lambda ()
	       (kill-buffer-interactive buffer)
	       (find-file filename))))
	(if (other-buffer buffer)
	    (do-it)
	    (let ((buffer* (new-buffer "*dummy*")))
	      (do-it)
	      (kill-buffer buffer*)))))))

(define-variable find-file-run-dired
  "True says run dired if find-file is given the name of a directory."
  true
  boolean?)

(define-variable find-file-not-found-hooks
  "List of procedures to be called for find-file on nonexistent file.
These functions are called as soon as the error is detected.
The functions are called in the order given,
until one of them returns non-false."
  '()
  list?)

(define-variable find-file-hooks
  "Event distributor to be invoked after a buffer is loaded from a file.
The buffer's local variables (if any) will have been processed before the
invocation."
  (make-event-distributor))

(define (find-file-noselect filename warn?)
  (let ((pathname (pathname-simplify (merge-pathnames filename))))
    (if (file-test-no-errors file-directory? pathname)
	(if (ref-variable find-file-run-dired)
	    (make-dired-buffer (pathname-as-directory pathname))
	    (editor-error (->namestring pathname) " is a directory."))
	(let ((buffer (pathname->buffer pathname)))
	  (if buffer
	      (begin
		(if warn? (find-file-revert buffer))
		buffer)
	      (let ((buffer (new-buffer (pathname->buffer-name pathname))))
		(let ((error?
		       (not
			(catch-file-errors
			 (lambda () false)
			 (lambda () (read-buffer buffer pathname true))))))
		  (if error?
		      (do ((hooks
			    (ref-variable find-file-not-found-hooks buffer)
			    (cdr hooks)))
			  ((or (null? hooks)
			       ((car hooks) buffer))))
		      (maybe-change-buffer-name! buffer pathname))
		  (after-find-file buffer error? warn?))
		buffer))))))

(define (maybe-change-buffer-name! buffer pathname)
  (let ((name (pathname->buffer-name pathname))
	(name* (pathname->buffer-name (buffer-pathname buffer))))
    (if (not (string=? name name*))
	(rename-buffer buffer (new-buffer-name name*)))))

(define (after-find-file buffer error? warn?)
  (let ((pathname (or (buffer-truename buffer) (buffer-pathname buffer))))
    (let ((buffer-read-only?
	   (not (file-test-no-errors file-writable? pathname))))
      (if buffer-read-only?
	  (set-buffer-read-only! buffer)
	  (set-buffer-writable! buffer))
      (setup-buffer-auto-save! buffer)
      (let ((serious-message
	     (lambda (msg)
	       (message msg)
	       (sit-for 1))))
	(cond ((not buffer-read-only?)
	       (cond ((and warn?
			   (let ((asp (buffer-auto-save-pathname buffer)))
			     (and asp
				  (file-newer-than-file? asp pathname))))
		      (serious-message
		       "Auto save file is newer; consider M-x recover-file"))
		     (error?
		      (message "(New file)"))))
	      ((not error?)
	       (message "File is write protected"))
	      (else
	       (serious-message
		(if (file-test-no-errors
		     (lambda (pathname) (file-access pathname 0))
		     pathname)
		    "File exists, but is read-protected."
		    (string-append
		     "File not found and directory "
		     (let ((directory
			    (directory-pathname-as-file
			     (directory-pathname
			      (buffer-pathname buffer)))))
		       (if (file-test-no-errors file-exists? directory)
			   "write-protected"
			   "doesn't exist")))))))))
    (normal-mode buffer true)
    (event-distributor/invoke! (ref-variable find-file-hooks buffer) buffer)
    (load-find-file-initialization buffer pathname)))

(define (file-test-no-errors test . args)
  (catch-file-errors (lambda () false)
		     (lambda () (apply test args))))

(define (file-newer-than-file? a b)
  (let ((a (file-modification-time-indirect a)))
    (and a
	 (let ((b (file-modification-time-indirect b)))
	   (or (not b)
	       (> a b))))))

(define (load-find-file-initialization buffer pathname)
  (let ((pathname
	 (catch-file-errors
	  (lambda () false)
	  (lambda () (os/find-file-initialization-filename pathname)))))
    (if pathname
	(let ((database
	       (with-output-to-transcript-buffer
		(lambda ()
		  (bind-condition-handler (list condition-type:error)
		      evaluation-error-handler
		    (lambda ()
		      (catch-file-errors (lambda () false)
			(lambda ()
			  (fluid-let ((load/suppress-loading-message? true))
			    (load pathname
				  '(EDWIN)
				  edwin-syntax-table))))))))))
	  (if (and (procedure? database)
		   (procedure-arity-valid? database 1))
	      (database buffer)
	      (message
	       "Ill-formed find-file initialization file: "
	       (os/pathname->display-string pathname)))))))

(define (standard-scheme-find-file-initialization database)
  ;; DATABASE -must- be a vector whose elements are all three element
  ;; lists.  The car of each element must be a string, and the
  ;; elements must be sorted on those strings.
  (sort! database (lambda (x y) (string<? (car x) (car y))))
  (lambda (buffer)
    (let ((entry
	   (let ((pathname (buffer-pathname buffer)))
	     (and pathname
		  (equal? "scm" (pathname-type pathname))
		  (let ((name (pathname-name pathname)))
		    (and name
			 (vector-binary-search database
					       string<?
					       car
					       name)))))))
      (if entry
	  (begin
	    (define-variable-local-value! buffer
		(ref-variable-object scheme-environment)
	      (cadr entry))
	    (if (and (eq? 'DEFAULT (ref-variable scheme-environment buffer))
		     (not (eq? 'default (cadr entry))))
		(begin
		  (message "Ignoring bad evaluation environment: "
			   (cadr entry))
		  (define-variable-local-value! buffer
		      (ref-variable-object scheme-syntax-table)
		    'DEFAULT))
		(define-variable-local-value! buffer
		    (ref-variable-object scheme-syntax-table)
		  (caddr entry))))))))

(define (find-file-revert buffer)
  (if (not (verify-visited-file-modification-time? buffer))
      (let ((pathname (buffer-pathname buffer)))
	(cond ((not (file-exists? pathname))
	       (editor-error "File "
			     (->namestring pathname)
			     " no longer exists!"))
	      ((prompt-for-yes-or-no?
		(string-append
		 "File has changed since last visited or saved.  "
		 (if (buffer-modified? buffer)
		     "Flush your changes"
		     "Read from disk")))
	       (revert-buffer buffer true true))))))

(define-command revert-buffer
  "Replace the buffer text with the text of the visited file on disk.
This undoes all changes since the file was visited or saved.
If latest auto-save file is more recent than the visited file,
asks user whether to use that instead.
Argument means don't offer to use auto-save file."
  "P"
  (lambda (argument)
    (revert-buffer (current-buffer) argument false)))

(define (revert-buffer buffer dont-use-auto-save? dont-confirm?)
  ((or (buffer-get buffer 'REVERT-BUFFER-METHOD) revert-buffer-default)
   buffer dont-use-auto-save? dont-confirm?))

(define (revert-buffer-default buffer dont-use-auto-save? dont-confirm?)
  (let ((auto-save?
	 (and (not dont-use-auto-save?)
	      (buffer-auto-saved? buffer)
	      (buffer-auto-save-pathname buffer)
	      (file-readable? (buffer-auto-save-pathname buffer))
	      (prompt-for-confirmation?
"Buffer has been auto-saved recently.  Revert from auto-save file"))))
    (let ((pathname
	   (if auto-save?
	       (buffer-auto-save-pathname buffer)
	       (buffer-pathname buffer))))
      (cond ((not pathname)
	     (editor-error
	      "Buffer does not seem to be associated with any file"))
	    ((not (file-readable? pathname))
	     (editor-error "File "
			   (->namestring pathname)
			   " no longer "
			   (if (file-exists? pathname) "exists" "readable")
			   "!"))
	    ((or dont-confirm?
		 (prompt-for-yes-or-no?
		  (string-append "Revert buffer from file "
				 (->namestring pathname))))
	     ;; If file was backed up but has changed since, we
	     ;; should make another backup.
	     (if (and (not auto-save?)
		      (not (verify-visited-file-modification-time? buffer)))
		 (set-buffer-backed-up?! buffer false))
	     (let ((where (mark-index (buffer-point buffer)))
		   (group (buffer-group buffer))
		   (do-it
		    (lambda ()
		      (read-buffer buffer pathname (not auto-save?)))))
	       (if (group-undo-data group)
		   (begin
		     ;; Throw away existing undo data.
		     (disable-group-undo! group)
		     (do-it)
		     (enable-group-undo! group))
		   (do-it))
	       (set-buffer-point!
		buffer
		(make-mark group (min where (buffer-length buffer))))
	       (after-find-file buffer false false)))))))

(define-command recover-file
  "Visit file FILE, but get contents from its last auto-save file."
  "FRecover file"
  (lambda (filename)
    (let ((pathname (pathname-simplify (merge-pathnames filename))))
      (let ((filename (->namestring pathname)))
	(if (os/auto-save-filename? filename)
	    (editor-error filename " is an auto-save file")))
      (let ((auto-save-pathname (os/auto-save-pathname pathname false)))
	(let ((auto-save-filename (->namestring auto-save-pathname)))
	  (if (not (file-newer-than-file? auto-save-pathname pathname))
	      (editor-error "Auto-save file "
			    auto-save-filename
			    " not current"))
	  (if (not (call-with-temporary-buffer "*Directory*"
		     (lambda (buffer)
		       (insert-dired-entry! pathname (buffer-end buffer))
		       (insert-dired-entry! auto-save-pathname
					    (buffer-end buffer))
		       (set-buffer-point! buffer (buffer-start buffer))
		       (buffer-not-modified! buffer)
		       (pop-up-buffer buffer false)
		       (prompt-for-yes-or-no?
			(string-append "Recover auto save file "
				       auto-save-filename)))))
	      (editor-error "Recover-file cancelled."))
	  (let ((buffer (find-file-noselect pathname false)))
	    (read-buffer buffer auto-save-pathname false)
	    (after-find-file buffer false false)
	    (disable-buffer-auto-save! buffer)
	    (message
	     "Auto-save off in this buffer till you do M-x auto-save-mode.")
	    (select-buffer buffer)))))))

(define-command insert-filename
  "Interactively read a file name and insert it at point.
The file name is normally inserted using Scheme syntax,
but see the variable insert-filename-format."
  "FInsert filename"
  (lambda (filename)
    (insert-string ((ref-variable insert-filename-format) filename)
		   (current-point))))

(define-variable insert-filename-format
  "Defines the format used by \[insert-filename].
The value of this variable must be a procedure of one argument.
The procedure is called with the filename as an argument,
and returns the string that is inserted into the buffer."
  write-to-string
  (lambda (object)
    (and (procedure? object)
	 (procedure-arity-valid? object 1))))

(define-command save-buffer
  "Save current buffer in visited file if modified.  Versions described below.

By default, makes the previous version into a backup file
 if previously requested or if this is the first save.
With 1 or 3 \\[universal-argument]'s, marks this version
 to become a backup when the next save is done.
With 2 or 3 \\[universal-argument]'s,
 unconditionally makes the previous version into a backup file.
With argument of 0, never makes the previous version into a backup file.

If a file's name is FOO, the names of its numbered backup versions are
 FOO.~i~ for various integers i.  A non-numbered backup file is called FOO~.
Numeric backups (rather than FOO~) will be made if value of
 `version-control' is not the atom `never' and either there are already
 numeric versions of the file being backed up, or `version-control' is
 not #F.
We don't want excessive versions piling up, so there are variables
 `kept-old-versions', which tells Edwin how many oldest versions to keep,
 and `kept-new-versions', which tells how many newest versions to keep.
 Defaults are 2 old versions and 2 new.
If `trim-versions-without-asking' is false, system will query user
 before trimming versions.  Otherwise it does it silently."
  "p"
  (lambda (argument)
    (save-buffer (current-buffer)
		 (case argument
		   ((0) 'NO-BACKUP)
		   ((4) 'BACKUP-NEXT)
		   ((16) 'BACKUP-PREVIOUS)
		   ((64) 'BACKUP-BOTH)
		   (else false)))))

(define (save-buffer buffer backup-mode)
  (if (buffer-modified? buffer)
      (begin
	(if (not (buffer-pathname buffer))
	    (set-visited-pathname
	     buffer
	     (prompt-for-pathname
	      (string-append "Write buffer " (buffer-name buffer) " to file")
	      false false)))
	(if (and (ref-variable enable-emacs-write-file-message)
		 (> (buffer-length buffer) 50000))
	    (message "Saving file "
		     (->namestring (buffer-pathname buffer))
		     "..."))
	(write-buffer-interactive buffer backup-mode))
      (message "(No changes need to be written)")))

(define-command save-some-buffers
  "Saves some modified file-visiting buffers.  Asks user about each one.
With argument, saves all with no questions."
  "P"
  (lambda (no-confirmation?)
    (save-some-buffers no-confirmation? false)))

(define (save-some-buffers no-confirmation? exiting?)
  (let ((buffers
	 (let ((exiting? (and (not (default-object? exiting?)) exiting?)))
	   (list-transform-positive (buffer-list)
	     (lambda (buffer)
	       (and (buffer-modified? buffer)
		    (or (buffer-pathname buffer)
			(and exiting?
			     (ref-variable buffer-offer-save buffer)
			     (> (buffer-length buffer) 0)))))))))
    (if (null? buffers)
	(message "(No files need saving)")
	(for-each (if (and (not (default-object? no-confirmation?))
			   no-confirmation?)
		      (lambda (buffer)
			(write-buffer-interactive buffer false))
		      (lambda (buffer)
			(if (prompt-for-confirmation?
			     (let ((pathname (buffer-pathname buffer)))
			       (if pathname
				   (string-append "Save file "
						  (->namestring pathname))
				   (string-append "Save buffer "
						  (buffer-name buffer)))))
			    (write-buffer-interactive buffer false))))
		  buffers))))

(define-variable-per-buffer buffer-offer-save
  "True in a buffer means offer to save the buffer on exit
even if the buffer is not visiting a file.  Automatically local in
all buffers."
  false
  boolean?)

(define (pathname->buffer-name pathname)
  (let ((pathname
	 (let ((pathname (->pathname pathname)))
	   (if (pathname-name pathname)
	       pathname
	       (directory-pathname-as-file pathname)))))
    (let ((name (file-namestring pathname)))
      (if (string-null? name)
	  (->namestring pathname)
	  name))))

(define (pathname->buffer pathname)
  (let ((pathname (->pathname pathname)))
    (list-search-positive (buffer-list)
      (lambda (buffer)
	(equal? pathname (buffer-pathname buffer))))))

(define-command set-visited-file-name
  "Change name of file visited in current buffer.
The next time the buffer is saved it will go in the newly specified file.
Delete the initial contents of the minibuffer
if you wish to make buffer not be visiting any file."
  "FSet visited file name"
  (lambda (filename)
    (set-visited-pathname
     (current-buffer)
     (let ((pathname (->pathname filename)))
       (and (not (string-null? (file-namestring pathname)))
	    pathname)))))

(define (set-visited-pathname buffer pathname)
  (if (and pathname (not (pathname-name pathname)))
      (editor-error "File name cannot be a directory: "
		    (->namestring pathname)))
  (set-buffer-pathname! buffer pathname)
  (set-buffer-truename! buffer false)
  (if pathname
      (let ((name (pathname->buffer-name pathname)))
	(if (not (find-buffer name))
	    (rename-buffer buffer name))))
  (set-buffer-backed-up?! buffer false)
  (clear-visited-file-modification-time! buffer)
  (cond ((buffer-auto-save-pathname buffer)
	 (rename-auto-save-file! buffer))
	(pathname
	 (setup-buffer-auto-save! buffer)))
  (if pathname
      (buffer-modified! buffer)))

(define-command write-file
  "Write current buffer into file FILENAME.
Makes buffer visit that file, and marks it not modified."
  "FWrite file"
  (lambda (filename)
    (write-file (current-buffer) filename)))

(define (write-file buffer filename)
  (if (and filename
	   (not (string-null? filename)))
      (set-visited-pathname buffer (->pathname filename)))
  (buffer-modified! buffer)
  (save-buffer buffer false))

(define-command write-region
  "Write current region into specified file."
  "r\nFWrite region to file"
  (lambda (region filename)
    (write-region region filename #t #t)))

(define-command append-to-file
  "Write current region into specified file."
  "r\nFAppend to file"
  (lambda (region filename)
    (append-to-file region filename #t #t)))

(define-command insert-file
  "Insert contents of file into existing text.
Leaves point at the beginning, mark at the end."
  "FInsert file"
  (lambda (filename)
    (let ((point (mark-right-inserting (current-point))))
      (let ((mark (mark-left-inserting point)))
	(insert-file point filename)
	(set-current-point! point)
	(push-current-mark! mark)))))

(define-command copy-file
  "Copy a file; the old and new names are read in the typein window.
If a file with the new name already exists, confirmation is requested first."
  (lambda ()
    (let ((old (prompt-for-existing-file "Copy file" false)))
      (list old (prompt-for-file "Copy to" old))))
  (lambda (old new)
    (if (or (not (file-exists? new))
	    (prompt-for-yes-or-no?
	     (string-append "File "
			    (->namestring new)
			    " already exists; copy anyway")))
	(begin
	  (copy-file old new)
	  (message "Copied " (->namestring old) " => " (->namestring new))))))

(define-command rename-file
  "Rename a file; the old and new names are read in the typein window.
If a file with the new name already exists, confirmation is requested first."
  (lambda ()
    (let ((old (prompt-for-existing-file "Rename file" false)))
      (list old (prompt-for-file "Rename to" old))))
  (lambda (old new)
    (let ((do-it
	   (lambda ()
	     (rename-file old new)
	     (message "Renamed " (->namestring old)
		      " => " (->namestring new)))))
      (if (file-exists? new)
	  (if (prompt-for-yes-or-no?
	       (string-append "File "
			      (->namestring new)
			      " already exists; rename anyway"))
	      (begin (delete-file new) (do-it)))
	  (do-it)))))

(define-command delete-file
  "Delete a file; the name is read in the typein window."
  "fDelete File"
  delete-file)

(define-command pwd
  "Show the current default directory."
  ()
  (lambda ()
    (message "Directory "
	     (->namestring (buffer-default-directory (current-buffer))))))

(define-command cd
  "Make DIR become the current buffer's default directory."
  "DChange default directory"
  (lambda (directory)
    (set-default-directory directory)
    ((ref-command pwd))))

(define (set-default-directory directory)
  (let ((buffer (current-buffer)))
    (let ((directory
	   (pathname-as-directory
	    (merge-pathnames directory (buffer-default-directory buffer)))))
      (if (not (file-directory? directory))
	  (editor-error (->namestring directory) " is not a directory"))
      (if (not (file-access directory 1))
	  (editor-error "Cannot cd to "
			(->namestring directory)
			": Permission denied"))
      (set-buffer-default-directory! buffer directory))))

;;;; Encryption

(define-command encrypt-file
  "Encrypt a file with the blowfish encryption algorithm.
Prompts for the plaintext and ciphertext filenames.
Prefix arg means treat the plaintext file as binary data.
Deletes the plaintext file after encryption."
  (lambda ()
    (if (not (blowfish-available?))
	(editor-error "Blowfish encryption not supported on this system"))
    (let ((from (prompt-for-existing-file "Encrypt file (plaintext)" #f)))
      (let ((to
	     (prompt-for-file
	      "Encrypt file to (ciphertext)"
	      (list (string-append (->namestring from) ".bf")))))
	(list from to (command-argument)))))
  (lambda (from to binary?)
    (if (or (not (file-exists? to))
	    (prompt-for-yes-or-no?
	     (string-append "File "
			    (->namestring to)
			    " already exists; overwrite")))
	(begin
	  (let ((password (prompt-for-confirmed-password)))
	    ((if binary?
		 call-with-binary-input-file
		 call-with-input-file)
	     from
	     (lambda (input)
	       (call-with-binary-output-file to
		 (lambda (output)
		   (write-blowfish-file-header output)
		   (blowfish-encrypt-port input output password #t))))))
	  (delete-file from)))))

(define-command decrypt-file
  "Decrypt a file with the blowfish encryption algorithm.
Prompts for the ciphertext and plaintext filenames.
Prefix arg means treat the plaintext file as binary data."
  (lambda ()
    (if (not (blowfish-available?))
	(editor-error "Blowfish encryption not supported on this system"))
    (let ((from (prompt-for-existing-file "Decrypt file (ciphertext)" #f)))
      (let ((to
	     (prompt-for-file
	      "Decrypt file to (plaintext)"
	      (and (pathname-type from)
		   (list (pathname-new-type from #f))))))
	(list from to (command-argument)))))
  (lambda (from to binary?)
    (if (or (not (file-exists? to))
	    (prompt-for-yes-or-no?
	     (string-append "File "
			    (->namestring to)
			    " already exists; overwrite")))
	(let ((password (prompt-for-password "Password")))
	  (call-with-binary-input-file from
	    (lambda (input)
	      (read-blowfish-file-header input)
	      ((if binary?
		   call-with-binary-output-file
		   call-with-output-file)
	       to
	       (lambda (output)
		 (blowfish-encrypt-port input output password #f)))))))))

;;;; Prompting

(define (prompt-for-file prompt default)
  (->namestring
   (prompt-for-pathname* prompt default file-non-directory? false)))

(define (prompt-for-existing-file prompt default)
  (->namestring
   (prompt-for-pathname* prompt default file-non-directory? true)))

(define (file-non-directory? file)
  (and (file-exists? file)
       (not (file-directory? file))))

(define (prompt-for-directory prompt default)
  (->namestring
   (let ((file-directory?
	  (lambda (pathname)
	    (and (not (pathname-wild? pathname))
		 (file-directory? pathname)))))
     (let ((directory
	    (prompt-for-pathname* prompt default file-directory? false)))
       (if (file-test-no-errors file-directory? directory)
	   (pathname-as-directory directory)
	   directory)))))

(define (prompt-for-existing-directory prompt default)
  (->namestring
   (pathname-as-directory
    (prompt-for-pathname* prompt default file-directory? true))))

(define (prompt-for-pathname prompt default require-match?)
  (prompt-for-pathname* prompt default file-exists? require-match?))

(define (prompt-for-pathname* prompt default
			      verify-final-value? require-match?)
  (let* ((directory
	  (if default
	      (directory-pathname
	       (if (pair? default)
		   (car default)
		   default))
	      (buffer-default-directory (current-buffer))))
	 (insertion
	  (os/pathname->display-string
	   (if (pair? default)
	       (car default)
	       directory))))
    (prompt-string->pathname
     (prompt-for-completed-string
      prompt
      insertion
      'INSERTED-DEFAULT
      (lambda (string if-unique if-not-unique if-not-found)
	(filename-complete-string
	 (prompt-string->pathname string insertion directory)
	 (lambda (filename)
	   (if-unique (os/pathname->display-string filename)))
	 (lambda (prefix get-completions)
	   (if-not-unique (os/pathname->display-string prefix)
			  get-completions))
	 if-not-found))
      (lambda (string)
	(filename-completions-list
	 (prompt-string->pathname string insertion directory)))
      (lambda (string)
	(file-test-no-errors
	 verify-final-value?
	 (prompt-string->pathname string insertion directory)))
      require-match?
      #f)
     insertion
     directory)))

;;;; Filename Completion

(define (filename-complete-string pathname
				  if-unique if-not-unique if-not-found)
  (define (loop directory filenames)
    (let ((unique-case
	   (lambda (filename)
	     (let ((pathname (merge-pathnames filename directory)))
	       (if (file-test-no-errors file-directory? pathname)
		   ;; Note: We assume here that all directories contain
		   ;; at least one file.  Thus directory names should 
		   ;; complete, but not uniquely.
		   (let ((dir (->namestring (pathname-as-directory pathname))))
		     (if-not-unique dir
				    (lambda ()
				      (canonicalize-filename-completions
				       dir
				       (os/directory-list dir)))))
		   (if-unique (->namestring pathname))))))
	  (non-unique-case
	   (lambda (filenames*)
	     (let ((string (string-greatest-common-prefix filenames*)))
	       (if-not-unique (->namestring (merge-pathnames string directory))
			      (lambda ()
				(canonicalize-filename-completions
				 directory
				 (list-transform-positive filenames
				   (lambda (filename)
				     (string-prefix? string filename))))))))))
      (if (null? (cdr filenames))
	  (unique-case (car filenames))
	  (let ((filtered-filenames
		 (list-transform-negative filenames
		   (lambda (filename)
		     (completion-ignore-filename?
		      (merge-pathnames filename directory))))))
	    (cond ((null? filtered-filenames)
		   (non-unique-case filenames))
		  ((null? (cdr filtered-filenames))
		   (unique-case (car filtered-filenames)))
		  (else
		   (non-unique-case filtered-filenames)))))))
  (let ((directory (directory-namestring pathname))
	(prefix (file-namestring pathname)))
    (cond ((not (file-test-no-errors file-directory? directory))
	   (if-not-found))
	  ((string-null? prefix)
	   ;; This optimization assumes that all directories
	   ;; contain at least one file.
	   (if-not-unique directory
			  (lambda ()
			    (canonicalize-filename-completions
			     directory
			     (os/directory-list directory)))))
	  (else
	   (let ((filenames (os/directory-list-completions directory prefix)))
	     (if (null? filenames)
		 (if-not-found)
		 (loop directory filenames)))))))

(define (filename-completions-list pathname)
  (let ((directory (directory-namestring pathname)))
    (canonicalize-filename-completions
     directory
     (os/directory-list-completions directory
				    (file-namestring pathname)))))

(define (prompt-string->pathname string insertion directory)
  (merge-pathnames (let ((pathname (os/trim-pathname-string string insertion)))
		     (if (memq (pathname-device pathname) '(#F UNSPECIFIC))
			 pathname
			 (pathname-default-directory pathname '(ABSOLUTE))))
		   directory))

(define (canonicalize-filename-completions directory filenames)
  (do ((filenames filenames (cdr filenames)))
      ((null? filenames))
    (if (file-test-no-errors file-directory?
			     (merge-pathnames (car filenames) directory))
	(set-car! filenames
		  (->namestring (pathname-as-directory (car filenames))))))
  (sort filenames string<?))

(define (completion-ignore-filename? filename)
  (os/completion-ignore-filename? (->namestring filename)))