;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/filcom.scm,v 1.145 1991/03/16 00:02:10 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989-91 Massachusetts Institute of Technology
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

(define (find-file-other-window filename)
  (select-buffer-other-window (find-file-noselect filename true)))

(define (find-file-other-screen filename)
  (select-buffer-other-screen (find-file-noselect filename true)))

(define (find-file-noselect filename warn?)
  (let ((pathname (pathname->absolute-pathname (->pathname filename))))
    (if (file-directory? pathname)
	(make-dired-buffer (pathname-as-directory pathname))
	(let ((buffer (pathname->buffer pathname)))
	  (if buffer
	      (begin
		(if (and warn?
			 (not (verify-visited-file-modification-time? buffer)))
		    (cond ((not (file-exists? pathname))
			   (editor-error "File "
					 (pathname->string pathname)
					 " no longer exists!"))
			  ((prompt-for-yes-or-no?
			    (string-append
			     "File has changed since last visited or saved.  "
			     (if (buffer-modified? buffer)
				 "Flush your changes"
				 "Read from disk")))
			   (revert-buffer buffer true true))))
		buffer)
	      (let ((buffer (new-buffer (pathname->buffer-name pathname))))
		(visit-file buffer pathname)
		buffer))))))

(define-command find-file
  "Visit a file in its own buffer.
If the file is already in some buffer, select that buffer.
Otherwise, visit the file in a buffer named after the file."
  "FFind file"
  find-file)

(define-command find-file-other-window
  "Visit a file in another window.
May create a window, or reuse one."
  "FFind file in other window"
  find-file-other-window)

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

(define-command find-file-other-screen
  "Visit a file in another screen."
  "FFind file in other screen"
  find-file-other-screen)

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
  (let ((method (buffer-get buffer 'REVERT-BUFFER-METHOD)))
    (if method
	(method buffer dont-use-auto-save? dont-confirm?)
	(let ((pathname (buffer-pathname buffer)))
	  (cond ((not pathname)
		 (editor-error
		  "Buffer does not seem to be associated with any file"))
		((not (file-exists? pathname))
		 (editor-error "File "
			       (pathname->string pathname)
			       " no longer exists!"))
		((or dont-confirm?
		     (prompt-for-yes-or-no?
		      (string-append "Revert buffer from file "
				     (pathname->string pathname))))
		 (let ((where (mark-index (buffer-point buffer))))
		   (visit-file buffer pathname)
		   (set-buffer-point!
		    buffer
		    (mark+ (buffer-start buffer) where 'LIMIT)))))))))

(define-command toggle-read-only
  "Change whether this buffer is visiting its file read-only."
  ()
  (lambda ()
    (let ((buffer (current-buffer)))
      ((if (buffer-writeable? buffer)
	   set-buffer-read-only!
	   set-buffer-writeable!)
       buffer))))

(define (visit-file buffer pathname)
  (let ((error?
	 (catch-file-errors (lambda () true)
	   (lambda ()
	     (not (read-buffer buffer pathname))))))
    (let ((pathname (or (buffer-truename buffer) pathname)))
      (if (file-writable? pathname)
	  (set-buffer-writeable! buffer)
	  (set-buffer-read-only! buffer))
      (let ((msg
	     (cond ((not (buffer-read-only? buffer))
		    (and error? "(New file)"))
		   ((not error?)
		    "File is write protected")
		   ((file-attributes pathname)
		    "File exists, but is read-protected.")
		   ((file-attributes (pathname-directory-path pathname))
		    "File not found and directory write-protected")
		   (else
		    "File not found and directory doesn't exist"))))
	(if msg
	    (message msg)))))
  (setup-buffer-auto-save! buffer)
  (initialize-buffer! buffer)
  (let ((filename (os/find-file-initialization-filename pathname)))
    (if filename
	(let ((database
	       (with-output-to-transcript-buffer
		(lambda ()
		  (bind-condition-handler (list condition-type:error)
		      evaluation-error-handler
		    (lambda ()
		      (catch-file-errors (lambda () false)
			(lambda ()
			  (fluid-let ((load/suppress-loading-message? true))
			    (load filename
				  '(EDWIN)
				  edwin-syntax-table))))))))))
	  (if (and (procedure? database)
		   (procedure-arity-valid? database 0))
	      (add-buffer-initialization! buffer database)
	      (message
	       "Ill-formed find-file initialization file: "
	       (os/pathname->display-string (->pathname filename))))))))

(define (standard-scheme-find-file-initialization database)
  ;; DATABASE -must- be a vector whose elements are all three element
  ;; lists.  The car of each element must be a string, and the
  ;; elements must be sorted on those strings.
  (lambda ()
    (let ((entry
	   (let ((pathname (buffer-pathname (current-buffer))))
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
	    (local-set-variable! scheme-environment (cadr entry))
	    (local-set-variable! scheme-syntax-table (caddr entry)))))))

(define (save-buffer buffer)
  (if (buffer-modified? buffer)
      (let ((exponent (command-argument-multiplier-only?)))
	(if (buffer-pathname buffer)
	    (save-buffer-prepare-version buffer)
	    (set-visited-pathname
	     buffer
	     (prompt-for-pathname
	      (string-append "Write buffer " (buffer-name buffer) " to file")
	      false false)))
	(if (memv exponent '(2 3)) (set-buffer-backed-up?! buffer false))
	(write-buffer-interactive buffer)
	(if (memv exponent '(1 3)) (set-buffer-backed-up?! buffer false)))
      (temporary-message "(No changes need to be written)")))

(define (save-some-buffers #!optional no-confirmation?)
  (let ((buffers
	 (list-transform-positive (buffer-list)
	   (lambda (buffer)
	     (and (buffer-modified? buffer)
		  (buffer-pathname buffer))))))
    (if (null? buffers)
	(temporary-message "(No files need saving)")
	(for-each (lambda (buffer)
		    (save-buffer-prepare-version buffer)
		    (if (or (and (not (default-object? no-confirmation?))
				 no-confirmation?)
			    (prompt-for-confirmation?
			     (string-append
			      "Save file "
			      (pathname->string (buffer-pathname buffer)))))
			(write-buffer-interactive buffer)))
		  buffers))))

(define (save-buffer-prepare-version buffer)
  (if pathname-newest
      (let ((pathname (buffer-pathname buffer)))
	(if (and pathname (integer? (pathname-version pathname)))
	    (set-buffer-pathname! buffer
				  (pathname-new-version pathname 'NEWEST))))))

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
  "P"
  (lambda (argument)
    (let ((do-it (lambda () (save-buffer (current-buffer)))))
      (if (eqv? argument 0)
	  (with-variable-value! (ref-variable-object make-backup-files) false
	    do-it)
	  (do-it)))))

(define-command save-some-buffers
  "Saves some modified file-visiting buffers.  Asks user about each one.
With argument, saves all with no questions."
  "P"
  save-some-buffers)

(define-command set-visited-file-name
  "Change name of file visited in current buffer.
The next time the buffer is saved it will go in the newly specified file.
Delete the initial contents of the minibuffer
if you wish to make buffer not be visiting any file."
  "FSet visited file name"
  (lambda (filename)
    (set-visited-pathname (current-buffer)
			  (and (not (string-null? filename))
			       (string->pathname filename)))))

(define (set-visited-pathname buffer pathname)
  (set-buffer-pathname! buffer pathname)
  (set-buffer-truename! buffer false)
  (if pathname
      (begin
       (let ((name (pathname->buffer-name pathname)))
	 (if (not (find-buffer name))
	     (rename-buffer buffer name)))
       (setup-buffer-auto-save! buffer)
       (buffer-modified! buffer))
      (disable-buffer-auto-save! buffer)))

(define-command write-file
  "Store buffer in specified file.
This file becomes the one being visited."
  "FWrite file"
  (lambda (filename)
    (write-file (current-buffer) filename)))

(define (write-file buffer filename)
  (set-visited-pathname buffer (->pathname filename))
  (write-buffer-interactive buffer))

(define-command write-region
  "Store the region in specified file."
  "FWrite region"
  (lambda (filename)
    (write-region (current-region) filename)))

(define-command insert-file
  "Insert contents of file into existing text.
Leaves point at the beginning, mark at the end."
  "FInsert file"
  (lambda (filename)
    (set-current-region! (insert-file (current-point) filename))))

(define (pathname->buffer-name pathname)
  (let ((name (pathname-name pathname)))
    (if name
	(pathname->string
	 (make-pathname false false false
			name (pathname-type pathname) false))
	(let ((name
	       (let ((directory (pathname-directory pathname)))
		 (and (pair? directory)
		      (car (last-pair directory))))))
	  (if (string? name) name "*random*")))))

(define (pathname->buffer pathname)
  (or (list-search-positive (buffer-list)
	(lambda (buffer)
	  (let ((pathname* (buffer-pathname buffer)))
	    (and pathname*
		 (pathname=? pathname pathname*)))))
      (let ((truename (pathname->input-truename pathname)))
	(and truename
	     (list-search-positive (buffer-list)
	       (lambda (buffer)
		 (let ((pathname* (buffer-pathname buffer)))
		   (and pathname*
			(or (pathname=? pathname pathname*)
			    (pathname=? truename pathname*)
			    (let ((truename* (buffer-truename buffer)))
			      (and truename*
				   (pathname=? truename truename*))))))))))))

(define-command copy-file
  "Copy a file; the old and new names are read in the typein window.
If a file with the new name already exists, confirmation is requested first."
  (lambda ()
    (let ((old (prompt-for-input-truename "Copy file" false)))
      (list old (prompt-for-output-truename "Copy to" old))))
  (lambda (old new)
    (if (or (not (file-exists? new))
	    (prompt-for-yes-or-no?
	     (string-append "File "
			    (pathname->string new)
			    " already exists; copy anyway")))
	(begin (copy-file old new)
	       (message "Copied " (pathname->string old)
			" => " (pathname->string new))))))

(define-command rename-file
  "Rename a file; the old and new names are read in the typein window.
If a file with the new name already exists, confirmation is requested first."
  (lambda ()
    (let ((old (prompt-for-input-truename "Rename file" false)))
      (list old (prompt-for-output-truename "Rename to" old))))
  (lambda (old new)
    (let ((do-it
	   (lambda ()
	     (rename-file old new)
	     (message "Renamed " (pathname->string old)
		      " => " (pathname->string new)))))
      (if (file-exists? new)
	  (if (prompt-for-yes-or-no?
	       (string-append "File "
			      (pathname->string new)
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
	     (pathname->string (buffer-default-directory (current-buffer))))))

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
	    (merge-pathnames (->pathname directory)
			     (buffer-default-directory buffer)))))
      (if (not (file-directory? directory))
	  (editor-error (pathname->string directory) " is not a directory"))
      (if (not (unix/file-access directory 1))
	  (editor-error "Cannot cd to "
			(pathname->string directory)
			": Permission denied"))
      (set-buffer-default-directory! buffer directory))))

;;;; Printer Support

#|

(define-command print-file
  "Print a file on the local printer."
  "fPrint File"
  (lambda (filename)
    (print-region (file->region (->pathname filename)))))

(define-command print-buffer
  "Print the current buffer on the local printer."
  ()
  (lambda ()
    (print-region (buffer-region (current-buffer)))))

(define-command print-page
  "Print the current page on the local printer."
  ()
  (lambda ()
    (print-region (page-interior-region (current-point)))))

(define-command print-region
  "Print the current region on the local printer."
  "r"
  (lambda (region)
    (print-region region)))

(define (print-region region)
  (let ((temp (temporary-buffer "*Printout*")))
    (region-insert! (buffer-point temp) region)
    (let ((temp-region (buffer-region temp)))
      (untabify-region temp-region)
      (region->file temp-region print-region-temp-filename))
    (translate-file print-region-temp-filename "PRINTER:")
    (delete-file print-region-temp-filename)
    (kill-buffer temp)))

(define print-region-temp-filename
  "*PRINTOUT")

(define translate-file
  (make-primitive-procedure 'TRANSLATE-FILE))

|#

;;;; Prompting

(define (prompt-for-input-truename prompt default)
  (pathname->input-truename (prompt-for-pathname prompt default true)))

(define (prompt-for-output-truename prompt default)
  (pathname->output-truename (prompt-for-pathname prompt default false)))

(define (prompt-for-directory prompt default require-match?)
  (let ((directory
	 (prompt-for-pathname* prompt default file-directory? require-match?)))
    (if (file-directory? directory)
	(pathname-as-directory directory)
	directory)))

(define-integrable (prompt-for-pathname prompt default require-match?)
  (prompt-for-pathname* prompt default file-exists? require-match?))

(define (prompt-for-pathname* prompt directory
			      verify-final-value? require-match?)
  (let ((directory
	 (if directory
	     (pathname-directory-path directory)
	     (buffer-default-directory (current-buffer)))))
    (prompt-string->pathname
     (prompt-for-completed-string
      prompt
      (os/pathname->display-string directory)
      'INSERTED-DEFAULT
      (lambda (string if-unique if-not-unique if-not-found)
	(filename-complete-string (prompt-string->pathname string directory)
				  if-unique if-not-unique if-not-found))
      (lambda (string)
	(filename-completions-list
	 (prompt-string->pathname string directory)))
      verify-final-value?
      require-match?)
     directory)))

;;;; Filename Completion

(define (filename-complete-string pathname
				  if-unique if-not-unique if-not-found)
  (define (loop directory filenames)
    (let ((unique-case
	   (lambda (filenames)
	     (let ((filename (os/make-filename directory (car filenames))))
	       (if (os/file-directory? filename)
		   (let ((directory (os/filename-as-directory filename)))
		     (let ((filenames (os/directory-list directory)))
		       (if (null? filenames)
			   (if-unique directory)
			   (loop directory filenames))))
		   (if-unique filename)))))
	  (non-unique-case
	   (lambda (filenames*)
	     (let ((string (string-greatest-common-prefix filenames*)))
	       (if-not-unique (os/make-filename directory string)
			      (lambda ()
				(canonicalize-filename-completions
				 directory
				 (list-transform-positive filenames
				   (lambda (filename)
				     (string-prefix? string filename))))))))))
      (if (null? (cdr filenames))
	  (unique-case filenames)
	  (let ((filtered-filenames
		 (list-transform-negative filenames
		   (lambda (filename)
		     (completion-ignore-filename?
		      (os/make-filename directory filename))))))
	    (cond ((null? filtered-filenames)
		   (non-unique-case filenames))
		  ((null? (cdr filtered-filenames))
		   (unique-case filtered-filenames))
		  (else
		   (non-unique-case filtered-filenames)))))))
  (let ((directory (pathname-directory-string pathname))
	(prefix (pathname-name-string pathname)))
    (cond ((not (os/file-directory? directory))
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
	   (let ((filenames
		  (os/directory-list-completions directory prefix)))
	     (if (null? filenames)
		 (if-not-found)
		 (loop directory filenames)))))))

(define (filename-completions-list pathname)
  (let ((directory (pathname-directory-string pathname)))
    (canonicalize-filename-completions
     directory
     (os/directory-list-completions directory
				    (pathname-name-string pathname)))))

(define-integrable (prompt-string->pathname string directory)
  (merge-pathnames (string->pathname (os/trim-pathname-string string))
		   directory))

(define (canonicalize-filename-completions directory filenames)
  (map (lambda (filename)
	 (if (os/file-directory? (os/make-filename directory filename))
	     (os/filename-as-directory filename)
	     filename))
       (sort filenames string<?)))

(define (completion-ignore-filename? filename)
  (and (not (os/file-directory? filename))
       (there-exists? (ref-variable completion-ignored-extensions)
	 (lambda (extension)
	   (string-suffix? extension filename)))))

(define-variable completion-ignored-extensions
  "Completion ignores filenames ending in any string in this list."
  (os/completion-ignored-extensions)
  (lambda (extensions)
    (and (list? extensions)
	 (for-all? extensions
	   (lambda (extension)
	     (and (string? extension)
		  (not (string-null? extension))))))))