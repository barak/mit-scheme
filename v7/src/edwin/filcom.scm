;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/filcom.scm,v 1.132 1989/04/05 18:19:16 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989 Massachusetts Institute of Technology
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

;;;; File Commands

(declare (usual-integrations))

(define-command ("Toggle Read Only")
  "Change whether this buffer is visiting its file read-only."
  (let ((buffer (current-buffer)))
    ((if (buffer-writeable? buffer)
	 set-buffer-read-only!
	 set-buffer-writeable!)
     buffer)))

(define-command ("Find File")
  "Visit a file in its own buffer.
If the file is already in some buffer, select that buffer.
Otherwise, visit the file in a buffer named after the file."
  (find-file (prompt-for-pathname "Find File" (current-default-pathname))))

(define-command ("Find File Other Window")
  "Visit a file in another window.
May create a window, or reuse one."
  (find-file-other-window
   (prompt-for-pathname "Find File Other Window" (current-default-pathname))))

(define-command ("^R Find Alternate File")
  "Find a file in its own buffer, killing the current buffer.
Like \\[Kill Buffer] followed by \\[Find File]."
  (let ((buffer (current-buffer)))
    (if (not (buffer-pathname buffer))
	(editor-error "Buffer not visiting any file"))
    (let ((pathname 
	   (prompt-for-pathname "Find Alternate File"
				(current-default-pathname))))
      (define (kernel)
	(kill-buffer-interactive buffer)
	(find-file pathname))
      (if (not (other-buffer buffer))
	  (let ((buffer* (new-buffer "*dummy*")))
	    (kernel)
	    (kill-buffer buffer*))
	  (kernel)))))

(define (find-file pathname)
  (select-buffer (find-file-noselect pathname)))

(define (find-file-other-window pathname)
  (select-buffer-other-window (find-file-noselect pathname)))

(define (find-file-noselect pathname)
  (let ((buffer (pathname->buffer pathname)))
    (or buffer
	(let ((buffer (new-buffer (pathname->buffer-name pathname))))
	  (after-find-file
	   buffer
	   (catch-file-errors (lambda () true)
			      (lambda () (not (read-buffer buffer pathname)))))
	  buffer))))

(define (after-find-file buffer error?)
  (let ((pathname (or (buffer-truename buffer) (buffer-pathname buffer))))
    (if (or (not pathname) (file-writable? pathname))
	(set-buffer-writeable! buffer)
	(set-buffer-read-only! buffer)))
  (let ((msg
	 (cond ((not (buffer-read-only? buffer))
		(and error? "(New file)"))
	       ((not error?)
		"File is write protected")
	       ((file-attributes (buffer-pathname buffer))
		"File exists, but is read-protected.")
	       ((file-attributes
		 (pathname-directory-path (buffer-pathname buffer)))
		"File not found and directory write-protected")
	       (else
		"File not found and directory doesn't exist"))))
    (if msg
	(message msg)))
  (setup-buffer-auto-save! buffer)
  (initialize-buffer! buffer))

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

(define (pathname=? x y)
  (string=? (pathname->string x)
	    (pathname->string y)))

(define-command ("^R Save File" argument)
  "Save current buffer in visited file if modified.  Versions described below.

By default, makes the previous version into a backup file
 if previously requested or if this is the first save.
With 1 or 3 \\[^R Universal Argument]'s, marks this version
 to become a backup when the next save is done.
With 2 or 3 \\[^R Universal Argument]'s,
 unconditionally makes the previous version into a backup file.
With argument of 0, never makes the previous version into a backup file.

If a file's name is FOO, the names of its numbered backup versions are
 FOO.~i~ for various integers i.  A non-numbered backup file is called FOO~.
Numeric backups (rather than FOO~) will be made if value of
 `Version Control' is not the atom `never' and either there are already
 numeric versions of the file being backed up, or `Version Control' is
 not #F.
We don't want excessive versions piling up, so there are variables
 `Kept Old Versions', which tells Edwin how many oldest versions to keep,
 and `Kept New Versions', which tells how many newest versions to keep.
 Defaults are 2 old versions and 2 new.
If `Trim Versions Without Asking' is false, system will query user
 before trimming versions.  Otherwise it does it silently."
  (let ((do-it (lambda () (save-file (current-buffer)))))
    (if (eqv? argument 0)
	(fluid-let (((ref-variable "Make Backup Files") false))
	  (do-it))
	(do-it))))

(define (save-file buffer)
  (if (buffer-modified? buffer)
      (let ((exponent (command-argument-multiplier-only?)))
	(if (buffer-pathname buffer)
	    (save-buffer-prepare-version buffer)
	    (set-visited-pathname buffer
				  (prompt-for-pathname
				   (string-append "Write buffer '"
						  (buffer-name buffer)
						  "' to file")
				   false)))
	(if (memv exponent '(2 3)) (set-buffer-backed-up?! buffer false))
	(write-buffer-interactive buffer)
	(if (memv exponent '(1 3)) (set-buffer-backed-up?! buffer false)))
      (temporary-message "(No changes need to be written)")))

(define-command ("Save Some Buffers" argument)
  "Saves some modified file-visiting buffers.  Asks user about each one.
With argument, saves all with no questions."
  (save-some-buffers argument))

(define (save-some-buffers #!optional no-confirmation?)
  (let ((buffers
	 (list-transform-positive (buffer-list)
	   (lambda (buffer)
	     (and (buffer-modified? buffer)
		  (buffer-pathname buffer))))))
    (if (null? buffers)
	(temporary-message "(No buffers need saving)")
	(for-each (lambda (buffer)
		    (save-buffer-prepare-version buffer)
		    (if (or (and (not (default-object? no-confirmation?))
				 no-confirmation?)
			    (prompt-for-confirmation?
			     (string-append
			      "Save file '"
			      (pathname->string (buffer-pathname buffer))
			      "'")))
			(write-buffer-interactive buffer)))
		  buffers))))

(define (save-buffer-prepare-version buffer)
  (let ((pathname (buffer-pathname buffer)))
    (if (and pathname (integer? (pathname-version pathname)))
	(set-buffer-pathname! buffer (newest-pathname pathname)))))

(define-command ("Set Visited File Name" argument)
  "Change name of file visited in current buffer to given name.
With an argument, means make buffer not be visiting any file.
The next time the buffer is saved it will go in the newly specified file. "
  (set-visited-pathname
   (current-buffer)
   (and (not argument)
	(prompt-for-pathname "Set Visited File Name"
			     (current-default-pathname)))))

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

(define-command ("Write File")
  "Store buffer in specified file.
This file becomes the one being visited."
  (write-file (current-buffer)
	      (prompt-for-pathname "Write File" (current-default-pathname))))

(define (write-file buffer pathname)
  (set-visited-pathname buffer pathname)
  (write-buffer-interactive buffer))

(define-command ("Write Region")
  "Store the region in specified file."
  (write-region (current-region)
		(prompt-for-pathname "Write Region"
				     (current-default-pathname))))

(define-variable "Previous Inserted File"
  "Pathname of the file that was most recently inserted."
  false)

(define-command ("Insert File")
  "Insert contents of file into existing text.
Leaves point at the beginning, mark at the end."
  (let ((pathname
	 (prompt-for-pathname
	  "Insert File"
	  (newest-pathname (or (ref-variable "Previous Inserted File")
			       (buffer-pathname (current-buffer)))))))
    (set-variable! "Previous Inserted File" pathname)
    (set-current-region! (insert-file (current-point) pathname))))

(define-command ("Revert Buffer" argument)
  "Loads current buffer with version of file from disk."
  (revert-buffer (current-buffer) argument false))

(define (revert-buffer buffer argument dont-confirm?)
  (let ((method (buffer-get buffer 'REVERT-BUFFER-METHOD)))
    (if method
	(method argument)
	(let ((pathname (buffer-pathname buffer)))
	  (cond ((not pathname)
		 (editor-error
		  "Buffer does not seem to be associated with any file"))
		((not (file-exists? pathname))
		 (editor-error "File "
			       (pathname-name-string pathname)
			       " no longer exists!"))
		((or dont-confirm?
		     (prompt-for-yes-or-no?
		      (string-append "Revert buffer from file "
				     (pathname-name-string pathname))))
		 (let ((where (mark-index (buffer-point buffer))))
		   (read-buffer buffer pathname)
		   (set-current-point!
		    (mark+ (buffer-start buffer) where 'LIMIT))
		   (after-find-file buffer false))))))))

(define-command ("Copy File")
  "Copy a file; the old and new names are read in the typein window.
If a file with the new name already exists, confirmation is requested first."
  (let ((old (prompt-for-input-truename "Copy File"
					(buffer-pathname (current-buffer)))))
    (let ((new (prompt-for-output-truename "Copy to" old)))
      (if (or (not (file-exists? new))
	      (prompt-for-yes-or-no?
	       (string-append "File '"
			      (pathname->string new)
			      "' already exists; copy anyway")))
	  (begin (copy-file old new)
		 (message "Copied '" (pathname->string old)
			  "' => '" (pathname->string new) "'"))))))

(define-command ("Rename File")
  "Rename a file; the old and new names are read in the typein window.
If a file with the new name already exists, confirmation is requested first."
  (let ((old (prompt-for-input-truename "Rename File"
					(buffer-pathname (current-buffer)))))
    (let ((new (prompt-for-output-truename "Rename to" old)))
      (define (do-it)
	(rename-file old new)
	(message "Renamed '" (pathname->string old)
		 "' => '" (pathname->string new) "'"))
      (if (file-exists? new)
	  (if (prompt-for-yes-or-no?
	       (string-append "File '"
			      (pathname->string new)
			      "' already exists; rename anyway"))
	      (begin (delete-file new) (do-it)))
	  (do-it)))))

(define-command ("Delete File")
  "Delete a file; the name is read in the typein window."
  (let ((old (prompt-for-input-truename "Delete File"
					(buffer-pathname (current-buffer)))))
    (if (prompt-for-confirmation?
	 (string-append "Delete '"
			(pathname->string old)
			"'"))
	(delete-file old))))

;;;; Printer Support

(define-command ("Print File")
  "Print a file on the local printer."
  (print-region
   (file->region
    (prompt-for-input-truename "Print File"
			       (buffer-pathname (current-buffer))))))

(define-command ("Print Buffer")
  "Print the current buffer on the local printer."
  (print-region (buffer-region (current-buffer))))

(define-command ("Print Page")
  "Print the current page on the local printer."
  (print-region (page-interior-region (current-point))))

(define-command ("Print Region")
  "Print the current region on the local printer."
  (print-region (current-region)))

#|

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

;;;; Supporting Stuff

(define *default-pathname*)

(define-command ("^R Complete Filename")
  "Attempt to complete the filename being edited in the echo area."
  (let ((region (buffer-region (current-buffer))))
    (let ((string (region->string region)))
      (if (string-null? string)
	  (insert-string
	   (pathname->string
	    (or (pathname->input-truename *default-pathname*)
		*default-pathname*)))
	  (complete-pathname (prompt-string->pathname string)
			     *default-pathname*
	    (lambda (pathname)
	      (region-delete! region)
	      (insert-string (pathname->string pathname)))
	    (lambda (string start end)
	      (region-delete! region)
	      (insert-string (substring string start end)))
	    editor-beep)))))

(define-command ("^R List Filename Completions")
  "List the possible completions for the filename being input."
  (list-completions
   (map pathname->string
	(pathname-completions
	 (prompt-string->pathname
	  (region->string (buffer-region (current-buffer))))
	 *default-pathname*))))

;;; Derives buffername from pathname

(define (pathname->buffer-name pathname)
  (let ((name (pathname-name pathname)))
    (if name
	(pathname->string
	 (make-pathname false false false
			name
			(pathname-type pathname)
			false))
	(let ((name
	       (let ((directory (pathname-directory pathname)))
		 (and (pair? directory)
		      (car (last-pair directory))))))
	  (if (string? name)
	      name
	      "*random*")))))

(define-integrable (prompt-string->pathname string)
  (string->pathname (os/trim-pathname-string string)))

;;;; Prompting

(define (prompt-for-input-truename prompt default)
  (let ((path (prompt-for-pathname prompt default)))
    (if (file-exists? path)
	(pathname->input-truename path)
	(editor-error "'" (pathname->string path) "' does not exist"))))

(define (prompt-for-output-truename prompt default)
  (pathname->output-truename (prompt-for-pathname prompt default)))

(define (prompt-for-pathname prompt #!optional default)
  (let ((default
	  (or (and (not (default-object? default)) default)
	      (current-default-pathname))))
    (prompt-string->pathname
     (fluid-let ((*default-pathname* default))
       (prompt-for-completed-string prompt
				    (pathname-directory-string default)
				    'INSERTED-DEFAULT
				    false
				    'NO-COMPLETION
				    prompt-for-pathname-mode)))))

(define (prompt-for-directory prompt default-pathname)
  (let ((pathname (prompt-for-pathname prompt default-pathname)))
    (if (file-directory? pathname)
	(pathname-as-directory pathname)
	pathname)))

(define (current-default-pathname)
  (newest-pathname
   (let ((buffer (current-buffer)))
     (or (buffer-pathname buffer)
	 (buffer-truename buffer)))))

(define (newest-pathname pathname)
  (pathname-new-version (or pathname (working-directory-pathname))
			(and pathname-newest 'NEWEST)))

(define-major-mode "Prompt for Pathname" "Fundamental"
  "Major mode for entering pathnames.
\\[^R Terminate Input] indicates that you are done entering the pathname.
\\[^R Complete Filename] will complete the pathname.
\\[^R List Filename Completions] will show you all possible completions.
\\[^R Yank Default String] will insert the default (if there is one.)")

(define-key "Prompt for Pathname" #\Return "^R Terminate Input")
(define-key "Prompt for Pathname" #\C-M-Y "^R Yank Default String")
(define-key "Prompt for Pathname" #\Space "^R Complete Filename")
(define-key "Prompt for Pathname" #\Tab "^R Complete Filename")
(define-key "Prompt for Pathname" #\? "^R List Filename Completions")