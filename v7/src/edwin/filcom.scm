;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1986 Massachusetts Institute of Technology
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
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; File Commands

(declare (usual-integrations))
(using-syntax edwin-syntax-table

(define-command ("Toggle Read Only" argument)
  "Change whether this buffer is visiting its file read-only."
  (let ((buffer (current-buffer)))
    ((if (buffer-writeable? buffer)
	 set-buffer-read-only!
	 set-buffer-writeable!)
     buffer)))

(define-command ("Find File" argument)
  "Visit a file in its own buffer.
If the file is already in some buffer, select that buffer.
Otherwise, visit the file in a buffer named after the file."
  (find-file (prompt-for-pathname "Find File" (current-default-pathname))))

(define-command ("Find File Other Window" argument)
  "Visit a file in another window.
May create a window, or reuse one."
  (find-file-other-window
   (prompt-for-pathname "Find File Other Window" (current-default-pathname))))

(define-command ("^R Find Alternate File" argument)
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

(define ((file-finder select-buffer) pathname)
  (let ((buffer (pathname->buffer pathname)))
    (if buffer
	(select-buffer buffer)
	(let ((buffer (new-buffer (pathname->buffer-name pathname))))
	  (read-buffer buffer pathname)
	  (select-buffer buffer)))))

(define find-file
  (file-finder select-buffer))

(define find-file-other-window
  (file-finder select-buffer-other-window))

(define find-file-noselect
  (file-finder identity-procedure))

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

(define (current-default-pathname)
  (newest-pathname (buffer-pathname (current-buffer))))

(define-command ("^R Save File" argument)
  "Save visited file on disk if modified."
  (save-file (current-buffer)))

(define (save-file buffer)
  (if (buffer-modified? buffer)
      (begin (if (buffer-pathname buffer)
		 (save-buffer-prepare-version buffer)
		 (set-visited-pathname buffer
				       (prompt-for-pathname
					(string-append "Write buffer '"
						       (buffer-name buffer)
						       "' to file")
					#!FALSE)))
	     (write-buffer-interactive buffer))
      (temporary-message "(No changes need to be written)")))

(define-command ("Save Some Buffers" argument)
  "Saves some modified file-visiting buffers.  Asks user about each one.
With argument, saves all with no questions."
  (save-some-buffers argument))

(define (save-some-buffers #!optional no-confirmation?)
  (if (unassigned? no-confirmation?) (set! no-confirmation? #!FALSE))
  (let ((buffers
	 (list-transform-positive (buffer-list)
	   (lambda (buffer)
	     (and (buffer-modified? buffer)
		  (buffer-pathname buffer))))))
    (if (null? buffers)
	(temporary-message "(No buffers need saving)")
	(for-each (lambda (buffer)
		    (save-buffer-prepare-version buffer)
		    (if (or no-confirmation?
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
  (set-visited-pathname (current-buffer)
			(if argument
			    #!FALSE
			    (prompt-for-pathname "Set Visited File Name"
						 (current-default-pathname)))))

(define (set-visited-pathname buffer pathname)
  (set-buffer-pathname! buffer pathname)
  (set-buffer-truename! buffer #!FALSE)
  (if pathname
      (begin (let ((name (pathname->buffer-name pathname)))
	       (if (not (find-buffer name))
		   (rename-buffer buffer name)))
	     (setup-buffer-auto-save! buffer)
	     (buffer-modified! buffer))
      (disable-buffer-auto-save! buffer)))

(define-command ("Write File" argument)
  "Store buffer in specified file.
This file becomes the one being visited."
  (write-file (current-buffer)
	      (prompt-for-pathname "Write File" (current-default-pathname))))

(define (write-file buffer pathname)
  (set-visited-pathname buffer pathname)
  (write-buffer-interactive buffer))

(define-command ("Write Region" argument)
  "Store the region in specified file."
  (write-region (current-region)
		(prompt-for-pathname "Write Region"
				     (current-default-pathname))))

(define-variable "Previous Inserted File"
  "Pathname of the file that was most recently inserted."
  #!FALSE)

(define-command ("Insert File" argument)
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
  (let ((buffer (current-buffer)))
    (let ((method (buffer-get buffer 'REVERT-BUFFER-METHOD)))
      (if method
	  (method argument)
	  (let ((pathname (buffer-pathname buffer))
		(point (current-point))
		(window (current-window)))
	    (if (not pathname) (editor-error "No file to revert from"))
	    (if (prompt-for-yes-or-no? "Restore file from disk")
		(let ((y-point (window-point-y window))
		      (where (mark-index point)))
		  (read-buffer buffer pathname)
		  (set-current-point!
		   (mark+ (buffer-start buffer) where 'LIMIT))
		  (window-scroll-y-absolute! window y-point))))))))

(define-command ("Copy File" argument)
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

(define-command ("Rename File" argument)
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

(define-command ("Delete File" argument)
  "Delete a file; the name is read in the typein window."
  (let ((old (prompt-for-input-truename "Delete File"
					(buffer-pathname (current-buffer)))))
    (if (prompt-for-confirmation?
	 (string-append "Delete '"
			(pathname->string old)
			"'"))
	(delete-file old))))

;;;; Printer Support

(define-command ("Print File" argument)
  "Print a file on the local printer."
  (print-region
   (file->region
    (prompt-for-input-truename "Print File"
			       (buffer-pathname (current-buffer))))))

(define-command ("Print Buffer" argument)
  "Print the current buffer on the local printer."
  (print-region (buffer-region (current-buffer))))

(define-command ("Print Page" argument)
  "Print the current page on the local printer."
  (print-region (page-interior-region (current-point))))

(define-command ("Print Region" argument)
  "Print the current region on the local printer."
  (print-region (current-region)))

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

;;;; Supporting Stuff

(define *default-pathname*)

(define-command ("^R Complete Filename" argument)
  "Attempt to complete the filename being edited in the echo area."
  (let ((buffer (current-buffer)))
    (let ((region (buffer-region buffer)))
      (let ((string (region->string region)))
	(if (string-null? string)
	    (begin (insert-string
		    (let ((truename
			   (pathname->input-truename *default-pathname*)))
		      (pathname->string (or truename *default-pathname*))))
		   (insert-string " "))
	    (complete-pathname (string->pathname string) *default-pathname*
	      (lambda (pathname)
		(region-delete! region)
		(insert-string (pathname->string pathname))
		(insert-string " "))
	      (lambda (string start end)
		(region-delete! region)
		(insert-string (substring string start end)))
	      beep))))))

(define-command ("^R List Filename Completions" argument)
  "List the possible completions for the filename being input."
  ((access list-completions prompt-package)
   (map pathname->string
	(pathname-completions
	 (string->pathname (region->string (buffer-region (current-buffer))))
	 *default-pathname*))))

;;; Derives buffername from pathname

(define (pathname->buffer-name pathname)
  (pathname-extract-string pathname 'NAME 'TYPE))

;;;; Prompting

(define (prompt-for-input-truename prompt default)
  (let ((path (prompt-for-pathname prompt default)))
    (if (file-exists? path)
	(pathname->input-truename path)
	(editor-error "'" (pathname->string path) "' does not exist"))))

(define (prompt-for-output-truename prompt default)
  (pathname->output-truename (prompt-for-pathname prompt default)))

(define (prompt-for-pathname prompt #!optional default)
  (if (unassigned? default) (set! default #!FALSE))
  (fluid-let ((*default-pathname* (or default (get-default-pathname)))
	      (*pathname-cache* #!FALSE))
    (let ((string
	   (prompt-for-completed-string prompt
					(pathname->string *default-pathname*)
					'VISIBLE-DEFAULT
					#!FALSE
					'NO-COMPLETION
					prompt-for-pathname-mode)))
      (cond ((string-null? string)
	     *default-pathname*)
	    ;; If pathname was completed, it should be exact.  But we
	    ;; do a merge of the directory part in case the completed
	    ;; file name was edited.
	    ((char=? #\Space (string-ref string (-1+ (string-length string))))
	     (merge-pathnames
	      (string->pathname
	       (substring string 0 (-1+ (string-length string))))
	      (pathname-extract *default-pathname* 'DEVICE 'DIRECTORY)))
	    ;; If it was quoted, then it may have strange name components,
	    ;; so we just default the directory part, taking the name as is.
	    ((char=? #\' (string-ref string 0))
	     (merge-pathnames
	      (string->pathname (substring string 1 (string-length string)))
	      (pathname-extract *default-pathname* 'DEVICE 'DIRECTORY)))
	    ;; But normally we just do ordinary defaulting.
	    (else
	     (merge-pathnames (string->pathname string)
			      *default-pathname*))))))

(define (newest-pathname pathname)
  (pathname-new-version (or pathname (get-default-pathname))
			'NEWEST))

(define (get-default-pathname)
  (merge-pathnames (ref-variable "Default Pathname")
		   (working-directory-pathname)))

(define-variable "Default Pathname"
  "Pathname to use for default when no other is available"
  (string->pathname "FOO.SCM.0"))

(define-major-mode "Prompt for Pathname" "Fundamental"
  "Major mode for entering pathnames.
\\[^R Terminate Input] indicates that you are done entering the pathname.
\\[^R Complete Filename] will complete the pathname.
\\[^R List Filename Completions] will show you all possible completions.
\\[^R Yank Default String] will insert the default (if there is one.)"
  'DONE)

(define-key "Prompt for Pathname" #\Return "^R Terminate Input")
(define-key "Prompt for Pathname" #\C-M-Y "^R Yank Default String")
(define-key "Prompt for Pathname" #\Space "^R Complete Filename")
(define-key "Prompt for Pathname" #\Tab "^R Complete Filename")
(define-key "Prompt for Pathname" #\? "^R List Filename Completions")

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: edwin-package
;;; Scheme Syntax Table: edwin-syntax-table
;;; Tags Table Pathname: (access edwin-tags-pathname edwin-package)
;;; End:
