;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/fileio.scm,v 1.88 1989/04/05 18:19:54 cph Exp $
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

;;;; File <-> Buffer I/O

(declare (usual-integrations))

;;;; Input

(define (read-buffer buffer pathname)
  (set-buffer-writeable! buffer)
  (set-buffer-pathname! buffer pathname)
  (let ((truename (pathname->input-truename pathname)))
    (if truename
	(begin
	 (let ((region (file->region-interactive truename)))
	   (region-delete! (buffer-unclipped-region buffer))
	   (region-insert! (buffer-start buffer) region))
	 (set-buffer-point! buffer (buffer-start buffer))
	 (set-buffer-modification-time! buffer
					(file-modification-time truename))))
    (set-buffer-truename! buffer truename))
  (set-buffer-save-length! buffer)
  (buffer-not-modified! buffer)
  (undo-done! (buffer-point buffer))
  (buffer-truename buffer))

(define (initialize-buffer! buffer)
  (initialize-buffer-modes! buffer)
  (initialize-buffer-local-variables! buffer))

(define (insert-file mark pathname)
  (let ((truename (pathname->input-truename pathname)))
    (if truename
	(region-insert! mark (file->region-interactive truename))
	(editor-error "File \"" (pathname->string pathname) "\" not found"))))

(define (file->region-interactive truename)
  (let ((filename (pathname->string truename)))
    (temporary-message "Reading file \"" filename "\"")
    (let ((region (file->region truename)))
      (append-message " -- done")
      region)))

(define (file->region pathname)
  (call-with-input-file pathname port->region))

(define (port->region port)
  (group-region
   (make-group
    (let ((rest->string (input-port/operation port 'REST->STRING)))
      (if rest->string
	  (rest->string port)
	  (read-string char-set:null port))))))

;;;; Buffer Mode Initialization

(define initialize-buffer-modes!)
(let ()

(set! initialize-buffer-modes!
(named-lambda (initialize-buffer-modes! buffer)
  (let ((mode
	 (or (let ((mode-name (parse-buffer-mode-header buffer)))
	       (and mode-name
		    (let ((mode (string-table-get editor-modes mode-name)))
		      (and mode
			   (mode-major? mode)
			   mode))))
	     (filename-default-mode buffer))))
    (set-buffer-major-mode! buffer
			    (or mode (ref-variable "Editor Default Mode"))))))

(define (filename-default-mode buffer)
  (let ((entry
	 (let ((pathname (buffer-pathname buffer)))
	   (and pathname
		(let ((type (pathname-type pathname)))
		  (and (string? type)
		       (assoc-string-ci
			type
			(ref-variable "File Type to Major Mode"))))))))
    (and entry (cdr entry))))

(define assoc-string-ci
  (association-procedure string-ci=? car))

(define (parse-buffer-mode-header buffer)
  (fluid-let (((ref-variable "Case Fold Search") true))
    (let ((start (buffer-start buffer)))
      (let ((end (line-end start 0)))
	(let ((start (re-search-forward "-\\*-[ \t]*" start end)))
	  (and start
	       (re-search-forward "[ \t]*-\\*-" start end)
	       (parse-mode-header start (re-match-start 0))))))))

(define (parse-mode-header start end)
  (if (not (char-search-forward #\: start end))
      (extract-string start end)
      (let ((mode-mark (re-search-forward "mode:[ \t]*" start end)))
	(and mode-mark
	     (extract-string mode-mark
			     (if (re-search-forward "[ \t]*;" mode-mark end)
				 (re-match-start 0)
				 end))))))

)

;;;; Local Variable Initialization

(define-variable "Local Variable Search Limit"
  "The maximum number of characters searched when looking for local variables
at the end of a file."
  3000)

(define initialize-buffer-local-variables!)
(let ()

(set! initialize-buffer-local-variables!
(named-lambda (initialize-buffer-local-variables! buffer)
  (let ((end (buffer-end buffer)))
    (let ((start
	   (with-narrowed-region!
	    (make-region (mark- end
				(ref-variable "Local Variable Search Limit")
				'LIMIT)
			 end)
	    (lambda ()
	      (backward-one-page end)))))
      (if start
	  (fluid-let (((ref-variable "Case Fold Search") true))
	    (if (re-search-forward "Edwin Variables:[ \t]*" start)
		(parse-local-variables buffer
				       (re-match-start 0)
				       (re-match-end 0)))))))))

(define (evaluate sexp)
  (scode-eval (syntax sexp system-global-syntax-table)
	      system-global-environment))

(define ((local-binding-thunk name value))
  (make-local-binding! name value))

(define (parse-local-variables buffer start end)
  (let ((prefix (extract-string (line-start start 0) start))
	(suffix (extract-string end (line-end end 0))))
    (let ((prefix? (not (string-null? prefix)))
	  (suffix? (not (string-null? suffix))))
      (define (loop mark)
	(let ((start (line-start mark 1)))
	  (if (not start) (editor-error "Missing local variables entry"))
	  (do-line start (line-end start 0))))

      (define (do-line start end)
	(define (check-suffix mark)
	  (if (and suffix? (not (match-forward suffix mark)))
	      (editor-error "Local variables entry is missing the suffix")))
	(let ((m1
	       (horizontal-space-end
		(if prefix?
		    (or (match-forward prefix start)
			(editor-error
			 "Local variables entry is missing the prefix"))
		    start))))
	  (let ((m2 (if (char-search-forward #\: m1 end)
			(re-match-start 0)
			(editor-error
			 "Missing colon in local variables entry"))))
	    (let ((var (extract-string m1 (horizontal-space-start m2)))
		  (m3 (horizontal-space-end (mark1+ m2))))
	      (if (not (string-ci=? var "End"))
		  (with-input-from-mark m3 read
		    (lambda (val m4)
		      (check-suffix (horizontal-space-end m4))
		      (if (string-ci=? var "Mode")
			  (let ((mode (string-table-get
				       editor-modes
				       (extract-string m3 m4))))
			    (if mode
				((if (mode-major? mode)
				     set-buffer-major-mode!
				     enable-buffer-minor-mode!)
				 buffer mode)))
			  (call-with-current-continuation
			   (lambda (continuation)
			     (bind-condition-handler '()
				 (lambda (condition)
				   (and (not (condition/internal? condition))
					(error? condition)
					(begin
					  (editor-beep)
					  (message "Error while processing local variable: "
						   var)
					  (continuation false))))
			       (lambda ()
				 (if (string-ci=? var "Eval")
				     (evaluate val)
				     (add-buffer-initialization!
				      buffer
				      (local-binding-thunk
				       (variable-symbol (name->variable var))
				       (evaluate val)))))))))
		      (loop m4))))))))

      (loop start))))

)

;;;; Output

(define-variable "Require Final Newline"
  "True says silently put a newline at the end whenever a file is saved.
Neither false nor true says ask user whether to add a newline in each
such case.  False means don't add newlines."
  false)

(define-variable "Make Backup Files"
  "*Create a backup of each file when it is saved for the first time.
This can be done by renaming the file or by copying.

Renaming means that Edwin renames the existing file so that it is a
backup file, then writes the buffer into a new file.  Any other names
that the old file had will now refer to the backup file.
The new file is owned by you and its group is defaulted.

Copying means that Edwin copies the existing file into the backup
file, then writes the buffer on top of the existing file.  Any other
names that the old file had will now refer to the new (edited) file.
The file's owner and group are unchanged.

The choice of renaming or copying is controlled by the variables
Backup By Copying, Backup By Copying When Linked and
Backup By Copying When Mismatch."
  true)

(define-variable "Backup By Copying"
  "*True means always use copying to create backup files.
See documentation of variable  Make Backup Files."
 false)

(define-variable "Trim Versions Without Asking"
  "*If true, deletes excess backup versions silently.
Otherwise asks confirmation."
  false)

(define (write-buffer-interactive buffer)
  ;; Need to check for correct modification time here.
  (let ((truename (pathname->output-truename (buffer-pathname buffer))))
    (let ((writable? (file-writable? truename)))
      (if (or writable?
	      (prompt-for-yes-or-no?
	       (string-append "File \""
			      (pathname-name-string truename)
			      "\" is write-protected; try to save anyway"))
	      (editor-error
	       "Attempt to save to a file which you aren't allowed to write"))
	  (begin
	   (if (not (or (verify-visited-file-modification-time buffer)
			(not (file-exists? truename))
			(prompt-for-yes-or-no?
			 "Disk file has changed since visited or saved.  Save anyway")))
	       (editor-error "Save not confirmed"))
	   (let ((modes
		  (and (not (buffer-backed-up? buffer))
		       (backup-buffer! buffer truename))))
	     (require-newline buffer)
	     (if (not (or writable? modes))
		 (begin
		   (set! modes (file-modes truename))
		   (set-file-modes! truename #o777)))
	     (write-buffer buffer)
	     (if modes
		 (bind-condition-handler '()
		     (lambda (condition)
		       (and (not (condition/internal? condition))
			    (error? condition)
			    ((condition/continuation condition) unspecific)))
		   (lambda ()
		     (set-file-modes! truename modes))))))))))

(define (verify-visited-file-modification-time buffer)
  (let ((truename (buffer-truename buffer))
	(modification-time (buffer-modification-time buffer)))
    (or (not truename)
	(not modification-time)
	(let ((new-time (file-modification-time truename)))
	  (and new-time
	       (or (= modification-time new-time)
		   (and (positive? modification-time)
			(positive? new-time)
			(= 1 (abs (- modification-time new-time))))))))))

(define (write-buffer buffer)
  (let ((truename
	 (write-region (buffer-unclipped-region buffer)
		       (buffer-pathname buffer))))
    (if truename
	(begin
	  (set-buffer-truename! buffer truename)
	  (delete-auto-save-file! buffer)
	  (set-buffer-save-length! buffer)
	  (buffer-not-modified! buffer)
	  (set-buffer-modification-time! buffer
					 (file-modification-time truename))))))

(define (write-region region pathname)
  (let ((truename (pathname->output-truename pathname)))
    (temporary-message "Writing file \"" (pathname->string truename) "\"")
    (region->file region truename)
    (append-message " -- done")
    truename))

(define (region->file region pathname)
  (call-with-output-file pathname
    (lambda (port)
      (write-string (region->string region) port))))

(define (require-newline buffer)
  (let ((require-final-newline? (ref-variable "Require Final Newline")))
    (if require-final-newline?
	(without-group-clipped! (buffer-group buffer)
	  (lambda ()
	    (let ((end (buffer-end buffer)))
	      (if (let ((last-char (extract-left-char end)))
		    (and last-char
			 (not (eqv? #\newline last-char))
			 (or (eq? require-final-newline? true)
			     (prompt-for-yes-or-no?
			      (string-append
			       "Buffer " (buffer-name buffer)
			       " does not end in newline.  Add one")))))
		  (insert-newline end))))))))

(define (backup-buffer! buffer truename)
  (let ((continue-with-false (lambda () false)))
    (and truename
	 (ref-variable "Make Backup Files")
	 (not (buffer-backed-up? buffer))
	 (file-exists? truename)
	 (os/backup-buffer? truename)
	 (catch-file-errors
	  continue-with-false
	  (lambda ()
	    (with-values (lambda () (os/buffer-backup-pathname truename))
	      (lambda (backup-pathname targets)
		(let ((modes
		       (catch-file-errors
			(lambda ()
			  (let ((filename (os/default-backup-filename)))
			    (temporary-message
			     "Cannot write backup file; backing up in \""
			     filename
			     "\"")
			    (copy-file truename
				       (string->pathname filename))
			    false))
			(lambda ()
			  (if (or (file-symbolic-link? truename)
				  (ref-variable "Backup By Copying")
				  (os/backup-by-copying? truename))
			      (begin
				(copy-file truename backup-pathname)
				false)
			      (begin
				(catch-file-errors
				 (lambda () false)
				 (lambda ()
				   (delete-file backup-pathname)))
				(rename-file truename backup-pathname)
				(file-modes backup-pathname)))))))
		  (set-buffer-backed-up?! buffer true)
		  (if (and (not (null? targets))
			   (or (ref-variable "Trim Versions Without Asking")
			       (prompt-for-confirmation?
				(string-append
				 "Delete excess backup versions of "
				 (pathname->string
				  (buffer-pathname buffer))))))
		      (for-each (lambda (target)
				  (catch-file-errors continue-with-false
						     (lambda ()
						       (delete-file target))))
				targets))
		  modes))))))))

(define (catch-file-errors if-error thunk)
  (call-with-current-continuation
   (lambda (continuation)
     (bind-condition-handler
	 (list error-type:file)
	 (lambda (condition)
	   condition
	   (continuation (if-error)))
       thunk))))