;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/fileio.scm,v 1.103 1991/11/04 20:51:04 cph Exp $
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

;;;; File <-> Buffer I/O

(declare (usual-integrations))

;;;; Input

(define-variable read-file-message
  "If true, messages are displayed when files are read into the editor."
  false
  boolean?)

(define (read-buffer buffer pathname visit?)
  (set-buffer-writeable! buffer)
  (let ((truename
	 (catch-file-errors (lambda () false)
			    (lambda () (->truename pathname)))))
    (if truename
	(begin
	  ;; Set modified so that file supercession check isn't done.
	  (set-group-modified! (buffer-group buffer) true)
	  (region-delete! (buffer-unclipped-region buffer))
	  (%insert-file (buffer-start buffer) truename)
	  (set-buffer-point! buffer (buffer-start buffer))))
    (if visit?
	(begin
	  (if truename
	      (set-buffer-modification-time!
	       buffer
	       (file-modification-time truename)))
	  (set-buffer-pathname! buffer pathname)
	  (set-buffer-truename! buffer truename)
	  (set-buffer-save-length! buffer)
	  (buffer-not-modified! buffer)
	  (undo-done! (buffer-point buffer))))
    truename))

(define (insert-file mark filename)
  (%insert-file
   mark
   (bind-condition-handler (list condition-type:file-error)
       (lambda (condition)
	 condition
	 (editor-error "File " (->namestring filename) " not found"))
     (lambda () (->truename filename)))))

(define (%insert-file mark truename)
  (if (ref-variable read-file-message)
      (let ((msg
	     (string-append "Reading file \""
			    (->namestring truename)
			    "\"...")))
	(temporary-message msg)
	(group-insert-file! (mark-group mark) (mark-index mark) truename)
	(temporary-message msg "done"))
      (group-insert-file! (mark-group mark) (mark-index mark) truename)))

(define (group-insert-file! group index truename)
  (let ((channel (file-open-input-channel (->namestring truename))))
    (let ((length (file-length channel)))
      (without-interrupts
       (lambda ()
	 (move-gap-to! group index)
	 (guarantee-gap-length! group length)))
      (let ((n
	     (channel-read channel (group-text group) index (+ index length))))
	(without-interrupts
	 (lambda ()
	   (let ((gap-start* (fix:+ index n)))
	     (undo-record-insertion! group index gap-start*)
	     (vector-set! group
			  group-index:gap-length
			  (fix:- (group-gap-length group) n))
	     (vector-set! group group-index:gap-start gap-start*)
	     (for-each-mark group
	       (lambda (mark)
		 (let ((index* (mark-index mark)))
		   (if (or (fix:> index* index)
			   (and (fix:= index* index)
				(mark-left-inserting? mark)))
		       (set-mark-index! mark (fix:+ index* n))))))
	     (record-insertion! group index gap-start*))))
	(channel-close channel)
	n))))

;;;; Buffer Mode Initialization

(define (normal-mode buffer find-file?)
  (initialize-buffer-modes! buffer)
  (initialize-buffer-local-variables! buffer find-file?))

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
			    (or mode (ref-variable editor-default-mode))))))

(define (filename-default-mode buffer)
  (let ((entry
	 (let ((pathname (buffer-pathname buffer)))
	   (and pathname
		(let ((type (pathname-type pathname)))
		  (and (string? type)
		       (assoc-string-ci
			type
			(ref-variable file-type-to-major-mode))))))))
    (and entry (name->mode (cdr entry)))))

(define assoc-string-ci
  (association-procedure string-ci=? car))

(define (parse-buffer-mode-header buffer)
  (let ((start (buffer-start buffer)))
    (let ((end (line-end start 0)))
      (let ((start (re-search-forward "-\\*-[ \t]*" start end false)))
	(and start
	     (re-search-forward "[ \t]*-\\*-" start end false)
	     (let ((end (re-match-start 0)))
	       (if (not (char-search-forward #\: start end false))
		   (extract-string start end)
		   (let ((m (re-search-forward "mode:[ \t]*" start end true)))
		     (and m
			  (extract-string
			   m
			   (if (re-search-forward "[ \t]*;" m end false)
			       (re-match-start 0)
			       end)))))))))))

)

;;;; Local Variable Initialization

(define-variable local-variable-search-limit
  "The maximum number of characters searched when looking for local variables
at the end of a file."
  3000
  exact-nonnegative-integer?)

(define-variable inhibit-local-variables
  "True means query before obeying a file's local-variables list.
This applies when the local-variables list is scanned automatically
after you find a file.  If you explicitly request such a scan with
\\[normal-mode], there is no query, regardless of this variable."
  false
  boolean?)

(define initialize-buffer-local-variables!)
(let ()

(set! initialize-buffer-local-variables!
(named-lambda (initialize-buffer-local-variables! buffer find-file?)
  (let ((end (buffer-end buffer)))
    (let ((start
	   (with-text-clipped
	    (mark- end (ref-variable local-variable-search-limit) 'LIMIT)
	    end
	    (lambda () (backward-one-page end)))))
      (if start
	  (if (re-search-forward "Edwin Variables:[ \t]*" start end true)
	      (let ((start (re-match-start 0))
		    (end (re-match-end 0)))
		(if (or (not find-file?)
			(not (ref-variable inhibit-local-variables buffer))
			(prompt-for-confirmation?
			 (string-append
			  "Set local variables as specified at end of "
			  (file-namestring (buffer-pathname buffer)))))
		    (parse-local-variables buffer start end)))))))))

(define edwin-environment (->environment '(edwin)))

(define (evaluate sexp)
  (scode-eval (syntax sexp edwin-syntax-table)
	      edwin-environment))

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
	      (editor-error "Local variables entry missing suffix")))
	(let ((m1
	       (horizontal-space-end
		(if prefix?
		    (or (match-forward prefix start end false)
			(editor-error "Local variables entry missing prefix"))
		    start))))
	  (let ((m2
		 (let ((m2 (char-search-forward #\: m1 end)))
		   (if (not m2)
		       (editor-error "Missing colon in local variables entry"))
		   (mark-1+ m2))))
	    (let ((var (extract-string m1 (horizontal-space-start m2)))
		  (m3 (horizontal-space-end (mark1+ m2))))
	      (if (not (string-ci=? var "End"))
		  (with-input-from-mark m3 read
		    (lambda (val m4)
		      (check-suffix (horizontal-space-end m4))
		      (if (string-ci=? var "Mode")
			  (let ((mode
				 (string-table-get editor-modes
						   (extract-string m3 m4))))
			    (if mode
				((if (mode-major? mode)
				     set-buffer-major-mode!
				     enable-buffer-minor-mode!)
				 buffer mode)))
			  (call-with-current-continuation
			   (lambda (continuation)
			     (bind-condition-handler
				 (list condition-type:error)
				 (lambda (condition)
				   condition
				   (editor-beep)
				   (message
				    "Error while processing local variable: "
				    var)
				   (continuation false))
			       (lambda ()
				 (if (string-ci=? var "Eval")
				     (evaluate val)
				     (add-buffer-initialization!
				      buffer
				      (let ((variable (name->variable var))
					    (value (evaluate val)))
					(lambda ()
					  (define-variable-local-value!
					   (current-buffer)
					   variable
					   value))))))))))
		      (loop m4))))))))

      (loop start))))

)

;;;; Output

(define-variable require-final-newline
  "True says silently put a newline at the end whenever a file is saved.
Neither false nor true says ask user whether to add a newline in each
such case.  False means don't add newlines."
  false
  boolean?)

(define-variable make-backup-files
  "Create a backup of each file when it is saved for the first time.
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
backup-by-copying ,  backup-by-copying-when-linked  and
backup-by-copying-when-mismatch ."
  true
  boolean?)

(define-variable backup-by-copying
  "True means always use copying to create backup files.
See documentation of variable  make-backup-files."
 false
  boolean?)

(define-variable file-precious-flag
  "True means protect against I/O errors while saving files.
Some modes set this true in particular buffers."
  false
  boolean?)

(define-variable trim-versions-without-asking
  "True means delete excess backup versions silently.
Otherwise asks confirmation."
  false
  boolean?)

(define-variable write-file-hooks
  "List of procedures to be called before writing out a buffer to a file.
If one of them returns non-false, the file is considered already written
and the rest are not called."
  '()
  list?)

(define (write-buffer-interactive buffer backup-mode)
  (let ((pathname (buffer-pathname buffer)))
    (let ((writable? (file-writable? pathname)))
      (if (or writable?
	      (prompt-for-yes-or-no?
	       (string-append "File "
			      (file-namestring pathname)
			      " is write-protected; try to save anyway"))
	      (editor-error
	       "Attempt to save to a file which you aren't allowed to write"))
	  (begin
	    (if (not (or (verify-visited-file-modification-time? buffer)
			 (not (file-exists? pathname))
			 (prompt-for-yes-or-no?
			  "Disk file has changed since visited or saved.  Save anyway")))
		(editor-error "Save not confirmed"))
	    (let ((modes (backup-buffer! buffer pathname backup-mode)))
	      (require-newline buffer)
	      (cond ((let loop ((hooks (ref-variable write-file-hooks buffer)))
		       (and (not (null? hooks))
			    (or ((car hooks) buffer)
				(loop (cdr hooks)))))
		     unspecific)
		    ((ref-variable file-precious-flag buffer)
		     (let ((old (os/precious-backup-pathname pathname)))
		       (let ((rename-back?
			      (catch-file-errors (lambda () false)
				(lambda ()
				  (rename-file pathname old)
				  (set! modes (file-modes old))
				  true))))
			 (dynamic-wind
			  (lambda () unspecific)
			  (lambda ()
			    (clear-visited-file-modification-time! buffer)
			    (write-buffer buffer)
			    (if rename-back?
				(begin
				  (set! rename-back? false)
				  (catch-file-errors
				   (lambda () unspecific)
				   (lambda () (delete-file old))))))
			  (lambda ()
			    (if rename-back?
				(begin
				  (rename-file old pathname)
				  (clear-visited-file-modification-time!
				   buffer))))))))
		    (else
		     (if (and (not writable?)
			      (not modes)
			      (file-exists? pathname))
			 (bind-condition-handler
			     (list condition-type:file-error)
			     (lambda (condition)
			       condition
			       (editor-error
				"Can't get write permission for file: "
				(->namestring pathname)))
			   (lambda ()
			     (let ((m (file-modes pathname)))
			       (set-file-modes! pathname #o777)
			       (set! modes m)))))
		     (write-buffer buffer)))
	      (if modes
		  (catch-file-errors
		   (lambda () unspecific)
		   (lambda () (set-file-modes! pathname modes))))))))))

(define (verify-visited-file-modification-time? buffer)
  (let ((truename (buffer-truename buffer))
	(buffer-time (buffer-modification-time buffer)))
    (or (not truename)
	(not buffer-time)
	(let ((file-time (file-modification-time truename)))
	  (and file-time
	       (< (abs (- buffer-time file-time)) 2))))))

(define-integrable (clear-visited-file-modification-time! buffer)
  (set-buffer-modification-time! buffer false))

(define (write-buffer buffer)
  (let ((truename
	 (->pathname
	  (write-region (buffer-unclipped-region buffer)
			(buffer-pathname buffer)
			true))))
    (set-buffer-truename! buffer truename)
    (delete-auto-save-file! buffer)
    (set-buffer-save-length! buffer)
    (buffer-not-modified! buffer)
    (set-buffer-modification-time! buffer (file-modification-time truename))))

(define-variable enable-emacs-write-file-message
  "If true, generate Emacs-style message when writing files.
Otherwise, a message is written both before and after long file writes."
  false
  boolean?)

(define (write-region region filename message?)
  (write-region* region filename message? group-write-to-file))

(define (append-to-file region filename message?)
  (write-region* region filename message? group-append-to-file))

(define (write-region* region filename message? group-write-to-file)
  (let ((filename (->namestring filename))
	(start (region-start-index region))
	(end (region-end-index region)))
    (let ((do-it
	   (lambda ()
	     (group-write-to-file (region-group region) start end filename))))
      (cond ((not message?)
	     (do-it))
	    ((or (ref-variable enable-emacs-write-file-message)
		 (<= (- end start) 50000))
	     (do-it)
	     (message "Wrote " filename))
	    (else
	     (let ((msg (string-append "Writing file " filename "...")))
	       (message msg)
	       (do-it)
	       (message msg "done")))))
    ;; This isn't the correct truename on systems that support version
    ;; numbers.  For those systems, the truename must be supplied by
    ;; the operating system after the channel is closed.
    filename))

(define (group-write-to-file group start end filename)
  (let ((channel (file-open-output-channel filename)))
    (group-write-to-channel group start end channel)
    (channel-close channel)))

(define (group-append-to-file group start end filename)
  (let ((channel (file-open-append-channel filename)))
    (group-write-to-channel group start end channel)
    (channel-close channel)))

(define (group-write-to-channel group start end channel)
  (let ((text (group-text group))
	(gap-start (group-gap-start group))
	(gap-end (group-gap-end group))
	(gap-length (group-gap-length group)))
    (cond ((fix:<= end gap-start)
	   (channel-write-block channel text start end))
	  ((fix:<= gap-start start)
	   (channel-write-block channel
				text
				(fix:+ start gap-length)
				(fix:+ end gap-length)))
	  (else
	   (channel-write-block channel text start gap-start)
	   (channel-write-block channel
				text
				gap-end
				(fix:+ end gap-length))))))

(define (require-newline buffer)
  (let ((require-final-newline? (ref-variable require-final-newline buffer)))
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

(define (backup-buffer! buffer truename backup-mode)
  (and (ref-variable make-backup-files buffer)
       (or (memq backup-mode '(BACKUP-PREVIOUS BACKUP-BOTH))
	   (and (not (eq? backup-mode 'NO-BACKUP))
		(not (buffer-backed-up? buffer))))
       truename
       (file-exists? truename)
       (os/backup-buffer? truename)
       (catch-file-errors
	(lambda () false)
	(lambda ()
	  (with-values (lambda () (os/buffer-backup-pathname truename))
	    (lambda (backup-pathname targets)
	      (let ((modes
		     (catch-file-errors
		      (lambda ()
			(let ((filename (os/default-backup-filename)))
			  (temporary-message
			   "Cannot write backup file; backing up in "
			   filename)
			  (copy-file truename filename)
			  false))
		      (lambda ()
			(if (or (ref-variable file-precious-flag buffer)
				(file-symbolic-link? truename)
				(ref-variable backup-by-copying buffer)
				(os/backup-by-copying? truename))
			    (begin
			      (copy-file truename backup-pathname)
			      false)
			    (begin
			      (catch-file-errors
			       (lambda () unspecific)
			       (lambda () (delete-file backup-pathname)))
			      (rename-file truename backup-pathname)
			      (file-modes backup-pathname)))))))
		(set-buffer-backed-up?!
		 buffer
		 (not (memv backup-mode '(BACKUP-NEXT BACKUP-BOTH))))
		(if (and (not (null? targets))
			 (or (ref-variable trim-versions-without-asking buffer)
			     (prompt-for-confirmation?
			      (string-append
			       "Delete excess backup versions of "
			       (->namestring (buffer-pathname buffer))))))
		    (for-each (lambda (target)
				(catch-file-errors
				 (lambda () unspecific)
				 (lambda () (delete-file target))))
			      targets))
		modes)))))))