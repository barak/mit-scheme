;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/dired.scm,v 1.117 1991/09/20 13:35:25 arthur Exp $
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

;;;; Directory Editor

(declare (usual-integrations))

(define-major-mode dired read-only "Dired"
  "Mode for \"editing\" directory listings.
In dired, you are \"editing\" a list of the files in a directory.
You can move using the usual cursor motion commands.
Letters no longer insert themselves.
Instead, type d to flag a file for Deletion.
Type u to Unflag a file (remove its D or C flag).
  Type Rubout to back up one line and unflag.
Type x to eXecute the deletions requested.
Type f to Find the current line's file
  (or Dired it, if it is a directory).
Type o to find file or dired directory in Other window.
Type # to flag temporary files (names beginning with #) for Deletion.
Type ~ to flag backup files (names ending with ~) for Deletion.
Type . to flag numerical backups for Deletion.
  (Spares dired-kept-versions or its numeric argument.)
Type r to rename a file.
Type c to copy a file.
Type k to mark a file for Copying.
Type y to copy files marked for Copying.
Type g to read the directory again.  This discards all deletion-flags.
Space and Rubout can be used to move down and up by lines.
Also:
 M, G, O -- change file's mode, group or owner.
 C -- compress this file.  U -- uncompress this file."
;;Type v to view a file in View mode, returning to Dired when done.
  (local-set-variable! case-fold-search false)
  (event-distributor/invoke! (ref-variable dired-mode-hook)))

(define-variable dired-mode-hook
  "An event distributor that is invoked when entering Dired mode."
  (make-event-distributor))

(define-key 'dired #\r 'dired-rename-file)
(define-key 'dired #\c-d 'dired-flag-file-deleted)
(define-key 'dired #\d 'dired-flag-file-deleted)
(define-key 'dired #\v 'dired-view-file)
(define-key 'dired #\e 'dired-find-file)
(define-key 'dired #\f 'dired-find-file)
(define-key 'dired #\o 'dired-find-file-other-window)
(define-key 'dired #\k 'dired-flag-file-for-copy)
(define-key 'dired #\u 'dired-unflag)
(define-key 'dired #\x 'dired-do-deletions)
(define-key 'dired #\y 'dired-do-copies)
(define-key 'dired #\rubout 'dired-backup-unflag)
(define-key 'dired #\? 'dired-summary)
(define-key 'dired #\c 'dired-copy-file)
(define-key 'dired #\# 'dired-flag-auto-save-files)
(define-key 'dired #\~ 'dired-flag-backup-files)
(define-key 'dired #\. 'dired-clean-directory)
(define-key 'dired #\h 'describe-mode)
(define-key 'dired #\space 'dired-next-line)
(define-key 'dired #\c-n 'dired-next-line)
(define-key 'dired down 'dired-next-line)
(define-key 'dired #\c-p 'dired-previous-line)
(define-key 'dired up 'dired-previous-line)
(define-key 'dired #\n 'dired-next-line)
(define-key 'dired #\p 'dired-previous-line)
(define-key 'dired #\g 'dired-revert)
(define-key 'dired #\C 'dired-compress)
(define-key 'dired #\U 'dired-uncompress)
(define-key 'dired #\M 'dired-chmod)
(define-key 'dired #\G 'dired-chgrp)
(define-key 'dired #\O 'dired-chown)
(define-key 'dired #\q 'dired-quit)
(define-key 'dired #\c-\] 'dired-abort)

(define-command dired
  "\"Edit\" directory DIRNAME--delete, rename, print, etc. some files in it.
Dired displays a list of files in DIRNAME.
You can move around in it with the usual commands.
You can flag files for deletion with C-d
and then delete them by typing `x'.
Type `h' after entering dired for more info."
  "DDired (directory)"
  (lambda (directory)
    (select-buffer (make-dired-buffer directory))))

(define-command dired-other-window
  "\"Edit\" directory DIRNAME.  Like \\[dired] but selects in another window."
  "DDired in other window (directory)"
  (lambda (directory)
    (select-buffer-other-window (make-dired-buffer directory))))

(define (make-dired-buffer directory)
  (let ((directory (->pathname directory)))
    (let ((buffer (get-dired-buffer directory)))
      (set-buffer-major-mode! buffer (ref-mode-object dired))
      (set-buffer-default-directory! buffer
				     (pathname-directory-path directory))
      (buffer-put! buffer 'REVERT-BUFFER-METHOD revert-dired-buffer)
      (buffer-put! buffer 'DIRED-DIRECTORY directory)
      (fill-dired-buffer! buffer directory)
      buffer)))

(define (get-dired-buffer directory)
  (or (list-search-positive (buffer-list)
	(lambda (buffer)
	  (let ((directory* (buffer-get buffer 'DIRED-DIRECTORY)))
	    (and directory*
		 (pathname=? directory* directory)))))
      (new-buffer (pathname->buffer-name directory))))

(define (dired-buffer-directory buffer)
  (or (buffer-get buffer 'DIRED-DIRECTORY)
      (let ((directory (buffer-default-directory buffer)))
	(buffer-put! buffer 'DIRED-DIRECTORY directory)
	directory)))

(define (revert-dired-buffer buffer dont-use-auto-save? dont-confirm?)
  dont-use-auto-save? dont-confirm?	;ignore
  (fill-dired-buffer! buffer (dired-buffer-directory buffer)))

(define-variable dired-listing-switches
  "Switches passed to ls for dired.  MUST contain the 'l' option.
CANNOT contain the 'F' option."
  "-al"
  string?)

(define-variable dired-kept-versions
  "When cleaning directory, number of versions to keep."
  2
  exact-nonnegative-integer?)

(define (fill-dired-buffer! buffer pathname)
  (set-buffer-writeable! buffer)
  (region-delete! (buffer-region buffer))
  (temporary-message
   (string-append "Reading directory "
		  (pathname->string pathname)
		  "..."))
  (read-directory pathname
		  (ref-variable dired-listing-switches)
		  (buffer-point buffer))
  (append-message "done")
  (let ((point (mark-left-inserting-copy (buffer-point buffer)))
	(group (buffer-group buffer)))
    (let ((index (mark-index (buffer-start buffer))))
      (if (not (group-end-index? group index))
	  (let loop ((index index))
	    (set-mark-index! point index)
	    (group-insert-string! group index "  ")
	    (let ((index (1+ (line-end-index group (mark-index point)))))
	      (if (not (group-end-index? group index))
		  (loop index)))))))
  (set-buffer-point! buffer (buffer-start buffer))
  (buffer-not-modified! buffer)
  (set-buffer-read-only! buffer))

(define (read-directory pathname switches mark)
  (with-working-directory-pathname (pathname-directory-path pathname)
    (lambda ()
      (if (file-directory? pathname)
	  (run-synchronous-process false
				   mark
				   (find-program "ls" false)
				   switches
				   (pathname->string pathname))
	  (shell-command (string-append "ls "
					switches
					" "
					(pathname-name-string pathname))
			 mark)))))

(define (add-dired-entry pathname)
  (let ((lstart (line-start (current-point) 0))
	(directory (pathname-directory-path pathname)))
    (if (pathname=? (buffer-default-directory (mark-buffer lstart)) directory)
	(let ((start (mark-right-inserting lstart)))
	  (run-synchronous-process false
				   lstart
				   (find-program "ls" directory)
				   "-d"
				   (ref-variable dired-listing-switches)
				   (pathname->string pathname))
	  (insert-string "  " start)
	  (let ((start (mark-right-inserting (dired-filename-start start))))
	    (insert-string
	     (pathname-name-string
	      (string->pathname
	       (extract-and-delete-string start (line-end start 0))))
	     start))))))

(define-command dired-find-file
  "Read the current file into a buffer."
  ()
  (lambda ()
    (find-file (dired-current-pathname))))

(define-command dired-find-file-other-window
  "Read the current file into a buffer in another window."
  ()
  (lambda ()
    (find-file-other-window (dired-current-pathname))))

(define-command dired-revert
  "Read the current buffer."
  ()
  (lambda ()
    (revert-buffer (current-buffer) true true)))

(define-command dired-flag-file-deleted
  "Mark the current file to be killed."
  "p"
  (lambda (argument)
    (dired-mark #\D argument)))

(define-command dired-flag-file-for-copy
  "Mark the current file to be copied."
  "p"
  (lambda (argument)
    (dired-mark #\C argument)))

(define-command dired-unflag
  "Cancel the kill or copy requested for the current file."
  "p"
  (lambda (argument)
    (dired-mark #\Space argument)))

(define-command dired-backup-unflag
  "Cancel the kill requested for the file on the previous line."
  "p"
  (lambda (argument)
    (set-dired-point! (line-start (current-point) -1 'ERROR))
    (dired-mark #\Space argument)
    (set-dired-point! (line-start (current-point) -1 'ERROR))))

(define-command dired-next-line
  "Move down to the next line."
  "p"
  (lambda (argument)
    (set-dired-point! (line-start (current-point) argument 'BEEP))))

(define-command dired-previous-line
  "Move up to the previous line."
  "p"
  (lambda (argument)
    (set-dired-point! (line-start (current-point) (- argument) 'BEEP))))

(define-command dired-do-deletions
  "Kill all marked files."
  ()
  (lambda ()
    (dired-kill-files)))

(define-command dired-do-copies
  "Copy marked files."
  ()
  (lambda ()
    (dired-copy-files)))

(define-command dired-quit
  "Exit Dired, offering to kill any files first."
  ()
  (lambda ()
    (dired-kill-files)
    (kill-buffer-interactive (current-buffer))))

(define-command dired-abort
  "Exit Dired."
  ()
  (lambda ()
    (kill-buffer-interactive (current-buffer))))

(define-command dired-summary
  "Summarize the Dired commands in the typein window."
  ()
  (lambda ()
    (message "d-elete, u-ndelete, x-ecute, q-uit, f-ind, o-ther window")))

(define-command dired-rename-file
  "Rename this file to TO-FILE."
  (lambda ()
    (list
     (pathname->string
      (let ((pathname (dired-current-pathname)))
	(prompt-for-pathname (string-append "Rename "
					    (pathname-name-string pathname)
					    " to")
			     pathname
			     false)))))
  (lambda (to-file)
    (let ((from (dired-current-pathname))
	  (to (->pathname to-file)))
      (bind-condition-handler (list condition-type:file-error
				    condition-type:port-error)
	  (lambda (condition)
	    (editor-error "Rename failed: "
			  (condition/report-string condition)))
	(lambda () (rename-file from to)))
      (dired-redisplay to))))

(define-command dired-copy-file
  "Copy this file to TO-FILE."
  (lambda ()
    (list
     (pathname->string
      (let ((pathname (dired-current-pathname)))
	(prompt-for-pathname (string-append "Copy "
					    (pathname-name-string pathname)
					    " to")
			     pathname
			     false)))))
  (lambda (to-file)
    (let ((from (dired-current-pathname))
	  (to (->pathname to-file)))
      (bind-condition-handler (list condition-type:file-error
				    condition-type:port-error)
	  (lambda (condition)
	    (editor-error "Copy failed: " (condition/report-string condition)))
	(lambda () (copy-file from to)))
      (let ((lstart (mark-right-inserting (line-start (current-point) 0))))
	(with-read-only-defeated lstart
	  (lambda ()
	    (add-dired-entry to)))
	(set-current-point! (dired-filename-start lstart))))))

(define-command dired-chmod
  "Change mode of this file."
  "sChange to Mode"
  (lambda (mode) (dired-change-line "chmod" mode)))

(define-command dired-chgrp
  "Change group of this file."
  "sChange to Group"
  (lambda (group) (dired-change-line "chgrp" group)))

(define-command dired-chown
  "Change owner of this file."
  "sChange to Owner"
  (lambda (owner) (dired-change-line "chown" owner)))

(define (dired-change-line program argument)
  (let ((pathname (dired-current-pathname)))
    (run-synchronous-process false
			     false
			     (find-program program
					   (pathname-directory-path pathname))
			     argument
			     (pathname->string pathname))
    (dired-redisplay pathname)))

(define (dired-redisplay pathname)
  (let ((lstart (mark-right-inserting (line-start (current-point) 0))))
    (with-read-only-defeated lstart
      (lambda ()
	(delete-string lstart (line-start lstart 1))
	(add-dired-entry pathname)))
    (set-current-point! (dired-filename-start lstart))))

(define (dired-filename-start lstart)
  (let ((eol (line-end lstart 0)))
    (let ((m
	   (re-search-forward
	    "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\)[ ]+[0-9]+"
	    lstart
	    eol
	    false)))
      (and m
	   (re-match-forward " *[^ ]* *" m eol)))))

(define (dired-filename-region lstart)
  (let ((start (dired-filename-start lstart)))
    (and start
	 (make-region start (line-end start 0)))))

(define (set-dired-point! mark)
  (set-current-point!
   (let ((lstart (line-start mark 0)))
     (or (dired-filename-start lstart)
	 lstart))))

(define (dired-current-pathname)
  (let ((lstart (line-start (current-point) 0)))
    (guarantee-dired-filename-line lstart)
    (dired-pathname lstart)))

(define (guarantee-dired-filename-line lstart)
  (if (not (dired-filename-start lstart))
      (editor-error "No file on this line")))

(define (dired-pathname lstart)
  (merge-pathnames
   (pathname-directory-path (dired-buffer-directory (current-buffer)))
   (string->pathname (region->string (dired-filename-region lstart)))))

(define (dired-mark char n)
  (with-read-only-defeated (current-point)
    (lambda ()
      (dotimes n
	(lambda (i)
	  i				;ignore
	  (let ((lstart (line-start (current-point) 0)))
	    (guarantee-dired-filename-line lstart)
	    (dired-mark-1 lstart char)
	    (set-dired-point! (line-start lstart 1))))))))

(define (dired-mark-1 lstart char)
  (delete-right-char lstart)
  (insert-chars char 1 lstart))

(define (dired-file-line? lstart)
  (and (dired-filename-start lstart)
       (not (re-match-forward ". d" lstart (mark+ lstart 3)))))

(define (for-each-file-line buffer procedure)
  (let ((point (mark-right-inserting-copy (buffer-start buffer))))
    (do () ((group-end? point))
      (if (dired-file-line? point)
	  (procedure point))
      (move-mark-to! point (line-start point 1)))))

(define-command dired-flag-auto-save-files
  "Flag for deletion files whose names suggest they are auto save files."
  ()
  (lambda ()
    (with-read-only-defeated (current-point)
      (lambda ()
	(for-each-file-line (current-buffer)
	  (lambda (lstart)
	    (if (match-forward "#"
			       (dired-filename-start lstart)
			       (line-end lstart 0))
		(dired-mark-1 lstart #\D))))))))

(define-command dired-flag-backup-files
  "Flag all backup files (names ending with ~) for deletion."
  ()
  (lambda ()
    (with-read-only-defeated (current-point)
      (lambda ()
	(for-each-file-line (current-buffer)
	  (lambda (lstart)
	    (if (let ((lend (line-end lstart 0)))
		  (match-forward "~" (mark- lend 1) lend))
		(dired-mark-1 lstart #\D))))))))

(define (dired-kill-files)
  (let ((filenames (dired-killable-filenames)))
    (if (not (null? filenames))
	(let ((buffer (temporary-buffer " *Deletions*")))
	  (write-strings-densely
	   (map (lambda (filename)
		  (pathname-name-string (car filename)))
		filenames)
	   (mark->output-port (buffer-point buffer))
	   (window-x-size (current-window)))
	  (set-buffer-point! buffer (buffer-start buffer))
	  (buffer-not-modified! buffer)
	  (set-buffer-read-only! buffer)
	  (if (with-selected-buffer buffer
		(lambda ()
		  (local-set-variable! truncate-partial-width-windows false)
		  (prompt-for-yes-or-no? "Delete these files")))
	      (let loop ((filenames filenames) (failures '()))
		(cond ((not (null? filenames))
		       (loop (cdr filenames)
			     (if (dired-kill-file! (car filenames))
				 failures
				 (cons (pathname-name-string (caar filenames))
				       failures))))
		      ((not (null? failures))
		       (message "Deletions failed: " (reverse! failures))))))
	  (kill-buffer buffer)))))

(define (dired-copy-files)
  (let ((filenames (dired-filenames-to-copy)))
    (if (not (null? filenames))
	(let ((buffer (temporary-buffer " *Copies*")))
	  (write-strings-densely
	   (map (lambda (filename)
		  (pathname-name-string (car filename)))
		filenames)
	   (mark->output-port (buffer-point buffer))
	   (window-x-size (current-window)))
	  (set-buffer-point! buffer (buffer-start buffer))
	  (buffer-not-modified! buffer)
	  (set-buffer-read-only! buffer)
	  (let ((destination
		 (pathname-directory
		  (->pathname
		   (with-selected-buffer
		    buffer
		    (lambda ()
		      (local-set-variable! truncate-partial-width-windows false)
		      (prompt-for-directory
		       "Directory to which to copy these files"
		       false true)))))))
	    (let loop ((filenames filenames) (failures '()))
	      (cond ((not (null? filenames))
		     (loop (cdr filenames)
			   (if (dired-copy-file! (caar filenames) destination)
			       (let ((where (cdar filenames)))
				 (with-read-only-defeated where
				   (lambda ()
				     (dired-mark-1 where #\Space)))
				 failures)
			       (cons (pathname-name-string (caar filenames))
				     failures))))
		    ((not (null? failures))
		     (message "Copies failed: " (reverse! failures))))))
	  (kill-buffer buffer)))))

(define (dired-filenames-to-copy)
  (define (loop start)
    (let ((next (line-start start 1)))
      (if next
	  (let ((rest (loop next)))
	    (if (char=? #\C (mark-right-char start))
		(cons (cons (dired-pathname start) (mark-permanent! start))
		      rest)
		rest))
	  '())))
  (loop (line-start (buffer-start (current-buffer)) 0)))

(define (dired-killable-filenames)
  (define (loop start)
    (let ((next (line-start start 1)))
      (if next
	  (let ((rest (loop next)))
	    (if (char=? #\D (mark-right-char start))
		(cons (cons (dired-pathname start) (mark-permanent! start))
		      rest)
		rest))
	  '())))
  (loop (line-start (buffer-start (current-buffer)) 0)))

(define (dired-kill-file! filename)
  (let ((deleted?
	 (catch-file-errors (lambda () false)
			    (lambda () (delete-file (car filename)) true))))
    (if deleted?
	(with-read-only-defeated (cdr filename)
	  (lambda ()
	    (delete-string (cdr filename)
			   (line-start (cdr filename) 1)))))
    deleted?))

(define (dired-copy-file! from to-directory)
  (let ((to (pathname-new-directory from to-directory)))
    (bind-condition-handler (list condition-type:file-error
				  condition-type:port-error)
	(lambda (condition)
	  condition			;ignored
	  false)
      (lambda ()
	(copy-file from to)
	true))))

;;;; List Directory

(define-variable list-directory-brief-switches
  "Switches for list-directory to pass to `ls' for brief listing,"
  "-CF"
  string?)

(define-variable list-directory-verbose-switches
  "Switches for list-directory to pass to `ls' for verbose listing,"
  "-l"
  string?)

(define-command list-directory
  "Display a list of files in or matching DIRNAME, a la `ls'.
DIRNAME is globbed by the shell if necessary.
Prefix arg (second arg if noninteractive) means supply -l switch to `ls'.
Actions controlled by variables list-directory-brief-switches
 and list-directory-verbose-switches."
  (lambda ()
    (let ((argument (command-argument)))
      (list (pathname->string
	     (prompt-for-directory (if argument
				       "List directory (verbose)"
				       "List directory (brief)")
				   false false))
	    argument)))
  (lambda (directory argument)
    (let ((directory (->pathname directory))
	  (buffer (temporary-buffer "*Directory*")))
      (disable-group-undo! (buffer-group buffer))
      (let ((point (buffer-end buffer)))
	(insert-string "Directory " point)
	(insert-string (pathname->string directory) point)
	(insert-newline point)
	(read-directory directory
			(if argument
			    (ref-variable list-directory-verbose-switches)
			    (ref-variable list-directory-brief-switches))
			point))
      (set-buffer-point! buffer (buffer-start buffer))
      (buffer-not-modified! buffer)
      (pop-up-buffer buffer false))))