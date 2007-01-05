#| -*-Scheme-*-

$Id: floppy.scm,v 1.29 2007/01/05 15:33:03 cph Exp $

Copyright (c) 1992-1999 Massachusetts Institute of Technology

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

;;;; 6.001: HP-UX Floppy Commands

(declare (usual-integrations))

(define (run-floppy-login-loop)
  (set! floppy-contents-loaded? false)
  (standard-configuration 'login login-loop))

(define (login-loop)
  (buffer-reset! (current-buffer))
  (append-string
   "Welcome to the 6.001 computer system.

Please choose one of the following login options.

To choose an option, just type the letter for that option.
Case is not significant; you can use upper or lower case.
Don't type RET after the letter.

Here are the login options:

L	Normal login (with previously initialized floppy).  Make sure
	that an initialized floppy is in the drive before selecting
	this option.  The files on your floppy will be copied into
	the computer so you can work on them. 

F	Login with new floppy disk(s).  Select this option if this is
	the first time that you are logging in with these floppy
	disks.  You will be guided through the process of
	initializing your floppies.   

N	Practice login (without floppy).   Select this option if you
	do not have any floppies or if you do not wish to use a
	floppy.  WARNING:  IF YOU SELECT THIS OPTION YOU WILL NOT BE
	ABLE TO SAVE YOUR WORK!

Q	Quit.  Select this option if you do not want to log in.")
  (show-dialog)
  (let loop ()
    (let ((char
	   (prompt-for-char
	    "Please choose a login option (default: L)")))
      (case char
	((#\f #\F) (first-login))
	((#\l #\L #\space #\return) (normal-login))
	((#\n #\N) (no-floppy-login))
	((#\q #\Q) (exit-scheme))
	(else (editor-beep) (loop))))))

(define (first-login)
  (append-string
   "
----------------------------------------------------------------------
You have chosen the \"First login\" option.

This login option will guide you through the steps of initializing
two floppy disks.  You should have obtained these floppy disks from
the instrument room.")
  (login-common
   (lambda ()
     (append-string
      "
----------------------------------------------------------------------
Please select one of your floppy disks, label it as your \"backup\"
disk, and insert it into the drive. When you have done this,
type any character to continue.")
     (show-dialog)
     (wait-for-user)
     (if (initialize-floppy)
	 (begin
	   (append-string
	    "
----------------------------------------------------------------------
Please eject your backup disk from the floppy drive.

Now select your other disk, label it as your \"primary\" disk, and
insert it into the floppy drive.")
	   (append-string
	    (case microcode-id/operating-system
	      ((DOS NT)
	       "
Again, use the File Manager to format the floppy.")
	      ((OS/2)
	       "
Again, use the Drive object to format the floppy.")
	      (else "")))
	   (append-string
	    "
When you have done this, type any character to continue.")
	   (show-dialog)
	   (wait-for-user)
	   (if (initialize-floppy)
	       (begin
		 (set! floppy-contents-loaded? true)
		 (append-string "
----------------------------------------------------------------------
Your disks are now initialized.
Type any character to finish logging in.")
		 (show-dialog)
		 (wait-for-user))
	       (login-loop)))
	 (login-loop)))))

(define (normal-login)
  (append-string
   "
----------------------------------------------------------------------
You have chosen the \"Normal login\" option.

This login option will read the contents of your floppy disk into the
computer.  You should make sure that your disk is in the floppy drive.")
  (login-common
   (lambda ()
     (let loop ()
       (append-string
	"
----------------------------------------------------------------------
")
       (show-dialog)
       (call-with-current-continuation
	(lambda (k)
	  (handle-floppy-errors
	   (lambda () (within-continuation k loop))
	   (lambda () (within-continuation k login-loop))
	   (lambda ()
	     (load-from-floppy)
	     (set! floppy-contents-loaded? true)
	     unspecific))))))))

(define (no-floppy-login)
  (append-string
   "
----------------------------------------------------------------------
You have chosen the \"Login without floppy\" option.

This login option assumes that you have no floppy disk.  You should
not use this login option if you have a floppy disk, because you
cannot save your work unless your floppy is loaded when you login.
IF YOU TRY TO SAVE YOUR WORK AFTER LOGGING IN WITH THIS OPTION, THE
FILES ON YOUR FLOPPY WILL BE DELETED.")
  (login-common
   (lambda ()
     (set! floppy-contents-loaded? false)
     unspecific)))

(define (login-common thunk)
  (append-string
   "

If you have chosen this login option by mistake, please type the
letter N, which will return you to the login loop.

Otherwise, type Y to continue with the initialization process.")
  (show-dialog)
  (if (prompt-for-confirmation? "Continue with login")
      (thunk)
      (login-loop)))

(define (run-floppy-logout)
  (standard-configuration 'logout
    (lambda ()
      (fluid-let ((paranoid-exit? false))
	(save-buffers-and-exit false "Scheme"
	  (lambda ()
	    (let ((abort
		   (lambda ()
		     (append-string
		      "
If you want to log out without saving your files, answer \"yes\" to
the question below.

Answer \"no\" if you want to return to the editor without logging out.")
		     (show-dialog)
		     (if (prompt-for-yes-or-no?
			  "Log out without saving files")
			 (exit-scheme)
			 (abort-current-command)))))
	      (if (not floppy-contents-loaded?)
		  (begin
		    (append-string
		     "You logged in without a floppy disk.\n")
		    (abort)))
	      (let loop ()
		(call-with-current-continuation
		 (lambda (k)
		   (handle-floppy-errors
		    (lambda ()
		      (append-string
		       "
----------------------------------------------------------------------
")
		      (show-dialog)
		      (within-continuation k loop))
		    (lambda ()
		      (append-string
		       "
----------------------------------------------------------------------")
		      (abort))
		    checkpoint-floppy)))))
	    (exit-scheme)))))))

(set-command-procedure! (ref-command-object logout) run-floppy-logout)

;; Disable key bindings that exit the editor.
;; M-x logout is all the students should need.
(define-key 'fundamental '(#\c-x #\c-c) false)
(define-key 'fundamental '(#\c-x #\c-z) false)
(define-key 'fundamental '(#\c-x #\c) false)
(define-key 'fundamental '(#\c-x #\z) false)

(define (standard-configuration command thunk)
  (with-editor-interrupts-disabled
   (lambda ()
     (let loop ()
       (call-with-current-continuation
	(lambda (k)
	  (with-saved-configuration
	   (lambda ()
	     (delete-other-windows (current-window))
	     (let ((buffer
		    (temporary-buffer
		     (string-append
		      " *"
		      (symbol->string command)
		      "-dialog*"))))
	       (select-buffer buffer)
	       (handle-floppy-errors (lambda () (within-continuation k loop))
				     default-floppy-abort-handler
				     thunk))))))))))

(define (append-string string)
  (insert-string string (buffer-end (current-buffer))))

(define (show-dialog)
  (let ((window (selected-window)))
    (let ((buffer (window-buffer window)))
      (set-window-point! window (buffer-start buffer))
      (if (not (window-mark-visible? window (buffer-end buffer)))
	  (set-window-point! window (buffer-end buffer)))))
  (update-screens! false)
  (sit-for 0))

(define (wait-for-user)
  ;; This should ignore input events (like focus change).
  (prompt-for-char "Type any character to continue"))

;;;; Initialize Floppy

(define-command initialize-floppy
  "Initialize a floppy disk.
Requests confirmation unless a prefix arg is given."
  "P"
  (lambda (no-confirmation?)
    (standard-configuration 'initialize-floppy
      (lambda ()
	(if (or no-confirmation?
		(begin
		  (append-string
		   "This command initializes a floppy disk.
The disk will be formatted as a DOS disk.
If you have any data on the disk, it will be destroyed.

The disk must be a high-density disk; high-density disks are black and
are marked \"HD\" in one corner.  These disks are also often marked
\"DS/HD\" somewhere.

If you want to continue, please insert a disk in the floppy drive,
then answer \"yes\" to the prompt below.")
		  (prompt-for-yes-or-no?
		   "Proceed with floppy disk initialization")))
	    (if (initialize-floppy)
		(begin
		  (append-string "

Floppy successfully initialized.")
		  (if (not no-confirmation?)
		      (wait-for-user)))
		(message "OK, not initializing floppy.")))))))

(define (initialize-floppy)
  (let loop ()
    (append-string
     "
----------------------------------------------------------------------
Initializing floppy.

Please wait, this will take about five to ten minutes.
")
    (call-with-current-continuation
     (lambda (k)
       (bind-condition-handler (list condition-type:floppy-error)
	   (lambda (condition)
	     (append-string
	      "
If you want to continue, please ")
	     (let ((type (condition/type condition)))
	       (cond ((eq? type condition-type:no-floppy-in-drive)
		      (append-string "insert a floppy disk"))
		     ((eq? type condition-type:floppy-drive-busy)
		      (append-string
		       "eject the floppy disk and re-insert it"))
		     (else
		      (append-string "correct the error")
		      (let ((buffer
			     (temporary-buffer " *floppy-error*")))
			(insert-string
			 (floppy-error/output condition)
			 (buffer-start buffer))
			(set-buffer-point! buffer
					   (buffer-start buffer))
			(set-buffer-read-only! buffer)
			(pop-up-buffer buffer false)))))
	     (append-string
	      ",
then answer \"yes\" to the prompt below.")
	     (if (prompt-for-yes-or-no?
		  "Proceed with floppy disk initialization")
		 (within-continuation k
		   (lambda ()
		     (delete-other-windows (current-window))
		     (loop)))
		 (k false)))
	 (lambda ()
	   (mediainit-floppy)
	   (make-floppy-file-system)
	   true))))))

(define (mediainit-floppy)
  (append-string "\nFormatting disk...")
  (call-with-temporary-buffer " *mediainit-floppy*"
    (lambda (buffer)
      (let ((result
	     (shell-command #f (cons (buffer-start buffer) #t) #f #f
			    mediainit-command))
	    (lose
	     (lambda (condition-type)
	       (error condition-type
		      'COMMAND mediainit-command
		      'OUTPUT (buffer->string buffer)))))
	(if (equal? result '(EXITED . 0))
	    (append-string "done")
	    (begin
	      (editor-beep)
	      (append-string "ERROR\n")
	      (if (equal? result '(EXITED . 1))
		  (let ((message
			 (let ((start (buffer-start buffer)))
			   (extract-string start (line-end start 0)))))
		    (cond ((string=? message mediainit-floppy-missing)
			   (append-string
			    "\nThere is no disk in the floppy drive.")
			   (lose condition-type:no-floppy-in-drive))
			  ((string=? message mediainit-floppy-busy)
			   (append-string "\nThe floppy drive is busy.")
			   (lose condition-type:floppy-drive-busy))
			  (else
			   (lose condition-type:floppy-error))))
		  (lose condition-type:floppy-error))))))))

(define mediainit-command
  "/usr/bin/mediainit -i 2 -f 16 /dev/rfd")

(define mediainit-floppy-missing
  "mediainit: initialize media command failed - No such device or address")

(define mediainit-floppy-busy
  "mediainit: can't open file /dev/rfd - Device busy")

(define (make-floppy-file-system)
  (append-string "\nCreating file system...")
  (call-with-temporary-buffer " *make-floppy-fs*"
    (lambda (buffer)
      (if (equal? '(EXITED . 0)
		  (shell-command #f (cons (buffer-start buffer) #t) #f #f
				 make-floppy-fs-command))
	  (append-string "done")
	  (error condition-type:floppy-error
		 'COMMAND make-floppy-fs-command
		 'OUTPUT (buffer->string buffer))))))

(define make-floppy-fs-command
  "/bin/dd if=/usr/local/lib/ibm-image.dd of=/dev/rfd obs=18k")

;;;; Reading and Writing Floppy

(define-command load-from-floppy
  "Copy the files from a floppy disk to the working directory."
  ()
  (lambda ()
    (standard-configuration 'load-from-floppy
      (lambda ()
	(load-from-floppy)))))

(define (load-from-floppy)
  (append-string "Loading files from floppy to working directory.\n")
  (let ((records (read-floppy-directory)))
    (if (null? records)
	(append-string "\nFloppy has no files to load.")
	(for-each (lambda (record)
		    (append-string
		     (string-append
		      "\nLoading file \""
		      (file-record/name record)
		      "\"..."))
		    (read-floppy-file record)
		    (append-string "done"))
		  records)))
  (set! floppy-contents-loaded? true)
  (append-string "\n\nFloppy contents loaded.")
  (wait-for-user))

(define floppy-contents-loaded?)

(define-command checkpoint-floppy
  "Update a floppy disk to contain the same files as the working directory."
  ()
  (lambda ()
    (standard-configuration 'checkpoint-floppy
      (lambda ()
	(checkpoint-floppy)
	(append-string "\n\nFloppy checkpoint finished.")
	(wait-for-user)))))

(define (checkpoint-floppy)
  (save-some-buffers #t #f)
  (append-string "Copying files from working directory to floppy.\n")
  (let* ((working-directory (read-working-directory))
	 (floppy-directory (read-floppy-directory)))
    (with-values
	(lambda ()
	  (three-way-sort file-record/name=?
			  working-directory
			  floppy-directory))
      (lambda (files-to-copy pairs files-to-delete)
	(for-each (lambda (pair)
		    (if (> (file-record/time (car pair))
			   (file-record/time (cdr pair)))
			(set! files-to-copy
			      (cons (car pair) files-to-copy))))
		  pairs)
	(if (null? files-to-delete)
	    (append-string "\nThere are no files to delete.")
	    (begin
	      (append-string "

The following files appear on your floppy, but not in the working directory:
")
	      (for-each (lambda (record)
			  (append-string
			   (string-append "\n\t" (file-record/name record))))
			files-to-delete)
	      (append-string "

Answer \"yes\" to delete these files from your floppy,
otherwise answer \"no\" to leave these files on your floppy.
")
	      (if (prompt-for-yes-or-no? "Delete these files")
		  (for-each (lambda (record)
			      (append-string
			       (string-append "\nDeleting file \""
					      (file-record/name record)
					      "\"..."))
			      (delete-floppy-file record)
			      (append-string "done"))
			    files-to-delete)
		  (append-string "\nOK, not deleting files."))))
	(if (null? files-to-copy)
	    (append-string "\nThere are no files to copy.")
	    (for-each (lambda (record)
			(append-string
			 (string-append "\nCopying file \""
					(file-record/name record)
					"\"..."))
			(write-floppy-file record)
			(append-string "done"))
		      files-to-copy))))))

(define (read-working-directory)
  (append-string "\nReading working directory...")
  (let ((result
	 (map (lambda (pathname)
		(make-file-record
		 (file-namestring pathname)
		 (* (quotient (file-modification-time pathname) 60) 60)))
	      (list-transform-negative (directory-read student-work-directory)
		file-directory?)))
	(valid-dos-record?
	 (lambda (record)
	   (valid-dos-filename? (file-record/name record)))))
    (append-string "done")
    (let ((non-dos (list-transform-negative result valid-dos-record?)))
      (if (null? non-dos)
	  result
	  (begin
	    (append-string
	     "

The following files have names that are not valid for DOS disks:

")
	    (for-each (lambda (record)
			(append-string (file-record/name record)))
		      non-dos)
	    (append-string
	     "

These files cannot be saved to your floppy disk.
If you want to save them, you must rename them to valid DOS filenames.

To abort this command, giving you the chance to rename these files,
answer \"yes\" to the question below.

To continue with this command, saving all files EXCEPT these,
answer \"no\" to the question below.")
	    (if (prompt-for-yes-or-no? "Abort this command")
		(begin
		  (append-string
		   "
----------------------------------------------------------------------
To see a description of valid DOS filenames, use the command
M-x describe-dos-filenames.  To rename your files, use the command
M-x rename-file, or use the `r' command in Dired.")
		  (sit-for (* 5 1000))
		  (*floppy-abort-handler*))
		(begin
		  (append-string
		   "
----------------------------------------------------------------------")
		  (list-transform-positive result valid-dos-record?))))))))

(define-command describe-dos-filenames
  "Describe the format of DOS filenames."
  ()
  (lambda ()
    (with-output-to-help-display
     (lambda ()
       (write-string dos-filename-description)
       (newline)))))

(define (write-floppy-file record)
  ;; It is necessary to delete the file before writing it, because
  ;; otherwise the modification time is not updated.  Furthermore we
  ;; must handle errors because the dosrm command might fail.
  (let ((dos-name (file-record/dos-name record)))
    (call-with-current-continuation
     (lambda (continue)
       (bind-condition-handler (list condition-type:floppy-error)
	   (lambda (condition)
	     (if (string-prefix? "dosrm: cannot open "
				 (floppy-error/output condition))
		 (continue unspecific)))
	 (lambda ()
	   (run-dosrm-command dos-name)))))
    (run-doscp-command (file-record/unix-name record) dos-name)))

(define (read-floppy-file record)
  (let ((unix-name (file-record/unix-name record)))
    (run-doscp-command (file-record/dos-name record) unix-name)
    (set-file-times! unix-name false (file-record/time record))))

(define (delete-floppy-file record)
  (run-dosrm-command (file-record/dos-name record)))

(define (read-floppy-directory)
  (append-string "\nReading floppy directory...")
  (let ((result (dosls floppy-directory)))
    (append-string "done")
    result))

(define (dos-file-modification-time dos-name)
  (let ((records (dosls dos-name)))
    (if (null? records)
	(error "No output from dosls command."))
    (if (not (null? (cdr records)))
	(error "Too much output from dosls command."))
    (file-record/time (car records))))

;;;; Reading Floppy Directory

(define (dosls directory)
  (let ((contents (run-dosll-command directory)))
    (parse-dosls-output contents 0 (string-length contents))))

(define parse-dosls-output
  (let ((leader-pattern
	 (re-compile-pattern "\nThe DOS Volume Label is +\\([^ \n]*\\) *\n *\n"
			     false)))
    (lambda (string start end)
      (let ((offset (time-zone-offset)))
	(let loop
	    ((start
	      (let ((r (re-substring-match leader-pattern string start end)))
		(if r
		    (re-match-end-index 0 r)
		    start))))
	  (if (= start end)
	      '()
	      (let ((eol
		     (or (substring-find-next-char string start end #\newline)
			 end)))
		(with-values
		    (lambda ()
		      (parse-dosls-line string start eol offset))
		  (lambda (filename time)
		    (let ((records (if (= eol end) '() (loop (+ eol 1)))))
		      (if (directory-filename? filename)
			  records
			  (cons (make-file-record filename time)
				records))))))))))))

(define parse-dosls-line
  (let ((line-pattern
	 (re-compile-pattern
	  (string-append "^ *[0-9]+ +[0-9]+ +"
			 "\\([a-zA-z]+\\) +\\([0-9]+\\) +"
			 "\\([0-9][0-9][0-9][0-9]\\) +"
			 "\\([0-9][0-9]\\):\\([0-9][0-9]\\) +"
			 "/dev/rfd:/\\(.+\\) *$")
	  false)))
    (lambda (string start end offset)
      (let ((r (re-substring-match line-pattern string start end)))
	(if (not r)
	    (error "Line doesn't match dosls -l pattern:"
		   (substring string start end)))
	(let ((month (extract-string-match string r 1))
	      (day (extract-string-match string r 2))
	      (year (extract-string-match string r 3))
	      (hour (extract-string-match string r 4))
	      (minute (extract-string-match string r 5))
	      (filename (extract-string-match string r 6)))
	  (values (string-downcase filename)
		  (+ (make-dos-time (string->number year)
				    (month-name->number month)
				    (string->number day)
				    (string->number hour)
				    (string->number minute))
		     offset)))))))

(define (month-name->number month)
  (let ((months
	 '#("Jan" "Feb" "Mar" "Apr" "May" "Jun"
		  "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
    (let loop ((index 0))
      (if (= index 12)
	  (error "Unknown month string:" month))
      (if (string-prefix-ci? (vector-ref months index) month)
	  (+ index 1)
	  (loop (+ index 1))))))

(define (make-dos-time year month day hour minute)
  (* 60
     (+ minute
	(* 60
	   (+ hour
	      (* 24
		 (+ (- day 1)
		    (vector-ref '#(0 31 59 90 120 151 181 212 243 273 304 334)
				(- month 1))
		    (if (and (= 0 (remainder year 4)) (> month 2))
			1
			0)
		    (- (quotient (- year 1) 4) (quotient 1970 4))
		    (* 365 (- year 1970)))))))))

(define (time-zone-offset)
  (let ((decoded-time (get-decoded-time)))
    (- (* (quotient (universal-time->file-time (get-universal-time)) 60) 60)
       (make-dos-time (decoded-time/year decoded-time)
		      (decoded-time/month decoded-time)
		      (decoded-time/day decoded-time)
		      (decoded-time/hour decoded-time)
		      (decoded-time/minute decoded-time)))))

(define-structure (file-record (conc-name file-record/))
  (name false read-only true)
  time)

(define (file-record/dos-name record)
  (string-append floppy-directory (file-record/name record)))

(define floppy-directory
  "/dev/rfd:/")

(define (file-record/unix-name record)
  (->namestring
   (merge-pathnames (file-record/name record) student-work-directory)))

(define (file-record/name=? x y)
  (string=? (file-record/name x) (file-record/name y)))

;;;; Floppy Command Subprocesses

(define (run-dosll-command directory)
  (run-dos-command "/usr/bin/dosls" "-Al" directory))

(define (run-doscp-command from to)
  (run-dos-command "/usr/bin/doscp" "-f" from to))

(define (run-dosrm-command filename)
  (run-dos-command "/usr/bin/dosrm" "-f" filename))

(define (run-dos-command program . arguments)
  (call-with-temporary-buffer " *dos-floppy-command*"
    (lambda (buffer)
      (let ((result
	     (apply run-synchronous-process
		    #f (cons (buffer-start buffer) #t) #f #f
		    program arguments)))
	(let ((output
	       (extract-string (buffer-start buffer) (buffer-end buffer))))
	  (if (equal? result '(EXITED . 0))
	      output
	      ((cond ((not (equal? result '(EXITED . 1)))
		      make-condition:floppy-error)
		     ((string-prefix? no-floppy-in-drive output)
		      make-condition:no-floppy-in-drive)
		     ((string-prefix? non-dos-floppy-in-drive output)
		      make-condition:non-dos-floppy-in-drive)
		     ((re-match-forward floppy-write-protected-regexp
					(buffer-start buffer)
					(buffer-end buffer)
					false)
		      make-condition:floppy-write-protected)
		     (else
		      make-condition:floppy-error))
	       (form-floppy-command program arguments) output)))))))

(define (form-floppy-command program arguments)
  (apply string-append
	 program
	 (let loop ((arguments arguments))
	   (if (null? arguments)
	       '()
	       (cons* " " (car arguments) (loop (cdr arguments)))))))

(define no-floppy-in-drive
  "Error reading. block = 0 on device /dev/rfd")

(define floppy-write-protected-regexp
  "Error writing. block = [0-9]+ on device /dev/rfd\nErrno = 13$")

(define non-dos-floppy-in-drive
  "Unrecognizable disc format on /dev/rfd")

(define (handle-floppy-errors continue abort thunk)
  (fluid-let ((*floppy-abort-handler* abort))
    (bind-condition-handler (list condition-type:floppy-error)
	(lambda (condition)
	  (append-string
	   (string-append
	    "ERROR\n\n"
	    (condition/report-string condition)
	    "\n\nTo try again, please "
	    (if (let ((type (condition/type condition)))
		  (or (eq? type condition-type:no-floppy-in-drive)
		      (eq? type condition-type:non-dos-floppy-in-drive)))
		"insert a DOS floppy"
		"correct the error")
	    ",\nthen answer \"yes\" to the question below."))
	  (if (prompt-for-yes-or-no? "Try again")
	      (continue)
	      (abort)))
      thunk)))

(define *floppy-abort-handler*)

(define (default-floppy-abort-handler)
  (message "OK, aborting command")
  (abort-current-command))

;;;; Floppy Errors

(define condition-type:floppy-error
  (make-condition-type 'FLOPPY-ERROR condition-type:error
      '(COMMAND OUTPUT)
    (lambda (condition port)
      condition
      (write-string "An error occurred while accessing the floppy.\n" port)
      (write-string "The command being executed was:\n\n" port)
      (write-string (floppy-error/command condition) port)
      (write-string "\n\nThe output from the command was:\n\n" port)
      (write-string (floppy-error/output condition) port))))

(define make-condition:floppy-error
  (condition-signaller condition-type:floppy-error
		       '(COMMAND OUTPUT)
		       standard-error-handler))

(define floppy-error/command
  (condition-accessor condition-type:floppy-error 'COMMAND))

(define floppy-error/output
  (condition-accessor condition-type:floppy-error 'OUTPUT))

(define condition-type:no-floppy-in-drive
  (make-condition-type 'NO-FLOPPY-IN-DRIVE condition-type:floppy-error '()
    (lambda (condition port)
      condition
      (write-string "No floppy disk in drive." port))))

(define make-condition:no-floppy-in-drive
  (condition-signaller condition-type:no-floppy-in-drive
		       '(COMMAND OUTPUT)
		       standard-error-handler))

(define condition-type:floppy-write-protected
  (make-condition-type 'FLOPPY-WRITE-PROTECTED condition-type:floppy-error '()
    (lambda (condition port)
      condition
      (write-string "The floppy disk is write-protected." port))))

(define make-condition:floppy-write-protected
  (condition-signaller condition-type:floppy-write-protected
		       '(COMMAND OUTPUT)
		       standard-error-handler))

(define condition-type:floppy-drive-busy
  (make-condition-type 'FLOPPY-DRIVE-BUSY condition-type:floppy-error '()
    (lambda (condition port)
      condition
      (write-string "The floppy drive is busy." port))))

(define condition-type:non-dos-floppy-in-drive
  (make-condition-type 'NON-DOS-FLOPPY-IN-DRIVE condition-type:floppy-error '()
    (lambda (condition port)
      condition
      (write-string "Floppy disk in drive is not DOS format." port))))

(define make-condition:non-dos-floppy-in-drive
  (condition-signaller condition-type:non-dos-floppy-in-drive
		       '(COMMAND OUTPUT)
		       standard-error-handler))

;;;; Miscellaneous

(define (directory-filename? filename)
  (char=? #\/ (string-ref filename (- (string-length filename) 1))))

(define (extract-string-match string r n)
  (substring string (re-match-start-index n r) (re-match-end-index n r)))

(define (three-way-sort = set set*)
  (let ((member? (member-procedure =)))
    (let loop ((set set) (set* (list-copy set*)))
      (if (null? set)
	  (values '() '() set*)
	  (let ((item (member? (car set) set*)))
	    (if item
		(with-values
		    (lambda () (loop (cdr set) (delq! (car item) set*)))
		  (lambda (set-only both set*-only)
		    (values set-only
			    (cons (cons (car set) (car item)) both)
			    set*-only)))
		(with-values (lambda () (loop (cdr set) set*))
		  (lambda (set-only both set*-only)
		    (values (cons (car set) set-only)
			    both
			    set*-only)))))))))

(define (buffer->string buffer)
  (extract-string (buffer-start buffer) (buffer-end buffer)))

;;;; DOS Filenames

(define valid-dos-filename?
  (let ((invalid-chars
	 (char-set-invert
	  (char-set-union
	   (char-set-union char-set:lower-case char-set:numeric)
	   (char-set #\_ #\^ #\$ #\! #\# #\% #\& #\-
		     #\{ #\} #\( #\) #\@ #\' #\`)))))
    (lambda (filename)
      (let ((end (string-length filename))
	    (valid-name?
	     (lambda (end)
	       (and (<= 1 end 8)
		    (not (substring-find-next-char-in-set filename 0 end
							  invalid-chars))
		    (not
		     (there-exists? '("clock$" "con" "aux" "com1" "com2"
					       "com3" "com4" "lpt1" "lpt2"
					       "lpt3" "nul" "prn")
		       (lambda (name)
			 (substring=? filename 0 end
				      name 0 (string-length name)))))))))
	(let ((dot (string-find-next-char filename #\.)))
	  (if (not dot)
	      (valid-name? end)
	      (and (valid-name? dot)
		   (<= 2 (- end dot) 4)
		   (not (substring-find-next-char-in-set filename (+ dot 1) end
							 invalid-chars)))))))))


(define dos-filename-description
  "DOS filenames are between 1 and 8 characters long, inclusive.  They
may optionally be followed by a period and a suffix of 1 to 3
characters.

In other words, a valid filename consists of:

* 1 to 8 characters, OR

* 1 to 8 characters, followed by a period, followed by 1 to 3
  characters.

The characters that may be used in a filename (or a suffix) are:

* The lower case letters: a b c ... z

* The digits: 0 1 2 ... 9

* These special characters: ' ` ! @ # $ % ^ & ( ) - _ { }

The period (.) may appear exactly once as a separator between the
filename and the suffix.

The following filenames are reserved and may not be used:

	aux	clock$	com1	com2	com3	com4
	con	lpt1	lpt2	lpt3	nul	prn")

;;;; Overrides of Editor Procedures

(set! os/auto-save-pathname
      (let ((usual os/auto-save-pathname))
	(lambda (pathname buffer)
	  (if pathname
	      (if (student-directory? pathname)
		  (pathname-new-type pathname "asv")
		  (usual pathname buffer))
	      (let ((directory (buffer-default-directory buffer)))
		(if (student-directory? directory)
		    (merge-pathnames
		     (let ((name
			    (string-append
			     (let ((name (buffer-name buffer)))
			       (let ((index (string-find-next-char name #\.)))
				 (if (not index)
				     (if (> (string-length name) 8)
					 (substring name 0 8)
					 name)
				     (substring name 0 (min 8 index)))))
			     ".asv")))
		       (if (valid-dos-filename? name)
			   name
			   "default.asv"))
		     directory)
		    (usual pathname buffer)))))))

(set! os/precious-backup-pathname
      (let ((usual os/precious-backup-pathname))
	(lambda (pathname)
	  (if (student-directory? pathname)
	      (pathname-new-type pathname "bak")
	      (usual pathname)))))

(set! os/default-backup-filename
      (lambda ()
	(->namestring (merge-pathnames "default.bak" student-work-directory))))

(set! os/buffer-backup-pathname
      (let ((usual os/buffer-backup-pathname))
	(lambda (truename buffer)
	  (if (student-directory? truename)
	      (values (pathname-new-type truename "bak") '())
	      (usual truename buffer)))))

;;; These next two depend on the fact that they are only invoked when
;;; the current buffer is the Dired buffer that is being tested.

(set! os/backup-filename?
      (let ((usual os/backup-filename?))
	(lambda (filename)
	  (if (student-directory? (dired-buffer-directory (current-buffer)))
	      (equal? "bak" (pathname-type filename))
	      (usual filename)))))

(set! os/auto-save-filename?
      (let ((usual os/auto-save-filename?))
	(lambda (filename)
	  (if (student-directory? (dired-buffer-directory (current-buffer)))
	      (equal? "asv" (pathname-type filename))
	      (usual filename)))))

(define (dired-buffer-directory buffer)
  ;; Similar to the definition in "dired.scm".  That definition should
  ;; be exported in order to eliminate this redundant definition.
  (or (buffer-get buffer 'DIRED-DIRECTORY)
      (buffer-default-directory buffer)))

(set! prompt-for-pathname*
      (let ((usual prompt-for-pathname*))
	(lambda args
	  (let ((pathname (apply usual args)))
	    (if (or (not (student-directory? pathname))
		    (valid-dos-filename? (file-namestring pathname))
		    (file-exists? pathname)
		    (with-saved-configuration
		     (lambda ()
		       (delete-other-windows (current-window))
		       (select-buffer
			(temporary-buffer " *invalid-filename-dialog*"))
		       (append-string
			"The file name you have specified,\n\n\t")
		       (append-string (file-namestring pathname))
		       (append-string
			"

is not a valid name for a DOS disk.  If you create a file with this
name, you will not be able to save it to your floppy disk.

If you want to use this name anyway, answer \"yes\" to the question
below.  Otherwise, answer \"no\" to use a different name.
----------------------------------------------------------------------
")
		       (append-string dos-filename-description)
		       (prompt-for-yes-or-no? "Use this non-DOS name"))))
		pathname
		(apply prompt-for-pathname* args))))))

(define (student-directory? pathname)
  (let ((pathname (->pathname pathname))
	(prefix student-work-directory))
    (and (host=? (pathname-host pathname) (pathname-host prefix))
	 (equal? (pathname-device pathname) (pathname-device prefix))
	 (let loop
	     ((d1 (pathname-directory pathname))
	      (d2 (pathname-directory prefix)))
	   (or (null? d2)
	       (and (not (null? d1))
		    (equal? (car d1) (car d2))
		    (loop (cdr d1) (cdr d2))))))))