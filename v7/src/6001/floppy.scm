#| -*-Scheme-*-

$Id: floppy.scm,v 1.9 1992/09/24 20:21:04 cph Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; 6.001: Floppy Commands

(declare (usual-integrations))

;;;; Login and Logout

(define (standard-login-initialization)
  (set! floppy-contents-loaded? false)
  (let ((homedir (user-homedir-pathname)))
    (let ((workdir
	   (let ((workdir (merge-pathnames "work/" homedir)))
	     (if (file-directory? workdir)
		 workdir
		 homedir))))
      (set! working-directory (->namestring workdir))
      (set-default-directory workdir)
      (set-working-directory-pathname! workdir))
    (standard-configuration 'login login-loop)
    (let ((buffer (temporary-buffer "*motd*")))
      (call-with-current-continuation
       (lambda (k)
	 (bind-condition-handler (list condition-type:file-error)
	     (lambda (condition)
	       condition
	       (kill-buffer buffer)
	       (k unspecific))
	   (lambda ()
	     (%insert-file (buffer-start buffer)
			   (merge-pathnames "motd" homedir)
			   false)))
	 (set-buffer-point! buffer (buffer-start buffer))
	 (select-buffer buffer)))))
  (message "Login completed."))

(define floppy-contents-loaded?)

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
disk, and insert it into the drive.  When you have done this, type any
character to continue.")
     (wait-for-user)
     (if (initialize-floppy)
	 (begin
	   (append-string
	    "
----------------------------------------------------------------------
Please eject your backup disk from the floppy drive.

Now select your other disk, label it as your \"primary\" disk, and
insert it into the floppy drive.  When you have done this, type any
character to continue.")
	   (wait-for-user)
	   (if (initialize-floppy)
	       (begin
		 (set! floppy-contents-loaded? true)
		 (append-string "
----------------------------------------------------------------------
Your disks are now initialized.
Type any character to finish logging in.")
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
  (if (prompt-for-confirmation? "Continue with login")
      (thunk)
      (login-loop)))

(define-command logout
  "Logout from the 6.001 Scheme system."
  ()
  (lambda ()
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
		       (if (prompt-for-yes-or-no?
			    "Log out without saving files")
			   (exit-scheme)
			   (abort-current-command)))))
		(if (not floppy-contents-loaded?)
		    (begin
		      (append-string "You logged in without a floppy disk.\n")
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
			(within-continuation k loop))
		      (lambda ()
			(append-string
			 "
----------------------------------------------------------------------")
			(abort))
		      checkpoint-floppy)))))
	      (exit-scheme))))))))

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
	     (shell-command false (buffer-start buffer) false false
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
		  (shell-command false (buffer-start buffer) false false
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
	    (for-each (lambda (record)
			(append-string
			 (string-append "\nDeleting file \""
					(file-record/name record)
					"\"..."))
			(delete-floppy-file record)
			(append-string "done"))
		      files-to-delete))
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
	      (list-transform-negative (directory-read working-directory)
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
	      (if (re-match-substring-forward leader-pattern false false
					      string start end)
		  (re-match-end-index 0)
		  start)))
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
      (if (not (re-match-substring-forward line-pattern false false
					   string start end))
	  (error "Line doesn't match dosls -l pattern:"
		 (substring string start end)))
      (let ((month (extract-string-match string 1))
	    (day (extract-string-match string 2))
	    (year (extract-string-match string 3))
	    (hour (extract-string-match string 4))
	    (minute (extract-string-match string 5))
	    (filename (extract-string-match string 6)))
	(values (string-downcase filename)
		(+ (make-dos-time (string->number year)
				  (month-name->number month)
				  (string->number day)
				  (string->number hour)
				  (string->number minute))
		   offset))))))

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
  (let ((decoded-time (get-decoded-time))
	(file-time (unix/current-file-time)))
    (- (* (quotient file-time 60) 60)
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
  (string-append working-directory (file-record/name record)))

(define working-directory)

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
		    false (buffer-start buffer) false false
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

(define (extract-string-match string n)
  (substring string (re-match-start-index n) (re-match-end-index n)))

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
	       (handle-floppy-errors
		(lambda () (within-continuation k loop))
		default-floppy-abort-handler
		thunk))))))))))

(define (with-saved-configuration thunk)
  (let ((screen (selected-screen)))
    (let ((configuration (screen-window-configuration screen)))
      (fluid-let ((restore-saved-continuation? true))
	(dynamic-wind
	 (lambda () unspecific)
	 thunk
	 (lambda ()
	   (if restore-saved-continuation?
	       (set-screen-window-configuration! screen configuration))))))))

(define (dont-restore-saved-configuration)
  (set! restore-saved-continuation? false)
  unspecific)

(define restore-saved-continuation?)

(define (append-string string)
  (insert-string string)
  (update-screens! false)
  (sit-for 0))

(define (buffer->string buffer)
  (extract-string (buffer-start buffer) (buffer-end buffer)))

(define (wait-for-user)
  ;; This should ignore input events (like focus change).
  (prompt-for-char "Type any character to continue"))