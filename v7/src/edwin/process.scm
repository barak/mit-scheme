;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/process.scm,v 1.12 1991/10/29 13:48:22 cph Exp $
;;;
;;;	Copyright (c) 1991 Massachusetts Institute of Technology
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

;;;; Subprocess Support

(declare (usual-integrations))

(define (initialize-processes!)
  (set! edwin-processes '())
  (let ((path (get-environment-variable "PATH")))
    (if (not path)
	(error "Can't find PATH environment variable."))
    (set-variable! exec-path (parse-path-string path))))

(define edwin-processes)

(define-variable exec-path
  "List of directories to search programs to run in subprocesses.
Each element is a string (directory name) or #F (try default directory)."
  '()
  (lambda (exec-path)
    (and (list? exec-path)
	 (for-all? exec-path
	   (lambda (element)
	     (or (not element)
		 (pathname? element)))))))

(define-variable process-connection-type
  "Control type of device used to communicate with subprocesses.
Values are #f to use a pipe, #t for a pty (or pipe if ptys not supported).
Value takes effect when `start-process' is called."
  true
  boolean?)

(define-variable delete-exited-processes
  "True means delete processes immediately when they exit.
False means don't delete them until \\[list-processes] is run."
  true
  boolean?)

(define-structure (process
		   (constructor %make-process (subprocess name %buffer)))
  (subprocess false read-only true)
  (name false read-only true)
  %buffer
  (mark false)
  (filter false)
  (sentinel false)
  (kill-without-query false)
  (notification-tick (cons false false)))

(define-integrable (process-arguments process)
  (subprocess-arguments (process-subprocess process)))

(define-integrable (process-input-channel process)
  (subprocess-input-channel (process-subprocess process)))

(define-integrable (process-output-channel process)
  (subprocess-output-channel (process-subprocess process)))

(define-integrable (process-status-tick process)
  (subprocess-status-tick (process-subprocess process)))

(define-integrable (process-exit-reason process)
  (subprocess-exit-reason (process-subprocess process)))

(define (process-status process)
  (status->emacs-status (subprocess-status (process-subprocess process))))

(define (status->emacs-status status)
  (case status
    ((RUNNING) 'RUN)
    ((STOPPED) 'STOP)
    ((EXITED) 'EXIT)
    ((SIGNALLED) 'SIGNAL)
    (else status)))

(define (process-runnable? process)
  (let ((status (subprocess-status (process-subprocess process))))
    (or (eq? 'RUNNING status)
	(eq? 'STOPPED status))))

(define-integrable (process-buffer process)
  (process-%buffer process))

(define (set-process-buffer! process buffer)
  (without-interrupts
   (lambda ()
     (if (not (eq? buffer (process-buffer process)))
	 (begin
	   (set-process-%buffer! process buffer)
	   (update-process-mark! process))))))

(define (update-process-mark! process)
  (set-process-mark!
   process
   (let ((buffer (process-buffer process)))
     (and buffer
	  (mark-right-inserting-copy (buffer-end buffer))))))

(define (start-process name buffer environment program . arguments)
  (let ((directory (buffer-default-directory buffer)))
    (let ((make-subprocess
	   (let ((filename (find-program program directory))
		 (arguments (list->vector (cons program arguments)))
		 (pty? (ref-variable process-connection-type buffer)))
	     (lambda ()
	       (start-subprocess filename arguments environment pty?)))))
      ;; Calling WITH-WORKING-DIRECTORY-PATHNAME is a kludge --
      ;; there's no other way to specify the working directory of the
      ;; subprocess.  The subprocess abstraction should be fixed to
      ;; allow this.
      (with-working-directory-pathname directory
	(lambda ()
	  (without-interrupts
	   (lambda ()
	     (let ((subprocess (make-subprocess)))
	       (let ((channel (subprocess-input-channel subprocess)))
		 (if channel
		     (begin
		       (channel-nonblocking channel)
		       (channel-register channel))))
	       (let ((process
		      (%make-process
		       subprocess
		       (do ((n 2 (+ n 1))
			    (name* name
				   (string-append name
						  "<" (number->string n) ">")))
			   ((not (get-process-by-name name*)) name*))
		       buffer)))
		 (update-process-mark! process)
		 (subprocess-put! subprocess 'EDWIN-PROCESS process)
		 (set! edwin-processes (cons process edwin-processes))
		 process)))))))))

(define (start-subprocess filename arguments environment pty?)
  (if (and pty? ((ucode-primitive have-ptys? 0)))
      (start-pty-subprocess filename arguments environment)
      (start-pipe-subprocess filename arguments environment)))

(define (delete-process process)
  (let ((subprocess (process-subprocess process)))
    (without-interrupts
     (lambda ()
       (set! edwin-processes (delq! process edwin-processes))
       (subprocess-remove! subprocess 'EDWIN-PROCESS)
       (if (process-runnable? process)
	   (begin
	     (subprocess-kill subprocess)
	     (perform-status-notification process 'SIGNALLED false)))
       (let ((channel (subprocess-input-channel subprocess)))
	 (if (and channel (channel-open? channel))
	     (channel-unregister channel)))
       (subprocess-delete subprocess)))))

(define (get-process-by-name name)
  (let loop ((processes edwin-processes))
    (cond ((null? processes) false)
	  ((eq? name (process-name (car processes))) (car processes))
	  (else (loop (cdr processes))))))

(define (get-buffer-process buffer)
  (let loop ((processes edwin-processes))
    (cond ((null? processes) false)
	  ((eq? buffer (process-buffer (car processes))) (car processes))
	  (else (loop (cdr processes))))))

(define (buffer-processes buffer)
  (let loop ((processes edwin-processes))
    (cond ((null? processes)
	   '())
	  ((eq? buffer (process-buffer (car processes)))
	   (cons (car processes) (loop (cdr processes))))
	  (else
	   (loop (cdr processes))))))

;;;; Input and Output

(define (process-send-eof process)
  (process-send-char process #\EOT))

(define (process-send-substring process string start end)
  (channel-write-block (process-output-channel process) string start end))

(define (process-send-string process string)
  (channel-write-string-block (process-output-channel process) string))

(define (process-send-char process char)
  (channel-write-char-block (process-output-channel process) char))

(define (accept-process-output)
  (without-interrupts
   (lambda ()
     (let loop ((processes edwin-processes) (output? false))
       (if (null? processes)
	   output?
	   (loop (cdr processes)
		 (if (poll-process-for-output (car processes))
		     true
		     output?)))))))

(define (poll-process-for-output process)
  (let ((channel (process-input-channel process))
	(buffer (make-string 512)))
    (and (channel-open? channel)
	 (let ((n (channel-read channel buffer 0 512)))
	   (and n
		(if (positive? n)
		    (output-substring process buffer n)
		    (begin
		      (channel-close channel)
		      false)))))))

(define (notify-process-status-changes)
  (without-interrupts
   (lambda ()
     (let ((tick (subprocess-global-status-tick)))
       (and (not (eq? tick global-notification-tick))
	    (begin
	      (set! global-notification-tick tick)
	      (let loop ((processes edwin-processes) (output? false))
		(if (null? processes)
		    output?
		    (loop (cdr processes)
			  (if (poll-process-for-status-change (car processes))
			      true
			      output?))))))))))

(define global-notification-tick
  (cons false false))

(define (poll-process-for-status-change process)
  (let ((status (subprocess-status (process-subprocess process))))
    (and (not (eq? (process-notification-tick process)
		   (process-status-tick process)))
	 (perform-status-notification process
				      status
				      (process-exit-reason process)))))

(define (perform-status-notification process status reason)
  (set-process-notification-tick! process (process-status-tick process))
  (let ((value
	 (cond ((process-sentinel process)
		=>
		(lambda (sentinel)
		  (sentinel process status reason)
		  true))
	       ((eq? status 'RUNNING)
		false)
	       (else
		(let ((message
		       (string-append "\nProcess "
				      (process-name process)
				      " "
				      (process-status-message
				       (status->emacs-status status)
				       reason)
				      "\n")))
		  (output-substring process
				    message
				    (string-length message)))))))
    (if (and (or (eq? 'EXITED status)
		 (eq? 'SIGNALLED status))
	     (ref-variable delete-exited-processes))
	(delete-process process))
    value))

(define (process-status-message status reason)
  (let ((message-with-reason
	 (lambda (prefix connective)
	   (if reason
	       (string-append prefix
			      (if connective (string-append " " connective) "")
			      " "
			      (number->string reason))
	       prefix))))
    (case status
      ((RUN) "running")
      ((STOP) (message-with-reason "stopped by signal" false))
      ((EXIT)
       (if (zero? reason)
	   "finished"
	   (message-with-reason "exited abnormally" "with code")))
      ((SIGNAL) (message-with-reason "terminated by signal" false))
      (else (error "illegal process status" status)))))

(define (output-substring process string length)
  (cond ((process-filter process)
	 =>
	 (lambda (filter)
	   (filter string 0 length)
	   true))
	((process-mark process)
	 =>
	 (lambda (mark)
	   (let ((index (mark-index mark)))
	     (group-insert-substring! (mark-group mark) index string 0 length)
	     (set-mark-index! mark (+ index length)))
	   true))
	(else false)))

;;;; Signals

(define (signal-process process signal group?)
  (let ((process (process-subprocess process)))
    (let ((pty-master (and group? (subprocess-pty-master process))))
      (if pty-master
	  (pty-master-send-signal pty-master signal)
	  (subprocess-signal process signal)))))

(define (interrupt-process process group?)
  (let ((process (process-subprocess process)))
    (let ((pty-master (and group? (subprocess-pty-master process))))
      (if pty-master
	  (pty-master-interrupt pty-master)
	  (subprocess-interrupt process)))))

(define (quit-process process group?)
  (let ((process (process-subprocess process)))
    (let ((pty-master (and group? (subprocess-pty-master process))))
      (if pty-master
	  (pty-master-quit pty-master)
	  (subprocess-quit process)))))

(define (hangup-process process group?)
  (let ((process (process-subprocess process)))
    (let ((pty-master (and group? (subprocess-pty-master process))))
      (if pty-master
	  (pty-master-hangup pty-master)
	  (subprocess-hangup process)))))

(define (stop-process process group?)
  (let ((process (process-subprocess process)))
    (let ((pty-master (and group? (subprocess-pty-master process))))
      (if pty-master
	  (pty-master-stop pty-master)
	  (subprocess-stop process)))))

(define (continue-process process group?)
  (let ((process (process-subprocess process)))
    (let ((pty-master (and group? (subprocess-pty-master process))))
      (if pty-master
	  (pty-master-continue pty-master)
	  (subprocess-continue-background process)))))

(define (kill-process process group?)
  (let ((process (process-subprocess process)))
    (let ((pty-master (and group? (subprocess-pty-master process))))
      (if pty-master
	  (pty-master-kill pty-master)
	  (subprocess-kill process)))))

;;;; LIST-PROCESSES

(define-command list-processes
  "Display a list of all processes.
\(Any processes listed as exited or signalled are actually eliminated
after the listing is made.)"
  ()
  (lambda ()
    (let ((buffer (temporary-buffer "*Process List*")))
      (let ((point (buffer-point buffer)))
	(let ((write-line
	       (lambda (process status buffer command)
		 (insert-string process point)
		 (insert-horizontal-space 13 point)
		 (insert-string status point)
		 (insert-horizontal-space 24 point)
		 (insert-string buffer point)
		 (insert-horizontal-space 40 point)
		 (insert-string command point)
		 (insert-newline point))))
	  (write-line "Process" "Status" "Buffer" "Command")
	  (write-line "-------" "------" "------" "-------")
	  (do ((processes edwin-processes (cdr processes)))
	      ((null? processes))
	    (let ((process (car processes)))
	      (write-line (or (process-name process) "")
			  (let ((status (process-status process)))
			    (let ((name (symbol->string status)))
			      (if (or (eq? 'EXIT status)
				      (eq? 'SIGNAL status))
				  (let ((reason (process-exit-reason process)))
				    (delete-process process)
				    (if (and (eq? 'EXIT status)
					     (zero? reason))
					name
					(string-append
					 name
					 " "
					 (number->string reason))))
				  name)))
			  (let ((buffer (process-buffer process)))
			    (cond ((not buffer) "(none)")
				  ((buffer-alive? buffer) (buffer-name buffer))
				  (else "(killed)")))
			  (process-arguments->string
			   (process-arguments process)))))))
      (set-buffer-point! buffer (buffer-start buffer))
      (buffer-not-modified! buffer)
      (pop-up-buffer buffer false))))

(define (process-arguments->string arguments)
  (if (zero? (vector-length arguments))
      ""
      (apply string-append
	     (let loop ((arguments (vector->list arguments)))
	       (cons (car arguments)
		     (if (null? (cdr arguments))
			 '()
			 (cons " " (loop (cdr arguments)))))))))

(define (process-list)
  (list-copy edwin-processes))

;;;; Synchronous Subprocesses

(define (run-synchronous-process input-region output-mark directory pty?
				 program . arguments)
  (let ((process false)
	(start-process
	 (lambda ()
	   (start-subprocess
	    program
	    (list->vector
	     (cons (os/filename-non-directory program) arguments))
	    false
	    pty?))))
    (dynamic-wind
     (lambda ()
       (if (not process)
	   (set! process
		 (if directory
		     (with-working-directory-pathname directory start-process)
		     (start-process))))
       unspecific)
     (lambda ()
       (call-with-output-copier process output-mark
	 (lambda (copy-output)
	   (call-with-input-copier process input-region
	     (lambda (copy-input)
	       (let loop ()
		 (copy-input)
		 (copy-output)
		 (let ((status (subprocess-status process)))
		   (if (eq? status 'RUNNING)
		       (loop)
		       status))))))))
     (lambda ()
       (if (and process (not (eq? process 'DELETED)))
	   (begin
	     (subprocess-delete process)
	     (set! process 'DELETED)))
       unspecific))))

(define (call-with-output-copier process output-mark receiver)
  (let ((output-mark (and output-mark (mark-left-inserting output-mark))))
    (let ((status
	   (if output-mark
	       (let ((output-channel (subprocess-input-channel process)))
		 (let ((copy-output
			(let ((buffer (make-string 512)))
			  (lambda ()
			    (let loop ()
			      (let ((n (channel-read output-channel
						     buffer 0 512)))
				(if (and n (positive? n))
				    (begin
				      (insert-substring buffer 0 n output-mark)
				      (loop)))))))))
		   (channel-nonblocking output-channel)
		   (let ((status (receiver copy-output)))
		     (channel-blocking output-channel)
		     (copy-output)
		     status)))
	       (receiver (lambda () unspecific)))))
      (let ((reason (subprocess-exit-reason process)))
	(let ((abnormal-termination
	       (lambda (message)
		 (if output-mark
		     (begin
		       (guarantee-newlines 2 output-mark)
		       (insert-string "Process " output-mark)
		       (insert-string message output-mark)
		       (insert-string " " output-mark)
		       (insert-string (number->string reason) output-mark)
		       (insert-string "." output-mark)
		       (insert-newline output-mark))))))
	  (case status
	    ((STOPPED)
	     (abnormal-termination "stopped with signal")
	     (subprocess-kill process)
	     (subprocess-wait process))
	    ((SIGNALLED)
	     (abnormal-termination "terminated with signal"))
	    ((EXITED)
	     (if (not (eqv? 0 reason))
		 (abnormal-termination "exited abnormally with code")))))
	(subprocess-delete process)
	(cons status reason)))))

(define (call-with-input-copier process input-region receiver)
  (if input-region
      (let ((group (region-group input-region))
	    (start-index (region-start-index input-region))
	    (end-index (region-end-index input-region))
	    (input-channel (subprocess-output-channel process)))
	(channel-nonblocking input-channel)
	(call-with-current-continuation
	 (lambda (continuation)
	   (bind-condition-handler (list condition-type:system-call-error)
	       (lambda (condition)
		 (if (and (eq? 'WRITE (system-call-name condition))
			  (eq? 'BROKEN-PIPE (system-call-error condition)))
		     (continuation (subprocess-wait process))))
	     (lambda ()
	       (receiver
		(lambda ()
		  (if (< start-index end-index)
		      (let ((index (min (+ start-index 512) end-index)))
			(let ((buffer
			       (group-extract-string group
						     start-index
						     index)))
			  (let ((n
				 (channel-write input-channel
						buffer
						0
						(string-length buffer))))
			    (if n
				(begin
				  (set! start-index (+ start-index n))
				  (if (= start-index end-index)
				      (channel-close input-channel)))))))
		      (channel-close input-channel)))))))))
      (begin
	(channel-close (subprocess-output-channel process))
	(receiver (lambda () unspecific)))))

(define system-call-name
  (condition-accessor condition-type:system-call-error 'SYSTEM-CALL))

(define system-call-error
  (condition-accessor condition-type:system-call-error 'ERROR-TYPE))

(define-command shell-command
  "Execute string COMMAND in inferior shell; display output, if any.
Optional second arg true (prefix arg, if interactive) means
insert output in current buffer after point (leave mark after it)."
  "sShell command\nP"
  (lambda (command insert-at-point?)
    (let ((directory (buffer-default-directory (current-buffer))))
      (if insert-at-point?
	  (begin
	    (if (buffer-read-only? (current-buffer))
		(barf-if-read-only))
	    (let ((point (current-point)))
	      (push-current-mark! point)
	      (shell-command false point directory false command))
	    ((ref-command exchange-point-and-mark)))
	  (shell-command-pop-up-output
	   (lambda (output-mark)
	      (shell-command false output-mark directory false command)))))))

(define-command shell-command-on-region
  "Execute string COMMAND in inferior shell with region as input.
Normally display output (if any) in temp buffer;
Prefix arg means replace the region with it."
  "r\nsShell command on region\nP"
  (lambda (region command replace-region?)
    (let ((directory (buffer-default-directory (current-buffer))))
      (if replace-region?
	  (let ((point (current-point))
		(mark (current-mark)))
	    (let ((swap? (mark< point mark))
		  (temp))
	      (dynamic-wind
	       (lambda () unspecific)
	       (lambda ()
		 (set! temp (temporary-buffer " *shell-output*"))
		 (shell-command (make-region point mark)
				(buffer-start temp)
				directory
				false
				command)
		 (without-interrupts
		  (lambda ()
		    (delete-string point mark)
		    (insert-region (buffer-start temp)
				   (buffer-end temp)
				   (current-point)))))
	       (lambda ()
		 (kill-buffer temp)
		 (set! temp)
		 unspecific))
	      (if swap? ((ref-command exchange-point-and-mark)))))
	  (shell-command-pop-up-output
	   (lambda (output-mark)
	     (shell-command region output-mark directory false command)))))))

(define (shell-command-pop-up-output generate-output)
  (let ((buffer (temporary-buffer "*Shell Command Output*")))
    (let ((start (buffer-start buffer)))
      (generate-output start)
      (set-buffer-point! buffer start)
      (if (mark< start (buffer-end buffer))
	  (pop-up-buffer buffer false)
	  (message "(Shell command completed with no output)")))))

(define (shell-command input-region output-mark directory pty? command)
  (run-synchronous-process input-region output-mark directory pty?
			   "/bin/sh" "-c" command))

;;; These procedures are not specific to the process abstraction.

(define (find-program program default-directory)
  (pathname->string
   (let ((program (->pathname program))
	 (lose (lambda () (error "Can't find program:" program))))
     (cond ((pathname-absolute? program)
	    (if (not (unix/file-access program 1)) (lose))
	    program)
	   ((not default-directory)
	    (let loop ((path (ref-variable exec-path)))
	      (if (null? path) (lose))
	      (or (and (car path)
		       (pathname-absolute? (car path))
		       (let ((pathname (merge-pathnames program (car path))))
			 (and (unix/file-access pathname 1)
			      pathname)))
		  (loop (cdr path)))))
	   (else
	    (let ((default-directory
		   (pathname->absolute-pathname default-directory)))
	      (let loop ((path (ref-variable exec-path)))
		(if (null? path) (lose))
		(let ((pathname
		       (merge-pathnames
			program
			(cond ((not (car path)) default-directory)
			      ((pathname-absolute? (car path)) (car path))
			      (else (merge-pathnames (car path)
						     default-directory))))))
		  (if (unix/file-access pathname 1)
		      pathname
		      (loop (cdr path)))))))))))

(define (parse-path-string string)
  (let ((end (string-length string))
	(substring
	 (lambda (string start end)
	   (pathname-as-directory
	    (string->pathname (substring string start end))))))
    (let loop ((start 0))
      (if (< start end)
	  (let ((index (substring-find-next-char string start end #\:)))
	    (if index
		(cons (if (= index start)
			  false
			  (substring string start index))
		      (loop (+ index 1)))
		(list (substring string start end))))
	  '()))))

(define (process-environment-bind environment . bindings)
  (let ((bindings* (vector->list environment)))
    (for-each (lambda (binding)
		(let ((b
		       (find-environment-variable
			(environment-binding-name binding)
			bindings*)))
		  (if b
		      (set-car! b binding)
		      (begin
			(set! bindings* (cons binding bindings*))
			unspecific))))
	      bindings)
    (list->vector bindings*)))

(define (environment-binding-name binding)
  (let ((index (string-find-next-char binding #\=)))
    (if (not index)
	binding
	(string-head binding index))))

(define (find-environment-variable name bindings)
  (let ((prefix (string-append name "=")))
    (let loop ((bindings bindings))
      (and (not (null? bindings))
	   (if (string-prefix? prefix (car bindings))
	       bindings
	       (loop (cdr bindings)))))))