;;; -*-Scheme-*-
;;;
;;;	$Id: process.scm,v 1.45 1996/05/14 00:13:04 cph Exp $
;;;
;;;	Copyright (c) 1991-96 Massachusetts Institute of Technology
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
;; package: (edwin process)

(declare (usual-integrations))

(define subprocesses-available? true)

(define (initialize-processes!)
  (set! edwin-processes '())
  (set! process-input-queue (cons '() '()))
  (set-variable! exec-path
		 (os/parse-path-string
		  (let ((path (get-environment-variable "PATH")))
		    (if (not path)
			(error "Can't find PATH environment variable."))
		    path)))
  (set-variable! shell-file-name (os/shell-file-name)))

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

(define-variable shell-file-name
  "File name to load inferior shells from.
Initialized from the SHELL environment variable."
  ""
  string?)

(define-structure (process
		   (constructor %make-process (subprocess name %buffer)))
  (subprocess false read-only true)
  (name false read-only true)
  %buffer
  (mark false)
  (filter false)
  (sentinel false)
  (kill-without-query false)
  (notification-tick (cons false false))
  (input-registration #f))

(define-integrable (process-arguments process)
  (subprocess-arguments (process-subprocess process)))

(define-integrable (process-output-port process)
  (subprocess-output-port (process-subprocess process)))

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

(define (deregister-process-input process)
  (let ((registration (process-input-registration process)))
    (if registration
	(begin
	  (set-process-input-registration! process #f)
	  (deregister-input-thread-event registration)))))

(define (start-process name buffer environment program . arguments)
  (let ((make-subprocess
	 (let ((directory (buffer-default-directory buffer)))
	   (let ((filename (os/find-program program directory))
		 (arguments (list->vector (cons program arguments)))
		 (pty? (ref-variable process-connection-type buffer)))
	     (lambda ()
	       (start-subprocess filename
				 arguments
				 (cons environment (->namestring directory))
				 pty?))))))
    (without-interrupts
     (lambda ()
       (let ((subprocess (make-subprocess)))
	 (let ((process
		(%make-process
		 subprocess
		 (do ((n 2 (+ n 1))
		      (name* name
			     (string-append name
					    "<" (number->string n) ">")))
		     ((not (get-process-by-name name*)) name*))
		 buffer)))
	   (let ((channel (subprocess-input-channel subprocess)))
	     (if channel
		 (begin
		   (channel-nonblocking channel)
		   (register-process-input process channel))))
	   (update-process-mark! process)
	   (subprocess-put! subprocess 'EDWIN-PROCESS process)
	   (set! edwin-processes (cons process edwin-processes))
	   (buffer-modeline-event! buffer 'PROCESS-STATUS)
	   process))))))

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
	     (%perform-status-notification process 'SIGNALLED false)))
       (deregister-process-input process)
       (let ((buffer (process-buffer process)))
	 (if (buffer-alive? buffer)
	     (buffer-modeline-event! buffer 'PROCESS-STATUS)))
       (subprocess-delete subprocess)))))

(define (get-process-by-name name)
  (let loop ((processes edwin-processes))
    (cond ((null? processes) false)
	  ((string=? name (process-name (car processes))) (car processes))
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

(define process-input-queue)

(define (register-process-input process channel)
  (set-process-input-registration!
   process
   (permanently-register-input-thread-event
    (channel-descriptor-for-select channel)
    (current-thread)
    (lambda ()
      (let ((queue process-input-queue)
	    (tail (list process)))
	(if (null? (cdr queue))
	    (set-car! queue tail)
	    (set-cdr! (cdr queue) tail))
	(set-cdr! queue tail))))))

(define (process-output-available?)
  (not (null? (car process-input-queue))))

(define (accept-process-output)
  (let ((queue process-input-queue))
    (let loop ((output? #f))
      (if (null? (car queue))
	  output?
	  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
	    (let ((process (caar queue)))
	      (set-car! queue (cdar queue))
	      (if (null? (car queue))
		  (set-cdr! queue '()))
	      (let ((output?
		     (if (poll-process-for-output process) #t output?)))
		(set-interrupt-enables! interrupt-mask)
		(loop output?))))))))

(define (poll-process-for-output process)
  (and (let ((channel (subprocess-input-channel (process-subprocess process))))
	 (and channel
	      (channel-open? channel)))
       (let ((port (subprocess-input-port (process-subprocess process)))
	     (buffer (make-string 512))
	     (output? #f))
	 (let ((read-chars (port/operation port 'READ-CHARS))
	       (close-input
		(lambda ()
		  (deregister-process-input process)
		  (close-port port)
		  (%update-global-notification-tick)
		  (if (poll-process-for-status-change process)
		      (set! output? #t)))))
	   (let loop ()
	     (if (process-runnable? process)
		 (let ((n (read-chars port buffer)))
		   (if n
		       (if (fix:= n 0)
			   (close-input)
			   (begin
			     (if (output-substring process buffer n)
				 (set! output? #t))
			     (loop)))))
		 (close-input))))
	 output?)))

(define (process-send-eof process)
  (process-send-char process #\EOT))

(define (process-send-substring process string start end)
  (let ((port (process-output-port process)))
    (output-port/write-substring port string start end)
    (output-port/flush-output port)))

(define (process-send-string process string)
  (let ((port (process-output-port process)))
    (output-port/write-string port string)
    (output-port/flush-output port)))

(define (process-send-char process char)
  (let ((port (process-output-port process)))
    (output-port/write-char port char)
    (output-port/flush-output port)))

(define (process-status-changes?)
  (without-interrupts
   (lambda ()
     (not (eq? (subprocess-global-status-tick) global-notification-tick)))))

(define (handle-process-status-changes)
  (without-interrupts
   (lambda ()
     (and (%update-global-notification-tick)
	  (let loop ((processes edwin-processes) (output? false))
	    (if (null? processes)
		output?
		(loop (cdr processes)
		      (if (poll-process-for-status-change (car processes))
			  true
			  output?))))))))

(define (%update-global-notification-tick)
  (let ((tick (subprocess-global-status-tick)))
    (and (not (eq? tick global-notification-tick))
	 (begin
	   (set! global-notification-tick tick)
	   #t))))

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
  (let ((value (%perform-status-notification process status reason)))
    (if (and (or (eq? 'EXITED status)
		 (eq? 'SIGNALLED status))
	     (ref-variable delete-exited-processes))
	(delete-process process))
    value))

(define (%perform-status-notification process status reason)
  (set-process-notification-tick! process (process-status-tick process))
  (cond ((process-sentinel process)
	 =>
	 (lambda (sentinel)
	   (sentinel process (status->emacs-status status) reason)
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
			     (string-length message))))))

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
  (let ((process false))
    (bind-condition-handler (list condition-type:abort-current-command)
	(lambda (condition)
	  (if (and process (not (eq? process 'DELETED)))
	      (begin
		(subprocess-delete process)
		(set! process 'DELETED)))
	  (signal-condition condition))
      (lambda ()
	(set! process
	      (start-subprocess
	       (os/find-program program directory)
	       (list->vector (cons (file-namestring program) arguments))
	       (if directory
		   (cons false (->namestring directory))
		   false)
	       pty?))
	(let* ((mark
		(and output-mark
		     (mark-left-inserting-copy
		      (if (pair? output-mark)
			  (car output-mark)
			  output-mark))))
	       (status
		(synchronous-process-wait process
					  input-region
					  mark
					  (if (pair? output-mark)
					      (cdr output-mark)
					      #f)))
	       (reason (subprocess-exit-reason process)))
	  (subprocess-delete process)
	  (let ((abnormal-termination
		 (lambda (message)
		   (if mark
		       (begin
			 (guarantee-newlines 2 mark)
			 (insert-string "Process " mark)
			 (insert-string message mark)
			 (insert-string " " mark)
			 (insert-string (number->string reason) mark)
			 (insert-string "." mark)
			 (insert-newline mark))))))
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
	  (if mark
	      (mark-temporary! mark))
	  (cons status reason))))))

(define (synchronous-process-wait process input-region output-mark
				  allow-redisplay?)
  ;; Initialize the subprocess line-translation appropriately.
  ;; Buffers that disable translation should have it disabled for
  ;; subprocess I/O as well as normal file I/O, since subprocesses are
  ;; used for reading and writing compressed files and such.
  (let ((mark-translation
	 (lambda (mark)
	   (let ((pathname
		  (let ((buffer (mark-buffer mark)))
		    (and buffer
			 (buffer-pathname buffer)))))
	     (if pathname
		 (pathname-newline-translation pathname)
		 'DEFAULT)))))
    (subprocess-i/o-port
     process
     (if output-mark
	 (and (ref-variable translate-file-data-on-input output-mark)
	      (mark-translation output-mark))
	 'DEFAULT)
     (if input-region
	 (let ((mark (region-start input-region)))
	   (and (ref-variable translate-file-data-on-output mark)
		(mark-translation mark)))
	 'DEFAULT)))
  (call-with-input-copier process input-region
    (lambda (copy-input)
      (call-with-output-copier process output-mark
	(lambda (copy-output)
	  (let loop ()
	    (copy-input)
	    (if (and (> (copy-output) 0) allow-redisplay?)
		(update-screens! #f))
	    (let ((status (subprocess-status process)))
	      (if (eq? status 'RUNNING)
		  (loop)
		  status))))))))

(define (call-with-input-copier process input-region receiver)
  (if input-region
      (handle-broken-pipe process
	(lambda ()
	  (let ((group (region-group input-region))
		(start-index (region-start-index input-region))
		(end-index (region-end-index input-region))
		(port (subprocess-output-port process))
		(buffer (make-string 512)))
	    (let ((output-port/set-blocking-mode
		   (port/operation port 'SET-OUTPUT-BLOCKING-MODE))
		  (output-port/write-chars (port/operation port 'WRITE-CHARS))
		  (output-port/close (port/operation port 'CLOSE-OUTPUT)))
	      (output-port/set-blocking-mode port 'NONBLOCKING)
	      (receiver
	       (lambda ()
		 (if (< start-index end-index)
		     (let ((index (min (+ start-index 512) end-index)))
		       (group-copy-substring! group start-index index
					      buffer 0)
		       (let ((n-written
			      (output-port/write-chars
			       port buffer 0 (- index start-index))))
			 (set! start-index (+ start-index n-written))
			 n-written))
		     (output-port/close port))))))))
      (receiver (lambda () 0))))

(define (handle-broken-pipe process thunk)
  (call-with-current-continuation
   (lambda (continuation)
     (bind-condition-handler (list condition-type:system-call-error)
	 (lambda (condition)
	   (if (and (eq? 'WRITE (system-call-name condition))
		    (eq? 'BROKEN-PIPE (system-call-error condition)))
	       (continuation (subprocess-wait process))))
       thunk))))

(define system-call-name
  (condition-accessor condition-type:system-call-error 'SYSTEM-CALL))

(define system-call-error
  (condition-accessor condition-type:system-call-error 'ERROR-TYPE))

(define (call-with-output-copier process output-mark receiver)
  (if output-mark
      (let ((port (subprocess-input-port process))
	    (buffer (make-string 512)))
	(let ((input-port/set-blocking-mode
	       (port/operation port 'SET-INPUT-BLOCKING-MODE))
	      (input-port/read-chars (port/operation port 'READ-CHARS))
	      (input-port/open? (port/operation port 'INPUT-OPEN?))
	      (input-port/close (port/operation port 'CLOSE-INPUT)))
	  (let ((copy-output
		 (lambda ()
		   (let ((n (input-port/read-chars port buffer)))
		     (if n
			 (begin
			   (if (> n 0)
			       (insert-substring buffer 0 n output-mark))
			   n)
			 0)))))
	    (input-port/set-blocking-mode port 'NONBLOCKING)
	    (let ((status (receiver copy-output)))
	      (if (input-port/open? port)
		  (begin
		    (input-port/set-blocking-mode port 'BLOCKING)
		    (do () ((= (copy-output) 0)))
		    (input-port/close port)))
	      status))))
      (receiver (lambda () 0))))

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
	      (unwind-protect
	       (lambda ()
		 (set! temp (temporary-buffer " *shell-output*"))
		 unspecific)
	       (lambda ()
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
  (apply run-synchronous-process
	 input-region output-mark directory pty?
	 (ref-variable shell-file-name)
	 (os/form-shell-command command)))

;;; These procedures are not specific to the process abstraction.

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