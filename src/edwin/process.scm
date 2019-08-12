#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; Subprocess Support
;; package: (edwin process)

(declare (usual-integrations))

(define subprocesses-available? #t)

(add-event-receiver! editor-initializations
  (lambda ()
    (set! edwin-processes '())
    (set-variable! exec-path (os/exec-path))
    (set-variable! shell-file-name (os/shell-file-name))))

(define edwin-processes)

(define-variable exec-path
  "List of directories to search programs to run in subprocesses.
Each element is a string (directory name) or #F (try default directory)."
  '()
  (lambda (exec-path)
    (and (list? exec-path)
	 (every (lambda (element)
		  (or (not element)
		      (pathname? element)))
		exec-path))))

(define-variable process-connection-type
  "Control type of device used to communicate with subprocesses.
Values are #f to use a pipe, #t for a pty (or pipe if ptys not supported).
Value takes effect when `start-process' is called."
  #t
  boolean?)

(define-variable delete-exited-processes
  "True means delete processes immediately when they exit.
False means don't delete them until \\[list-processes] is run."
  #t
  boolean?)

(define-variable shell-file-name
  "File name to load inferior shells from.
Initialized from the SHELL environment variable."
  ""
  string?)

(define-structure (process
		   (constructor %make-process (subprocess name %buffer)))
  (subprocess #f read-only #t)
  (name #f read-only #t)
  %buffer
  (mark #f)
  (filter #f)
  (sentinel #f)
  (kill-without-query #f)
  (status-registration #f)
  (current-status #f)
  (pending-status #f))

(define-integrable (process-arguments process)
  (subprocess-arguments (process-subprocess process)))

(define-integrable (process-output-port process)
  (subprocess-output-port (process-subprocess process)))

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

(define (deregister-process-status process)
  (let ((registration (process-status-registration process)))
    (if registration
	(begin
	  (deregister-subprocess-event registration)
	  (set-process-status-registration! process #f)))))

(define (start-process name buffer environment program . arguments)
  (let ((make-subprocess
	 (let ((directory (buffer-default-directory buffer)))
	   (let ((filename
		  (os/find-program program directory (ref-variable exec-path)))
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
		 (channel-nonblocking channel)))
           (let ((fix-port
                  (lambda (port)
                    (if (and port (port/supports-coding? port))
                        (port/set-coding port 'iso-8859-1)))))
             (fix-port (subprocess-input-port subprocess))
             (fix-port (subprocess-output-port subprocess)))
	   (set-process-status-registration!
	    process
	    (register-subprocess-event
	     subprocess 'RUNNING (current-thread)
	     (named-lambda (edwin-process-status-event status)
	       (set-process-pending-status! process status))))
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
	     (%perform-status-notification process 'SIGNALLED #f)))
       (deregister-process-status process)
       (let ((buffer (process-buffer process)))
	 (if (buffer-alive? buffer)
	     (buffer-modeline-event! buffer 'PROCESS-STATUS)))
       (subprocess-delete subprocess)))))

(define (get-process-by-name name)
  (let loop ((processes edwin-processes))
    (cond ((null? processes) #f)
	  ((string=? name (process-name (car processes))) (car processes))
	  (else (loop (cdr processes))))))

(define (get-buffer-process buffer)
  (let loop ((processes edwin-processes))
    (cond ((null? processes) #f)
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

(define (process-output-available?)
  (let loop ((processes edwin-processes))
    (and (pair? processes)
	 (or (let ((port (subprocess-input-port
			  (process-subprocess (car processes)))))
	       (and port
		    (textual-port-open? port)
		    (call-with-current-continuation
		     (lambda (k)
		       (bind-condition-handler
			   (list condition-type:port-error)
			   (lambda (condition) condition (k #f))
			 (lambda ()
			   (input-port/peek-char port)))))))
	     (loop (cdr processes))))))

(define (accept-process-output)
  (let loop ((processes edwin-processes)
	     (output? #f))
    (if (pair? processes)
	(loop (cdr processes)
	      (or (poll-process-for-output (car processes))
		  output?))
	output?)))

(define input-buffer (make-string 512))

(define (poll-process-for-output process)
  (let ((port (subprocess-input-port (process-subprocess process))))
    (and (textual-port-open? port)
	 (let ((n
		(call-with-current-continuation
		 (lambda (k)
		   (bind-condition-handler (list condition-type:port-error)
		       (lambda (condition) condition (k 'ERROR))
		     (lambda ()
		       (input-port/read-string! port input-buffer)))))))
	   (if (or (eq? 'ERROR n)
		   (and (fixnum? n) (fix:= n 0)))
	       (close-port port)
	       (if (and (fixnum? n) (fix:> n 0))
		   (output-substring process input-buffer n)))
	   (and (fixnum? n)
		(fix:> n 0))))))

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
  (any (lambda (process)
	 (not (eq? (process-current-status process)
		   (process-pending-status process))))
       edwin-processes))

(define (handle-process-status-changes)
  (let loop ((processes edwin-processes) (output? #f))
    (if (pair? processes)
	(loop (cdr processes)
	      (or (let* ((process (car processes))
			 (pending (process-pending-status process)))
		    (and (not (eq? pending (process-current-status process)))
			 (begin
			   (perform-status-notification
			    process pending (process-exit-reason process))
			   #t)))
		  output?))
	output?)))

(define (register-process-output-events thread event)
  (append-map!
   (lambda (process)
     (let* ((subprocess (process-subprocess process))
	    (channel (subprocess-output-channel subprocess)))
       (if (channel-open? channel)
	   (list (register-io-thread-event
		  (channel-descriptor-for-select channel) 'READ
		  thread event))
	   '())))
   edwin-processes))

(define (perform-status-notification process status reason)
  (if (or (eq? 'EXITED status)
	  (eq? 'SIGNALLED status))
      (let drain ()
	(if (poll-process-for-output process)
	    (drain))))
  (let ((value (%perform-status-notification process status reason)))
    (if (and (or (eq? 'EXITED status)
		 (eq? 'SIGNALLED status))
	     (ref-variable delete-exited-processes))
	(delete-process process))
    value))

(define (%perform-status-notification process status reason)
  (set-process-current-status! process status)
  (cond ((process-sentinel process)
	 =>
	 (lambda (sentinel)
	   (sentinel process (status->emacs-status status) reason)
	   #t))
	((eq? status 'RUNNING)
	 #f)
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
      ((STOP) (message-with-reason "stopped by signal" #f))
      ((EXIT)
       (if (zero? reason)
	   "finished"
	   (message-with-reason "exited abnormally" "with code")))
      ((SIGNAL) (message-with-reason "terminated by signal" #f))
      (else (error "illegal process status" status)))))

(define (output-substring process string length)
  (cond ((process-filter process)
	 =>
	 (lambda (filter)
	   (filter process string 0 length)))
	((process-mark process)
	 =>
	 (lambda (mark)
	   (let ((index (mark-index mark)))
	     (group-insert-substring! (mark-group mark) index string 0 length)
	     (set-mark-index! mark (+ index length)))
	   #t))
	(else #f)))

(define (add-process-filter process filter)
  (let ((filter* (process-filter process)))
    (if (filter-dispatcher? filter*)
	(add-filter-to-dispatcher filter* filter)
	(set-process-filter! process
			     (make-filter-dispatcher (if filter*
							 (list filter* filter)
							 (list filter)))))))

(define (remove-process-filter process filter)
  (set-process-filter!
   process
   (let ((filter* (process-filter process)))
     (cond ((eq? filter filter*) #f)
	   ((filter-dispatcher? filter*)
	    (remove-filter-from-dispatcher filter* filter))
	   (else filter*)))))

(define (make-filter-dispatcher filters)
  (make-entity filter-dispatcher-procedure filters))

(define (filter-dispatcher? object)
  (and (entity? object)
       (eq? filter-dispatcher-procedure (entity-procedure object))))

(define (filter-dispatcher-procedure dispatcher process string start end)
  (let loop ((filters (entity-extra dispatcher)))
    (and (not (null? filters))
	 (or ((car filters) process string start end)
	     (loop (cdr filters))))))

(define (add-filter-to-dispatcher dispatcher filter)
  (let ((filters (entity-extra dispatcher)))
    (if (pair? filters)
	(set-cdr! (last-pair filters) (list filter))
	(set-entity-extra! dispatcher (list filter)))))

(define (remove-filter-from-dispatcher dispatcher filter)
  (let ((filters (delq! filter (entity-extra dispatcher))))
    (set-entity-extra! dispatcher filters)
    (and (not (null? filters))
	 dispatcher)))

(define (standard-process-filter filter)
  (lambda (process string start end)
    (let ((mark (process-mark process)))
      (and mark
	   (begin
	     (filter mark string start end)
	     #t)))))

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
      (pop-up-buffer buffer #f))))

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
  (let ((input-port
	 (and input-region
	      (make-buffer-input-port (region-start input-region)
				      (region-end input-region))))
	(output-port
	 (and output-mark
	      (mark->output-port
	       (if (pair? output-mark) (car output-mark) output-mark)))))
    (let ((result
	   (run-synchronous-process-1 output-port
	     (lambda ()
	       (run-synchronous-subprocess
		program arguments
		'INPUT input-port
		'OUTPUT output-port
		'REDISPLAY-HOOK
		(and (pair? output-mark)
		     (cdr output-mark)
		     (lambda () (update-screens! '(IGNORE-INPUT))))
		'WORKING-DIRECTORY directory
		'USE-PTY? pty?
		'LINE-ENDING
		(if (cond (input-region
			   (ref-variable translate-file-data-on-output
					 (region-start input-region)))
			  (output-mark
			   (ref-variable translate-file-data-on-input
					 output-mark))
			  (else #t))
		    #f
		    'NEWLINE)
		)))))
      (if input-port (close-port input-port))
      (if output-port (close-port output-port))
      result)))

(define (run-synchronous-process-1 port thunk)
  (call-with-current-continuation
   (lambda (k)
     (bind-condition-handler
	 (list condition-type:subprocess-abnormal-termination)
	 (lambda (condition)
	   (if port
	       (begin
		 (fresh-line port)
		 (newline port)
		 (write-condition-report condition port)
		 (newline port)))
	   (k
	    (cons (if (eq? condition-type:subprocess-stopped
			   (condition/type condition))
		      'STOPPED
		      'SIGNALLED)
		  (access-condition condition 'REASON))))
       (lambda ()
	 (let ((code (thunk)))
	   (if (and port (not (= 0 code)))
	       (begin
		 (fresh-line port)
		 (newline port)
		 (write-string "Subprocess exited abnormally with code " port)
		 (write code port)
		 (write-string "." port)
		 (newline port)))
	   (cons 'EXITED code)))))))

;;;; Shell Commands

(define-command shell-command
  "Execute string COMMAND in inferior shell; display output, if any.
Optional second arg true (prefix arg, if interactive) means
insert output in current buffer after point (leave mark after it)."
  (lambda ()
    (list (shell-command-prompt "Shell command")
	  (command-argument)))
  (lambda (command insert-at-point?)
    (let ((directory (buffer-default-directory (current-buffer))))
      (if insert-at-point?
	  (begin
	    (if (buffer-read-only? (current-buffer))
		(barf-if-read-only))
	    (let ((point (current-point)))
	      (push-current-mark! point)
	      (shell-command #f point directory #f command))
	    ((ref-command exchange-point-and-mark)))
	  (shell-command-pop-up-output
	   (lambda (output-mark)
	      (shell-command #f output-mark directory #f command)))))))

(define-command shell-command-on-region
  "Execute string COMMAND in inferior shell with region as input.
Normally display output (if any) in temp buffer;
Prefix arg means replace the region with it."
  (lambda ()
    (list (current-region)
	  (shell-command-prompt "Shell command on region")
	  (command-argument)))
  (lambda (region command replace-region?)
    (let ((directory (buffer-default-directory (current-buffer))))
      (if replace-region?
	  (let ((point (current-point))
		(mark (current-mark)))
	    (let ((swap? (mark< point mark))
		  (temp))
	      (dynamic-wind
	       (lambda ()
		 (set! temp (temporary-buffer " *shell-output*"))
		 unspecific)
	       (lambda ()
		 (shell-command (make-region point mark)
				(buffer-start temp)
				directory
				#f
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
	     (shell-command region output-mark directory #f command)))))))

(define (shell-command-prompt prompt)
  (prompt-for-string prompt #f
		     'DEFAULT-TYPE 'INSERTED-DEFAULT
		     'HISTORY 'SHELL-COMMAND))

(define (shell-command-pop-up-output generate-output)
  (let ((buffer (temporary-buffer "*Shell Command Output*")))
    (let ((start (buffer-start buffer)))
      (generate-output start)
      (set-buffer-point! buffer start)
      (if (mark< start (buffer-end buffer))
	  (pop-up-buffer buffer #f)
	  (message "(Shell command completed with no output)")))))

(define (shell-command input-region output-mark directory pty? command)
  (apply run-synchronous-process
	 input-region output-mark directory pty?
	 (ref-variable shell-file-name)
	 (os/form-shell-command command)))