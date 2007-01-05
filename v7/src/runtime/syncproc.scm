#| -*-Scheme-*-

$Id: syncproc.scm,v 1.12 2007/01/05 15:33:10 cph Exp $

Copyright 1999,2004 Massachusetts Institute of Technology

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

;;;; Synchronous Subprocess Support
;;; package: (runtime synchronous-subprocess)

(declare (usual-integrations))

(load-option 'SUBPROCESS)

(define-structure (subprocess-context
		   (keyword-constructor make-subprocess-context)
		   (conc-name subprocess-context/))
  ;; Where to get input data to send to the subprocess.  Either an
  ;; input port, or #F meaning that nothing is to be sent.
  (input #f read-only #t)
  ;; What size is the input buffer?
  (input-buffer-size 512 read-only #t)
  ;; Where to put output data that is received from the subprocess.
  ;; Either an output port, or #F meaning to discard any output.
  (output (current-output-port) read-only #t)
  ;; What size is the output buffer?
  (output-buffer-size 512 read-only #t)
  ;; A thunk that is periodically called while the subprocess is
  ;; running, to allow the calling program to notice output from the
  ;; subprocess and show it to the user.  Can also be #F.
  (redisplay-hook #f read-only #t)
  ;; An environment to pass to the subprocess.  Usually #F.
  (environment #f read-only #t)
  ;; A working directory for the subprocess.  #F means current working
  ;; directory.
  (working-directory #f read-only #t)
  ;; Whether to use PTYs to talk to the subprocess (if supported by
  ;; the operating system).
  (use-pty? #f read-only #t)
  ;; The name of the shell interpreter.
  (shell-file-name (os/shell-file-name) read-only #t)
  ;; How lines are terminated when talking to the subprocess.
  (line-ending #f read-only #t))

(define (run-shell-command command . options)
  (let ((context (apply make-subprocess-context options)))
    (run-synchronous-subprocess-1 (subprocess-context/shell-file-name context)
				  (os/form-shell-command command)
				  context)))

(define (run-synchronous-subprocess program arguments . options)
  (run-synchronous-subprocess-1 program arguments
				(apply make-subprocess-context options)))

(define (run-synchronous-subprocess-1 program arguments context)
  (let ((directory
	 (let ((directory (subprocess-context/working-directory context)))
	   (if directory
	       (merge-pathnames directory)
	       (working-directory-pathname))))
	(process #f))
    (bind-condition-handler '()
	(lambda (condition)
	  (if (and process (not (eq? process 'DELETED)))
	      (begin
		(subprocess-delete process)
		(set! process 'DELETED)))
	  (signal-condition condition))
      (lambda ()
	(set! process
	      ((if (and (subprocess-context/use-pty? context)
			((ucode-primitive have-ptys? 0)))
		   start-pty-subprocess
		   start-pipe-subprocess)
	       (os/find-program program directory)
	       (list->vector (cons (file-namestring program) arguments))
	       (let ((environment (subprocess-context/environment context)))
		 (if directory
		     (cons environment (->namestring directory))
		     environment))))
	(let loop ()
	  (let* ((status (synchronous-process-wait process context))
		 (reason (subprocess-exit-reason process))
		 (p process))
	    (subprocess-delete process)
	    (set! process 'DELETED)
	    (case status
	      ((EXITED)
	       reason)
	      ((SIGNALLED)
	       (error:subprocess-signalled p reason))
	      ((STOPPED)
	       (subprocess-kill p)
	       (subprocess-wait p)
	       (error:subprocess-stopped p reason))
	      ((RUNNING)
	       (loop))
	      (else
	       (error "Unknown subprocess status:" status)))))))))

(define condition-type:subprocess-abnormal-termination
  (make-condition-type 'SUBPROCESS-ABNORMAL-TERMINATION condition-type:error
      '(SUBPROCESS REASON)
    #f))

(define (abnormal-termination-type name message)
  (make-condition-type name
      condition-type:subprocess-abnormal-termination
      '()
    (lambda (condition port)
      (write-string "Subprocess " port)
      (write (access-condition condition 'SUBPROCESS) port)
      (write-string " " port)
      (write-string message port)
      (write-string " " port)
      (write (access-condition condition 'REASON) port)
      (write-string "." port))))

(define condition-type:subprocess-stopped
  (abnormal-termination-type 'SUBPROCESS-STOPPED "stopped with signal"))

(define error:subprocess-stopped
  (condition-signaller condition-type:subprocess-stopped
		       '(SUBPROCESS REASON)
		       standard-error-handler))

(define condition-type:subprocess-signalled
  (abnormal-termination-type 'SUBPROCESS-SIGNALLED "terminated with signal"))

(define error:subprocess-signalled
  (condition-signaller condition-type:subprocess-signalled
		       '(SUBPROCESS REASON)
		       standard-error-handler))

(define (synchronous-process-wait process context)
  ;; Initialize the subprocess I/O.
  (let ((port (subprocess-i/o-port process))
	(line-ending (subprocess-context/line-ending context)))
    (if line-ending
	(port/set-line-ending port line-ending)))
  (let ((redisplay-hook (subprocess-context/redisplay-hook context)))
    (call-with-input-copier process
			    (subprocess-context/input context)
			    (subprocess-context/output context)
			    (subprocess-context/input-buffer-size context)
      (lambda (copy-input)
	(call-with-output-copier process
				 (subprocess-context/output context)
				 (subprocess-context/input context)
				 (subprocess-context/output-buffer-size
				  context)
	  (lambda (copy-output)
	    (if copy-input
		(if copy-output
		    (begin
		      (if redisplay-hook (redisplay-hook))
		      (let loop ()
			(copy-input)
			(let ((n (copy-output)))
			  (cond ((not n)
				 (loop))
				((fix:> n 0)
				 (if redisplay-hook (redisplay-hook))
				 (loop))))))
		    (do ()
			((let ((n (copy-input)))
			   (and n
				(not (fix:> n 0)))))))
		(if copy-output
		    (begin
		      (if redisplay-hook (redisplay-hook))
		      (do ()
			  ((= (copy-output) 0))
			(if redisplay-hook (redisplay-hook)))))))))))
  (subprocess-wait process))

(define (call-with-input-copier process process-input nonblock? bsize receiver)
  (let ((port (subprocess-output-port process)))
    (let ((output-port/close (port/operation port 'CLOSE-OUTPUT)))
      (if process-input
	  (handle-broken-pipe process
	    (lambda ()
	      (if nonblock?
		  ((port/operation port 'SET-OUTPUT-BLOCKING-MODE)
		   port 'NONBLOCKING))
	      (receiver
	       (let ((buffer (make-wide-string bsize)))
		 (lambda ()
		   (port/with-input-blocking-mode process-input 'BLOCKING
		     (lambda ()
		       (let ((n
			      (input-port/read-wide-string! process-input
							    buffer)))
			 (if n
			     (if (fix:> n 0)
				 (output-port/write-wide-substring port
								   buffer 0 n)
				 (output-port/close port)))
			 n))))))))
	  (begin
	    (output-port/close port)
	    (receiver #f))))))

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

(define (call-with-output-copier process process-output nonblock? bsize
				 receiver)
  (let ((port (subprocess-input-port process)))
    (let ((input-port/open? (port/operation port 'INPUT-OPEN?))
	  (input-port/close (port/operation port 'CLOSE-INPUT)))
      (if process-output
	  (let ((buffer (make-wide-string bsize)))
	    (let ((copy-output
		   (lambda ()
		     (let ((n (input-port/read-wide-string! port buffer)))
		       (if (and n (fix:> n 0))
			   (port/with-output-blocking-mode process-output
							   'BLOCKING
			     (lambda ()
			       (output-port/write-wide-substring
				process-output buffer 0 n))))
		       n))))
	      (if nonblock? (port/set-input-blocking-mode port 'NONBLOCKING))
	      (let ((status (receiver copy-output)))
		(if (and nonblock? (input-port/open? port))
		    (begin
		      (port/set-input-blocking-mode port 'BLOCKING)
		      (do () ((not (fix:> (copy-output) 0))))
		      (input-port/close port)))
		status)))
	  (receiver #f)))))