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

;;;; Synchronous Subprocess Support
;;; package: (runtime synchronous-subprocess)

(declare (usual-integrations))

(load-option 'subprocess)

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
    (dynamic-wind
     (lambda ()
       (set! process (start-subprocess program arguments directory context)))
     (lambda ()
       (let loop ()
	 (receive (status reason) (synchronous-subprocess-wait process context)
	   (case status
	     ((exited) reason)
	     ((signalled) (error:subprocess-signalled process reason))
	     ;++ Give a restart to continue the process and loop?
	     ((stopped) (error:subprocess-stopped process reason))
	     (else
	      (error "Invalid synchronous subprocess status:" status))))))
     (lambda ()
       (if (and process
		;++ Need a predicate SUBPROCESS-LIVE? or something.
		(not (memq (subprocess-status process) '(exited signalled))))
	   (ignore-errors (lambda () (subprocess-kill process))))))))

(define (start-subprocess program arguments directory context)
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

(define condition-type:subprocess-abnormal-termination
  (make-condition-type 'subprocess-abnormal-termination condition-type:error
      '(subprocess reason)
    #f))

(define (abnormal-termination-type name message)
  (make-condition-type name
      condition-type:subprocess-abnormal-termination
      '()
    (lambda (condition port)
      (write-string "Subprocess " port)
      (write (access-condition condition 'subprocess) port)
      (write-string " " port)
      (write-string message port)
      (write-string " " port)
      (write (access-condition condition 'reason) port)
      (write-string "." port))))

(define condition-type:subprocess-stopped
  (abnormal-termination-type 'subprocess-stopped "stopped with signal"))

(define error:subprocess-stopped
  (condition-signaller condition-type:subprocess-stopped
		       '(subprocess reason)
		       standard-error-handler))

(define condition-type:subprocess-signalled
  (abnormal-termination-type 'subprocess-signalled "terminated with signal"))

(define error:subprocess-signalled
  (condition-signaller condition-type:subprocess-signalled
		       '(subprocess reason)
		       standard-error-handler))

(define (synchronous-subprocess-wait process context)
  (setup-synchronous-io process context)
  (let ((input (subprocess-context/input context))
	(output (subprocess-context/output context)))
    (let ((copy-input (input-copier input process context))
	  (copy-output (output-copier output process context))
	  (thread (current-thread))
	  (input-descriptor (and output (input-channel-descriptor process)))
	  (output-descriptor (and input (output-channel-descriptor process)))
	  ;; These can be any of reading, writing, blocked-read,
	  ;; blocked-write or closed.
	  (input-state 'reading)
	  (output-state 'reading))
      (let wait ()
	(let ((event? #f)
	      (input-registration)
	      (output-registration)
	      (status-registration))

	  (define (event! mode/status)
	    mode/status
	    (set! event? #t))

	  (let copy ()
	    (set! input-state (copy-input))
	    (set! output-state (copy-output))
	    (let ((oport (subprocess-output-port process)))
	      (if (and (eq? input-state 'end)
		       (output-port-open? oport))
		  (close-output-port oport)))
	    (if (or (memq input-state '(reading writing))
		    (memq output-state '(reading writing)))
		(copy)))

	  (dynamic-wind
	   (lambda ()
	     (set! input-registration
		   (and (eq? output-state 'blocked-read)
			(register-io-thread-event
			 input-descriptor 'read thread event!)))
	     (set! output-registration
		   (and (eq? input-state 'blocked-write)
			(register-io-thread-event
			 output-descriptor 'write thread event!)))
	     (set! status-registration
		   (register-subprocess-event process 'running
					      thread event!)))
	   (lambda ()
	     (with-thread-events-blocked
	      (lambda ()
		(if (not event?)
		    (suspend-current-thread)))))
	   (lambda ()
	     (if (eq? (current-thread) thread)
		 (begin
		   (if input-registration
		       (deregister-io-thread-event input-registration))
		   (if output-registration
		       (deregister-io-thread-event output-registration))
		   (deregister-subprocess-event status-registration))))))

	(if (eq? 'running (subprocess-status process))
	    (wait)
	    (let ((in (and output (subprocess-input-port process))))
	      (if (and in (input-port-open? in))
		  (begin
		    (set-input-port-blocking-mode! in 'blocking)
		    (let drain ()
		      (set! output-state (copy-output))
		      (cond ((eq? output-state 'reading)
			     (drain))
			    ((not (eq? output-state 'end))
			     (error "could not drain subprocess output"))))))
	      (subprocess-delete process)
	      (values (subprocess-status process)
		      (subprocess-exit-reason process))))))))

(define (setup-synchronous-io process context)
  (let ((line-ending (subprocess-context/line-ending context))
	(input (subprocess-context/input context))
	(output (subprocess-context/output context)))
    (if line-ending
	(port/set-line-ending (subprocess-i/o-port process) line-ending))
    (set-input-port-blocking-mode! (subprocess-input-port process)
				   'nonblocking)
    (set-output-port-blocking-mode! (subprocess-output-port process)
				    'nonblocking)
    (if (and input (eq? (input-port-blocking-mode input) 'nonblocking))
	(error "subprocess input is non-blocking:" input))
    (if (and output (eq? (output-port-blocking-mode output) 'nonblocking))
	(error "subprocess output is non-blocking:" output))
    (if (not input)
	(let ((port (subprocess-output-port process)))
	  (if (output-port-open? port)
	      (close-output-port port))))))

(define (input-channel-descriptor process)
  (channel-descriptor-for-select (subprocess-input-channel process)))

(define (output-channel-descriptor process)
  (channel-descriptor-for-select (subprocess-output-channel process)))

(define (input-copier input process context)
  (if input
      (copier input (subprocess-output-port process) #t
	      (subprocess-context/input-buffer-size context)
	      #f)
      (named-lambda (null-input-copier)
	'end)))

(define (output-copier output process context)
  (copier (subprocess-input-port process) output #f
	  (subprocess-context/output-buffer-size context)
	  (subprocess-context/redisplay-hook context)))

(define (copier in out flush? buffer-size hook)
  (let ((buffer (make-string buffer-size))
	(start 0)
	(end 0))
    (named-lambda (synchronous-copy)

      (define (fill)
	(assert (and (fix:= start 0) (fix:= end 0)))
	(if (and out (input-port-open? in))
	    (let ((n (read-string! buffer in)))
	      (cond ((not n)
		     'blocked-read)
		    ((fix:= n 0)
		     (set! end -1)
		     'end)
		    ((fix:> n 0)
		     (set! end n)
		     'reading)
		    (else
		     (error "bogus read:" n))))
	    'closed))

      (if (fix:= -1 end)
	  'end
	  (if (fix:< start end)
	      ;; Flush buffer.
	      (if (not out)
		  (begin		;discard
		    (set! start 0)
		    (set! end 0)
		    (fill))
		  (if (output-port-open? out)
		      (let ((n (output-port/write-substring
				out buffer start end)))
			(if flush?
			    (flush-output-port out))
			(cond ((not n)
			       'blocked-write)
			      ((fix:> n 0)
			       (if hook (hook))
			       (let ((start* (fix:+ start n)))
				 (if (fix:= start* end)
				     (begin
				       (set! start 0)
				       (set! end 0)
				       (fill))
				     (begin
				       (set! start start*)
				       'writing))))
			      (else
			       (error "bogus write:" n))))
		      'closed))
	      (fill))))))