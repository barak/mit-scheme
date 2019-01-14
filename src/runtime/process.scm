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
;;; package: (runtime subprocess)

(declare (usual-integrations))

(define subprocess-finalizer)
(define scheme-subprocess-environment)

(define (initialize-package!)
  (set! subprocess-finalizer
	(make-gc-finalizer (ucode-primitive process-delete 1)
			   subprocess?
			   subprocess-index
			   set-subprocess-index!))
  (set! subprocess-support-loaded? #t)
  (reset-package!)
  (add-event-receiver! event:after-restore reset-package!)
  (add-event-receiver! event:before-exit delete-all-processes))

(define (reset-package!)
  (set! scheme-subprocess-environment ((ucode-primitive scheme-environment 0)))
  unspecific)

(define (delete-all-processes)
  (for-each subprocess-delete (subprocess-list)))

(define (subprocess-list)
  (gc-finalizer-elements subprocess-finalizer))

(define-structure (subprocess
		   (constructor %make-subprocess
				(filename arguments index pty-master
					  input-channel output-channel id))
		   (conc-name subprocess-))
  (filename #f read-only #t)
  (arguments #f read-only #t)
  index
  pty-master
  input-channel
  output-channel
  (id #f read-only #t)
  (%i/o-port #f)
  (status #f)
  (exit-reason #f)
  (properties (make-1d-table) read-only #t))

(define (subprocess-get process key)
  (1d-table/get (subprocess-properties process) key #f))

(define (subprocess-put! process key datum)
  (1d-table/put! (subprocess-properties process) key datum))

(define (subprocess-remove! process key)
  (1d-table/remove! (subprocess-properties process) key))

(define (subprocess-i/o-port process)
  (%subprocess-i/o-port process 'subprocess-i/o-port))

(define (subprocess-input-port process)
  (let ((port (%subprocess-i/o-port process 'subprocess-input-port)))
    (and (input-port? port)
	 port)))

(define (subprocess-output-port process)
  (let ((port (%subprocess-i/o-port process 'subprocess-output-port)))
    (and (output-port? port)
	 port)))

(define (%subprocess-i/o-port process caller)
  (without-interruption
   (lambda ()
     (or (subprocess-%i/o-port process)
	 (let ((port
		(let ((input-channel (subprocess-input-channel process))
		      (output-channel (subprocess-output-channel process)))
		  (and (or input-channel output-channel)
		       (make-generic-i/o-port
			(make-binary-port
			 (and input-channel
			      (make-channel-input-source input-channel))
			 (and output-channel
			      (make-channel-output-sink output-channel))
			 caller)
			(default-object)
			caller)))))
	   (set-subprocess-%i/o-port! process port)
	   port)))))

(define (close-subprocess-i/o process)
  (cond ((subprocess-%i/o-port process)
	 => (lambda (port)
	      (close-port port)
	      (set-subprocess-%i/o-port! process #f)
	      (set-subprocess-input-channel! process #f)
	      (set-subprocess-output-channel! process #f))))
  (cond ((subprocess-input-channel process)
	 => (lambda (input-channel)
	      (channel-close input-channel)
	      (set-subprocess-input-channel! process #f))))
  (cond ((subprocess-output-channel process)
	 => (lambda (output-channel)
	      (channel-close output-channel)
	      (set-subprocess-output-channel! process #f))))
  (cond ((subprocess-pty-master process)
	 => (lambda (pty-master)
	      (channel-close pty-master)
	      (set-subprocess-pty-master! process #f)))))

(define (make-subprocess filename arguments environment
			 ctty stdin stdout stderr
			 pty-master input-channel output-channel)
  (let ((process
	 (let ((ctty-allowed? (string? ctty)))
	   (define-integrable (convert-stdio-arg stdio)
	     (cond ((not stdio) #f)
		   ((eq? stdio 'inherit) -1)
		   ((and ctty-allowed? (eq? stdio 'ctty)) -2)
		   ((channel? stdio) (channel-descriptor stdio))
		   (else
		    (error:wrong-type-argument stdio "process I/O channel"
					       'make-subprocess))))
	   (let ((working-directory #f)
		 (ctty
		  (cond ((eq? ctty 'background) -1)
			((eq? ctty 'foreground) -2)
			((or (not ctty) (string? ctty)) ctty)
			(else
			 (error:wrong-type-argument
			  ctty
			  "process controlling terminal"
			  'make-subprocess))))
		 (stdin (convert-stdio-arg stdin))
		 (stdout (convert-stdio-arg stdout))
		 (stderr (convert-stdio-arg stderr)))
	     (if (pair? environment)
		 (begin
		   (set! working-directory
			 (and (cdr environment)
			      (->namestring (cdr environment))))
		   (set! environment (car environment))))
	     (without-interruption
	      (lambda ()
		(let ((index
		       (os/make-subprocess filename arguments environment
					   working-directory ctty
					   stdin stdout stderr)))
		  (let ((process
			 (%make-subprocess
			  filename arguments index pty-master
			  input-channel output-channel
			  ((ucode-primitive process-id 1) index))))
		    (set-subprocess-status!
		     process
		     (convert-subprocess-status
		      ((ucode-primitive process-status 1) index)))
		    (set-subprocess-exit-reason!
		     process
		     ((ucode-primitive process-reason 1) index))
		    (add-to-gc-finalizer! subprocess-finalizer process)
		    (poll-subprocess-status process)
		    process))))))))
    (if (and (eq? ctty 'foreground)
	     (eq? (subprocess-status process) 'running))
	(subprocess-continue-foreground process))
    process))

(define (subprocess-delete process)
  (if (subprocess-index process)
      (begin
	(poll-subprocess-status process)
	(close-subprocess-i/o process)
	(deregister-subprocess process)
	(remove-from-gc-finalizer! subprocess-finalizer process))))

(define (subprocess-wait process)
  (let ((result #f)
	(registration))
    (dynamic-wind
     (lambda ()
       (set! registration
	     (register-subprocess-event
	      process 'running (current-thread)
	      (named-lambda (subprocess-wait-event status)
		(set! result status)))))
     (lambda ()
       (let loop ()
	 (with-thread-events-blocked
	  (lambda ()
	    (if (eq? result '#f)
		(suspend-current-thread))
	    (if (eq? result 'running)
		(set! result #f))))
	 (if (not result)
	     (loop)
	     result)))
     (lambda ()
       (deregister-subprocess-event registration)))))

(define (subprocess-continue-foreground process)
  (let loop ()
    ((ucode-primitive process-continue-foreground 1)
     (subprocess-index process))
    (let ((status (subprocess-status process)))
      (if (eq? status 'running)
	  (loop)
	  status))))

(define (poll-subprocess-status process)
  (let ((index (subprocess-index process)))
    (if (and index ((ucode-primitive process-status-sync 1) index))
	(begin
	  (set-subprocess-status!
	   process
	   (convert-subprocess-status
	    ((ucode-primitive process-status 1) index)))
	  (set-subprocess-exit-reason!
	   process
	   ((ucode-primitive process-reason 1) index))))))

(define (convert-subprocess-status status)
  (case status
    ((0) 'running)
    ((1) 'stopped)
    ((2) 'exited)
    ((3) 'signalled)
    (else (error "Illegal process status:" status))))

(define (subprocess-job-control-status process)
  (let ((n
	 ((ucode-primitive process-job-control-status 1)
	  (subprocess-index process))))
    (case n
      ((0) 'no-ctty)
      ((1) 'unrelated-ctty)
      ((2) 'no-job-control)
      ((3) 'job-control)
      (else (error "Illegal process job-control status:" n)))))

;;;; Subprocess Events

(define-structure (subprocess-registration
		   (conc-name subprocess-registration/))
  (subprocess #f read-only #t)
  (status #f)
  (thread () read-only #t)
  (event () read-only #t))

(define (guarantee-subprocess-registration object procedure)
  (if (not (subprocess-registration? object))
      (error:wrong-type-argument object "subprocess-registration" procedure)))

(define (guarantee-subprocess object procedure)
  (if (not (subprocess? object))
      (error:wrong-type-argument object "subprocess" procedure)))

(define (register-subprocess-event subprocess status thread event)
  (guarantee-subprocess subprocess 'register-subprocess-event)
  (guarantee thread? thread 'register-subprocess-event)
  (guarantee unary-procedure? event 'register-subprocess-event)
  (let ((registration (make-subprocess-registration
		       subprocess status thread event)))
    (without-interrupts
     (lambda ()
       (set! subprocess-registrations
	     (cons registration subprocess-registrations))
       (let ((current (subprocess-status subprocess)))
	 (if (not (eq? status current))
	     (begin
	       (%signal-thread-event
		thread (and event
			    (named-lambda (immediate-subprocess-status-event)
			      (event current))))
	       (%maybe-toggle-thread-timer)
	       (set-subprocess-registration/status! registration current))))))
    registration))

(define (deregister-subprocess-event registration)
  (guarantee-subprocess-registration registration
				     'deregister-subprocess-event)
  (without-interrupts
   (lambda ()
     (set! subprocess-registrations
	   (delq! registration subprocess-registrations)))))

(define (deregister-subprocess subprocess)
  (without-interrupts
   (lambda ()
     (set! subprocess-registrations
	   (filter!
	    (lambda (registration)
	      (not (eq? subprocess
			(subprocess-registration/subprocess registration))))
		    subprocess-registrations)))))

(define (deregister-subprocess-events thread)
  (set! subprocess-registrations
	(filter!
	 (lambda (registration)
	   (not (eq? thread (subprocess-registration/thread registration))))
	 subprocess-registrations)))

(define (handle-subprocess-status-change)
  (without-interrupts %handle-subprocess-status-change)
  (if (eq? 'nt microcode-id/operating-system)
      (for-each (lambda (process)
		  (if (memq (subprocess-status process) '(exited signalled))
		      (close-subprocess-i/o process)))
		(subprocess-list))))

(define (%handle-subprocess-status-change)
  (if ((ucode-primitive process-status-sync-all 0))
      (let ((signaled? #f))
	(for-each (lambda (weak)
		    (let ((subprocess (weak-car weak)))
		      (if subprocess
			  (poll-subprocess-status subprocess))))
		  (gc-finalizer-items subprocess-finalizer))
	(for-each
	  (lambda (registration)
	    (let ((status (subprocess-status
			   (subprocess-registration/subprocess registration)))
		  (old (subprocess-registration/status registration)))
	      (if (not (eq? status old))
		  (let ((event (subprocess-registration/event registration)))
		    (%signal-thread-event
		     (subprocess-registration/thread registration)
		     (and event
			  (named-lambda (subprocess-status-event)
			    (event status))))
		    (set! signaled? #t)
		    (set-subprocess-registration/status! registration
							 status)))))
	  subprocess-registrations)
	(set! subprocess-registrations
	      (filter! (lambda (registration)
			 (let ((status
				(subprocess-registration/status registration)))
			   (not (or (eq? status 'exited)
				    (eq? status 'signalled)))))
		       subprocess-registrations))
	(if signaled? (%maybe-toggle-thread-timer)))))

(define-integrable subprocess-job-control-available?
  (ucode-primitive os-job-control? 0))

(define (subprocess-continue-background process)
  ((ucode-primitive process-continue-background 1) (subprocess-index process)))

(define (subprocess-signal process signal)
  ((ucode-primitive process-signal 2) (subprocess-index process) signal))

(define (subprocess-kill process)
  ((ucode-primitive process-kill 1) (subprocess-index process))
  (maybe-close-subprocess-i/o process))

(define (subprocess-interrupt process)
  ((ucode-primitive process-interrupt 1) (subprocess-index process)))

(define (subprocess-quit process)
  ((ucode-primitive process-quit 1) (subprocess-index process)))

(define (subprocess-hangup process)
  ((ucode-primitive process-hangup 1) (subprocess-index process))
  (maybe-close-subprocess-i/o process))

(define (maybe-close-subprocess-i/o process)
  (if (eq? 'nt microcode-id/operating-system)
      (close-subprocess-i/o process)))

(define (subprocess-stop process)
  ((ucode-primitive process-stop 1) (subprocess-index process)))

(define (start-batch-subprocess filename arguments environment)
  (make-subprocess filename arguments environment
		   #f #f #f #f
		   #f #f #f))

(define (start-subprocess-in-background filename arguments environment)
  (make-subprocess filename arguments environment
		   'background 'inherit 'inherit 'inherit
		   #f #f #f))

(define (run-subprocess-in-foreground filename arguments environment)
  (make-subprocess filename arguments environment
		   'foreground 'inherit 'inherit 'inherit
		   #f #f #f))

(define (start-pipe-subprocess filename arguments environment)
  (with-values make-pipe
    (lambda (child-read parent-write)
      (with-values make-pipe
	(lambda (parent-read child-write)
	  (let ((process
		 (make-subprocess filename arguments environment
				  #f child-read child-write child-write
				  #f parent-read parent-write)))
	    (channel-close child-read)
	    (channel-close child-write)
	    process))))))

(define (start-pty-subprocess filename arguments environment)
  (with-values open-pty-master
    (lambda (master-channel master-name slave-name)
      master-name
      (make-subprocess filename arguments environment
		       slave-name 'ctty 'ctty 'ctty
		       master-channel master-channel master-channel))))

;;;; Environment Bindings

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