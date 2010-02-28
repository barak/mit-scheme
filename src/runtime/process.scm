#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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
(define global-status-tick)

(define (initialize-package!)
  (set! subprocess-finalizer
	(make-gc-finalizer (ucode-primitive process-delete 1)
			   subprocess?
			   subprocess-index
			   set-subprocess-index!))
  (reset-package!)
  (add-event-receiver! event:after-restore reset-package!)
  (add-event-receiver! event:before-exit delete-all-processes))

(define (reset-package!)
  (set! scheme-subprocess-environment ((ucode-primitive scheme-environment 0)))
  (set! global-status-tick (cons #f #f))
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
  (%status #f)
  (exit-reason #f)
  (%status-tick #f)
  (properties (make-1d-table) read-only #t))

(define (subprocess-get process key)
  (1d-table/get (subprocess-properties process) key #f))

(define (subprocess-put! process key datum)
  (1d-table/put! (subprocess-properties process) key datum))

(define (subprocess-remove! process key)
  (1d-table/remove! (subprocess-properties process) key))

(define (subprocess-i/o-port process)
  (without-interrupts
   (lambda ()
     (or (subprocess-%i/o-port process)
	 (let ((port
		(let ((input-channel (subprocess-input-channel process))
		      (output-channel (subprocess-output-channel process)))
		  (and (or input-channel output-channel)
		       (make-generic-i/o-port input-channel output-channel)))))
	   (set-subprocess-%i/o-port! process port)
	   port)))))

(define (subprocess-input-port process)
  (let ((port (subprocess-i/o-port process)))
    (and (input-port? port)
	 port)))

(define (subprocess-output-port process)
  (let ((port (subprocess-i/o-port process)))
    (and (output-port? port)
	 port)))

(define (close-subprocess-i/o process)
  (without-interrupts (lambda () (%close-subprocess-i/o process))))

(define (%close-subprocess-i/o process)
  ;; Assumes that interrupts are locked.
  (cond ((subprocess-%i/o-port process)
	 => (lambda (port)
	      (set-subprocess-%i/o-port! process #f)
	      (set-subprocess-input-channel! process #f)
	      (set-subprocess-output-channel! process #f)
	      (close-port port))))
  (cond ((subprocess-input-channel process)
	 => (lambda (input-channel)
	      (set-subprocess-input-channel! process #f)
	      (channel-close input-channel))))
  (cond ((subprocess-output-channel process)
	 => (lambda (output-channel)
	      (set-subprocess-output-channel! process #f)
	      (channel-close output-channel))))
  (cond ((subprocess-pty-master process)
	 => (lambda (pty-master)
	      (set-subprocess-pty-master! process #f)
	      (channel-close pty-master)))))

(define (make-subprocess filename arguments environment
			 ctty stdin stdout stderr
			 pty-master input-channel output-channel)
  (let ((process
	 (let ((ctty-allowed? (string? ctty)))
	   (define-integrable (convert-stdio-arg stdio)
	     (cond ((not stdio) #f)
		   ((eq? stdio 'INHERIT) -1)
		   ((and ctty-allowed? (eq? stdio 'CTTY)) -2)
		   ((channel? stdio) (channel-descriptor stdio))
		   (else
		    (error:wrong-type-argument stdio "process I/O channel"
					       'MAKE-SUBPROCESS))))
	   (let ((working-directory #f)
		 (ctty
		  (cond ((eq? ctty 'BACKGROUND) -1)
			((eq? ctty 'FOREGROUND) -2)
			((or (not ctty) (string? ctty)) ctty)
			(else
			 (error:wrong-type-argument
			  ctty
			  "process controlling terminal"
			  'MAKE-SUBPROCESS))))
		 (stdin (convert-stdio-arg stdin))
		 (stdout (convert-stdio-arg stdout))
		 (stderr (convert-stdio-arg stderr)))
	     (if (pair? environment)
		 (begin
		   (set! working-directory
			 (and (cdr environment)
			      (->namestring (cdr environment))))
		   (set! environment (car environment))))
	     (without-interrupts
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
		    (set-subprocess-%status!
		     process
		     ((ucode-primitive process-status 1) index))
		    (set-subprocess-exit-reason!
		     process
		     ((ucode-primitive process-reason 1) index))
		    (add-to-gc-finalizer! subprocess-finalizer process)))))))))
    (if (and (eq? ctty 'FOREGROUND)
	     (eqv? (%subprocess-status process) 0))
	(subprocess-continue-foreground process))
    process))

(define (subprocess-delete process)
  (without-interrupts
   (lambda ()
     (if (subprocess-index process)
	 (begin
	   (remove-from-gc-finalizer! subprocess-finalizer process)
	   (%close-subprocess-i/o process))))))

(define (subprocess-status process)
  (convert-subprocess-status (%subprocess-status process)))

(define (subprocess-wait process)
  (let loop ()
    ((ucode-primitive process-wait 1) (subprocess-index process))
    (let ((status (%subprocess-status process)))
      (if (eqv? status 0)
	  (loop)
	  (convert-subprocess-status status)))))

(define (subprocess-continue-foreground process)
  (let loop ()
    ((ucode-primitive process-continue-foreground 1)
     (subprocess-index process))
    (let ((status (%subprocess-status process)))
      (if (eqv? status 0)
	  (loop)
	  (convert-subprocess-status status)))))

(define (%subprocess-status process)
  (without-interrupts
   (lambda ()
     (let ((index (subprocess-index process)))
       (if (and index ((ucode-primitive process-status-sync 1) index))
	   (begin
	     (set-subprocess-%status!
	      process
	      ((ucode-primitive process-status 1) index))
	     (set-subprocess-exit-reason!
	      process
	      ((ucode-primitive process-reason 1) index))
	     (set-subprocess-%status-tick! process #f))))))
  (subprocess-%status process))

(define (subprocess-status-tick process)
  (or (subprocess-%status-tick process)
      (let ((tick (cons #f #f)))
	(set-subprocess-%status-tick! process tick)
	tick)))

(define (subprocess-global-status-tick)
  (without-interrupts
   (lambda ()
     (if ((ucode-primitive process-status-sync-all 0))
	 (let ((tick (cons #f #f)))
	   (set! global-status-tick tick)
	   tick)
	 global-status-tick))))

(define (convert-subprocess-status status)
  (case status
    ((0) 'RUNNING)
    ((1) 'STOPPED)
    ((2) 'EXITED)
    ((3) 'SIGNALLED)
    (else (error "Illegal process status:" status))))

(define (subprocess-job-control-status process)
  (let ((n
	 ((ucode-primitive process-job-control-status 1)
	  (subprocess-index process))))
    (case n
      ((0) 'NO-CTTY)
      ((1) 'UNRELATED-CTTY)
      ((2) 'NO-JOB-CONTROL)
      ((3) 'JOB-CONTROL)
      (else (error "Illegal process job-control status:" n)))))

(define (handle-subprocess-status-change)
  (if (eq? 'NT microcode-id/operating-system)
      (for-each (lambda (process)
		  (if (memq (subprocess-status process) '(EXITED SIGNALLED))
		      (close-subprocess-i/o process)))
		(subprocess-list))))

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
  (if (eq? 'NT microcode-id/operating-system)
      (close-subprocess-i/o process)))

(define (subprocess-stop process)
  ((ucode-primitive process-stop 1) (subprocess-index process)))

(define (start-batch-subprocess filename arguments environment)
  (make-subprocess filename arguments environment
		   #f #f #f #f
		   #f #f #f))

(define (start-subprocess-in-background filename arguments environment)
  (make-subprocess filename arguments environment
		   'BACKGROUND 'INHERIT 'INHERIT 'INHERIT
		   #f #f #f))

(define (run-subprocess-in-foreground filename arguments environment)
  (make-subprocess filename arguments environment
		   'FOREGROUND 'INHERIT 'INHERIT 'INHERIT
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
		       slave-name 'CTTY 'CTTY 'CTTY
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