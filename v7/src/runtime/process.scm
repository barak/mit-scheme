#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/process.scm,v 1.12 1991/03/14 10:14:24 cph Exp $

Copyright (c) 1989-91 Massachusetts Institute of Technology

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

;;;; Subprocess Support
;;; package: (runtime subprocess)

(declare (usual-integrations))

(define subprocesses)
(define scheme-subprocess-environment)
(define global-status-tick)

(define (initialize-package!)
  (reset-package!)
  (add-event-receiver! event:after-restore reset-package!)
  (add-event-receiver! event:before-exit delete-all-processes))

(define (reset-package!)
  (set! subprocesses '())
  (set! scheme-subprocess-environment ((ucode-primitive scheme-environment 0)))
  (set! global-status-tick (cons false false))
  unspecific)

(define (delete-all-processes)
  (for-each subprocess-delete subprocesses))

(define (subprocess-list)
  (list-copy subprocesses))

(define-structure (subprocess
		   (constructor %make-subprocess
				(filename arguments index pty-master
					  input-channel output-channel))
		   (conc-name subprocess-))
  (filename false read-only true)
  (arguments false read-only true)
  index
  pty-master
  input-channel
  output-channel
  (id ((ucode-primitive process-id 1) index) read-only true)
  (%input-port false)
  (%output-port false)
  (%status false)
  (exit-reason false)
  (%status-tick false)
  (properties (make-1d-table) read-only true))

(define (subprocess-get process key)
  (1d-table/get (subprocess-properties process) key false))

(define (subprocess-put! process key datum)
  (1d-table/put! (subprocess-properties process) key datum))

(define (subprocess-remove! process key)
  (1d-table/remove! (subprocess-properties process) key))

(define (subprocess-input-port process)
  (without-interrupts
   (lambda ()
     (or (subprocess-%input-port process)
	 (let ((channel (subprocess-input-channel process)))
	   (and channel
		(let ((input-port (make-generic-input-port channel 512))
		      (output-port (subprocess-%output-port process)))
		  (set-subprocess-%input-port! process input-port)
		  (if output-port
		      (set-input-port/associated-port! input-port output-port))
		  input-port)))))))

(define (subprocess-output-port process)
  (without-interrupts
   (lambda ()
     (or (subprocess-%output-port process)
	 (let ((channel (subprocess-output-channel process)))
	   (and channel
		(let ((output-port (make-generic-output-port channel 512))
		      (input-port (subprocess-%input-port process)))
		  (set-subprocess-%output-port! process output-port)
		  (if input-port
		      (set-output-port/associated-port! output-port
							input-port))
		  output-port)))))))

(define (make-subprocess filename arguments environment
			 ctty stdin stdout stderr
			 pty-master input-channel output-channel)
  (let ((process
	 (let ((ctty-allowed? (string? ctty)))
	   (define-integrable (convert-stdio-arg stdio)
	     (cond ((not stdio) false)
		   ((eq? stdio 'INHERIT) -1)
		   ((and ctty-allowed? (eq? stdio 'CTTY)) -2)
		   ((channel? stdio) (channel-descriptor stdio))
		   (else
		    (error:wrong-type-argument stdio "process I/O channel"
					       'MAKE-SUBPROCESS))))
	   (let ((ctty
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
	     (without-interrupts
	      (lambda ()
		(let ((index
		       ((ucode-primitive make-subprocess 7)
			filename arguments environment
			ctty stdin stdout stderr)))
		  (let ((process
			 (%make-subprocess filename arguments index pty-master
					   input-channel output-channel)))
		    (set-subprocess-%status!
		     process
		     ((ucode-primitive process-status 1) index))
		    (set-subprocess-exit-reason!
		     process
		     ((ucode-primitive process-reason 1) index))
		    (set! subprocesses (cons process subprocesses))
		    process))))))))
    (if (and (eq? ctty 'FOREGROUND)
	     (eqv? (%subprocess-status process) 0))
	(subprocess-continue-foreground process))
    process))

(define (subprocess-delete process)
  (without-interrupts
   (lambda ()
     (if (subprocess-index process)
	 (begin
	   ((ucode-primitive process-delete 1) (subprocess-index process))
	   (set! subprocesses (delq! process subprocesses))
	   (set-subprocess-index! process false)
	   (cond ((subprocess-input-port process)
		  => (lambda (input-port)
		       (set-subprocess-%input-port! process false)
		       (set-subprocess-input-channel! process false)
		       (close-input-port input-port)))
		 ((subprocess-input-channel process)
		  => (lambda (input-channel)
		       (set-subprocess-input-channel! process false)
		       (channel-close input-channel))))
	   (cond ((subprocess-output-port process)
		  => (lambda (output-port)
		       (set-subprocess-%output-port! process false)
		       (set-subprocess-output-channel! process false)
		       (close-output-port output-port)))
		 ((subprocess-output-channel process)
		  => (lambda (output-channel)
		       (set-subprocess-output-channel! process false)
		       (channel-close output-channel))))
	   (cond ((subprocess-pty-master process)
		  => (lambda (pty-master)
		       (set-subprocess-pty-master! process false)
		       (channel-close pty-master)))))))))

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
       (if ((ucode-primitive process-status-sync 1) index)
	   (begin
	     (set-subprocess-%status!
	      process
	      ((ucode-primitive process-status 1) index))
	     (set-subprocess-exit-reason!
	      process
	      ((ucode-primitive process-reason 1) index))
	     (set-subprocess-%status-tick! process false))))))
  (subprocess-%status process))

(define (subprocess-status-tick process)
  (or (subprocess-%status-tick process)
      (let ((tick (cons false false)))
	(set-subprocess-%status-tick! process tick)
	tick)))

(define (subprocess-global-status-tick)
  (without-interrupts
   (lambda ()
     (if ((ucode-primitive process-status-sync-all 0))
	 (let ((tick (cons false false)))
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

(define-integrable subprocess-job-control-available?
  (ucode-primitive os-job-control? 0))

(define (subprocess-continue-background process)
  ((ucode-primitive process-continue-background 1) (subprocess-index process)))

(define (subprocess-signal process signal)
  ((ucode-primitive process-signal 2) (subprocess-index process) signal))

(define (subprocess-kill process)
  ((ucode-primitive process-kill 1) (subprocess-index process)))

(define (subprocess-interrupt process)
  ((ucode-primitive process-interrupt 1) (subprocess-index process)))

(define (subprocess-quit process)
  ((ucode-primitive process-quit 1) (subprocess-index process)))

(define (subprocess-hangup process)
  ((ucode-primitive process-hangup 1) (subprocess-index process)))

(define (subprocess-stop process)
  ((ucode-primitive process-stop 1) (subprocess-index process)))

(define (start-batch-subprocess filename arguments environment)
  (make-subprocess filename arguments environment
		   false false false false
		   false false false))

(define (start-subprocess-in-background filename arguments environment)
  (make-subprocess filename arguments environment
		   'BACKGROUND 'INHERIT 'INHERIT 'INHERIT
		   false false false))

(define (run-subprocess-in-foreground filename arguments environment)
  (make-subprocess filename arguments environment
		   'FOREGROUND 'INHERIT 'INHERIT 'INHERIT
		   false false false))

(define (start-pipe-subprocess filename arguments environment)
  (with-values make-pipe
    (lambda (child-read parent-write)
      (with-values make-pipe
	(lambda (parent-read child-write)
	  (let ((process
		 (make-subprocess filename arguments environment
				  false child-read child-write child-write
				  false parent-read parent-write)))
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