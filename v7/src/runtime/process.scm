#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/process.scm,v 1.6 1991/03/01 01:06:22 cph Exp $

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

(define (initialize-package!)
  (reset-package!)
  (add-event-receiver! event:after-restore reset-package!))

(define (reset-package!)
  (set! subprocesses '())
  (set! scheme-subprocess-environment ((ucode-primitive scheme-environment 0)))
  unspecific)

(define (subprocess-list)
  (list-copy subprocesses))

(define-structure (subprocess
		   (constructor %make-subprocess)
		   (conc-name subprocess-))
  index
  pty-master
  (id false read-only true)
  input-channel
  output-channel
  %input-port
  %output-port)

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
  (let ((index
	 (let ((ctty-allowed? (string? ctty)))
	   (define-integrable (convert-stdio-arg stdio)
	     (cond ((not stdio) false)
		   ((eq? stdio 'INHERIT) -1)
		   ((and ctty-allowed? (eq? stdio 'CTTY)) -2)
		   ((channel? stdio) (channel-descriptor stdio))
		   (else
		    (error:wrong-type-argument stdio "process I/O channel"
					       'MAKE-SUBPROCESS))))
	   ((ucode-primitive make-subprocess 7)
	    filename arguments environment
	    (cond ((eq? ctty 'BACKGROUND) -1)
		  ((eq? ctty 'FOREGROUND) -2)
		  ((or (not ctty) (string? ctty)) ctty)
		  (else
		   (error:wrong-type-argument ctty
					      "process controlling terminal"
					      'MAKE-SUBPROCESS)))
	    (convert-stdio-arg stdin)
	    (convert-stdio-arg stdout)
	    (convert-stdio-arg stderr)))))
    (let ((process
	   (%make-subprocess index
			     pty-master
			     ((ucode-primitive process-id 1) index)
			     input-channel
			     output-channel
			     false
			     false)))
      (set! subprocesses (cons process subprocesses))
      (if (eq? ctty 'FOREGROUND)
	  (do ((status
		((ucode-primitive process-status 1) index)
		((ucode-primitive process-continue-foreground 1) index)))
	      ((not (fix:= status 0)))))
      process)))

(define (subprocess-delete process)
  (without-interrupts
   (lambda ()
     (if (subprocess-index process)
	 (begin
	   ;; `process-delete' will signal an error if the process is
	   ;; running or stopped.
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
  (convert-subprocess-status
   process
   ((ucode-primitive process-status 1) (subprocess-index process))))

(define (subprocess-wait process)
  (let ((index (subprocess-index process)))
    (let loop ()
      (let ((status ((ucode-primitive process-wait 1) index)))
	(case status
	  ((0) (loop))
	  (else (convert-subprocess-status process status)))))))

(define (subprocess-continue-foreground process)
  (let ((index (subprocess-index process)))
    (let loop ()
      (let ((status ((ucode-primitive process-continue-foreground 1) index)))
	(case status
	  ((0) (loop))
	  (else (convert-subprocess-status process status)))))))

(define (convert-subprocess-status process status)
  (let ((get-reason
	 (lambda (status)
	   (cons status
		 ((ucode-primitive process-reason 1)
		  (subprocess-index process))))))
    (case status
      ((0) 'RUNNING)
      ((1) (get-reason 'STOPPED))
      ((2) (get-reason 'EXITED))
      ((3) (get-reason 'SIGNALLED))
      (else (error "Illegal process status:" status)))))

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