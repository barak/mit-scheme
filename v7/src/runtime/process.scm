#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/process.scm,v 1.4 1990/11/09 08:44:17 cph Rel $

Copyright (c) 1989, 1990 Massachusetts Institute of Technology

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

(define-structure (subprocess
		   (constructor %make-subprocess)
		   (conc-name subprocess-))
  (index false read-only true)
  (ctty-type false read-only true)
  (pty false read-only true)
  (id false read-only true)
  (synchronous? false read-only true)
  ;; Input to the subprocess; an OUTPUT port.
  (input-port false read-only true)
  ;; Output from the subprocess; an INPUT port.
  (output-port false read-only true))

(define (make-subprocess filename arguments environment ctty-type)
  (let ((index
	 ((ucode-primitive make-subprocess 4)
	  filename
	  arguments
	  environment
	  (case ctty-type
	    ((none) 0)
	    ((inherited) 1)
	    ((pipe) 2)
	    ((pty) 3)
	    (else (error:illegal-datum ctty-type 'MAKE-SUBPROCESS))))))
    (let ((input-channel
	   (without-interrupts
	    (lambda ()
	      (make-channel ((ucode-primitive process-input 1) index)))))
	  (output-channel
	   (without-interrupts
	    (lambda ()
	      (make-channel ((ucode-primitive process-output 1) index)))))
	  (ctty-type
	   (let ((type ((ucode-primitive process-ctty-type 1) index))
		 (types '#(NONE INHERITED PIPE PTY)))
	     (and (< type (vector-length types))
		  (vector-ref types type)))))
      (let ((input-port (make-generic-output-port input-channel 512))
	    (output-port (make-generic-input-port output-channel 512)))
	(set-input-port/associated-port! input-port output-port)
	(set-output-port/associated-port! output-port input-port)
	(let ((process
	       (%make-subprocess
		index
		ctty-type
		(and (eq? ctty-type 'PTY) input-channel)
		((ucode-primitive process-id 1) index)
		((ucode-primitive process-synchronous? 1) index)
		input-port
		output-port)))
	  (set! subprocesses (cons process subprocesses))
	  process)))))

(define (subprocess-delete process)
  (close-output-port (subprocess-input-port process))
  (close-input-port (subprocess-output-port process))
  ((ucode-primitive process-delete 1) (subprocess-index process))
  (set! subprocesses (delq! process subprocesses))
  unspecific)

(define (subprocess-list)
  (list-copy subprocesses))

(define subprocesses)
(define scheme-subprocess-environment)

(define (initialize-package!)
  (reset-package!)
  (add-event-receiver! event:after-restore reset-package!))

(define (reset-package!)
  (set! subprocesses '())
  (set! scheme-subprocess-environment ((ucode-primitive scheme-environment 0)))
  unspecific)

(define (subprocess-status process)
  (let ((index (subprocess-index process)))
    (let ((status
	   (let ((status ((ucode-primitive process-status 1) index))
		 (statuses '#(RUNNING STOPPED EXITED SIGNALLED UNSTARTED)))
	     (and (< status (vector-length statuses))
		  (vector-ref statuses status)))))
      (if (or (eq? status 'STOPPED)
	      (eq? status 'EXITED)
	      (eq? status 'SIGNALLED))
	  (cons status ((ucode-primitive process-reason 1) index))
	  status))))

(define-integrable os-job-control?
  (ucode-primitive os-job-control? 0))

(define (subprocess-signal process signal to-process-group?)
  (let ((pty (and to-process-group? (subprocess-pty process))))
    (if (not pty)
	((ucode-primitive process-signal 2) (subprocess-index process) signal)
	(pty-master-send-signal pty signal))))

(define (subprocess-kill process to-process-group?)
  (let ((pty (and to-process-group? (subprocess-pty process))))
    (if (not pty)
	((ucode-primitive process-kill 1) (subprocess-index process))
	(pty-master-kill pty))))

(define (subprocess-stop process to-process-group?)
  (let ((pty (and to-process-group? (subprocess-pty process))))
    (if (not pty)
	((ucode-primitive process-stop 1) (subprocess-index process))
	(pty-master-stop pty))))

(define (subprocess-continue process to-process-group?)
  (let ((pty (and to-process-group? (subprocess-pty process))))
    (if (not pty)
	((ucode-primitive process-continue 1) (subprocess-index process))
	(pty-master-continue pty))))

(define (subprocess-interrupt process to-process-group?)
  (let ((pty (and to-process-group? (subprocess-pty process))))
    (if (not pty)
	((ucode-primitive process-interrupt 1) (subprocess-index process))
	(pty-master-interrupt pty))))

(define (subprocess-quit process to-process-group?)
  (let ((pty (and to-process-group? (subprocess-pty process))))
    (if (not pty)
	((ucode-primitive process-quit 1) (subprocess-index process))
	(pty-master-quit pty))))