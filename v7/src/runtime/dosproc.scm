#| -*-Scheme-*-

$Id: dosproc.scm,v 1.1 1993/02/25 02:46:19 gjr Exp $

Copyright (c) 1992-1993 Massachusetts Institute of Technology

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

;;;; Subprocess Support for DOS
;;; package: (runtime)

(declare (usual-integrations))

(define run-subprocess
  (let ((prim (make-primitive-procedure 'run-subprocess 4))
	(channel-descriptor
	 (access channel-descriptor (->environment '(runtime primitive-io)))))

    (lambda (string #!optional stdin stdout stderr)
      (define (run in out err)
	(let ((value (prim string in out err)))
	  (cond ((zero? value)
		 unspecific)
		((< value 0)
		 (error "run-subprocess: Not available"))
		(else
		 (error "run-subprocess: Command failed" value)))))

      (define (with-channel-output-port port recvr)
	(call-with-temporary-filename
	 (lambda (fname)
	   (let ((value
		  (call-with-output-file fname
		    (lambda (port*)
		      (recvr
		       (channel-descriptor
			(output-port/channel port*)))))))
	     (call-with-input-file fname
	       (lambda (input)
		 (let ((string (read-string (char-set) input)))
		   (if (not (eof-object? string))
		       (write-string string
				     port)))))
	     value))))

      (define (with-channel-input-port port recvr)
	(call-with-temporary-filename
	 (lambda (fname)
	   (call-with-output-file fname
	     (lambda (output)
	       (write-string (read-string (char-set) port)
			     output)))
	   (call-with-input-file fname
	     (lambda (port*)
	       (recvr
		(channel-descriptor
		 (input-port/channel port*))))))))	

      (define (with-output-channel in out)
	(cond ((default-object? stderr)
	       (run in out out))
	      ((eq? stderr #t)
	       (run in out -1))
	      ((not (output-port? stderr))
	       (error "run: stderr not an output port" stderr))
	      ((output-port/channel stderr)
	       =>
	       (lambda (channel)
		 (output-port/flush-output stderr)
		 (run in out (channel-descriptor channel))))
	      (else
	       (with-channel-output-port stdout
		 (lambda (err)
		   (run in out err))))))

      (define (with-input-channel in)
	(let ((stdout
	       (if (or (default-object? stdout)
		       (not stdout))
		   (let ((port (current-output-port)))
		     (fresh-line port)
		     port)
		   stdout)))
	  (cond ((eq? stdout #t)
		 (with-output-channel in -1))
		((not (output-port? stdout))
		 (error "run: stdout not an output port" stdout))
		((output-port/channel stdout)
		 =>
		 (lambda (channel)
		   (output-port/flush-output stdout)
		   (with-output-channel in (channel-descriptor channel))))
		(else
		 (with-channel-output-port stdout
		   (lambda (out)
		     (with-output-channel in out)))))))

      (cond ((or (default-object? stdin)
		 (eq? stdin #t))
	     (with-input-channel -1))
	    ((not (input-port? stdin))
	     (error "run: stdin not an input port" stdin))
	    ((input-port/channel stdin)
	     => (lambda (channel)
		  (with-input-channel (channel-descriptor channel))))
	    (else
	     (with-channel-input-port stdin
	       with-input-channel))))))