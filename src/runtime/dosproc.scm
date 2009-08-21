#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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
			(port/output-channel port*)))))))
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
		 (port/input-channel port*))))))))	

      (define (with-output-channel in out)
	(cond ((default-object? stderr)
	       (run in out out))
	      ((eq? stderr #t)
	       (run in out -1))
	      ((not (output-port? stderr))
	       (error "run: stderr not an output port" stderr))
	      ((port/output-channel stderr)
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
		((port/output-channel stdout)
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
	    ((port/input-channel stdin)
	     => (lambda (channel)
		  (with-input-channel (channel-descriptor channel))))
	    (else
	     (with-channel-input-port stdin
	       with-input-channel))))))