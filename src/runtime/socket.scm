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

;;;; Socket Support
;;; package: (runtime socket)

(declare (usual-integrations))

(define (open-tcp-server-socket service #!optional host)
  (let ((server-socket (create-tcp-server-socket)))
    (bind-tcp-server-socket server-socket
			    service
			    (if (or (default-object? host) (not host))
				((ucode-primitive host-address-any 0))
				host))
    (listen-tcp-server-socket server-socket)
    server-socket))

(define (create-tcp-server-socket)
  (open-channel
   (lambda (p)
     (system-pair-set-cdr! p ((ucode-primitive create-tcp-server-socket 0)))
     #t)))

(define (bind-tcp-server-socket server-socket service host)
  ((ucode-primitive bind-tcp-server-socket 3)
   (channel-descriptor server-socket)
   host
   (tcp-service->port service)))

(define (listen-tcp-server-socket server-socket)
  ((ucode-primitive listen-tcp-server-socket 1)
   (channel-descriptor server-socket)))

(define (tcp-service->port service)
  (if (exact-nonnegative-integer? service)
      ((ucode-primitive get-service-by-number 1) service)
      ((ucode-primitive get-service-by-name 2)
       (string-for-primitive service)
       (string-for-primitive "tcp"))))

(define (open-unix-server-socket pathname)
  (let ((filename (string-for-primitive (->namestring pathname))))
    (open-channel
     (lambda (p)
       ((ucode-primitive create-unix-server-socket 2) filename p)
       #t))))

(define (close-tcp-server-socket server-socket)
  (channel-close server-socket))

(define (close-unix-server-socket server-socket)
  (channel-close server-socket))

(define (tcp-server-connection-accept server-socket block? peer-address)
  (connection-accept (ucode-primitive new-tcp-server-connection-accept 3)
		     server-socket block? peer-address
		     make-socket-port 'tcp-server-connection-accept))

(define (tcp-server-binary-connection-accept server-socket block? peer-address)
  (connection-accept (ucode-primitive new-tcp-server-connection-accept 3)
		     server-socket block? peer-address
		     make-binary-socket-port
		     'tcp-server-binary-connection-accept))

(define (unix-server-connection-accept server-socket block?)
  (connection-accept (named-lambda (new-unix-server-connection-accept
				    socket peer pair)
		       (declare (ignore peer))
		       ((ucode-primitive new-unix-server-connection-accept 2)
			socket pair))
		     server-socket block? #f
		     make-socket-port 'unix-server-connection-accept))

(define (unix-server-binary-connection-accept server-socket block?)
  (connection-accept (named-lambda (new-unix-server-connection-accept
				    socket peer pair)
		       (declare (ignore peer))
		       ((ucode-primitive new-unix-server-connection-accept 2)
			socket pair))
		     server-socket block? #f
		     make-binary-socket-port
		     'unix-server-binary-connection-accept))

(define (connection-accept accept! server-socket block? peer-address
			   make-port caller)
  (let ((channel
	 (with-thread-events-blocked
	   (lambda ()
	     (let ((do-test
		    (lambda (k)
		      (let ((result
			     (test-for-io-on-channel server-socket
						     'read
						     block?)))
			(case result
			  ((read)
			   (open-channel
			    (lambda (p)
			      (with-thread-timer-stopped
				(lambda ()
				  (accept!
				   (channel-descriptor server-socket)
				   peer-address
				   p))))))
			  ((process-status-change)
			   (handle-subprocess-status-change)
			   (if (channel-closed? server-socket) #f (k)))
			  (else
			   (k)))))))
	       (if block?
		   (let loop () (do-test loop))
		   (do-test (lambda () #f))))))))
    (and channel
	 (make-port channel caller))))

(define (open-tcp-stream-socket host-name service)
  (let ((channel (open-tcp-stream-socket-channel host-name service)))
    (make-socket-port channel 'open-tcp-stream-socket)))

(define (open-binary-tcp-stream-socket host-name service)
  (let* ((channel (open-tcp-stream-socket-channel host-name service))
	 (port (make-binary-socket-port channel
					'open-binary-tcp-stream-socket)))
    (set-port-property! port 'pathname (string host-name":"service))
    port))

(define (open-unix-stream-socket pathname)
  (let ((channel (open-unix-stream-socket-channel pathname)))
    (make-socket-port channel 'open-unix-stream-socket)))

(define (open-binary-unix-stream-socket pathname)
  (let* ((channel (open-unix-stream-socket-channel pathname))
	 (port (make-binary-socket-port channel
					'open-binary-unix-stream-socket)))
    (set-port-property! port 'pathname (string pathname))
    port))

(define (open-tcp-stream-socket-channel host-name service)
  (let ((host
	 (vector-ref (or (get-host-by-name host-name)
			 (error:bad-range-argument
			  host-name
			  'open-tcp-stream-socket-channel))
		     0))
	(port (tcp-service->port service)))
    (open-channel
     (lambda (p)
       (with-thread-timer-stopped
	 (lambda ()
	   ((ucode-primitive new-open-tcp-stream-socket 3) host port p)))))))

(define (open-unix-stream-socket-channel pathname)
  (let ((filename (string-for-primitive (->namestring pathname))))
    (open-channel
     (lambda (p)
       (with-thread-timer-stopped
	 (lambda ()
	   ((ucode-primitive new-open-unix-stream-socket 2) filename p)))))))

(define (make-socket-port channel caller)
  (make-generic-i/o-port (make-binary-port (make-channel-input-source channel)
					   (make-channel-output-sink channel)
					   caller)
			 socket-port-type
			 caller))

(define (make-binary-socket-port channel caller)
  (make-binary-port (make-channel-input-source channel)
		    (make-channel-output-sink channel)
		    caller))

(define socket-port-type)
(define (initialize-package!)
  (set! socket-port-type
	(make-textual-port-type `((close-input ,socket/close-input)
				  (close-output ,socket/close-output))
				(generic-i/o-port-type 'channel 'channel)))
  unspecific)

(define (socket/close-input port)
  (if (textual-port-open? port)
      ((ucode-primitive shutdown-socket 2)
       (channel-descriptor (input-port-channel port))
       1))
  (generic-io/close-input port))

(define (socket/close-output port)
  (if (textual-port-open? port)
      ((ucode-primitive shutdown-socket 2)
       (channel-descriptor (input-port-channel port))
       2))
  (generic-io/close-output port))

(define (get-host-by-name host-name)
  (with-thread-timer-stopped
    (lambda ()
      ((ucode-primitive get-host-by-name 1) (string-for-primitive host-name)))))

(define (get-host-by-address host-address)
  (with-thread-timer-stopped
    (lambda ()
      ((ucode-primitive get-host-by-address 1) host-address))))

(define (canonical-host-name host-name)
  (with-thread-timer-stopped
    (lambda ()
      ((ucode-primitive canonical-host-name 1)
       (string-for-primitive host-name)))))

(define get-host-name
  (ucode-primitive get-host-name 0))

(define (os/hostname)
  (canonical-host-name (get-host-name)))

(define (allocate-host-address)
  (make-bytevector ((ucode-primitive host-address-length 0))))

(define host-address-any
  (ucode-primitive host-address-any 0))

(define host-address-loopback
  (ucode-primitive host-address-loopback 0))