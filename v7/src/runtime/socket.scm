#| -*-Scheme-*-

$Id: socket.scm,v 1.10 1997/11/01 19:19:39 cph Exp $

Copyright (c) 1990-97 Massachusetts Institute of Technology

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

;;;; Socket Support
;;; package: (runtime socket)

(declare (usual-integrations))

(define (open-tcp-stream-socket host-name service
				#!optional buffer-size line-translation)
  (socket-port (open-tcp-stream-socket-channel host-name service)
	       (if (default-object? buffer-size) #f buffer-size)
	       (if (default-object? line-translation) #f line-translation)))

(define (open-unix-stream-socket filename
				#!optional buffer-size line-translation)
  (socket-port (open-unix-stream-socket-channel filename)
	       (if (default-object? buffer-size) #f buffer-size)
	       (if (default-object? line-translation) #f line-translation)))

(define (socket-ports channel buffer-size line-translation)
  (let ((buffer-size (or buffer-size 4096))
	(line-translation (or line-translation 'DEFAULT)))
    (make-generic-i/o-port channel channel
			   buffer-size buffer-size
			   line-translation line-translation)))

(define (open-tcp-stream-socket-channel host-name service)
  (let ((host (vector-ref (get-host-by-name host-name) 0))
	(port (tcp-service->port service)))
    (open-channel
     (lambda (p)
       (with-thread-timer-stopped
	 (lambda ()
	   ((ucode-primitive new-open-tcp-stream-socket 3) host port p)))))))

(define (get-host-by-name host-name)
  (with-thread-timer-stopped
    (lambda ()
      ((ucode-primitive get-host-by-name 1) host-name))))

(define (os/hostname)
  ((ucode-primitive canonical-host-name 1)
   ((ucode-primitive get-host-name 0))))

(define (open-unix-stream-socket-channel filename)
  (open-channel
   (lambda (p)
     (with-thread-timer-stopped
       (lambda ()
	 ((ucode-primitive new-open-unix-stream-socket 2) filename p))))))

(define (open-tcp-server-socket service)
  (open-channel
   (lambda (p)
     (with-thread-timer-stopped
       (lambda ()
	 ((ucode-primitive new-open-tcp-server-socket 2)
	  (tcp-service->port service)
	  p))))))

(define (tcp-service->port service)
  (if (exact-nonnegative-integer? service)
      service
      ((ucode-primitive get-service-by-name 2) service "tcp")))

(define (close-tcp-server-socket server-socket)
  (channel-close server-socket))

(define (allocate-host-address)
  (string-allocate ((ucode-primitive host-address-length 0))))

(define (tcp-server-connection-accept server-socket block?)
  (let ((peer-address (allocate-host-address)))
    (let ((channel
	   (with-channel-blocking server-socket block?
	     (lambda ()
	       (open-channel
		(lambda (p)
		  (with-thread-timer-stopped
		    (lambda ()
		      ((ucode-primitive new-tcp-server-connection-accept 3)
		       (channel-descriptor server-socket)
		       peer-address
		       p)))))))))
      (if channel
	  (let ((port (make-generic-i/o-port channel channel 64 64)))
	    (values port port peer-address))
	  (values false false false)))))