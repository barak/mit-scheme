#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/socket.scm,v 1.3 1991/11/15 05:15:24 cph Exp $

Copyright (c) 1990 Massachusetts Institute of Technology

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

(define (open-tcp-stream-socket host-name service)
  (socket-ports
   (let ((host (vector-ref ((ucode-primitive get-host-by-name 1) host-name) 0))
	 (port ((ucode-primitive get-service-by-name 2) service "tcp")))
     (without-interrupts
      (lambda ()
	(make-channel
	 ((ucode-primitive open-tcp-stream-socket 2) host port)))))))

(define (open-unix-stream-socket filename)
  (socket-ports
   (without-interrupts
    (lambda ()
      (make-channel ((ucode-primitive open-unix-stream-socket 1) filename))))))

(define (socket-ports channel)
  (let ((port (make-generic-i/o-port channel channel 64 64)))
    (values port port)))

(define (open-tcp-server-socket service)
  (without-interrupts
   (lambda ()
     (make-channel
      ((ucode-primitive open-tcp-server-socket 1)
       ((ucode-primitive get-service-by-name 2) service "tcp"))))))

(define (close-tcp-server-socket server-socket)
  (channel-close server-socket))

(define (allocate-host-address)
  (string-allocate ((ucode-primitive host-address-length 0))))

(define (tcp-server-connection-accept server-socket block?)
  (let ((peer-address (allocate-host-address)))
    (let ((channel
	   (with-channel-blocking server-socket block?
	     (lambda ()
	       (without-interrupts
		(lambda ()
		  (let ((descriptor
			 ((ucode-primitive tcp-server-connection-accept 2)
			  (channel-descriptor server-socket)
			  peer-address)))
		    (and descriptor
			 (make-channel descriptor)))))))))
      (if channel
	  (let ((port (make-generic-i/o-port channel channel 64 64)))
	    (values port port peer-address))
	  (values false false false)))))