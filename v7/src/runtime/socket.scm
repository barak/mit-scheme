#| -*-Scheme-*-

$Id: socket.scm,v 1.13 1999/01/02 06:19:10 cph Exp $

Copyright (c) 1990-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

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

(define (socket-port channel buffer-size line-translation)
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
      ((ucode-primitive get-service-by-number 1) service)
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