#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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
      ((ucode-primitive get-service-by-name 2) service "tcp")))

(define (close-tcp-server-socket server-socket)
  (channel-close server-socket))

(define (tcp-server-connection-accept server-socket block? peer-address)
  (let ((channel
	 (with-thread-events-blocked
	   (lambda ()
	     (let ((do-test
		    (lambda (k)
		      (let ((result
			     (test-for-io-on-channel server-socket
						     'READ
						     block?)))
			(case result
			  ((READ)
			   (open-channel
			    (lambda (p)
			      (with-thread-timer-stopped
				(lambda ()
				  ((ucode-primitive
				    new-tcp-server-connection-accept
				    3)
				   (channel-descriptor server-socket)
				   peer-address
				   p))))))
			  ((PROCESS-STATUS-CHANGE)
			   (handle-subprocess-status-change)
			   (if (channel-closed? server-socket) #f (k)))
			  (else
			   (k)))))))
	       (if block?
		   (let loop () (do-test loop))
		   (do-test (lambda () #f))))))))
    (and channel
	 (make-socket-port channel))))

(define (open-tcp-stream-socket host-name service)
  (let ((channel (open-tcp-stream-socket-channel host-name service)))
    (make-socket-port channel)))

(define (open-unix-stream-socket filename)
  (let ((channel (open-unix-stream-socket-channel filename)))
    (make-socket-port channel)))

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

(define (open-unix-stream-socket-channel filename)
  (open-channel
   (lambda (p)
     (with-thread-timer-stopped
       (lambda ()
	 ((ucode-primitive new-open-unix-stream-socket 2) filename p))))))

(define (make-socket-port channel)
  (make-generic-i/o-port channel channel socket-port-type))

(define socket-port-type)
(define (initialize-package!)
  (set! socket-port-type
	(make-port-type `((CLOSE-INPUT ,socket/close-input)
			  (CLOSE-OUTPUT ,socket/close-output))
			(generic-i/o-port-type 'CHANNEL 'CHANNEL)))
  unspecific)

(define (socket/close-input port)
  (if (port/open? port)
      ((ucode-primitive shutdown-socket 2)
       (channel-descriptor (port/input-channel port))
       1))
  (generic-io/close-input port))

(define (socket/close-output port)
  (if (port/open? port)
      ((ucode-primitive shutdown-socket 2)
       (channel-descriptor (port/input-channel port))
       2))
  (generic-io/close-output port))

(define (get-host-by-name host-name)
  (with-thread-timer-stopped
    (lambda ()
      ((ucode-primitive get-host-by-name 1) host-name))))

(define (get-host-by-address host-address)
  (with-thread-timer-stopped
    (lambda ()
      ((ucode-primitive get-host-by-address 1) host-address))))

(define (canonical-host-name host-name)
  (with-thread-timer-stopped
    (lambda ()
      ((ucode-primitive canonical-host-name 1) host-name))))

(define get-host-name
  (ucode-primitive get-host-name 0))

(define (os/hostname)
  (canonical-host-name (get-host-name)))

(define (allocate-host-address)
  (string-allocate ((ucode-primitive host-address-length 0))))

(define host-address-any
  (ucode-primitive host-address-any 0))

(define host-address-loopback
  (ucode-primitive host-address-loopback 0))