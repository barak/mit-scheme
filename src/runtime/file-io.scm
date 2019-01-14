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

;;;; File I/O Ports
;;; package: (runtime file-i/o-port)

(declare (usual-integrations))

(define input-file-type)
(define output-file-type)
(define i/o-file-type)
(define (initialize-package!)
  (let ((other-operations
	 `((length ,operation/length)
	   (pathname ,operation/pathname)
	   (position ,operation/position)
	   (set-position! ,operation/set-position!)
	   (truename ,operation/pathname)
	   (write-self ,operation/write-self))))
    (let ((make-type
	   (lambda (source sink)
	     (make-textual-port-type other-operations
				     (generic-i/o-port-type source sink)))))
      (set! input-file-type (make-type 'channel #f))
      (set! output-file-type (make-type #f 'channel))
      (set! i/o-file-type (make-type 'channel 'channel))))
  unspecific)

(define (operation/pathname port)
  (port-property port 'pathname))

(define (set-port-pathname! port pathname)
  (set-port-property! port 'pathname pathname))

(define (operation/length port)
  (binary-port-length (generic-i/o-port->binary-port port)))

(define (operation/write-self port output-port)
  (write-string " for file: " output-port)
  (write (->namestring (operation/pathname port)) output-port))

(define (operation/position port)
  (binary-port-position (generic-i/o-port->binary-port port)))

(define (operation/set-position! port position)
  (set-binary-port-position! (generic-i/o-port->binary-port port) position))

(define (input-file-opener caller make-port)
  (lambda (filename)
    (let* ((pathname (merge-pathnames filename))
	   (channel (file-open-input-channel (->namestring pathname))))
      (make-port channel #f pathname caller))))

(define (output-file-opener caller make-port)
  (lambda (filename #!optional append?)
    (let* ((pathname (merge-pathnames filename))
	   (filename (->namestring pathname))
	   (channel
	    (if (if (default-object? append?) #f append?)
		(file-open-append-channel filename)
		(file-open-output-channel filename))))
      (make-port #f channel pathname caller))))

(define (exclusive-output-file-opener caller make-port)
  (lambda (filename)
    (let* ((pathname (merge-pathnames filename))
	   (channel
	    (file-open-exclusive-output-channel (->namestring pathname))))
      (make-port #f channel pathname caller))))

(define (i/o-file-opener caller make-port)
  (lambda (filename)
    (let* ((pathname (merge-pathnames filename))
	   (channel (file-open-io-channel (->namestring pathname))))
      (make-port channel channel pathname caller))))

(define (make-textual-file-port input-channel output-channel pathname caller)
  (let ((port
	 (%make-textual-file-port input-channel output-channel pathname
				  caller)))
    (port/set-line-ending port (file-line-ending pathname))
    port))

(define (make-legacy-binary-file-port input-channel output-channel pathname
				      caller)
  (let ((port
	 (%make-textual-file-port input-channel output-channel pathname
				  caller)))
    (port/set-coding port 'binary)
    (port/set-line-ending port 'binary)
    port))

(define (%make-textual-file-port input-channel output-channel pathname caller)
  (let ((port
	 (make-generic-i/o-port (make-binary-port
				 (and input-channel
				      (make-channel-input-source input-channel))
				 (and output-channel
				      (make-channel-output-sink output-channel))
				 caller)
				(cond ((not input-channel) output-file-type)
				      ((not output-channel) input-file-type)
				      (else i/o-file-type))
				caller)))
    (set-port-pathname! port pathname)
    port))

(define open-input-file
  (input-file-opener 'open-input-file make-textual-file-port))

(define open-output-file
  (output-file-opener 'open-output-file make-textual-file-port))

(define open-exclusive-output-file
  (exclusive-output-file-opener 'open-exclusive-output-file
				make-textual-file-port))

(define open-i/o-file
  (i/o-file-opener 'open-i/o-file make-textual-file-port))

(define open-legacy-binary-input-file
  (input-file-opener 'open-legacy-binary-input-file
		     make-legacy-binary-file-port))

(define open-legacy-binary-output-file
  (output-file-opener 'open-legacy-binary-output-file
		      make-legacy-binary-file-port))

(define open-exclusive-legacy-binary-output-file
  (exclusive-output-file-opener 'open-exclusive-legacy-binary-output-file
				make-legacy-binary-file-port))

(define open-legacy-binary-i/o-file
  (i/o-file-opener 'open-legacy-binary-i/o-file make-legacy-binary-file-port))

(define (make-binary-file-port input-channel output-channel pathname caller)
  (let ((port (%make-binary-file-port input-channel output-channel caller)))
    (set-port-pathname! port pathname)
    port))

(define (%make-binary-file-port input-channel output-channel caller)
  (make-binary-port
     (and input-channel (make-channel-input-source input-channel))
     (and output-channel (make-channel-output-sink output-channel))
     caller))

(define open-binary-input-file
  (input-file-opener 'open-binary-input-file make-binary-file-port))

(define open-binary-output-file
  (output-file-opener 'open-binary-output-file make-binary-file-port))

(define open-exclusive-binary-output-file
  (exclusive-output-file-opener 'open-exclusive-binary-output-file
				make-binary-file-port))

(define open-binary-i/o-file
  (i/o-file-opener 'open-binary-i/o-file make-binary-file-port))

(define ((make-call-with-file open) input-specifier receiver)
  (let ((port (open input-specifier)))
    (let ((value (receiver port)))
      (close-port port)
      value)))

(define call-with-input-file
  (make-call-with-file open-input-file))

(define call-with-output-file
  (make-call-with-file open-output-file))

(define call-with-exclusive-output-file
  (make-call-with-file open-exclusive-output-file))

(define call-with-append-file
  (make-call-with-file (lambda (filename) (open-output-file filename #t))))


(define call-with-binary-input-file
  (make-call-with-file open-binary-input-file))

(define call-with-binary-output-file
  (make-call-with-file open-binary-output-file))

(define call-with-exclusive-binary-output-file
  (make-call-with-file open-exclusive-binary-output-file))

(define call-with-binary-append-file
  (make-call-with-file
   (lambda (filename) (open-binary-output-file filename #t))))


(define call-with-legacy-binary-input-file
  (make-call-with-file open-legacy-binary-input-file))

(define call-with-legacy-binary-output-file
  (make-call-with-file open-legacy-binary-output-file))

(define call-with-exclusive-legacy-binary-output-file
  (make-call-with-file open-exclusive-legacy-binary-output-file))

(define call-with-legacy-binary-append-file
  (make-call-with-file
   (lambda (filename) (open-legacy-binary-output-file filename #t))))

(define ((make-with-input-from-file call) input-specifier thunk)
  (call input-specifier
    (lambda (port)
      (parameterize ((current-input-port port))
	(thunk)))))

(define with-input-from-file
  (make-with-input-from-file call-with-input-file))

(define with-input-from-legacy-binary-file
  (make-with-input-from-file call-with-legacy-binary-input-file))

(define ((make-with-output-to-file call) output-specifier thunk)
  (call output-specifier
    (lambda (port)
      (parameterize ((current-output-port port))
	(thunk)))))

(define with-output-to-file
  (make-with-output-to-file call-with-output-file))

(define with-output-to-exclusive-file
  (make-with-output-to-file call-with-exclusive-output-file))

(define with-output-to-legacy-binary-file
  (make-with-output-to-file call-with-legacy-binary-output-file))

(define with-output-to-exclusive-legacy-binary-file
  (make-with-output-to-file call-with-exclusive-legacy-binary-output-file))