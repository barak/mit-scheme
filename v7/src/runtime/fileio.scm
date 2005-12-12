#| -*-Scheme-*-

$Id: fileio.scm,v 1.26 2005/12/12 21:41:23 cph Exp $

Copyright 1991,1993,1994,1995,1996,1999 Massachusetts Institute of Technology
Copyright 2001,2004,2005 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; File I/O Ports
;;; package: (runtime file-i/o-port)

(declare (usual-integrations))

(define (initialize-package!)
  (let ((other-operations
	 `((WRITE-SELF ,operation/write-self)
	   (LENGTH ,operation/length)
	   (PATHNAME ,operation/pathname)
	   (TRUENAME ,operation/truename))))
    (let ((make-type
	   (lambda (source sink)
	     (make-port-type other-operations
			     (generic-i/o-port-type source sink)))))
      (set! input-file-type (make-type 'CHANNEL #f))
      (set! output-file-type (make-type #f 'CHANNEL))
      (set! i/o-file-type (make-type 'CHANNEL 'CHANNEL))))
  unspecific)

(define input-file-type)
(define output-file-type)
(define i/o-file-type)

(define-structure (fstate (type vector)
			  (initial-offset 4) ;must match "genio.scm"
			  (constructor #f))
  (pathname #f read-only #t))

(define (operation/length port)
  (channel-file-length
   (or (port/input-channel port)
       (port/output-channel port))))

(define (operation/pathname port)
  (fstate-pathname (port/state port)))

(define operation/truename
  ;; This works for unix because truename and pathname are the same.
  ;; On operating system where they differ, there must be support to
  ;; determine the truename.
  operation/pathname)

(define (operation/write-self port output-port)
  (write-string " for file: " output-port)
  (write (->namestring (operation/truename port)) output-port))

(define (open-input-file filename)
  (let* ((pathname (merge-pathnames filename))
	 (channel (file-open-input-channel (->namestring pathname)))
	 (port
	  (make-port input-file-type
		     (make-gstate channel #f 'TEXT pathname))))
    (set-channel-port! channel port)
    (port/set-line-ending port (file-line-ending pathname))
    port))

(define (open-output-file filename #!optional append?)
  (let* ((pathname (merge-pathnames filename))
	 (channel
	  (let ((filename (->namestring pathname)))
	    (if (if (default-object? append?) #f append?)
		(file-open-append-channel filename)
		(file-open-output-channel filename))))
	 (port
	  (make-port output-file-type
		     (make-gstate #f channel 'TEXT pathname))))
    (set-channel-port! channel port)
    (port/set-line-ending port (file-line-ending pathname))
    port))

(define (open-i/o-file filename)
  (let* ((pathname (merge-pathnames filename))
	 (channel (file-open-io-channel (->namestring pathname)))
	 (port
	  (make-port i/o-file-type
		     (make-gstate channel channel 'TEXT pathname))))
    (set-channel-port! channel port)
    (port/set-line-ending port (file-line-ending pathname))
    port))

(define (open-binary-input-file filename)
  (let* ((pathname (merge-pathnames filename))
	 (channel (file-open-input-channel (->namestring pathname)))
	 (port
	  (make-port input-file-type
		     (make-gstate channel #f 'BINARY pathname))))
    (set-channel-port! channel port)
    port))

(define (open-binary-output-file filename #!optional append?)
  (let* ((pathname (merge-pathnames filename))
	 (channel
	  (let ((filename (->namestring pathname)))
	    (if (if (default-object? append?) #f append?)
		(file-open-append-channel filename)
		(file-open-output-channel filename))))
	 (port
	  (make-port output-file-type
		     (make-gstate #f channel 'BINARY pathname))))
    (set-channel-port! channel port)
    port))

(define (open-binary-i/o-file filename)
  (let* ((pathname (merge-pathnames filename))
	 (channel (file-open-io-channel (->namestring pathname)))
	 (port
	  (make-port i/o-file-type
		     (make-gstate channel channel 'BINARY pathname))))
    (set-channel-port! channel port)
    port))

(define ((make-call-with-file open) input-specifier receiver)
  (let ((port (open input-specifier)))
    (let ((value (receiver port)))
      (close-port port)
      value)))

(define call-with-input-file 
  (make-call-with-file open-input-file))

(define call-with-binary-input-file
  (make-call-with-file open-binary-input-file))

(define call-with-output-file
  (make-call-with-file open-output-file))

(define call-with-binary-output-file
  (make-call-with-file open-binary-output-file))

(define call-with-append-file
  (make-call-with-file (lambda (filename) (open-output-file filename #t))))

(define call-with-binary-append-file
  (make-call-with-file
   (lambda (filename) (open-binary-output-file filename #t))))

(define ((make-with-input-from-file call) input-specifier thunk)
  (call input-specifier
    (lambda (port)
      (with-input-from-port port thunk))))

(define with-input-from-file
  (make-with-input-from-file call-with-input-file))

(define with-input-from-binary-file
  (make-with-input-from-file call-with-binary-input-file))

(define ((make-with-output-to-file call) output-specifier thunk)
  (call output-specifier
    (lambda (port)
      (with-output-to-port port thunk))))

(define with-output-to-file
  (make-with-output-to-file call-with-output-file))

(define with-output-to-binary-file
  (make-with-output-to-file call-with-binary-output-file))