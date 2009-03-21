#| -*-Scheme-*-

$Id: fileio.scm,v 1.41 2009/03/21 07:14:23 riastradh Exp $

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

;;;; File I/O Ports
;;; package: (runtime file-i/o-port)

(declare (usual-integrations))

(define (initialize-package!)
  (set! operation/pathname (generic-i/o-port-accessor 0))
  (let ((other-operations
	 `((LENGTH ,operation/length)
	   (PATHNAME ,operation/pathname)
	   (POSITION ,operation/position)
	   (SET-POSITION! ,operation/set-position!)
	   (TRUENAME ,operation/pathname)
	   (WRITE-SELF ,operation/write-self))))
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
(define operation/pathname)

(define (operation/length port)
  (channel-file-length
   (or (port/input-channel port)
       (port/output-channel port))))

(define (operation/write-self port output-port)
  (write-string " for file: " output-port)
  (write (->namestring (operation/pathname port)) output-port))

(define (operation/position port)
  (guarantee-positionable-port port 'OPERATION/POSITION)
  (if (output-port? port)
      (flush-output port))
  (if (input-port? port)
      (let ((input-buffer (port-input-buffer port)))
	(- (channel-file-position (port/input-channel port))
	   (input-buffer-free-bytes input-buffer)))
      (channel-file-position (port/output-channel port))))

(define (operation/set-position! port position)
  (guarantee-positionable-port port 'OPERATION/SET-POSITION!)
  (guarantee-exact-nonnegative-integer position 'OPERATION/SET-POSITION!)
  (if (output-port? port)
      (flush-output port))
  (if (input-port? port)
      (clear-input-buffer (port-input-buffer port)))
  (channel-file-set-position (if (input-port? port)
				 (port/input-channel port)
				 (port/output-channel port))
			     position))

(define (guarantee-positionable-port port caller)
  (guarantee-port port caller)
  (if (and (i/o-port? port)
	   (not (eq? (port/input-channel port) (port/output-channel port))))
      (error:bad-range-argument port caller))
  (if (and (input-port? port)
	   (not (input-buffer-using-binary-normalizer?
		 (port-input-buffer port))))
      (error:bad-range-argument port caller))
  (if (and (output-port? port)
	   (not (output-buffer-using-binary-denormalizer?
		 (port-output-buffer port))))
      (error:bad-range-argument port caller)))

(define (open-input-file filename)
  (let* ((pathname (merge-pathnames filename))
	 (channel (file-open-input-channel (->namestring pathname)))
	 (port (make-generic-i/o-port channel #f input-file-type pathname)))
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
	 (port (make-generic-i/o-port #f channel output-file-type pathname)))
    (set-channel-port! channel port)
    (port/set-line-ending port (file-line-ending pathname))
    port))

(define (open-exclusive-output-file filename)
  (let* ((pathname (merge-pathnames filename))
	 (channel (file-open-exclusive-output-channel (->namestring pathname)))
	 (port (make-generic-i/o-port #f channel output-file-type pathname)))
    (set-channel-port! channel port)
    (port/set-line-ending port (file-line-ending pathname))
    port))

(define (open-i/o-file filename)
  (let* ((pathname (merge-pathnames filename))
	 (channel (file-open-io-channel (->namestring pathname)))
	 (port (make-generic-i/o-port channel channel i/o-file-type pathname)))
    (set-channel-port! channel port)
    (port/set-line-ending port (file-line-ending pathname))
    port))

(define (open-binary-input-file filename)
  (let* ((pathname (merge-pathnames filename))
	 (channel (file-open-input-channel (->namestring pathname)))
	 (port (make-generic-i/o-port channel #f input-file-type pathname)))
    (set-channel-port! channel port)
    (port/set-coding port 'BINARY)
    (port/set-line-ending port 'BINARY)
    port))

(define (open-binary-output-file filename #!optional append?)
  (let* ((pathname (merge-pathnames filename))
	 (channel
	  (let ((filename (->namestring pathname)))
	    (if (if (default-object? append?) #f append?)
		(file-open-append-channel filename)
		(file-open-output-channel filename))))
	 (port (make-generic-i/o-port #f channel output-file-type pathname)))
    (set-channel-port! channel port)
    (port/set-coding port 'BINARY)
    (port/set-line-ending port 'BINARY)
    port))

(define (open-exclusive-binary-output-file filename)
  (let* ((pathname (merge-pathnames filename))
	 (channel (file-open-exclusive-output-channel (->namestring pathname)))
	 (port (make-generic-i/o-port #f channel output-file-type pathname)))
    (set-channel-port! channel port)
    (port/set-coding port 'BINARY)
    (port/set-line-ending port 'BINARY)
    port))

(define (open-binary-i/o-file filename)
  (let* ((pathname (merge-pathnames filename))
	 (channel (file-open-io-channel (->namestring pathname)))
	 (port (make-generic-i/o-port channel channel i/o-file-type pathname)))
    (set-channel-port! channel port)
    (port/set-coding port 'BINARY)
    (port/set-line-ending port 'BINARY)
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

(define call-with-exclusive-output-file
  (make-call-with-file open-exclusive-output-file))

(define call-with-binary-output-file
  (make-call-with-file open-binary-output-file))

(define call-with-exclusive-binary-output-file
  (make-call-with-file open-exclusive-binary-output-file))

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

(define with-output-to-exclusive-file
  (make-with-output-to-file call-with-exclusive-output-file))

(define with-output-to-binary-file
  (make-with-output-to-file call-with-binary-output-file))

(define with-output-to-exclusive-binary-file
  (make-with-output-to-file call-with-exclusive-binary-output-file))