#| -*-Scheme-*-

$Id: fileio.scm,v 1.20 2002/11/20 19:46:19 cph Exp $

Copyright (c) 1991-2001 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

;;;; File I/O Ports
;;; package: (runtime file-i/o-port)

(declare (usual-integrations))

(define (initialize-package!)
  (let ((input-operations
	 `((LENGTH ,operation/length)
	   (REST->STRING ,operation/rest->string)))
	(other-operations
	 `((WRITE-SELF ,operation/write-self)
	   (PATHNAME ,operation/pathname)
	   (TRUENAME ,operation/truename))))
    (set! input-file-type
	  (make-port-type (append input-operations other-operations)
			  generic-input-type))
    (set! output-file-type
	  (make-port-type other-operations
			  generic-output-type))
    (set! i/o-file-type
	  (make-port-type (append input-operations other-operations)
			  generic-i/o-type)))
  unspecific)

(define input-file-type)
(define output-file-type)
(define i/o-file-type)

(define input-buffer-size 512)
(define output-buffer-size 512)

(define (open-input-file filename)
  (let* ((pathname (merge-pathnames filename))
	 (channel (file-open-input-channel (->namestring pathname)))
	 (port
	  (make-port
	   input-file-type
	   (make-file-state
	    (make-input-buffer channel
			       input-buffer-size
			       (pathname-newline-translation pathname))
	    #f
	    pathname))))
    (set-channel-port! channel port)
    port))

(define (open-output-file filename #!optional append?)
  (let* ((pathname (merge-pathnames filename))
	 (channel
	  (let ((filename (->namestring pathname)))
	    (if (and (not (default-object? append?)) append?)
		(file-open-append-channel filename)
		(file-open-output-channel filename))))
	 (port
	  (make-port
	   output-file-type
	   (make-file-state
	    #f
	    (make-output-buffer channel
				output-buffer-size
				(pathname-newline-translation pathname))
	    pathname))))
    (set-channel-port! channel port)
    port))

(define (open-i/o-file filename)
  (let* ((pathname (merge-pathnames filename))
	 (channel (file-open-io-channel (->namestring pathname)))
	 (translation (pathname-newline-translation pathname))
	 (port
	  (make-port
	   i/o-file-type
	   (make-file-state
	    (make-input-buffer channel input-buffer-size translation)
	    (make-output-buffer channel output-buffer-size translation)
	    pathname))))
    (set-channel-port! channel port)
    port))

(define (pathname-newline-translation pathname)
  (let ((end-of-line (pathname-end-of-line-string pathname)))
    (and (not (string=? "\n" end-of-line))
	 end-of-line)))

(define (open-binary-input-file filename)
  (let* ((pathname (merge-pathnames filename))
	 (channel (file-open-input-channel (->namestring pathname)))
	 (port
	  (make-port input-file-type
		     (make-file-state (make-input-buffer channel
							 input-buffer-size
							 #f)
				      #f
				      pathname))))
    (set-channel-port! channel port)
    port))

(define (open-binary-output-file filename #!optional append?)
  (let* ((pathname (merge-pathnames filename))
	 (channel
	  (let ((filename (->namestring pathname)))
	    (if (and (not (default-object? append?)) append?)
		(file-open-append-channel filename)
		(file-open-output-channel filename))))
	 (port
	  (make-port output-file-type
		     (make-file-state #f
				      (make-output-buffer channel
							  output-buffer-size
							  #f)
				      pathname))))
    (set-channel-port! channel port)
    port))

(define (open-binary-i/o-file filename)
  (let* ((pathname (merge-pathnames filename))
	 (channel (file-open-io-channel (->namestring pathname)))
	 (port
	  (make-port i/o-file-type
		     (make-file-state (make-input-buffer channel
							 input-buffer-size
							 #f)
				      (make-output-buffer channel
							  output-buffer-size
							  #f)
				      pathname))))
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

(define-structure (file-state (type vector)
			      (conc-name file-state/))
  ;; First two elements of this vector are required by the generic
  ;; I/O port operations.
  (input-buffer #f read-only #t)
  (output-buffer #f read-only #t)
  (pathname #f read-only #t))

(define (operation/length port)
  (channel-file-length (port/input-channel port)))

(define (operation/pathname port)
  (file-state/pathname (port/state port)))

(define operation/truename
  ;; This works for unix because truename and pathname are the same.
  ;; On operating system where they differ, there must be support to
  ;; determine the truename.
  operation/pathname)

(define (operation/write-self port output-port)
  (write-string " for file: " output-port)
  (write (operation/truename port) output-port))

(define (operation/rest->string port)
  ;; This operation's intended purpose is to snarf an entire file in
  ;; a single gulp, exactly what a text editor would need.
  (let ((buffer (file-state/input-buffer (port/state port))))
    (let ((remaining (input-buffer/chars-remaining buffer))
	  (fill-buffer
	   (lambda (string)
	     (let ((length (string-length string)))
	       (let loop ()
		 (or (input-buffer/read-substring buffer string 0 length)
		     (loop)))))))
      (if remaining
	  (let ((result (make-string remaining)))
	    (let ((n (fill-buffer result)))
	      (if (fix:< n remaining)
		  (substring result 0 n)
		  result)))
	  (let loop ((strings '()))
	    (let ((string (make-string input-buffer-size)))
	      (let ((n (fill-buffer string)))
		(if (fix:< n input-buffer-size)
		    (apply string-append
			   (reverse! (cons (substring string 0 n) strings)))
		    (loop (cons string strings))))))))))