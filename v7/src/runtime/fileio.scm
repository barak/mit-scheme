#| -*-Scheme-*-

$Id: fileio.scm,v 1.14 1999/02/16 00:49:52 cph Exp $

Copyright (c) 1991-1999 Massachusetts Institute of Technology

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

;;;; File I/O Ports
;;; package: (runtime file-i/o-port)

(declare (usual-integrations))

(define (initialize-package!)
  (let ((input-operations
	 `((BUFFERED-INPUT-CHARS ,operation/buffered-input-chars)
	   (CHAR-READY? ,operation/char-ready?)
	   (CHARS-REMAINING ,operation/chars-remaining)
	   (CLOSE-INPUT ,operation/close-input)
	   (DISCARD-CHAR ,operation/discard-char)
	   (DISCARD-CHARS ,operation/discard-chars)
	   (EOF? ,operation/eof?)
	   (INPUT-BLOCKING-MODE ,operation/input-blocking-mode)
	   (INPUT-BUFFER-SIZE ,operation/input-buffer-size)
	   (INPUT-CHANNEL ,operation/input-channel)
	   (INPUT-OPEN? ,operation/input-open?)
	   (INPUT-TERMINAL-MODE ,operation/input-terminal-mode)
	   (LENGTH ,operation/length)
	   (PEEK-CHAR ,operation/peek-char)
	   (READ-CHAR ,operation/read-char)
	   (READ-STRING ,operation/read-string)
	   (READ-SUBSTRING ,operation/read-substring)
	   (REST->STRING ,operation/rest->string)
	   (SET-INPUT-BLOCKING-MODE ,operation/set-input-blocking-mode)
	   (SET-INPUT-BUFFER-SIZE ,operation/set-input-buffer-size)
	   (SET-INPUT-TERMINAL-MODE ,operation/set-input-terminal-mode)))
	(output-operations
	 `((BUFFERED-OUTPUT-CHARS ,operation/buffered-output-chars)
	   (CLOSE-OUTPUT ,operation/close-output)
	   (FLUSH-OUTPUT ,operation/flush-output)
	   (OUTPUT-BLOCKING-MODE ,operation/output-blocking-mode)
	   (OUTPUT-BUFFER-SIZE ,operation/output-buffer-size)
	   (OUTPUT-CHANNEL ,operation/output-channel)
	   (OUTPUT-OPEN? ,operation/output-open?)
	   (OUTPUT-TERMINAL-MODE ,operation/output-terminal-mode)
	   (SET-OUTPUT-BLOCKING-MODE ,operation/set-output-blocking-mode)
	   (SET-OUTPUT-BUFFER-SIZE ,operation/set-output-buffer-size)
	   (SET-OUTPUT-TERMINAL-MODE ,operation/set-output-terminal-mode)
	   (WRITE-CHAR ,operation/write-char)
	   (WRITE-SUBSTRING ,operation/write-substring)))
	(other-operations
	 `((CLOSE ,operation/close)
	   (PATHNAME ,operation/pathname)
	   (WRITE-SELF ,operation/write-self)
	   (TRUENAME ,operation/truename))))
    (set! input-file-template
	  (make-input-port (append input-operations
				   other-operations)
			   false))
    (set! output-file-template
	  (make-output-port (append output-operations
				    other-operations)
			    false))
    (set! i/o-file-template
	  (make-i/o-port (append input-operations
				 output-operations
				 other-operations)
			 false)))
  unspecific)

(define input-file-template)
(define output-file-template)
(define i/o-file-template)

(define input-buffer-size 512)
(define output-buffer-size 512)

(define (open-input-file filename)
  (let* ((pathname (merge-pathnames filename))
	 (channel (file-open-input-channel (->namestring pathname)))
	 (port
	  (port/copy
	   input-file-template
	   (make-file-state
	    (make-input-buffer channel
			       input-buffer-size
			       (pathname-newline-translation pathname))
	    false
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
	  (port/copy
	   output-file-template
	   (make-file-state
	    false
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
	  (port/copy
	   i/o-file-template
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
	  (port/copy input-file-template
		     (make-file-state (make-input-buffer channel
							 input-buffer-size
							 false)
				      false
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
	  (port/copy output-file-template
		     (make-file-state false
				      (make-output-buffer channel
							  output-buffer-size
							  false)
				      pathname))))
    (set-channel-port! channel port)
    port))

(define (open-binary-i/o-file filename)
  (let* ((pathname (merge-pathnames filename))
	 (channel (file-open-io-channel (->namestring pathname)))
	 (port
	  (port/copy i/o-file-template
		     (make-file-state (make-input-buffer channel
							 input-buffer-size
							 false)
				      (make-output-buffer channel
							  output-buffer-size
							  false)
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
  (input-buffer false read-only true)
  (output-buffer false read-only true)
  (pathname false read-only true))

(define (operation/length port)
  (channel-file-length (operation/input-channel port)))

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
	      (if (< n remaining)
		  (substring result 0 n)
		  result)))
	  (apply string-append
		 (let loop ()
		   (let ((string (make-string input-buffer-size)))
		     (let ((n (fill-buffer string)))
		       (cond ((zero? n) '())
			     ((< n remaining) (list (substring string 0 n)))
			     (else (cons string (loop))))))))))))