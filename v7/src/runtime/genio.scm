#| -*-Scheme-*-

$Id: genio.scm,v 1.21 2004/01/19 04:30:20 cph Exp $

Copyright 1991,1993,1995,1996,1999,2002 Massachusetts Institute of Technology
Copyright 2003,2004 Massachusetts Institute of Technology

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

;;;; Generic I/O Ports
;;; package: (runtime generic-i/o-port)

(declare (usual-integrations))

(define (initialize-package!)
  (let ((input-operations
	 `((BUFFERED-INPUT-CHARS ,operation/buffered-input-chars)
	   (CHAR-READY? ,operation/char-ready?)
	   (CHARS-REMAINING ,operation/chars-remaining)
	   (CLOSE-INPUT ,operation/close-input)
	   (DISCARD-CHAR ,operation/read-char)
	   (EOF? ,operation/eof?)
	   (INPUT-BLOCKING-MODE ,operation/input-blocking-mode)
	   (INPUT-BUFFER-SIZE ,operation/input-buffer-size)
	   (INPUT-CHANNEL ,operation/input-channel)
	   (INPUT-OPEN? ,operation/input-open?)
	   (INPUT-TERMINAL-MODE ,operation/input-terminal-mode)
	   (PEEK-CHAR ,operation/peek-char)
	   (READ-CHAR ,operation/read-char)
	   (READ-SUBSTRING ,operation/read-substring)
	   (SET-INPUT-BLOCKING-MODE ,operation/set-input-blocking-mode)
	   (SET-INPUT-BUFFER-SIZE ,operation/set-input-buffer-size)
	   (SET-INPUT-TERMINAL-MODE ,operation/set-input-terminal-mode)))
	(output-operations
	 `((BUFFERED-OUTPUT-CHARS ,operation/buffered-output-chars)
	   (CLOSE-OUTPUT ,operation/close-output)
	   (FLUSH-OUTPUT ,operation/flush-output)
	   (FRESH-LINE ,operation/fresh-line)
	   (OUTPUT-BLOCKING-MODE ,operation/output-blocking-mode)
	   (OUTPUT-BUFFER-SIZE ,operation/output-buffer-size)
	   (OUTPUT-CHANNEL ,operation/output-channel)
	   (OUTPUT-COLUMN ,operation/output-column)
	   (OUTPUT-OPEN? ,operation/output-open?)
	   (OUTPUT-TERMINAL-MODE ,operation/output-terminal-mode)
	   (SET-OUTPUT-BLOCKING-MODE ,operation/set-output-blocking-mode)
	   (SET-OUTPUT-BUFFER-SIZE ,operation/set-output-buffer-size)
	   (SET-OUTPUT-TERMINAL-MODE ,operation/set-output-terminal-mode)
	   (WRITE-CHAR ,operation/write-char)
	   (WRITE-SUBSTRING ,operation/write-substring)))
	(other-operations
	 `((CLOSE ,operation/close)
	   (WRITE-SELF ,operation/write-self))))
    (set! generic-input-type
	  (make-port-type (append input-operations
				  other-operations)
			  #f))
    (set! generic-output-type
	  (make-port-type (append output-operations
				  other-operations)
			  #f))
    (set! generic-i/o-type
	  (make-port-type (append input-operations
				  output-operations
				  other-operations)
			  #f)))
  unspecific)

(define generic-input-type)
(define generic-output-type)
(define generic-i/o-type)

(define (make-generic-input-port input-channel input-buffer-size
				 #!optional line-translation)
  (let ((line-translation
	 (if (default-object? line-translation)
	     'DEFAULT
	     line-translation)))
    (make-generic-port generic-input-type
		       (make-input-buffer input-channel
					  input-buffer-size
					  line-translation)
		       #f)))

(define (make-generic-output-port output-channel output-buffer-size
				  #!optional line-translation)
  (let ((line-translation
	 (if (default-object? line-translation)
	     'DEFAULT
	     line-translation)))
    (make-generic-port generic-output-type
		       #f
		       (make-output-buffer output-channel
					   output-buffer-size
					   line-translation))))

(define (make-generic-i/o-port input-channel output-channel
			       input-buffer-size output-buffer-size
			       #!optional input-line-translation
			       output-line-translation)
  (let ((input-line-translation
	 (if (default-object? input-line-translation)
	     'DEFAULT
	     input-line-translation)))
    (let ((output-line-translation
	   (if (default-object? output-line-translation)
	       input-line-translation
	       output-line-translation)))
      (make-generic-port generic-i/o-type
			 (make-input-buffer input-channel
					    input-buffer-size
					    input-line-translation)
			 (make-output-buffer output-channel
					     output-buffer-size
					     output-line-translation)))))

(define (make-generic-port type input-buffer output-buffer)
  (let ((port (make-port type (vector input-buffer output-buffer))))
    (if input-buffer
	(set-channel-port! (input-buffer/channel input-buffer) port))
    (if output-buffer
	(set-channel-port! (output-buffer/channel output-buffer) port))
    port))

(define-integrable (port/input-buffer port)
  (vector-ref (port/state port) 0))

(define-integrable (port/output-buffer port)
  (vector-ref (port/state port) 1))

(define (operation/write-self port output-port)
  (cond ((i/o-port? port)
	 (write-string " for channels: " output-port)
	 (write (operation/input-channel port) output-port)
	 (write-string " " output-port)
	 (write (operation/output-channel port) output-port))
	((input-port? port)
	 (write-string " for channel: " output-port)
	 (write (operation/input-channel port) output-port))
	((output-port? port)
	 (write-string " for channel: " output-port)
	 (write (operation/output-channel port) output-port))
	(else
	 (write-string " for channel" output-port))))

(define (operation/char-ready? port interval)
  (input-buffer/char-ready? (port/input-buffer port) interval))

(define (operation/chars-remaining port)
  (input-buffer/chars-remaining (port/input-buffer port)))

(define (operation/eof? port)
  (input-buffer/eof? (port/input-buffer port)))

(define (operation/peek-char port)
  (input-buffer/peek-char (port/input-buffer port)))

(define (operation/read-char port)
  (input-buffer/read-char (port/input-buffer port)))

(define (operation/read-substring port string start end)
  (input-buffer/read-substring (port/input-buffer port) string start end))

(define (operation/input-buffer-size port)
  (input-buffer/size (port/input-buffer port)))

(define (operation/buffered-input-chars port)
  (input-buffer/buffered-chars (port/input-buffer port)))

(define (operation/set-input-buffer-size port buffer-size)
  (input-buffer/set-size (port/input-buffer port) buffer-size))

(define (operation/input-channel port)
  (input-buffer/channel (port/input-buffer port)))

(define (operation/input-blocking-mode port)
  (if (channel-blocking? (operation/input-channel port))
      'BLOCKING
      'NONBLOCKING))

(define (operation/set-input-blocking-mode port mode)
  (case mode
    ((BLOCKING) (channel-blocking (operation/input-channel port)))
    ((NONBLOCKING) (channel-nonblocking (operation/input-channel port)))
    (else (error:wrong-type-datum mode "blocking mode"))))

(define (operation/input-terminal-mode port)
  (let ((channel (operation/input-channel port)))
    (cond ((not (channel-type=terminal? channel)) #f)
	  ((terminal-cooked-input? channel) 'COOKED)
	  (else 'RAW))))

(define (operation/set-input-terminal-mode port mode)
  (let ((channel (operation/input-channel port)))
    (if (channel-type=terminal? channel)
	(case mode
	  ((COOKED) (terminal-cooked-input channel))
	  ((RAW) (terminal-raw-input channel))
	  ((#F) unspecific)
	  (else (error:wrong-type-datum mode "terminal mode")))
	unspecific)))

(define (operation/flush-output port)
  (output-buffer/drain-block (port/output-buffer port)))

(define (operation/write-char port char)
  (output-buffer/write-char-block (port/output-buffer port) char))

(define (operation/write-substring port string start end)
  (output-buffer/write-substring-block (port/output-buffer port)
				       string start end))

(define (operation/fresh-line port)
  (if (not (fix:= 0 (output-buffer/column (port/output-buffer port))))
      (operation/write-char port #\newline)))

(define (operation/output-column port)
  (output-buffer/column (port/output-buffer port)))

(define (operation/output-buffer-size port)
  (output-buffer/size (port/output-buffer port)))

(define (operation/buffered-output-chars port)
  (output-buffer/buffered-chars (port/output-buffer port)))

(define (operation/set-output-buffer-size port buffer-size)
  (output-buffer/set-size (port/output-buffer port) buffer-size))

(define (operation/output-channel port)
  (output-buffer/channel (port/output-buffer port)))

(define (operation/output-blocking-mode port)
  (if (channel-blocking? (operation/output-channel port))
      'BLOCKING
      'NONBLOCKING))

(define (operation/set-output-blocking-mode port mode)
  (case mode
    ((BLOCKING) (channel-blocking (operation/output-channel port)))
    ((NONBLOCKING) (channel-nonblocking (operation/output-channel port)))
    (else (error:wrong-type-datum mode "blocking mode"))))

(define (operation/output-terminal-mode port)
  (let ((channel (operation/output-channel port)))
    (cond ((not (channel-type=terminal? channel)) #f)
	  ((terminal-cooked-output? channel) 'COOKED)
	  (else 'RAW))))

(define (operation/set-output-terminal-mode port mode)
  (let ((channel (operation/output-channel port)))
    (if (channel-type=terminal? channel)
	(case mode
	  ((COOKED) (terminal-cooked-output (operation/output-channel port)))
	  ((RAW) (terminal-raw-output (operation/output-channel port)))
	  ((#F) unspecific)
	  (else (error:wrong-type-datum mode "terminal mode")))
	unspecific)))

(define (operation/close port)
  (operation/close-input port)
  (operation/close-output port))

(define (operation/close-output port)
  (let ((output-buffer (port/output-buffer port)))
    (if output-buffer
	(output-buffer/close output-buffer (port/input-buffer port)))))

(define (operation/close-input port)
  (let ((input-buffer (port/input-buffer port)))
    (if input-buffer
	(input-buffer/close input-buffer (port/output-buffer port)))))

(define (operation/output-open? port)
  (let ((output-buffer (port/output-buffer port)))
    (and output-buffer
	 (output-buffer/open? output-buffer))))

(define (operation/input-open? port)
  (let ((input-buffer (port/input-buffer port)))
    (and input-buffer
	 (input-buffer/open? input-buffer))))