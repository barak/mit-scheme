#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/genio.scm,v 1.2 1991/11/26 07:06:12 cph Exp $

Copyright (c) 1991 Massachusetts Institute of Technology

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

;;;; Generic I/O Ports
;;; package: (runtime generic-i/o-port)

(declare (usual-integrations))

(define (initialize-package!)
  (let ((input-operations
	 `((BUFFERED-INPUT-CHARS ,operation/buffered-input-chars)
	   (CHAR-READY? ,operation/char-ready?)
	   (CHARS-REMAINING ,operation/chars-remaining)
	   (DISCARD-CHAR ,operation/discard-char)
	   (DISCARD-CHARS ,operation/discard-chars)
	   (EOF? ,operation/eof?)
	   (INPUT-BLOCKING-MODE ,operation/input-blocking-mode)
	   (INPUT-BUFFER-SIZE ,operation/input-buffer-size)
	   (INPUT-CHANNEL ,operation/input-channel)
	   (INPUT-TERMINAL-MODE ,operation/input-terminal-mode)
	   (PEEK-CHAR ,operation/peek-char)
	   (READ-CHAR ,operation/read-char)
	   (READ-CHARS ,operation/read-chars)
	   (READ-STRING ,operation/read-string)
	   (READ-SUBSTRING ,operation/read-substring)
	   (SET-INPUT-BLOCKING-MODE ,operation/set-input-blocking-mode)
	   (SET-INPUT-BUFFER-SIZE ,operation/set-input-buffer-size)
	   (SET-INPUT-TERMINAL-MODE ,operation/set-input-terminal-mode)))
	(output-operations
	 `((BUFFERED-OUTPUT-CHARS ,operation/buffered-output-chars)
	   (FLUSH-OUTPUT ,operation/flush-output)
	   (OUTPUT-BLOCKING-MODE ,operation/output-blocking-mode)
	   (OUTPUT-BUFFER-SIZE ,operation/output-buffer-size)
	   (OUTPUT-CHANNEL ,operation/output-channel)
	   (OUTPUT-TERMINAL-MODE ,operation/output-terminal-mode)
	   (SET-OUTPUT-BLOCKING-MODE ,operation/set-output-blocking-mode)
	   (SET-OUTPUT-BUFFER-SIZE ,operation/set-output-buffer-size)
	   (SET-OUTPUT-TERMINAL-MODE ,operation/set-output-terminal-mode)
	   (WRITE-CHAR ,operation/write-char)
	   (WRITE-STRING ,operation/write-string)
	   (WRITE-SUBSTRING ,operation/write-substring)))
	(other-operations
	 `((CLOSE ,operation/close)
	   (PRINT-SELF ,operation/print-self))))
    (set! generic-input-template
	  (make-input-port (append input-operations
				   other-operations)
			   false))
    (set! generic-output-template
	  (make-output-port (append output-operations
				    other-operations)
			    false))
    (set! generic-i/o-template
	  (make-i/o-port (append input-operations
				 output-operations
				 other-operations)
			 false)))
  unspecific)

(define generic-input-template)
(define generic-output-template)
(define generic-i/o-template)

(define (make-generic-input-port input-channel input-buffer-size)
  (make-generic-port generic-input-template
		     (make-input-buffer input-channel input-buffer-size)
		     false))

(define (make-generic-output-port output-channel output-buffer-size)
  (make-generic-port generic-output-template
		     false
		     (make-output-buffer output-channel output-buffer-size)))

(define (make-generic-i/o-port input-channel output-channel
			       input-buffer-size output-buffer-size)
  (make-generic-port generic-i/o-template
		     (make-input-buffer input-channel input-buffer-size)
		     (make-output-buffer output-channel output-buffer-size)))

(define (make-generic-port template input-buffer output-buffer)
  (let ((port (port/copy template (vector input-buffer output-buffer))))
    (if input-buffer
	(set-channel-port! (input-buffer/channel input-buffer) port))
    (if output-buffer
	(set-channel-port! (output-buffer/channel output-buffer) port))
    port))

(define-integrable (port/input-buffer port)
  (vector-ref (port/state port) 0))

(define-integrable (port/output-buffer port)
  (vector-ref (port/state port) 1))

(define (operation/print-self unparser-state port)
  (cond ((i/o-port? port)
	 (unparse-string unparser-state "for channels: ")
	 (unparse-object unparser-state (operation/input-channel port))
	 (unparse-string unparser-state " ")
	 (unparse-object unparser-state (operation/output-channel port)))
	((input-port? port)
	 (unparse-string unparser-state "for channel: ")
	 (unparse-object unparser-state (operation/input-channel port)))
	((output-port? port)
	 (unparse-string unparser-state "for channel: ")
	 (unparse-object unparser-state (operation/output-channel port)))
	(else
	 (unparse-string unparser-state "for channel"))))

(define (operation/char-ready? port interval)
  (input-buffer/char-ready? (port/input-buffer port) interval))

(define (operation/chars-remaining port)
  (input-buffer/chars-remaining (port/input-buffer port)))

(define (operation/discard-char port)
  (input-buffer/discard-char (port/input-buffer port)))

(define (operation/discard-chars port delimiters)
  (input-buffer/discard-until-delimiter (port/input-buffer port) delimiters))

(define (operation/eof? port)
  (input-buffer/eof? (port/input-buffer port)))

(define (operation/peek-char port)
  (input-buffer/peek-char (port/input-buffer port)))

(define (operation/read-char port)
  (input-buffer/read-char (port/input-buffer port)))

(define (operation/read-chars port result-buffer)
  (input-buffer/read-substring (port/input-buffer port)
			       result-buffer
			       0
			       (string-length result-buffer)))

(define (operation/read-substring port string start end)
  (input-buffer/read-substring (port/input-buffer port) string start end))

(define (operation/read-string port delimiters)
  (input-buffer/read-until-delimiter (port/input-buffer port) delimiters))

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
    (cond ((not (channel-type=terminal? channel)) false)
	  ((terminal-cooked-input? channel) 'COOKED)
	  (else 'RAW))))

(define (operation/set-input-terminal-mode port mode)
  (case mode
    ((COOKED) (terminal-cooked-input (operation/input-channel port)))
    ((RAW) (terminal-raw-input (operation/input-channel port)))
    ((#F) unspecific)
    (else (error:wrong-type-datum mode "terminal mode"))))

(define (operation/flush-output port)
  (output-buffer/drain-block (port/output-buffer port)))

(define (operation/write-char port char)
  (output-buffer/write-char-block (port/output-buffer port) char))

(define (operation/write-string port string)
  (output-buffer/write-string-block (port/output-buffer port) string))

(define (operation/write-substring port string start end)
  (output-buffer/write-substring-block (port/output-buffer port)
				       string start end))

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
    (cond ((not (channel-type=terminal? channel)) false)
	  ((terminal-cooked-output? channel) 'COOKED)
	  (else 'RAW))))

(define (operation/set-output-terminal-mode port mode)
  (case mode
    ((COOKED) (terminal-cooked-output (operation/output-channel port)))
    ((RAW) (terminal-raw-output (operation/output-channel port)))
    ((#F) unspecific)
    (else (error:wrong-type-datum mode "terminal mode"))))

(define (operation/close port)
  (let ((input-buffer (port/input-buffer port)))
    (if input-buffer (input-buffer/close input-buffer)))
  (let ((output-buffer (port/output-buffer port)))
    (if output-buffer (output-buffer/close output-buffer))))