#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/ttyio.scm,v 1.2 1991/11/26 07:07:11 cph Exp $

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

;;;; Console I/O Ports
;;; package: (runtime console-i/o-port)

(declare (usual-integrations))

(define (initialize-package!)
  (let ((input-channel (tty-input-channel))
	(output-channel (tty-output-channel)))
    (set! the-console-port
	  (make-i/o-port
	   `((BEEP ,operation/beep)
	     (BUFFERED-INPUT-CHARS ,operation/buffered-input-chars)
	     (BUFFERED-OUTPUT-CHARS ,operation/buffered-output-chars)
	     (CHAR-READY? ,operation/char-ready?)
	     (CLEAR ,operation/clear)
	     (DISCARD-CHAR ,operation/read-char)
	     (DISCRETIONARY-FLUSH-OUTPUT ,operation/discretionary-flush-output)
	     (FLUSH-OUTPUT ,operation/flush-output)
	     (INPUT-BLOCKING-MODE ,operation/input-blocking-mode)
	     (INPUT-BUFFER-SIZE ,operation/input-buffer-size)
	     (INPUT-CHANNEL ,operation/input-channel)
	     (INPUT-TERMINAL-MODE ,operation/input-terminal-mode)
	     (OUTPUT-BLOCKING-MODE ,operation/output-blocking-mode)
	     (OUTPUT-BUFFER-SIZE ,operation/output-buffer-size)
	     (OUTPUT-CHANNEL ,operation/output-channel)
	     (OUTPUT-TERMINAL-MODE ,operation/output-terminal-mode)
	     (PEEK-CHAR ,operation/peek-char)
	     (PRINT-SELF ,operation/print-self)
	     (READ-CHAR ,operation/read-char)
	     (READ-FINISH ,operation/read-finish)
	     (SET-INPUT-BLOCKING-MODE ,operation/set-input-blocking-mode)
	     (SET-INPUT-BUFFER-SIZE ,operation/set-input-buffer-size)
	     (SET-INPUT-TERMINAL-MODE ,operation/set-input-terminal-mode)
	     (SET-OUTPUT-BLOCKING-MODE ,operation/set-output-blocking-mode)
	     (SET-OUTPUT-BUFFER-SIZE ,operation/set-output-buffer-size)
	     (SET-OUTPUT-TERMINAL-MODE ,operation/set-output-terminal-mode)
	     (WRITE-CHAR ,operation/write-char)
	     (WRITE-SUBSTRING ,operation/write-substring)
	     (X-SIZE ,operation/x-size)
	     (Y-SIZE ,operation/y-size))
	   (make-console-port-state
	    (make-input-buffer input-channel input-buffer-size)
	    (make-output-buffer output-channel output-buffer-size)
	    (channel-type=file? input-channel))))
    (set-channel-port! input-channel the-console-port)
    (set-channel-port! output-channel the-console-port))
  (add-event-receiver! event:before-exit save-console-input)
  (add-event-receiver! event:after-restore reset-console)
  (set-console-i/o-port! the-console-port)
  (set-current-input-port! the-console-port)
  (set-current-output-port! the-console-port))

(define the-console-port)
(define input-buffer-size 512)
(define output-buffer-size 512)

(define (save-console-input)
  ((ucode-primitive reload-save-string 1)
   (input-buffer/buffer-contents (port/input-buffer console-input-port))))

(define (reset-console)
  (let ((input-channel (tty-input-channel))
	(output-channel (tty-output-channel))
	(state (port/state the-console-port)))
    (set-channel-port! input-channel the-console-port)
    (set-channel-port! output-channel the-console-port)
    (set-console-port-state/input-buffer!
     state
     (let ((buffer
	    (make-input-buffer
	     input-channel
	     (input-buffer/size (console-port-state/input-buffer state)))))
       (let ((contents ((ucode-primitive reload-retrieve-string 0))))
	 (if contents
	     (input-buffer/set-buffer-contents buffer contents)))
       buffer))
    (set-console-port-state/output-buffer!
     state
     (make-output-buffer
      output-channel
      (output-buffer/size (console-port-state/output-buffer state))))
    (set-console-port-state/echo-input?! state
					 (channel-type=file? input-channel))))

(define (set-console-i/o-port! port)
  (if (not (i/o-port? port))
      (error:wrong-type-argument port "I/O port" 'SET-CONSOLE-I/O-PORT!))
  (set! console-i/o-port port)
  (set! console-input-port port)
  (set! console-output-port port)
  unspecific)

(define console-i/o-port)
(define console-input-port)
(define console-output-port)

(define-structure (console-port-state (type vector)
				      (conc-name console-port-state/))
  ;; First two elements of this vector are required by the generic
  ;; I/O port operations.
  input-buffer
  output-buffer
  echo-input?)

(define-integrable (port/input-buffer port)
  (console-port-state/input-buffer (port/state port)))

(define-integrable (port/output-buffer port)
  (console-port-state/output-buffer (port/state port)))

(define (operation/peek-char port)
  (let ((char (input-buffer/peek-char (port/input-buffer port))))
    (if (eof-object? char)
	(signal-end-of-input port))
    char))

(define (operation/read-char port)
  (let ((char (input-buffer/read-char (port/input-buffer port))))
    (if (eof-object? char)
	(signal-end-of-input port))
    (if char
	(cond ((console-port-state/echo-input? (port/state port))
	       (output-port/write-char port char))
	      (transcript-port
	       (output-port/write-char transcript-port char)
	       (output-port/discretionary-flush transcript-port))))
    char))

(define (signal-end-of-input port)
  (fresh-line port)
  (write-string "End of input stream reached" port)
  (%exit))

(define (operation/read-finish port)
  (let ((buffer (port/input-buffer port)))
    (let loop ()
      (if (input-buffer/char-ready? buffer 0)
	  (let ((char (input-buffer/peek-char buffer)))
	    (if (char-whitespace? char)
		(begin
		  (operation/read-char port)
		  (loop)))))))
  (output-port/discretionary-flush port))

(define (operation/write-char port char)
  (output-buffer/write-char-block (port/output-buffer port) char)
  (if transcript-port (output-port/write-char transcript-port char)))

(define (operation/write-substring port string start end)
  (output-buffer/write-substring-block (port/output-buffer port)
				       string start end)
  (if transcript-port
      (output-port/write-substring transcript-port string start end)))

(define (operation/flush-output port)
  (output-buffer/drain-block (port/output-buffer port))
  (if transcript-port (output-port/flush-output transcript-port)))

(define (operation/discretionary-flush-output port)
  (output-buffer/drain-block (port/output-buffer port))
  (if transcript-port
      (output-port/discretionary-flush transcript-port)))

(define (operation/clear port)
  (output-port/write-string port ((ucode-primitive tty-command-clear 0))))

(define (operation/beep port)
  (output-port/write-string port ((ucode-primitive tty-command-beep 0))))

(define (operation/x-size port)
  port
  ((ucode-primitive tty-x-size 0)))

(define (operation/y-size port)
  port
  ((ucode-primitive tty-y-size 0)))

(define (operation/print-self state port)
  port
  (unparse-string state "for console"))