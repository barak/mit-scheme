#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/ttyio.scm,v 1.1 1991/11/15 05:17:32 cph Exp $

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
  (set! hook/read-start default/read-start)
  (set! hook/read-finish default/read-finish)
  (set! console-i/o-port
	(make-i/o-port
	 `((BEEP ,operation/beep)
	   (BUFFERED-INPUT-CHARS ,operation/buffered-input-chars)
	   (BUFFERED-OUTPUT-CHARS ,operation/buffered-output-chars)
	   (CHAR-READY? ,operation/char-ready?)
	   (CLEAR ,operation/clear)
	   (DISCARD-CHAR ,operation/read-char)
	   (FLUSH-OUTPUT ,operation/flush-output)
	   (INPUT-BUFFER-SIZE ,operation/input-buffer-size)
	   (INPUT-CHANNEL ,operation/input-channel)
	   (OUTPUT-BUFFER-SIZE ,operation/output-buffer-size)
	   (OUTPUT-CHANNEL ,operation/output-channel)
	   (PEEK-CHAR ,operation/peek-char)
	   (PRINT-SELF ,operation/print-self)
	   (READ-CHAR ,operation/read-char)
	   (READ-FINISH! ,operation/read-finish!)
	   (READ-START! ,operation/read-start!)
	   (SET-INPUT-BUFFER-SIZE ,operation/set-input-buffer-size)
	   (SET-OUTPUT-BUFFER-SIZE ,operation/set-output-buffer-size)
	   (WRITE-CHAR ,operation/write-char)
	   (WRITE-STRING ,operation/write-string)
	   (X-SIZE ,operation/x-size)
	   (Y-SIZE ,operation/y-size))
	 false))
  (set! console-input-port console-i/o-port)
  (set! console-output-port console-i/o-port)
  (reset-console)
  (add-event-receiver! event:after-restore reset-console)
  (add-event-receiver! event:before-exit save-console-input)
  (set-current-input-port! console-i/o-port)
  (set-current-output-port! console-i/o-port))

(define console-i/o-port)
(define console-input-port)
(define console-output-port)

(define (save-console-input)
  ((ucode-primitive reload-save-string 1)
   (input-buffer/buffer-contents (port/input-buffer console-input-port))))

(define (reset-console)
  (set-port/state!
   console-i/o-port
   (let ((input-channel (tty-input-channel))
	 (output-channel (tty-output-channel)))
     (set-channel-port! input-channel console-i/o-port)
     (set-channel-port! output-channel console-i/o-port)
     (make-console-port-state
      (let ((buffer (make-input-buffer input-channel input-buffer-size)))
	(let ((contents ((ucode-primitive reload-retrieve-string 0))))
	  (if contents
	      (input-buffer/set-buffer-contents buffer contents)))
	buffer)
      (make-output-buffer output-channel output-buffer-size)
      (channel-type=file? input-channel)))))

(define input-buffer-size 512)
(define output-buffer-size 512)

(define-structure (console-port-state (type vector)
				      (conc-name console-port-state/))
  ;; First two elements of this vector are required by the generic
  ;; I/O port operations.
  (input-buffer false read-only true)
  (output-buffer false read-only true)
  (echo-input? false read-only true))

(define-integrable (port/input-buffer port)
  (console-port-state/input-buffer (port/state port)))

(define-integrable (port/output-buffer port)
  (console-port-state/output-buffer (port/state port)))

(define (operation/peek-char port)
  (let ((char (input-buffer/peek-char (port/input-buffer port))))
    (if (eof-object? char)
	(signal-end-of-input))
    char))

(define (operation/read-char port)
  (let ((char (input-buffer/read-char (port/input-buffer port))))
    (if (eof-object? char)
	(signal-end-of-input))
    (if char
	(cond ((console-port-state/echo-input? (port/state port))
	       (output-port/write-char console-output-port char)
	       (output-port/flush-output console-output-port))
	      (transcript-port
	       (output-port/write-char transcript-port char)
	       (output-port/flush-output transcript-port))))
    char))

(define (signal-end-of-input)
  (write-string "\nEnd of input stream reached" console-output-port)
  (%exit))

(define (operation/read-start! port)
  port
  (hook/read-start))

(define hook/read-start)
(define (default/read-start) false)

(define (operation/read-finish! port)
  (let ((buffer (port/input-buffer port)))
    (let loop ()
      (if (input-buffer/char-ready? buffer 0)
	  (let ((char (input-buffer/peek-char buffer)))
	    (if (char-whitespace? char)
		(begin
		  (operation/read-char port)
		  (loop)))))))
  (hook/read-finish))

(define hook/read-finish)
(define (default/read-finish) false)

(define (operation/write-char port char)
  (output-buffer/write-char-block (port/output-buffer port) char)
  (if transcript-port (output-port/write-char transcript-port char)))

(define (operation/write-string port string)
  (output-buffer/write-string-block (port/output-buffer port) string)
  (if transcript-port (output-port/write-string transcript-port string)))

(define (operation/flush-output port)
  (output-buffer/drain-block (port/output-buffer port))
  (if transcript-port (output-port/flush-output transcript-port)))

(define (operation/clear port)
  (operation/write-string port ((ucode-primitive tty-command-clear 0))))

(define (operation/beep port)
  (operation/write-string port ((ucode-primitive tty-command-beep 0))))

(define (operation/x-size port)
  port
  ((ucode-primitive tty-x-size 0)))

(define (operation/y-size port)
  port
  ((ucode-primitive tty-y-size 0)))

(define (operation/print-self state port)
  port
  (unparse-string state "for console"))