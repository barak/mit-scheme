#| -*-Scheme-*-

$Id: ttyio.scm,v 1.15 2003/03/21 17:51:19 cph Exp $

Copyright 1991,1993,1996,1999,2003 Massachusetts Institute of Technology

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

;;;; Console I/O Ports
;;; package: (runtime console-i/o-port)

(declare (usual-integrations))

(define hook/read-char)
(define hook/peek-char)

(define (initialize-package!)
  (let ((input-channel (tty-input-channel))
	(output-channel (tty-output-channel)))
    (set! hook/read-char operation/read-char)
    (set! hook/peek-char operation/peek-char)
    (set! the-console-port-type
	  (make-port-type
	   `((BEEP ,operation/beep)
	     (CLEAR ,operation/clear)
	     (DISCRETIONARY-FLUSH-OUTPUT ,operation/flush-output)
	     (PEEK-CHAR ,(lambda (port) (hook/peek-char port)))
	     (READ-CHAR ,(lambda (port) (hook/read-char port)))
	     (READ-FINISH ,operation/read-finish)
	     (WRITE-SELF ,operation/write-self)
	     (X-SIZE ,operation/x-size)
	     (Y-SIZE ,operation/y-size))
	   generic-i/o-type))
    (set! the-console-port
	  (make-port the-console-port-type
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

(define the-console-port-type)
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
    (if (and char
	     (not (nearest-cmdl/batch-mode?))
	     (console-port-state/echo-input? (port/state port)))
	(output-port/write-char port char))
    char))

(define (signal-end-of-input port)
  (if (not (nearest-cmdl/batch-mode?))
      (begin
	(fresh-line port)
	(write-string "End of input stream reached" port)))
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

(define (operation/write-self port output-port)
  port
  (write-string " for console" output-port))