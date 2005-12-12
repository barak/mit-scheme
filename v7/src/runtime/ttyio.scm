#| -*-Scheme-*-

$Id: ttyio.scm,v 1.22 2005/12/12 21:55:44 cph Exp $

Copyright 1991,1993,1996,1999,2003,2004 Massachusetts Institute of Technology
Copyright 2005 Massachusetts Institute of Technology

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

(define (initialize-package!)
  (let ((input-channel (tty-input-channel))
	(output-channel (tty-output-channel))
	(gtype (generic-i/o-port-type 'CHANNEL 'CHANNEL)))
    (let ((type
	   (make-port-type
	    `((BEEP ,operation/beep)
	      (CHAR-READY? ,generic-io/char-ready?)
	      (CLEAR ,operation/clear)
	      (DISCRETIONARY-FLUSH-OUTPUT ,generic-io/flush-output)
	      (READ-CHAR ,operation/read-char)
	      (READ-FINISH ,operation/read-finish)
	      (WRITE-SELF ,operation/write-self)
	      (X-SIZE ,operation/x-size)
	      (Y-SIZE ,operation/y-size))
	    gtype)))
      (let ((port (make-port type (make-cstate input-channel output-channel))))
	(set-channel-port! input-channel port)
	(set-channel-port! output-channel port)
	(set! the-console-port port)
	(set-console-i/o-port! port)
	(set-current-input-port! port)
	(set-current-output-port! port)))
    (set! *char-ready? (port-type/char-ready? gtype))
    (set! *read-char (port-type/read-char gtype))
    (set! *unread-char (port-type/unread-char gtype)))
  (add-event-receiver! event:before-exit save-console-input)
  (add-event-receiver! event:after-restore reset-console))

(define-structure (cstate (type vector)
			  (initial-offset 4) ;must match "genio.scm"
			  (constructor #f))
  (echo-input? #f read-only #t))

(define (save-console-input)
  ((ucode-primitive reload-save-string 1)
   (input-buffer-contents (port-input-buffer console-input-port))))

(define (reset-console)
  (let ((input-channel (tty-input-channel))
	(output-channel (tty-output-channel)))
    (set-port/state! the-console-port
		     (make-cstate input-channel output-channel))
    (let ((s ((ucode-primitive reload-retrieve-string 0))))
      (if s
	  (set-input-buffer-contents! (port-input-buffer the-console-port)
				      s)))
    (set-channel-port! input-channel the-console-port)
    (set-channel-port! output-channel the-console-port)))

(define (make-cstate input-channel output-channel)
  (make-gstate input-channel
	       output-channel
	       'TEXT
	       (channel-type=file? input-channel)))

(define (set-console-i/o-port! port)
  (if (not (i/o-port? port))
      (error:wrong-type-argument port "I/O port" 'SET-CONSOLE-I/O-PORT!))
  (set! console-i/o-port port)
  (set! console-input-port port)
  (set! console-output-port port)
  unspecific)

(define (console-i/o-port? port)
  (port=? port console-i/o-port))

(define the-console-port)
(define console-i/o-port)
(define console-input-port)
(define console-output-port)
(define *char-ready?)
(define *read-char)
(define *unread-char)

(define (operation/read-char port)
  (let ((char (generic-io/read-char port)))
    (if (eof-object? char)
	(begin
	  (if (not (nearest-cmdl/batch-mode?))
	      (begin
		(fresh-line port)
		(write-string "End of input stream reached." port)))
	  (%exit)))
    (maybe-echo-input port char)
    char))

(define (operation/read-finish port)
  (let loop ()
    (if (*char-ready? port)
	(let ((char (*read-char port)))
	  (if (not (eof-object? char))
	      (begin
		(maybe-echo-input port char)
		(if (char-whitespace? char)
		    (loop)
		    (*unread-char port char)))))))
  (output-port/discretionary-flush port))

(define (maybe-echo-input port char)
  (if (and char
	   (cstate-echo-input? (port/state port))
	   (not (nearest-cmdl/batch-mode?)))
      (output-port/write-char port char)))

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