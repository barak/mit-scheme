#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; Console I/O Ports
;;; package: (runtime console-i/o-port)

(declare (usual-integrations))

(define (make-binary-console-port)
  (make-binary-port (make-channel-input-source (tty-input-channel))
		    (make-channel-output-sink (tty-output-channel))))

(define-deferred the-console-port
  (let ((binary-port (make-binary-console-port)))
    (make-generic-i/o-port
     binary-port
     (make-textual-port-type `((beep ,operation/beep)
			       (char-ready? ,generic-io/char-ready?)
			       (clear ,operation/clear)
			       (discretionary-write-char
				,operation/discretionary-write-char)
			       (discretionary-flush-output
				,generic-io/flush-output)
			       (peek-char ,generic-io/peek-char)
			       (read-char ,operation/read-char)
			       (read-finish ,operation/read-finish)
			       (unread-char ,generic-io/unread-char)
			       (write-self ,operation/write-self)
			       (x-size ,operation/x-size)
			       (y-size ,operation/y-size))
			     (generic-i/o-port-type 'channel 'channel))
     (default-object)
     (should-echo-input? binary-port))))

(define (should-echo-input? binary-port)
  (channel-type=file? (binary-port-input-channel binary-port)))

(define-deferred echo-input?
  (generic-i/o-port-accessor 0))

(define-deferred set-echo-input!
  (generic-i/o-port-modifier 0))

(define (make-binary-error-port)
  (make-binary-port #f
		    (make-channel-output-sink (tty-error-channel))))

(define-deferred the-error-port
  (make-generic-i/o-port
   (make-binary-error-port)
   (make-textual-port-type `((beep ,operation/beep)
			     (clear ,operation/clear)
			     (discretionary-flush-output
			      ,generic-io/flush-output)
			     (write-self ,operation/write-self)
			     (x-size ,operation/x-size)
			     (y-size ,operation/y-size))
			   (generic-i/o-port-type #f 'channel))))

(define (console-i/o-port)
  the-console-port)

(define (console-i/o-port? port)
  (eqv? port the-console-port))

(define (console-error-port)
  the-error-port)

(add-boot-init!
 (lambda ()
   (current-input-port the-console-port)
   (current-output-port the-console-port)
   (current-error-port the-error-port)
   (add-event-receiver! event:before-exit save-console-input)
   (add-event-receiver! event:after-restore reset-console)))

(define (save-console-input)
  ((ucode-primitive reload-save-string 1)
   (generic-io/buffer-contents the-console-port)))

(define (reset-console)
  (let ((binary-port (make-binary-console-port)))
    (replace-binary-port! the-console-port binary-port)
    (set-echo-input! the-console-port (should-echo-input? binary-port)))
  (let ((contents ((ucode-primitive reload-retrieve-string 0))))
    (if contents
	(generic-io/set-buffer-contents the-console-port contents)))
  (replace-binary-port! the-error-port (make-binary-error-port)))

(define (operation/read-char port)
  (let ((char (generic-io/read-char port)))
    (if (eof-object? char)
	(begin
	  (if (not (nearest-cmdl/batch-mode?))
	      (begin
		(fresh-line port)
		(write-string "End of input stream reached." port)))
	  (if (let ((condition (nearest-repl/condition)))
		(and condition
		     (condition/error? condition)))
	      (exit 'eof)
	      (exit))))
    char))

(define (operation/read-finish port)
  (let loop ()
    (if (char-ready? port)
	(let ((char (generic-io/peek-char port)))
	  (if (not (eof-object? char))
	      (if (char-whitespace? char)
		  (begin
		    (generic-io/read-char port)
		    (loop)))))))
  (output-port/discretionary-flush port))

(define (operation/discretionary-write-char port char)
  (if (and (echo-input? port)
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