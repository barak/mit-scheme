#| -*-Scheme-*-

$Id: usrint.scm,v 1.25 2006/10/26 05:01:47 cph Exp $

Copyright 1991,1992,1993,1994,1995,2001 Massachusetts Institute of Technology
Copyright 2003,2005,2006 Massachusetts Institute of Technology

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

;;;; User Interface
;;; package: (runtime user-interface)

(declare (usual-integrations))

;;;; Prompting

(define (prompt-for-command-expression prompt #!optional port environment)
  (let ((prompt (canonicalize-command-prompt prompt))
	(port (optional-port port 'PROMPT-FOR-COMMAND-EXPRESSION))
	(environment
	 (optional-environment environment 'PROMPT-FOR-COMMAND-EXPRESSION))
	(level (nearest-cmdl/level)))
    (let ((operation (port/operation port 'PROMPT-FOR-COMMAND-EXPRESSION)))
      (if operation
	  (operation port environment prompt level)
	  (begin
	    (write-command-prompt port prompt level)
	    (port/with-input-terminal-mode port 'COOKED
	      (lambda ()
		(read port environment))))))))

(define (prompt-for-expression prompt #!optional port environment)
  (%prompt-for-expression
   (optional-port port 'PROMPT-FOR-EXPRESSION)
   (optional-environment environment 'PROMPT-FOR-EXPRESSION)
   prompt))

(define (prompt-for-evaluated-expression prompt #!optional environment port)
  (let ((environment
	 (optional-environment environment 'PROMPT-FOR-EVALUATED-EXPRESSION))
	(port (optional-port port 'PROMPT-FOR-EVALUATED-EXPRESSION)))
    (repl-eval (%prompt-for-expression port environment prompt)
	       environment)))

(define (%prompt-for-expression port environment prompt)
  (let ((prompt (canonicalize-prompt prompt ": ")))
    (let ((operation (port/operation port 'PROMPT-FOR-EXPRESSION)))
      (if operation
	  (operation port environment prompt)
	  (begin
	    (port/with-output-terminal-mode port 'COOKED
	      (lambda ()
		(fresh-line port)
		(newline port)
		(write-string prompt port)
		(flush-output port)))
	    (port/with-input-terminal-mode port 'COOKED
	      (lambda ()
		(read port environment))))))))

(define (optional-port port caller)
  (if (default-object? port)
      (interaction-i/o-port)
      (begin
	(guarantee-i/o-port port caller)
	port)))

(define (optional-environment environment caller)
  (if (default-object? environment)
      (nearest-repl/environment)
      (begin
	(guarantee-environment environment caller)
	environment)))

(define (prompt-for-command-char prompt #!optional port)
  (let ((prompt (canonicalize-command-prompt prompt))
	(port (if (default-object? port) (interaction-i/o-port) port))
	(level (nearest-cmdl/level)))
    (let ((operation (port/operation port 'PROMPT-FOR-COMMAND-CHAR)))
      (if operation
	  (operation port prompt level)
	  (default/prompt-for-command-char port prompt level)))))

(define (default/prompt-for-command-char port prompt level)
  (write-command-prompt port prompt level)
  (let loop ()
    (let ((char
	   (port/with-input-terminal-mode port 'RAW
	     (lambda ()
	       (read-char port)))))
      (if (char-graphic? char)
	  (begin
	    (port/with-output-terminal-mode port 'COOKED
	      (lambda ()
		(write-char char port)
		(flush-output port)))
	    char)
	  (loop)))))

(define (prompt-for-confirmation prompt #!optional port)
  (let ((prompt (canonicalize-prompt prompt " (y or n)? "))
	(port (if (default-object? port) (interaction-i/o-port) port)))
    (let ((operation (port/operation port 'PROMPT-FOR-CONFIRMATION)))
      (if operation
	  (operation port prompt)
	  (default/prompt-for-confirmation port prompt)))))

(define (default/prompt-for-confirmation port prompt)
  (port/with-output-terminal-mode port 'COOKED
    (lambda ()
      (fresh-line port)))
  (let loop ()
    (port/with-output-terminal-mode port 'COOKED
      (lambda ()
	(newline port)
	(write-string prompt port)
	(flush-output port)))
    (let ((char
	   (port/with-input-terminal-mode port 'RAW
	     (lambda ()
	       (read-char port)))))
      (case char
	((#\y #\Y #\space)
	 (port/with-output-terminal-mode port 'COOKED
	   (lambda ()
	     (write-string "Yes" port)
	     (flush-output port)))
	 true)
	((#\n #\N #\rubout)
	 (port/with-output-terminal-mode port 'COOKED
	   (lambda ()
	     (write-string "No" port)
	     (flush-output port)))
	 false)
	((#\newline)
	 (loop))
	(else
	 (port/with-output-terminal-mode port 'COOKED
	   (lambda ()
	     (write char port)
	     (beep port)
	     (flush-output port)))
	 (loop))))))

(define (canonicalize-prompt prompt suffix)
  (if (let ((length (string-length prompt)))
	(and (not (fix:= length 0))
	     (char=? (string-ref prompt (fix:- length 1)) #\space)))
      prompt
      (string-append prompt suffix)))

(define (canonicalize-command-prompt prompt)
  (cond ((string? prompt)
	 prompt)
	((and (pair? prompt)
	      (eq? 'STANDARD (car prompt))
	      (string? (cdr prompt)))
	 (cons (car prompt) (canonicalize-prompt (cdr prompt) " ")))
	(else
	 (error:wrong-type-datum prompt "a string or standard prompt"))))

(define (write-command-prompt port prompt level)
  (if (not (nearest-cmdl/batch-mode?))
      (port/with-output-terminal-mode port 'COOKED
	(lambda ()
	  (fresh-line port)
	  (newline port)
	  (if (and (pair? prompt)
		   (eq? 'STANDARD (car prompt)))
	      (begin
		(write level port)
		(write-string " " port)
		(write-string (cdr prompt) port))
	      (write-string prompt port))
	  (flush-output port)))))

;;;; Debugger Support

(define (port/debugger-failure port message)
  (let ((operation (port/operation port 'DEBUGGER-FAILURE)))
    (if operation
	(operation port message)
	(default/debugger-failure port message))))

(define (default/debugger-failure port message)
  (beep port)
  (default/debugger-message port message))

(define (port/debugger-message port message)
  (let ((operation (port/operation port 'DEBUGGER-MESSAGE)))
    (if operation
	(operation port message)
	(default/debugger-message port message))))

(define (default/debugger-message port message)
  (fresh-line port)
  (write-string message port))

(define (port/debugger-presentation port thunk)
  (let ((operation (port/operation port 'DEBUGGER-PRESENTATION)))
    (if operation
	(operation port thunk)
	(default/debugger-presentation port thunk))))

(define (default/debugger-presentation port thunk)
  (fresh-line port)
  (thunk))

;;;; Miscellaneous Hooks

(define (port/write-result port expression value hash-number
			   #!optional environment)
  (let ((operation (port/operation port 'WRITE-RESULT))
	(environment
	 (if (default-object? environment)
	     (nearest-repl/environment)
	     (begin
	       (guarantee-environment environment 'PORT/WRITE-RESULT)
	       environment))))
    (if operation
	(operation port expression value hash-number environment)
	(default/write-result port expression value hash-number environment))))

(define (default/write-result port expression object hash-number environment)
  expression
  (if (not (nearest-cmdl/batch-mode?))
      (port/with-output-terminal-mode port 'COOKED
	(lambda ()
	  (fresh-line port)
	  (write-string ";" port)
	  (if (and write-result:undefined-value-is-special?
		   (undefined-value? object))
	      (write-string "Unspecified return value" port)
	      (begin
		(write-string "Value" port)
		(if hash-number
		    (begin
		      (write-string " " port)
		      (write hash-number port environment)))
		(write-string ": " port)
		(write object port environment)))))))

(define write-result:undefined-value-is-special? true)

(define (port/set-default-directory port directory)
  (let ((operation (port/operation port 'SET-DEFAULT-DIRECTORY)))
    (if operation
	(operation port directory))))

(define (port/set-default-environment port environment)
  (let ((operation (port/operation port 'SET-DEFAULT-ENVIRONMENT)))
    (if operation
	(operation port environment))))

(define (port/gc-start port)
  (let ((operation (port/operation port 'GC-START)))
    (if (and operation (not *within-restore-window?*))
	(operation port))))

(define (port/gc-finish port)
  (let ((operation (port/operation port 'GC-FINISH)))
    (if (and operation (not *within-restore-window?*))
	(operation port))))

(define (port/read-start port)
  (let ((operation (port/operation port 'READ-START)))
    (if operation
	(operation port))))

(define (port/read-finish port)
  (let ((operation (port/operation port 'READ-FINISH)))
    (if operation
	(operation port))))

;;;; Activity notification

(define *notification-depth* 0)

(define (with-notification message thunk)
  (let ((port (notification-output-port))
	(done? #f)
	(n))
    (dynamic-wind
     (lambda ()
       (start-notification-line)
       (message port)
       (write-string "... " port)
       (set! n (output-port/bytes-written port))
       unspecific)
     (lambda ()
       (let ((v
	      (fluid-let ((*notification-depth* (+ *notification-depth* 1)))
		(thunk))))
	 (set! done? #t)
	 v))
     (lambda ()
       (if done?
	   (begin
	     (if (if n
		     (> (output-port/bytes-written port) n)
		     (output-port/line-start? port))
		 (begin
		   (start-notification-line)
		   (write-string "... " port)))
	     (set! n)
	     (write-string "done" port)
	     (newline port)))))))

(define (write-notification-line message)
  (start-notification-line)
  (message (notification-output-port)))

(define (start-notification-line)
  (let ((port (notification-output-port)))
    (fresh-line port)
    (write-string ";" port)
    (do ((i 0 (+ i 1)))
	((not (< i *notification-depth*)))
      (write-string "  " port))))