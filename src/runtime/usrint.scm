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

;;;; User Interface
;;; package: (runtime user-interface)

(declare (usual-integrations))

;;;; Prompting

(define (prompt-for-command-expression prompt #!optional port)
  (let ((prompt (canonicalize-command-prompt prompt))
	(port (optional-port port 'prompt-for-command-expression))
	(level (nearest-cmdl/level)))
    (let ((operation
	   (textual-port-operation port 'prompt-for-command-expression)))
      (if operation
	  (operation port prompt level)
	  (begin
	    (guarantee textual-i/o-port? port 'prompt-for-command-expression)
	    (write-command-prompt port prompt level)
	    (with-input-port-terminal-mode port 'cooked
	      (lambda ()
		(read port))))))))

(define (prompt-for-expression prompt #!optional port)
  (%prompt-for-expression port prompt 'prompt-for-expression))

(define (prompt-for-evaluated-expression prompt #!optional environment port)
  (repl-eval
   (%prompt-for-expression port prompt 'prompt-for-evaluated-expression)
   (if (default-object? environment)
       (nearest-repl/environment)
       (guarantee environment? environment 'prompt-for-evaluated-expression))))

(define (%prompt-for-expression port prompt caller)
  (let ((port (optional-port port caller))
	(prompt (canonicalize-prompt prompt ": ")))
    (let ((operation (textual-port-operation port 'prompt-for-expression)))
      (if operation
	  (operation port prompt)
	  (begin
	    (guarantee textual-i/o-port? port caller)
	    (with-output-port-terminal-mode port 'cooked
	      (lambda ()
		(fresh-line port)
		(newline port)
		(write-string prompt port)
		(flush-output-port port)))
	    (with-input-port-terminal-mode port 'cooked
	      (lambda ()
		(read port))))))))

(define (optional-port port caller)
  (if (default-object? port)
      (interaction-i/o-port)
      (guarantee textual-port? port caller)))

(define (prompt-for-command-char prompt #!optional port)
  (let ((prompt (canonicalize-command-prompt prompt))
	(port (if (default-object? port) (interaction-i/o-port) port))
	(level (nearest-cmdl/level)))
    (let ((operation (textual-port-operation port 'prompt-for-command-char)))
      (if operation
	  (operation port prompt level)
	  (default/prompt-for-command-char port prompt level)))))

(define (default/prompt-for-command-char port prompt level)
  (write-command-prompt port prompt level)
  (let loop ()
    (let ((char
	   (with-input-port-terminal-mode port 'raw
	     (lambda ()
	       (read-char port)))))
      (if (char-graphic? char)
	  (begin
	    (with-output-port-terminal-mode port 'cooked
	      (lambda ()
		(write-char char port)
		(flush-output-port port)))
	    char)
	  (loop)))))

(define (prompt-for-confirmation prompt #!optional port)
  (let ((prompt (canonicalize-prompt prompt " (y or n)? "))
	(port (if (default-object? port) (interaction-i/o-port) port)))
    (let ((operation (textual-port-operation port 'prompt-for-confirmation)))
      (if operation
	  (operation port prompt)
	  (default/prompt-for-confirmation port prompt)))))

(define (default/prompt-for-confirmation port prompt)
  (with-output-port-terminal-mode port 'cooked
    (lambda ()
      (fresh-line port)))
  (let loop ()
    (with-output-port-terminal-mode port 'cooked
      (lambda ()
	(newline port)
	(write-string prompt port)
	(flush-output-port port)))
    (let ((char
	   (with-input-port-terminal-mode port 'raw
	     (lambda ()
	       (read-char port)))))
      (case char
	((#\y #\Y #\space)
	 (with-output-port-terminal-mode port 'cooked
	   (lambda ()
	     (write-string "Yes" port)
	     (flush-output-port port)))
	 true)
	((#\n #\N #\rubout)
	 (with-output-port-terminal-mode port 'cooked
	   (lambda ()
	     (write-string "No" port)
	     (flush-output-port port)))
	 false)
	((#\newline)
	 (loop))
	(else
	 (with-output-port-terminal-mode port 'cooked
	   (lambda ()
	     (write char port)
	     (beep port)
	     (flush-output-port port)))
	 (loop))))))

(define (prompt-for-string prompt #!optional port)
  ;; Returns a string (the normal, "cooked" input line) or eof-object.
  (let ((port (if (default-object? port) (interaction-i/o-port) port)))
    (let ((operation (textual-port-operation port 'prompt-for-string)))
      (if operation
	  (operation port prompt)
	  (default/prompt-for-string port prompt)))))

(define (default/prompt-for-string port prompt)
  (with-output-port-terminal-mode port 'cooked
    (lambda ()
      (fresh-line port)
      (newline port)
      (write-string prompt port)
      (flush-output-port port)))
  (with-input-port-terminal-mode port 'cooked
    (lambda ()
      (read-line port))))

(define (call-with-pass-phrase prompt receiver #!optional port)
  (let ((port
	 (if (default-object? port)
	     (interaction-i/o-port)
	     (begin
	       (guarantee textual-i/o-port? port 'call-with-pass-phrase)
	       port))))
    (let ((operation (textual-port-operation port 'call-with-pass-phrase)))
      (if operation
	  (operation port prompt receiver)
	  (default/call-with-pass-phrase port prompt receiver)))))

(define (default/call-with-pass-phrase port prompt receiver)
  ;; Kludge: Uses RAW mode and "cooks" #\backspace, #\return, etc.
  ;; without regard for the tty's current "special characters".
  ;; Signals an error if PORT is not an i/o port.
  (let ((buffer (make-string 16))
	(index 0)
	(fill-char (integer->char #x155555)))
    (with-output-port-terminal-mode port 'cooked
      (lambda ()
	(fresh-line port)
	(newline port)
	(write-string (canonicalize-prompt prompt ": ") port)
	(flush-output-port port)))
    (let loop ()
      (let ((char
	     (with-binary-line-ending port
	      (lambda ()
		(with-input-port-terminal-mode port 'raw
		  (lambda ()
		    (read-char port)))))))
	(cond ((or (eof-object? char)
		   (char=? char #\return)
		   (char=? char #\linefeed))
	       (with-output-port-terminal-mode port 'cooked
		 (lambda ()
		   (newline port)))
	       (receiver (string-slice buffer 0 index))
	       (string-fill! buffer fill-char)
	       unspecific)
	      ((or (char=? char #\backspace)
		   (char=? char #\delete))
	       (if (fix:> index 0)
		   (set! index (fix:- index 1)))
	       (loop))
	      ((char=? char (integer->char #x15)) ;C-w
	       (set! index 0)
	       (loop))
	      (else
	       (let ((n (string-length buffer)))
		 (if (not (fix:< index n))
		     (let ((buffer* (make-string (fix:* 2 n))))
		       (string-copy! buffer* 0 buffer)
		       (string-fill! buffer fill-char)
		       (set! buffer buffer*))))
	       (string-set! buffer index char)
	       (set! index (fix:+ index 1))
	       (loop)))))))

(define (with-binary-line-ending port thunk)
  (let ((outside))
    (dynamic-wind
	(lambda ()
	  (if (textual-port-open? port)
	      (begin
		(set! outside (port/line-ending port))
		(port/set-line-ending port 'binary))))
	thunk
	(lambda ()
	  (if (textual-port-open? port)
	      (begin
		(port/set-line-ending port outside)
		(set! outside)))))))

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
	      (eq? 'standard (car prompt))
	      (string? (cdr prompt)))
	 (cons (car prompt) (canonicalize-prompt (cdr prompt) " ")))
	(else
	 (error:wrong-type-datum prompt "a string or standard prompt"))))

(define (write-command-prompt port prompt level)
  (if (not (nearest-cmdl/batch-mode?))
      (with-output-port-terminal-mode port 'cooked
	(lambda ()
	  (fresh-line port)
	  (newline port)
	  (if (and (pair? prompt)
		   (eq? 'standard (car prompt)))
	      (begin
		(write level port)
		(write-string " " port)
		(write-string (cdr prompt) port))
	      (write-string prompt port))
	  (flush-output-port port)))))

;;;; Debugger Support

(define (port/debugger-failure port message)
  (let ((operation (textual-port-operation port 'debugger-failure)))
    (if operation
	(operation port message)
	(default/debugger-failure port message))))

(define (default/debugger-failure port message)
  (beep port)
  (default/debugger-message port message))

(define (port/debugger-message port message)
  (let ((operation (textual-port-operation port 'debugger-message)))
    (if operation
	(operation port message)
	(default/debugger-message port message))))

(define (default/debugger-message port message)
  (fresh-line port)
  (write-string message port))

(define (port/debugger-presentation port thunk)
  (let ((operation (textual-port-operation port 'debugger-presentation)))
    (if operation
	(operation port thunk)
	(default/debugger-presentation port thunk))))

(define (default/debugger-presentation port thunk)
  (fresh-line port)
  (thunk))

;;;; Miscellaneous Hooks

(define (port/write-result port expression value hash-number)
  (let ((operation (textual-port-operation port 'write-result)))
    (if operation
	(operation port expression value hash-number)
	(default/write-result port expression value hash-number))))

(define (default/write-result port expression object hash-number)
  expression
  (if (not (nearest-cmdl/batch-mode?))
      (with-output-port-terminal-mode port 'cooked
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
		      (write hash-number port)))
		(write-string ": " port)
		(write object port)))))))

(define write-result:undefined-value-is-special? true)

(define (port/set-default-directory port directory)
  (let ((operation (textual-port-operation port 'set-default-directory)))
    (if operation
	(operation port directory))))

(define (port/set-default-environment port environment)
  (let ((operation (textual-port-operation port 'set-default-environment)))
    (if operation
	(operation port environment))))

(define (port/gc-start port)
  (let ((operation (textual-port-operation port 'gc-start)))
    (if (and operation (not (*within-restore-window?*)))
	(operation port))))

(define (port/gc-finish port)
  (let ((operation (textual-port-operation port 'gc-finish)))
    (if (and operation (not (*within-restore-window?*)))
	(operation port))))

(define (port/read-start port)
  (let ((operation (textual-port-operation port 'read-start)))
    (if operation
	(operation port))))

(define (port/read-finish port)
  (let ((operation (textual-port-operation port 'read-finish)))
    (if operation
	(operation port))))

;;;; Activity notification

(define (with-notification message #!optional thunk)
  (if (or (default-object? thunk) (not thunk))
      (let ((port (notification-output-port)))
	(fresh-line port)
	(write-notification-prefix port)
	(message (wrap-notification-port port)))
      (let ((done? #f)
	    (n))
	(dynamic-wind
	 (lambda ()
	   (let ((port (notification-output-port)))
	     (fresh-line port)
	     (write-notification-prefix port)
	     (message (wrap-notification-port port))
	     (write-string "..." port)
	     (set! n (output-port/bytes-written port))
	     unspecific))
	 (lambda ()
	   (let ((v
		  (parameterize ((*notification-depth*
				  (1+ (*notification-depth*))))
		    (thunk))))
	     (set! done? #t)
	     v))
	 (lambda ()
	   (let ((port (notification-output-port)))
	     (if (if n
		     (> (output-port/bytes-written port) n)
		     (output-port/line-start? port))
		 (begin
		   (fresh-line port)
		   (write-notification-prefix port)
		   (write-string "..." port)))
	     (set! n)
	     (write-string (if done? " done" " aborted") port)
	     (newline port)))))))

(define (wrap-notification-port port)
  (make-textual-port wrapped-notification-port-type port))

(define (make-wrapped-notification-port-type)
  (make-textual-port-type `((write-char ,operation/write-char)
			    (x-size ,operation/x-size)
			    (column ,operation/column)
			    (flush-output ,operation/flush-output)
			    (discretionary-flush-output
			     ,operation/discretionary-flush-output))
			  #f))

(define (operation/write-char port char)
  (let ((port* (textual-port-state port)))
    (let ((n (output-port/write-char port* char)))
      (if (char=? char #\newline)
	  (write-notification-prefix port*))
      n)))

(define (operation/x-size port)
  (let ((port* (textual-port-state port)))
    (let ((op (textual-port-operation port* 'x-size)))
      (and op
	   (let ((n (op port*)))
	     (and n
		  (max (- n (notification-prefix-length))
		       0)))))))

(define (operation/column port)
  (let ((port* (textual-port-state port)))
    (let ((op (textual-port-operation port* 'column)))
      (and op
	   (let ((n (op port*)))
	     (and n
		  (max (- n (notification-prefix-length))
		       0)))))))

(define (operation/flush-output port)
  (output-port/flush-output (textual-port-state port)))

(define (operation/discretionary-flush-output port)
  (output-port/discretionary-flush (textual-port-state port)))

(define (write-notification-prefix port)
  (write-string ";" port)
  (let ((depth (*notification-depth*)))
    (do ((i 0 (+ i 1)))
	((not (< i depth)))
      (write-string indentation-atom port))))

(define (notification-prefix-length)
  (+ 1
     (* (string-length indentation-atom)
	(*notification-depth*))))

(define *notification-depth*)
(define indentation-atom)
(define wrapped-notification-port-type)

(define (initialize-package!)
  (set! *notification-depth* (make-unsettable-parameter 0))
  (set! indentation-atom "  ")
  (set! wrapped-notification-port-type (make-wrapped-notification-port-type))
  unspecific)