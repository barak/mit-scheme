#| -*-Scheme-*-

$Id: usrint.scm,v 1.13 1994/11/04 20:06:03 cph Exp $

Copyright (c) 1991-94 Massachusetts Institute of Technology

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

;;;; User Interface
;;; package: (runtime user-interface)

(declare (usual-integrations))

;;;; Prompting

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
	 (error:wrong-type-datum prompt))))

(define (write-command-prompt port prompt level)
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
      (flush-output port))))

(define (prompt-for-command-expression prompt #!optional port)
  (let ((prompt (canonicalize-command-prompt prompt))
	(port (if (default-object? port) (interaction-i/o-port) port))
	(level (nearest-cmdl/level)))
    (let ((operation (port/operation port 'PROMPT-FOR-COMMAND-EXPRESSION)))
      (if operation
	  (operation port prompt level)
	  (default/prompt-for-command-expression port prompt level)))))

(define (default/prompt-for-command-expression port prompt level)
  (write-command-prompt port prompt level)
  (port/with-input-terminal-mode port 'COOKED
    (lambda ()
      (read port))))

(define (prompt-for-expression prompt #!optional port)
  (let ((prompt (canonicalize-prompt prompt ": "))
	(port (if (default-object? port) (interaction-i/o-port) port)))
    (let ((operation (port/operation port 'PROMPT-FOR-EXPRESSION)))
      (if operation
	  (operation port prompt)
	  (default/prompt-for-expression port prompt)))))

(define (default/prompt-for-expression port prompt)
  (port/with-output-terminal-mode port 'COOKED
    (lambda ()
      (fresh-line port)
      (newline port)
      (write-string prompt port)
      (flush-output port)))
  (port/with-input-terminal-mode port 'COOKED
    (lambda ()
      (read port))))

(define (prompt-for-evaluated-expression prompt #!optional environment port)
  (hook/repl-eval #f
		  (prompt-for-expression prompt
					 (if (default-object? port)
					     (interaction-i/o-port)
					     port))
		  (if (default-object? environment)
		      (nearest-repl/environment)
		      environment)
		  (nearest-repl/syntax-table)))

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

(define (port/write-result port expression value hash-number)
  (let ((operation (port/operation port 'WRITE-RESULT)))
    (if operation
	(operation port expression value hash-number)
	(default/write-result port expression value hash-number))))

(define (default/write-result port expression object hash-number)
  expression
  (port/with-output-terminal-mode port 'COOKED
    (lambda ()
      (fresh-line port)
      (write-string ";" port)
      (if (and write-result:undefined-value-is-special?
	       (undefined-value? object))
	  (write-string "No useful value" port)
	  (begin
	    (write-string "Value" port)
	    (if hash-number
		(begin
		  (write-string " " port)
		  (write hash-number port)))
	    (write-string ": " port)
	    (write object port))))))

(define write-result:undefined-value-is-special? true)

(define (port/set-default-directory port directory)
  (let ((operation (port/operation port 'SET-DEFAULT-DIRECTORY)))
    (if operation
	(operation port directory))))

(define (port/set-default-environment port environment)
  (let ((operation (port/operation port 'SET-DEFAULT-ENVIRONMENT)))
    (if operation
	(operation port environment))))

(define (port/set-default-syntax-table port syntax-table)
  (let ((operation (port/operation port 'SET-DEFAULT-SYNTAX-TABLE)))
    (if operation
	(operation port syntax-table))))

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