#| -*-Scheme-*-

$Id: emacs.scm,v 14.41 2007/01/05 15:33:09 cph Exp $

Copyright 1986,1987,1991,1993,1994,1999 Massachusetts Institute of Technology
Copyright 2001,2003,2004,2005 Massachusetts Institute of Technology

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

;;;; GNU Emacs/Scheme Interface
;;; package: (runtime emacs-interface)

(declare (usual-integrations))

;;;; Prompting

(define (emacs/prompt-for-command-expression port environment prompt level)
  (transmit-modeline-string port prompt level)
  (transmit-signal port #\R)
  (read port environment))

(define (emacs/prompt-for-command-char port prompt level)
  (transmit-modeline-string port prompt level)
  (transmit-signal-with-argument port #\D "")
  (transmit-signal port #\o)
  (read-char-internal port))

(define (transmit-modeline-string port prompt level)
  (transmit-signal-with-argument
   port
   #\p
   (string-append (number->string level)
		  " "
		  (if (and (pair? prompt)
			   (eq? 'STANDARD (car prompt)))
		      (let ((entry (assoc (cdr prompt) cmdl-prompt-alist)))
			(if entry
			    (cadr entry)
			    "[Evaluator]"))
		      (string-append "[Evaluator] " prompt)))))

(define cmdl-prompt-alist
  '(("debug> " "[Debug]")
    ("where> " "[Where]")))

(define (emacs/prompt-for-expression port environment prompt)
  (transmit-signal-with-argument port #\i prompt)
  (read port environment))

(define (emacs/prompt-for-confirmation port prompt)
  (transmit-signal-with-argument
   port
   #\n
   (let ((suffix " (y or n)? "))
     (if (string-suffix? suffix prompt)
	 (string-append (string-head prompt
				     (fix:- (string-length prompt)
					    (string-length suffix)))
			"? ")
	 prompt)))
  (char=? #\y (read-char-internal port)))

(define (read-char-internal port)
  (transmit-signal port #\s)
  (let loop ()
    (let ((char (input-port/read-char port)))
      (if (char=? char #\newline)
	  (loop)
	  (begin
	    (transmit-signal port #\f)
	    char)))))

;;;; Debugger Support

(define (emacs/debugger-failure port message)
  (beep port)
  (emacs-typeout port message))

(define (emacs/debugger-message port message)
  (emacs-typeout port message))

(define (emacs/debugger-presentation port thunk)
  (newline port)
  (if emacs-presentation-top-justify?
      (begin
	(emacs-eval port "(setq xscheme-temp-1 (point))")
	(thunk)
	(emacs-eval
	 port
	 "(set-window-start (selected-window) xscheme-temp-1 nil)"))
      (thunk)))

(define emacs-presentation-top-justify? #f)

;;;; Interrupt Support

(define (emacs/clean-input/flush-typeahead char)
  char
  (let loop ()
    (if (not (char=? #\U+0000 (input-port/read-char the-console-port)))
	(loop)))
  #t)

(define (emacs/^G-interrupt)
  (transmit-signal the-console-port #\g))

;;;; Miscellaneous Hooks

(define (emacs/write-result port expression object hash-number environment)
  expression
  (cond ((undefined-value? object)
	 (transmit-signal-with-argument port #\v ""))
	(hash-number
	 ;; The #\P command used to do something useful, but now
	 ;; it just sets the Emacs variable `xscheme-prompt' to
	 ;; its string argument.  We use this to advantage here.
	 (transmit-signal-with-argument port #\P (write-to-string object))
	 (emacs-eval
	  port
	  "(xscheme-write-message-1 xscheme-prompt (format \";Value "
	  (number->string hash-number)
	  ": %s\" xscheme-prompt))"))
	(else
	 (transmit-signal-with-argument
	  port #\v
	  (call-with-output-string
	    (lambda (port)
	      (write object port environment)))))))

(define (emacs/error-decision repl condition)
  condition
  (let ((port (cmdl/port repl)))
    (if (eq? port the-console-port)
	(begin
	  (transmit-signal port #\z)
	  (beep port)
	  (if paranoid-error-decision?
	      (cmdl-interrupt/abort-previous))))))

(define paranoid-error-decision? #f)

(define (emacs/set-default-directory port pathname)
  (transmit-signal-with-argument port #\w (->namestring pathname)))

(define (emacs/read-start port)
  (transmit-signal port #\s)
  (let ((operation (deferred-operation 'READ-START)))
    (if operation
	(operation port))))

(define (emacs/read-finish port)
  (let ((operation (deferred-operation 'READ-FINISH)))
    (if operation
	(operation port)))
  (transmit-signal port #\f))

;;;; Protocol Encoding

;;; GC-light operations are special because they must not cons.
;;; On an interpreted system, they will cons a little anyway.

(define (emacs/gc-start port)
  (output-port/flush-output port)
  (cwb (port/output-channel port) "\033b" 0 2))

(define (emacs/gc-finish port)
  (cwb (port/output-channel port) "\033e" 0 2))

(define (transmit-signal port type)
  (let ((channel (port/output-channel port))
	(buffer (string #\altmode type)))
    (output-port/flush-output port)
    (with-absolutely-no-interrupts
     (lambda ()
       (cwb channel buffer 0 2)))))

(define (transmit-signal-with-argument port type string)
  (let ((channel (port/output-channel port))
	(length (string-length string)))
    (let ((buffer-length (+ length 3)))
      (let ((buffer (make-string buffer-length)))
	(string-set! buffer 0 #\altmode)
	(string-set! buffer 1 type)
	(substring-move! string 0 length buffer 2)
	(string-set! buffer (- buffer-length 1) #\altmode)
	(output-port/flush-output port)
	(with-absolutely-no-interrupts
	 (lambda ()
	   (cwb channel buffer 0 buffer-length)))))))

(define (cwb channel string start end)
  ;; This is a private copy of CHANNEL-WRITE-BLOCK that bypasses all
  ;; the threading hair in that procedure.
  (let loop ((start start) (n-left (fix:- end start)))
    (let ((n
	   ((ucode-primitive channel-write 4) (channel-descriptor channel)
					      string start end)))
      (cond ((not n) (loop start n-left))
	    ((fix:< n n-left) (loop (fix:+ start n) (fix:- n-left n)))))))

(define (emacs-typeout port message)
  (emacs-eval port "(message \"%s\" " (write-to-string message) ")"))

(define (emacs-eval port . strings)
  (transmit-signal-with-argument port #\E (apply string-append strings)))

;;;; Initialization

(define vanilla-console-port-type)
(define emacs-console-port-type)

(define (initialize-package!)
  (set! vanilla-console-port-type (port/type the-console-port))
  (set! emacs-console-port-type
	(make-port-type
	 `((PROMPT-FOR-EXPRESSION ,emacs/prompt-for-expression)
	   (PROMPT-FOR-COMMAND-CHAR ,emacs/prompt-for-command-char)
	   (PROMPT-FOR-COMMAND-EXPRESSION ,emacs/prompt-for-command-expression)
	   (PROMPT-FOR-CONFIRMATION ,emacs/prompt-for-confirmation)
	   (DEBUGGER-FAILURE ,emacs/debugger-failure)
	   (DEBUGGER-MESSAGE ,emacs/debugger-message)
	   (DEBUGGER-PRESENTATION ,emacs/debugger-presentation)
	   (WRITE-RESULT ,emacs/write-result)
	   (SET-DEFAULT-DIRECTORY ,emacs/set-default-directory)
	   (READ-START ,emacs/read-start)
	   (READ-FINISH ,emacs/read-finish)
	   (GC-START ,emacs/gc-start)
	   (GC-FINISH ,emacs/gc-finish))
	 vanilla-console-port-type))
  (add-event-receiver! event:after-restore
    (lambda ()
      (let ((type (select-console-port-type)))
	(if (let ((type (port/type the-console-port)))
	      (or (eq? type vanilla-console-port-type)
		  (eq? type emacs-console-port-type)))
	    (set-port/type! the-console-port type))))))

(define (select-console-port-type)
  (if ((ucode-primitive under-emacs? 0))
      (begin
	(set! hook/clean-input/flush-typeahead
	      emacs/clean-input/flush-typeahead)
	(set! hook/^G-interrupt emacs/^G-interrupt)
	(set! hook/error-decision emacs/error-decision)
	emacs-console-port-type)
      (begin
	(set! hook/clean-input/flush-typeahead #f)
	(set! hook/^G-interrupt #f)
	(set! hook/error-decision #f)
	vanilla-console-port-type)))

(define (deferred-operation name)
  (port-type/operation vanilla-console-port-type name))