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

;;;; GNU Emacs/Scheme Interface
;;; package: (runtime emacs-interface)

(declare (usual-integrations))

;;;; Prompting

(define (emacs/prompt-for-command-expression port prompt level)
  (transmit-modeline-string port prompt level)
  (transmit-signal port #\R)
  (read port))

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
			   (eq? 'standard (car prompt)))
		      (let ((entry (assoc (cdr prompt) cmdl-prompt-alist)))
			(if entry
			    (cadr entry)
			    "[Evaluator]"))
		      (string-append "[Evaluator] " prompt)))))

(define cmdl-prompt-alist
  '(("debug> " "[Debug]")
    ("where> " "[Where]")))

(define (emacs/prompt-for-expression port prompt)
  (transmit-signal-with-argument port #\i prompt)
  (read port))

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

(define (emacs/^g-interrupt)
  (transmit-signal the-console-port #\g))

;;;; Miscellaneous Hooks

(define (emacs/write-result port expression object hash-number)
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
	      (write object port)))))))

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
  (let ((operation (deferred-operation 'read-start)))
    (if operation
	(operation port))))

(define (emacs/read-finish port)
  (let ((operation (deferred-operation 'read-finish)))
    (if operation
	(operation port)))
  (transmit-signal port #\f))

;;;; Protocol Encoding

;;; GC-light operations are special because they must not cons.
;;; On an interpreted system, they will cons a little anyway.

(define (emacs/gc-start port)
  (output-port/flush-output port)
  (cwb console-output-channel gc-start-bytes))

(define (emacs/gc-finish port)
  (declare (ignore port))
  (cwb console-output-channel gc-end-bytes))

(define (transmit-signal port type)
  (let ((buffer (string->utf8 (string #\esc type))))
    (output-port/flush-output port)
    (with-absolutely-no-interrupts
     (lambda ()
       (cwb console-output-channel buffer)))))

(define (transmit-signal-with-argument port type string)
  (let ((buffer
	 (let ((builder (bytevector-builder)))
	   (builder (char->integer #\esc))
	   (builder (char->integer type))
	   (builder (string->utf8 string))
	   (builder (char->integer #\esc))
	   (builder))))
    (output-port/flush-output port)
    (with-absolutely-no-interrupts
     (lambda ()
       (cwb console-output-channel buffer)))))

(define (cwb channel bytes)
  ;; This is a private copy of CHANNEL-WRITE-BLOCK that bypasses all
  ;; the threading hair in that procedure.
  (let ((end (bytevector-length bytes)))
    (let loop ((start 0) (n-left end))
      (let ((n
	     ((ucode-primitive channel-write 4) (channel-descriptor channel)
						bytes start end)))
	(cond ((not n) (loop start n-left))
	      ((fix:< n n-left) (loop (fix:+ start n) (fix:- n-left n))))))))

(define (emacs-typeout port message)
  (emacs-eval port "(message \"%s\" " (write-to-string message) ")"))

(define (emacs-eval port . strings)
  (transmit-signal-with-argument port #\E (apply string-append strings)))

;;;; Initialization

(define gc-start-bytes)
(define gc-end-bytes)
(define console-output-channel)
(define vanilla-console-port-type)
(define emacs-console-port-type)

(define (initialize-package!)
  (set! gc-start-bytes
	(bytevector (char->integer #\esc)
		    (char->integer #\b)))
  (set! gc-end-bytes
	(bytevector (char->integer #\esc)
		    (char->integer #\e)))
  (set! console-output-channel (output-port-channel the-console-port))
  (set! vanilla-console-port-type (textual-port-type the-console-port))
  (set! emacs-console-port-type
	(make-textual-port-type
	 `((prompt-for-expression ,emacs/prompt-for-expression)
	   (prompt-for-command-char ,emacs/prompt-for-command-char)
	   (prompt-for-command-expression ,emacs/prompt-for-command-expression)
	   (prompt-for-confirmation ,emacs/prompt-for-confirmation)
	   (debugger-failure ,emacs/debugger-failure)
	   (debugger-message ,emacs/debugger-message)
	   (debugger-presentation ,emacs/debugger-presentation)
	   (write-result ,emacs/write-result)
	   (set-default-directory ,emacs/set-default-directory)
	   (read-start ,emacs/read-start)
	   (read-finish ,emacs/read-finish)
	   (gc-start ,emacs/gc-start)
	   (gc-finish ,emacs/gc-finish))
	 vanilla-console-port-type))
  (add-event-receiver! event:after-restore
    (lambda ()
      (let ((type (select-console-port-type)))
	(if (let ((type (textual-port-type the-console-port)))
	      (or (eq? type vanilla-console-port-type)
		  (eq? type emacs-console-port-type)))
	    (set-textual-port-type! the-console-port type))))))

(define (select-console-port-type)
  (if ((ucode-primitive under-emacs? 0))
      (begin
	(set! hook/clean-input/flush-typeahead
	      emacs/clean-input/flush-typeahead)
	(set! hook/^g-interrupt emacs/^g-interrupt)
	(set! hook/error-decision emacs/error-decision)
	emacs-console-port-type)
      (begin
	(set! hook/clean-input/flush-typeahead #f)
	(set! hook/^g-interrupt #f)
	(set! hook/error-decision #f)
	vanilla-console-port-type)))

(define (deferred-operation name)
  (textual-port-type-operation vanilla-console-port-type name))