#| -*-Scheme-*-

$Id: emacs.scm,v 14.30 2002/11/20 19:46:19 cph Exp $

Copyright (c) 1988-2001 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

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
			   (eq? 'STANDARD (car prompt)))
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

(define emacs-presentation-top-justify?
  false)

;;;; Interrupt Support

(define (emacs/clean-input/flush-typeahead char)
  char
  (let loop ()
    (if (not (char=? #\NUL (input-port/read-char the-console-port)))
	(loop)))
  true)

(define (emacs/^G-interrupt)
  (transmit-signal the-console-port #\g))

;;;; Miscellaneous Hooks

(define (emacs/write-result port expression object hash-number)
  expression
  (cond ((eq? object emacs/write-result/ignore)
	 unspecific)
	((undefined-value? object)
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
	 (transmit-signal-with-argument port #\v (write-to-string object)))))

(define emacs/write-result/ignore
  (list 'EMACS/WRITE-RESULT/IGNORE))

(define (emacs/error-decision repl condition)
  repl condition
  (transmit-signal the-console-port #\z)
  (beep the-console-port)
  (if paranoid-error-decision?
      (cmdl-interrupt/abort-previous)))

(define paranoid-error-decision?
  false)

(define (emacs/set-default-directory port pathname)
  (transmit-signal-with-argument port #\w (->namestring pathname)))

(define (emacs/read-start port)
  (transmit-signal port #\s)
  (port/read-start the-console-port))

(define (emacs/read-finish port)
  (port/read-finish the-console-port)
  (transmit-signal port #\f))

;;;; Protocol Encoding

;;; GC-light operations are special because they must not cons.
;;; On an interpreted system, they will cons a little anyway.

(define (emacs/gc-start port)
  (output-port/flush-output port)
  (channel-write-block (port/output-channel port) "\033b" 0 2))

(define (emacs/gc-finish port)
  (channel-write-block (port/output-channel port) "\033e" 0 2))

(define (transmit-signal port type)
  (let ((channel (port/output-channel port))
	(buffer (string #\altmode type)))
    (output-port/flush-output port)
    (with-absolutely-no-interrupts
     (lambda ()
       (channel-write-block channel buffer 0 2)))))

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
	   (channel-write-block channel buffer 0 buffer-length)))))))

(define (emacs-typeout port message)
  (emacs-eval port "(message \"%s\" " (write-to-string message) ")"))

(define (emacs-eval port . strings)
  (transmit-signal-with-argument port #\E (apply string-append strings)))

;;;; Initialization

(define emacs-console-port)
(define console-output-channel)

(define (initialize-package!)
  (set! emacs-console-port
	(make-port (make-port-type
		    `((PROMPT-FOR-EXPRESSION ,emacs/prompt-for-expression)
		      (PROMPT-FOR-COMMAND-CHAR ,emacs/prompt-for-command-char)
		      (PROMPT-FOR-COMMAND-EXPRESSION
		       ,emacs/prompt-for-command-expression)
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
		    the-console-port-type)
		   (port/state the-console-port)))
  ;; YUCCH!  Kludge to copy mutex of console port into emacs port.
  (set-port/thread-mutex! emacs-console-port
			  (port/thread-mutex the-console-port))
  (set-console-i/o-port! (select-console-port))
  (add-event-receiver! event:after-restore reset-console-port!))

(define (reset-console-port!)
  ;; This is a kludge.  Maybe this method shouldn't be used.
  (let* ((new-port (select-console-port))
	 (old-port?
	  (lambda (port)
	    (and (or (eq? port the-console-port)
		     (eq? port emacs-console-port))
		 (not (eq? port new-port)))))
	 (replacement-port
	  (lambda (port)
	    (cond ((old-port? port) new-port)
		  ((and (transcriptable-port? port)
			(old-port? (encapsulated-port/port port)))
		   (make-transcriptable-port new-port))
		  (else #f)))))
    (if (let ((port console-i/o-port))
	  (or (eq? port the-console-port)
	      (eq? port emacs-console-port)))
	(set-console-i/o-port! new-port))
    (do ((pairs standard-port-accessors (cdr pairs)))
	((null? pairs))
      (let ((port (replacement-port ((caar pairs)))))
	(if port
	    ((cdar pairs) port))))
    (do ((cmdl (nearest-cmdl) (cmdl/parent cmdl)))
	((not cmdl))
      (let ((port (replacement-port (cmdl/port cmdl))))
	(if port
	    (set-cmdl/port! cmdl port))))))

(define (select-console-port)
  (set! console-output-channel (port/output-channel the-console-port))
  (if ((ucode-primitive under-emacs? 0))
      (begin
	(set! hook/clean-input/flush-typeahead
	      emacs/clean-input/flush-typeahead)
	(set! hook/^G-interrupt emacs/^G-interrupt)
	(set! hook/error-decision emacs/error-decision)
	emacs-console-port)
      (begin
	(set! hook/clean-input/flush-typeahead false)
	(set! hook/^G-interrupt false)
	(set! hook/error-decision false)
	the-console-port)))