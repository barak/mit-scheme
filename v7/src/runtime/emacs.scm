#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/emacs.scm,v 14.12 1992/02/26 22:39:18 cph Exp $

Copyright (c) 1988-92 Massachusetts Institute of Technology

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

;;;; GNU Emacs/Scheme Interface
;;; package: (runtime emacs-interface)

(declare (usual-integrations))

;;;; Prompting

(define (emacs/prompt-for-command-expression port prompt)
  (transmit-modeline-string port prompt)
  (transmit-signal port #\R)
  (read port))

(define (emacs/prompt-for-command-char port prompt)
  (transmit-modeline-string port prompt)
  (transmit-signal-with-argument port #\D "")
  (transmit-signal port #\o)
  (read-char-internal port))

(define (transmit-modeline-string port prompt)
  (transmit-signal-with-argument
   port
   #\p
   (with-values (lambda () (parse-repl-prompt prompt))
     (lambda (prefix prompt)
       (if prefix
	   (string-append prefix
			  (let ((entry (assoc prompt cmdl-prompt-alist)))
			    (if entry
				(cadr entry)
				"[Evaluator]")))
	   prompt)))))

(define (parse-repl-prompt prompt)
  ;; If the prompt is of the form "NNN foo", then it is a REP loop
  ;; prompt and should be treated specially.
  (let ((end (string-length prompt)))
    (let ((index
	   (and (> end 0)
		(char-numeric? (string-ref prompt 0))
		(let skip-digits ((index 1))
		  (and (< index end)
		       (cond ((char-numeric? (string-ref prompt index))
			      (skip-digits (+ index 1)))
			     ((char=? #\space (string-ref prompt index))
			      (let ((index (+ index 1)))
				(and (< index end)
				     index)))
			     (else
			      false)))))))
      (if index
	  (values (string-head prompt index) (string-tail prompt index))
	  (values false prompt)))))

(define cmdl-prompt-alist
  '(("debug>" "[Debugger]")
    ("where>" "[Environment Inspector]")
    ("which>" "[Task Inspector]")))

(define (emacs/prompt-for-expression port prompt)
  (transmit-signal-with-argument port #\i (string-append prompt ": "))
  (read port))

(define (emacs/prompt-for-confirmation port prompt)
  (transmit-signal-with-argument port #\n (string-append prompt "? "))
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

(define (emacs/write-result port object hash-number)
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
	 (transmit-signal-with-argument port #\v (write-to-string object)))))

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

(define (emacs/print-self state port)
  port
  (unparse-string state "for emacs"))

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
	(substring-move-left! string 0 length buffer 2)
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
	(make-i/o-port
	 (let ((operations
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
		  (GC-FINISH ,emacs/gc-finish))))
	   (append-map* operations
			(lambda (name)
			  (if (assq name operations)
			      '()
			      `((,name
				 ,(port/operation the-console-port name)))))
			(port/operation-names the-console-port)))
	 (port/state the-console-port)))
  ;; YUCCH!  Kludge to copy mutex of console port into emacs port.
  ((record-modifier port-rtd 'THREAD-MUTEX)
   emacs-console-port
   (port/thread-mutex the-console-port))
  (set-console-i/o-port! (select-console-port))
  (add-event-receiver! event:after-restore reset-console-port!))

(define (reset-console-port!)
  ;; This is a kludge.  Maybe this method shouldn't be used.
  (let ((new-port (select-console-port)))
    (if (let ((port console-i/o-port))
	  (or (eq? port the-console-port)
	      (eq? port emacs-console-port)))
	(set-console-i/o-port! new-port))
    (do ((cmdl (nearest-cmdl) (cmdl/parent cmdl)))
	((not cmdl))
      (if (let ((port (cmdl/port cmdl)))
	    (or (eq? port the-console-port)
		(eq? port emacs-console-port)))
	  (set-cmdl/port! cmdl new-port)))))

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