#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/emacs.scm,v 14.2 1988/07/13 20:09:56 hal Rel $

Copyright (c) 1988 Massachusetts Institute of Technology

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

;;;; GNU Emacs/Scheme Modeline Interface
;;; package: (runtime emacs-interface)

(declare (usual-integrations))

(define-primitives
  tty-read-char-ready?
  tty-read-char-immediate
  (under-emacs? 0))

(define (transmit-signal type)
  (write-char #\Altmode console-output-port)
  (write-char type console-output-port))

(define (transmit-signal-without-gc type)
  (with-absolutely-no-interrupts
   (lambda ()
     (transmit-signal type))))

(define (transmit-signal-with-argument type string)
  (with-absolutely-no-interrupts
   (lambda ()
     (transmit-signal type)
     (write-string string console-output-port)
     (write-char #\Altmode console-output-port))))

(define (object->string object)
  (with-output-to-string
    (lambda ()
      (write object))))

(define (emacs/read-start)
  (transmit-signal-without-gc #\s))

(define (emacs/read-finish)
  (transmit-signal-without-gc #\f))

(define (emacs/gc-start)
  (transmit-signal #\b)
  (normal/gc-start))

(define (emacs/gc-finish start-value space-remaining)
  (transmit-signal #\e)
  (normal/gc-finish start-value space-remaining))

(define (emacs/repl-read repl)
  (if (cmdl/io-to-console? repl)
      (begin
	(transmit-signal-without-gc #\R)
	(let ((s-expression (read console-input-port)))
	  (repl-history/record! (repl/reader-history repl) s-expression)
	  s-expression))
      (normal/repl-read repl)))

(define (emacs/repl-write repl object)
  (if (cmdl/io-to-console? repl)
      (begin
	(repl-history/record! (repl/printer-history repl) object)
	(transmit-signal-with-argument #\v
				       (if (undefined-value? object)
					   ""
					   (object->string object))))
      (normal/repl-write repl object)))

(define (emacs/cmdl-message cmdl string)
  (if (cmdl/io-to-console? cmdl)
      (transmit-signal-with-argument #\m string)
      (normal/cmdl-message cmdl string)))

(define (emacs/cmdl-prompt cmdl prompt)
  (transmit-signal-with-argument
   #\p
   (string-append (object->string (cmdl/level cmdl))
		  " "
		  (let ((entry (assoc prompt cmdl-prompt-alist)))
		    (if entry
			(cdr entry)
			prompt)))))

(define cmdl-prompt-alist
  '(("]=>" . "[Normal REPL]")
    ("==>" . "[Normal REPL]")
    ("Eval-in-env-->" . "[Normal REPL]")
    ("Bkpt->" . "[Breakpoint REPL]")
    ("Error->" . "[Error REPL]")
    ("Debugger-->" . "[Debugger REPL]")
    ("Visiting->" . "[Visiting environment]")
    ("Debug-->" . "[Debugger]")
    ("Where-->" . "[Environment Inspector]")
    ("Which-->" . "[Task Inspector]")))

(define (emacs/error-decision)
  (transmit-signal-without-gc #\z)
  (beep console-output-port)
  (if paranoid-error-decision?
      (begin
	(transmit-signal-with-argument #\P "Error!")
	(abort-to-previous-driver "Quit!"))))

(define paranoid-error-decision?
  false)

(define (emacs/^G-interrupt interrupt-enables)
  (transmit-signal #\g)
  (normal/^G-interrupt interrupt-enables))

(define (emacs/read-char-immediate)
  (emacs/read-start)
  (let ((char (tty-read-char-immediate)))
    (emacs/read-finish)
    char))

(define (emacs/read-command-char cmdl prompt)
  (if (cmdl/io-to-console? cmdl)
      (begin
	(transmit-signal-with-argument
	 #\D
	 (cond ((string=? "Debug-->" prompt) "Scheme-debug")
	       ((string=? "Where-->" prompt) "Scheme-where")
	       ((string=? "Which-->" prompt) "Scheme-which")
	       (else "Scheme")))
	(transmit-signal-without-gc #\o)
	(read-char-internal))
      (normal/read-command-char cmdl prompt)))

(define (emacs/prompt-for-confirmation cmdl prompt)
  (if (cmdl/io-to-console? cmdl)
      (begin
	(transmit-signal-with-argument #\n prompt)
	(char=? #\y (read-char-internal)))
      (normal/prompt-for-confirmation cmdl prompt)))

(define (emacs/prompt-for-expression cmdl prompt)
  (if (cmdl/io-to-console? cmdl)
      (begin
	(transmit-signal-with-argument #\i prompt)
	(read console-input-port))
      (normal/prompt-for-expression cmdl prompt)))

(define (read-char-internal)
  (let ((char (emacs/read-char-immediate)))
    (if (char=? char char:newline)
	(read-char-internal)
	char)))

(define (cmdl/io-to-console? cmdl)
  (and (eq? console-input-port (cmdl/input-port cmdl))
       (eq? console-output-port (cmdl/output-port cmdl))))

(define (emacs/set-working-directory-pathname! pathname)
  (transmit-signal-with-argument #\w (pathname->string pathname)))

(define normal/gc-start)
(define normal/gc-finish)
(define normal/cmdl-message)
(define normal/cmdl-prompt)
(define normal/repl-write)
(define normal/repl-read)
(define normal/read-char-immediate)
(define normal/read-start)
(define normal/read-finish)
(define normal/error-decision)
(define normal/read-command-char)
(define normal/prompt-for-confirmation)
(define normal/prompt-for-expression)
(define normal/^G-interrupt)
(define normal/set-working-directory-pathname!)

(define (initialize-package!)
  (set! normal/gc-start hook/gc-start)
  (set! normal/gc-finish hook/gc-finish)
  (set! normal/cmdl-message hook/cmdl-message)
  (set! normal/cmdl-prompt hook/cmdl-prompt)
  (set! normal/repl-write hook/repl-write)
  (set! normal/repl-read hook/repl-read)
  (set! normal/read-char-immediate hook/read-char-immediate)
  (set! normal/read-start hook/read-start)
  (set! normal/read-finish hook/read-finish)
  (set! normal/error-decision hook/error-decision)
  (set! normal/read-command-char hook/read-command-char)
  (set! normal/prompt-for-confirmation hook/prompt-for-confirmation)
  (set! normal/prompt-for-expression hook/prompt-for-expression)
  (set! normal/^G-interrupt hook/^G-interrupt)
  (set! normal/set-working-directory-pathname!
	hook/set-working-directory-pathname!)
  (add-event-receiver! event:after-restore install!)
  (install!))

(define (install!)
  ((if (under-emacs?)
       install-emacs-hooks!
       install-normal-hooks!)))

(define (install-emacs-hooks!)
  (set! hook/gc-start emacs/gc-start)
  (set! hook/gc-finish emacs/gc-finish)
  (set! hook/cmdl-message emacs/cmdl-message)
  (set! hook/cmdl-prompt emacs/cmdl-prompt)
  (set! hook/repl-write emacs/repl-write)
  (set! hook/repl-read emacs/repl-read)
  (set! hook/read-char-immediate emacs/read-char-immediate)
  (set! hook/read-start emacs/read-start)
  (set! hook/read-finish emacs/read-finish)
  (set! hook/error-decision emacs/error-decision)
  (set! hook/read-command-char emacs/read-command-char)
  (set! hook/prompt-for-confirmation emacs/prompt-for-confirmation)
  (set! hook/prompt-for-expression emacs/prompt-for-expression)
  (set! hook/^G-interrupt emacs/^G-interrupt)
  (set! hook/set-working-directory-pathname!
	emacs/set-working-directory-pathname!))

(define (install-normal-hooks!)
  (set! hook/gc-start normal/gc-start)
  (set! hook/gc-finish normal/gc-finish)
  (set! hook/cmdl-message normal/cmdl-message)
  (set! hook/cmdl-prompt normal/cmdl-prompt)
  (set! hook/repl-write normal/repl-write)
  (set! hook/repl-read normal/repl-read)
  (set! hook/read-char-immediate normal/read-char-immediate)
  (set! hook/read-start normal/read-start)
  (set! hook/read-finish normal/read-finish)
  (set! hook/error-decision normal/error-decision)
  (set! hook/read-command-char normal/read-command-char)
  (set! hook/prompt-for-confirmation normal/prompt-for-confirmation)
  (set! hook/prompt-for-expression normal/prompt-for-expression)
  (set! hook/^G-interrupt normal/^G-interrupt)
  (set! hook/set-working-directory-pathname!
	normal/set-working-directory-pathname!))