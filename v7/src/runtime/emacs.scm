;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/emacs.scm,v 13.47 1987/10/09 14:41:00 cph Rel $
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; GNU Emacs/Scheme Modeline Interface

(declare (usual-integrations))

(define emacs-interface-package
  (make-environment

(define (transmit-signal type)
  (write-char #\Altmode console-output-port)
  (write-char type console-output-port))

(define (transmit-signal-without-gc type)
  (with-interrupts-reduced interrupt-mask-none
    (lambda (old-mask)
      (transmit-signal type))))

(define (emacs-read-start)
  (transmit-signal-without-gc #\s))

(define (emacs-read-finish)
  (transmit-signal-without-gc #\f))

(define (emacs-start-gc)
  (transmit-signal #\b))

(define (emacs-finish-gc state)
  (transmit-signal #\e))

(define (transmit-signal-with-argument type string)
  (with-interrupts-reduced interrupt-mask-none
    (lambda (old-mask)
      (transmit-signal type)
      (write-string string console-output-port)
      (write-char #\Altmode console-output-port))))

(define (emacs-rep-message string)
  (transmit-signal-with-argument #\m string))

(define (emacs-rep-value object)
  (transmit-signal-with-argument #\v (object->string object)))

(define (object->string object)
  (with-output-to-string
    (lambda ()
      (write object))))

(define paranoid-error-hook?
  false)

(define (emacs-error-hook)
  (transmit-signal-without-gc #\z)
  (beep)
  (if paranoid-error-hook?
      (begin
	(transmit-signal-with-argument #\P
"Error! Type ctl-E to enter error loop, anything else to return to top level.")
	(if (not (char-ci=? (emacs-read-char-immediate) #\C-E))
	    (abort-to-previous-driver "Quit!")))))

(define (emacs-rep-prompt level string)
  (transmit-signal-with-argument
   #\p
   (string-append (object->string level)
		  " "
		  (let ((entry (assoc string emacs-rep-prompt-alist)))
		    (if entry
			(cdr entry)
			string)))))

(define emacs-rep-prompt-alist
  '(("]=>" . "[Normal REPL]")
    ("==>" . "[Normal REPL]")
    ("Eval-in-env-->" . "[Normal REPL]")
    ("Bkpt->" . "[Breakpoint REPL]")
    ("Error->" . "[Error REPL]")
    ("Debug-->" . "[Debugger]")
    ("Debugger-->" . "[Debugger REPL]")
    ("Visiting->" . "[Visiting environment]")
    ("Where-->" . "[Environment Inspector]")
    ("Which-->" . "[Task Inspector]")))

(define (emacs-read-char-immediate)
  (define (loop)
    (let ((char (primitive-read-char-immediate)))
      (if (char=? char char:newline)
	  (loop)
	  (begin (emacs-read-finish)
		 char))))
  (emacs-read-start)
  (if (not (primitive-read-char-ready? 0))
      (transmit-signal-without-gc #\c))
  (loop))

(define (emacs-check-and-clean-up-input-channel delete-mode interrupt-char)
  (if (= delete-mode
	 (access until-most-recent-interrupt-character interrupt-system))
      (begin
	(let loop ()
	  (if (not (char=? (primitive-read-char-immediate) #\C-@))
	      (loop)))
	(if (= (char->ascii #\G) interrupt-char)
	    (transmit-signal #\g))))
  true)

(define primitive-read-char-ready?
  (make-primitive-procedure 'TTY-READ-CHAR-READY?))

(define primitive-read-char-immediate
  (make-primitive-procedure 'TTY-READ-CHAR-IMMEDIATE))

(define normal-start-gc (access gc-start-hook gc-statistics-package))
(define normal-finish-gc (access gc-finish-hook gc-statistics-package))
(define normal-rep-message rep-message-hook)
(define normal-rep-prompt rep-prompt-hook)
(define normal-rep-value rep-value-hook)
(define normal-read-start (access read-start-hook console-input-port))
(define normal-read-finish (access read-finish-hook console-input-port))
(define normal-read-char-immediate
  (access tty-read-char-immediate console-input-port))
(define normal-error-hook (access *error-decision-hook* error-system))
(define normal-check-and-clean-up-input-channel
  (access check-and-clean-up-input-channel interrupt-system))

(define (install-emacs-hooks!)
  (set! (access gc-start-hook gc-statistics-package) emacs-start-gc)
  (set! (access gc-finish-hook gc-statistics-package) emacs-finish-gc)
  (set! rep-message-hook emacs-rep-message)
  (set! rep-prompt-hook emacs-rep-prompt)
  (set! rep-value-hook emacs-rep-value)
  (set! (access read-start-hook console-input-port) emacs-read-start)
  (set! (access read-finish-hook console-input-port) emacs-read-finish)
  (set! (access tty-read-char-immediate console-input-port)
	emacs-read-char-immediate)
  (set! (access *error-decision-hook* error-system) emacs-error-hook)
  (set! (access check-and-clean-up-input-channel interrupt-system)
	emacs-check-and-clean-up-input-channel))

(define (install-normal-hooks!)
  (set! (access gc-start-hook gc-statistics-package) normal-start-gc)
  (set! (access gc-finish-hook gc-statistics-package) normal-finish-gc)
  (set! rep-message-hook normal-rep-message)
  (set! rep-prompt-hook normal-rep-prompt)
  (set! rep-value-hook normal-rep-value)
  (set! (access read-start-hook console-input-port) normal-read-start)
  (set! (access read-finish-hook console-input-port) normal-read-finish)
  (set! (access tty-read-char-immediate console-input-port)
	normal-read-char-immediate)
  (set! (access *error-decision-hook* error-system) normal-error-hook)
  (set! (access check-and-clean-up-input-channel interrupt-system)
	normal-check-and-clean-up-input-channel))

(define under-emacs?
  (make-primitive-procedure 'UNDER-EMACS?))

(define (install!)
  ((if (under-emacs?)
       install-emacs-hooks!
       install-normal-hooks!)))

(add-event-receiver! event:after-restore install!)
(install!)

;;; end EMACS-INTERFACE-PACKAGE
))