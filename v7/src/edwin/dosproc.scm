;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/dosproc.scm,v 1.1 1992/05/12 15:30:35 mhwu Exp $
;;;
;;;	Copyright (c) 1991-92 Massachusetts Institute of Technology
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
;;; NOTE: Parts of this program (Edwin) were created by translation
;;; from corresponding parts of GNU Emacs.  Users should be aware that
;;; the GNU GENERAL PUBLIC LICENSE may apply to these parts.  A copy
;;; of that license should have been included along with this file.
;;;

;;;; Subprocess Support, faked in DOS because too many things depend on this

(declare (usual-integrations))

(define (initialize-processes!)
  (set! edwin-processes '())
  )

(define edwin-processes)

(define-variable exec-path
  "List of directories to search programs to run in subprocesses.
Each element is a string (directory name) or #F (try default directory)."
  '()
  null?)

(define-variable process-connection-type
  "Control type of device used to communicate with subprocesses.
Values are #f to use a pipe, #t for a pty (or pipe if ptys not supported).
Value takes effect when `start-process' is called."
  true
  boolean?)

(define-variable delete-exited-processes
  "True means delete processes immediately when they exit.
False means don't delete them until \\[list-processes] is run."
  true
  boolean?)

(define-structure (process
           	   (constructor %make-process (subprocess name %buffer)))
  (subprocess false read-only true)
  (name false read-only true)
  %buffer
  (mark false)
  (filter false)
  (sentinel false)
  (kill-without-query false)
  (notification-tick (cons false false)))

(define-integrable (process-arguments process)
  (subprocess-arguments (process-subprocess process)))

(define-integrable (process-input-channel process)
  (subprocess-input-channel (process-subprocess process)))

(define-integrable (process-output-channel process)
  (subprocess-output-channel (process-subprocess process)))

(define-integrable (process-status-tick process)
  (subprocess-status-tick (process-subprocess process)))

(define-integrable (process-exit-reason process)
  (subprocess-exit-reason (process-subprocess process)))

(define (process-status process)
  process
  false)

(define (process-runnable? process)
  process
  false)

(define-integrable (process-buffer process)
  process
  false)

(define (set-process-buffer! process buffer)
  process buffer
  false)


(define (start-process name buffer environment program . arguments)
  name buffer environment program arguments
  false)

(define (delete-process process)
  process
  false)

(define (get-process-by-name name)
  name
  false)

(define (get-buffer-process buffer)
  buffer
  false)

(define (buffer-processes buffer)
  buffer
  '())


;;;; Input and Output

(define (process-send-eof process)
  process
  false)

(define (process-send-substring process string start end)
  process string start end
  false)

(define (process-send-string process string)
  process string
  false)

(define (process-send-char process char)
  process char
  false)

(define (accept-process-output) "")

(define (handle-process-status-changes) false)

(define (process-status-message status reason)
  status reason
  "")


;;;; Signals

(define (interrupt-process process group?)
  process group?
  false)

(define (quit-process process group?)
  process group?
  false)

(define (hangup-process process group?)
  process group?
  false)

(define (stop-process process group?)
  process group?
  false)

(define (continue-process process group?)
  process group?
  false)

(define (kill-process process group?)
  process group?
  false)

;;;; LIST-PROCESSES

(define-command list-processes
  "Display a list of all processes.
\(Any processes listed as exited or signalled are actually eliminated
after the listing is made.)"
  ()
  (lambda () '()))


(define (process-arguments->string arguments)
  arguments
  "")

(define (process-list)
  (list-copy edwin-processes))

;;;; Synchronous Subprocesses

(define (run-synchronous-process input-region output-mark directory pty?
				 program . arguments)
  input-region output-mark directory pty? program arguments
  false)


(define (synchronous-process-wait process input-region output-mark)
  process input-region output-mark
  false)


(define (call-with-output-copier process output-mark receiver)
  process output-mark
  (receiver (lambda () false)))

(define (call-with-input-copier process input-region receiver)
  process input-region
  (receiver (lambda () false)))

(define system-call-name
  (condition-accessor condition-type:system-call-error 'SYSTEM-CALL))

(define system-call-error
  (condition-accessor condition-type:system-call-error 'ERROR-TYPE))

(define-command shell-command
  "Execute string COMMAND in inferior shell; display output, if any.
Optional second arg true (prefix arg, if interactive) means
insert output in current buffer after point (leave mark after it)."
  "sShell command\nP"
  (lambda (command insert-at-point?)
    command insert-at-point?
    (message "(Shell command not available)")
    false))

(define-command shell-command-on-region
  "Execute string COMMAND in inferior shell with region as input.
Normally display output (if any) in temp buffer;
Prefix arg means replace the region with it."
  "r\nsShell command on region\nP"
  (lambda (region command replace-region?)
    region command replace-region?
    (message "(Shell command not available)")
    false))

(define (shell-command-pop-up-output generate-output)
  generate-output
  false)

(define (shell-command input-region output-mark directory pty? command)
  input-region output-mark directory pty? command
  false)

;;; These procedures are not specific to the process abstraction.

(define (find-program program default-directory)
  program default-directory
  false)

(define (parse-path-string string)
  string
  false)

(define (process-environment-bind environment . bindings)
  environment bindings
  false)

(define (environment-binding-name binding)
  binding
  false)

(define (find-environment-variable name bindings)
  name bindings
  false)
