;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/intmod.scm,v 1.42 1992/02/08 15:23:37 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989-92 Massachusetts Institute of Technology
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

;;;; Inferior REPL Mode
;;; Package: (edwin inferior-repl)

(declare (usual-integrations))

(define-command repl
  "Run an inferior read-eval-print loop (REPL), with I/O through buffer *repl*.
If buffer exists, just select it; otherwise create it and start REPL.
REPL uses current evaluation environment,
but prefix argument means prompt for different environment."
  "P"
  (lambda (argument)
    (select-buffer
     (or (find-buffer initial-buffer-name)
	 (let ((environment (evaluation-environment argument)))
	   (let ((buffer (create-buffer initial-buffer-name)))
	     (start-inferior-repl! buffer
				   environment
				   (evaluation-syntax-table environment)
				   false)
	     buffer))))))

(define (start-inferior-repl! buffer environment syntax-table message)
  (set-buffer-major-mode! buffer (ref-mode-object inferior-repl))
  (set-buffer-default-directory! buffer (working-directory-pathname))
  (add-buffer-initialization!
   buffer
   (lambda ()
     (create-thread
      command-reader-reset-continuation
      (lambda ()
	(let ((thread (current-thread)))
	  (detach-thread thread)
	  (let ((port (make-interface-port buffer thread)))
	    (register-interface-port! port)
	    (attach-buffer-interface-port! buffer port)
	    (with-input-from-port port
	      (lambda ()
		(with-output-to-port port
		  (lambda ()
		    (repl/start (make-repl false
					   port
					   environment
					   syntax-table
					   false
					   '()
					   user-initial-prompt)
				message))))))))))))

(define (initialize-inferior-repls!)
  (set! interface-ports '())
  unspecific)

(define (register-interface-port! port)
  (set! interface-ports
	(system-pair-cons (ucode-type weak-cons) port interface-ports))
  unspecific)

(define (accept-inferior-repl-output/unsafe)
  (let loop ((ports interface-ports) (prev false) (output? false))
    (if (null? ports)
	output?
	(let ((port (system-pair-car ports))
	      (next (system-pair-cdr ports)))
	  (cond ((not port)
		 (if prev
		     (system-pair-set-cdr! prev next)
		     (set! interface-ports next))
		 (loop next prev output?))
		((or (not (null? (port/output-strings port)))
		     (not (queue-empty? (port/output-queue port))))
		 (process-output-queue port)
		 (loop next ports true))
		(else
		 (loop next ports output?)))))))

(define interface-ports)

(define (wait-for-input port level mode)
  (enqueue-output-operation! port
    (lambda (mark)
      (if (not (group-start? mark))
	  (guarantee-newlines 2 mark))
      (undo-boundary! mark)))
  (signal-thread-event editor-thread
    (lambda ()
      (maybe-switch-modes! port mode)
      (let ((buffer (port/buffer port)))
	(define-variable-local-value! buffer
	  (ref-variable-object mode-line-process)
	  (list (string-append ": " (or level "???") " ") 'RUN-LIGHT))
	(set-run-light! buffer false))))
  (suspend-current-thread))

(define (end-input-wait port)
  (set-run-light! (port/buffer port) true)
  (signal-thread-event (port/thread port) false))

(define (maybe-switch-modes! port mode)
  (let ((buffer (port/buffer port)))
    (let ((mode* (buffer-major-mode buffer)))
      (if (not (eq? mode* mode))
	  (if (or (eq? mode* (ref-mode-object inferior-repl))
		  (eq? mode* (ref-mode-object inferior-debugger)))
	      ;; Modes are compatible, so no need to reset the buffer's
	      ;; variables and properties.
	      (begin
		(without-interrupts
		 (lambda ()
		   (set-car! (buffer-modes buffer) mode)
		   (set-buffer-comtabs! buffer (mode-comtabs mode))))
		(buffer-modeline-event! buffer 'BUFFER-MODES))
	      (begin
		(set-buffer-major-mode! buffer mode)
		(attach-buffer-interface-port! buffer port)))))))

(define (attach-buffer-interface-port! buffer port)
  (buffer-put! buffer 'INTERFACE-PORT port)
  (define-variable-local-value! buffer
    (ref-variable-object comint-input-ring)
    (port/input-ring port))
  (set-run-light! buffer false))

(define (set-run-light! buffer run?)
  (define-variable-local-value! buffer (ref-variable-object run-light)
    (if run? "run" "listen"))
  (buffer-modeline-event! buffer 'RUN-LIGHT))

(define-integrable (buffer-interface-port buffer)
  (buffer-get buffer 'INTERFACE-PORT))

(define (kill-buffer-inferior-repl buffer)
  (let ((port (buffer-interface-port buffer)))
    (if port
	(begin
	  (signal-thread-event (port/thread port)
	    (lambda ()
	      (exit-current-thread unspecific)))
	  (buffer-remove! buffer 'INTERFACE-PORT)))))

;;;; Modes

(define-major-mode inferior-repl scheme "Inferior REPL"
  "Major mode for communicating with an inferior read-eval-print loop (REPL).
Editing and evaluation commands are like Scheme mode:

\\[inferior-repl-eval-last-sexp] evaluates the expression preceding point.
\\[inferior-repl-eval-defun] evaluates the current definition.
\\[inferior-repl-eval-region] evaluates the current region.
C-g aborts any evaluation.

Expressions submitted for evaluation are saved in an expression history.
The history may be accessed with the following commands:

\\[comint-previous-input] cycles backwards through the history;
\\[comint-next-input] cycles forwards.
\\[comint-history-search-backward] searches backwards for a matching string;
\\[comint-history-search-forward] searches forwards.

The REPL may be controlled by the following commands:

\\[inferior-repl-abort-top-level] returns to top level.
\\[inferior-repl-abort-previous] goes up one level.")

(define-key 'inferior-repl '(#\C-c #\C-b) 'inferior-repl-breakpoint)
(define-key 'inferior-repl '(#\C-c #\C-c) 'inferior-repl-abort-top-level)
(define-key 'inferior-repl '(#\C-c #\C-u) 'inferior-repl-abort-previous)
(define-key 'inferior-repl '(#\C-c #\C-x) 'inferior-repl-abort-nearest)

(define-key 'inferior-repl #\M-o 'undefined)
(define-key 'inferior-repl #\M-z 'inferior-repl-eval-defun)
(define-key 'inferior-repl #\C-M-z 'inferior-repl-eval-region)
(define-key 'inferior-repl '(#\C-x #\C-e) 'inferior-repl-eval-last-sexp)

(define-key 'inferior-repl #\M-p 'comint-previous-input)
(define-key 'inferior-repl #\M-n 'comint-next-input)
(define-key 'inferior-repl '(#\C-c #\C-r) 'comint-history-search-backward)
(define-key 'inferior-repl '(#\C-c #\C-s) 'comint-history-search-forward)

(define-key 'inferior-repl '(#\C-c #\C-d) 'inferior-repl-debug)

(define-major-mode inferior-debugger scheme "Inferior Debugger"
  "Major mode for communicating with an inferior debugger.
Like Scheme mode except that the evaluation commands are disabled,
and characters that would normally be self inserting are debugger commands.
Typing ? will show you which characters perform useful functions.

Additionally, these commands abort the debugger:

\\[inferior-repl-abort-top-level] returns to the top-level REPL.
\\[inferior-repl-abort-previous] returns to the previous level REPL.")

(define-key 'inferior-debugger '(#\C-c #\C-b) 'inferior-repl-breakpoint)
(define-key 'inferior-debugger '(#\C-c #\C-c) 'inferior-repl-abort-top-level)
(define-key 'inferior-debugger '(#\C-c #\C-u) 'inferior-repl-abort-previous)
(define-key 'inferior-debugger '(#\C-c #\C-x) 'inferior-repl-abort-nearest)

(define-key 'inferior-debugger #\M-o 'undefined)
(define-key 'inferior-debugger #\M-z 'undefined)
(define-key 'inferior-debugger #\C-M-z 'undefined)
(define-key 'inferior-debugger '(#\C-x #\C-e) 'undefined)

(define-key 'inferior-debugger #\M-p 'undefined)
(define-key 'inferior-debugger #\M-n 'undefined)
(define-key 'inferior-debugger '(#\C-c #\C-r) 'undefined)
(define-key 'inferior-debugger '(#\C-c #\C-s) 'undefined)

(define-key 'inferior-debugger char-set:graphic 'inferior-debugger-self-insert)

;;;; Commands

(define (interrupt-command interrupt)
  (lambda ()
    (signal-thread-event (port/thread (buffer-interface-port (current-buffer)))
      interrupt)))

(define-command inferior-repl-breakpoint
  "Force the inferior REPL into a breakpoint."
  ()
  (interrupt-command cmdl-interrupt/breakpoint))

(define-command inferior-repl-abort-nearest
  "Force the inferior REPL back to the current level."
  ()
  (interrupt-command cmdl-interrupt/abort-nearest))

(define-command inferior-repl-abort-previous
  "Force the inferior REPL up to the previous level."
  ()
  (interrupt-command cmdl-interrupt/abort-previous))

(define-command inferior-repl-abort-top-level
  "Force the inferior REPL up to top level."
  ()
  (interrupt-command cmdl-interrupt/abort-top-level))

(define-command inferior-repl-eval-defun
  "Evaluate defun that point is in or before."
  ()
  (lambda ()
    (inferior-repl-eval-from-mark (current-definition-start))))

(define-command inferior-repl-eval-last-sexp
  "Evaluate the expression preceding point."
  ()
  (lambda ()
    (inferior-repl-eval-from-mark (backward-sexp (current-point) 1 'ERROR))))

(define-command inferior-repl-eval-region
  "Evaluate the region."
  "r"
  (lambda (region)
    (inferior-repl-eval-region (region-start region) (region-end region))))

(define-command inferior-repl-debug
  "Select a debugger buffer to examine the current REPL state.
If this is an error, the debugger examines the error condition."
  ()
  (lambda ()
    (let ((buffer (current-buffer)))
      (let ((port (buffer-interface-port buffer)))
	(let ((browser
	       (continuation-browser
		(or (let ((cmdl (port/inferior-cmdl port)))
		      (and (repl? cmdl)
			   (repl/condition cmdl)))
		    (thread-continuation (port/thread port))))))
	  (buffer-put! browser 'INVOKE-CONTINUATION
	    (lambda (continuation arguments)
	      (if (not (buffer-alive? buffer))
		  (editor-error
		   "Can't continue; REPL buffer no longer exists!"))
	      (signal-thread-event (port/thread port)
		(lambda ()
		  ;; This call to UNBLOCK-THREAD-EVENTS is a kludge.
		  ;; The continuation should be able to decide whether
		  ;; or not to unblock, but that isn't so right now.
		  ;; As a default, having them unblocked is better
		  ;; than having them blocked.
		  (unblock-thread-events)
		  (apply continuation arguments)))))
	  (select-buffer browser))))))

(define (port/inferior-cmdl port)
  (let ((thread (current-thread))
	(cmdl false))
    (signal-thread-event (port/thread port)
      (lambda ()
	(set! cmdl (nearest-cmdl))
	(signal-thread-event thread false)))
    (do () (cmdl)
      (suspend-current-thread))
    cmdl))

(define-command inferior-debugger-self-insert
  "Send this character to the inferior debugger process."
  ()
  (lambda ()
    (let ((port (buffer-interface-port (current-buffer))))
      (set-port/command-char! port (last-command-key))
      (end-input-wait port))))

(define (inferior-repl-eval-from-mark mark)
  (inferior-repl-eval-region mark (forward-sexp mark 1 'ERROR)))

(define (inferior-repl-eval-region start end)
  (let ((buffer (mark-buffer start)))
    (let ((port (buffer-interface-port buffer)))
      (set-buffer-point! buffer end)
      (move-mark-to! (port/mark port) end)
      (ring-push! (port/input-ring port) (extract-string start end))
      (let ((queue (port/expression-queue port)))
	(let ((input-port (make-buffer-input-port start end)))
	  (bind-condition-handler (list condition-type:error)
	      evaluation-error-handler
	    (lambda ()
	      (let loop ()
		(let ((sexp (read input-port)))
		  (if (not (eof-object? sexp))
		      (begin
			(enqueue! queue sexp)
			(loop))))))))
	(if (not (queue-empty? queue))
	    (end-input-wait port))))))

;;;; Queue

(define-integrable (make-queue)
  (cons '() '()))

(define-integrable (queue-empty? queue)
  (null? (car queue)))

(declare (integrate-operator enqueue!/unsafe dequeue!/unsafe))

(define (enqueue!/unsafe queue object)
  (let ((next (cons object '())))
    (if (null? (cdr queue))
	(set-car! queue next)
	(set-cdr! (cdr queue) next))
    (set-cdr! queue next)))

(define (dequeue!/unsafe queue empty)
  (let ((this (car queue)))
    (if (null? this)
	empty
	(begin
	  (set-car! queue (cdr this))
	  (if (null? (cdr this))
	      (set-cdr! queue '()))
	  (car this)))))

(define (enqueue! queue object)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (enqueue!/unsafe queue object)
    (set-interrupt-enables! interrupt-mask)))

(define (dequeue! queue empty)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (let ((value (dequeue!/unsafe queue empty)))
      (set-interrupt-enables! interrupt-mask)
      value)))

;;;; Interface Port

(define (make-interface-port buffer thread)
  (port/copy interface-port-template
	     (make-interface-port-state
	      thread
	      (mark-left-inserting-copy (buffer-end buffer))
	      (make-ring (ref-variable comint-input-ring-size))
	      (make-queue)
	      false
	      (make-queue)
	      '())))

(define-structure (interface-port-state (conc-name interface-port-state/))
  (thread false read-only true)
  (mark false read-only true)
  (input-ring false read-only true)
  (expression-queue false read-only true)
  command-char
  (output-queue false read-only true)
  output-strings)

(define-integrable (port/thread port)
  (interface-port-state/thread (port/state port)))

(define-integrable (port/mark port)
  (interface-port-state/mark (port/state port)))

(define-integrable (port/buffer port)
  (mark-buffer (port/mark port)))

(define-integrable (port/input-ring port)
  (interface-port-state/input-ring (port/state port)))

(define-integrable (port/expression-queue port)
  (interface-port-state/expression-queue (port/state port)))

(define-integrable (port/command-char port)
  (interface-port-state/command-char (port/state port)))

(define-integrable (set-port/command-char! port command-char)
  (set-interface-port-state/command-char! (port/state port) command-char))

(define-integrable (port/output-queue port)
  (interface-port-state/output-queue (port/state port)))

(define-integrable (port/output-strings port)
  (interface-port-state/output-strings (port/state port)))

(define-integrable (set-port/output-strings! port strings)
  (set-interface-port-state/output-strings! (port/state port) strings))

;;; Output operations

(define (operation/write-char port char)
  (enqueue-output-string! port (string char)))

(define (operation/write-substring port string start end)
  (enqueue-output-string! port (substring string start end)))

(define (operation/fresh-line port)
  (enqueue-output-operation! port guarantee-newline))

(define (operation/x-size port)
  (let ((buffer (port/buffer port)))
    (and buffer
	 (let ((windows (buffer-windows buffer)))
	   (and (not (null? windows))
		(apply min (map window-x-size windows)))))))

(define (enqueue-output-string! port string)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (set-port/output-strings! port (cons string (port/output-strings port)))
    (set! inferior-thread-changes? true)
    (set-interrupt-enables! interrupt-mask)))

(define (enqueue-output-operation! port operator)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (let ((strings (port/output-strings port)))
      (if (not (null? strings))
	  (begin
	    (set-port/output-strings! port '())
	    (enqueue!/unsafe
	     (port/output-queue port)
	     (let ((string (apply string-append (reverse! strings))))
	       (lambda (mark)
		 (region-insert-string! mark string)))))))
    (enqueue!/unsafe (port/output-queue port) operator)
    (set! inferior-thread-changes? true)
    (set-interrupt-enables! interrupt-mask)))

(define (process-output-queue port)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok))
	(mark (port/mark port)))
    (let loop ()
      (let ((operation (dequeue!/unsafe (port/output-queue port) false)))
	(if operation
	    (begin
	      (operation mark)
	      (loop)))))
    (let ((strings (port/output-strings port)))
      (if (not (null? strings))
	  (begin
	    (set-port/output-strings! port '())
	    (do ((strings (reverse! strings) (cdr strings)))
		((null? strings))
	      (region-insert-string! mark (car strings))))))
    (set-interrupt-enables! interrupt-mask)))

;;; Input operations

(define (operation/peek-char port)
  (error "PEEK-CHAR not supported on this port:" port))

(define (operation/read-char port)
  (error "READ-CHAR not supported on this port:" port))

(define (operation/read port parser-table)
  parser-table
  (read-expression port (number->string (nearest-cmdl/level))))

(define read-expression
  (let ((empty (cons '() '())))
    (lambda (port level)
      (let loop ()
	(let ((expression (dequeue! (port/expression-queue port) empty)))
	  (if (eq? expression empty)
	      (begin
		(wait-for-input port level (ref-mode-object inferior-repl))
		(loop))
	      expression))))))

;;; Debugger

(define (operation/debugger-failure port string)
  (enqueue-output-operation! port
    (lambda (mark)
      mark
      (message string)
      (editor-beep))))

(define (operation/debugger-message port string)
  (enqueue-output-operation! port (lambda (mark) mark (message string))))

(define (operation/debugger-presentation port thunk)
  (fresh-line port)
  (thunk))

;;; Prompting

(define (operation/prompt-for-expression port prompt)
  (unsolicited-prompt port prompt-for-expression prompt))

(define (operation/prompt-for-confirmation port prompt)
  (unsolicited-prompt port prompt-for-confirmation prompt))

(define unsolicited-prompt
  (let ((unique (list false)))
    (lambda (port procedure prompt)
      (let ((value unique))
	(signal-thread-event editor-thread
	  (lambda ()
	    ;; This is unlikely to work.  We've got to get a better
	    ;; mechanism to handle this kind of stuff.
	    (override-next-command!
	     (lambda ()
	       (set! value
		     (cleanup-pop-up-buffers
		      (lambda ()
			(let ((buffer (port/buffer port)))
			  (if (not (buffer-visible? buffer))
			      (pop-up-buffer buffer false)))
			(procedure prompt))))
	       (signal-thread-event (port/thread port) false)))))
	(do () ((not (eq? value unique)))
	  (suspend-current-thread))
	value))))

(define (operation/prompt-for-command-expression port prompt)
  (read-expression port (parse-command-prompt prompt)))

(define (operation/prompt-for-command-char port prompt)
  (set-port/command-char! port false)
  (let ((level (parse-command-prompt prompt))
	(mode (ref-mode-object inferior-debugger)))
    (let loop ()
      (wait-for-input port level mode)
      (or (port/command-char port)
	  (loop)))))

(define (parse-command-prompt prompt)
  (and (re-match-string-forward (re-compile-pattern "\\([0-9]+\\) " false)
				false false prompt)
       (substring prompt
		  (re-match-start-index 1)
		  (re-match-end-index 1))))

;;; Miscellaneous

(define (operation/set-default-directory port directory)
  (enqueue-output-operation! port
    (lambda (mark)
      (set-buffer-default-directory! (mark-buffer mark) directory)
      (message (->namestring directory)))))

(define (operation/set-default-environment port environment)
  (enqueue-output-operation! port
    (lambda (mark)
      (define-variable-local-value! (mark-buffer mark)
	(ref-variable-object scheme-environment)
	environment))))

(define (operation/set-default-syntax-table port syntax-table)
  (enqueue-output-operation! port
    (lambda (mark)
      (define-variable-local-value! (mark-buffer mark)
	(ref-variable-object scheme-syntax-table)
	syntax-table))))

(define interface-port-template
  (make-i/o-port
   `((WRITE-CHAR ,operation/write-char)
     (WRITE-SUBSTRING ,operation/write-substring)
     (FRESH-LINE ,operation/fresh-line)
     (X-SIZE ,operation/x-size)
     (DEBUGGER-FAILURE ,operation/debugger-failure)
     (DEBUGGER-MESSAGE ,operation/debugger-message)
     (DEBUGGER-PRESENTATION ,operation/debugger-presentation)
     (PROMPT-FOR-EXPRESSION ,operation/prompt-for-expression)
     (PROMPT-FOR-CONFIRMATION ,operation/prompt-for-confirmation)
     (PROMPT-FOR-COMMAND-EXPRESSION ,operation/prompt-for-command-expression)
     (PROMPT-FOR-COMMAND-CHAR ,operation/prompt-for-command-char)
     (SET-DEFAULT-DIRECTORY ,operation/set-default-directory)
     (SET-DEFAULT-ENVIRONMENT ,operation/set-default-environment)
     (SET-DEFAULT-SYNTAX-TABLE ,operation/set-default-syntax-table)
     (PEEK-CHAR ,operation/peek-char)
     (READ-CHAR ,operation/read-char)
     (READ ,operation/read))
   false))