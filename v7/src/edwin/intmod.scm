;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/intmod.scm,v 1.40 1991/11/26 08:03:18 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989-91 Massachusetts Institute of Technology
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
	   (start-inferior-repl! (create-buffer initial-buffer-name)
				 environment
				 (evaluation-syntax-table environment)
				 false))))))

(define (start-inferior-repl! buffer environment syntax-table message)
  (set-buffer-major-mode! buffer (ref-mode-object inferior-repl))
  (let ((port (make-interface-port buffer)))
    (attach-buffer-interface-port! buffer port)
    (set-port/inferior-continuation! port command-reader-reset-continuation)
    (add-buffer-initialization!
     buffer
     (lambda ()
       (set-buffer-default-directory! buffer (working-directory-pathname))
       (within-inferior port
	 (lambda ()
	   (fluid-let ((*^G-interrupt-handler* cmdl-interrupt/abort-nearest))
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
				 message))))))))))
    buffer))

(define (within-inferior port thunk)
  (without-interrupts
   (lambda ()
     (set-run-light! port true)
     (update-screens! false)
     (call-with-current-continuation
      (lambda (continuation)
	(set-port/editor-continuation! port continuation)
	(let ((continuation (port/inferior-continuation port)))
	  (set-port/inferior-continuation! port false)
	  (within-continuation continuation thunk)))))))

(define (within-editor port thunk)
  (call-with-current-continuation
   (lambda (continuation)
     (without-interrupts
      (lambda ()
	(set-port/inferior-continuation! port continuation)
	(let ((continuation (port/editor-continuation port)))
	  (set-port/editor-continuation! port false)
	  (within-continuation continuation
	    (lambda ()
	      (set-run-light! port false)
	      (thunk)))))))))

(define (invoke-inferior port result)
  (within-inferior port (lambda () result)))

(define (within-editor-temporarily port thunk)
  (within-editor port
    (lambda ()
      (invoke-inferior port (thunk)))))

(define (return-to-editor port level mode)
  (within-editor port
    (lambda ()
      (process-output-queue port)
      (maybe-switch-modes! port mode)
      (add-buffer-initialization! (port/buffer port)
	(lambda ()
	  (local-set-variable! mode-line-process
			       (list (string-append ": " (or level "???") " ")
				     'RUN-LIGHT))))
      (let ((mark (port/mark port)))
	(if (not (group-start? mark))
	    (guarantee-newlines 2 mark))))))

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
  (add-buffer-initialization! buffer
    (lambda ()
      (local-set-variable! comint-input-ring (port/input-ring port))
      (set-run-light! port false))))

(define-integrable (buffer-interface-port buffer)
  (buffer-get buffer 'INTERFACE-PORT))

(define (set-run-light! port run?)
  (let ((buffer (port/buffer port)))
    (define-variable-local-value! buffer (ref-variable-object run-light)
      (if run? "run" "listen"))
    (buffer-modeline-event! buffer 'RUN-LIGHT)))

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
    (within-inferior (buffer-interface-port (current-buffer)) interrupt)))

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
		    (port/inferior-continuation port)))))
	  (buffer-put! browser 'INVOKE-CONTINUATION
	    (lambda (continuation arguments)
	      (if (not (buffer-alive? buffer))
		  (editor-error
		   "Can't continue; REPL buffer no longer exists!"))
	      (select-buffer buffer)
	      (within-continuation *command-continuation*
		(lambda ()
		  (within-inferior port
		    (lambda ()
		      (apply continuation arguments)))
		  'ABORT))))
	  (select-buffer browser))))))

(define (port/inferior-cmdl port)
  (call-with-current-continuation
   (lambda (continuation)
     (within-continuation (port/inferior-continuation port)
       (lambda ()
	 (continuation (nearest-cmdl)))))))

(define-command inferior-debugger-self-insert
  "Send this character to the inferior debugger process."
  ()
  (lambda ()
    (invoke-inferior (buffer-interface-port (current-buffer))
		     (last-command-key))))

;;;; Evaluation

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
	(let ((empty (cons '() '())))
	  (let ((expression (dequeue! queue empty)))
	    (if (not (eq? expression empty))
		(invoke-inferior port expression))))))))

(define (dequeue! queue empty)
  (without-interrupts
   (lambda ()
     (if (queue-empty? queue)
	 empty
	 (dequeue!/unsafe queue)))))

;;;; Interface Port

(define (make-interface-port buffer)
  (port/copy interface-port-template
	     (make-interface-port-state
	      (mark-left-inserting-copy (buffer-end buffer))
	      (make-ring (ref-variable comint-input-ring-size))
	      (make-queue)
	      (make-queue)
	      '()
	      false
	      false)))

(define-structure (interface-port-state (conc-name interface-port-state/))
  (mark false read-only true)
  (input-ring false read-only true)
  (expression-queue false read-only true)
  (output-queue false read-only true)
  output-strings
  editor-continuation
  inferior-continuation)

(define-integrable (port/mark port)
  (interface-port-state/mark (port/state port)))

(define-integrable (port/buffer port)
  (mark-buffer (port/mark port)))

(define-integrable (port/input-ring port)
  (interface-port-state/input-ring (port/state port)))

(define-integrable (port/expression-queue port)
  (interface-port-state/expression-queue (port/state port)))

(define-integrable (port/output-queue port)
  (interface-port-state/output-queue (port/state port)))

(define-integrable (port/output-strings port)
  (interface-port-state/output-strings (port/state port)))

(define-integrable (set-port/output-strings! port strings)
  (set-interface-port-state/output-strings! (port/state port) strings))

(define-integrable (port/editor-continuation port)
  (interface-port-state/editor-continuation (port/state port)))

(define-integrable (set-port/editor-continuation! port continuation)
  (set-interface-port-state/editor-continuation! (port/state port)
						 continuation))

(define-integrable (port/inferior-continuation port)
  (interface-port-state/inferior-continuation (port/state port)))

(define-integrable (set-port/inferior-continuation! port continuation)
  (set-interface-port-state/inferior-continuation! (port/state port)
						   continuation))

;;; Output operations

(define (operation/write-char port char)
  (set-port/output-strings! port
			    (cons (string char)
				  (port/output-strings port))))

(define (operation/write-substring port string start end)
  (set-port/output-strings! port
			    (cons (substring string start end)
				  (port/output-strings port))))

(define (process-output-queue port)
  (synchronize-output port)
  (let ((queue (port/output-queue port))
	(mark (port/mark port)))
    (let loop ()
      (let ((operation (dequeue! queue false)))
	(if operation
	    (begin
	      (operation mark)
	      (loop)))))))

(define (operation/fresh-line port)
  (enqueue-output-operation! port guarantee-newline))

(define (enqueue-output-operation! port operator)
  (synchronize-output port)
  (enqueue! (port/output-queue port) operator))

(define (synchronize-output port)
  (without-interrupts
   (lambda ()
     (let ((strings (port/output-strings port)))
       (set-port/output-strings! port '())
       (if (not (null? strings))
	   (enqueue! (port/output-queue port)
		     (let ((string (apply string-append (reverse! strings))))
		       (lambda (mark)
			 (region-insert-string! mark string)))))))))

(define (operation/x-size port)
  (let ((buffer (port/buffer port)))
    (and buffer
	 (let ((windows (buffer-windows buffer)))
	   (and (not (null? windows))
		(apply min (map window-x-size windows)))))))

;;; Input operations

(define (operation/peek-char port)
  (error "PEEK-CHAR not supported on this port:" port))

(define (operation/read-char port)
  (error "READ-CHAR not supported on this port:" port))

(define (operation/read port parser-table)
  parser-table
  (read-expression port (number->string (nearest-cmdl/level))))

(define (read-expression port level)
  (let ((empty (cons '() '())))
    (let ((expression (dequeue! (port/expression-queue port) empty)))
      (if (eq? expression empty)
	  (return-to-editor port level (ref-mode-object inferior-repl))
	  expression))))

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
  (within-editor-temporarily port
    (lambda ()
      (process-output-queue port)
      (prompt-for-expression prompt))))

(define (operation/prompt-for-confirmation port prompt)
  (within-editor-temporarily port
    (lambda ()
      (process-output-queue port)
      (prompt-for-confirmation prompt))))

(define (operation/prompt-for-command-expression port prompt)
  (read-expression port (parse-command-prompt prompt)))

(define (operation/prompt-for-command-char port prompt)
  (return-to-editor port
		    (parse-command-prompt prompt)
		    (ref-mode-object inferior-debugger)))

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