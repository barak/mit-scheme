;;; -*-Scheme-*-
;;;
;;;	$Id: intmod.scm,v 1.79 1994/04/22 05:19:43 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989-94 Massachusetts Institute of Technology
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

(define-variable repl-enable-transcript-buffer
  "If true, record input and output from inferior REPLs in transcript buffer.
This flag has effect only when ENABLE-TRANSCRIPT-BUFFER is also true."
  #t
  boolean?)

(define-variable repl-error-decision
  "If true, errors in REPL evaluation force the user to choose an option.
Otherwise, they start a nested error REPL."
  #f
  boolean?)

(define-variable repl-mode-locked
  "If true, user cannot change the mode of REPL and CMDL buffers."
  #t
  boolean?)

(define-variable inferior-repl-write-results
  "If true, results of evaluation commands are written in the REPL buffer.
This includes evaluation of expressions in other buffers.
Otherwise, only evaluation of expressions in the REPL buffer itself do this."
  #t
  boolean?)

(define (call-with-transcript-output-mark buffer procedure)
  (if (and (ref-variable repl-enable-transcript-buffer buffer)
	   (ref-variable enable-transcript-buffer buffer))
      (call-with-transcript-buffer
       (lambda (buffer)
	 (procedure (buffer-end buffer))))
      (procedure false)))

(define-command repl
  "Run an inferior read-eval-print loop (REPL), with I/O through buffer *scheme*.
If buffer exists, just select it; otherwise create it and start REPL.
REPL uses current evaluation environment."
  ()
  (lambda ()
    (select-buffer
     (or (find-buffer initial-buffer-name)
	 (let ((current-buffer (current-buffer)))
	   (let ((environment (evaluation-environment current-buffer)))
	     (let ((buffer (create-buffer initial-buffer-name)))
	       (start-inferior-repl! buffer
				     environment
				     (evaluation-syntax-table current-buffer
							      environment)
				     false)
	       buffer)))))))

(define (start-inferior-repl! buffer environment syntax-table message)
  (set-buffer-major-mode! buffer (ref-mode-object inferior-repl))
  (if (ref-variable repl-mode-locked)
      (buffer-put! buffer 'MAJOR-MODE-LOCKED true))
  (create-thread editor-thread-root-continuation
    (lambda ()
      (let ((port
	     (make-interface-port buffer
				  (let ((thread (current-thread)))
				    (detach-thread thread)
				    thread))))
	(attach-buffer-interface-port! buffer port)
	(fluid-let ((%exit inferior-repl/%exit)
		    (quit inferior-repl/quit))
	  (dynamic-wind
	   (lambda () unspecific)
	   (lambda ()
	     (repl/start (make-repl false
				    port
				    environment
				    syntax-table
				    false
				    `((ERROR-DECISION ,error-decision))
				    user-initial-prompt)
			 (make-init-message message)))
	   (lambda ()
	     (signal-thread-event editor-thread
	       (lambda ()
		 (unwind-inferior-repl-buffer buffer))))))))))

(define (make-init-message message)
  (if message
      (cmdl-message/append cmdl-message/init-inferior message)
      cmdl-message/init-inferior))

(define cmdl-message/init-inferior
  (cmdl-message/active
   (lambda (port)
     port
     (set-working-directory-pathname!
      (buffer-default-directory (port/buffer port))))))

(define (inferior-repl/%exit #!optional integer)
  (exit-current-thread (if (default-object? integer) 0 integer)))

(define (inferior-repl/quit)
  unspecific)

(define (current-repl-buffer buffer)
  (let ((buffer (current-repl-buffer* buffer)))
    (if (not buffer)
	(error "No REPL to evaluate in."))
    buffer))

(define (current-repl-buffer* buffer)
  (if (and buffer (repl-buffer? buffer))
      buffer
      (let ((buffer (current-buffer)))
	(if (buffer-interface-port buffer)
	    buffer
	    (global-repl-buffer)))))

(define (global-repl-buffer)
  (let ((buffers repl-buffers))
    (and (not (null? buffers))
	 (car buffers))))

(define (repl-buffer? buffer)
  (buffer-interface-port buffer))

(define repl-buffers)

(define (initialize-inferior-repls!)
  (set! repl-buffers '())
  unspecific)

(define (wait-for-input port mode ready? level)
  (signal-thread-event editor-thread
    (lambda ()
      (maybe-switch-modes! port mode)
      (let ((buffer (port/buffer port)))
	(define-variable-local-value! buffer
	  (ref-variable-object mode-line-process)
	  (list ": "
		'RUN-LIGHT
		(if (= level 1)
		    ""
		    (string-append " [level: " (number->string level) "]"))))
	(set-run-light! buffer #f))))
  ;; This doesn't do any output, but prods the editor to notice that
  ;; the modeline has changed and a redisplay is needed.
  (inferior-thread-output! (port/output-registration port))
  (do () ((ready? port))
    (suspend-current-thread)))

(define (end-input-wait port)
  (set-run-light! (port/buffer port) #t)
  (signal-thread-event (port/thread port) false))

(define (standard-prompt-spacing port)
  (enqueue-output-operation! port
    (lambda (mark transcript?)
      transcript?
      (if (not (group-start? mark))
	  (guarantee-newlines 2 mark))
      (undo-boundary! mark)
      #t)))

(define (maybe-switch-modes! port mode)
  (let ((buffer (port/buffer port)))
    (let ((mode* (buffer-major-mode buffer)))
      (if (not (eq? mode* mode))
	  (if (or (eq? mode* (ref-mode-object inferior-repl))
		  (eq? mode* (ref-mode-object inferior-cmdl)))
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
  (if (not (memq buffer repl-buffers))
      (set! repl-buffers (append! repl-buffers (list buffer))))
  (buffer-put! buffer 'INTERFACE-PORT port)
  (add-kill-buffer-hook buffer kill-buffer-inferior-repl)
  (define-variable-local-value! buffer
    (ref-variable-object comint-input-ring)
    (port/input-ring port))
  (set-run-light! buffer #f))

(define-integrable (buffer-interface-port buffer)
  (buffer-get buffer 'INTERFACE-PORT))

(define (kill-buffer-inferior-repl buffer)
  (let ((port (buffer-interface-port buffer)))
    (if port
	(let ((thread (port/thread port)))
	  (if (not (thread-dead? thread))
	      (signal-thread-event thread
		(lambda ()
		  (exit-current-thread unspecific)))))))
  (unwind-inferior-repl-buffer buffer))

(define (unwind-inferior-repl-buffer buffer)
  (without-interrupts
   (lambda ()
     (let ((port (buffer-interface-port buffer)))
       (if port
	   (begin
	     (deregister-inferior-thread! (port/output-registration port))
	     (if (eq? buffer (global-run-light-buffer))
		 (set-global-run-light! #f))
	     (set! repl-buffers (delq! buffer repl-buffers))
	     (let ((buffer (global-run-light-buffer)))
	       (if buffer
		   (set-global-run-light! (local-run-light buffer))))
	     (buffer-remove! buffer 'INTERFACE-PORT)))))))

(define (set-run-light! buffer run?)
  (let ((value (if run? "eval" "listen")))
    (if (eq? buffer (global-run-light-buffer))
	(set-global-run-light! value))
    (set-local-run-light! buffer value)))

(define (global-run-light-buffer)
  (and (variable-default-value (ref-variable-object evaluate-in-inferior-repl))
       (global-repl-buffer)))

(define (set-global-run-light! value)
  (set-variable-default-value! (ref-variable-object run-light) value)
  (global-window-modeline-event!))

(define (local-run-light buffer)
  (variable-local-value buffer (ref-variable-object run-light)))

(define (set-local-run-light! buffer value)
  (define-variable-local-value! buffer (ref-variable-object run-light) value)
  (buffer-modeline-event! buffer 'RUN-LIGHT))

(add-variable-assignment-daemon!
 (ref-variable-object evaluate-in-inferior-repl)
 (lambda (buffer variable)
   buffer variable
   (let ((buffer (global-run-light-buffer)))
     (if buffer
	 (set-global-run-light! (local-run-light buffer))
	 (set-global-run-light! #f)))))

(define (error-decision repl condition)
  (if (ref-variable repl-error-decision)
      (let ((port (cmdl/port repl)))
	(if (interface-port? port)
	    (begin
	      (enqueue-output-operation! port
		(lambda (mark transcript?)
		  (if (and (not transcript?)
			   (not (buffer-visible? (mark-buffer mark))))
		      (begin
			(message "Evaluation error in "
				 (buffer-name (mark-buffer mark))
				 " buffer")
			(editor-beep)))
		  #t))
	      (dynamic-wind
	       (lambda () unspecific)
	       (lambda ()
		 (let loop ()
		   (fresh-line port)
		   (write-string
		    ";Type D to debug error, Q to quit back to REP loop: "
		    port)
		   (let ((char (read-command-char port (cmdl/level repl))))
		     (write-char char port)
		     (cond ((char-ci=? char #\d)
			    (fresh-line port)
			    (write-string ";Starting debugger..." port)
			    (enqueue-output-operation! port
			      (lambda (mark transcript?)
				mark
				(if (not transcript?)
				    (start-continuation-browser port
								condition))
				#t)))
			   ((not (char-ci=? char #\q))
			    (beep port)
			    (loop))))))
	       cmdl-interrupt/abort-top-level))))))

;;;; Modes

(define-major-mode inferior-repl scheme "REPL"
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

\\[inferior-cmdl-abort-top-level] returns to top level.
\\[inferior-cmdl-abort-previous] goes up one level."
  (lambda (buffer)
    (event-distributor/invoke! (ref-variable inferior-repl-mode-hook buffer)
			       buffer)))

(define-variable inferior-repl-mode-hook
  "An event distributor that is invoked when entering Inferior REPL mode."
  (make-event-distributor))

(define-key 'inferior-repl '(#\C-c #\C-b) 'inferior-cmdl-breakpoint)
(define-key 'inferior-repl '(#\C-c #\C-c) 'inferior-cmdl-abort-top-level)
(define-key 'inferior-repl '(#\C-c #\C-u) 'inferior-cmdl-abort-previous)
(define-key 'inferior-repl '(#\C-c #\C-x) 'inferior-cmdl-abort-nearest)

(define-key 'inferior-repl #\M-o 'undefined)
(define-key 'inferior-repl #\M-z 'inferior-repl-eval-defun)
(define-key 'inferior-repl #\C-M-z 'inferior-repl-eval-region)
(define-key 'inferior-repl '(#\C-x #\C-e) 'inferior-repl-eval-last-sexp)

(define-key 'inferior-repl #\M-p 'comint-previous-input)
(define-key 'inferior-repl #\M-n 'comint-next-input)
(define-key 'inferior-repl '(#\C-c #\C-r) 'comint-history-search-backward)
(define-key 'inferior-repl '(#\C-c #\C-s) 'comint-history-search-forward)

(define-key 'inferior-repl '(#\C-c #\C-d) 'inferior-repl-debug)

(define-major-mode inferior-cmdl scheme "CMDL"
  "Major mode for communicating with an inferior command loop.
Like Scheme mode except that the evaluation commands are disabled,
and characters that would normally be self inserting are commands.
Typing ? will show you which characters perform useful functions.

Additionally, these commands abort the command loop:

\\[inferior-cmdl-abort-top-level] returns to the top-level REPL.
\\[inferior-cmdl-abort-previous] returns to the previous level REPL."
  (lambda (buffer)
    (event-distributor/invoke! (ref-variable inferior-cmdl-mode-hook buffer)
			       buffer)))

(define-variable inferior-cmdl-mode-hook
  "An event distributor that is invoked when entering Inferior CMDL mode."
  (make-event-distributor))

(define-key 'inferior-cmdl '(#\C-c #\C-b) 'inferior-cmdl-breakpoint)
(define-key 'inferior-cmdl '(#\C-c #\C-c) 'inferior-cmdl-abort-top-level)
(define-key 'inferior-cmdl '(#\C-c #\C-u) 'inferior-cmdl-abort-previous)
(define-key 'inferior-cmdl '(#\C-c #\C-x) 'inferior-cmdl-abort-nearest)

(define-key 'inferior-cmdl #\M-o 'undefined)
(define-key 'inferior-cmdl #\M-z 'undefined)
(define-key 'inferior-cmdl #\C-M-z 'undefined)
(define-key 'inferior-cmdl '(#\C-x #\C-e) 'undefined)

(define-key 'inferior-cmdl #\M-p 'undefined)
(define-key 'inferior-cmdl #\M-n 'undefined)
(define-key 'inferior-cmdl '(#\C-c #\C-r) 'undefined)
(define-key 'inferior-cmdl '(#\C-c #\C-s) 'undefined)

(define-key 'inferior-cmdl char-set:graphic 'inferior-cmdl-self-insert)

;;;; Commands

(define (interrupt-command interrupt)
  (lambda ()
    (signal-thread-event
	(port/thread (buffer-interface-port (current-repl-buffer #f)))
      interrupt)))

(define-command inferior-cmdl-breakpoint
  "Force the inferior REPL into a breakpoint."
  ()
  (interrupt-command cmdl-interrupt/breakpoint))

(define-command inferior-cmdl-abort-nearest
  "Force the inferior REPL back to the current level."
  ()
  (interrupt-command cmdl-interrupt/abort-nearest))

(define-command inferior-cmdl-abort-previous
  "Force the inferior REPL up to the previous level."
  ()
  (interrupt-command cmdl-interrupt/abort-previous))

(define-command inferior-cmdl-abort-top-level
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

(define (inferior-repl-eval-from-mark mark)
  ((ref-command inferior-repl-eval-region)
   (make-region mark (forward-sexp mark 1 'ERROR))))

(define-command inferior-repl-eval-region
  "Evaluate the region."
  "r"
  (lambda (region)
    (let ((buffer (mark-buffer (region-start region))))
      (comint-record-input (port/input-ring (buffer-interface-port buffer))
			   (region->string region))
      (inferior-repl-eval-region buffer region))))

(define-command inferior-repl-debug
  "Select a debugger buffer to examine the current REPL state.
If this is an error, the debugger examines the error condition."
  ()
  (lambda ()
    (let ((port (buffer-interface-port (current-buffer))))
      (start-continuation-browser
       port
       (let ((object
	      (let ((cmdl (port/inferior-cmdl port)))
		(or (and (repl? cmdl)
			 (repl/condition cmdl))
		    (thread-continuation (port/thread port))))))
	 (if (not object)
	     (editor-error "No error condition to debug."))
	 object)))))

(define (start-continuation-browser port condition)
  ((ref-command browse-continuation) condition)
  (buffer-put! (current-buffer) 'INVOKE-CONTINUATION
    (lambda (continuation arguments)
      (if (not (buffer-alive? (port/buffer port)))
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
	  (apply continuation arguments))))))

(define (buffer/inferior-cmdl buffer)
  (let ((port (buffer-interface-port buffer)))
    (and port
	 (port/inferior-cmdl port))))

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

(define-command inferior-cmdl-self-insert
  "Send this character to the inferior debugger process."
  ()
  (lambda ()
    (let ((port (buffer-interface-port (current-buffer))))
      (set-port/command-char! port (last-command-key))
      (end-input-wait port))))

(define (inferior-repl-eval-region buffer region)
  (inferior-repl-eval-ok? buffer)
  (call-with-transcript-output-mark buffer
    (lambda (mark)
      (if mark
	  (insert-region (region-start region)
			 (region-end region)
			 mark))))
  (let ((port (buffer-interface-port buffer)))
    (move-mark-to! (port/mark port)
		   (let ((end (buffer-end buffer))
			 (end* (region-end region)))
		     (if (mark~ end end*)
			 (begin
			   (set-buffer-point! buffer end*)
			   end*)
			 end)))
    (let ((queue (port/expression-queue port)))
      (bind-condition-handler (list condition-type:error)
	  evaluation-error-handler
	(lambda ()
	  (for-each (let ((context
			   (if (eq? (group-buffer (region-group region))
				    buffer)
			       'REPL-BUFFER
			       'OTHER-BUFFER)))
		      (lambda (expression)
			(enqueue! queue (cons expression context))))
		    (read-expressions-from-region region))))
      (if (not (queue-empty? queue))
	  (end-input-wait port)))))

(define (inferior-repl-eval-expression buffer expression)
  (inferior-repl-eval-ok? buffer)
  (call-with-transcript-output-mark buffer
    (lambda (mark)
      (if mark
	  (insert-string
	   (fluid-let ((*unparse-with-maximum-readability?* true))
	     (write-to-string expression))
	   mark))))
  (let ((port (buffer-interface-port buffer)))
    ;;(move-mark-to! (port/mark port) (buffer-end buffer))
    (enqueue! (port/expression-queue port) (cons expression 'EXPRESSION))
    (end-input-wait port)))

(define (inferior-repl-eval-ok? buffer)
  (let ((mode (buffer-major-mode buffer)))
    (if (not (eq? mode (ref-mode-object inferior-repl)))
	(editor-error
	 (if (eq? mode (ref-mode-object inferior-cmdl))
	     "REPL needs response before evaluation will be enabled."
	     "Can't evaluate -- REPL buffer in anomalous mode.")))))

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
    (set-interrupt-enables! interrupt-mask)
    unspecific))

(define (dequeue! queue empty)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (let ((value (dequeue!/unsafe queue empty)))
      (set-interrupt-enables! interrupt-mask)
      value)))

;;;; Interface Port

(define (make-interface-port buffer thread)
  (letrec
      ((port
	(port/copy interface-port-template
		   (make-interface-port-state
		    thread
		    (mark-right-inserting-copy (buffer-end buffer))
		    (make-ring (ref-variable comint-input-ring-size))
		    (make-queue)
		    false
		    false
		    (make-queue)
		    '()
		    (register-inferior-thread!
		     thread
		     (lambda () (process-output-queue port)))))))
    port))

(define (interface-port? object)
  (and (port? object)
       (interface-port-state? (port/state object))))

(define-structure (interface-port-state (conc-name interface-port-state/))
  (thread false read-only true)
  (mark false read-only true)
  (input-ring false read-only true)
  (expression-queue false read-only true)
  current-queue-element
  command-char
  (output-queue false read-only true)
  output-strings
  (output-registration false read-only true))

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

(define-integrable (port/current-queue-element port)
  (interface-port-state/current-queue-element (port/state port)))

(define-integrable (set-port/current-queue-element! port element)
  (set-interface-port-state/current-queue-element! (port/state port) element))

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

(define-integrable (port/output-registration port)
  (interface-port-state/output-registration (port/state port)))

;;; Output operations

(define (operation/write-char port char)
  (enqueue-output-string! port (string char)))

(define (operation/write-substring port string start end)
  (enqueue-output-string! port (substring string start end)))

(define (operation/fresh-line port)
  (enqueue-output-operation!
   port
   (lambda (mark transcript?) transcript? (guarantee-newline mark) #t)))

(define (operation/beep port)
  (enqueue-output-operation!
   port
   (lambda (mark transcript?) mark (if (not transcript?) (editor-beep)) #t)))

(define (operation/x-size port)
  (let ((buffer (port/buffer port)))
    (and buffer
	 (let ((windows (buffer-windows buffer)))
	   (and (not (null? windows))
		(apply min (map window-x-size windows)))))))

(define (operation/write-result port expression value hash-number)
  (let ((buffer (port/buffer port))
	(other-buffer?
	 (memq (operation/current-expression-context port expression)
	       '(OTHER-BUFFER EXPRESSION))))
    (if (and other-buffer?
	     (not (ref-variable inferior-repl-write-results buffer)))
	(transcript-write value
			  (and (ref-variable enable-transcript-buffer buffer)
			       (transcript-buffer)))
	(begin
	  (default/write-result port expression value hash-number)
	  (if (and other-buffer? (not (mark-visible? (port/mark port))))
	      (transcript-write value #f))))))

(define (mark-visible? mark)
  (there-exists? (buffer-windows (mark-buffer mark))
    (lambda (window)
      (window-mark-visible? window mark))))

(define (enqueue-output-string! port string)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (set-port/output-strings! port (cons string (port/output-strings port)))
    (inferior-thread-output!/unsafe (port/output-registration port))
    (set-interrupt-enables! interrupt-mask)
    unspecific))

(define (enqueue-output-operation! port operator)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (let ((strings (port/output-strings port)))
      (if (not (null? strings))
	  (begin
	    (set-port/output-strings! port '())
	    (enqueue!/unsafe
	     (port/output-queue port)
	     (let ((string (apply string-append (reverse! strings))))
	       (lambda (mark transcript?)
		 transcript?
		 (region-insert-string! mark string)
		 #t))))))
    (enqueue!/unsafe (port/output-queue port) operator)
    (inferior-thread-output!/unsafe (port/output-registration port))
    (set-interrupt-enables! interrupt-mask)
    unspecific))

(define (process-output-queue port)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok))
	(mark (mark-left-inserting-copy (port/mark port)))
	(result #t))
    (call-with-transcript-output-mark (port/buffer port)
      (lambda (transcript-mark)
	(let ((run-operation
	       (lambda (operation mark transcript?)
		 (let ((flag (operation mark transcript?)))
		   (if (eq? flag 'FORCE-RETURN)
		       (set! result flag)))
		 unspecific)))
	  (let loop ()
	    (let ((operation (dequeue!/unsafe (port/output-queue port) false)))
	      (if operation
		  (begin
		    (run-operation operation mark false)
		    (if transcript-mark
			(run-operation operation transcript-mark true))
		    (loop))))))
	(let ((strings (port/output-strings port)))
	  (if (not (null? strings))
	      (begin
		(set-port/output-strings! port '())
		(do ((strings (reverse! strings) (cdr strings)))
		    ((null? strings))
		  (region-insert-string! mark (car strings))
		  (if transcript-mark
		      (region-insert-string! transcript-mark
					     (car strings)))))))))
    (move-mark-to! (port/mark port) mark)
    (mark-temporary! mark)
    (set-interrupt-enables! interrupt-mask)
    result))

;;; Input operations

(define (operation/peek-char port)
  (error "PEEK-CHAR not supported on this port:" port))

(define (operation/read-char port)
  (error "READ-CHAR not supported on this port:" port))

(define (operation/read port parser-table)
  parser-table
  (standard-prompt-spacing port)
  (read-expression port (nearest-cmdl/level)))

(define read-expression
  (let ((empty (cons '() '())))
    (lambda (port level)
      (let ((queue (port/expression-queue port))
	    (mode (ref-mode-object inferior-repl))
	    (ready?
	     (lambda (port)
	       (not (queue-empty? (port/expression-queue port))))))
	(let loop ()
	  (let ((element (dequeue! queue empty)))
	    (if (eq? element empty)
		(begin
		  (wait-for-input port mode ready? level)
		  (loop))
		(begin
		  (set-port/current-queue-element! port element)
		  (car element)))))))))

(define (operation/current-expression-context port expression)
  (let ((element (port/current-queue-element port)))
    (and (pair? element)
	 (eq? (car element) expression)
	 (cdr element))))

;;; Debugger

(define (operation/debugger-failure port string)
  (enqueue-output-operation! port
    (lambda (mark transcript?)
      mark
      (if (not transcript?)
	  (begin
	    (message string)
	    (editor-beep)))
      #t)))

(define (operation/debugger-message port string)
  (enqueue-output-operation!
   port
   (lambda (mark transcript?)
     mark
     (if (not transcript?) (message string))
     #t)))

(define (operation/debugger-presentation port thunk)
  (fresh-line port)
  (thunk))

;;; Prompting

(define (operation/prompt-for-expression port prompt)
  (unsolicited-prompt port prompt-for-expression prompt))

(define (operation/prompt-for-confirmation port prompt)
  (unsolicited-prompt port prompt-for-confirmation? prompt))

(define unsolicited-prompt
  (let ((wait-value (list false))
	(abort-value (list false)))
    (lambda (port procedure prompt)
      (let ((value wait-value))
	(signal-thread-event editor-thread
	  (lambda ()
	    ;; This would be even better if it could notify the user
	    ;; that the inferior REPL wanted some attention.
	    (when-buffer-selected (port/buffer port)
	      (lambda ()
		;; We're using ENQUEUE-OUTPUT-OPERATION! here solely
		;; to force KEYBOARD-READ to exit so that the command
		;; reader loop will get control and notice the command
		;; override.
		(enqueue-output-operation! port
		  (lambda (mark transcript?)
		    mark transcript?
		    (if (not transcript?)
			(override-next-command!
			 (lambda ()
			   (let ((continue
				  (lambda (v)
				    (set! value v)
				    (signal-thread-event (port/thread port)
				      #f))))
			     (bind-condition-handler
				 (list condition-type:abort-current-command)
				 (lambda (condition)
				   (continue abort-value)
				   (signal-condition condition))
			       (lambda ()
				 (continue (procedure prompt))))))))
		    'FORCE-RETURN))))))
	(let loop ()
	  (cond ((eq? value wait-value) (suspend-current-thread) (loop))
		((eq? value abort-value) (abort->nearest))
		(else value)))))))

(define (when-buffer-selected buffer thunk)
  (if (current-buffer? buffer)
      (thunk)
      (letrec
	  ((hook
	    (lambda (buffer)
	      (thunk)
	      (remove-select-buffer-hook buffer hook))))
	(add-select-buffer-hook buffer hook))))

(define (operation/prompt-for-command-expression port prompt level)
  (parse-command-prompt port prompt)
  (read-expression port level))

(define (operation/prompt-for-command-char port prompt level)
  (parse-command-prompt port prompt)
  (read-command-char port level))

(define (read-command-char port level)
  (set-port/command-char! port false)
  (wait-for-input port (ref-mode-object inferior-cmdl) port/command-char level)
  (port/command-char port))

(define (parse-command-prompt port prompt)
  (standard-prompt-spacing port)
  (if (not (and suppress-standard-prompts?
		(or (string=? prompt user-initial-prompt)
		    (member prompt standard-prompts))))
      (begin
	(write-string prompt port)
	(write-char #\space port))))

(define suppress-standard-prompts? #t)
(define standard-prompts
  '("]=> "
    "error> "
    "break> "
    "bkpt> "
    "debug> "
    "where> "))

;;; Miscellaneous

(define (operation/set-default-directory port directory)
  (enqueue-output-operation! port
    (lambda (mark transcript?)
      (if (not transcript?)
	  (begin
	    (set-buffer-default-directory! (mark-buffer mark) directory)
	    ;;(message (->namestring directory))
	    ))
      #t)))

(define (operation/set-default-environment port environment)
  (enqueue-output-operation! port
    (lambda (mark transcript?)
      (if (not transcript?)
	  (define-variable-local-value! (mark-buffer mark)
	    (ref-variable-object scheme-environment)
	    environment))
      #t)))

(define (operation/set-default-syntax-table port syntax-table)
  (enqueue-output-operation! port
    (lambda (mark transcript?)
      (if (not transcript?)
	  (define-variable-local-value! (mark-buffer mark)
	    (ref-variable-object scheme-syntax-table)
	    syntax-table))
      #t)))

(define interface-port-template
  (make-i/o-port
   `((WRITE-CHAR ,operation/write-char)
     (WRITE-SUBSTRING ,operation/write-substring)
     (FRESH-LINE ,operation/fresh-line)
     (BEEP ,operation/beep)
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
     (READ ,operation/read)
     (CURRENT-EXPRESSION-CONTEXT ,operation/current-expression-context)
     (WRITE-RESULT ,operation/write-result))
   #f))