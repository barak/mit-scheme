;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/comred.scm,v 1.92 1992/02/17 22:08:30 cph Exp $
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

;;;; Command Reader

(declare (usual-integrations))

(define *command-continuation*)	;Continuation of current command
(define *command-key*)		;Key read to find current command
(define *command*)		;The current command
(define *last-command*)		;The previous command, excluding arg commands
(define *command-argument*)	;Argument from last command
(define *next-argument*)	;Argument to next command
(define *command-message*)	;Message from last command
(define *next-message*)		;Message to next command
(define *non-undo-count*)	;# of self-inserts since last undo boundary
(define keyboard-keys-read)	;# of keys read from keyboard
(define command-history)
(define command-history-limit 30)
(define command-reader-override-queue)

(define (initialize-command-reader!)
  (set! keyboard-keys-read 0)
  (set! command-history (make-circular-list command-history-limit false))
  (set! command-reader-override-queue (make-queue))
  unspecific)

(define (top-level-command-reader initialization)
  (let loop ((initialization initialization))
    (with-keyboard-macro-disabled
     (lambda ()
       (intercept-^G-interrupts (lambda () unspecific)
	 (lambda ()
	   (command-reader initialization)))))
    (loop false)))

(define (override-next-command! override)
  (enqueue! command-reader-override-queue override))

(define (abort-current-command #!optional input)
  (keyboard-macro-disable)
  (if (or (default-object? input) (not input))
      (*command-continuation* 'ABORT)
      (within-continuation *command-continuation*
	(lambda ()
	  (cond ((input-event? input)
		 (apply-input-event input))
		((command? input)
		 (execute-command input))
		(else
		 (execute-key (current-comtabs) input)))
	  'ABORT))))

(define-structure (input-event
		   (constructor %make-input-event)
		   (conc-name input-event/))
  (operator false read-only true)
  (operands false read-only true))

(define (make-input-event operator . operands)
  (%make-input-event operator operands))

(define (apply-input-event input-event)
  (if (not (input-event? input-event))
      (error:wrong-type-argument input-event "input event" apply-input-event))
  (clear-message)
  (reset-command-state!)
  (apply (input-event/operator input-event)
	 (input-event/operands input-event)))

(define (command-reader #!optional initialization)
  (fluid-let ((*last-command* false)
	      (*command* false)
	      (*command-argument*)
	      (*next-argument* false)
	      (*command-message*)
	      (*next-message* false)
	      (*non-undo-count* 0)
	      (*command-key* false)
	      (*command-continuation*))
    (bind-condition-handler (list condition-type:editor-error)
	editor-error-handler
      (lambda ()
	(if (and (not (default-object? initialization)) initialization)
	    (call-with-current-continuation
	     (lambda (continuation)
	       (set! *command-continuation* continuation)
	       (reset-command-state!)
	       (initialization))))
	(do () (false)
	  (call-with-current-continuation
	   (lambda (continuation)
	     (set! *command-continuation* continuation)
	     (do () (false)
	       (reset-command-state!)
	       (if (queue-empty? command-reader-override-queue)
		   (let ((input
			  (with-editor-interrupts-disabled keyboard-read)))
		     (if (input-event? input)
			 (apply-input-event input)
			 (begin
			   (set! *command-key* input)
			   (clear-message)
			   (set-command-prompt!
			    (if (not (command-argument))
				(key-name input)
				(string-append-separated
				 (command-argument-prompt)
				 (key-name input))))
			   (let ((window (current-window)))
			     (%dispatch-on-command
			      window
			      (comtab-entry (buffer-comtabs
					     (window-buffer window))
					    input)
			      false)))))
		   ((dequeue! command-reader-override-queue)))))))))))

(define (reset-command-state!)
  (set! *last-command* *command*)
  (set! *command* false)
  (set! *command-argument* *next-argument*)
  (set! *next-argument* false)
  (set! *command-message* *next-message*)
  (set! *next-message* false)
  (if (command-argument)
      (set-command-prompt! (command-argument-prompt))
      (reset-command-prompt!))
  (if *defining-keyboard-macro?*
      (keyboard-macro-finalize-keys)))

(define-integrable (current-command-key)
  *command-key*)

(define (last-command-key)
  (if (key? *command-key*)
      *command-key*
      (car (last-pair *command-key*))))

(define (set-current-command! command)
  (set! *command* command)
  unspecific)

(define-integrable (current-command)
  *command*)

(define-integrable (last-command)
  *last-command*)

(define (set-command-argument! argument mode)
  (set! *next-argument* (cons argument mode))
  ;; Preserve message and last command.
  (set! *next-message* *command-message*)
  (set! *command* *last-command*)
  unspecific)

(define-integrable (command-argument)
  (and *command-argument* (car *command-argument*)))

(define (auto-argument-mode?)
  (and *command-argument* (cdr *command-argument*)))

(define (set-command-message! tag . arguments)
  (set! *next-message* (cons tag arguments))
  unspecific)

(define (command-message-receive tag if-received if-not-received)
  (if (and *command-message*
	   (eq? (car *command-message*) tag))
      (apply if-received (cdr *command-message*))
      (if-not-received)))

(define (command-history-list)
  (let loop ((history command-history))
    (if (car history)
	(let loop ((history (cdr history)) (result (list (car history))))
	  (if (eq? history command-history)
	      result
	      (loop (cdr history) (cons (car history) result))))
	(let ((history (cdr history)))
	  (if (eq? history command-history)
	      '()
	      (loop history))))))

;;; The procedures for executing commands come in two flavors.  The
;;; difference is that the EXECUTE-foo procedures reset the command
;;; state first, while the DISPATCH-ON-foo procedures do not.  The
;;; latter should only be used by "prefix" commands such as C-X or
;;; C-4, since they want arguments, messages, etc. to be passed on.

(define-integrable (execute-key comtab key)
  (reset-command-state!)
  (dispatch-on-key comtab key))

(define-integrable (execute-command command)
  (reset-command-state!)
  (%dispatch-on-command (current-window) command false))

(define (execute-button-command screen button x y)
  (send (screen-root-window screen) ':button-event! button x y))

(define (read-and-dispatch-on-key)
  (dispatch-on-key (current-comtabs)
		   (with-editor-interrupts-disabled keyboard-read)))

(define (dispatch-on-key comtab key)
  (if (input-event? key)
      (apply-input-event key)
      (begin
	(set! *command-key* key)
	(set-command-prompt!
	 (string-append-separated (command-argument-prompt) (xkey->name key)))
	(%dispatch-on-command (current-window)
			      (comtab-entry comtab key)
			      false))))

(define (dispatch-on-command command #!optional record?)
  (%dispatch-on-command (current-window)
			command
			(if (default-object? record?) false record?)))

(define (%dispatch-on-command window command record?)
  (set! *command* command)
  (guarantee-command-loaded command)
  (let ((point (window-point window))
	(point-x (window-point-x window))
	(procedure (command-procedure command)))
    (let ((normal
	   (lambda ()
	     (set! *non-undo-count* 0)
	     (undo-boundary! point)
	     (apply procedure (interactive-arguments command record?)))))
      (cond ((or *executing-keyboard-macro?* *command-argument*)
	     (set! *non-undo-count* 0)
	     (apply procedure (interactive-arguments command record?)))
	    ((window-needs-redisplay? window)
	     (normal))
	    ((and (char? *command-key*)
		  (or (eq? command (ref-command-object self-insert-command))
		      (and (eq? command (ref-command-object auto-fill-space))
			   (not (auto-fill-break? point)))
		      (command-argument-self-insert? command)))
	     (let ((key *command-key*))
	       (if (let ((buffer (window-buffer window)))
		     (and (buffer-auto-save-modified? buffer)
			  (null? (cdr (buffer-windows buffer)))
			  (line-end? point)
			  (char-graphic? key)
			  (fix:< point-x (fix:- (window-x-size window) 1))))
		   (begin
		     (if (fix:< *non-undo-count* 20)
			 (set! *non-undo-count* (fix:+ *non-undo-count* 1))
			 (begin
			   (set! *non-undo-count* 1)
			   (undo-boundary! point)))
		     (window-direct-output-insert-char! window key))
		   (begin
		     (set! *non-undo-count* 0)
		     (undo-boundary! point)
		     (region-insert-char! point key)))))
	    ((eq? command (ref-command-object forward-char))
	     (if (and (not (group-end? point))
		      (char-graphic? (mark-right-char point))
		      (fix:< point-x (fix:- (window-x-size window) 2))
		      (null? (group-move-point-daemons (mark-group point))))
		 (window-direct-output-forward-char! window)
		 (normal)))
	    ((eq? command (ref-command-object backward-char))
	     (if (and (not (group-start? point))
		      (char-graphic? (mark-left-char point))
		      (fix:< 0 point-x)
		      (fix:< point-x (fix:- (window-x-size window) 1))
		      (null? (group-move-point-daemons (mark-group point))))
		 (window-direct-output-backward-char! window)
		 (normal)))
	    (else
	     (normal))))))

(define (interactive-arguments command record?)
  (let ((specification (command-interactive-specification command))
	(record-command-arguments
	 (lambda (arguments)
	   (let ((history command-history))
	     (set-car! history (cons (command-name command) arguments))
	     (set! command-history (cdr history))))))
    (cond ((string? specification)
	   (with-values
	       (lambda ()
		 (let ((end (string-length specification)))
		   (let loop
		       ((index
			 (if (and (not (zero? end))
				  (char=? #\* (string-ref specification 0)))
			     (begin
			       (if (buffer-read-only? (current-buffer))
				   (barf-if-read-only))
			       1)
			     0)))
		     (if (< index end)
			 (let ((newline
				(substring-find-next-char specification
							  index
							  end
							  #\newline)))
			   (with-values
			       (lambda ()
				 (interactive-argument
				  (string-ref specification index)
				  (substring specification
					     (+ index 1)
					     (or newline end))))
			     (lambda (argument expression from-tty?)
			       (with-values
				   (lambda ()
				     (if newline
					 (loop (+ newline 1))
					 (values '() '() false)))
				 (lambda (arguments expressions any-from-tty?)
				   (values (cons argument arguments)
					   (cons expression expressions)
					   (or from-tty? any-from-tty?)))))))
			 (values '() '() false)))))
	     (lambda (arguments expressions any-from-tty?)
	       (if (or record?
		       (and any-from-tty?
			    (not (prefix-key-list? (current-comtabs)
						   (current-command-key)))))
		   (record-command-arguments expressions))
	       arguments)))
	  ((null? specification)
	   (if record? (record-command-arguments '()))
	   '())
	  (else
	   (let ((old-keys-read keyboard-keys-read))
	     (let ((arguments (specification)))
	       (if (or record? (not (= keyboard-keys-read old-keys-read)))
		   (record-command-arguments (map quotify-sexp arguments)))
	       arguments))))))

(define (execute-command-history-entry entry)
  (let ((history command-history))
    (if (not (equal? entry
		     (let loop ((entries (cdr history)) (tail history))
		       (if (eq? entries history)
			   (car tail)
			   (loop (cdr entries) entries)))))
	(begin
	  (set-car! history entry)
	  (set! command-history (cdr history)))))
  (apply (command-procedure (name->command (car entry)))
	 (map (let ((environment (->environment '(EDWIN))))
		(lambda (expression)
		  (eval-with-history expression environment)))
	      (cdr entry))))

(define (interactive-argument key prompt)
  (let ((prompting
	 (lambda (value)
	   (values value (quotify-sexp value) true)))
	(prefix
	 (lambda (prefix)
	   (values prefix (quotify-sexp prefix) false)))
	(varies
	 (lambda (value expression)
	   (values value expression false))))
    (case key
      ((#\b)
       (prompting
	(buffer-name (prompt-for-existing-buffer prompt (current-buffer)))))
      ((#\B)
       (prompting (buffer-name (prompt-for-buffer prompt (current-buffer)))))
      ((#\c)
       (prompting (prompt-for-char prompt)))
      ((#\C)
       (prompting (command-name (prompt-for-command prompt))))
      ((#\d)
       (varies (current-point) '(CURRENT-POINT)))
      ((#\D)
       (prompting (prompt-for-directory prompt false)))
      ((#\f)
       (prompting (prompt-for-existing-file prompt false)))
      ((#\F)
       (prompting (prompt-for-file prompt false)))
      ((#\k)
       (prompting (prompt-for-key prompt (current-comtabs))))
      ((#\m)
       (varies (current-mark) '(CURRENT-MARK)))
      ((#\n)
       (prompting (prompt-for-number prompt false)))
      ((#\N)
       (prefix (or (command-argument) (prompt-for-number prompt false))))
      ((#\p)
       (prefix (or (command-argument-value (command-argument)) 1)))
      ((#\P)
       (prefix (command-argument)))
      ((#\r)
       (varies (current-region) '(CURRENT-REGION)))
      ((#\s)
       (prompting (or (prompt-for-string prompt false 'NULL-DEFAULT) "")))
      ((#\v)
       (prompting (variable-name (prompt-for-variable prompt))))
      ((#\x)
       (prompting (prompt-for-expression prompt)))
      ((#\X)
       (prompting (prompt-for-expression-value prompt)))
      (else
       (editor-error "Invalid control letter "
		     key
		     " in interactive calling string")))))

(define (quotify-sexp sexp)
  (if (or (boolean? sexp)
	  (number? sexp)
	  (string? sexp)
	  (char? sexp))
      sexp
      `(QUOTE ,sexp)))