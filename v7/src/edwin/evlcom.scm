;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/evlcom.scm,v 1.15 1989/08/07 08:44:48 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989 Massachusetts Institute of Technology
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

;;;; Evaluation Commands

(declare (usual-integrations))

(define-variable scheme-environment
  "The environment used by the evaluation commands, or 'DEFAULT.
If 'DEFAULT, use the default (REP loop) environment."
  'DEFAULT)

(define-variable scheme-syntax-table
  "The syntax table used by the evaluation commands, or false.
If false, use the default (REP loop) syntax-table."
  false)

(define-variable previous-evaluation-expression
  "The last expression evaluated in the typein window."
  false)

(define-variable debug-on-evaluation-error
  "True means enter debugger if error is signalled while evaluating.
This does not affect editor errors."
  true)

(define-command eval-definition
  "Evaluate the definition at point.
Prints the result in the typein window.
With an argument, prompts for the evaluation environment.
Output goes to the transcript buffer."
  "P"
  (lambda (argument)
    (evaluate-from-mark (current-definition-start)
			(evaluation-environment argument))))

(define-command eval-next-sexp
  "Evaluate the expression following point.
Prints the result in the typein window.
With an argument, prompts for the evaluation environment.
Output goes to the transcript buffer."
  "P"
  (lambda (argument)
    (evaluate-from-mark (current-point)
			(evaluation-environment argument))))

(define-command eval-previous-sexp
  "Evaluate the expression preceding point.
Prints the result in the typein window.
With an argument, prompts for the evaluation environment.
Output goes to the transcript buffer."
  "P"
  (lambda (argument)
    (evaluate-from-mark (backward-one-sexp (current-point))
			(evaluation-environment argument))))

(define-command eval-region
  "Evaluate the region, printing the results in the typein window.
With an argument, prompts for the evaluation environment.
Output goes to the transcript buffer."
  "r\nP"
  (lambda (region argument)
    (evaluate-region region (evaluation-environment argument))))

(define-command eval-current-buffer
  "Evaluate the buffer.
The values are printed in the typein window.
With an argument, prompts for the evaluation environment.
Output goes to the transcript buffer."
  "P"
  (lambda (argument)
    (evaluate-region (buffer-region (current-buffer))
		     (evaluation-environment argument))))

(define-command eval-previous-sexp-into-buffer
  "Evaluate the expression preceding point.
With an argument, prompts for the evaluation environment.
Output is inserted into the buffer at point."
  "P"
  (lambda (argument)
    (let ((start (backward-sexp (current-point) 1 false)))
      (if (not start) (editor-error "No previous expression"))
      (let ((environment (evaluation-environment argument)))
	(with-output-to-current-point
	 (lambda ()
	   (write-line
	    (eval-with-history (read-from-mark start) environment))))))))

(define-command eval-expression
  "Read an evaluate an expression in the typein window.
With an argument, prompts for the evaluation environment."
  "xEvaluate expression\nP"
  (lambda (expression argument)
    (editor-eval expression (evaluation-environment argument))))

(define-command set-environment
  "Sets the environment for the editor and any inferior REP loops."
  "XSet environment"
  (lambda (environment)
    (set-repl/environment! (nearest-repl) (->environment environment))))

(define (evaluation-environment argument)
  (cond (argument
	 (->environment
	  (prompt-for-expression-value "Evaluate in environment" false)))
	((eq? 'DEFAULT (ref-variable scheme-environment))
	 (nearest-repl/environment))
	(else
	 (->environment (ref-variable scheme-environment)))))

(define-command set-syntactic-environment
  "Sets the current syntactic environment."
  "XSet syntactic environment"
  (lambda (syntactic-environment)
    (set-repl/syntax-table! (nearest-repl) syntactic-environment)))

(define (evaluation-syntax-table)
  (or (ref-variable scheme-syntax-table)
      (nearest-repl/syntax-table)))

(define (evaluate-from-mark input-mark environment)
  (editor-eval (read-from-mark input-mark) environment))

(define (read-from-mark input-mark)
  (with-input-from-mark input-mark read))

(define (editor-eval sexp environment)
  (with-output-to-transcript-buffer
   (lambda ()
     (let ((value (eval-with-history sexp environment)))
       (transcript-write value)
       value))))

(define (evaluate-region region environment)
  (with-output-to-transcript-buffer
    (lambda ()
      (with-input-from-region region
	(lambda ()
	  (let loop ((object (read)))
	    (if (not (eof-object? object))
		(begin
		  (transcript-write (eval-with-history object environment))
		  (loop (read))))))))))

(define (eval-with-history expression environment)
  (let ((scode (syntax expression (evaluation-syntax-table))))
    (bind-condition-handler '()
	(lambda (condition)
	  (and (not (condition/internal? condition))
	       (error? condition)
	       (if (ref-variable debug-on-evaluation-error)
		   (debug-scheme-error condition)
		   (let ((string
			  (with-output-to-string
			    (lambda ()
			      ((condition/reporter condition)
			       condition
			       (current-output-port))))))
		     (if (and (not (string-find-next-char string #\newline))
			      (< (string-column-length string 18) 80))
			 (message "Evaluation error: " string)
			 (begin
			   (with-output-to-temporary-buffer "*error*" string)
			   (message "Evaluation error")))))
	       (%editor-error)))
      (lambda ()
	(with-new-history
	 (lambda () (extended-scode-eval scode environment)))))))

(define (prompt-for-expression-value prompt default)
  (eval-with-history (prompt-for-expression prompt default)
		     (evaluation-environment false)))

(define (prompt-for-expression prompt default-object #!optional default-type)
  (read-from-string
   (prompt-for-string prompt
		      (and default-object
			   (write-to-string default-object))
		      (if (default-object? default-type)
			  'VISIBLE-DEFAULT
			  default-type)
		      (ref-mode-object prompt-for-expression))))

(define-major-mode prompt-for-expression scheme #f
  "Major mode for editing solicited input expressions.
Depending on what is being solicited, either defaulting or completion
may be available.  The following commands are special to this mode:

\\[exit-minibuffer] terminates the input.
\\[minibuffer-yank-default] yanks the default string, if there is one.")

(define-key 'prompt-for-expression #\return 'exit-minibuffer)
(define-key 'prompt-for-expression #\c-m-y 'minibuffer-yank-default)

;;;; Transcript Buffer

(define-variable transcript-buffer-name
  "Name of buffer to which evaluation commands record their output."
  "*Transcript*")

(define-variable enable-transcript-buffer
  "If true, I/O from evaluation commands is recorded in transcript buffer.
Recording is done only for commands that write their output to the
message area, not commands that write to a specific buffer."
  false)

(define (transcript-buffer)
  (find-or-create-buffer (ref-variable transcript-buffer-name)))

(define (transcript-write value)
  (if (ref-variable enable-transcript-buffer)
      (write-line value))
  (if (or (not (ref-variable enable-transcript-buffer))
	  (null? (buffer-windows (transcript-buffer))))
      (message (write-to-string value))))

(define (with-output-to-transcript-buffer thunk)
  (if (ref-variable enable-transcript-buffer)
      (with-interactive-output-port (transcript-output-port) thunk)
      (thunk)))

(define (transcript-output-port)
  (output-port/copy transcript-output-port-template (transcript-buffer)))

(define (operation/write-char port char)
  (region-insert-char! (buffer-end (output-port/state port)) char))

(define (operation/write-string port string)
  (region-insert-string! (buffer-end (output-port/state port)) string))

(define (operation/flush-output port)
  (let ((buffer (output-port/state port)))
    (let ((end (buffer-end buffer)))
      (for-each (lambda (window)
		  (set-window-point! window end)
		  (window-direct-update! window false))
		(buffer-windows buffer)))))

(define (operation/print-self state port)
  (unparse-string state "to transcript buffer ")
  (unparse-object state (output-port/state port)))

(define transcript-output-port-template
  (make-output-port `((FLUSH-OUTPUT ,operation/flush-output)
		      (PRINT-SELF ,operation/print-self)
		      (WRITE-CHAR ,operation/write-char)
		      (WRITE-STRING ,operation/write-string))
		    false))