;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/evlcom.scm,v 1.12 1989/03/15 19:11:35 cph Exp $
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

;;;; Evaluation Commands

(declare (usual-integrations))

(define-variable "Scheme Environment"
  "The environment used by the evaluation commands, or 'DEFAULT.
If 'DEFAULT, use the default (REP loop) environment."
  'DEFAULT)

(define-variable "Scheme Syntax Table"
  "The syntax table used by the evaluation commands, or false.
If false, use the default (REP loop) syntax-table."
  false)

(define-variable "Previous Evaluation Environment"
  "The last explicit environment for an evaluation command."
  false)

(define-command ("^R Evaluate Definition" argument)
  "Evaluate the definition at point.
Prints the result in the typein window.
With an argument, prompts for the evaluation environment.
Output goes to the transcript buffer."
  (evaluate-sexp (current-definition-start)
		 (evaluation-environment argument)))

(define-command ("^R Evaluate Sexp" argument)
  "Evaluate the expression following point.
Prints the result in the typein window.
With an argument, prompts for the evaluation environment.
Output goes to the transcript buffer."
  (evaluate-sexp (current-point)
		 (evaluation-environment argument)))

(define-command ("^R Evaluate Previous Sexp" argument)
  "Evaluate the expression preceding point.
Prints the result in the typein window.
With an argument, prompts for the evaluation environment.
Output goes to the transcript buffer."
  (evaluate-sexp (backward-one-sexp (current-point))
		 (evaluation-environment argument)))

(define-command ("^R Evaluate Region" argument)
  "Evaluate the region, printing the results in the typein window.
With an argument, prompts for the evaluation environment.
Output goes to the transcript buffer."
  (evaluate-region (current-region)
		   (evaluation-environment argument)))

(define-command ("^R Evaluate Buffer" argument)
  "Evaluate the buffer.
The values are printed in the typein window.
With an argument, prompts for the evaluation environment.
Output goes to the transcript buffer."
  (evaluate-region (buffer-region (current-buffer))
		   (evaluation-environment argument)))

(define-command ("^R Evaluate Previous Sexp into Buffer" argument)
  "Evaluate the expression preceding point.
With an argument, prompts for the evaluation environment.
Output is inserted into the buffer at point."
  (let ((start (backward-sexp (current-point) 1 false)))
    (if (not start) (editor-error "No previous expression"))
    (let ((environment (evaluation-environment argument)))
      (with-output-to-current-point
       (lambda ()
	 (write-line (eval-with-history (with-input-from-mark start read)
					environment)))))))

(define-variable "Previous Typein Expression"
  "The last expression evaluated in the typein window."
  false)

(define-command ("^R Evaluate Sexp Typein" argument)
  "Read an evaluate an expression in the typein window.
With an argument, prompts for the evaluation environment."
  (let ((string
	 (prompt-for-expression "Evaluate Sexp"
				(ref-variable "Previous Typein Expression")
				'INVISIBLE-DEFAULT)))
    (set-variable! "Previous Typein Expression" string)
    (editor-eval (with-input-from-string string read)
		 (evaluation-environment argument))))

(define-command ("Set Environment")
  "Sets the environment for the editor and any inferior REP loops."
  (set-repl/environment! (nearest-repl)
			 (->environment
			  (prompt-for-expression-value
			   "REP environment"
			   (ref-variable "Previous Evaluation Environment")))))

(define-command ("Set Syntax Table")
  "Sets the current syntax table (for the syntaxer, not the editor)."
  (set-repl/syntax-table! (nearest-repl)
			  (prompt-for-expression-value "Set Syntax Table"
						       false)))

(define (evaluate-sexp input-mark environment)
  (editor-eval (with-input-from-mark input-mark read) environment))

(define (evaluate-string string environment)
  (eval-with-history (with-input-from-string string read) environment))

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
	 (define (loop object)
	   (if (not (eof-object? object))
	       (begin (transcript-write (eval-with-history object environment))
		      (loop (read)))))
	 (loop (read)))))))

(define (eval-with-history expression environment)
  (let ((scode (syntax expression (evaluation-syntax-table))))
    (bind-condition-handler '()
	(lambda (condition)
	  (and (not (condition/internal? condition))
	       (error? condition)
	       (begin
		(with-output-to-temporary-buffer "*Error*"
		  (lambda ()
		    (format-error-message (condition/message condition)
					  (condition/irritants condition)
					  (current-output-port))))
		(editor-error "Error while evaluating expression"))))
      (lambda ()
	(with-new-history (lambda () (scode-eval scode environment)))))))
(define (prompt-for-expression prompt default-string #!optional default-type)
  (prompt-for-completed-string prompt
			       default-string
			       (if (default-object? default-type)
				   'VISIBLE-DEFAULT
				   default-type)
			       false 'NO-COMPLETION
			       prompt-for-expression-mode))

(define-major-mode "Prompt for Expression" "Scheme"
  "Major mode for editing solicited input expressions.
Depending on what is being solicited, either defaulting or completion
may be available.  The following commands are special to this mode:

\\[^R Terminate Input] terminates the input.
\\[^R Yank Default String] yanks the default string, if there is one.")

(define-key "Prompt for Expression" #\Return "^R Terminate Input")
(define-key "Prompt for Expression" #\C-M-Y "^R Yank Default String")

(define (prompt-for-expression-value prompt default)
  (evaluate-string (prompt-for-expression prompt default)
		   (evaluation-environment false)))

(define (evaluation-syntax-table)
  (or (ref-variable "Scheme Syntax Table")
      (nearest-repl/syntax-table)))

(define (evaluation-environment argument)
  (cond (argument
	 (let ((string
		(prompt-for-expression
		 "Evaluate in environment"
		 (ref-variable "Previous Evaluation Environment"))))
	   (set-variable! "Previous Evaluation Environment" string)
	   (->environment (eval (with-input-from-string string read)
				(evaluation-environment false)))))
	((eq? 'DEFAULT (ref-variable "Scheme Environment"))
	 (nearest-repl/environment))
	(else
	 (->environment (ref-variable "Scheme Environment")))))

;;;; Transcript Buffer

(define-variable "Transcript Buffer Name"
  "Name of buffer to which evaluation commands record their output."
  "*Transcript*")

(define-variable "Enable Transcript Buffer"
  "If true, I/O from evaluation commands is recorded in transcript buffer.
Recording is done only for commands that write their output to the
message area, not commands that write to a specific buffer."
  false)

(define (transcript-buffer)
  (find-or-create-buffer (ref-variable "Transcript Buffer Name")))

(define (transcript-write value)
  (if (ref-variable "Enable Transcript Buffer")
      (write-line value))
  (if (or (not (ref-variable "Enable Transcript Buffer"))
	  (null? (buffer-windows (transcript-buffer))))
      (message (write-to-string value))))

(define (with-output-to-transcript-buffer thunk)
  (if (ref-variable "Enable Transcript Buffer")
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