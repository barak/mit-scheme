;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/evlcom.scm,v 1.23 1991/05/10 05:06:57 cph Exp $
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

;;;; Evaluation Commands
;;; Package: (edwin)

(declare (usual-integrations))

;;;; Variables

(define-variable scheme-environment
  "The environment used by the evaluation commands, or 'DEFAULT.
If 'DEFAULT, use the default (REP loop) environment."
  'DEFAULT)

(define-variable scheme-syntax-table
  "The syntax table used by the evaluation commands, or #F
If #F, use the default (REP loop) syntax-table."
  false)

(define-variable debug-on-evaluation-error
  "True means enter debugger if error is signalled while evaluating.
This does not affect editor errors."
  true
  boolean?)

(define-variable enable-transcript-buffer
  "If true, output from evaluation commands is recorded in transcript buffer."
  false
  boolean?)

(define-variable transcript-buffer-name
  "Name of evaluation transcript buffer.
This can also be a buffer object."
  "*scratch*")

(define-variable transcript-buffer-mode
  "Mode of evaluation transcript buffer.
This can be either a mode object or the name of one."
  'scheme-interaction)

(define-variable transcript-input-recorder
  "A procedure which receives each input region before evaluation.
If #F, disables input recording."
  false)

(define-variable transcript-output-wrapper
  "A procedure which is called to setup transcript output.
It is passed a thunk as its only argument.
If #F, normal transcript output is done."
  false)

(define-variable transcript-list-depth-limit
  "List depth to which evaluation results are printed.  #F means no limit."
  false
  (lambda (object) (or (not object) (exact-nonnegative-integer? object))))

(define-variable transcript-list-breadth-limit
  "List breadth to which evaluation results are printed.  #F means no limit."
  false
  (lambda (object) (or (not object) (exact-nonnegative-integer? object))))

;;;; Commands

(define-command eval-defun
  "Evaluate defun that point is in or before.
Print value in minibuffer.
With argument, prompts for the evaluation environment."
  "P"
  (lambda (argument)
    (evaluate-from-mark (current-definition-start) argument)))

(define-command eval-next-sexp
  "Evaluate the expression following point.
Prints the result in the typein window.
With an argument, prompts for the evaluation environment."
  "P"
  (lambda (argument)
    (evaluate-from-mark (current-point) argument)))

(define-command eval-last-sexp
  "Evaluate the expression preceding point.
Prints the result in the typein window.
With an argument, prompts for the evaluation environment."
  "P"
  (lambda (argument)
    (evaluate-from-mark (backward-sexp (current-point) 1 'ERROR) argument)))

(define-command eval-region
  "Evaluate the region, printing the results in the typein window.
With an argument, prompts for the evaluation environment."
  "r\nP"
  (lambda (region argument)
    (evaluate-region region argument)))

(define-command eval-current-buffer
  "Evaluate the current buffer.
The values are printed in the typein window.
With an argument, prompts for the evaluation environment."
  "P"
  (lambda (argument)
    (evaluate-region (buffer-region (current-buffer)) argument)))

(define-command eval-expression
  "Read an evaluate an expression in the typein window.
With an argument, prompts for the evaluation environment."
  "xEvaluate expression\nP"
  (lambda (expression argument)
    (editor-eval expression (evaluation-environment argument))))

(define-command set-environment
  "Make ENVIRONMENT the current evaluation environment."
  "XSet environment"
  (lambda (environment)
    (set-variable! scheme-environment (->environment environment))))

(define-command set-syntax-table
  "Make SYNTAX-TABLE the current syntax table."
  "XSet syntax table"
  (lambda (syntax-table)
    (set-variable! scheme-syntax-table syntax-table)))

(define-command set-default-environment
  "Make ENVIRONMENT the default evaluation environment."
  "XSet default environment"
  (lambda (environment)
    (set-variable-default-value! (ref-variable-object scheme-environment)
				 (->environment environment))))

(define-command set-default-syntax-table
  "Make SYNTAX-TABLE the default syntax table."
  "XSet default syntax table"
  (lambda (syntax-table)
    (set-variable-default-value! (ref-variable-object scheme-syntax-table)
				 syntax-table)))

(define-command set-repl-environment
  "Make ENVIRONMENT the environment of the nearest REP loop."
  "XSet REPL environment"
  (lambda (environment)
    (set-repl/environment! (nearest-repl) (->environment environment))))

(define-command set-repl-syntax-table
  "Make SYNTAX-TABLE the syntax table of the nearest REP loop."
  "XSet REPL syntax table"
  (lambda (syntax-table)
    (set-repl/syntax-table! (nearest-repl) syntax-table)))

(define-command select-transcript-buffer
  "Select the transcript buffer."
  ()
  (lambda ()
    (select-buffer (transcript-buffer))))

;;;; Expression Prompts

(define (prompt-for-expression-value prompt #!optional default)
  (eval-with-history (if (default-object? default)
			 (prompt-for-expression prompt)
			 (prompt-for-expression prompt default))
		     (evaluation-environment false)))

(define (prompt-for-expression prompt #!optional default-object default-type)
  (read-from-string
   (prompt-for-string prompt
		      (and (not (default-object? default-object))
			   (write-to-string default-object))
		      (if (default-object? default-type)
			  'VISIBLE-DEFAULT
			  default-type)
		      (ref-mode-object prompt-for-expression))))

(define (read-from-string string)
  (bind-condition-handler (list condition-type:error) evaluation-error-handler
    (lambda ()
      (with-input-from-string string read))))

(define-major-mode prompt-for-expression scheme #f
  "Major mode for editing solicited input expressions.
Depending on what is being solicited, either defaulting or completion
may be available.  The following commands are special to this mode:

\\[exit-minibuffer] terminates the input.
\\[minibuffer-yank-default] yanks the default string, if there is one.")

(define-key 'prompt-for-expression #\return 'exit-minibuffer)
(define-key 'prompt-for-expression #\c-m-y 'minibuffer-yank-default)

;;;; Evaluation

(define (evaluate-from-mark input-mark argument)
  (evaluate-region (make-region input-mark (forward-sexp input-mark 1 'ERROR))
		   argument))

(define (evaluate-region region argument)
  (let ((transcript-input-recorder (ref-variable transcript-input-recorder)))
    (if transcript-input-recorder
	(transcript-input-recorder region)))
  (let ((environment (evaluation-environment argument)))
    (with-input-from-region region
      (lambda ()
	(bind-condition-handler (list condition-type:error) evaluation-error-handler
	  (letrec
	      ((loop
		(lambda ()
		  (let ((sexp (read)))
		    (if (not (eof-object? sexp))
			(begin
			  (editor-eval sexp environment)
			  (loop)))))))
	    loop))))))

(define (evaluation-environment argument)
  (let ((->environment
	 (lambda (object)
	   (bind-condition-handler (list condition-type:error)
	       (lambda (condition)
		 condition
		 (editor-error "Illegal environment: " object))
	     (lambda ()
	       (->environment object))))))
    (if argument
	(if (environment? argument)
	    argument
	    (->environment
	     (prompt-for-expression-value "Evaluate in environment")))
	(let ((environment (ref-variable scheme-environment)))
	  (if (eq? 'DEFAULT environment)
	      (nearest-repl/environment)
	      (->environment environment))))))

(define (evaluation-syntax-table environment)
  (let ((syntax-table (ref-variable scheme-syntax-table)))
    (cond ((or (not syntax-table) (eq? 'DEFAULT syntax-table))
	   (nearest-repl/syntax-table))
	  ((scheme-syntax-table? syntax-table)
	   syntax-table)
	  ((and (symbol? syntax-table)
		(not (lexical-unreferenceable? environment syntax-table))
		(let ((syntax-table
		       (lexical-reference environment syntax-table)))
		  (and (scheme-syntax-table? syntax-table)
		       syntax-table))))
	  (else
	   (editor-error "Illegal syntax table: " syntax-table)))))

(define scheme-syntax-table?
  (access syntax-table? system-global-environment))

(define (editor-eval sexp environment)
  (with-output-to-transcript-buffer
   (lambda ()
     (let ((value (eval-with-history sexp environment)))
       (transcript-write value)
       value))))

(define (eval-with-history expression environment)
  (scode-eval-with-history (syntax expression
				   (evaluation-syntax-table environment))
			   environment))

(define (scode-eval-with-history scode environment)
  (bind-condition-handler (list condition-type:error) evaluation-error-handler
    (lambda ()
      (with-new-history
       (lambda ()
	 (extended-scode-eval scode environment))))))

(define (evaluation-error-handler condition)
  (if (ref-variable debug-on-evaluation-error)
      (debug-scheme-error condition)
      (let ((string
	     (with-string-output-port
	       (lambda (port)
		 (write-condition-report condition port)))))
	(if (and (not (string-find-next-char string #\newline))
		 (< (string-columns string 18 false) 80))
	    (message "Evaluation error: " string)
	    (begin
	      (string->temporary-buffer string "*Error*")
	      (message "Evaluation error")))))
  (%editor-error))

;;;; Transcript Buffer

(define (with-output-to-transcript-buffer thunk)
  (if (ref-variable enable-transcript-buffer)
      (let ((output-wrapper (ref-variable transcript-output-wrapper)))
	(if output-wrapper
	    (output-wrapper thunk)
	    (let ((output-port
		   (let ((buffer (transcript-buffer)))
		     (mark->output-port (buffer-end buffer) buffer))))
	      (with-output-to-port output-port
		(lambda ()
		  (with-cmdl/output-port (nearest-cmdl)
		    (lambda ()
		      (fresh-lines 1)
		      (thunk))))))))
      (let ((value))
	(let ((output
	       (with-output-to-string
		 (lambda ()
		   (set! value (thunk))
		   unspecific))))
	  (if (not (string-null? output))
	      (string->temporary-buffer output "*Unsolicited-Output*")))
	value)))

(define (transcript-write value)
  (let ((value-string
	 (if (undefined-value? value)
	     "No value"
	     (string-append
	      "Value: "
	      (fluid-let ((*unparser-list-depth-limit*
			   (ref-variable transcript-list-depth-limit))
			  (*unparser-list-breadth-limit*
			   (ref-variable transcript-list-breadth-limit)))
		(write-to-string value))))))
    (let ((value-message (lambda () (message value-string))))
      (if (ref-variable enable-transcript-buffer)
	  (begin
	    (fresh-lines 1)
	    (write-char #\;)
	    (write-string value-string)
	    (fresh-lines 2)
	    (if (null? (buffer-windows (transcript-buffer)))
		(value-message)))
	  (value-message)))))

(define (transcript-buffer)
  (let ((name (ref-variable transcript-buffer-name)))
    (if (buffer? name)
	name
	(or (find-buffer name)
	    (let ((buffer (create-buffer name)))
	      (set-buffer-major-mode!
	       buffer
	       (->mode (ref-variable transcript-buffer-mode)))
	      buffer)))))