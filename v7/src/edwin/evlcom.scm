;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/evlcom.scm,v 1.31 1991/11/26 08:03:13 cph Exp $
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
  "*transcript*")

(define-variable transcript-buffer-mode
  "Mode of evaluation transcript buffer.
This can be either a mode object or the name of one."
  'scheme)

(define-variable transcript-input-recorder
  "A procedure that receives each input region before evaluation.
If #F, disables input recording."
  false)

(define-variable transcript-output-wrapper
  "A procedure that is called to setup transcript output.
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
  "Read and evaluate an expression in the typein window.
With an argument, prompts for the evaluation environment."
  "xEvaluate expression\nP"
  (lambda (expression argument)
    (let ((enable-transcript-buffer (ref-variable enable-transcript-buffer)))
      (if enable-transcript-buffer
	  (insert-string
	   (fluid-let ((*unparse-with-maximum-readability?* true))
	     (write-to-string expression))
	   (buffer-end (transcript-buffer)))))
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
  (eval-with-history
   (if (default-object? default)
       (prompt-for-expression prompt)
       (prompt-for-expression prompt
			      (if (or (symbol? default)
				      (pair? default)
				      (vector? default))
				  `',default
				  default)))
   (evaluation-environment false)))

(define (prompt-for-expression prompt #!optional default-object default-type)
  (let ((default-string
	  (and (not (default-object? default-object))
	       (write-to-string default-object)))
	(default-type
	  (if (default-object? default-type)
	      'VISIBLE-DEFAULT
	      default-type)))
    (read-from-string
     (prompt-for-string
      (prompt-for-string/prompt prompt
				(and (eq? default-type 'VISIBLE-DEFAULT)
				     default-string))
      default-string
      (if (eq? default-type 'VISIBLE-DEFAULT)
	  'INVISIBLE-DEFAULT
	  default-type)
      (ref-mode-object prompt-for-expression)))))

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
  (let ((enable-transcript-buffer (ref-variable enable-transcript-buffer)))
    (if enable-transcript-buffer
	(insert-region (region-start region)
		       (region-end region)
		       (buffer-end (transcript-buffer)))))
  (let ((environment (evaluation-environment argument)))
    (with-input-from-region region
      (lambda ()
	(bind-condition-handler (list condition-type:error)
	    evaluation-error-handler
	  (letrec
	      ((loop
		(lambda (result)
		  (let ((sexp (read)))
		    (if (eof-object? sexp)
			result
			(loop (editor-eval sexp environment)))))))
	    (lambda ()
	      (loop unspecific))))))))

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

(define-variable run-light
  "Scheme run light.  Not intended to be modified by users, but needed to
kludge the mode line."
  false)

(define-variable enable-run-light?
  "Whether to display the Scheme run light."
  true
  boolean?)

(define (editor-eval sexp environment)
  (let ((core
	 (lambda ()
	   (with-input-from-string ""
	     (lambda ()
	       (with-output-to-transcript-buffer
		(lambda ()
		  (let ((value (eval-with-history sexp environment)))
		    (transcript-write
		     value
		     (and (ref-variable enable-transcript-buffer)
			  (transcript-buffer)))
		    value))))))))
    (if (ref-variable enable-run-light?)
	(dynamic-wind
	 (lambda ()
	   (set-variable! run-light "eval")
	   (for-each (lambda (window)
		       (window-modeline-event! window 'RUN-LIGHT))
		     (window-list))
	   (update-screens! false))
	 core
	 (lambda ()
	   (set-variable! run-light false)
	   (for-each (lambda (window)
		       (window-modeline-event! window 'RUN-LIGHT))
		     (window-list))
	   (update-screens! false)))
	(core))))

(define (eval-with-history expression environment)
  (let ((syntax-table (evaluation-syntax-table environment)))
    (bind-condition-handler (list condition-type:error)
	evaluation-error-handler
      (lambda ()
	(hook/repl-eval expression environment syntax-table)))))

(define (evaluation-error-handler condition)
  (default-report-error condition "evaluation")
  (if (ref-variable debug-on-evaluation-error)
      (debug-scheme-error condition "evaluation")
      (begin
	(editor-beep)
	(abort-current-command))))

(define (default-report-error condition error-type-name)
  (let ((report-string (condition/report-string condition)))
    (let ((typein-report
	   (lambda ()
	     (message (string-capitalize error-type-name)
		      " error: "
		      report-string)))
	  (error-buffer-report
	   (lambda ()
	     (string->temporary-buffer report-string "*error*")
	     (message (string-capitalize error-type-name) " error"))))
      (case (ref-variable error-display-mode)
	((TRANSCRIPT)
	 (if (ref-variable enable-transcript-buffer)
	     (with-output-to-transcript-buffer
	       (lambda ()
		 (fresh-line)
		 (write-string ";Error: ")
		 (write-string report-string)
		 (newline)
		 (newline)))
	     (error-buffer-report)))
	((ERROR-BUFFER)
	 (error-buffer-report))
	((TYPEIN)
	 (typein-report))
	((FIT)
	 (if (and (not (string-find-next-char report-string #\newline))
		  (< (string-columns report-string 18 false)
		     (window-x-size (typein-window))))
	     (typein-report)
	     (error-buffer-report)))))))

(define-variable error-display-mode
  "Value of this variable controls the way evaluation errors are displayed:
TRANSCRIPT    Error messages appear in transcript buffer.
ERROR-BUFFER  Error messages appear in *error* buffer.
TYPEIN        Error messages appear in typein window.
FIT           Error messages appear in typein window if they fit;
                in *error* buffer if they don't."
  'TRANSCRIPT
  (lambda (value) (memq value '(TRANSCRIPT ERROR-BUFFER TYPEIN FIT))))

;;;; Transcript Buffer

(define (with-output-to-transcript-buffer thunk)
  (if (ref-variable enable-transcript-buffer)
      (let ((output-wrapper (ref-variable transcript-output-wrapper)))
	(if output-wrapper
	    (output-wrapper thunk)
	    (let ((output-port
		   (let ((buffer (transcript-buffer)))
		     (mark->output-port (buffer-end buffer) buffer))))
	      (fresh-line output-port)
	      (with-output-to-port output-port thunk))))
      (let ((value))
	(let ((output
	       (with-output-to-string
		 (lambda ()
		   (set! value (thunk))
		   unspecific))))
	  (if (not (string-null? output))
	      (string->temporary-buffer output "*Unsolicited-Output*")))
	value)))

(define (transcript-write value buffer)
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
    (if buffer
	(let ((point (mark-left-inserting-copy (buffer-end buffer))))
	  (guarantee-newlines 1 point)
	  (insert-char #\; point)
	  (insert-string value-string point)
	  (insert-newlines 2 point)
	  (mark-temporary! point)))
    (if (or (not buffer) (null? (buffer-windows buffer)))
	(message value-string))))

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