;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/evlcom.scm,v 1.38 1992/08/18 23:31:58 cph Exp $
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

;;;; Evaluation Commands
;;; Package: (edwin)

(declare (usual-integrations))

;;;; Variables

(define-variable scheme-environment
  "The environment used by the evaluation commands, or 'DEFAULT.
If 'DEFAULT, use the default (REP loop) environment."
  'DEFAULT)

(define-variable scheme-syntax-table
  "The syntax table used by the evaluation commands, or #F.
If #F, use the default (REP loop) syntax-table."
  false)

(define-variable debug-on-evaluation-error
  "True means enter debugger if error is signalled while evaluating.
This does not affect editor errors."
  true
  boolean?)

(define-variable evaluation-input-recorder
  "A procedure that receives each input region before evaluation.
If #F, disables input recording."
  false)

(define-variable evaluation-output-receiver
  "Procedure to call with the value and output from evaluation.
The value is an object, and the output is a string.
If #F, the value is printed in the typein window,
and the output, if non-null, is shown in a pop-up buffer."
  false)

(define-variable enable-transcript-buffer
  "If true, output from evaluation commands is recorded in transcript buffer."
  false
  boolean?)

(define-variable disable-evaluation-commands
  "If true, evaluation commands signal an error."
  false
  boolean?)

(define-variable evaluate-in-inferior-repl
  "If true, evaluation commands evaluate expressions in an inferior REPL.
Also, the inferior REPL's run light appears in all Scheme mode buffers.
Otherwise, expressions are evaluated directly by the commands."
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

(define-variable transcript-buffer-read-only
  "If true, transcript buffer is initialized to read-only when created."
  true
  boolean?)

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

(define-variable transcript-disable-evaluation
  "If true, evaluation commands are disabled in the transcript buffer."
  true
  boolean?)

;;;; Commands

(define-command eval-defun
  "Evaluate defun that point is in or before.
Print value in minibuffer."
  ()
  (lambda () (evaluate-from-mark (current-definition-start))))

(define-command eval-next-sexp
  "Evaluate the expression following point.
Prints the result in the typein window."
  ()
  (lambda () (evaluate-from-mark (current-point))))

(define-command eval-last-sexp
  "Evaluate the expression preceding point.
Prints the result in the typein window."
  ()
  (lambda () (evaluate-from-mark (backward-sexp (current-point) 1 'ERROR))))

(define (evaluate-from-mark input-mark)
  ((ref-command eval-region)
   (make-region input-mark
		(forward-sexp input-mark 1 'ERROR))))

(define-command eval-region
  "Evaluate the region, printing the results in the typein window.
With an argument, prompts for the evaluation environment."
  "r"
  (lambda (region)
    (let ((buffer (mark-buffer (region-start region))))
      (cond ((ref-variable disable-evaluation-commands buffer)
	     (editor-error "Evaluation commands disabled in this buffer."))
	    ((ref-variable evaluate-in-inferior-repl buffer)
	     (inferior-repl-eval-region (current-repl-buffer) region))
	    (else
	     (evaluate-region region (evaluation-environment buffer)))))))

(define-command eval-current-buffer
  "Evaluate the current buffer.
The values are printed in the typein window."
  ()
  (lambda () ((ref-command eval-region) (buffer-region (current-buffer)))))

(define-command eval-expression
  "Read and evaluate an expression in the typein window."
  "xEvaluate expression"
  (lambda (expression)
    (let ((buffer (current-buffer)))
      (cond ((ref-variable disable-evaluation-commands buffer)
	     (editor-error "Evaluation commands disabled in this buffer."))
	    ((ref-variable evaluate-in-inferior-repl buffer)
	     (inferior-repl-eval-expression (current-repl-buffer) expression))
	    (else
	     (if (ref-variable enable-transcript-buffer buffer)
		 (call-with-transcript-buffer
		  (lambda (buffer)
		    (insert-string
		     (fluid-let ((*unparse-with-maximum-readability?* true))
		       (write-to-string expression))
		     (buffer-end buffer)))))
	     (editor-eval buffer
			  expression
			  (evaluation-environment buffer)))))))

(define-command eval-abort-top-level
  "Force the evaluation REPL up to top level.
Has no effect if evaluate-in-inferior-repl is false."
  ()
  (lambda ()
    (let ((buffer (current-buffer)))
      (if (ref-variable evaluate-in-inferior-repl buffer)
	  ((ref-command inferior-cmdl-abort-top-level))
	  (editor-error "Nothing to abort.")))))

(define-command set-environment
  "Make ENVIRONMENT the current evaluation environment."
  "XSet environment"
  (lambda (environment)
    (let ((buffer (current-buffer)))
      (define-variable-local-value! buffer
	(ref-variable-object scheme-environment)
	(if (eq? environment 'DEFAULT)
	    'DEFAULT
	    (->environment environment)))
      (normal-buffer-evaluation-mode buffer))))

(define-command set-syntax-table
  "Make SYNTAX-TABLE the current syntax table."
  "XSet syntax table"
  (lambda (syntax-table)
    (let ((buffer (current-buffer)))
      (define-variable-local-value! buffer
				    (ref-variable-object scheme-syntax-table)
				    syntax-table)
      (normal-buffer-evaluation-mode buffer))))

(define (normal-buffer-evaluation-mode buffer)
  (let ((evaluate-in-inferior-repl
	 (ref-variable-object evaluate-in-inferior-repl))
	(run-light (ref-variable-object run-light)))
    (if (and (eq? (ref-variable scheme-environment buffer) 'DEFAULT)
	     (memq (ref-variable scheme-syntax-table buffer) '(#F DEFAULT)))
	(begin
	  (undefine-variable-local-value! buffer evaluate-in-inferior-repl)
	  (undefine-variable-local-value! buffer run-light))
	(begin
	  (define-variable-local-value! buffer evaluate-in-inferior-repl false)
	  (define-variable-local-value! buffer run-light false)))))

(define-command set-default-environment
  "Make ENVIRONMENT the default evaluation environment."
  "XSet default environment"
  (lambda (environment)
    (set-variable-default-value! (ref-variable-object scheme-environment)
				 (if (eq? environment 'DEFAULT)
				     'DEFAULT
				     (->environment environment)))))

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
    (call-with-transcript-buffer select-buffer)))

;;;; Expression Prompts

(define (prompt-for-expression-value prompt #!optional default)
  (let ((buffer (current-buffer)))
    (eval-with-history
     buffer
     (if (default-object? default)
	 (prompt-for-expression prompt)
	 (prompt-for-expression prompt
				(if (or (symbol? default)
					(pair? default)
					(vector? default))
				    `',default
				    default)))
     (evaluation-environment buffer))))

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

(define (evaluate-region region environment)
  (let ((buffer (mark-buffer (region-start region))))
    (let ((evaluation-input-recorder
	   (ref-variable evaluation-input-recorder buffer)))
      (if evaluation-input-recorder
	  (evaluation-input-recorder region)))
    (if (ref-variable enable-transcript-buffer buffer)
	(call-with-transcript-buffer
	 (lambda (buffer)
	   (insert-region (region-start region)
			  (region-end region)
			  (buffer-end buffer)))))
    (bind-condition-handler (list condition-type:error)
	evaluation-error-handler
      (lambda ()
	(let loop
	    ((expressions (read-expressions-from-region region))
	     (result unspecific))
	  (if (null? expressions)
	      result
	      (loop (cdr expressions)
		    (editor-eval buffer (car expressions) environment))))))))

(define (read-expressions-from-region region)
  (with-input-from-region region
    (lambda ()
      (let loop ()
	(let ((expression (read)))
	  (if (eof-object? expression)
	      '()
	      (cons expression (loop))))))))

(define (evaluation-environment buffer)
  (let ((environment
	 (ref-variable scheme-environment (or buffer (current-buffer)))))
    (if (eq? 'DEFAULT environment)
	(nearest-repl/environment)
	(bind-condition-handler (list condition-type:error)
	    (lambda (condition)
	      condition
	      (editor-error "Illegal environment: " environment))
	  (lambda ()
	    (->environment environment))))))

(define (evaluation-syntax-table buffer environment)
  (let ((syntax-table (ref-variable scheme-syntax-table buffer)))
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
  "Scheme run light.  Not intended to be modified by users.
Set by Scheme evaluation code to update the mode line."
  false
  (lambda (object) (or (not object) (string? object))))

(define-variable enable-run-light?
  "If true, Scheme evaluation commands display a run light in the mode line."
  true
  boolean?)

(define (editor-eval buffer sexp environment)
  (let ((core
	 (lambda ()
	   (with-input-from-string ""
	     (lambda ()
	       (let ((value))
		 (let ((output-string
			(with-output-to-string
			  (lambda ()
			    (set! value
				  (eval-with-history buffer sexp environment))
			    unspecific))))
		   (let ((evaluation-output-receiver
			  (ref-variable evaluation-output-receiver buffer)))
		     (if evaluation-output-receiver
			 (evaluation-output-receiver value output-string)
			 (with-output-to-transcript-buffer
			  (lambda ()
			    (write-string output-string)
			    (transcript-write
			     value
			     (and (ref-variable enable-transcript-buffer
						buffer)
				  (transcript-buffer))))))))
		 value))))))
    (if (ref-variable enable-run-light? buffer)
	(let ((run-light (ref-variable-object run-light))
	      (outside)
	      (inside "eval"))
	  (dynamic-wind
	   (lambda ()
	     (set! outside (variable-local-value buffer run-light))
	     (set-variable-local-value! buffer run-light inside)
	     (set! inside)
	     (global-window-modeline-event!)
	     (update-screens! false))
	   core
	   (lambda ()
	     (set! inside (variable-local-value buffer run-light))
	     (set-variable-local-value! buffer run-light outside)
	     (set! outside)
	     (global-window-modeline-event!)
	     (update-screens! false))))
	(core))))

(define (eval-with-history buffer expression environment)
  (let ((syntax-table (evaluation-syntax-table buffer environment)))
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
	    (call-with-transcript-buffer
	     (lambda (buffer)
	       (let ((output-port
		      (mark->output-port (buffer-end buffer) buffer)))
		 (fresh-line output-port)
		 (with-output-to-port output-port thunk))))))
      (let ((value))
	(let ((output
	       (with-output-to-string
		 (lambda ()
		   (set! value (thunk))
		   unspecific))))
	  (if (and (not (string-null? output))
		   (not (ref-variable evaluation-output-receiver)))
	      (string->temporary-buffer output "*Unsolicited-Output*")))
	value)))

(define (transcript-write value buffer)
  (let ((value-string
	 (string-append
	  (transcript-value-prefix-string value false)
	  (transcript-value-string value))))
    (if buffer
	(let ((point (mark-left-inserting-copy (buffer-end buffer))))
	  (guarantee-newlines 1 point)
	  (insert-string value-string point)
	  (insert-newlines 2 point)
	  (mark-temporary! point)))
    (if (or (not buffer) (null? (buffer-windows buffer)))
	(message value-string))))

(define (transcript-value-prefix-string value hash-number?)
  (if (undefined-value? value)
      ";No value"
      (string-append
       ";Value"
       (if (and hash-number?
		(object-pointer? value)
		(not (interned-symbol? value))
		(not (number? value)))
	   (string-append
	    " "
	    (write-to-string (object-hash value)))
	   "")
       ": ")))

(define (transcript-value-string value)
  (if (undefined-value? value)
      ""
      (fluid-let ((*unparser-list-depth-limit*
		   (ref-variable transcript-list-depth-limit))
		  (*unparser-list-breadth-limit*
		   (ref-variable transcript-list-breadth-limit)))
	(write-to-string value))))

(define (call-with-transcript-buffer procedure)
  (let ((buffer (transcript-buffer)))
    (let ((group (buffer-group buffer))
	  (outside)
	  (inside false))
      (dynamic-wind (lambda ()
		      (set! outside (group-read-only? group))
		      (if inside
			  (set-group-read-only! group)
			  (set-group-writeable! group)))
		    (lambda ()
		      (procedure buffer))
		    (lambda ()
		      (set! inside (group-read-only? group))
		      (if outside
			  (set-group-read-only! group)
			  (set-group-writeable! group)))))))

(define (transcript-buffer)
  (let ((name (ref-variable transcript-buffer-name)))
    (if (buffer? name)
	name
	(or (find-buffer name)
	    (let ((buffer (create-buffer name)))
	      (set-buffer-major-mode!
	       buffer
	       (->mode (ref-variable transcript-buffer-mode)))
	      (if (ref-variable transcript-buffer-read-only)
		  (set-buffer-read-only! buffer))
	      (if (ref-variable transcript-disable-evaluation)
		  (add-buffer-initialization! buffer
		    (lambda ()
		      (local-set-variable! disable-evaluation-commands true)
		      (if (eq? (buffer-major-mode buffer)
			       (ref-mode-object scheme))
			  (begin
			    (local-set-variable! evaluate-in-inferior-repl
						 false)
			    (local-set-variable! run-light false))))))
	      buffer)))))