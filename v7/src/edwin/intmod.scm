;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/intmod.scm,v 1.31 1989/03/30 16:39:58 jinx Exp $
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

;;;; Interaction Mode

(declare (usual-integrations))

(define-major-mode "Interaction" "Scheme"
  "Major mode for evaluating Scheme expressions interactively.
Same as Scheme mode, except for

\\[^R Interaction Execute] evaluates the current expression.
\\[^R Interaction Refresh] deletes the contents of the buffer.
\\[^R Interaction Yank] yanks the last expression.
\\[^R Interaction Yank Pop] yanks an earlier expression, replacing a yank."
  (local-set-variable! "Interaction Prompt"
		       (ref-variable "Interaction Prompt"))
  (local-set-variable! "Interaction Kill Ring" (make-ring 32))
  (local-set-variable! "Scheme Environment"
		       (ref-variable "Scheme Environment"))
  (local-set-variable! "Scheme Syntax-table"
		       (ref-variable "Scheme Syntax-table")))

(define-key "Interaction" #\Return "^R Interaction Execute")
(define-prefix-key "Interaction" #\C-C "^R Prefix Character")
(define-key "Interaction" '(#\C-C #\Page) "^R Interaction Refresh")
(define-key "Interaction" '(#\C-C #\C-Y) "^R Interaction Yank")
(define-key "Interaction" '(#\C-C #\C-R) "^R Interaction Yank Pop")

(define-command ("Interaction Mode")
  "Make the current mode be Interaction mode."
  (set-current-major-mode! Interaction-mode)
  (let ((buffer (current-buffer)))
    (if (not (mark= (buffer-start buffer) (buffer-end buffer)))
	(begin (set-current-point! (buffer-end buffer))
	       (insert-interaction-prompt))
	(insert-interaction-prompt false))))

(define (insert-interaction-prompt #!optional newlines?)
  (if (or (default-object? newlines?) newlines?)
      (insert-newlines 2))
  (insert-string "1 ")
  (insert-string (ref-variable "Interaction Prompt"))
  (insert-string " ")
  (buffer-put! (current-buffer)
	       interaction-mode:buffer-mark-tag
	       (mark-right-inserting (current-point))))

(define interaction-mode:buffer-mark-tag
  "Mark")

(define-variable "Interaction Prompt"
  "Prompt string used by Interaction mode."
  "]=>")

(define-variable "Interaction Kill Ring"
  "Kill ring used by Interaction mode evaluation commands.")

(define-command ("^R Interaction Execute" argument)
  "Evaluate the input expression.
With an argument, calls ^R Insert Self instead.

If invoked in the current `editing area', evaluates the expression there.
 The editing area is defined as the space between the last prompt and
 the end of the buffer.  The expression is checked to make sure that it
 is properly balanced, and that there is only one such expression.

Otherwise, goes to the end of the current line, copies the preceding
 expression to the editing area, then evaluates it.  In this case the
 editing area must be empty.

Output is inserted into the buffer at the end."
  (define (extract-expression start)
    (let ((expression (extract-string start (or (forward-one-sexp start)
						(editor-error "No Expression")))))
      (ring-push! (ref-variable "Interaction Kill Ring") expression)
      expression))

  (if argument
      (^r-insert-self-command argument)
      (let ((mark (or (buffer-get (current-buffer)
				  interaction-mode:buffer-mark-tag)
		      (error "Missing interaction buffer mark")))
	    (point (current-point)))
	(if (mark< point (line-start mark 0))
	    (begin
	     (if (not (group-end? mark))
		 (editor-error "Can't copy: unfinished expression"))
	     (let ((start (backward-one-sexp (line-end point 0))))
	       (if (not start) (editor-error "No previous expression"))
	       (let ((expression (extract-expression start)))
		 (set-current-point! mark)
		 (insert-string expression mark))))
	    (let ((state (parse-partial-sexp mark (group-end mark))))
	      (if (or (not (zero? (parse-state-depth state)))
		      (parse-state-in-string? state)
		      (parse-state-in-comment? state)
		      (parse-state-quoted? state))
		  (editor-error "Imbalanced expression"))
	      (let ((last-sexp (parse-state-last-sexp state)))
		(if (not last-sexp)
		    (editor-error "No expression"))
		(extract-expression last-sexp))
	      (set-current-point! (group-end point))))
	(dynamic-wind
	 (lambda () 'DONE)
	 (lambda ()
	   (with-output-to-current-point
	    (lambda ()
	      (intercept-^G-interrupts
	       (lambda ()
		 (newline)
		 (write-string "Abort!"))
	       (lambda ()
		 (write-line
		  (eval-with-history (with-input-from-mark mark
							   read)
				     (evaluation-environment false))))))))
	 insert-interaction-prompt))))

(define-command ("^R Interaction Refresh")
  "Delete the contents of the buffer, then prompt for input.
Preserves the current `editing area'."
  (let ((buffer (current-buffer)))
    (let ((edit-area
	   (extract-string (buffer-get buffer interaction-mode:buffer-mark-tag)
			   (buffer-end buffer))))
      (region-delete! (buffer-region buffer))
      (insert-interaction-prompt false)
      (insert-string edit-area))))

(define interaction-mode:yank-command-message
  "Yank")

(define-command ("^R Interaction Yank")
  "Yank the last input expression."
  (push-current-mark! (mark-right-inserting (current-point)))
  (insert-string (ring-ref (ref-variable "Interaction Kill Ring") 0))
  (set-command-message! interaction-mode:yank-command-message))

(define-command ("^R Interaction Yank Pop")
  "Yank the last input expression."
  (command-message-receive interaction-mode:yank-command-message
    (lambda ()
      (delete-string (pop-current-mark!) (current-point))
      (push-current-mark! (mark-right-inserting (current-point)))
      (ring-pop! (ref-variable "Interaction Kill Ring"))
      (insert-string (ring-ref (ref-variable "Interaction Kill Ring") 0))
      (set-command-message! interaction-mode:yank-command-message))
    (lambda ()
      (editor-error "No previous yank to replace"))))