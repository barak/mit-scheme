;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/intmod.scm,v 1.32 1989/04/15 00:50:13 cph Exp $
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

(define-major-mode interaction scheme "Interaction"
  "Major mode for evaluating Scheme expressions interactively.
Same as Scheme mode, except for

\\[interaction-execute] evaluates the current expression.
\\[interaction-refresh] deletes the contents of the buffer.
\\[interaction-yank] yanks the last expression.
\\[interaction-yank-pop] yanks an earlier expression, replacing a yank."
  (local-set-variable! interaction-prompt (ref-variable interaction-prompt))
  (local-set-variable! interaction-kill-ring (make-ring 32))
  (local-set-variable! scheme-environment (ref-variable scheme-environment))
  (local-set-variable! scheme-syntax-table (ref-variable scheme-syntax-table)))

(define-key 'interaction #\return 'interaction-execute)
(define-prefix-key 'interaction #\c-c 'prefix-char)
(define-key 'interaction '(#\c-c #\page) 'interaction-refresh)
(define-key 'interaction '(#\c-c #\c-y) 'interaction-yank)
(define-key 'interaction '(#\c-c #\c-r) 'interaction-yank-pop)

(define-command interaction-mode
  "Make the current mode be Interaction mode."
  ()
  (lambda ()
    (set-current-major-mode! (ref-mode-object interaction))
    (let ((buffer (current-buffer)))
      (if (not (mark= (buffer-start buffer) (buffer-end buffer)))
	  (begin (set-current-point! (buffer-end buffer))
		 (insert-interaction-prompt))
	  (insert-interaction-prompt false)))))

(define (insert-interaction-prompt #!optional newlines?)
  (if (or (default-object? newlines?) newlines?)
      (insert-newlines 2))
  (insert-string "1 ")
  (insert-string (ref-variable interaction-prompt))
  (insert-string " ")
  (buffer-put! (current-buffer)
	       interaction-mode:buffer-mark-tag
	       (mark-right-inserting (current-point))))

(define interaction-mode:buffer-mark-tag
  "Mark")

(define-variable interaction-prompt
  "Prompt string used by Interaction mode."
  "]=>")

(define-variable interaction-kill-ring
  "Kill ring used by Interaction mode evaluation commands.")

(define-command interaction-execute
  "Evaluate the input expression.
With an argument, calls \\[self-insert-command] instead.

If invoked in the current `editing area', evaluates the expression there.
 The editing area is defined as the space between the last prompt and
 the end of the buffer.  The expression is checked to make sure that it
 is properly balanced, and that there is only one such expression.

Otherwise, goes to the end of the current line, copies the preceding
 expression to the editing area, then evaluates it.  In this case the
 editing area must be empty.

Output is inserted into the buffer at the end."
  "P"
  (lambda (argument)
    (define (extract-expression start)
      (let ((expression
	     (extract-string start
			     (or (forward-one-sexp start)
				 (editor-error "No Expression")))))
	(ring-push! (ref-variable interaction-kill-ring) expression)
	expression))

    (if argument
	((ref-command self-insert-command) argument)
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
	   insert-interaction-prompt)))))

(define-command interaction-refresh
  "Delete the contents of the buffer, then prompt for input.
Preserves the current `editing area'."
  ()
  (lambda ()
    (let ((buffer (current-buffer)))
      (let ((edit-area
	     (extract-string
	      (buffer-get buffer interaction-mode:buffer-mark-tag)
	      (buffer-end buffer))))
	(region-delete! (buffer-region buffer))
	(insert-interaction-prompt false)
	(insert-string edit-area)))))

(define interaction-mode:yank-command-message
  "Yank")

(define-command interaction-yank
  "Yank the last input expression."
  ()
  (lambda ()
    (push-current-mark! (mark-right-inserting (current-point)))
    (insert-string (ring-ref (ref-variable interaction-kill-ring) 0))
    (set-command-message! interaction-mode:yank-command-message)))

(define-command interaction-yank-pop
  "Yank the last input expression."
  ()
  (lambda ()
    (command-message-receive interaction-mode:yank-command-message
      (lambda ()
	(delete-string (pop-current-mark!) (current-point))
	(push-current-mark! (mark-right-inserting (current-point)))
	(ring-pop! (ref-variable interaction-kill-ring))
	(insert-string (ring-ref (ref-variable interaction-kill-ring) 0))
	(set-command-message! interaction-mode:yank-command-message))
      (lambda ()
	(editor-error "No previous yank to replace")))))