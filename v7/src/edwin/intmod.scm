;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/intmod.scm,v 1.33 1989/04/23 23:23:00 cph Exp $
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

(define-command interaction-mode
  "Make the current mode be Interaction mode."
  ()
  (lambda ()
    (set-current-major-mode! (ref-mode-object interaction))))

(define-major-mode interaction scheme "Interaction"
  "Major mode for evaluating Scheme expressions interactively.
Same as Scheme mode, except for

\\[interaction-eval-previous-sexp] evaluates the current expression.
\\[interaction-eval-definition] evaluates the current definition.
\\[interaction-eval-region] evaluates the region.
\\[interaction-yank] yanks the most recently evaluated expression.
\\[interaction-yank-pop] yanks an earlier expression, replacing a yank."
  (local-set-variable! interaction-kill-ring (make-ring 32))
  (local-set-variable! scheme-environment (ref-variable scheme-environment))
  (local-set-variable! scheme-syntax-table (ref-variable scheme-syntax-table)))

(define-prefix-key 'interaction #\C-x 'prefix-char)
(define-prefix-key 'interaction #\C-c 'prefix-char)
(define-key 'interaction '(#\C-x #\C-e) 'interaction-eval-previous-sexp)
(define-key 'interaction #\M-return 'interaction-eval-previous-sexp)
(define-key 'interaction #\M-z 'interaction-eval-definition)
(define-key 'interaction #\C-M-z 'interaction-eval-region)
(define-key 'interaction '(#\C-c #\C-y) 'interaction-yank)
(define-key 'interaction '(#\C-c #\C-r) 'interaction-yank-pop)

(define-variable interaction-kill-ring
  "Kill ring used by Interaction mode evaluation commands.")

(define (interaction-eval-region region argument)
  (set-current-point! (region-end region))
  (let ((string (region->string region)))
    (ring-push! (ref-variable interaction-kill-ring) string)
    (let ((expression (with-input-from-string string read)))
      (let ((value
	     (with-output-to-current-point
	      (lambda ()
		(intercept-^G-interrupts
		 (lambda ()
		   (interaction-guarantee-newlines 1)
		   (insert-string "Abort!")
		   (insert-newlines 2)
		   (^G-signal))
		 (lambda ()
		   (eval-with-history expression
				      (evaluation-environment argument))))))))
	(interaction-guarantee-newlines 1)
	(if (undefined-value? value)
	    (insert-string ";No value")
	    (begin
	      (insert-string ";Value: ")
	      (insert-string (interaction-object->string value))))
	(interaction-guarantee-newlines 2)))))

(define (interaction-guarantee-newlines n)
  (insert-newlines (if (line-start? (current-point)) (-1+ n) n)))

(define (interaction-object->string object)
  (fluid-let ((*unparser-list-depth-limit* 2)
	      (*unparser-list-breadth-limit* 5))
    (write-to-string object)))

(define-command interaction-eval-previous-sexp
  "Evaluate the expression to the left of point."
  "P"
  (lambda (argument)
    (let ((point (current-point)))
      (interaction-eval-region (make-region (backward-one-sexp point) point)
			       argument))))

(define-command interaction-eval-definition
  "Evaluate the definition at point.
Moves point to the definition's end.
Output and the result are written at that point.
With an argument, prompts for the evaluation environment."
  "P"
  (lambda (argument)
    (interaction-eval-region
     (let ((start (current-definition-start)))
       (make-region start (forward-one-definition-end start)))
     argument)))

(define-command interaction-eval-region
  "Evaluate the definition at point.
Moves point to the definition's end.
Output and the result are written at that point.
With an argument, prompts for the evaluation environment."
  "r\nP"
  interaction-eval-region)

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