;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/intmod.scm,v 1.34 1989/04/25 02:07:56 cph Exp $
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

(define-command scheme-interaction-mode
  "Make the current mode be Scheme Interaction mode."
  ()
  (lambda ()
    (set-current-major-mode! (ref-mode-object scheme-interaction))))

(define-major-mode scheme-interaction scheme "Scheme Interaction"
  "Major mode for evaluating Scheme expressions interactively.
Same as Scheme mode, except for

\\[scheme-interaction-eval-previous-sexp] evaluates the current expression.
\\[scheme-interaction-eval-definition] evaluates the current definition.
\\[scheme-interaction-eval-region] evaluates the region.
\\[scheme-interaction-yank] yanks the most recently evaluated expression.
\\[scheme-interaction-yank-pop] yanks an earlier expression, replacing a yank."
  (local-set-variable! scheme-interaction-kill-ring (make-ring 32))
  (local-set-variable! scheme-environment (ref-variable scheme-environment))
  (local-set-variable! scheme-syntax-table (ref-variable scheme-syntax-table)))

(define-prefix-key 'scheme-interaction #\C-x 'prefix-char)
(define-prefix-key 'scheme-interaction #\C-c 'prefix-char)
(define-key 'scheme-interaction '(#\C-x #\C-e)
  'scheme-interaction-eval-previous-sexp)
(define-key 'scheme-interaction #\M-return
  'scheme-interaction-eval-previous-sexp)
(define-key 'scheme-interaction #\M-z 'scheme-interaction-eval-definition)
(define-key 'scheme-interaction #\C-M-z 'scheme-interaction-eval-region)
(define-key 'scheme-interaction '(#\C-c #\C-y) 'scheme-interaction-yank)
(define-key 'scheme-interaction '(#\C-c #\C-r) 'scheme-interaction-yank-pop)

(define-variable scheme-interaction-kill-ring
  "Kill ring used by Interaction mode evaluation commands.")

(define (scheme-interaction-eval-region region argument)
  (set-current-point! (region-end region))
  (let ((string (region->string region)))
    (ring-push! (ref-variable scheme-interaction-kill-ring) string)
    (let ((expression (with-input-from-string string read)))
      (let ((value
	     (with-output-to-current-point
	      (lambda ()
		(intercept-^G-interrupts
		 (lambda ()
		   (guarantee-newline)
		   (insert-string "Abort!")
		   (insert-newlines 2)
		   (^G-signal))
		 (lambda ()
		   (eval-with-history expression
				      (evaluation-environment argument))))))))
	(guarantee-newline)
	(if (undefined-value? value)
	    (insert-string ";No value")
	    (begin
	      (insert-string ";Value: ")
	      (insert-string (scheme-interaction-object->string value))))
	(guarantee-newlines 2)))))

(define (scheme-interaction-object->string object)
  (fluid-let ((*unparser-list-depth-limit* 5)
	      (*unparser-list-breadth-limit* 10))
    (write-to-string object)))

(define-command scheme-interaction-eval-previous-sexp
  "Evaluate the expression to the left of point."
  "P"
  (lambda (argument)
    (let ((point (current-point)))
      (scheme-interaction-eval-region
       (make-region (backward-one-sexp point) point)
       argument))))

(define-command scheme-interaction-eval-definition
  "Evaluate the definition at point.
Moves point to the definition's end.
Output and the result are written at that point.
With an argument, prompts for the evaluation environment."
  "P"
  (lambda (argument)
    (scheme-interaction-eval-region
     (let ((start (current-definition-start)))
       (make-region start (forward-one-definition-end start)))
     argument)))

(define-command scheme-interaction-eval-region
  "Evaluate the definition at point.
Moves point to the definition's end.
Output and the result are written at that point.
With an argument, prompts for the evaluation environment."
  "r\nP"
  scheme-interaction-eval-region)

(define scheme-interaction-mode:yank-command-message
  "Yank")

(define-command scheme-interaction-yank
  "Yank the last input expression."
  ()
  (lambda ()
    (push-current-mark! (mark-right-inserting (current-point)))
    (insert-string (ring-ref (ref-variable scheme-interaction-kill-ring) 0))
    (set-command-message! scheme-interaction-mode:yank-command-message)))

(define-command scheme-interaction-yank-pop
  "Yank the last input expression."
  ()
  (lambda ()
    (command-message-receive scheme-interaction-mode:yank-command-message
      (lambda ()
	(delete-string (pop-current-mark!) (current-point))
	(push-current-mark! (mark-right-inserting (current-point)))
	(ring-pop! (ref-variable scheme-interaction-kill-ring))
	(insert-string
	 (ring-ref (ref-variable scheme-interaction-kill-ring) 0))
	(set-command-message! scheme-interaction-mode:yank-command-message))
      (lambda ()
	(editor-error "No previous yank to replace")))))