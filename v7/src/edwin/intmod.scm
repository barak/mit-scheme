;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/intmod.scm,v 1.36 1989/08/09 13:17:37 cph Rel $
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

;;;; Interaction Mode
;;; Package: (edwin)

(declare (usual-integrations))

(define-command scheme-interaction-mode
  "Make the current mode be Scheme Interaction mode."
  ()
  (lambda ()
    (set-current-major-mode! (ref-mode-object scheme-interaction))))

(define-major-mode scheme-interaction scheme "Scheme Interaction"
  "Major mode for evaluating Scheme expressions interactively.
Same as Scheme mode, except for

\\[scheme-interaction-yank] yanks the most recently evaluated expression.
\\[scheme-interaction-yank-pop] yanks an earlier expression, replacing a yank."
  (local-set-variable! enable-transcript-buffer true)
  (local-set-variable! transcript-buffer-name (current-buffer))
  (local-set-variable! transcript-input-recorder
		       scheme-interaction-input-recorder)
  (local-set-variable! transcript-output-wrapper
		       scheme-interaction-output-wrapper)
  (local-set-variable! scheme-interaction-kill-ring (make-ring 32)))

(define (scheme-interaction-input-recorder region)
  (ring-push! (ref-variable scheme-interaction-kill-ring)
	      (region->string region)))

(define (scheme-interaction-output-wrapper thunk)
  (set-current-point! (buffer-end (current-buffer)))
  (with-output-to-current-point
   (lambda ()
     (intercept-^G-interrupts
      (lambda ()
	(fresh-line)
	(write-string ";Abort!")
	(fresh-lines 2)
	(^G-signal))
      thunk))))

(define-prefix-key 'scheme-interaction #\C-c 'prefix-char)
(define-key 'scheme-interaction '(#\C-c #\C-y) 'scheme-interaction-yank)
(define-key 'scheme-interaction '(#\C-c #\C-r) 'scheme-interaction-yank-pop)

(define-variable scheme-interaction-kill-ring
  "Kill ring used by Interaction mode evaluation commands.")

(define scheme-interaction-mode:yank-command-message "Yank")

(define-command scheme-interaction-yank
  "Re-insert the last input expression.
Puts point after it and the mark before it."
  ()
  (lambda ()
    (let ((kill-ring (ref-variable scheme-interaction-kill-ring)))
      (if (ring-empty? kill-ring)
	  (editor-error "Nothing to yank"))
      (push-current-mark! (mark-right-inserting (current-point)))
      (insert-string (ring-ref kill-ring 0))
      (set-command-message! scheme-interaction-mode:yank-command-message))))

(define-command scheme-interaction-yank-pop
  "Correct after \\[scheme-interaction-yank] to use an earlier expression.
Requires that the region contain the most recent expression,
as it does immediately after using \\[scheme-interaction-yank].
It is deleted and replaced with the previous expression,
which is rotated to the front of the expression ring."
  ()
  (lambda ()
    (let ((kill-ring (ref-variable scheme-interaction-kill-ring)))
      (if (ring-empty? kill-ring)
	  (editor-error "Nothing to yank"))
      (command-message-receive scheme-interaction-mode:yank-command-message
	(lambda ()
	  (delete-string (pop-current-mark!) (current-point))
	  (push-current-mark! (mark-right-inserting (current-point)))
	  (ring-pop! kill-ring)
	  (insert-string (ring-ref kill-ring 0))
	  (set-command-message! scheme-interaction-mode:yank-command-message))
	(lambda ()
	  (editor-error "No previous yank to replace"))))))