;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/intmod.scm,v 1.39 1991/08/28 21:06:47 arthur Exp $
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
Like Scheme mode, except that a history of evaluated expressions is saved.
The history may be accessed with the following commands:

\\[comint-previous-input] cycles backwards through the input history;
\\[comint-next-input] cycles forwards;
\\[comint-history-search-backward] searches backwards for a matching string;
\\[comint-history-search-forward] searchs forwards."
  (local-set-variable! enable-transcript-buffer true)
  (local-set-variable! transcript-buffer-name (current-buffer))
  (local-set-variable! transcript-input-recorder
		       scheme-interaction-input-recorder)
  (local-set-variable! transcript-output-wrapper
		       scheme-interaction-output-wrapper)
  (local-set-variable! comint-input-ring
		       (make-ring (ref-variable comint-input-ring-size))))

(define (scheme-interaction-input-recorder region)
  (ring-push! (ref-variable comint-input-ring)
	      (region->string region)))

(define (scheme-interaction-output-wrapper thunk)
  (let ((point (buffer-end (current-buffer))))
    (set-current-point! point)
    (with-output-to-mark
     point
     (lambda ()
       (intercept-^G-interrupts
	(lambda ()
	  (fresh-line)
	  (write-string ";Abort!")
	  (fresh-lines 2)
	  (^G-signal))
	thunk)))))

(define-key 'scheme-interaction #\M-p 'comint-previous-input)
(define-key 'scheme-interaction #\M-n 'comint-next-input)

(define-key 'scheme-interaction '(#\C-c #\C-r) 'comint-history-search-backward)
(define-key 'scheme-interaction '(#\C-c #\C-s) 'comint-history-search-forward)