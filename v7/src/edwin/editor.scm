;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/editor.scm,v 1.183 1989/03/14 08:00:27 cph Exp $
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

;;;; Editor Top Level

(declare (usual-integrations))

(define (edwin)
  (if (not edwin-editor)
      (edwin-reset))
  (with-editor-input-port edwin-input-port
    (lambda ()
      (with-editor-interrupts
       (lambda ()
	 (within-editor edwin-editor
	   (lambda ()
	     (perform-buffer-initializations! (current-buffer))
	     (update-screens! true)
	     (if edwin-initialization (edwin-initialization))
	     (let ((message (cmdl-message/null)))
	       (push-cmdl (lambda (cmdl)
			    cmdl		;ignore
			    (top-level-command-reader)
			    message)
			  false
			  message))))))))
  (if edwin-finalization (edwin-finalization))
  unspecific)

;; Set this before entering the editor to get something done after the
;; editor's dynamic environment is initialized, but before the command
;; loop is started.  [Should this bind the ^G interrupt also? -- CPH](define edwin-initialization false)

;; Set this while in the editor to get something done after leaving
;; the editor's dynamic environment; for example, this can be used to
;; reset and then reenter the editor.
(define edwin-finalization false)

(define (within-editor editor thunk)
  (call-with-current-continuation
   (lambda (continuation)
     (fluid-let ((editor-continuation continuation)
		 (recursive-edit-continuation false)
		 (recursive-edit-level 0)
		 (current-editor editor)
		 (*auto-save-keystroke-count* 0))
       (thunk)))))

(define editor-continuation)
(define recursive-edit-continuation)
(define recursive-edit-level)
(define current-editor)

(define (enter-recursive-edit)
  (let ((value
	 (call-with-current-continuation
	   (lambda (continuation)
	     (fluid-let ((recursive-edit-continuation continuation)
			 (recursive-edit-level (1+ recursive-edit-level)))
	       (let ((recursive-edit-event!
		      (lambda ()
			(for-each (lambda (window)
				    (window-modeline-event! window
							    'RECURSIVE-EDIT))
				  (window-list)))))
		 (dynamic-wind recursive-edit-event!
			       command-reader
			       recursive-edit-event!)))))))
    (if (eq? value 'ABORT)
	(abort-current-command)
	(begin
	  (reset-command-prompt!)
	  value))))

(define (exit-recursive-edit value)
  (if recursive-edit-continuation
      (recursive-edit-continuation value)
      (editor-abort value)))

(define (editor-abort value)
  (editor-continuation value))

(define *^G-interrupt-continuations*
  '())

(define (^G-signal)
  (let ((continuations *^G-interrupt-continuations*))
    (if (pair? continuations)
	((car continuations))
	(error "can't signal ^G interrupt"))))

(define (intercept-^G-interrupts interceptor thunk)
  (let ((signal-tag "signal-tag"))
    (let ((value
	   (call-with-current-continuation
	     (lambda (continuation)
	       (fluid-let ((*^G-interrupt-continuations*
			    (cons (lambda () (continuation signal-tag))
				  *^G-interrupt-continuations*)))
		 (thunk))))))
      (if (eq? value signal-tag)
	  (interceptor)
	  value))))