;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/editor.scm,v 1.198 1990/11/02 03:23:48 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989, 1990 Massachusetts Institute of Technology
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

;;;; Editor Top Level

(declare (usual-integrations))

(define (edit)
  (if (not edwin-editor)
      (create-editor))
  (call-with-current-continuation
   (lambda (continuation)
     (fluid-let ((editor-abort continuation)
		 (*auto-save-keystroke-count* 0)
		 (current-editor edwin-editor)
		 (recursive-edit-continuation false)
		 (recursive-edit-level 0))
       (editor-grab-display edwin-editor
	 (lambda (with-editor-ungrabbed)
	   (let ((message (cmdl-message/null)))
	     (push-cmdl (lambda (cmdl)
			  cmdl ;ignore
			  (top-level-command-reader edwin-initialization)
			  message)
			false
			message
			(editor-spawn-child-cmdl with-editor-ungrabbed))))))))
  (if edwin-finalization (edwin-finalization))
  unspecific)

(define (editor-grab-display editor receiver)
  (display-type/with-display-grabbed (editor-display-type editor)
    (lambda (with-display-ungrabbed)
      (with-current-local-bindings!
	(lambda ()
	  (let ((enter
		 (lambda ()
		   (let ((screen (selected-screen)))
		     (screen-enter! screen)
		     (update-screen! screen true))))
		(exit (lambda () (screen-exit! (selected-screen)))))
	    (dynamic-wind enter
			  (lambda ()
			    (receiver
			     (lambda (thunk)
			       (dynamic-wind exit
					     (lambda ()
					       (with-display-ungrabbed thunk))
					     enter))))
			  exit)))))))

(define (editor-spawn-child-cmdl with-editor-ungrabbed)
  (lambda (editor-cmdl input-port output-port driver state message spawn-child)
    (with-editor-ungrabbed
     (lambda ()
       (make-cmdl editor-cmdl
		  (if (eq? input-port (cmdl/input-port editor-cmdl))
		      (cmdl/input-port (cmdl/parent editor-cmdl))
		      input-port)
		  (if (eq? output-port (cmdl/output-port editor-cmdl))
		      (cmdl/output-port (cmdl/parent editor-cmdl))
		      output-port)
		  driver
		  state
		  message
		  spawn-child)))))

(define (within-editor?)
  (not (unassigned? current-editor)))

(define editor-abort)
(define edwin-editor false)
(define current-editor)

;; Set this before entering the editor to get something done after the
;; editor's dynamic environment is initialized, but before the command
;; loop is started.
(define edwin-initialization false)

;; Set this while in the editor to get something done after leaving
;; the editor's dynamic environment; for example, this can be used to
;; reset and then reenter the editor.
(define edwin-finalization false)

(define create-editor-args
  (list false))

(define (create-editor . args)
  (let ((args
	 (if (null? args)
	     create-editor-args
	     (begin
	       (set! create-editor-args args)
	       args))))
    (reset-editor)
    (initialize-typein!)
    (initialize-typeout!)
    (initialize-syntax-table!)
    (initialize-command-reader!)
    (set! edwin-editor
	  (make-editor "Edwin"
		       (let ((name (car args)))
			 (cond (name
				(name->display-type name))
			       ((display-type/available? console-display-type)
				console-display-type)
			       ((display-type/available? x-display-type)
				x-display-type)
			       (else
				(error "can't find usable display type"))))
		       (cdr args)))
    (set! edwin-initialization
	  (lambda ()
	    (set! edwin-initialization false)
	    (with-editor-interrupts-disabled standard-editor-initialization)))
    unspecific))

(define (standard-editor-initialization)
  (if (not init-file-loaded?)
      (begin
	(let ((filename (os/init-file-name)))
	  (if (file-exists? filename)
	      (load-edwin-file filename '(EDWIN) true)))
	(set! init-file-loaded? true)))
  (if (not (ref-variable inhibit-startup-message))
      (let ((window (current-window)))
	(let ((buffer (window-buffer window)))
	  (dynamic-wind
	   (lambda () unspecific)
	   (lambda ()
	     (with-output-to-mark (window-point window)
				  write-initial-buffer-greeting!)
	     (set-window-start-mark! window (buffer-start buffer) false)
	     (buffer-not-modified! buffer)
	     (sit-for 120000))
	   (lambda ()
	     (region-delete! (buffer-unclipped-region buffer))
	     (buffer-not-modified! buffer)))))))

(define inhibit-editor-init-file? false)
(define init-file-loaded? false)

(define-variable inhibit-startup-message
  "*True inhibits the initial startup messages.
This is for use in your personal init file, once you are familiar
with the contents of the startup message."
  false)

(define (write-initial-buffer-greeting!)
  (identify-world)
  (write-string initial-buffer-greeting))

(define initial-buffer-greeting
  "

;You are in an interaction window of the Edwin editor.
;Type C-h for help.  C-h m will describe some commands.

")

(define (reset-editor)
  (without-interrupts
   (lambda ()
     (if edwin-editor
	 (begin
	   (for-each (lambda (screen)
		       (screen-discard! screen))
		     (editor-screens edwin-editor))
	   (set! edwin-editor false)
	   (set! init-file-loaded? false)
	   (set! *previous-popped-up-buffer* (object-hash false))
	   (set! *previous-popped-up-window* (object-hash false))
	   unspecific)))))

(define (reset-editor-windows)
  (for-each (lambda (screen)
	      (send (screen-root-window screen) ':salvage!))
	    (editor-screens edwin-editor)))

;;; There is a problem with recursive edits and multiple screens.
;;; When you switch screens the recursive edit aborts. The problem
;;; is that a top level ^G in a recursive edit aborts the recursive
;;; edit and a ^G is signalled when you switch screens. I think that
;;; ^G should not abort a recursive edit.

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
      (editor-error "No recursive edit is in progress")))

(define recursive-edit-continuation)
(define recursive-edit-level)

(define (internal-error-handler condition)
  (and (not (condition/internal? condition))
       (error? condition)
       (cond ((ref-variable debug-on-internal-error)
	      (debug-scheme-error condition)
	      (message "Scheme error")
	      (%editor-error))
	     (debug-internal-errors?
	      (signal-error condition))
	     (else
	      (exit-editor-and-signal-error condition)))))

(define-variable debug-on-internal-error
  "True means enter debugger if error is signalled while the editor is running.
This does not affect editor errors or evaluation errors."
  false)

(define debug-internal-errors?
  false)

(define (exit-editor-and-signal-error condition)
  (within-continuation editor-abort
    (lambda ()
      (signal-error condition))))

(define (^G-signal)
  (let ((continuations *^G-interrupt-continuations*))
    (if (not (pair? continuations))
	(error "can't signal ^G interrupt"))
    ((car continuations))))

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

(define *^G-interrupt-continuations*
  '())