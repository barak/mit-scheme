;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/kmacro.scm,v 1.38 1992/04/16 22:30:00 cph Exp $
;;;
;;;	Copyright (c) 1985, 1989-92 Massachusetts Institute of Technology
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

;;;; Keyboard Macros

(declare (usual-integrations))

(define *defining-keyboard-macro?* false)
(define *executing-keyboard-macro?* false)
(define *keyboard-macro-position*)
(define *keyboard-macro-continuation*)
(define last-keyboard-macro false)
(define keyboard-macro-buffer)
(define keyboard-macro-buffer-end)
(define named-keyboard-macros (make-string-table))

(define (with-keyboard-macro-disabled thunk)
  (fluid-let ((*executing-keyboard-macro?* false)
	      (*defining-keyboard-macro?* false))
    (dynamic-wind keyboard-macro-event
		  thunk
		  keyboard-macro-event)))

(define (keyboard-macro-disable)
  (set! *defining-keyboard-macro?* false)
  (set! *executing-keyboard-macro?* false)
  (keyboard-macro-event))

(define (keyboard-macro-event)
  (window-modeline-event! (current-window) 'KEYBOARD-MACRO-EVENT))

(define (keyboard-macro-read-key)
  (let ((key (keyboard-macro-peek-key)))
    (set! *keyboard-macro-position* (cdr *keyboard-macro-position*))
    key))

(define (keyboard-macro-peek-key)
  (if (null? *keyboard-macro-position*)
      (*keyboard-macro-continuation* true)
      (car *keyboard-macro-position*)))

(define (keyboard-macro-write-key key)
  (set! keyboard-macro-buffer (cons key keyboard-macro-buffer)))

(define (keyboard-macro-finalize-keys)
  (set! keyboard-macro-buffer-end keyboard-macro-buffer))

(define (keyboard-macro-execute *macro repeat)
  (fluid-let ((*executing-keyboard-macro?* true)
	      (*keyboard-macro-position*)
	      (*keyboard-macro-continuation*))
    (define (loop n)
      (set! *keyboard-macro-position* *macro)
      (if (call-with-current-continuation
	   (lambda (c)
	     (set! *keyboard-macro-continuation* c)
	     (command-reader)))
	  (cond ((zero? n) (loop 0))
		((> n 1) (loop (-1+ n))))))
    (if (not (negative? repeat)) (loop repeat))))

(define (keyboard-macro-define name *macro)
  (string-table-put! named-keyboard-macros name last-keyboard-macro)
  (make-command (intern name)
		"Command defined by keyboard macro"
		"P"
		(lambda (#!optional argument)
		  (keyboard-macro-execute *macro
					  (if (or (default-object? argument)
						  (not argument))
					      1
					      argument)))))

(define-command start-kbd-macro
  "Record subsequent keyboard input, defining a keyboard macro.
The commands are recorded even as they are executed.
Use \\[end-kbd-macro] to finish recording and make the macro available.
Use \\[name-last-kbd-macro] to give it a permanent name.
With argument, append to last keyboard macro defined;
 this begins by re-executing that macro as if you typed it again."
  "P"
  (lambda (argument)
    (if *defining-keyboard-macro?*
	(editor-error "Already defining keyboard macro"))
    (cond ((not argument)
	   (set! keyboard-macro-buffer '())
	   (set! keyboard-macro-buffer-end '())
	   (set! *defining-keyboard-macro?* true)
	   (keyboard-macro-event)
	   (message "Defining keyboard macro..."))
	  ((not last-keyboard-macro)
	   (editor-error "No keyboard macro has been defined"))
	  (else
	   (set! *defining-keyboard-macro?* true)
	   (keyboard-macro-event)
	   (message "Appending to keyboard macro...")
	   (keyboard-macro-execute last-keyboard-macro 1)))))

(define-command end-kbd-macro
  "Finish defining a keyboard macro.
The definition was started by \\[start-kbd-macro].
The macro is now available for use via \\[call-last-kbd-macro],
 or it can be given a name with \\[name-last-kbd-macro] and then invoked
 under that name.
With numeric argument, repeat macro now that many times,
 counting the definition just completed as the first repetition."
  "p"
  (lambda (argument)
    (if *defining-keyboard-macro?*
	(begin
	  (set! *defining-keyboard-macro?* false)
	  (keyboard-macro-event)
	  (set! last-keyboard-macro (reverse keyboard-macro-buffer-end))
	  (message "Keyboard macro defined")))
    (cond ((zero? argument)
	   (keyboard-macro-execute last-keyboard-macro 0))
	  ((> argument 1)
	   (keyboard-macro-execute last-keyboard-macro (-1+ argument))))))

(define-command call-last-kbd-macro
  "Call the last keyboard macro that you defined with \\[start-kbd-macro].
To make a macro permanent so you can call it even after
 defining others, use \\[name-last-kbd-macro]."
  "p"
  (lambda (argument)
    (if *defining-keyboard-macro?*
	(editor-error "Can execute anonymous macro while defining one."))
    (if (not last-keyboard-macro)
	(editor-error "No keyboard macro has been defined"))
    (keyboard-macro-execute last-keyboard-macro argument)))

(define-command name-last-kbd-macro
  "Assign a name to the last keyboard macro defined."
  "sName last keyboard macro"
  (lambda (name)
    (if *defining-keyboard-macro?*
	(editor-error "Can't name a keyboard macro while defining one."))
    (if (not last-keyboard-macro)
	(editor-error "No keyboard macro has been defined"))
    (keyboard-macro-define name last-keyboard-macro)))

(define-command write-kbd-macro
  "Save keyboard macro in file.
Use LOAD to load the file.
With argument, also record the keys it is bound to."
  "P"
  (lambda (argument)
    (let ((name
	   (prompt-for-string-table-name "Write keyboard macro"
					 false
					 'NO-DEFAULT
					 named-keyboard-macros
					 true)))
      (let ((pathname
	     (prompt-for-pathname (string-append "Write keyboard macro "
						 name
						 " to file")
				  false
				  false))
	    (buffer (temporary-buffer "*Write-Keyboard-Macro-temp*")))
	(call-with-output-mark (buffer-point buffer)
	  (lambda (port)
	    (pretty-print
	     `(IN-PACKAGE EDWIN-PACKAGE
		(KEYBOARD-MACRO-DEFINE
		 ',name
		 ',(string-table-get named-keyboard-macros name))
		,@(if argument
		      (map (lambda (key)
			     `(DEFINE-KEY 'FUNDAMENTAL ',key ',name))
			   (comtab-key-bindings
			    (mode-comtabs (ref-mode-object fundamental))
			    (name->command name)))
		      '()))
	     port
	     true)))
	(set-buffer-pathname! buffer pathname)
	(write-buffer buffer)
	(kill-buffer buffer)))))

(define-command kbd-macro-query
  "Query user during keyboard macro execution.
With prefix argument, enters recursive edit,
 reading keyboard commands even within a keyboard macro.
 You can give different commands each time the macro executes.
Without argument, reads a character.  Your options are:
 Space -- execute the rest of the macro.
 Rubout -- skip the rest of the macro; start next repetition.
 C-d -- skip the rest of the macro and don't repeat it any more.
 C-r -- Enter a recursive edit, then on exit ask again for a character
 C-l -- redisplay screen and ask again."
  "P"
  (lambda (argument)
    (cond ((and (not *defining-keyboard-macro?*)
		(not *executing-keyboard-macro?*))
	   (editor-error "Not defining or executing kbd macro"))
	  (argument
	   (with-keyboard-macro-disabled enter-recursive-edit))
	  (*executing-keyboard-macro?*
	   (let loop ()
	     (let ((char
		    (with-keyboard-macro-disabled
		     (lambda ()
		       (set-command-prompt!
			"Proceed with macro? (Space, DEL, C-d, C-r or C-l)")
		       (keyboard-read)))))
	       (let ((test-for
		      (lambda (char*)
			(char=? char (remap-alias-key char*)))))
		 (cond ((input-event? char)
			(abort-current-command char))
		       ((not (char? char))
			(editor-beep)
			(loop))
		       ((test-for #\space)
			unspecific)
		       ((test-for #\rubout)
			(*keyboard-macro-continuation* true))
		       ((test-for #\C-d)
			(*keyboard-macro-continuation* false))
		       ((test-for #\C-r)
			(with-keyboard-macro-disabled enter-recursive-edit)
			(loop))
		       ((test-for #\C-l)
			((ref-command recenter) false)
			(loop))
		       (else
			(editor-beep)
			(loop))))))))))