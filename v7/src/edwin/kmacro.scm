;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1985 Massachusetts Institute of Technology
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
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Keyboard Macros

(declare (usual-integrations))
(using-syntax edwin-syntax-table

(define *defining-keyboard-macro?* false)
(define *executing-keyboard-macro?* false)
(define *keyboard-macro-position*)
(define *keyboard-macro-continuation*)
(define last-keyboard-macro false)
(define keyboard-macro-buffer)
(define keyboard-macro-buffer-end)
(define named-keyboard-macros (make-string-table))

(define (with-keyboard-macro-disabled thunk)
  (define old-executing)
  (define old-defining)
  (define new-executing false)
  (define new-defining false)
  (dynamic-wind (lambda ()
		  (set! old-executing
			(set! *executing-keyboard-macro?*
			      (set! new-executing)))
		  (set! old-defining
			(set! *defining-keyboard-macro?*
			      (set! new-defining)))
		  (if (not (eq? old-defining *defining-keyboard-macro?*))
		      (keyboard-macro-event)))
		thunk
		(lambda ()
		  (set! new-executing
			(set! *executing-keyboard-macro?*
			      (set! old-executing)))
		  (set! new-defining
			(set! *defining-keyboard-macro?*
			      (set! old-defining)))
		  (if (not (eq? new-defining *defining-keyboard-macro?*))
		      (keyboard-macro-event)))))

(define (keyboard-macro-disable)
  (set! *defining-keyboard-macro?* false)
  (set! *executing-keyboard-macro?* false)
  (keyboard-macro-event))

(define (keyboard-macro-event)
  (window-modeline-event! (current-window) 'KEYBOARD-MACRO-EVENT))

(define (keyboard-macro-read-char)
  (let ((char (keyboard-macro-peek-char)))
    (set! *keyboard-macro-position* (cdr *keyboard-macro-position*))
    char))

(define (keyboard-macro-peek-char)
  (if (null? *keyboard-macro-position*)
      (*keyboard-macro-continuation* true)
      (car *keyboard-macro-position*)))

(define (keyboard-macro-write-char char)
  (set! keyboard-macro-buffer (cons char keyboard-macro-buffer)))

(define (keyboard-macro-finalize-chars)
  (set! keyboard-macro-buffer-end keyboard-macro-buffer))

(define (keyboard-macro-execute macro repeat)
  (fluid-let ((*executing-keyboard-macro?* true)
	      (*keyboard-macro-position*)
	      (*keyboard-macro-continuation*))
    (define (loop n)
      (set! *keyboard-macro-position* macro)
      (if (call-with-current-continuation
	   (lambda (c)
	     (set! *keyboard-macro-continuation* c)
	     (command-reader)))
	  (cond ((zero? n) (loop 0))
		((> n 1) (loop (-1+ n))))))
    (if (not (negative? repeat)) (loop repeat))))

(define (keyboard-macro-define name macro)
  (string-table-put! named-keyboard-macros name last-keyboard-macro)
  (make-command name "Command defined by keyboard macro"
		(lambda (#!optional argument)
		  (if (or (unassigned? argument) (not argument))
		      (set! argument 1))
		  (keyboard-macro-execute macro argument))))

(define-command ("Start Keyboard Macro" argument)
  "Record subsequent keyboard input, defining a keyboard macro.
The commands are recorded even as they are executed.
Use \\[End Keyboard Macro] to finish recording and make the macro available.
Use \\[Name Last Keyboard Macro] to give it a permanent name.
With argument, append to last keyboard macro defined;
 this begins by re-executing that macro as if you typed it again."
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
	 (keyboard-macro-execute last-keyboard-macro 1))))

(define-command ("End Keyboard Macro" (argument 1))
  "Finish defining a keyboard macro.
The definition was started by \\[Start Keyboard Macro].
The macro is now available for use via \\[Call Last Keyboard Macro],
 or it can be given a name with \\[Name Last Keyboard Macro] and then invoked
 under that name.
With numeric argument, repeat macro now that many times,
 counting the definition just completed as the first repetition."
  (if *defining-keyboard-macro?*
      (begin (set! *defining-keyboard-macro?* false)
	     (keyboard-macro-event)
	     (set! last-keyboard-macro (reverse keyboard-macro-buffer-end))
	     (message "Keyboard macro defined")))
  (cond ((zero? argument)
	 (keyboard-macro-execute last-keyboard-macro 0))
	((> argument 1)
	 (keyboard-macro-execute last-keyboard-macro (-1+ argument)))))

(define-command ("Call Last Keyboard Macro" (argument 1))
  "Call the last keyboard macro that you defined with \\[Start Keyboard Macro].
To make a macro permanent so you can call it even after
 defining others, use \\[Name Last Keyboard Macro]."
  (if *defining-keyboard-macro?*
      (editor-error "Can execute anonymous macro while defining one."))
  (if (not last-keyboard-macro)
      (editor-error "No keyboard macro has been defined"))
  (keyboard-macro-execute last-keyboard-macro argument))

(define-command ("Name Last Keyboard Macro" argument)
  "Assign a name to the last keyboard macro defined."
  (if *defining-keyboard-macro?*
      (editor-error "Can't name a keyboard macro while defining one."))
  (if (not last-keyboard-macro)
      (editor-error "No keyboard macro has been defined"))
  (keyboard-macro-define (prompt-for-string "Name last keyboard macro" false)
			 last-keyboard-macro))

(define-command ("Write Keyboard Macro" argument)
  "Save keyboard macro in file.
Use LOAD to load the file.
With argument, also record the keys it is bound to."
  (let ((name (prompt-for-completed-string "Write keyboard macro"
					   false 'NO-DEFAULT
					   named-keyboard-macros
					   'STRICT-COMPLETION)))
    (let ((pathname (prompt-for-pathname (string-append "Write keyboard macro "
							name
							" to file")
					 (current-default-pathname)))
	  (buffer (temporary-buffer "*Write-Keyboard-Macro-temp*")))
      (with-output-to-mark (buffer-point buffer)
	(lambda ()
	  (write-string "(IN-PACKAGE EDWIN-PACKAGE")
	  (newline) (write-string "  (KEYBOARD-MACRO-DEFINE ") (write name)
	  (newline) (write-string "    '")
	  (write (string-table-get named-keyboard-macros name))
	  (write-string ")")
	  (if argument
	      (for-each (lambda (key)
			  (newline)
			  (write-string "  (DEFINE-KEY \"Fundamental\" '")
			  (write key)
			  (write-string " ")
			  (write name)
			  (write-string ")"))
			(comtab-key-bindings (mode-comtabs fundamental-mode)
					     (name->command name))))
	  (newline) (write-string ")")))
      (set-buffer-pathname! buffer pathname)
      (write-buffer buffer)
      (kill-buffer buffer))))

(define-command ("Keyboard Macro Query" argument)
  "Query user during keyboard macro execution.
With prefix argument, enters recursive edit,
 reading keyboard commands even within a keyboard macro.
 You can give different commands each time the macro executes.
Without argument, reads a character.  Your options are:
 Space -- execute the rest of the macro.
 Rubout -- skip the rest of the macro; start next repetition.
 C-D -- skip the rest of the macro and don't repeat it any more.
 C-R -- Enter a recursive edit, then on exit ask again for a character
 C-L -- redisplay screen and ask again."
  (define (loop)
    (let ((char (with-keyboard-macro-disabled
		 (lambda ()
		   (set-command-prompt!
		    "Proceed with macro? (Space, Rubout, C-D, C-R or C-L)")
		   (char-upcase (keyboard-read-char))))))
      (cond ((char=? char #\Space))
	    ((char=? char #\Rubout)
	     (*keyboard-macro-continuation* true))
	    ((char=? char #\C-D)
	     (*keyboard-macro-continuation* false))
	    ((char=? char #\C-R)
	     (with-keyboard-macro-disabled enter-recursive-edit)
	     (loop))
	    ((or (char=? char #\C-L) (char=? char #\Page))
	     (window-redraw! (current-window) false)
	     (loop))
	    (else
	     (beep)
	     (loop)))))
  (cond (argument (with-keyboard-macro-disabled enter-recursive-edit))
	(*executing-keyboard-macro?* (loop))))

;;; end USING-SYNTAX
)
;;; Edwin Variables:
;;; Scheme Environment: edwin-package
;;; Scheme Syntax Table: edwin-syntax-table
;;; Tags Table Pathname: (access edwin-tags-pathname edwin-package)
;;; End:
