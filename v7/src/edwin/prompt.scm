;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1986 Massachusetts Institute of Technology
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

;;;; User Prompting

(declare (usual-integrations))
(using-syntax edwin-syntax-table

(define-variable "Enable Recursive Minibuffers"
  "If true, allow minibuffers to invoke commands which use
recursive minibuffers."
  false)

(define within-typein-edit)
(define within-typein-edit?)
(define prompt-for-typein)
(define prompt-for-completed-string)
(define prompt-for-string)
(define prompt-for-string-table-value)
(define prompt-for-alist-value)
(define prompt-for-command)
(define prompt-for-variable)
(define prompt-for-char)
(define prompt-for-char-without-interrupts)
(define prompt-for-char-with-interrupts)
(define prompt-for-key)
(define prompt-for-confirmation?)
(define prompt-for-yes-or-no?)

(define prompt-package
  (make-environment

(define typein-edit-continuation false)
(define typein-edit-abort-flag "Abort")
(define typein-edit-depth -1)
(define typein-saved-buffers '())
(define typein-saved-window)

(set! within-typein-edit
(named-lambda (within-typein-edit thunk)
  (if (and (not (ref-variable "Enable Recursive Minibuffers"))
	   (typein-window? (current-window)))
      (editor-error "Command attempted to use minibuffer while in minibuffer"))
  (let ((value
	 (call-with-current-continuation
	  (lambda (continuation)
	    (fluid-let ((typein-edit-continuation continuation)
			(typein-edit-depth (1+ typein-edit-depth))
			(typein-saved-buffers
			 (cons (window-buffer (typein-window))
			       typein-saved-buffers))
			(typein-saved-window (current-window)))
	      (dynamic-wind
	       (lambda ()
		 (select-window (typein-window))
		 (select-buffer
		  (find-or-create-buffer
		   (string-append " *Typein-"
				  (write-to-string typein-edit-depth)
				  "*")))
		 (buffer-reset! (current-buffer))
		 (reset-command-prompt!)
		 (%clear-message!))
	       thunk
	       (lambda ()
		 (select-window (typein-window))
		 (let ((buffer (car typein-saved-buffers)))
		   (bufferset-guarantee-buffer! (current-bufferset) buffer)
		   (select-buffer buffer))
		 (reset-command-prompt!)
		 (%clear-message!)
		 (if (zero? typein-edit-depth)
		     (buffer-reset! (current-buffer)))
		 (cond ((window-visible? typein-saved-window)
			(select-window typein-saved-window))
		       ((zero? typein-edit-depth)
			(select-window (other-window)))))))))))
    (if (eq? value typein-edit-abort-flag)
	(abort-current-command)
	value))))

(set! within-typein-edit?
(named-lambda (within-typein-edit?)
  (not (false? typein-edit-continuation))))

;;; The following are used by MESSAGE and friends.

(define (set-message! message)
  ((access set-override-message! window-package)
   ((access frame-text-inferior window-package) (typein-window))
   message)
  (window-direct-update! (typein-window) true))

(define (clear-message!)
  (%clear-message!)
  (window-direct-update! (typein-window) true))

(define (%clear-message!)
  ((access clear-override-message! window-package)
   ((access frame-text-inferior window-package) (typein-window))))

(define (update-typein!)
  (window-direct-update! (typein-window) false))

(define (typein-window)
  ((access editor-frame-typein-window window-package) (current-frame)))

(set! prompt-for-typein
(named-lambda (prompt-for-typein prompt-string thunk)
  (within-typein-edit
   (lambda ()
     (insert-string prompt-string)
     (with-narrowed-region! (let ((mark (current-point)))
			      (make-region (mark-right-inserting mark)
					   (mark-left-inserting mark)))
       (lambda ()
	 (^G-interceptor ^G-wrapper thunk)))))))

(define ((^G-wrapper continuation) value)
  (cond ((not (eq? (current-window) (typein-window))) (abort-current-command))
	;; This should NEVER happen.
	((not typein-edit-continuation) (continuation value))
	(else (typein-edit-continuation typein-edit-abort-flag))))

(define ((typein-editor-thunk mode))
  (let ((buffer (current-buffer)))
    (ring-clear! (buffer-mark-ring buffer))
    (push-current-mark! (buffer-start buffer)))
  (set-current-major-mode! mode)
  (command-reader))

(define (exit-typein-edit)
  (if (not typein-edit-continuation)
      (error "Not editing typein; can't exit"))
  ;; Indicate that typein has been accepted.
  ((access home-cursor! window-package)
   ((access frame-text-inferior window-package) (current-window)))
  (typein-edit-continuation (typein-string)))

(define (typein-string)
  (region->string (buffer-region (current-buffer))))

(define (set-typein-string! string #!optional update?)
  (if (unassigned? update?) (set! update? true))
  (let ((dont-update?
	 (or (not update?)
	     (window-needs-redisplay? (typein-window)))))
    (region-delete! (buffer-region (current-buffer)))
    (insert-string string)
    (if (not dont-update?) (update-typein!))))

(define (set-typein-substring! string start end #!optional update?)
  (if (unassigned? update?) (set! update? true))
  (let ((dont-update?
	 (or (not update?)
	     (window-needs-redisplay? (typein-window)))))
    (region-delete! (buffer-region (current-buffer)))
    (insert-substring string start end)
    (if (not dont-update?) (update-typein!))))

;;;; String Prompt

(define *default-string*)
(define *default-type*)
(define *completion-string-table*)
(define *completion-type*)
(define *pop-up-window*)

(set! prompt-for-completed-string
(named-lambda (prompt-for-completed-string prompt default-string default-type
					   completion-string-table
					   completion-type #!optional mode)
  (if (unassigned? mode) (set! mode prompt-for-string-mode))
  (fluid-let ((*default-string* default-string)
	      (*default-type* default-type)
	      (*completion-string-table* completion-string-table)
	      (*completion-type* completion-type)
	      (*pop-up-window* false))
    (dynamic-wind
     (lambda () 'DONE)
     (lambda ()
       (prompt-for-typein
	(string-append
	 prompt
	 (if (or (memq default-type
		       '(NO-DEFAULT NULL-DEFAULT INVISIBLE-DEFAULT))
		 (not default-string))
	     ""
	     (string-append " (Default is: '" default-string "')"))
	 ": ")
	(typein-editor-thunk mode)))
     (lambda ()
       (if (and *pop-up-window* (window-visible? *pop-up-window*))
	   (window-delete! *pop-up-window*)
	   (let ((buffer (find-buffer " *Completions*")))
	     (if buffer
		 (let ((replacement (other-buffer buffer)))
		   (for-each (lambda (window)
			       (select-buffer-in-window replacement window))
			     (buffer-windows buffer))
		   (bury-buffer buffer))))))))))

(define-major-mode "Prompt for String" "Fundamental"
  "Major mode for editing solicited input strings.
Depending on what is being solicited, either defaulting or completion
may be available.  The following commands are special to this mode:

\\[^R Terminate Input] terminates the input.
\\[^R Yank Default String] yanks the default string, if there is one.
\\[^R Complete Input] completes as much of the input as possible.
\\[^R Complete Input Space] completes up to the next space.
\\[^R List Completions] displays possible completions of the input."
  'DONE)

(define-key "Prompt for String" #\Return "^R Terminate Input")
(define-key "Prompt for String" #\C-M-Y "^R Yank Default String")
(define-key "Prompt for String" #\Tab "^R Complete Input")
(define-key "Prompt for String" #\Space "^R Complete Input Space")
(define-key "Prompt for String" #\? "^R List Completions")

(define-command ("^R Yank Default String" argument)
  "Insert the default string at point."
  (if *default-string*
      (insert-string *default-string*)
      (editor-failure)))

(define-command ("^R Complete Input" argument)
  "Attempt to complete the current input string."
  (cond ((not *completion-string-table*)
	 ;; Effectively, this means do what would be done if this
	 ;; command was not defined by this mode.
	 (dispatch-on-command (comtab-entry (cdr (current-comtab))
					    (current-command-char))))
	((not (complete-input-string *completion-string-table* true))
	 (editor-failure))))

(define-command ("^R Complete Input Space" argument)
  "Attempt to complete the input string, up to the next space."
  (cond ((not *completion-string-table*)
	 (dispatch-on-command (comtab-entry (cdr (current-comtab))
					    (current-command-char))))
	((not (complete-input-string-to-char *completion-string-table*
					     #\Space))
	 (editor-failure))))

(define-command ("^R List Completions" argument)
  "List the possible completions for the given input."
  (if *completion-string-table*
      (list-completions
       (string-table-completions *completion-string-table*
				 (typein-string)))
      (^r-insert-self-command)))

(define (list-completions strings)
  (let ((window
	 (with-output-to-temporary-buffer " *Completions*"
	   (lambda ()
	     (if (null? strings)
		 (write-string
		  "There are no valid completions for this input.")
		 (begin (write-string "Possible completions:")
			(newline)
			(write-strings-densely strings)))))))
    (if (not *pop-up-window*)
	(set! *pop-up-window* window))))

(define-command ("^R Terminate Input" argument)
  "Terminate the input string.
If defaulting is in effect, and there is no input, use the default.
If completion is in effect, then:
  If completion is cautious, return only if the input is completed.
  If completion is strict, don't return unless the input completes."
  (cond ((string-null? (typein-string))
	 (cond ((eq? *default-type* 'NULL-DEFAULT)
		(exit-typein-edit))
	       ((or (eq? *default-type* 'NO-DEFAULT)
		    (not *default-string*))
		(if (and (eq? *completion-type* 'STRICT-COMPLETION)
			 (complete-input-string *completion-string-table*
						false))
		    (exit-typein-edit)
		    (begin (update-typein!)
			   (editor-failure))))
	       (else
		(set-typein-string! *default-string* false)
		(exit-typein-edit))))
	((eq? *completion-type* 'CAUTIOUS-COMPLETION)
	 (if (string-table-get *completion-string-table* (typein-string))
	     (exit-typein-edit)
	     (editor-failure)))
	((eq? *completion-type* 'STRICT-COMPLETION)
	 (if (complete-input-string *completion-string-table* false)
	     (exit-typein-edit)
	     (begin (update-typein!)
		    (editor-failure))))
	(else
	 (exit-typein-edit))))

(define (complete-input-string string-table update?)
  (string-table-complete string-table (typein-string)
    (lambda (string)
      (set-typein-string! string update?))
    (lambda (string limit)
      (set-typein-substring! string 0 limit update?))
    (lambda ()
      'DONE))
  (string-table-get string-table (typein-string)))

(define (complete-input-string-to-char string-table char)
  (let ((original (typein-string)))
    (string-table-complete-to-char string-table original char
      (lambda (string limit)
	(if (> limit (string-length original))
	    (set-typein-substring! string 0 limit))
	true)
      (lambda (string limit)
	(and (> limit (string-length original))
	     (begin (set-typein-substring! string 0 limit)
		    true)))
      (lambda ()
	false))))

(define (string-table-complete-to-char string-table string char if-unambiguous
				       if-ambiguous if-not-found)
  (string-table-complete string-table string
    (lambda (new-string)
      (if-unambiguous
       new-string
       (let ((end (string-length new-string)))
	 (let ((index
		(substring-find-next-char new-string (string-length string)
					  end char)))
	   (if index
	       (1+ index)
	       end)))))
    (lambda (new-string limit)
      (let ((index (substring-find-next-char new-string (string-length string)
					     limit char)))
	(if index
	    (if-unambiguous new-string (1+ index))
	    (let ((string (string-append-char string char)))
	      (string-table-complete string-table string
		(lambda (new-string)
		  (if-unambiguous new-string (string-length string)))
		(lambda (new-string limit)
		  (if-ambiguous new-string (string-length string)))
		(lambda ()
		  (if-ambiguous new-string limit)))))))
    if-not-found))

(set! prompt-for-string
(named-lambda (prompt-for-string prompt default-string #!optional default-type)
  (if (unassigned? default-type) (set! default-type 'VISIBLE-DEFAULT))
  (prompt-for-completed-string prompt
			       default-string default-type
			       false 'NO-COMPLETION)))

(set! prompt-for-string-table-value
(named-lambda (prompt-for-string-table-value prompt string-table)
  (string-table-get string-table
		    (prompt-for-completed-string prompt
						 false 'NO-DEFAULT
						 string-table
						 'STRICT-COMPLETION))))

(set! prompt-for-alist-value
(named-lambda (prompt-for-alist-value prompt alist)
  (prompt-for-string-table-value prompt (alist->string-table alist))))

(define ((string-table-prompter string-table) prompt)
  (prompt-for-string-table-value prompt string-table))

(set! prompt-for-command
  (string-table-prompter editor-commands))

(set! prompt-for-variable
  (string-table-prompter editor-variables))

;;;; Character Prompts

(define ((character-prompter read-char) prompt)
  (set-command-prompt! (string-append prompt ": "))
  (let ((char (read-char)))
    (set-command-prompt! (string-append (command-prompt) (char->name char)))
    char))

(set! prompt-for-char-without-interrupts
      (character-prompter
       (lambda ()
	 (with-editor-interrupts-disabled keyboard-read-char))))

(set! prompt-for-char-with-interrupts
      (character-prompter
       (lambda ()
	 (keyboard-read-char))))

(set! prompt-for-char
      prompt-for-char-with-interrupts)

(set! prompt-for-key
(named-lambda (prompt-for-key prompt #!optional comtab)
  (if (unassigned? comtab) (set! comtab (current-comtab)))
  (let ((string (string-append prompt ": ")))
    (define (outer-loop prefix)
      (define (inner-loop char)
	(let ((chars (append! prefix (list char))))
	  (set-command-prompt! (string-append string (xchar->name chars)))
	  (if (prefix-char-list? comtab chars)
	      (outer-loop chars)
	      (let ((command (comtab-entry comtab chars)))
		(if (memq command extension-commands)
		    (inner-loop (fluid-let ((execute-extended-chars? false))
				  (dispatch-on-command command)))
		    chars)))))
      (inner-loop (keyboard-read-char)))
    (set-command-prompt! string)
    (outer-loop '()))))

;;;; Confirmation Prompts

(set! prompt-for-confirmation?
(named-lambda (prompt-for-confirmation? prompt)
  (define (loop)
    (let ((char (char-upcase (keyboard-read-char))))
      (cond ((or (char=? char #\Y)
		 (char=? char #\Space))
	     (set-command-prompt! (string-append (command-prompt) "Yes"))
	     true)
	    ((or (char=? char #\N)
		 (char=? char #\Rubout))
	     (set-command-prompt! (string-append (command-prompt) "No"))
	     false)
	    (else
	     (editor-failure)
	     (loop)))))
  (set-command-prompt! (string-append prompt " (Y or N)? "))
  (loop)))

(set! prompt-for-yes-or-no?
(named-lambda (prompt-for-yes-or-no? prompt)
  (string-ci=?
   "Yes"
   (prompt-for-typein (string-append prompt " (Yes or No)? ")
		      (typein-editor-thunk prompt-for-yes-or-no-mode)))))

(define-command ("^R Terminate Yes or No" argument)
  "Like ^R Terminate Input, but insists on ``Yes'' or ``No'' as an answer."
  (let ((string (typein-string)))
    (if (or (string-ci=? "Yes" string)
	    (string-ci=? "No" string))
	(exit-typein-edit)
	(editor-error "Please enter ``Yes'' or ``No''"))))

(define-major-mode "Prompt for Yes or No" "Fundamental"
  "Enter either ``Yes'' or ``No''."
  'DONE)

(define-key "Prompt for Yes or No" #\Return "^R Terminate Yes or No")

;;; end PROMPT-PACKAGE
)))

;;; Edwin Variables:
;;; Scheme Environment: (access prompt-package edwin-package)
;;; Scheme Syntax Table: edwin-syntax-table
;;; Tags Table Pathname: (access edwin-tags-pathname edwin-package)
;;; End:
