;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/prompt.scm,v 1.150 1991/10/21 12:06:00 cph Exp $
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

;;;; User Prompting
;;; Package: (edwin prompt)

(declare (usual-integrations))

(define typein-edit-abort-flag "Abort")
(define typein-edit-continuation)
(define typein-edit-depth)
(define typein-saved-buffers)
(define typein-saved-windows)
(define map-name/internal->external)
(define map-name/external->internal)

(define (initialize-typein!)
  (set! typein-edit-continuation false)
  (set! typein-edit-depth -1)
  (set! typein-saved-buffers '())
  (set! typein-saved-windows '())
  (set! map-name/internal->external identity-procedure)
  (set! map-name/external->internal identity-procedure)
  unspecific)

(define (make-typein-buffer-name depth)
  (string-append " *Typein-" (number->string depth) "*"))

(define (within-typein-edit thunk)
  (let ((value
	 (call-with-current-continuation
	  (lambda (continuation)
	    (fluid-let ((typein-edit-continuation continuation)
			(typein-edit-depth (1+ typein-edit-depth))
			(typein-saved-buffers
			 (cons (window-buffer (typein-window))
			       typein-saved-buffers))
			(typein-saved-windows
			 (cons (current-window)
			       typein-saved-windows)))
	      (dynamic-wind
	       (lambda ()
		 (let ((window (typein-window)))
		   (select-window window)
		   (select-buffer
		    (find-or-create-buffer
		     (make-typein-buffer-name typein-edit-depth)))
		   (buffer-reset! (current-buffer))
		   (reset-command-prompt!)
		   (window-clear-override-message! window)))
	       thunk
	       (lambda ()
		 (let ((window (typein-window)))
		   (select-window window)
		   (let ((buffer (car typein-saved-buffers)))
		     (bufferset-guarantee-buffer! (current-bufferset) buffer)
		     (select-buffer buffer))
		   (reset-command-prompt!)
		   (window-clear-override-message! window))
		 (if (zero? typein-edit-depth)
		     (buffer-reset! (current-buffer)))
		 (cond ((window-visible? (car typein-saved-windows))
			(select-window (car typein-saved-windows)))
		       ((zero? typein-edit-depth)
			(select-window (other-window)))))))))))
    (if (eq? value typein-edit-abort-flag)
	(abort-current-command)
	value)))

(define-integrable (within-typein-edit?)
  (not (null? typein-saved-windows)))

(define (typein-edit-other-window)
  (let loop ((windows typein-saved-windows))
    (and (not (null? windows))
	 (if (typein-window? (car windows))
	     (loop (cdr windows))
	     (car windows)))))

(define-variable enable-recursive-minibuffers
  "True means allow minibuffers to invoke commands that use recursive minibuffers."
  false
  boolean?)

(define-variable completion-auto-help
  "True means automatically provide help for invalid completion input."
  true
  boolean?)

(define (prompt-for-typein prompt-string check-recursion? thunk)
  (if (and check-recursion?
	   (not (ref-variable enable-recursive-minibuffers))
	   (typein-window? (current-window)))
      (editor-error "Command attempted to use minibuffer while in minibuffer"))
  (cleanup-pop-up-buffers
   (lambda ()
     (within-typein-edit
      (lambda ()
	(insert-string prompt-string)
	(let ((mark (current-point)))
	  (with-text-clipped (mark-right-inserting mark)
			     (mark-left-inserting mark)
	    (lambda ()
	      (intercept-^G-interrupts
	       (lambda ()
		 (cond ((not (eq? (current-window) (typein-window)))
			(abort-current-command))
		       (typein-edit-continuation
			(typein-edit-continuation typein-edit-abort-flag))
		       (else
			(error "illegal ^G signaled in typein window"))))
	       thunk)))))))))

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
  (let ((window (current-window)))
    (window-home-cursor! window)
    (typein-edit-continuation (buffer-string (window-buffer window)))))

(define-integrable (typein-string)
  (map-name/external->internal (buffer-string (current-buffer))))

(define (set-typein-string! string #!optional update?)
  (let ((dont-update?
	 (or (not (or (default-object? update?) update?))
	     (window-needs-redisplay? (typein-window)))))
    (region-delete! (buffer-region (current-buffer)))
    (insert-string (map-name/internal->external string))
    (if (not dont-update?) (update-typein!))))

(define (update-typein!)
    (if (not *executing-keyboard-macro?*)
	(window-direct-update! (typein-window) false)))

;;;; String Prompt

(define *default-string*)
(define *default-type*)
(define completion-procedure/complete-string)
(define completion-procedure/list-completions)
(define completion-procedure/verify-final-value?)
(define *completion-confirm?*)

(define (prompt-for-string prompt default-string #!optional default-type mode)
  (fluid-let ((*default-string* default-string)
	      (*default-type*
	       (if (default-object? default-type)
		   'VISIBLE-DEFAULT
		   default-type)))
    (%prompt-for-string prompt
			(if (default-object? mode)
			    (ref-mode-object minibuffer-local)
			    mode))))

(define (prompt-for-completed-string prompt
				     default-string
				     default-type
				     complete-string
				     list-completions
				     verify-final-value?
				     require-match?)
  (fluid-let ((*default-string* default-string)
	      (*default-type* default-type)
	      (completion-procedure/complete-string complete-string)
	      (completion-procedure/list-completions list-completions)
	      (completion-procedure/verify-final-value? verify-final-value?)
	      (*completion-confirm?* (not (eq? require-match? true))))
    (%prompt-for-string
     prompt
     (if require-match?
	 (ref-mode-object minibuffer-local-must-match)
	 (ref-mode-object minibuffer-local-completion)))))

(define (%prompt-for-string prompt mode)
  (prompt-for-typein
   (prompt-for-string/prompt prompt
			     (and (eq? *default-type* 'VISIBLE-DEFAULT)
				  *default-string*
				  (write-to-string *default-string*)))
   true
   (let ((thunk (typein-editor-thunk mode)))
     (if (and (eq? *default-type* 'INSERTED-DEFAULT) *default-string*)
	 (let ((string *default-string*))
	   (set! *default-string* false)
	   (lambda ()
	     (insert-string string)
	     ((thunk))))
	 thunk))))

(define (prompt-for-string/prompt prompt default-string)
  (cond ((string? prompt)
	 (string-append
	  prompt
	  (if default-string
	      (string-append " (default is: " default-string ")")
	      "")
	  (if (string-suffix? " " prompt)
	      ""
	      ": ")))
	((and (pair? prompt)
	      (string? (car prompt))
	      (null? (cdr prompt)))
	 (car prompt))
	(else
	 (error:wrong-type-argument prompt
				    "prompt string"
				    'PROMPT-FOR-STRING/PROMPT))))

(define (prompt-for-number prompt default)
  (let ((string
	 (let ((default (and default (number->string default))))
	   (prompt-for-string
	    (prompt-for-string/prompt prompt default)
	    default
	    'INVISIBLE-DEFAULT))))
    (or (string->number string)
	(editor-error "Input not a number: " string))))

(define (prompt-for-string-table-name prompt
				      default-string
				      default-type
				      string-table
				      require-match?)
  (prompt-for-completed-string
   prompt
   default-string
   default-type
   (lambda (string if-unique if-not-unique if-not-found)
     (string-table-complete string-table
			    string
			    if-unique
			    if-not-unique
			    if-not-found))
   (lambda (string)
     (string-table-completions string-table string))
   (lambda (string)
     (string-table-get string-table string))
   require-match?))

(define (prompt-for-string-table-value prompt
				       default-string
				       default-type
				       string-table
				       require-match?)
  (string-table-get string-table
		    (prompt-for-string-table-name prompt
						  default-string
						  default-type
						  string-table
						  require-match?)))

(define (prompt-for-alist-value prompt alist #!optional default)
  (fluid-let ((map-name/external->internal identity-procedure)
	      (map-name/internal->external identity-procedure))
    (prompt-for-string-table-value prompt
				   (and (not (default-object? default))
					default)
				   'VISIBLE-DEFAULT
				   (alist->string-table alist)
				   true)))

(define (prompt-for-command prompt)
  (fluid-let ((map-name/external->internal editor-name/external->internal)
	      (map-name/internal->external editor-name/internal->external))
    (prompt-for-string-table-value prompt
				   false
				   'NO-DEFAULT
				   editor-commands
				   true)))

(define (prompt-for-variable prompt)
  (fluid-let ((map-name/external->internal editor-name/external->internal)
	      (map-name/internal->external editor-name/internal->external))
    (prompt-for-string-table-value prompt
				   false
				   'NO-DEFAULT
				   editor-variables
				   true)))

;;;; String Prompt Modes

(define-major-mode minibuffer-local fundamental #f
  "Major mode for editing solicited input strings.
The following commands are special to this mode:

\\[exit-minibuffer] terminates the input.
\\[minibuffer-yank-default] yanks the default string, if there is one.")

(define-key 'minibuffer-local #\return 'exit-minibuffer)
(define-key 'minibuffer-local #\linefeed 'exit-minibuffer)
(define-key 'minibuffer-local #\c-m-y 'minibuffer-yank-default)

(define-major-mode minibuffer-local-completion fundamental #f
  "Major mode for editing solicited input strings.
The following commands are special to this mode:

\\[exit-minibuffer] terminates the input.
\\[minibuffer-yank-default] yanks the default string, if there is one.
\\[minibuffer-complete] completes as much of the input as possible.
\\[minibuffer-complete-word] completes up to the next space.
\\[minibuffer-completion-help] displays possible completions of the input.")

(define-key 'minibuffer-local-completion #\return 'exit-minibuffer)
(define-key 'minibuffer-local-completion #\linefeed 'exit-minibuffer)
(define-key 'minibuffer-local-completion #\c-m-y 'minibuffer-yank-default)
(define-key 'minibuffer-local-completion #\tab 'minibuffer-complete)
(define-key 'minibuffer-local-completion #\space 'minibuffer-complete-word)
(define-key 'minibuffer-local-completion #\? 'minibuffer-completion-help)

(define-major-mode minibuffer-local-must-match fundamental #f
  "Major mode for editing solicited input strings.
The following commands are special to this mode:

\\[minibuffer-complete-and-exit] terminates the input.
\\[minibuffer-yank-default] yanks the default string, if there is one.
\\[minibuffer-complete] completes as much of the input as possible.
\\[minibuffer-complete-word] completes up to the next space.
\\[minibuffer-completion-help] displays possible completions of the input.")

(define-key 'minibuffer-local-must-match #\return
  'minibuffer-complete-and-exit)
(define-key 'minibuffer-local-must-match #\linefeed
  'minibuffer-complete-and-exit)
(define-key 'minibuffer-local-must-match #\c-m-y 'minibuffer-yank-default)
(define-key 'minibuffer-local-must-match #\tab 'minibuffer-complete)
(define-key 'minibuffer-local-must-match #\space 'minibuffer-complete-word)
(define-key 'minibuffer-local-must-match #\? 'minibuffer-completion-help)

(define-command exit-minibuffer
  "Terminate this minibuffer argument."
  ()
  (lambda ()
    (cond ((or (not (string-null? (typein-string)))
	       (memq *default-type* '(NULL-DEFAULT INSERTED-DEFAULT)))
	   (exit-typein-edit))
	  ((or (not *default-string*)
	       (eq? *default-type* 'NO-DEFAULT))
	   (editor-failure))
	  (else
	   (if (and (memq *default-type* '(INVISIBLE-DEFAULT VISIBLE-DEFAULT))
		    *default-string*)
	       (set-typein-string! *default-string* false))
	   (exit-typein-edit)))))

(define-command minibuffer-yank-default
  "Insert the default string at point."
  ()
  (lambda ()
    (if *default-string*
	(insert-string *default-string*)
	(editor-failure))))

(define-command minibuffer-complete
  "Complete the minibuffer contents as far as possible."
  ()
  (lambda ()
    (case (complete-input-string completion-procedure/complete-string true)
      ((WAS-ALREADY-EXACT-AND-UNIQUE-COMPLETION)
       (temporary-typein-message " [Sole completion]"))
      ((WAS-ALREADY-EXACT-COMPLETION)
       (temporary-typein-message " [Complete, but not unique]")))))

(define-command minibuffer-complete-word
  "Complete the minibuffer contents at most a single word."
  ()
  (lambda ()
    (case (complete-input-string completion-procedure/complete-word true)
      ((WAS-ALREADY-EXACT-AND-UNIQUE-COMPLETION)
       (temporary-typein-message " [Sole completion]"))
      ((WAS-ALREADY-EXACT-COMPLETION)
       (temporary-typein-message " [Complete, but not unique]")))))

(define-command minibuffer-completion-help
  "Display a list of possible completions of the current minibuffer contents."
  ()
  (lambda ()
    (minibuffer-completion-help
     (lambda ()
       (completion-procedure/list-completions (typein-string))))))

(define (minibuffer-completion-help list-completions)
  (pop-up-generated-completions
   (lambda ()
     (map map-name/internal->external (list-completions)))))

(define-command minibuffer-complete-and-exit
  "Complete the minibuffer contents, and maybe exit.
Exit if the name is valid with no completion needed.
If name was completed to a valid match,
a repetition of this command will exit."
  ()
  (lambda ()
    (let ((string (typein-string)))
      (if (and (string-null? string)
	       (memq *default-type* '(INVISIBLE-DEFAULT VISIBLE-DEFAULT))
	       *default-string*)
	  (set-typein-string! *default-string* false))
      (case (complete-input-string completion-procedure/complete-string false)
	((WAS-ALREADY-EXACT-AND-UNIQUE-COMPLETION
	  WAS-ALREADY-EXACT-COMPLETION)
	 (exit-typein-edit))
	((COMPLETED-TO-EXACT-AND-UNIQUE-COMPLETION
	  COMPLETED-TO-EXACT-COMPLETION)
	 (if *completion-confirm?*
	     (temporary-typein-message " [Confirm]")
	     (exit-typein-edit)))
	(else
	 (update-typein!)
	 (editor-failure))))))

;;;; Completion Primitives

(define (complete-input-string complete-string update?)
  (let ((original (typein-string)))
    (complete-string original
      (lambda (string)
	(if (not (string=? string original))
	    (set-typein-string! string update?))
	(if (string-ci=? string original)
	    'WAS-ALREADY-EXACT-AND-UNIQUE-COMPLETION
	    'COMPLETED-TO-EXACT-AND-UNIQUE-COMPLETION))
      (lambda (string list-completions)
	(if (not (string=? string original))
	    (set-typein-string! string update?))
	(if (completion-procedure/verify-final-value? string)
	    (if (string-ci=? string original)
		'WAS-ALREADY-EXACT-COMPLETION
		'COMPLETED-TO-EXACT-COMPLETION)
	    (if (string-ci=? string original)
		(begin
		  (if (ref-variable completion-auto-help)
		      (minibuffer-completion-help list-completions)
		      (temporary-typein-message " [Next char not unique]"))
		  'NO-COMPLETION-HAPPENED)
		'SOME-COMPLETION-HAPPENED)))
      (lambda ()
	(editor-beep)
	(temporary-typein-message " [No match]")
	'NO-MATCH))))

(define (completion-procedure/complete-word string
					    if-unique
					    if-not-unique
					    if-not-found)
  (let ((truncate-string
	 (lambda (new-string)
	   (let ((end (string-length new-string)))
	     (let ((index
		    (and (string-prefix-ci? string new-string)
			 (substring-find-next-char-not-of-syntax
			  new-string (string-length string) end
			  (ref-variable syntax-table) #\w))))
	       (if index
		   (substring new-string 0 (1+ index))
		   new-string))))))
    (let ((if-unique
	   (lambda (new-string)
	     (if-unique (truncate-string new-string))))
	  (if-not-unique
	   (lambda (new-string list-completions)
	     (if-not-unique (truncate-string new-string) list-completions))))
      (completion-procedure/complete-string string
	if-unique
	(lambda (new-string list-completions)
	  (if (= (string-length new-string) (string-length string))
	      (let ((completions (list-completions)))
		(let ((try-suffix
		       (lambda (suffix if-not-found)
			 (let ((completions
				(list-transform-positive completions
				  (let ((prefix (string-append string suffix)))
				    (lambda (completion)
				      (string-prefix-ci? prefix
							 completion))))))
			   (cond ((null? completions)
				  (if-not-found))
				 ((null? (cdr completions))
				  (if-unique (car completions)))
				 (else
				  (if-not-unique
				   (string-greatest-common-prefix completions)
				   (lambda () completions))))))))
		  (try-suffix "-"
		    (lambda ()
		      (try-suffix " "
			(lambda ()
			  (if-not-unique string (lambda () completions))))))))
	      (if-not-unique new-string list-completions)))
	if-not-found))))

;;;; Support for Completion

(define (standard-completion prefix complete-string insert-completed-string)
  (complete-string prefix
    (lambda (completion)
      (if (not (string=? prefix completion))
	  (insert-completed-string completion)
	  (completion-message "Sole completion")))
    (lambda (completion generate-completions)
      (cond ((not (string=? prefix completion))
	     (insert-completed-string completion))
	    ((ref-variable completion-auto-help)
	     (pop-up-generated-completions generate-completions))
	    (else
	     (completion-message "Next char not unique"))))
    (lambda ()
      (editor-beep)
      (completion-message "No completions"))))

(define (pop-up-generated-completions generate-completions)
  (message "Making completion list...")
  (let ((completions (generate-completions)))
    (clear-message)
    (if (null? completions)
	(begin
	  (editor-beep)
	  (completion-message "No completions"))
	(begin
	  (pop-up-completions-list completions)
	  (if (not (typein-window? (current-window)))
	      (begin
		(message "Hit space to flush.")
		(reset-command-prompt!)
		(let ((char (keyboard-peek)))
		  (if (and (char? char)
			   (char=? #\space char))
		      (begin
			(keyboard-read)
			(kill-pop-up-buffer false))))
		(clear-message)))))))

(define (pop-up-completions-list strings)
  (with-output-to-temporary-buffer " *Completions*"
    (lambda ()
      (write-completions-list strings))))

(define (write-completions-list strings)
  (if (null? strings)
      (write-string
       "There are no possible completions of what you have typed.")
      (begin
	(write-string "Possible completions are:\n")
	(write-strings-densely strings))))

(define (completion-message string)
  (if (typein-window? (current-window))
      (temporary-typein-message (string-append " [" string "]"))
      (message string)))

(define (temporary-typein-message string)
  (let ((point) (start) (end))
    (dynamic-wind (lambda ()
		    (set! point (current-point))
		    (set! end (buffer-end (current-buffer)))
		    (set! start (mark-right-inserting end))
		    (insert-string string start)
		    (set-current-point! start))
		  (lambda ()
		    (sit-for 2000))
		  (lambda ()
		    (delete-string start end)
		    (set-current-point! point)
		    (set! point)
		    (set! start)
		    (set! end)
		    unspecific))))

;;;; Character Prompts

(define (prompt-for-char prompt)
  (with-editor-interrupts-disabled
   (lambda ()
     (prompt-for-typein (string-append prompt ": ") false
       (lambda ()
	 (let ((key (keyboard-read)))
	   (if (not (and (char? key)
			 (char-ascii? key)))
	       (editor-error "Not an ASCII character" key))
	   (set-typein-string! (key-name key) true)
	   key))))))

(define (prompt-for-key prompt #!optional comtab)
  (let ((comtab (if (default-object? comtab) (current-comtabs) comtab)))
    (prompt-for-typein (string-append prompt ": ") false
      (lambda ()
	(with-editor-interrupts-disabled
	 (lambda ()
	   (let outer-loop ((prefix '()))
	     (let inner-loop ((char (keyboard-read)))
	       (let ((chars (append! prefix (list char))))
		 (set-typein-string! (xkey->name chars) true)
		 (if (prefix-key-list? comtab chars)
		     (outer-loop chars)
		     (let ((command (comtab-entry comtab chars)))
		       (if (memq command extension-commands)
			   (inner-loop
			    (fluid-let ((execute-extended-keys? false))
			      (dispatch-on-command command)))
			   chars))))))))))))

;;;; Confirmation Prompts

(define (prompt-for-confirmation? prompt)
  (prompt-for-typein (string-append prompt " (y or n)? ") false
    (lambda ()
      (let loop ((lost? false))
	(let ((char (keyboard-read)))
	  (cond ((and (char? char)
		      (or (char-ci=? char #\y)
			  (char-ci=? char #\space)))
		 (set-typein-string! "y" true)
		 true)
		((and (char? char)
		      (or (char-ci=? char #\n)
			  (char-ci=? char #\rubout)))
		 (set-typein-string! "n" true)
		 false)
		(else
		 (editor-beep)
		 (if (not lost?)
		     (insert-string "Please answer y or n.  "
				    (buffer-absolute-start (current-buffer))))
		 (loop true))))))))

(define (prompt-for-yes-or-no? prompt)
  (string-ci=?
   "Yes"
   (prompt-for-typein (string-append prompt " (yes or no)? ") true
     (typein-editor-thunk (ref-mode-object minibuffer-local-yes-or-no)))))

(define-major-mode minibuffer-local-yes-or-no fundamental #f
  "Enter either \"yes\" or \"no\".")

(define-key 'minibuffer-local-yes-or-no #\return 'exit-minibuffer-yes-or-no)

(define-command exit-minibuffer-yes-or-no
  "Like \\[exit-minibuffer], but insists on \"yes\" or \"no\" as an answer."
  ()
  (lambda ()
    (let ((string (typein-string)))
      (if (or (string-ci=? "yes" string)
	      (string-ci=? "no" string))
	  (exit-typein-edit)
	  (begin
	    (editor-beep)
	    (message "Please answer yes or no.")
	    (sit-for 2000)
	    (clear-message)
	    (set-typein-string! "" false))))))

;;;; Command History Prompt

(define-command repeat-complex-command
  "Edit and re-evaluate last complex command, or ARGth from last.
A complex command is one which used the minibuffer.
The command is placed in the minibuffer as a Scheme form for editing.
The result is executed, repeating the command as changed.
If the command has been changed or is not the most recent previous command
it is added to the front of the command history.
Whilst editing the command, the following commands are available:
\\{repeat-complex-command}"
  "p"
  (lambda (argument)
    (fluid-let ((*command-history* (command-history-list))
		(*command-history-index* argument))
      (if (or (<= argument 0)
	      (> argument (length *command-history*)))
	  (editor-error "argument out of range: " argument))
      (execute-command-history-entry
       (read-from-string
	(prompt-for-string "Redo"
			   (command-history-entry->string
			    (list-ref *command-history* (-1+ argument)))
			   'INSERTED-DEFAULT
			   (ref-mode-object repeat-complex-command)))))))

(define *command-history*)
(define *command-history-index*)

(define (command-history-entry->string command)
  (fluid-let ((*unparse-with-maximum-readability?* true))
    (write-to-string command)))

(define-major-mode repeat-complex-command minibuffer-local #f
  "Major mode for editing command history.")

(define-key 'repeat-complex-command #\M-n 'next-complex-command)
(define-key 'repeat-complex-command #\M-p 'previous-complex-command)

(define-command next-complex-command
  "Inserts the next element of `command-history' into the minibuffer."
  "p"
  (lambda (argument)
    (let ((index
	   (min (max 1 (- *command-history-index* argument))
		(length *command-history*))))
      (if (and (not (zero? argument))
	       (= index *command-history-index*))
	  (editor-error (if (= index 1)
			    "No following item in command history"
			    "No preceeding item in command history")))
      (set! *command-history-index* index)
      (set-typein-string!
       (command-history-entry->string (list-ref *command-history* (-1+ index)))
       true)
      (set-current-point! (buffer-start (current-buffer))))))

(define-command previous-complex-command
  "Inserts the next element of `command-history' into the minibuffer."
  "p"
  (lambda (argument)
    ((ref-command next-complex-command) (- argument))))