;;; -*-Scheme-*-
;;;
;;;	$Id: hlpcom.scm,v 1.104 1992/09/25 01:01:02 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989-92 Massachusetts Institute of Technology
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

;;;; Help Commands

(declare (usual-integrations))

(define-command help-prefix
  "This is a prefix for more commands.
It reads another character (a subcommand) and dispatches on it."
  "cA B C F I K L M T V W or C-h for more help"
  (lambda (char)
    (dispatch-on-key
     (current-comtabs)
     (list #\Backspace
	   (if (or (char=? char #\Backspace)
		   (char=? char #\?))
	       (cleanup-pop-up-buffers
		(lambda ()
		  (let ((buffer (temporary-buffer "*Help*")))
		    (insert-string 
		     "You have typed C-h, the help character.  Type a Help option:

A  command-apropos.  Type a substring, and see a list of commands
              that contain that substring.
B  describe-bindings.  Display table of all key bindings.
C  describe-key-briefly.  Type a key sequence;
              it prints the name of the command that sequence runs.
F  describe-function.  Type a command name and get its documentation.
I  info.  The Info documentation reader.
K  describe-key.  Type a key sequence;
              it prints the full documentation.
L  view-lossage.  Prints the last 100 characters you typed.
M  describe-mode.  Print documentation of current major mode,
              which describes the commands peculiar to it.
T  help-with-tutorial.  Select the Emacs learn-by-doing tutorial.
V  describe-variable.  Type a variable name and get its documentation.
W  where-is.  Type a command name and get its key binding."
		     (buffer-point buffer))
		    (set-buffer-point! buffer (buffer-start buffer))
		    (buffer-not-modified! buffer)
		    (pop-up-buffer buffer false)
		    (let ((window (get-buffer-window buffer)))
		      (let loop ()
			(let ((char
			       (prompt-for-char
				"A B C F I K L M T V W or space to scroll")))
			  (let ((test-for
				 (lambda (char*)
				   (char=? char (remap-alias-key char*)))))
			    (cond ((or (test-for #\C-h)
				       (test-for #\?))
				   (loop))
				  ((or (test-for #\space)
				       (test-for #\C-v))
				   (scroll-window
				    window
				    (standard-scroll-window-argument
				     window false 1)
				    editor-beep)
				   (loop))
				  ((or (test-for #\rubout)
				       (test-for #\M-v))
				   (scroll-window
				    window
				    (standard-scroll-window-argument
				     window false -1)
				    editor-beep)
				   (loop))
				  (else char)))))))))
	       char)))))

;;;; Commands and Keys

(define-command command-apropos
  "Show all commands whose names contain a match for REGEXP."
  "sCommand apropos (regexp)"
  (lambda (regexp)
    (with-output-to-help-display
     (lambda ()
       (command-apropos regexp)))))

(define-command apropos-command
  (command-description (ref-command-object command-apropos))
  (command-interactive-specification (ref-command-object command-apropos))
  (command-procedure (ref-command-object command-apropos)))

(define (command-apropos regexp)
  (for-each (lambda (command)
	      (let ((name (command-name-string command)))
		(write-string name)
		(print-key-bindings command (string-length name)))
	      (newline)
	      (print-short-description "Command"
				       (command-description command)))
	    (string-table-apropos editor-commands regexp)))

(define-command describe-function
  "Prompts for a command, and describes it.
Prints the full documentation for the given command."
  "CDescribe command"
  (lambda (name)
    (help-describe-command (name->command name))))

(define (help-describe-command command)
  (with-output-to-help-display
   (lambda ()
     (write-string (command-name-string command))
     (write-string ":\n")
     (write-description (command-description command)))))

(define-command where-is
  "Prompts for a command, and shows what key it is bound to."
  "CWhere is command"
  (lambda (name)
    (let ((command (name->command name)))
      (let ((bindings (comtab-key-bindings (current-comtabs) command)))
	(if (null? bindings)
	    (message (command-name-string command) " is not on any keys")
	    (message (command-name-string command) " is on "
		     (xkey->name (car bindings))))))))

(define-command describe-key-briefly
  "Prompts for a key, and describes the command it is bound to.
Prints the brief documentation for that command."
  "kDescribe key briefly"
  (lambda (key)
    (let ((command (comtab-entry (current-comtabs) key)))
      (if (eq? command (ref-command-object undefined))
	  (help-describe-unbound-key key)
	  (message (xkey->name key)
		   " runs the command "
		   (command-name-string command))))))

(define-command describe-key
  "Prompts for a key, and describes the command it is bound to.
Prints the full documentation for that command."
  "kDescribe key"
  (lambda (key)
    (let ((command (comtab-entry (current-comtabs) key)))
      (if (eq? command (ref-command-object undefined))
	  (help-describe-unbound-key key)
	  (help-describe-command command)))))

(define (help-describe-unbound-key key)
  (message (xkey->name key) " is undefined"))

;;;; Variables

(define-command variable-apropos
  "Show all variables whose names contain a match for REGEXP."
  "sVariable apropos (regexp)"
  (lambda (regexp)
    (with-output-to-help-display
     (lambda ()
       (variable-apropos regexp)))))

(define-command apropos-variable
  (command-description (ref-command-object variable-apropos))
  (command-interactive-specification (ref-command-object variable-apropos))
  (command-procedure (ref-command-object variable-apropos)))

(define (variable-apropos regexp)
  (for-each (lambda (variable)
	      (write-string (variable-name-string variable))
	      (newline)
	      (print-short-description "Variable"
				       (variable-description variable)))
	    (string-table-apropos editor-variables regexp)))

(define-command describe-variable
  "Prompts for a variable, and describes it.
Prints the full documentation for the given variable."
  "vDescribe variable"
  (lambda (name)
    (let ((variable (name->variable name)))
      (with-output-to-help-display
       (lambda ()
	 (write-string (variable-name-string variable))
	 (newline)
	 (print-variable-binding variable)
	 (write-string "\nDocumentation:\n")
	 (write-description (variable-description variable)))))))

(define-command set-variable
  "Set VARIABLE to VALUE.  VALUE is a Scheme object.
When using this interactively, supply a Scheme expression for VALUE.
If you want VALUE to be a string, you must surround it with doublequotes."
  (lambda ()
    (let ((variable (prompt-for-variable "Set variable")))
      (list (variable-name variable)
	    (prompt-for-expression-value
	     (string-append "Set " (variable-name-string variable) " to value")
	     (variable-value variable)))))
  (lambda (variable value)
    (let ((variable (name->variable variable)))
      (if (not (variable-value-valid? variable value))
	  (editor-error "illegal value for variable:" value))
      (set-variable-value! variable value))))

(define-command make-local-variable
  "Make a variable have a local value in the current buffer."
  (lambda ()
    (let ((variable (prompt-for-variable "Make local variable")))
      (list (variable-name variable)
	    (prompt-for-expression-value
	     (string-append "Set " (variable-name-string variable) " to value")
	     (variable-value variable)))))
  (lambda (variable value)
    (let ((variable (name->variable variable)))
      (if (not (variable-value-valid? variable value))
	  (editor-error "illegal value for variable:" value))
      (define-variable-local-value! (current-buffer) variable value))))

(define-command kill-local-variable
  "Make a variable use its global value in the current buffer."
  "vKill local variable"
  (lambda (name)
    (undefine-variable-local-value! (current-buffer) (name->variable name))))

;;;; Other Stuff

(define-command apropos
  "Show all commands, variables, and modes whose names contain a match for REGEXP."
  "sApropos (regexp)"
  (lambda (regexp)
    (with-output-to-help-display
     (lambda ()
       (command-apropos regexp)
       (variable-apropos regexp)
       (mode-apropos regexp)))))

(define (mode-apropos regexp)
  (for-each (lambda (mode)
	      (write (mode-name mode))
	      (newline)
	      (print-short-description "Mode" (mode-description mode)))
	    (string-table-apropos editor-modes regexp)))

(define-command view-lossage
  "Print the keyboard history."
  ()
  (lambda ()
    (with-output-to-help-display
     (lambda ()
       (for-each (lambda (key)
		   (write-string (string-append (key-name key) " ")))
		 (reverse (ring-list (current-char-history))))))))

(define-command describe-mode
  "Print the documentation for the current mode."
  ()
  (lambda ()
    (with-output-to-help-display
     (lambda ()
       (write-description (mode-description (current-major-mode)))))))

(define-command help-with-tutorial
  "Visit the Emacs learn-by-doing tutorial."
  ()
  (lambda ()
    (delete-other-windows (current-window))
    (let ((pathname (merge-pathnames "TUTORIAL" (default-homedir-pathname))))
      (let ((buffer (pathname->buffer pathname)))
	(if buffer
	    (select-buffer buffer)
	    (let ((buffer (new-buffer (pathname->buffer-name pathname))))
	      (read-buffer buffer (edwin-tutorial-pathname) true)
	      (set-buffer-pathname! buffer pathname)
	      (set-buffer-truename! buffer false)
	      (select-buffer buffer)
	      (set-current-major-mode! (ref-mode-object fundamental))
	      (disable-buffer-auto-save! buffer)
	      (let ((mark
		     (line-start (search-forward "\n<<"
						 (buffer-start buffer)
						 (buffer-end buffer))
				 0)))
		(delete-string (line-end mark -1) (line-end mark 0))
		(insert-newlines (- (window-y-size (current-window))
				    (+ 4 (region-count-lines
					  (make-region (buffer-start buffer)
						       mark))))
				 mark))
	      (set-buffer-point! buffer (buffer-start buffer))
	      (buffer-not-modified! buffer)))))))

(define (with-output-to-help-display thunk)
  (with-output-to-temporary-buffer "*Help*" thunk))

(define (write-description description)
  (write-string (substitute-command-keys description)))

(define (print-key-bindings command column)
  (let ((bindings (comtab-key-bindings (current-comtabs) command)))
    (if (not (null? bindings))
	(begin
	  (write-string
	   (if (< column 30)
	       (make-string (- 30 column) #\space)
	       " "))
	  (write-string (key-list-string bindings))))))

(define (key-list-string xkeys)
  (let loop ((xkeys (sort xkeys xkey<?)))
    (if (null? (cdr xkeys))
	(xkey->name (car xkeys))
	(string-append (xkey->name (car xkeys))
		       ", "
		       (loop (cdr xkeys))))))

(define (print-variable-binding variable)
  (write-string "    which is bound to: ")
  (write (variable-value variable))
  (newline))

(define (print-short-description prefix description)
  (write-string "    ")
  (if prefix
      (begin
	(write-string prefix)
	(write-string ": ")))
  (write-description (string-first-line description))
  (newline))

(define (string-first-line string)
    (let ((index (string-find-next-char string #\newline)))
      (if index
	  (substring string 0 index)
	  string)))

(define (substitute-command-keys string #!optional buffer)
  (let ((buffer (if (default-object? buffer) (current-buffer) buffer))
	(end (string-length string)))
    (letrec
	((find-escape
	  (lambda (start*)
	    (let loop ((start start*))
	      (let ((index (substring-find-next-char string start end #\\)))
		(if (not index)
		    (list (substring string start* end))
		    (let ((next (+ index 1)))
		      (cond ((= next end)
			     (list (substring string start* end)))
			    ((char=? #\[ (string-ref string next))
			     (cons (substring string start* index)
				   (subst-key (+ next 1))))
			    ((char=? #\= (string-ref string next))
			     (cons (substring string start* index)
				   (quote-next (+ next 1))))
			    (else
			     (loop next)))))))))
	 (subst-key
	  (lambda (start)
	    (let ((index (substring-find-next-char string start end #\])))
	      (if (not index)
		  (error "SUBSTITUTE-COMMAND-KEYS: Missing ]")
		  (cons (command->key-name
			 (name->command (substring string start index))
			 buffer)
			(find-escape (+ index 1)))))))
	 (quote-next
	  (lambda (start)
	    (if (= start end)
		(finish start)
		(let ((next (+ start 1)))
		  (if (char=? #\\ (string-ref string start))
		      (if (= next end)
			  (finish start)
			  (continue start (+ next 1)))
		      (continue start next))))))
	 (continue
	  (lambda (start end)
	    (cons (substring string start end)
		  (find-escape end))))
	 (finish
	  (lambda (start)
	    (list (substring string start end)))))
      (apply string-append (find-escape 0)))))

(define (command->key-name command buffer)
  (let ((bindings (comtab-key-bindings (buffer-comtabs buffer) command)))
    (if (null? bindings)
	(string-append "M-x " (command-name-string command))
	(xkey->name (car bindings)))))