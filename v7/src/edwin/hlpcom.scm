;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/hlpcom.scm,v 1.86 1989/04/05 18:21:08 cph Exp $
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

;;;; Help Commands

(declare (usual-integrations))

(define-command ("^R Help Prefix")
  "This is a prefix for more commands.
It reads another character (a subcommand) and dispatches on it."
  (let ((char (prompt-for-char "A C D I K L M T V W or C-h for more help")))
    (dispatch-on-char
     (current-comtabs)
     (list #\Backspace
	   (if (or (char=? char #\Backspace)
		   (char=? char #\?))
	       (let ((buffer (temporary-buffer "*Help*")))
		 (insert-string 
		  "You have typed C-h, the help character.  Type a Help option:

A   Command apropos.  Type a substring, and see a list of commands
	that contain that substring.
C   Describe key briefly.  Type a key sequence;
	it prints the name of the command that sequence runs.
D   Describe command.  Type a command name and get its documentation.
I   Info.  The Info documentation reader.
K   Describe key.  Type a key sequence;
	it prints the full documentation.
L   View Lossage.  Prints the last 100 characters you typed.
M   Describe Mode.  Print documentation of current major mode,
	which describes the commands peculiar to it.
T   Help with Tutorial.  Select the Emacs learn-by-doing tutorial.
V   Describe variable.  Type a variable name and get its documentation.
W   Where is.  Type a command name and get its key binding."
		  (buffer-point buffer))
		 (set-buffer-point! buffer (buffer-start buffer))
		 (buffer-not-modified! buffer)
		 (pop-up-buffer buffer false)
		 (let ((window (get-buffer-window buffer)))
		   (define (loop)
		     (let ((char
			    (char-upcase
			     (prompt-for-typein
			      "A C D I K L M T V W or space to scroll: "
			       keyboard-read-char))))
		       (cond ((or (char=? char #\Backspace)
				  (char=? char #\?))
			      (loop))
			     ((or (char=? char #\Space)
				  (char=? char #\C-V))
			      (scroll-window window
					     (standard-scroll-window-argument
					      window false 1)
					     editor-beep)
			      (loop))
			     ((or (char=? char #\Rubout)
				  (char=? char #\M-V))
			      (scroll-window window
					     (standard-scroll-window-argument
					      window false -1)
					     editor-beep)
			      (loop))
			     (else char))))
		   (loop)))
	       char)))))

(define-prefix-key "Fundamental" #\Backspace "^R Help Prefix")
(define-key "Fundamental" '(#\Backspace #\A) "Command Apropos")
(define-key "Fundamental" '(#\Backspace #\C) "Describe Key Briefly")
(define-key "Fundamental" '(#\Backspace #\D) "Describe Command")
(define-key "Fundamental" '(#\Backspace #\I) "Info")
(define-key "Fundamental" '(#\Backspace #\K) "Describe Key")
(define-key "Fundamental" '(#\Backspace #\L) "View Lossage")
(define-key "Fundamental" '(#\Backspace #\M) "Describe Mode")
(define-key "Fundamental" '(#\Backspace #\T) "Teach Emacs")
(define-key "Fundamental" '(#\Backspace #\V) "Describe Variable")
(define-key "Fundamental" '(#\Backspace #\W) "Where Is")

;;;; Commands and Keys

(define-command ("Command Apropos")
  "Prompts for a string, lists all commands containing it."
  (let ((string (or (prompt-for-string "Command apropos" false) "")))
    (with-output-to-help-display
     (lambda ()
       (for-each (lambda (command)
		   (write-string (command-name command))
		   (newline)
		   (print-key-bindings command)
		   (print-short-description (command-description command)))
		 (string-table-apropos editor-commands string))))))

(define-command ("Describe Command")
  "Prompts for a command, and describes it.
Prints the full documentation for the given command."
  (let ((command (prompt-for-command "Describe Command")))
    (with-output-to-help-display
     (lambda ()
       (write-string (command-name command))
       (newline)
       (print-key-bindings command)
       (write-description (command-description command))))))

(define-command ("Where Is")
  "Prompts for a command, and shows what key it is bound to."
  (let ((command (prompt-for-command "Where is command")))
    (let ((bindings (comtab-key-bindings (current-comtabs) command)))
      (if (null? bindings)
	  (message "\"" (command-name command) "\" is not on any keys")
	  (message "\"" (command-name command) "\" is on "
		   (xchar->name (car bindings)))))))

(define-command ("Describe Key Briefly")
  "Prompts for a key, and describes the command it is bound to.
Prints the brief documentation for that command."
  (let ((char (prompt-for-key "Describe key briefly" (current-comtabs))))
    (let ((command (comtab-entry (current-comtabs) char)))
      (if (eq? command (name->command "^R Bad Command"))
	  (help-describe-unbound-key char)
	  (message (xchar->name char)
		   " runs the command \""
		   (command-name command)
		   "\"")))))

(define-command ("Describe Key")
  "Prompts for a key, and describes the command it is bound to.
Prints the full documentation for that command."
  (let ((char (prompt-for-key "Describe key" (current-comtabs))))
    (let ((command (comtab-entry (current-comtabs) char)))
      (if (eq? command (name->command "^R Bad Command"))
	  (help-describe-unbound-key char)
	  (with-output-to-help-display
	   (lambda ()
	     (write-string (string-append (xchar->name char)
					  " runs the command \""
					  (command-name command)
					  "\":"))
	     (newline)
	     (write-description (command-description command))))))))

(define (help-describe-unbound-key char)
  (message (xchar->name char) " is undefined"))

;;;; Variables

(define-command ("Variable Apropos")
  "Prompts for a string, lists all variables containing it."
  (let ((string (or (prompt-for-string "Variable apropos" false) "")))
    (with-output-to-help-display
     (lambda ()
       (for-each (lambda (variable)
		   (write-string (variable-name variable))
		   (newline)
		   (print-variable-binding variable)
		   (print-short-description (variable-description variable)))
		 (string-table-apropos editor-variables string))))))

(define-command ("Describe Variable")
  "Prompts for a variable, and describes it.
Prints the full documentation for the given variable."
  (let ((variable (prompt-for-variable "Describe Variable")))
    (with-output-to-help-display
     (lambda ()
       (write-string (variable-name variable))
       (newline)
       (print-variable-binding variable)
       (write-description (variable-description variable))))))

(define-command ("Set Variable" argument)
  "Change the value of a variable.
Prompts for a variable, then sets its value to the argument, if any.
If no argument is given, reads a Scheme expression and evaluates it,
using that for the value."
  (let ((variable (prompt-for-variable "Set Variable")))
    (variable-set! variable
		   (or argument
		       (prompt-for-expression-value
			"Value"
			(write-to-string (variable-ref variable)))))))

(define-command ("Make Local Variable" argument)
  "Make a variable have a local value in the current buffer.
With no argument, the variable's value is unchanged.
A numeric argument becomes the new value of the variable.
Just \\[^R Universal Argument] means prompt for the new value."
  (let ((variable (prompt-for-variable "Make Local Variable")))
    (make-local-binding! (variable-symbol variable)
			 (cond ((not argument) (variable-ref variable))
			       ((command-argument-multiplier-only?)
				(prompt-for-expression-value
				 "Value"
				 (write-to-string (variable-ref variable))))
			       (else argument)))))

(define-command ("Kill Local Variable")
  "Make a variable use its global value in the current buffer."
  (unmake-local-binding!
   (variable-symbol (prompt-for-variable "Kill Local Variable"))))

;;;; Other Stuff

(define-command ("View Lossage")
  "Print the keyboard history."
  (with-output-to-help-display
   (lambda ()
     (for-each (lambda (char)
		 (write-string (string-append (char-name char) " ")))
	       (reverse (ring-list (current-char-history)))))))

(define-command ("Describe Mode")
  "Print the documentation for the current mode."
  (with-output-to-help-display
   (lambda ()
     (write-description (mode-description (current-major-mode))))))

(define-command ("Teach Emacs")
  "Visit the Emacs learn-by-doing tutorial."
  (delete-other-windows (current-window))
  (let ((pathname
	 (merge-pathnames (string->pathname "TUTORIAL")
			  (home-directory-pathname))))
    (let ((buffer (pathname->buffer pathname)))
      (if buffer
	  (select-buffer buffer)
	  (let ((buffer (new-buffer (pathname->buffer-name pathname))))
	    (read-buffer buffer edwin-tutorial-pathname)
	    (set-buffer-pathname! buffer pathname)
	    (set-buffer-truename! buffer false)
	    (select-buffer buffer)
	    (set-current-major-mode! fundamental-mode)
	    (disable-buffer-auto-save! buffer)
	    (let ((mark
		   (mark1+
		    (line-end (search-forward "\n<<" (buffer-start buffer))
			      0))))
	      (delete-string mark (line-end mark 0))
	      (insert-newlines (- (window-y-size (current-window))
				  (+ 4 (region-count-lines
					(make-region (buffer-start buffer)
						     mark))))
			       mark))
	    (set-buffer-point! buffer (buffer-start buffer))
	    (buffer-not-modified! buffer))))))

(define (with-output-to-help-display thunk)
  (with-output-to-temporary-buffer "*Help*" thunk))

(define (write-description description)
  (write-string (substitute-command-keys description)))

(define (print-key-bindings command)
  (let ((bindings (comtab-key-bindings (current-comtabs) command)))
    (if (not (null? bindings))
	(begin (write-string "    which is bound to:    ")
	       (write-string (char-list-string bindings))
	       (newline)))))

(define (char-list-string xchars)
  (if (null? (cdr xchars))
      (xchar->name (car xchars))
      (string-append (xchar->name (car xchars))
		     ", "
		     (char-list-string (cdr xchars)))))
(define (print-variable-binding variable)
  (write-string "    which is ")
  (cond ((variable-unbound? variable)
	 (write-string "unbound"))
	((variable-unassigned? variable)
	 (write-string "unassigned"))
	(else
	 (write-string "bound to: ")
	 (write (variable-ref variable))))
  (newline))

(define (print-short-description description)
  (write-string "    ")
  (write-description (string-first-line description))
  (newline))

(define (string-first-line string)
    (let ((index (string-find-next-char string #\newline)))
      (if index
	  (substring string 0 index)
	  string)))

(define (substitute-command-keys string #!optional start end)
  (let ((start (if (default-object? start) 0 start))
	(end (if (default-object? end) (string-length string) end)))
    
      (define (find-escape start*)
	(define (loop start)
	  (let ((index (substring-find-next-char string start end #\\)))
	    (if (not index)
		(list (substring string start* end))
		(let ((next (1+ index)))
		  (if (= next end)
		      (list (substring string start* end))
		      (cond ((char=? #\[ (string-ref string next))
			     (cons (substring string start* index)
				   (subst-key (1+ next))))
			    ((char=? #\= (string-ref string next))
			     (cons (substring string start* index)
				   (quote-next (1+ next))))
			    (else (loop next))))))))
	(loop start*))

      (define (subst-key start)
	(let ((index (substring-find-next-char string start end #\])))
	  (if (not index)
	      (error "SUBSTITUTE-COMMAND-KEYS: Missing ]")
	      (cons (command->key-name
		     (name->command (substring string start index)))
		    (find-escape (1+ index))))))

      (define (quote-next start)
	(if (= start end)
	    (finish start)
	    (let ((next (1+ start)))
	      (if (char=? #\\ (string-ref string start))
		  (if (= next end)
		      (finish start)
		      (continue start (1+ next)))
		  (continue start next)))))

      (define (continue start end)
	(cons (substring string start end)
	      (find-escape end)))

      (define (finish start)
	(list (substring string start end)))

      (apply string-append (find-escape start))))

(define (command->key-name command)
  (let ((bindings (comtab-key-bindings (current-comtabs) command)))
    (if (null? bindings)
	(string-append "M-X " (command-name command))
	(xchar->name (car bindings)))))