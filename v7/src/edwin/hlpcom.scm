;;; -*-Scheme-*-
;;;
;;; $Id: hlpcom.scm,v 1.116 2000/06/05 19:22:36 cph Exp $
;;;
;;; Copyright (c) 1986, 1989-2000 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

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
S  describe-syntax.  Display contents of syntax table, plus explanations.
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
    (let ((command (local-comtab-entry (current-comtabs) key (current-point))))
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
    (let ((command (local-comtab-entry (current-comtabs) key (current-point))))
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
    (set-variable-value! (name->variable variable) value)))

(define-command make-local-variable
  "Make a variable have a local value in the current buffer."
  (lambda ()
    (let ((variable (prompt-for-variable "Make local variable")))
      (list (variable-name variable)
	    (prompt-for-expression-value
	     (string-append "Set " (variable-name-string variable) " to value")
	     (variable-value variable)))))
  (lambda (variable value)
    (define-variable-local-value! (current-buffer) (name->variable variable)
      value)))

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
  (with-output-to-temporary-buffer "*Help*" '(READ-ONLY) thunk))

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
	  (lambda (start* comtabs)
	    (let loop ((start start*))
	      (let ((index (substring-find-next-char string start end #\\)))
		(if (not index)
		    (list (substring string start* end))
		    (let ((next (fix:+ index 1)))
		      (cond ((fix:= next end)
			     (list (substring string start* end)))
			    ((char=? #\[ (string-ref string next))
			     (find-terminator start* index #\]
					      subst-key comtabs))
			    ((char=? #\{ (string-ref string next))
			     (find-terminator start* index #\}
					      show-bindings comtabs))
			    ((char=? #\< (string-ref string next))
			     (find-terminator start* index #\>
					      new-mode comtabs))
			    ((char=? #\= (string-ref string next))
			     (cons (substring string start* index)
				   (quote-next (fix:+ next 1) comtabs)))
			    (else
			     (loop next)))))))))
	 (find-terminator
	  (lambda (start slash char procedure comtabs)
	    (cons (substring string start slash)
		  (let ((start (fix:+ slash 2)))
		    (let ((terminator
			   (substring-find-next-char string start end char)))
		      (if (not terminator)
			  (error "Missing terminator character:" char))
		      (procedure (substring string start terminator)
				 (fix:+ terminator 1)
				 comtabs))))))
	 (subst-key
	  (lambda (argument next comtabs)
	    (cons (let ((command (name->command argument #f)))
		    (if command
			(let ((bindings (comtab-key-bindings comtabs command)))
			  (if (null? bindings)
			      (string-append "M-x "
					     (command-name-string command))
			      (xkey->name (car bindings))))
			(string-append "M-x " argument)))
		  (find-escape next comtabs))))
	 (show-bindings
	  (lambda (argument next comtabs)
	    comtabs
	    (cons (let ((port (make-accumulator-output-port)))
		    (describe-bindings
		     (mode-comtabs (name->mode argument 'ERROR))
		     #f
		     port)
		    (newline port)
		    (get-output-from-accumulator port))
		  (find-escape next comtabs))))
	 (new-mode
	  (lambda (argument next comtabs)
	    comtabs
	    (find-escape next
			 (mode-comtabs (name->mode argument 'ERROR)))))
	 (quote-next
	  (lambda (start comtabs)
	    (if (fix:= start end)
		(finish start)
		(let ((next (fix:+ start 1)))
		  (if (char=? #\\ (string-ref string start))
		      (if (fix:= next end)
			  (finish start)
			  (continue start (fix:+ next 1) comtabs))
		      (continue start next comtabs))))))
	 (continue
	  (lambda (start end comtabs)
	    (cons (substring string start end)
		  (find-escape end comtabs))))
	 (finish
	  (lambda (start)
	    (list (substring string start end)))))
      (apply string-append
	     (find-escape 0 (buffer-comtabs buffer))))))