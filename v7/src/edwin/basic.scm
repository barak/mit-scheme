;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/basic.scm,v 1.95 1989/03/14 07:58:42 cph Exp $
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

;;;; Basic Commands

(declare (usual-integrations))

(define-command ("^R Bad Command")
  "This command is used to capture undefined keys.
It is usually called directly by the command lookup
procedure when it fails to find a command."
  (editor-error "Undefined command: " (xchar->name (current-command-char))))

(define-command ("^R Insert Self" (argument 1))
  "Insert the character used to invoke this.
With an argument, insert the character that many times."
  (insert-chars (current-command-char) argument))

(define-command ("^R Quoted Insert" (argument 1))
  "Reads a character and inserts it."
  (define (read-char)
    (let ((char (keyboard-read-char)))
      (set-command-prompt! (string-append (command-prompt) (char-name char)))
      char))

  (define (read-digit)
    (or (char->digit (read-char) 8)
	(editor-error "Not an octal digit")))

  (set-command-prompt! "Quote Character: ")
  (insert-chars (let ((char (read-char)))
		  (let ((digit (char->digit char 4)))
		    (if digit
			(ascii->char
			 (let ((digit2 (read-digit)))
			   (let ((digit3 (read-digit)))
			     (+ (* (+ (* digit 8) digit2) 8) digit3))))
			char)))
		argument))

(define-command ("^R Open Line" (argument 1))
  "Insert a newline after point.
Differs from ordinary insertion in that point remains
before the inserted characters.
With an argument, inserts several newlines."
  (let ((m* (mark-right-inserting (current-point))))
    (insert-newlines argument)
    (set-current-point! m*)))

(define (xchar->name char)
  (if (pair? char)
      (chars->name char)
      (char-name char)))

(define (chars->name chars)
  (if (null? chars)
      ""
      (string-append-separated (char-name (car chars))
			       (chars->name (cdr chars)))))

(define (string-append-separated x y)
  (cond ((string-null? x) y)
	((string-null? y) x)
	(else (string-append x " " y))))

(define (editor-error . strings)
  (if (not (null? strings)) (apply temporary-message strings))
  (editor-beep)
  (abort-current-command))

(define (editor-failure . strings)
  (cond ((not (null? strings)) (apply temporary-message strings))
	(*defining-keyboard-macro?* (clear-message)))
  (editor-beep)
  (keyboard-macro-disable))

(define-integrable (editor-beep)
  (screen-beep (current-screen)))

(define (not-implemented)
  (editor-error "Not yet implemented"))

(define-command ("^R Prefix Control")
  "Sets Control-bit of following character.
This command followed by an = is equivalent to a Control-=."
  (read-extension-char "C-" char-controlify))

(define-command ("^R Prefix Meta")
  "Sets Meta-bit of following character. 
Turns a following A into a Meta-A.
If the Metizer character is Altmode, it turns ^A
into Control-Meta-A.  Otherwise, it turns ^A into plain Meta-A."
  (read-extension-char "M-"
		       (if (let ((char (current-command-char)))
			     (and (char? char)
				  (char=? #\Altmode char)))
			   char-metafy
			   (lambda (char)
			     (char-metafy (char-base char))))))

(define-command ("^R Prefix Control-Meta")
  "Sets Control- and Meta-bits of following character.
Turns a following A (or C-A) into a Control-Meta-A."
  (read-extension-char "C-M-" char-control-metafy))

(define execute-extended-chars?
  true)

(define extension-commands
  (list (name->command "^R Prefix Control")
	(name->command "^R Prefix Meta")
	(name->command "^R Prefix Control-Meta")))

(define (read-extension-char prefix-string modifier)
  (if execute-extended-chars?
      (set-command-prompt-prefix! prefix-string))
  (let ((char (modifier (keyboard-read-char))))
    (if execute-extended-chars?
	(dispatch-on-char (current-comtabs) char)
	char)))

(define (set-command-prompt-prefix! prefix-string)
  (set-command-prompt!
   (string-append-separated (command-argument-prompt)
			    prefix-string)))

(define-command ("^R Prefix Character")
  "This is a prefix for more commands.
It reads another character (a subcommand) and dispatches on it."
  (let ((prefix-char (current-command-char)))
    (set-command-prompt-prefix! (string-append (xchar->name prefix-char) " "))
    (dispatch-on-char (current-comtabs)
		      ((if (pair? prefix-char) append cons)
		       prefix-char
		       (list (keyboard-read-char))))))

(define-command ("^R Extended Command")
  "Read an extended command from the terminal with completion.
This command reads the name of a function, with completion.  Then the
function is called.  Completion is done as the function name is typed
For more information type the HELP key while entering the name."
  (dispatch-on-command (prompt-for-command "Extended Command")))

(define-command ("^R Return to Superior" argument)
  "Go back to Scheme's superior job.
With argument, saves visited file first."
  (if argument (^r-save-file-command))
  (quit)
  (update-screens! true))

(define-command ("^R Scheme")
  "Stop Edwin and return to Scheme."
  (editor-abort *the-non-printing-object*))

(define-command ("^R Exit")
  "Exit normally from a subsystem of a level of editing.
At top level, exit from Edwin like \\[^R Return to Superior]."
  (exit-recursive-edit 'EXIT))

(define-command ("Abort Recursive Edit")
  "Abnormal exit from recursive editing command.
The recursive edit is exited and the command that invoked it is aborted.
For a normal exit, you should use \\[^R Exit], NOT this command."
  (exit-recursive-edit 'ABORT))

(define-command ("^R Narrow Bounds to Region")
  "Restrict editing in current buffer to text between point and mark.
Use \\[^R Widen Bounds] to undo the effects of this command."
  (region-clip! (current-region)))

(define-command ("^R Widen Bounds")
  "Remove restrictions from current buffer.
Allows full text to be seen and edited."
  (buffer-widen! (current-buffer)))

(define-command ("Set Key")
  "Define a key binding from the keyboard.
Prompts for a command and a key, and sets the key's binding.
The key is bound in Fundamental Mode."
  (let ((command (prompt-for-command "Command")))
    (let ((key (prompt-for-key (string-append "Put \""
					      (command-name command)
					      "\" on key")
			       (mode-comtabs fundamental-mode))))
      (if (prompt-for-confirmation? "Go ahead")
	  (define-key "Fundamental" key (command-name command))))))

;;;; Comment Commands

(define-variable "Comment Column"
  "Column to indent right-margin comments to."
  32)

(define-variable "Comment Locator Hook"
  "Procedure to find a comment, or false if no comment syntax defined.
The procedure is passed a mark, and should return false if it cannot
find a comment, or a pair of marks.  The car should be the start of
the comment, and the cdr should be the end of the comment's starter."
  false)

(define-variable "Comment Indent Hook"
  "Procedure to compute desired indentation for a comment.
The procedure is passed the start mark of the comment
and should return the column to indent the comment to."
  false)

(define-variable "Comment Start"
  "String to insert to start a new comment."
  "")

(define-variable "Comment End"
  "String to insert to end a new comment.
This should be a null string if comments are terminated by Newline."
  "")

(define-command ("^R Set Comment Column" argument)
  "Set the comment column based on point.
With no arg, set the comment column to the current column.
With just minus as an arg, kill any comment on this line.
Otherwise, set the comment column to the argument."
  (cond ((command-argument-negative-only?)
	 (^r-kill-comment-command))
	(else
	 (set! comment-column (or argument (current-column)))
	 (message "Comment column set to " (write-to-string comment-column)))))

(define-command ("^R Indent for Comment")
  "Indent this line's comment to comment column, or insert an empty comment."
  (if (not (ref-variable "Comment Locator Hook"))
      (editor-error "No comment syntax defined")
      (let ((start (line-start (current-point) 0))
	    (end (line-end (current-point) 0)))
	(let ((com ((ref-variable "Comment Locator Hook") start)))
	  (set-current-point! (if com (car com) end))
	  (if com (mark-permanent! (cdr com)))
	  (let ((indent ((ref-variable "Comment Indent Hook")
			 (current-point))))
	    (maybe-change-column indent)
	    (if com
		(set-current-point! (cdr com))
		(begin (insert-string (ref-variable "Comment Start"))
		       (insert-comment-end))))))))

(define-variable "Comment Multi Line"
  "If true, means \\[^R Indent New Comment Line] should continue same comment
on new line, with no new terminator or starter."
  false)

(define-command ("^R Indent New Comment Line")
  "Break line at point and indent, continuing comment if presently within one."
  (define (if-not-in-comment)
    (if (ref-variable "Fill Prefix")
	(insert-string (ref-variable "Fill Prefix"))
	(^r-indent-according-to-mode-command)))
  (delete-horizontal-space)
  (insert-newlines 1)
  (if (ref-variable "Comment Locator Hook")
      (let ((com ((ref-variable "Comment Locator Hook")
		  (line-start (current-point) -1))))
	(if com
	    (let ((start-column (mark-column (car com)))
		  (end-column (mark-column (cdr com)))
		  (comment-start (extract-string (car com) (cdr com))))
	      (if (ref-variable "Comment Multi Line")
		  (maybe-change-column end-column)
		  (begin (insert-string (ref-variable "Comment End")
					(line-end (current-point) -1))
			 (maybe-change-column start-column)
			 (insert-string comment-start)))
	      (if (line-end? (current-point))
		  (insert-comment-end)))
	    (if-not-in-comment)))
      (if-not-in-comment)))

(define (insert-comment-end)
  (let ((point (mark-right-inserting (current-point))))
    (insert-string (ref-variable "Comment End"))
    (set-current-point! point)))

(define-command ("^R Kill Comment")
  "Kill the comment on this line, if any."
  (if (not (ref-variable "Comment Locator Hook"))
      (editor-error "No comment syntax defined")
      (let ((start (line-start (current-point) 0))
	    (end (line-end (current-point) 0)))
	(let ((com ((ref-variable "Comment Locator Hook") start)))
	  (if com
	      (kill-string (horizontal-space-start (car com)) end))))))