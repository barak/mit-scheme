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

;;;; Autoloads for Edwin

(declare (usual-integrations))
(using-syntax edwin-syntax-table

;;;; Definitions

(define (define-autoload-major-mode name super-mode-name library-name
	  description)
  (define mode
    (make-mode name #!TRUE
	       (if super-mode-name
		   (mode-comtabs (name->mode super-mode-name))
		   '())
	       description
	       (lambda arguments
		 (load-library library-name)
		 (apply (mode-initialization mode) arguments))))
  mode)

(define (define-autoload-minor-mode name library-name description)
  (define mode
    (make-mode name #!FALSE '()
	       description
	       (lambda arguments
		 (load-library library-name)
		 (apply (mode-initialization mode) arguments))))
  mode)

(define (autoloading-mode? mode)
  (or (autoloading-procedure? (mode-initialization mode)
			      define-autoload-major-mode)
      (autoloading-procedure? (mode-initialization mode)
			      define-autoload-minor-mode)))

(define (define-autoload-command name library-name description)
  (define command
    (make-command name description
		  (lambda arguments
		    (load-library library-name)
		    (apply (command-procedure command) arguments))))
  command)

(define (autoloading-command? command)
  (autoloading-procedure? (command-procedure command)
			  define-autoload-command))

(define (define-autoload-procedure package name library-name)
  (local-assignment package name
		    (lambda arguments
		      (load-library library-name)
		      (apply (lexical-reference package name) arguments))))

(define (autoloading-procedure? procedure parent)
  (and (compound-procedure? procedure)
       (let ((environment (procedure-environment procedure)))
	 (and (environment? environment)
	      (eq? (environment-procedure environment) parent)
	      (access library-name environment)))))

;;;; Libraries

(define loaded-libraries
  '())

(define (library-loaded? name)
  (memq name loaded-libraries))

(define library-load-hooks
  '())

(define (add-library-load-hook! name hook)
  (if (library-loaded? name)
      (hook)
      (let ((entry (assq name library-load-hooks)))
	(if entry
	    (append! entry (list hook))
	    (set! library-load-hooks
		  (cons (list name hook)
			library-load-hooks))))))

(define (run-library-load-hooks! name)
  (let ((entry (assq name library-load-hooks)))
    (define (loop)
      (if (null? (cdr entry))
	  (set! library-load-hooks (delq! entry library-load-hooks))
	  (let ((hook (cadr entry)))
	    (set-cdr! entry (cddr entry))
	    (hook)
	    (loop))))
    (if entry (loop))))

(define (load-library name)
  (if (not (library-loaded? name))
      (let ((entry (assq name (access :libraries edwin-system))))
	(if entry
	    (%load-library entry)
	    (error "LOAD-LIBRARY: Unknown library name" name)))))

(define (%load-library library)
  (apply load-edwin-file (cdr library))
  (if (not (memq (car library) loaded-libraries))
      (set! loaded-libraries (cons (car library) loaded-libraries)))
  (run-library-load-hooks! (car library)))

;;;; Loading

(define load-edwin-file)
(let ()

(define binary-fasload
  (make-primitive-procedure 'BINARY-FASLOAD))

(set! load-edwin-file
(named-lambda (load-edwin-file filename purify? package)
  (let ((filename (canonicalize-input-filename filename)))
    (temporary-message "Loading file '" filename "'")
    (let ((scode (binary-fasload filename)))
      (append-message " -- done")
      (if purify?
	  (begin (temporary-message "Purify...")
		 (purify scode (eq? purify? 'PURE))))
      (temporary-message "Evaluate...")
      (scode-eval scode package)))
  (temporary-message "Done")))

)

(define-variable "Load File Default"
  "Pathname given as default for \\[Load File]."
  (string->pathname "EDB:FOO.BIN.0"))

(define-command ("Load File" argument)
  "Load an Edwin binary file.
An argument, if given, means purify the file too."
  (let ((pathname (prompt-for-pathname "Load File"
				       (ref-variable "Load File Default"))))
    (set-variable! "Load File Default" pathname)
    (load-edwin-file pathname argument edwin-package)))

(define-command ("Load Library" argument)
  "Load an Edwin library."
  (%load-library
   (prompt-for-alist-value "Load Library"
			   (map (lambda (library)
				  (cons (symbol->string (car library))
					library))
				(access :libraries edwin-system)))))

;;;; Various Libraries

(define-variable "Info Enable Edit"
  "If true, the \\[^R Info Edit] command in Info can edit the current node."
  #!FALSE)

(define-variable "Info Enable Active Nodes"
  "If true, allows Info to execute Scheme code associated with nodes.
The Scheme code is executed when the node is selected."
  #!TRUE)

(define-variable "Info Directory"
  "Default directory pathname for Info documentation files."
  "SDOC:DIR.INFO.0")

(define-variable "Info Previous Search"
  "Default search string for Info \\[^R Info Search] command to search for."
  #!FALSE)

(define-variable "Info Tag Table Start" "")
(define-variable "Info Tag Table End" "")

(define-autoload-command "Info" 'INFO
  "Create a buffer for Info, the documentation browser program.")

(define-variable "List Directory Unpacked"
  "If not false, \\[List Directory] puts one file on each line.
Normally it packs many onto a line.
This has no effect if \\[List Directory] is invoked with an argument."
  #!FALSE)

(define-autoload-command "Dired" 'DIRED
  "Edit a directory.  You type the directory name.")

(define-autoload-command "Dired Other Window" 'DIRED
  "Edit a directory in another window.  You type the directory name.")

(define-autoload-command "List Directory" 'DIRED
  "Generate a directory listing.")

(define-autoload-command "Kill Rectangle" 'RECTANGLE-COMMANDS
  "Delete rectangle with corners at point and mark; save as last killed one.")

(define-autoload-command "Delete Rectangle" 'RECTANGLE-COMMANDS
  "Delete (don't save) text in rectangle with point and mark as corners.
The same range of columns is deleted in each line
starting with the line where the region begins
and ending with the line where the region ends.")

(define-autoload-command "Open Rectangle" 'RECTANGLE-COMMANDS
  "Blank out rectangle with corners at point and mark, shifting text right.
The text previously in the region is not overwritten by the blanks,
but instead winds up to the right of the rectangle.")

(define-autoload-command "Clear Rectangle" 'RECTANGLE-COMMANDS
  "Blank out rectangle with corners at point and mark.
The text previously in the region is overwritten by the blanks.")

(define-autoload-command "Yank Rectangle" 'RECTANGLE-COMMANDS
  "Yank the last killed rectangle with upper left corner at point.")

(define-autoload-procedure rectangle-package 'delete-rectangle
  'RECTANGLE-COMMANDS)

(define-autoload-procedure rectangle-package 'yank-rectangle
  'RECTANGLE-COMMANDS)

(define-autoload-command "Make Command Summary" 'COMMAND-SUMMARY
  "Make a summary of current key bindings in the buffer *Summary*.
Previous contents of that buffer are killed first.")

;;;; Tags Package

(define-variable "Tags Table Pathname"
  "Pathname of current tags table."
  false)

(define-autoload-command "Visit Tags Table" 'TAGS
  "Tell tags commands to use a given tags table file.")

(define-autoload-command "Find Tag" 'TAGS
  "Find tag (in current tags table) whose name contains a given string.
 Selects the buffer that the tag is contained in
and puts point at its definition.
 With argument, searches for the next tag in the tags table that matches
the string used in the previous Find Tag.")

(define-autoload-command "Find Tag Other Window" 'TAGS
  "Like \\[Find Tag], but selects buffer in another window.")

(define-autoload-command "Generate Tags Table" 'TAGS
  "Generate a tags table from a files list of Scheme files.
 A files list is a file containing only strings which are file names.
 The generated tags table has the same name as the files list, except that
the file type is TAG.")

(define-autoload-command "Tags Search" 'TAGS
  "Search through all files listed in tag table for a given string.
Stops when a match is found.
To continue searching for next match, use command \\[Tags Loop Continue].")

(define-autoload-command "RE Tags Search" 'TAGS
  "Search through all files listed in tag table for a given regexp.
Stops when a match is found.
To continue searching for next match, use command \\[Tags Loop Continue].")

(define-autoload-command "Tags Query Replace" 'TAGS
  "Query replace a given string with another one though all files listed
in tag table.  If you exit (C-G or Altmode), you can resume the query
replace with the command \\[Tags Loop Continue].")

(define-autoload-command "Tags Loop Continue" 'TAGS
  "Continue last \\[Tags Search] or \\[Tags Query Replace] command.")

;;;; Debug Library

(define-variable "Continuation Browser Student Walk"
  "If true, changes \\[^R Continuation Browser Forward] and
\\[^R Continuation Browser Backward] to only walk through reductions
of subproblem 0."
  #!FALSE)

(define (debugger-scheme-error-hook environment message irritant
				    substitute-environment?)
  (fluid-let ((processing-error? #!TRUE))
    (if (within-typein-edit
	 (lambda ()
	   (let ((window (current-window)))
	     (define (loop)
	       (let ((char (char-upcase (keyboard-read-char))))
		 (cond ((or (char=? #\Y char)
			    (char=? #\Space char))
			(insert-string "Yes" (window-point window))
			(window-direct-update! window #!FALSE)
			#!TRUE)
		       ((or (char=? #\N char)
			    (char=? #\Rubout char))
			(insert-string "No" (window-point window))
			(window-direct-update! window #!FALSE)
			#!FALSE)
		       (else
			(beep)
			(loop)))))
	     (with-output-to-mark-truncating (window-point window)
					     (- (window-x-size window) 15)
	       (lambda ()
		 (write-string message)
		 (if (not (eq? irritant *the-non-printing-object*))
		     (begin (write-char #\Space)
			    (write irritant)))))
	     (insert-string " -- Debug? " (window-point window))
	     (beep)
	     (loop))))
	(begin (load-library 'DEBUG)
	       ((access start-debugger debugger-package)))
	(abort-current-command))))

(define-variable "& Scheme Error Hook"
  "The error hook to use for handling Scheme errors."
  debugger-scheme-error-hook)

;;;; Major Mode Libraries

(define-autoload-major-mode "Midas" "Fundamental" 'MIDAS-MODE
  "Major mode for editing assembly code.")

(define-autoload-command "Midas Mode" 'MIDAS-MODE
  "Enter Midas mode.")

(define-variable "Midas Mode Hook"
  "If not false, a thunk to call when entering Midas mode."
  #!FALSE)

(define-autoload-major-mode "Pascal" "Fundamental" 'PASCAL-MODE
  "Major mode specialized for editing Pascal code.")

(define-autoload-command "Pascal Mode" 'PASCAL-MODE
  "Enter Pascal mode.")

(define-variable "Pascal Mode Hook"
  "If not false, a thunk to call when entering Pascal mode."
  #!FALSE)

(define-variable "Pascal Shift Increment"
  "Indentation increment for Pascal Shift commands."
  2)

(define-variable "Pascal Indentation Keywords"
  "These keywords cause the lines below them to be indented to the right.
This must be a regular expression, or #!FALSE to disable the option."
  #!FALSE)

(define-autoload-major-mode "Texinfo" "Text" 'TEXINFO-MODE
  "Major mode for editing texinfo files.
These are files that are input for TeX and also to be turned
into Info files by \\[Texinfo Format Buffer].
These files must be written in a very restricted and
modified version of TeX input format.")

(define-autoload-command "Texinfo Mode" 'TEXINFO-MODE
  "Make the current mode be Texinfo mode.")

(define-variable "Texinfo Mode Hook"
  "A procedure to be called when Texinfo mode is entered, or false."
  #!FALSE)

(define-autoload-major-mode "C" "Fundamental" 'C-MODE
  "Major mode for editing C code.
Expression and list commands understand all C brackets.
Tab indents for C code.
Comments are delimited with /* ... */.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.
The characters { } ; : correct indentation when typed.

Variables controlling indentation style:
 C Auto Newline
    Non-false means automatically newline before and after braces,
    and after colons and semicolons, inserted in C code.
 C Indent Level
    Indentation of C statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.
 C Continued Statement Offset
    Extra indentation given to a substatement, such as the
    then-clause of an if or body of a while.
 C Brace Offset
    Extra indentation for line if it starts with an open brace.
 C Brace Imaginary Offset
    An open brace following other text is treated as if it were
    this far to the right of the start of its line.
 C Argdecl Indent
    Indentation level of declarations of C function arguments.
 C Label Offset
    Extra indentation for line that is a label, or case or default.")

(define-autoload-command "C Mode" 'C-MODE
  "Enter C mode.")

(define-variable "C Mode Hook"
  "If not false, a thunk to call when entering C mode."
  #!FALSE)

(define-variable "C Indent Level"
  "Indentation of C statements with respect to containing block."
  2)

(define-variable "C Brace Offset"
  "Extra indentation for braces, compared with other text in same context."
  0)

(define-variable "C Brace Imaginary Offset"
  "Imagined indentation of a C open brace that actually follows a statement."
  0)

(define-variable "C Argdecl Indent"
  "Indentation level of declarations of C function arguments."
  5)

(define-variable "C Label Offset"
  "Offset of C label lines and case statements relative to usual indentation."
  -2)

(define-variable "C Continued Statement Offset"
  "Extra indent for lines not starting new statements."
  2)

(define-variable "C Auto Newline"
  "Non-false means automatically newline before and after braces,
and after colons and semicolons, inserted in C code."
  #!FALSE)

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: edwin-package
;;; Scheme Syntax Table: edwin-syntax-table
;;; Tags Table Pathname: (access edwin-tags-pathname edwin-package)
;;; End:
