;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/c-mode.scm,v 1.46 1991/04/12 23:17:56 cph Exp $
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

;;;; C Mode (from GNU Emacs)

(declare (usual-integrations))

(define-command c-mode
  "Enter C mode."
  ()
  (lambda () (set-current-major-mode! (ref-mode-object c))))

(define-major-mode c fundamental "C"
  "Major mode for editing C code.
Expression and list commands understand all C brackets.
Tab indents for C code.
Comments are delimited with /* ... */.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.
The characters { } ; : correct indentation when typed.

Variables controlling indentation style:
 c-auto-newline
    Non-false means automatically newline before and after braces,
    and after colons and semicolons, inserted in C code.
 c-indent-level
    Indentation of C statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.
 c-continued-statement-offset
    Extra indentation given to a substatement, such as the
    then-clause of an if or body of a while.
 c-brace-offset
    Extra indentation for line if it starts with an open brace.
 c-brace-imaginary-offset
    An open brace following other text is treated as if it were
    this far to the right of the start of its line.
 c-argdecl-indent
    Indentation level of declarations of C function arguments.
 c-label-offset
    Extra indentation for line that is a label, or case or default."

  (local-set-variable! syntax-table c-mode:syntax-table)
  (local-set-variable! syntax-ignore-comments-backwards true)
  (local-set-variable! paragraph-start "^$")
  (local-set-variable! paragraph-separate (ref-variable paragraph-start))
  (local-set-variable! indent-line-procedure (ref-command c-indent-line))
  (local-set-variable! require-final-newline true)
  (local-set-variable! comment-locator-hook c-mode:comment-locate)
  (local-set-variable! comment-indent-hook c-mode:comment-indent)
  (local-set-variable! comment-start "/* ")
  (local-set-variable! comment-end " */")
  (local-set-variable! comment-column 32)
  (event-distributor/invoke! (ref-variable c-mode-hook)))

(define-variable c-mode-hook
  "An event distributor that is invoked when entering C mode."
  (make-event-distributor))

(define-key 'c #\linefeed 'reindent-then-newline-and-indent)
(define-key 'c #\) 'lisp-insert-paren)
(define-key 'c #\{ 'electric-c-brace)
(define-key 'c #\} 'electric-c-brace)
(define-key 'c #\; 'electric-c-semi)
(define-key 'c #\: 'electric-c-terminator)
(define-key 'c #\c-m-h 'mark-c-procedure)
(define-key 'c #\c-m-q 'c-indent-expression)
(define-key 'c #\rubout 'backward-delete-char-untabify)
(define-key 'c #\tab 'c-indent-line)

(define c-mode:syntax-table (make-syntax-table))
(modify-syntax-entry! c-mode:syntax-table #\\ "\\")
(modify-syntax-entry! c-mode:syntax-table #\/ ". 14")
(modify-syntax-entry! c-mode:syntax-table #\* ". 23")
(modify-syntax-entry! c-mode:syntax-table #\+ ".")
(modify-syntax-entry! c-mode:syntax-table #\- ".")
(modify-syntax-entry! c-mode:syntax-table #\= ".")
(modify-syntax-entry! c-mode:syntax-table #\% ".")
(modify-syntax-entry! c-mode:syntax-table #\< ".")
(modify-syntax-entry! c-mode:syntax-table #\> ".")
(modify-syntax-entry! c-mode:syntax-table #\' "\"")

(define (c-mode:comment-locate start)
  (and (re-search-forward "/\\*[ \t]*" start (line-end start 0))
       (cons (re-match-start 0) (re-match-end 0))))

(define (c-mode:comment-indent start)
  (if (re-match-forward "^/\\*" start (line-end start 0))
      0
      (max (1+ (mark-column (horizontal-space-start start)))
	   (ref-variable comment-column))))

(define-variable c-auto-newline
  "Non-false means automatically newline before and after braces,
and after colons and semicolons, inserted in C code."
  false)

(define-command electric-c-brace
  "Insert character and correct line's indentation."
  "P"
  (lambda (argument)
    (let ((point (current-point)))
      (if (and (not argument)
	       (line-end? point)
	       (or (line-blank? point)
		   (and (ref-variable c-auto-newline)
			(begin
			  ((ref-command c-indent-line) false)
			  (insert-newline)
			  true))))
	  (begin
	    ((ref-command self-insert-command) false)
	    ((ref-command c-indent-line) false)
	    (if (ref-variable c-auto-newline)
		(begin
		  (insert-newline)
		  ((ref-command c-indent-line) false))))
	  ((ref-command self-insert-command) false))
      (if (eqv? #\} (current-command-char))
	  (mark-flash (backward-one-sexp (current-point)) 'RIGHT)))))

(define-command electric-c-semi
  "Insert character and correct line's indentation."
  "P"
  (lambda (argument)
    (if (ref-variable c-auto-newline)
	((ref-command electric-c-terminator) argument)
	((ref-command self-insert-command) argument))))

(define-command electric-c-terminator
  "Insert character and correct line's indentation."
  "P"
  (lambda (argument)
    (let ((point (current-point)))
      (if (and (not argument)
	       (line-end? point)
	       (not (let ((mark (indentation-end point)))
		      (or (char-match-forward #\# mark)
			  (let ((state (parse-partial-sexp mark point)))
			    (or (parse-state-in-string? state)
				(parse-state-in-comment? state)
				(parse-state-quoted? state)))))))
	  (begin
	    ((ref-command self-insert-command) false)
	    ((ref-command c-indent-line) false)
	    (if (and (ref-variable c-auto-newline)
		     (not (c-inside-parens? point)))
		(begin
		  (insert-newline)
		  ((ref-command c-indent-line) false))))
	  ((ref-command self-insert-command) argument)))))

(define-command mark-c-procedure
  "Put mark at end of C procedure, point at beginning."
  ()
  (lambda ()
    (push-current-mark! (current-point))
    (let ((end (forward-definition-end (current-point) 1 'LIMIT)))
      (push-current-mark! end)
      (set-current-point!
       (backward-paragraph (backward-definition-start end 1 'LIMIT)
			   1
			   'LIMIT)))))

(define-command c-indent-line
  "Indent current line as C code.
Argument means shift any additional lines of grouping
rigidly with this line."
  "P"
  (lambda (#!optional argument)
    (let ((argument (and (not (default-object? argument)) argument))
	  (start (line-start (current-point) 0)))
      (let ((indentation (c-indent-line:indentation start)))
	(let ((shift-amount (- indentation (mark-indentation start))))
	  (cond ((not (zero? shift-amount))
		 (change-indentation indentation start)
		 (if argument
		     (indent-code-rigidly start
					  (forward-sexp start 1 'ERROR)
					  shift-amount
					  "#")))
		((within-indentation? (current-point))
		 (set-current-point! (indentation-end (current-point))))))))))

(define-command c-indent-expression
  "Indent each line of the C grouping following point."
  ()
  (lambda ()
    (c-indent-expression (current-point))))