;;; -*-Scheme-*-
;;;
;;; $Id: javamode.scm,v 1.3 1999/01/02 06:11:34 cph Exp $
;;;
;;; Copyright (c) 1998-1999 Massachusetts Institute of Technology
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

;;;; Major Mode for Java Programs

;;; This isn't a very good mode for Java, but it is good enough for
;;; some purposes and it was quickly implemented.  The major flaw is
;;; that it indents the body of a definition, such as a method or
;;; nested class, exactly the same as a continued statement.  The only
;;; way to treat these cases differently is to do more sophisticated
;;; parsing that recognizes that the contexts are different.  This
;;; could be done using the keyparser, but that would be much more
;;; work than this was.

(declare (usual-integrations))

(define-major-mode java c "Java"
  "Major mode for editing Java code.
Expression and list commands understand all Java brackets.
Tab indents for Java code.
Comments begin with // and end at the end of line.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.

Variables controlling indentation style:
 c-tab-always-indent
    True means TAB in Java mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 c-auto-newline
    True means automatically newline before and after braces,
    and after colons and semicolons, inserted in Java code.
 c-indent-level
    Indentation of Java statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.
 c-continued-statement-offset
    Extra indentation given to a substatement, such as the
    then-clause of an if or body of a while.
 c-continued-brace-offset
    Extra indent for substatements that start with open-braces.
    This is in addition to c-continued-statement-offset.
 c-brace-offset
    Extra indentation for line if it starts with an open brace.
 c-brace-imaginary-offset
    An open brace following other text is treated as if it were
    this far to the right of the start of its line.
 c-argdecl-indent
    Indentation level of declarations of Java function arguments.
 c-label-offset
    Extra indentation for line that is a label, or case or default."
  (lambda (buffer)
    (local-set-variable! syntax-table java-mode:syntax-table buffer)
    (local-set-variable! syntax-ignore-comments-backwards #f buffer)
    (local-set-variable! comment-locator-hook java-comment-locate buffer)
    (local-set-variable! comment-indent-hook java-comment-indentation buffer)
    (local-set-variable! comment-start "// " buffer)
    (local-set-variable! comment-end "" buffer)
    (local-set-variable! c-continued-brace-offset -2 buffer)
    (event-distributor/invoke! (ref-variable java-mode-hook buffer) buffer)))

(define-command java-mode
  "Enter Java mode."
  ()
  (lambda () (set-current-major-mode! (ref-mode-object java))))

(define-variable java-mode-hook
  "An event distributor that is invoked when entering Java mode."
  (make-event-distributor))

(define java-mode:syntax-table
  (let ((syntax-table (make-syntax-table c-mode:syntax-table)))
    (modify-syntax-entry! syntax-table #\/ ". 1456")
    (modify-syntax-entry! syntax-table #\newline ">")
    syntax-table))

;;;; Syntax Description

(define (java-comment-locate mark)
  (let ((state (parse-partial-sexp mark (line-end mark 0))))
    (and (parse-state-in-comment? state)
	 (java-comment-match-start (parse-state-comment-start state))
	 (cons (re-match-start 0) (re-match-end 0)))))

(define (java-comment-match-start mark)
  (re-match-forward "/\\(/+\\|\\*+\\)[ \t]*" mark))

(define (java-comment-indentation mark)
  (let ((column
	 (cond ((re-match-forward "^/\\*" mark)
		0)
	       ((and (match-forward "//" mark)
		     (within-indentation? mark))
		(c-compute-indentation mark))
	       (else
		(ref-variable comment-column mark)))))
    (if (within-indentation? mark)
	column
	(max (+ (mark-column (horizontal-space-start mark)) 1)
	     column))))