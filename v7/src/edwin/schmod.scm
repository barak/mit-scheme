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

;;;; Scheme Mode

(declare (usual-integrations))
(using-syntax edwin-syntax-table

(define-command ("Scheme Mode" argument)
  "Enter Scheme mode."
  (set-current-major-mode! scheme-mode))

(define-major-mode "Scheme" "Fundamental"
  "Major mode specialized for editing Scheme code.
Tab indents the current line for Scheme.
\\[^R Indent Sexp] indents the next s-expression.

\\[^R Evaluate Previous Sexp into Buffer] evaluates the expression preceding point.
    All output is inserted into the buffer at point.
\\[^R Evaluate Sexp Typein] reads and evaluates an expression in the typein window.

The following evaluation commands keep a transcript of all output in
the buffer *Transcript*:

\\[^R Evaluate Definition] evaluates the current definition.
\\[^R Evaluate Buffer] evaluates the buffer.
\\[^R Evaluate Sexp] evaluates the expression following point.
\\[^R Evaluate Previous Sexp] evaluates the expression preceding point.
\\[^R Evaluate Region] evaluates the current region."

  (local-set-variable! "Syntax Table" scheme-mode:syntax-table)
  (local-set-variable! "Syntax Ignore Comments Backwards" #!FALSE)
  (local-set-variable! "Lisp Indent Hook"
		       (access standard-lisp-indent-hook
			       lisp-indentation-package))
  (local-set-variable! "Lisp Indent Methods" scheme-mode:indent-methods)
  (local-set-variable! "Comment Column" 40)
  (local-set-variable! "Comment Locator Hook"
		       (access lisp-comment-locate lisp-indentation-package))
  (local-set-variable! "Comment Indent Hook"
		       (access lisp-comment-indentation
			       lisp-indentation-package))
  (local-set-variable! "Comment Start" ";")
  (local-set-variable! "Comment End" "")
  (local-set-variable! "Paragraph Start" "^$")
  (local-set-variable! "Paragraph Separate" (ref-variable "Paragraph Start"))
  (local-set-variable! "Indent Line Procedure" ^r-indent-for-lisp-command)
  (if (ref-variable "Scheme Mode Hook") ((ref-variable "Scheme Mode Hook"))))

(define-variable "Scheme Mode Hook"
  "If not false, a thunk to call when entering Scheme mode."
  #!FALSE)

(define-key "Scheme" #\Rubout "^R Backward Delete Hacking Tabs")
(define-key "Scheme" #\) "^R Lisp Insert Paren")
(define-key "Scheme" #\M-O "^R Evaluate Buffer")
(define-key "Scheme" #\M-Z "^R Evaluate Definition")
(define-key "Scheme" #\C-M-= "^R Evaluate Previous Sexp into Buffer")
(define-key "Scheme" #\C-M-Q "^R Indent Sexp")
(define-key "Scheme" #\C-M-X "^R Evaluate Sexp")
(define-key "Scheme" #\C-M-Z "^R Evaluate Region")

;;;; Read Syntax

(define scheme-mode:syntax-table (make-syntax-table))

(modify-syntax-entries! scheme-mode:syntax-table #\C-\@ #\/ "_   ")
(modify-syntax-entries! scheme-mode:syntax-table #\: #\@ "_   ")
(modify-syntax-entries! scheme-mode:syntax-table #\[ #\` "_   ")
(modify-syntax-entries! scheme-mode:syntax-table #\{ #\Rubout "_   ")

(modify-syntax-entry! scheme-mode:syntax-table #\Space "    ")
(modify-syntax-entry! scheme-mode:syntax-table #\Tab "    ")
(modify-syntax-entry! scheme-mode:syntax-table #\Page "    ")
(modify-syntax-entry! scheme-mode:syntax-table #\[ "    ")
(modify-syntax-entry! scheme-mode:syntax-table #\] "    ")
(modify-syntax-entry! scheme-mode:syntax-table #\{ "    ")
(modify-syntax-entry! scheme-mode:syntax-table #\} "    ")
(modify-syntax-entry! scheme-mode:syntax-table #\| "  23")

(modify-syntax-entry! scheme-mode:syntax-table #\; "<   ")
(modify-syntax-entry! scheme-mode:syntax-table char:newline ">   ")

(modify-syntax-entry! scheme-mode:syntax-table #\' "'   ")
(modify-syntax-entry! scheme-mode:syntax-table #\` "'   ")
(modify-syntax-entry! scheme-mode:syntax-table #\, "'   ")
(modify-syntax-entry! scheme-mode:syntax-table #\@ "'   ")
(modify-syntax-entry! scheme-mode:syntax-table #\# "' 14")

(modify-syntax-entry! scheme-mode:syntax-table #\" "\"   ")
(modify-syntax-entry! scheme-mode:syntax-table #\\ "\\   ")
(modify-syntax-entry! scheme-mode:syntax-table #\( "()  ")
(modify-syntax-entry! scheme-mode:syntax-table #\) ")(  ")

;;;; Indentation

(define (scheme-mode:indent-let-method state indent-point normal-indent)
  ((access lisp-indent-special-form lisp-indentation-package)
   (let ((m (parse-state-containing-sexp state)))
     (let ((start (forward-to-sexp-start (forward-one-sexp (mark1+ m)
							   indent-point)
					 indent-point)))
       (if (and start
		(not (re-match-forward "\\s(" start)))
	   2
	   1)))
   state indent-point normal-indent))

(define scheme-mode:indent-methods (make-string-table))

(for-each (lambda (entry)
	    (string-table-put! scheme-mode:indent-methods
			       (symbol->string (car entry))
			       (cdr entry)))
	  `((CASE . 1)
	    (DO . 2)
	    (FLUID-LET . 1)
	    (IN-PACKAGE . 1)
	    (LAMBDA . 1)
	    (LET . ,scheme-mode:indent-let-method)
	    (LET* . 1)
	    (LET-SYNTAX . 1)
	    (LETREC . 1)
	    (LOCAL-DECLARE . 1)
	    (MACRO . 1)
	    (MAKE-ENVIRONMENT . 0)
	    (MAKE-PACKAGE . 2)
	    (NAMED-LAMBDA . 1)
	    (REC . 1)
	    (USING-SYNTAX . 1)

	    (CALL-WITH-INPUT-FILE . 1)
	    (WITH-INPUT-FROM-FILE . 1)
	    (WITH-INPUT-FROM-PORT . 1)
	    (WITH-INPUT-FROM-STRING . 1)
	    (CALL-WITH-OUTPUT-FILE . 1)
	    (WITH-OUTPUT-TO-FILE . 1)
	    (WITH-OUTPUT-TO-PORT . 1)
	    (WITH-OUTPUT-TO-STRING . 1)
	    (SYNTAX-TABLE-DEFINE . 2)
	    (LIST-TRANSFORM-POSITIVE . 1)
	    (LIST-TRANSFORM-NEGATIVE . 1)
	    (LIST-SEARCH-POSITIVE . 1)
	    (LIST-SEARCH-NEGATIVE . 1)

	    (ACCESS-COMPONENTS . 1)
	    (ASSIGNMENT-COMPONENTS . 1)
	    (COMBINATION-COMPONENTS . 1)
	    (COMMENT-COMPONENTS . 1)
	    (CONDITIONAL-COMPONENTS . 1)
	    (DISJUNCTION-COMPONENTS . 1)
	    (DECLARATION-COMPONENTS . 1)
	    (DEFINITION-COMPONENTS . 1)
	    (DELAY-COMPONENTS . 1)
	    (IN-PACKAGE-COMPONENTS . 1)
	    (LAMBDA-COMPONENTS . 1)
	    (LAMBDA-COMPONENTS* . 1)
	    (LAMBDA-COMPONENTS** . 1)
	    (OPEN-BLOCK-COMPONENTS . 1)
	    (PATHNAME-COMPONENTS . 1)
	    (PROCEDURE-COMPONENTS . 1)
	    (SEQUENCE-COMPONENTS . 1)
	    (UNASSIGNED?-COMPONENTS . 1)
	    (UNBOUND?-COMPONENTS . 1)
	    (VARIABLE-COMPONENTS . 1)
	    ))

;;; end USING-SYNTAX
)
;;; Edwin Variables:
;;; Scheme Environment: edwin-package
;;; Scheme Syntax Table: edwin-syntax-table
;;; End:
