;;; -*-Scheme-*-
;;;
;;; $Id: schmod.scm,v 1.49 2001/03/21 19:25:37 cph Exp $
;;;
;;; Copyright (c) 1986, 1989-2001 Massachusetts Institute of Technology
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
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

;;;; Scheme Mode

(declare (usual-integrations))

(define-command scheme-mode
  "Enter Scheme mode."
  ()
  (lambda () (set-current-major-mode! (ref-mode-object scheme))))

(define-major-mode scheme fundamental "Scheme"
  "Major mode specialized for editing Scheme code.
\\[lisp-indent-line] indents the current line for Scheme.
\\[indent-sexp] indents the next s-expression.
\\[scheme-complete-variable] completes the variable preceding point.
\\[show-parameter-list] shows the parameters of the call surrounding point.

The following commands evaluate Scheme expressions:

\\[eval-expression] reads and evaluates an expression in minibuffer.
\\[eval-last-sexp] evaluates the expression preceding point.
\\[eval-defun] evaluates the current definition.
\\[eval-current-buffer] evaluates the buffer.
\\[eval-region] evaluates the current region.

\\{scheme}"
  (lambda (buffer)
    (local-set-variable! syntax-table scheme-mode:syntax-table buffer)
    (local-set-variable! syntax-ignore-comments-backwards #f buffer)
    (local-set-variable! lisp-indent-hook standard-lisp-indent-hook buffer)
    (local-set-variable! lisp-indent-methods scheme-mode:indent-methods buffer)
    (local-set-variable! lisp-indent-regexps scheme-mode:indent-regexps buffer)
    (local-set-variable! comment-column 40 buffer)
    (local-set-variable! comment-locator-hook lisp-comment-locate buffer)
    (local-set-variable! comment-indent-hook lisp-comment-indentation buffer)
    (local-set-variable! comment-start ";" buffer)
    (local-set-variable! comment-end "" buffer)
    (standard-alternate-paragraph-style! buffer)
    (local-set-variable! paragraph-ignore-fill-prefix #t buffer)
    (local-set-variable! indent-line-procedure
			 (ref-command lisp-indent-line)
			 buffer)
    (local-set-variable! mode-line-process
			 '(RUN-LIGHT (": " RUN-LIGHT) "")
			 buffer)
    (local-set-variable! local-abbrev-table
			 (ref-variable scheme-mode-abbrev-table buffer)
			 buffer)
    (event-distributor/invoke! (ref-variable scheme-mode-hook buffer) buffer)))

(define-variable scheme-mode-abbrev-table
  "Mode-specific abbrev table for Scheme code.")
(define-abbrev-table 'scheme-mode-abbrev-table '())

(define-variable scheme-mode-hook
  "An event distributor that is invoked when entering Scheme mode."
  (make-event-distributor))

(define-key 'scheme #\rubout 'backward-delete-char-untabify)
(define-key 'scheme #\tab 'lisp-indent-line)
(define-key 'scheme #\) 'lisp-insert-paren)
(define-key 'scheme #\m-A 'show-parameter-list)
(define-key 'scheme #\m-g 'undefined)
(define-key 'scheme #\m-o 'eval-current-buffer)
(define-key 'scheme #\m-q 'undefined)
(define-key 'scheme #\m-s 'step-defun)
(define-key 'scheme #\m-z 'eval-defun)
(define-key 'scheme #\c-m-q 'indent-sexp)
(define-key 'scheme #\c-m-z 'eval-region)
(define-key 'scheme #\m-tab 'scheme-complete-variable)
(define-key 'scheme '(#\c-c #\c-c) 'eval-abort-top-level)

;;;; Read Syntax

(define scheme-mode:syntax-table (make-syntax-table))

(modify-syntax-entries! scheme-mode:syntax-table #\nul #\/ "_")
(modify-syntax-entries! scheme-mode:syntax-table #\: #\@ "_")
(modify-syntax-entries! scheme-mode:syntax-table #\[ #\` "_")
(modify-syntax-entries! scheme-mode:syntax-table #\{ #\rubout "_")

(modify-syntax-entry! scheme-mode:syntax-table #\space " ")
(modify-syntax-entry! scheme-mode:syntax-table #\tab " ")
(modify-syntax-entry! scheme-mode:syntax-table #\page " ")
(modify-syntax-entry! scheme-mode:syntax-table #\[ "(]")
(modify-syntax-entry! scheme-mode:syntax-table #\] ")[")
(modify-syntax-entry! scheme-mode:syntax-table #\{ "(}")
(modify-syntax-entry! scheme-mode:syntax-table #\} "){")
(modify-syntax-entry! scheme-mode:syntax-table #\| "  23")

(modify-syntax-entry! scheme-mode:syntax-table #\; "< ")
(modify-syntax-entry! scheme-mode:syntax-table #\newline "> ")

(modify-syntax-entry! scheme-mode:syntax-table #\' "  p")
(modify-syntax-entry! scheme-mode:syntax-table #\` "  p")
(modify-syntax-entry! scheme-mode:syntax-table #\, "_ p")
(modify-syntax-entry! scheme-mode:syntax-table #\@ "_ p")
(modify-syntax-entry! scheme-mode:syntax-table #\# "_ p14")

(modify-syntax-entry! scheme-mode:syntax-table #\" "\" ")
(modify-syntax-entry! scheme-mode:syntax-table #\\ "\\ ")
(modify-syntax-entry! scheme-mode:syntax-table #\( "()")
(modify-syntax-entry! scheme-mode:syntax-table #\) ")(")

;;;; Indentation

(define (scheme-mode:indent-let-method state indent-point normal-indent)
  (lisp-indent-special-form
   (if (let ((start
	      (forward-to-sexp-start
	       (forward-one-sexp (mark1+ (parse-state-containing-sexp state))
				 indent-point)
	       indent-point)))
	 (and start
	      (not (re-match-forward "\\s(" start))))
       2
       1)
   state indent-point normal-indent))

(define scheme-mode:indent-methods
  (alist->string-table
   (map (lambda (entry) (cons (symbol->string (car entry)) (cdr entry)))
	`((BEGIN . 0)
	  (CASE . 1)
	  (DELAY . 0)
	  (DO . 2)
	  (LAMBDA . 1)
	  (LET . ,scheme-mode:indent-let-method)
	  (LET* . 1)
	  (LETREC . 1)

	  (CALL-WITH-INPUT-FILE . 1)
	  (WITH-INPUT-FROM-FILE . 1)
	  (CALL-WITH-OUTPUT-FILE . 1)
	  (WITH-OUTPUT-TO-FILE . 1)

	  ;; Remainder are MIT Scheme specific.

	  (FLUID-LET . 1)
	  (IN-PACKAGE . 1)
	  (LET-SYNTAX . 1)
	  (LOCAL-DECLARE . 1)
	  (MACRO . 1)
	  (MAKE-ENVIRONMENT . 0)
	  (NAMED-LAMBDA . 1)
	  (USING-SYNTAX . 1)

	  (CALL-WITH-APPEND-FILE . 1)
	  (CALL-WITH-BINARY-APPEND-FILE . 1)
	  (CALL-WITH-BINARY-INPUT-FILE . 1)
	  (CALL-WITH-BINARY-OUTPUT-FILE . 1)
	  (WITH-INPUT-FROM-PORT . 1)
	  (WITH-INPUT-FROM-STRING . 1)
	  (WITH-OUTPUT-TO-PORT . 1)
	  (WITH-OUTPUT-TO-STRING . 0)
	  (CALL-WITH-VALUES . 1)
	  (WITH-VALUES . 1)
	  (WITHIN-CONTINUATION . 1)

	  (MAKE-CONDITION-TYPE . 3)
	  (WITH-RESTART . 4)
	  (WITH-SIMPLE-RESTART . 2)
	  (BIND-CONDITION-HANDLER . 2)
	  (LIST-TRANSFORM-POSITIVE . 1)
	  (LIST-TRANSFORM-NEGATIVE . 1)
	  (LIST-SEARCH-POSITIVE . 1)
	  (LIST-SEARCH-NEGATIVE . 1)
	  (SYNTAX-TABLE-DEFINE . 2)
	  (FOR-ALL? . 1)
	  (THERE-EXISTS? . 1)))))

(define scheme-mode:indent-regexps
  `(SCHEME-MODE:INDENT-REGEXPS
    ("DEF" . DEFINITION)
    ("WITH-" . 1)))

;;;; Completion

(define (scheme-complete-symbol bound-only?)
  (let ((end
	 (let ((point (current-point)))
	   (or (re-match-forward "\\(\\sw\\|\\s_\\)+"
				 point (group-end point) #f)
	       (let ((start (group-start point)))
		 (if (not (and (mark< start point)
			       (re-match-forward "\\sw\\|\\s_"
						 (mark-1+ point)
						 point
						 #f)))
		     (editor-error "No symbol preceding point"))
		 point)))))
    (let ((start (forward-prefix-chars (backward-sexp end 1 'LIMIT) end)))
      (standard-completion (extract-string start end)
	(lambda (prefix if-unique if-not-unique if-not-found)
	  (let ((completions
		 (let ((completions
			(obarray-completions
			 (if *parser-canonicalize-symbols?*
			     (string-downcase prefix)
			     prefix))))
		   (if (not bound-only?)
		       completions
		       (let ((environment (evaluation-environment #f)))
			 (list-transform-positive completions
			   (lambda (name)
			     (environment-bound? environment name))))))))
	    (cond ((null? completions)
		   (if-not-found))
		  ((null? (cdr completions))
		   (if-unique (system-pair-car (car completions))))
		  (else
		   (let ((completions (map system-pair-car completions)))
		     (if-not-unique
		      (string-greatest-common-prefix completions)
		      (lambda () (sort completions string<=?))))))))
	(lambda (completion)
	  (delete-string start end)
	  (insert-string completion start))))))

(define (obarray-completions prefix)
  (let ((obarray (fixed-objects-item 'OBARRAY)))
    (let ((prefix-length (string-length prefix))
	  (obarray-length (vector-length obarray)))
      (let index-loop ((i 0))
	(if (fix:< i obarray-length)
	    (let bucket-loop ((symbols (vector-ref obarray i)))
	      (if (null? symbols)
		  (index-loop (fix:+ i 1))
		  (let ((string (system-pair-car (car symbols))))
		    (if (and (fix:<= prefix-length (string-length string))
			     (let loop ((index 0))
			       (or (fix:= index prefix-length)
				   (and (char=? (string-ref prefix index)
						(string-ref string index))
					(loop (fix:+ index 1))))))
			(cons (car symbols) (bucket-loop (cdr symbols)))
			(bucket-loop (cdr symbols))))))
	    '())))))

(define-command scheme-complete-symbol
  "Perform completion on Scheme symbol preceding point.
That symbol is compared against the symbols that exist
and any additional characters determined by what is there
are inserted.
With prefix arg, only symbols that are bound in the buffer's
environment are considered."
  "P"
  scheme-complete-symbol)

(define-command scheme-complete-variable
  "Perform completion on Scheme variable name preceding point.
That name is compared against the bound variables in the evaluation environment
and any additional characters determined by what is there are inserted.
With prefix arg, the evaluation environment is ignored and all symbols
are considered for completion."
  "P"
  (lambda (all-symbols?) (scheme-complete-symbol (not all-symbols?))))

(define-command show-parameter-list
  "Show the parameter list of the procedure in the call surrounding point.
With prefix argument, the parameter list is inserted at point.
Otherwise, it is shown in the echo area."
  "d\nP"
  (lambda (point insert?)
    (let ((start
	   (forward-down-list (backward-up-list point 1 'ERROR) 1 'ERROR))
	  (buffer (mark-buffer point)))
      (let ((end (forward-sexp start 1 'ERROR)))
	(let ((procedure
	       (let ((environment (evaluation-environment buffer)))
		 (extended-scode-eval
		  (syntax (with-input-from-region (make-region start end) read)
			  (evaluation-syntax-table buffer environment))
		  environment))))
	  (if (procedure? procedure)
	      (let ((argl (procedure-argl procedure)))
		(if (and insert?
			 (let loop ((argl argl))
			   (or (symbol? argl)
			       (null? argl)
			       (and (pair? argl)
				    (or (symbol? (car argl))
					(eq? (car argl) #!optional)
					(eq? (car argl) #!rest)
					(eq? (car argl) #!aux))
				    (loop (cdr argl))))))
		    (let ((point (mark-left-inserting-copy point)))
		      (let loop ((argl argl))
			(cond ((pair? argl)
			       (insert-char #\space point)
			       (insert-string (if (symbol? (car argl))
						  (symbol-name (car argl))
						  (write-to-string (car argl)))
					      point)
			       (loop (cdr argl)))
			      ((symbol? argl)
			       (insert-string " . " point)
			       (insert-string (symbol-name argl) point)))))
		    (fluid-let ((*unparse-uninterned-symbols-by-name?* #t))
		      (message argl))))
	      (editor-error "Expression does not evaluate to a procedure: "
			    (extract-string start end))))))))

(define (procedure-argl proc)
  "Returns the arg list of PROC.
Grumbles if PROC is an undocumented primitive."
  (if (primitive-procedure? proc)
      (let ((doc-string (primitive-procedure-documentation proc)))
	(if doc-string
	    (let ((newline (string-find-next-char doc-string #\newline)))
	      (if newline
		  (string-head doc-string newline)
		  doc-string))
	    (string-append (write-to-string proc)
			   " has no documentation string.")))
      (let ((code (procedure-lambda proc)))
	(if code
	    (lambda-components* code
	      (lambda (name required optional rest body)
		name body
		(append required
			(if (null? optional) '() `(#!OPTIONAL ,@optional))
			(if rest `(#!REST ,rest) '()))))
	    "No debugging information available for this procedure."))))