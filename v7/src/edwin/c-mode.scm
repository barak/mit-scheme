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

;;;; C Mode (from GNU Emacs)

(declare (usual-integrations))
(using-syntax edwin-syntax-table

(define-command ("C Mode" argument)
  "Enter C mode."
  (set-current-major-mode! c-mode))

(define-major-mode "C" "Fundamental"
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
    Extra indentation for line that is a label, or case or default."

  (local-set-variable! "Syntax Table" c-mode:syntax-table)
  (local-set-variable! "Syntax Ignore Comments Backwards" #!TRUE)
  (local-set-variable! "Paragraph Start" "^$")
  (local-set-variable! "Paragraph Separate" (ref-variable "Paragraph Start"))
  (local-set-variable! "Indent Line Procedure" c-indent-line-command)
  (local-set-variable! "Require Final Newline" #!TRUE)
  (local-set-variable! "Comment Locator Hook" c-mode:comment-locate)
  (local-set-variable! "Comment Indent Hook" c-mode:comment-indent)
  (local-set-variable! "Comment Start" "/* ")
  (local-set-variable! "Comment End" " */")
  (local-set-variable! "Comment Column" 32)
  (if (ref-variable "C Mode Hook") ((ref-variable "C Mode Hook"))))

(define-variable "C Mode Hook"
  "If not false, a thunk to call when entering C mode."
  #!FALSE)

(define-key "C" #\Linefeed "Reindent then Newline and Indent")
(define-key "C" #\{ "Electric C Brace")
(define-key "C" #\} "Electric C Brace")
(define-key "C" #\; "Electric C Semi")
(define-key "C" #\: "Electric C Terminator")
(define-key "C" #\C-M-H "Mark C Function")
(define-key "C" #\C-M-Q "C Indent Expression")
(define-key "C" #\Rubout "^R Backward Delete Hacking Tabs")
(define-key "C" #\Tab "C Indent Line")

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
	   (ref-variable "Comment Column"))))

(define-command ("Electric C Brace" argument)
  "Insert character and correct line's indentation."
  (let ((point (current-point)))
    (if (and (not argument)
	     (line-end? point)
	     (or (line-blank? point)
		 (and (ref-variable "C Auto Newline")
		      (begin (c-indent-line-command #!FALSE)
			     (insert-newline)
			     #!TRUE))))
	(begin (^r-insert-self-command #!FALSE)
	       (c-indent-line-command #!FALSE)
	       (if (ref-variable "C Auto Newline")
		   (begin (insert-newline)
			  (c-indent-line-command #!FALSE))))
	(^r-insert-self-command argument))))

(define-command ("Electric C Semi" argument)
  "Insert character and correct line's indentation."
  (if (ref-variable "C Auto Newline")
      (electric-c-terminator-command argument)
      (^r-insert-self-command argument)))

(define-command ("Electric C Terminator" argument)
  "Insert character and correct line's indentation."
  (let ((point (current-point)))
    (if (and (not argument)
	     (line-end? point)
	     (not (let ((mark (indentation-end point)))
		    (or (char-match-forward #\# mark)
			(let ((state (parse-partial-sexp mark point)))
			  (or (parse-state-in-string? state)
			      (parse-state-in-comment? state)
			      (parse-state-quoted? state)))))))
	(begin (^r-insert-self-command #!FALSE)
	       (c-indent-line-command #!FALSE)
	       (if (and (ref-variable "C Auto Newline")
			(not ((access inside-parens? c-indentation-package)
			      point)))
		   (begin (insert-newline)
			  (c-indent-line-command #!FALSE))))
	(^r-insert-self-command argument))))

(define-command ("Mark C Procedure" argument)
  "Put mark at end of C procedure, point at beginning."
  (push-current-mark! (current-point))
  (let ((end (forward-definition-end (current-point) 1 'LIMIT)))
    (push-current-mark! end)
    (set-current-point!
     (backward-paragraph (backward-definition-start end 1 'LIMIT) 1 'LIMIT))))

(define-command ("C Indent Line" argument)
  "Indent current line as C code.
Argument means shift any additional lines of grouping
rigidly with this line."
  (let ((start (line-start (current-point) 0)))
    (let ((indentation
	   ((access indent-line:indentation c-indentation-package) start)))
      (let ((shift-amount (- indentation (mark-indentation start))))
	(cond ((not (zero? shift-amount))
	       (change-indentation indentation start)
	       (if argument
		   ((access indent-code-rigidly lisp-indentation-package)
		    start (forward-sexp start 1 'ERROR) shift-amount "#")))
	      ((within-indentation? (current-point))
	       (set-current-point! (indentation-end (current-point)))))))))

(define-command ("C Indent Expression" argument)
  "Indent each line of the C grouping following point."
  ((access indent-expression c-indentation-package) (current-point)))

(define c-indentation-package
  (make-environment

(define (indent-line start)
  (maybe-change-indentation (indent-line:indentation start) start))

(define (indent-line:indentation start)
  (fluid-let (((ref-variable "Case Fold Search") #!FALSE))
    (let ((indentation (calculate-indentation start #!FALSE)))
      (cond ((not indentation) (mark-indentation start))
	    ((eq? indentation #!TRUE)
	     ;; Inside a comment; indentation of line depends on
	     ;; whether or not it starts with a *.
	     (mark-column
	      (let ((end (whitespace-start start (group-start start))))
		(let ((iend (indentation-end end)))
		  (let ((comstart (re-search-forward "/\\*[ \t]*" iend end)))
		    (cond ((not comstart) iend)
			  ((re-match-forward "[ \t]*\\*" start)
			   (mark1+ (re-match-start 0)))
			  (else comstart)))))))
	    ((char-match-forward #\# start) 0)
	    (else
	     (indent-line:adjust-indentation (horizontal-space-end start)
					     indentation))))))

(define (indent-line:adjust-indentation start indentation)
  (cond ((or (re-match-forward "case\\b" start)
	     (and (re-match-forward "[A-Za-z]" start)
		  (char-match-forward #\: (forward-one-sexp start))))
	 (max 1 (+ indentation (ref-variable "C Label Offset"))))
	((re-match-forward "else\\b" start)
	 (mark-indentation
	  (backward-to-start-of-if start
				   (backward-one-definition-start start))))
	((char-match-forward #\} start)
	 (- indentation (ref-variable "C Indent Level")))
	((char-match-forward #\{ start)
	 (+ indentation (ref-variable "C Brace Offset")))
	(else indentation)))

(define (calculate-indentation mark parse-start)
  (let ((gstart (group-start mark))
	(gend (group-end mark))
	(indent-point (line-start mark 0)))
    (define (find-outer-container start)
      (let ((state (parse-partial-sexp start indent-point 0)))
	(if (mark= (parse-state-location state) indent-point)
	    state
	    (find-outer-container (parse-state-location state)))))
    (let ((state
	   (find-outer-container (or parse-start
				     (backward-one-definition-start mark)
				     gstart))))
      (if (or (parse-state-in-string? state)
	      (parse-state-in-comment? state))
	  ;; Return boolean if line should not be changed.
	  (not (not (parse-state-in-comment? state)))
	  (let ((container (parse-state-containing-sexp state)))
	    (cond ((not container)
		   ;; Line is at top level.  Discriminate between
		   ;; procedure definition and other cases.
		   (if (re-match-forward "[ \t]*{" indent-point)
		       0
		       ;; May be data definition, or may be function
		       ;; argument declaration.  Indent like the
		       ;; previous top level line unless that ends
		       ;; in a closeparen without semicolon, in
		       ;; which case this line is the first argument
		       ;; decl.
		       (let ((mark
			      (backward-to-noncomment indent-point
						      (or parse-start
							  gstart))))
			 (if (char-match-backward #\) mark)
			     (ref-variable "C Argdecl Indent")
			     (mark-indentation mark)))))
		  ((char-match-forward #\{ container)
		   (calculate-indentation:statement indent-point container))
		  (else
		   ;; Line is expression, not statement: indent to just
		   ;; after the surrounding open.
		   (mark-column (mark1+ container)))))))))

(define (calculate-indentation:statement indent-point container)
  (let ((mark (backward-to-noncomment indent-point container)))
    (if (and mark
	     (re-match-forward "[^,;:{}]" (mark-1+ mark)))
	;; This line is continuation of preceding line's statement;
	;; indent C Continued Statement Offset more than the previous
	;; line of the statement.
	(+ (ref-variable "C Continued Statement Offset")
	   (mark-column (backward-to-start-of-continued-exp mark container)))
	(let ((mark (skip-comments&labels (mark1+ container) indent-point)))
	  (if (not mark)
	      ;; If this is first statement after open brace, indent
	      ;; it relative to line brace is on.  For open brace in
	      ;; column zero, don't let statement start there too.  If
	      ;; C Indent Level is zero, use C Brace Offset + C
	      ;; Continued Statement Offset instead.  For open-braces
	      ;; not the first thing in a line, add in C Brace
	      ;; Imaginary Offset.
	      (+ (if (and (line-start? container)
			  (zero? (ref-variable "C Indent Level")))
		     (+ (ref-variable "C Brace Offset")
			(ref-variable "C Continued Statement Offset"))
		     (ref-variable "C Indent Level"))
		 (+ (if (within-indentation? container)
			0
			(ref-variable "C Brace Imaginary Offset"))
		    (mark-indentation container)))
	      ;; Otherwise, indent under that first statement.
	      (mark-column mark))))))

(define (skip-comments&labels start end)
  (define (phi1 mark)
    (cond ((mark= mark end) #!FALSE)
	  ((char-match-forward #\# mark)
	   (phi2 (line-start mark 1)))
	  ((match-forward "/*" mark)
	   (phi2 (search-forward "*/" mark end)))
	  ((re-match-forward "case[ \t\n]\\|[a-zA-Z0-9_$]*:" mark)
	   (phi2 (char-search-forward #\: mark end)))
	  (else mark)))

  (define (phi2 mark)
    (and mark
	 (phi1 (whitespace-end mark end))))

  (phi1 (whitespace-end start end)))

(define (whitespace-start start end)
  (skip-chars-backward " \t\n" start end))

(define (whitespace-end start end)
  (skip-chars-forward " \t\n" start end))

(define (inside-parens? mark)
  (let ((container (backward-up-one-list mark)))
    (and container
	 (mark>= container (backward-one-definition-start mark))
	 (char-match-forward #\( container))))

(define (backward-to-noncomment start end)
  (define (loop start)
    (let ((mark (whitespace-start start end)))
      (if (match-backward "*/" mark)
	  (and (search-backward "/*" (re-match-start 0) end)
	       (loop (re-match-start 0)))
	  (let ((mark* (indentation-end mark)))
	    (cond ((not (char-match-forward #\# mark*)) mark)
		  ((mark<= mark* end) mark*)
		  (else (loop mark*)))))))
  (loop start))

(define (backward-to-start-of-continued-exp start end)
  (let ((mark
	 (line-start (if (char-match-backward #\) start)
			 (backward-one-sexp start)
			 start)
		     0)))
    (horizontal-space-end (if (mark<= mark end) (mark1+ end) mark))))

(define (backward-to-start-of-if start end)
  (define (phi2 mark if-level)
    (define (phi1 if-level)
      (if (zero? if-level)
	  mark
	  (phi2 (backward-sexp mark 1 'LIMIT) if-level)))
    (cond ((re-match-forward "else\\b" mark)
	   (phi1 (1+ if-level)))
	  ((re-match-forward "if\\b" mark)
	   (phi1 (-1+ if-level)))
	  ((mark>= mark end)
	   (phi1 if-level))
	  (else end)))
  (phi2 (backward-sexp start 1 'LIMIT) 1))

(define (indent-expression expression-start)
  (fluid-let (((ref-variable "Case Fold Search") #!FALSE))
    (let ((end (mark-left-inserting (line-start (forward-sexp expression-start
							      1 'ERROR)
						0))))
      (define (loop start indent-stack contain-stack last-depth)
	(next-line-start start #!FALSE
	  (lambda (start state)
	    (let ((depth-delta (- (parse-state-depth state) last-depth)))
	      (let ((indent-stack (adjust-stack depth-delta indent-stack))
		    (contain-stack (adjust-stack depth-delta contain-stack)))
		(if (not (car contain-stack))
		    (set-car! contain-stack
			      (or (parse-state-containing-sexp state)
				  (backward-one-sexp start))))
		(if (not (line-blank? start))
		    (indent-line start indent-stack contain-stack))
		(if (not (mark= start end))
		    (loop start indent-stack contain-stack
			  (parse-state-depth state))))))))

      (define (next-line-start start state receiver)
	(define (loop start state)
	  (let ((start* (line-start start 1)))
	    (let ((state*
		   (parse-partial-sexp start start* #!FALSE #!FALSE state)))
	      (if (and state (parse-state-in-comment? state))
		  (indent-line start))
	      (cond ((mark= start* end)
		     (receiver start* state*))
		    ((parse-state-in-comment? state*)
		     (if (not (and state (parse-state-in-comment? state)))
			 (if (re-search-forward "/\\*[ \t]*" start start*)
			     (c-mode:comment-indent (re-match-start 0))
			     (error "C-Indent-Expression: Missing comment")))
		     (loop start* state*))
		    ((parse-state-in-string? state*)
		     (loop start* state*))
		    (else
		     (receiver start* state*))))))
	(loop start state))

      (define (indent-line start indent-stack contain-stack)
	(let ((indentation
	       (indent-line:adjust-indentation
		start
		(if (car indent-stack)
		    (if (char-match-forward #\{ (car contain-stack))
			;; Line is at statement level.  Is it a new
			;; statement?  Is it an else?  Find last
			;; non-comment character before this line.
			(let ((mark
			       (backward-to-noncomment
				start expression-start)))
			  (cond ((not (memv (extract-previous-char mark)
					    '(#!FALSE #\. #\; #\} #\:)))
				 (+ (ref-variable
				     "C Continued Statement Offset")
				    (mark-column
				     (backward-to-start-of-continued-exp
				      mark (car contain-stack)))))
				((re-match-forward "else\\b" start)
				 (mark-indentation
				  (backward-to-start-of-if mark
							   expression-start)))
				(else (car indent-stack))))
			(car indent-stack))
		    (let ((indentation (calculate-indentation start #!FALSE)))
		      (set-car! indent-stack indentation)
		      indentation)))))
	  (if (not (or (= indentation (mark-indentation start))
		       (re-match-forward "[ \t]*#" start)))
	      (change-indentation indentation start))))

      (loop expression-start
	    (list #!FALSE)
	    (list expression-start)
	    0))))

(define (adjust-stack depth-delta indent-stack)
  (cond ((zero? depth-delta) indent-stack)
	((positive? depth-delta) (up-stack depth-delta indent-stack))
	(else (down-stack depth-delta indent-stack))))

(define (down-stack n stack)
  (if (= -1 n)
      (cdr stack)
      (down-stack (1+ n) (cdr stack))))

(define (up-stack n stack)
  (if (= 1 n)
      (cons #!FALSE stack)
      (up-stack (-1+ n) (cons #!FALSE stack))))

;;; end C-INDENTATION-PACKAGE
))

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: edwin-package
;;; Scheme Syntax Table: edwin-syntax-table
;;; End:
