;;; -*-Scheme-*-
;;;
;;;	$Id: vhdl.scm,v 1.2 1997/03/08 00:16:14 cph Exp $
;;;
;;;	Copyright (c) 1997 Massachusetts Institute of Technology
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

;;;; Major Mode for VHDL Programs

(declare (usual-integrations))

(define-command vhdl-mode
  "Enter VHDL mode."
  ()
  (lambda () (set-current-major-mode! (ref-mode-object vhdl))))

(define-major-mode vhdl fundamental "VHDL"
  "Major mode specialized for editing VHDL code."
  (lambda (buffer)
    (local-set-variable! syntax-table vhdl-mode:syntax-table buffer)
    (local-set-variable! syntax-ignore-comments-backwards #f buffer)
    (local-set-variable! comment-column 40 buffer)
    (local-set-variable! comment-locator-hook vhdl-comment-locate buffer)
    (local-set-variable! comment-indent-hook vhdl-comment-indentation buffer)
    (local-set-variable! comment-start "-- " buffer)
    (local-set-variable! comment-end "" buffer)
    (let ((paragraph-start
	   (string-append "^$\\|" (ref-variable page-delimiter buffer))))
      (local-set-variable! paragraph-start paragraph-start buffer)
      (local-set-variable! paragraph-separate paragraph-start buffer))
    (local-set-variable! indent-line-procedure
			 (ref-command keyparser-indent-line)
			 buffer)
    (local-set-variable! definition-start vhdl-defun-start-regexp buffer)
    (local-set-variable! require-final-newline #t buffer)
    (local-set-variable! keyparser-description
			 vhdl-keyparser-description
			 buffer)
    (local-set-variable! keyword-table vhdl-keyword-table buffer)
    (event-distributor/invoke! (ref-variable vhdl-mode-hook buffer)
			       buffer)))

(define vhdl-mode:syntax-table
  (let ((syntax-table (make-syntax-table)))
    (for-each (lambda (char) (modify-syntax-entry! syntax-table char "_"))
	      (string->list "_.#+"))
    (for-each (lambda (char) (modify-syntax-entry! syntax-table char "."))
	      (string->list "*/&|<>=$%"))
    (modify-syntax-entry! syntax-table #\\ "\"")
    (modify-syntax-entry! syntax-table #\' "\"")
    (modify-syntax-entry! syntax-table #\- "_ 56")
    (modify-syntax-entry! syntax-table #\newline ">")
    syntax-table))

(define-key 'vhdl #\linefeed 'reindent-then-newline-and-indent)
(define-key 'vhdl #\rubout 'backward-delete-char-untabify)
(define-key 'vhdl #\tab 'keyparser-indent-line)
(define-key 'vhdl #\c-m-\\ 'keyparser-indent-region)
(define-key 'vhdl #\) 'lisp-insert-paren)
(define-key 'vhdl #\] 'lisp-insert-paren)
(define-key 'vhdl #\} 'lisp-insert-paren)
(define-key 'vhdl #\m-tab 'complete-keyword)

;;;; Syntax Description

(define (vhdl-comment-locate mark)
  (let ((state (parse-partial-sexp mark (line-end mark 0))))
    (and (parse-state-in-comment? state)
	 (vhdl-comment-match-start (parse-state-comment-start state))
	 (cons (re-match-start 0) (re-match-end 0)))))

(define (vhdl-comment-match-start mark)
  (re-match-forward "--+[ \t]*" mark))

(define (vhdl-comment-indentation mark)
  (let ((column
	 (cond ((match-forward "----" mark)
		0)
	       ((match-forward "---" mark)
		(keyparser-compute-indentation mark))
	       (else
		(ref-variable comment-column mark)))))
    (if (within-indentation? mark)
	column
	(max (+ (mark-column (horizontal-space-start mark)) 1)
	     column))))

(define vhdl-defun-start-regexp
  (string-append
   "^"
   (regexp-group "architecture" "configuration" "entity"
		 "library" "package" "use")
   (regexp-group "\\s " "$")))

(define vhdl-keyword-table
  (alist->string-table
   (map list
	'("abs" "access" "after" "alias" "all" "and" "architecture" "array"
	  "assert" "attribute" "begin" "block" "body" "buffer" "bus" "case"
	  "component" "configuration" "constant" "disconnect" "downto" "else"
	  "elsif" "end" "entity" "exit" "file" "for" "function" "generate"
	  "generic" "group" "guarded" "if" "impure" "in" "inertial" "inout"
	  "is" "label" "library" "linkage" "literal" "loop" "map" "mod" "nand"
	  "new" "next" "nor" "not" "null" "of" "on" "open" "or" "others" "out"
	  "package" "port" "postponed" "procedure" "process" "pure" "range"
	  "record" "register" "reject" "rem" "report" "return" "rol" "ror"
	  "select" "severity" "signal" "shared" "sla" "sll" "sra" "srl"
	  "subtype" "then" "to" "transport" "type" "unaffected" "units" "until"
	  "use" "variable" "wait" "when" "while" "with" "xnor" "xor"))
   #f))

(define (continued-header-indent mark)
  (+ (mark-indentation mark)
     (ref-variable vhdl-continued-header-offset mark)))

(define (continued-statement-indent mark)
  (+ (mark-indentation mark)
     (ref-variable vhdl-continued-statement-offset mark)))

(define comatch:identifier-start
  (comatch:general
   (lambda (start end)
     (and (re-match-forward "\\s \\|^" start end)
	  start))))

(define comatch:identifier-end
  (comatch:general
   (lambda (start end)
     (and (re-match-forward "\\s \\|$" start end)
	  start))))

(define comatch:skip-whitespace
  (comatch:general
   (lambda (start end)
     (let loop ((start start))
       (let ((start (skip-chars-forward " \t\f\n" start end)))
	 (if (match-forward "--" start end)
	     (let ((le (line-end start 0)))
	       (and (mark<= le end)
		    (loop le)))
	     start))))))

(define comatch:identifier
  (comatch:append comatch:skip-whitespace
		  (comatch:regexp "[a-zA-Z][a-zA-Z0-9_]*")
		  comatch:identifier-end))

(define (comatch:keyword keyword)
  (comatch:append comatch:skip-whitespace
		  (comatch:string keyword)
		  comatch:identifier-end))

(define comatch:list
  (comatch:append comatch:skip-whitespace
		  (comatch:and (comatch:char #\()
			       comatch:sexp)))

(define (match-for-loop mark)
  (and (comatch-apply comatch:for-header:control mark)
       mark))

(define (match-for-component mark)
  (and (comatch-apply comatch:for-header:component mark)
       mark))

(define (match-for-block mark)
  (and (not (or (comatch-apply comatch:for-header:control mark)
		(comatch-apply comatch:for-header:component mark)))
       mark))

(define comatch:for-header:control
  (comatch:append comatch:identifier
		  (comatch:keyword "in")))

(define comatch:for-header:component
  (comatch:append comatch:identifier
		  (comatch:*
		   (comatch:append comatch:skip-whitespace
				   (comatch:char #\,)
				   comatch:identifier))
		  comatch:skip-whitespace
		  (comatch:char #\:)))

(define (match-if-then mark)
  (and (eq? 'THEN (classify-if-header mark))
       mark))

(define (match-if-generate mark)
  (and (eq? 'GENERATE (classify-if-header mark))
       mark))

(define (classify-if-header mark)
  (let ((m (parse-forward-past-generate/then mark (group-end mark))))
    (and m
	 (let ((s (backward-one-sexp m)))
	   (and s
		(let ((e (forward-one-sexp s)))
		  (and e
		       (if (string-ci=? "then" (extract-string s e))
			   'THEN
			   'GENERATE))))))))

(define ((parse-forward-past search) start end)
  (let loop ((start start) (state #f))
    (let ((mark (search start end)))
      (and mark
	   (let ((state (parse-partial-sexp start mark #f #f state)))
	     (if (in-char-syntax-structure? state)
		 (loop mark state)
		 mark))))))

(define (parse-forward-past-char char)
  (parse-forward-past
   (lambda (start end) (char-search-forward char start end #f))))

(define parse-forward-past-semicolon
  (parse-forward-past-char #\;))

(define parse-forward-past-colon
  (parse-forward-past-char #\:))

(define (parse-forward-past-token token)
  (parse-forward-past
   (let ((regexp
	  (string-append (regexp-group "\\s " "^")
			 token
			 (regexp-group "\\s " "$"))))
     (lambda (start end)
       (re-search-forward regexp start end)))))

(define parse-forward-past-is
  (parse-forward-past-token "is"))

(define parse-forward-past-=>
  (parse-forward-past-token "=>"))

(define parse-forward-past-then
  (parse-forward-past-token "then"))

(define parse-forward-past-units
  (parse-forward-past-token "units"))

(define parse-forward-past-loop
  (parse-forward-past-token "loop"))

(define parse-forward-past-generate/loop
  (parse-forward-past-token (regexp-group "generate" "loop")))

(define parse-forward-past-generate/then
  (parse-forward-past-token (regexp-group "generate" "then")))

(define (parse-forward-noop start end)
  end
  start)

(define (parse-process-header start end)
  (comatch-apply comatch:process-header start end))

(define comatch:process-header
  (comatch:append (comatch:? comatch:list)
		  (comatch:? (comatch:keyword "is"))))

(define (parse-postponed-header start end)
  (comatch-apply comatch:postponed-header start end))

(define comatch:postponed-header
  (comatch:append (comatch:keyword "process")
		  comatch:process-header))

(define (parse-component-header start end)
  (comatch-apply comatch:component-header start end))

(define comatch:component-header
  (comatch:append comatch:identifier
		  (comatch:? (comatch:keyword "is"))))

(define vhdl-keyparser-description
  (make-keyparser-description
   'PATTERNS
   (let ((standard-keyword
	  (lambda (keyword match-header parse-header . rest)
	    (apply make-keyparser-fragment
		   'KEYWORD
		   keyword
		   'MATCH-HEADER
		   match-header
		   'PARSE-HEADER
		   parse-header
		   'INDENT-HEADER
		   continued-header-indent
		   'PARSE-BODY
		   keyparse-forward
		   'INDENT-BODY
		   continued-statement-indent
		   rest))))
     (let ((begin-frag (standard-keyword "begin" #f parse-forward-noop))
	   (end-frag (standard-keyword "end" #f parse-forward-past-semicolon)))
       (append
	(map (lambda (entry)
	       (cons* (standard-keyword (car entry) (cadr entry) (caddr entry))
		      end-frag
		      (cdddr entry)))
	     `(("architecture" #f ,parse-forward-past-is ,begin-frag)
	       ("block" #f ,parse-process-header ,begin-frag)
	       ("case" #f ,parse-forward-past-is)
	       ("component" #f ,parse-component-header ,begin-frag)
	       ("configuration" #f ,parse-forward-past-is)
	       ("entity" #f ,parse-forward-past-is ,begin-frag)
	       ("for" ,match-for-block ,parse-forward-noop)
	       ("for" ,match-for-component ,parse-forward-past-colon)
	       ("for" ,match-for-loop ,parse-forward-past-generate/loop)
	       ("function" #f ,parse-forward-past-is ,begin-frag)
	       ("pure" #f ,parse-forward-past-is ,begin-frag)
	       ("impure" #f ,parse-forward-past-is ,begin-frag)
	       ("if" ,match-if-then
		     ,parse-forward-past-then
		     ,(standard-keyword "elsif" #f parse-forward-past-then)
		     ,(standard-keyword "else" #f parse-forward-noop))
	       ("if" ,match-if-generate ,parse-forward-past-generate/then)
	       ("package" #f ,parse-forward-past-is)
	       ("procedure" #f ,parse-forward-past-is ,begin-frag)
	       ("process" #f ,parse-process-header ,begin-frag)
	       ("postponed" #f ,parse-postponed-header ,begin-frag)
	       ("range" #f ,parse-forward-past-units)
	       ("record" #f ,parse-forward-noop)
	       ("while" #f ,parse-forward-past-loop)))
	(list
	 (let ((when (standard-keyword "when" #f parse-forward-past-=>)))
	   (list when
		 (standard-keyword "end" #f parse-forward-past-semicolon
				   'POP-CONTAINER 1)
		 when))))))
	

   'STATEMENT-LEADERS
   `((,(re-compile-pattern "[a-zA-Z0-9_]+\\s *:" #f) . ,parse-forward-noop))
   
   'FIND-STATEMENT-END
   parse-forward-past-semicolon

   'INDENT-CONTINUED-STATEMENT
   continued-statement-indent
   
   'INDENT-CONTINUED-COMMENT
   (lambda (mark)
     (mark-column (or (vhdl-comment-match-start mark) mark)))))