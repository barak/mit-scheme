;;; -*-Scheme-*-
;;;
;;;	$Id: verilog.scm,v 1.4 1997/03/10 05:40:20 cph Exp $
;;;
;;;	Copyright (c) 1996-97 Massachusetts Institute of Technology
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

;;;; Major Mode for Verilog Programs

(declare (usual-integrations))

(define-command verilog-mode
  "Enter Verilog mode."
  ()
  (lambda () (set-current-major-mode! (ref-mode-object verilog))))

(define-major-mode verilog fundamental "Verilog"
  "Major mode specialized for editing Verilog code."
  (lambda (buffer)
    (local-set-variable! syntax-table verilog-mode:syntax-table buffer)
    (local-set-variable! syntax-ignore-comments-backwards #f buffer)
    (local-set-variable! comment-column 40 buffer)
    (local-set-variable! comment-locator-hook verilog-comment-locate buffer)
    (local-set-variable! comment-indent-hook verilog-comment-indentation
			 buffer)
    (local-set-variable! comment-start "// " buffer)
    (local-set-variable! comment-end "" buffer)
    (let ((paragraph-start
	   (string-append "^$\\|" (ref-variable page-delimiter buffer))))
      (local-set-variable! paragraph-start paragraph-start buffer)
      (local-set-variable! paragraph-separate paragraph-start buffer))
    (local-set-variable! indent-line-procedure
			 (ref-command keyparser-indent-line)
			 buffer)
    (local-set-variable! definition-start verilog-defun-start-regexp buffer)
    (local-set-variable! require-final-newline #t buffer)
    (local-set-variable! keyparser-description verilog-description buffer)
    (local-set-variable! keyword-table verilog-keyword-table buffer)
    (event-distributor/invoke! (ref-variable verilog-mode-hook buffer)
			       buffer)))

(define verilog-mode:syntax-table
  (let ((syntax-table (make-syntax-table)))
    (for-each (lambda (char) (modify-syntax-entry! syntax-table char "."))
	      (string->list "+-=%<>&|"))
    (modify-syntax-entry! syntax-table #\' "_")
    (modify-syntax-entry! syntax-table #\` ". p")
    (modify-syntax-entry! syntax-table #\# ". p")
    (modify-syntax-entry! syntax-table #\@ ". p")
    (modify-syntax-entry! syntax-table #\/ ". 1456")
    (modify-syntax-entry! syntax-table #\* ". 23")
    (modify-syntax-entry! syntax-table #\newline ">")
    syntax-table))

(define-key 'verilog #\linefeed 'reindent-then-newline-and-indent)
(define-key 'verilog #\rubout 'backward-delete-char-untabify)
(define-key 'verilog #\tab 'keyparser-indent-line)
(define-key 'verilog #\c-m-\\ 'keyparser-indent-region)
(define-key 'verilog #\) 'lisp-insert-paren)
(define-key 'verilog #\] 'lisp-insert-paren)
(define-key 'verilog #\} 'lisp-insert-paren)
(define-key 'verilog #\m-tab 'complete-keyword)

;;;; Syntax Description

(define (verilog-comment-locate mark)
  (let ((state (parse-partial-sexp mark (line-end mark 0))))
    (and (parse-state-in-comment? state)
	 (verilog-comment-match-start (parse-state-comment-start state))
	 (cons (re-match-start 0) (re-match-end 0)))))

(define (verilog-comment-match-start mark)
  (re-match-forward "/\\(/+\\|\\*+\\)[ \t]*" mark))

(define (verilog-comment-indentation mark)
  (let ((column
	 (cond ((or (and (line-start? mark)
			 (match-forward "/*" mark))
		    (match-forward "////" mark))
		0)
	       ((match-forward "///" mark)
		(keyparser-compute-indentation mark #t))
	       (else
		(ref-variable comment-column mark)))))
    (if (within-indentation? mark)
	column
	(max (+ (mark-column (horizontal-space-start mark)) 1)
	     column))))

(define verilog-defun-start-regexp
  (string-append
   "^"
   (regexp-group "module" "macromodule" "primitive" "parameter")
   (regexp-group "\\s " "$")))

(define verilog-keyword-table
  (alist->string-table
   (map list
	'("always" "and" "assign" "begin" "buf" "bufif0" "bufif1"
		   "case" "casex" "casez" "cmos" "deassign" "default"
		   "define" "defparam" "disable" "else" "end"
		   "endcase" "endfunction" "endmodule" "endprimitive"
		   "endtable" "endtask" "event" "for" "force"
		   "forever" "fork" "function" "if" "ifdef" "include"
		   "initial" "inout" "input" "integer" "join" "macromodule"
		   "module" "nand" "negedge" "nmos" "nor" "not"
		   "notif0" "notif1" "or" "output" "parameter" "pmos"
		   "posedge" "primitive" "pulldown" "pullup" "rcmos"
		   "real" "reg" "release" "repeat" "rnmos" "rpmos"
		   "rtran" "rtranif0" "rtranif1" "scalared" "supply0"
		   "supply1" "table" "task" "time" "tran" "tranif0"
		   "tranif1" "tri" "tri0" "tri1" "triand" "trior"
		   "trireg" "udp" "vectored" "wait" "wand" "while"
		   "wire" "wor" "xnor" "xor"))
   #f))

(define (parse-forward-past-semicolon start end)
  (let loop ((start start) (state #f))
    (let ((semi (char-search-forward #\; start end #f)))
      (and semi
	   (let ((state (parse-partial-sexp start semi #f #f state)))
	     (if (in-char-syntax-structure? state)
		 (loop semi state)
		 semi))))))

(define (parse-forward-past-block-tag start end)
  (if (re-match-forward "[ \t]*:[ \t]*" start end)
      (forward-one-sexp start end)
      start))

(define (parse-forward-noop start end)
  end
  start)

(define (continued-header-indent mark)
  (+ (mark-indentation mark)
     (ref-variable verilog-continued-header-offset mark)))

(define (continued-statement-indent mark)
  (+ (mark-indentation mark)
     (ref-variable verilog-continued-statement-offset mark)))

(define verilog-description
  (make-keyparser-description
   'FIND-STATEMENT-END
   parse-forward-past-semicolon
   'INDENT-CONTINUED-STATEMENT
   continued-statement-indent
   'INDENT-CONTINUED-COMMENT
   (lambda (mark)
     (mark-column (or (verilog-comment-match-start mark) mark)))))

(define-keyparser-statement-leader #\# verilog-description
  (re-compile-char #\# #f)
  forward-one-sexp)

(define-keyparser-statement-leader #\@ verilog-description
  (re-compile-char #\@ #f)
  forward-one-sexp)

(define (define-standard-keyword keyword end parse-header)
  (define-keyparser-pattern keyword verilog-description
    (make-keyparser-fragment 'KEYWORD
			     keyword
			     'PARSE-HEADER
			     parse-header
			     'INDENT-HEADER
			     continued-header-indent
			     'PARSE-BODY
			     keyparse-forward
			     'INDENT-BODY
			     continued-statement-indent)
    (and end
	 (make-keyparser-fragment 'KEYWORD
				  end
				  'PARSE-HEADER
				  parse-forward-noop
				  'INDENT-HEADER
				  continued-header-indent
				  'PARSE-BODY
				  #f
				  'INDENT-BODY
				  #f))))

(define-standard-keyword "always" #f
  parse-forward-noop)

(define-standard-keyword "begin" "end"
  parse-forward-past-block-tag)

(define-standard-keyword "case" "endcase"
  forward-one-sexp)

(define-standard-keyword "casex" "endcase"
  forward-one-sexp)

(define-standard-keyword "casez" "endcase"
  forward-one-sexp)

(define-standard-keyword "else" #f
  parse-forward-noop)

(define-standard-keyword "for" #f
  forward-one-sexp)

(define-standard-keyword "forever" #f
  parse-forward-noop)

(define-standard-keyword "fork" "join"
  parse-forward-past-block-tag)

(define-standard-keyword "function" "endfunction"
  parse-forward-past-semicolon)

(define-standard-keyword "if" #f
  forward-one-sexp)

(define-standard-keyword "initial" #f
  parse-forward-noop)

(define-standard-keyword "macromodule" "endmodule"
  parse-forward-past-semicolon)

(define-standard-keyword "module" "endmodule"
  parse-forward-past-semicolon)

(define-standard-keyword "primitive" "endprimitive"
  parse-forward-past-semicolon)

(define-standard-keyword "repeat" #f
  forward-one-sexp)

(define-standard-keyword "table" "endtable"
  parse-forward-noop)

(define-standard-keyword "task" "endtask"
  parse-forward-past-semicolon)

(define-standard-keyword "while" #f
  forward-one-sexp)