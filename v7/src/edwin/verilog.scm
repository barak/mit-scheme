;;; -*-Scheme-*-
;;;
;;;	$Id: verilog.scm,v 1.2 1997/03/04 06:43:51 cph Exp $
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
			 (ref-command verilog-indent-line)
			 buffer)
    (local-set-variable! require-final-newline #t buffer)
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

(define (verilog-comment-locate mark)
  (let ((state (parse-partial-sexp mark (line-end mark 0))))
    (and (parse-state-in-comment? state)
	 (and (verilog-comment-match-start state)
	      (cons (re-match-start 0) (re-match-end 0))))))

(define (verilog-comment-match-start state)
  (re-match-forward "/\\(/+\\|\\*+\\)[ \t]*"
		    (parse-state-comment-start state)))

(define (verilog-comment-indentation mark)
  (let ((column
	 (cond ((or (and (line-start? mark)
			 (match-forward "/*" mark))
		    (match-forward "////" mark))
		0)
	       ((match-forward "///" mark)
		(verilog-compute-indentation mark))
	       (else
		(ref-variable comment-column mark)))))
    (if (within-indentation? mark)
	column
	(max (+ (mark-column (horizontal-space-start mark)) 1)
	     column))))

(define-key 'verilog #\linefeed 'reindent-then-newline-and-indent)
(define-key 'verilog #\rubout 'backward-delete-char-untabify)
(define-key 'verilog #\tab 'verilog-indent-line)
(define-key 'verilog #\) 'lisp-insert-paren)
(define-key 'verilog #\] 'lisp-insert-paren)
(define-key 'verilog #\} 'lisp-insert-paren)
(define-key 'verilog #\m-tab 'verilog-complete-keyword)

;;;; Indentation

(define-command verilog-indent-line
  "Indent current line as Verilog code."
  "d"
  (lambda (#!optional mark)
    (let* ((mark (if (default-object? mark) (current-point) mark))
	   (point? (mark= (line-start mark 0) (line-start (current-point) 0))))
      (verilog-indent-line mark (verilog-compute-indentation mark))
      (if point?
	  (let ((point (current-point)))
	    (if (within-indentation? point)
		(set-current-point! (indentation-end point))))))))

(define-command verilog-indent-region
  "Indent current region as Verilog code."
  "r"
  (lambda (region)
    (let ((start
	   (mark-left-inserting-copy (line-start (region-start region) 0)))
	  (end (mark-left-inserting-copy (line-start (region-end region) 0))))
      (let ((dstart (or (find-verilog-defun-start start) (group-start start))))
	(let loop ((state (verilog-parse-initial dstart start)))
	  ;; The temporary marks in STATE are to the left of START, and
	  ;; thus are unaffected by insertion at START.
	  (if (not (line-blank? start))
	      (verilog-indent-line start
				   (verilog-compute-indentation-1 dstart
								  start
								  state)))
	  (let ((start* (line-start start 1 'LIMIT)))
	    (if (mark<= start* end)
		(let ((state (verilog-parse-partial start start* state)))
		  (move-mark-to! start start*)
		  (loop state))))))
      (mark-temporary! start)
      (mark-temporary! end))))

(define (verilog-indent-line mark indentation)
  (let ((indent-point (indentation-end mark)))
    (if (not (= indentation (mark-column indent-point)))
	(change-indentation indentation indent-point))))

(define (verilog-parse-initial dstart mark)
  (let ((mark (line-start mark 0))
	(state (initial-verilog-parse-state)))
    (if (mark= dstart mark)
	state
	(verilog-parse-partial dstart mark state))))

(define (find-verilog-defun-start mark)
  (let ((start (line-start mark 0))
	(regexp verilog-defun-start-regexp))
    (if (re-match-forward regexp start (group-end mark) #f)
	start
	(re-search-backward regexp start (group-start mark) #f))))

(define (verilog-compute-indentation mark)
  (let ((dstart (or (find-verilog-defun-start mark) (group-start mark)))
	(end (line-start mark 0)))
    (verilog-compute-indentation-1 dstart
				   end
				   (verilog-parse-initial dstart end))))

(define (verilog-compute-indentation-1 dstart lstart state)
  (let ((indent-point (indentation-end lstart))
	(sexp-state (verilog-parse-state/sexp-state state))
	(statement-state (verilog-parse-state/statement-state state)))
    (cond ((and sexp-state (in-char-syntax-structure? sexp-state))
	   (cond ((parse-state-in-comment? sexp-state)
		  (mark-column (if (verilog-comment-match-start sexp-state)
				   (re-match-end 0)
				   (parse-state-comment-start sexp-state))))
		 ((> (parse-state-depth sexp-state) 0)
		  (+ (mark-column
		      (or (parse-state-containing-sexp sexp-state)
			  (parse-state-containing-sexp
			   (parse-partial-sexp dstart lstart))))
		     1))
		 (else 0)))
	  ((verilog-parse-state/restart-point state)
	   => continued-statement-indent)
	  ((match-statement-ending indent-point statement-state)
	   (mark-indentation (caar statement-state)))
	  ((pair? statement-state)
	   (continued-statement-indent (caar statement-state)))
	  (else 0))))

(define (continued-statement-indent mark)
  (+ (mark-indentation mark)
     (ref-variable verilog-continued-statement-offset mark)))

(define-structure (verilog-parse-state (conc-name verilog-parse-state/))
  (sexp-state #f read-only #t)
  (statement-state #f read-only #t)
  (restart-point #f read-only #t))

(define (initial-verilog-parse-state)
  (make-verilog-parse-state #f '() #f))

(define (verilog-parse-partial start end state)
  (let ((ss
	 (parse-partial-sexp start end #f #f
			     (verilog-parse-state/sexp-state state))))
    (if (in-char-syntax-structure? ss)
	(make-verilog-parse-state
	 ss
	 (verilog-parse-state/statement-state state)
	 (or (verilog-parse-state/restart-point state) start))
	(call-with-values
	    (lambda ()
	      (parse-forward (or (verilog-parse-state/restart-point state)
				 start)
			     end
			     (verilog-parse-state/statement-state state)))
	  (lambda (statement-state restart-point)
	    (make-verilog-parse-state ss statement-state restart-point))))))

(define (parse-forward start end nesting)
  (let ((start (parse-forward-to-statement start end)))
    (cond ((not start)
	   (values nesting #f))
	  ((match-statement-ending start nesting)
	   (parse-keyword-statement-end start end nesting))
	  ((match-statement-keyword start)
	   => (lambda (record)
		(parse-keyword-statement start end record nesting)))
	  (else
	   (let ((semi (parse-forward-past-semicolon start end)))
	     (if semi
		 (finish-parsing-statement semi end nesting)
		 (values nesting start)))))))

(define (parse-keyword-statement start end record nesting)
  (let ((keyword (keyword-record/name record)))
    (let ((mark (skip-keyword start keyword)))
      (if (mark<= mark end)
	  (let ((mark (skip-keyword-noise mark end keyword))
		(nesting (cons (cons start record) nesting)))
	    (if mark
		(parse-forward mark end nesting)
		(values nesting #f)))
	  ;; This can't happen if END is at a line start.
	  (values nesting start)))))

(define (parse-keyword-statement-end start end nesting)
  (let ((mark (skip-keyword start (keyword-record/ending (cdar nesting)))))
    (if (mark<= mark end)
	(finish-parsing-statement mark end (cdr nesting))
	;; This can't happen if END is at a line start.
	(values nesting start))))

(define (finish-parsing-statement start end nesting)
  (parse-forward start
		 end
		 (let loop ((nesting nesting))
		   (if (and (pair? nesting)
			    (not (keyword-record/ending (cdar nesting))))
		       (loop (cdr nesting))
		       nesting))))

(define (parse-forward-to-statement start end)
  (let ((mark (forward-to-sexp-start start end)))
    (cond ((mark= mark end) #f)
	  ((and (mark< start mark)
		(memv (mark-left-char mark) '(#\# #\@)))
	   (let ((m (forward-one-sexp mark end)))
	     (and m
		  (parse-forward-to-statement m end))))
	  (else mark))))

(define (match-statement-keyword start)
  (let loop ((records verilog-statement-keywords))
    (and (not (null? records))
	 (if (re-match-forward (keyword-record/pattern (car records)) start)
	     (car records)
	     (loop (cdr records))))))

(define (match-statement-ending mark nesting)
  (let ((record (and (pair? nesting) (cdar nesting))))
    (and record
	 (keyword-record/ending-pattern record)
	 (re-match-forward (keyword-record/ending-pattern record) mark))))

(define (parse-forward-past-semicolon start end)
  (let loop ((start start) (state #f))
    (let ((semi (char-search-forward #\; start end #f)))
      (and semi
	   (let ((state (parse-partial-sexp start semi #f #f state)))
	     (if (in-char-syntax-structure? state)
		 (loop semi state)
		 semi))))))

(define (skip-keyword start keyword)
  (mark+ start (string-length keyword)))

(define (skip-keyword-noise start end keyword)
  (cond ((member keyword verilog-keywords:semicolon-delimited-header)
	 (parse-forward-past-semicolon start end))
	((or (member keyword verilog-keywords:paren-delimited-header)
	     (and (member keyword verilog-keywords:optional-block-tag)
		  (re-match-forward "[ \t]*:[ \t]*" start end #f)))
	 (forward-one-sexp start end))
	(else start)))

(define (in-char-syntax-structure? state)
  (or (parse-state-in-comment? state)
      (parse-state-in-string? state)
      (parse-state-quoted? state)
      (not (= (parse-state-depth state) 0))))

(define verilog-defun-start-regexp
  "^\\(module\\|macromodule\\|primitive\\|parameter\\)\\(\\s \\|$\\)")

(define verilog-keywords:semicolon-delimited-header
  '("function" "macromodule" "module" "primitive" "task"))

(define verilog-keywords:paren-delimited-header
  '("case" "casex" "casez" "for" "if" "repeat" "while"))

(define verilog-keywords:optional-block-tag
  '("begin" "fork"))

(define-structure (keyword-record (constructor make-keyword-record
					       (name ending))
				  (conc-name keyword-record/)
				  (print-procedure
				   (standard-unparser-method 'KEYWORD-RECORD
				     (lambda (record port)
				       (write-char #\space port)
				       (write-string
					(keyword-record/name record)
					port)))))
  (name #f read-only #t)
  (pattern (keyword->pattern name) read-only #t)
  (ending #f read-only #t)
  (ending-pattern (and ending (keyword->pattern ending)) read-only #t))

(define (keyword->pattern keyword)
  (re-compile-pattern
   (string-append keyword
		  (if (member keyword verilog-keywords:optional-block-tag)
		      "\\(\\s \\|$\\|:\\)"
		      "\\(\\s \\|$\\)"))
   #f))

(define verilog-statement-keywords
  (map (lambda (entry) (make-keyword-record (car entry) (cadr entry)))
       '(("always"	#f)
	 ("begin"	"end")
	 ("case"	"endcase")
	 ("casex"	"endcase")
	 ("casez"	"endcase")
	 ("else"	#f)
	 ("for"		#f)
	 ("forever"	#f)
	 ("fork"	"join")
	 ("function"	"endfunction")
	 ("if"		#f)
	 ("initial"	#f)
	 ("macromodule"	"endmodule")
	 ("module"	"endmodule")
	 ("primitive"	"endprimitive")
	 ("repeat"	#f)
	 ("table"	"endtable")
	 ("task"	"endtask")
	 ("while"	#f))))

;;;; Keyword Completion

(define-command verilog-complete-keyword
  "Perform completion on Verilog keyword preceding point."
  ()
  (lambda ()
    (let ((end 
	   (let ((point (current-point)))
	     (let ((end (group-end point)))
	       (or (re-match-forward "\\sw+" point end #f)
		   (and (mark< (group-start point) point)
			(re-match-forward "\\sw+" (mark-1+ point) end #f))
		   (editor-error "No keyword preceding point"))))))
      (let ((start (backward-word end 1 'LIMIT)))
	(standard-completion (extract-string start end)
	  (lambda (prefix if-unique if-not-unique if-not-found)
	    (string-table-complete verilog-keyword-table
				   prefix
				   if-unique
				   if-not-unique
				   if-not-found))
	  (lambda (completion)
	    (delete-string start end)
	    (insert-string completion start)))))))

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