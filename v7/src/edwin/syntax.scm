;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/syntax.scm,v 1.74 1991/10/25 00:03:18 cph Exp $
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

;;;; Syntax Tables

(declare (usual-integrations))

(define-structure (syntax-table (constructor %make-syntax-table)
				(conc-name syntax-table/))
  (entries false read-only true))

(define (modify-syntax-entry! syntax-table char string)
  (if (not (syntax-table? syntax-table))
      (error:wrong-type-argument syntax-table
				 "syntax table"
				 'MODIFY-SYNTAX-ENTRY!))
  (vector-set! (syntax-table/entries syntax-table)
	       (char->ascii char)
	       ((ucode-primitive string->syntax-entry) string)))

(define (modify-syntax-entries! syntax-table cl ch string)
  (if (not (syntax-table? syntax-table))
      (error:wrong-type-argument syntax-table
				 "syntax table"
				 'MODIFY-SYNTAX-ENTRIES!))
  (let ((entries (syntax-table/entries syntax-table))
	(ah (char->ascii ch))
	(entry ((ucode-primitive string->syntax-entry) string)))
    (do ((a (char->ascii cl) (+ a 1)))
	((> a ah) unspecific)
      (vector-set! entries a entry))))

(define standard-syntax-table
  (let ((table
	 (%make-syntax-table
	  (make-vector 256 ((ucode-primitive string->syntax-entry) "")))))
    (modify-syntax-entries! table #\0 #\9 "w")
    (modify-syntax-entries! table #\A #\Z "w")
    (modify-syntax-entries! table #\a #\z "w")
    (modify-syntax-entry! table #\$ "w")
    (modify-syntax-entry! table #\% "w")
    (modify-syntax-entry! table #\( "()")
    (modify-syntax-entry! table #\) ")(")
    (modify-syntax-entry! table #\[ "(]")
    (modify-syntax-entry! table #\] ")[")
    (modify-syntax-entry! table #\{ "(}")
    (modify-syntax-entry! table #\} "){")
    (modify-syntax-entry! table #\" "\"")
    (modify-syntax-entry! table #\\ "\\")
    (for-each (lambda (char)
		(modify-syntax-entry! table char "_"))
	      (string->list "_-+*/&|<>="))
    (for-each (lambda (char)
		(modify-syntax-entry! table char "."))
	      (string->list ".,;:?!#@~^'`"))
    table))

(define (make-syntax-table)
  (%make-syntax-table
   (vector-copy (syntax-table/entries standard-syntax-table))))

(define (char->syntax-code syntax-table char)
  ((ucode-primitive char->syntax-code) (syntax-table/entries syntax-table)
				       char))

(define (substring-find-next-char-of-syntax string start end
					    syntax-table syntax)
  (let loop ((index start))
    (and (< index end)
	 (if (char=? syntax
		     (char->syntax-code syntax-table
					(string-ref string index)))
	     index
	     (loop (+ index 1))))))

(define (substring-find-next-char-not-of-syntax string start end
						syntax-table syntax)
  (let loop ((index start))
    (and (< index end)
	 (if (char=? syntax
		     (char->syntax-code syntax-table
					(string-ref string index)))
	     (loop (+ index 1))
	     index))))

;;;; Word Parsing

(define-variable syntax-table
  "The syntax-table used for word and list parsing."
  (make-syntax-table))

(define-variable syntax-ignore-comments-backwards
  "If true, ignore comments in backwards expression parsing.
This can be #T for comments that end in }, as in Pascal or C.
It should be #F for comments that end in Newline, as in Lisp;
this is because Newline occurs often when it doesn't indicate
a comment ending."
  false
  boolean?)

(define forward-word)
(define backward-word)
(let ()

(define (%forward-word mark n limit?)
  (let ((group (mark-group mark)))
    (let ((end (group-end-index group))
	  (entries (syntax-table/entries (group-syntax-table group))))
      (let loop ((start (mark-index mark)) (n n))
	(let ((m
	       ((ucode-primitive scan-word-forward) entries group start end)))
	  (cond ((not m) (limit-mark-motion limit? (make-mark group start)))
		((= n 1) (make-mark group m))
		(else (loop m (-1+ n)))))))))

(define (%backward-word mark n limit?)
  (let ((group (mark-group mark)))
    (let ((end (group-start-index group))
	  (entries (syntax-table/entries (group-syntax-table group))))
      (let loop ((start (mark-index mark)) (n n))
	(let ((m
	       ((ucode-primitive scan-word-backward) entries group start end)))
	  (cond ((not m) (limit-mark-motion limit? (make-mark group start)))
		((= n 1) (make-mark group m))
		(else (loop m (-1+ n)))))))))

(set! forward-word
(named-lambda (forward-word mark n #!optional limit?)
  (let ((limit? (and (not (default-object? limit?)) limit?)))
    (cond ((positive? n) (%forward-word mark n limit?))
	  ((negative? n) (%backward-word mark (- n) limit?))
	  (else mark)))))

(set! backward-word
(named-lambda (backward-word mark n #!optional limit?)
  (let ((limit? (and (not (default-object? limit?)) limit?)))
    (cond ((positive? n) (%backward-word mark n limit?))
	  ((negative? n) (%forward-word mark (- n) limit?))
	  (else mark)))))

)

(define (forward-to-word mark #!optional limit?)
  (let ((limit? (and (not (default-object? limit?)) limit?))
	(group (mark-group mark)))
    (let ((index
	   ((ucode-primitive scan-forward-to-word)
	    (syntax-table/entries (group-syntax-table group))
	    group
	    (mark-index mark)
	    (group-end-index group))))
      (if (not index)
	  (limit-mark-motion limit? (group-end mark))
	  (make-mark group index)))))

;;;; Lisp Parsing

(define-macro (default-end/forward start end)
  `(COND ((DEFAULT-OBJECT? ,end)
	  (GROUP-END ,start))
	 ((MARK<= ,start ,end)
	  ,end)
	 (ELSE
	  (ERROR "Marks incorrectly related:" ,start ,end))))

(define-macro (default-end/backward start end)
  `(COND ((DEFAULT-OBJECT? ,end)
	  (GROUP-START ,start))
	 ((MARK>= ,start ,end)
	  ,end)
	 (ELSE
	  (ERROR "Marks incorrectly related:" ,start ,end))))

(define (forward-prefix-chars start #!optional end)
  (let ((group (mark-group start))
	(end (default-end/forward start end)))
    (make-mark group
	       ((ucode-primitive scan-forward-prefix-chars 4)
		(syntax-table/entries (group-syntax-table group))
		group
		(mark-index start)
		(mark-index end)))))

(define (backward-prefix-chars start #!optional end)
  (let ((group (mark-group start))
	(end (default-end/backward start end)))
    (make-mark group
	       ((ucode-primitive scan-backward-prefix-chars 4)
		(syntax-table/entries (group-syntax-table group))
		group
		(mark-index start)
		(mark-index end)))))

(define (mark-right-char-quoted? mark)
  (let ((group (mark-group mark)))
    ((ucode-primitive quoted-char?)
     (syntax-table/entries (group-syntax-table group))
     group
     (mark-index mark)
     (group-start-index group))))

(define (mark-left-char-quoted? mark)
  (if (group-start? mark)
      (error "Mark has no left char" mark))
  (mark-right-char-quoted? (mark-1+ mark)))

(define-structure (parse-state (type vector))
  (depth false read-only true)
  (in-string? false read-only true)	;#F or ASCII delimiter.
  (in-comment? false read-only true)	;#F or 1 or 2.
  (quoted? false read-only true)
  (last-sexp false)
  (containing-sexp false)
  (location false))

(define (forward-to-sexp-start mark end)
  (parse-state-location (parse-partial-sexp mark end 0 true)))

(define (parse-partial-sexp start end
			    #!optional target-depth stop-before? old-state)
  (if (not (mark<= start end))
      (error "Marks incorrectly related:" start end))
  (let ((target-depth
	 (if (or (default-object? target-depth) (not target-depth))
	     -1000000
	     target-depth))
	(stop-before? (if (default-object? stop-before?) false stop-before?))
	(old-state (if (default-object? old-state) false old-state))
	(group (mark-group start)))
    (let ((state
	   ((ucode-primitive scan-sexps-forward)
	    (syntax-table/entries (group-syntax-table group))
	    group
	    (mark-index start)
	    (mark-index end)
	    target-depth stop-before? old-state)))
      ;; Convert the returned indices to marks.
      (if (parse-state-last-sexp state)
	  (set-parse-state-last-sexp! 
	   state 
	   (make-mark group (parse-state-last-sexp state))))
      (if (parse-state-containing-sexp state)
	  (set-parse-state-containing-sexp! 
	   state
	   (make-mark group (parse-state-containing-sexp state))))
      (set-parse-state-location! state
				 (make-mark group
					    (parse-state-location state)))
      state)))

(define forward-one-sexp)
(define backward-one-sexp)
(define forward-one-list)
(define backward-one-list)
(define forward-up-one-list)
(define backward-up-one-list)
(define forward-down-one-list)
(define backward-down-one-list)
(let ()

(define (%forward-list start end depth sexp?)
  (let ((group (mark-group start)))
    (let ((index
	   ((ucode-primitive scan-list-forward)
	    (syntax-table/entries (group-syntax-table group))
	    group
	    (mark-index start)
	    (mark-index end)
	    depth
	    sexp?
	    true)))
      (and index (make-mark group index)))))

(define (%backward-list start end depth sexp?)
  (let ((group (mark-group start)))
    (let ((index
	   ((ucode-primitive scan-list-backward)
	    (syntax-table/entries (group-syntax-table group))
	    group
	    (mark-index start)
	    (mark-index end)
	    depth
	    sexp?
	    (group-local-ref
	     group
	     (ref-variable-object syntax-ignore-comments-backwards)))))
      (and index (make-mark group index)))))

(set! forward-one-sexp
(named-lambda (forward-one-sexp start #!optional end)
  (%forward-list start (default-end/forward start end) 0 true)))

(set! backward-one-sexp
(named-lambda (backward-one-sexp start #!optional end)
  (let ((end (default-end/backward start end)))
    (let ((mark (%backward-list start end 0 true)))
      (and mark (backward-prefix-chars mark end))))))

(set! forward-one-list
(named-lambda (forward-one-list start #!optional end)
  (%forward-list start (default-end/forward start end) 0 false)))

(set! backward-one-list
(named-lambda (backward-one-list start #!optional end)
  (%backward-list start (default-end/backward start end) 0 false)))

(set! forward-up-one-list
(named-lambda (forward-up-one-list start #!optional end)
  (%forward-list start (default-end/forward start end) 1 false)))

(set! backward-up-one-list
(named-lambda (backward-up-one-list start #!optional end)
  (%backward-list start (default-end/backward start end) 1 false)))

(set! forward-down-one-list
(named-lambda (forward-down-one-list start #!optional end)
  (%forward-list start (default-end/forward start end) -1 false)))

(set! backward-down-one-list
(named-lambda (backward-down-one-list start #!optional end)
  (%backward-list start (default-end/backward start end) -1 false)))

)

;;;; Definition Start/End

(define-variable definition-start
  "Regexp to match start of a definition."
  "^\\s("
  string?)

(define (definition-start? mark)
  (re-match-forward
   (mark-local-ref mark (ref-variable-object definition-start))
   mark))

(define (forward-one-definition-start mark)
  (and (re-search-forward
	(mark-local-ref mark (ref-variable-object definition-start))
	(if (line-start? mark) (line-end mark 0) mark)
	(group-end mark))
       (re-match-start 0)))

(define (backward-one-definition-start mark)
  (re-search-backward
   (mark-local-ref mark (ref-variable-object definition-start))
   mark
   (group-start mark)))

(define (forward-one-definition-end mark)
  (define (loop start)
    (let ((end (forward-one-list start)))
      (and end
	   (let ((end*
		  (let ((end (horizontal-space-end end)))
		    (if (re-match-forward "[;\n]" end)
			(line-start end 1 'LIMIT)
			end))))
	     (if (mark> end* mark)
		 end*
		 (loop (forward-one-definition-start end)))))))
  (and (not (group-end? mark))
       (loop 
	(or (backward-one-definition-start (mark1+ mark))
	    (forward-one-definition-start (group-start mark))))))

(define (backward-one-definition-end mark)
  (let ((start (backward-one-definition-start mark)))
    (and start
	 (let ((end (forward-one-definition-end start)))
	   (and end
		(if (mark< end mark)
		    end
		    (let ((start (backward-one-definition-start start)))
		      (and start (forward-one-definition-end start)))))))))