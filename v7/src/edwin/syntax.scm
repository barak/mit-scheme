;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/syntax.scm,v 1.70 1991/04/23 06:44:12 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989 Massachusetts Institute of Technology
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

;;;; Syntax tables for Edwin

(declare (usual-integrations))

;;;; Syntax Tables

(define-variable syntax-table
  "The syntax-table used for word and list parsing.")

(define-variable syntax-ignore-comments-backwards
  "If true, ignore comments in backwards expression parsing.
This can be #T for comments that end in }, as in Pascal or C.
It should be #F for comments that end in Newline, as in Lisp;
this is because Newline occurs often when it doesn't indicate
a comment ending."
  false)

(define-structure (syntax-table (constructor %make-syntax-table)
				(conc-name syntax-table/))
  (entries false read-only true))

(define (guarantee-syntax-table syntax-table)
  (if (not (syntax-table? syntax-table))
      (error "not a syntax table" syntax-table))
  syntax-table)

(define (modify-syntax-entry! syntax-table char string)
  (guarantee-syntax-table syntax-table)
  (vector-set! (syntax-table/entries syntax-table)
	       (char->ascii char)
	       ((ucode-primitive string->syntax-entry) string))
  unspecific)

(define (modify-syntax-entries! syntax-table cl ch string)
  (guarantee-syntax-table syntax-table)
  (let ((entries (syntax-table/entries syntax-table))
	(ah (char->ascii ch))
	(entry ((ucode-primitive string->syntax-entry) string)))
    (let loop ((a (char->ascii cl)))
      (vector-set! entries a entry)
      (if (< a ah) (loop (1+ a))))))

(define make-syntax-table
  (let ((standard-syntax-table
	 (%make-syntax-table
	  (make-vector 256 ((ucode-primitive string->syntax-entry) "")))))
    (modify-syntax-entries! standard-syntax-table #\0 #\9 "w")
    (modify-syntax-entries! standard-syntax-table #\A #\Z "w")
    (modify-syntax-entries! standard-syntax-table #\a #\z "w")
    (modify-syntax-entry! standard-syntax-table #\$ "w")
    (modify-syntax-entry! standard-syntax-table #\% "w")
    (modify-syntax-entry! standard-syntax-table #\( "()")
    (modify-syntax-entry! standard-syntax-table #\) ")(")
    (modify-syntax-entry! standard-syntax-table #\[ "(]")
    (modify-syntax-entry! standard-syntax-table #\] ")[")
    (modify-syntax-entry! standard-syntax-table #\{ "(}")
    (modify-syntax-entry! standard-syntax-table #\} "){")
    (modify-syntax-entry! standard-syntax-table #\" "\"")
    (modify-syntax-entry! standard-syntax-table #\\ "\\")
    (for-each (lambda (char)
		(modify-syntax-entry! standard-syntax-table char "_"))
	      (string->list "_-+*/&|<>="))
    (for-each (lambda (char)
		(modify-syntax-entry! standard-syntax-table char "."))
	      (string->list ".,;:?!#@~^'`"))
    (lambda ()
      (%make-syntax-table
       (vector-copy (syntax-table/entries standard-syntax-table))))))

(define (initialize-syntax-table!)
  (set-variable! syntax-table (make-syntax-table)))

;;;; Word Parsing

(define forward-word)
(define backward-word)
(let ()

(define (%forward-word mark n limit?)
  (let ((group (mark-group mark))
	(end (mark-index (group-end mark))))
    (let loop ((start (mark-index mark)) (n n))
      (let ((m
	     ((ucode-primitive scan-word-forward)
	      (syntax-table/entries (ref-variable syntax-table))
	      group start end)))
	(cond ((not m) (limit-mark-motion limit? (make-mark group start)))
	      ((= n 1) (make-mark group m))
	      (else (loop m (-1+ n))))))))

(define (%backward-word mark n limit?)
  (let ((group (mark-group mark))
	(end (mark-index (group-start mark))))
    (let loop ((start (mark-index mark)) (n n))
      (let ((m
	     ((ucode-primitive scan-word-backward)
	      (syntax-table/entries (ref-variable syntax-table))
	      group start end)))
	(cond ((not m) (limit-mark-motion limit? (make-mark group start)))
	      ((= n 1) (make-mark group m))
	      (else (loop m (-1+ n))))))))

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
	(index
	 ((ucode-primitive scan-forward-to-word)
	  (syntax-table/entries (ref-variable syntax-table))
	  (mark-group mark)
	  (mark-index mark)
	  (mark-index (group-end mark)))))
    (if (not index)
	(limit-mark-motion limit? (group-end mark))
	(make-mark (mark-group mark) index))))

;;;; Lisp Parsing

(define-macro (default-end/forward start end)
  `(COND ((DEFAULT-OBJECT? ,end) (GROUP-END ,start))
	 ((NOT (MARK<= ,start ,end)) (ERROR "END less than START" ,end))
	 (ELSE ,end)))

(define-macro (default-end/backward start end)
  `(COND ((DEFAULT-OBJECT? ,end) (GROUP-START ,start))
	 ((NOT (MARK>= ,start ,end)) (ERROR "END greater than START" ,end))
	 (ELSE ,end)))

(define (backward-prefix-chars start #!optional end)
  (make-mark (mark-group start)
	     ((ucode-primitive scan-backward-prefix-chars)
	      (syntax-table/entries (ref-variable syntax-table))
	      (mark-group start)
	      (mark-index start)
	      (mark-index (default-end/backward start end)))))

(define (mark-right-char-quoted? mark)
  ((ucode-primitive quoted-char?)
   (syntax-table/entries (ref-variable syntax-table))
   (mark-group mark)
   (mark-index mark)
   (group-start-index (mark-group mark))))

(define (mark-left-char-quoted? mark)
  (if (not (group-start? mark))
      (mark-right-char-quoted? (mark-1+ mark))
      (error "Mark has no left char" mark)))

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
  (let ((index
	 ((ucode-primitive scan-list-forward)
	  (syntax-table/entries (ref-variable syntax-table))
	  (mark-group start)
	  (mark-index start)
	  (mark-index end)
	  depth
	  sexp?
	  true)))
    (and index (make-mark (mark-group start) index))))

(define (%backward-list start end depth sexp?)
  (let ((index
	 ((ucode-primitive scan-list-backward)
	  (syntax-table/entries (ref-variable syntax-table))
	  (mark-group start)
	  (mark-index start)
	  (mark-index end)
	  depth
	  sexp?
	  (ref-variable syntax-ignore-comments-backwards))))
    (and index (make-mark (mark-group start) index))))

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
      (error "Marks incorrectly related" start end))
  (let ((target-depth
	 (if (or (default-object? target-depth) (not target-depth))
	     -1000000
	     target-depth))
	(stop-before? (if (default-object? stop-before?) false stop-before?))
	(old-state (if (default-object? old-state) false old-state))
	(group (mark-group start)))
    (let ((state
	   ((ucode-primitive scan-sexps-forward)
	    (syntax-table/entries (ref-variable syntax-table))
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

(define (char->syntax-code char)
  ((ucode-primitive char->syntax-code)
   (syntax-table/entries (ref-variable syntax-table))
   char))

(define (substring-find-next-char-of-syntax string start end syntax)
  (let loop ((index start))
    (and (not (= index end))
	 (if (char=? syntax (char->syntax-code (string-ref string index)))
	     index
	     (loop (1+ index))))))

(define (substring-find-next-char-not-of-syntax string start end syntax)
  (let loop ((index start))
    (and (not (= index end))
	 (if (char=? syntax (char->syntax-code (string-ref string index)))
	     (loop (1+ index))
	     index))))

;;;; Definition Start/End

(define-variable definition-start
  "Regexp to match start of a definition."
  "^\\s("
  string?)

(define (definition-start? mark)
  (re-match-forward (ref-variable definition-start) mark))

(define (forward-one-definition-start mark)
  (and (re-search-forward (ref-variable definition-start)
			  (if (line-start? mark) (line-end mark 0) mark)
			  (group-end mark))
       (re-match-start 0)))

(define (backward-one-definition-start mark)
  (re-search-backward (ref-variable definition-start) mark (group-start mark)))

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