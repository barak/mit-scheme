;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/cinden.scm,v 1.7 1992/01/09 23:10:12 markf Exp $
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

;;;; C Indentation (from GNU Emacs)

(declare (usual-integrations))

(define-variable c-indent-level
  "Indentation of C statements with respect to containing block."
  2
  exact-integer?)

(define-variable c-brace-imaginary-offset
  "Imagined indentation of a C open brace that actually follows a statement."
  0
  exact-integer?)

(define-variable c-brace-offset
  "Extra indentation for braces, compared with other text in same context."
  0
  exact-integer?)

(define-variable c-argdecl-indent
  "Indentation level of declarations of C function arguments."
  5
  exact-integer?)

(define-variable c-label-offset
  "Offset of C label lines and case statements relative to usual indentation."
  -2
  exact-integer?)

(define-variable c-continued-statement-offset
  "Extra indent for lines not starting new statements."
  2
  exact-integer?)

(define-variable c-continued-brace-offset
  "Extra indent for substatements that start with open-braces.
This is in addition to c-continued-statement-offset."
  0
  exact-integer?)

(define (c-indent-line start)
  (let* ((start (line-start start 0))
	 (old-indentation (mark-indentation start))
	 (new-indentation
	  (let ((indentation (calculate-indentation start false)))
	    (cond ((not indentation)
		   old-indentation)
		  ((eq? indentation true)
		   ;; Inside a comment.
		   (mark-column
		    (let* ((star?
			    (char-match-forward #\* (indentation-end start)))
			   (pend (whitespace-start start (group-start start)))
			   (pstart (indentation-end pend))
			   (comment-start
			    (and (mark< pstart pend)
				 (re-search-forward "/\\*[ \t]*" pstart pend
						    false))))
		      (cond ((not comment-start)
			     pstart)
			    (star?
			     (mark1+ (re-match-start 0)))
			    (else
			     comment-start)))))
		  ((char-match-forward #\# start)
		   0)
		  (else
		   (indent-line:adjust-indentation (horizontal-space-end start)
						   indentation))))))
    (if (not (= new-indentation old-indentation))
	(change-indentation new-indentation start))
    (- new-indentation old-indentation)))

(define (indent-line:adjust-indentation start indentation)
  (cond ((or (looking-at-keyword? "case" start)
	     (and (re-match-forward "[A-Za-z]" start)
		  (char-match-forward #\: (forward-sexp start 1 'LIMIT))))
	 (max 1 (+ indentation (ref-variable c-label-offset start))))
	((looking-at-keyword? "else" start)
	 (mark-indentation
	  (backward-to-start-of-if
	   start
	   (backward-definition-start start 1 'LIMIT))))
	((char-match-forward #\} start)
	 (- indentation (ref-variable c-indent-level start)))
	((char-match-forward #\{ start)
	 (+ indentation (ref-variable c-brace-offset start)))
	(else indentation)))

(define (calculate-indentation mark parse-start)
  (let ((indent-point (line-start mark 0)))
    (let ((state
	   (let loop
	       ((start
		 (or parse-start (backward-definition-start mark 1 'LIMIT))))
	     (let ((state (parse-partial-sexp start indent-point 0)))
	       (if (mark= (parse-state-location state) indent-point)
		   state
		   (loop (parse-state-location state)))))))
      (let ((container (parse-state-containing-sexp state)))
	(cond ((parse-state-in-string? state)
	       false)
	      ((parse-state-in-comment? state)
	       true)
	      ((not container)
	       (calculate-indentation:top-level indent-point
						(or parse-start
						    (group-start mark))))
	      ((char-match-forward #\{ container)
	       (calculate-indentation:statement indent-point container))
	      (else
	       ;; Line is expression, not statement: indent to just
	       ;; after the surrounding open.
	       (mark-column (mark1+ container))))))))

(define (calculate-indentation:top-level indent-point parse-start)
  (let ((gend (group-end indent-point)))
    ;; Discriminate between procedure definition and other cases.
    (if (re-match-forward "[ \t]*{" indent-point gend false)
	0
	;; May be data definition, or may be function argument
	;; declaration.  Indent like the previous top level line
	;; unless that ends in a closeparen without semicolon, in
	;; which case this line is the first argument decl.
	(let* ((mark (backward-to-noncomment indent-point parse-start))
	       ;; Look at previous line that's at column zero to
	       ;; determine whether we are in top-level decls or
	       ;; function's arg decls.  Set basic-indent accordingly.
	       (basic-indent
		(let ((m
		       (and mark
			    (re-search-backward "^[ \t\n\f#]"
						mark
						(group-start indent-point)
						false))))
		  (if (and m
			   (re-match-forward "\\sw\\|\\s_" m gend false)
			   (re-match-forward ".*(" m gend false)
			   (let ((m
				  (forward-sexp (mark-1+ (re-match-end 0)) 1)))
			     (and m
				  (mark< m indent-point)
				  (not (re-match-forward "[ \t\n]*[,;]"
							 m gend false)))))
		      (ref-variable c-argdecl-indent indent-point)
		      0))))
	  (if (or (not mark) (memv (extract-left-char mark) '(#F #\) #\; #\})))
	      basic-indent
	      (+ basic-indent
		 (ref-variable c-continued-statement-offset indent-point)))))))

(define (calculate-indentation:statement indent-point container)
  (let ((mark
	 (let loop ((start indent-point))
	   (let ((m (backward-to-noncomment start container)))
	     ;; Back up over label lines, since they don't affect
	     ;; whether our line is a continuation.
	     (case (and m (mark> m container) (extract-left-char m))
	       ((#\:)
		(if (let ((m (backward-to-noncomment (mark-1+ m) container)))
		      (and m
			   (mark> m container)
			   (re-match-forward "'\\|\\sw\\|\\s_" (mark-1+ m))))
		    (loop (line-start m 0))
		    m))
	       ((#\,)
		(loop
		 (line-start (backward-to-start-of-continued-exp m container)
			     0)))
	       (else m))))))
    (if (and mark (not (memv (extract-left-char mark) '(#F #\, #\; #\{ #\}))))
	;; This line is continuation of preceding line's statement;
	;; indent c-continued-statement-offset more than the previous
	;; line of the statement.
	(+ (ref-variable c-continued-statement-offset indent-point)
	   (mark-column (backward-to-start-of-continued-exp mark container))
	   (if (re-match-forward "[ \t]*{" indent-point)
	       (ref-variable c-continued-brace-offset indent-point)
	       0))
	(or (skip-comments&labels (mark1+ container) indent-point)
	    ;; If no previous statement, indent it relative to line
	    ;; brace is on.  For open brace in column zero, don't let
	    ;; statement start there too.  If c-indent-level is zero,
	    ;; use c-brace-offset + c-continued-statement-offset
	    ;; instead.  For open-braces not the first thing in a
	    ;; line, add in c-brace-imaginary-offset.
	    (+ (if (and (line-start? container)
			(= 0 (ref-variable c-indent-level indent-point)))
		   (+ (ref-variable c-brace-offset indent-point)
		      (ref-variable c-continued-statement-offset indent-point))
		   (ref-variable c-indent-level indent-point))
	       (let ((container (skip-chars-backward " \t" container)))
		 ;; If open brace is not the first non-white thing on
		 ;; the line, add the c-brace-imaginary-offset.
		 (+ (if (line-start? container)
			0
			(ref-variable c-brace-imaginary-offset indent-point))
		    (mark-indentation
		     ;; If the open brace is preceded by a
		     ;; parenthesized expression, move to the
		     ;; beginning of that; possibly a different line.
		     (if (eqv? #\) (extract-left-char container))
			 (backward-sexp container 1 'LIMIT)
			 container)))))))))

(define (skip-comments&labels start end)
  (let ((gend (group-end start)))
    ;; There was an bug here where COLON-LINE-END was initialized
    ;; to 0. I have initialized it to what seems to be the obvious
    ;; thing. It seems to work, but I really have no idea whether
    ;; it is correct. -markf
    (let loop ((mark start) (colon-line-end (line-end start 0)))
      (let ((mark (whitespace-end mark gend)))
	(cond ((mark>= mark end)
	       false)
	      ((char-match-forward #\# mark)
	       (loop (line-start mark 1 'LIMIT) colon-line-end))
	      ((match-forward "/*" mark)
	       (loop (or (search-forward "*/" mark gend) gend) colon-line-end))
	      ((re-match-forward "case[ \t\n].*:\\|[a-zA-Z0-9_$]*[ \t\n]*:"
				 mark gend false)
	       (loop (re-match-end 0) (line-end mark 0)))
	      ((mark> colon-line-end mark)
	       (- (mark-indentation mark)
		  (ref-variable c-label-offset mark)))
	      (else
	       (mark-column mark)))))))

(define (c-indent-expression expression-start)
  (let ((end
	 (mark-left-inserting-copy
	  (line-start (forward-sexp expression-start 1 'ERROR) 0))))
    (let loop
	((start expression-start)
	 (state false)
	 (indent-stack (list false))
	 (contain-stack (list expression-start))
	 (last-depth 0))
      (with-values (lambda () (c-indent-expression:parse-line start end state))
	(lambda (start state)
	  (let* ((depth (parse-state-depth state))
		 (depth-delta (- depth last-depth))
		 (indent-stack (adjust-stack depth-delta indent-stack))
		 (contain-stack (adjust-stack depth-delta contain-stack))
		 (indent-line
		  (lambda ()
		    (if (not (line-blank? start))
			(c-indent-expression:per-line
			 (skip-chars-forward " \t" start)
			 expression-start
			 indent-stack
			 contain-stack)))))
	    (if (not (car contain-stack))
		(set-car! contain-stack
			  (or (parse-state-containing-sexp state)
			      (backward-sexp start 1 'LIMIT))))
	    (if (mark= start end)
		(begin
		  (mark-temporary! end)
		  (indent-line))
		(begin
		  (indent-line)
		  (loop start state indent-stack contain-stack depth)))))))))

(define (c-indent-expression:parse-line start end state)
  (let loop ((start start) (state state))
    (let ((start* (line-start start 1)))
      (let ((state*
	     (parse-partial-sexp start start* false false state)))
	(if (and state (parse-state-in-comment? state))
	    (c-indent-line start))
	(cond ((mark= start* end)
	       (values start* state*))
	      ((parse-state-in-comment? state*)
	       (if (not (and state (parse-state-in-comment? state)))
		   (if (re-search-forward "/\\*[ \t]*" start start* false)
		       (c-mode:comment-indent (re-match-start 0))
		       (error "Missing comment")))
	       (loop start* state*))
	      ((parse-state-in-string? state*)
	       (loop start* state*))
	      (else
	       (values start* state*)))))))

(define (c-indent-expression:per-line start expression-start
				      indent-stack contain-stack)
  (let ((indentation
	 (indent-line:adjust-indentation
	  start
	  (cond ((not (car indent-stack))
		 (let ((indentation (calculate-indentation start false)))
		   (set-car! indent-stack indentation)
		   indentation))
		((not (char-match-forward #\{ (car contain-stack)))
		 (car indent-stack))
		(else
		 ;; Line is at statement level.  Is it a new
		 ;; statement?  Is it an else?  Find last non-comment
		 ;; character before this line.
		 (let ((mark (backward-to-noncomment start expression-start)))
		   (cond ((not (memv (extract-left-char mark)
				     '(#F #\, #\; #\} #\: #\{)))
			  (+ (ref-variable c-continued-statement-offset mark)
			     (mark-column
			      (backward-to-start-of-continued-exp
			       mark
			       (car contain-stack)))
			     (if (char-match-forward #\{ start)
				 (ref-variable c-continued-brace-offset mark)
				 0)))
			 ((looking-at-keyword? "else" start)
			  (mark-indentation
			   (backward-to-start-of-if mark expression-start)))
			 (else
			  (car indent-stack)))))))))
    (if (not (char-match-forward #\# start))
	(maybe-change-indentation indentation start))))

(define (whitespace-start start end)
  (skip-chars-backward " \t\n" start end))

(define (whitespace-end start end)
  (skip-chars-forward " \t\n" start end))

(define (looking-at-keyword? keyword start)
  (let ((end (line-end start 0)))
    (and (re-match-forward (string-append keyword "\\b") start end false)
	 (not (re-match-forward (string-append keyword "\\s_") start end
				false)))))

(define (backward-to-noncomment start end)
  (let loop ((start start))
    (let ((mark (whitespace-start start end)))
      (let ((m (match-backward "*/" mark)))
	(if m
	    (let ((m (search-backward "/*" m end)))
	      (and m
		   (loop m)))
	    (let ((mark* (indentation-end mark)))
	      (cond ((not (char-match-forward #\# mark*)) mark)
		    ((mark<= mark* end) mark*)
		    (else (loop mark*)))))))))

(define (backward-to-start-of-continued-exp start end)
  (let ((mark
	 (line-start (if (char-match-backward #\) start)
			 (backward-sexp start 1 'LIMIT)
			 start)
		     0)))
    (horizontal-space-end (if (mark<= mark end) (mark1+ end) mark))))

(define (backward-to-start-of-if start end)
  (let loop ((mark start) (if-level 1))
    (let ((mark (backward-sexp mark 1 'LIMIT)))
      (let ((adjust-level
	     (lambda (if-level)
	       (if (= if-level 0)
		   mark
		   (loop mark if-level)))))
	(cond ((looking-at-keyword? "else" mark)
	       (adjust-level (+ if-level 1)))
	      ((looking-at-keyword? "if" mark)
	       (adjust-level (- if-level 1)))
	      ((mark>= mark end)
	       (adjust-level if-level))
	      (else
	       end))))))

(define (adjust-stack depth-delta stack)
  (cond ((= depth-delta 0)
	 stack)
	((> depth-delta 0)
	 (let loop ((depth-delta depth-delta) (stack stack))
	   (if (= 1 depth-delta)
	       (cons false stack)
	       (loop (- depth-delta 1) (cons false stack)))))
	(else
	 (let loop ((depth-delta depth-delta) (stack stack))
	   (if (= -1 depth-delta)
	       (cdr stack)
	       (loop (+ depth-delta 1) (cdr stack)))))))