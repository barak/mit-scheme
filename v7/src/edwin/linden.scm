;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/linden.scm,v 1.122 1991/10/13 01:50:08 arthur Exp $
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

;;;; Lisp Indentation

(declare (usual-integrations))

(define-variable lisp-indent-offset
  "If not false, the number of extra columns to indent a subform."
  false)

(define-variable lisp-indent-hook
  "If not false, a procedure for modifying lisp indentation."
  false)

(define-variable lisp-indent-methods
  "String table identifying special forms for lisp indentation.")

(define-variable lisp-body-indent
  "Number of extra columns to indent the body of a special form."
  2)

;;; CALCULATE-LISP-INDENTATION returns either an integer, which is the
;;; column to indent to, or a pair.  In the latter case this means
;;; that subsequent forms in the same expression may not be indented
;;; the same way; so the car is the indentation, and the cdr is a mark
;;; pointing at the beginning of the containing expression.  Typically
;;; this is passed back in as PARSE-START to speed up the indentation
;;; of many forms at once.

(define (calculate-lisp-indentation mark #!optional parse-start)
  (find-outer-container (if (default-object? parse-start)
			    (or (backward-one-definition-start mark)
				(group-start mark))
			    parse-start)
			(line-start mark 0)))

(define (find-outer-container start indent-point)
  (let ((state (parse-partial-sexp start indent-point 0)))
    (if (mark= (parse-state-location state) indent-point)
	(find-inner-container state false false indent-point)
	(find-outer-container (parse-state-location state) indent-point))))

(define (find-inner-container state container last-sexp indent-point)
  (if (<= (parse-state-depth state) 0)
      (simple-indent state container last-sexp indent-point)
      (let ((container (parse-state-containing-sexp state))
	    (last-sexp (parse-state-last-sexp state)))
	(let ((after-opener (mark1+ container)))
	  (if (and last-sexp (mark> last-sexp after-opener))
	      (let ((peek (parse-partial-sexp last-sexp indent-point 0)))
		(if (not (parse-state-containing-sexp peek))
		    (simple-indent state container last-sexp indent-point)
		    (find-inner-container peek container last-sexp
					  indent-point)))
	      (simple-indent state container last-sexp indent-point))))))

(define (simple-indent state container last-sexp indent-point)
  (cond ((parse-state-in-string? state)
	 (mark-column (horizontal-space-end indent-point)))
	((and (integer? (ref-variable lisp-indent-offset)) container)
	 (+ (ref-variable lisp-indent-offset) (mark-column container)))
	((positive? (parse-state-depth state))
	 (if (not last-sexp)
	     (mark-column (mark1+ container))
	     (normal-indent state container last-sexp indent-point)))
	(else
	 (mark-column (parse-state-location state)))))

;;; The following are true when the indent hook is called:
;;;
;;; * CONTAINER < NORMAL-INDENT <= LAST-SEXP < INDENT-POINT
;;; * Since INDENT-POINT is a line start, LAST-SEXP is on a
;;;   line previous to that line.
;;; * NORMAL-INDENT is at the start of an expression.

(define (normal-indent state container last-sexp indent-point)
  (let ((first-sexp (forward-to-sexp-start (mark1+ container) last-sexp)))
    (let ((normal-indent
	   (if (mark> (line-end container 0) last-sexp)
	       ;; CONTAINER and LAST-SEXP are on same line.
	       ;; If FIRST-SEXP = LAST-SEXP, indent under that, else
	       ;; indent under the second expression on that line.
	       (if (mark= first-sexp last-sexp)
		   last-sexp
		   (forward-to-sexp-start (forward-one-sexp first-sexp)
					  last-sexp))
	       ;; LAST-SEXP is on subsequent line -- indent under the
	       ;; first expression on that line.
	       (forward-to-sexp-start (line-start last-sexp 0) last-sexp))))
      (if (char=? #\(
		  (char->syntax-code (ref-variable syntax-table)
				     (mark-right-char first-sexp)))
	  ;; The first expression is a list -- don't bother to call
	  ;; the indent hook.
	  (mark-column (backward-prefix-chars normal-indent))
	  (let ((normal-indent (backward-prefix-chars normal-indent)))
	    (or (and (ref-variable lisp-indent-hook)
		     ((ref-variable lisp-indent-hook)
		      state indent-point normal-indent))
		(mark-column normal-indent)))))))

;;;; Indent Hook

;;; Look at the first expression in the containing expression, and if
;;; it is an atom, look it up in the Lisp Indent Methods table.  Three
;;; types of entry are recognized:
;;;
;;; 'DEFINITION means treat this form as a definition.
;;; <n> means treat this form as a special form.
;;; Otherwise, the entry must be a procedure, which is called.

(define (standard-lisp-indent-hook state indent-point normal-indent)
  (let ((first-sexp
	 (forward-to-sexp-start (mark1+ (parse-state-containing-sexp state))
				indent-point)))
    (and (let ((syntax
		(char->syntax-code (ref-variable syntax-table)
				   (mark-right-char first-sexp))))
	   (or (char=? #\w syntax)
	       (char=? #\_ syntax)))
	 (let ((name (extract-string first-sexp
				     (forward-one-sexp first-sexp))))
	   (let ((method
		  (string-table-get (ref-variable lisp-indent-methods)
				    name)))
	     (cond ((or (eq? method 'DEFINITION)
			(and (not method)
			     (<= 3 (string-length name))
			     (substring-ci=? "DEF" 0 3 name 0 3)))
		    (lisp-indent-definition state indent-point normal-indent))
		   ((and (not method)
			 (<= 5 (string-length name))
			 (substring-ci=? "WITH-" 0 5 name 0 5))
		    (lisp-indent-special-form 1 state indent-point
					      normal-indent))
		   ((integer? method)
		    (lisp-indent-special-form method state indent-point
					      normal-indent))
		   (method
		    (method state indent-point normal-indent))
                   (else
		    false)))))))

;;; Indent the first subform in a definition at the body indent.
;;; Indent subsequent subforms normally.

(define (lisp-indent-definition state indent-point normal-indent)
  indent-point normal-indent		;ignore
  (let ((container (parse-state-containing-sexp state)))
    (and (mark> (line-end container 0) (parse-state-last-sexp state))
	 (+ (ref-variable lisp-body-indent) (mark-column container)))))

;;; Indent the first N subforms normally, but then indent the
;;; remaining forms at the body-indent.  If this is one of the first
;;; N, a cons is returned, the cdr of which is CONTAINING-SEXP.  This
;;; is to speed up indentation of successive forms.

(define (lisp-indent-special-form n state indent-point normal-indent)
  (if (negative? n) (error "Special form indent hook negative" n))
  (let ((container (parse-state-containing-sexp state)))
    (let ((body-indent
	   (+ (mark-column container) (ref-variable lisp-body-indent)))
	  (normal-indent (mark-column normal-indent)))
      (let loop ((count n) (mark (mark1+ container)))
	(let ((mark
	       (let ((mark (forward-one-sexp mark indent-point)))
		 (and mark
		      (forward-to-sexp-start mark indent-point)))))
	  (cond ((and mark (mark< mark indent-point))
		 (loop (-1+ count) mark))
		((positive? count)
		 (cons (+ body-indent (ref-variable lisp-body-indent))
		       (mark-permanent! container)))
		((and (zero? count)
		      (or (zero? n)
			  (<= body-indent normal-indent)))
		 body-indent)
		(else
		 normal-indent)))))))

;;;; Indent Line

(define (lisp-indent-line whole-sexp?)
  (let ((start (indentation-end (current-point))))
    (if (not (match-forward ";;;" start))
	(let ((indentation
	       (let ((indent (calculate-lisp-indentation start)))
		 (if (pair? indent)
		     (car indent)
		     indent))))
	  (let ((shift-amount (- indentation (mark-column start))))
	    (cond ((not (zero? shift-amount))
		   (if whole-sexp?
		       (mark-permanent! start))
		   (change-indentation indentation start)
		   (if whole-sexp?
		       (indent-code-rigidly start
					    (forward-sexp start 1 'ERROR)
					    shift-amount
					    false)))
		  ((within-indentation? (current-point))
		   (set-current-point! start))))))))

(define (indent-code-rigidly start end shift-amount nochange-regexp)
  (let ((end (mark-left-inserting end)))
    (let loop ((start start) (state false))
      (let ((start* (line-start start 1 'LIMIT)))
	(if (mark< start* end)
	    (let ((start start*)
		  (state (parse-partial-sexp start start* false false state)))
	      (if (not (or (parse-state-in-string? state)
			   (parse-state-in-comment? state)
			   (and nochange-regexp
				(re-match-forward nochange-regexp start))))
		  (let ((start (horizontal-space-end start)))
		    (cond ((line-end? start)
			   (delete-horizontal-space start))
			  ((not (match-forward ";;;" start))
			   (change-indentation (max 0
						    (+ (mark-column start)
						       shift-amount))
					       start)))))
	      (loop start state)))))))

;;;; Indent Comment

(define (lisp-comment-locate mark)
  (and (re-search-forward ";+[ \t]*" mark (line-end mark 0))
       (cons (re-match-start 0) (re-match-end 0))))

(define (lisp-comment-indentation mark)
  (cond ((match-forward ";;;" mark)
	 0)
	((match-forward ";;" mark)
	 (let ((indentation (calculate-lisp-indentation mark)))
	   (if (pair? indentation) (car indentation) indentation)))
	(else
	 (max (1+ (mark-column (horizontal-space-start mark)))
	      (ref-variable comment-column)))))

;;;; Indent Expression

(define (lisp-indent-sexp point)
  (let ((end (mark-permanent! (line-start (forward-sexp point 1 'ERROR) 0))))
    (if (mark< point end)
	(let loop ((index point) (stack '()))
	  (let next-line-start ((index index) (state false))
	    (let ((start (line-start index 1)))
	      (let ((state (parse-partial-sexp index start false false state)))
		(let ((stack (adjust-stack (parse-state-depth state) stack)))
		  (cond ((not (mark= start end))
			 (let ((start (mark-right-inserting start)))
			   (if (or (parse-state-in-string? state)
				   (parse-state-in-comment? state))
			       (next-line-start start state)
			       (begin
				 (cond ((indent-comment-line start stack)
					unspecific)
				       ((line-blank? start)
					(delete-horizontal-space start))
				       (else
					(indent-expression-line start stack)))
				 (loop start stack)))))
			((not (or (parse-state-in-string? state)
				  (parse-state-in-comment? state)))
			 (indent-expression-line start stack)))))))))))

(define (indent-comment-line start stack)
  (let ((mark (horizontal-space-end start)))
    (and (match-forward ";" mark)
	 (begin
	   (maybe-change-indentation
	    (cond ((match-forward ";;;" mark) (mark-column mark))
		  ((match-forward ";;" mark) (compute-indentation start stack))
		  (else (ref-variable comment-column)))
	    mark)
	   true))))

(define (indent-expression-line start stack)
  (maybe-change-indentation (compute-indentation start stack) start))

(define (compute-indentation start stack)
  (cond ((null? stack)
	 (let ((indent (calculate-lisp-indentation start)))
	   (if (pair? indent)
	       (car indent)
	       indent)))
	((and (car stack)
	      (integer? (car stack)))
	 (car stack))
	(else
	 (let ((indent
		(calculate-lisp-indentation
		 start
		 (or (car stack)
		     (backward-one-definition-start start)
		     (group-start start)))))
	   (if (pair? indent)
	       (begin
		 (set-car! stack (cdr indent))
		 (car indent))
	       (begin
		 (set-car! stack indent)
		 indent))))))

(define (adjust-stack depth-delta stack)
  (cond ((zero? depth-delta) stack)
	((positive? depth-delta) (up-stack depth-delta stack))
	(else (down-stack depth-delta stack))))

(define (down-stack n stack)
  (if (= -1 n) (cdr stack) (down-stack (1+ n) (cdr stack))))

(define (up-stack n stack)
  (if (= 1 n) (cons false stack) (up-stack (-1+ n) (cons false stack))))