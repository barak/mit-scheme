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
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Lisp Indentation

(declare (usual-integrations))
(using-syntax edwin-syntax-table

(define lisp-indentation-package
  (make-environment

;;; CALCULATE-LISP-INDENTATION returns either an integer, which is the
;;; column to indent to, or a pair.  In the latter case this means
;;; that subsequent forms in the same expression may not be indented
;;; the same way; so the car is the indentation, and the cdr is a mark
;;; pointing at the beginning of the containing expression.  Typically
;;; this is passed back in as PARSE-START to speed up the indentation
;;; of many forms at once.

(define (calculate-lisp-indentation mark #!optional parse-start)
  (if (unassigned? parse-start)
      (set! parse-start
	    (or (backward-one-definition-start mark)
		(group-start mark))))
  (find-outer-container parse-start (line-start mark 0)))

(define (find-outer-container start indent-point)
  (let ((state (parse-partial-sexp start indent-point 0)))
    (if (mark= (parse-state-location state) indent-point)
	(find-inner-container state #!FALSE #!FALSE indent-point)
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
	((and (integer? (ref-variable "Lisp Indent Offset")) container)
	 (+ (ref-variable "Lisp Indent Offset") (mark-column container)))
	((positive? (parse-state-depth state))
	 (if (not last-sexp)
	     (mark-column (mark1+ container))
	     (normal-indent state container last-sexp indent-point)))
	(else
	 (mark-column (parse-state-location state)))))

;;;
;;; The following are true when the indent hook is called:
;;;
;;; * CONTAINER < NORMAL-INDENT <= LAST-SEXP < INDENT-POINT
;;; * Since INDENT-POINT is a line start, LAST-SEXP is on a
;;;   line previous to that line.
;;; * NORMAL-INDENT is at the start of an expression.
;;;

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
      (if (char=? #\( (char->syntax-code (mark-right-char first-sexp)))
	  ;; The first expression is a list -- don't bother to call
	  ;; the indent hook.
	  (mark-column (backward-prefix-chars normal-indent))
	  (let ((normal-indent (backward-prefix-chars normal-indent)))
	    (or (and (ref-variable "Lisp Indent Hook")
		     ((ref-variable "Lisp Indent Hook")
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
    (and (let ((syntax (char->syntax-code (mark-right-char first-sexp))))
	   (or (char=? #\w syntax)
	       (char=? #\_ syntax)))
	 (let ((name (extract-string first-sexp
				     (forward-one-sexp first-sexp))))
	   (let ((method
		  (string-table-get (ref-variable "Lisp Indent Methods")
				    name)))
	     (cond ((or (eq? method 'DEFINITION)
			(and (not method)
			     (<= 3 (string-length name))
			     (substring-ci=? "DEF" 0 3 name 0 3)))
		    (lisp-indent-definition state indent-point normal-indent))
		   ((integer? method)
		    (lisp-indent-special-form method state indent-point
					      normal-indent))
		   (method
		    (method state indent-point normal-indent))))))))

;;; Indent the first subform in a definition at the body indent.
;;; Indent subsequent subforms normally.

(define (lisp-indent-definition state indent-point normal-indent)
  (let ((container (parse-state-containing-sexp state)))
    (and (mark> (line-end container 0) (parse-state-last-sexp state))
	 (+ (ref-variable "Lisp Body Indent") (mark-column container)))))

;;; Indent the first N subforms normally, but then indent the
;;; remaining forms at the body-indent.  If this is one of the first
;;; N, a cons is returned, the cdr of which is CONTAINING-SEXP.  This
;;; is to speed up indentation of successive forms.

(define (lisp-indent-special-form n state indent-point normal-indent)
  (if (negative? n) (error "Special form indent hook negative" n))
  (let ((container (parse-state-containing-sexp state)))
    (let ((body-indent (+ (ref-variable "Lisp Body Indent")
			  (mark-column container)))
	  (normal-indent (mark-column normal-indent)))
      (define (loop n mark)
	(cond ((not mark)
	       (cons normal-indent container))
	      ((zero? n)
	       (if (forward-one-sexp mark indent-point)
		   normal-indent
		   (min body-indent normal-indent)))
	      (else
	       (loop (-1+ n) (forward-one-sexp mark indent-point)))))
      (let ((second-sexp
	     (forward-to-sexp-start (forward-one-sexp (mark1+ container)
						      indent-point)
				    indent-point)))
	(cond ((mark< second-sexp indent-point) (loop n second-sexp))
	      ((zero? n) body-indent)
	      (else (cons normal-indent container)))))))

;;;; Indent Line

(define (lisp-indent-line whole-sexp?)
  (let ((start (indentation-end (current-point))))
    (if (not (match-forward ";;;" start))
	(let ((indentation
	       (let ((indent (calculate-lisp-indentation start)))
		 (if (pair? indent) (car indent) indent))))
	  (let ((shift-amount (- indentation (mark-column start))))
	    (cond ((not (zero? shift-amount))
		   (change-indentation indentation start)
		   (if whole-sexp?
		       (indent-code-rigidly start (forward-sexp start 1 'ERROR)
					    shift-amount #!FALSE)))
		  ((within-indentation? (current-point))
		   (set-current-point! start))))))))

(define (indent-code-rigidly start end shift-amount nochange-regexp)
  (let ((end (mark-left-inserting end)))
    (define (phi1 start state)
      (let ((start* (line-start start 1 'LIMIT)))
	(if (mark< start* end)
	    (phi2 start*
		  (parse-partial-sexp start start* #!FALSE #!FALSE state)))))

    (define (phi2 start state)
      (if (not (or (parse-state-in-string? state)
		   (parse-state-in-comment? state)
		   (and nochange-regexp
			(re-match-forward nochange-regexp start))))
	  (let ((start (horizontal-space-end start))
		(end (line-end start 0)))
	    (cond ((line-end? start) (delete-horizontal-space start))
		  ((match-forward ";;;" start) 'DONE)
		  (else
		   (change-indentation (max 0
					    (+ (mark-column start)
					       shift-amount))
				       start)))))
      (phi1 start state))

    (phi1 start #!FALSE)))

;;;; Indent Expression

(define (lisp-indent-sexp point)
  (let ((end (mark-permanent! (line-start (forward-sexp point 1 'ERROR) 0))))
    (define (loop start indent-stack)
      (next-line-start start #!FALSE
	(lambda (start state)
	  (let ((indent-stack (adjust-stack (parse-state-depth state)
					    indent-stack)))
	    (cond ((mark= start end)
		   (if (not (or (parse-state-in-string? state)
				(parse-state-in-comment? state)))
		       (indent-expression-line start indent-stack)))
		  ((indent-comment-line start indent-stack)
		   (loop start indent-stack))
		  ((line-blank? start)
		   (delete-horizontal-space start)
		   (loop start indent-stack))
		  (else
		   (indent-expression-line start indent-stack)
		   (loop start indent-stack)))))))

    (define (next-line-start start state receiver)
      (let ((start* (line-start start 1)))
	(let ((state* (parse-partial-sexp start start* #!FALSE #!FALSE state)))
	(if (or (not (or (parse-state-in-string? state*)
			 (parse-state-in-comment? state*)))
		(mark= start* end))
	    (receiver start* state*)
	    (next-line-start start* state* receiver)))))

    (if (mark< point end) (loop point '()))))

(define (indent-comment-line start indent-stack)
  (let ((mark (horizontal-space-end start)))
    (and (match-forward ";" mark)
	 (begin (maybe-change-indentation
		 (cond ((match-forward ";;;" mark)
			(mark-column mark))
		       ((match-forward ";;" mark)
			(compute-indentation start indent-stack))
		       (else comment-column))
		 mark)
		#!TRUE))))

(define (indent-expression-line start indent-stack)
  (maybe-change-indentation (compute-indentation start indent-stack)
			    start))

(define (compute-indentation start indent-stack)
  (cond ((null? indent-stack)
	 (let ((indent (calculate-lisp-indentation start)))
	   (if (pair? indent)
	       (car indent)
	       indent)))
	((and (car indent-stack)
	      (integer? (car indent-stack)))
	 (car indent-stack))
	(else
	 (let ((indent
		(calculate-lisp-indentation
		 start
		 (or (car indent-stack)
		     (backward-one-definition-start start)
		     (group-start start)))))
	   (if (pair? indent)
	       (begin (set-car! indent-stack (cdr indent))
		      (car indent))
	       (begin (set-car! indent-stack indent)
		      indent))))))

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
	      comment-column))))

;;; end LISP-INDENTATION-PACKAGE
))

;;;; Control Variables

(define-variable "Lisp Indent Offset"
  "If not false, the number of extra columns to indent a subform."
  #!FALSE)

(define-variable "Lisp Indent Hook"
  "If not false, a procedure for modifying lisp indentation."
  #!FALSE)

(define-variable "Lisp Indent Methods"
  "String table identifying special forms for lisp indentation.")

(define-variable "Lisp Body Indent"
  "Number of extra columns to indent the body of a special form."
  2)

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: (access lisp-indentation-package edwin-package)
;;; Scheme Syntax Table: edwin-syntax-table
;;; Tags Table Pathname: (access edwin-tags-pathname edwin-package)
;;; End:
