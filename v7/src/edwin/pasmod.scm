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

;;;; Pascal Mode

(declare (usual-integrations))
(using-syntax edwin-syntax-table

(define-command ("Pascal Mode" argument)
  "Enter Pascal mode."
  (set-current-major-mode! pascal-mode))

(define-major-mode "Pascal" "Fundamental"
  "Major mode specialized for editing Pascal code."
  (local-set-variable! "Syntax Table" pascal-mode:syntax-table)
  (local-set-variable! "Syntax Ignore Comments Backwards" #!TRUE)
  (local-set-variable! "Indent Line Procedure" ^r-pascal-indent-command)
  (local-set-variable! "Comment Column" 32)
  (local-set-variable! "Comment Locator Hook" pascal-comment-locate)
  (local-set-variable! "Comment Indent Hook" pascal-comment-indentation)
  (local-set-variable! "Comment Start" "(* ")
  (local-set-variable! "Comment End" " *)")
  (local-set-variable! "Paragraph Start" "^$")
  (local-set-variable! "Paragraph Separate" (ref-variable "Paragraph Start"))
  (local-set-variable! "Delete Indentation Right Protected" (char-set #\( #\[))
  (local-set-variable! "Delete Indentation Left Protected" (char-set #\) #\]))
  (if (ref-variable "Pascal Mode Hook")
      ((ref-variable "Pascal Mode Hook"))))

(define pascal-mode:syntax-table (make-syntax-table))
(modify-syntax-entry! pascal-mode:syntax-table #\( "()1 ")
(modify-syntax-entry! pascal-mode:syntax-table #\) ")( 4")
(modify-syntax-entry! pascal-mode:syntax-table #\[ "(]  ")
(modify-syntax-entry! pascal-mode:syntax-table #\] ")[  ")
(modify-syntax-entry! pascal-mode:syntax-table #\{ "<   ")
(modify-syntax-entry! pascal-mode:syntax-table #\} ">   ")
(modify-syntax-entry! pascal-mode:syntax-table #\' "\"   ")
(modify-syntax-entry! pascal-mode:syntax-table #\$ "\"   ")
(modify-syntax-entry! pascal-mode:syntax-table #\* "_ 23")
(modify-syntax-entry! pascal-mode:syntax-table #\. "_   ")
(modify-syntax-entry! pascal-mode:syntax-table #\^ "_   ")
(modify-syntax-entry! pascal-mode:syntax-table #\@ "'   ")
(modify-syntax-entry! pascal-mode:syntax-table #\% "    ")
(modify-syntax-entry! pascal-mode:syntax-table #\" "    ")
(modify-syntax-entry! pascal-mode:syntax-table #\\ "    ")

(define (pascal-comment-locate mark)
  (if (re-search-forward "\\((\\*\\|{\\)[ \t]*" mark (line-end mark 0))
      (cons (re-match-start 0) (re-match-end 0))))

(define (pascal-comment-indentation mark)
  (let ((start (horizontal-space-start mark)))
    (if (line-start? start)
	(indentation-of-previous-non-blank-line mark)
	(max (1+ (mark-column start))
	     (ref-variable "Comment Column")))))

(define-key "Pascal" #\C-\( "^R Pascal Shift Left")
(define-key "Pascal" #\C-\) "^R Pascal Shift Right")
(define-key "Pascal" #\Rubout "^R Backward Delete Hacking Tabs")

(define-command ("^R Pascal Indent" argument)
  "Indents the current line for Pascal code."
  (let ((point (current-point)))
    (let ((indentation (calculate-pascal-indentation point)))
      (cond ((not (= indentation (current-indentation point)))
	     (change-indentation indentation point))
	    ((line-start? (horizontal-space-start point))
	     (set-current-point! (horizontal-space-end point)))))))

(define-command ("^R Pascal Shift Right" (argument 1))
  "Shift the current line right by Pascal Shift Increment.
With an argument, shifts right that many times."
  (if (not (zero? argument))
      (let ((mark (line-start (current-point) 0)))
	(change-indentation (+ (current-indentation mark)
			       (* argument
				  (ref-variable "Pascal Shift Increment")))
			    mark))))

(define-command ("^R Pascal Shift Left" (argument 1))
  "Shift the current line left by Pascal Shift Increment.
With an argument, shifts left that many times."
  (if (not (zero? argument))
      (let ((mark (line-start (current-point) 0)))
	(change-indentation (- (current-indentation mark)
			       (* argument
				  (ref-variable "Pascal Shift Increment")))
			    mark))))

(define (calculate-pascal-indentation mark)
  (let ((def-start
	  (let ((nb (find-previous-non-blank-line mark)))
	    (if (not nb)
		(group-start mark)
		(let ((start (backward-one-paragraph nb)))
		  (if (not start)
		      (group-start mark)
		      (line-start start 1)))))))
    (define (find-statement-start mark)
      (let ((start (find-previous-non-blank-line mark)))
	(cond ((not start) #!FALSE)
	      ((mark< start def-start) def-start)
	      (else
	       (let ((container
		      (parse-state-containing-sexp
		       (parse-partial-sexp def-start start))))
		 (if container
		     (find-statement-start start)
		     start))))))
    (let ((state (parse-partial-sexp def-start (line-start mark 0))))
      (let ((container (parse-state-containing-sexp state))
	    (last-sexp (parse-state-last-sexp state)))
	(if container
	    ;; Inside some parenthesized expression or arglist.
	    (if (mark> (line-end container 0) last-sexp)
		;; Indent first line under opening paren.
		(mark-column (horizontal-space-end (mark1+ container)))
		;; Indent subsequent line under previous line.
		(indentation-of-previous-non-blank-line mark))
	    (let ((start (find-statement-start mark)))
	      (if (not start)
		  0
		  (let ((start (horizontal-space-end start)))
		    (let ((indentation (mark-column start)))
		      (if (and (ref-variable "Pascal Indentation Keywords")
			       (re-match-forward
				(ref-variable "Pascal Indentation Keywords")
				start))
			  (+ indentation
			     (ref-variable "Pascal Shift Increment"))
			  indentation))))))))))

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: edwin-package
;;; Scheme Syntax Table: edwin-syntax-table
;;; End:
