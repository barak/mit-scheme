;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/lspcom.scm,v 1.154 1991/08/06 15:39:46 arthur Exp $
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

;;;; Lisp Commands

(declare (usual-integrations))

;;;; S-expression Commands

(define-command forward-sexp
  "Move forward across one balanced expression.
With argument, do this that many times."
  "p"
  (lambda (argument)
    (move-thing forward-sexp argument)))

(define-command backward-sexp
  "Move backward across one balanced expression.
With argument, do this that many times."
  "p"
  (lambda (argument)
    (move-thing backward-sexp argument)))

(define-command flash-sexp
  "Flash the char which ends the expression to the right of point.
Shows you where \\[forward-sexp] would go."
  "p"
  (lambda (argument)
    (mark-flash (forward-sexp (current-point) argument)
		(if (negative? argument) 'RIGHT 'LEFT))))

(define-command backward-flash-sexp
  "Flash the char which starts the expression to the left of point.
Shows you where \\[backward-sexp] would go."
  "p"
  (lambda (argument)
    (mark-flash (backward-sexp (current-point) argument)
		(if (negative? argument) 'LEFT 'RIGHT))))

(define-command kill-sexp
  "Kill the syntactic expression following the cursor.
With argument, kill that many expressions after (or before) the cursor."
  "p"
  (lambda (argument)
    (kill-thing forward-sexp argument)))

(define-command backward-kill-sexp
  "Kill the syntactic expression preceding the cursor.
With argument, kill that many expressions before (or after) the cursor."
  "p"
  (lambda (argument)
    (kill-thing backward-sexp argument)))

(define-command transpose-sexps
  "Transpose the sexps before and after point.
See \\[transpose-words], reading 'sexp' for 'word'."
  "p"
  (lambda (argument)
    (transpose-things forward-sexp argument)))

(define-command mark-sexp
  "Mark one or more sexps from point."
  "p"
  (lambda (argument)
    (mark-thing forward-sexp argument)))

;;;; List Commands

(define-command forward-list
  "Move forward across one balanced group of parentheses.
With argument, do this that many times."
  "p"
  (lambda (argument)
    (move-thing forward-list argument)))

(define-command backward-list
  "Move backward across one balanced group of parentheses.
With argument, do this that many times."
  "p"
  (lambda (argument)
    (move-thing backward-list argument)))

(define-command down-list
  "Move forward down one level of parentheses.
With argument, do this that many times.
A negative argument means move backward but still go down a level."
  "p"
  (lambda (argument)
    (move-thing forward-down-list argument)))

(define-command backward-down-list
  "Move backward down one level of parentheses.
With argument, do this that many times.
A negative argument means move forward but still go down a level."
  "p"
  (lambda (argument)
    (move-thing backward-down-list argument)))

(define-command up-list
  "Move forward out one level of parentheses.
With argument, do this that many times.
A negative argument means move backward but still to a less deep spot."
  "p"
  (lambda (argument)
    (move-thing forward-up-list argument)))

(define-command backward-up-list
  "Move backward out one level of parentheses.
With argument, do this that many times.
A negative argument means move forward but still to a less deep spot."
  "p"
  (lambda (argument)
    (move-thing backward-up-list argument)))

;;;; Definition Commands

(define-command beginning-of-defun
  "Move backward to next beginning-of-defun.
With argument, do this that many times."
  "p"
  (lambda (argument)
    (move-thing backward-definition-start argument)))

(define-command end-of-defun
  "Move forward to next end of defun.
An end of a defun is found by moving forward from the beginning of one."
  "p"
  (lambda (argument)
    (move-thing forward-definition-end (if (zero? argument) 1 argument))))

(define-command mark-defun
  "Put mark at end of defun, point at beginning."
  ()
  (lambda ()
    (let ((point (current-point)))
      (let ((end (forward-definition-end point 1 'ERROR)))
	(let ((start (backward-definition-start end 1 'ERROR)))
	  (push-current-mark! point)
	  (push-current-mark! end)
	  (set-current-point!
	   (or (re-search-backward "^\n" start (mark-1+ start))
	       start)))))))

(define-command align-defun
  "Reposition window so current defun is at the top.
If this would place point off screen, nothing happens."
  ()
  (lambda ()
    (reposition-window-top (current-definition-start))))

(define (current-definition-start)
  (let ((point (current-point)))
    (if (definition-start? point)
	point
	(backward-definition-start point 1 'ERROR))))

;;;; Miscellaneous Commands

(define-command lisp-insert-paren
  "Insert one or more close parens, flashing the matching open paren."
  "p"
  (lambda (argument)
    (let ((key (current-command-key)))
      (if (char? key)
	  (begin
	    (insert-chars key argument)
	    (if (positive? argument)
		(let ((point (current-point)))
		  (if (not (mark-left-char-quoted? point))
		      (mark-flash (backward-one-sexp point) 'RIGHT)))))))))

(define-command lisp-indent-line
  "Indent current line as lisp code.
With argument, indent any additional lines of the same expression
rigidly along with this one."
  "P"
  (lambda (#!optional argument)
    (lisp-indent-line (and (not (default-object? argument)) argument))))

(define-command indent-sexp
  "Indent each line of the expression starting just after the point."
  "d"
  (lambda (mark)
    (lisp-indent-sexp mark)))

(define-command insert-parentheses
  "Insert a pair of matching parentheses, leaving the point after the
open parenthesis.  With argument, wrap parentheses around that many
following sexps.)"
  "P"
  (lambda (argument)
    (if argument
	(set-current-point! (skip-chars-forward " \t"))
	(or (group-start? (current-point))
	    (memv (char->syntax-code standard-syntax-table
				     (mark-left-char (current-point)))
		  '(#\\ #\> #\( #\space #\.))
	    (insert-char #\space)))
    (insert-char #\()
    (let ((mark (mark-right-inserting-copy (current-point))))
      (insert-char #\)
		   (if (and argument
			    (exact-nonnegative-integer? argument))
		       (forward-sexp (current-point) argument 'LIMIT)
		       (current-point)))
      (or argument
	  (group-end? (current-point))
	  (memv (char->syntax-code standard-syntax-table
				   (mark-right-char (current-point)))
		'(#\\ #\> #\( #\) #\space))
	  (insert-char #\space))
      (set-current-point! mark))))

(define-command move-past-close-and-reindent
  "Move past next right parenthesis, delete indentation before it, and
indent after it."
  ()
  (lambda ()
    (set-current-point! (mark-1+ (forward-up-list (current-point) 1 'limit)))
    (let delete-more-indentation ((before-parenthesis (current-point)))
      (if (mark= before-parenthesis
		 (horizontal-space-end (line-start (current-point) 0)))
	  (begin ((ref-command delete-indentation) #f)
		 (delete-more-indentation (current-point)))))
    (move-thing mark+ 1)
    ((ref-command newline-and-indent))))

;;;; Motion Covers

(define forward-sexp)
(define backward-sexp)
(make-motion-pair forward-one-sexp backward-one-sexp
  (lambda (f b)
    (set! forward-sexp f)
    (set! backward-sexp b)))

(define forward-list)
(define backward-list)
(make-motion-pair forward-one-list backward-one-list
  (lambda (f b)
    (set! forward-list f)
    (set! backward-list b)))

(define forward-down-list)
(define backward-down-list)
(make-motion-pair forward-down-one-list backward-down-one-list
  (lambda (f b)
    (set! forward-down-list f)
    (set! backward-down-list b)))

(define forward-up-list)
(define backward-up-list)
(make-motion-pair forward-up-one-list backward-up-one-list
  (lambda (f b)
    (set! forward-up-list f)
    (set! backward-up-list b)))

(define forward-definition-start)
(define backward-definition-start)
(make-motion-pair forward-one-definition-start backward-one-definition-start
  (lambda (f b)
    (set! forward-definition-start f)
    (set! backward-definition-start b)))

(define forward-definition-end)
(define backward-definition-end)
(make-motion-pair forward-one-definition-end backward-one-definition-end
  (lambda (f b)
    (set! forward-definition-end f)
    (set! backward-definition-end b)))