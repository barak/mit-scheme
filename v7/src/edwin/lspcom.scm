;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/lspcom.scm,v 1.148 1989/03/14 08:01:23 cph Exp $
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

;;;; Lisp Commands

(declare (usual-integrations))

;;;; S-expression Commands

(define-command ("^R Forward Sexp" (argument 1))
  "Move forward across one balanced expression.
With argument, do this that many times."
  (move-thing forward-sexp argument))

(define-command ("^R Backward Sexp" (argument 1))
  "Move backward across one balanced expression.
With argument, do this that many times."
  (move-thing backward-sexp argument))

(define-command ("^R Flash Forward Sexp" (argument 1))
  "Flash the char which ends the expression to the right of point.
Shows you where \\[^R Forward Sexp] would go."
  (mark-flash (forward-sexp (current-point) argument)
	      (if (negative? argument) 'RIGHT 'LEFT)))

(define-command ("^R Flash Backward Sexp" (argument 1))
  "Flash the char which starts the expression to the left of point.
Shows you where \\[^R Backward Sexp] would go."
  (mark-flash (backward-sexp (current-point) argument)
	      (if (negative? argument) 'LEFT 'RIGHT)))

(define-command ("^R Kill Sexp" (argument 1))
  "Kill the syntactic expression following the cursor.
With argument, kill that many expressions after (or before) the cursor."
  (kill-thing forward-sexp argument))

(define-command ("^R Backward Kill Sexp" (argument 1))
  "Kill the syntactic expression preceding the cursor.
With argument, kill that many expressions before (or after) the cursor."
  (kill-thing backward-sexp argument))

(define-command ("^R Transpose Sexps" (argument 1))
  "Transpose the sexps before and after point.
See ^R Transpose Words, reading 'sexp' for 'word'."
  (transpose-things forward-sexp argument))

(define-command ("^R Mark Sexp" (argument 1))
  "Mark one or more sexps from point."
  (mark-thing forward-sexp argument))

;;;; List Commands

(define-command ("^R Forward List" (argument 1))
  "Move forward across one balanced group of parentheses.
With argument, do this that many times."
  (move-thing forward-list argument))

(define-command ("^R Backward List" (argument 1))
  "Move backward across one balanced group of parentheses.
With argument, do this that many times."
  (move-thing backward-list argument))

(define-command ("^R Forward Down List" (argument 1))
  "Move forward down one level of parentheses.
With argument, do this that many times.
A negative argument means move backward but still go down a level."
  (move-thing forward-down-list argument))

(define-command ("^R Backward Down List" (argument 1))
  "Move backward down one level of parentheses.
With argument, do this that many times.
A negative argument means move forward but still go down a level."
  (move-thing backward-down-list argument))

(define-command ("^R Forward Up List" (argument 1))
  "Move forward out one level of parentheses.
With argument, do this that many times.
A negative argument means move backward but still to a less deep spot."
  (move-thing forward-up-list argument))

(define-command ("^R Backward Up List" (argument 1))
  "Move backward out one level of parentheses.
With argument, do this that many times.
A negative argument means move forward but still to a less deep spot."
  (move-thing backward-up-list argument))

;;;; Definition Commands

(define-command ("^R Beginning of Definition" (argument 1))
  "Move to beginning of this or previous definition.
Leaves the mark behind, in case typed by accident.
With a negative argument, moves forward to the beginning of a definition.
The beginning of a definition is determined by Definition Start."
  (move-thing backward-definition-start argument))

(define-command ("^R End of Definition" (argument 1))
  "Move to end of this or next definition.
Leaves the mark behind, in case typed by accident.
With argument of 2, finds end of following definition.
With argument of -1, finds end of previous definition, etc."
  (move-thing forward-definition-end (if (zero? argument) 1 argument)))

(define-command ("^R Mark Definition")
  "Put mark at end of definition, point at beginning."
  (let ((point (current-point)))
    (let ((end (forward-definition-end point 1 'ERROR)))
      (let ((start (backward-definition-start end 1 'ERROR)))
	(push-current-mark! point)
	(push-current-mark! end)
	(set-current-point!
	 (or (re-search-backward "^\n" start (mark-1+ start))
	     start))))))

(define-command ("^R Reposition Window")
  "Reposition window so current definition is at the top.
If this would place point off screen, nothing happens."
  (reposition-window-top (current-definition-start)))

(define (current-definition-start)
  (let ((point (current-point)))
    (if (definition-start? point)
	point
	(backward-definition-start point 1 'ERROR))))

;;;; Miscellaneous Commands

(define-command ("^R Lisp Insert Paren" (argument 1))
  "Insert one or more close parens, flashing the matching open paren."
  (insert-chars (current-command-char) argument)
  (if (positive? argument)
      (let ((point (current-point)))
	(if (and (not (mark-left-char-quoted? point))
		 (not (keyboard-active? 5)))
	    (mark-flash (backward-one-sexp point) 'RIGHT)))))

(define-command ("^R Indent for Lisp" argument)
  "Indent current line as lisp code.
With argument, indent any additional lines of the same expression
rigidly along with this one."
  (lisp-indent-line argument))

(define-command ("^R Indent Sexp")
  "Indent each line of the expression starting just after the point."
  (lisp-indent-sexp (current-point)))

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