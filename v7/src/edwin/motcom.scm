;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1985 Massachusetts Institute of Technology
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

;;;; Motion Commands

(declare (usual-integrations))
(using-syntax edwin-syntax-table

(define-command ("^R Beginning of Line" (argument 1))
  "Move point to beginning of line."
  (set-current-point! (line-start (current-point) (-1+ argument) 'LIMIT)))

(define-command ("^R Backward Character" (argument 1))
  "Move back one character.
With argument, move that many characters backward.
Negative arguments move forward."
  (move-thing mark- argument))

(define-command ("^R End of Line" (argument 1))
  "Move point to end of line."
  (set-current-point! (line-end (current-point) (-1+ argument) 'LIMIT)))

(define-command ("^R Forward Character" (argument 1))
  "Move forward one character.
With argument, move that many characters forward.
Negative args move backward."
  (move-thing mark+ argument))

(define-command ("^R Goto Beginning" argument)
  "Go to beginning of buffer (leaving mark behind).
With arg from 0 to 10, goes that many tenths of the file
down from the beginning.  Just C-U as arg means go to end."
  (push-current-mark! (current-point))
  (cond ((not argument)
	 (set-current-point! (buffer-start (current-buffer))))
	((command-argument-multiplier-only?)
	 (set-current-point! (buffer-end (current-buffer))))
	((<= 0 argument 10)
	 (set-current-point! (region-10ths (buffer-region (current-buffer))
					   argument)))))

(define-command ("^R Goto End" argument)
  "Go to end of buffer (leaving mark behind).
With arg from 0 to 10, goes up that many tenths of the file from the end."
  (push-current-mark! (current-point))
  (cond ((not argument)
	 (set-current-point! (buffer-end (current-buffer))))
	((<= 0 argument 10)
	 (set-current-point! (region-10ths (buffer-region (current-buffer))
					   (- 10 argument))))))

(define (region-10ths region n)
  (mark+ (region-start region)
	 (quotient (* n (region-count-chars region)) 10)))

(define-command ("Goto Char" (argument 0))
  "Goto the Nth character from the start of the buffer.
A negative argument goes to the -Nth character from the end of the buffer."
  (let ((mark (mark+ ((if (negative? argument) buffer-end buffer-start)
		      (current-buffer))
		     argument)))
    (if mark
	(set-current-point! mark)
	(editor-error))))

(define-command ("Goto Line" (argument 0))
  "Goto the Nth line from the start of the buffer.
A negative argument goes to the -Nth line from the end of the buffer."
  (let ((mark (line-start ((if (negative? argument) buffer-end buffer-start)
			   (current-buffer))
			  argument)))
    (if mark
	(set-current-point! mark)
	(editor-error))))

(define-command ("Goto Page" (argument 1))
  "Goto the Nth page from the start of the buffer.
A negative argument goes to the -Nth page from the end of the buffer."
  (let ((mark (forward-page ((if (negative? argument) buffer-end buffer-start)
			     (current-buffer))
			    (cond ((negative? argument) argument)
				  ((positive? argument) (-1+ argument))
				  (else 1)))))
    (if mark
	(set-current-point! mark)
	(editor-error))))

(define-variable "Goal Column"
  "Semipermanent goal column for vertical motion,
as set by \\[^R Set Goal Column], or false, indicating no goal column."
  #!FALSE)

(define temporary-goal-column-tag
  "Temporary Goal Column")

(define-command ("^R Set Goal Column" argument)
  "Set (or flush) a permanent goal for vertical motion.
With no argument, makes the current column the goal for vertical
motion commands.  They will always try to go to that column.
With argument, clears out any previously set goal.
Only \\[^R Up Real Line] and \\[^R Down Real Line] are affected."
  (set! goal-column
	(and (not argument)
	     (current-column))))

(define (current-goal-column)
  (or goal-column
      (command-message-receive temporary-goal-column-tag
	identity-procedure
	current-column)))

(define-command ("^R Down Real Line" argument)
  "Move down vertically to next real line.
Continuation lines are skipped.  If given after the
last newline in the buffer, makes a new one at the end."
  (let ((column (current-goal-column)))
    (cond ((not argument)
	   (let ((mark (line-start (current-point) 1 #!FALSE)))
	     (if mark
		 (set-current-point! (move-to-column mark column))
		 (begin (set-current-point! (group-end (current-point)))
			(insert-newlines 1)))))
	  ((not (zero? argument))
	   (set-current-point!
	    (move-to-column (line-start (current-point) argument 'FAILURE)
			    column))))
    (set-command-message! temporary-goal-column-tag column)))

(define-command ("^R Up Real Line" (argument 1))
  "Move up vertically to next real line.
Continuation lines are skipped."
  (let ((column (current-goal-column)))
    (if (not (zero? argument))
	(set-current-point!
	 (move-to-column (line-start (current-point) (- argument) 'FAILURE)
			 column)))
    (set-command-message! temporary-goal-column-tag column)))

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: edwin-package
;;; Scheme Syntax Table: edwin-syntax-table
;;; End:
