;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/argred.scm,v 1.28 1989/04/15 00:46:21 cph Exp $
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

;;;; Command Argument Reader

(declare (usual-integrations))

;;; 1.  The reader keeps track of:
;;;
;;; [] The MAGNITUDE of the argument.  If there are no digits, the
;;;    magnitude is false.
;;; [] The SIGN of the argument.
;;; [] The MULTIPLIER-EXPONENT, which is the number of C-U's typed.
;;; [] Whether or not "Autoargument mode" is in effect.  In autoarg
;;;    mode, ordinary digits are interpreted as part of the argument;
;;;    normally they are self-inserting.
;;;
;;; 2.  From these, it can compute:
;;;
;;; [] VALUE = (* MAGNITUDE (EXPT 4 MULTIPLIER-EXPONENT)).
;;;    If the magnitude is false, then the value is too.

;;;; Commands

(define-command universal-argument
  "Increments the argument multiplier and enters Autoarg mode.
In Autoarg mode, - negates the numeric argument, and the
digits 0, ..., 9 accumulate it."
  ()
  (lambda ()
    (command-argument-increment-multiplier-exponent!)
    (enter-autoargument-mode!)
    (update-argument-prompt!)
    (read-and-dispatch-on-char)))

(define-command digit-argument
  "Sets the numeric argument for the next command.
Several such digits typed consecutively accumulate to form
the argument.  This command should *only* be placed on a character
which is a digit (modulo control/meta bits)."
  ()
  (lambda ()
    (command-argument-accumulate-digit! (char-base (current-command-char)))
    (update-argument-prompt!)
    (read-and-dispatch-on-char)))

(define-command negative-argument
  "Negates the numeric argument for the next command.
If no argument has yet been given, the argument defaults to -1."
  ()
  (lambda ()
    (command-argument-negate!)
    (update-argument-prompt!)
    (read-and-dispatch-on-char)))

(define (command-argument-self-insert? procedure)
  (and (or (eq? procedure (ref-command auto-digit-argument))
	   (and (eq? procedure (ref-command auto-negative-argument))
		(command-argument-beginning?)))
       (not *autoargument-mode?*)))

(define-command auto-digit-argument
  "In Autoargument mode, sets numeric argument to the next command.
Otherwise, the digit inserts itself.  This just dispatches to either
\\[digit-argument] or \\[self-insert-command], depending on the mode."
  ()
  (lambda ()
    (dispatch-on-command
     (if (autoargument-mode?)
	 (ref-command-object digit-argument)
	 (ref-command-object self-insert-command)))))

(define-command auto-negative-argument
  "In Autoargument mode, sets numeric sign to the next command.
Otherwise, the character inserts itself.  This just dispatches to either
\\[negative-argument] or \\[insert-self-command], depending on the mode."
  ()
  (lambda ()
    (dispatch-on-command
     (if (and *autoargument-mode?* (command-argument-beginning?))
	 (ref-command-object negative-argument)
	 (ref-command-object self-insert-command)))))

(define-command auto-argument
  "Used to start a command argument and enter Autoargument mode.
This should only be placed on digits or -, with or without control
or meta bits."
  "P"
  (lambda (argument)
    (let ((char (char-base (current-command-char))))
      (cond ((not (eq? char #\-))
	     (enter-autoargument-mode!)
	     (dispatch-on-command (ref-command-object digit-argument)))
	    ((command-argument-beginning?)
	     (enter-autoargument-mode!)
	     (dispatch-on-command (ref-command-object negative-argument)))
	    (else
	     (insert-chars char argument))))))

;;;; Primitives

(define (with-command-argument-reader thunk)
  (fluid-let ((*magnitude*)
	      (*negative?*)
	      (*multiplier-exponent*)
	      (*multiplier-value*)
	      (*autoargument-mode?*)
	      (*previous-prompt*))
    (thunk)))

(define (reset-command-argument-reader!)
  ;; Call this at the beginning of a command cycle.
  (set! *magnitude* false)
  (set! *negative?* false)
  (set! *multiplier-exponent* 0)
  (set! *multiplier-value* 1)
  (set! *autoargument-mode?* false)
  (set! *previous-prompt* ""))

(define (command-argument-prompt)
  (or *previous-prompt* (%command-argument-prompt)))

(define *previous-prompt*)

(define (update-argument-prompt!)
  (let ((prompt (%command-argument-prompt)))
    (set! *previous-prompt* prompt)
    (set-command-prompt! prompt)))

(define (%command-argument-prompt)
  (if (and (not *magnitude*)
	   (if (autoargument-mode?)
	       (and (not *negative?*)
		    (= *multiplier-exponent* 1))
	       *negative?*))
      (xchar->name (current-command-char))
      (let ((prefix (if (autoargument-mode?) "Autoarg" "Arg"))
	    (value (command-argument-value)))
	(cond (value (string-append-separated prefix (write-to-string value)))
	      (*negative?* (string-append-separated prefix "-"))
	      (else "")))))

;;;; Argument Number

(define *magnitude*)
(define *negative?*)

(define (command-argument-accumulate-digit! digit-char)
  (set! *multiplier-exponent* 0)
  (set! *multiplier-value* 1)
  (let ((digit (or (char->digit digit-char 10)
		   (error "Not a valid digit" digit-char))))
    (set! *magnitude*
	  (if (not *magnitude*)
	      digit
	      (+ digit (* 10 *magnitude*))))))

(define (command-argument-negate!)
  (set! *multiplier-exponent* 0)
  (set! *multiplier-value* 1)
  (set! *negative?* (not *negative?*)))

(define (command-argument-magnitude)
  *magnitude*)

(define (command-argument-negative?)
  *negative?*)

;;;; Argument Multiplier

(define *multiplier-exponent*)
(define *multiplier-value*)

(define (command-argument-increment-multiplier-exponent!)
  (set! *magnitude* false)
  (set! *negative?* false)
  (set! *multiplier-exponent* (1+ *multiplier-exponent*))
  (set! *multiplier-value* (* 4 *multiplier-value*)))

(define (command-argument-multiplier-exponent)
  *multiplier-exponent*)

;;;; Autoargument Mode

(define *autoargument-mode?*)

(define (enter-autoargument-mode!)
  (set! *autoargument-mode?* true))

(define (autoargument-mode?)
  *autoargument-mode?*)

;;;; Value

(define (command-argument-standard-value?)
  (or *magnitude*
      (not (zero? *multiplier-exponent*))
      *negative?*))

(define (command-argument-standard-value)
  (or (command-argument-value)
      (and *negative?* -1)))

(define (command-argument-value)
  ;; This returns the numeric value of the argument, or false if none.
  (cond (*magnitude*
	 (* (if *negative?* (- *magnitude*) *magnitude*)
	    *multiplier-value*))
	((not (zero? *multiplier-exponent*))
	 (if *negative?* (- *multiplier-value*) *multiplier-value*))
	(else false)))

(define (command-argument-multiplier-only?)
  (and (not *magnitude*)
       (not (zero? *multiplier-exponent*))
       *multiplier-exponent*))

(define (command-argument-negative-only?)
  (and (not *magnitude*)
       (zero? *multiplier-exponent*)
       *negative?*))

(define (command-argument-beginning?)
  (and (not *magnitude*)
       (not *negative?*)
       (< *multiplier-exponent* 2)))