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

;;;; Command Argument Reader

(declare (usual-integrations))
(using-syntax (access edwin-syntax-table edwin-package)

;;;; Description
;;; 
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
;;; 2.  It has the following (alterable) parameters:
;;;
;;; [] RADIX, which is between 2 and 36 inclusive. (default: 10)
;;; [] MULTIPLIER-BASE, a non-negative integer. (default: 4)
;;;
;;; 3.  From these, it can compute:
;;;
;;; [] VALUE = (* MAGNITUDE MULTIPLIER-EXPONENT MULTIPLIER-BASE).
;;;    If the magnitude is false, then the value is too.

(define with-command-argument-reader)
(define reset-command-argument-reader!)
(define command-argument-beginning?)
(define command-argument-multiplier-exponent)
(define command-argument-multiplier-only?)
(define command-argument-negative-only?)
(define command-argument-negative?)
(define command-argument-prompt)
(define command-argument-value)
(define command-argument-standard-value)
(define command-argument-self-insert?)

(define command-argument-package
  (make-environment

;;;; Commands

(define-command ("^R Universal Argument" argument)
  "Increments the argument multiplier and enters Autoarg mode.
In Autoarg mode, - negates the numeric argument, and the
digits 0, ..., 9 accumulate it."
  (command-argument-increment-multiplier-exponent!)
  (enter-autoargument-mode!)
  (update-argument-prompt!)
  (read-and-dispatch-on-char))

(define-command ("^R Argument Digit" argument)
  "Sets the numeric argument for the next command.
Several such digits typed consecutively accumulate in the radix
specified by the variable COMMAND-ARGUMENT-RADIX (normally 10) to form
the argument.  This command should *only* be placed on a character
which is a digit (modulo control/meta bits)."
  (command-argument-accumulate-digit! (char-base (current-command-char)))
  (update-argument-prompt!)
  (read-and-dispatch-on-char))

(define-command ("^R Negative Argument" argument)
  "Negates the numeric argument for the next command.
If no argument has yet been given, the argument defaults to -1."
  (command-argument-negate!)
  (update-argument-prompt!)
  (read-and-dispatch-on-char))

(set! command-argument-self-insert?
(named-lambda (command-argument-self-insert? procedure)
  (and (not *autoargument-mode?*)
       (or (eq? procedure ^r-autoargument-digit-command)
	   (and (eq? procedure ^r-auto-negative-argument-command)
		(command-argument-beginning?))))))

(define-command ("^R Autoargument Digit" argument)
  "In Autoargument mode, sets numeric argument to the next command.
Otherwise, the digit inserts itself.  This just dispatches to either
Argument Digit or Insert Self, depending on the mode."
  ((if (autoargument-mode?)
       ^r-argument-digit-command
       ^r-insert-self-command)
   argument))

(define-command ("^R Auto Negative Argument" argument)
  "In Autoargument mode, sets numeric sign to the next command.
Otherwise, the character inserts itself.  This just dispatches to either
Negative Argument or Insert Self, depending on the mode."
  ((if (and *autoargument-mode?* (command-argument-beginning?))
       ^r-negative-argument-command
       ^r-insert-self-command)
   argument))

(define-command ("^R Autoargument" argument)
  "Used to start a command argument and enter Autoargument mode.
This should only be placed on digits or -, with or without control
or meta bits."
  (let ((char (char-base (current-command-char))))
    (if (eq? char #\-)
	(if (command-argument-beginning?)
	    (begin (enter-autoargument-mode!)
		   (^r-negative-argument-command argument))
	    (insert-chars char argument))
	(begin (enter-autoargument-mode!)
	       (^r-argument-digit-command argument)))))

;;;; Primitives

(set! with-command-argument-reader
(named-lambda (with-command-argument-reader thunk)
  (fluid-let ((*magnitude*)
	      (*negative?*)
	      (*multiplier-exponent*)
	      (*autoargument-mode?*)
	      (*previous-prompt*))
    (thunk))))

(set! reset-command-argument-reader!
(named-lambda (reset-command-argument-reader!)
  ;; Call this at the beginning of a command cycle.
  (set! *magnitude* false)
  (set! *negative?* false)
  (set! *multiplier-exponent* 0)
  (set! *autoargument-mode?* false)
  (set! *previous-prompt* "")))

(set! command-argument-prompt
(named-lambda (command-argument-prompt)
  (or *previous-prompt* (%command-argument-prompt))))

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
(define *radix*)
(define *negative?*)

(define (command-argument-accumulate-digit! digit-char)
  (set! *multiplier-exponent* 0)
  (let ((digit (or (char->digit digit-char *radix*)
		   (error "Not a valid digit" digit-char))))
    (set! *magnitude*
	  (if (not *magnitude*)
	      digit
	      (+ digit (* *radix* *magnitude*))))))

(define (set-command-argument-radix! n)
  (if (not (and (integer? n) (<= 2 n 36)))
      (error "Radix must be an integer between 2 and 36, inclusive" n))
  (set! *radix* n))

(define (command-argument-negate!)
  (set! *multiplier-exponent* 0)
  (set! *negative?* (not *negative?*)))

(define (command-argument-magnitude)
  *magnitude*)

(define (command-argument-radix)
  *radix*)

(set! command-argument-negative?
(named-lambda (command-argument-negative?)
  *negative?*))

;; **** Kludge ****
(set-command-argument-radix! 10)

;;;; Argument Multiplier

(define *multiplier-exponent*)
(define *multiplier-base*)

(define (command-argument-increment-multiplier-exponent!)
  (set! *magnitude* false)
  (set! *negative?* false)
  (set! *multiplier-exponent* (1+ *multiplier-exponent*)))

(set! command-argument-multiplier-exponent
(named-lambda (command-argument-multiplier-exponent)
  *multiplier-exponent*))

(define (command-argument-multiplier-base)
  *multiplier-base*)

(define (set-command-argument-multiplier-base! n)
  (if (not (and (integer? n) (not (negative? n))))
      (error "Multiplier Base" n "must be a non-negative integer."))
  (set! *multiplier-base* n))

;; **** Kludge ****
(set-command-argument-multiplier-base! 4)

;;;; Autoargument Mode

(define *autoargument-mode?*)

(define (enter-autoargument-mode!)
  (set! *autoargument-mode?* true))

(define (autoargument-mode?)
  *autoargument-mode?*)

;;;; Value

(set! command-argument-standard-value
(named-lambda (command-argument-standard-value)
  (or (command-argument-value)
      (and *negative?* -1))))

(set! command-argument-value
(named-lambda (command-argument-value)
  ;; This returns the numeric value of the argument, or false if none.
  (cond (*magnitude*
	 (* (if *negative?* (- *magnitude*) *magnitude*)
	    (expt *multiplier-base* *multiplier-exponent*)))
	((not (zero? *multiplier-exponent*))
	 (if *negative?*
	     (- (expt *multiplier-base* *multiplier-exponent*))
	     (expt *multiplier-base* *multiplier-exponent*)))
	(else false))))

(set! command-argument-multiplier-only?
(named-lambda (command-argument-multiplier-only?)
  (and (not *magnitude*)
       (not (zero? *multiplier-exponent*))
       *multiplier-exponent*)))

(set! command-argument-negative-only?
(named-lambda (command-argument-negative-only?)
  (and (not *magnitude*)
       (zero? *multiplier-exponent*)
       *negative?*)))

(set! command-argument-beginning?
(named-lambda (command-argument-beginning?)
  (and (not *magnitude*)
       (not *negative?*)
       (< *multiplier-exponent* 2))))

;;; end COMMAND-ARGUMENT-PACKAGE
))

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: (access command-argument-package edwin-package)
;;; Scheme Syntax Table: (access edwin-syntax-table edwin-package)
;;; Tags Table Pathname: (access edwin-tags-pathname edwin-package)
;;; End:
