;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/argred.scm,v 1.30 1991/05/02 01:11:56 cph Exp $
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

;;;; Command Argument Reader

(declare (usual-integrations))

(define-command universal-argument
  "Begin a numeric argument for the following command.
Digits or minus sign following this command make up the numeric argument.
If no digits or minus sign follow, this command by itself provides 4 as argument.
Used more than once, this command multiplies the argument by 4 each time."
  "P"
  (lambda (argument)
    (set-command-argument! (list (* (if (pair? argument) (car argument) 1) 4)))
    (set-command-message! 'AUTO-ARGUMENT (char-name (last-command-char)))))

(define-command digit-argument
  "Part of the numeric argument for the next command."
  "P"
  (lambda (argument)
    (let ((digit (char->digit (char-base (last-command-char)))))
      (if digit
	  (begin
	    (set-command-argument!
	     (cond ((eq? '- argument) (- digit))
		   ((not (number? argument)) digit)
		   ((negative? argument) (- (* 10 argument) digit))
		   (else (+ (* 10 argument) digit))))
	    (set-command-message! 'AUTO-ARGUMENT (auto-argument-mode?)))))))

(define-command negative-argument
  "Begin a negative numeric argument for the next command."
  "P"
  (lambda (argument)
    (set-command-argument!
     (cond ((eq? '- argument) false)
	   ((number? argument) (- argument))
	   (else '-)))
    (set-command-message! 'AUTO-ARGUMENT (auto-argument-mode?))))

(define-command auto-digit-argument
  "When reading a command argument, part of the numeric argument.
Otherwise, the digit inserts itself."
  "P"
  (lambda (argument)
    (if (auto-argument-mode?)
	((ref-command digit-argument) argument)
	((ref-command self-insert-command) argument))))

(define-command auto-negative-argument
  "When reading a command argument, begin a negative argument.
Otherwise, the character inserts itself."
  "P"
  (lambda (argument)
    (if (and (auto-argument-mode?)
	     (not (number? argument)))
	((ref-command negative-argument) argument)
	((ref-command self-insert-command) argument))))

(define-command auto-argument
  "Start a command argument.
Digits following this command become part of the argument."
  "P"
  (lambda (argument)
    (if (char=? #\- (char-base (last-command-char)))
	(if (not (number? argument))
	    ((ref-command negative-argument) argument))
	((ref-command digit-argument) argument))
    (if (not argument)
	(set-command-message! 'AUTO-ARGUMENT true))))

(define (command-argument-self-insert? command)
  (and (or (eq? command (ref-command-object auto-digit-argument))
	   (and (eq? command (ref-command-object auto-negative-argument))
		(not (number? (command-argument)))))
       (not (auto-argument-mode?))))

(define (auto-argument-mode?)
  (command-message-receive 'AUTO-ARGUMENT (lambda (x) x) (lambda () false)))

(define (command-argument-prompt)
  (let ((arg (command-argument)))
    (if (not arg)
	""
	(let ((mode (auto-argument-mode?)))
	  (string-append
	   (if (and (pair? arg) (string? mode))
	       (let loop ((n (car arg)))
		 (if (= n 4)
		     mode
		     (string-append mode " " (loop (quotient n 4)))))
	       (string-append
		(cond ((string? mode) mode)
		      (mode "Autoarg")
		      (else "Arg"))
		" "
		(if (eq? '- arg)
		    "-"
		    (number->string (if (pair? arg) (car arg) arg)))))
	   " -")))))

(define (command-argument-multiplier-only? argument)
  (pair? argument))

(define (command-argument-negative-only? argument)
  (eq? '- argument))

(define (command-argument-value argument)
  (cond ((not argument) false)
	((eq? '- argument) -1)
	((pair? argument) (car argument))
	(else argument)))

(define (command-argument-numeric-value argument)
  (or (command-argument-value argument) 1))