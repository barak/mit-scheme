;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/rep.scm,v 13.44 1988/04/26 19:41:15 cph Exp $
;;;
;;;	Copyright (c) 1988 Massachusetts Institute of Technology
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

;;;; Read-Eval-Print Loop

(declare (usual-integrations))

;;;; Command Loops

(define make-command-loop)
(define push-command-loop)
(define push-command-hook)
(define with-rep-continuation)
(define continue-rep)
(define rep-continuation)
(define rep-state)
(define rep-level)
(define abort->nearest)
(define abort->previous)
(define abort->top-level)
(let ()

(define top-level-driver-hook)
(define previous-driver-hook)
(define nearest-driver-hook)
(define current-continuation)
(define current-state)
(define current-level 0)

;; PUSH-COMMAND-HOOK is provided so that the Butterfly, in particular,
;; can add its own little code just before creating a REP loop
(set! push-command-hook
  (lambda (startup driver state continuation)
    (continuation startup driver state (lambda () 'ignore))))

(set! make-command-loop
      (named-lambda (make-command-loop message driver)
	(define (driver-loop message)
	  (driver-loop
	   (with-rep-continuation
	    (lambda (quit)
	      (set! top-level-driver-hook quit)
	      (set! nearest-driver-hook quit)
	      (driver message)))))
	(set-interrupt-enables! interrupt-mask-gc-ok)
	(fluid-let ((top-level-driver-hook)
		    (nearest-driver-hook))
	  (driver-loop message))))

(set! push-command-loop
(named-lambda (push-command-loop startup-hook driver initial-state)
  (define (restart entry-hook each-time)
    (let ((reentry-hook
	   (call-with-current-continuation
	    (lambda (again)
	      (set! nearest-driver-hook again)
	      (set-interrupt-enables! interrupt-mask-all)
	      (each-time)
	      (entry-hook)
	      (loop)))))
      (set-interrupt-enables! interrupt-mask-gc-ok)
      (restart reentry-hook each-time)))

  (define (loop)
    (set! current-state (driver current-state))
    (loop))

  (fluid-let ((current-level (1+ current-level))
	      (previous-driver-hook nearest-driver-hook)
	      (nearest-driver-hook)
	      (current-state))
    (push-command-hook
     startup-hook driver initial-state
     (lambda (startup-hook driver initial-state each-time)
       (set! current-state initial-state)
       (restart startup-hook each-time))))))

(set! with-rep-continuation
(named-lambda (with-rep-continuation receiver)
  (call-with-current-continuation
   (lambda (raw-continuation)
     (let ((continuation (raw-continuation->continuation raw-continuation)))
       (fluid-let ((current-continuation continuation))
	 (receiver continuation)))))))

(set! continue-rep
(named-lambda (continue-rep value)
  (current-continuation
   (if (eq? current-continuation top-level-driver-hook)
       (lambda ()
	 (write-line value))
       value))))

(set! abort->nearest
(named-lambda (abort->nearest message)
  (nearest-driver-hook message)))

(set! abort->previous
(named-lambda (abort->previous message)
  ((if (null? previous-driver-hook)
       nearest-driver-hook
       previous-driver-hook)
   message)))

(set! abort->top-level
(named-lambda (abort->top-level message)
  (top-level-driver-hook message)))

(set! rep-continuation
(named-lambda (rep-continuation)
  current-continuation))

(set! rep-state
(named-lambda (rep-state)
  current-state))

(set! rep-level
(named-lambda (rep-level)
  current-level))

) ; LET

;;;; Read-Eval-Print Loops

(define *rep-base-environment*)
(define *rep-current-environment*)
(define *rep-base-syntax-table*)
(define *rep-current-syntax-table*)
(define *rep-base-prompt*)
(define *rep-current-prompt*)
(define *rep-base-input-port*)
(define *rep-current-input-port*)
(define *rep-base-output-port*)
(define *rep-current-output-port*)
(define *rep-keyboard-map*)
(define *rep-error-hook*)

(define (rep-environment)
  *rep-current-environment*)

(define (rep-base-environment)
  *rep-base-environment*)

(define (set-rep-environment! environment)
  (set! *rep-current-environment* environment)
  (environment-warning-hook *rep-current-environment*))

(define (set-rep-base-environment! environment)
  (set! *rep-base-environment* environment)
  (set! *rep-current-environment* environment)
  (environment-warning-hook *rep-current-environment*))

(define (rep-syntax-table)
  *rep-current-syntax-table*)

(define (rep-base-syntax-table)
  *rep-base-syntax-table*)

(define (set-rep-syntax-table! syntax-table)
  (set! *rep-current-syntax-table* syntax-table))

(define (set-rep-base-syntax-table! syntax-table)
  (set! *rep-base-syntax-table* syntax-table)
  (set! *rep-current-syntax-table* syntax-table))

(define (rep-prompt)
  *rep-current-prompt*)

(define (set-rep-prompt! prompt)
  (set! *rep-current-prompt* prompt))

(define (rep-base-prompt)
  *rep-base-prompt*)

(define (set-rep-base-prompt! prompt)
  (set! *rep-base-prompt* prompt)
  (set! *rep-current-prompt* prompt))

(define (rep-input-port)
  *rep-current-input-port*)

(define (rep-output-port)
  *rep-current-output-port*)

(define environment-warning-hook
  identity-procedure)

(define rep-read-hook
  read)

(define rep-value-hook
  write-line)

(define make-rep)
(define push-rep)
(define rep-eval-hook)
(define rep-value)
(define reader-history)
(define printer-history)
(let ()

(set! make-rep
(named-lambda (make-rep environment syntax-table prompt input-port output-port
			message)
  (fluid-let ((*rep-base-environment* environment)
	      (*rep-base-syntax-table* syntax-table)
	      (*rep-base-prompt* prompt)
	      (*rep-base-input-port* input-port)
	      (*rep-base-output-port* output-port)
	      (*rep-keyboard-map* (keyboard-interrupt-dispatch-table))
	      (*rep-error-hook* (access *error-hook* error-system)))
    (make-command-loop message rep-top-driver))))

(define (rep-top-driver message)
  (push-rep *rep-base-environment* message *rep-base-prompt*))

(set! push-rep
(named-lambda (push-rep environment message prompt)
  (fluid-let ((*rep-current-environment* environment)
	      (*rep-current-syntax-table* *rep-base-syntax-table*)
	      (*rep-current-prompt* prompt)
	      (*rep-current-input-port* *rep-base-input-port*)
	      (*rep-current-output-port* *rep-base-output-port*)
	      (*current-input-port* *rep-base-input-port*)
	      (*current-output-port* *rep-base-output-port*)
	      ((access *error-hook* error-system) *rep-error-hook*))
    (with-keyboard-interrupt-dispatch-table *rep-keyboard-map*
      (lambda ()
	(environment-warning-hook *rep-current-environment*)
	(push-command-loop message
			   rep-driver
			   (make-rep-state (make-history 5)
					   (make-history 10))))))))

(define (rep-driver state)
  (*rep-current-prompt*)
  (rep-value (rep-eval-hook (rep-read-hook)
			    *rep-current-environment*
			    *rep-current-syntax-table*))
  state)

(set! rep-eval-hook
  (named-lambda (rep-eval-hook s-expression environment syntax-table)
    (record-in-history! (rep-state-reader-history (rep-state)) s-expression)
    (with-new-history
     (let ((scode (syntax s-expression syntax-table)))
       (lambda () (scode-eval scode environment))))))

(set! rep-value
  (named-lambda (rep-value object)
    (record-in-history! (rep-state-printer-history (rep-state)) object)
    (rep-value-hook object)))

;;; History Manipulation

(define (make-history size)
  (let ((list (make-list size '())))
    (append! list list)
    (vector history-tag size list)))

(define history-tag
  '(REP-HISTORY))

(define (record-in-history! history object)
  (if (not (null? (vector-ref history 2)))
      (begin (set-car! (vector-ref history 2) object)
	     (vector-set! history 2 (cdr (vector-ref history 2))))))

(define (read-history history n)
  (if (not (and (integer? n)
		(not (negative? n))
		(< n (vector-ref history 1))))
      (error "Bad argument: READ-HISTORY" n))
  (list-ref (vector-ref history 2)
	    (- (-1+ (vector-ref history 1)) n)))

(define ((history-reader selector name) n)
  (let ((state (rep-state)))
    (if (rep-state? state)
	(read-history (selector state) n)
	(error "Not in REP loop" name))))

(define rep-state-tag
  "REP State")

(define (make-rep-state reader-history printer-history)
  (vector rep-state-tag reader-history printer-history))

(define (rep-state? object)
  (and (vector? object)
       (not (zero? (vector-length object)))
       (eq? (vector-ref object 0) rep-state-tag)))

(define rep-state-reader-history vector-second)
(define rep-state-printer-history vector-third)

(set! reader-history
      (history-reader rep-state-reader-history 'READER-HISTORY))

(set! printer-history
      (history-reader rep-state-printer-history 'PRINTER-HISTORY))

)