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

;;;; Command Reader

(declare (usual-integrations)
	 (integrate-external "edb:curren.bin.0"))
(using-syntax (access edwin-syntax-table edwin-package)

(define (top-level-command-reader)
  (fluid-let ((*auto-save-keystroke-count* 0))
    (define (^G-loop)
      (with-keyboard-macro-disabled
       (lambda ()
	 (call-with-current-continuation
	   (lambda (continuation)
	     (fluid-let ((*^G-interrupt-continuation* continuation))
	       (command-reader))))))
      (^G-loop))
    (^G-loop)))

(define command-reader)
(define execute-char)
(define execute-command)
(define read-and-dispatch-on-char)
(define dispatch-on-char)
(define dispatch-on-command)
(define abort-current-command)
(define current-command-char)
(define current-command)
(define set-command-message!)
(define command-message-receive)

(define command-reader-package
  (make-environment

(define *command-continuation*)	;Continuation of current command
(define *command-char*)		;Character read to find current command
(define *command*)		;The current command
(define *command-message*)	;Message from last command
(define *next-message*)		;Message to next command
(define *non-undo-count*)	;# of self-inserts since last undo boundary

(let ()

(set! command-reader
(named-lambda (command-reader)
  (fluid-let ((*command-message*)
	      (*non-undo-count* 0))
    (with-command-argument-reader command-reader-loop))))

(define (command-reader-loop)
  (let ((value
	 (call-with-current-continuation
	  (lambda (continuation)
	    (fluid-let ((*command-continuation* continuation)
			(*command-char*)
			(*command*)
			(*next-message* false))
	      (start-next-command))))))
    (if (not (eq? value 'ABORT)) (value)))
  (command-reader-loop))

(define (start-next-command)
  (reset-command-state!)
  (let ((window (current-window))
	(char (keyboard-read-char)))
    (set! *command-char* char)
    (set-command-prompt! (char->name char))
    (%dispatch-on-command
     window
     (comtab-entry (buffer-comtabs (window-buffer window)) char)))
  (start-next-command))

)

(define (reset-command-state!)
  (reset-command-argument-reader!)
  (reset-command-prompt!)
  (set! *command-message* *next-message*)
  (set! *next-message* false)
  (if *defining-keyboard-macro?* (keyboard-macro-finalize-chars)))

;;; The procedures for executing commands come in two flavors.  The
;;; difference is that the EXECUTE-foo procedures reset the command
;;; state first, while the DISPATCH-ON-foo procedures do not.  The
;;; latter should only be used by "prefix" commands such as C-X or
;;; C-3, since they want arguments, messages, etc. to be passed on.

(set! execute-char
(named-lambda (execute-char comtab char)
  (reset-command-state!)
  (dispatch-on-char comtab char)))

(set! execute-command
(named-lambda (execute-command command)
  (reset-command-state!)
  (dispatch-on-command command)))

(set! read-and-dispatch-on-char
(named-lambda (read-and-dispatch-on-char)
  (dispatch-on-char (current-comtab) (keyboard-read-char))))

(set! dispatch-on-char
(named-lambda (dispatch-on-char comtab char)
  (set! *command-char* char)
  (set-command-prompt!
   (string-append-separated (command-argument-prompt)
			    (xchar->name char)))
  (dispatch-on-command (comtab-entry comtab char))))

(set! dispatch-on-command
(named-lambda (dispatch-on-command command)
  (%dispatch-on-command (current-window) command)))

(define (%dispatch-on-command window command)
  (set! *command* command)
  (let ((procedure (command-procedure command))
	(argument (command-argument-standard-value)))
    (if (or argument
	    *executing-keyboard-macro?*
	    (window-needs-redisplay? window))
	(begin (set! *non-undo-count* 0)
	       (procedure argument))
	(cond ((or (eq? procedure ^r-insert-self-command)
		   (and (eq? procedure ^r-auto-fill-space-command)
			(not (auto-fill-break? (current-point))))
		   (command-argument-self-insert? procedure))
	       (let ((point (window-point window)))
		 (if (and (buffer-auto-save-modified? (window-buffer window))
			  (null? (cdr (buffer-windows (window-buffer window))))
			  (line-end? point)
			  (char-graphic? *command-char*)
			  (< (window-point-x window)
			     (-1+ (window-x-size window))))
		     (begin (if (or (zero? *non-undo-count*)
				    (>= *non-undo-count* 20))
				(begin (undo-boundary! point)
				       (set! *non-undo-count* 0)))
			    (set! *non-undo-count* (1+ *non-undo-count*))
			    (window-direct-output-insert-char! window
							       *command-char*))
		     (region-insert-char! point *command-char*))))
	      ((eq? procedure ^r-forward-character-command)
	       (let ((point (window-point window)))
		 (if (and (not (group-end? point))
			  (char-graphic? (mark-right-char point))
			  (< (window-point-x window)
			     (- (window-x-size window) 2)))
		     (window-direct-output-forward-char! window)
		     (procedure argument))))
	      ((eq? procedure ^r-backward-character-command)
	       (let ((point (window-point window)))
		 (if (and (not (group-start? point))
			  (char-graphic? (mark-left-char point))
			  (positive? (window-point-x window)))
		     (window-direct-output-backward-char! window)
		     (procedure argument))))
	      (else
	       (if (not (typein-window? window))
		   (undo-boundary! (window-point window)))
	       (set! *non-undo-count* 0)
	       (procedure argument))))))

(set! abort-current-command
(named-lambda (abort-current-command #!optional value)
  (if (unassigned? value) (set! value 'ABORT))
  (keyboard-macro-disable)
  (*command-continuation* value)))

(set! current-command-char
(named-lambda (current-command-char)
  *command-char*))

(set! current-command
(named-lambda (current-command)
  *command*))

(set! set-command-message!
(named-lambda (set-command-message! tag . arguments)
  (set! *next-message* (cons tag arguments))))

(set! command-message-receive
(named-lambda (command-message-receive tag if-received if-not-received)
  (if (and *command-message*
	   (eq? (car *command-message*) tag))
      (apply if-received (cdr *command-message*))
      (if-not-received))))

;;; end COMMAND-READER-PACKAGE
))

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: (access command-reader-package edwin-package)
;;; Scheme Syntax Table: (access edwin-syntax-table edwin-package)
;;; Tags Table Pathname: (access edwin-tags-pathname edwin-package)
;;; End:
