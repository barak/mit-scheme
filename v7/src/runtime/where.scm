;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/where.scm,v 13.42 1987/03/17 18:55:18 cph Rel $
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
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

;;;; Environment Inspector

(in-package debugger-package

(declare (usual-integrations))

(define env-package
  (let ((env)
	(current-frame)
	(current-frame-depth)
	(env-commands (make-command-set 'WHERE-COMMANDS)))

(define (define-where-command letter function help-text)
  (define-letter-command env-commands letter function help-text))

;;; Basic Commands

(define-where-command #\? (standard-help-command env-commands)
  "Help, list command letters")

(define-where-command #\Q standard-exit-command
  "Quit (exit from Where)")

;;; Lexpr since it can take one or no arguments

(define (where #!optional env-spec)
  (if (unassigned? env-spec) (set! env-spec (rep-environment)))
  (let ((environment
	 (cond ((or (eq? env-spec system-global-environment)
		    (environment? env-spec))
		env-spec)
	       ((compound-procedure? env-spec)
		(procedure-environment env-spec))
	       ((delayed? env-spec)
		(if (delayed-evaluation-forced? env-spec)
		    (error "Not a valid environment, already forced"
			   (list where env-spec))
		    (delayed-evaluation-environment env-spec)))
	       (else
		(error "Not a legal environment object" 'WHERE
		       env-spec)))))
    (environment-warning-hook environment)
    (fluid-let ((env environment)
		(current-frame environment)
		(current-frame-depth 0))
      (letter-commands env-commands
		       (standard-rep-message "Environment Inspector")
		       (standard-rep-prompt "Where-->")))))

;;;; Display Commands

(define (show)
  (show-frame current-frame current-frame-depth))

(define (show-all)
  (let s1 ((env env)
	   (depth 0))
    (if (eq? system-global-environment env)
	*the-non-printing-object*
	(begin (show-frame env depth)
	       (if (environment-has-parent? env)
		   (s1 (environment-parent env) (1+ depth))
		   *the-non-printing-object*)))))

(define (show-frame frame depth)
  (if (eq? system-global-environment frame)
      (begin (newline)
	     (write-string "This frame is the system global environment"))
      (begin (newline) (write-string "Frame created by ")
	     (print-user-friendly-name frame)
	     (if (>= depth 0)
		 (begin (newline)
			(write-string "Depth (relative to starting frame): ")
			(write depth)))
	     (newline)
	     (let ((bindings (environment-bindings frame)))
	       (if (null? bindings)
		   (write-string "Has no bindings")
		   (begin (write-string "Has bindings:")
			  (newline)
			  (for-each print-binding bindings))))))
  (newline))

(define print-user-friendly-name
  (let ((rename-list
	 `((,lambda-tag:unnamed . LAMBDA)
	   (,(access internal-lambda-tag lambda-package) . LAMBDA)
	   (,(access internal-lexpr-tag lambda-package) . LAMBDA)
	   (,lambda-tag:let . LET)
	   (,lambda-tag:shallow-fluid-let . FLUID-LET)
	   (,lambda-tag:deep-fluid-let . FLUID-LET)
	   (,lambda-tag:common-lisp-fluid-let . FLUID-BIND)
	   (,lambda-tag:make-environment . MAKE-ENVIRONMENT))))
    (lambda (frame)
      (let ((name (environment-name frame)))
	(let ((rename (assq name rename-list)))
	  (if rename
	      (begin (write-string "a ")
		     (write (cdr rename))
		     (write-string " special form"))
	      (begin (write-string "the procedure ")
		     (write name))))))))

(define (print-binding binding)
  (define line-width 79)
  (define name-width 40)
  (define (truncate str length)
    (set-string-length! str (- length 4))
    (string-append str " ..."))
  (newline)
  (let ((s (write-to-string (car binding) name-width)))
    (if (car s)		      ; Name was truncated
	(set! s (truncate (cdr s) name-width))
	(set! s (cdr s)))
    (if (null? (cdr binding))
	(set! s (string-append s " is unassigned"))
	(let ((s1 (write-to-string (cadr binding)
				   (- line-width (string-length s)))))
	  (set! s (string-append s " = " (cdr s1)));
	  (if (car s1)	      ; Value truncated
	      (set! s (truncate s line-width)))))
    (write-string s)))

(define-where-command #\C show
  "Display the bindings in the current frame")

(define-where-command #\A show-all
  "Display the bindings of all the frames in the current chain")

;;;; Motion Commands

(define (parent)
  (cond ((eq? system-global-environment current-frame)
	 (newline)
	 (write-string 
"The current frame is the system global environment, it has no parent."))
	((environment-has-parent? current-frame)
	 (set! current-frame (environment-parent current-frame))
	 (set! current-frame-depth (1+ current-frame-depth))
	 (show))
	(else
	 (newline)
	 (write-string "The current frame has no parent."))))


(define (son)
  (cond ((eq? current-frame env)
	 (newline)
	 (write-string "This is the original frame.  Its children cannot be found."))
	(else
	 (let son-1 ((prev env)
		     (prev-depth 0)
		     (next (environment-parent env)))
	   (if (eq? next current-frame)
	       (begin (set! current-frame prev)
		      (set! current-frame-depth prev-depth))
	       (son-1 next
		      (1+ prev-depth)
		      (environment-parent next))))
	 (show))))

(define (recursive-where)
  (write-string "; Object to eval and examine-> ")
  (let ((inp (read)))
    (write-string "New where!")
    (where (eval inp current-frame))))

(define-where-command #\P parent
  "Find the parent frame of the current one")

(define-where-command #\S son
  "Find the son of the current environment in the current chain")

(define-where-command #\W recursive-where
  "Eval an expression in the current frame and do WHERE on it")

;;;; Relative Evaluation Commands

(define (show-object)
  (write-string "; Object to eval and print-> ")
  (let ((inp (read)))
    (newline)
    (write (eval inp current-frame))
    (newline)))

(define (enter)
  (read-eval-print current-frame
		   "You are now in the desired environment"
		   "Eval-in-env-->"))

(define-where-command #\V show-object
  "Eval an expression in the current frame and print the result")

(define-where-command #\E enter
  "Create a read-eval-print loop in the current environment")

;;;; Miscellaneous Commands

(define (name)
  (newline)
  (write-string "This frame was created by ")
  (print-user-friendly-name current-frame))

(define-where-command #\N name
  "Name of procedure which created current environment")

;;; end ENV-PACKAGE.
(the-environment)))

(define print-user-friendly-name
  (access print-user-friendly-name env-package))

;;; end IN-PACKAGE DEBUGGER-PACKAGE.
)

;;;; Exports

(define where
  (access where env-package debugger-package))