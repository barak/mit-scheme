;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/debug.scm,v 13.44 1987/12/05 16:40:13 cph Exp $
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
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Debugger

(in-package debugger-package
(declare (usual-integrations))

(define debug-package
  (make-environment

(define current-continuation)
(define previous-continuations)
(define current-reduction-number)
(define current-number-of-reductions)
(define current-reduction)
(define current-environment)

(define command-set
  (make-command-set 'DEBUG-COMMANDS))

(define reduction-wrap-around-tag
  'WRAP-AROUND)

(define print-user-friendly-name
  (access print-user-friendly-name env-package))

(define print-expression
  pp)

(define student-walk?
  false)

(define print-return-values?
  false)

(define environment-arguments-truncation
  68)

(define (define-debug-command letter function help-text)
  (define-letter-command command-set letter function help-text))

;;; Basic Commands

(define-debug-command #\? (standard-help-command command-set)
		      "Help, list command letters")

(define-debug-command #\Q standard-exit-command "Quit (exit DEBUG)")

(define (debug #!optional the-continuation)
  (fluid-let ((current-continuation)
	      (previous-continuations '())
	      (current-reduction-number)
	      (current-number-of-reductions)
	      (current-reduction false)
	      (current-environment '()))
    (debug-abstract-continuation
     (cond ((unassigned? the-continuation) (rep-continuation))
	   ((raw-continuation? the-continuation); Must precede next test!
	    (raw-continuation->continuation the-continuation))
	   ((continuation? the-continuation) the-continuation)
	   (else (error "DEBUG: Not a continuation" the-continuation))))))

(define (debug-abstract-continuation continuation)
  (set-current-continuation! continuation initial-reduction-number)
  (letter-commands command-set
		   (lambda ()
		     (print-current-expression)
		     ((standard-rep-message "Debugger")))
		   "Debug-->"))

(define (undefined-environment? environment)
  (or (continuation-undefined-environment? environment)
      (eq? environment system-global-environment)
      (and (environment? environment)
	   ((access system-external-environment? environment-package)
	    environment))))

(define (print-undefined-environment)
  (format "~%Undefined environment at this subproblem/reduction level"))

(define (with-rep-alternative env receiver)
  (if (undefined-environment? env)
      (begin
       (print-undefined-environment)
       (format "~%Using the read-eval-print environment instead!")
       (receiver (rep-environment)))
      (receiver env)))

(define (if-valid-environment env receiver)
  (if (undefined-environment? env)
      (print-undefined-environment)
      (receiver env)))

(define (current-expression)
   (if current-reduction
       (reduction-expression current-reduction)
       (let ((exp (continuation-expression current-continuation)))
	 (if (or (not (continuation-undefined-expression? exp))
		 (null? (continuation-annotation current-continuation)))
	     exp
	     (cons 'UNDEFINED-EXPRESSION
		   (continuation-annotation current-continuation))))))

;;;; Random display commands

(define (pretty-print-current-expression)
  (print-expression (current-expression)))

(define-debug-command #\L pretty-print-current-expression
  "(list expression) Pretty-print the current expression")

(define (pretty-print-reduction-function)
  (if-valid-environment (if current-reduction
			    (reduction-environment current-reduction)
			    current-environment)
			(lambda (env) (pp (environment-procedure env)))))

(define-debug-command #\P pretty-print-reduction-function
  "Pretty print current procedure")

(define (print-current-expression)
  (define (print-current-reduction)
    (format "~2xReduction Number:~x~o~%Expression:" current-reduction-number)
    (print-expression (reduction-expression current-reduction)))

  (define (print-application-information env)
    (let ((do-it
	   (lambda (return?)
	     (if return? (newline))
	     (write-string "within ")
	     (print-user-friendly-name env)
	     (if return? (newline))
	     (write-string " applied to ")
	     (write-string
	      (cdr (write-to-string (environment-arguments env)
				    environment-arguments-truncation))))))
      (let ((output (with-output-to-string (lambda () (do-it false)))))
	(if (< (string-length output)
	       (access printer-width implementation-dependencies))
	    (begin (newline) (write-string output))
	    (do-it true)))))

  (newline)
  (if (null-continuation? current-continuation)
      (write-string "Null continuation")
      (begin
	(write-string "Subproblem Level: ")
	(write (length previous-continuations))
	(if current-reduction
	    (print-current-reduction)
	    (begin
	      (newline)
	      (write-string "Possibly Incomplete Expression:")
	      (print-expression
	       (continuation-expression current-continuation))))
	(if-valid-environment current-environment
			      print-application-information))))

(define-debug-command #\S print-current-expression
  "Print the current subproblem/reduction")

(define (reductions-command)
  (if (null-continuation? current-continuation)
      (format "~%Null continuation")
      (let loop ((r (continuation-reductions current-continuation)))
	(cond ((pair? r)
	       (print-expression (reduction-expression (car r)))
	       (loop (cdr r)))
	      ((wrap-around-in-reductions? r)
	       (format "~%Wrap Around in the reductions at this level."))
	      (else 'done)))))

(define-debug-command #\R reductions-command
  "Print the reductions of the current subproblem level")

;;;; Short history display

(define (summarize-history-command)
  (define (print-continuations cont level)
    (define (print-reductions reductions show-all?)
      (define (print-reduction red number)
	(terse-print-expression level
				(reduction-expression red)
				(reduction-environment red)))
      
      (let loop ((reductions reductions) (number 0))
	   (if (pair? reductions)
	       (begin
		(print-reduction (car reductions) number)
		(if show-all? (loop (cdr reductions) (1+ number)))))))

    (if (null-continuation? cont)
	*the-non-printing-object*
	(begin
	 (let ((reductions (continuation-reductions cont)))
	   (if (not (pair? reductions))
	       (terse-print-expression level
				       (continuation-expression cont)
				       (continuation-environment cont))
	       (print-reductions reductions (= level 0))))
	 (print-continuations (continuation-next-continuation cont)
			      (1+ level)))))

  (let ((top-continuation (if (null? previous-continuations)
			      current-continuation
			      (car (last-pair previous-continuations)))))
    (if (null-continuation? top-continuation)
	(format "~%No history available")
	(begin
	 (format "~%Sub Prb. Procedure Name    Expression~%")
	 (print-continuations top-continuation 0)))))

(define (terse-print-expression level expression environment)
  (format "~%~@3o~:20o~4x~@:52c"
	  level
	  ;; procedure name
	  (if (or (undefined-environment? environment)
		  (special-name? (environment-name environment)))
	      *the-non-printing-object*
	      (environment-name environment))
	  expression))

(define-debug-command #\H summarize-history-command
  "Prints a summary of the entire history")

;;;; Motion to earlier expressions

(define (earlier-reduction)
  (define (up! message)
    (format "~%~s~%Going to the previous (earlier) continuation!" message)
    (earlier-continuation-command))
  
  (cond ((and student-walk?
	      (> (length previous-continuations) 0)
	      (= current-reduction-number 0))
	 (earlier-continuation-command))
	((< current-reduction-number (-1+ current-number-of-reductions))
	 (set-current-reduction! (1+ current-reduction-number))
	 (print-current-expression))
	((wrap-around-in-reductions?
	  (continuation-reductions current-continuation))
	 (up! "Wrap around in reductions at this level!"))
	(else (up! "No more reductions at this level!"))))

(define-debug-command #\B earlier-reduction "Earlier reduction (Back in time)")

(define (earlier-subproblem)
  (let ((new (continuation-next-continuation current-continuation)))
    (set! previous-continuations
	  (cons current-continuation previous-continuations))
    (set-current-continuation! new normal-reduction-number)))

(define (earlier-continuation-command)
  (if (not (null-continuation? (continuation-next-continuation
				current-continuation)))
      (earlier-subproblem)
      (format "~%There are only ~o subproblem levels"
	      (length previous-continuations)))
  (print-current-expression))

(define-debug-command #\U earlier-continuation-command
  "Move (Up) to the previous (earlier) continuation")

;;;; Motion to later expressions

(define (later-reduction)
  (cond ((> current-reduction-number 0)
	 (set-current-reduction! (-1+ current-reduction-number))
	 (print-current-expression))
	((or (not student-walk?)
	     (= (length previous-continuations) 1))
	 (later-continuation-TO-LAST-REDUCTION))
	(else (later-continuation))))

(define-debug-command #\F later-reduction "Later reduction (Forward in time)")

(define (later-continuation)
  (if (null? previous-continuations)
      (format "~%Already at lowest subproblem level")
      (begin (later-subproblem) (print-current-expression))))

(define (later-continuation-TO-LAST-REDUCTION)
  (define (later-subproblem-TO-LAST-REDUCTION)
    (set-current-continuation!
     (car (set! previous-continuations (cdr previous-continuations)))
     last-reduction-number))

  (if (null? previous-continuations)
      (format "~%Already at lowest subproblem level")
      (begin (later-subproblem-TO-LAST-REDUCTION)
	     (print-current-expression))))

(define (later-subproblem)
  (set-current-continuation!
   (car (set! previous-continuations (cdr previous-continuations)))
   normal-reduction-number))

(define (later-continuation-command)
  (if (null? previous-continuations)
      (format "~%Already at oldest continuation")
      (begin (later-subproblem) (print-current-expression))))

(define-debug-command #\D later-continuation-command
  "Move (Down) to the next (later) continuation")

;;;; General motion command

(define (goto-command)
  (define (get-reduction-number)
    (let ((red
	   (prompt-for-expression
	    (format false
		    "Reduction Number (0 through ~o inclusive): "
		    (-1+ current-number-of-reductions)))))
      (cond ((not (number? red))
	     (beep)
	     (format "~%Reduction number must be numeric!")
	     (get-reduction-number))
	    ((not (and (>= red 0)
		       (< red current-number-of-reductions)))
	     (format "~%Reduction number out of range.!")
	     (get-reduction-number))
	    (else (set-current-reduction! red)))))

  (define (choose-reduction)
    (cond ((> current-number-of-reductions 1) (get-reduction-number))
	  ((= current-number-of-reductions 1)
	   (format "~%There is only one reduction for this subproblem")
	   (set-current-reduction! 0))
	  (else (format "~%There are no reductions for this subproblem."))))
  
  (define (get-subproblem-number)
    (let ((len (length previous-continuations))
	  (sub (prompt-for-expression "Subproblem number: ")))
      (cond ((not (number? sub))
	     (beep)
	     (format "~%Subproblem level must be numeric!")
	     (get-subproblem-number))
	    ((< sub len) (repeat later-subproblem (- len sub))
			 (choose-reduction))
	    (else
	     (let loop ((len len))
	       (cond ((= sub len) (choose-reduction))
		     ((null-continuation?
		       (continuation-next-continuation current-continuation))
		      (format "~%There is no such subproblem.")
		      (format "~%Now at subproblem number: ~o"
			      (length previous-continuations))
		      (choose-reduction))
		     (else (earlier-subproblem) (loop (1+ len)))))))))

  (get-subproblem-number)
  (print-current-expression))

(define-debug-command #\G goto-command
  "Go to a particular Subproblem/Reduction level")

;;;; Evaluation and frame display commands

(define (enter-read-eval-print-loop)
  (with-rep-alternative current-environment
    (lambda (env)
      (debug/read-eval-print env
			     "You are now in the desired environment"
			     "Eval-in-env-->"))))

(define-debug-command #\E enter-read-eval-print-loop
  "Enter a read-eval-print loop in the current environment")

(define (eval-in-current-environment)
  (with-rep-alternative current-environment
    (lambda (env)
      (environment-warning-hook env)
      (debug/eval (prompt-for-expression "Eval--> ") env))))

(define-debug-command #\V eval-in-current-environment
  "Evaluate expression in current environment")

(define show-current-frame
  (let ((show-frame (access show-frame env-package)))
    (named-lambda (show-current-frame)
      (if-valid-environment current-environment
			    (lambda (env) (show-frame env -1))))))

(define-debug-command #\C show-current-frame
  "Show Bindings of identifiers in the current environment")

(define (enter-where-command)
  (with-rep-alternative current-environment debug/where))

(define-debug-command #\W enter-where-command
  "Enter WHERE on the current environment")

(define (error-info-command)
  (format "~% Message: ~s~%Irritant: ~o" (error-message) (error-irritant)))

(define-debug-command #\I error-info-command "Redisplay the error message")

;;;; Advanced hacking commands

(define (return-command)		;command Z
  (define (do-it environment next)
    (environment-warning-hook environment)
    (let ((value
	   (debug/eval
	    (let ((expression
		   (prompt-for-expression
		    "Expression to EVALUATE and CONTINUE with ($ to retry): "
		    )))
	      (if (eq? expression '$)
		  (unsyntax (current-expression))
		  expression))
	    environment)))
      (if print-return-values?
	  (begin
	    (format "~%That evaluates to:~%~o" value)
	    (if (prompt-for-confirmation "Confirm: ") (next value)))
	  (next value))))

  (let ((next (continuation-next-continuation current-continuation)))
    (if (null-continuation? next)
	(begin (beep) (format "~%Can't continue!!!"))
	(with-rep-alternative current-environment
			      (lambda (env) (do-it env next))))))

(define-debug-command #\Z return-command
  "Return (continue with) an expression after evaluating it")

(define user-debug-environment (make-environment))

(define (internal-command)
  (debug/read-eval-print user-debug-environment
			 "You are now in the debugger environment"
			 "Debugger-->"))

(define-debug-command #\X internal-command
  "Create a read eval print loop in the debugger environment")

;;;; Reduction and continuation motion low-level

(define reduction-expression car)
(define reduction-environment cadr)

(define (last-reduction-number)
  (-1+ current-number-of-reductions))

(define (normal-reduction-number)
  (min (-1+ current-number-of-reductions) 0))

(define (initial-reduction-number)
   (let ((environment (continuation-environment current-continuation)))
     (if (and (environment? environment)
	      (let ((procedure (environment-procedure environment)))
		(or (eq? procedure error-procedure)
		    (eq? procedure breakpoint-procedure))))
	 1
	 0)))

(define (set-current-continuation! continuation hook)
  (set! current-continuation continuation)
  (set! current-number-of-reductions
	(if (null-continuation? continuation)
	    0
	    (dotted-list-length
	     (continuation-reductions current-continuation))))
  (set-current-reduction! (hook)))

(define (set-current-reduction! number)
  (set! current-reduction-number number)
  (if (and (not (= current-number-of-reductions 0)) (>= number 0))
      (set! current-reduction 
	    (list-ref (continuation-reductions current-continuation) number))
      (set! current-reduction false))
  (set! current-environment 
	(if current-reduction
	    (reduction-environment current-reduction)
	    (continuation-environment current-continuation))))

(define (repeat f n)
  (if (> n 0)
      (begin (f)
	     (repeat f (-1+ n)))))

(define (dotted-list-length l)
  (let count ((n 0) (L L))
    (if (pair? l)
	(count (1+ n) (CDR L))
	n)))

(define (wrap-around-in-reductions? reductions)
  (eq? (list-tail reductions (dotted-list-length reductions))
       reduction-wrap-around-tag))

;;; end DEBUG-PACKAGE.
))

;;; end IN-PACKAGE DEBUGGER-PACKAGE.
)

(define debug
  (access debug debug-package debugger-package))

(define special-name?
  (let ((the-special-names
	 (list lambda-tag:unnamed
	       (access internal-lambda-tag lambda-package)
	       (access internal-lexpr-tag lambda-package)
	       lambda-tag:let
	       lambda-tag:shallow-fluid-let
	       lambda-tag:deep-fluid-let
	       lambda-tag:common-lisp-fluid-let
	       lambda-tag:make-environment)))
    (named-lambda (special-name? symbol)
      (memq symbol the-special-names))))