#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/debug.scm,v 14.6 1988/12/30 06:42:33 cph Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; Debugger
;;; package: (runtime debugger)

(declare (usual-integrations))

(define (initialize-package!)
  (set! command-set
	(make-command-set
	 'DEBUG-COMMANDS
	 `((#\? ,standard-help-command
		"Help, list command letters")
	   (#\A ,show-all-frames
		"Show bindings in current environment and its ancestors")
	   (#\B ,earlier-reduction-command
		"Earlier reduction (Back in time)")
	   (#\C ,show-current-frame
		"Show bindings of identifiers in the current environment")
	   (#\D ,later-subproblem-command
		"Move (Down) to the next (later) subproblem")
	   (#\E ,enter-read-eval-print-loop
		"Enter a read-eval-print loop in the current environment")
	   (#\F ,later-reduction-command
		"Later reduction (Forward in time)")
	   (#\G ,goto-command
		"Go to a particular Subproblem/Reduction level")
	   (#\H ,summarize-history-command
		"Prints a summary of the entire history")
	   (#\I ,error-info-command
		"Redisplay the error message")
	   (#\L ,pretty-print-current-expression
		"(list expression) Pretty-print the current expression")
	   (#\P ,pretty-print-reduction-function
		"Pretty print current procedure")
	   (#\Q ,standard-exit-command
		"Quit (exit DEBUG)")
	   (#\R ,reductions-command
		"Print the reductions of the current subproblem level")
	   (#\S ,print-current-expression
		"Print the current subproblem/reduction")
	   (#\U ,earlier-subproblem-command
		"Move (Up) to the previous (earlier) subproblem")
	   (#\V ,eval-in-current-environment
		"Evaluate expression in current environment")
	   (#\W ,enter-where-command
		"Enter WHERE on the current environment")
	   (#\X ,internal-command
		"Create a read eval print loop in the debugger environment")
	   (#\Z ,return-command
		"Return (continue with) an expression after evaluating it")
	   )))
  unspecific)

(define command-set)

;;; Basic Commands

(define current-subproblem)
(define previous-subproblems)
(define current-subproblem-number)
(define current-reduction-number)
(define current-reductions)
(define current-number-of-reductions)
(define current-reduction)
(define current-environment)
(define current-expression)

(define reduction-wrap-around-tag 'WRAP-AROUND)
(define student-walk? false)
(define print-return-values? false)
(define environment-arguments-truncation 68)

(define (debug #!optional object)
  (fluid-let ((current-subproblem)
	      (previous-subproblems)
	      (current-subproblem-number)
	      (current-reduction-number)
	      (current-reductions)
	      (current-number-of-reductions)
	      (current-reduction)
	      (current-environment)
	      (current-expression))
    (set-current-subproblem!
     (let ((object
	    (if (default-object? object)
		(or (error-continuation)
		    (current-proceed-continuation))
		object)))
       (or (coerce-to-stack-frame object)
	   (error "DEBUG: null continuation" object)))
     '()
     (lambda () 0))
    (letter-commands command-set
		     (cmdl-message/append
		      (cmdl-message/active print-current-expression)
		      (cmdl-message/standard "Debugger"))
		     "Debug-->")))

(define (coerce-to-stack-frame object)
  (cond ((stack-frame? object)
	 (stack-frame/skip-non-subproblems object))
	((continuation? object)
	 (coerce-to-stack-frame (continuation->stack-frame object)))
	(else
	 (error "DEBUG: illegal argument" object))))

;;;; Random display commands

(define (pretty-print-current-expression)
  (cond ((debugging-info/undefined-expression? current-expression)
	 (newline)
	 (write-string "<undefined-expression>"))
	((debugging-info/compiled-code? current-expression)
	 (newline)
	 (write-string "<compiled-code>"))
	(else
	 (pp current-expression))))

(define (pretty-print-reduction-function)
  (if-valid-ic-environment current-environment
    (lambda (environment)
      (pp (ic-environment/procedure environment)))))

(define (print-current-expression)
  (newline)
  (write-string "Subproblem level: ")
  (write current-subproblem-number)
  (cond (current-reduction
	 (write-string "  Reduction number: ")
	 (write current-reduction-number)
	 (newline)
	 (write-string "Expression (from execution history):")
	 (pp current-expression)
	 (print-current-environment false))
	((debugging-info/undefined-expression? current-expression)
	 (newline)
	 (write-string "Unknown expression frame")
	 (print-current-environment true))
	((debugging-info/compiled-code? current-expression)
	 (newline)
	 (write-string "Compiled code frame")
	 (print-current-environment true))
	(else
	 (newline)
	 (write-string "Expression (from stack):")
	 (pp current-expression)
	 (print-current-environment false))))

(define (print-current-environment continue-previous-line?)
  (if-valid-environment current-environment
    (lambda (environment)
      (if (not continue-previous-line?)
	  (begin
	    (newline)
	    (write-string "Frame")))
      (write-string " created by ")
      (print-user-friendly-name environment)
      (newline)
      (let ((arguments (environment-arguments environment)))
	(if (eq? arguments 'UNKNOWN)
	    (show-environment-bindings environment true)
	    (begin
	      (write-string "applied to ")
	      (write-string
	       (cdr
		(write-to-string arguments
				 environment-arguments-truncation)))))))))

(define (reductions-command)
  (let loop ((reductions current-reductions))
    (cond ((pair? reductions)
	   (pp (reduction-expression (car reductions)))
	   (loop (cdr reductions)))
	  ((wrap-around-in-reductions? reductions)
	   (newline)
	   (write-string "Wrap Around in the reductions at this level.")))))

;;;; Short history display

(define (summarize-history-command)
  (let ((top-subproblem
	 (if (null? previous-subproblems)
	     current-subproblem
	     (car (last-pair previous-subproblems)))))
    (newline)
    (write-string "SL#  Procedure Name          Expression")
    (newline)
    (let loop ((frame top-subproblem) (level 0))
      (if frame
	  (begin
	    (let ((reductions (stack-frame/reductions frame)))
	      (if (pair? reductions)
		  (let ((print-reduction
			 (lambda (reduction)
			   (terse-print-expression
			    level
			    (reduction-expression reduction)
			    (reduction-environment reduction)))))
		    (print-reduction (car reductions))
		    (if (= level 0)
			(let loop ((reductions (cdr reductions)))
			  (if (pair? reductions)
			      (begin (print-reduction (car reductions))
				     (loop (cdr reductions)))))))
		  (with-values
		      (lambda () (stack-frame/debugging-info frame))
		    (lambda (expression environment)
		      (terse-print-expression level
					      expression
					      environment)))))
	    (loop (stack-frame/next-subproblem frame) (1+ level)))))))

(define (terse-print-expression level expression environment)
  (newline)
  (write-string (string-pad-right (number->string level) 4))
  (write-string " ")
  ;;; procedure name
  (write-string
   (string-pad-right
    (let ((name
	   (and (environment? environment)
		(environment-procedure-name environment))))
      (if (or (not name)
	      (special-name? name))
	  ""
	  (output-to-string 20 (lambda () (write-dbg-name name)))))
    20))
  (write-string "    ")
  (write-string
   (cond ((debugging-info/undefined-expression? expression)
	  "<undefined-expression>")
	 ((debugging-info/compiled-code? expression)
	  "<compiled-code>")
	 (else
	  (output-to-string 50 (lambda () (write (unsyntax expression))))))))

;;;; Motion to earlier expressions

(define (earlier-subproblem-command)
  (if (stack-frame/next-subproblem current-subproblem)
      (begin
	(earlier-subproblem)
	(print-current-expression))
      (begin
	(beep)
	(newline)
	(write-string "There are only ")
	(write current-subproblem-number)
	(write-string " subproblem levels; already at earliest level"))))

(define (earlier-reduction-command)
  (cond ((and student-walk?
	      (> current-subproblem-number 0)
	      (= current-reduction-number 0))
	 (earlier-subproblem-command))
	((< current-reduction-number (-1+ current-number-of-reductions))
	 (set-current-reduction! (1+ current-reduction-number))
	 (print-current-expression))
	(else
	 (newline)
	 (write-string
	  (if (wrap-around-in-reductions? current-reductions)
	      "Wrap around in reductions at this level!"
	      "No more reductions at this level!"))
	 (newline)
	 (write-string "Going to the previous (earlier) subproblem")
	 (earlier-subproblem-command))))

(define (earlier-subproblem)
  ;; Assumption: (not (not (stack-frame/next-subproblem current-subproblem)))
  (set-current-subproblem! (stack-frame/next-subproblem current-subproblem)
			   (cons current-subproblem previous-subproblems)
			   normal-reduction-number))

;;;; Motion to later expressions

(define (later-subproblem-command)
  (later-subproblem normal-reduction-number))

(define (later-reduction-command)
  (if (positive? current-reduction-number)
      (begin
	(set-current-reduction! (-1+ current-reduction-number))
	(print-current-expression))
      (later-subproblem
       (if (or (not student-walk?)
	       (= current-subproblem-number 1))
	   last-reduction-number
	   normal-reduction-number))))

(define (later-subproblem select-reduction-number)
  (if (null? previous-subproblems)
      (begin
	(beep)
	(newline)
	(write-string "Already at latest subproblem level"))
      (begin
	(set-current-subproblem! (car previous-subproblems)
				 (cdr previous-subproblems)
				 select-reduction-number)
	(print-current-expression))))

;;;; General motion command

(define (goto-command)
  (let loop ()
    (let ((subproblem-number (prompt-for-expression "Subproblem number: ")))
      (cond ((not (and (integer? subproblem-number)
		       (not (negative? subproblem-number))))
	     (beep)
	     (newline)
	     (write-string "Subproblem level must be nonnegative integer!")
	     (loop))
	    ((< subproblem-number current-subproblem-number)
	     (repeat (lambda ()
		       (set-current-subproblem! (car previous-subproblems)
						(cdr previous-subproblems)
						normal-reduction-number))
		     (- current-subproblem-number subproblem-number)))
	    (else
	     (let loop ()
	       (if (< current-subproblem-number subproblem-number)
		   (if (stack-frame/next-subproblem current-subproblem)
		       (begin
			 (earlier-subproblem)
			 (loop))
		       (begin
			 (beep)
			 (newline)
			 (write-string "There is no such subproblem.")
			 (newline)
			 (write-string "Now at subproblem number: ~o")
			 (write current-subproblem-number)))))))))
  (set-current-reduction!
   (cond ((> current-number-of-reductions 1)
	  (let get-reduction-number ()
	    (let ((reduction-number
		   (prompt-for-expression
		    (string-append
		     "Reduction Number (0 through "
		     (number->string (-1+ current-number-of-reductions))
		     " inclusive): "))))
	      (cond ((not (and (integer? reduction-number)
			       (not (negative? reduction-number))))
		     (beep)
		     (newline)
		     (write-string
		      "Reduction number must be nonnegative integer!")
		     (get-reduction-number))
		    ((not (< reduction-number current-number-of-reductions))
		     (beep)
		     (newline)
		     (write-string "Reduction number too large!")
		     (get-reduction-number))
		    (else
		     reduction-number)))))
	 ((= current-number-of-reductions 1)
	  (newline)
	  (write-string "There is only one reduction for this subproblem")
	  0)
	 (else
	  (newline)
	  (write-string "There are no reductions for this subproblem.")
	  -1)))
  (print-current-expression))

;;;; Evaluation and frame display commands

(define (enter-read-eval-print-loop)
  (with-rep-alternative current-environment
    (lambda (environment)
      (debug/read-eval-print environment
			     "You are now in the desired environment"
			     "Eval-in-env-->"))))

(define (eval-in-current-environment)
  (with-rep-alternative current-environment debug/read-eval-print-1))

(define (show-current-frame)
  (if-valid-environment current-environment
    (lambda (environment)
      (show-frame environment -1 false))))

(define (show-all-frames)
  (if-valid-environment current-environment
    (lambda (environment)
      (show-frames environment 0))))

(define (enter-where-command)
  (if-valid-environment current-environment debug/where))

(define (error-info-command)
  (let ((message (error-message))
	(irritants (error-irritants))
	(port (current-output-port)))
    (newline)
    (write-string " Message: ")
    (write-string message)
    (newline)
    (if (null? irritants)
	(write-string " No irritants")
	(begin
	  (write-string " Irritants: ")
	  (for-each
	   (let ((n (- (output-port/x-size port) 4)))
	     (lambda (irritant)
	       (newline)
	       (write-string "    ")
	       (if (error-irritant/noise? irritant)
		   (begin
		     (write-string "noise: ")
		     (write (error-irritant/noise-value irritant)))
		   (write-string
		    (let ((result (write-to-string irritant n)))
		      (if (car result)
			  (substring-move-right! "..." 0 3
						 (cdr result) (- n 3)))
		      (cdr result))))))
	   irritants)))
    (newline)
    (write-string " Formatted output:")
    (newline)
    (format-error-message message irritants port)))

;;;; Advanced hacking commands

(define (return-command)
  (let ((next (stack-frame/next-subproblem current-subproblem)))
    (if next
	(with-rep-alternative current-environment
	  (lambda (environment)
	    (let ((value
		   (debug/eval
		    (let ((expression
			   (prompt-for-expression
			    "Expression to EVALUATE and CONTINUE with ($ to retry): ")))
		      (if (eq? expression '$)
			  (unsyntax current-expression)
			  expression))
		    environment)))
	      (if print-return-values?
		  (begin
		    (newline)
		    (write-string "That evaluates to:")
		    (newline)
		    (write value)
		    (if (prompt-for-confirmation "Confirm: ") (next value)))
		  (next value)))))
	(begin
	  (beep)
	  (newline)
	  (write-string "Can't continue!!!")))))

(define (internal-command)
  (debug/read-eval-print user-debug-environment
			 "You are now in the debugger environment"
			 "Debugger-->"))
(define user-debug-environment
  (the-environment))

;;;; Reduction and subproblem motion low-level

(define (set-current-subproblem! stack-frame previous-frames
				 select-reduction-number)
  (set! current-subproblem stack-frame)
  (set! previous-subproblems previous-frames)
  (set! current-subproblem-number (length previous-subproblems))
  (set! current-reductions
	(if stack-frame (stack-frame/reductions current-subproblem) '()))
  (set! current-number-of-reductions (dotted-list-length current-reductions))
  (set-current-reduction! (select-reduction-number)))

(define (last-reduction-number)
  (-1+ current-number-of-reductions))

(define (normal-reduction-number)
  (min (-1+ current-number-of-reductions) 0))

(define (set-current-reduction! number)
  (set! current-reduction-number number)
  (set! current-reduction
	(and (not (null? current-reductions))
	     (>= number 0)
	     (list-ref current-reductions number)))
  (if current-reduction
      (begin
	(set! current-expression (reduction-expression current-reduction))
	(set! current-environment (reduction-environment current-reduction)))
      (with-values (lambda () (stack-frame/debugging-info current-subproblem))
	(lambda (expression environment)
	  (set! current-expression expression)
	  (set! current-environment environment)))))

;;;; Utilities

(define (repeat f n)
  (if (> n 0)
      (begin (f)
	     (repeat f (-1+ n)))))

(define (dotted-list-length l)
  (let count ((n 0) (L L))
    (if (pair? l)
	(count (1+ n) (CDR L))
	n)))

(define-integrable (reduction-expression reduction)
  (car reduction))

(define-integrable (reduction-environment reduction)
  (cadr reduction))

(define (wrap-around-in-reductions? reductions)
  (eq? (list-tail reductions (dotted-list-length reductions))
       reduction-wrap-around-tag))

(define (with-rep-alternative environment receiver)
  (if (interpreter-environment? environment)
      (receiver environment)
      (begin
	(print-undefined-environment)
	(newline)
	(write-string "Using the read-eval-print environment instead!")
	(receiver (nearest-repl/environment)))))

(define (if-valid-environment environment receiver)
  (cond ((debugging-info/undefined-environment? environment)
	 (print-undefined-environment))
	((system-global-environment? environment)
	 (newline)
	 (write-string
	  "System global environment at this subproblem/reduction level"))
	(else
	 (receiver environment))))

(define (if-valid-ic-environment environment receiver)
  (if-valid-environment environment
			(if (ic-environment? environment)
			    receiver
			    (lambda (environment)
			      environment
			      (print-undefined-environment)))))

(define (print-undefined-environment)
  (newline)
  (write-string "Undefined environment at this subproblem/reduction level"))