#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/debug.scm,v 14.8 1989/01/06 20:59:51 cph Exp $

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
  (set!
   command-set
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
      (#\O ,pretty-print-environment-procedure
	   "Pretty print the procedure that created the current environment")
      (#\P ,move-to-parent-environment
	   "Move to environment which is parent of current environment")
      (#\Q ,standard-exit-command
	   "Quit (exit DEBUG)")
      (#\R ,reductions-command
	   "Print the reductions of the current subproblem level")
      (#\S ,move-to-child-environment
	   "Move to child of current environment (in current chain)")
      (#\T ,print-current-reduction
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

(define current-subproblem)
(define previous-subproblems)
(define current-subproblem-number)
(define current-reduction-number)
(define current-reductions)
(define current-number-of-reductions)
(define current-reduction)
(define current-expression)
(define environment-list)

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
	      (current-expression)
	      (environment-list))
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
		      (cmdl-message/active print-current-reduction)
		      (cmdl-message/standard "Debugger"))
		     "Debug-->")))

(define (coerce-to-stack-frame object)
  (cond ((stack-frame? object)
	 (stack-frame/skip-non-subproblems object))
	((continuation? object)
	 (coerce-to-stack-frame (continuation->stack-frame object)))
	(else
	 (error "DEBUG: illegal argument" object))))

;;;; Display commands

(define (print-current-reduction)
  (print-current-expression)
  (print-current-environment))

(define (print-current-expression)
  (newline)
  (write-string "Subproblem level: ")
  (write current-subproblem-number)
  (if current-reduction
      (begin
	(write-string "  Reduction number: ")
	(write current-reduction-number)
	(newline)
	(write-string "Expression (from execution history):")
	(print-expression current-expression))
      (begin
	(newline)
	(write-string
	 (if (stack-frame/compiled-code? current-subproblem)
	     "Compiled code expression"
	     "Expression"))
	(if (or (debugging-info/undefined-expression? current-expression)
		(debugging-info/compiled-code? current-expression))
	    (write-string " unknown")
	    (begin
	      (write-string " (from stack):")
	      (print-expression current-expression))))))

(define (stack-frame/compiled-code? frame)
  (compiled-return-address? (stack-frame/return-address frame)))

(define (print-current-environment)
  (if (pair? environment-list)
      (let ((environment (car environment-list)))
	(show-environment-name environment)
	(show-environment-arguments environment))
      (begin
	(newline)
	(write-string "There is no current environment"))))

(define (show-environment-arguments environment)
  (if (not (environment->package environment))
      (begin
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

(define (show-environment-arguments environment)
  (if (not (environment->package environment))
      (begin
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

(define (pretty-print-current-expression)
  (cond ((debugging-info/undefined-expression? current-expression)
	 (newline)
	 (write-string ";undefined expression"))
	((debugging-info/compiled-code? current-expression)
	 (newline)
	 (write-string ";compiled code"))
	(else
	 (print-expression current-expression))))

(define (pretty-print-environment-procedure)
  (with-current-environment
   (lambda (environment)
     (let ((scode-lambda (environment-lambda environment)))
       (if scode-lambda
	   (print-expression scode-lambda)
	   (begin
	     (newline)
	     (write-string
	      "Unable to get procedure for this environment")))))))

(define (reductions-command)
  (let loop ((reductions current-reductions))
    (cond ((pair? reductions)
	   (print-expression (reduction-expression (car reductions)))
	   (loop (cdr reductions)))
	  ((wrap-around-in-reductions? reductions)
	   (newline)
	   (write-string "Wrap around in the reductions at this level")))))

(define (print-expression expression)
  (pp expression))

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
	      (special-form-procedure-name? name))
	  ""
	  (output-to-string 20 (lambda () (write-dbg-name name)))))
    20))
  (write-string "    ")
  (write-string
   (cond ((debugging-info/undefined-expression? expression)
	  ";undefined expression")
	 ((debugging-info/compiled-code? expression)
	  ";compiled code")
	 (else
	  (output-to-string 50
			    (lambda () (write-sexp (unsyntax expression))))))))

(define (write-sexp sexp)
  (fluid-let ((*unparse-primitives-by-name?* true))
    (write sexp)))

;;;; Subproblem/reduction motion

(define (earlier-subproblem-command)
  (if (stack-frame/next-subproblem current-subproblem)
      (begin
	(earlier-subproblem)
	(print-current-reduction))
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
	 (print-current-reduction))
	(else
	 (newline)
	 (write-string
	  (if (wrap-around-in-reductions? current-reductions)
	      "Wrap around in reductions at this level"
	      "No more reductions at this level"))
	 (newline)
	 (write-string "Going to the previous (earlier) subproblem")
	 (newline)
	 (earlier-subproblem-command))))

(define (earlier-subproblem)
  ;; Assumption: (not (not (stack-frame/next-subproblem current-subproblem)))
  (set-current-subproblem! (stack-frame/next-subproblem current-subproblem)
			   (cons current-subproblem previous-subproblems)
			   normal-reduction-number))

(define (later-subproblem-command)
  (later-subproblem normal-reduction-number))

(define (later-reduction-command)
  (if (positive? current-reduction-number)
      (begin
	(set-current-reduction! (-1+ current-reduction-number))
	(print-current-reduction))
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
	(print-current-reduction))))

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
			 (write-string "There is no such subproblem")
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
	  (write-string "There are no reductions for this subproblem")
	  -1)))
  (print-current-reduction))

;;;; Environment motion and display

(define (show-current-frame)
  (if (pair? environment-list)
      (show-current-frame-1 false)
      (print-undefined-environment)))

(define (show-current-frame-1 brief?)
  (show-frame (car environment-list) (length (cdr environment-list)) brief?))

(define (show-all-frames)
  (if (pair? environment-list)
      (show-frames (car (last-pair environment-list)) 0)
      (print-undefined-environment)))

(define (move-to-parent-environment)
  (cond ((not (pair? environment-list))
	 (print-undefined-environment))
	((environment-has-parent? (car environment-list))
	 (set! environment-list
	       (cons (environment-parent (car environment-list))
		     environment-list))
	 (show-current-frame-1 true))
	(else
	 (beep)
	 (newline)
	 (write-string "The current environment has no parent"))))

(define (move-to-child-environment)
  (cond ((not (pair? environment-list))
	 (print-undefined-environment))
	((not (pair? (cdr environment-list)))
	 (beep)
	 (newline)
	 (write-string "This is the initial environment; can't move to child"))
	(else
	 (set! environment-list (cdr environment-list))
	 (show-current-frame-1 true))))

(define (enter-read-eval-print-loop)
  (with-rep-environment
   (lambda (environment)
     (debug/read-eval-print environment
			    "You are now in the desired environment"
			    "Eval-in-env-->"))))

(define (eval-in-current-environment)
  (with-rep-environment debug/read-eval-print-1))

(define (enter-where-command)
  (with-current-environment debug/where))

;;;; Error info

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
	(with-rep-environment
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
	(set! environment-list
	      (list (reduction-environment current-reduction))))
      (with-values (lambda () (stack-frame/debugging-info current-subproblem))
	(lambda (expression environment)
	  (set! current-expression expression)
	  (set! environment-list
		(if (debugging-info/undefined-environment? environment)
		    '()
		    (list environment)))))))

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

(define (with-current-environment receiver)
  (if (pair? environment-list)
      (receiver (car environment-list))
      (print-undefined-environment)))

(define (with-rep-environment receiver)
  (if (and (pair? environment-list)
	   (interpreter-environment? (car environment-list)))
      (receiver (car environment-list))
      (begin
	(newline)
	(write-string "Cannot evaluate in current environment")
	(newline)
	(write-string "Using the read-eval-print environment instead")
	(receiver (nearest-repl/environment)))))

(define (print-undefined-environment)
  (beep)
  (newline)
  (write-string "There is no current environment"))