#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/debug.scm,v 14.18 1990/08/21 04:18:33 jinx Exp $

Copyright (c) 1988, 1989 Massachusetts Institute of Technology

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

(define student-walk? false)
(define print-return-values? false)

(define (debug #!optional object)
  (let ((dstate
	 (make-initial-dstate 
	  (if (default-object? object)
	      (or (error-continuation)
		  (current-proceed-continuation))
	      object))))
    (letter-commands command-set
		     (cmdl-message/append
		      (cmdl-message/active
		       (lambda ()
			 (command/print-reduction dstate)))
		      (cmdl-message/standard "Debugger"))
		     "Debug-->"
		     dstate)))

(define (make-initial-dstate object)
  (let ((dstate (allocate-dstate)))
    (set-current-subproblem!
     dstate
     (or (coerce-to-stack-frame object)
	 (error "DEBUG: null continuation" object))
     '()
     first-reduction-number)
    dstate))

(define (coerce-to-stack-frame object)
  (cond ((stack-frame? object)
	 (stack-frame/skip-non-subproblems object))
	((continuation? object)
	 (coerce-to-stack-frame (continuation->stack-frame object)))
	(else
	 (error "DEBUG: illegal argument" object))))

(define-structure (dstate
		   (conc-name dstate/)
		   (constructor allocate-dstate ()))
  subproblem
  previous-subproblems
  subproblem-number
  reduction-number
  reductions
  number-of-reductions
  reduction
  expression
  environment-list)

(define (initialize-package!)
  (set!
   command-set
   (make-command-set
    'DEBUG-COMMANDS
    `((#\? ,standard-help-command
	   "Help, list command letters")
      (#\A ,command/show-all-frames
	   "Show bindings in current environment and its ancestors")
      (#\B ,command/earlier-reduction
	   "Earlier reduction (Back in time)")
      (#\C ,command/show-current-frame
	   "Show bindings of identifiers in the current environment")
      (#\D ,command/later-subproblem
	   "Move (Down) to the next (later) subproblem")
      (#\E ,command/enter-read-eval-print-loop
	   "Enter a read-eval-print loop in the current environment")
      (#\F ,command/later-reduction
	   "Later reduction (Forward in time)")
      (#\G ,command/goto
	   "Go to a particular Subproblem/Reduction level")
      (#\H ,command/summarize-history
	   "Prints a summary of the entire history")
      (#\I ,command/error-info
	   "Redisplay the error message")
      (#\L ,command/print-expression
	   "(list expression) Pretty-print the current expression")
      (#\O ,command/print-environment-procedure
	   "Pretty print the procedure that created the current environment")
      (#\P ,command/move-to-parent-environment
	   "Move to environment which is parent of current environment")
      (#\Q ,standard-exit-command
	   "Quit (exit DEBUG)")
      (#\R ,command/print-reductions
	   "Print the reductions of the current subproblem level")
      (#\S ,command/move-to-child-environment
	   "Move to child of current environment (in current chain)")
      (#\T ,command/print-reduction
	   "Print the current subproblem/reduction")
      (#\U ,command/earlier-subproblem
	   "Move (Up) to the previous (earlier) subproblem")
      (#\V ,command/eval-in-current-environment
	   "Evaluate expression in current environment")
      (#\W ,command/enter-where
	   "Enter WHERE on the current environment")
      (#\X ,command/internal
	   "Create a read eval print loop in the debugger environment")
      (#\Y ,command/frame
	   "Display the current stack frame")
      (#\Z ,command/return
	   "Return (continue with) an expression after evaluating it")
      )))
  unspecific)

(define command-set)

(define (command/print-reduction dstate)
  (presentation
   (lambda ()
     (write-string "Subproblem level: ")
     (write (dstate/subproblem-number dstate))
     (let ((expression (dstate/expression dstate)))
       (if (dstate/reduction dstate)
	   (begin
	     (write-string "  Reduction number: ")
	     (write (dstate/reduction-number dstate))
	     (newline)
	     (write-string "Expression (from execution history):")
	     (newline)
	     (pretty-print expression))
	   (let ((subproblem (dstate/subproblem dstate)))
	     (newline)
	     (cond ((not (invalid-expression? expression))
		    (write-string
		     (if (stack-frame/compiled-code? subproblem)
			 "Compiled code expression (from stack):"
			 "Expression (from stack):"))
		    (newline)
		    (pretty-print expression))
		   ((or (not (debugging-info/undefined-expression? expression))
			(not (debugging-info/noise? expression)))
		    (write-string
		     (if (stack-frame/compiled-code? subproblem)
			 "Compiled code expression unknown"
			 "Expression unknown")))
		   (else
		    (write-string
		     ((debugging-info/noise expression) true)))))))
     (let ((environment-list (dstate/environment-list dstate)))
       (if (pair? environment-list)
	   (let ((environment (car environment-list)))
	     (show-environment-name environment)
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
			     (write-to-string
			      arguments
			      (- (output-port/x-size (current-output-port))
				 11))))))))))
	   (begin
	     (newline)
	     (write-string "There is no current environment")))))))

(define (command/print-expression dstate)
  (presentation
   (lambda ()
     (let ((expression (dstate/expression dstate)))
       (cond ((debugging-info/compiled-code? expression)
	      (write-string ";compiled code"))
	     ((not (debugging-info/undefined-expression? expression))
	      (pretty-print expression))
	     ((debugging-info/noise? expression)
	      (write-string ";")
	      (write-string ((debugging-info/noise expression) false)))
	     (else
	      (write-string ";undefined expression")))))))

(define (command/print-environment-procedure dstate)
  (with-current-environment dstate
    (lambda (environment)
      (let ((scode-lambda (environment-lambda environment)))
	(if scode-lambda
	    (presentation (lambda () (pretty-print scode-lambda)))
	    (debugger-failure "No procedure for this environment"))))))

(define (command/print-reductions dstate)
  (let ((reductions (dstate/reductions dstate)))
    (if (pair? reductions)
	(presentation
	 (lambda ()
	   (pretty-print (reduction-expression (car reductions)))
	   (let loop ((reductions (cdr reductions)))
	     (cond ((pair? reductions)
		    (newline)
		    (pretty-print (reduction-expression (car reductions)))
		    (loop (cdr reductions)))
		   ((eq? 'WRAP-AROUND reductions)
		    (newline)
		    (write-string
		     "Wrap around in the reductions at this level"))))))
	(debugger-failure "No reductions at this level"))))

;;;; Short history display

(define (command/summarize-history dstate)
  (let ((top-subproblem
	 (let ((previous-subproblems (dstate/previous-subproblems dstate)))
	   (if (null? previous-subproblems)
	       (dstate/subproblem dstate)
	       (car (last-pair previous-subproblems))))))
    (presentation
     (lambda ()
       (write-string "SL#  Procedure-name          Expression")
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
				 (begin
				   (print-reduction (car reductions))
				   (loop (cdr reductions)))))))
		     (with-values
			 (lambda () (stack-frame/debugging-info frame))
		       (lambda (expression environment)
			 (terse-print-expression level
						 expression
						 environment)))))
	       (loop (stack-frame/next-subproblem frame) (1+ level)))))))))

(define (terse-print-expression level expression environment)
  (newline)
  (write-string (string-pad-right (number->string level) 4))
  (write-string " ")
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
   (cond ((debugging-info/compiled-code? expression)
	  ";compiled code")
	 ((not (debugging-info/undefined-expression? expression))
	  (output-to-string
	   50
	   (lambda ()
	     (fluid-let ((*unparse-primitives-by-name?* true))
	       (write (unsyntax expression))))))
	 ((debugging-info/noise? expression)
	  (output-to-string
	   50
	   (lambda ()
	     (write-string ((debugging-info/noise expression) false)))))
	 (else
	  ";undefined expression"))))

;;;; Subproblem/reduction motion

(define (command/earlier-subproblem dstate)
  (if (stack-frame/next-subproblem (dstate/subproblem dstate))
      (let ((subproblem (dstate/subproblem dstate)))
	(move-to-subproblem! dstate
			     (stack-frame/next-subproblem subproblem)
			     (cons subproblem
				   (dstate/previous-subproblems dstate))
			     normal-reduction-number))
      (debugger-failure "There are only "
			(1+ (dstate/subproblem-number dstate))
			" subproblem levels; already at earliest level")))

(define (command/earlier-reduction dstate)
  (let ((reduction-number (dstate/reduction-number dstate)))
    (cond ((and student-walk?
		(> (dstate/subproblem-number dstate) 0)
		(= reduction-number 0))
	   (command/earlier-subproblem dstate))
	  ((< reduction-number
	      (-1+ (dstate/number-of-reductions dstate)))
	   (move-to-reduction! dstate (1+ reduction-number)))
	  (else
	   (debugger-message
	    (if (wrap-around-in-reductions? (dstate/reductions dstate))
		"Wrap around in"
		"No more")
	    " reductions; going to the previous (earlier) subproblem")
	   (command/earlier-subproblem dstate)))))

(define (command/later-subproblem dstate)
  (later-subproblem dstate normal-reduction-number))

(define (command/later-reduction dstate)
  (if (positive? (dstate/reduction-number dstate))
      (move-to-reduction! dstate (-1+ (dstate/reduction-number dstate)))
      (later-subproblem dstate
			(if (or (not student-walk?)
				(= (dstate/subproblem-number dstate) 1))
			    last-reduction-number
			    normal-reduction-number))))

(define (later-subproblem dstate select-reduction-number)
  (if (null? (dstate/previous-subproblems dstate))
      (debugger-failure "Already at latest subproblem level")
      (let ((previous-subproblems (dstate/previous-subproblems dstate)))
	(move-to-subproblem! dstate
			     (car previous-subproblems)
			     (cdr previous-subproblems)
			     select-reduction-number))))

;;;; General motion command

(define (command/goto dstate)
  (let* ((subproblems (select-subproblem dstate))
	 (subproblem (car subproblems))
	 (reduction-number
	  (select-reduction
	   (improper-list-length (stack-frame/reductions subproblem)))))
    (move-to-subproblem! dstate
			 subproblem
			 (cdr subproblems)
			 (lambda (number-of-reductions)
			   number-of-reductions ;ignore
			   reduction-number))))

(define (select-subproblem dstate)
  (let top-level-loop ()
    (let ((delta
	   (- (prompt-for-nonnegative-integer "Subproblem number" false)
	      (dstate/subproblem-number dstate))))
      (if (negative? delta)
	  (list-tail (dstate/previous-subproblems dstate) (-1+ (- delta)))
	  (let loop
	      ((subproblem (dstate/subproblem dstate))
	       (subproblems (dstate/previous-subproblems dstate))
	       (delta delta))
	    (if (zero? delta)
		(cons subproblem subproblems)
		(let ((next (stack-frame/next-subproblem subproblem)))
		  (if next
		      (loop next (cons subproblem subproblems) (-1+ delta))
		      (begin
			(debugger-failure
			 "Subproblem number too large (limit is "
			 (length subproblems)
			 " inclusive)")
			(top-level-loop))))))))))

(define (select-reduction number-of-reductions)
  (cond ((> number-of-reductions 1)
	 (prompt-for-nonnegative-integer "Reduction number"
					 number-of-reductions))
	((= number-of-reductions 1)
	 (debugger-message "Exactly one reduction for this subproblem")
	 0)
	(else
	 (debugger-message "No reductions for this subproblem")
	 -1)))

(define (prompt-for-nonnegative-integer prompt limit)
  (let loop ()
    (let ((expression
	   (prompt-for-expression
	    (string-append prompt
			   (if limit
			       (string-append " (0 through "
					      (number->string (-1+ limit))
					      " inclusive)")
			       "")))))
      (cond ((not (exact-nonnegative-integer? expression))
	     (debugger-failure prompt " must be nonnegative integer")
	     (loop))
	    ((and limit (>= expression limit))
	     (debugger-failure prompt " too large")
	     (loop))
	    (else
	     expression)))))

;;;; Environment motion and display

(define (command/show-current-frame dstate)
  (if (pair? (dstate/environment-list dstate))
      (show-current-frame dstate false)
      (undefined-environment)))

(define (command/show-all-frames dstate)
  (let ((environment-list (dstate/environment-list dstate)))
    (if (pair? environment-list)
	(show-frames (car (last-pair environment-list)) 0)
	(undefined-environment))))

(define (command/move-to-parent-environment dstate)
  (let ((environment-list (dstate/environment-list dstate)))
    (cond ((not (pair? environment-list))
	   (undefined-environment))
	  ((environment-has-parent? (car environment-list))
	   (set-dstate/environment-list!
	    dstate
	    (cons (environment-parent (car environment-list))
		  environment-list))
	   (show-current-frame dstate true))
	  (else
	   (debugger-failure "The current environment has no parent")))))

(define (command/move-to-child-environment dstate)
  (let ((environment-list (dstate/environment-list dstate)))
    (cond ((not (pair? (dstate/environment-list dstate)))
	   (undefined-environment))
	  ((not (pair? (cdr environment-list)))
	   (debugger-failure
	    "This is the initial environment; can't move to child"))
	  (else
	   (set-dstate/environment-list! dstate (cdr environment-list))
	   (show-current-frame dstate true)))))

(define (show-current-frame dstate brief?)
  (presentation
   (lambda ()
     (let ((environment-list (dstate/environment-list dstate)))
       (show-frame (car environment-list)
		   (length (cdr environment-list))
		   brief?)))))

(define (command/enter-read-eval-print-loop dstate)
  (debug/read-eval-print (get-evaluation-environment dstate)
			 "You are now in the desired environment"
			 "Eval-in-env-->"))

(define (command/eval-in-current-environment dstate)
  (debug/read-eval-print-1 (get-evaluation-environment dstate)))

(define (command/enter-where dstate)
  (with-current-environment dstate debug/where))

;;;; Error info

(define (command/error-info dstate)
  dstate				;ignore
  (show-error-info (error-condition)))

(define (show-error-info condition)
  (if condition
      (presentation
       (lambda ()
	 (let ((message (condition/message condition))
	       (irritants (condition/irritants condition))
	       (port (current-output-port)))
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
	   ((condition/reporter condition) condition port))))
      (debugger-failure "No error to report")))

;;;; Advanced hacking commands

(define (command/return dstate)
  (let ((next (stack-frame/next-subproblem (dstate/subproblem dstate))))
    (if next
	(let ((invalid-expression?
	       (invalid-expression? (dstate/expression dstate)))
	      (environment (get-evaluation-environment dstate))
	      (return
	       (lambda (value)
		 ((stack-frame->continuation next) value))))
	  (let ((value
		 (debug/eval
		  (let ((expression
			 (prompt-for-expression
			  (string-append
			   "Expression to EVALUATE and CONTINUE with"
			   (if invalid-expression?
			       ""
			       " ($ to retry)")))))
		    (if (and (not invalid-expression?)
			     (eq? expression '$))
			(unsyntax (dstate/expression dstate))
			expression))
		  environment)))
	    (if print-return-values?
		(begin
		  (newline)
		  (write-string "That evaluates to:")
		  (newline)
		  (write value)
		  (if (prompt-for-confirmation "Confirm") (return value)))
		(return value))))
	(debugger-failure "Can't continue!!!"))))

(define *dstate*)

(define (command/internal dstate)
  (fluid-let ((*dstate* dstate))
    (debug/read-eval-print (->environment '(RUNTIME DEBUGGER))
			   "You are now in the debugger environment"
			   "Debugger-->")))

(define (command/frame dstate)
  (presentation
   (lambda ()
     (write-string "Stack frame: ")
     (write (dstate/subproblem dstate))
     (for-each (lambda (element)
		 (newline)
		 (pretty-print element))
	       (named-structure/description (dstate/subproblem dstate))))))

;;;; Low-level Side-effects

(define (move-to-subproblem! dstate
			     stack-frame
			     previous-frames
			     select-reduction-number)
  (dynamic-wind
   (lambda ()
     unspecific)
   (lambda ()
     (set-current-subproblem! dstate
			      stack-frame
			      previous-frames
			      select-reduction-number))
   (lambda ()
     (command/print-reduction dstate))))

(define (move-to-reduction! dstate reduction-number)
  (dynamic-wind (lambda () unspecific)
		(lambda () (set-current-reduction! dstate reduction-number))
		(lambda () (command/print-reduction dstate))))

(define (set-current-subproblem! dstate
				 stack-frame
				 previous-frames
				 select-reduction-number)
  (set-dstate/subproblem! dstate stack-frame)
  (set-dstate/previous-subproblems! dstate previous-frames)
  (set-dstate/subproblem-number! dstate (length previous-frames))
  (let* ((reductions (if stack-frame (stack-frame/reductions stack-frame) '()))
	 (number-of-reductions (improper-list-length reductions)))
    (set-dstate/reductions! dstate reductions)
    (set-dstate/number-of-reductions! dstate number-of-reductions)
    (set-current-reduction! dstate
			    (select-reduction-number number-of-reductions))))

(define (normal-reduction-number number-of-reductions)
  (min (-1+ number-of-reductions) 0))

(define (first-reduction-number number-of-reductions)
  number-of-reductions			;ignore
  0)

(define (last-reduction-number number-of-reductions)
  (-1+ number-of-reductions))

(define (set-current-reduction! dstate number)
  (set-dstate/reduction-number! dstate number)
  (let ((reduction
	 (and (>= number 0)
	      (let loop
		  ((reductions (dstate/reductions dstate))
		   (number number))
		(and (pair? reductions)
		     (if (zero? number)
			 (car reductions)
			 (loop (cdr reductions) (-1+ number))))))))
    (set-dstate/reduction! dstate reduction)
    (if reduction
	(begin
	  (set-dstate/expression! dstate (reduction-expression reduction))
	  (set-dstate/environment-list!
	   dstate
	   (list (reduction-environment reduction))))
	(with-values
	    (lambda ()
	      (stack-frame/debugging-info (dstate/subproblem dstate)))
	  (lambda (expression environment)
	    (set-dstate/expression! dstate expression)
	    (set-dstate/environment-list!
	     dstate
	     (if (debugging-info/undefined-environment? environment)
		 '()
		 (list environment))))))))

;;;; Utilities

(define (improper-list-length l)
  (let count ((n 0) (l l))
    (if (pair? l)
	(count (1+ n) (cdr l))
	n)))

(define-integrable (reduction-expression reduction)
  (car reduction))

(define-integrable (reduction-environment reduction)
  (cadr reduction))

(define (wrap-around-in-reductions? reductions)
  (or (eq? 'WRAP-AROUND reductions)
      (and (pair? reductions)
	   (eq? 'WRAP-AROUND (cdr (last-pair reductions))))))

(define (invalid-expression? expression)
  (or (debugging-info/undefined-expression? expression)
      (debugging-info/compiled-code? expression)))

(define (get-evaluation-environment dstate)
  (let ((environment-list (dstate/environment-list dstate)))
    (if (and (pair? environment-list)
	     (environment? (car environment-list)))
	(car environment-list)
	(begin
	  (debugger-message
	   "Cannot evaluate in current environment;\nusing the read-eval-print environment instead")
	  (nearest-repl/environment)))))

(define (with-current-environment dstate receiver)
  (let ((environment-list (dstate/environment-list dstate)))
    (if (pair? environment-list)
	(receiver (car environment-list))
	(undefined-environment))))

(define (undefined-environment)
  (debugger-failure "There is no current environment"))