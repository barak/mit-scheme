#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/debug.scm,v 14.21 1990/09/13 23:30:16 cph Exp $

Copyright (c) 1988, 1989, 1990 Massachusetts Institute of Technology

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

(define debugger:student-walk? false)
(define debugger:print-return-values? false)
(define debugger:auto-toggle? true)
(define debugger:count-subproblems-limit 50)
(define debugger:use-history? false)
(define debugger:list-depth-limit 5)
(define debugger:list-breadth-limit 5)

(define (debug #!optional object)
  (let ((dstate
	 (make-initial-dstate 
	  (if (default-object? object)
	      (or (error-continuation)
		  (current-proceed-continuation))
	      object))))
    (letter-commands
     command-set
     (cmdl-message/active
      (lambda ()
	(presentation
	 (lambda ()
	   (let ((n (count-subproblems dstate)))
	     (write-string "There ")
	     (write-string (if (= n 1) "is" "are"))
	     (write-string " ")
	     (if (> n debugger:count-subproblems-limit)
		 (begin
		   (write-string "more than ")
		   (write debugger:count-subproblems-limit))
		 (write n))
	     (write-string " subproblem")
	     (if (not (= n 1))
		 (write-string "s")))
	   (write-string " on the stack.")
	   (newline)
	   (newline)
	   (print-subproblem dstate)))
	(debugger-message
	 "You are now in the debugger.  Type q to quit, ? for commands.")))
     "Debug-->"
     dstate)))

(define (make-initial-dstate object)
  (let ((dstate (allocate-dstate)))
    (set-dstate/history-state!
     dstate
     (cond (debugger:use-history? 'ALWAYS)
	   (debugger:auto-toggle? 'ENABLED)
	   (else 'DISABLED)))
    (let ((stack-frame (coerce-to-stack-frame object)))
     (if (not stack-frame)
	 (error "DEBUG: null continuation" object))
      (set-current-subproblem! dstate stack-frame '()))
    dstate))

(define (coerce-to-stack-frame object)
  (cond ((stack-frame? object)
	 (stack-frame/skip-non-subproblems object))
	((continuation? object)
	 (coerce-to-stack-frame (continuation->stack-frame object)))
	(else
	 (error "DEBUG: illegal argument" object))))

(define (count-subproblems dstate)
  (do ((i 0 (1+ i))
       (subproblem (dstate/subproblem dstate)
		   (stack-frame/next-subproblem subproblem)))
      ((or (not subproblem) (> i debugger:count-subproblems-limit)) i)))

(define-structure (dstate
		   (conc-name dstate/)
		   (constructor allocate-dstate ()))
  subproblem
  previous-subproblems
  subproblem-number
  number-of-reductions
  reduction-number
  history-state
  expression
  subexpression
  environment-list)

(define (dstate/reduction dstate)
  (nth-reduction (dstate/reductions dstate)
		 (dstate/reduction-number dstate)))

(define (dstate/reductions dstate)
  (stack-frame/reductions (dstate/subproblem dstate)))

(define (initialize-package!)
  (set!
   command-set
   (make-command-set
    'DEBUG-COMMANDS
    `((#\? ,standard-help-command
	   "help, list command letters")
      (#\A ,command/show-all-frames
	   "show All bindings in current environment and its ancestors")
      (#\B ,command/earlier-reduction
	   "move (Back) to next reduction (earlier in time)")
      (#\C ,command/show-current-frame
	   "show bindings of identifiers in the Current environment")
      (#\D ,command/later-subproblem
	   "move (Down) to the previous subproblem (later in time)")
      (#\E ,command/enter-read-eval-print-loop
	   "Enter a read-eval-print loop in the current environment")
      (#\F ,command/later-reduction
	   "move (Forward) to previous reduction (later in time)")
      (#\G ,command/goto
	   "Go to a particular subproblem")
      (#\H ,command/summarize-subproblems
	   "prints a summary (History) of all subproblems")
      (#\I ,command/error-info
	   "redisplay the error message Info")
      (#\L ,command/print-expression
	   "(List expression) pretty print the current expression")
      (#\O ,command/print-environment-procedure
	   "pretty print the procedure that created the current environment")
      (#\P ,command/move-to-parent-environment
	   "move to environment that is Parent of current environment")
      (#\Q ,standard-exit-command
	   "Quit (exit debugger)")
      (#\R ,command/print-reductions
	   "print the execution history (Reductions) of the current subproblem level")
      (#\S ,command/move-to-child-environment
	   "move to child of current environment (in current chain)")
      (#\T ,command/print-subproblem-or-reduction
	   "print the current subproblem or reduction")
      (#\U ,command/earlier-subproblem
	   "move (Up) to the next subproblem (earlier in time)")
      (#\V ,command/eval-in-current-environment
	   "eValuate expression in current environment")
      (#\W ,command/enter-where
	   "enter environment inspector (Where) on the current environment")
      (#\X ,command/internal
	   "create a read eval print loop in the debugger environment")
      (#\Y ,command/frame
	   "display the current stack frame")
      (#\Z ,command/return
	   "return (continue with) an expression after evaluating it")
      )))
  unspecific)

(define command-set)

(define (command/print-subproblem-or-reduction dstate)
  (if (dstate/reduction-number dstate)
      (command/print-reduction dstate)
      (command/print-subproblem dstate)))

(define (command/print-subproblem dstate)
  (presentation (lambda () (print-subproblem dstate))))

(define (print-subproblem dstate)
  (let ((subproblem (dstate/subproblem dstate)))
    (write-string "Subproblem level: ")
    (let ((level (dstate/subproblem-number dstate))
	  (qualify-level
	   (lambda (adjective)
	     (write-string " (this is the ")
	     (write-string adjective)
	     (write-string " subproblem level)"))))
      (write level)
      (cond ((not (stack-frame/next-subproblem subproblem))
	     (qualify-level (if (zero? level) "only" "highest")))
	    ((zero? level)
	     (qualify-level "lowest"))))
    (newline)
    (let ((expression (dstate/expression dstate)))
      (cond ((not (invalid-expression? expression))
	     (write-string
	      (if (stack-frame/compiled-code? subproblem)
		  "Compiled code expression (from stack):"
		  "Expression (from stack):"))
	     (newline)
	     (let ((subexpression (dstate/subexpression dstate)))
	       (if (or (debugging-info/undefined-expression? subexpression)
		       (debugging-info/undefined-expression? subexpression))
		   (debugger-pp expression expression-indentation)
		   (begin
		     (debugger-pp
		      (unsyntax-with-substitutions
		       expression
		       (list (cons subexpression subexpression-marker)))
		      expression-indentation)
		     (newline)
		     (write-string " subproblem being executed (marked by ")
		     (write subexpression-marker)
		     (write-string "):")
		     (newline)
		     (debugger-pp subexpression expression-indentation)))))
	    ((or (not (debugging-info/undefined-expression? expression))
		 (not (debugging-info/noise? expression)))
	     (write-string
	      (if (stack-frame/compiled-code? subproblem)
		  "Compiled code expression unknown"
		  "Expression unknown"))
	     (newline)
	     (write (stack-frame/return-address subproblem)))
	    (else
	     (write-string ((debugging-info/noise expression) true)))))
    (let ((environment-list (dstate/environment-list dstate)))
      (if (pair? environment-list)
	  (print-environment (car environment-list))
	  (begin
	    (newline)
	    (write-string "There is no current environment."))))
    (let ((n-reductions (dstate/number-of-reductions dstate)))
      (newline)
      (if (positive? n-reductions)
	  (begin
	    (write-string
	     "The execution history for this subproblem contains ")
	    (write n-reductions)
	    (write-string " reduction")
	    (if (> n-reductions 1)
		(write-string "s"))
	    (write-string "."))
	  (write-string
	   "There is no execution history for this subproblem.")))))

(define subexpression-marker (string->symbol "#SUBPROBLEM#"))

(define (command/print-reductions dstate)
  (let ((reductions (dstate/reductions dstate))
	(subproblem-level (dstate/subproblem-number dstate)))
    (if (pair? reductions)
	(presentation
	 (lambda ()
	   (write-string "Execution history for this subproblem:")
	   (let loop ((reductions reductions) (number 0))
	     (newline)
	     (write-string "----------------------------------------")
	     (newline)
	     (print-reduction (car reductions) subproblem-level number)
	     (if (pair? (cdr reductions))
		 (loop (cdr reductions) (1+ number))))))
	(debugger-failure
	 "There is no execution history for this subproblem."))))

(define (command/print-reduction dstate)
  (presentation
   (lambda ()
     (print-reduction (dstate/reduction dstate)
		      (dstate/subproblem-number dstate)
		      (dstate/reduction-number dstate)))))

(define (print-reduction reduction subproblem-level reduction-number)
  (write-string "Subproblem level: ")
  (write subproblem-level)
  (write-string "  Reduction number: ")
  (write reduction-number)
  (newline)
  (write-string "Expression (from execution history):")
  (newline)
  (debugger-pp (reduction-expression reduction) expression-indentation)
  (print-environment (reduction-environment reduction)))

(define (print-environment environment)
  (show-environment-name environment)
  (if (not (environment->package environment))
      (begin
	(newline)
	(let ((arguments (environment-arguments environment)))
	  (if (eq? arguments 'UNKNOWN)
	      (show-environment-bindings environment true)
	      (begin
		(write-string " applied to: ")
		(write-string
		 (cdr
		  (write-to-string
		   arguments
		   (- (output-port/x-size (current-output-port)) 11))))))))))

(define (debugger-pp expression indentation)
  (fluid-let ((*unparser-list-depth-limit* debugger:list-depth-limit)
	      (*unparser-list-breadth-limit* debugger:list-breadth-limit))
    (pretty-print expression (current-output-port) true indentation)))

(define expression-indentation 4)

(define (command/print-expression dstate)
  (presentation
   (lambda ()
     (let ((expression (dstate/expression dstate)))
       (cond ((debugging-info/compiled-code? expression)
	      (write-string ";compiled code"))
	     ((not (debugging-info/undefined-expression? expression))
	      (debugger-pp expression 0))
	     ((debugging-info/noise? expression)
	      (write-string ";")
	      (write-string ((debugging-info/noise expression) false)))
	     (else
	      (write-string ";undefined expression")))))))

(define (command/print-environment-procedure dstate)
  (with-current-environment dstate show-environment-procedure))

;;;; Short subproblem display

(define (command/summarize-subproblems dstate)
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
	       (with-values
		   (lambda () (stack-frame/debugging-info frame))
		 (lambda (expression environment subexpression)
		   subexpression
		   (terse-print-expression level
					   expression
					   environment)))
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

;;;; Subproblem motion

(define (command/earlier-subproblem dstate)
  (maybe-stop-using-history! dstate)
  (earlier-subproblem dstate false finish-move-to-subproblem!))

(define (earlier-subproblem dstate reason if-successful)
  (let ((subproblem (dstate/subproblem dstate)))
    (let ((next (stack-frame/next-subproblem subproblem)))
      (if next
	  (begin
	    (set-current-subproblem!
	     dstate
	     next
	     (cons subproblem (dstate/previous-subproblems dstate)))
	    (if-successful dstate))
	  (debugger-failure
	   (reason+message (or reason "no more subproblems")
			   "already at highest subproblem level."))))))

(define (command/later-subproblem dstate)
  (maybe-stop-using-history! dstate)
  (later-subproblem dstate false finish-move-to-subproblem!))

(define (later-subproblem dstate reason if-successful)
  (if (null? (dstate/previous-subproblems dstate))
      (debugger-failure
       (reason+message reason "already at lowest subproblem level."))
      (begin
	(let ((p (dstate/previous-subproblems dstate)))
	  (set-current-subproblem! dstate (car p) (cdr p)))
	(if-successful dstate))))

(define (command/goto dstate)
  (maybe-stop-using-history! dstate)
  (let ((subproblems (select-subproblem dstate)))
    (set-current-subproblem! dstate (car subproblems) (cdr subproblems)))
  (finish-move-to-subproblem! dstate))

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
			 " inclusive).")
			(top-level-loop))))))))))

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
	     (debugger-failure prompt " must be nonnegative integer.")
	     (loop))
	    ((and limit (>= expression limit))
	     (debugger-failure prompt " too large.")
	     (loop))
	    (else
	     expression)))))

;;;; Reduction motion

(define (command/earlier-reduction dstate)
  (maybe-start-using-history! dstate)
  (let ((up
	 (lambda ()
	   (earlier-subproblem dstate false finish-move-to-subproblem!))))
    (if (not (dstate/using-history? dstate))
	(up)
	(let ((n-reductions (dstate/number-of-reductions dstate))
	      (reduction-number (dstate/reduction-number dstate))
	      (wrap
	       (lambda (reason)
		 (earlier-subproblem
		  dstate
		  reason
		  (lambda (dstate)
		    (debugger-message
		     (reason+message
		      reason
		      "going to the next (less recent) subproblem."))
		    (finish-move-to-subproblem! dstate))))))
	  (cond ((zero? n-reductions)
		 (up))
		((not reduction-number)
		 (move-to-reduction! dstate 0))
		((and (< reduction-number (-1+ n-reductions))
		      (not (and debugger:student-walk?
				(positive? (dstate/subproblem-number dstate))
				(= reduction-number 0))))
		 (move-to-reduction! dstate (1+ reduction-number)))
		(debugger:student-walk?
		 (up))
		(else
		 (wrap "no more reductions")))))))

(define (command/later-reduction dstate)
  (maybe-start-using-history! dstate)
  (let ((down
	 (lambda ()
	   (later-subproblem dstate false finish-move-to-subproblem!))))
    (if (not (dstate/using-history? dstate))
	(later-subproblem dstate false finish-move-to-subproblem!)
	(let ((reduction-number (dstate/reduction-number dstate))
	      (wrap
	       (lambda (reason)
		 (later-subproblem
		  dstate
		  reason
		  (lambda (dstate)
		    (debugger-message
		     (reason+message
		      reason
		      "going to the previous (more recent) subproblem."))
		    (let ((n (dstate/number-of-reductions dstate)))
		      (if (and n (positive? n))
			  (move-to-reduction!
			   dstate
			   (if (and debugger:student-walk?
				    (positive?
				     (dstate/subproblem-number dstate)))
			       0
			       (-1+ n)))
			  (finish-move-to-subproblem! dstate))))))))
	  (cond ((zero? (dstate/number-of-reductions dstate))
		 (down))
		((not reduction-number)
		 (wrap false))
		((positive? reduction-number)
		 (move-to-reduction! dstate (-1+ reduction-number)))
		(debugger:student-walk?
		 (down))
		(else
		 (wrap "no more reductions")))))))

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
	  ((eq? true (environment-has-parent? (car environment-list)))
	   (set-dstate/environment-list!
	    dstate
	    (cons (environment-parent (car environment-list))
		  environment-list))
	   (show-current-frame dstate true))
	  (else
	   (debugger-failure "The current environment has no parent.")))))

(define (command/move-to-child-environment dstate)
  (let ((environment-list (dstate/environment-list dstate)))
    (cond ((not (pair? (dstate/environment-list dstate)))
	   (undefined-environment))
	  ((not (pair? (cdr environment-list)))
	   (debugger-failure
	    "This is the initial environment; can't move to child."))
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
			 "the debugger"
			 "the desired environment"
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
      (debugger-failure "No error to report.")))

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
		 (let ((expression
			(prompt-for-expression
			 (string-append
			  "Expression to EVALUATE and CONTINUE with"
			  (if invalid-expression?
			      ""
			      " ($ to retry)")))))
		   (if (and (not invalid-expression?)
			    (eq? expression '$))
		       (debug/scode-eval (dstate/expression dstate)
					 environment)
		       (debug/eval expression environment)))))
	    (if debugger:print-return-values?
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
			   "the debugger"
			   "the debugger environment"
			   "Debugger-->")))

(define (command/frame dstate)
  (presentation
   (lambda ()
     (write-string "Stack frame: ")
     (write (dstate/subproblem dstate))
     (for-each (lambda (element)
		 (newline)
		 (debugger-pp element 0))
	       (named-structure/description (dstate/subproblem dstate))))))

;;;; Low-level Side-effects

(define (maybe-start-using-history! dstate)
  (if (eq? 'ENABLED (dstate/history-state dstate))
      (begin
	(set-dstate/history-state! dstate 'NOW)
	(debugger-message
	 "Now using information from the execution history."))))

(define (maybe-stop-using-history! dstate)
  (if (eq? 'NOW (dstate/history-state dstate))
      (begin
	(set-dstate/history-state! dstate 'ENABLED)
	(debugger-message
	 "Now ignoring information from the execution history."))))

(define (dstate/using-history? dstate)
  (or (eq? 'ALWAYS (dstate/history-state dstate))
      (eq? 'NOW (dstate/history-state dstate))))

(define (dstate/auto-toggle? dstate)
  (not (eq? 'DISABLED (dstate/history-state dstate))))

(define (set-current-subproblem! dstate stack-frame previous-frames)
  (set-dstate/subproblem! dstate stack-frame)
  (set-dstate/previous-subproblems! dstate previous-frames)
  (set-dstate/subproblem-number! dstate (length previous-frames))
  (set-dstate/number-of-reductions!
   dstate
   (improper-list-length (stack-frame/reductions stack-frame)))
  (with-values (lambda () (stack-frame/debugging-info stack-frame))
    (lambda (expression environment subexpression)
      (set-dstate/expression! dstate expression)
      (set-dstate/subexpression! dstate subexpression)
      (set-dstate/environment-list!
       dstate
       (if (debugging-info/undefined-environment? environment)
	   '()
	   (list environment))))))

(define (finish-move-to-subproblem! dstate)
  (if (and (dstate/using-history? dstate)
	   (positive? (dstate/number-of-reductions dstate)))
      (move-to-reduction! dstate 0)
      (begin
	(set-dstate/reduction-number! dstate false)
	(command/print-subproblem dstate))))

(define (move-to-reduction! dstate reduction-number)
  (set-dstate/reduction-number! dstate reduction-number)
  (set-dstate/environment-list!
   dstate
   (list (reduction-environment (dstate/reduction dstate))))
  (command/print-reduction dstate))

;;;; Utilities

(define (improper-list-length l)
  (let count ((n 0) (l l))
    (if (pair? l)
	(count (1+ n) (cdr l))
	n)))

(define (nth-reduction reductions n)
  (let loop ((reductions reductions) (n n))
    (if (zero? n)
	(car reductions)
	(loop (cdr reductions) (-1+ n)))))

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
	   "Cannot evaluate in current environment;
using the read-eval-print environment instead.")
	  (nearest-repl/environment)))))

(define (with-current-environment dstate receiver)
  (let ((environment-list (dstate/environment-list dstate)))
    (if (pair? environment-list)
	(receiver (car environment-list))
	(undefined-environment))))

(define (undefined-environment)
  (debugger-failure "There is no current environment."))

(define (reason+message reason message)
  (string-capitalize (if reason (string-append reason "; " message) message)))