#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

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
(define debugger:string-length-limit 70)

(define (debug #!optional object)
  (if (default-object? object)
      (let ((condition (nearest-repl/condition)))
	(if condition
	    (debug-internal condition)
	    (call-with-current-continuation debug-internal)))
      (debug-internal object)))

(define (debug-internal object)
  (let ((dstate (make-initial-dstate object)))
    (with-simple-restart 'CONTINUE "Return from DEBUG."
      (lambda ()
	(letter-commands
	 command-set
	 (cmdl-message/active
	  (lambda (port)
	    (debugger-presentation port
	      (lambda ()
		(let ((thread (dstate/other-thread dstate)))
		  (if thread
		      (begin
			(write-string "This error occurred in another thread: "
				      port)
			(write thread port)
			(newline port))))
		(let ((n (count-subproblems dstate)))
		  (write-string "There " port)
		  (write-string (if (= n 1) "is" "are") port)
		  (write-string " " port)
		  (if (> n debugger:count-subproblems-limit)
		      (begin
			(write-string "more than " port)
			(write debugger:count-subproblems-limit port))
		      (write n port))
		  (write-string " subproblem" port)
		  (if (not (= n 1))
		      (write-string "s" port)))
		(write-string " on the stack." port)
		(newline port)
		(newline port)
		(print-subproblem dstate port)))
	    (debugger-message
	     port
	     "You are now in the debugger.  Type q to quit, ? for commands.")))
	 "debug>"
	 dstate)))))

(define (stack-trace condition port)
  (let ((dstate (make-initial-dstate condition)))
    (command/print-subproblem dstate port)
    (let loop ()
      (if (let ((next
		 (stack-frame/next-subproblem
		  (dstate/subproblem dstate))))
	    (and next (not (stack-frame/repl-eval-boundary? next))))
	  (begin
	    (newline port)
	    (newline port)
	    (command/earlier-subproblem dstate port)
	    (loop))))))

(define (make-initial-dstate object)
  (let ((make-dstate
	 (lambda (stack-frame condition)
	   (let ((dstate (allocate-dstate)))
	     (set-dstate/history-state!
	      dstate
	      (cond (debugger:use-history? 'ALWAYS)
		    (debugger:auto-toggle? 'ENABLED)
		    (else 'DISABLED)))
	     (set-dstate/condition! dstate condition)
	     (set-current-subproblem!
	      dstate
	      (let loop ((stack-frame stack-frame))
		(let ((stack-frame
		       (stack-frame/skip-non-subproblems stack-frame)))
		  (if (not stack-frame)
		      (error "No frames on stack!"))
		  (if (stack-frame/repl-eval-boundary? stack-frame)
		      (loop (stack-frame/next stack-frame))
		      stack-frame)))
	      '())
	     dstate))))
    (cond ((condition? object)
	   (make-dstate
	    (continuation->stack-frame (condition/continuation object))
	    object))
	  ((continuation? object)
	   (make-dstate (continuation->stack-frame object) false))
	  ((stack-frame? object)
	   (make-dstate object false))
	  (else
	   (error:wrong-type-argument object
				      "condition or continuation"
				      'DEBUG)))))

(define (count-subproblems dstate)
  (do ((i 0 (1+ i))
       (subproblem (dstate/subproblem dstate)
		   (next-subproblem subproblem)))
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
  environment-list
  condition)

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
      (#\I ,command/condition-report
	   "redisplay the error message Info")
      (#\J ,command/return-to
	   "return TO the current subproblem with a value")
      (#\K ,command/condition-restart
	   "continue the program using a standard restart option")
      (#\L ,command/print-expression
	   "(List expression) pretty print the current expression")
      (#\M ,command/print-frame-elements
	   "(Frame elements) show the contents of the stack frame, in raw form")
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
      (#\Z ,command/return-from
	   "return FROM the current subproblem with a value")
      )))
  (set! hook/debugger-before-return default/debugger-before-return)
  unspecific)

(define command-set)

(define-syntax define-command
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '((IDENTIFIER IDENTIFIER IDENTIFIER) + EXPRESSION)
			(cdr form))
	 (let ((dstate (cadr (cadr form)))
	       (port (caddr (cadr form))))
	   `(DEFINE (,(car (cadr form)) #!OPTIONAL ,dstate ,port)
	      (LET ((,dstate (IF (DEFAULT-OBJECT? ,dstate) *DSTATE* ,dstate))
		    (,port (IF (DEFAULT-OBJECT? ,port) *PORT* ,port)))
		,@(map (let ((free (list dstate port)))
			 (lambda (expression)
			   (make-syntactic-closure environment free
			     expression)))
		       (cddr form)))))
	 (ill-formed-syntax form)))))

;;;; Display commands

(define-command (command/print-subproblem-or-reduction dstate port)
  (if (dstate/reduction-number dstate)
      (command/print-reduction dstate port)
      (command/print-subproblem dstate port)))

(define-command (command/print-subproblem dstate port)
  (debugger-presentation port
    (lambda ()
      (print-subproblem dstate port))))

(define-command (command/print-reduction dstate port)
  (debugger-presentation port
    (lambda ()
      (print-reduction (dstate/reduction dstate)
		       (dstate/subproblem-number dstate)
		       (dstate/reduction-number dstate)
		       port))))

(define-command (command/print-reductions dstate port)
  (let ((reductions (dstate/reductions dstate))
	(subproblem-level (dstate/subproblem-number dstate)))
    (if (pair? reductions)
	(debugger-presentation port
	  (lambda ()
	    (write-string "Execution history for this subproblem:" port)
	    (let loop ((reductions reductions) (number 0))
	      (newline port)
	      (write-string "----------------------------------------" port)
	      (newline port)
	      (print-reduction (car reductions) subproblem-level number port)
	      (if (pair? (cdr reductions))
		  (loop (cdr reductions) (1+ number))))))
	(debugger-failure
	 port
	 "There is no execution history for this subproblem."))))

(define-command (command/print-expression dstate port)
  (debugger-presentation port
    (lambda ()
      (let ((expression (dstate/expression dstate)))
	(cond ((debugging-info/compiled-code? expression)
	       (write-string ";compiled code" port))
	      ((not (debugging-info/undefined-expression? expression))
	       (pretty-print expression port true 0))
	      ((debugging-info/noise? expression)
	       (write-string ";" port)
	       (write-string ((debugging-info/noise expression) false) port))
	      (else
	       (write-string ";undefined expression" port)))))))

(define-command (command/print-environment-procedure dstate port)
  (with-current-environment dstate port
    (lambda (environment)
      (show-environment-procedure environment port))))

(define (print-subproblem dstate port)
  (print-subproblem-identification dstate port)
  (newline port)
  (print-subproblem-expression dstate port)
  (print-subproblem-environment dstate port)
  (print-subproblem-reduction dstate port))

(define (print-subproblem-identification dstate port)
  (let ((subproblem (dstate/subproblem dstate)))
    (write-string "Subproblem level: " port)
    (let ((level (dstate/subproblem-number dstate))
	  (qualify-level
	   (lambda (adjective)
	     (write-string " (this is the " port)
	     (write-string adjective port)
	     (write-string " subproblem level)" port))))
      (write level port)
      (cond ((not (next-subproblem subproblem))
	     (qualify-level (if (zero? level) "only" "highest")))
	    ((zero? level)
	     (qualify-level "lowest"))))))

(define (print-subproblem-expression dstate port)
  (let ((expression (dstate/expression dstate))
	(subproblem (dstate/subproblem dstate)))
    (cond ((not (invalid-expression? expression))
	   (write-string (if (stack-frame/compiled-code? subproblem)
			     "Compiled code expression (from stack):"
			     "Expression (from stack):")
			 port)
	   (newline port)
	   (let ((subexpression (dstate/subexpression dstate)))
	     (if (or (debugging-info/undefined-expression? subexpression)
		     (debugging-info/unknown-expression? subexpression))
		 (debugger-pp expression expression-indentation port)
		 (begin
		   (debugger-pp
		    (unsyntax-with-substitutions
		     expression
		     (list (cons subexpression subexpression-marker)))
		    expression-indentation
		    port)
		   (newline port)
		   (write-string " subproblem being executed (marked by " port)
		   (write subexpression-marker port)
		   (write-string "):" port)
		   (newline port)
		   (debugger-pp subexpression expression-indentation port)))))
	  ((debugging-info/noise? expression)
	   (write-string ((debugging-info/noise expression) true) port))
	  (else
	   (write-string (if (stack-frame/compiled-code? subproblem)
			     "Compiled code expression unknown"
			     "Expression unknown")
			 port)
	   (newline port)
	   (write (stack-frame/return-address subproblem) port)))))

(define subexpression-marker
  ((ucode-primitive string->symbol) "###"))

(define (print-subproblem-environment dstate port)
  (let ((environment-list (dstate/environment-list dstate)))
    (if (pair? environment-list)
	(print-environment (car environment-list) port)
	(begin
	  (newline port)
	  (write-string "There is no current environment." port)))))

(define (print-subproblem-reduction dstate port)
  (let ((n-reductions (dstate/number-of-reductions dstate)))
    (newline port)
    (if (positive? n-reductions)
	(begin
	  (write-string "The execution history for this subproblem contains "
			port)
	  (write n-reductions port)
	  (write-string " reduction" port)
	  (if (> n-reductions 1)
	      (write-string "s" port))
	  (write-string "." port))
	(write-string "There is no execution history for this subproblem."
		      port))))

(define (print-reduction reduction subproblem-number reduction-number port)
  (print-reduction-identification subproblem-number reduction-number port)
  (newline port)
  (print-reduction-expression reduction port)
  (print-reduction-environment reduction port))

(define (print-reduction-identification subproblem-number reduction-number
					port)
  (write-string "Subproblem level: " port)
  (write subproblem-number port)
  (write-string "  Reduction number: " port)
  (write reduction-number port))

(define (print-reduction-expression reduction port)
  (write-string "Expression (from execution history):" port)
  (newline port)
  (debugger-pp (reduction-expression reduction) expression-indentation port))

(define (print-reduction-environment reduction port)
  (print-environment (reduction-environment reduction) port))

(define (print-environment environment port)
  (newline port)
  (show-environment-name environment port)
  (if (not (environment->package environment))
      (begin
	(newline port)
	(let ((arguments (environment-arguments environment)))
	  (if (eq? arguments 'UNKNOWN)
	      (show-environment-bindings environment true port)
	      (begin
		(write-string " applied to: " port)
		(write-string
		 (cdr
		  (write-to-string
		   arguments
		   (- (output-port/x-size port) 11)))
		 port)))))))

;;;; Subproblem summary

(define-command (command/summarize-subproblems dstate port)
  (let ((top-subproblem
	 (let ((previous-subproblems (dstate/previous-subproblems dstate)))
	   (if (null? previous-subproblems)
	       (dstate/subproblem dstate)
	       (car (last-pair previous-subproblems))))))
    (debugger-presentation port
      (lambda ()
	(write-string "SL#  Procedure-name          Expression" port)
	(newline port)
	(let loop ((frame top-subproblem) (level 0))
	  (if frame
	      (begin
		(with-values (lambda () (stack-frame/debugging-info frame))
		  (lambda (expression environment subexpression)
		    subexpression
		    (terse-print-expression level
					    expression
					    environment
					    port)))
		(loop (next-subproblem frame) (1+ level)))))))))

(define (terse-print-expression level expression environment port)
  (newline port)
  (write-string (string-pad-right (number->string level) 4) port)
  (write-string " " port)
  (write-string
   (string-pad-right
    (let ((name
	   (and (environment? environment)
		(environment-procedure-name environment))))
      (if (or (not name)
	      (special-form-procedure-name? name))
	  ""
	  (output-to-string 20
	    (lambda ()
	      (write-dbg-name name (current-output-port))))))
    20)
   port)
  (write-string "    " port)
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
	  ";undefined expression"))
   port))

;;;; Subproblem motion

(define-command (command/earlier-subproblem dstate port)
  (maybe-stop-using-history! dstate port)
  (earlier-subproblem dstate port false finish-move-to-subproblem!))

(define (earlier-subproblem dstate port reason if-successful)
  (let ((subproblem (dstate/subproblem dstate)))
    (let ((next (next-subproblem subproblem)))
      (if next
	  (begin
	    (set-current-subproblem!
	     dstate
	     next
	     (cons subproblem (dstate/previous-subproblems dstate)))
	    (if-successful dstate port))
	  (debugger-failure
	   port
	   (reason+message (or reason "no more subproblems")
			   "already at highest subproblem level."))))))

(define (next-subproblem stack-frame)
  (let ((next (stack-frame/next-subproblem stack-frame)))
    (if (and next (stack-frame/repl-eval-boundary? next))
	(next-subproblem next)
	next)))

(define-command (command/later-subproblem dstate port)
  (maybe-stop-using-history! dstate port)
  (later-subproblem dstate port false finish-move-to-subproblem!))

(define (later-subproblem dstate port reason if-successful)
  (if (null? (dstate/previous-subproblems dstate))
      (debugger-failure
       port
       (reason+message reason "already at lowest subproblem level."))
      (begin
	(let ((p (dstate/previous-subproblems dstate)))
	  (set-current-subproblem! dstate (car p) (cdr p)))
	(if-successful dstate port))))

(define-command (command/goto dstate port)
  (maybe-stop-using-history! dstate port)
  (let ((subproblems (select-subproblem dstate port)))
    (set-current-subproblem! dstate (car subproblems) (cdr subproblems)))
  (finish-move-to-subproblem! dstate port))

(define (select-subproblem dstate port)
  (let top-level-loop ()
    (let ((delta
	   (- (prompt-for-nonnegative-integer "Subproblem number" false port)
	      (dstate/subproblem-number dstate))))
      (if (negative? delta)
	  (list-tail (dstate/previous-subproblems dstate) (-1+ (- delta)))
	  (let loop
	      ((subproblem (dstate/subproblem dstate))
	       (subproblems (dstate/previous-subproblems dstate))
	       (delta delta))
	    (if (zero? delta)
		(cons subproblem subproblems)
		(let ((next (next-subproblem subproblem)))
		  (if next
		      (loop next (cons subproblem subproblems) (-1+ delta))
		      (begin
			(debugger-failure
			 port
			 "Subproblem number too large (limit is "
			 (length subproblems)
			 " inclusive).")
			(top-level-loop))))))))))

;;;; Reduction motion

(define-command (command/earlier-reduction dstate port)
  (maybe-start-using-history! dstate port)
  (let ((up
	 (lambda ()
	   (earlier-subproblem dstate port false finish-move-to-subproblem!))))
    (if (not (dstate/using-history? dstate))
	(up)
	(let ((n-reductions (dstate/number-of-reductions dstate))
	      (reduction-number (dstate/reduction-number dstate))
	      (wrap
	       (lambda (reason)
		 (earlier-subproblem
		  dstate
		  port
		  reason
		  (lambda (dstate port)
		    (debugger-message
		     port
		     (reason+message
		      reason
		      "going to the next (less recent) subproblem."))
		    (finish-move-to-subproblem! dstate port))))))
	  (cond ((zero? n-reductions)
		 (up))
		((not reduction-number)
		 (move-to-reduction! dstate port 0))
		((and (< reduction-number (-1+ n-reductions))
		      (not (and debugger:student-walk?
				(positive? (dstate/subproblem-number dstate))
				(= reduction-number 0))))
		 (move-to-reduction! dstate port (1+ reduction-number)))
		(debugger:student-walk?
		 (up))
		(else
		 (wrap "no more reductions")))))))

(define-command (command/later-reduction dstate port)
  (maybe-start-using-history! dstate port)
  (let ((down
	 (lambda ()
	   (later-subproblem dstate port false finish-move-to-subproblem!))))
    (if (not (dstate/using-history? dstate))
	(later-subproblem dstate port false finish-move-to-subproblem!)
	(let ((reduction-number (dstate/reduction-number dstate))
	      (wrap
	       (lambda (reason)
		 (later-subproblem
		  dstate
		  port
		  reason
		  (lambda (dstate port)
		    (debugger-message
		     port
		     (reason+message
		      reason
		      "going to the previous (more recent) subproblem."))
		    (let ((n (dstate/number-of-reductions dstate)))
		      (if (and n (positive? n))
			  (move-to-reduction!
			   dstate
			   port
			   (if (and debugger:student-walk?
				    (positive?
				     (dstate/subproblem-number dstate)))
			       0
			       (-1+ n)))
			  (finish-move-to-subproblem! dstate port))))))))
	  (cond ((zero? (dstate/number-of-reductions dstate))
		 (down))
		((not reduction-number)
		 (wrap false))
		((positive? reduction-number)
		 (move-to-reduction! dstate port (-1+ reduction-number)))
		((special-history-subproblem? dstate)
		 ;; Reset state
		 (set-current-subproblem! dstate
					  (dstate/subproblem dstate)
					  (dstate/previous-subproblems dstate))
		 (set-dstate/reduction-number! dstate false)
		 (command/print-subproblem dstate port))
		(debugger:student-walk?
		 (down))
		(else
		 (wrap "no more reductions")))))))

;;;; Environment motion and display

(define-command (command/show-current-frame dstate port)
  (if (pair? (dstate/environment-list dstate))
      (show-current-frame dstate false port)
      (undefined-environment port)))

(define-command (command/show-all-frames dstate port)
  (let ((environment-list (dstate/environment-list dstate)))
    (if (pair? environment-list)
	(show-frames (car (last-pair environment-list)) 0 port)
	(undefined-environment port))))

(define-command (command/move-to-parent-environment dstate port)
  (let ((environment-list (dstate/environment-list dstate)))
    (cond ((not (pair? environment-list))
	   (undefined-environment port))
	  ((eq? true (environment-has-parent? (car environment-list)))
	   (set-dstate/environment-list!
	    dstate
	    (cons (environment-parent (car environment-list))
		  environment-list))
	   (show-current-frame dstate true port))
	  (else
	   (debugger-failure port "The current environment has no parent.")))))

(define-command (command/move-to-child-environment dstate port)
  (let ((environment-list (dstate/environment-list dstate)))
    (cond ((not (pair? (dstate/environment-list dstate)))
	   (undefined-environment port))
	  ((not (pair? (cdr environment-list)))
	   (debugger-failure
	    port
	    "This is the initial environment; can't move to child."))
	  (else
	   (set-dstate/environment-list! dstate (cdr environment-list))
	   (show-current-frame dstate true port)))))

(define (show-current-frame dstate brief? port)
  (debugger-presentation port
    (lambda ()
      (let ((environment-list (dstate/environment-list dstate)))
	(show-frame (car environment-list)
		    (length (cdr environment-list))
		    brief?
		    port)))))

(define-command (command/enter-read-eval-print-loop dstate port)
  (debug/read-eval-print (get-evaluation-environment dstate port)
			 "the debugger"
			 "the environment for this frame"))

(define-command (command/eval-in-current-environment dstate port)
  (debug/read-eval-print-1 (get-evaluation-environment dstate port) port))

(define-command (command/enter-where dstate port)
  (with-current-environment dstate port debug/where))

;;;; Condition commands

(define-command (command/condition-report dstate port)
  (let ((condition (dstate/condition dstate)))
    (if condition
	(debugger-presentation port
	  (lambda ()
	    (write-condition-report condition port)))
	(debugger-failure port "No condition to report."))))

(define-command (command/condition-restart dstate port)
  (let ((condition (dstate/condition dstate)))
    (let ((restarts
	   (if condition
	       (condition/restarts condition)
	       (bound-restarts))))
      (if (null? restarts)
	  (debugger-failure port "No options to choose from.")
	  (let ((n-restarts (length restarts))
		(write-index
		 (lambda (index port)
		   (write-string (string-pad-left (number->string index) 3)
				 port)
		   (write-string ":" port))))
	    (let ((invoke-option
		   (lambda (n)
		     (invoke-restart-interactively
		      (list-ref restarts (- n-restarts n))
		      condition))))
	      (debugger-presentation port
		(lambda ()
		  (if (= n-restarts 1)
		      (begin
			(write-string "There is only one option:" port)
			(write-restarts restarts port write-index)
			(if (prompt-for-confirmation "Use this option" port)
			    (invoke-option 1)))
		      (begin
			(write-string "Choose an option by number:" port)
			(write-restarts restarts port write-index)
			(invoke-option
			 (prompt-for-integer "Option number"
					     1
					     (+ n-restarts 1)
					     port))))))))))))

;;;; Advanced hacking commands

(define-command (command/return-from dstate port)
  (let ((next (next-subproblem (dstate/subproblem dstate))))
    (if next
	(enter-subproblem dstate port next)
	(debugger-failure port "Can't continue!!!"))))

(define-command (command/return-to dstate port)
  (enter-subproblem dstate port (dstate/subproblem dstate)))

(define (enter-subproblem dstate port subproblem)
  (let ((invalid-expression?
	 (invalid-expression? (dstate/expression dstate)))
	(environment (get-evaluation-environment dstate port)))
    (let ((value
	   (let ((expression
		  (prompt-for-expression
		   (string-append
		    "Expression to EVALUATE and CONTINUE with"
		    (if invalid-expression?
			""
			" ($ to retry)"))
		   port
		   environment)))
	     (if (and (not invalid-expression?)
		      (eq? expression '$))
		 (debug/scode-eval (dstate/expression dstate)
				   environment)
		 (debug/eval expression environment)))))
      (if (or (not debugger:print-return-values?)
	      (begin
		(newline port)
		(write-string "That evaluates to:" port)
		(newline port)
		(write value port)
		(prompt-for-confirmation "Confirm" port)))
	  (begin
	    (hook/debugger-before-return)
	    (let ((thread (dstate/other-thread dstate)))
	      (if (not thread)
		  ((stack-frame->continuation subproblem) value)
		  (begin
		    (restart-thread thread 'ASK
		      (lambda ()
			((stack-frame->continuation subproblem) value)))
		    (continue-from-derived-thread-error
		     (dstate/condition dstate))))))))))

(define (dstate/other-thread dstate)
  (let ((condition (dstate/condition dstate)))
    (and condition
	 (condition/other-thread condition))))

(define hook/debugger-before-return)
(define (default/debugger-before-return)
  '())

(define *dstate*)
(define *port*)

(define (command/internal dstate port)
  (fluid-let ((*dstate* dstate)
	      (*port* port))
    (debug/read-eval-print (->environment '(RUNTIME DEBUGGER))
			   "the debugger"
			   "the debugger environment")))

(define-command (command/frame dstate port)
  (debugger-presentation port
    (lambda ()
      (write-string "Stack frame: " port)
      (write (dstate/subproblem dstate) port)
      (for-each (lambda (element)
		  (newline port)
		  (debugger-pp element 0 port))
		(named-structure/description (dstate/subproblem dstate))))))

(define-command (command/print-frame-elements dstate port)
  (debugger-presentation
   port
   (lambda ()
     (write-string "Stack frame elements: " port)
     (for-each-vector-element
      (stack-frame/elements (dstate/subproblem dstate))
      (lambda (element)
	(newline)
	(write element))))))

;;;; Low-level Side-effects

(define (maybe-start-using-history! dstate port)
  (if (eq? 'ENABLED (dstate/history-state dstate))
      (begin
	(set-dstate/history-state! dstate 'NOW)
	(if (not (zero? (dstate/number-of-reductions dstate)))
	    (debugger-message
	     port
	     "Now using information from the execution history.")))))

(define (maybe-stop-using-history! dstate port)
  (if (eq? 'NOW (dstate/history-state dstate))
      (begin
	(set-dstate/history-state! dstate 'ENABLED)
	(if (not (zero? (dstate/number-of-reductions dstate)))
	    (debugger-message
	     port
	     "Now ignoring information from the execution history.")))))

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

(define (finish-move-to-subproblem! dstate port)
  (if (and (dstate/using-history? dstate)
	   (positive? (dstate/number-of-reductions dstate))
	   (not (special-history-subproblem? dstate)))
      (move-to-reduction! dstate port 0)
      (begin
	(set-dstate/reduction-number! dstate false)
	(command/print-subproblem dstate port))))

(define (move-to-reduction! dstate port reduction-number)
  (set-dstate/reduction-number! dstate reduction-number)
  (set-dstate/environment-list!
   dstate
   (list (reduction-environment (dstate/reduction dstate))))
  (command/print-reduction dstate port))

(define (special-history-subproblem? dstate)
  (eq? (stack-frame/type (dstate/subproblem dstate))
       stack-frame-type/compiled-return-address))

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

(define (get-evaluation-environment dstate port)
  (let ((environment-list (dstate/environment-list dstate)))
    (if (and (pair? environment-list)
	     (environment? (car environment-list)))
	(car environment-list)
	(begin
	  (debugger-message
	   port
	   "Cannot evaluate in current environment;
using the read-eval-print environment instead.")
	  (nearest-repl/environment)))))

(define (with-current-environment dstate port receiver)
  (let ((environment-list (dstate/environment-list dstate)))
    (if (pair? environment-list)
	(receiver (car environment-list))
	(undefined-environment port))))

(define (undefined-environment port)
  (debugger-failure port "There is no current environment."))

(define (reason+message reason message)
  (string-capitalize (if reason (string-append reason "; " message) message)))

(define (debugger-pp expression indentation port)
  (fluid-let ((*unparser-list-depth-limit* debugger:list-depth-limit)
	      (*unparser-list-breadth-limit* debugger:list-breadth-limit)
	      (*unparser-string-length-limit* debugger:string-length-limit))
    (pretty-print expression port true indentation)))

(define expression-indentation 4)

(define (prompt-for-nonnegative-integer prompt limit port)
  (prompt-for-integer prompt 0 limit port))

(define (prompt-for-integer prompt lower upper port)
  (let loop ()
    (let ((expression
	   (prompt-for-expression
	    (string-append
	     prompt
	     (if lower
		 (if upper
		     (string-append " (" (number->string lower)
				    " through "
				    (number->string (- upper 1))
				    " inclusive)")
		     (string-append " (minimum " (number->string lower) ")"))
		 (if upper
		     (string-append " (maximum "
				    (number->string (- upper 1))
				    ")")
		     "")))
	    port)))
      (cond ((not (exact-integer? expression))
	     (debugger-failure port prompt " must be exact integer.")
	     (loop))
	    ((and lower (< expression lower))
	     (debugger-failure port prompt " too small.")
	     (loop))
	    ((and upper (>= expression upper))
	     (debugger-failure port prompt " too large.")
	     (loop))
	    (else
	     expression)))))