#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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
;;; package: (runtime new-debugger)

(declare (usual-integrations))

;; (define debugger:student-walk? #f)
;; (define debugger:print-return-values? #f)
;; (define debugger:auto-toggle? #t)
;; (define debugger:count-subproblems-limit 50)
;; (define debugger:use-history? #f)
;; (define debugger:list-depth-limit 5)
;; (define debugger:list-breadth-limit 5)
;; (define debugger:string-length-limit 70)

(define (ndebug #!optional object)
  (cond ((default-object? object)
	 (let ((condition (nearest-repl/condition)))
	   (if condition
	       (debug-internal (condition/continuation condition)
			       condition)
	       (call-with-current-continuation
		 (lambda (k)
		   (debug-internal k #f))))))
	((condition? object)
	 (debug-internal (condition/continuation object) object))
	((continuation? object)
	 (debug-internal object #f))
	(else
	 (error:wrong-type-argument object
				    "condition or continuation"
				    'debug))))

(define (debug-internal continuation condition)
  (let ((dstate (initial-dstate continuation condition)))
    (with-simple-restart 'continue "Return from DEBUG."
      (lambda ()
	(letter-commands
	 command-set
	 (cmdl-message/active
	  (lambda (port)
	    (port/debugger-presentation port
	      (lambda ()
		(let ((thread (dstate-other-thread dstate)))
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

(define (count-subproblems dstate)
  (let loop ((frames (dstate-all-subproblems dstate)) (n 0))
    (if (and (stream-pair? frames)
	     (<= n debugger:count-subproblems-limit))
	(loop (stream-cdr frames) (+ n 1))
	n)))

(define-deferred command-set
  (make-command-set 'debug-commands
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
	   "return FROM the current subproblem with a value"))
    'immutable-state? #t))

;;;; Display commands

(define (command/print-subproblem-or-reduction dstate port)
  (if (dstate-have-reductions? dstate)
      (command/print-reduction dstate port)
      (command/print-subproblem dstate port))
  dstate)

(define (command/print-subproblem dstate port)
  (port/debugger-presentation port
    (lambda ()
      (print-subproblem dstate port)))
  dstate)

(define (command/print-reduction dstate port)
  (port/debugger-presentation port
    (lambda ()
      (print-reduction (dstate-reduction dstate)
		       (dstate-subproblem-index dstate)
		       (dstate-reduction-index dstate)
		       port)))
  dstate)

(define (command/print-reductions dstate port)
  (let ((subproblem-index (dstate-subproblem-index dstate)))
    (if (dstate-have-reductions? dstate)
	(port/debugger-presentation port
	  (lambda ()
	    (write-string "Execution history for this subproblem:" port)
	    (for-each
	     (lambda (i)
	       (newline port)
	       (write-string "----------------------------------------" port)
	       (newline port)
	       (print-reduction (dstate-reduction dstate i)
				subproblem-index
				i
				port))
	     (iota (dstate-n-reductions dstate)))))
	(debugger-failure
	 port
	 "There is no execution history for this subproblem.")))
  dstate)

(define (command/print-expression dstate port)
  (port/debugger-presentation port
    (lambda ()
      (let ((expression (dstate-expression dstate)))
	(cond ((cframe-dbg-expression-compiled? expression)
	       (write-string ";compiled code" port))
	      ((cframe-dbg-expression-undefined? expression)
	       (write-string ";undefined expression" port))
	      ((cframe-dbg-printer? expression)
	       (write-string ";" port)
	       (cframe-dbg-printer-apply expression #f port))
	      (else
	       (pretty-print expression port #t 0))))))
  dstate)

(define (command/print-environment-procedure dstate port)
  (with-current-environment dstate port
    (lambda (environment)
      (show-environment-procedure environment port)))
  dstate)

(define (print-subproblem dstate port)
  (print-subproblem-identification dstate port)
  (newline port)
  (print-subproblem-expression dstate port)
  (print-subproblem-environment dstate port)
  (print-subproblem-reduction dstate port))

(define (print-subproblem-identification dstate port)
  (write-string "Subproblem level: " port)
  (let ((index (dstate-subproblem-index dstate))
	(qualify-level
	 (lambda (adjective)
	   (write-string " (this is the " port)
	   (write-string adjective port)
	   (write-string " subproblem level)" port))))
    (write index port)
    (cond ((not (dstate-next-frame? dstate))
	   (qualify-level (if (zero? index) "only" "highest")))
	  ((zero? index)
	   (qualify-level "lowest")))))

(define (print-subproblem-expression dstate port)
  (let ((expression (dstate-expression dstate))
	(frame (dstate-frame dstate)))
    (cond ((not (invalid-expression? expression))
	   (write-string (if (cframe-compiled-address? frame)
			     "Compiled code expression (from stack):"
			     "Expression (from stack):")
			 port)
	   (newline port)
	   (let ((subexpression (dstate-subexpression dstate)))
	     (if (cframe-dbg-expression-undefined? subexpression)
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
	  ((cframe-dbg-printer? expression)
	   (cframe-dbg-printer-apply expression #t port))
	  (else
	   (write-string (if (cframe-compiled-address? frame)
			     "Compiled code expression unknown"
			     "Expression unknown")
			 port)
	   (newline port)
	   (write (cframe-return-address frame) port)))))

(define-integrable subexpression-marker '<!>)

(define (print-subproblem-environment dstate port)
  (if (dstate-have-environment? dstate)
      (print-environment (dstate-environment dstate) port)
      (begin
	(newline port)
	(write-string "There is no current environment." port))))

(define (print-subproblem-reduction dstate port)
  (let ((n-reductions (dstate-n-reductions dstate)))
    (newline port)
    (if (> n-reductions 0)
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

(define (print-reduction reduction subproblem-index reduction-index port)
  (print-reduction-identification subproblem-index reduction-index port)
  (newline port)
  (print-reduction-expression reduction port)
  (print-reduction-environment reduction port))

(define (print-reduction-identification subproblem-index reduction-index port)
  (write-string "Subproblem level: " port)
  (write subproblem-index port)
  (write-string "  Reduction number: " port)
  (write reduction-index port))

(define (print-reduction-expression reduction port)
  (write-string "Expression (from execution history):" port)
  (newline port)
  (debugger-pp (history-reduction-expression reduction)
	       expression-indentation
	       port))

(define (print-reduction-environment reduction port)
  (print-environment (history-reduction-environment reduction)
		     port))

(define (print-environment environment port)
  (newline port)
  (print-environment-name environment port)
  (if (not (environment-has-name? environment))
      (begin
	(newline port)
	(let ((arguments (environment-arguments environment)))
	  (if (eq? arguments 'unknown)
	      (show-environment-bindings environment #t port)
	      (begin
		(write-string " applied to: " port)
		(write-string
		 (cdr
		  (write-to-string
		   arguments
		   (- (output-port/x-size port) 11)))
		 port)))))))

;;;; Subproblem summary

(define (command/summarize-subproblems dstate port)
  (let ((frames (dstate-all-subproblems dstate)))
    (port/debugger-presentation port
      (lambda ()
	(write-string "SL#  Procedure-name          Expression" port)
	(newline port)
	(let loop ((frames frames) (level 0))
	  (if (stream-pair? frames)
	      (let ((frame (stream-car frames)))
		(terse-print-expression level
					(cframe-dbg-expression frame)
					(cframe-dbg-environment frame)
					port)
		(loop (stream-car frames) (+ level 1))))))))
  dstate)

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
	      (scode-lambda-name->syntax-name name))
	  ""
	  (output-to-string 20
	    (lambda ()
	      (write-dbg-name name (current-output-port))))))
    20)
   port)
  (write-string "    " port)
  (write-string
   (cond ((cframe-dbg-expression-compiled? expression)
	  ";compiled code")
	 ((cframe-dbg-expression-undefined? expression)
	  ";undefined expression")
	 ((cframe-dbg-printer? expression)
	  (output-to-string
	   terse-print-expression-limit
	   (lambda ()
	     (cframe-dbg-printer-apply expression #f (current-output-port)))))
	 (else(not )
	  (output-to-string
	   terse-print-expression-limit
	   (lambda ()
	     (parameterize ((param:print-primitives-by-name? #t))
	       (write (unsyntax expression)))))))
   port))

(define terse-print-expression-limit 50)

;;;; Subproblem motion

(define (command/earlier-subproblem dstate port)
  (earlier-subproblem (dstate-stop-using-history dstate) port #f #f))

(define (earlier-subproblem dstate port failure-reason if-succeed)
  (let ((dstate* (dstate-earlier-subproblem dstate)))
    (if dstate*
	(command/print-subproblem-or-reduction (if if-succeed
						   (if-succeed dstate* port)
						   dstate*)
					       port)
	(begin
	  (debugger-failure
	   port
	   (reason+message (or failure-reason "no more subproblems")
			   "already at highest subproblem level."))
	  dstate))))

(define (command/later-subproblem dstate port)
  (later-subproblem (dstate-stop-using-history dstate) port #f #f))

(define (later-subproblem dstate port failure-reason if-succeed)
  (let ((dstate* (dstate-later-subproblem dstate)))
    (if dstate*
	(command/print-subproblem-or-reduction (if if-succeed
						   (if-succeed dstate* port)
						   dstate*)
					       port)
	(begin
	  (debugger-failure
	   port
	   (reason+message (or failure-reason "no more subproblems")
			   "already at lowest subproblem level."))
	  dstate))))

(define (command/goto dstate port)
  (let loop ()
    (let ((result
	   (dstate-nth-subproblem
	    (dstate-stop-using-history dstate)
	    (prompt-for-nonnegative-integer "Subproblem number" #f port))))
      (if (dstate? result)
	  (command/print-subproblem-or-reduction result port)
	  (begin
	    (debugger-failure
	     port
	     "Subproblem number too large (limit is " result " exclusive).")
	    (loop))))))

;;;; Reduction motion

(define (only-latest-reduction? dstate)
  (and debugger:student-walk?
       (> (dstate-subproblem-index dstate) 0)))

(define (command/earlier-reduction dstate port)
  (let ((dstate* (dstate-start-using-history dstate)))
    (if (dstate-have-reductions? dstate*)
	(let ((dstate**
	       (and (not (only-latest-reduction? dstate*))
		    (dstate-earlier-reduction dstate*))))
	  (if dstate**
	      (command/print-reduction dstate** port)
	      (earlier-subproblem dstate* port "no more reductions"
		(lambda (dstate** port)
		  (if (not debugger:student-walk?)
		      (debugger-message
		       port
		       (reason+message
			"no more reductions"
			"going to the next (less recent) subproblem.")))
		  dstate**))))
	(earlier-subproblem dstate* port #f #f))))

(define (command/later-reduction dstate port)
  (let ((dstate* (dstate-start-using-history dstate)))
    (if (dstate-have-reductions? dstate*)
	(let ((dstate** (dstate-later-reduction dstate*)))
	  (if dstate**
	      (command/print-reduction dstate** port)
	      (later-subproblem dstate port "no more reductions"
		(lambda (dstate** port)
		  (debugger-message
		   port
		   (reason+message
		    "no more reductions"
		    "going to the previous (more recent) subproblem."))
		  (if (only-latest-reduction? dstate**)
		      dstate**
		      (dstate-earliest-reduction dstate**))))))
	(later-subproblem dstate port #f #f))))

;;;; Environment motion and display

(define (command/show-current-frame dstate port)
  (if (dstate-have-environment? dstate)
      (show-current-frame dstate #f port)
      (begin
	(undefined-environment port)
	dstate)))

(define (command/show-all-frames dstate port)
  (if (dstate-have-environment? dstate)
      (show-frames (dstate-current-environment dstate) 0 port)
      (begin
	(undefined-environment port)
	dstate)))

(define (command/move-to-parent-environment dstate port)
  (if (dstate-have-environment? dstate)
      (let ((dstate* (dstate-parent-environment dstate)))
	(if dstate*
	    (begin
	      (show-current-frame dstate* #t port)
	      dstate*)
	    (begin
	      (debugger-failure port "The current environment has no parent.")
	      dstate)))
      (begin
	(undefined-environment port)
	dstate)))

(define (command/move-to-child-environment dstate port)
  (if (dstate-have-environment? dstate)
      (let ((dstate* (dstate-child-environment dstate)))
	(if dstate*
	    (begin
	      (show-current-frame dstate* #t port)
	      dstate*)
	    (begin
	      (debugger-failure
	       port
	       "This is the initial environment; can't move to child.")
	      dstate)))
      (undefined-environment port)))

(define (show-current-frame dstate brief? port)
  (port/debugger-presentation port
    (lambda ()
      (show-frame (dstate-current-environment dstate)
		  (dstate-current-environment-index dstate)
		  brief?
		  port))))

(define (command/enter-read-eval-print-loop dstate port)
  (debug/read-eval-print (get-evaluation-environment dstate port)
			 "the debugger"
			 "the environment for this frame")
  dstate)

(define (command/eval-in-current-environment dstate port)
  (debug/read-eval-print-1 (get-evaluation-environment dstate port) port)
  dstate)

(define (command/enter-where dstate port)
  (with-current-environment dstate port debug/where)
  dstate)

;;;; Condition commands

(define (command/condition-report dstate port)
  (let ((condition (dstate-condition dstate)))
    (if condition
	(port/debugger-presentation port
	  (lambda ()
	    (write-condition-report condition port)))
	(debugger-failure port "No condition to report.")))
  dstate)

(define (command/condition-restart dstate port)
  (let ((condition (dstate-condition dstate)))
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
	      (port/debugger-presentation port
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

(define (command/return-from dstate port)
  (let ((frames (stream-cdr (dstate-frames dstate))))
    (if (stream-pair? frames)
	(enter-subproblem dstate port frames)
	(debugger-failure port "Can't continue!!!"))))

(define (command/return-to dstate port)
  (enter-subproblem dstate port (dstate-frames dstate)))

(define (enter-subproblem dstate port frames)
  (let ((invalid-expression?
	 (invalid-expression? (dstate-expression dstate)))
	(environment (get-evaluation-environment dstate port)))
    (let ((value
	   (let ((expression
		  (prompt-for-expression
		   (string-append
		    "Expression to EVALUATE and CONTINUE with"
		    (if invalid-expression?
			""
			" ($ to retry)"))
		   port)))
	     (if (and (not invalid-expression?)
		      (eq? expression '$))
		 (debug/scode-eval (dstate-expression dstate)
				   environment)
		 (debug/eval expression environment)))))
      (if (or (not debugger:print-return-values?)
	      (begin
		(newline port)
		(write-string "That evaluates to:" port)
		(newline port)
		(write value port)
		(prompt-for-confirmation "Confirm" port)))
	  (let ((k (cframe-stream->continuation frames))
		(thread (dstate-other-thread dstate)))
	    (if thread
		(begin
		  (restart-thread thread 'ask
		    (lambda ()
		      (k value)))
		  (continue-from-derived-thread-error
		   (dstate-condition dstate)))
		(k value))))))
  dstate)

(define (command/internal dstate port)
  (declare (ignore port))
  (debug/read-eval-print (->environment '(runtime debugger))
			 "the debugger"
			 "the debugger environment")
  dstate)

(define (command/frame dstate port)
  (port/debugger-presentation port
    (lambda ()
      (write-string "Stack frame: " port)
      (write (dstate-frame dstate) port)
      (for-each (lambda (element)
		  (newline port)
		  (debugger-pp element 0 port))
		(pp-description (dstate-frame dstate)))))
  dstate)

(define (command/print-frame-elements dstate port)
  (port/debugger-presentation port
    (lambda ()
      (write-string "Stack frame elements:" port)
      (vector-for-each (lambda (element)
			 (newline)
			 (write element))
		       (cframe-raw (dstate-frame dstate)))))
  dstate)

;;;; Utilities

(define (invalid-expression? expression)
  (or (cframe-dbg-expression-undefined? expression)
      (cframe-dbg-expression-compiled? expression)))

(define (get-evaluation-environment dstate port)
  (if (dstate-have-environment? dstate)
      (dstate-environment dstate)
      (begin
	(debugger-message
	 port
	 "Cannot evaluate in current environment;
using the read-eval-print environment instead.")
	(nearest-repl/environment))))

(define (with-current-environment dstate port receiver)
  (if (dstate-have-environment? dstate)
      (receiver (dstate-environment dstate))
      (undefined-environment port)))

(define (undefined-environment port)
  (debugger-failure port "There is no current environment."))

(define (reason+message reason message)
  (string-titlecase (if reason (string-append reason "; " message) message)))

(define (debugger-pp expression indentation port)
  (parameterize ((param:printer-list-depth-limit debugger:list-depth-limit)
		 (param:printer-list-breadth-limit debugger:list-breadth-limit)
		 (param:printer-string-length-limit
		  debugger:string-length-limit))
    (pretty-print expression port #t indentation)))

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