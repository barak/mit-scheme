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
;; (define debugger:count-subproblems-limit 10)
;; (define debugger:use-history? #f)
;; (define debugger:list-depth-limit 5)
;; (define debugger:list-breadth-limit 5)
;; (define debugger:string-length-limit 70)

(define (ndebug #!optional object)
  (if (default-object? object)
      (let ((condition (nearest-repl/condition)))
	(if condition
	    (debug-internal condition)
	    (call-with-current-continuation
	      (lambda (k)
		(debug-internal k)))))
      (debug-internal object)))

(define (debug-internal object)
  (let ((dstate (initial-dstate object)))
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
		(print-frame-summary dstate port)))
	    (debugger-message
	     port
	     "You are now in the debugger.  Type q to quit, ? for commands.")))
	 "debug>"
	 dstate)))))

(define (count-subproblems dstate)
  (let loop ((dstates (dstate-all-subproblems dstate)) (n 0))
    (if (and (stream-pair? dstates)
	     (<= n debugger:count-subproblems-limit))
	(loop (stream-cdr dstates) (+ n 1))
	n)))

(define-deferred command-set
  (make-command-set 'debug-commands
    `((#\? ,standard-help-command "help, list command letters")
      (#\q ,standard-exit-command "Quit (exit debugger)"))
    'immutable-state? #t))

(define-deferred command-procedures
  (alist-table eq?))

(define (define-command name letter help-text proc)
  (add-boot-init!
   (lambda ()
     (define-letter-command command-set letter help-text proc)
     (alist-table-set! command-procedures name proc))))

(define (call-debugger-command name dstate port)
  ((alist-table-ref command-procedures name) dstate port))

;;;; Display commands

(define-command 'print-subproblem-summary #\t
  "print the current subproblem or reduction"
  (lambda (dstate port)
    (print-frame-summary dstate port)))

(define (print-frame-summary dstate port)
  (port/debugger-presentation port
    (lambda ()
      (if (dstate-use-history? dstate)
	  (print-reduction (dstate-current-reduction dstate)
			   (dstate-subproblem-index dstate)
			   (dstate-reduction-index dstate)
			   port)
	  (print-subproblem dstate port))))
  dstate)

(define-command 'print-reductions #\r
  "print the execution history (Reductions) of the current subproblem level"
  (lambda (dstate port)
    (let ((subproblem-index (dstate-subproblem-index dstate)))
      (if (dstate-has-reductions? dstate)
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
    dstate))

(define-command 'print-subproblem-expression #\l
  "(List expression) pretty print the current expression"
  (lambda (dstate port)
    (port/debugger-presentation port
      (lambda ()
	(let ((expression (dstate-dbg-expression dstate)))
	  (cond ((dbg-expression-compiled? expression)
		 (write-string ";compiled code" port))
		((dbg-expression-undefined? expression)
		 (write-string ";undefined expression" port))
		((dbg-printer? expression)
		 (write-string ";" port)
		 (dbg-printer-apply expression #f port))
		(else
		 (pretty-print expression port #t 0))))))
    dstate))

(define-command 'print-environment-procedure #\o
  "pretty print the procedure that created the current environment"
  (lambda (dstate port)
    (with-current-environment dstate port
      (lambda (environment)
	(show-environment-procedure environment port)))
    dstate))

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
    (cond ((not (dstate-earlier-subproblem? dstate))
	   (qualify-level (if (zero? index) "only" "highest")))
	  ((zero? index)
	   (qualify-level "lowest")))))

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

;;;; Subproblem summary

(define-command 'print-subproblem-summary #\h
  "prints a summary (History) of all subproblems"
  (lambda (dstate port)
    (let ((dstates (dstate-all-subproblems dstate)))
      (port/debugger-presentation port
	(lambda ()
	  (write-string "SL#  Procedure/form          Expression" port)
	  (newline port)
	  (let loop ((dstates dstates) (level 0))
	    (if (stream-pair? dstates)
		(begin
		  (terse-print-expression level (stream-car dstates) port)
		  (loop (stream-cdr dstates) (+ level 1))))))))
    dstate))

(define (terse-print-expression level dstate port)
  (let ((expression (dstate-dbg-expression dstate))
	(environment (dstate-dbg-environment dstate)))
    (newline port)
    (write-string (string-pad-right (number->string level) 4) port)
    (write-string " " port)
    (write-string
     (string-pad-right
      (let ((name
	     (and (environment? environment)
		  (environment-procedure-name environment))))
	(if (not name)
	    ""
	    (output-to-string 20
	      (lambda ()
		(write-dbg-name (or (scode-lambda-name->syntax-name name)
				    name)
				(current-output-port))))))
      20)
     port)
    (write-string "    " port)
    (write-string
     (cond ((dbg-expression-compiled? expression)
	    ";compiled code")
	   ((dbg-expression-undefined? expression)
	    ";undefined expression")
	   ((dbg-printer? expression)
	    (output-to-string terse-print-expression-limit
	      (lambda ()
		(dbg-printer-apply expression #f (current-output-port)))))
	   (else
	    (output-to-string terse-print-expression-limit
	      (lambda ()
		(parameterize ((param:print-primitives-by-name? #t))
		  (write (unsyntax expression)))))))
     port)))

(define terse-print-expression-limit 50)

;;;; Subproblem motion

(define-command 'move-to-earlier-subproblem #\u
  "move (Up) to the next subproblem (earlier in time)"
  (lambda (dstate port)
    (earlier-subproblem (dstate-stop-using-history dstate) port #f #f)))

(define (earlier-subproblem dstate port failure-reason if-succeed)
  (let ((dstate* (dstate-earlier-subproblem dstate)))
    (if dstate*
	(print-frame-summary (if if-succeed (if-succeed dstate* port) dstate*)
			     port)
	(begin
	  (debugger-failure
	   port
	   (reason+message (or failure-reason "no more subproblems")
			   "already at highest subproblem level."))
	  dstate))))

(define-command 'move-to-later-subproblem #\d
  "move (Down) to the previous subproblem (later in time)"
  (lambda (dstate port)
    (later-subproblem (dstate-stop-using-history dstate) port #f #f)))

(define (later-subproblem dstate port failure-reason if-succeed)
  (let ((dstate* (dstate-later-subproblem dstate)))
    (if dstate*
	(print-frame-summary (if if-succeed (if-succeed dstate* port) dstate*)
			     port)
	(begin
	  (debugger-failure
	   port
	   (reason+message (or failure-reason "no more subproblems")
			   "already at lowest subproblem level."))
	  dstate))))

(define-command 'move-to-specified-subproblem #\g
  "Go to a particular subproblem"
  (lambda (dstate port)
    (let ((dstate* (dstate-stop-using-history dstate)))
      (let loop ((limit #f))
	(let ((dstate**
	       (dstate-nth-subproblem
		dstate*
		(prompt-for-nonnegative-integer "Subproblem number" limit
						port))))
	  (if dstate**
	      (print-frame-summary dstate** port)
	      (loop (dstate-n-subproblems dstate*))))))))

;;;; Reduction motion

(define (only-latest-reduction? dstate)
  (and debugger:student-walk?
       (> (dstate-subproblem-index dstate) 0)))

(define-command 'move-to-earlier-reduction #\b
  "move (Back) to next reduction (earlier in time)"
  (lambda (dstate port)
    (let ((dstate* (dstate-start-using-history dstate)))
      (if (dstate-use-history? dstate*)
	  (let ((dstate**
		 (and (not (only-latest-reduction? dstate*))
		      (dstate-earlier-reduction dstate*))))
	    (if dstate**
		(print-frame-summary dstate** port)
		(earlier-subproblem dstate* port "no more reductions"
		  (lambda (dstate** port)
		    (if (not debugger:student-walk?)
			(debugger-message
			 port
			 (reason+message
			  "no more reductions"
			  "going to the next (less recent) subproblem.")))
		    dstate**))))
	  (earlier-subproblem dstate* port #f #f)))))

(define-command 'move-to-later-reduction #\f
  "move (Forward) to previous reduction (later in time)"
  (lambda (dstate port)
    (let ((dstate* (dstate-start-using-history dstate)))
      (if (dstate-use-history? dstate*)
	  (let ((dstate** (dstate-later-reduction dstate*)))
	    (if dstate**
		(print-frame-summary dstate** port)
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
	  (later-subproblem dstate port #f #f)))))

;;;; Environment motion and display

(define-command 'print-current-env-frame #\c
  "show bindings of identifiers in the Current environment"
  (lambda (dstate port)
    (if (dstate-has-environment? dstate)
	(print-current-frame dstate #f port)
	(undefined-environment dstate port))))

(define-command 'print-all-env-frames #\a
  "show All bindings in current environment and its ancestors"
  (lambda (dstate port)
    (if (dstate-has-environment? dstate)
	(begin
	  (show-frames (dstate-current-environment dstate) 0 port)
	  dstate)
	(undefined-environment dstate port))))

(define-command 'move-to-parent-environment #\p
  "move to environment that is Parent of current environment"
  (lambda (dstate port)
    (if (dstate-has-environment? dstate)
	(let ((dstate* (dstate-parent-environment dstate)))
	  (if dstate*
	      (print-current-frame dstate* #t port)
	      (begin
		(debugger-failure port
				  "The current environment has no parent.")
		dstate)))
	(undefined-environment dstate port))))

(define-command 'move-to-child-environment #\s
  "move to child of current environment (in current chain)"
  (lambda (dstate port)
    (if (dstate-has-environment? dstate)
	(let ((dstate* (dstate-child-environment dstate)))
	  (if dstate*
	      (print-current-frame dstate* #t port)
	      (begin
		(debugger-failure
		 port
		 "This is the initial environment; can't move to child.")
		dstate)))
	(undefined-environment dstate port))))

(define (print-current-frame dstate brief? port)
  (port/debugger-presentation port
    (lambda ()
      (show-frame (dstate-current-environment dstate)
		  (dstate-current-environment-index dstate)
		  brief?
		  port)))
  dstate)

(define-command 'repl-in-current-env-frame #\e
  "Enter a read-eval-print loop in the current environment"
  (lambda (dstate port)
    (debug/read-eval-print (get-evaluation-environment dstate port)
			   "the debugger"
			   "the environment for this frame")
    dstate))

(define-command 'eval-in-current-env-frame #\v
  "eValuate expression in current environment"
  (lambda (dstate port)
    (debug/read-eval-print-1 (get-evaluation-environment dstate port) port)
    dstate))

(define-command 'inspect-current-env-frame #\w
  "enter environment inspector (Where) on the current environment"
  (lambda (dstate port)
    (with-current-environment dstate port debug/where)
    dstate))

;;;; Condition commands

(define-command 'print-condition-report #\i
  "redisplay the error message Info"
  (lambda (dstate port)
    (let ((condition (dstate-condition dstate)))
      (if condition
	  (port/debugger-presentation port
	    (lambda ()
	      (write-condition-report condition port)))
	  (debugger-failure port "No condition to report.")))
    dstate))


(define-command 'invoke-restart #\k
  "continue the program using a standard restart option"
  (lambda (dstate port)
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
					       port)))))))))))))

;;;; Advanced hacking commands

(define-command 'return-from-subproblem #\Z
  "return from the current subproblem with a value"
  (lambda (dstate port)
    (let ((dstate* (dstate-earlier-subproblem dstate)))
      (if dstate*
	  (enter-subproblem dstate* port)
	  (begin
	    (debugger-failure port "Can't continue!!!")
	    dstate)))))

(define-command 'return-to-subproblem #\J
  "return to the current subproblem with a value"
  (lambda (dstate port)
    (enter-subproblem dstate port)))

(define (enter-subproblem dstate port)
  (let ((exp (dstate-dbg-expression dstate))
	(env (get-evaluation-environment dstate port)))
    (let ((value
	   (prompt-for-evaluated-value
	    "Expression to EVALUATE and CONTINUE with" exp env port)))
      (if (or (not debugger:print-return-values?)
	      (begin
		(newline port)
		(write-string "That evaluates to:" port)
		(newline port)
		(write value port)
		(prompt-for-confirmation "Confirm" port)))
	  (let ((k (dstate-continuation dstate))
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

(define-command 'print-raw-stack-frame #\M
  "show the elements of the stack frame, in raw form"
  (lambda (dstate port)
    (port/debugger-presentation port
      (lambda ()
	(write-string "Stack frame elements:" port)
	(vector-for-each (lambda (element)
			   (newline port)
			   (write element port))
			 (dstate-raw-frame dstate))))
    dstate))

(define-command 'print-internal-state #\S
  "show the debugger's internal State"
  (lambda (dstate port)
    (port/debugger-presentation port
      (lambda ()
	(write-string "Debugger state:" port)
	(newline port)
	(pp dstate port)))
    dstate))

;;;; Utilities

(define (get-evaluation-environment dstate port)
  (if (dstate-has-environment? dstate)
      (dstate-current-environment dstate)
      (begin
	(debugger-message
	 port
	 "Cannot evaluate in current environment;
using the read-eval-print environment instead.")
	(nearest-repl/environment))))

(define (with-current-environment dstate port receiver)
  (if (dstate-has-environment? dstate)
      (receiver (dstate-environment dstate))
      (undefined-environment dstate port)))

(define (undefined-environment dstate port)
  (debugger-failure port "There is no current environment.")
  dstate)

(define (reason+message reason message)
  (string-titlecase (if reason (string-append reason "; " message) message)))

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

(define (prompt-for-evaluated-value prompt exp env port)
  (let ((exp-evaluable?
	 (not (or (dbg-expression-undefined? exp)
		  (dbg-expression-compiled? exp)
		  (dbg-printer? exp)))))
    (let ((exp*
	   (prompt-for-expression
	    (string-append prompt (if exp-evaluable? " ($ to retry)" ""))
	    port)))
      (if (and exp-evaluable? (eq? exp* '$))
	  (debug/scode-eval exp env)
	  (debug/eval exp* env)))))