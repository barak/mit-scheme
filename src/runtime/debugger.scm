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
;;; package: (runtime debugger)

(declare (usual-integrations))

(define debugger:student-walk? #f)
(define debugger:print-return-values? #f)
(define debugger:auto-toggle? #t)
(define debugger:count-subproblems-limit 10)
(define debugger:use-history? #f)
(define debugger:list-depth-limit 5)
(define debugger:list-breadth-limit 5)
(define debugger:string-length-limit 70)

(define (debug #!optional object)
  (if (default-object? object)
      (let ((condition (nearest-repl/condition)))
	(if condition
	    (debug-internal condition)
	    (call-with-current-continuation
	      (lambda (k)
		(debug-internal k)))))
      (debug-internal object)))

(define (debug-internal object)
  (let ((dstate
	 (new-subproblem (ctree-subproblems (make-ctree object))
			 (cond (debugger:use-history? 'always)
			       (debugger:auto-toggle? 'enabled)
			       (else 'disabled)))))
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

(define-record-type <dstate>
    make-dstate
    dstate?
  (ctree-node dstate-ctree-node)
  (hist-state dstate-hist-state)
  (env-list dstate-env-list))

(define (select-subproblem snode dstate)
  (new-subproblem snode (dstate-hist-state dstate)))

(define (new-subproblem snode hist-state)
  (let ((rnode
	 (and (hist-state:using-history? hist-state)
	      (ctree-subproblem-reductions snode))))
    (if rnode
	(new-reduction rnode hist-state)
	(make-dstate snode
		     hist-state
		     (if (ctree-subproblem-has-environment? snode)
			 (list (ctree-subproblem-environment snode))
			 '())))))

(define (select-reduction rnode dstate)
  (new-reduction rnode (dstate-hist-state dstate)))

(define (new-reduction rnode hist-state)
  (make-dstate rnode
	       hist-state
	       (list (ctree-reduction-environment rnode))))

(define (dstate-using-history? dstate)
  (hist-state:using-history? (dstate-hist-state dstate)))

(define (hist-state:using-history? hist-state)
  (or (eq? hist-state 'always)
      (eq? hist-state 'now)))

(define (dstate-start-using-history dstate)
  (if (eq? (dstate-hist-state dstate) 'enabled)
      (new-hist-state 'now dstate)
      dstate))

(define (dstate-stop-using-history dstate)
  (if (eq? (dstate-hist-state dstate) 'now)
      (new-hist-state 'enabled dstate)
      dstate))

(define (new-hist-state hist-state dstate)
  (make-dstate (dstate-ctree-node dstate)
	       hist-state
	       (dstate-env-list dstate)))

(define (dstate-has-environment? dstate)
  (pair? (dstate-env-list dstate)))

(define (dstate-environment dstate)
  (let ((envs (dstate-env-list dstate)))
    (assert (pair? envs))
    (car envs)))

(define (dstate-environment-index dstate)
  (let ((envs (dstate-env-list dstate)))
    (assert (pair? envs))
    (length (cdr envs))))

(define (dstate-parent-environment dstate)
  (let ((envs (dstate-env-list dstate)))
    (assert (pair? envs))
    (and (eq? #t (environment-has-parent? (car envs)))
	 (select-environment (cons (environment-parent (car envs)) envs)
			     dstate))))

(define (dstate-child-environment dstate)
  (let ((envs (dstate-env-list dstate)))
    (assert (pair? envs))
    (and (pair? (cdr envs))
	 (select-environment (cdr envs) dstate))))

(define (select-environment env-list dstate)
  (make-dstate (dstate-ctree-node dstate)
	       (dstate-hist-state dstate)
	       env-list))

(define (dstate-ctree-subproblem dstate)
  (->ctree-subproblem (dstate-ctree-node dstate)))

(define (dstate-ctree dstate)
  (->ctree (dstate-ctree-node dstate)))

(define (count-subproblems dstate)
  (let loop ((node (ctree-subproblems (dstate-ctree dstate))) (n 0))
    (if (and node
	     (<= n debugger:count-subproblems-limit))
	(loop (ctree-subproblem-earlier node) (+ n 1))
	n)))

(define (dstate-other-thread dstate)
  (let ((condition (ctree-condition (dstate-ctree dstate))))
    (and condition
	 (condition/other-thread condition))))

(define (dstate-condition dstate)
  (ctree-condition (dstate-ctree dstate)))

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

(define-command 'print-reductions #\r
  "print the execution history (Reductions) of the current subproblem level"
  (lambda (dstate port)
    (let ((snode (dstate-ctree-subproblem dstate)))
      (let ((rnode (ctree-subproblem-reductions snode)))
	(if rnode
	    (port/debugger-presentation port
	      (lambda ()
		(write-string "Execution history for this subproblem:" port)
		(let loop ((rnode rnode))
		   (newline port)
		   (write-string "----------------------------------------"
				 port)
		   (newline port)
		   (print-reduction rnode port)
		   (let ((rnode* (ctree-reduction-earlier rnode)))
		     (if rnode*
			 (loop rnode*))))))
	    (debugger-failure
	     port
	     "There is no execution history for this subproblem."))))
    dstate))

(define-command 'print-subproblem-expression #\l
  "(List expression) pretty print this subproblem's expression"
  (lambda (dstate port)
    (port/debugger-presentation port
      (lambda ()
	(let ((expression
	       (ctree-subproblem-expression (dstate-ctree-subproblem dstate))))
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

(define (print-frame-summary dstate port)
  (print-cnode-summary (dstate-ctree-node dstate) port)
  dstate)

(define (print-cnode-summary cnode port)
  (port/debugger-presentation port
    (lambda ()
      (if (ctree-reduction? cnode)
	  (print-reduction cnode port)
	  (print-subproblem cnode port)))))

(define (print-subproblem snode port)
  (print-subproblem-identification snode port)
  (newline port)
  (print-subproblem-expression snode port)
  (print-subproblem-environment snode port)
  (print-subproblem-reduction snode port))

(define (print-subproblem-identification snode port)
  (write-string "Subproblem level: " port)
  (let ((qualify-level
	 (lambda (adjective)
	   (write-string " (this is the " port)
	   (write-string adjective port)
	   (write-string " subproblem level)" port))))
    (write (ctree-subproblem-index snode) port)
    (if (not (ctree-subproblem-earlier snode))
	(if (not (ctree-subproblem-later snode))
	    (qualify-level "only")
	    (qualify-level "earliest"))
	(if (not (ctree-subproblem-later snode))
	    (qualify-level "latest")))))

(define (print-subproblem-reduction snode port)
  (let ((n-reductions (ctree-subproblem-n-reductions snode)))
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

(define (print-reduction rnode port)
  (print-reduction-identification rnode port)
  (newline port)
  (print-reduction-expression rnode port)
  (print-reduction-environment rnode port))

(define (print-reduction-identification rnode port)
  (write-string "Subproblem level: " port)
  (write (ctree-subproblem-index (ctree-reduction-subproblem rnode)) port)
  (write-string "  Reduction number: " port)
  (write (ctree-reduction-index rnode) port))

;;;; Subproblem summary

(define-command 'print-subproblem-summary #\h
  "prints a summary (History) of all subproblems"
  (lambda (dstate port)
    (port/debugger-presentation port
      (lambda ()
	(write-string "SL#  Procedure/form          Expression" port)
	(newline port)
	(let loop ((snode (ctree-subproblems (dstate-ctree dstate))))
	  (if snode
	      (begin
		(terse-print-expression snode port)
		(loop (ctree-subproblem-earlier snode)))))))
    dstate))

(define (terse-print-expression snode port)
  (let ((expression (ctree-subproblem-expression snode))
	(environment (ctree-subproblem-environment snode)))
    (newline port)
    (write-string
     (string-pad-right (number->string (ctree-subproblem-index snode)) 4)
     port)
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
		(write-dbg-name (or (scode-lambda-name->syntax-name name) name)
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
  (let ((snode (ctree-subproblem-earlier (dstate-ctree-subproblem dstate))))
    (if snode
	(print-frame-summary (let ((dstate* (select-subproblem snode dstate)))
			       (if if-succeed
				   (if-succeed dstate* port)
				   dstate*))
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
  (let ((snode (ctree-subproblem-later (dstate-ctree-subproblem dstate))))
    (if snode
	(print-frame-summary (let ((dstate* (select-subproblem snode dstate)))
			       (if if-succeed
				   (if-succeed dstate* port)
				   dstate*))
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
      (let ((ctree (dstate-ctree dstate*)))
	(let loop ((limit #f))
	  (let ((snode
		 (ctree-nth-subproblem
		  ctree
		  (prompt-for-nonnegative-integer "Subproblem number" limit
						  port))))
	    (if snode
		(print-frame-summary (select-subproblem snode dstate*) port)
		(loop (ctree-n-subproblems ctree)))))))))

;;;; Reduction motion

(define (only-latest-reduction? cnode)
  (and debugger:student-walk?
       (> (ctree-subproblem-index cnode) 0)))

(define-command 'move-to-earlier-reduction #\b
  "move (Back) to next reduction (earlier in time)"
  (lambda (dstate port)
    (let ((dstate* (dstate-start-using-history dstate)))
      (if (dstate-using-history? dstate*)
	  (let ((rnode
		 (let ((cnode (dstate-ctree-node dstate*)))
		   (and (ctree-reduction? cnode)
			(not (only-latest-reduction? cnode))
			(ctree-reduction-earlier cnode)))))
	    (if rnode
		(print-frame-summary (select-reduction rnode dstate*)
				     port)
		(earlier-subproblem dstate* port "no more reductions"
				    select-latest-reduction)))
	  (earlier-subproblem dstate* port #f #f)))))

(define (select-latest-reduction dstate port)
  (if (not debugger:student-walk?)
      (debugger-message
       port
       (reason+message
	"no more reductions"
	"going to the next (less recent) subproblem.")))
  dstate)

(define-command 'move-to-later-reduction #\f
  "move (Forward) to previous reduction (later in time)"
  (lambda (dstate port)
    (let ((dstate* (dstate-start-using-history dstate)))
      (if (dstate-using-history? dstate*)
	  (let ((rnode
		 (let ((cnode (dstate-ctree-node dstate*)))
		   (and (ctree-reduction? cnode)
			(not (only-latest-reduction? cnode))
			(ctree-reduction-later cnode)))))
	    (if rnode
		(print-frame-summary (select-reduction rnode dstate*) port)
		(later-subproblem dstate port "no more reductions"
				  select-earliest-reduction)))
	  (later-subproblem dstate port #f #f)))))

(define (select-earliest-reduction dstate port)
  (if (not debugger:student-walk?)
      (debugger-message
       port
       (reason+message
	"no more reductions"
	"going to the previous (more recent) subproblem.")))
  (let ((cnode (dstate-ctree-node dstate)))
    (if (and (ctree-reduction? cnode)
	     (not (only-latest-reduction? cnode)))
	(select-reduction (ctree-reduction-earliest cnode) dstate)
	dstate)))

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
	  (show-frames (dstate-environment dstate) 0 port)
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
      (show-frame (dstate-environment dstate)
		  (dstate-environment-index dstate)
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
    (debug/invoke-restart (dstate-ctree dstate) port)))

;;;; Advanced hacking commands

(define-command 'return-from-subproblem #\Z
  "return from the current subproblem with a value"
  (lambda (dstate port)
    (let ((snode
	   (ctree-subproblem-earlier (dstate-ctree-subproblem dstate))))
      (if snode
	  (enter-subproblem snode
			    (ctree-condition (dstate-ctree dstate))
			    port)
	  (debugger-failure port "Can't continue!!!")))
    dstate))

(define-command 'return-to-subproblem #\J
  "return to the current subproblem with a value"
  (lambda (dstate port)
    (enter-subproblem (dstate-ctree-subproblem dstate)
		      (ctree-condition (dstate-ctree dstate))
		      port)
    dstate))

(define (enter-subproblem snode condition port)
  (let ((value
	 (prompt-for-evaluated-value
	  "Expression to EVALUATE and CONTINUE with" snode port)))
    (if (or (not debugger:print-return-values?)
	    (begin
	      (newline port)
	      (write-string "That evaluates to:" port)
	      (newline port)
	      (write value port)
	      (prompt-for-confirmation "Confirm" port)))
	(let ((k (ctree-subproblem->continuation snode))
	      (thread
	       (and condition
		    (condition/other-thread condition))))
	  (if thread
	      (begin
		(restart-thread thread 'ask
		  (lambda ()
		    (k value)))
		(continue-from-derived-thread-error condition))
	      (k value))))))

(define-command 'print-raw-stack-frame #\M
  "show the elements of the stack frame, in raw form"
  (lambda (dstate port)
    (print-raw-frame (dstate-ctree-subproblem dstate) port)
    dstate))

(define (print-raw-frame snode port)
  (port/debugger-presentation port
    (lambda ()
      (write-string "Stack frame elements:" port)
      (vector-for-each (lambda (element)
			 (newline port)
			 (write element port))
		       (ctree-subproblem-raw-frame snode)))))

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
      (dstate-environment dstate)
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
  (no-current-environment port)
  dstate)

(define (reason+message reason message)
  (string-titlecase (if reason (string-append reason "; " message) message)))