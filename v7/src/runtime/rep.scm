#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/rep.scm,v 14.24 1992/02/25 22:56:08 cph Exp $

Copyright (c) 1988-92 Massachusetts Institute of Technology

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

;;;; Read-Eval-Print Loop
;;; package: (runtime rep)

(declare (usual-integrations))

(define repl:allow-restart-notifications?
  true)

(define (initialize-package!)
  (set! *nearest-cmdl* false)
  (set! hook/repl-eval default/repl-eval)
  (set! hook/repl-write default/repl-write)
  (set! hook/set-default-environment default/set-default-environment)
  (set! hook/error-decision false)
  unspecific)

(define (initial-top-level-repl)
  (call-with-current-continuation
   (lambda (continuation)
     (set! root-continuation continuation)
     (repl/start (make-repl false
			    console-i/o-port
			    user-initial-environment
			    user-initial-syntax-table
			    false
			    '()
			    user-initial-prompt)
		 (cmdl-message/strings "Cold load finished")))))

(define root-continuation)

;;;; Command Loops

(define cmdl-rtd
  (make-record-type "cmdl" '(LEVEL PARENT PORT DRIVER STATE OPERATIONS)))

(define cmdl? (record-predicate cmdl-rtd))
(define cmdl/level (record-accessor cmdl-rtd 'LEVEL))
(define cmdl/parent (record-accessor cmdl-rtd 'PARENT))
(define cmdl/port (record-accessor cmdl-rtd 'PORT))
(define set-cmdl/port! (record-updater cmdl-rtd 'PORT))
(define cmdl/driver (record-accessor cmdl-rtd 'DRIVER))
(define cmdl/state (record-accessor cmdl-rtd 'STATE))
(define set-cmdl/state! (record-updater cmdl-rtd 'STATE))
(define cmdl/operations (record-accessor cmdl-rtd 'OPERATIONS))

(define make-cmdl
  (let ((constructor
	 (record-constructor cmdl-rtd
			     '(LEVEL PARENT PORT DRIVER STATE OPERATIONS))))
    (lambda (parent port driver state operations)
      (if (not (or (false? parent) (cmdl? parent)))
	  (error:wrong-type-argument parent "cmdl" 'MAKE-CMDL))
      (constructor (if parent (+ (cmdl/level parent) 1) 1)
		   parent
		   port
		   driver
		   state
		   (parse-operations-list operations 'MAKE-CMDL)))))

(define (push-cmdl driver state operations)
  (let ((parent (nearest-cmdl)))
    (make-cmdl parent (cmdl/port parent) driver state operations)))

(define (cmdl/base cmdl)
  (let ((parent (cmdl/parent cmdl)))
    (if parent
	(cmdl/base parent)
	cmdl)))

(define (cmdl/start cmdl message)
  (let ((operation
	 (let ((parent (cmdl/parent cmdl)))
	   (and parent
		(cmdl/local-operation parent 'START-CHILD))))
	(thunk
	 (lambda ()
	   (fluid-let ((*nearest-cmdl* cmdl)
		       (dynamic-handler-frames '())
		       (*bound-restarts*
			(if (cmdl/parent cmdl) *bound-restarts* '())))
	     (let loop ((message message))
	       (loop
		(call-with-current-continuation
		 (lambda (continuation)
		   (bind-restart 'ABORT
		       (string-append "Return to "
				      (if (repl? cmdl)
					  "read-eval-print"
					  "command")
				      " level "
				      (number->string (cmdl/level cmdl))
				      ".")
		       (lambda (#!optional message)
			 (continuation
			  (if (default-object? message)
			      (cmdl-message/strings "Abort!")
			      message)))
		     (lambda (restart)
		       (restart/put! restart make-cmdl cmdl)
		       (with-interrupt-mask interrupt-mask/all
			 (lambda (interrupt-mask)
			   interrupt-mask
			   (unblock-thread-events)
			   (message cmdl)
			   ((cmdl/driver cmdl) cmdl)))))))))))))
    (if operation
	(operation cmdl thunk)
	(with-thread-mutex-locked (port/thread-mutex (cmdl/port cmdl))
	  thunk))))

(define *nearest-cmdl*)

(define (nearest-cmdl)
  (if (not *nearest-cmdl*) (error "NEAREST-CMDL: no cmdl"))
  *nearest-cmdl*)

(define (nearest-cmdl/port)
  (let ((cmdl *nearest-cmdl*))
    (if cmdl
	(cmdl/port cmdl)
	console-i/o-port)))

(define (nearest-cmdl/level)
  (let ((cmdl *nearest-cmdl*))
    (if cmdl
	(cmdl/level cmdl)
	0)))

;;;; Operations

(define (parse-operations-list operations procedure)
  (if (not (list? operations))
      (error:wrong-type-argument operations "list" procedure))
  (map (lambda (operation)
	 (if (not (and (pair? operation)
		       (symbol? (car operation))
		       (pair? (cdr operation))
		       (procedure? (cadr operation))
		       (null? (cddr operation))))
	     (error:wrong-type-argument operation
					"operation binding"
					procedure))
	 (cons (car operation) (cadr operation)))
       operations))

(define (cmdl/local-operation cmdl name)
  (let ((binding (assq name (cmdl/operations cmdl))))
    (and binding
	 (cdr binding))))

(define (cmdl/operation cmdl name)
  (let loop ((cmdl cmdl))
    (or (cmdl/local-operation cmdl name)
	(let ((parent (cmdl/parent cmdl)))
	  (and parent
	       (loop parent))))))

(define (cmdl/operation-names cmdl)
  (let cmdl-loop ((cmdl cmdl) (names '()))
    (let loop ((bindings (cmdl/operations cmdl)) (names names))
      (if (null? bindings)
	  (let ((parent (cmdl/parent cmdl)))
	    (if parent
		(cmdl-loop parent names)
		names))
	  (loop (cdr bindings)
		(if (memq (caar bindings) names)
		    names
		    (cons (caar bindings) names)))))))

;;;; Messages

(define ((cmdl-message/strings . strings) cmdl)
  (let ((port (cmdl/port cmdl)))
    (port/with-output-terminal-mode port 'COOKED
      (lambda ()
	(for-each (lambda (string)
		    (fresh-line port)
		    (write-string ";" port)
		    (write-string string port))
		  strings)))))

(define ((cmdl-message/active actor) cmdl)
  (let ((port (cmdl/port cmdl)))
    (port/with-output-terminal-mode port 'COOKED
      (lambda ()
	(actor port)))))

(define (cmdl-message/append . messages)
  (let ((messages (delq! %cmdl-message/null messages)))
    (cond ((null? messages)
	   (cmdl-message/null))
	  ((null? (cdr messages))
	   (car messages))
	  (else
	   (lambda (cmdl)
	     (for-each (lambda (message) (message cmdl)) messages))))))

(define-integrable (cmdl-message/null)
  %cmdl-message/null)

(define (%cmdl-message/null cmdl)
  cmdl
  false)

;;;; Interrupts

(define (cmdl-interrupt/breakpoint)
  ((or (cmdl/operation (nearest-cmdl) 'INTERRUPT/BREAKPOINT)
       breakpoint)))

(define (cmdl-interrupt/abort-nearest)
  ((or (cmdl/operation (nearest-cmdl) 'INTERRUPT/ABORT-NEAREST)
       abort->nearest)))

(define (cmdl-interrupt/abort-previous)
  ((or (cmdl/operation (nearest-cmdl) 'INTERRUPT/ABORT-PREVIOUS)
       abort->previous)))

(define (cmdl-interrupt/abort-top-level)
  ((or (cmdl/operation (nearest-cmdl) 'INTERRUPT/ABORT-TOP-LEVEL)
       abort->top-level)))

(define (abort->nearest #!optional message)
  (invoke-abort (let ((restart (find-restart 'ABORT)))
		  (if (not restart)
		      (error:no-such-restart 'ABORT))
		  restart)
		(if (default-object? message) "Abort!" message)))

(define (abort->previous #!optional message)
  (invoke-abort (let ((restarts (find-restarts 'ABORT (bound-restarts))))
		  (let ((next (find-restarts 'ABORT (cdr restarts))))
		    (cond ((not (null? next)) (car next))
			  ((not (null? restarts)) (car restarts))
			  (else (error:no-such-restart 'ABORT)))))
		(if (default-object? message) "Up!" message)))

(define (abort->top-level #!optional message)
  (invoke-abort (let loop ((restarts (find-restarts 'ABORT (bound-restarts))))
		  (let ((next (find-restarts 'ABORT (cdr restarts))))
		    (cond ((not (null? next)) (loop next))
			  ((not (null? restarts)) (car restarts))
			  (else (error:no-such-restart 'ABORT)))))
		(if (default-object? message) "Quit!" message)))

(define (find-restarts name restarts)
  (let loop ((restarts restarts))
    (if (or (null? restarts)
	    (eq? name (restart/name (car restarts))))
	restarts
	(loop (cdr restarts)))))

(define (invoke-abort restart message)
  (let ((effector (restart/effector restart)))
    (if (restart/get restart make-cmdl)
	(effector
	 (if (string? message) (cmdl-message/strings message) message))
	(effector))))

;;;; REP Loops

(define (make-repl parent port environment syntax-table
		   #!optional condition operations prompt)
  (make-cmdl parent
	     port
	     repl-driver
	     (let ((inherit
		    (let ((repl (and parent (skip-non-repls parent))))
		      (lambda (argument default name)
			(if (eq? 'INHERIT argument)
			    (begin
			      (if (not repl)
				  (error "Can't inherit -- no REPL ancestor:"
					 name))
			      (default repl))
			    argument)))))
	       (make-repl-state
		(inherit (if (default-object? prompt) 'INHERIT prompt)
			 repl/prompt
			 'PROMPT)
		(inherit environment repl/environment 'ENVIRONMENT)
		(inherit syntax-table repl/syntax-table 'SYNTAX-TABLE)
		(if (default-object? condition) false condition)))
	     (append (if (default-object? operations) '() operations)
		     default-repl-operations)))

(define (push-repl environment syntax-table
		   #!optional condition operations prompt)
  (let ((parent (nearest-cmdl)))
    (make-repl parent
	       (cmdl/port parent)
	       environment
	       syntax-table
	       (if (default-object? condition) false condition)
	       (if (default-object? operations) '() operations)
	       (if (default-object? prompt) 'INHERIT prompt))))

(define (repl-driver repl)
  (let ((reader-history (repl/reader-history repl))
	(printer-history (repl/printer-history repl)))
    (port/set-default-environment (cmdl/port repl) (repl/environment repl))
    (port/set-default-syntax-table (cmdl/port repl) (repl/syntax-table repl))
    (fluid-let ((standard-error-hook false)
		(standard-warning-hook false))
      (do () (false)
	(hook/repl-write
	 repl
	 (let ((value
		(hook/repl-eval
		 (let ((s-expression
			(prompt-for-command-expression
			 (string-append (number->string (cmdl/level repl))
					" "
					(repl/prompt repl))
			 (cmdl/port repl))))
		   (repl-history/record! reader-history s-expression)
		   s-expression)
		 (repl/environment repl)
		 (repl/syntax-table repl))))
	   (repl-history/record! printer-history value)
	   value))))))

(define hook/repl-eval)
(define (default/repl-eval s-expression environment syntax-table)
  (let ((scode (syntax s-expression syntax-table)))
    (with-new-history (lambda () (extended-scode-eval scode environment)))))

(define hook/repl-write)
(define (default/repl-write repl object)
  (port/write-result (cmdl/port repl)
		     object
		     (and (object-pointer? object)
			  (not (interned-symbol? object))
			  (not (number? object))
			  (object-hash object))))

(define default-repl-operations
  `((START-CHILD ,(lambda (cmdl thunk) cmdl (with-history-disabled thunk)))))

(define (repl/start repl #!optional message)
  (cmdl/start repl
	      (make-repl-message repl
				 (if (default-object? message)
				     false
				     message))))

(define (make-repl-message repl message)
  (let ((condition (repl/condition repl)))
    (cmdl-message/append
     (cond ((not message)
	    (if condition
		(cmdl-message/strings (condition/report-string condition))
		(cmdl-message/null)))
	   ((string? message)
	    (cmdl-message/strings message))
	   (else
	    message))
     (if condition
	 (cmdl-message/append
	  (if (condition/error? condition)
	      (lambda (repl)
		(cond ((cmdl/operation repl 'ERROR-DECISION)
		       => (lambda (operation)
			    (operation repl condition)))
		      (hook/error-decision
		       (hook/error-decision repl condition))))
	      (cmdl-message/null))
	  (if repl:allow-restart-notifications?
	      (condition-restarts-message condition)
	      (cmdl-message/null)))
	 (cmdl-message/null))
     repl/set-default-environment)))

(define hook/error-decision)

(define (repl/set-default-environment repl)
  (let ((parent (cmdl/parent repl))
	(environment (repl/environment repl)))
    (if (not (and parent
		  (repl? parent)
		  (eq? (repl/environment parent) environment)))
	(let ((operation (cmdl/operation repl 'SET-DEFAULT-ENVIRONMENT)))
	  (if operation
	      (operation repl environment)
	      (hook/set-default-environment repl environment))))))

(define hook/set-default-environment)
(define (default/set-default-environment port environment)
  (let ((port (cmdl/port port)))
    (port/with-output-terminal-mode port 'COOKED
      (lambda ()
	(if (not (interpreter-environment? environment))
	    (begin
	      (fresh-line port)
	      (write-string ";Warning! this environment is a compiled-code environment:
; Assignments to most compiled-code bindings are prohibited,
; as are certain other environment operations."
			    port)))
	(let ((package (environment->package environment)))
	  (if package
	      (begin
		(fresh-line port)
		(write-string ";Package: " port)
		(write (package/name package) port))))))))

(define (condition-restarts-message condition)
  (cmdl-message/active
   (lambda (port)
     (fresh-line port)
     (write-string ";To continue, call RESTART with an option number:" port)
     (write-restarts (filter-restarts (condition/restarts condition)) port
       (lambda (index port)
	 (write-string "; (RESTART " port)
	 (write index port)
	 (write-string ") =>" port))))))

(define (restart #!optional n)
  (let ((restarts
	 (filter-restarts
	  (let ((condition (nearest-repl/condition)))
	    (if condition
		(condition/restarts condition)
		(bound-restarts))))))
    (let ((n-restarts (length restarts)))
      (if (zero? n-restarts)
	  (error "Can't RESTART: no options available."))
      (invoke-restart-interactively
       (list-ref
	restarts
	(- n-restarts
	   (if (default-object? n)
	       (let ((port (nearest-cmdl/port)))
		 (fresh-line port)
		 (write-string ";Choose an option by number:" port)
		 (write-restarts restarts port
		   (lambda (index port)
		     (write-string ";" port)
		     (write-string (string-pad-left (number->string index) 3)
				   port)
		     (write-string ":" port)))
		 (let loop ()
		   (let ((n
			  (prompt-for-evaluated-expression
			   "Option number"
			   (nearest-repl/environment)
			   port)))
		     (if (and (exact-integer? n) (<= 1 n n-restarts))
			 n
			 (begin
			   (beep port)
			   (fresh-line port)
			   (write-string
			    ";Option must be an integer between 1 and "
			    port)
			   (write n-restarts port)
			   (write-string ", inclusive.")
			   (loop))))))
	       (begin
		 (if (not (exact-integer? n))
		     (error:wrong-type-argument n "exact integer" 'RESTART))
		 (if (not (<= 1 n n-restarts))
		     (error:bad-range-argument n 'RESTART))
		 n))))))))

(define (write-restarts restarts port write-index)
  (newline port)
  (do ((restarts restarts (cdr restarts))
       (index (length restarts) (- index 1)))
      ((null? restarts))
    (write-index index port)
    (write-string " " port)
    (write-restart-report (car restarts) port)
    (newline port)))

(define (filter-restarts restarts)
  (let loop ((restarts restarts))
    (if (null? restarts)
	'()
	(cons (car restarts)
	      (if (restart/get (car restarts) make-cmdl)
		  (list-transform-positive (cdr restarts)
		    (lambda (restart)
		      (restart/get restart make-cmdl)))
		  (loop (cdr restarts)))))))

(define-structure (repl-state
		   (conc-name repl-state/)
		   (constructor make-repl-state
				(prompt environment syntax-table condition)))
  prompt
  environment
  syntax-table
  (condition false read-only true)
  (reader-history (make-repl-history reader-history-size))
  (printer-history (make-repl-history printer-history-size)))

(define (repl? object)
  (and (cmdl? object)
       (repl-state? (cmdl/state object))))

(define-integrable (repl/prompt repl)
  (repl-state/prompt (cmdl/state repl)))

(define-integrable (set-repl/prompt! repl prompt)
  (set-repl-state/prompt! (cmdl/state repl) prompt))

(define-integrable (repl/environment repl)
  (repl-state/environment (cmdl/state repl)))

(define (set-repl/environment! repl environment)
  (set-repl-state/environment! (cmdl/state repl) environment)
  (repl/set-default-environment repl)
  (port/set-default-environment (cmdl/port repl) environment))

(define-integrable (repl/syntax-table repl)
  (repl-state/syntax-table (cmdl/state repl)))

(define (set-repl/syntax-table! repl syntax-table)
  (set-repl-state/syntax-table! (cmdl/state repl) syntax-table)
  (port/set-default-syntax-table (cmdl/port repl) syntax-table))

(define-integrable (repl/condition repl)
  (repl-state/condition (cmdl/state repl)))

(define-integrable (repl/reader-history repl)
  (repl-state/reader-history (cmdl/state repl)))

(define-integrable (set-repl/reader-history! repl reader-history)
  (set-repl-state/reader-history! (cmdl/state repl) reader-history))

(define-integrable (repl/printer-history repl)
  (repl-state/printer-history (cmdl/state repl)))

(define-integrable (set-repl/printer-history! repl printer-history)
  (set-repl-state/printer-history! (cmdl/state repl) printer-history))

(define (repl/parent repl)
  (skip-non-repls (cmdl/parent repl)))

(define (nearest-repl)
  (or (skip-non-repls (nearest-cmdl))
      (error "NEAREST-REPL: no REPLs")))

(define (skip-non-repls cmdl)
  (and cmdl
       (if (repl-state? (cmdl/state cmdl))
	   cmdl
	   (skip-non-repls (cmdl/parent cmdl)))))

(define (repl/base repl)
  (let ((parent (repl/parent repl)))
    (if parent
	(repl/base parent)
	repl)))

(define (nearest-repl/environment)
  (repl/environment (nearest-repl)))

(define (nearest-repl/syntax-table)
  (repl/syntax-table (nearest-repl)))

(define (nearest-repl/condition)
  (repl/condition (nearest-repl)))

;;;; History

(define reader-history-size 5)
(define printer-history-size 10)

(define-structure (repl-history (constructor %make-repl-history)
				(conc-name repl-history/))
  (size false read-only true)
  elements)

(define (make-repl-history size)
  (%make-repl-history size (make-circular-list size '())))

(define (repl-history/record! history object)
  (let ((elements (repl-history/elements history)))
    (if (not (null? elements))
	(begin
	  (set-car! elements object)
	  (set-repl-history/elements! history (cdr elements))))))

(define (repl-history/replace-current! history object)
  (let ((elements (repl-history/elements history)))
    (if (not (null? elements))
	(set-car! (list-tail elements (- (repl-history/size history) 1))
		  object))))

(define (repl-history/read history n)
  (if (not (and (exact-nonnegative-integer? n)
		(< n (repl-history/size history))))
      (error:wrong-type-argument n "history index" 'REPL-HISTORY/READ))
  (list-ref (repl-history/elements history)
	    (- (- (repl-history/size history) 1) n)))

;;; User Interface Stuff

(define (pe)
  (let ((environment (nearest-repl/environment)))
    (let ((package (environment->package environment)))
      (if package
	  (package/name package)
	  environment))))

(define (ge environment)
  (let ((environment (->environment environment)))
    (set-repl/environment! (nearest-repl) environment)
    environment))

(define (->environment object)
  (cond ((environment? object) object)
	((package? object) (package/environment object))
	((procedure? object) (procedure-environment object))
	((promise? object) (promise-environment object))
	(else
	 (let ((package
		(let ((package-name
		       (cond ((symbol? object) (list object))
			     ((list? object) object)
			     (else false))))
		  (and package-name
		       (name->package package-name)))))
	   (if (not package)
	       (error:wrong-type-argument object "environment" '->ENVIRONMENT))
	   (package/environment package)))))

(define (gst syntax-table)
  (guarantee-syntax-table syntax-table)
  (set-repl/syntax-table! (nearest-repl) syntax-table))

(define (re #!optional index)
  (let ((repl (nearest-repl)))
    (hook/repl-eval (let ((history (repl/reader-history repl)))
		      (let ((s-expression
			     (repl-history/read history
						(if (default-object? index)
						    1
						    index))))
			(repl-history/replace-current! history s-expression)
			s-expression))
		    (repl/environment repl)
		    (repl/syntax-table repl))))

(define (in #!optional index)
  (repl-history/read (repl/reader-history (nearest-repl))
		     (if (default-object? index) 1 index)))

(define (out #!optional index)
  (repl-history/read (repl/printer-history (nearest-repl))
		     (- (if (default-object? index) 1 index) 1)))

(define (read-eval-print environment message prompt)
  (repl/start (push-repl environment 'INHERIT false '() prompt) message))

(define (breakpoint #!optional message environment)
  (with-simple-restart 'CONTINUE "Continue from breakpoint."
    (lambda ()
      (read-eval-print (if (default-object? environment) 'INHERIT environment)
		       (if (default-object? message) "Break!" message)
		       "break>"))))

(define (bkpt datum . arguments)
  (apply breakpoint-procedure 'INHERIT datum arguments))

(define (breakpoint-procedure environment datum . arguments)
  ;; For upwards compatibility.
  (with-simple-restart 'CONTINUE "Return from BKPT."
    (lambda ()
      (read-eval-print environment
		       (cmdl-message/active
			(lambda (port)
			  (newline port)
			  (format-error-message datum arguments port)))
		       "break>"))))

(define (ve environment)
  (read-eval-print (->environment environment) false 'INHERIT))

(define (proceed #!optional value)
  (if (default-object? value)
      (continue)
      (use-value value))
  (let ((port (nearest-cmdl/port)))
    (fresh-line port)
    (write-string ";Unable to PROCEED" port)))