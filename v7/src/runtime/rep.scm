#| -*-Scheme-*-

$Id: rep.scm,v 14.58 2002/11/20 19:46:22 cph Exp $

Copyright (c) 1988-2001 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

;;;; Read-Eval-Print Loop
;;; package: (runtime rep)

(declare (usual-integrations))

(define repl:allow-restart-notifications? #t)
(define repl:write-result-hash-numbers? #t)

(define (initialize-package!)
  (set! *nearest-cmdl* #f)
  (set! hook/repl-eval default/repl-eval)
  (set! hook/repl-write default/repl-write)
  (set! hook/set-default-environment default/set-default-environment)
  (set! hook/error-decision #f)
  (initialize-breakpoint-condition!)
  unspecific)

(define (initial-top-level-repl)
  (call-with-current-continuation
   (lambda (continuation)
     (set! root-continuation continuation)
     (repl/start (make-repl #f
			    console-i/o-port
			    user-initial-environment
			    #f
			    `((SET-DEFAULT-DIRECTORY
			       ,top-level-repl/set-default-directory))
			    user-initial-prompt)
		 (cmdl-message/strings "Cold load finished")))))

(define root-continuation)

(define (top-level-repl/set-default-directory cmdl pathname)
  cmdl
  ((ucode-primitive set-working-directory-pathname! 1)
   (->namestring pathname)))

;;;; Command Loops

(define cmdl-rtd
  (make-record-type "cmdl"
		    '(LEVEL PARENT PORT DRIVER STATE OPERATIONS PROPERTIES)))

(define cmdl? (record-predicate cmdl-rtd))
(define cmdl/level (record-accessor cmdl-rtd 'LEVEL))
(define cmdl/parent (record-accessor cmdl-rtd 'PARENT))
(define cmdl/port (record-accessor cmdl-rtd 'PORT))
(define set-cmdl/port! (record-updater cmdl-rtd 'PORT))
(define cmdl/driver (record-accessor cmdl-rtd 'DRIVER))
(define cmdl/state (record-accessor cmdl-rtd 'STATE))
(define set-cmdl/state! (record-updater cmdl-rtd 'STATE))
(define cmdl/operations (record-accessor cmdl-rtd 'OPERATIONS))
(define cmdl/properties (record-accessor cmdl-rtd 'PROPERTIES))

(define make-cmdl
  (let ((constructor
	 (record-constructor
	  cmdl-rtd
	  '(LEVEL PARENT PORT DRIVER STATE OPERATIONS PROPERTIES))))
    (lambda (parent port driver state operations)
      (if (not (or (not parent) (cmdl? parent)))
	  (error:wrong-type-argument parent "cmdl" 'MAKE-CMDL))
      (if (not (or parent port))
	  (error:bad-range-argument port 'MAKE-CMDL))
      (constructor (if parent (+ (cmdl/level parent) 1) 1)
		   parent
		   (let ((port* (and parent (cmdl/child-port parent))))
		     (if port
			 (if (eq? port port*)
			     port
			     (make-transcriptable-port port))
			 port*))
		   driver
		   state
		   (parse-operations-list operations 'MAKE-CMDL)
		   (make-1d-table)))))

(define (cmdl/child-port cmdl)
  (or (let ((operation (cmdl/local-operation cmdl 'CHILD-PORT)))
	(and operation
	     (operation cmdl)))
      (cmdl/port cmdl)))

(define (push-cmdl driver state operations)
  (make-cmdl (nearest-cmdl) #f driver state operations))

(define (cmdl/base cmdl)
  (let ((parent (cmdl/parent cmdl)))
    (if parent
	(cmdl/base parent)
	cmdl)))

(define (cmdl/set-default-directory cmdl pathname)
  (let ((operation (cmdl/local-operation cmdl 'SET-DEFAULT-DIRECTORY)))
    (if operation
	(operation cmdl pathname)))
  (port/set-default-directory (cmdl/port cmdl) pathname))

(define (cmdl/start cmdl message)
  (let ((port (cmdl/port cmdl)))
    (let ((thunk
	   (lambda ()
	     (fluid-let ((*nearest-cmdl* cmdl)
			 (dynamic-handler-frames '())
			 (*bound-restarts*
			  (if (cmdl/parent cmdl) *bound-restarts* '()))
			 (standard-error-hook #f)
			 (standard-warning-hook #f)
			 (standard-breakpoint-hook #f)
			 (*working-directory-pathname*
			  *working-directory-pathname*)
			 (*default-pathname-defaults*
			  *default-pathname-defaults*)
			 (*current-input-port* #f)
			 (*current-output-port* #f)
			 (*notification-output-port* #f)
			 (*trace-output-port* #f)
			 (*interaction-i/o-port* #f))
	       (let loop ((message message))
		 (loop
		  (bind-abort-restart cmdl
		    (lambda ()
		      (deregister-all-events)
		      (with-interrupt-mask interrupt-mask/all
			(lambda (interrupt-mask)
			  interrupt-mask
			  (unblock-thread-events)
			  (ignore-errors
			   (lambda ()
			     ((->cmdl-message message) cmdl)))
			  (call-with-current-continuation
			   (lambda (continuation)
			     (with-create-thread-continuation continuation
			       (lambda ()
				 ((cmdl/driver cmdl) cmdl))))))))))))))
	  (mutex (port/thread-mutex port)))
      (let ((thread (current-thread))
	    (owner (thread-mutex-owner mutex)))
	(cond ((and owner (not (eq? thread owner)))
	       (signal-thread-event owner
		 (let ((signaller
			(or (cmdl/local-operation cmdl 'START-NON-OWNED)
			    (lambda (cmdl thread)
			      cmdl
			      (error "Non-owner thread can't start CMDL:"
				     thread)))))
		   (lambda ()
		     (unblock-thread-events)
		     (signaller cmdl thread))))
	       (stop-current-thread))
	      ((let ((parent (cmdl/parent cmdl)))
		 (and parent
		      (cmdl/local-operation parent 'START-CHILD)))
	       => (lambda (operation) (operation cmdl thunk)))
	      (else
	       (with-thread-mutex-locked mutex thunk)))))))

(define (bind-abort-restart cmdl thunk)
  (call-with-current-continuation
   (lambda (continuation)
     (with-restart 'ABORT
	 (string-append "Return to "
			(if (repl? cmdl)
			    "read-eval-print"
			    "command")
			" level "
			(number->string (cmdl/level cmdl))
			".")
	 (lambda (#!optional message)
	   (continuation
	    (cmdl-message/append
	     (cmdl-message/active
	      (lambda (port)
		;; Inform the port that the default directory has changed.
		(port/set-default-directory port
					    (working-directory-pathname))))
	     (if (default-object? message) "Abort!" message))))
	 values
       (lambda ()
	 (restart/put! (first-bound-restart) cmdl-abort-restart-tag cmdl)
	 (thunk))))))

(define (cmdl-abort-restart? restart)
  (restart/get restart cmdl-abort-restart-tag))

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
      (if (pair? bindings)
	  (loop (cdr bindings)
		(if (memq (caar bindings) names)
		    names
		    (cons (caar bindings) names)))
	  (let ((parent (cmdl/parent cmdl)))
	    (if parent
		(cmdl-loop parent names)
		names))))))

;;;; Messages

(define (->cmdl-message object)
  (cond ((not object) (cmdl-message/null))
	((string? object) (cmdl-message/strings object))
	(else object)))

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
  (do ((messages messages (cdr messages)))
      ((not (pair? messages)))
    (set-car! messages (->cmdl-message (car messages))))
  (let ((messages (delq! %cmdl-message/null messages)))
    (if (pair? messages)
	(if (pair? (cdr messages))
	    (lambda (cmdl)
	      (for-each (lambda (message) (message cmdl)) messages))
	    (car messages))
	(cmdl-message/null))))

(define-integrable (cmdl-message/null)
  %cmdl-message/null)

(define (%cmdl-message/null cmdl)
  cmdl
  #f)

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
		    (cond ((pair? next) (car next))
			  ((pair? restarts) (car restarts))
			  (else (error:no-such-restart 'ABORT)))))
		(if (default-object? message) "Up!" message)))

(define (abort->top-level #!optional message)
  (invoke-abort (let loop ((restarts (find-restarts 'ABORT (bound-restarts))))
		  (let ((next (find-restarts 'ABORT (cdr restarts))))
		    (cond ((pair? next) (loop next))
			  ((pair? restarts) (car restarts))
			  (else (error:no-such-restart 'ABORT)))))
		(if (default-object? message) "Quit!" message)))

(define (find-restarts name restarts)
  (let loop ((restarts restarts))
    (if (or (not (pair? restarts))
	    (eq? name (restart/name (car restarts))))
	restarts
	(loop (cdr restarts)))))

(define (invoke-abort restart message)
  (let ((effector (restart/effector restart)))
    (if (cmdl-abort-restart? restart)
	(effector message)
	(effector))))

(define cmdl-abort-restart-tag
  (list 'CMDL-ABORT-RESTART-TAG))

;;;; REP Loops

(define (make-repl parent port environment
		   #!optional condition operations prompt)
  (make-cmdl parent
	     port
	     repl-driver
	     (let ((inherit
		    (let ((repl (and parent (skip-non-repls parent))))
		      (lambda (argument default name check-arg)
			(if (eq? 'INHERIT argument)
			    (begin
			      (if (not repl)
				  (error "Can't inherit -- no REPL ancestor:"
					 name))
			      (default repl))
			    (check-arg argument 'MAKE-REPL))))))
	       (make-repl-state
		(inherit (if (default-object? prompt) 'INHERIT prompt)
			 repl/prompt
			 'PROMPT
			 (lambda (object procedure)
			   (if (not (string? object))
			       (error:wrong-type-argument object
							  "string"
							  procedure))
			   object))
		(inherit environment
			 repl/environment
			 'ENVIRONMENT
			 ->environment)
		(if (default-object? condition) #f condition)))
	     (append (if (default-object? operations) '() operations)
		     default-repl-operations)))

(define default-repl-operations
  `((START-CHILD ,(lambda (cmdl thunk) cmdl (with-history-disabled thunk)))
    (START-NON-OWNED
     ,(lambda (repl thread)
	(let ((condition (repl/condition repl)))
	  (if condition
	      (error:derived-thread thread condition)
	      (error "Non-owner thread can't start REPL:" thread)))))))

(define (push-repl environment
		   #!optional condition operations prompt)
  (let ((parent (nearest-cmdl)))
    (make-repl parent
	       #f
	       environment
	       (if (default-object? condition) #f condition)
	       (if (default-object? operations) '() operations)
	       (if (default-object? prompt) 'INHERIT prompt))))

(define (repl-driver repl)
  (let ((condition (repl/condition repl)))
    (if (and condition (condition/error? condition))
	(cond ((cmdl/operation repl 'ERROR-DECISION)
	       => (lambda (operation)
		    (operation repl condition)))
	      (hook/error-decision
	       (hook/error-decision repl condition)))))
  (let ((reader-history (repl/reader-history repl))
	(printer-history (repl/printer-history repl)))
    (port/set-default-environment (cmdl/port repl) (repl/environment repl))
    (do () (#f)
      (let ((s-expression
	     (prompt-for-command-expression (cons 'STANDARD (repl/prompt repl))
					    (cmdl/port repl))))
	(repl-history/record! reader-history s-expression)
	(let ((value
	       (hook/repl-eval repl s-expression (repl/environment repl))))
	  (repl-history/record! printer-history value)
	  (hook/repl-write repl s-expression value))))))

(define hook/repl-eval)
(define (default/repl-eval repl s-expression environment)
  (let ((scode (syntax s-expression environment)))
    (with-repl-eval-boundary repl
      (lambda ()
	(extended-scode-eval scode environment)))))

(define (repl-scode-eval repl scode environment)
  (with-repl-eval-boundary repl
    (lambda ()
      (extended-scode-eval scode environment))))

(define (with-repl-eval-boundary repl thunk)
  ((ucode-primitive with-stack-marker 3)
   (lambda () (with-new-history thunk))
   with-repl-eval-boundary
   repl))

(define hook/repl-write)
(define (default/repl-write repl s-expression object)
  (port/write-result (cmdl/port repl)
		     s-expression
		     object
		     (and repl:write-result-hash-numbers?
			  (object-pointer? object)
			  (not (interned-symbol? object))
			  (not (number? object))
			  (object-hash object))))

(define (repl/start repl #!optional message)
  (cmdl/start repl
	      (make-repl-message repl
				 (if (default-object? message)
				     #f
				     message))))

(define (make-repl-message repl message)
  (let ((condition (repl/condition repl)))
    (cmdl-message/append
     (or message
	 (and condition
	      (cmdl-message/strings
	       (fluid-let ((*unparser-list-depth-limit* 25)
			   (*unparser-list-breadth-limit* 100)
			   (*unparser-string-length-limit* 500))
		 (condition/report-string condition)))))
     (and condition
	  repl:allow-restart-notifications?
	  (condition-restarts-message condition))
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
	      (write-string
	       ";Warning! this environment is a compiled-code environment:
; Assignments to most compiled-code bindings are prohibited,
; as are certain other environment operations."
	       port)))
	(let ((package (environment->package environment)))
	  (if package
	      (begin
		(fresh-line port)
		(write-string ";Package: " port)
		(write (package/name package) port))))))))

(define (restart #!optional n)
  (let ((condition (nearest-repl/condition)))
    (let ((restarts
	   (filter-restarts
	    (if condition
		(condition/restarts condition)
		(bound-restarts)))))
      (let ((n-restarts (length restarts)))
	(if (zero? n-restarts)
	    (error "Can't RESTART: no options available."))
	(invoke-restart-interactively
	 (list-ref
	  restarts
	  (- n-restarts
	     (if (default-object? n)
		 (let ((port (interaction-i/o-port)))
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
		   n))))
	 condition)))))

(define (write-restarts restarts port write-index)
  (newline port)
  (do ((restarts restarts (cdr restarts))
       (index (length restarts) (- index 1)))
      ((not (pair? restarts)))
    (write-index index port)
    (write-string " " port)
    (write-restart-report (car restarts) port)
    (newline port)))

(define (filter-restarts restarts)
  (let loop ((restarts restarts))
    (if (pair? restarts)
	(let ((rest
	       (if (cmdl-abort-restart? (car restarts))
		   (list-transform-positive (cdr restarts) cmdl-abort-restart?)
		   (loop (cdr restarts)))))
	  (if (restart/interactor (car restarts))
	      (cons (car restarts) rest)
	      rest))
	'())))

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

(define-structure (repl-state
		   (conc-name repl-state/)
		   (constructor make-repl-state
				(prompt environment condition)))
  prompt
  environment
  (condition #f read-only #t)
  (reader-history (make-repl-history repl-reader-history-size))
  (printer-history (make-repl-history repl-printer-history-size)))

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

(define (nearest-repl/condition)
  (repl/condition (nearest-repl)))

;;;; History

(define repl-reader-history-size 5)
(define repl-printer-history-size 10)

(define-structure (repl-history (constructor %make-repl-history)
				(conc-name repl-history/))
  (size #f read-only #t)
  elements)

(define (make-repl-history size)
  (%make-repl-history size (make-circular-list size '())))

(define (repl-history/record! history object)
  (let ((elements (repl-history/elements history)))
    (if (pair? elements)
	(begin
	  (set-car! elements object)
	  (set-repl-history/elements! history (cdr elements))))))

(define (repl-history/replace-current! history object)
  (let ((elements (repl-history/elements history)))
    (if (pair? elements)
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
  (let ((environment (->environment environment 'GE)))
    (set-repl/environment! (nearest-repl) environment)
    environment))

(define (->environment object #!optional procedure)
  (let ((procedure
	 (if (or (default-object? procedure) (not procedure))
	     '->ENVIRONMENT
	     procedure)))
    (cond ((environment? object) object)
	  ((package? object) (package/environment object))
	  ((procedure? object) (procedure-environment object))
	  ((promise? object) (promise-environment object))
	  (else
	   (let ((package
		  (let ((package-name
			 (cond ((symbol? object) (list object))
			       ((list? object) object)
			       (else #f))))
		    (and package-name
			 (name->package package-name)))))
	     (if (not package)
		 (error:wrong-type-argument object "environment" procedure))
	     (package/environment package))))))

(define (re #!optional index)
  (let ((repl (nearest-repl)))
    (hook/repl-eval repl
		    (let ((history (repl/reader-history repl)))
		      (let ((s-expression
			     (repl-history/read history
						(if (default-object? index)
						    1
						    index))))
			(repl-history/replace-current! history s-expression)
			s-expression))
		    (repl/environment repl))))

(define (in #!optional index)
  (repl-history/read (repl/reader-history (nearest-repl))
		     (if (default-object? index) 1 index)))

(define (out #!optional index)
  (repl-history/read (repl/printer-history (nearest-repl))
		     (- (if (default-object? index) 1 index) 1)))

(define (read-eval-print environment message prompt)
  (repl/start (push-repl environment #f '() prompt) message))

(define (ve environment)
  (read-eval-print (->environment environment 'VE) #f 'INHERIT))

(define (proceed #!optional value)
  (if (default-object? value)
      (continue)
      (use-value value))
  (let ((port (notification-output-port)))
    (fresh-line port)
    (write-string ";Unable to PROCEED" port)))

;;;; Breakpoints

(define (bkpt datum . arguments)
  (apply breakpoint-procedure 'CONTINUATION-ENVIRONMENT datum arguments))

(define (breakpoint-procedure environment datum . arguments)
  (signal-breakpoint #f
		     environment
		     (cmdl-message/active
		      (lambda (port)
			(fresh-line port)
			(format-error-message datum arguments port)))
		     "bkpt>"))

(define (breakpoint #!optional message environment continuation)
  (signal-breakpoint (if (default-object? continuation)
			 #f
			 continuation)
		     (if (default-object? environment)
			 (nearest-repl/environment)
			 environment)
		     (if (default-object? message)
			 "Break!"
			 message)
		     "break>"))

(define (signal-breakpoint continuation environment message prompt)
  (call-with-current-continuation
   (lambda (restart-continuation)
     (let ((continuation (or continuation restart-continuation)))
       (with-restart 'CONTINUE
	   (if (string=? "bkpt>" prompt)
	       "Return from BKPT."
	       "Continue from breakpoint.")
	   (lambda () (restart-continuation unspecific))
	   values
	 (lambda ()
	   (call-with-values
	       (lambda ()
		 (get-breakpoint-environment continuation environment message))
	     (lambda (environment message)
	       (%signal-breakpoint continuation
				   environment
				   message
				   prompt)))))))))

(define (get-breakpoint-environment continuation environment message)
  (let ((environment
	 (if (eq? 'CONTINUATION-ENVIRONMENT environment)
	     (continuation/first-subproblem-environment continuation)
	     environment)))
    (if (eq? 'NO-ENVIRONMENT environment)
	(values (nearest-repl/environment)
		(cmdl-message/append
		 message
		 (cmdl-message/strings
		  "Breakpoint environment unavailable;"
		  "using REPL environment instead.")))
	(values environment message))))

(define (continuation/first-subproblem-environment continuation)
  (let ((frame (continuation/first-subproblem continuation)))
    (if frame
	(call-with-values (lambda () (stack-frame/debugging-info frame))
	  (lambda (expression environment subexpression)
	    expression subexpression
	    (if (debugging-info/undefined-environment? environment)
		'NO-ENVIRONMENT
		environment)))
	'NO-ENVIRONMENT)))

(define condition-type:breakpoint)
(define condition/breakpoint?)
(define breakpoint/environment)
(define breakpoint/message)
(define breakpoint/prompt)
(define %signal-breakpoint)

(define (initialize-breakpoint-condition!)
  (set! condition-type:breakpoint
	(make-condition-type 'BREAKPOINT #f '(ENVIRONMENT MESSAGE PROMPT)
	  (lambda (condition port)
	    condition
	    (write-string "Breakpoint." port))))
  (set! condition/breakpoint?
	(condition-predicate condition-type:breakpoint))
  (set! breakpoint/environment
	(condition-accessor condition-type:breakpoint 'ENVIRONMENT))
  (set! breakpoint/message
	(condition-accessor condition-type:breakpoint 'MESSAGE))
  (set! breakpoint/prompt
	(condition-accessor condition-type:breakpoint 'PROMPT))
  (set! %signal-breakpoint
	(let ((make-condition
	       (condition-constructor condition-type:breakpoint
				      '(ENVIRONMENT MESSAGE PROMPT))))
	  (lambda (continuation environment message prompt)
	    (let ((condition
		   (make-condition continuation
				   'BOUND-RESTARTS
				   environment
				   message
				   prompt)))
	      (signal-condition condition)
	      (standard-breakpoint-handler condition)))))
  unspecific)

(define (standard-breakpoint-handler condition)
  (let ((hook standard-breakpoint-hook))
    (if hook
	(fluid-let ((standard-breakpoint-hook #f))
	  (hook condition))))
  (repl/start (push-repl (breakpoint/environment condition)
			 condition
			 '()
			 (breakpoint/prompt condition))
	      (breakpoint/message condition)))

(define standard-breakpoint-hook #f)