#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/rep.scm,v 14.19 1991/03/14 04:27:13 cph Exp $

Copyright (c) 1988-91 Massachusetts Institute of Technology

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

(define (initialize-package!)
  (set! *nearest-cmdl* false)
  (set! with-cmdl/input-port
	(object-component-binder cmdl/input-port set-cmdl/input-port!))
  (set! with-cmdl/output-port
	(object-component-binder cmdl/output-port set-cmdl/output-port!))
  (set! hook/cmdl-prompt default/cmdl-prompt)
  (set! hook/cmdl-message default/cmdl-message)
  (set! hook/error-decision false)
  (set! hook/repl-environment default/repl-environment)
  (set! hook/repl-read default/repl-read)
  (set! hook/repl-write default/repl-write)
  (set! hook/repl-eval default/repl-eval)
  (set! hook/read-command-char default/read-command-char)
  (set! hook/prompt-for-confirmation default/prompt-for-confirmation)
  (set! hook/prompt-for-expression default/prompt-for-expression)
  unspecific)

(define (initial-top-level-repl)
  (make-cmdl false
	     console-input-port
	     console-output-port
	     repl-driver
	     (make-repl-state user-initial-prompt
			      user-initial-environment
			      user-initial-syntax-table
			      false)
	     (cmdl-message/standard "Cold load finished")
	     make-cmdl))

;;;; Command Loops

(define-structure (cmdl (conc-name cmdl/) (constructor %make-cmdl))
  (parent false read-only true)
  (level false read-only true)
  (driver false read-only true)
  (spawn-child false read-only true)
  input-port
  output-port
  state)

(define (make-cmdl parent input-port output-port driver state message
		   spawn-child)
  (if (not (or (false? parent) (cmdl? parent)))
      (error:wrong-type-argument parent "cmdl or #f" 'MAKE-CMDL))
  (let ((level (if parent (+ (cmdl/level parent) 1) 1)))
    (let ((cmdl
	   (%make-cmdl parent level driver spawn-child input-port output-port
		       state)))
      (let loop ((message message))
	(loop
	 (call-with-current-continuation
	  (lambda (continuation)
	    (bind-restart 'ABORT
		(string-append "Return to "
			       (if (repl? cmdl) "read-eval-print" "command")
			       " level "
			       (number->string level)
			       ".")
		(lambda (#!optional message)
		  (continuation
		   (if (default-object? message)
		       (cmdl-message/standard "Abort!")
		       message)))
	      (lambda (restart)
		(restart/put! restart make-cmdl cmdl)
		(fluid-let ((*nearest-cmdl* cmdl))
		  (with-interrupt-mask interrupt-mask/all
		    (lambda (interrupt-mask)
		      interrupt-mask
		      (message cmdl)
		      ((cmdl/driver cmdl) cmdl)))))))))))))

(define *nearest-cmdl*)

(define (nearest-cmdl)
  (if (not *nearest-cmdl*) (error "NEAREST-CMDL: no cmdl"))
  *nearest-cmdl*)

(define (nearest-cmdl/input-port)
  (cmdl/input-port (nearest-cmdl)))

(define (nearest-cmdl/output-port)
  (cmdl/output-port (nearest-cmdl)))

(define (push-cmdl driver state message spawn-child)
  (let ((parent (nearest-cmdl)))
    ((cmdl/spawn-child parent) parent
			       (cmdl/input-port parent)
			       (cmdl/output-port parent)
			       driver
			       state
			       message
			       spawn-child)))

(define (cmdl/base cmdl)
  (let ((parent (cmdl/parent cmdl)))
    (if parent
	(cmdl/base parent)
	cmdl)))

(define with-cmdl/input-port)
(define with-cmdl/output-port)

;;;; Messages

(define hook/cmdl-prompt)
(define (default/cmdl-prompt cmdl prompt)
  (with-output-port-cooked cmdl
    (lambda (output-port)
      (write-string
       (string-append "\n\n"
		      (number->string (cmdl/level cmdl))
		      " "
		      prompt
		      " ")
       output-port))))

(define ((cmdl-message/standard string) cmdl)
  (hook/cmdl-message cmdl string))

(define hook/cmdl-message)
(define (default/cmdl-message cmdl string)
  (with-output-port-cooked cmdl
    (lambda (output-port)
      (write-string (string-append "\n" string) output-port))))

(define ((cmdl-message/strings . strings) cmdl)
  (with-output-port-cooked cmdl
    (lambda (output-port)
      (for-each (lambda (string)
		  (write-string (string-append "\n" string) output-port))
		strings))))

(define ((cmdl-message/active actor) cmdl)
  (with-output-port-cooked cmdl
    (lambda (output-port)
      (with-output-to-port output-port
	(lambda ()
	  (actor cmdl))))))

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

(define (cmdl-interrupt/abort-nearest)
  (abort->nearest "Abort!"))

(define (cmdl-interrupt/abort-previous)
  (abort->previous "Up!"))

(define (cmdl-interrupt/abort-top-level)
  (abort->top-level "Quit!"))

(define (abort->nearest message)
  (invoke-abort (let ((restart (find-restart 'ABORT)))
		  (if (not restart)
		      (error:no-such-restart 'ABORT))
		  restart)
		message))

(define (abort->previous message)
  (invoke-abort (let ((restarts (find-restarts 'ABORT (bound-restarts))))
		  (let ((next (find-restarts 'ABORT (cdr restarts))))
		    (cond ((not (null? next)) (car next))
			  ((not (null? restarts)) (car restarts))
			  (else (error:no-such-restart 'ABORT)))))
		message))

(define (abort->top-level message)
  (invoke-abort (let loop ((restarts (find-restarts 'ABORT (bound-restarts))))
		  (let ((next (find-restarts 'ABORT (cdr restarts))))
		    (cond ((not (null? next)) (loop next))
			  ((not (null? restarts)) (car restarts))
			  (else (error:no-such-restart 'ABORT)))))
		message))

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
	 (if (string? message) (cmdl-message/standard message) message))
	(effector))))

(define (cmdl-interrupt/breakpoint)
  (with-simple-restart 'CONTINUE "Continue from ^B interrupt."
    (lambda ()
      (push-repl "^B interrupt" false "^B>"))))

;;;; REP Loops

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

(define (push-repl message condition
		   #!optional prompt environment syntax-table)
  (let ((environment (if (default-object? environment) 'INHERIT environment)))
    (push-cmdl repl-driver
	       (let ((repl (nearest-repl)))
		 (make-repl-state (if (or (default-object? prompt)
					  (eq? 'INHERIT prompt))
				      (repl/prompt repl)
				      prompt)
				  (if (eq? 'INHERIT environment)
				      (repl/environment repl)
				      environment)
				  (if (or (default-object? syntax-table)
					  (eq? 'INHERIT syntax-table))
				      (repl/syntax-table repl)
				      syntax-table)
				  condition))
	       (cmdl-message/append
		(cond ((not message)
		       (if condition
			   (cmdl-message/strings
			    (with-string-output-port
			      (lambda (port)
				(write-string ";" port)
				(write-condition-report condition
							port))))
			   (cmdl-message/null)))
		      ((string? message)
		       (cmdl-message/standard message))
		      (else
		       message))
		(if condition
		    (cmdl-message/append
		     (if hook/error-decision
			 (cmdl-message/active
			  (lambda (cmdl)
			    cmdl
			    (hook/error-decision)))
			 (cmdl-message/null))
		     (condition-restarts-message condition))
		    (cmdl-message/null))
		(if (eq? 'INHERIT environment)
		    (cmdl-message/null)
		    (cmdl-message/active
		     (lambda (cmdl)
		       cmdl
		       (repl-environment (nearest-repl) environment)))))
	       (lambda args
		 (with-history-disabled
		  (lambda ()
		    (apply make-cmdl args)))))))

(define hook/error-decision)

(define (repl-driver repl)
  (fluid-let ((standard-error-hook false)
	      (standard-warning-hook false))
    (hook/cmdl-prompt repl (repl/prompt repl))
    (let ((s-expression (hook/repl-read repl)))
      (cmdl-message/value
       (hook/repl-eval repl
		       s-expression
		       (repl/environment repl)
		       (repl/syntax-table repl))))))

(define (condition-restarts-message condition)
  (cmdl-message/active
   (lambda (cmdl)
     (let ((port (cmdl/output-port cmdl)))
       (write-string "
;To continue, call RESTART with an option number:" port)
       (write-restarts (filter-restarts (condition/restarts condition)) port
	 (lambda (index port)
	   (write-string " (RESTART " port)
	   (write index port)
	   (write-string ") =>" port)))))))

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
	       (let ((port (nearest-cmdl/output-port)))
		 (newline port)
		 (write-string ";Choose an option by number:" port)
		 (write-restarts restarts port
		   (lambda (index port)
		     (write-string (string-pad-left (number->string index) 3)
				   port)
		     (write-string ":" port)))
		 (let loop ()
		   (let ((n
			  (prompt-for-evaluated-expression "Option number")))
		     (if (and (exact-integer? n) (<= 1 n n-restarts))
			 n
			 (begin
			   (beep port)
			   (newline port)
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
    (write-string ";" port)
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
  (repl-environment repl environment))

(define-integrable (repl/syntax-table repl)
  (repl-state/syntax-table (cmdl/state repl)))

(define-integrable (set-repl/syntax-table! repl syntax-table)
  (set-repl-state/syntax-table! (cmdl/state repl) syntax-table))

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

;;;; Hooks

(define hook/repl-environment)
(define hook/repl-read)
(define hook/repl-eval)
(define hook/repl-write)

(define (repl-environment repl environment)
  (with-output-port-cooked repl
    (lambda (output-port)
      output-port
      (hook/repl-environment repl environment))))

(define (default/repl-environment repl environment)
  (let ((port (cmdl/output-port repl)))
    (if (not (interpreter-environment? environment))
	(begin
	  (write-string "
;Warning! this environment is a compiled-code environment:
; Assignments to most compiled-code bindings are prohibited,
; as are certain other environment operations.")))
    (let ((package (environment->package environment)))
      (if package
	  (begin
	    (write-string "\n;Package: " port)
	    (write (package/name package) port))))))

(define (default/repl-read repl)
  (let ((s-expression (read-internal (cmdl/input-port repl))))
    (repl-history/record! (repl/reader-history repl) s-expression)
    s-expression))

(define (default/repl-eval repl s-expression environment syntax-table)
  repl					;ignore
  (let ((scode (syntax s-expression syntax-table)))
    (with-new-history (lambda () (extended-scode-eval scode environment)))))

(define ((cmdl-message/value value) repl)
  (hook/repl-write repl value))

(define (default/repl-write repl object)
  (repl-history/record! (repl/printer-history repl) object)
  (with-output-port-cooked repl
    (lambda (output-port)
      (if (undefined-value? object)
	  (write-string "\n;No value" output-port)
	  (begin
	    (write-string "\n;Value" output-port)
	    (if (repl-write/show-hash? object)
		(begin
		  (write-string " " output-port)
		  (write (object-hash object) output-port)))
	    (write-string ": " output-port)
	    (write object output-port))))))

(define (repl-write/show-hash? object)
  (and (object-pointer? object)
       (not (interned-symbol? object))
       (not (number? object))))

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
  (set-repl-state/syntax-table! (cmdl/state (nearest-repl)) syntax-table)
  unspecific)

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
		    (repl/environment repl)
		    (repl/syntax-table repl))))

(define (in #!optional index)
  (repl-history/read (repl/reader-history (nearest-repl))
		     (if (default-object? index) 1 index)))

(define (out #!optional index)
  (repl-history/read (repl/printer-history (nearest-repl))
		     (- (if (default-object? index) 1 index) 1)))

(define (read-eval-print environment message prompt)
  (push-repl message false prompt environment))

(define (breakpoint message environment)
  (with-simple-restart 'CONTINUE "Continue from breakpoint."
    (lambda ()
      (read-eval-print environment message "Breakpoint->"))))

(define (bkpt datum . arguments)
  (apply breakpoint-procedure 'INHERIT datum arguments))

(define (breakpoint-procedure environment datum . arguments)
  ;; For upwards compatibility.
  (with-simple-restart 'CONTINUE "Return from BKPT."
    (lambda ()
      (read-eval-print environment
		       (cmdl-message/active
			(lambda (cmdl)
			  (let ((port (cmdl/output-port cmdl)))
			    (newline port)
			    (format-error-message datum arguments port))))
		       "Bkpt->"))))

(define (ve environment)
  (read-eval-print (->environment environment) false 'INHERIT))

(define (proceed #!optional value)
  (if (default-object? value)
      (continue)
      (use-value value))
  (write-string "\n;Unable to PROCEED" (nearest-cmdl/output-port)))

;;;; Prompting

(define (prompt-for-command-char prompt #!optional cmdl)
  (let ((cmdl (if (default-object? cmdl) (nearest-cmdl) cmdl)))
    (hook/cmdl-prompt cmdl prompt)
    (hook/read-command-char cmdl prompt)))

(define (prompt-for-confirmation prompt #!optional cmdl)
  (hook/prompt-for-confirmation (if (default-object? cmdl) (nearest-cmdl) cmdl)
				prompt))

(define (prompt-for-expression prompt #!optional cmdl)
  (hook/prompt-for-expression (if (default-object? cmdl) (nearest-cmdl) cmdl)
			      prompt))

(define (prompt-for-evaluated-expression prompt #!optional
					 environment syntax-table)
  (let ((repl (nearest-repl)))
    (hook/repl-eval repl
		    (prompt-for-expression prompt)
		    (if (default-object? environment)
			(repl/environment repl)
			environment)
		    (if (default-object? syntax-table)
			(repl/syntax-table repl)
			syntax-table))))

(define hook/read-command-char)
(define hook/prompt-for-confirmation)
(define hook/prompt-for-expression)

(define (default/read-command-char cmdl prompt)
  ;; Prompt argument is random.  Emacs interface needs it right now.
  prompt
  (read-char-internal (cmdl/input-port cmdl)))

(define (default/prompt-for-confirmation cmdl prompt)
  (let ((input-port (cmdl/input-port cmdl))
	(prompt (string-append "\n" prompt " (y or n)? ")))
    (with-output-port-cooked cmdl
      (lambda (output-port)
	(let loop ()
	  (write-string prompt output-port)
	  (let ((char (read-char-internal input-port)))
	    (cond ((or (char-ci=? #\Y char)
		       (char-ci=? #\Space char))
		   (write-string "Yes" output-port)
		   true)
		  ((or (char-ci=? #\N char)
		       (char-ci=? #\Rubout char))
		   (write-string "No" output-port)
		   false)
		  (else
		   (write char output-port)
		   (beep output-port)
		   (loop)))))))))

(define (default/prompt-for-expression cmdl prompt)
  (with-output-port-cooked cmdl
    (lambda (output-port)
      (write-string (string-append "\n" prompt ": ") output-port)))
  (read-internal (cmdl/input-port cmdl)))

(define (with-output-port-cooked cmdl user)
  (let ((output-port (cmdl/output-port cmdl)))
    (terminal-bind terminal-cooked-output (output-port/channel output-port)
      (lambda ()
	(user output-port)))))

(define (read-internal input-port)
  (terminal-bind terminal-cooked-input (input-port/channel input-port)
    (lambda ()
      (read input-port))))

(define (read-char-internal input-port)
  (terminal-bind terminal-raw-input (input-port/channel input-port)
    (lambda ()
      (let loop ()
	(let ((char (read-char input-port)))
	  (if (char=? char char:newline)
	      (loop)
	      char))))))

(define (terminal-bind operation terminal thunk)
  (if (and terminal
	   (channel-type=terminal? terminal))
      (let ((outside-state)
	    (inside-state false))
	(dynamic-wind
	 (lambda ()
	   (set! outside-state (terminal-get-state terminal))
	   (if inside-state
	       (begin
		 (terminal-set-state terminal inside-state)
		 (set! inside-state)
		 unspecific)
	       (operation terminal)))
	 thunk
	 (lambda ()
	   (set! inside-state (terminal-get-state terminal))
	   (terminal-set-state terminal outside-state)
	   (set! outside-state)
	   unspecific)))
      (thunk)))