#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/rep.scm,v 14.10 1989/08/03 23:03:04 cph Exp $

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
  (set! cmdl-interrupt/breakpoint default/breakpoint)
  (set! cmdl-interrupt/abort-top-level default/abort-top-level)
  (set! cmdl-interrupt/abort-previous default/abort-previous)
  (set! cmdl-interrupt/abort-nearest default/abort-nearest)
  (set! hook/repl-environment default/repl-environment)
  (set! hook/repl-read default/repl-read)
  (set! hook/repl-write default/repl-write)
  (set! hook/repl-eval default/repl-eval)
  (set! hook/read-command-char default/read-command-char)
  (set! hook/prompt-for-confirmation default/prompt-for-confirmation)
  (set! hook/prompt-for-expression default/prompt-for-expression))

(define (initial-top-level-repl)
  (fluid-let ((user-repl-environment user-initial-environment)
	      (user-repl-syntax-table user-initial-syntax-table))
    (let loop ((message "Cold load finished"))
      (with-standard-proceed-point
       (lambda ()
	 (make-repl false
		    user-repl-environment
		    user-repl-syntax-table
		    user-initial-prompt
		    console-input-port
		    console-output-port
		    (cmdl-message/standard message))))
      (loop "Reset!"))))

;;;; Command Loops

(define-structure (cmdl (conc-name cmdl/) (constructor %make-cmdl))
  (parent false read-only true)
  (level false read-only true)
  (driver false read-only true)
  (proceed-continuation false read-only true)
  continuation
  input-port
  output-port
  state)

(define (make-cmdl parent input-port output-port driver state message)
  (if (and parent (not (cmdl? parent)))
      (error "MAKE-CMDL: illegal parent" parent))
  (let ((cmdl
	 (%make-cmdl parent
		     (let loop ((parent parent))
		       (if parent
			   (1+ (loop (cmdl/parent parent)))
			   1))
		     driver
		     (current-proceed-continuation)
		     false
		     input-port
		     output-port
		     state)))
    (let loop ((message message))
      (loop
       (fluid-let
	   ((*nearest-cmdl* cmdl)
	    (cmdl-interrupt/abort-nearest default/abort-nearest)
	    (cmdl-interrupt/abort-previous default/abort-previous)
	    (cmdl-interrupt/abort-top-level default/abort-top-level)
	    (cmdl-interrupt/breakpoint default/breakpoint))
	 (with-interrupt-mask interrupt-mask/all
	   (lambda (interrupt-mask)
	     interrupt-mask
	     (call-with-current-continuation
	      (lambda (continuation)
		(set-cmdl/continuation! cmdl continuation)
		(message cmdl)
		(driver cmdl))))))))))

(define *nearest-cmdl*)

(define (nearest-cmdl)
  (if (not *nearest-cmdl*) (error "NEAREST-CMDL: no cmdl"))
  *nearest-cmdl*)

(define (push-cmdl driver state message)
  (let ((cmdl (nearest-cmdl)))
    (make-cmdl cmdl
	       (cmdl/input-port cmdl)
	       (cmdl/output-port cmdl)
	       driver
	       state
	       message)))

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
  (write-string
   (string-append "\n\n" (number->string (cmdl/level cmdl)) " " prompt " ")
   (cmdl/output-port cmdl)))

(define ((cmdl-message/standard string) cmdl)
  (hook/cmdl-message cmdl string))

(define hook/cmdl-message)

(define (default/cmdl-message cmdl string)
  (write-string (string-append "\n" string) (cmdl/output-port cmdl)))

(define ((cmdl-message/strings . strings) cmdl)
  (let ((port (cmdl/output-port cmdl)))
    (for-each (lambda (string)
		(write-string (string-append "\n" string) port))
	      strings)))

(define ((cmdl-message/null) cmdl)
  cmdl
  false)

(define ((cmdl-message/active thunk) cmdl)
  (with-output-to-port (cmdl/output-port cmdl)
    thunk))

(define ((cmdl-message/append . messages) cmdl)
  (for-each (lambda (message) (message cmdl)) messages))

;;;; Interrupts

(define cmdl-interrupt/abort-nearest)
(define cmdl-interrupt/abort-previous)
(define cmdl-interrupt/abort-top-level)
(define cmdl-interrupt/breakpoint)

(define (default/abort-nearest)
  (abort-to-nearest-driver "Abort!"))

(define (abort-to-nearest-driver message)
  (abort->nearest (cmdl-message/standard message)))

(define (abort->nearest message)
  ((cmdl/continuation (nearest-cmdl)) message))

(define (default/abort-previous)
  (abort-to-previous-driver "Up!"))

(define (abort-to-previous-driver message)
  (abort->previous (cmdl-message/standard message)))

(define (abort->previous message)
  ((cmdl/continuation 
    (let ((cmdl (nearest-cmdl)))
      (or (cmdl/parent cmdl)
	  cmdl)))
   message))

(define (default/abort-top-level)
  (abort-to-top-level-driver "Quit!"))

(define (abort-to-top-level-driver message)
  (abort->top-level (cmdl-message/standard message)))

(define (abort->top-level message)
  ((let ((cmdl (cmdl/base (nearest-cmdl))))
     (if cmdl-interrupt/abort-top-level/reset?
	 (cmdl/proceed-continuation cmdl)
	 (cmdl/continuation cmdl)))
   message))

;; User option variable
(define cmdl-interrupt/abort-top-level/reset? false)

(define (default/breakpoint)
  (with-standard-proceed-point
   (lambda ()
     (breakpoint (cmdl-message/standard "^B interrupt")
		 (nearest-repl/environment)))))

;;;; Proceed

(define (with-proceed-point value-filter thunk)
  (call-with-current-continuation
   (lambda (continuation)
     (fluid-let ((proceed-continuation continuation)
		 (proceed-value-filter value-filter))
       (thunk)))))

(define (current-proceed-continuation)
  proceed-continuation)

(define (proceed . arguments)
  (proceed-value-filter proceed-continuation arguments))

(define proceed-continuation false)
(define proceed-value-filter)

(define (with-standard-proceed-point thunk)
  (with-proceed-point standard-value-filter thunk))

(define (standard-value-filter continuation arguments)
  (continuation
   (if (null? arguments)
       unspecific
       (car arguments))))

;;;; REP Loops

(define-structure (repl-state (conc-name repl-state/))
  prompt
  environment
  syntax-table
  reader-history
  printer-history)

(define (make-repl parent environment syntax-table prompt input-port
		   output-port message)
  (make-cmdl parent
	     input-port
	     output-port
	     repl-driver
	     (make-repl-state prompt
			      environment
			      syntax-table
			      (make-repl-history reader-history-size)
			      (make-repl-history printer-history-size))
	     (cmdl-message/append
	      message
	      (cmdl-message/active
	       (lambda ()
		 (hook/repl-environment (nearest-repl) environment))))))

(define (repl-driver repl)
  (fluid-let ((hook/error-handler default/error-handler))
    (hook/cmdl-prompt repl (repl/prompt repl))
    (let ((s-expression (hook/repl-read repl)))
      (cmdl-message/value
       (hook/repl-eval repl
		       s-expression
		       (repl/environment repl)
		       (repl/syntax-table repl))))))

(define (repl? object)
  (and (cmdl? object)
       (repl-state? (cmdl/state object))))

(define-integrable (repl/prompt repl)
  (repl-state/prompt (cmdl/state repl)))

(define-integrable (set-repl/prompt! repl prompt)
  (set-repl-state/prompt! (cmdl/state repl) prompt))

(define-integrable (repl/environment repl)
  (repl-state/environment (cmdl/state repl)))

(define-integrable (set-repl/environment! repl environment)
  (set-repl-state/environment! (cmdl/state repl) environment))

(define-integrable (repl/syntax-table repl)
  (repl-state/syntax-table (cmdl/state repl)))

(define-integrable (set-repl/syntax-table! repl syntax-table)
  (set-repl-state/syntax-table! (cmdl/state repl) syntax-table))

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
  (let ((repl (nearest-repl)))
    (if repl
	(repl/environment repl)
	user-initial-environment)))

(define (nearest-repl/syntax-table)
  (let ((repl (nearest-repl)))
    (if repl
	(repl/syntax-table repl)
	user-initial-syntax-table)))

(define (push-repl environment message prompt)
  (let ((parent (nearest-cmdl)))
    (make-repl parent
	       environment
	       (nearest-repl/syntax-table)
	       prompt
	       (cmdl/input-port parent)
	       (cmdl/output-port parent)
	       message)))

(define (read-eval-print environment message prompt)
  (with-standard-proceed-point
   (lambda ()
     (push-repl environment message prompt))))

(define (breakpoint message environment)
  (push-repl environment message "Bkpt->"))

(define (breakpoint-procedure environment message . irritants)
  (with-history-disabled
   (lambda ()
     (with-standard-proceed-point
      (lambda ()
	(breakpoint (apply cmdl-message/error message irritants)
		    environment))))))

;;;; Hooks

(define hook/repl-environment)
(define hook/repl-read)
(define hook/repl-eval)
(define hook/repl-write)

(define (default/repl-environment repl environment)
  (let ((port (cmdl/output-port repl)))
    (if (not (interpreter-environment? environment))
	(begin
	  (write-string
	   "\n;Warning! this environment is a compiled-code environment:")
	  (write-string
	   "\n; Assignments to most compiled-code bindings are prohibited,")
	  (write-string
	   "\n; as are certain other environment operations.")))
    (let ((package (environment->package environment)))
      (if package
	  (begin
	    (write-string "\n;Package: " port)
	    (write (package/name package) port)))))
  unspecific)

(define (default/repl-read repl)
  (let ((s-expression (read (cmdl/input-port repl))))
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
  (let ((port (cmdl/output-port repl)))
    (if (undefined-value? object)
	(write-string "\n;No value" port)
	(begin
	  (write-string "\n;Value: " port)
	  (write object port)))))

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
	(begin (set-car! elements object)
	       (set-repl-history/elements! history (cdr elements))))))

(define (repl-history/read history n)
  (if (not (and (integer? n)
		(not (negative? n))		(< n (repl-history/size history))))
      (error "REPL-HISTORY/READ: Bad argument" n))
  (list-ref (repl-history/elements history)
	    (- (-1+ (repl-history/size history)) n)))

;;; User Interface Stuff

(define user-repl-environment)
(define user-repl-syntax-table)

(define (pe)
  (let ((environment (nearest-repl/environment)))
    (let ((package (environment->package environment)))
      (if package
	  (package/name package)
	  environment))))

(define (ge environment)
  (let ((repl (nearest-repl))
	(environment (->environment environment)))
    (set! user-repl-environment environment)
    (set-repl-state/environment! (cmdl/state repl) environment)
    (hook/repl-environment repl environment)
    environment))

(define (ve environment)
  (let ((repl (nearest-repl))
	(environment (->environment environment)))
    (set-repl-state/environment! (cmdl/state repl) environment)
    (set-repl-state/prompt! (cmdl/state repl) "Visiting->")
    (hook/repl-environment repl environment)
    environment))

(define (->environment object)
  (cond ((environment? object)
	 object)
	((package? object)
	 (package/environment object))
	((compound-procedure? object)	 (procedure-environment object))
	((promise? object)
	 (promise-environment object))
	(else
	 (let ((package
		(let ((package-name
		       (cond ((symbol? object) (list object))
			     ((list? object) object)
			     (else false))))
		  (and package-name
		       (name->package package-name)))))
	   (if (not package)
	       (error "->ENVIRONMENT: Not an environment" object))
	   (package/environment package)))))

(define (gst syntax-table)
  (guarantee-syntax-table syntax-table)
  (set! user-repl-syntax-table syntax-table)
  (set-repl-state/syntax-table! (cmdl/state (nearest-repl)) syntax-table)
  unspecific)

(define (vst syntax-table)
  (guarantee-syntax-table syntax-table)
  (set-repl-state/syntax-table! (cmdl/state (nearest-repl)) syntax-table)
  unspecific)

(define (re #!optional index)
  (let ((repl (nearest-repl)))
    (hook/repl-eval repl
		    (repl-history/read (repl/reader-history repl)
				       (if (default-object? index) 1 index))
		    (repl/environment repl)
		    (repl/syntax-table repl))))

(define (in #!optional index)
  (repl-history/read (repl/reader-history (nearest-repl))
		     (if (default-object? index) 1 index)))

(define (out #!optional index)
  (repl-history/read (repl/printer-history (nearest-repl))
		     (-1+ (if (default-object? index) 1 index))))

;; Compatibility.
(define %ge ge)
(define %ve ve)
(define %gst gst)
(define %vst vst)
(define %in in)
(define %out out)

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

(define hook/read-command-char)
(define hook/prompt-for-confirmation)
(define hook/prompt-for-expression)

(define (default/read-command-char cmdl prompt)
  ;; Prompt argument is random.  Emacs interface needs it right now.
  prompt
  (read-char-internal (cmdl/input-port cmdl)))

(define (default/prompt-for-confirmation cmdl prompt)
  (let ((input-port (cmdl/input-port cmdl))
	(output-port (cmdl/output-port cmdl)))
    (let loop ()
      (newline output-port)
      (write-string prompt output-port)
      (write-string "(y or n) " output-port)
      (let ((char (char-upcase (read-char-internal input-port))))
	(cond ((or (char=? #\Y char)
		   (char=? #\Space char))
	       (write-string "Yes" output-port)
	       true)
	      ((or (char=? #\N char)
		   (char=? #\Rubout char))
	       (write-string "No" output-port)
	       false)
	      (else
	       (beep output-port)
	       (loop)))))))

(define (default/prompt-for-expression cmdl prompt)
  (let ((output-port (cmdl/output-port cmdl)))
    (newline output-port)
    (write-string prompt output-port)    (read (cmdl/input-port cmdl))))

(define (read-char-internal input-port)
  (let loop ()
    (let ((char (read-char input-port)))
      (if (char=? char char:newline)
	  (loop)
	  char))))