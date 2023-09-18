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

;;;; Continuation Browser

(declare (usual-integrations))

#| TO DO

Make environment browsing mode; the debugger mode can be a superset
of that mode: Add optional marker lines for environments.  If you do
the C-c C-a command to describe the environment frames in the current
subproblem or reduction, the debugger should use the correct
environment when you do evaluations in those environment frames.
Make commands for moving by environment level.  Later, change this to
execute Where in another buffer depending on the state of a flag.

Make a variable that specifies whether to prompt the user if more
than a certain number of variables are about to be printed during an
environment-browsing command.

By default, when the debugger starts, don't show history levels
inside the system.  To detect system code, see
~arthur/new6001/detect.scm.  Predicate SYSTEM-FRAME? is already
in place.

MarkF has code to use the correct syntax tables for evaluation.

Add limits to the depth and breadth of objects printed by the
debugger, to avoid problems caused by displaying circular objects.
Note $se/evlcom.scm: TRANSCRIPT-LIST-DEPTH-LIMIT and
TRANSCRIPT-LIST-BREADTH-LIMIT.

Make C-c C-k evaluate in the environment in which the error occurred.
Otherwise, the "Define x to a given value" restart for unbound
variable errors won't work.  This seems to be a bug in the regular
debugger, too.

Make C-c C-z work in the case where an error happens during
evaluation of the return expression, the debugger starts on the new
error, and return is done from the second debugger straight through
the first back into the original computation.  The restart itself
works, but the message "Scheme error" is printed upon starting the
second debugger.

Jinx: Depending on the state of a flag, never invoke debugger on
unbound variable errors from the expression you eval in the
interaction buffer (or debugger buffer).  Actually, how about a
general filter on conditions that will start the debugger?  Provide a
default filter for ignoring unbound variables.

Jinx: Display the offending expression evaluated by the user.  Display
it just above the error message line.

Make a way to restrict the possible restarts to not include restarts
that could stop Edwin.

Make a narrow interface between Edwin and the debugger so it will be
easy to write this debugger for Emacs.

Number input lines so that it is possible to tell the order in which
you evaluated your expressions.  This could be particularly useful
for TAs looking over students' shoulders.

Once outline mode has been written for Edwin, add commands to expand
and contract subproblems and reductions.

|#

(define-variable debugger-split-window?
  "True means use another window for the debugger buffer; false means
use the current window."
  #t
  boolean?)

(define-variable debugger-open-markers?
  "True means newlines are inserted between marker lines."
  #t
  boolean?)

(define-variable debugger-verbose-mode?
  "True means display extra information without the user requesting it."
  #f
  boolean?)

(define-variable debugger-expand-reductions?
  "True says to insert reductions when reduction motion commands are used
in a subproblem whose reductions aren't already inserted."
  #t
  boolean?)

(define-variable debugger-debug-evaluations?
  "True means evaluation errors in a debugger buffer start new debuggers."
  #f
  boolean?)

(define-variable debugger-default
  "The default debugger to use:
'debug specifies the standard debugger
'continuation-browser specifies the continuation browser."
  'debug
  (lambda (object) (memq object '(debugger continuation-browser))))

(define starting-debugger? #f)
(define in-debugger-evaluation? #f)

(define (browse-scheme-error error-type condition ask?)
  (cond (starting-debugger?
	 (quit-editor-and-signal-error condition))
	((and in-debugger-evaluation?
	      (not (ref-variable debugger-debug-evaluations? #f)))
	 unspecific)
	(else
	 (let ((start-debugger
		(lambda ()
		  (fluid-let ((starting-debugger? #t))
		    ((if (ref-variable debugger-split-window? #f)
			 select-buffer-other-window
			 select-buffer)
		     (continuation-browser-buffer condition))))))
	   (if ask?
	       (if (cleanup-pop-up-buffers
		    (lambda ()
		      (standard-error-report error-type condition #t)
		      (editor-beep)
		      (prompt-for-confirmation? "Start debugger")))
		   (start-debugger))
	       (begin
		 (start-debugger)
		 (message (string-capitalize (symbol->string error-type))
			  " error")
		 (editor-beep))))
	 (return-to-command-loop condition))))

(define-command continuation-browser-start
  "Invoke the continuation-browser on CONTINUATION."
  "XBrowse Continuation"
  (lambda (continuation)
    (let ((buffer (continuation-browser-buffer continuation)))
      ((if (ref-variable debugger-split-window?)
	   select-buffer-other-window
	   select-buffer)
       buffer))))

(define-integrable (buffer-ctree buffer)
  (buffer-get buffer 'ctree))

;;;; Main Entry

(define (continuation-browser-buffer object)
  (let ((buffers (find-debugger-buffers)))
    (if (and (not (null? buffers))
	     (null? (cdr buffers))
	     (ref-variable debugger-one-at-a-time?)
	     (or (eq? #t (ref-variable debugger-one-at-a-time?))
		 (prompt-for-confirmation?
		  "Another debugger buffer exists.  Delete it")))
	(kill-buffer (car buffers))))
  (let ((buffer (new-buffer "*debug*"))
	(ctree (->ctree object)))
    (set-buffer-major-mode! buffer (ref-mode-object continuation-browser))
    (buffer-put! buffer 'ctree ctree)
    (let ((max-subproblems (ref-variable debugger-max-subproblems buffer))
	  (hide-system-code? (ref-variable debugger-hide-system-code? buffer)))
      (with-group-undo-disabled (buffer-group buffer)
	(lambda ()
	  (let ((port (mark->output-port (buffer-start buffer))))
	    (if (ref-variable debugger-show-help-message? buffer)
		(print-help-message buffer port))
	    (if (condition? object)
		(begin
		  (write-string "The error that started the debugger is:" port)
		  (newline port)
		  (write-string "  " port)
		  (write-condition-report object port)
		  (newline port)
		  (newline port)
		  (print-restarts object buffer port)))
	    (if (let loop ((snode (ctree-subproblems ctree)))
		  (or (and max-subproblems
			   (= (ctree-subproblem-index snode)
			      max-subproblems))
		      (and hide-system-code?
			   (ctree-subproblem-system-boundary? snode))
		      (begin
			(newline port)
			(print-subproblem snode port)
			(let ((next (ctree-subproblem-earlier snode)))
			  (and next
			       (loop next))))))
		(display-more-subproblems-message buffer)))))
      (let ((point (forward-subproblem (buffer-start buffer) 1)))
	(set-buffer-point! buffer point)
	(if (ref-variable debugger-verbose-mode? buffer)
	    (call-with-interface-port point
	      (lambda (port)
		(print-cnode-summary (mark-cnode point) port))))
	(push-buffer-mark! buffer point)
	(buffer-not-modified! buffer)
	buffer))))

(define (find-debugger-buffers)
  (let ((debugger-mode (ref-mode-object continuation-browser)))
    (let loop ((buffers (buffer-list)))
      (cond ((null? buffers)
	     buffers)
	    ((eq? (buffer-major-mode (car buffers)) debugger-mode)
	     (cons (car buffers) (loop (cdr buffers))))
	    (else
	     (loop (cdr buffers)))))))

(define (print-help-message buffer port)
  (write-string (substitute-command-keys debugger-help-message buffer) port)
  (newline port)
  (newline port))

(define debugger-help-message
  "This is a debugger buffer:

  Marker lines identify stack frames, most recent first.
  Expressions are evaluated in the environment of the line above the point.

  In the marker lines,

    -C- means frame was generated by Compiled code
    -I- means frame was generated by Interpreted code

    S=x means frame is in subproblem number x
    R=y means frame is reduction number y
    #R=z means there are z reductions in the subproblem;
      use \\[continuation-browser-forward-reduction] to see them

  \\[continuation-browser-print-subproblem-summary] describes the current subproblem or reduction.
  \\[describe-mode] shows information about debugger commands.
  Use \\[kill-buffer] to quit the debugger.")

(define (print-restarts condition buffer port)
  (let ((restarts (condition/restarts condition)))
    (if (not (null? restarts))
	(begin
	  (write-string "Restart options:" port)
	  (write-restarts restarts port
	    (lambda (index port)
	      (write-string (string-pad-left (number->string index) 3) port)
	      (write-string ":" port)))
	  (write-string
	   (substitute-command-keys
	    "Use \\[continuation-browser-condition-restart] to invoke any of these restarts."
	    buffer)
	   port)
	  (newline port)))))

(define-major-mode continuation-browser scheme "Debug"
  "Major mode for debugging Scheme programs and browsing Scheme continuations.
Evaluation commands are similar to those of Scheme Interaction mode.

  Marker lines identify stack frames, most recent first.
  Expressions are evaluated in the environment of the line above the point.

  In the marker lines,

    -C- means frame was generated by Compiled code
    -I- means frame was generated by Interpreted code

    S=x means frame is in subproblem number x
    R=y means frame is reduction number y
    #R=z means there are z reductions in the subproblem;
      use \\[continuation-browser-forward-reduction] to see them

Evaluate expressions

  \\[continuation-browser-eval-last-sexp] evaluates the expression preceding point in the
    environment of the current frame.
  \\[continuation-browser-eval-last-sexp/dynamic] evaluates the expression preceding point in the
    environment AND DYNAMIC STATE of the current frame.

Move between subproblems and reductions

  \\[continuation-browser-forward-reduction] moves forward one reduction (earlier in time).
  \\[continuation-browser-backward-reduction] moves backward one reduction (later in time).

  \\[continuation-browser-forward-subproblem] moves forward one subproblem (earlier in time).
  \\[continuation-browser-backward-subproblem] moves backward one subproblem (later in time).

  \\[continuation-browser-go-to] moves directly to a subproblem (given its number).

Display debugging information

  \\[continuation-browser-show-all-frames] shows All bindings of the current environment and its ancestors.
  \\[continuation-browser-show-current-frame] shows bindings of identifiers in the Current environment.
  \\[continuation-browser-print-environment] describes the current Environment.
  \\[continuation-browser-print-expression] pretty prints the current expression.
  \\[continuation-browser-print-environment-procedure] pretty prints the procedure that created the current environment.
  \\[continuation-browser-expand-reductions] shows the Reductions of the current subproblem level.
  \\[continuation-browser-print-subproblem-summary] describes the current subproblem or reduction.
  \\[continuation-browser-expand-subproblems] shows subproblems not already displayed.
  \\[continuation-browser-frame] displays the current stack frame in internal format.

Miscellany

  \\[continuation-browser-condition-restart] continues the program using a standard restart option.
  \\[continuation-browser-return-from] returns from the current subproblem with the value of the expression
    preceding the point.
  \\[continuation-browser-return-to] returns to the current subproblem with the value of the expression
    preceding the point.
  \\[continuation-browser-retry] retries the offending expression, returning from the current
    subproblem with its value.

Use \\[kill-buffer] to quit the debugger."
  (lambda (buffer)
    (define-variable-local-value! buffer
      (ref-variable-object comint-input-ring)
      (make-ring (ref-variable comint-input-ring-size)))
    (define-variable-local-value! buffer
      (ref-variable-object evaluation-input-recorder)
      continuation-browser-input-recorder)
    (define-variable-local-value! buffer
      (ref-variable-object evaluation-output-receiver)
      continuation-browser-output-receiver)))

(define (continuation-browser-input-recorder region)
  (ring-push! (ref-variable comint-input-ring) (region->string region)))

(define (continuation-browser-output-receiver value output)
  (let ((point (mark-left-inserting-copy (current-point))))
    (insert-string output point)
    (guarantee-newlines 1 point)
    (insert-string (transcript-value-prefix-string value #t) point)
    (insert-string (transcript-value-string value) point)
    (insert-newlines 2 point)
    (mark-temporary! point)))

;;; Disable EVAL-CURRENT-BUFFER in Debugger Mode.  It is inherited
;;; from Scheme mode but does not make sense here:

(define-key 'continuation-browser #\M-o
  'undefined)

;;; Comint History
(define-key 'continuation-browser #\M-p
  'comint-previous-input)
(define-key 'continuation-browser #\M-n
  'comint-next-input)
(define-key 'continuation-browser '(#\C-c #\C-r)
  'comint-history-search-backward)
(define-key 'continuation-browser '(#\C-c #\C-s)
  'comint-history-search-forward)

;;; Evaluation Commands
(define-key 'continuation-browser '(#\C-x #\C-e)
  'continuation-browser-eval-last-sexp)
(define-key 'continuation-browser '(#\C-x #\C-r)
  'continuation-browser-eval-last-sexp/dynamic)
(define-key 'continuation-browser #\M-z
  'continuation-browser-eval-defun)
(define-key 'continuation-browser '(#\M-C-z)
  'continuation-browser-eval-region)

;;; Motion Commands
(define-key 'continuation-browser '(#\C-c #\C-f)
  'continuation-browser-forward-reduction)
(define-key 'continuation-browser '(#\C-c #\C-n)
  'continuation-browser-forward-subproblem)
(define-key 'continuation-browser '(#\C-c #\C-b)
  'continuation-browser-backward-reduction)
(define-key 'continuation-browser '(#\C-c #\C-p)
  'continuation-browser-backward-subproblem)
(define-key 'continuation-browser '(#\C-c #\C-w)
  'continuation-browser-go-to)

;;; Information-display Commands
(define-key 'continuation-browser '(#\C-c #\C-a)
  'continuation-browser-show-all-frames)
(define-key 'continuation-browser '(#\C-c #\C-c)
  'continuation-browser-show-current-frame)
(define-key 'continuation-browser '(#\C-c #\C-e)
  'continuation-browser-print-environment)
(define-key 'continuation-browser '(#\C-c #\C-l)
  'continuation-browser-print-expression)
(define-key 'continuation-browser '(#\C-c #\C-o)
  'continuation-browser-print-environment-procedure)
(define-key 'continuation-browser '(#\C-c #\C-m)
  'continuation-browser-expand-reductions)
(define-key 'continuation-browser '(#\C-c #\C-t)
  'continuation-browser-print-subproblem-summary)
(define-key 'continuation-browser '(#\C-c #\C-x)
  'continuation-browser-expand-subproblems)
(define-key 'continuation-browser '(#\C-c #\C-y)
  'continuation-browser-frame)

;;; Miscellaneous Commands
(define-key 'continuation-browser '(#\C-c #\C-k)
  'continuation-browser-condition-restart)
(define-key 'continuation-browser '(#\C-c #\C-j)
  'continuation-browser-return-to)
(define-key 'continuation-browser '(#\C-c #\C-z)
  'continuation-browser-return-from)
(define-key 'continuation-browser '(#\C-c #\C-d)
  'continuation-browser-retry)
(define-key 'continuation-browser '(#\C-c #\C-g)
  'continuation-browser-abort-all)
(define-key 'continuation-browser '(#\C-c #\C-u)
  'continuation-browser-abort-previous)
(define-key 'continuation-browser '(#\C-c #\C-M-y)
  'continuation-browser-display-stack-elements)

;;;; Evaluation Commands

(define-command continuation-browser-eval-region
  "Evaluate the region."
  "r"
  (lambda (region)
    (let ((environment
	   (cnode-evaluation-environment (start-evaluation region))))
      (fluid-let ((in-debugger-evaluation? #t))
	(evaluate-region region environment)))))

(define (start-evaluation region)
  (if (region-contains-marker? region)
      (editor-error "Can't evaluate region containing markers."))
  (set-current-point! (region-end region))
  (mark-cnode (region-start region)))

(define-command continuation-browser-eval-defun
  "Evaluate definition that point is in or before."
  ()
  (lambda ()
    ((ref-command continuation-browser-eval-region)
     (let ((input-mark (current-definition-start)))
       (make-region input-mark (forward-sexp input-mark 1 'error))))))

(define-command continuation-browser-eval-last-sexp
  "Evaluate the expression preceding point."
  ()
  (lambda ()
    ((ref-command continuation-browser-eval-region)
     (let ((input-mark (backward-sexp (current-point) 1 'error)))
       (make-region input-mark (forward-sexp input-mark 1 'error))))))

(define-command continuation-browser-eval-region/dynamic
  "Evaluate the region.
The evaluation occurs in the dynamic state of the current frame."
  "r"
  (lambda (region)
    (let ((cnode (start-evaluation region)))
      (let ((environment (cnode-evaluation-environment cnode))
	    (continuation
	     (ctree-subproblem->continuation
	      (ctree-subproblem cnode)))
	    (old-hook hook/repl-eval))
	(fluid-let
	    ((in-debugger-evaluation? #t)
	     (hook/repl-eval
	      (lambda (expression environment repl)
		(let ((unique (cons 'unique 'id)))
		  (let ((result
			 (call-with-current-continuation
			  (lambda (continuation*)
			    (within-continuation continuation
			      (lambda ()
				(bind-condition-handler
				    '()
				    (lambda (condition)
				      (continuation* (cons unique condition)))
				  (lambda ()
				    (continuation*
				     (old-hook expression
					       environment
					       repl))))))))))
		    (if (and (pair? result)
			     (eq? unique (car result)))
			(error (cdr result))
			result))))))
	  (evaluate-region region environment))))))

(define-command continuation-browser-eval-last-sexp/dynamic
  "Evaluate the expression preceding point.
The evaluation occurs in the dynamic state of the current frame."
  ()
  (lambda ()
    ((ref-command continuation-browser-eval-region/dynamic)
     (let ((input-mark (backward-sexp (current-point) 1 'error)))
       (make-region input-mark (forward-sexp input-mark 1 'error))))))

;;;; Motion Commands

;;; The subproblem and reduction motion commands rely, in many
;;; places, on the assumption that subproblem and reduction numbers
;;; increase downward in the buffer, and that no subproblem/reduction
;;; marker line is repeated.  Of course, the user can violate this
;;; assumption by constructing or copying a marker, but the program
;;; is robust with respect to such conniving, as long as the
;;; reduction and subproblem specified by the numbers in the marker
;;; exist.  The only time it should be possible to notice an effect
;;; of this assumption is when a reduction or subproblem that is
;;; already displayed is automatically redisplayed because the
;;; existing one appeared out of order.

(define-command continuation-browser-forward-subproblem
  "Move one or more subproblems forward."
  "p"
  (lambda (argument) (move-thing forward-subproblem argument 'error)))

(define-command continuation-browser-backward-subproblem
  "Move one or more subproblems backward."
  "p"
  (lambda (argument) (move-thing backward-subproblem argument 'error)))

(define-command continuation-browser-forward-reduction
  "Move one or more reductions forward.
Display reductions that exist but are not yet displayed.
If there are no more reductions for the current subproblem,
move to the first reduction shown in the next subproblem."
  "p"
  (lambda (argument) (move-thing forward-reduction argument 'error)))

(define-command continuation-browser-backward-reduction
  "Move one or more reductions backward.
Display reductions that exist but are not yet displayed.
If there are no more reductions for the current subproblem,
move to the last reduction shown in the previous subproblem."
  "p"
  (lambda (argument) (move-thing backward-reduction argument 'error)))

(define-command continuation-browser-go-to
  "Move to an arbitrary subproblem.
Prompt for the subproblem number if not given as an argument.
Move to the last subproblem if the subproblem number is too high."
  "NSubproblem number"
  (lambda (destination-subproblem-number)
    (set-current-point!
     (let ((end (group-end (current-point)))
	   (not-found
	    (lambda ()
	      (editor-error "Cannot find subproblem"
			    destination-subproblem-number))))
       (let ((last-subproblem-number (current-subproblem-number end)))
	 (if (not last-subproblem-number)
	     (not-found))
	 (cond ((< destination-subproblem-number last-subproblem-number)
		(let loop ((point (backward-subproblem end 1)))
		  (if (not point)
		      (not-found))
		  (let ((subproblem (current-subproblem-number point)))
		    (if (not subproblem)
			(not-found))
		    (if (= subproblem destination-subproblem-number)
			point
			(loop (backward-subproblem point 1))))))
	       ((> destination-subproblem-number last-subproblem-number)
		(forward-subproblem
		 end
		 (- destination-subproblem-number last-subproblem-number)
		 'limit))
	       (else end)))))))

;;;; Information-display Commands

(define-command continuation-browser-show-all-frames
  "Print the bindings of all frames of the current environment."
  ()
  (lambda ()
    (display-current-environment
     (lambda (env port)
       (show-frames env 0 port)))))

(define-command continuation-browser-show-current-frame
  "Print the bindings of the current frame of the current environment."
  ()
  (lambda ()
    (display-current-environment
     (lambda (env port)
       (debugger-presentation port
	 (lambda ()
	   (show-frame env #f #f port)))))))

(define-command continuation-browser-print-environment
  "Identify the environment of the current frame."
  ()
  (lambda ()
    (let ((point (current-point)))
      (call-with-interface-port point
	(lambda (port)
	  (debugger-presentation port
	    (lambda ()
	      (print-subproblem-environment (mark-snode point) port))))))))

(define-command continuation-browser-print-expression
  "Pretty print the current expression."
  "P"
  (lambda (argument)
    (let ((point (current-point)))
      (call-with-interface-port point
	(lambda (port)
	  (push-current-mark! point)
	  (let ((cnode (mark-cnode point))
		(message
		 (lambda (string)
		   (fresh-line port)
		   (write-string "; " port)
		   (write-string string port)))
		(pp (lambda (obj)
		      (fresh-line port)
		      (pp obj port #t))))
	    (if (ctree-reduction? cnode)
		(pp (ctree-reduction-expression cnode))
		(let ((exp (ctree-subproblem-expression cnode))
		      (sub (ctree-subproblem-subexpression cnode)))
		  (cond ((or (dbg-expression-compiled? exp)
			     (dbg-expression-undefined? exp))
			 (message "Unknown expression"))
			((dbg-printer? exp)
			 (message
			  (call-with-output-string
			    (lambda (port)
			      (dbg-printer-apply exp #t port)))))
			((or argument (dbg-expression-undefined? sub))
			 (pp exp port))
			(else
			 (debugger-pp-highlight-subexpression
			  exp sub 0 port)))))))))))

(define-command continuation-browser-print-environment-procedure
  "Pretty print the procedure that created the current environment."
  ()
  (lambda ()
    (display-current-environment show-environment-procedure)))

(define-command continuation-browser-expand-reductions
  "Expand all the reductions of the current subproblem.
If already expanded, move the point to one of the reductions."
  ()
  (lambda ()
    (let ((point (current-point)))
      (if (reductions-expanded? point)
	  (temporary-message
	   "Reductions for this subproblem already expanded.")
	  (expand-reductions point)))))

(define-command continuation-browser-print-subproblem-summary
  "Print the current subproblem or reduction in the standard format."
  ()
  (lambda ()
    (let ((mark (current-point)))
      (call-with-interface-port mark
	(lambda (port)
	  (print-cnode-summary (mark-cnode mark) port))))))

(define-command continuation-browser-expand-subproblems
  "Expand all subproblems, or ARG more subproblems if argument is given."
  "P"
  (lambda (argument)
    (let ((subproblem-number
	   (if argument
	       (+ (or (current-subproblem-number (group-end (current-point)))
		      (editor-error "Can't find subproblem marker"))
		  (command-argument-numeric-value argument))
	       (- (count-subproblems (current-buffer)) 1))))
      (let ((point (mark-right-inserting-copy (current-point))))
	((ref-command continuation-browser-go-to) subproblem-number)
	(mark-temporary! point)
	(set-current-point! point)))))

(define-command continuation-browser-frame
  "Show the current subproblem's stack frame in internal format."
  ()
  (lambda ()
    (let ((mark (current-point)))
      (call-with-interface-port mark
	(lambda (port)
	  (print-raw-frame (mark-snode mark) port))))))

;;;; Miscellaneous Commands

(define-command continuation-browser-condition-restart
  "Continue the program using a standard restart option.
Prefix argument means do not kill the debugger buffer."
  "P"
  (lambda (avoid-deletion?)
    (fluid-let ((hook/invoke-restart
		 (lambda (continuation arguments)
		   (invoke-continuation continuation
					arguments
					avoid-deletion?))))
      (let ((point (current-point)))
	(call-with-interface-port point
	  (lambda (port)
	    (debug/invoke-restart (mark-ctree point) port)))))))

(define-command continuation-browser-return-to
  "Return TO the current subproblem with a value.
Invoke the continuation corresponding to this subproblem on the value
of the expression before the point.
Prefix argument means do not kill the debugger buffer."
  "P"
  (lambda (avoid-deletion?)
    (subproblem-enter (mark-snode (current-point))
		      ((ref-command continuation-browser-eval-last-sexp))
		      avoid-deletion?)))

(define-command continuation-browser-return-from
  "Return FROM the current subproblem with a value.
Invoke the continuation that is waiting for the value of the current
subproblem on the value of the expression before the point.
Prefix argument means do not kill the debugger buffer."
  "P"
  (lambda (avoid-deletion?)
    (subproblem-enter (guarantee-earlier-subproblem
		       (mark-snode (current-point)))
		      ((ref-command continuation-browser-eval-last-sexp))
		      avoid-deletion?)))

(define-command continuation-browser-retry
  "Retry the expression of the current subproblem.
Prefix argument means do not kill the debugger buffer."
  "P"
  (lambda (avoid-deletion?)
    (let ((snode (mark-snode (current-point))))
      (if (not (ctree-subproblem-has-expression? snode))
	  (editor-error "Can't retry; invalid expression."))
      (subproblem-enter
       (guarantee-earlier-subproblem snode)
       (extended-scode-eval (ctree-subproblem-expression snode)
			    (ctree-subproblem-environment snode))
       avoid-deletion?))))

(define-command continuation-browser-abort-all
  "Insert restarts"
  ()
  (lambda ()
    (continuation-browser-abort (reverse (current-restarts)))))

(define-command continuation-browser-abort-previous
  "Insert restarts"
  ()
  (lambda ()
    (continuation-browser-abort (current-restarts))))

(define-command continuation-browser-display-stack-elements
  "Show the elements on the current stack frame"
  ()
  (lambda ()
    (let* ((point (current-point))
	   (vec (ctree-subproblem-raw-frame (mark-snode point))))
      (call-with-interface-port point
	(lambda (port)
	  (push-current-mark! point)
	  (fresh-line port)
	  (let* ((depth (-1+ (vector-length vec)))
		 (mlen (string-length (number->string depth)))
		 (pad-len (max 5 mlen))
		 (padded
		  (lambda (s)
		    (string-pad-left s pad-len #\Space)))
		 (blanks (make-string pad-len #\Space)))

	    (write-string ";; " port)
	    (write-string (padded "Depth") port)
	    (write-string "  Bottom of stack frame" port)
	    (newline port)
	    (write-string ";;" port)
	    (let ((pad (if (= pad-len mlen)
			   padded
			   (let* ((right (quotient (- pad-len mlen) 2))
				  (rest (- pad-len right))
				  (blanks (make-string right #\Space)))
			     (lambda (s)
			       (string-append
				(string-pad-left s rest #\Space)
				blanks))))))

	      (do ((elements (reverse! (vector->list vec))
			     (cdr elements))
		   (depth depth (-1+ depth)))
		  ((null? elements))
		(newline port)
		(write-string ";; " port)
		(write-string (pad (number->string depth)) port)
		(write-string "  " port)
		(write (car elements) port)))
	    (newline port)
	    (write-string ";;" port)
	    (newline port)
	    (write-string ";; " port)
	    (write-string blanks port)
	    (write-string "  Top of stack frame" port))
	  (newline port)
	  (newline port))))))

(define (subproblem-enter cnode value avoid-deletion?)
  (if (or (not (ref-variable debugger-confirm-return?))
	  (prompt-for-confirmation? "Continue with this value"))
      (invoke-continuation (ctree-subproblem->continuation
			    (ctree-subproblem cnode))
			   (list value)
			   avoid-deletion?)))

(define (invoke-continuation continuation arguments avoid-deletion?)
  (let ((buffer (current-buffer)))
    (if (and (not avoid-deletion?)
	     (ref-variable debugger-quit-on-return?))
	(kill-buffer-interactive buffer))
    ((or (buffer-get buffer 'invoke-continuation) apply)
     continuation arguments)))

(define (guarantee-earlier-subproblem cnode)
  (or (ctree-subproblem-earlier (ctree-subproblem cnode))
      (editor-error "Can't continue; no earlier subproblem")))

(define (current-restarts)
  (let ((condition (ctree-condition (mark-ctree (current-point)))))
    (if condition
	(condition/restarts condition)
	(bound-restarts))))

(define (continuation-browser-abort restarts)
  (let ((restart
	 (find (lambda (restart)
		 (eq? (restart/name restart) 'abort))
	       restarts)))
    (if (not restart)
	(editor-error "Can't find an abort restart")
	(fluid-let ((hook/invoke-restart
		     (lambda (continuation arguments)
		       (invoke-continuation continuation
					    arguments
					    #f))))
	  (invoke-restart restart)))))

;;;; Marker Generation

(define (expand-subproblem mark)
  (let ((buffer (mark-buffer mark))
	(number (current-subproblem-number mark)))
    (if (not number)
	(editor-error "No subproblem or reduction marks"))
    (let ((number (+ number 1))
	  (count (count-subproblems buffer)))
      (if (>= number count)
	  (editor-error "No more subproblems or reductions"))
      (remove-more-subproblems-message buffer)
      (let ((port (mark->output-port mark)))
	(newline port)
	(print-subproblem (nth-subproblem buffer number) port))
      (if (< number (- count 1))
	  (display-more-subproblems-message buffer)))))

(define (display-more-subproblems-message buffer)
  (define-variable-local-value! buffer (ref-variable-object mode-line-process)
    '(run-light (": more-subproblems " run-light) ": more-subproblems"))
  (buffer-modeline-event! buffer 'process-status))

(define (remove-more-subproblems-message buffer)
  (let ((variable (ref-variable-object mode-line-process)))
    (define-variable-local-value! buffer variable
      (variable-default-value variable)))
  (buffer-modeline-event! buffer 'process-status))

(define (perhaps-expand-reductions mark)
  (if (and (ref-variable debugger-expand-reductions?)
	   (not (reductions-expanded? mark)))
      (begin
	(message "Expanding reductions...")
	(expand-reductions (end-of-subproblem mark))
	(temporary-message "Expanding reductions...done"))))

(define (expand-reductions mark)
  (let ((port (mark->output-port mark))
	(snode (mark-snode mark)))
    (let loop ((rnode (ctree-subproblem-reductions snode)))
      (if rnode
	  (begin
	    (newline port)
	    (print-reduction rnode port)
	    (loop (ctree-reduction-earlier rnode)))))))

(define (reductions-expanded? mark)
  ;; Return true whenever expansion is impossible at MARK, even if
  ;; because MARK is outside any subproblem or because there are no
  ;; reductions for the subproblem.  If only some of the reductions
  ;; appear already (e.g. if the others have been deleted by the
  ;; user), still return true.
  (let ((subproblem-above (find-previous-subproblem-marker mark)))
    (or (not subproblem-above)
	(let ((subproblem-number-above (re-match-extract-subproblem))
	      (reduction-count (re-match-extract-reduction-count)))
	  (and reduction-count
	       (let ((reduction-below
		      (find-next-marker
		       (line-end subproblem-above 0))))
		 (and reduction-below
		      (= (re-match-extract-subproblem)
			 subproblem-number-above))))))))


(define-structure (unparser-literal
		   (conc-name unparser-literal/)
		   (print-procedure
		    (lambda (instance port)
		      (write-string (unparser-literal/string instance)
				    port)))
		   (constructor unparser-literal/make))
  string)

(define-variable subexpression-start-marker
  "Subexpressions are preceeded by this value."
  "#"
  string?)

(define-variable subexpression-end-marker
  "Subexpressions are followed by this value."
  "#"
  string?)

(define (print-subproblem snode port)
  (print-history-level
   (ctree-subproblem-cc-frame? snode)
   (ctree-subproblem-index snode)
   (let ((reductions (ctree-subproblem-n-reductions snode)))
     (if (zero? reductions)
	 " -------- "
	 (string-append " #R=" (number->string reductions) " --- ")))
   (lambda (port*)
     (let ((exp (ctree-subproblem-expression snode))
	   (sub (ctree-subproblem-subexpression snode)))
       (cond ((dbg-expression-undefined? exp)
	      (write-string ";undefined expression" port*))
	     ((dbg-expression-compiled? exp)
	      (write-string ";compiled code" port*))
	     ((dbg-printer? exp)
	      (dbg-printer-apply exp #f port*))
	     (else
	      (print-with-subexpression exp sub port*)))))
   (ctree-subproblem-environment snode)
   port))

(define (print-with-subexpression expression subexpression port)
  (parameterize ((param:print-primitives-by-name? #t))
    (if (dbg-expression-undefined? subexpression)
	(write (unsyntax expression) port)
	(let ((sub (write-to-string (unsyntax subexpression))))
	  (write (unsyntax-with-substitutions
		  expression
		  (list
		   (cons subexpression
			 (unparser-literal/make
			  (string-append
			   (ref-variable subexpression-start-marker)
			   sub
			   (ref-variable subexpression-end-marker))))))
		 port)))))

(define (print-reduction rnode port)
  (print-history-level
   #f
   (ctree-subproblem-index (ctree-reduction->subproblem rnode))
   (string-append ", R=" (ctree-reduction-index rnode) " --- ")
   (lambda (port*)
     (print-reduction-as-subexpression
      (ctree-reduction-expression rnode)
      port*))
   (ctree-reduction-environment rnode)
   port))

(define (print-reduction-as-subexpression expression port)
  (parameterize ((param:print-primitives-by-name? #t))
    (write-string (ref-variable subexpression-start-marker) port)
    (write (unsyntax expression) port)
    (write-string (ref-variable subexpression-end-marker) port)))

(define (print-history-level compiled? subproblem-number reduction-id
			     write-expression environment port)
  (fresh-line port)
  (let ((level-identification
	 (string-append (if compiled? "-C- S=" "-I- S=")
			(number->string subproblem-number)
			reduction-id)))
    (write-string level-identification port)
    (let ((pad-width (max 0 (- 78 (string-length level-identification)))))
      (write-string (string-pad-right
		     (string-append
		      (cdr
		       (call-with-truncated-output-string pad-width
			 (lambda (port)
			   (write-expression port))))
		      " ")
		     pad-width
		     #\-)
		    port)))
  (if (ref-variable debugger-verbose-mode?)
      (begin
	(newline port)
	(if (environment? environment)
	    (show-environment-name environment port)
	    (write-string "There is no environment stored for this frame."
			  port))))
  (if (ref-variable debugger-open-markers?)
      (newline port)))

;;;; Marker Location

(define forward-subproblem)
(define backward-subproblem)
(make-motion-pair (lambda (start)
		    (forward-one-level start find-next-subproblem-marker))
		  (lambda (start)
		    (backward-one-level start find-previous-subproblem-marker))
  (lambda (f b)
    (set! forward-subproblem f)
    (set! backward-subproblem b)
    unspecific))

(define forward-reduction)
(define backward-reduction)
(make-motion-pair (lambda (start)
		    (let ((mark (mark-right-inserting-copy start)))
		      (perhaps-expand-reductions mark)
		      (let ((result (forward-one-level mark find-next-marker)))
			(mark-temporary! mark)
			result)))
		  (lambda (start)
		    (let ((mark (mark-left-inserting-copy start)))
		      (if (below-subproblem-marker? mark)
			  (perhaps-expand-reductions
			   (backward-subproblem mark 1)))
		      (let ((result
			     (backward-one-level mark find-previous-marker)))
			(mark-temporary! mark)
			result)))
  (lambda (f b)
    (set! forward-reduction f)
    (set! backward-reduction b)
    unspecific))

(define (forward-one-level start finder)
  (let ((next-level (finder start)))
    (if next-level
	(let ((second-next-level
	       (find-next-marker
		(line-end next-level 0))))
	  (if second-next-level
	      (line-end second-next-level -1)
	      (group-end next-level)))
	(begin
	  (message "Expanding subproblem...")
	  (expand-subproblem (group-end start))
	  (temporary-message "Expanding subproblem...done")
	  (group-end start)))))

(define (backward-one-level start finder)
  (let ((level-top (finder start)))
    (if (or (not level-top) (not (finder level-top)))
	(editor-error "Can't move beyond top level"))
    (line-end level-top -1)))

(define (end-of-subproblem mark)
  (let ((subproblem-below (find-next-subproblem-marker mark)))
    (if subproblem-below
	(line-end subproblem-below -1)
	(group-end mark))))

(define (below-subproblem-marker? mark)
  (let ((mark (find-previous-marker mark)))
    (and mark
	 (re-match-forward subproblem-regexp mark))))

(define (region-contains-marker? region)
  (re-search-forward marker-regexp
		     (line-start (region-start region) 0)
		     (line-end (region-end region) 0)))

(define (current-subproblem-number mark)
  (and (find-previous-marker mark)
       (re-match-extract-subproblem)))

(define (current-reduction-number mark)
  (and (not (below-subproblem-marker? mark))
       (find-previous-reduction-marker mark)
       (re-match-extract-reduction)))

(define (find-next-subproblem-marker mark)
  (and (re-search-forward subproblem-regexp mark (group-end mark))
       (re-match-start 0)))

(define (find-next-reduction-marker mark)
  (and (re-search-forward reduction-regexp mark (group-end mark))
       (re-match-start 0)))

(define (find-next-marker mark)
  (and (re-search-forward marker-regexp mark (group-end mark))
       (re-match-start 0)))

(define (find-previous-subproblem-marker mark)
  (re-search-backward subproblem-regexp mark (group-start mark)))

(define (find-previous-reduction-marker mark)
  (re-search-backward reduction-regexp mark (group-start mark)))

(define (find-previous-marker mark)
  (re-search-backward marker-regexp mark (group-start mark)))

(define (re-match-extract-subproblem)
  (or (re-match-extract-number 1)
      (editor-error "Ill-formed subproblem marker")))

(define (re-match-extract-reduction)
  (or (re-match-extract-number 2)
      (editor-error "Ill-formed reduction marker")))

(define (re-match-extract-reduction-count)
  (re-match-extract-number 3))

(define (re-match-extract-number register-number)
  (let ((start (re-match-start register-number))
	(end (re-match-end register-number)))
    (and start
	 end
	 (string->number (extract-string end start)))))

;;; Regular expressions for finding subproblem and reduction marker
;;; lines.  After a match on REDUCTION-REGEXP, register 1 must match
;;; the subproblem number and register 2 must match the reduction
;;; number.  After a match on SUBPROBLEM-REGEXP, register 1 must
;;; match the subproblem number and register 3 must match the maximum
;;; reduction number in that subproblem.

(define subproblem-regexp
  "^-[CI]- S=\\([0-9]+\\) \\(#R=\\([0-9]+\\)\\|\\)")

(define reduction-regexp
  "^-I- S=\\([0-9]+\\), R=\\([0-9]+\\)")

(define marker-regexp
  "^-[CI]- S=\\([0-9]+\\)\\(, R=[0-9]+\\| #R=[0-9]+\\|\\)")

;;;; Continuation tree

(define (mark-cnode mark)
  (let ((snode (mark-snode mark)))
    (let ((rn (current-reduction-number mark)))
      (or (and rn (ctree-subproblem-nth-reduction snode rn))
	  snode))))

(define (mark-snode mark)
  (or (let ((ctree (mark-ctree mark))
	    (sn (current-subproblem-number mark)))
	(and sn
	     (ctree-nth-subproblem ctree sn)))
      (editor-error "Cannot find subproblem.")))

(define (mark-ctree mark)
  (buffer-ctree (mark-buffer mark)))

(define (count-subproblems buffer)
  (ctree-n-subproblems (buffer-ctree buffer)))

(define (nth-subproblem buffer n)
  (or (ctree-nth-subproblem (buffer-ctree buffer) n)
      (editor-error "No such subproblem" n)))

(define (cnode-evaluation-environment cnode)
  (cond ((ctree-reduction? cnode)
	 (ctree-reduction-environment cnode))
	((ctree-subproblem-has-environment? cnode)
	 (ctree-subproblem-environment cnode))
	(else
	 (evaluation-environment-no-repl))))

(define (display-current-environment printer)
  (let* ((point (current-point))
	 (cnode (mark-cnode point)))
    (call-with-interface-port point
      (lambda (port)
	(cond ((ctree-reduction? cnode)
	       (printer (ctree-reduction-environment cnode) port))
	      ((ctree-subproblem-has-environment? cnode)
	       (printer (ctree-subproblem-environment cnode) port))
	      (else
	       (no-current-environment port)))))))

;;;; Interface Port

(define (call-with-interface-port mark receiver)
  (let ((mark (mark-left-inserting-copy mark)))
    (let ((value (receiver (make-port interface-port-type mark))))
      (mark-temporary! mark)
      value)))

(define (operation/write-char port char)
  (guarantee 8-bit-char? char)
  (region-insert-char! (port/state port) char))

(define (operation/write-substring port string start end)
  (if (string? string)
      (region-insert-substring! (port/state port) string start end)
      (generic-port-operation:write-substring port string start end)))

(define (operation/x-size port)
  (let ((buffer (mark-buffer (port/state port))))
    (and buffer
	 (let ((windows (buffer-windows buffer)))
	   (and (not (null? windows))
		(apply min (map window-x-size windows)))))))

(define (operation/debugger-failure port string)
  port
  (message string)
  (editor-beep))

(define (operation/debugger-message port string)
  port
  (message string))

(define (debugger-presentation port thunk)
  (fresh-line port)
  (fluid-let ((debugger-pp
	       (lambda (expression indentation port)
		 (pretty-print expression port #t indentation))))
    (thunk))
  (newline port)
  (newline port))

(define (operation/prompt-for-expression port prompt)
  port
  (prompt-for-expression prompt))

(define (operation/prompt-for-confirmation port prompt)
  port
  (prompt-for-confirmation? prompt))

(define interface-port-type
  (make-port-type
   `((write-char ,operation/write-char)
     (write-substring ,operation/write-substring)
     (x-size ,operation/x-size)
     (debugger-failure ,operation/debugger-failure)
     (debugger-message ,operation/debugger-message)
     (debugger-presentation ,debugger-presentation)
     (prompt-for-expression ,operation/prompt-for-expression)
     (prompt-for-confirmation ,operation/prompt-for-confirmation))
   #f))