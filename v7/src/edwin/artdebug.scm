;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/artdebug.scm,v 1.13 1991/11/04 20:46:39 cph Exp $
;;;
;;;	Copyright (c) 1989-91 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;
;;; NOTE: Parts of this program (Edwin) were created by translation
;;; from corresponding parts of GNU Emacs.  Users should be aware that
;;; the GNU GENERAL PUBLIC LICENSE may apply to these parts.  A copy
;;; of that license should have been included along with this file.
;;;

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
~arthur/new6001/detect.scm.  Predicate SYSTEM-EXPRESSION? is already
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

(define-variable debugger-confirm-return?
  "True means to prompt for confirmation in RETURN-FROM and RETURN-TO
commands before returning the value."
  true
  boolean?)

(define-variable debugger-split-window?
  "True means use another window for the debugger buffer; false means
use the current window."
  true
  boolean?)

(define-variable debugger-one-at-a-time?
  "True means delete an existing debugger buffer before before
starting a new debugger, ASK means ask the user, and false means
always create a new debugger buffer.  If there is more than one
debugger buffer at the time a new debugger is started, the debugger
will always create a new buffer."
  'ASK
  (lambda (value) (or (boolean? value) (eq? value 'ASK))))

(define-variable debugger-start-on-error?
  "True means always start the debugger on evaluation errors, false
means never start the debugger on errors, and ASK means ask the user
each time."
  'ASK
  (lambda (value) (or (boolean? value) (eq? value 'ASK))))

(define-variable debugger-quit-on-return?
  "True means quit debugger when executing a \"return\" command."
  true
  boolean?)

(define-variable debugger-quit-on-restart?
  "True means quit debugger when executing a \"restart\" command."
  true
  boolean?)

(define-variable debugger-open-markers?
  "True means newlines are inserted between marker lines."
  true
  boolean?)

(define-variable debugger-verbose-mode?
  "True means display extra information without the user requesting it."
  false
  boolean?)

(define-variable debugger-expand-reductions?
  "True says to insert reductions when reduction motion commands are used
in a subproblem whose reductions aren't already inserted."
  true
  boolean?)

(define-variable debugger-max-subproblems
  "Maximum number of subproblems displayed when debugger starts,
or #F meaning no limit."
  3
  (lambda (number)
    (or (not number)
	(and (exact-integer? number)
	     (> number 0)))))

(define-variable debugger-hide-system-code?
  "True means don't show subproblems created by the runtime system."
  true
  boolean?)

(define-variable debugger-show-help-message?
  "True means show a help message in the debugger buffer."
  true
  boolean?)

(define-variable debugger-debug-evaluations?
  "True means evaluation errors in a debugger buffer start new debuggers."
  false
  boolean?)

(define in-debugger? false)
(define in-debugger-evaluation? false)

(define (debug-scheme-error condition error-type-name)
  (if in-debugger?
      (exit-editor-and-signal-error condition)
      (begin
	(editor-beep)
	(if (and (if in-debugger-evaluation?
		     (ref-variable debugger-debug-evaluations?)
		     (ref-variable debugger-start-on-error?))
		 (or (not (eq? (ref-variable debugger-start-on-error?) 'ASK))
		     (prompt-for-confirmation? "Start debugger")))
	    (begin
	      (fluid-let ((in-debugger? true))
		((if (ref-variable debugger-split-window?)
		     select-buffer-other-window
		     select-buffer)
		 (continuation-browser condition)))
	      (message error-type-name " error")))
	(abort-current-command))))

(define-command browse-continuation
  "Invoke the continuation-browser on CONTINUATION."
  "XBrowse Continuation"
  (lambda (continuation)
    (if (not (continuation? continuation)) (editor-error "Not a continuation"))
    (let ((buffer (continuation-browser continuation)))
      ((if (ref-variable debugger-split-window?)
	   select-buffer-other-window
	   select-buffer)
       buffer))))

(define-integrable (buffer-dstate buffer)
  (buffer-get buffer 'DEBUG-STATE))

(define (continuation-browser object)
  (let ((buffer
	 (let ((buffers (find-debugger-buffers)))
	   (if (and (not (null? buffers))
		    (null? (cdr buffers))
		    (let ((one-at-a-time?
			   (ref-variable debugger-one-at-a-time?)))
		      (if (boolean? one-at-a-time?)
			  one-at-a-time?
			  (prompt-for-confirmation?
			   "Another debugger buffer exists.  Delete it"))))
	       (kill-buffer (car buffers)))
	   (new-buffer "*debug*")))
	(dstate (make-initial-dstate object)))
    (set-buffer-major-mode! buffer (ref-mode-object continuation-browser))
    (buffer-put! buffer 'DEBUG-STATE dstate)
    (let ((hide-system-code? (ref-variable debugger-hide-system-code? buffer))
	  (max-subproblems (ref-variable debugger-max-subproblems buffer))
	  (top-subproblem
	   (let ((previous-subproblems (dstate/previous-subproblems dstate)))
	     (if (null? previous-subproblems)
		 (dstate/subproblem dstate)
		 (car (last-pair previous-subproblems))))))
      (with-group-undo-disabled (buffer-group buffer)
	(lambda ()
	  (with-output-to-mark (buffer-start buffer)
	    (lambda ()
	      (let ((port (current-output-port)))
		(if (ref-variable debugger-show-help-message? buffer)
		    (print-help-message buffer port))
		(if (condition? object)
		    (begin
		      (write-string "The error that started the debugger is:"
				    port)
		      (newline port)
		      (write-string "  " port)
		      (write-condition-report object port)
		      (newline port)
		      (newline port)
		      (print-restarts object buffer port))))
	      (case
		  (non-reentrant-call-with-current-continuation
		   (lambda (finish)
		     (let loop ((frame top-subproblem) (level 0))
		       (if (and frame
				(or (not max-subproblems)
				    (< level max-subproblems)
				    (finish 'NOT-ALL-SHOWN)))
			   (with-values
			       (lambda () (stack-frame/debugging-info frame))
			     (lambda (expression environment subexpression)
			       (if (and hide-system-code?
					(system-expression? subexpression))
				   (finish 'NOT-ALL-SHOWN))
			       (newline)
			       (print-subproblem-level level
						       frame
						       expression
						       environment)
			       (loop (stack-frame/next-subproblem frame)
				     (1+ level))))
			   'ALL-SHOWN))))
		((NOT-ALL-SHOWN)
		 (display-more-subproblems-message buffer)))))))
      (let ((point (forward-one-subproblem (buffer-start buffer))))
	(set-buffer-point! buffer point)
	(if (ref-variable debugger-verbose-mode? buffer)
	    (print-subproblem-or-reduction point (debug-dstate point)))
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
  (write-string
   (with-selected-buffer buffer
     (lambda ()
       (substitute-command-keys debugger-help-message)))
   port)
  (newline port)
  (newline port))

(define debugger-help-message
  "This is a debugger buffer:

  Expressions appear one to a line, most recent first.
  Expressions are evaluated in the environment of the line above the point.

  In the marker lines,

    -C- means frame was generated by Compiled code.
    -I- means frame was generated by Interpreted code.

    S=x means frame is in subproblem number x .
    R=y means frame is reduction number y .
    #R=z means there are z reductions in the subproblem;
      use \\[continuation-browser-forward-reduction] to see them.

  \\[continuation-browser-print-subproblem-or-reduction] describes the current subproblem or reduction.
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
	   (with-selected-buffer buffer
	     (lambda ()
	       (substitute-command-keys
		"Use \\[continuation-browser-condition-restart] to invoke any of these restarts.")))
	   port)
	  (newline port)))))

(define (count-subproblems dstate)
  (do ((i 0 (1+ i))
       (subproblem (dstate/subproblem dstate)
		   (stack-frame/next-subproblem subproblem)))
      ((not subproblem) i)))

(define (nth-subproblem buffer n)
  (let ((dstate (buffer-dstate buffer)))
    (let ((top-subproblem
	   (let ((previous-subproblems (dstate/previous-subproblems dstate)))
	     (if (null? previous-subproblems)
		 (dstate/subproblem dstate)
		 (car (last-pair previous-subproblems))))))
      (let next-subproblem ((frame top-subproblem)
			    (level 0))
	(cond ((not frame)
	       (editor-error "No such subproblem" n))
	      ((= level n) frame)
	      (else (next-subproblem (stack-frame/next-subproblem frame)
				     (1+ level))))))))

(define (system-expression? expression)
  expression				;ignore
  #f)

(define (print-reductions mark)
  (let ((frame (dstate/subproblem (debug-dstate mark)))
	(subproblem-number (current-subproblem-number mark)))
    (let ((reductions (stack-frame/reductions frame)))
      (if (pair? reductions)
	  (let next-reduction ((reductions reductions)
			       (reduction-level 0))
	    (if (pair? reductions)
		(begin
		  (newline)
		  (print-reduction-level
		   (car reductions) subproblem-number reduction-level)
		  (next-reduction (cdr reductions) (1+ reduction-level)))))))))

(define (print-history-level compiled? subproblem-number reduction-id thunk)
  (fresh-line)
  (let ((level-identification
	 (string-append (if compiled? "-C- S=" "-I- S=")
			(number->string subproblem-number)
			reduction-id)))
    (let ((pad-width (max 0 (- 78 (string-length level-identification)))))
      (write-string level-identification)
      (write-string
       (string-pad-right
	(string-append
	 (cdr (with-output-to-truncated-string pad-width thunk)) " ")
	pad-width
	#\-)))))

(define (print-subproblem-level subproblem-number frame expression environment)
  (print-history-level
   (stack-frame/compiled-code? frame)
   subproblem-number
   (let ((reductions
	  (improper-list-length (stack-frame/reductions frame))))
     (if (zero? reductions)
	 " -------- "
	 (string-append " #R=" (number->string reductions) " --- ")))
   (cond ((debugging-info/compiled-code? expression)
	  (lambda () (write-string ";compiled code")))
	 ((not (debugging-info/undefined-expression? expression))
	  (lambda ()
	    (fluid-let ((*unparse-primitives-by-name?* true))
	      (write (unsyntax expression)))))
	 ((debugging-info/noise? expression)
	  (lambda ()
	    (write-string ((debugging-info/noise expression) false))))
	 (else
	  (lambda () (write-string ";undefined expression")))))
  (if (ref-variable debugger-verbose-mode?)
      (begin
	(newline)
	(if (environment? environment)
	    (show-environment-name environment)
	    (write-string "There is no environment stored for this frame."))))
  (if (ref-variable debugger-open-markers?)
      (newline)))

(define (print-reduction-level reduction subproblem-number reduction-level)
  (print-history-level
   #f
   subproblem-number
   (string-append ", R=" (number->string reduction-level) " --- ")
   (lambda ()
     (fluid-let ((*unparse-primitives-by-name?* true))
       (write (unsyntax (reduction-expression reduction))))))
  (if (ref-variable debugger-verbose-mode?)
      (let ((environment (reduction-environment reduction)))
	(begin
	  (newline)
	  (if (environment? environment)
	      (show-environment-name environment)
	      (write-string
	       "There is no environment stored for this frame.")))))
  (if (ref-variable debugger-open-markers?)
      (newline)))

;; Regular expressions for finding subproblem and reduction marker
;; lines.  After a match on REDUCTION-REGEXP, register 1 must match
;; the subproblem number and register 2 must match the reduction
;; number.  After a match on SUBPROBLEM-REGEXP, register 1 must match
;; the subproblem number and register 3 must match the maximum
;; reduction number in that subproblem.  The FIND- procedures below
;; use these regexps.

(define reduction-regexp
  "^-I- S=\\([0-9]+\\), R=\\([0-9]+\\)")
(define subproblem-regexp
  "^-[CI]- S=\\([0-9]+\\) \\(#R=\\([0-9]+\\)\\|\\)")
(define subproblem-or-reduction-regexp
  "^-[CI]- S=\\([0-9]+\\)\\(, R=[0-9]+\\| #R=[0-9]+\\|\\)")

(define (region-contains-marker? region)
  (let ((start (line-start (region-start region) 0))
	(end (line-end (region-end region) 0)))
    (or (re-search-forward subproblem-regexp start end)
	(re-search-forward reduction-regexp start end))))

(define (find-next-subproblem-marker mark)
  (let ((found
	 (re-search-forward subproblem-regexp
			    mark
			    (group-end mark))))
    (and found (line-start found 0))))

(define (find-next-reduction-marker mark)
  (let ((found
	 (re-search-forward reduction-regexp
			    mark
			    (group-end mark))))
    (and found (line-start found 0))))

(define (find-next-subproblem-or-reduction-marker mark)
  (let ((found (re-search-forward subproblem-or-reduction-regexp
				  mark
				  (group-end mark))))
    (and found (line-start found 0))))

(define (find-previous-subproblem-marker mark)
  (re-search-backward subproblem-regexp
		      mark
		      (group-start mark)))

(define (find-previous-reduction-marker mark)
  (re-search-backward reduction-regexp
		      mark
		      (group-start mark)))

(define (find-previous-subproblem-or-reduction-marker mark)
  (re-search-backward subproblem-or-reduction-regexp
		      mark
		      (group-start mark)))

(define (end-of-subproblem mark)
  (let ((subproblem-below (find-next-subproblem-marker mark)))
    (if subproblem-below
	(line-end subproblem-below -1)
	(group-end mark))))

(define (re-match-extract-number register-number)
  (let ((start (re-match-start register-number))
	(end (re-match-end register-number)))
    (and start
	 end
	 (string->number (extract-string end start)))))

(define (re-match-extract-subproblem)
  (or (re-match-extract-number 1)
      (editor-error "Bad subproblem marker.")))

(define (re-match-extract-reduction)
  (or (re-match-extract-number 2)
      (editor-error "Bad reduction marker.")))

(define (re-match-extract-reduction-count)
  (re-match-extract-number 3))

(define (current-subproblem-number mark)
  (and (find-previous-subproblem-or-reduction-marker mark)
       (re-match-extract-subproblem)))

(define (current-reduction-number mark)
  (and (not (below-subproblem-marker? mark))
       (begin
	 (find-previous-reduction-marker mark)
	 (re-match-extract-reduction))))

;; Return true whenever expansion is impossible at MARK, even if
;; because MARK is outside any subproblem or because there are no
;; reductions for the subproblem.  If only some of the reductions
;; appear already (e.g. if the others have been deleted by the user),
;; still return true.

(define (reductions-expanded? mark)
  (let ((subproblem-above (find-previous-subproblem-marker mark)))
    (or (not subproblem-above)
	(let ((subproblem-number-above (re-match-extract-subproblem))
	      (reduction-count (re-match-extract-reduction-count)))
	  (and reduction-count
	      (let ((reduction-below
		     (find-next-subproblem-or-reduction-marker
		      (line-end subproblem-above 0))))
		(and reduction-below
		     (= (re-match-extract-subproblem)
			subproblem-number-above))))))))

(define (perhaps-expand-reductions mark)
  (if (and (ref-variable debugger-expand-reductions?)
	   (not (reductions-expanded? mark)))
      (with-output-to-mark (end-of-subproblem mark)
	(lambda ()
	  (message "Expanding reductions...")
	  (print-reductions mark)
	  (temporary-message "Expanding reductions...done")))))

(define (above-subproblem-marker? mark)
  (let ((next-marker
	 (find-next-subproblem-or-reduction-marker mark))
	(next-subproblem (find-next-subproblem-marker mark)))
    (and next-marker
	 (mark= next-marker next-subproblem))))

(define (below-subproblem-marker? mark)
  (let ((previous-marker
	 (find-previous-subproblem-or-reduction-marker mark))
	(previous-subproblem (find-previous-subproblem-marker mark)))
    (and previous-marker
	 (mark= previous-marker previous-subproblem))))

(define (display-more-subproblems-message buffer)
  (with-selected-buffer buffer
    (lambda ()
      (local-set-variable! mode-line-process
			   '(run-light
			     (": more-subproblems " run-light)
			     ": more-subproblems"))))
  (buffer-modeline-event! buffer 'PROCESS-STATUS))

(define (remove-more-subproblems-message buffer)
  (with-selected-buffer buffer
    (lambda ()
	(local-set-variable! mode-line-process
			     (variable-default-value
			      (ref-variable-object mode-line-process)))))
  (buffer-modeline-event! buffer 'PROCESS-STATUS))

(define (forward-one-level start finder)
  (let ((next-level (finder start)))
    (if next-level
	(let ((second-next-level
	       (find-next-subproblem-or-reduction-marker
		(line-end next-level 0))))
	  (if second-next-level
	      (line-end second-next-level -1)
	      (group-end next-level)))
	(let ((buffer (mark-buffer start))
	      (number (current-subproblem-number (group-end start))))
	  (if number
	      (let ((count (count-subproblems (buffer-dstate buffer))))
		(if (< number (-1+ count))
		    (with-output-to-mark
		     (group-end start)
		     (lambda ()
		       (remove-more-subproblems-message buffer)
		       (let ((subproblem (nth-subproblem buffer (1+ number))))
			 (with-values
			     (lambda ()
			       (stack-frame/debugging-info subproblem))
			   (lambda (expression environment subexpression)
			     subexpression
			     (message
			      "Expanding subproblems...")
			     (newline)
			     (print-subproblem-level
			      (1+ number)
			      subproblem
			      expression
			      environment)
			     (temporary-message
			      "Expanding subproblems...done"))))
		       (if (< number (- count 2))
			   (display-more-subproblems-message buffer))
		       (group-end start)))
		    (editor-error "No more subproblems or reductions")))
	      (editor-error "No subproblem or reduction marks"))))))

(define (forward-one-subproblem start)
  (forward-one-level start find-next-subproblem-marker))
(define (forward-one-reduction start)
  (let ((mark (mark-right-inserting-copy start)))
    (perhaps-expand-reductions mark)
    (forward-one-level mark find-next-subproblem-or-reduction-marker)))

(define (backward-one-level start finder)
  (let ((level-top (finder start)))
    (if level-top
	(let ((previous-level (finder level-top)))
	  (if previous-level
	      (line-end level-top -1)
	      (editor-error "Cannot move beyond top level")))
	(editor-error "Cannot move beyond top level"))))

(define (backward-one-subproblem start)
  (backward-one-level start find-previous-subproblem-marker))
(define (backward-one-reduction start)
  (let ((mark (mark-left-inserting-copy start)))
    (if (below-subproblem-marker? mark)
	(let ((previous-subproblem (backward-one-subproblem mark)))
	  (perhaps-expand-reductions previous-subproblem)))
    (backward-one-level mark find-previous-subproblem-or-reduction-marker)))

(define forward-reduction)
(define backward-reduction)
(make-motion-pair forward-one-reduction backward-one-reduction
  (lambda (f b)
    (set! forward-reduction f)
    (set! backward-reduction b)))

(define forward-subproblem)
(define backward-subproblem)
(make-motion-pair forward-one-subproblem backward-one-subproblem
  (lambda (f b)
    (set! forward-subproblem f)
    (set! backward-subproblem b)))

(define (change-subproblem! dstate subproblem-number)
  (let ((finish-move-to-subproblem!
	 (lambda (dstate)
	   (if (and (dstate/using-history? dstate)
		    (positive? (dstate/number-of-reductions dstate)))
	       (change-reduction! dstate 0)
	       (set-dstate/reduction-number! dstate false))))
	(delta (- subproblem-number (dstate/subproblem-number dstate))))
    (if (negative? delta)
	(let ((subproblems
	       (list-tail (dstate/previous-subproblems dstate)
			  (-1+ (- delta)))))
	  (set-current-subproblem! dstate (car subproblems) (cdr subproblems))
	  (finish-move-to-subproblem! dstate))
	(let loop
	    ((subproblem (dstate/subproblem dstate))
	     (subproblems (dstate/previous-subproblems dstate))
	     (delta delta))
	  (if (zero? delta)
	      (begin
		(set-current-subproblem! dstate subproblem subproblems)
		(finish-move-to-subproblem! dstate))
	      (loop (stack-frame/next-subproblem subproblem)
		    (cons subproblem subproblems)
		    (-1+ delta)))))))

(define (change-reduction! dstate reduction-number)
  (set-dstate/reduction-number! dstate reduction-number)
  (set-dstate/environment-list!
   dstate
   (list (reduction-environment (dstate/reduction dstate)))))

;; UGLY BECAUSE IT MUTATES THE DSTATE.

(define (debug-dstate mark)
  (let ((dstate (buffer-dstate (mark-buffer mark))))
    (let ((subproblem-number (current-subproblem-number mark))
	  (reduction-number (current-reduction-number mark)))
      (if subproblem-number
	  (begin (change-subproblem! dstate subproblem-number)
		 (if (and reduction-number
			  (positive? (dstate/number-of-reductions dstate)))
		     (change-reduction! dstate reduction-number)
		     (set-dstate/reduction-number! dstate false))
		 dstate)
	  (editor-error "Cannot find environment for evaluation.")))))

(define (dstate-evaluation-environment dstate)
  (let ((environment-list (dstate/environment-list dstate)))
    (if (and (pair? environment-list)
	     (environment? (car environment-list)))
	(car environment-list)
	(let ((environment (ref-variable scheme-environment)))
	  (if (eq? 'DEFAULT environment)
	      (nearest-repl/environment)
	      (->environment environment))))))

(define (dstate-evaluation-information dstate)
  (values (dstate-evaluation-environment dstate)
	  (stack-frame->continuation (dstate/subproblem dstate))))

(define (debug-evaluation-information mark)
  (let ((dstate (debug-dstate mark)))
    (if dstate
	(dstate-evaluation-information dstate)
	(editor-error "Point must be between frame marker lines"))))

(define (debugger-command-invocation command)
  (lambda ()
    (invoke-debugger-command command (current-point))))

(define (invoke-debugger-command command mark)
  (with-debugger-hooks mark
    (lambda ()
      (command (debug-dstate mark)))))

(define (with-debugger-hooks mark thunk)
  (fluid-let ((hook/prompt-for-confirmation
	       (lambda (cmdl prompt)
		 cmdl			;ignore
		 (prompt-for-confirmation prompt)))
	      (hook/prompt-for-expression
	       (lambda (cmdl prompt)
		 cmdl			;ignore
		 (prompt-for-expression prompt)))
	      (hook/debugger-failure
	       (lambda (string)
		 (message string)
		 (editor-beep)))
	      (hook/debugger-message message)
	      (hook/presentation
	       (lambda (thunk)
		 (edwin-debugger-presentation mark thunk))))
    (thunk)))

(define (edwin-debugger-presentation mark thunk)
  (with-output-to-mark mark
    (lambda ()
      (fresh-line)
      (fluid-let ((debugger-pp
		   (lambda (expression indentation)
		     (pretty-print expression
				   (current-output-port)
				   true
				   indentation))))
	(thunk))
      (newline)
      (newline))))

(define (continuation-browser-start-eval region)
  (fluid-let ((in-debugger-evaluation? true))
    (if (region-contains-marker? region)
	(editor-error "Cannot evaluate a region that contains markers.")
	(let ((end (region-end region)))
	  (set-buffer-point! (mark-buffer end) end)
	  (debug-evaluation-information (region-start region))))))

(define (continuation-browser-evaluate-region/static region)
  (with-values (lambda () (continuation-browser-start-eval region))
    (lambda (environment continuation)
      continuation			;ignored
      (evaluate-region region environment))))

(define (continuation-browser-evaluate-region/dynamic region)
  (with-values (lambda () (continuation-browser-start-eval region))
    (lambda (environment continuation)
      (let ((repl-eval hook/repl-eval))
	(fluid-let
	    ((hook/repl-eval
	      (lambda (repl sexp env syntax-table)
		(let ((unique (cons 'unique 'id)))
		  (let ((result
			 (call-with-current-continuation
			  (lambda (new-continuation)
			    (within-continuation
				continuation
			      (lambda ()
				(bind-condition-handler
				    '()
				    (lambda (condition)
				      (new-continuation
				       (cons unique condition)))
				  (lambda ()
				    (new-continuation
				     (repl-eval repl
						sexp
						env
						syntax-table))))))))))
		    (if (and (pair? result)
			     (eq? unique (car result)))
			(error (cdr result))
			result))))))
	  (evaluate-region region environment))))))

(define (continuation-browser-evaluate-from-mark input-mark)
  (continuation-browser-evaluate-region/static
   (make-region input-mark (forward-sexp input-mark 1 'ERROR))))

(define-command continuation-browser-eval-last-expression/static
  "Evaluate the expression before the point."
  ()
  (lambda ()
    (continuation-browser-evaluate-from-mark
     (backward-sexp (current-point) 1))))

(define-command continuation-browser-eval-last-expression/dynamic
  "Evaluate the expression before the point in the dynamic state of the
continuation of the current frame."
  ()
  (lambda ()
    (let ((input-mark (backward-sexp (current-point) 1)))
      (continuation-browser-evaluate-region/dynamic
       (make-region input-mark
		    (forward-sexp input-mark 1 'ERROR))))))

(define-command continuation-browser-eval-region
  "Evaluate the expressions in the region.  Give an error if the
region includes part of any subproblem or reduction marker."
  "r"
  (lambda (region)
    (continuation-browser-evaluate-region/static region)))

(define-command continuation-browser-eval-definition
  "Evaluate the definition the point is in or before."
  ()
  (lambda ()
    (continuation-browser-evaluate-from-mark (current-definition-start))))

(define (print-subproblem-or-reduction mark dstate)
  (edwin-debugger-presentation mark
   (lambda ()
     (if (dstate/reduction-number dstate)
	 (print-reduction-expression (dstate/reduction dstate))
	 (print-subproblem-expression dstate)))))

(define (identify-environment dstate)
  (let ((environment-list (dstate/environment-list dstate)))
    (if (pair? environment-list)
	(print-environment (car environment-list))
	(begin (newline)
	       (write-string "There is no current environment.")))))

(define-command continuation-browser-print-environment
  "Identify the environment of the current frame."
  ()
  (lambda ()
    (let ((cp (current-point)))
      (edwin-debugger-presentation
       cp
       (lambda ()
	 (identify-environment (debug-dstate cp)))))))

(define-command continuation-browser-print-subproblem-or-reduction
  "Print the current subproblem or reduction in the standard format."
  ()
  (lambda ()
    (let ((cp (current-point)))
      (print-subproblem-or-reduction cp (debug-dstate cp)))))

(define-command continuation-browser-print-expression
  "Pretty print the current expression."
  ()
  (debugger-command-invocation command/print-expression))

(define-command continuation-browser-print-environment-procedure
  "Pretty print the procedure that created the current environment."
  ()
  (debugger-command-invocation command/print-environment-procedure))

(define-command continuation-browser-expand-reductions
  "Expand all the reductions of the current subproblem.  If already
expanded, move the point to one of the reductions."
  ()
  (lambda ()
    (let ((cp (current-point)))
      (if (reductions-expanded? cp)
	  (temporary-message
	   "Reductions for this subproblem already expanded.")
	  (with-output-to-mark
	    cp
	    (lambda ()
	      (print-reductions (current-point))))))))

(define-command continuation-browser-go-to
  "Move to an arbitrary subproblem.  Prompt for the subproblem number
if not given as an argument.  Move to the last subproblem if the
subproblem number is too high."
  "NSubproblem number"
  (lambda (destination-subproblem-number)
    (let ((end (group-end (current-point)))
	  (not-found
	   (lambda ()
	     (editor-error "Cannot find subproblem"
			   destination-subproblem-number))))
      (let ((last-subproblem-number (current-subproblem-number end)))
	(if last-subproblem-number
	    (set-buffer-point!
	     (current-buffer)
	     (cond ((< destination-subproblem-number last-subproblem-number)
		    (let loop ((point (backward-subproblem end 1)))
		      (if point
			  (let ((subproblem (current-subproblem-number point)))
			    (if subproblem
				(if (= subproblem
				       destination-subproblem-number)
				    point
				    (loop (backward-subproblem point 1)))
				(not-found)))
			  (not-found))))
		   ((> destination-subproblem-number last-subproblem-number)
		    (forward-subproblem
		     end
		     (- destination-subproblem-number last-subproblem-number)
		     'limit))
		   (else end)))
	    (not-found))))))

(define-command continuation-browser-expand-subproblems
  "Expand all subproblems, or ARG more subproblems if argument is given."
  "P"
  (lambda (argument)
    (let ((subproblem-number
	   (if argument
	       (let ((number
		      (current-subproblem-number
		       (group-end (current-point)))))
		 (if number
		     (+ number (command-argument-numeric-value argument))
		     (editor-error "Cannot find subproblem marker.")))
	       (-1+ (count-subproblems
		     (buffer-dstate (current-buffer)))))))
      (let ((point (mark-right-inserting-copy (current-point))))
	((ref-command continuation-browser-go-to) subproblem-number)
	(set-current-point! point)))))

;; The subproblem and reduction motion commands rely, in many places,
;; on the assumption that subproblem and reduction numbers increase
;; downward in the buffer, and that no subproblem/reduction marker
;; line is repeated.  Of course, the user can violate this assumption
;; by constructing or copying a marker, but the program is robust with
;; respect to such conniving, as long as the reduction and subproblem
;; specified by the numbers in the marker exist.  The only time it
;; should be possible to notice an effect of this assumption is when a
;; reduction or subproblem that is already displayed is automatically
;; redisplayed because the existing one appeared out of order.

(define-command continuation-browser-forward-reduction
  "Move one or more reductions forward.
Display reductions that exist but are not yet displayed.  If there are
no more reductions for the current subproblem, move to the first
reduction shown in the next subproblem."
  "p"
  (lambda (argument)
    (move-thing forward-reduction argument)))

(define-command continuation-browser-forward-subproblem
  "Move one or more subproblems forward."
  "p"
  (lambda (argument)
    (move-thing forward-subproblem argument)))

(define-command continuation-browser-backward-reduction
  "Move one or more reductions backward.
Display reductions that exist but are not yet displayed.  If there are
no more reductions for the current subproblem, move to the last
reduction shown in the previous subproblem."
  "p"
  (lambda (argument)
    (move-thing backward-reduction argument)))

(define-command continuation-browser-backward-subproblem
  "Move one or more subproblems backward."
  "p"
  (lambda (argument)
    (move-thing backward-subproblem argument)))

(define (show-frame environment depth brief?)
  (show-environment-name environment)
  (if (not (negative? depth))
      (begin (newline)
	     (write-string "Depth (relative to initial environment): ")
	     (write depth)))
  (if (not (and (environment->package environment) brief?))
      (begin
	(newline)
	(show-environment-bindings environment brief?))))

(define (show-current-frame dstate brief?)
  (edwin-debugger-presentation
   (current-point)
   (lambda ()
     (let ((environment-list (dstate/environment-list dstate)))
       (show-frame (car environment-list)
		   (length (cdr environment-list))
		   brief?)))))

(define (show-frames environment depth)
  (edwin-debugger-presentation
   (current-point)
   (lambda ()
     (let loop ((environment environment) (depth depth))
       (write-string "----------------------------------------")
       (newline)
       (show-frame environment depth true)
       (if (eq? true (environment-has-parent? environment))
	   (begin
	     (newline)
	     (newline)
	     (loop (environment-parent environment) (1+ depth))))))))

(define-command continuation-browser-show-current-frame
  "Print the bindings of the current frame of the current environment."
  ()
  (lambda ()
    (show-current-frame (debug-dstate (current-point)) false)))

(define-command continuation-browser-show-all-frames
  "Print the bindings of all frames of the current environment."
  ()
  (debugger-command-invocation command/show-all-frames))

(define (subproblem-enter subproblem value avoid-deletion?)
  (if (or (not (ref-variable debugger-confirm-return?))
	  (prompt-for-confirmation? "Continue with this value"))
      (begin
	(if (and (not avoid-deletion?)
		 (ref-variable debugger-quit-on-return?))
	    (kill-buffer-interactive (current-buffer)))
	((stack-frame->continuation subproblem)
	 value))))

(define (guarantee-next-subproblem dstate)
  (or (stack-frame/next-subproblem (dstate/subproblem dstate))
      (editor-error "Can't continue.")))

(define-command continuation-browser-retry
  "Retry the offending expression, returning from the current
subproblem with its value.
Prefix argument means do not kill the debugger buffer."
  "P"
  (lambda (avoid-deletion?)
    (let* ((dstate (debug-dstate (current-point)))
	   (next (guarantee-next-subproblem dstate)))
      (subproblem-enter
       next
       (let ((expression (dstate/expression dstate)))
	 (if (invalid-expression? expression)
	     (editor-error "Cannot retry; invalid expression."
			   expression)
	     (extended-scode-eval
	      expression
	      (dstate-evaluation-environment dstate))))
       avoid-deletion?))))

(define-command continuation-browser-return-from
  "Return FROM the current subproblem with a value.
Invoke the continuation that is waiting for the value of the current
subproblem on the value of the expression before the point.
Prefix argument means do not kill the debugger buffer."
  "P"
  (lambda (avoid-deletion?)
    (let ((next
	   (guarantee-next-subproblem
	    (debug-dstate (current-point)))))
      (subproblem-enter
       next
       (continuation-browser-evaluate-from-mark
	(backward-sexp (current-point) 1))
       avoid-deletion?))))

(define-command continuation-browser-return-to
  "Return TO the current subproblem with a value.
Invoke the continuation corresponding to this subproblem on the value
of the expression before the point.
Prefix argument means do not kill the debugger buffer."
  "P"
  (lambda (avoid-deletion?)
    (let ((subproblem (dstate/subproblem (debug-dstate (current-point)))))
      (subproblem-enter subproblem
			(continuation-browser-evaluate-from-mark
			 (backward-sexp (current-point) 1))
			avoid-deletion?))))

(define-command continuation-browser-frame
  "Show the current subproblem's stack frame in internal format."
  ()
  (debugger-command-invocation command/frame))

(define-command continuation-browser-condition-restart
  "Continue the program using a standard restart option.
Prefix argument means do not kill the debugger buffer."
  "P"
  (lambda (avoid-deletion?)
    (fluid-let ((hook/before-restart
		 (lambda ()
		   (if (and (not avoid-deletion?)
			    (ref-variable debugger-quit-on-restart?))
		       (kill-buffer-interactive (current-buffer))))))
      (invoke-debugger-command command/condition-restart (current-point)))))

(define-major-mode continuation-browser scheme "Debug"
  "Major mode for debugging Scheme programs and browsing Scheme continuations.
Editing and evaluation commands are similar to those of Scheme Interaction mode.

  Expressions appear one to a line, most recent first.  Expressions
  are evaluated in the environment of the line above the point.

  In the marker lines,

    -C- means frame was generated by Compiled code
    -I- means frame was generated by Interpreted code

    S=x means frame is in subproblem number x
    R=y means frame is reduction number y
    #R=z means there are z reductions in the subproblem
      Use \\[continuation-browser-forward-reduction] to see them

Evaluate expressions

  \\[continuation-browser-eval-last-expression/static] evaluates the expression preceding the point in the
    environment of the current frame.
  \\[continuation-browser-eval-last-expression/dynamic] evaluates the expression preceding the point in the
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
  \\[continuation-browser-print-subproblem-or-reduction] describes the current subproblem or reduction.
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
  (local-set-variable! enable-transcript-buffer true)
  (local-set-variable! transcript-buffer-name (current-buffer))
  (local-set-variable! transcript-buffer-mode
		       (ref-mode-object continuation-browser))
  (local-set-variable! transcript-input-recorder
		       scheme-interaction-input-recorder)
  (local-set-variable! transcript-output-wrapper
		       scheme-interaction-output-wrapper)
  (local-set-variable! comint-input-ring
		       (make-ring (ref-variable comint-input-ring-size)))
  (local-set-variable! transcript-output-wrapper
		       debug-interaction-output-wrapper))

(define (debug-interaction-output-wrapper thunk)
  (with-output-to-current-point
   (lambda ()
     (intercept-^G-interrupts
      (lambda ()
	(fresh-line)
	(write-string ";Abort!")
	(fresh-lines 2)
	(^G-signal))
      thunk))))

;; Disable EVAL-CURRENT-BUFFER in Debugger Mode; it is inherited from
;; Scheme mode but does not make sense here:

(define-key 'continuation-browser #\M-o
  (ref-command-object undefined))

;; Evaluation

(define-key 'continuation-browser '(#\C-x #\C-e)
  'continuation-browser-eval-last-expression/static)
(define-key 'continuation-browser '(#\C-x #\C-r)
  'continuation-browser-eval-last-expression/dynamic)
(define-key 'continuation-browser #\M-z
  'continuation-browser-eval-definition)
(define-key 'continuation-browser '(#\M-C-z)
  'continuation-browser-eval-region)

;; Comint history

(define-key 'continuation-browser #\M-p
  'comint-previous-input)
(define-key 'continuation-browser #\M-n
  'comint-next-input)

(define-key 'continuation-browser '(#\C-c #\C-r)
  'comint-history-search-backward)
(define-key 'continuation-browser '(#\C-c #\C-s)
  'comint-history-search-forward)

;; Subproblem/reduction motion

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

;; Information display

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
  'continuation-browser-print-subproblem-or-reduction)
(define-key 'continuation-browser '(#\C-c #\C-x)
  'continuation-browser-expand-subproblems)
(define-key 'continuation-browser '(#\C-c #\C-y)
  'continuation-browser-frame)

;; Miscellany

(define-key 'continuation-browser '(#\C-c #\C-k)
  'continuation-browser-condition-restart)
(define-key 'continuation-browser '(#\C-c #\C-j)
  'continuation-browser-return-to)
(define-key 'continuation-browser '(#\C-c #\C-z)
  'continuation-browser-return-from)
(define-key 'continuation-browser '(#\C-c #\C-d)
  'continuation-browser-retry)