;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/artdebug.scm,v 1.7 1991/07/19 00:38:18 arthur Exp $
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

#| TO DO

Make environment browsing mode; the debugger mode can be a superset of
that mode: Add optional marker lines for environments.  If you do the
C-c A command to describe the environment frames in the current
subproblem or reduction, the debugger should use the correct
environment when you do evaluations in those environment frames.  Make
commands for moving by environment level.  Later, change this to
execute Where in another buffer depending on the state of a flag.

Make C-c k evaluate in the environment in which the error occurred.
Otherwise, the "Define x to a given value" restart for unbound
variable errors won't work.

Make C-c k and C-c z not get confused about where to finish their
output if you evaluate buggy code in *scratch*, causing Debug to fire,
then restart or return when buffer *foo* is the next buffer instead of
*scratch*.  Currently, this causes output intended for *foo* to go to
*scratch*.

Make C-c z, if given a argument, use the value resulting from the
previous evaluation instead of prompting for a value.

Make C-c z work in the case where an error happens during evaluation
of the return expression, the debugger starts on the new error, and
return is done from the second debugger straight through the first
back into the original computation.  The restart itself works, but the
message "Scheme error" is printed upon starting the second debugger.

Make a way to restrict the possible restarts to not include restarts
that could stop Edwin.

Make reductions display "-I-" and "-C-" appropriately.

MORE-SUBPROBLEMS-MESSAGE doesn't work quite right when auto-expanding
subproblems with DEBUGGER-OPEN-MARKERS? false; it leaves extra space.

By default, when the debugger starts, don't show history levels inside
the system.  To detect system code to ignore it in the debugger:

  (define (make-dummy-thunk value)
    (lambda () value))

  (define (with-stack-mark thunk mark-value)
    (let ((dummy (make-dummy-thunk mark-value)))
      (dynamic-wind dummy thunk dummy)))

  Look for the DYNAMIC-WIND on the stack.

  Define CLOSURE/LAST-VARIABLE in $sr/uproc.scm.  It should do

    (system-vector-ref
     (-1+
      (system-vector-length
       (compiled-code-address->block closure))))

Make a narrow interface between Edwin and the debugger so it will be
easy to write this debugger for Emacs.

Perhaps indent everything except level separator lines.

Number input lines so that it is possible to tell what order you
evaluated your expressions.  This could be particularly useful for
TA's looking over students' shoulders.

Once outline mode has been written for Edwin, add commands to expand
and contract subproblems and reductions.

|#

(declare (usual-integrations))

(define-variable debugger-quit-on-return?
  "True iff debugger should automatically quit when it executing a
\"return\" command."
  true
  boolean?)

(define-variable debugger-quit-on-restart?
  "True iff debugger should automatically quit when it executing a
\"restart\" command."
  true
  boolean?)

(define-variable debugger-open-markers?
  "True iff debugger should automatically insert newlines between reduction and
subproblem marker lines."
  true
  boolean?)

(define-variable debugger-verbose-mode?
  "True iff debugger should display extra information without the user requesting
it."
  true
  boolean?)

(define-variable debugger-automatically-expand-reductions?
  "True iff debugger should automatically insert reductions when reduction motion
commands are used in a subproblem where reductions don't already appear."
  true
  boolean?)

(define-variable debugger-max-subproblems
  "Maximum number of subproblems displayed when debugger starts, or false if
there is no limit."
  10
  (lambda (number)
    (or (not number)
	(and (exact-nonnegative-integer? number)
	     (positive? number)))))

(define-variable debugger-hide-system-code?
  "The debugger will, on startup, show subproblems in system code only
if this variable is false."
  true
  boolean?)

(define-variable debugger-show-help-message?
  "The debugger will include a help message in its buffer only if this
variable is true."
  true
  boolean?)

(define in-debugger? false)
(define in-debugger-evaluation? false)

(define-variable debugger-debug-evaluations?
  "True iff evaluation errors in the debugger buffer should start new debuggers."
  false
  boolean?)

(define (debug-scheme-error condition)
  (cond (in-debugger?
	 (exit-editor-and-signal-error condition))
	((and in-debugger-evaluation?
	      (not (ref-variable debugger-debug-evaluations?)))
	 (%editor-error))
	(else
	 (fluid-let ((in-debugger? true))
	   (let ((buffer (continuation-browser condition)))
	     (select-buffer buffer)
	     (if (ref-variable debugger-show-help-message?)
		 (with-output-to-mark
		   (buffer-start buffer)
		   (lambda ()
		     (with-group-undo-disabled
		       (buffer-group buffer)
		       (lambda ()
			 (write-string
			  (substitute-command-keys
			   "This is a debugger buffer:

  Subproblems and reductions are marked with lines of dashes.  Any
    evaluations you do when the point is between the ----- lines for
    one subproblem or reduction level will happen in the environment
    of that level, if possible.
  The subproblem number appears before the comma.  The reduction
    number (or range of reduction numbers in the subproblem) appears
    after the comma.
  Type \\[continuation-browser-print-subproblem-or-reduction] for a description of the current subproblem or reduction.
  Type \\[continuation-browser-quit] when you are finished using the debugger.
  Type \\[describe-mode] for information on debugger commands.

The error that started the debugger is:
"))
			 (write-condition-report condition (current-output-port))
			 (newline)
			 (buffer-not-modified! buffer)))))))))))

(define-command browse-continuation
  "Invoke the continuation-browser on CONTINUATION."
  "XBrowse Continuation"
  (lambda (continuation)
    (if (not (continuation? continuation)) (editor-error "Not a continuation"))
    (let ((buffer (continuation-browser continuation)))
      (select-buffer buffer))))

(define-integrable (buffer-dstate buffer)
  (buffer-get buffer 'DEBUG-STATE))

(define more-subproblems-message "\nThere are more subproblems below this one.")

(define (continuation-browser object)
  (message "Starting debugger...")
  (let ((buffer (new-buffer "*debug*"))
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
      (with-group-undo-disabled
	(buffer-group buffer)
	(lambda ()
	  (with-output-to-mark (buffer-start buffer)
	    (lambda ()
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
			       subexpression
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
		 (display more-subproblems-message)))))))
      (let ((point (forward-one-subproblem (buffer-start buffer))))
	(set-buffer-point! buffer point)
	(if (ref-variable debugger-verbose-mode? buffer)
	    ;(print-subproblem-or-reduction (current-point) (debug-dstate (current-point)))
	    (invoke-debugger-command command/print-subproblem-or-reduction point)
	    )
	(push-buffer-mark! buffer point)
	(buffer-not-modified! buffer)
	(temporary-message "Starting debugger...done")
	buffer))))

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
  #f)

(define (print-reductions mark)
  (let ((frame (dstate/subproblem (debug-dstate mark)))
	(subproblem-number (current-subproblem-number mark)))
    (let ((reductions (stack-frame/reductions frame)))
      (if (pair? reductions)
	  (let next-reduction ((reductions (cdr reductions))
			       (reduction-level 1))
	    (if (pair? reductions)
		(begin
		  (newline)
		  (print-reduction-level
		   (car reductions) subproblem-number reduction-level)
		  (next-reduction (cdr reductions) (1+ reduction-level)))))))))

(define compiled-marker "-C- ")
(define interpreted-marker "-I- ")
(define no-marker "--- ")		;THIS SHOULD NOT BE NEEDED!

(define (print-history-level compiled? subproblem-number reduction-id string)
  (let ((level-identification
	 (string-append (case compiled?
			  ((unknown) no-marker)
			  ((#t) compiled-marker)
			  (else interpreted-marker))
			(number->string subproblem-number)
			", "
			reduction-id)))
    (let ((pad-width (max 0 (- 74 (string-length level-identification)))))
      (write-string level-identification)
      (write-string " --- ")
      (write-string (string-pad-right (string-append string " ") pad-width #\-)))))

(define (max-reduction-number frame)
  (max 0 (-1+ (improper-list-length (stack-frame/reductions frame)))))

(define (print-subproblem-level subproblem-number frame expression environment)
  (print-history-level
   (stack-frame/compiled-code? frame)
   subproblem-number
   (string-append "0-" (number->string (max-reduction-number frame)))
   (cond ((debugging-info/compiled-code? expression)
	  ";compiled code")
	 ((not (debugging-info/undefined-expression? expression))
	  (output-to-string
	   57
	   (lambda ()
	     (fluid-let ((*unparse-primitives-by-name?* true))
	       (write (unsyntax expression))))))
	 ((debugging-info/noise? expression)
	  (output-to-string
	   57
	   (lambda ()
	     (write-string ((debugging-info/noise expression) false)))))
	 (else
	  ";undefined expression")))
  (if (ref-variable debugger-verbose-mode?)
      (begin (newline)
	     (if (environment? environment)
		 (show-environment-name environment)
		 (write-string "There is no environment stored for this frame."))))
  (if (ref-variable debugger-open-markers?)
      (newline)))

(define (print-reduction-level reduction subproblem-number reduction-level)
  (print-history-level
   'unknown				;SHOULD KNOW!
   subproblem-number
   (number->string reduction-level)
   (output-to-string
    60
    (lambda ()
      (fluid-let ((*unparse-primitives-by-name?* true))
	(write (unsyntax (reduction-expression reduction)))))))
  (if (ref-variable debugger-verbose-mode?)
      (let ((environment (reduction-environment reduction)))
	(begin
	  (newline)
	  (if (environment? environment)
	      (show-environment-name environment)
	      (write-string "There is no environment stored for this frame.")))))
  (if (ref-variable debugger-open-markers?)
      (newline)))

;; Regular expressions for finding subproblem and reduction markers.
;; REDUCTION-REGEXP must match anything that SUBPROBLEM-REGEXP
;; matches.  After a match on REDUCTION-REGEXP, register 1 must match
;; the subproblem number and register 2 must match the reduction
;; number; register 3 doesn't matter.  After a match on
;; SUBPROBLEM-REGEXP, register 1 must match the subproblem number and
;; register 2 must match the maximum reduction number.  The FIND-
;; procedures below must use these regexps.

(define reduction-regexp
  "^-[---CI]- \\([0-9]+\\), \\([0-9]\\)\\(-[0-9]+\\|\\)")
(define subproblem-regexp
  "^-[---CI]- \\([0-9]+\\), 0-\\([0-9]+\\)")

(define (find-next-subproblem-marker point)
  (let ((found
	 (re-search-forward subproblem-regexp
			    point
			    (buffer-end (mark-buffer point)))))
    (and found (line-start found 0))))

(define (find-next-reduction-marker point)
  (let ((found
	 (re-search-forward reduction-regexp
			    point
			    (buffer-end (mark-buffer point)))))
    (and found (line-start found 0))))

(define (find-previous-subproblem-marker point)
  (re-search-backward subproblem-regexp
		      point
		      (buffer-start (mark-buffer point))))

(define (find-previous-reduction-marker point)
  (re-search-backward reduction-regexp
		      point
		      (buffer-start (mark-buffer point))))

(define (end-of-subproblem mark)
  (let ((subproblem-below (find-next-subproblem-marker mark)))
    (if subproblem-below
	(line-end subproblem-below -1)
	(buffer-end (mark-buffer mark)))))

(define (re-match-extract-number register-number)
  (string->number (extract-string (re-match-end register-number)
				  (re-match-start register-number))))

;; Return true whenever expansion is impossible at MARK, even if
;; because MARK is outside any subproblem or because there are no
;; reductions for the subproblem.  If only some of the reductions
;; appear already (e.g. if the others have been deleted by the user),
;; still return true.

(define (reductions-expanded? mark)
  (let ((subproblem-above (find-previous-subproblem-marker mark)))
    (or (not subproblem-above)
	(let ((subproblem-number-above (re-match-extract-number 1))
	      (max-reduction-number (re-match-extract-number 2)))
	  (or (zero? max-reduction-number)
	      (let ((reduction-below
		     (find-next-reduction-marker
		      (line-end subproblem-above 0))))
		(and reduction-below
		     (= (re-match-extract-number 1)
			subproblem-number-above))))))))

(define (perhaps-expand-reductions mark)
  (if (and (ref-variable debugger-automatically-expand-reductions?)
	   (not (reductions-expanded? mark)))
      (with-output-to-mark (end-of-subproblem mark)
	(lambda ()
	  (message "Automatically expanding reductions...")
	  (print-reductions mark)
	  (temporary-message "Automatically expanding reductions...done")))))

(define (above-subproblem-boundary? mark)
  (let ((next-reduction (find-next-reduction-marker mark))
	(next-subproblem (find-next-subproblem-marker mark)))
    (and next-reduction
	 (mark= next-reduction next-subproblem))))

(define (below-subproblem-boundary? mark)
  (let ((previous-reduction (find-previous-reduction-marker mark))
	(previous-subproblem (find-previous-subproblem-marker mark)))
    (and previous-reduction
	 (mark= previous-reduction previous-subproblem))))

(define (remove-more-subproblems-message start)
  (let ((found
	 (search-forward more-subproblems-message
			 start
			 (buffer-end (mark-buffer start))
			 #t)))
    (and found
	 (delete-string (re-match-start 0)
			(re-match-end 0)))))

(define (forward-one-level start finder)
  (let ((next-level (finder start)))
    (if next-level
	(let ((second-next-level
	       (find-next-reduction-marker (line-end next-level 0))))
	  (if second-next-level
	      (line-end second-next-level -1)
	      (buffer-end (mark-buffer next-level))))
	(let* ((buffer (mark-buffer start))
	       (buf-end (buffer-end buffer))
	       (number (or (current-subproblem-number start)
			   (current-subproblem-number (buffer-end buffer)))))
	  (if number
	      (let ((count (count-subproblems (buffer-dstate buffer))))
		(if (< number (-1+ count))
		    (with-output-to-mark (buffer-end buffer)
		      (lambda ()
			(remove-more-subproblems-message
			 (find-previous-subproblem-marker buf-end))
			(fresh-line)
			(newline)
			(let ((subproblem (nth-subproblem buffer (1+ number))))
			  (with-values
			      (lambda () (stack-frame/debugging-info subproblem))
			    (lambda (expression environment subexpression)
			      subexpression
			      (message "Automatically expanding subproblems...")
			      (print-subproblem-level
			       (1+ number)
			       subproblem
			       expression
			       environment)
			      (temporary-message
			       "Automatically expanding subproblems...done"))))
			(if (< number (- count 2))
			    (display more-subproblems-message))
			(buffer-end buffer)))
		    (editor-error "No more subproblems or reductions")))
	      (editor-error "No subproblem or reduction marks"))))))

(define (forward-one-subproblem start)
  (forward-one-level start find-next-subproblem-marker))
(define (forward-one-reduction start)
  (let ((mark (mark-right-inserting-copy start)))
    (perhaps-expand-reductions mark)
    (forward-one-level mark find-next-reduction-marker)))

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
    (if (below-subproblem-boundary? mark)
	(let ((previous-subproblem (backward-one-subproblem mark)))
	  (perhaps-expand-reductions previous-subproblem)))
    (backward-one-level mark find-previous-reduction-marker)))

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

(define (current-subproblem-number mark)
  (and (find-previous-reduction-marker mark)
       (re-match-extract-number 1)))

(define (current-reduction-number mark)
  (and (find-previous-reduction-marker mark)
       (re-match-extract-number 2)))

(define (current-subproblem-and-reduction-numbers mark)
  (and (find-previous-reduction-marker mark)
       (values (re-match-extract-number 1)
	       (re-match-extract-number 2))))

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
    (let ((marker-numbers (current-subproblem-and-reduction-numbers mark)))
      (and marker-numbers
	   (with-values (lambda () marker-numbers)
	     (lambda (subproblem-number reduction-number)
	       (change-subproblem! dstate subproblem-number)
	       (if (positive? (dstate/number-of-reductions dstate))
		 (change-reduction! dstate reduction-number)
		 (set-dstate/reduction-number! dstate false))
	     dstate))))))

(define (debug-evaluation-environment mark)
  (let ((dstate (debug-dstate mark)))
    (if dstate
	(let ((environment-list (dstate/environment-list dstate)))
	  (if (and (pair? environment-list)
		   (environment? (car environment-list)))
	      (car environment-list)
	      (let ((environment (ref-variable scheme-environment)))
		(if (eq? 'DEFAULT environment)
		    (nearest-repl/environment)
		    (->environment environment)))))
	(editor-error "Point must be between frame markers (\"------\")"))))

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

(define-command continuation-browser-evaluate-previous-expression
  "Evaluate the expression before the point."
  ()
  (lambda ()
    (let ((cp (current-point)))
      (let* ((region (make-region (backward-sexp cp 1) cp))
	     (expression (with-input-from-region region read)))
	(fluid-let ((in-debugger-evaluation? true))
	  (editor-eval expression
		       (debug-evaluation-environment cp)))))))

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
	  (temporary-message "Reductions for this subproblem already expanded.")
	  (with-output-to-mark
	    cp
	    (lambda ()
	      (print-reductions (current-point))))))))

(define-command continuation-browser-goto
  "Move to an arbitrary subproblem.
Prompt for the subproblem number if not given as an argument."
  "NSubproblem number"
  (lambda (subproblem-number)
    (let* ((buffer (current-buffer))
	   (max-subproblem-number
	    (-1+ (count-subproblems (buffer-dstate buffer)))))
      (if (and (exact-nonnegative-integer? subproblem-number)
	       (<= subproblem-number max-subproblem-number))
	  (set-buffer-point!
	   buffer
	   (forward-subproblem (buffer-start buffer)
			       (1+ subproblem-number)))
	  (editor-error "Subproblem number must be an integer between 0 and "
			max-subproblem-number)))))

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

(define-command continuation-browser-show-current-frame
  "Print the bindings of the current frame of the current environment."
  ()
  (debugger-command-invocation command/show-current-frame))

(define-command continuation-browser-show-all-frames
  "Print the bindings of all frames of the current environment."
  ()
  (debugger-command-invocation command/show-all-frames))

(define-command continuation-browser-quit
  "Kill the current continuation browser."
  ()
  (lambda ()
    (kill-buffer-interactive (current-buffer))))

(define-command continuation-browser-return
  "Invoke the continuation that is the current subproblem.
Prompts for a value to give the continuation as an argument."
  ()
  (lambda ()
    (fluid-let ((hook/debugger-before-return
		 (lambda ()
		   (if (ref-variable debugger-quit-on-return?)
		       (kill-buffer-interactive (current-buffer))))))
      (invoke-debugger-command command/return (current-point)))))

(define-command continuation-browser-frame
  "Show the current subproblem's stack frame in internal format."
  ()
  (debugger-command-invocation command/frame))

(define-command continuation-browser-condition-restart
  "Continue the program using a standard restart option."
  ()
  (lambda ()
    (fluid-let ((hook/before-restart
		 (lambda ()
		   (if (ref-variable debugger-quit-on-restart?)
		       (kill-buffer-interactive (current-buffer))))))
      (invoke-debugger-command command/condition-restart (current-point)))))

(define-major-mode continuation-browser scheme "Debug"
  "You are in the Scheme debugger, where you can do the following:

Evaluate expressions

  \\[continuation-browser-evaluate-previous-expression] evaluates the expression preceding the point in the
  environment of the current frame.

Move between subproblems and reductions

  \\[continuation-browser-forward-reduction] moves forward one reduction (earlier in time).
  \\[continuation-browser-backward-reduction] moves backward one reduction (later in time).

  \\[continuation-browser-forward-subproblem] moves forward one subproblem (earlier in time).
  \\[continuation-browser-backward-subproblem] moves backward one subproblem (later in time).

  \\[continuation-browser-goto] moves directly to a subproblem (given its number).

Display debugging information

  \\[continuation-browser-show-all-frames] shows All bindings of the current environment and its ancestors.
  \\[continuation-browser-show-current-frame] shows bindings of identifiers in the Current environment.
  \\[continuation-browser-print-environment] describes the current Environment.
  \\[continuation-browser-print-expression] pretty prints the current expression.
  \\[continuation-browser-print-environment-procedure] pretty prints the procedure that created the current environment.
  \\[continuation-browser-expand-reductions] shows the execution history (Reductions) of the current subproblem level.
  \\[continuation-browser-print-subproblem-or-reduction] describes the current subproblem or reduction.
  \\[continuation-browser-frame] displays the current stack frame in internal format.

Miscellany

  \\[continuation-browser-condition-restart] continues the program using a standard restart option.
  \\[continuation-browser-quit] Quits the debugger, killing the debugging buffer.
  \\[continuation-browser-return] returns (continues with) an expression after evaluating it."
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
  (local-set-variable! transcript-output-wrapper debug-interaction-output-wrapper))

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
;; Interaction mode but does not make sense here:

(define-key 'continuation-browser #\M-o
  (ref-command-object undefined))

;; Evaluation

(define-key 'continuation-browser '(#\c-x #\c-e)
  'continuation-browser-evaluate-previous-expression)

;; Subproblem/reduction motion

(define-key 'continuation-browser #\M-n
  'continuation-browser-forward-reduction)
(define-key 'continuation-browser #\M-C-n
  'continuation-browser-forward-subproblem)
(define-key 'continuation-browser #\M-p
  'continuation-browser-backward-reduction)
(define-key 'continuation-browser '(#\c-c #\g)
  'continuation-browser-goto)
(define-key 'continuation-browser #\M-C-p
  'continuation-browser-backward-subproblem)

;; Information display

(define-key 'continuation-browser '(#\c-c #\a)
  'continuation-browser-show-all-frames)
(define-key 'continuation-browser '(#\c-c #\c)
  'continuation-browser-show-current-frame)
(define-key 'continuation-browser '(#\c-c #\e)
  'continuation-browser-print-environment)
(define-key 'continuation-browser '(#\c-c #\l)
  'continuation-browser-print-expression)
(define-key 'continuation-browser '(#\c-c #\o)
  'continuation-browser-print-environment-procedure)
(define-key 'continuation-browser '(#\c-c #\r)
  'continuation-browser-expand-reductions)
(define-key 'continuation-browser '(#\c-c #\t)
  'continuation-browser-print-subproblem-or-reduction)
(define-key 'continuation-browser '(#\c-c #\y)
  'continuation-browser-frame)

;; Miscellany

(define-key 'continuation-browser '(#\c-c #\k)
  'continuation-browser-condition-restart)
(define-key 'continuation-browser '(#\c-c #\q)
  'continuation-browser-quit)
(define-key 'continuation-browser '(#\c-c #\z)
  'continuation-browser-return)