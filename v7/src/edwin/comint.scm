#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/comint.scm,v 1.5 1991/05/20 22:05:08 cph Exp $

Copyright (c) 1991 Massachusetts Institute of Technology

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
MIT in each case.

NOTE: Parts of this program (Edwin) were created by translation from
corresponding parts of GNU Emacs.  Users should be aware that the GNU
GENERAL PUBLIC LICENSE may apply to these parts.  A copy of that
license should have been included along with this file. |#

;;;; Command interpreter subprocess control
;;; Translated from "comint.el", by Olin Shivers.

(declare (usual-integrations))

(define (make-comint mode name program . switches)
  (let ((buffer (find-or-create-buffer (string-append "*" name "*"))))
    (if (let ((process (get-buffer-process buffer)))
	  (or (not process)
	      (not (process-runnable? process))))
	(begin
	  (set-buffer-major-mode! buffer mode)
	  (comint-exec buffer name program switches)))
    buffer))

(define (comint-exec buffer name program switches)
  ;; Get rid of any old processes.
  (for-each delete-process (buffer-processes buffer))
  (set-buffer-point! buffer (buffer-end buffer))
  (define-variable-local-value! buffer
    (ref-variable-object comint-program-name)
    program)
  (apply start-process
	 name
	 buffer
	 (process-environment-bind scheme-subprocess-environment
				   (string-append
				    "TERMCAP=emacs:co#"
				    (number->string
				     (screen-x-size (selected-screen)))
				    ":tc=unknown")
				   "TERM=emacs"
				   "EMACS=t")
	 program
	 switches))

(define-variable-per-buffer comint-prompt-regexp
  "Regexp to recognise prompts in the inferior process.
Defaults to \"^\", the null string at BOL.

Good choices:
  Canonical Lisp: \"^[^> ]*>+:? *\" (Lucid, franz, kcl, T, cscheme, oaklisp)
  Lucid Common Lisp: \"^\\(>\\|\\(->\\)+\\) *\"
  franz: \"^\\(->\\|<[0-9]*>:\\) *\"
  kcl: \"^>+ *\"
  shell: \"^[^#$%>]*[#$%>] *\"
  T: \"^>+ *\"

This is a good thing to set in mode hooks."
  "^")

(define-variable-per-buffer comint-input-ring-size
  "Size of input history ring."
  30)

(define-variable comint-last-input-end "" false)
(define-variable comint-input-ring "" false)

(define-variable comint-program-name
  "File name of program that is running in this buffer."
  false)

(define-major-mode comint fundamental "Comint"
  "Major mode for interacting with an inferior interpreter.
Interpreter name is same as buffer name, sans the asterisks.
Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.

This mode is typically customised to create inferior-lisp-mode,
shell-mode, etc.. This can be done by setting the hooks
comint-input-sentinel, comint-input-filter, and comint-get-old-input
to appropriate procedures, and the variable comint-prompt-regexp to
the appropriate regular expression.

An input history is maintained of size comint-input-ring-size, and
can be accessed with the commands comint-next-input [\\[comint-next-input]] and
comint-previous-input [\\[comint-previous-input]].  Commands not keybound by
default are send-invisible, comint-dynamic-complete, and 
comint-list-dynamic-completions.

If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it.

Entry to this mode runs the hooks on comint-mode-hook."
  (local-set-variable! mode-line-process '(": %s"))
  (local-set-variable! comint-input-ring
		       (make-ring (ref-variable comint-input-ring-size)))
  (local-set-variable! comint-last-input-end
		       (mark-right-inserting-copy
			(buffer-end (current-buffer))))
  (local-set-variable! comint-last-input-match false)
  (event-distributor/invoke! (ref-variable comint-mode-hook)))

(define-variable comint-mode-hook
  "An event distributor that is invoked when entering Comint mode."
  (make-event-distributor))

(define-key 'comint #\C-a 'comint-bol)
(define-key 'comint #\C-d 'comint-delchar-or-maybe-eof)
(define-key 'comint #\C-m 'comint-send-input)

(define-key 'comint #\M-p 'comint-previous-input)
(define-key 'comint #\M-n 'comint-next-input)
(define-key 'comint #\M-s 'comint-previous-similar-input)

(define-key 'comint '(#\C-c #\C-c) 'comint-interrupt-subjob)
(define-key 'comint '(#\C-c #\C-f) 'comint-continue-subjob)
(define-key 'comint '(#\C-c #\C-l) 'comint-show-output)
(define-key 'comint '(#\C-c #\C-o) 'comint-flush-output)
(define-key 'comint '(#\C-c #\C-r) 'comint-history-search-backward)
(define-key 'comint '(#\C-c #\C-s) 'comint-history-search-forward)
(define-key 'comint '(#\C-c #\C-u) 'comint-kill-input)
(define-key 'comint '(#\C-c #\C-w) 'backward-kill-word)
(define-key 'comint '(#\C-c #\C-z) 'comint-stop-subjob)
(define-key 'comint '(#\C-c #\C-\\) 'comint-quit-subjob)

(define-command comint-send-input
  "Send input to process.
After the process output mark, sends all text from the process mark to
point as input to the process.  Before the process output mark, calls
value of variable comint-get-old-input to retrieve old input, copies
it to the end of the buffer, and sends it.  A terminal newline is also
inserted into the buffer and sent to the process.  In either case,
value of variable comint-input-sentinel is called on the input before
sending it.  The input is entered into the input history ring, if
value of variable comint-input-filter returns non-false when called on
the input."
  ()
  (lambda () (comint-send-input "\n" false)))

(define (comint-send-input terminator delete?)
  (let ((process (current-process)))
    (let ((mark (process-mark process)))
      (let ((string
	     (let ((point (current-point)))
	       (if (mark>= point mark)
		   (let ((end (group-end point)))
		     (set-current-point! end)
		     (extract-string mark end))
		   (let ((string ((ref-variable comint-get-old-input))))
		     (delete-string mark (group-end mark))
		     (set-current-point! mark)
		     (insert-string string mark)
		     string)))))
	(let ((point (current-point)))
	  (move-mark-to! (ref-variable comint-last-input-end) point)
	  (if ((ref-variable comint-input-filter) string)
	      (ring-push! (ref-variable comint-input-ring) string))
	  ((ref-variable comint-input-sentinel) string)
	  (if delete?
	      (delete-string mark point)
	      (insert-newline point))
	  (move-mark-to! mark point)
	  (process-send-string process (string-append string terminator)))))))

(define-variable-per-buffer comint-get-old-input
  "Procedure that submits old text in comint mode.
This procedure is called when return is typed while the point is in old text.
It returns the text to be submitted as process input.  The default is
comint-get-old-input-default, which grabs the current line and strips off
leading text matching comint-prompt-regexp."
  (lambda ()
    (let ((mark (comint-line-start (current-point))))
      (extract-string mark (line-end mark 0)))))

(define-variable-per-buffer comint-input-sentinel
  "Called on each input submitted to comint mode process by comint-send-input.
Thus it can, for instance, track cd/pushd/popd commands issued to the shell."
  (lambda (string)
    string
    unspecific))

(define-variable-per-buffer comint-input-filter
  "Predicate for filtering additions to input history.
Only inputs answering true to this procedure are saved on the input
history list.  Default is to save anything that isn't all whitespace."
  (lambda (string)
    (not (re-match-string-forward (re-compile-pattern "\\`\\s *\\'" false)
				  false (ref-variable syntax-table) string))))

(define-command comint-previous-input
  "Cycle backwards through input history."
  "*p"
  (lambda (argument)
    (let ((point (current-point))
	  (ring (ref-variable comint-input-ring)))
      (let ((size (+ (ring-size ring) 1)))
	(let ((index
	       (modulo (+ argument
			  (command-message-receive comint-input-ring-tag
			    (lambda (index)
			      (delete-string (current-mark) point)
			      index)
			    (lambda ()
			      (push-current-mark! point)
			      (cond ((positive? argument) 0)
				    ((negative? argument) 2)
				    (else 1)))))
		       size)))
	  (message (number->string index))
	  (if (positive? index)
	      (insert-string (ring-ref ring (- index 1)) point))
	  (set-command-message! comint-input-ring-tag index))))))

(define comint-input-ring-tag
  '(COMINT-INPUT-RING))
	 
(define-command comint-next-input
  "Cycle forwards through input history."
  "*p"
  (lambda (argument)
    ((ref-command comint-previous-input) (- argument))))

(define-variable comint-last-input-match "" false)

(define-command comint-history-search-backward
  "Search backwards through the input history for a matching substring."
  (lambda ()
    (list (prompt-for-string "History search backward"
			     (ref-variable comint-last-input-match))))
  (lambda (string)
    (comint-history-search string true)))

(define-command comint-history-search-forward
  "Search forwards through the input history for a matching substring."
  (lambda ()
    (list (prompt-for-string "History search forward"
			     (ref-variable comint-last-input-match))))
  (lambda (string)
    (comint-history-search string false)))

(define (comint-history-search string backward?)
  (let ((ring (ref-variable comint-input-ring))
	(syntax-table (ref-variable syntax-table))
	(pattern (re-compile-pattern (re-quote-string string) false)))
    (let ((size (+ (ring-size ring) 1)))
      (let ((start
	     (command-message-receive comint-input-ring-tag
	       (lambda (index) index)
	       (lambda () (if backward? 0 size)))))
	(let loop ((index start))
	  (let ((index (+ index (if backward? 1 -1))))
	    (cond ((if backward? (>= index size) (< index 0))
		   (set-command-message! comint-input-ring-tag start)
		   (editor-failure "Not found"))
		  ((re-search-string-forward pattern
					     false
					     syntax-table
					     (ring-ref ring (- index 1)))
		   (set-variable! comint-last-input-match string)
		   ((ref-command comint-previous-input) (- index start)))
		  (else
		   (loop index)))))))))

(define-command comint-previous-similar-input
  "Reenter the last input that matches the string typed so far.
If repeated successively, older inputs are reentered.
With negative arg, newer inputs are reentered."
  "p"
  (lambda (argument)
    (let ((tag '(COMINT-PREVIOUS-SIMILAR-INPUT))
	  (mark (process-mark (current-process)))
	  (point (current-point))
	  (ring (ref-variable comint-input-ring)))
      (if (mark< point mark)
	  (editor-error "Not after process mark"))
      (let ((do-it
	     (lambda (index* prefix)
	       (let ((size (ring-size ring)))
		 (let loop ((index index*))
		   (let ((index (+ index (if (negative? argument) -1 1))))
		     (if (or (negative? index)
			     (>= index size))
			 (begin
			   (editor-failure "Not found")
			   (if (not (= index* -1))
			       (set-command-message! tag index* prefix)))
			 (let ((string (ring-ref ring index)))
			   (if (string-prefix? prefix string)
			       (begin
				 (delete-string mark point)
				 (insert-string string point)
				 (set-command-message! tag index prefix))
			       (loop index))))))))))
	(command-message-receive tag
	  do-it
	  (lambda () (do-it -1 (extract-string mark point))))))))

(define-command comint-kill-input
  "Kill all text from last stuff output by interpreter to point."
  ()
  (lambda ()
    (let ((mark (process-mark (current-process)))
	  (point (current-point)))
      (if (mark>= point mark)
	  (kill-string mark point)
	  (editor-error "Nothing to kill")))))

(define-command comint-flush-output
  "Kill all output from interpreter since last input."
  ()
  (lambda ()
    (let ((mark
	   (mark-permanent! (line-start (process-mark (current-process)) 0))))
      (delete-string (mark1+ (ref-variable comint-last-input-end) 'LIMIT) mark)
      (insert-string "*** output flushed ***\n" mark))))

(define-command comint-show-output
  "Start display of the current window at line preceding start of last output.
\"Last output\" is considered to start at the line following the last command
entered to the process."
  ()
  (lambda ()
    (let ((mark (line-start (ref-variable comint-last-input-end) 0)))
      (set-current-point! (comint-line-start mark))
      (set-window-start-mark! (current-window) mark true))))

(define-command comint-bol
  "Goes to the beginning of line, then skips past the prompt, if any.
With argument, don't skip the prompt -- go straight to column 0.

The prompt skip is done by skipping text matching the regular expression
comint-prompt-regexp."
  "P"
  (lambda (argument)
    (set-current-point!
     (if argument
	 (line-start (current-point) 0)
	 (comint-line-start (current-point))))))

(define (comint-line-start mark)
  (let ((start (line-start mark 0)))
    (let ((mark
	   (re-match-forward (ref-variable comint-prompt-regexp)
			     start
			     (group-end mark))))
      (if (mark<= mark (line-end start 0))
	  mark
	  start))))

(define-command comint-delchar-or-maybe-eof
  "If at end of buffer, send EOF to the current subprocess.
If not at end of buffer, just like \\[delete-char]."
  "p"
  (lambda (argument)
    (if (group-end? (current-point))
	(process-send-eof (current-process))
	((ref-command delete-char) argument))))

(define-command comint-interrupt-subjob
  "Sent an interrupt signal to the current subprocess.
If the process-connection-type is via ptys, the signal is sent to the current
process group of the pseudoterminal which Edwin is using to communicate with
the subprocess.  If the process is a job-control shell, this means the
shell's current subjob.  If the process connection is via pipes, the signal is
sent to the immediate subprocess."
  ()
  (lambda () (interrupt-process (current-process) true)))

(define-command comint-kill-subjob
  "Send a kill signal to the current subprocess.
See comint-interrupt-subjob for a description of \"current subprocess\"."
  ()
  (lambda () (kill-process (current-process) true)))

(define-command comint-quit-subjob
  "Send a quit signal to the current subprocess.
See comint-interrupt-subjob for a description of \"current subprocess\"."
  ()
  (lambda () (quit-process (current-process) true)))

(define-command comint-stop-subjob
  "Stop the current subprocess.
See comint-interrupt-subjob for a description of \"current subprocess\".

WARNING: if there is no current subjob, you can end up suspending
the top-level process running in the buffer.  If you accidentally do
this, use \\[comint-continue-subjob] to resume the process.   (This is not a
problem with most shells, since they ignore this signal.)"
  ()
  (lambda () (stop-process (current-process) true)))

(define-command comint-continue-subjob
  "Send a continue signal to current subprocess.
See comint-interrupt-subjob for a description of \"current subprocess\".
Useful if you accidentally suspend the top-level process."
  ()
  (lambda () (continue-process (current-process) true)))

;;;; Filename Completion

(define-command comint-replace-by-expanded-filename
  "Replace the filename at point with its expanded, canonicalised completion.
\"Expanded\" means environment variables (e.g., $HOME) and ~'s are
replaced with the corresponding directories.  \"Canonicalised\" means ..
and . are removed, and the filename is made absolute instead of relative.
See also \\[comint-dynamic-complete]."
  ()
  (lambda ()
    (let ((region (comint-current-filename-region)))
      (let ((filename (region->string region)))
	(set-current-point! (region-end region))
	(comint-filename-complete
	 (merge-pathnames (->pathname filename)
			  (buffer-default-directory (current-buffer)))
	 filename
	 (lambda (filename*)
	   (region-delete! region)
	   (insert-string filename* (region-start region))))))))

(define-command comint-dynamic-complete
  "Complete the filename at point.
This function is similar to \\[comint-replace-by-expanded-filename], except
that it won't change parts of the filename already entered in the buffer; 
it just adds completion characters to the end of the filename."
  ()
  (lambda ()
    (let ((region (comint-current-filename-region)))
      (let ((pathname
	     (merge-pathnames (->pathname (region->string region))
			      (buffer-default-directory (current-buffer)))))
	(let ((filename (pathname->string pathname)))
	  (set-current-point! (region-end region))
	  (comint-filename-complete
	   pathname
	   filename
	   (lambda (filename*)
	     (insert-substring filename*
			       (string-length filename)
			       (string-length filename*)
			       (region-end region)))))))))

(define-command comint-dynamic-list-completions
  "List all possible completions of the filename at point."
  ()
  (lambda ()
    (comint-list-filename-completions
     (lambda ()
       (filename-completions-list
	(merge-pathnames
	 (->pathname (region->string (comint-current-filename-region)))
	 (buffer-default-directory (current-buffer))))))))

(define (comint-current-filename-region)
  (let ((point (current-point))
	(chars "~/A-Za-z0-9---_.$#,"))
    (let ((start
	   (skip-chars-backward chars
				point
				(comint-line-start point)
				'LIMIT)))
      (let ((end
	     (skip-chars-forward chars start (line-end start 0) 'LIMIT)))
	(and (mark< start end)
	     (make-region start end))))))

(define (comint-filename-complete pathname filename insert-completion)
  (filename-complete-string pathname
    (lambda (filename*)
      (if (string=? filename filename*)
	  (message "Sole completion")
	  (insert-completion filename*)))
    (lambda (filename* list-completions)
      (if (string=? filename filename*)
	  (if (ref-variable completion-auto-help)
	      (comint-list-filename-completions list-completions)
	      (message "Next char not unique"))
	  (insert-completion filename*)))
    (lambda ()
      (editor-failure "No completions"))))

(define (comint-list-filename-completions list-completions)
  (message "Making completion list...")
  (let ((completions (list-completions)))
    (clear-message)
    (if (null? completions)
	(editor-failure "No completions")
	(begin
	  (write-completions-list completions)
	  (message "Hit space to flush.")
	  (reset-command-prompt!)
	  (let ((char (keyboard-peek-char)))
	    (if (char=? #\space char)
		(begin
		  (keyboard-read-char)
		  (kill-pop-up-buffer false))))
	  (clear-message)))))