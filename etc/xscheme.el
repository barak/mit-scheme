;; Run Scheme under Emacs
;; Copyright (C) 1986, 1987 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;; Requires C-Scheme release 5 or later
;;; Changes to Control-G handler require runtime version 13.85 or later

;;; $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/etc/xscheme.el,v 1.3 1987/11/23 18:32:32 cph Exp $

(require 'scheme)

(defvar scheme-program-name "scheme"
  "*Program invoked by the `run-scheme' command.")

(defvar scheme-band-name nil
  "*Band loaded by the `run-scheme' command.")

(defvar scheme-program-arguments nil
  "*Arguments passed to the Scheme program by the `run-scheme' command.")

(defun run-scheme (command-line)
  "Run an inferior Scheme process.
Output goes to the buffer `*scheme*'.
With argument, asks for a command line."
  (interactive
   (list (let ((default
		 (or xscheme-process-command-line
		     (xscheme-default-command-line))))
	   (if current-prefix-arg
	       (read-string "Run Scheme: " default)
	       default))))
  (setq xscheme-process-command-line command-line)
  (switch-to-buffer (xscheme-start-process command-line)))

(defun reset-scheme ()
  "Reset the Scheme process."
  (interactive)
  (let ((process (get-process "scheme")))
    (cond ((or (not process)
	       (not (eq (process-status process) 'run))
	       (yes-or-no-p
"The Scheme process is running, are you SURE you want to reset it? "))
	   (message "Resetting Scheme process...")
	   (if process (kill-process process t))
	   (xscheme-start-process xscheme-process-command-line)
	   (message "Resetting Scheme process...done")))))

(defun xscheme-default-command-line ()
  (concat scheme-program-name " -emacs"
	  (if scheme-program-arguments
	      (concat " " scheme-program-arguments)
	      "")
	  (if scheme-band-name
	      (concat " -band " scheme-band-name)
	      "")))

;;;; Internal Variables

(defvar xscheme-process-command-line nil
  "Command used to start the most recent Scheme process.")

(defvar xscheme-previous-send ""
  "Most recent expression transmitted to the Scheme process.")

(defvar xscheme-process-filter-state 'idle
  "State of scheme process escape reader state machine:
idle                   waiting for an escape sequence
reading-type           received an altmode but nothing else
reading-string         reading prompt string")

(defvar xscheme-process-filter-queue '()
  "Queue used to synchronize filter actions properly.")

(defvar xscheme-running-p nil
  "This variable, if nil, indicates that the scheme process is
waiting for input.  Otherwise, it is busy evaluating something.")

(defconst xscheme-control-g-synchronization-p (eq system-type 'hpux)
  "If non-nil, insert markers in the scheme input stream to indicate when
control-g interrupts were signalled.  Do not allow more control-g's to be
signalled until the scheme process acknowledges receipt.")

(defvar xscheme-control-g-disabled-p nil
  "This variable, if non-nil, indicates that a control-g is being processed
by the scheme process, so additional control-g's are to be ignored.")

(defvar xscheme-allow-output-p t
  "This variable, if nil, prevents output from the scheme process
from being inserted into the process-buffer.")

(defvar xscheme-prompt ""
  "The current scheme prompt string.")

(defvar xscheme-string-accumulator ""
  "Accumulator for the string being received from the scheme process.")

(defvar xscheme-string-receiver nil
  "Procedure to send the string argument from the scheme process.")

(defvar xscheme-start-hook nil
  "If non-nil, a procedure to call when the Scheme process is started.
When called, the current buffer will be the Scheme process-buffer.")

(defvar xscheme-signal-death-message nil
  "If non-nil, causes a message to be generated when the Scheme process dies.")

(defvar xscheme-mode-string ""
  "String displayed in the mode line when the Scheme process is running.")

;;;; Evaluation Commands

(define-key scheme-mode-map "\eo" 'xscheme-send-buffer)
(define-key scheme-mode-map "\ez" 'xscheme-send-definition)
(define-key scheme-mode-map "\e\C-m" 'xscheme-send-previous-expression)
(define-key scheme-mode-map "\e\C-x" 'xscheme-send-definition)
(define-key scheme-mode-map "\e\C-z" 'xscheme-send-region)
(define-key scheme-mode-map "\C-cb" 'xscheme-send-breakpoint-interrupt)
(define-key scheme-mode-map "\C-cg" 'xscheme-send-control-g-interrupt)
(define-key scheme-mode-map "\C-cn" 'xscheme-send-next-expression)
(define-key scheme-mode-map "\C-cp" 'xscheme-send-previous-expression)
(define-key scheme-mode-map "\C-cu" 'xscheme-send-control-u-interrupt)
(define-key scheme-mode-map "\C-cx" 'xscheme-send-control-x-interrupt)
;(define-key scheme-mode-map "\C-c\C-m" 'xscheme-send-current-line)
(define-key scheme-mode-map "\C-c\C-y" 'xscheme-yank-previous-send)
(define-key scheme-mode-map "\C-x\C-e" 'xscheme-send-previous-expression)

(defun xscheme-send-string (&rest strings)
  "Send the string arguments to the Scheme process.
The strings are concatenated and terminated by a newline."
  (cond (xscheme-running-p
	 (error "No sends allowed while Scheme running"))
	((xscheme-process-running-p)
	 (xscheme-send-string-1 strings))
	((yes-or-no-p "The Scheme process has died.  Reset it? ")
	 (reset-scheme)
	 (xscheme-wait-for-process)
	 (goto-char (point-max))
	 (apply 'insert-before-markers strings)
	 (xscheme-send-string-1 strings))))

(defun xscheme-send-string-1 (strings)
  (let ((string (apply 'concat strings)))
    (xscheme-send-string-2 string)
    (if (xscheme-process-buffer-current-p)
	(setq xscheme-previous-send string))))

(defun xscheme-send-string-2 (string)
  (let ((process (get-process "scheme")))
    (send-string process (concat string "\n"))
    (if (xscheme-process-buffer-current-p)
	(set-marker (process-mark process) (point)))))

(defun xscheme-yank-previous-send ()
  "Insert the most recent expression at point."
  (interactive)
  (push-mark)
  (insert xscheme-previous-send))

(defun xscheme-send-region (start end)
  "Send the current region to the Scheme process.
The region is sent terminated by a newline."
  (interactive "r")
  (if (xscheme-process-buffer-current-p)
      (progn (goto-char end)
	     (set-marker (process-mark (get-process "scheme")) end)))
  (xscheme-send-string (buffer-substring start end)))

(defun xscheme-send-definition ()
  "Send the current definition to the Scheme process.
If the current line begins with a non-whitespace character,
parse an expression from the beginning of the line and send that instead."
  (interactive)
  (let ((start nil) (end nil))
    (save-excursion
      (end-of-defun)
      (setq end (point))
      (if (re-search-backward "^\\s(" nil t)
	  (setq start (point))
	  (error "Can't find definition")))
    (xscheme-send-region start end)))

(defun xscheme-send-next-expression ()
  "Send the expression to the right of `point' to the Scheme process."
  (interactive)
  (let ((start (point)))
    (xscheme-send-region start (save-excursion (forward-sexp) (point)))))

(defun xscheme-send-previous-expression ()
  "Send the expression to the left of `point' to the Scheme process."
  (interactive)
  (let ((end (point)))
    (xscheme-send-region (save-excursion (backward-sexp) (point)) end)))

(defun xscheme-send-current-line ()
  "Send the current line to the Scheme process.
Useful for working with `adb'."
  (interactive)
  (let ((line
	 (save-excursion
	   (beginning-of-line)
	   (let ((start (point)))
	     (end-of-line)
	     (buffer-substring start (point))))))
    (end-of-line)
    (insert ?\n)
    (xscheme-send-string-2 line)))

(defun xscheme-send-buffer ()
  "Send the current buffer to the Scheme process."
  (interactive)
  (if (xscheme-process-buffer-current-p)
      (error "Not allowed to send this buffer's contents to Scheme"))
  (xscheme-send-region (point-min) (point-max)))

(defun xscheme-send-char (char)
  "Prompt for a character and send it to the Scheme process."
  (interactive "cCharacter to send: ")
  (send-string "scheme" (char-to-string char)))

(defun xscheme-send-breakpoint-interrupt ()
  "Cause the Scheme process to enter a breakpoint."
  (interactive)
  (xscheme-send-interrupt ?b nil))

(defun xscheme-send-control-g-interrupt ()
  "Cause the Scheme processor to halt and flush input.
Control returns to the top level rep loop."
  (interactive)
  (let ((inhibit-quit t))
    (cond ((not xscheme-control-g-synchronization-p)
	   (interrupt-process "scheme" t))
	  (xscheme-control-g-disabled-p
	   (message "Relax..."))
	  (else
	   (setq xscheme-control-g-disabled-p t)
	   (message "Sending C-G interrupt to Scheme...")
	   (interrupt-process "scheme" t)
	   (send-string "scheme" (char-to-string 0))))))

(defun xscheme-send-control-u-interrupt ()
  "Cause the Scheme process to halt, returning to previous rep loop."
  (interactive)
  (xscheme-send-interrupt ?u t))

(defun xscheme-send-control-x-interrupt ()
  "Cause the Scheme process to halt, returning to current rep loop."
  (interactive)
  (xscheme-send-interrupt ?x t))

;;; This doesn't really work right -- Scheme just gobbles the first
;;; character in the input.  There is no way for us to guarantee that
;;; the argument to this procedure is the first char unless we put
;;; some kind of marker in the input stream.

(defun xscheme-send-interrupt (char mark-p)
  "Send a ^A type interrupt to the Scheme process."
  (interactive "cInterrupt character to send: ")
  (quit-process "scheme" t)
  (send-string "scheme" (char-to-string char))
  (if mark-p
      (send-string "scheme" (char-to-string 0))))

;;;; Basic Process Control

(defun xscheme-start-process (command-line)
  (let ((buffer (get-buffer-create "*scheme*")))
    (let ((process (get-buffer-process buffer)))
      (save-excursion
	(set-buffer buffer)
	(if (and process (memq (process-status process) '(run stop)))
	    (set-marker (process-mark process) (point-max))
	    (progn (if process (delete-process process))
		   (goto-char (point-max))
		   (scheme-mode)
		   (setq mode-line-process '(": %s"))
		   (add-to-global-mode-string 'xscheme-mode-string)
		   (setq process
			 (apply 'start-process
				(cons "scheme"
				      (cons buffer
					    (xscheme-parse-command-line
					     command-line)))))
		   (set-marker (process-mark process) (point-max))
		   (xscheme-process-filter-initialize t)
		   (xscheme-modeline-initialize)
		   (set-process-sentinel process 'xscheme-process-sentinel)
		   (set-process-filter process 'xscheme-process-filter)
		   (run-hooks 'xscheme-start-hook)))))
    buffer))

(defun xscheme-parse-command-line (string)
  (setq string (substitute-in-file-name string))
  (let ((start 0)
	(result '()))
    (while start
      (let ((index (string-match "[ \t]" string start)))
	(setq start
	      (cond ((not index)
		     (setq result
			   (cons (substring string start)
				 result))
		     nil)
		    ((= index start)
		     (string-match "[^ \t]" string start))
		    (t
		     (setq result
			   (cons (substring string start index)
				 result))
		     (1+ index))))))
    (nreverse result)))

(defun xscheme-wait-for-process ()
  (sleep-for 2)
  (while xscheme-running-p
    (sleep-for 1)))

(defun xscheme-process-running-p ()
  "True iff there is a Scheme process whose status is `run'."
  (let ((process (get-process "scheme")))
    (and process
	 (eq (process-status process) 'run))))

(defun xscheme-process-buffer ()
  (let ((process (get-process "scheme")))
    (and process (process-buffer process))))

(defun xscheme-process-buffer-window ()
  (let ((buffer (xscheme-process-buffer)))
    (and buffer (get-buffer-window buffer))))

(defun xscheme-process-buffer-current-p ()
  "True iff the current buffer is the Scheme process buffer."
  (eq (xscheme-process-buffer) (current-buffer)))

;;;; Process Filter

(defun xscheme-process-sentinel (proc reason)
  (let ((inhibit-quit t))
    (xscheme-process-filter-initialize (eq reason 'run))
    (if (eq reason 'run)
	(xscheme-modeline-initialize)
	(setq xscheme-mode-string "")))
  (if (and (not (memq reason '(run stop)))
	   xscheme-signal-death-message)
      (progn (beep)
	     (message
"The Scheme process has died!  Do M-x reset-scheme to restart it"))))

(defun xscheme-process-filter-initialize (running-p)
  (setq xscheme-process-filter-state 'idle)
  (setq xscheme-process-filter-queue (cons '() '()))
  (setq xscheme-running-p running-p)
  (setq xscheme-control-g-disabled-p nil)
  (setq xscheme-allow-output-p t)
  (setq xscheme-prompt "")
  (setq xscheme-string-accumulator "")
  (setq xscheme-string-receiver nil))

(defun xscheme-process-filter (proc string)
  (let ((inhibit-quit t))
    (cond ((eq xscheme-process-filter-state 'idle)
	   (xscheme-process-filter:idle string))
	  ((eq xscheme-process-filter-state 'reading-type)
	   (xscheme-process-filter:reading-type string))
	  ((eq xscheme-process-filter-state 'reading-string)
	   (xscheme-process-filter:reading-string string))
	  (t (error "Scheme process filter -- bad state")))))

(defun xscheme-process-filter:idle (string)
  (setq xscheme-process-filter-state 'idle)
  (let ((start (string-match "\e" string)))
    (if start
	(progn (xscheme-process-filter:idle-1 (substring string 0 start))
	       (xscheme-process-filter:reading-type
		(substring string (1+ start))))
	(progn (xscheme-process-filter:idle-1 string)
	       (xscheme-process-filter:finish)))))

(defun xscheme-process-filter:idle-1 (string)
  (while (string-match "\\(\007\\|\f\\)" string)
    (let ((start (match-beginning 0))
	  (end (match-end 0)))
      (xscheme-process-filter-output (substring string 0 start))
      (if (= ?\f (aref string start))
	  (progn (xscheme-guarantee-newlines 1)
		 (xscheme-process-filter-output ?\f))
	  (beep))
      (setq string (substring string (1+ start)))))
  (xscheme-process-filter-output string))

(defun xscheme-process-filter:reading-type (string)
  (let ((len (length string)))
    (if (= 0 len)
	(progn (setq xscheme-process-filter-state 'reading-type)
	       (xscheme-process-filter:finish))
	(xscheme-process-filter-dispatch (aref string 0)
					 (substring string 1 len)))))

(defun xscheme-process-filter:reading-string (string)
  (let ((start (string-match "\e" string)))
    (if start
	(progn (xscheme-process-filter:enqueue
		(list xscheme-string-receiver
		      (concat xscheme-string-accumulator
			      (substring string 0 start))))
	       (setq xscheme-string-accumulator "")
	       (setq xscheme-string-receiver nil)
	       (xscheme-process-filter:idle
		(substring string (1+ start) (length string))))
	(progn (setq xscheme-string-accumulator
		     (concat xscheme-string-accumulator string))
	       (setq xscheme-process-filter-state 'reading-string)
	       (xscheme-process-filter:finish)))))

(defun xscheme-process-filter:enqueue (action)
  (let ((next (cons action '())))
    (if (cdr xscheme-process-filter-queue)
	(setcdr (cdr xscheme-process-filter-queue) next)
	(setcar xscheme-process-filter-queue next))
    (setcdr xscheme-process-filter-queue next)))

(defun xscheme-process-filter:finish ()
  (while (car xscheme-process-filter-queue)
    (let ((next (car xscheme-process-filter-queue)))
      (setcar xscheme-process-filter-queue (cdr next))
      (if (not (cdr next))
	  (setcdr xscheme-process-filter-queue '()))
      (apply (car (car next)) (cdr (car next))))))

;;;; Process Filter Output

(defun xscheme-process-filter-output (&rest args)
  (if (not (and args
		(null (cdr args))
		(stringp (car args))
		(string-equal "" (car args))))
      (xscheme-process-filter:enqueue
       (cons 'xscheme-process-filter-output-1 args))))

(defun xscheme-process-filter-output-1 (&rest args)
  (if xscheme-allow-output-p
      (save-excursion
	(xscheme-goto-output-point)
	(apply 'insert-before-markers args))))

(defun xscheme-guarantee-newlines (n)
  (if xscheme-allow-output-p
      (save-excursion
	(xscheme-goto-output-point)
	(let ((stop nil))
	  (while (and (not stop)
		      (bolp))
	    (setq n (1- n))
	    (if (bobp)
		(setq stop t)
		(backward-char))))
	(xscheme-goto-output-point)
	(while (> n 0)
	  (insert-before-markers ?\n)
	  (setq n (1- n))))))

(defun xscheme-goto-output-point ()
  (let ((process (get-process "scheme")))
    (set-buffer (process-buffer process))
    (goto-char (process-mark process))))

(defun xscheme-modeline-initialize ()
  (setq xscheme-mode-string "  "))

(defun xscheme-set-runlight (runlight)
  (aset xscheme-mode-string 0 runlight)
  (xscheme-modeline-redisplay))

(defun xscheme-modeline-redisplay ()
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p))
  (sit-for 0))

;;;; Process Filter Operations

(defun xscheme-process-filter-dispatch (char string)
  (cond ((= char ?b)
	 (xscheme-process-filter:simple-action
	  'xscheme-start-gc
	  string))
	((= char ?e)
	 (xscheme-process-filter:simple-action
	  'xscheme-finish-gc
	  string))
	((= char ?s)
	 (xscheme-process-filter:simple-action
	  'xscheme-enter-input-wait
	  string))
	((= char ?f)
	 (xscheme-process-filter:simple-action
	  'xscheme-exit-input-wait
	  string))
	((= char ?c)
	 (xscheme-process-filter:simple-action
	  'xscheme-input-char-immediately
	  string))
	((= char ?z)
	 (xscheme-process-filter:simple-action
	  'xscheme-select-process-buffer
	  string))
	((= char ?m)
	 (xscheme-process-filter:string-action 'xscheme-message string))
	((= char ?p)
	 (xscheme-process-filter:string-action 'xscheme-set-prompt string))
	((= char ?P)
	 (xscheme-process-filter:string-action 'xscheme-set-prompt-variable
					      string))
	((= char ?v)
	 (xscheme-process-filter:string-action 'xscheme-write-value string))
	((= char ?g)
	 (xscheme-process-filter:simple-action 'xscheme-enable-control-g
					       string))
	(t
	 (xscheme-process-filter-output ?\e char)
	 (xscheme-process-filter:idle string))))

(defun xscheme-process-filter:simple-action (action string)
  (xscheme-process-filter:enqueue (list action))
  (xscheme-process-filter:idle string))

(defun xscheme-process-filter:string-action (action string)
  (setq xscheme-string-receiver action)
  (xscheme-process-filter:reading-string string))

(defconst xscheme-runlight:running ?R
  "The character displayed when the Scheme process is running.")

(defconst xscheme-runlight:input ?I
  "The character displayed when the Scheme process is waiting for input.")

(defconst xscheme-runlight:gc ?G
  "The character displayed when the Scheme process is garbage collecting.")

(defun xscheme-start-gc ()
  (xscheme-set-runlight xscheme-runlight:gc))

(defun xscheme-finish-gc ()
  (xscheme-set-runlight
   (if xscheme-running-p xscheme-runlight:running xscheme-runlight:input)))

(defun xscheme-enter-input-wait ()
  (xscheme-set-runlight xscheme-runlight:input)
  (setq xscheme-running-p nil))

(defun xscheme-exit-input-wait ()
  (xscheme-set-runlight xscheme-runlight:running)
  (setq xscheme-running-p t))

(defun xscheme-enable-control-g ()
  (setq xscheme-control-g-disabled-p nil))

(defun xscheme-input-char-immediately ()
  (xscheme-message xscheme-prompt)
  (let ((char nil)
	(aborted-p t)
	(not-done t))
    (unwind-protect
	(while not-done
	  (setq char
		(let ((cursor-in-echo-area t))
		  (read-char)))
	  (cond ((= char ?\C-g)
		 (setq char nil)
		 (setq not-done nil))
		((= char ?\n)
		 ;; Disallow newlines, as Scheme is explicitly
		 ;; ignoring them.  This is necessary because
		 ;; otherwise Scheme will attempt to read another
		 ;; character.
		 (beep))
		(t
		 (setq aborted-p nil)
		 (setq not-done nil))))
      (if aborted-p
	  (xscheme-send-control-g-interrupt)))
    (xscheme-message "")
    (if char
	(xscheme-send-char char))))

(defun xscheme-select-process-buffer ()
  (let ((window (or (xscheme-process-buffer-window)
		    (display-buffer (xscheme-process-buffer)))))
    (save-window-excursion
      (select-window window)
      (xscheme-goto-output-point))))

(defun xscheme-message (string)
  (message "%s" string))

(defun xscheme-write-value (string)
  (if (not (zerop (length string)))
      (progn (xscheme-guarantee-newlines 1)
	     (xscheme-process-filter-output-1 (concat ";Value: " string))
	     (if (not (xscheme-process-buffer-window))
		 (xscheme-message string)))))

(defun xscheme-set-prompt-variable (string)
  (setq xscheme-prompt string))

(defun xscheme-set-prompt (string)
  (setq xscheme-prompt string)
  (xscheme-guarantee-newlines 2)
  (setq xscheme-mode-string
	(concat (substring xscheme-mode-string 0 2)
		(xscheme-coerce-prompt string)))
  (xscheme-modeline-redisplay))

(defun xscheme-coerce-prompt (string)
  (if (string-match "^[0-9]+ " string)
      (let ((end (match-end 0)))
	(concat (substring string 0 end)
		(let ((prompt (substring string end)))
		  (cond ((or (string-equal prompt "]=>")
			     (string-equal prompt "==>")
			     (string-equal prompt "Eval-in-env-->"))
			 "[Normal REP]")
			((string-equal prompt "Bkpt->") "[Breakpoint REP]")
			((string-equal prompt "Error->") "[Error REP]")
			((string-equal prompt "Debug-->") "[Debugger]")
			((string-equal prompt "Debugger-->") "[Debugger REP]")
			((string-equal prompt "Where-->")
			 "[Environment Inspector]")
			(t prompt)))))
      string))

(defun add-to-global-mode-string (x)
  (cond ((null global-mode-string)
	 (setq global-mode-string (list "" x " ")))
	((not (memq x global-mode-string))
	 (setq global-mode-string
	       (cons ""
		     (cons x
			   (cons " "
				 (if (equal "" (car global-mode-string))
				     (cdr global-mode-string)
				     global-mode-string))))))))
