;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/comred.scm,v 1.86 1991/05/02 01:12:45 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989-91 Massachusetts Institute of Technology
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

;;;; Command Reader

(declare (usual-integrations))

(define *command-continuation*)	;Continuation of current command
(define *command-char*)		;Character read to find current command
(define *command*)		;The current command
(define *command-argument*)	;Argument from last command
(define *next-argument*)	;Argument to next command
(define *command-message*)	;Message from last command
(define *next-message*)		;Message to next command
(define *non-undo-count*)	;# of self-inserts since last undo boundary
(define keyboard-chars-read)	;# of chars read from keyboard
(define command-history)
(define command-history-limit 30)
(define command-reader-reset-thunk)
(define command-reader-reset-continuation)

(define (initialize-command-reader!)
  (set! keyboard-chars-read 0)
  (set! command-history (make-circular-list command-history-limit false))
  (set! command-reader-reset-thunk false)
  unspecific)

(define (top-level-command-reader initialization)
  (let loop ((initialization initialization))
    (with-keyboard-macro-disabled
     (lambda ()
       (call-with-current-continuation
	(lambda (continuation)
	  (fluid-let ((command-reader-reset-continuation continuation))
	    (dynamic-wind
	     (lambda () unspecific)
	     (lambda ()
	       (intercept-^G-interrupts (lambda () unspecific)
		 (lambda ()
		   (command-reader initialization))))
	     (lambda ()
	       (let ((thunk command-reader-reset-thunk))
		 (if thunk
		     (begin
		       (set! command-reader-reset-thunk false)
		       (thunk)))))))))))
    (loop false)))

(define (command-reader/reset-and-execute thunk)
  (set! command-reader-reset-thunk thunk)
  (command-reader-reset-continuation false))

(define (command-reader #!optional initialization)
  (define (command-reader-loop)
    (let ((value (with-command-variables start-next-command)))
      (if (not (eq? value 'ABORT))
	  (value)))
    (command-reader-loop))

  (define (with-command-variables start-next-command)
    (call-with-current-continuation
     (lambda (continuation)
       (fluid-let ((*command-continuation* continuation)
		   (*command-char* false)
		   (*command*)
		   (*next-argument* false)
		   (*next-message* false))
	 (bind-condition-handler (list condition-type:editor-error)
	     editor-error-handler
	   start-next-command)))))

  (define (start-next-command)
    (reset-command-state!)
    (let ((char (with-editor-interrupts-disabled keyboard-read-char)))
      (set! *command-char* char)
      (clear-message)
      (set-command-prompt!
       (if (not *command-argument*)
	   (char-name char)
	   (string-append-separated (command-argument-prompt)
				    (char-name char))))
      (let ((window (current-window)))
	(%dispatch-on-command window
			      (comtab-entry (buffer-comtabs
					     (window-buffer window))
					    char)
			      false)))
    (start-next-command))

  (fluid-let ((*command-argument*)
	      (*command-message*)
	      (*non-undo-count* 0))
    (if (and (not (default-object? initialization)) initialization)
	(with-command-variables
	 (lambda ()
	   (reset-command-state!)
	   (initialization))))
    (command-reader-loop)))

(define (reset-command-state!)
  (set! *command-argument* *next-argument*)
  (set! *next-argument* false)
  (set! *command-message* *next-message*)
  (set! *next-message* false)
  (if *command-argument*
      (set-command-prompt! (command-argument-prompt))
      (reset-command-prompt!))
  (if *defining-keyboard-macro?* (keyboard-macro-finalize-chars)))

;;; The procedures for executing commands come in two flavors.  The
;;; difference is that the EXECUTE-foo procedures reset the command
;;; state first, while the DISPATCH-ON-foo procedures do not.  The
;;; latter should only be used by "prefix" commands such as C-X or
;;; C-4, since they want arguments, messages, etc. to be passed on.

(define-integrable (execute-char comtab char)
  (reset-command-state!)
  (dispatch-on-char comtab char))

(define-integrable (execute-command command)
  (reset-command-state!)
  (%dispatch-on-command (current-window) command false))

(define (read-and-dispatch-on-char)
  (dispatch-on-char (current-comtabs)
		    (with-editor-interrupts-disabled keyboard-read-char)))

(define (dispatch-on-char comtab char)
  (set! *command-char* char)
  (set-command-prompt!
   (string-append-separated (command-argument-prompt) (xchar->name char)))
  (%dispatch-on-command (current-window) (comtab-entry comtab char) false))

(define (dispatch-on-command command #!optional record?)
  (%dispatch-on-command (current-window)
			command
			(if (default-object? record?) false record?)))

(define (abort-current-command #!optional value)
  (keyboard-macro-disable)
  (*command-continuation* (if (default-object? value) 'ABORT value)))

(define-integrable (current-command-char)
  *command-char*)

(define (last-command-char)
  (if (char? *command-char*)
      *command-char*
      (car (last-pair *command-char*))))

(define-integrable (current-command)
  *command*)

(define (set-command-argument! argument)
  (set! *next-argument* argument)
  unspecific)

(define-integrable (command-argument)
  *command-argument*)

(define (set-command-message! tag . arguments)
  (set! *next-message* (cons tag arguments))
  unspecific)

(define (command-message-receive tag if-received if-not-received)
  (if (and *command-message*
	   (eq? (car *command-message*) tag))
      (apply if-received (cdr *command-message*))
      (if-not-received)))

(define (command-history-list)
  (let loop ((history command-history))
    (if (car history)
	(let loop ((history (cdr history)) (result (list (car history))))
	  (if (eq? history command-history)
	      result
	      (loop (cdr history) (cons (car history) result))))
	(let ((history (cdr history)))
	  (if (eq? history command-history)
	      '()
	      (loop history))))))

(define (%dispatch-on-command window command record?)
  (set! *command* command)
  (guarantee-command-loaded command)
  (let ((point (window-point window))
	(point-x (window-point-x window))
	(procedure (command-procedure command)))
    (let ((normal
	   (lambda ()
	     (set! *non-undo-count* 0)
	     (undo-boundary! point)
	     (apply procedure (interactive-arguments command record?)))))
      (cond ((or *executing-keyboard-macro?* (command-argument))
	     (set! *non-undo-count* 0)
	     (apply procedure (interactive-arguments command record?)))
	    ((window-needs-redisplay? window)
	     (normal))
	    ((eq? command (ref-command-object forward-char))
	     (if (and (not (group-end? point))
		      (char-graphic? (mark-right-char point))
		      (< point-x (- (window-x-size window) 2)))
		 (window-direct-output-forward-char! window)
		 (normal)))
	    ((eq? command (ref-command-object backward-char))
	     (if (and (not (group-start? point))
		      (char-graphic? (mark-left-char point))
		      (positive? point-x)
		      (< point-x (-1+ (window-x-size window))))
		 (window-direct-output-backward-char! window)
		 (normal)))
	    ((or (eq? command (ref-command-object self-insert-command))
		 (and (eq? command (ref-command-object auto-fill-space))
		      (not (auto-fill-break? point)))
		 (command-argument-self-insert? command))
	     (let ((char *command-char*))
	       (if (let ((buffer (window-buffer window)))
		     (and (buffer-auto-save-modified? buffer)
			  (null? (cdr (buffer-windows buffer)))
			  (line-end? point)
			  (char-graphic? char)
			  (< point-x (-1+ (window-x-size window)))))
		   (begin
		     (if (or (zero? *non-undo-count*)
			     (>= *non-undo-count* 20))
			 (begin
			   (set! *non-undo-count* 0)
			   (undo-boundary! point)))
		     (set! *non-undo-count* (1+ *non-undo-count*))
		     (window-direct-output-insert-char! window char))
		   (region-insert-char! point char))))
	    (else
	     (normal))))))

(define (interactive-arguments command record?)
  (let ((specification (command-interactive-specification command))
	(record-command-arguments
	 (lambda (arguments)
	   (let ((history command-history))
	     (set-car! history (cons (command-name command) arguments))
	     (set! command-history (cdr history))))))
    (cond ((string? specification)
	   (with-values
	       (lambda ()
		 (let ((end (string-length specification)))
		   (let loop
		       ((index
			 (if (and (not (zero? end))
				  (char=? #\* (string-ref specification 0)))
			     (begin
			       (if (buffer-read-only? (current-buffer))
				   (barf-if-read-only))
			       1)
			     0)))
		     (if (< index end)
			 (let ((newline
				(substring-find-next-char specification
							  index
							  end
							  #\newline)))
			   (with-values
			       (lambda ()
				 (interactive-argument
				  (string-ref specification index)
				  (substring specification
					     (1+ index)
					     (or newline end))))
			     (lambda (argument expression from-tty?)
			       (with-values
				   (lambda ()
				     (if newline
					 (loop (1+ newline))
					 (values '() '() false)))
				 (lambda (arguments expressions any-from-tty?)
				   (values (cons argument arguments)
					   (cons expression expressions)
					   (or from-tty? any-from-tty?)))))))
			 (values '() '() false)))))
	     (lambda (arguments expressions any-from-tty?)
	       (if (or record?
		       (and any-from-tty?
			    (not (prefix-char-list? (current-comtabs)
						    (current-command-char)))))
		   (record-command-arguments expressions))
	       arguments)))
	  ((null? specification)
	   (if record? (record-command-arguments '()))
	   '())
	  (else
	   (let ((old-chars-read keyboard-chars-read))
	     (let ((arguments (specification)))
	       (if (or record? (not (= keyboard-chars-read old-chars-read)))
		   (record-command-arguments (map quotify-sexp arguments)))
	       arguments))))))

(define (execute-command-history-entry entry)
  (let ((history command-history))
    (if (not (equal? entry
		     (let loop ((entries (cdr history)) (tail history))
		       (if (eq? entries history)
			   (car tail)
			   (loop (cdr entries) entries)))))
	(begin
	  (set-car! history entry)
	  (set! command-history (cdr history)))))
  (apply (command-procedure (name->command (car entry)))
	 (map (let ((environment (->environment '(EDWIN))))
		(lambda (expression)
		  (eval-with-history expression environment)))
	      (cdr entry))))

(define (interactive-argument char prompt)
  (let ((prompting
	 (lambda (value)
	   (values value (quotify-sexp value) true)))
	(prefix
	 (lambda (prefix)
	   (values prefix (quotify-sexp prefix) false)))
	(varies
	 (lambda (value expression)
	   (values value expression false))))
    (case char
      ((#\b)
       (prompting
	(buffer-name (prompt-for-existing-buffer prompt (current-buffer)))))
      ((#\B)
       (prompting (buffer-name (prompt-for-buffer prompt (current-buffer)))))
      ((#\c)
       (prompting (prompt-for-char prompt)))
      ((#\C)
       (prompting (command-name (prompt-for-command prompt))))
      ((#\d)
       (varies (current-point) '(CURRENT-POINT)))
      ((#\D)
       (prompting
	(pathname->string (prompt-for-directory prompt false false))))
      ((#\f)
       (prompting (pathname->string (prompt-for-input-truename prompt false))))
      ((#\F)
       (prompting (pathname->string (prompt-for-pathname prompt false false))))
      ((#\k)
       (prompting (prompt-for-key prompt (current-comtabs))))
      ((#\m)
       (varies (current-mark) '(CURRENT-MARK)))
      ((#\n)
       (prompting (prompt-for-number prompt false)))
      ((#\N)
       (prefix (or (command-argument) (prompt-for-number prompt false))))
      ((#\p)
       (prefix (or (command-argument-value (command-argument)) 1)))
      ((#\P)
       (prefix (command-argument)))
      ((#\r)
       (varies (current-region) '(CURRENT-REGION)))
      ((#\s)
       (prompting (or (prompt-for-string prompt false 'NULL-DEFAULT) "")))
      ((#\v)
       (prompting (variable-name (prompt-for-variable prompt))))
      ((#\x)
       (prompting (prompt-for-expression prompt)))
      ((#\X)
       (prompting (prompt-for-expression-value prompt)))
      (else
       (editor-error "Invalid control letter "
		     char
		     " in interactive calling string")))))

(define (quotify-sexp sexp)
  (if (or (boolean? sexp)
	  (number? sexp)
	  (string? sexp)
	  (char? sexp))
      sexp
      `(QUOTE ,sexp)))