;;; -*-Scheme-*-
;;;
;;;	$Id: debug.scm,v 1.14 1993/08/23 01:51:40 jbank Exp $
;;;
;;;	Copyright (c) 1992-93 Massachusetts Institute of Technology
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

;;;; Browser-style Debug and Where
;;; Package: (edwin new-debugger)

(declare (usual-integrations))


;;;;;;;;Text prop setup stuff

(define (port/buffer port)
  (mark-buffer (port/mark port)))

(define (with-output-props props thunk port)
  (let ((start (mark-index (port/mark port))))
    (thunk)
    (let ((end (mark-index (port/mark port))))
      (add-text-properties (buffer-group (port/buffer port))
			   (min start end)
			   (max start end)
			   props))))

(define (readable-between start end)
  (remove-text-properties  (buffer-group (mark-buffer start))
			   (mark-index start)
			   (mark-index end)
			   (list (list 'READ-ONLY))))

(define (dehigh-between start end)
  (remove-text-properties (buffer-group (mark-buffer start))
		       (mark-index start)
		       (mark-index end)
		       '((highlighted))))

(define (read-only-between start end)
  (add-text-properties (buffer-group (mark-buffer start))
		       (mark-index start)
		       (mark-index end)
		       (list (list 'READ-ONLY (generate-uninterned-symbol)))))

(define (debugger-pp-highlight-subexpression expression subexpression
 					     indentation port)
  (let ((start-mark #f)
 	(end-mark #f))
    (fluid-let ((*pp-no-highlights?* #f))
      (debugger-pp
       (unsyntax-with-substitutions
 	expression
 	(list (cons subexpression
 		    (make-pretty-printer-highlight
 		     (unsyntax subexpression)
 		     (lambda (port)
 		       (set! start-mark
 			     (mark-right-inserting-copy
 			      (output-port->mark port)))
 		       unspecific)
 		     (lambda (port)
 		       (set! end-mark
 			     (mark-right-inserting-copy
 			      (output-port->mark port)))
 		       unspecific)))))
       indentation
       port))
    (if (and start-mark end-mark)
 	(highlight-region-excluding-indentation start-mark end-mark))
    (if start-mark (mark-temporary! start-mark))
    (if end-mark (mark-temporary! end-mark))))
 
(define (highlight-region-excluding-indentation start end)
  (let loop ((start start))
    (let ((lend (line-end start 0)))
      (if (mark<= lend end)
 	  (begin
 	    (highlight-region (horizontal-space-end start)
 			      (horizontal-space-start lend))
 	    (loop (mark1+ lend)))
	  (let ((start (horizontal-space-end start))
		(end (horizontal-space-start end)))
	    (if (mark< start end)
		(highlight-region start end)))))))
 
(define (highlight-region start end)
  (if (not (mark<= start end))
      (error "Marks incorrectly related:" start end))
  (group-highlight (mark-group start) (mark-index start) (mark-index end)))
 
(define (group-highlight group start end)
  (add-text-properties group start end '((HIGHLIGHTED . #T))))

;;;;;;End of text setup stuff.


;;;; Browsers

(define browser-rtd
  (make-record-type
   "browser"
   '(
     ;; The browser's buffer.
     BUFFER

     ;; The object being browsed.
     OBJECT

     ;; Name of this browser, a string.  Not necessarily unique.
     NAME

     ;; Vector of BLINE objects, sorted in order of increasing INDEX.
     LINES

     ;; The current selected BLINE object.
     SELECTED-LINE

     ;; List of buffers associated with this browser.
     BUFFERS

     PROPERTIES)))

(define browser? (record-predicate browser-rtd))
(define browser/buffer (record-accessor browser-rtd 'BUFFER))
(define browser/object (record-accessor browser-rtd 'OBJECT))
(define browser/lines (record-accessor browser-rtd 'LINES))
(define set-browser/lines! (record-modifier browser-rtd 'LINES))
(define browser/selected-line (record-accessor browser-rtd 'SELECTED-LINE))
(define set-browser/selected-line!
  (record-modifier browser-rtd 'SELECTED-LINE))
(define browser/name (record-accessor browser-rtd 'NAME))
(define browser/buffers (record-accessor browser-rtd 'BUFFERS))
(define set-browser/buffers! (record-modifier browser-rtd 'BUFFERS))
(define browser/properties (record-accessor browser-rtd 'PROPERTIES))

(define make-browser
  (let ((constructor (record-constructor browser-rtd)))
    (lambda (name mode object)
      (let ((buffer (new-buffer name)))
	(buffer-reset! buffer)
	(set-buffer-read-only! buffer)
	(set-buffer-major-mode! buffer mode)
	(add-kill-buffer-hook buffer kill-browser-buffer)
	(let ((browser
	       (constructor buffer
			    object
			    name
			    (vector)
			    false
			    '()
			    (make-1d-table))))
	  (buffer-put! buffer 'BROWSER browser)
	  browser)))))

;;; Delete the screen if :  it is the debugger, not the env browser
;;;                        there is more than one active screen
;;;                        there is only one debugger buffer

(define (kill-browser-buffer buffer)
  (let ((browser (buffer-get buffer 'BROWSER)))
    (if browser
	(for-each kill-buffer (browser/buffers browser)))
    (if (and (equal? (browser/name browser) "*debug*")
	     (> (length (screen-list)) 1)
	     (= (length (find-debugger-buffers)) 1))
	(delete-screen! (selected-screen)))))

(define (buffer-browser buffer)
  (let ((browser (buffer-get buffer 'BROWSER)))
    (if (not browser)
	(error "This buffer has no associated browser:" buffer))
    browser))

(define (browser/new-buffer browser initializer)
  (let ((buffer
	 (create-buffer
	  (let ((prefix (browser/name browser)))
	    (let loop ((index 1))
	      (let ((name
		     (string-append
		      (if (1d-table/get (browser/properties browser)
					'VISIBLE-SUB-BUFFERS?
					false)
			  ""
			  " ")
		      prefix
		      "-"
		      (number->string index))))
		(if (find-buffer name)
		    (loop (+ index 1))
		    name)))))))
    (if initializer
	(initializer buffer))
    (add-browser-buffer! browser buffer)
    buffer))

(define (add-browser-buffer! browser buffer)
  (add-rename-buffer-hook
   buffer
   (letrec
       ((hook
	 (lambda (buffer name)
	   name
	   (set-browser/buffers! browser
				 (delq! buffer (browser/buffers browser)))
	   (remove-rename-buffer-hook buffer hook))))
     hook))
  (add-kill-buffer-hook
   buffer
   (lambda (buffer)
     (set-browser/buffers! browser
			   (delq! buffer (browser/buffers browser)))))
  (set-browser/buffers! browser (cons buffer (browser/buffers browser)))
  (buffer-put! buffer 'ASSOCIATED-WITH-BROWSER browser))

;;;; Browser Commands

(define-command browser-select-line
  "Select the current browser line."
  ()
  (lambda ()
    (let ((bline (mark->bline (current-point))))
      (if (not bline)
	  (editor-error "Nothing to select on this line."))
      (select-bline bline))))

;;; If the mouse clicks on a bline, select it.
(define-command debugger-mouse-select-bline
  "Select a bline when mouse clicked there."
  ()
  (lambda ()
    ((ref-command x-mouse-set-point))
    (let ((bline (mark->bline (current-point))))
      (if bline
	  (select-bline bline)))))

(define-command browser-next-line
  "Move down to the next line."
  "p"
  (lambda (argument)
    (let* ((browser (buffer-browser (current-buffer)))
	   (bline
	    (letrec
		((loop
		  (lambda (index argument)
		    (let ((bline (browser/line browser index)))
		      (cond ((bline/continuation? bline)
			     (replace-continuation-bline bline)
			     (loop index argument))
			    ((= argument 0)
			     bline)
			    ((> argument 0)
			     (let ((index (+ index 1)))
			       (if (< index (browser/n-lines browser))
				   (loop index (- argument 1))
				   (begin
				     (select-bline bline)
				     false))))
			    (else
			     (let ((index (- index 1)))
			       (if (<= 0 index)
				   (loop index (+ argument 1))
				   (begin
				     (select-bline bline)
				     false)))))))))
	      (let ((point (current-point)))
		(let ((index (mark->bline-index point)))
		  (cond (index
			 (loop index argument))
			((= argument 0)
			 false)
			(else
			 (let ((n (if (< argument 0) -1 1)))
			   (let find-next ((mark point))
			     (let ((mark (line-start mark n false)))
			       (and mark
				    (let ((index (mark->bline-index mark)))
				      (if index
					  (loop index (- argument n))
					  (find-next mark))))))))))))))
      (cond (bline
	     (select-bline bline))
	    ((= argument 0)
	     (editor-failure "Nothing to select on this line."))
	    (else
	     (editor-failure))))))

(define-command browser-previous-line
  "Move up to the previous line."
  "p"
  (lambda (argument)
    ((ref-command browser-next-line) (- argument))))

(define (select-bline bline)
  (let ((bline
	 (if (bline/continuation? bline)
	     (replace-continuation-bline bline)
	     bline)))
    (let ((browser (bline/browser bline)))
      (unselect-bline browser)
      (let ((mark (bline/start-mark bline)))
	(with-buffer-open mark
	  (lambda ()
	    (insert-char #\> (mark1+ mark))
	    (delete-right-char mark)
	    (highlight-the-number mark)))
	(set-browser/selected-line! browser bline)
	(set-buffer-point! (mark-buffer mark) mark)))

    (let ((buffer (bline/description-buffer bline)))
      (if buffer
	  (pop-up-buffer buffer false)))))

(define (highlight-the-number mark)
  (let ((end (re-search-forward "[RSE][0-9]+ " mark (line-end mark 0))))
    (highlight-region mark (if (mark? end)
				      (mark- end 1) 
				      (line-end mark 0)))))

(define (unselect-bline browser)
  (let ((bline (browser/selected-line browser)))
    (if bline
	(let ((mark (bline/start-mark bline)))
	  (with-buffer-open mark
	    (lambda ()
	      (dehigh-between mark (line-end mark 0))
	      (insert-char #\space (mark1+ mark))
	      (delete-right-char mark)))))))

;;;For any frame with an environment (excluding the mark frame)
;;;an inferior repl is started below the other descriptions.
(define (bline/description-buffer bline)
  (let* ((system? 
	  (and (subproblem? (bline/object bline))
	       (system-frame? (subproblem/stack-frame (bline/object bline)))))
	 (buffer
	  (1d-table/get (bline/properties bline) 'DESCRIPTION-BUFFER false))
	 (get-environment 
	  (1d-table/get (bline-type/properties (bline/type bline))
			'GET-ENVIRONMENT
			false))
	 (env-exists? (if (and get-environment (not system?))
			  (let ((environment* (get-environment bline)))
			    (environment? environment*))
			  #f))
	 (environment (if env-exists?
			  (get-environment bline)
			  #f)))
    (if (and buffer (buffer-alive? buffer))
	buffer
	(let ((write-description
	       (bline-type/write-description (bline/type bline))))
	  (temporary-message "Computing, please wait...")
	  (and write-description
	       (let ((buffer (browser/new-buffer (bline/browser bline) false)))
		 (call-with-output-mark (buffer-start buffer)
					(lambda (port)
					  (write-description bline port)
					  (if env-exists?
					      (write-string "\n;EVALUATION may occur below in the environment of the selected frame.\n" port))))
		 (set-buffer-point! buffer (buffer-start buffer))
		 (1d-table/put! (bline/properties bline)
				'DESCRIPTION-BUFFER
				buffer)
		 (read-only-between (buffer-start buffer) (buffer-end buffer))	 
		 (buffer-not-modified! buffer)
		 (if env-exists?
		     (start-inferior-repl!
		      buffer
		      environment
		      (evaluation-syntax-table buffer environment)
		      #f))
		 (append-message "done")
		 buffer))))))


;;;Main addition deals with possibility that the debugger was
;;;called by a break procure, makes sure to restart the thread

(define-command browser-quit
  "Exit the current browser, deleting its buffer."
  ()
  (lambda ()
    (let ((buffer (current-buffer)))
      (let ((window (current-window))
	    (buffers (browser/buffers (buffer-browser buffer))))
	(for-each (lambda (window*)
		    (if (and (not (eq? window* window))
			     (not (typein-window? window*))
			     (memq (window-buffer window*) buffers))
			(window-delete! window*)))
		  (screen-window-list (selected-screen))))
      (let ((browser (buffer-get buffer 'ASSOCIATED-WITH-BROWSER)))
	(kill-buffer-interactive buffer)
	(if (maybe-select-browser browser)
	    (let ((buffer (current-buffer)))
	      (if (maybe-select-browser (buffer-get buffer 'BROWSER))
		  (maybe-select-browser
		   (buffer-get buffer 'ASSOCIATED-WITH-BROWSER))))))
      (clear-current-message!)
      (let ((cont (maybe-get-continuation buffer))
	    (thread (buffer-get buffer 'THREAD)))
	(if (and thread cont)
	    (if (eq? thread editor-thread)
		(signal-thread-event editor-thread
				     (lambda () (cont unspecific)))
		(restart-thread thread #f #f)))))))

;;;Just gets the current browser continuation if it exists
(define (maybe-get-continuation buffer)
  (let* ((browser (buffer-get buffer 'BROWSER))
	 (object (browser/object browser)))
    (if (continuation? object)
	object
	#f)))

(define (maybe-select-browser browser)
  (if (and (browser? browser)
	   (buffer-alive? (browser/buffer browser)))
      (begin
	(select-buffer (browser/buffer browser))
	((ref-command browser-select-line))
	false)
      true))

;;;addition for when debugger is called from a break
;;;should quit the debugger, and give the continuation
;;;a value to proceed with (restarting that thread)
;;;if in a normal error debug it will envoke the standard
;;;restarts
(define-command quit-with-restart-value
  "Quit the breakpoint, exiting with a specified value."
  ()
  (lambda ()
    (let* ((buffer (current-buffer))
	   (thread (buffer-get buffer 'THREAD)))
      (if (thread? thread)
	  (let ((value (prompt-for-expression-value 
			"Please enter a value to continue with:  "))
		(cont (maybe-get-continuation buffer)))
	    (buffer-put! buffer 'THREAD #f)
	    ((ref-command browser-quit))
	    (cond ((eq? thread editor-thread)
		   (signal-thread-event editor-thread (lambda ()
							(cont value))))
		  (else
		   (set! value? #t)
		   (restart-thread thread #t (lambda ()
					       (cont value))))))
	  (invoke-restarts #f)))))

;;;Method for invoking the standard restarts from within the
;;;debugger.
(define (invoke-restarts avoid-deletion?)
    (let* ((mark (current-point))
	   (bline (mark->bline mark))
	   (browser (bline/browser bline))
	   (buffer
	    (1d-table/get (bline/properties bline) 'DESCRIPTION-BUFFER false))
	   (condition
	    (browser/object browser)))
      (if (condition? condition)
	  (fluid-let ((prompt-for-confirmation
		       (lambda (prompt #!optional port)
			 (call-with-interface-port
			  (buffer-end buffer)
			  (lambda (port)
			    (prompt-for-yes-or-no? prompt)))))
		      (prompt-for-evaluated-expression
		       (lambda (prompt #!optional environment port)
			 (call-with-interface-port 
			  (buffer-end buffer) 
			  (lambda (port)
			    (hook/repl-eval #f
					    (prompt-for-expression prompt)
					    (if (unassigned? environment)
						(nearest-repl/environment)
						environment)
					    (nearest-repl/syntax-table))))))
		      (hook/invoke-restart
		       (lambda (continuation arguments)
			 (invoke-continuation continuation
					      arguments
					      avoid-deletion?))))
	    (call-with-interface-port 
	     (let ((buff (new-buffer " *debug*-RESTARTS")))
	       (add-browser-buffer! browser buff)
	       (pop-up-buffer buff)
	       (buffer-start buff))
	     (lambda (port)
	       (write-string "  " port)
	       (write-condition-report condition port)
	       (newline port)
	       (command/condition-restart 
		(make-initial-dstate condition)
		port))))
	  (message "No condition to restart from."))))

;;;
;;;Sort of a kludge, borrowed from arthur's debugger, 
;;;this makes sure that the interface port that the restart
;;;stuff gets called with uses the minibuffer for prompts
(define (call-with-interface-port mark receiver)
  (let ((mark (mark-left-inserting-copy mark)))
    (let ((value (receiver (port/copy interface-port-template mark))))
      (mark-temporary! mark)
      value)))

;;;Another thing borrowed from arthur, calls the cont
;;;and exits the debugger
(define (invoke-continuation continuation arguments avoid-deletion?)
  (let ((buffer (current-buffer)))
    (if (and (not avoid-deletion?)
	     (ref-variable debugger-quit-on-return?))
	((ref-command browser-quit)))
    ((or (buffer-get buffer 'INVOKE-CONTINUATION) apply)
     continuation arguments)))

;;;; Where

(define-command browser-where
  "Select an environment browser for this line's environment."
  ()
  (lambda ()
    (select-buffer
     (bline/environment-browser-buffer (current-selected-line)))))

(define (bline/environment-browser-buffer bline)
  (let ((environment (bline/evaluation-environment bline)))
    (bline/attached-buffer bline 'ENVIRONMENT-BROWSER
      (lambda ()
	(or (list-search-positive (buffer-list)
	      (lambda (buffer)
		(let ((browser (buffer-get buffer 'BROWSER)))
		  (and browser (eq? environment (browser/object browser))))))
	    (environment-browser-buffer environment))))))

(define (bline/attached-buffer bline type make-buffer)
  (let ((buffer (1d-table/get (bline/properties bline) type false)))
    (if (and buffer (buffer-alive? buffer))
	buffer
	(let ((buffer (make-buffer)))
	  (1d-table/put! (bline/properties bline) type buffer)
	  (add-browser-buffer! (bline/browser bline) buffer)
	  buffer))))

(define (current-selected-line)
  (let ((bline (browser/selected-line (buffer-browser (current-buffer)))))
    (if (not bline)
	(editor-error "There is no selected line; please select one."))
    bline))

(define (bline/evaluation-environment bline)
  (let ((get-environment
	 (1d-table/get (bline-type/properties (bline/type bline))
		       'GET-ENVIRONMENT
		       false))
	(lose
	 (lambda () (editor-error "The selected line has no environment."))))
    (if get-environment
	(let ((environment (get-environment bline)))
	  (if (environment? environment)
	      environment
	      (lose)))
	(lose))))

;;;; Browser Lines

(define bline-rtd
  (make-record-type
   "browser-line"
   '(
     ;; Index of this bline within browser lines vector.  #F if line
     ;; is invisible.
     INDEX
     
     ;; Line start within browser buffer.  #F if line is invisible.
     START-MARK

     ;; Object that this line represents.
     OBJECT

     ;; Type of OBJECT.  This type is specific to the browser; it
     ;; tells the browser how to manipulate OBJECT.
     TYPE

     ;; BLINE representing the object that this object is a component
     ;; of, or #F if none.
     PARENT

     ;; Nonnegative integer indicating the depth of this object in
     ;; the component nesting.
     DEPTH

     ;; BLINEs representing the objects that are adjacent to this one
     ;; in the component ordering, or #F if none.
     NEXT
     PREV

     ;; Nonnegative integer indicating the position of this object in
     ;; the component ordering.
     OFFSET

     PROPERTIES)))

(define bline? (record-predicate bline-rtd))
(define bline/index (record-accessor bline-rtd 'INDEX))
(define set-bline/index! (record-modifier bline-rtd 'INDEX))
(define bline/start-mark (record-accessor bline-rtd 'START-MARK))
(define set-bline/start-mark! (record-modifier bline-rtd 'START-MARK))
(define bline/object (record-accessor bline-rtd 'OBJECT))
(define bline/type (record-accessor bline-rtd 'TYPE))
(define bline/parent (record-accessor bline-rtd 'PARENT))
(define bline/depth (record-accessor bline-rtd 'DEPTH))
(define bline/next (record-accessor bline-rtd 'NEXT))
(define bline/prev (record-accessor bline-rtd 'PREV))
(define bline/offset (record-accessor bline-rtd 'OFFSET))
(define bline/properties (record-accessor bline-rtd 'PROPERTIES))

(define (bline/browser bline)
  (buffer-browser (mark-buffer (bline/start-mark bline))))

(define make-bline
  (let ((constructor
	 (record-constructor
	  bline-rtd
	  '(START-MARK OBJECT TYPE PARENT DEPTH NEXT PREV OFFSET PROPERTIES)))
	(set-bline/next! (record-modifier bline-rtd 'NEXT)))
    (lambda (object type parent prev)
      (let ((bline
	     (constructor false object type
			  parent (if parent (+ (bline/depth parent) 1) 0)
			  false prev (if prev (+ (bline/offset prev) 1) 0)
			  (make-1d-table))))
	(if prev
	    (set-bline/next! prev bline))
	bline))))

;;;; Browser Line Editing

(define (browser/n-lines browser)
  (vector-length (browser/lines browser)))

(define (browser/line browser index)
  (vector-ref (browser/lines browser) index))

(define (mark->bline mark)
  (let ((blines (browser/lines (buffer-browser (mark-buffer mark))))
	(group (mark-group mark))
	(index (mark-index mark)))
    (let loop ((low 0) (high (vector-length blines)))
      (and (fix:< low high)
	   (let ((middle (fix:quotient (fix:+ low high) 2)))
	     (let ((bline (vector-ref blines middle)))
	       (let ((ls (mark-index (bline/start-mark bline))))
		 (cond ((fix:< index ls) (loop low middle))
		       ((fix:<= index (line-end-index group ls)) bline)
		       (else (loop (fix:+ middle 1) high))))))))))

(define (mark->bline-index mark)
  (let ((bline (mark->bline mark)))
    (and bline
	 (bline/index bline))))

(define (delete-blines browser start end)
  (if (< start end)
      (let ((bv (browser/lines browser)))
	(if (subvector-find-next-element bv start end
					 (browser/selected-line browser))
	    (unselect-bline browser))
	(let ((nbv (vector-length bv)))
	  (let ((bv* (make-vector (- nbv (- end start)))))
	    (do ((i 0 (+ i 1)))
		((= i start))
	      (vector-set! bv* i (vector-ref bv i)))
	    (do ((i end (+ i 1))
		 (j start (+ j 1)))
		((= i nbv))
	      (let ((bline (vector-ref bv i)))
		(set-bline/index! bline j)
		(vector-set! bv* j bline)))
	    (let ((start-mark (bline/start-mark (vector-ref bv start))))
	      (with-buffer-open start-mark
		(lambda ()
		  (delete-string
		   start-mark
		   (if (< end nbv)
		       (bline/start-mark (vector-ref bv end))
		       (buffer-end (browser/buffer browser)))))))
	    (set-browser/lines! browser bv*))))))

(define (insert-blines browser index blines)
  (if (not (null? blines))
      (let ((bv (browser/lines browser))
	    (n-blines (length blines)))
	(let ((nbv (vector-length bv)))
	  (let ((bv* (make-vector (+ nbv n-blines))))
	    (do ((i 0 (+ i 1)))
		((= i index))
	      (vector-set! bv* i (vector-ref bv i)))
	    (do ((blines blines (cdr blines))
		 (i index (+ i 1)))
		((null? blines))
	      (let ((bline (car blines)))
		(set-bline/index! bline i)
		(vector-set! bv* i bline)))
	    (do ((i index (+ i 1))
		 (j (+ index n-blines) (+ j 1)))
		((= i nbv))
	      (let ((bline (vector-ref bv i)))
		(set-bline/index! bline j)
		(vector-set! bv* j bline)))
	    (let ((start-mark
		   (if (< index nbv)
		       (bline/start-mark (vector-ref bv index))
		       (buffer-end (browser/buffer browser)))))
	      (with-buffer-open start-mark
		(lambda ()
		  (let ((mark (mark-left-inserting-copy start-mark))
			(columns 79))
		    (for-each
		     (lambda (bline)
		       (let ((index (mark-index mark))
			     (indentation
			      (+ 1
				 (* summary-indentation-increment
				    (bline/depth bline)))))
			 (insert-horizontal-space indentation mark)
			 (let ((summary
				(with-output-to-truncated-string
				    (max summary-minimum-columns
					 (- columns indentation 4))
				  (lambda ()
				    ((bline-type/write-summary
				      (bline/type bline))
				     bline
				     (current-output-port))))))
			   (insert-string (cdr summary) mark)
			   (if (car summary)
			       (insert-string " ..." mark)))
			 (insert-newline mark)
			 (set-bline/start-mark!
			  bline
			  (make-permanent-mark (mark-group mark) index true))))
		     blines)
		    (mark-temporary! mark)))))
	    (set-browser/lines! browser bv*))))))

(define summary-indentation-increment 3)
(define summary-minimum-columns 10)

;;;; Browser Line Types

(define bline-type-rtd
  (make-record-type
   "browser-element-type"
   '(
     ;; Procedure that is called to generate the browser line that
     ;; represents this object.  Two arguments: BLINE and PORT.  The
     ;; summary of BLINE is written to PORT.  The summary should fit
     ;; on one line; PORT will limit the number of characters that can
     ;; be printed so that it fits.
     WRITE-SUMMARY

     ;; Procedure that is called to generate a full description of the
     ;; object.  Two arguments: BLINE and PORT.  This description may
     ;; use multiple lines; it will be presented in its own buffer, so
     ;; the presentation style is not very constrained.  This
     ;; component may be #F to indicate that the object is not
     ;; normally viewed.
     WRITE-DESCRIPTION

     ;; Procedure that generates the standard mark at which the point
     ;; should be placed when this object is selected.  One argument:
     ;; BLINE.  This component may be a nonnegative exact integer
     ;; meaning an offset from the START-MARK of the bline.
     SELECTION-MARK

     PROPERTIES
     )))

(define bline-type/write-summary
  (record-accessor bline-type-rtd 'WRITE-SUMMARY))

(define bline-type/write-description
  (record-accessor bline-type-rtd 'WRITE-DESCRIPTION))

(define bline-type/selection-mark
  (record-accessor bline-type-rtd 'SELECTION-MARK))

(define bline-type/properties
  (record-accessor bline-type-rtd 'PROPERTIES))

(define make-bline-type
  (let ((constructor
	 (record-constructor
	  bline-type-rtd
	  '(WRITE-SUMMARY WRITE-DESCRIPTION SELECTION-MARK PROPERTIES))))
    (lambda (write-summary write-description selection-mark)
      (constructor write-summary
		   write-description
		   selection-mark
		   (make-1d-table)))))

(define (make-continuation-bline expander parent prev)
  (make-bline expander bline-type:continuation-line parent prev))

(define (continuation-line/write-summary bline port)
  bline
  (write-string "--more--" port))

(define bline-type:continuation-line
  (make-bline-type continuation-line/write-summary false 0))

(define (bline/continuation? bline)
  (eq? (bline/type bline) bline-type:continuation-line))

(define (replace-continuation-bline bline)
  (let ((browser (bline/browser bline))
	(index (bline/index bline))
	(expansion ((bline/object bline))))
    (delete-blines browser index (+ index 1))
    (insert-blines browser index expansion)
    (car expansion)))

;;;; Control Variables

(define (boolean-or-ask? object)
  (or (boolean? object)
      (eq? 'ASK object)))

(define-variable debugger-one-at-a-time?
  "Allow only one debugger buffer to exist at a given time.
#T means delete an existing debugger buffer before making a new one.
#F means leave existing buffers alone.
'ASK means ask user what to do each time."
  'ASK
  boolean-or-ask?)

(define-variable debugger-start-on-error?
  "#T means start the debugger whenever there is an evaluation error.
#F means ignore evaluation errors.
'ASK means ask user what to do for each evaluation error."
  'ASK
  boolean-or-ask?)

(define-variable debugger-max-subproblems
  "Maximum number of subproblems displayed when debugger starts.
Set this variable to #F to disable this limit."
  10
  (lambda (object)
    (or (not object)
	(and (exact-integer? object)
	     (> object 0)))))

(define-variable debugger-confirm-return?
  "True means prompt for confirmation in \"return\" commands.
The prompting occurs prior to returning the value."
  true
  boolean?)

(define-variable debugger-quit-on-return?
  "True means quit debugger when executing a \"return\" command.
Quitting the debugger kills the debugger buffer and any associated buffers."
  true
  boolean?)

(define-variable debugger-quit-on-restart?
  "True means quit debugger when executing a \"restart\" command.
Quitting the debugger kills the debugger buffer and any associated buffers."
  true
  boolean?)
 
;;;Limited this bc the bindings are now pretty-printed
(define-variable environment-package-limit
  "Packages with more than this number of bindings will be abbreviated.
Set this variable to #F to disable this abbreviation."
  10
  (lambda (object)
    (or (not object)
	(exact-nonnegative-integer? object))))

(define-variable debugger-show-help-message?
  "True means show the help message, false means don't."
  #T
  boolean?)

(define-variable debugger-start-new-screen?
  "#T means start a new-screen whenever the debugger is invoked.  
#F means continue in same screen.  
'ASK means ask user whether to start new-screen."
  #T
  boolean-or-ask?)

(define-variable debugger-prompt-geometry?
  "#T means always prompt for screen geometry.
#F means use default screen geometry"
  #F
  boolean?)

(define-variable debugger-sticky-prompt
  "#T means don't change variable debugger-prompt-geometry?.
#F means change debugger-prompt-geometry? if true after the first time."
  #F
  boolean?)

(define-variable debugger-hide-system-code?
  "True means don't show subproblems created by the runtime system."
  #T
  boolean?)

(define-variable new-screen-geometry
  "Geometry string for screens created by the debugger.
False means use default."
  "80x75-0+0"
  (lambda (object)
    (or (not object)
	(string? object))))

(define-variable debugger-debug-evaluations?
  "True means evaluation errors in a debugger buffer start new debuggers."
  #F
  boolean?); *not currently used

(define-variable debugger-quit-on-restart?
  "True means quit debugger when executing a \"restart\" command."
  #T
  boolean?)

(define-variable subexpression-start-marker
  "Subexpressions are preceeded by this value."
  "#"
  string?)

(define-variable subexpression-end-marker
  "Subexpressions are followed by this value."
  "#"
  string?)

(define-variable debugger-show-frames?
  "If true show the environment frames in the description buffer. 
If false show the bindings without frames."
  #T
  boolean?)

;;;; Pred's

;;;Used to check if the debugger has been started from
;;;within the debugger, a bit of a kludge
(define (debugger-evaluation-buffer? buffer-name)
  (let ((debug-pattern
	 " \\*debug\\*-[0-9]+")
	(where-pattern
	 " \\*where\\*-[0-9]+"))
    (or (re-match-string-forward 
	 (re-compile-pattern debug-pattern false) false false buffer-name)
	(re-match-string-forward 
	 (re-compile-pattern where-pattern false) false false buffer-name))))

;;;Makes sure that the prompted geometry is legal
(define (geometry? geometry)
  (let ((geometry-pattern
	 "[0-9]+x[0-9]+\\(-[0-9]+\\|+[0-9]+\\|\\)\\(-[0-9]+\\|+[0-9]+\\|\\)"))
    (re-match-string-forward  (re-compile-pattern geometry-pattern false) 
			      false 
			      false
			      geometry)))

;;;Determines if a frame is marked
(define (system-frame? stack-frame)
  (stack-frame/repl-eval-boundary? stack-frame))

;;;Bad implementation to determine for breaks
;;;if a value to proceed with is desired
(define value? #f)			     

(define (invalid-subexpression? subexpression)
  (or (debugging-info/undefined-expression? subexpression)
      (debugging-info/unknown-expression? subexpression)))

(define (invalid-expression? expression)
  (or (debugging-info/undefined-expression? expression)
      (debugging-info/compiled-code? expression)))

;;;; Help Messages

;;;The help messages for the debugger


(define where-help-message
"     COMMANDS:  ? - Help   q - Quit Environment browser

This is an environment browser buffer.

Lines identify environment frames.
The buffer below shows the bindings of the selected environment.
-----------
")

(define debugger-help-message
"     COMMANDS:   ? - Help   q - Quit Debugger   e - Environment browser

This is a debugger buffer.

Lines identify stack frames, most recent first.

   Sx means frame is in subproblem number x
   Ry means frame is reduction number y

The buffer below describes the current subproblem or reduction.
-----------")

;;;; Debugger Entry

;;;many changes
;;;see comments after each change
(define (continuation-browser-buffer object #!optional thread)
  ;;**NOTE: if a thread is passed that means it is being called by a breakpoint
  (let ((in-debugger? 
	 (debugger-evaluation-buffer? (buffer-name (current-buffer))))
	(break-thread 
	 (if (default-object? thread)
	     #f
	     thread)))
    ;;the above sets the break-thread
    (set! value? #f)
    (let ((buffers (find-debugger-buffers)))
      (if (and (not (null? buffers))
	       (null? (cdr buffers))
	       (if (eq? 'ASK (ref-variable debugger-one-at-a-time?))
		   (prompt-for-confirmation?
		    "Another debugger buffer exists.  Delete it")
		   (ref-variable debugger-one-at-a-time?)))
	  (fluid-let ((find-debugger-buffers (lambda () '()))); kludge, works
	    ;;otherwise, killing the buffer will delete the screen also
	    (kill-buffer (car buffers)))))
    (let ((debug-screen (if in-debugger?
			    (selected-screen)
			    (make-debug-screen))))
      ;;sets up the debug screen
      (let ((browser
	     (make-browser "*debug*"
			   (ref-mode-object continuation-browser)
			   object))
	    (blines
	     (continuation->blines
	      (cond ((continuation? object)
		     object)
		    ((condition? object)
		     (condition/continuation object))
		    (else
		     (error:wrong-type-argument object
						"condition or continuation"
						continuation-browser-buffer)))
	      (ref-variable debugger-max-subproblems))))
	(let ((buffer (browser/buffer browser)))
	  (let ((mark (buffer-end buffer)))
	    (with-buffer-open mark
	      (lambda ()
		(call-with-output-mark
		 mark
		 (lambda (port)
		   (if (ref-variable debugger-show-help-message?)
		       (write-string debugger-help-message port))
		   (newline port)
		   (if (condition? object)
		       (begin 
			 (write-string 
			  "The *ERROR* that started the debugger is:"
			  port)
			 (newline port)
			 (newline port)
			 (write-string "  " port)
			 (with-output-props '((highlighted))
			   (lambda () (write-condition-report object port))
			   port)
			 (newline port)))
		   (newline port))))))
	  (insert-blines browser 0 blines)
	  (buffer-put! buffer 'THREAD break-thread) ;  adds thread
	  (select-screen debug-screen)
	  (select-window (screen-window0 debug-screen))
	  (if (null? blines)
	      (set-buffer-point! buffer (buffer-end buffer))
	      (select-bline (car blines)))
	  buffer)))))

(define (find-debugger-buffers)
  (list-transform-positive (buffer-list)
    (let ((debugger-mode (ref-mode-object continuation-browser)))
      (lambda (buffer)
	(eq? (buffer-major-mode buffer) debugger-mode)))))

;;;Determines if necessary to make a new screen and if so makes it
(define (make-debug-screen)
  (cond ((> (length (screen-list)) 1) 
	 (screen1+ (selected-screen)))
	((and (multiple-screens?)
	      (if (eq? (ref-variable debugger-start-new-screen?) 'ASK)
		  (prompt-for-confirmation? "Start new Xwindow?")
		  (ref-variable debugger-start-new-screen?)))
	 (let* ((def-geometry (ref-variable new-screen-geometry))
		(geometry 
		 (if (ref-variable debugger-prompt-geometry?)
		     (let ((prompted-geometry 
			    (prompt-for-string 
			     "Please enter a geometry" def-geometry)))
		       (if (geometry? prompted-geometry)
			   (begin
			     (if (not (ref-variable debugger-sticky-prompt))
				 (set-variable! debugger-prompt-geometry? #f))
			     (set-variable! new-screen-geometry 
					    prompted-geometry)
			     prompted-geometry)
			   (begin
			     (message "Invalid geometry! Using default")
			     def-geometry)))
		     def-geometry)))
	   (make-screen (current-buffer) geometry)))
	(else (selected-screen))))

;;;Procedure that actually calls the cont-browser with the continuation
;;;and stops the thread when a break-pt is called
(define (break-to-debugger #!optional pred-thunk)
  (let ((pred 
	 (if (default-object? pred-thunk)
	     (prompt-for-yes-or-no?
	      "Enter the continuation browser at breakpoint")
	     (pred-thunk))))
    (if pred
	(with-simple-restart 'CONTINUE "Return from BKPT."
	  (lambda ()
	    (let ((thread (current-thread)))
	      (call-with-current-continuation 
	       (lambda (cont)
		 (select-buffer 
		  (continuation-browser-buffer cont thread))
		 (if (eq? thread editor-thread)
		     (abort-current-command)
		     (stop-current-thread))
		 (if value?
		     (abort-current-command))))))))))

;;;Calls the break pt thing with a pred thunk and a thunk to do
(define (with-break-on pred-thunk val-thunk)
  (let ((val value?)
	(bkvalue (break-to-debugger pred-thunk)))
    (set! value? #f)
    (if val
	bkvalue 
	(val-thunk))))

;;;Calls the break pt thing with a pred-thunk a proc and args
(define (call-with-break pred-thunk proc . args)
  (let ((val  value?)
	(bkvalue (break-to-debugger pred-thunk)))
    (set! value? #f)
    (if val
	bkvalue 
	(apply proc args))))

(define (select-continuation-browser-buffer object)
  (select-buffer (continuation-browser-buffer object)))

(define-command browse-continuation
  "Invoke the continuation-browser on CONTINUATION."
  "XBrowse Continuation"
  select-continuation-browser-buffer)

(define (debug-scheme-error condition error-type-name)
  (if starting-debugger?
      (quit-editor-and-signal-error condition)
      (begin
	(editor-beep)
	(if (if (eq? 'ASK (ref-variable debugger-start-on-error?))
		(prompt-for-confirmation? "Start debugger")
		(ref-variable debugger-start-on-error?))
	    (begin
	      (fluid-let ((starting-debugger? true))
		(select-continuation-browser-buffer condition))
	      (message error-type-name " error")))
	(abort-current-command))))

(define starting-debugger? false)

;;;; Continuation Browser Mode

(define-major-mode continuation-browser read-only "Debug"
  "                     ********Debugger Help********

Commands:

`mouse-button-1'
     Select a subproblem or reduction and display information in the
     description buffer.

`C-n'
`down-arrow'
     Move the cursor down the list of subproblems and reductions and
     display info in the description buffer.

`C-p'
`up-arrow'
     Move the cursor up the list of subproblems and reductions and
     display info in the description buffer.

`e'
     Show the environment structure.

`q'
     Quit the debugger, destroying its window.

`p'
     Invoke the standard restarts.

`SPC'
     Display info on current item in the description buffer.

`?'
     Display help information.

   Each line beginning with `S' represents either a subproblem or stack
frame.  A subproblem line may be followed by one or more indented lines
(beginning with the letter `R') which represent reductions associated
with that subproblem.  The subproblems are indexed with the natural
numbers.  To obtain a more complete description of a subproblem or
reduction, click the mouse on the desired line or move the cursor to the
line using the arrow keys (or `C-n' and `C-p').  The description buffer
will display the additional information.

   The description buffer contains three major regions.  The first
region contains a pretty printed version of the current expression. The
current subproblem within the expression is highlighted.  The second
region contains a representation of the frames of the environment of the
current expression.  The bindings of each frame are listed below the
frame header.  If there are no bindings in the frame, none will be
listed.  The frame of the current expression is preceeded with ==>.

   The bottom of the description buffer contains a region for evaluating
expressions in the environment of the selected subproblem or reduction.
This is the only portion of the buffer where editing is possible.  This
region can be used to find the values of variables in different
environments; you cannot, however, use mutators (set!, etc.) on compiled
code.

   Typing  `e' creates a new buffer in which you may browse through the
current environment.  In this new buffer, you can use the mouse, the
arrows, or `C-n' and `C-p' to select lines and view different
environments.  The environments listed are the same as those in the
description buffer.  If the selected environment structure is too large
to display (if there are more than `environment-package-limit' items in
the environment) an appropriate message is displayed.  To display the
environment in this case, set the `environment-package-limit' variable
to  `#f'.  This process is initiated by the command `M-x set-variable'.
 You can not use `set!' to set the variable because it is an editor
variable and does not exist in the current scheme environment.  At the
bottom of the new buffer is a region for evaluating expressions similar
to that of the description buffer.

   Type `q' to quit the debugger, killing its primary buffer and any
others that it has created.

   NOTE: The debugger creates discription buffers in which debugging
information is presented.  These buffers are given names beginning with
spaces so that they do not appear in the buffer list; they are
automatically deleted when you quit the debugger.  If you wish to keep
one of these buffers, simply rename it using `M-x rename-buffer': once
it has been renamed, it will not be deleted automatically."
  )


(define-key 'continuation-browser #\p 'quit-with-restart-value)

(define-key 'continuation-browser down 'browser-next-line)

(define-key 'continuation-browser up 'browser-previous-line)

(define-key 'continuation-browser x-button1-down 'debugger-mouse-select-bline)
(define-key 'continuation-browser #\c-n 'browser-next-line)
(define-key 'continuation-browser #\c-p 'browser-previous-line)
(define-key 'continuation-browser #\? 'describe-mode)
(define-key 'continuation-browser #\q 'browser-quit)
(define-key 'continuation-browser #\space 'browser-select-line)
(define-key 'continuation-browser #\e 'browser-where)

;;;; Subproblems

;; A continuation consists of subproblems.  A subproblem has
;; expression information that identifies what the subproblem means.
;; It additionally has reductions and an environment.  Similarly,
;; reductions have expression and environment information.
;; Environments consist of environment frames, and each frame consists
;; of bindings.  Subproblems, reductions, and environment frames are
;; ordered; bindings are not.

;;;Stops from displaying subproblems past marked frame by default
(define (continuation->blines continuation limit)
  (let ((beyond-system-code #f))
    (let loop ((frame (continuation/first-subproblem continuation))
	       (prev false)
	       (n 0))
      (if (not frame)
	  '()
	  (let* ((next-subproblem 
		  (lambda (bline)
		    (loop (stack-frame/next-subproblem frame)
			  bline
			  (+ n 1))))
		 (walk-reductions	
		  (lambda (bline reductions)
		    (cons bline
			  (let loop ((reductions reductions) (prev false))
			    (if (null? reductions)
				(next-subproblem bline)
				(let ((bline
				       (make-bline (car reductions)
						   bline-type:reduction
						   bline
						   prev)))
				  (cons bline
					(loop (cdr reductions) bline))))))))
		 (continue
		  (lambda ()
		    (let* ((subproblem (stack-frame->subproblem frame n)))
		      (if debugger:student-walk?
			  (let ((reductions 
				 (subproblem/reductions subproblem)))
			    (if (null? reductions)
				(let ((bline
				       (make-bline subproblem
						   bline-type:subproblem
						   false
						   prev)))
				  (cons bline
					(next-subproblem bline)))
				(let ((bline
				       (make-bline (car reductions)
						   bline-type:reduction
						   false
						   prev)))
				  (walk-reductions bline
						   (if (> n 0)
						       '()
						       (cdr reductions))))))
			  (walk-reductions
			   (make-bline subproblem
				       bline-type:subproblem
				       false
				       prev)
			   (subproblem/reductions subproblem)))))))
	    (cond ((and (not (ref-variable debugger-hide-system-code?))
			(system-frame? frame))
		   (loop (stack-frame/next-subproblem frame)
			 prev
			 n))
		  ((or (and limit (>= n limit))
		       (if (system-frame? frame)
			   (begin (set! beyond-system-code #t) #t)
			   #f)
		       beyond-system-code)
		   (list (make-continuation-bline continue false prev)))
		  (else (continue))))))))

(define subproblem-rtd
  (make-record-type
   "subproblem"
   '(STACK-FRAME EXPRESSION ENVIRONMENT SUBEXPRESSION NUMBER)))

(define subproblem? (record-predicate subproblem-rtd))
(define subproblem/stack-frame (record-accessor subproblem-rtd 'STACK-FRAME))
(define subproblem/expression (record-accessor subproblem-rtd 'EXPRESSION))
(define subproblem/environment (record-accessor subproblem-rtd 'ENVIRONMENT))
(define subproblem/subexpression
  (record-accessor subproblem-rtd 'SUBEXPRESSION))
(define subproblem/number (record-accessor subproblem-rtd 'NUMBER))

(define stack-frame->subproblem
  (let ((constructor
	 (record-constructor
	  subproblem-rtd
	  '(STACK-FRAME EXPRESSION ENVIRONMENT SUBEXPRESSION NUMBER))))
    (lambda (frame number)
      (with-values (lambda () (stack-frame/debugging-info frame))
	(lambda (expression environment subexpression)
	  (constructor frame expression environment subexpression number))))))

(define reduction-rtd
  (make-record-type "reduction" '(SUBPROBLEM EXPRESSION ENVIRONMENT NUMBER)))

(define reduction? (record-predicate reduction-rtd))
(define reduction/subproblem (record-accessor reduction-rtd 'SUBPROBLEM))
(define reduction/expression (record-accessor reduction-rtd 'EXPRESSION))
(define reduction/environment (record-accessor reduction-rtd 'ENVIRONMENT))
(define reduction/number (record-accessor reduction-rtd 'NUMBER))

(define make-reduction
  (record-constructor reduction-rtd
		      '(SUBPROBLEM EXPRESSION ENVIRONMENT NUMBER)))

(define (subproblem/reductions subproblem)
  (let ((frame (subproblem/stack-frame subproblem)))
    (let loop ((reductions (stack-frame/reductions frame)) (n 0))
      (if (pair? reductions)
	  (cons (make-reduction subproblem
				(caar reductions)
				(cadar reductions)
				n)
		(loop (cdr reductions) (+ n 1)))
	  '()))))


(define (subproblem/write-summary bline port)
  (let* ((subproblem (bline/object bline))
	 (frame (subproblem/stack-frame subproblem)))
    (if (system-frame? frame)
	(write-string "***************Internal System Code Follows***********" 
		      port)
	(begin
	  (write-string "S" port)
	  (write-string (bline/offset-string (subproblem/number subproblem))
			port)
	  (write-string " " port)
	  (let ((expression (subproblem/expression subproblem))
		(subexpression (subproblem/subexpression subproblem)))
	    (cond ((debugging-info/compiled-code? expression)
		   (write-string ";unknown compiled code" port))
		  ((not (debugging-info/undefined-expression? expression))
		   (print-with-subexpression expression subexpression))
		  ((debugging-info/noise? expression)
		   (write-string ";" port)
		   (write-string ((debugging-info/noise expression) false) 
				 port))
		  (else
		   (write-string ";undefined expression" port))))))))

;;;also marks the subexpression with # #
(define (print-with-subexpression expression subexpression)
  (fluid-let ((*unparse-primitives-by-name?* true))
    (if (invalid-subexpression? subexpression)
	(write (unsyntax expression))
	(let ((sub (write-to-string (unsyntax subexpression))))
	  (write (unsyntax-with-substitutions
		  expression
		  (list
		   (cons subexpression
			 (unparser-literal/make
			  (string-append
			   (ref-variable subexpression-start-marker)
			   sub
			   (ref-variable subexpression-end-marker)))))))))))

(define-structure (unparser-literal
		   (conc-name unparser-literal/)
		   (print-procedure
		    (lambda (state instance)
		      (unparse-string state
				      (unparser-literal/string instance))))
		   (constructor unparser-literal/make))
  string)

(define (subproblem/write-description bline port)
  (let* ((subproblem (bline/object bline))
	 (frame (subproblem/stack-frame subproblem)))
    (cond ((system-frame? frame)
	   (write-string "The subproblems which follow are part of the " port)

	   (write-string "internal system workings." port))
	  (else
	   (write-string "                         SUBPROBLEM LEVEL: " port)
	   (write (subproblem/number subproblem) port)
	   (newline port)
	   (newline port)
	   (let ((expression (subproblem/expression subproblem))
		 (frame (subproblem/stack-frame subproblem)))
	     (cond ((not (invalid-expression? expression))
		    (write-string (if (stack-frame/compiled-code? frame)
				      "COMPILED expression"
				      "Expression")
				  port)
		    (write-string " (from stack):" port)
		    (newline port)
		    (write-string 
		     " Subproblem being executed highlighted.\n"
		     port)
		    (newline port)
		    (let ((subexpression 
			   (subproblem/subexpression subproblem)))
		      (if (invalid-subexpression? subexpression)
			  (debugger-pp expression expression-indentation port)
			  (debugger-pp-highlight-subexpression expression
							       subexpression
							       expression-indentation
							       port))))
		   ((debugging-info/noise? expression)
		    (write-string ((debugging-info/noise expression) true)
				  port))
		   (else
		    (write-string (if (stack-frame/compiled-code? frame)
				      "Compiled expression unknown"
				      "Expression unknown")
				  port)
		    (newline port)
		    (write (stack-frame/return-address frame) port))))
	   (let ((environment (subproblem/environment subproblem)))
	     (if (not (debugging-info/undefined-environment? environment))
		 (begin
		   (newline port)
		   (newline port)
		   (desc-show-environment-name-and-bindings environment port))))))))

(define bline-type:subproblem
  (make-bline-type subproblem/write-summary
		   subproblem/write-description
		   1))

(1d-table/put! (bline-type/properties bline-type:subproblem)
	       'GET-ENVIRONMENT
	       (lambda (bline)
		 (subproblem/environment (bline/object bline))))

;;;; Reductions

(define (reduction/write-summary bline port)
  (let ((reduction (bline/object bline)))
    (if (bline/parent bline)
	(begin
	  (write-string "R" port)
	  (write-string (bline/offset-string (reduction/number reduction))
			port))
	(begin
	  (write-string "S" port)
	  (write-string
	   (bline/offset-string
	    (subproblem/number (reduction/subproblem reduction)))
	   port)))
    (write-string " " port)
    (fluid-let ((*unparse-primitives-by-name?* true))
      (write (unsyntax (reduction/expression reduction)) port))))

(define (reduction/write-description bline port)
  (let ((reduction (bline/object bline)))
    (write-string "              SUBPROBLEM LEVEL: " port)
    (write (subproblem/number (reduction/subproblem reduction)) port)
    (write-string "  REDUCTION NUMBER: " port)
    (write (reduction/number reduction) port)
    (newline port)
    (newline port)
    (write-string "Expression (from execution history):" port)
    (newline port)
    (newline port)
    (debugger-pp (reduction/expression reduction) expression-indentation port)
    (newline port)
    (newline port)
    (desc-show-environment-name-and-bindings (reduction/environment reduction) 
					port)))

(define bline-type:reduction
  (make-bline-type reduction/write-summary
		   reduction/write-description
		   1))

(1d-table/put! (bline-type/properties bline-type:reduction)
	       'GET-ENVIRONMENT
	       (lambda (bline)
		 (reduction/environment (bline/object bline))))

;;;; Environments

(define-command browse-environment
  "Invoke the environment-browser on ENVIRONMENT."
  "XBrowse Environment"
  (lambda (environment)
    (select-buffer (environment-browser-buffer environment))))

;;;adds a help line
(define (environment-browser-buffer object)
  (let ((environment (->environment object)))
    (let ((browser
	   (make-browser "*where*"
			 (ref-mode-object environment-browser)
			 object))
	  (blines (environment->blines environment)))
      
      (let ((buffer (browser/buffer browser)))
	(let ((mark (buffer-end buffer)))
	  (with-buffer-open mark
	    (lambda ()
	      (call-with-output-mark
	       mark
	       (lambda (port)
		 (if (ref-variable debugger-show-help-message?)
		     (write-string where-help-message port))
		 (newline port))))))
	(insert-blines browser 0 blines)
	(if (null? blines)
	    (set-buffer-point! buffer (buffer-end buffer))
	    (select-bline (car blines)))
	buffer))))

(define (environment->blines environment)
  (let loop ((environment environment) (prev false))
    (let ((bline (make-bline environment bline-type:environment false prev)))
      (cons bline
	    (if (eq? true (environment-has-parent? environment))
		(loop (environment-parent environment) bline)
		'())))))

(define-major-mode environment-browser read-only "Environment Browser"
  "             ********Environment Browser Help********

Commands:

`mouse-button-1'
     Select a subproblem or reduction and display information in the
     description buffer.

`C-n'
`down-arrow'
     Move the cursor down the list of subproblems and reductions and
     display info in the description buffer.

`C-p'
`up-arrow'
     Move the cursor up the list of subproblems and reductions and
     display info in the description buffer.

`q'
     Quit the environment browser, destroying its window.

`SPC'
     Display info on current item in the description buffer.

`?'
     Display help information.

   In this buffer, you can use the mouse, the arrows, or `C-n' and
`C-p' to select lines and view different environments.
If the selected environment structure is too large to display (if
there are more than `environment-package-limit' items in the
environment) an appropriate message is displayed.  To display the
environment in this case, set the `environment-package-limit' variable
to  `#f'.  This process is initiated by the command `M-x
set-variable'. You can not use `set!' to set the variable because it
is an editor variable and does not exist in the current scheme
environment. 

   The bottom of the description buffer contains a region for evaluating
expressions in the environment of the selected subproblem or reduction.
This is the only portion of the buffer where editing is possible.  This
region can be used to find the values of variables in different
environments; you cannot, however, use mutators (set!, etc.) on
compiled code. 

   Type `q' to quit the environment browser, killing its primary buffer
and any others that it has created.

NOTE: The environment browser creates discription buffers in which
debugging information is presented.  These buffers are given names 
beginning with spaces so that they do not appear in the buffer list; 
they are automatically deleted when you quit the debugger.  If you wish
to keep one of these buffers, simply rename it using `M-x rename-buffer': 
once it has been renamed, it will not be deleted automatically.")



(define-key 'environment-browser down 'browser-next-line)

(define-key 'environment-browser up 'browser-previous-line)

(define-key 'environment-browser x-button1-down 'debugger-mouse-select-bline)
(define-key 'environment-browser #\c-n 'browser-next-line)
(define-key 'environment-browser #\c-p 'browser-previous-line)
(define-key 'environment-browser #\? 'describe-mode)
(define-key 'environment-browser #\q 'browser-quit)
(define-key 'environment-browser #\space 'browser-select-line)


(define (environment/write-summary bline port)
  (write-string "E" port)
  (write-string (bline/offset-string (bline/offset bline)) port)
  (write-string " " port)
  (show-environment-name (bline/object bline) port))

(define (environment/write-description bline port)
  (let ((environment (bline/object bline)))

    (show-environment-name-and-bindings environment port)
))

(define (show-environment-name-and-bindings environment port)
  (show-environment-name environment port)
  (newline port)
  (newline port)
  (let ((names (environment-bound-names environment))
	(package (environment->package environment))
	(finish (lambda (names) 
		  (newline port)
		  (for-each (lambda (name)
			      (myprint-binding name
					       (environment-lookup environment name)
					       environment
					       port))
			    names))))
    (cond ((null? names)
	   (write-string " has no bindings" port))
	  ((and package
		(let ((limit 
		       (ref-variable
			environment-package-limit)))
		  (and limit
		       (let ((n (length names)))
			 (and (>= n limit)
			      (begin
				(write-string " has " port)
				(write n port)
				(write-string " bindings (first" port)
				(write limit port)
				(write-string " shown):" port)
				(finish (list-head names limit))
				true)))))))
	  (else
	   (write-string "  BINDINGS:" port)
	   (finish
	    (if package
		(sort names
		      (lambda (x y)
			(string<? (symbol->string x)
				  (symbol->string y))))
		names)))))
  (newline port)
  (newline port)
  (write-string 
   "---------------------------------------------------------------------"
   port))

;;;This does some stuff who's end product is to pp the bindings
(define (myprint-binding name value environment port)
    (let ((x-size (output-port/x-size port)))
      (newline port)
      (write-string
       (let ((name1
	      (output-to-string 
	       (quotient x-size 2)
	       (lambda ()
		 (write-dbg-name name (current-output-port))))))
	 (if (unassigned-reference-trap? value)
	     (string-append name1 " is unassigned")
	     (let* ((s (string-append name1 " = "))
		    (length (string-length s))
		    (pret 
		     (with-output-to-string 
		       (lambda ()
			 (eval `(pp ,name (current-output-port) #t ,length) 
			       environment)))))
	       (string-append 
		s
		(string-tail pret (+ length 1))))))
       port)
      (newline port)))

(define bline-type:environment
  (make-bline-type environment/write-summary
		   environment/write-description
		   1))

(1d-table/put! (bline-type/properties bline-type:environment)
	       'GET-ENVIRONMENT
	       bline/object)

(define (bline/offset-string number)
  (let ((string (number->string number)))
    (let ((n (- offset-string-min (string-length string))))
      (if (> n 0)
	  (string-append string (make-string n #\space))
	  string))))

(define offset-string-min
  2)

(define (with-buffer-open mark thunk)
  (with-read-only-defeated mark thunk)
  (buffer-not-modified! (mark-buffer mark)))

(define (desc-show-environment-name-and-bindings environment port)
  (write-string "---------------------------------------------------------------------"
	      port)  
  (if (ref-variable debugger-show-frames?)
      (show-frames-and-bindings environment port)
      (print-the-local-bindings environment port))
  (newline port)
  (write-string "---------------------------------------------------------------------"
		port))


			 
(define (show-frames-and-bindings environment port)
  (define (envs environment)
    (if  (eq? true (environment-has-parent? environment))
	 (cons environment (envs (environment-parent environment))) ;
	 '()))
  (let ((env-list (envs environment))
	(depth 0))
    (map (lambda (env) 
	   (let ((ind (make-string (* 2 depth) #\space)))
	     (newline port)
	     (if (eq? env environment)
		 (write-string (if (< 2 (string-length ind))
				   (string-append 
				    (string-tail ind 2) "==> ")
				   "==> ")
			       port)
		 (write-string ind port))
	     (show-environment-name env port)
	     (newline port)
	     (set! depth (1+ depth))
	     (show-environmend-bindings-with-ind env ind port)))
	 env-list)))


(define (print-the-local-bindings environment port)
  (let ((names (get-all-local-bindings environment)))
    (let ((n-bindings (length names))
	  (finish
	   (lambda (names)
	     (for-each (lambda (name)
			 (let loop ((env environment))
			   (if (environment-bound? env name)
			       (print-binding-with-ind name
						       (environment-lookup env name)
						       "  "
						       port)
			       (loop (environment-parent env)))))
		       names))))
      (newline port)
      (show-environment-name environment port)
      (cond ((zero? n-bindings)
	     (write-string "\n    has no bindings\n" port))
	    ((> n-bindings (ref-variable environment-package-limit)))
	    (else
	     (write-string "\n\n  Local Bindings:\n" port)
	     (finish names))))))

(define (show-environment-name environment port)
  (write-string "ENVIRONMENT " port)
  (let ((package (environment->package environment)))
    (if package
	(begin
	  (write-string "named: " port)
	  (write (package/name package) port))
	(begin
	  (write-string "created by " port)
	  (print-user-friendly-name environment port)))))

(define (get-all-local-bindings environment)
  (define (envs environment)
    (if  (eq? true (environment-has-parent? environment))
	 (cons environment (envs (environment-parent environment))) ;
	 '()))
  (let* ((env-list (envs environment))
	 (names1 (map (lambda (envir)
			(let ((names (environment-bound-names envir)))
			  (if (< (length names) 
				 (ref-variable environment-package-limit))
			      names
			      '())))
		      env-list))
	 (names2 (reduce append '() names1))
	 (names3 (let loop ((l names2))
		     (if (null? l)
			 l
			 (cons (car l) (loop (delete (car l) l))))))
	 (names4 (sort names3
		       (lambda (x y)
			 (string<? (symbol->string x)
				   (symbol->string y))))))
    names4))


(define (show-environmend-bindings-with-ind environment ind port)
  (let ((names (environment-bound-names environment)))
    (let ((n-bindings (length names))
	  (finish
	   (lambda (names)
	     (newline port)
	     (for-each (lambda (name)
			 (print-binding-with-ind name
						 (environment-lookup environment name)
						 ind
						 port))
		       names))))
      (cond ((zero? n-bindings)
	     #|(write-string (string-append ind "   has no bindings") port)
	     (newline port)|#)
	    ((> n-bindings (ref-variable environment-package-limit))
	     (write-string (string-append ind "   has ") port)
	     (write n-bindings port)
	     (write-string 
	      " bindings (see editor variable environment-package-limit) " port)
	     (newline port))
	    (else
	     (finish names))))))
      
(define (print-binding-with-ind name value ind port)
  (let ((x-size (- (output-port/x-size port) (string-length ind) 4)))
    (write-string (string-append ind "    ")
		  port)
    (write-string
     (let ((name
	    (output-to-string (quotient x-size 2)
	      (lambda ()
		(write-dbg-name name (current-output-port))))))
       (if (unassigned-reference-trap? value)
	   (string-append name " is unassigned")
	   (let ((s (string-append name " = ")))
	     (string-append
	      s
	      (output-to-string (max (- x-size (string-length s)) 0)
		(lambda ()
		  (write value)))))))
     port)
    (newline port)))


;;;; Interface Port

(define (operation/write-char port char)
  (region-insert-char! (port/state port) char))

(define (operation/prompt-for-confirmation port prompt)
  port
  (prompt-for-confirmation prompt))

(define (operation/prompt-for-expression port prompt)
  port
  (prompt-for-expression prompt))

(define interface-port-template
  (make-output-port
   `((WRITE-CHAR ,operation/write-char)
     (PROMPT-FOR-CONFIRMATION ,operation/prompt-for-confirmation)
     (PROMPT-FOR-EXPRESSION ,operation/prompt-for-expression))
   false))


;; Edwin Variables:
;; scheme-environment: '(edwin debugger)
;; scheme-syntax-table: (access edwin-syntax-table (->environment '(edwin)))
;; End:
