;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/debug.scm,v 1.3 1992/08/20 22:21:33 cph Exp $
;;;
;;;	Copyright (c) 1992 Massachusetts Institute of Technology
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

(define (kill-browser-buffer buffer)
  (let ((browser (buffer-get buffer 'BROWSER)))
    (if browser
	(for-each kill-buffer (browser/buffers browser)))))

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
	    (delete-right-char mark)))
	(set-browser/selected-line! browser bline)
	(set-buffer-point! (mark-buffer mark) mark)))
    (let ((buffer (bline/description-buffer bline)))
      (if buffer
	  (pop-up-buffer buffer false)))))

(define (unselect-bline browser)
  (let ((bline (browser/selected-line browser)))
    (if bline
	(let ((mark (bline/start-mark bline)))
	  (with-buffer-open mark
	    (lambda ()
	      (insert-char #\space (mark1+ mark))
	      (delete-right-char mark)))))))

(define (bline/description-buffer bline)
  (let ((buffer
	 (1d-table/get (bline/properties bline) 'DESCRIPTION-BUFFER false)))
    (if (and buffer (buffer-alive? buffer))
	buffer
	(let ((write-description
	       (bline-type/write-description (bline/type bline))))
	  (and write-description
	       (let ((buffer (browser/new-buffer (bline/browser bline) false)))
		 (call-with-output-mark (buffer-start buffer)
		   (lambda (port)
		     (write-description bline port)))
		 (set-buffer-point! buffer (buffer-start buffer))
		 (1d-table/put! (bline/properties bline)
				'DESCRIPTION-BUFFER
				buffer)
		 (buffer-not-modified! buffer)
		 (set-buffer-read-only! buffer)
		 buffer))))))

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
		   (buffer-get buffer 'ASSOCIATED-WITH-BROWSER)))))))))

(define (maybe-select-browser browser)
  (if (and (browser? browser)
	   (buffer-alive? (browser/buffer browser)))
      (begin
	(select-buffer (browser/buffer browser))
	((ref-command browser-select-line))
	false)
      true))

;;;; Evaluators

(define-command browser-evaluator
  "Select an evaluation buffer for this line's environment."
  ()
  (lambda ()
    (select-buffer (bline/evaluation-buffer (current-selected-line)))))

(define (bline/evaluation-buffer bline)
  (let ((environment (bline/evaluation-environment bline)))
    (bline/attached-buffer bline 'EVALUATION-BUFFER
      (lambda ()
	(or (list-search-positive (buffer-list)
	      (lambda (buffer)
		(and (eq? 'EVALUATION-BUFFER
			  (buffer-get buffer 'BROWSER-BUFFER/TYPE))
		     (let ((cmdl (buffer/inferior-cmdl buffer)))
		       (and cmdl
			    (let ((cmdl (cmdl/base cmdl)))
			      (and (repl? cmdl)
				   (eq? environment
					(repl/environment cmdl)))))))))
	    (let ((buffer (new-buffer "*eval*")))
	      (start-inferior-repl!
	       buffer
	       environment
	       (evaluation-syntax-table buffer environment)
	       (cmdl-message/strings
		"You are now in the environment for the selected line"))
	      (buffer-put! buffer 'BROWSER-BUFFER/TYPE 'EVALUATION-BUFFER)
	      buffer))))))

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

(define-variable environment-browser-package-limit
  "Packages with more than this number of bindings will be abbreviated.
Set this variable to #F to disable this abbreviation."
  50
  (lambda (object)
    (or (not object)
	(exact-nonnegative-integer? object))))

;;;; Debugger Entry

(define (continuation-browser-buffer object)
  (let ((buffers (find-debugger-buffers)))
    (if (and (not (null? buffers))
	     (null? (cdr buffers))
	     (if (eq? 'ASK (ref-variable debugger-one-at-a-time?))
		 (prompt-for-confirmation?
		  "Another debugger buffer exists.  Delete it")
		 (ref-variable debugger-one-at-a-time?)))
	(kill-buffer (car buffers))))
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
      (if (condition? object)
	  (let ((mark (buffer-end buffer)))
	    (with-buffer-open mark
	      (lambda ()
		(call-with-output-mark mark
		  (lambda (port)
		    (write-string "The error that started the debugger is:"
				  port)
		    (newline port)
		    (write-string "  " port)
		    (write-condition-report object port)
		    (newline port)
		    (newline port)))))))
      (insert-blines browser 0 blines)
      (if (null? blines)
	  (set-buffer-point! buffer (buffer-end buffer))
	  (select-bline (car blines)))
      buffer)))

(define (find-debugger-buffers)
  (list-transform-positive (buffer-list)
    (let ((debugger-mode (ref-mode-object continuation-browser)))
      (lambda (buffer)
	(eq? (buffer-major-mode buffer) debugger-mode)))))

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
  "This buffer is a Scheme debugger.
Each line beginning with `S' represents a subproblem, or stack frame.
A subproblem line may be followed by one or more indented lines beginning
with `R'; these lines represent reductions associated with that subproblem.
Every subproblem or reduction line has an associated index number,
with the indexes starting at zero for the nearest one.
To see a more complete description of a given subproblem or reduction,
move the cursor to that line using \\[browser-next-line] and \\[browser-previous-line];
when the line you are interested in has been selected, it will be described
more fully in another window.

Type \\[browser-evaluator] to get an evaluation buffer for the selected line.
Type \\[browser-quit] to quit the browser, killing its buffer.

The debugger creates other buffers at various times, to show you descriptions
of subproblems and reductions.  These buffers are given names beginning with a
space so that they do not appear in the buffer list; these auxiliary buffers
are also automatically deleted when you quit the debugger.  If you wish to keep
one of these buffers, just give it another name using \\[rename-buffer]: once
it has been renamed it will not be automatically deleted."
  )

(define-key 'continuation-browser #\c-n 'browser-next-line)
(define-key 'continuation-browser #\c-p 'browser-previous-line)
(define-key 'continuation-browser #\? 'describe-mode)
(define-key 'continuation-browser #\q 'browser-quit)
(define-key 'continuation-browser #\space 'browser-select-line)
(define-key 'continuation-browser #\e 'browser-where)
(define-key 'continuation-browser #\v 'browser-evaluator)

;;;; Subproblems

;; A continuation consists of subproblems.  A subproblem has
;; expression information that identifies what the subproblem means.
;; It additionally has reductions and an environment.  Similarly,
;; reductions have expression and environment information.
;; Environments consist of environment frames, and each frame consists
;; of bindings.  Subproblems, reductions, and environment frames are
;; ordered; bindings are not.

(define (continuation->blines continuation limit)
  (let loop
      ((frame (continuation/first-subproblem continuation))
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
			(let ((reductions (subproblem/reductions subproblem)))
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
	  (if (and limit (>= n limit))
	      (list (make-continuation-bline continue false prev))
	      (continue))))))

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
  (let ((subproblem (bline/object bline)))
    (write-string "S" port)
    (write-string (bline/offset-string (subproblem/number subproblem)) port)
    (write-string " " port)
    (let ((expression (subproblem/expression subproblem)))
      (cond ((debugging-info/compiled-code? expression)
	     (write-string ";unknown compiled code" port))
	    ((not (debugging-info/undefined-expression? expression))
	     (fluid-let ((*unparse-primitives-by-name?* true))
	       (write (unsyntax expression) port)))
	    ((debugging-info/noise? expression)
	     (write-string ";" port)
	     (write-string ((debugging-info/noise expression) false) port))
	    (else
	     (write-string ";undefined expression" port))))))

(define (subproblem/write-description bline port)
  (let ((subproblem (bline/object bline)))
    (write-string "Subproblem level: " port)
    (write (subproblem/number subproblem) port)
    (newline port)
    (let ((expression (subproblem/expression subproblem))
	  (frame (subproblem/stack-frame subproblem)))
      (cond ((not (invalid-expression? expression))
	     (write-string (if (stack-frame/compiled-code? frame)
			       "Compiled expression"
			       "Expression")
			   port)
	     (write-string " (from stack):" port)
	     (newline port)
	     (let ((subexpression (subproblem/subexpression subproblem)))
	       (if (or (debugging-info/undefined-expression? subexpression)
		       (debugging-info/unknown-expression? subexpression))
		   (debugger-pp expression expression-indentation port)
		   (begin
		     (debugger-pp
		      (unsyntax-with-substitutions
		       expression
		       (list (cons subexpression subexpression-marker)))
		      expression-indentation
		      port)
		     (newline port)
		     (write-string " subproblem being executed (marked by "
				   port)
		     (write subexpression-marker port)
		     (write-string "):" port)
		     (newline port)
		     (debugger-pp subexpression
				  expression-indentation
				  port)))))
	    ((debugging-info/noise? expression)
	     (write-string ((debugging-info/noise expression) true) port))
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
	    (show-environment-name environment port))))))

(define subexpression-marker
  (string->symbol "###"))

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
    (write-string "Subproblem level: " port)
    (write (subproblem/number (reduction/subproblem reduction)) port)
    (write-string "  Reduction number: " port)
    (write (reduction/number reduction) port)
    (newline port)
    (write-string "Expression (from execution history):" port)
    (newline port)
    (debugger-pp (reduction/expression reduction) expression-indentation port)
    (newline port)
    (show-environment-name (reduction/environment reduction) port)))

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

(define (environment-browser-buffer object)
  (let ((environment (->environment object)))
    (let ((browser
	   (make-browser "*where*"
			 (ref-mode-object environment-browser)
			 object))
	  (blines (environment->blines environment)))
      (insert-blines browser 0 blines)
      (let ((buffer (browser/buffer browser)))
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
  "This buffer is a Scheme environment browser.
Each line describes one frame in the environment being browsed.
The frames are numbered starting at zero for the innermost frame.
To see a more complete description of a given frame, move the cursor to that
frame's line using \\[browser-next-line] and \\[browser-previous-line];
when the line you are interested in has been selected, it will be described
more fully in another window.

Type \\[browser-evaluator] to get an evaluation buffer for the selected frame.
Type \\[browser-quit] to quit the browser, killing its buffer.

The environment browser creates other buffers at various times, to
show you descriptions of environment frames.  These buffers are given
names beginning with a space so that they do not appear in the buffer
list; these auxiliary buffers are also automatically deleted when you
quit the debugger.  If you wish to keep one of these buffers, just
give it another name using \\[rename-buffer]: once it has been
renamed it will not be automatically deleted.")

(define-key 'environment-browser #\c-n 'browser-next-line)
(define-key 'environment-browser #\c-p 'browser-previous-line)
(define-key 'environment-browser #\? 'describe-mode)
(define-key 'environment-browser #\q 'browser-quit)
(define-key 'environment-browser #\space 'browser-select-line)
(define-key 'environment-browser #\v 'browser-evaluator)

(define (environment/write-summary bline port)
  (write-string "E" port)
  (write-string (bline/offset-string (bline/offset bline)) port)
  (write-string " " port)
  (show-environment-name (bline/object bline) port))

(define (environment/write-description bline port)
  (let ((environment (bline/object bline)))
    (show-environment-name environment port)
    (newline port)
    (write-string "Depth (relative to initial environment): " port)
    (write (bline/offset bline) port)
    (newline port)
    (temporary-message "Computing environment bindings...")
    (let ((names (environment-bound-names environment))
	  (package (environment->package environment)))
      (cond ((null? names)
	     (write-string " has no bindings" port))
	    ((and package
		  (let ((limit
			 (ref-variable
			  environment-browser-package-limit
			  (browser/buffer (bline/browser bline)))))
		    (and limit
			 (let ((n (length names)))
			   (and (>= n limit)
				(begin
				  (write-string " has " port)
				  (write n port)
				  (write-string
				   " bindings (see editor variable environment-browser-package-limit)."
				   port)
				  true)))))))
	    (else
	     (write-string " has bindings:" port)
	     (newline port)
	     (for-each (lambda (name)
			 (print-binding name
					(environment-lookup environment name)
					port))
		       (if package
			   (sort names
				 (lambda (x y)
				   (string<? (symbol->string x)
					     (symbol->string y))))
			   names)))))
    (append-message "done")))

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