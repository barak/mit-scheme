;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/iserch.scm,v 1.1 1989/03/14 08:01:07 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989 Massachusetts Institute of Technology
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

;;;; Incremental Search Commands

(declare (usual-integrations))

(define-command ("^R Incremental Search")
  "Search for character string as you type it.
C-Q quotes special characters.  Rubout cancels last character.
C-S repeats the search, forward, and C-R repeats it backward.
C-R or C-S with search string empty changes the direction of search
 or brings back search string from previous search.
Altmode exits the search.
Other Control and Meta chars exit the search and then are executed.
If not all the input string can be found, the rest is not discarded.
 You can rub it out, discard it all with C-G, exit,
 or use C-R or C-S to search the other way.
Quitting a successful search aborts the search and moves point back;
 quitting a failing search just discards whatever input wasn't found."
  (incremental-search true))

(define-command ("^R Reverse Search")
  "Incremental Search Backwards.
Like \\[^R Incremental Search] but in reverse."
  (incremental-search false))

(define-command ("^R I-Search Append Char")
  "Append this character to the current string being searched."
  (i-search-append-char (current-command-char)))

(define-command ("^R I-Search Append Newline")
  "Append this character to the current string being searched."
  (i-search-append-char #\Newline))

(define-command ("^R I-Search Append Word")
  "Append the next word to the current string being searched."
  (i-search-append-string
   (let ((end-point (search-state-end-point current-search-state)))
     (extract-string end-point (forward-word end-point 1 'LIMIT)))))

(define-command ("^R I-Search Append Line")
  "Append the rest of the line to the current string being searched."
  (i-search-append-string
   (let ((end-point (search-state-end-point current-search-state)))
     (extract-string end-point
		     (line-end end-point
			       (if (line-end? end-point) 1 0)
			       'LIMIT)))))

(define-command ("^R I-Search Quote Character")
  "Append a quoted character to the current string being searched."
  (i-search-append-char (with-editor-interrupts-disabled keyboard-read-char)))

(define-command ("^R I-Search Editor Command")
  "Exit search and push this character back for normal processing."
  (incremental-search:terminate! current-search-state (current-command-char)))

(define-command ("^R I-Search Next Occurrence")
  "Search for the next occurrence of the current search string."
  (set-current-search-state!
   (incremental-search:next-occurrence current-search-state))
  (i-search-detect-failure current-search-state))

(define-command ("^R I-Search Previous Occurrence")
  "Search for the previous occurrence of the current search string."
  (set-current-search-state!
   (incremental-search:previous-occurrence current-search-state))
  (i-search-detect-failure current-search-state))

(define-command ("^R I-Search Previous State")
  "Revert to the last state the search was in."
  (set-current-search-state!
   (incremental-search:delete-char current-search-state)))

(define-command ("^R I-Search Previous Successful State")
  "Revert to the last successful state and exit search if there is none."
  (incremental-search:pop!))

(define-command ("^R I-Search Terminate")
  "Terminates I-Search Mode."
  (incremental-search:terminate! current-search-state false))

(define (i-search-append-char char)
  (i-search-append-string (string char)))

(define (i-search-append-string string)
  (set-current-search-state!
   (incremental-search:append-string current-search-state string))
  (i-search-detect-failure current-search-state))

(define (i-search-detect-failure search-state)
  (if (and (not (search-state-successful? search-state))
	   (or (search-state-successful? (search-state-parent search-state))
	       (not (eq? (search-state-forward? search-state)
			 (search-state-forward?
			  (search-state-parent search-state))))))
      (editor-failure)))

(define-major-mode "Incremental Search" #F
  "Major mode for incremental search.
See \"^R Incremental Search\" for details.")

(define-default-key "Incremental Search" "^R I-Search Editor Command")
(define-key "Incremental Search" char-set:graphic "^R I-Search Append Char")
(define-key "Incremental Search" #\Tab "^R I-Search Append Char")
(define-key "Incremental Search" #\Return "^R I-Search Append Newline")
(define-key "Incremental Search" #\C-Q "^R I-Search Quote Character")
(define-key "Incremental Search" #\C-R "^R I-Search Previous Occurrence")
(define-key "Incremental Search" #\C-S "^R I-Search Next Occurrence")
(define-key "Incremental Search" #\C-W "^R I-Search Append Word")
(define-key "Incremental Search" #\C-Y "^R I-Search Append Line")
(define-key "Incremental Search" #\Rubout "^R I-Search Previous State")
(define-key "Incremental Search" #\C-G "^R I-Search Previous Successful State")
(define-key "Incremental Search" #\Altmode "^R I-Search Terminate")

(define incremental-search-exit)
(define incremental-search-window)
(define current-search-state)

(define (incremental-search forward?)
  (if (typein-window? (current-window)) (editor-error))
  (let ((old-point (current-point))
	(old-window (current-window))
	(old-case-fold-search (ref-variable "Case Fold Search")))
    (let ((y-point (window-point-y old-window)))
      (let ((result
	     (call-with-current-continuation
	       (lambda (continuation)
		 (fluid-let ((incremental-search-exit continuation)
			     (incremental-search-window old-window)
			     (current-search-state false))
		   (within-typein-edit
		    (lambda ()
		      (set-current-major-mode! incremental-search-mode)
		      (local-set-variable! "Case Fold Search"
					   old-case-fold-search)
		      (select-cursor old-window)
		      (set-current-search-state!
		       (initial-search-state forward? old-point))
		      (incremental-search-loop))))))))
	(cond ((eq? result 'ABORT)
	       (set-current-point! old-point)
	       (window-scroll-y-absolute! (current-window) y-point))
	      ((command? result)
	       (dispatch-on-command result))
	      (else
	       (push-current-mark! old-point)
	       (if (char? result)
		   (execute-char (current-comtabs) result))))))))

(define (incremental-search-loop)
  (intercept-^G-interrupts (lambda ()
			     (incremental-search:pop!)
			     (incremental-search-loop))
			   command-reader))

(define (incremental-search:append-string state string)
  (let ((text (string-append (search-state-text state) string)))
    (cond ((not (search-state-successful? state))
	   (unsuccessful-search-state state text
				      (search-state-forward? state)))
	  ((search-state-forward? state)
	   (find-next-search-state state
				   text
				   (search-state-start-point state)))
	  (else
	   (find-previous-search-state
	    state text
	    (let ((end (search-state-end-point state)))
	      (if (or (group-end? end)
		      (mark= end (search-state-initial-point state)))
		  end
		  (mark1+ end))))))))

(define (incremental-search:delete-char state)
  (let ((parent (search-state-parent state)))
    (if (null? parent) (editor-error))
    parent))

(define (incremental-search:terminate! state char)
  (if (and (not char)
	   (null? (search-state-parent state)))
      (incremental-search-exit
       (name->command
	(if (search-state-forward? state)
	    "Search Forward"
	    "Search Backward"))))
  (save-search-state-text! state)
  (set-window-point!
   incremental-search-window
   (search-state-point (most-recent-successful-search-state state)))
  (incremental-search-exit char))

(define (incremental-search:pop!)
  (let ((success (most-recent-successful-search-state current-search-state)))
    (if (eq? success current-search-state)
	(begin (save-search-state-text! success)
	       (incremental-search-exit 'ABORT))
	(set-current-search-state! success))))

(define (save-search-state-text! state)
  (if (not (null? (search-state-parent state)))
      (set-variable! "Previous Search String" (search-state-text state))))

(define (incremental-search:next-occurrence state)
  (cond ((null? (search-state-parent state))
	 (let ((point (search-state-initial-point state)))
	   (if (not (search-state-forward? state))
	       (initial-search-state true point)
	       (begin
		(insert-string (ref-variable "Previous Search String"))
		(find-next-search-state state
					(ref-variable "Previous Search String")
					point)))))
	((search-state-successful? state)
	 (find-next-search-state state
				 (search-state-text state)
				 ((if (search-state-forward? state)
				      search-state-end-point
				      search-state-start-point)
				  state)))
	((not (search-state-forward? state))
	 (find-next-search-state state
				 (search-state-text state)
				 (search-state-point state)))
	(else
	 (unsuccessful-search-state state (search-state-text state) true))))

(define (incremental-search:previous-occurrence state)
  (cond ((null? (search-state-parent state))
	 (let ((point (search-state-initial-point state)))
	   (if (search-state-forward? state)
	       (initial-search-state false point)
	       (begin
		(insert-string (ref-variable "Previous Search String"))
		(find-previous-search-state
		 state
		 (ref-variable "Previous Search String")
		 point)))))
	((search-state-successful? state)
	 (find-previous-search-state state
				     (search-state-text state)
				     ((if (search-state-forward? state)
					  search-state-end-point
					  search-state-start-point)
				      state)))
	((search-state-forward? state)
	 (find-previous-search-state state
				     (search-state-text state)
				     (search-state-point state)))
	(else
	 (unsuccessful-search-state state (search-state-text state) false))))

(define (initial-search-state forward? point)
  (make-search-state "" '() forward? true point point point point))

(define (unsuccessful-search-state parent text forward?)
  (let ((start-point (search-state-start-point parent)))
    (make-search-state text parent forward? false
		       start-point
		       (mark+ start-point (string-length text))
		       (search-state-point parent)
		       (search-state-initial-point parent))))

(define (find-next-search-state state text start)
  (if (search-forward text start)
      (let ((start-point (re-match-start 0))
	    (end-point (re-match-end 0)))
	(make-search-state text state true true
			   start-point end-point end-point
			   (if (search-state-forward? state)
			       (search-state-initial-point state)
			       (search-state-start-point state))))
      (unsuccessful-search-state state text true)))

(define (find-previous-search-state state text start)
  (if (search-backward text start)
      (let ((start-point (re-match-start 0))
	    (end-point (re-match-end 0)))
	(make-search-state text state false true
			   start-point end-point start-point
			   (if (search-state-forward? state)
			       (search-state-end-point state)
			       (search-state-initial-point state))))
      (unsuccessful-search-state state text false)))

(define (set-current-search-state! state)
  (let ((window (current-window)))
    (let ((point (window-point window)))
      (region-delete! (buffer-region (window-buffer window)))
      (region-insert-string!
       point
       (string-append (if (search-state-successful? state)
			  "" "Failing ")
		      (if (search-state-forward? state)
			  "" "Reverse ")
		      "I-Search: "))
      (region-insert-string!
       point
       (image-representation (make-image (search-state-text state))))
      (window-direct-update! window false)))
  (if (not (keyboard-active? 0))
      (set-window-point! incremental-search-window (search-state-point state)))
  (set! current-search-state state)
  unspecific)

(define (most-recent-successful-search-state state)
  (cond ((search-state-successful? state)
	 state)
	((null? (search-state-parent state))
	 (error "Search state chain terminated improperly"))
	(else
	 (most-recent-successful-search-state (search-state-parent state)))))

(define-named-structure "Search-State"
  text
  parent
  forward?
  successful?
  start-point
  end-point
  point
  initial-point)

(define (make-search-state text parent forward? successful?
			   start-point end-point point initial-point)
  (let ((state (%make-search-state)))
    (vector-set! state search-state-index:text text)
    (vector-set! state search-state-index:parent parent)
    (vector-set! state search-state-index:forward? forward?)
    (vector-set! state search-state-index:successful? successful?)
    (vector-set! state search-state-index:start-point start-point)
    (vector-set! state search-state-index:end-point end-point)
    (vector-set! state search-state-index:point point)
    (vector-set! state search-state-index:initial-point initial-point)
    state))