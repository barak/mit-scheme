;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/iserch.scm,v 1.5 1989/04/28 03:52:53 cph Exp $
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

;;;; Incremental Search

(declare (usual-integrations))

(define (isearch forward? regexp?)
  (reset-command-prompt!)
  (let ((window (current-window)))
    (let ((point (window-point window))
	  (y-point (window-point-y window)))
      (let ((result
	     (with-editor-interrupts-disabled
	      (lambda ()
		(isearch-loop
		 (initial-search-state false forward? regexp? point))))))
	(clear-message)
	(cond ((eq? result 'ABORT)
	       (set-window-point! window point)
	       (window-scroll-y-absolute! window y-point))
	      ((command? result)
	       (dispatch-on-command result))
	      (else
	       (push-current-mark! point)
	       (if result (execute-char (current-comtabs) result))))))))

(define (isearch-loop state)
  (if (not (keyboard-active? 0))
      (begin
	(set-current-point! (search-state-point state))
	(message (search-state-message state))))
  (let ((char (keyboard-read-char)))
    (let ((test-for
	   (lambda (char*)
	     (char=? char (remap-alias-char char*)))))
      (cond ((test-for (ref-variable search-quote-char))
	     (isearch-append-char
	      state
	      (prompt-for-typein
	       (string-append (search-state-message state) "^Q")
	       false
	       keyboard-read-char)))
	    ((test-for (ref-variable search-exit-char))
	     (if (string-null? (search-state-text state))
		 (if (search-state-forward? state)
		     (if (search-state-regexp? state)
			 (ref-command-object re-search-forward)
			 (ref-command-object search-forward))
		     (if (search-state-regexp? state)
			 (ref-command-object re-search-backward)
			 (ref-command-object search-backward)))
		 (begin
		   (isearch-exit state)
		   false)))
	    ((test-for #\C-g)
	     (editor-beep)
	     (isearch-pop state))
	    ((test-for (ref-variable search-repeat-char))
	     (isearch-continue (search-state-next state true)))
	    ((test-for (ref-variable search-reverse-char))
	     (isearch-continue (search-state-next state false)))
	    ((test-for (ref-variable search-delete-char))
	     (isearch-loop (or (search-state-parent state) (editor-error))))
	    ((test-for (ref-variable search-yank-word-char))
	     (isearch-append-string
	      state
	      (extract-next-word (search-state-end-point state))))
	    ((test-for (ref-variable search-yank-line-char))
	     (isearch-append-string
	      state
	      (extract-rest-of-line (search-state-end-point state))))
	    ((char=? char #\return)
	     (isearch-append-char state #\newline))
	    ((or (not (zero? (char-bits char)))
		 (and (ref-variable search-exit-option)
		      (< (char-code char) #x20)))
	     (isearch-exit state)
	     char)
	    (else
	     (isearch-append-char state char))))))

(define (isearch-append-char state char)
  (isearch-append-string state (string char)))

(define (isearch-append-string state string)
  (isearch-continue (search-state-append-string state string)))

(define (isearch-continue state)
  (if (and (not (search-state-successful? state))
	   (let ((parent (search-state-parent state)))
	     (or (search-state-successful? parent)
		 (not (eq? (search-state-forward? state)
			   (search-state-forward? parent))))))
      (editor-failure))
  (isearch-loop state))

(define (isearch-pop state)
  (let ((success (most-recent-successful-search-state state)))
    (if (eq? success state)
	'ABORT
	(isearch-loop success))))

(define (isearch-exit state)
  (set-current-point!
   (search-state-point (most-recent-successful-search-state state)))
  (if (not (string-null? (search-state-text state)))
      (let ((text (search-state-text state)))
	(if (search-state-regexp? state)
	    (set-variable! search-last-regexp text)
	    (set-variable! search-last-string text)))))

(define (extract-next-word mark)
  (extract-string mark (forward-word mark 1 'LIMIT)))

(define (extract-rest-of-line mark)
  (extract-string mark (line-end mark (if (line-end? mark) 1 0) 'LIMIT)))

(define (search-state-message state)
  (let ((invalid-regexp (search-state-invalid-regexp state)))
    (let ((m
	   (string-append
	    (if (search-state-successful? state) "" "failing ")
	    (if (search-state-wrapped? state) "wrapped " "")
	    (if (search-state-regexp? state) "regexp " "")
	    "I-search"
	    (if (search-state-forward? state) "" " backward")
	    ": "
	    (image-representation (make-image (search-state-text state)))	    (if invalid-regexp (string-append " [" invalid-regexp "]") ""))))
      (string-set! m 0 (char-upcase (string-ref m 0)))
      m)))

(define (search-state-append-string state string)
  (let ((text (string-append (search-state-text state) string)))
    (if (search-state-successful? state)
	(next-search-state
	 state
	 text
	 (search-state-forward? state)
	 (cond ((and (search-state-regexp? state)
		     (string-find-next-char-in-set string regexp-retry-chars))
		(search-state-initial-point state))
	       ((search-state-forward? state)
		(search-state-start-point state))
	       (else
		(let ((end
		       (mark+ (search-state-end-point state)
			      (string-length string)))
		      (initial-point (search-state-initial-point state)))
		  (if (and end (mark< end initial-point))
		      end
		      initial-point))))
	 (search-state-initial-point state))
	(unsuccessful-search-state state
				   text
				   (search-state-forward? state)))))

(define regexp-retry-chars
  ;; If one of these characters is entered, retry the regexp search
  ;; from the initial point since it may now match something that it
  ;; didn't match before.
  (char-set #\* #\? #\|))

(define (search-state-next state forward?)
  (cond ((not (string-null? (search-state-text state)))
	 (let ((start
		(cond ((search-state-successful? state)
		       (if (search-state-forward? state)
			   (search-state-end-point state)
			   (search-state-start-point state)))
		      (forward?
		       (if (search-state-forward? state)
			   (buffer-start (current-buffer))
			   (search-state-point state)))
		      (else
		       (if (search-state-forward? state)
			   (search-state-point state)
			   (buffer-end (current-buffer)))))))
	   (next-search-state state
			      (search-state-text state)
			      forward?
			      start
			      start)))
	((eq? forward? (search-state-forward? state))
	 (next-search-state state
			    (if (search-state-regexp? state)
				(ref-variable search-last-regexp)
				(ref-variable search-last-string))
			    forward?
			    (search-state-initial-point state)
			    (search-state-initial-point state)))
	(else
	 (initial-search-state state
			       forward?
			       (search-state-regexp? state)
			       (search-state-initial-point state)))))

(define-structure (search-state)
  (text false read-only true)
  (parent false read-only true)
  (forward? false read-only true)
  (regexp? false read-only true)
  (successful? false read-only true)
  (wrapped? false read-only true)
  (invalid-regexp false read-only true)
  (start-point false read-only true)
  (end-point false read-only true)
  (point false read-only true)
  (initial-point false read-only true))

(define (most-recent-successful-search-state state)
  (if (search-state-successful? state)
      state
      (most-recent-successful-search-state
       (or (search-state-parent state)
	   (error "Search state chain terminated improperly")))))

(define (initial-search-state parent forward? regexp? point)
  (make-search-state ""
		     parent
		     forward?
		     regexp?
		     true
		     false
		     false
		     point
		     point
		     point
		     point))

(define (unsuccessful-search-state parent text forward?)
  (let ((start-point (search-state-start-point parent)))
    (make-search-state text
		       parent
		       forward?
		       (search-state-regexp? parent)
		       false
		       (search-state-wrapped? parent)
		       false
		       start-point
		       (mark+ start-point (string-length text))
		       (search-state-point parent)
		       (search-state-initial-point parent))))

(define (next-search-state parent text forward? start initial-point)
  (let ((regexp? (search-state-regexp? parent)))
    (let ((result (perform-search forward? regexp? text start)))
      (cond ((not result)
	     (unsuccessful-search-state parent text forward?))
	    ((eq? result 'ABORT)
	     (most-recent-successful-search-state parent))
	    ((string? result)
	     (make-search-state text
				parent
				forward?
				regexp?
				(search-state-successful? parent)
				(search-state-wrapped? parent)
				(if (or (string-prefix? "Premature " result)
					(string-prefix? "Unmatched " result))
				    "incomplete input"
				    result)
				(search-state-start-point parent)
				(search-state-end-point parent)
				(search-state-point parent)
				(search-state-initial-point parent)))
	    (else
	     (make-search-state
	      text
	      parent
	      forward?
	      regexp?
	      true
	      (and (boolean=? forward? (search-state-forward? parent))
		   (or (search-state-wrapped? parent)
		       (not (search-state-successful? parent))))
	      false
	      (re-match-start 0)
	      (re-match-end 0)
	      result
	      initial-point))))))

(define (perform-search forward? regexp? text start)
  (call-with-current-continuation
   (lambda (continuation)
     (bind-condition-handler
	 (list error-type:re-compile-pattern)
	 (lambda (condition)
	   (continuation (car (condition/irritants condition))))
       (lambda ()
	 (intercept-^G-interrupts (lambda () 'ABORT)
	   (lambda ()
	     (with-editor-interrupts-enabled
	      (lambda ()
		(if forward?
		    (if regexp?
			(re-search-forward text start)
			(search-forward text start))
		    (if regexp?
			(re-search-backward text start)
			(search-backward text start))))))))))))