;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/replaz.scm,v 1.73 1991/09/11 02:11:57 arthur Exp $
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

;;;; Replacement Commands

(declare (usual-integrations))

(define-variable case-replace
  "If true, means replacement commands should preserve case."
  true
  boolean?)

(define (replace-string-arguments name)
  (let ((source (prompt-for-string name false)))
    (list source
	  (prompt-for-string (string-append name " " source " with")
			     false
			     'NULL-DEFAULT)
	  (command-argument))))

 (define-command replace-string
  "Replace occurrences of FROM-STRING with TO-STRING.
Preserve case in each match if  case-replace  and  case-fold-search
are true and FROM-STRING has no uppercase letters.
Third arg DELIMITED (prefix arg if interactive) true means replace
only matches surrounded by word boundaries."
  (lambda () (replace-string-arguments "Replace string"))
  (lambda (from-string to-string delimited)
    (replace-string from-string to-string delimited false false)
    (message "Done")))

(define-command replace-regexp
  "Replace things after point matching REGEXP with TO-STRING.
Preserve case in each match if case-replace and case-fold-search
are true and REGEXP has no uppercase letters.
Third arg DELIMITED (prefix arg if interactive) true means replace
only matches surrounded by word boundaries.
In TO-STRING, \\& means insert what matched REGEXP,
and \\<n> means insert what matched <n>th \\(...\\) in REGEXP."
  (lambda () (replace-string-arguments "Replace regexp"))
  (lambda (regexp to-string delimited)
    (replace-string regexp to-string delimited false true)
    (message "Done")))

(define-command query-replace
  "Replace some occurrences of FROM-STRING with TO-STRING.
As each match is found, the user must type a character saying
what to do with it.  For directions, type \\[help-command] at that time.

Preserve case in each replacement if  case-replace  and  case-fold-search
are true and FROM-STRING has no uppercase letters.
Third arg DELIMITED (prefix arg if interactive) true means replace
only matches surrounded by word boundaries."
  (lambda () (replace-string-arguments "Query replace"))
  (lambda (from-string to-string delimited)
    (replace-string from-string to-string delimited true false)
    (message "Done")))

(define-command query-replace-regexp
  "Replace some things after point matching REGEXP with TO-STRING.
As each match is found, the user must type a character saying
what to do with it.  For directions, type \\[help-command] at that time.

Preserve case in each replacement if  case-replace  and  case-fold-search
are true and REGEXP has no uppercase letters.
Third arg DELIMITED (prefix arg if interactive) true means replace
only matches surrounded by word boundaries.
In TO-STRING, \\& means insert what matched REGEXP,
and \\<n> means insert what matched <n>th \\(...\\) in REGEXP."
  (lambda () (replace-string-arguments "Query replace regexp"))
  (lambda (regexp to-string delimited)
    (replace-string regexp to-string delimited true true)
    (message "Done")))

(define (replace-string source target delimited? query? regexp?)
  ;; Returns TRUE iff the query loop was exited at the user's request,
  ;; FALSE iff the loop finished by failing to find an occurrence.
  (let ((preserve-case?
	 (and (ref-variable case-replace)
	      (ref-variable case-fold-search)
	      (string-lower-case? source)
	      (not (string-null? target))
	      (string-lower-case? target)))
	(source*
	 (if delimited?
	     (string-append "\\b"
			    (if regexp? source (re-quote-string source))
			    "\\b")
	     source))
	(message-string
	 (string-append "Query replacing " source " with " target)))

    (define (replacement-loop point)
      (undo-boundary! point)
      (let ((done
	     (lambda ()
	       (set-current-point! point)
	       (done false))))
	(cond ((not (find-next-occurrence point))
	       (done))
	      ((mark< point (re-match-end 0))
	       (replacement-loop (perform-replacement)))
	      ((not (group-end? point))
	       (replacement-loop (mark1+ point)))
	      (else
	       (done)))))

    (define (query-loop point)
      (undo-boundary! point)
      (cond ((not (find-next-occurrence point))
	     (done false))
	    ((mark< point (re-match-end 0))
	     (set-current-mark! point)
	     (set-current-point! (re-match-end 0))
	     (perform-query false))
	    ((not (group-end? point))
	     (query-loop (mark1+ point)))
	    (else
	     (done false))))

    (define (find-next-occurrence start)
      (if (or regexp? delimited?)
	  (re-search-forward source* start (group-end start))
	  (search-forward source* start (group-end start))))

    (define (perform-replacement)
      (replace-match target preserve-case? (not regexp?)))

    (define (done value)
      (pop-current-mark!)
      value)

    (define (perform-query replaced?)
      (message message-string ":")
      (let ((key (with-editor-interrupts-disabled keyboard-peek)))
	(let ((test-for
	       (lambda (key*)
		 (and (char? key)
		      (char=? key (remap-alias-key key*))
		      (begin
			(keyboard-read)
			true)))))
	  (cond ((test-for #\C-h)
		 (with-output-to-help-display
		  (lambda ()
		    (write-string message-string)
		    (write-string ".

Type space to replace one match, Rubout to skip to next,
Altmode to exit, Period to replace one match and exit,
Comma to replace but not move point immediately,
C-R to enter recursive edit, C-W to delete match and recursive edit,
! to replace all remaining matches with no more questions,
^ to move point back to previous match.")))
		 (perform-query replaced?))
		((or (test-for #\altmode)
		     (test-for #\q))
		 (done true))
		((test-for #\^)
		 (set-current-point! (current-mark))
		 (perform-query true))
		((or (test-for #\space)
		     (test-for #\y))
		 (if (not replaced?) (perform-replacement))
		 (query-loop (current-point)))
		((test-for #\.)
		 (if (not replaced?) (perform-replacement))
		 (done true))
		((test-for #\,)
		 (if (not replaced?) (perform-replacement))
		 (perform-query true))
		((test-for #\!)
		 (if (not replaced?) (perform-replacement))
		 (replacement-loop (current-point)))
		((or (test-for #\rubout)
		     (test-for #\n))
		 (query-loop (current-point)))
		((test-for #\C-l)
		 ((ref-command recenter) false)
		 (perform-query replaced?))
		((test-for #\C-r)
		 (edit)
		 (perform-query replaced?))
		((test-for #\C-w)
		 (if (not replaced?) (delete-match))
		 (edit)
		 (perform-query true))
		(else
		 (done true))))))

    (define (edit)
      (clear-message)
      (preserving-match-data enter-recursive-edit))

    (let ((point (current-point)))
      (push-current-mark! point)
      (push-current-mark! point)
      (if query?
	  (query-loop point)
	  (replacement-loop point)))))

;;;; Occurrence Commands

(define-command count-matches
  "Print the number of occurrences of a given regexp following point."
  "sHow many matches for (regexp)"
  (lambda (regexp)
    (let loop ((start (current-point)) (n 0))
      (let ((mark (re-search-forward regexp start)))
	(if (not mark)
	    (message (write-to-string n) " occurrences")
	    (loop mark (1+ n)))))))

(define-command list-matching-lines
  "Show all lines following point containing a match for a given regexp.
The argument, if given, is the number of context lines to show
 on either side of each line; this defaults to zero."
  "sList matching lines (regexp)\nP"
  (lambda (regexp argument)
    (let ((argument (or argument 0)))
      (let ((end (group-end (current-point)))
	    (-arg (- argument))
	    (1+arg (1+ argument)))
	(with-output-to-temporary-buffer "*Occur*"
	  (lambda ()
	    (define (loop start)
	      (let ((mark (re-search-forward regexp start end)))
		(if mark
		    (begin (write-string
			    (extract-string (line-start mark -arg)
					    (line-start mark 1+arg)))
			   (write-string "--------")
			   (newline)
			   (loop (line-start mark 1))))))
	    (loop (current-point))))))))