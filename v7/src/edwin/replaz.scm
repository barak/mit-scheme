;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/replaz.scm,v 1.63 1989/03/14 08:02:12 cph Exp $
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

;;;; Replacement Commands

(declare (usual-integrations))

(define-variable "Replace String Search"
  "The last string that a replacement command searched for."
  false)

(define-variable "Replace String Replace"
  "The last string that a replacement command replaced with."
  false)

(define-variable "Case Replace"
  "If not false, means replacement commands should preserve case."
  true)

(define-command ("Replace String" argument)
  "Replace occurrences of a given string with another one.
Preserve case in each match if Case Replace and Case Fold Search
are true and the given strings have no uppercase letters.
With an argument, replace only matches surrounded by word boundaries."
  (interactive-replace-string "Replace String" argument false))

(define-command ("Query Replace" argument)
  "Replace some occurrences of a given string with another one.
As each match is found, the user must type a character saying
what to do with it.
Type C-H within Query Replace for directions.

Preserve case in each match if Case Replace and Case Fold Search
are true and the given strings have no uppercase letters.
With an argument, replace only matches surrounded by word boundaries."
  (interactive-replace-string "Query Replace" argument true))

(define (interactive-replace-string name replace-words-only? query?)
  (replace-string-arguments name
			    (replace-string name replace-words-only? query?
					    true)))

(define (replace-string-arguments name receiver)
  (let ((source
	 (prompt-for-string name
			    (ref-variable "Replace String Search")
			    'VISIBLE-DEFAULT)))
    (let ((target 
	   (prompt-for-string "Replace with"
			      (ref-variable "Replace String Replace")
			      'NULL-DEFAULT)))
      (set-variable! "Replace String Search" source)
      (set-variable! "Replace String Replace" target)
      (receiver source target))))

(define ((replace-string name replace-words-only? query? clear-on-exit?)
	 source target)
  ;; Returns TRUE iff the query loop was exited at the user's request,
  ;; FALSE iff the loop finished by failing to find an occurrence.
  (let ((preserve-case? (and (ref-variable "Case Replace")
			     (ref-variable "Case Fold Search")
			     (string-lower-case? source)
			     (not (string-null? target))
			     (string-lower-case? target)))
	(upper (delay (string-upcase target)))
	(capital (delay (string-capitalize target)))
	(words-only-source
	 (delay (string-append "\\b" (re-quote-string source) "\\b")))
	(message-string
	 (string-append name ": " (write-to-string source)
			" => " (write-to-string target)))
	(old-notification (ref-variable "Auto Push Point Notification")))

    (define (find-next-occurrence start receiver)
      (if (if replace-words-only?
	      (re-search-forward (force words-only-source) start)
	      (search-forward source start))
	  (receiver (re-match-start 0) (re-match-end 0))
	  (begin (if clear-on-exit? (clear-message))
		 false)))

    (define (query-loop start end)
      (undo-boundary! end)
      (push-current-mark! start)
      (find-next-occurrence end
	(lambda (start end)
	  (set-current-point! end)
	  (perform-query (mark-right-inserting start)
			 (current-point)
			 false))))

    (define (replacement-loop start)
      (undo-boundary! start)
      (find-next-occurrence start
	(lambda (start end)
	  (let ((end (mark-left-inserting end)))
	    (perform-replacement start end)
	    (replacement-loop end)))))

    (define (perform-replacement start end)
      (let ((replaced (extract-string start end)))
	(delete-string start end)
	(insert-string (cond ((not preserve-case?) target)
			     ((string-upper-case? replaced) (force upper))
			     ((string-capitalized? replaced)
			      (force capital))
			     (else target))
		       end)))

    (define (edit)
      (fluid-let (((ref-variable "Auto Push Point Notification")
		   old-notification))
	(clear-message)
	(enter-recursive-edit)
	(set-message)))

    (define (set-message)
      (message message-string))

    (define (perform-query start end replaced?)
      (let ((char (char-upcase (keyboard-read-char))))
	(cond ((char=? #\Space char)
	       (if (not replaced?) (perform-replacement start end))
	       (query-loop start end))
	      ((char=? #\Rubout char)
	       (query-loop start end))
	      ((char=? #\Altmode char)
	       (if clear-on-exit? (clear-message))
	       true)
	      ((char=? #\. char)
	       (if (not replaced?) (perform-replacement start end))
	       (if clear-on-exit? (clear-message))
	       true)
	      ((char=? #\, char)
	       (if (not replaced?) (perform-replacement start end))
	       (perform-query start end true))
	      ((char=? #\C-R char)
	       (edit)
	       (perform-query start end replaced?))
	      ((char=? #\C-W char)
	       (if (not replaced?) (delete-string start end))
	       (edit)
	       (query-loop start end))
	      ((char=? #\! char)
	       (if (not replaced?) (perform-replacement start end))
	       (replacement-loop end))
	      ((char=? #\^ char)
	       (set-current-point! (pop-current-mark!))
	       (perform-query (current-mark) (current-mark) true))
	      ((or (char=? #\C-H char) (char=? #\Backspace char))
	       (with-output-to-help-display
		(lambda ()
		  (write-string "Query replacing ")
		  (write source)
		  (write-string " with ")
		  (write target)
		  (write-string ".

Type space to replace one match, Rubout to skip to next,
Altmode to exit, Period to replace one match and exit,
Comma to replace but not move point immediately,
C-R to enter recursive edit, C-W to delete match and recursive edit,
! to replace all remaining matches with no more questions,
^ to move point back to previous match.")))
	       (perform-query start end replaced?))
	      (else
	       (if clear-on-exit? (clear-message))
	       (execute-char (current-comtabs) char)
	       true))))

    (set-message)
    (let ((point (current-point)))
      (if query?
	  (fluid-let (((ref-variable "Auto Push Point Notification") false))
	    (query-loop point point))
	  (replacement-loop point)))))

;;;; Occurrence Commands

(define-command ("Count Occurrences")
  "Print the number of occurrences of a given regexp following point."
  (let ((regexp (prompt-for-string "Count Occurrences (regexp)" false)))
    (define (loop start n)
      (let ((mark (re-search-forward regexp start)))
	(if (not mark)
	    (message (write-to-string n) " occurrences")
	    (loop mark (1+ n)))))
    (loop (current-point) 0)))

(define-command ("List Occurrences" (argument 0))
  "Show all lines containing a given regexp following point.
The argument, if given, is the number of context lines to show
 on either side of each line; this defaults to zero."
  (let ((regexp (prompt-for-string "List Occurrences (regexp)" false))
	(-arg (- argument))
	(1+arg (1+ argument)))
    (with-output-to-temporary-buffer "*Occur*"
      (lambda ()
	(define (loop start)
	  (let ((mark (re-search-forward regexp start)))
	    (if mark
		(begin (write-string (extract-string (line-start mark -arg)
						     (line-start mark 1+arg)))
		       (write-string "--------")
		       (newline)
		       (loop (line-start mark 1))))))
	(loop (current-point))))))