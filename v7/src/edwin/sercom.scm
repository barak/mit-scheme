;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/sercom.scm,v 1.56 1991/04/21 00:52:01 cph Exp $
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

;;;; Search Commands

(declare (usual-integrations))

;;;; Variables

(define-variable-per-buffer case-fold-search
  "*True if searches should ignore case.
Automatically becomes local when set in any fashion."
  true)

(define-variable search-last-string
  "Last string search for by a non-regexp search command.
This does not include direct calls to the primitive search functions,
and does not include searches that are aborted."
  "")

(define-variable search-last-regexp
  "Last string searched for by a regexp search command.
This does not include direct calls to the primitive search functions,
and does not include searches that are aborted."
  "")

(define-variable search-repeat-char
  "*Character to repeat incremental search forwards."
  #\C-s)

(define-variable search-reverse-char
  "*Character to repeat incremental search backwards."
  #\C-r)

(define-variable search-exit-char
  "*Character to exit incremental search."
  #\altmode)

(define-variable search-delete-char
  "*Character to delete from incremental search string."
  #\rubout)

(define-variable search-quote-char
  "*Character to quote special characters for incremental search."
  #\C-q)

(define-variable search-yank-word-char
  "*Character to pull next word from buffer into search string."
  #\C-w)

(define-variable search-yank-line-char
  "*Character to pull rest of line from buffer into search string."
  #\C-y)

(define-variable search-exit-option
  "*True means random control characters terminate incremental search."
  true)

(define-variable search-slow-speed
  "*Highest terminal speed at which to use \"slow\" style incremental search.
This is the style where a one-line window is created to show the line
that the search has reached."
  1200)

(define-variable search-slow-window-lines
  "*Number of lines in slow search display windows.
These are the short windows used during incremental search on slow terminals.
Negative means put the slow search window at the top (normally it's at bottom)
and the value is minus the number of lines."
  1)

;;;; String Search

(define (search-prompt prompt)
  (lambda ()
    (let ((string
	   (prompt-for-string prompt (ref-variable search-last-string))))
      (set-variable! search-last-string string)
      (list string))))

(define (re-search-prompt prompt)
  (lambda ()
    (let ((regexp
	   (prompt-for-string prompt (ref-variable search-last-regexp))))
      (set-variable! search-last-regexp regexp)
      (list regexp))))

(define (search-command procedure pattern)
  (let ((mark (procedure pattern)))
    (if mark
	(begin
	  (push-current-mark! (current-point))
	  (set-current-point! mark))
	(editor-failure))))

(define-command search-forward
  "Search forward from point for a character string.
Sets point at the end of the occurrence found."
  (search-prompt "Search")
  (lambda (string)
    (search-command search-forward string)))

(define-command search-backward
  "Search backward from point for a character string.
Sets point at the beginning of the occurrence found."
  (search-prompt "Search backward")
  (lambda (string)
    (search-command search-backward string)))

(define-command re-search-forward
  "Search forward from point for a regular expression.
Sets point at the end of the occurrence found."
  (search-prompt "RE search")
  (lambda (regexp)
    (search-command re-search-forward regexp)))

(define-command re-search-backward
  "Search backward from point for a character string.
Sets point at the beginning of the occurrence found."
  (search-prompt "RE search backward")
  (lambda (regexp)
    (search-command re-search-backward regexp)))

;;;; Incremental Search

(define-command isearch-forward
  "Do incremental search forward.
As you type characters, they add to the search string and are found.
Type Delete to cancel characters from end of search string.
Type ESC to exit, leaving point at location found.
Type C-s to search again forward, C-r to search again backward.
Type C-w to yank word from buffer onto end of search string and search for it.
Type C-y to yank rest of line onto end of search string, etc.
Type C-q to quote control character to search for it.
Other control and meta characters terminate the search
 and are then executed normally.
The above special characters are mostly controlled by parameters;
 do M-x variable-apropos on search-.*-char to find them.
C-g while searching or when search has failed
 cancels input back to what has been found successfully.
C-g when search is successful aborts and moves point to starting point."
  ()
  (lambda ()
    (isearch true false)))

(define-command isearch-forward-regexp
  "Do incremental search forward for regular expression.
Like ordinary incremental search except that your input
is treated as a regexp.  See \\[isearch-forward] for more info."
  ()
  (lambda ()
    (isearch true true)))

(define-command isearch-backward
  "Do incremental search backward.
See \\[isearch-forward] for more information."
  ()
  (lambda ()
    (isearch false false)))

(define-command isearch-backward-regexp
  "Do incremental search backward for regular expression.
Like ordinary incremental search except that your input
is treated as a regexp.  See \\[isearch-forward] for more info."
  ()
  (lambda ()
    (isearch false true)))

;;;; Character Search
;;;  (Courtesy of Jonathan Rees)

(define-command char-search-forward
  "Search for a single character.
Special characters:
  C-a  calls \\[search-forward].
  C-r  searches backwards for the current default.
  C-s  searches forward for the current default.
  C-q  quotes the character to be searched for;
       this allows search for special characters."
  ()
  (lambda ()
    (character-search true)))

(define-command char-search-backward
  "Like \\[char-search-forward], but searches backwards."
  ()
  (lambda ()
    (character-search false)))

(define (character-search forward?)
  (define (char-search char)
    (search-finish
     (let ((point (current-point)))
       (if forward?
	   (char-search-forward char point (group-end point))
	   (char-search-backward char point (group-start point))))))

  (define (string-search operator)
    (search-finish (operator (ref-variable search-last-string))))

  (define (search-finish mark)
    (if mark
	(set-current-point! mark)
	(editor-failure)))

  (let ((char (prompt-for-char "Character search")))
    (let ((test-for
	   (lambda (char*)
	     (char=? char (remap-alias-char char*)))))
      (cond ((test-for #\C-a)
	     (dispatch-on-command
	      (if forward?
		  (ref-command-object search-forward)
		  (ref-command-object search-backward))))
	    ((test-for #\C-s)
	     (string-search search-forward))
	    ((test-for #\C-r)
	     (string-search search-backward))
	    ((test-for #\C-q)
	     (char-search (prompt-for-char "Quote character")))
	    (else
	     (char-search char))))))