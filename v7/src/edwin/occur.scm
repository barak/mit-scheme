;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/occur.scm,v 1.1 1992/04/09 17:22:04 cph Exp $
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

;;;; Occurrence Commands

(declare (usual-integrations))

(define-command keep-lines
  "Delete all lines except those containing matches for REGEXP.
A match split across lines preserves all the lines it lies in.
Applies to all lines after point."
  "sKeep lines (containing match for regexp)"
  (lambda (regexp)
    (let ((point (current-point)))
      (keep-lines point (group-end point) regexp))))

(define-command delete-non-matching-lines
  (command-description (ref-command-object keep-lines))
  (command-interactive-specification (ref-command-object keep-lines))
  (command-procedure (ref-command-object keep-lines)))

(define (keep-lines start end regexp)
  (let ((case-fold-search (ref-variable case-fold-search start))
	(syntax-table (ref-variable syntax-table start))
	(group (mark-group start))
	(start (mark-index start))
	(anchor (mark-left-inserting-copy start))
	(end (mark-left-inserting-copy end)))
    (let ((pattern (re-compile-pattern regexp case-fold-search)))
      (letrec
	  ((loop
	    (lambda (start point)
	      (let ((point
		     (re-search-buffer-forward pattern
					       case-fold-search
					       syntax-table
					       group
					       point
					       (mark-index end))))
		(if point
		    (begin
		      (set-mark-index! anchor point)
		      (let ((end
			     (line-start-index group
					       (re-match-start-index 0))))
			(if (< start end)
			    (group-delete! group start end)))
		      (continue (mark-index anchor)))
		    (group-delete! group start (mark-index end))))))
	   (continue
	    (lambda (point)
	      (let ((start (line-end-index group point)))
		(if (< start (mark-index end))
		    (loop (+ start 1) point))))))
	(if (line-start-index? group start)
	    (loop start start)
	    (continue start))))
    (mark-temporary! anchor)
    (mark-temporary! end)))

(define-command flush-lines
  "Delete lines containing matches for REGEXP.
If a match is split across lines, all the lines it lies in are deleted.
Applies to lines after point."
  "sFlush lines (containing match for regexp)"
  (lambda (regexp)
    (let ((point (current-point)))
      (flush-lines point (group-end point) regexp))))

(define-command delete-matching-lines
  (command-description (ref-command-object flush-lines))
  (command-interactive-specification (ref-command-object flush-lines))
  (command-procedure (ref-command-object flush-lines)))

(define (flush-lines start end regexp)
  (let ((case-fold-search (ref-variable case-fold-search start))
	(syntax-table (ref-variable syntax-table start))
	(group (mark-group start))
	(start (mark-left-inserting-copy start))
	(end (mark-left-inserting-copy end)))
    (let ((pattern (re-compile-pattern regexp case-fold-search)))
      (do ()
	  ((not (re-search-buffer-forward pattern
					  case-fold-search
					  syntax-table
					  group
					  (mark-index start)
					  (mark-index end))))
	(let ((point (line-end-index group (re-match-end-index 0))))
	  (set-mark-index! start point)
	  (group-delete! group
			 (line-start-index group (re-match-start-index 0))
			 (if (< point (mark-index end)) (+ point 1) point)))))
    (mark-temporary! start)
    (mark-temporary! end)))

(define-command count-matches
  "Print number of matches for REGEXP following point."
  "sCount matches for (regexp)"
  (lambda (regexp)
    (message (let ((point (current-point)))
	       (count-matches point (group-end point) regexp))
	     " occurrences")))

(define-command how-many
  (command-description (ref-command-object count-matches))
  (command-interactive-specification (ref-command-object count-matches))
  (command-procedure (ref-command-object count-matches)))

(define (count-matches start end regexp)
  (let ((case-fold-search (ref-variable case-fold-search start))
	(syntax-table (ref-variable syntax-table start))
	(group (mark-group start))
	(end (mark-index end)))
    (let ((pattern (re-compile-pattern regexp case-fold-search)))
      (let loop ((start (mark-index start)) (result 0))
	(let ((match
	       (re-search-buffer-forward pattern
					 case-fold-search
					 syntax-table
					 group
					 start
					 end)))
	  (if match
	      (loop match (+ result 1))
	      result))))))

(define-major-mode occur fundamental "Occur"
  "Major mode for output from \\[occur].
Move point to one of the occurrences in this buffer,
then use \\[occur-mode-goto-occurrence] to go to the same occurrence
in the buffer that the occurrences were found in.")

(define-key 'occur '(#\c-c #\c-c) 'occur-mode-goto-occurrence)

(define-command occur-mode-goto-occurrence
  "Go to the line this occurrence was found in, in the buffer it was found in."
  ()
  (lambda ()
    (let ((mark
	   (let ((point (current-point)))
	     (let ((index (mark-index point))
		   (occurrences (buffer-get (mark-buffer point) 'OCCURRENCES)))
	       (if (or (null? occurrences)
		       (< index (caar occurrences)))
		   (editor-error "No occurrence selected."))
	       (let loop ((occurrences occurrences))
		 (if (or (null? (cdr occurrences))
			 (< index (caadr occurrences)))
		     (cdar occurrences)
		     (loop (cdr occurrences))))))))
      (let ((buffer (mark-buffer mark)))
	(if (not (buffer-alive? buffer))
	    (editor-error "Buffer in which occurences were found is deleted."))
	(pop-up-buffer buffer true)
	(set-buffer-point! buffer mark)))))

(define-variable list-matching-lines-default-context-lines
  "Default number of context lines to include around a list-matching-lines
match.  A negative number means to include that many lines before the match.
A positive number means to include that many lines both before and after."
  0
  exact-integer?)

(define-command occur
  "Show all lines following point containing a match for REGEXP.
Display each line with NLINES lines before and after,
 or -NLINES before if NLINES is negative.
NLINES defaults to list-matching-lines-default-context-lines.
Interactively it is the prefix arg.

The lines are shown in a buffer named *Occur*.
It serves as a menu to find any of the occurrences in this buffer.
\\[describe-mode] in that buffer will explain how."
  "sList lines matching regexp\nP"
  (lambda (regexp argument)
    (let ((occur-buffer (temporary-buffer "*Occur*")))
      (let ((point (current-point))
	    (output (mark-left-inserting-copy (buffer-start occur-buffer))))
	(insert-string "Lines matching " output)
	(insert-string (write-to-string regexp) output)
	(insert-string " in buffer " output)
	(insert-string (buffer-name (mark-buffer point)) output)
	(insert-string ".\n" output)
	(set-buffer-major-mode! occur-buffer (ref-mode-object occur))
	(let ((occurrences
	       (format-occurrences
		(let ((occurrences
		       (re-occurrences point (group-end point) regexp)))
		  (for-each mark-permanent! occurrences)
		  occurrences)
		(if argument
		    (command-argument-numeric-value argument)
		    (ref-variable list-matching-lines-default-context-lines))
		output)))
	  (buffer-put! occur-buffer 'OCCURRENCES occurrences)
	  (message (number->string (length occurrences)) " matching lines."))
	(mark-temporary! output))
      (pop-up-buffer occur-buffer false))))

(define-command list-matching-lines
  (command-description (ref-command-object occur))
  (command-interactive-specification (ref-command-object occur))
  (command-procedure (ref-command-object occur)))

(define (re-occurrences start end regexp)
  (let ((case-fold-search (ref-variable case-fold-search start))
	(syntax-table (ref-variable syntax-table start))
	(group (mark-group start))
	(end (mark-index end)))
    (let ((pattern (re-compile-pattern regexp case-fold-search)))
      (let loop ((start (mark-index start)))
	(let ((match
	       (re-search-buffer-forward pattern
					 case-fold-search
					 syntax-table
					 group
					 start
					 end)))
	  (if match
	      (cons (make-temporary-mark group
					 (line-start-index group match)
					 false)
		    (loop (line-end-index group match)))
	      '()))))))

(define (format-occurrences occurrences nlines output)
  (if (null? occurrences)
      '()
      (let loop
	  ((occurrences occurrences)
	   (previous (group-start (car occurrences)))
	   (line 1))
	(let ((lstart (car occurrences))
	      (index (mark-index output)))
	  (let ((line (+ line (count-lines previous lstart))))
	    (format-occurrence lstart line nlines output)
	    (cons (cons index lstart)
		  (if (null? (cdr occurrences))
		      '()
		      (begin
			(if (not (= nlines 0))
			    (insert-string "--------\n" output))
			(loop (cdr occurrences) lstart line)))))))))

(define (format-occurrence lstart line nlines output)
  (let ((tag (pad-on-left-to (number->string line) 3)))
    (let ((empty
	   (and (not (= nlines 0))
		(make-string (string-length tag) #\space))))
      (if (not (= nlines 0))
	  (let loop ((lstart* (line-start lstart (- (abs nlines)) 'LIMIT)))
	    (if (not (mark= lstart* lstart))
		(let ((next (line-start lstart* 1 'ERROR)))
		  (insert-string empty output)
		  (insert-string ":" output)
		  (insert-region lstart* next output)
		  (loop next)))))
      (insert-string tag output)
      (insert-string ":" output)
      (insert-region lstart (line-end lstart 0) output)
      (insert-newline output)
      (if (> nlines 0)
	  (let ((lstart (line-start lstart 1 false)))
	    (if lstart
		(let loop ((lstart lstart) (n nlines))
		  (let ((lend (line-end lstart 0)))
		    (insert-string empty output)
		    (insert-string ":" output)
		    (insert-region lstart lend output)
		    (insert-newline output)
		    (if (and (not (group-end? lend)) (> n 1))
			(loop (mark1+ lend) (- n 1)))))))))))