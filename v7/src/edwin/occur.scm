#| -*-Scheme-*-

$Id: occur.scm,v 1.8 2003/02/14 18:28:12 cph Exp $

Copyright 1992-2000 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

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
  (let ((pattern
	 (re-compile-pattern regexp (ref-variable case-fold-search start)))
	(syntax-table (ref-variable syntax-table start))
	(group (mark-group start))
	(start (mark-index start))
	(anchor (mark-left-inserting-copy start))
	(end (mark-left-inserting-copy end)))
    (letrec
	((loop
	  (lambda (start point)
	    (let ((point
		   (re-search-buffer-forward pattern syntax-table
					     group point (mark-index end))))
	      (if point
		  (begin
		    (set-mark-index! anchor point)
		    (let ((end
			   (line-start-index group (re-match-start-index 0))))
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
	  (continue start)))
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
  (let ((pattern
	 (re-compile-pattern regexp (ref-variable case-fold-search start)))
	(syntax-table (ref-variable syntax-table start))
	(group (mark-group start))
	(start (mark-left-inserting-copy start))
	(end (mark-left-inserting-copy end)))
    (do ()
	((not (re-search-buffer-forward pattern
					syntax-table
					group
					(mark-index start)
					(mark-index end))))
      (let ((point (line-end-index group (re-match-end-index 0))))
	(set-mark-index! start point)
	(group-delete! group
		       (line-start-index group (re-match-start-index 0))
		       (if (< point (mark-index end)) (+ point 1) point))))
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
  (let ((pattern
	 (re-compile-pattern regexp (ref-variable case-fold-search start)))
	(syntax-table (ref-variable syntax-table start))
	(group (mark-group start))
	(end (mark-index end)))
    (let loop ((start (mark-index start)) (result 0))
      (let ((match
	     (re-search-buffer-forward pattern syntax-table group start end)))
	(if match
	    (loop match (+ result 1))
	    result)))))

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
	(pop-up-buffer buffer #t)
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
    (pop-up-occur-buffer (current-point)
			 (buffer-end (selected-buffer))
			 regexp
			 (and argument
			      (command-argument-numeric-value argument)))))

(define-command list-matching-lines
  (command-description (ref-command-object occur))
  (command-interactive-specification (ref-command-object occur))
  (command-procedure (ref-command-object occur)))

(define (pop-up-occur-buffer start end regexp n-lines)
  (let ((occur-buffer (temporary-buffer "*Occur*")))
    (let ((output (mark-left-inserting-copy (buffer-start occur-buffer))))
      (insert-string "Lines matching " output)
      (insert-string (write-to-string regexp) output)
      (insert-string " in buffer " output)
      (insert-string (buffer-name (mark-buffer start)) output)
      (insert-string ".\n" output)
      (set-buffer-major-mode! occur-buffer (ref-mode-object occur))
      (let ((occurrences
	     (format-occurrences
	      (let ((occurrences (re-occurrences start end regexp)))
		(for-each mark-permanent! occurrences)
		occurrences)
	      (or n-lines
		  (ref-variable list-matching-lines-default-context-lines
				start))
	      output)))
	(buffer-put! occur-buffer 'OCCURRENCES occurrences)
	(message (number->string (length occurrences)) " matching lines."))
      (mark-temporary! output))
    (set-buffer-point! occur-buffer (buffer-start occur-buffer))
    (pop-up-buffer occur-buffer #f)))

(define (re-occurrences start end regexp)
  (let ((pattern
	 (re-compile-pattern regexp (ref-variable case-fold-search start)))
	(syntax-table (ref-variable syntax-table start))
	(group (mark-group start))
	(end (mark-index end)))
    (let loop ((start (mark-index start)))
      (let ((match
	     (re-search-buffer-forward pattern syntax-table group start end)))
	(if match
	    (cons (make-temporary-mark group (line-start-index group match) #f)
		  (loop (line-end-index group match)))
	    '())))))

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
	  (let ((lstart (line-start lstart 1 #f)))
	    (if lstart
		(let loop ((lstart lstart) (n nlines))
		  (let ((lend (line-end lstart 0)))
		    (insert-string empty output)
		    (insert-string ":" output)
		    (insert-region lstart lend output)
		    (insert-newline output)
		    (if (and (not (group-end? lend)) (> n 1))
			(loop (mark1+ lend) (- n 1)))))))))))