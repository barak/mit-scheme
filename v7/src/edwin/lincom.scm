;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/lincom.scm,v 1.101 1989/03/14 08:01:14 cph Exp $
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

;;;; Line/Indentation Commands

(declare (usual-integrations))

;;;; Lines

(define-command ("^R Count Lines Region")
  "Type number of lines from point to mark."
  (message "Region has "
	   (write-to-string (region-count-lines (current-region)))
	   " lines"))

(define-command ("^R Transpose Lines" (argument 1))
  "Transpose the lines before and after the cursor.
With a positive argument it transposes the lines before and after the
cursor, moves right, and repeats the specified number of times,
dragging the line to the left of the cursor right.

With a negative argument, it transposes the two lines to the left of
the cursor, moves between them, and repeats the specified number of
times, exactly undoing the positive argument form.

With a zero argument, it transposes the lines at point and mark.

At the end of a buffer, with no argument, the preceding two lines are
transposed."

  (cond ((and (= argument 1) (group-end? (current-point)))
	 (if (not (line-start? (current-point)))
	     (insert-newlines 1))
	 (let ((region
		(region-extract!
		 (make-region (forward-line (current-point) -2 'ERROR)
			      (forward-line (current-point) -1 'ERROR)))))
	   (region-insert! (current-point) region)))
	(else
	 (transpose-things forward-line argument))))

;;;; Pages

(define-command ("^R Next Page" (argument 1))
  "Move forward to page boundary.  With arg, repeat, or go back if negative.
A page boundary is any string in Page Delimiters, at a line's beginning."
  (set-current-point! (forward-page (current-point) argument 'BEEP)))

(define-command ("^R Previous Page" (argument 1))
  "Move backward to page boundary.  With arg, repeat, or go fwd if negative.
A page boundary is any string in Page Delimiters, at a line's beginning."
  (set-current-point! (backward-page (current-point) argument 'BEEP)))

(define-command ("^R Mark Page" (argument 0))
  "Put mark at end of page, point at beginning."
  (let ((end (forward-page (current-point) (1+ argument) 'LIMIT)))
    (set-current-region! (make-region (backward-page end 1 'LIMIT) end))))

(define-command ("^R Narrow Bounds to Page")
  "Make text outside current page invisible."
  (region-clip! (page-interior-region (current-point))))

(define (page-interior-region point)
  (if (and (group-end? point)
	   (mark= (re-match-forward (ref-variable "Page Delimiter")
				    (line-start point 0)
				    point)
		  point))
      (make-region point point)
      (let ((end (forward-page point 1 'LIMIT)))
	(make-region (backward-page end 1 'LIMIT)
		     (let ((end* (line-end end -1 'LIMIT)))
		       (if (mark< end* point)
			   end
			   end*))))))

(define-command ("^R Count Lines Page")
  "Report number of lines on current page."
  (let ((point (current-point)))
    (let ((end
	   (let ((end (forward-page point 1 'LIMIT)))
	     (if (group-end? end) end (line-start end 0)))))
      (let ((start (backward-page end 1 'LIMIT)))
	(message "Page has " (count-lines-string start end)
		 " lines (" (count-lines-string start point)
		 " + " (count-lines-string point end) ")")))))

(define (count-lines-string start end)
  (write-to-string (region-count-lines (make-region start end))))

(define-command ("What Page")
  "Report page and line number of point."
  (without-group-clipped! (buffer-group (current-buffer))
    (lambda ()
      (message "Page " (write-to-string (current-page))
	       ", Line " (write-to-string (current-line))))))

(define (current-page)
  (region-count-pages (make-region (buffer-start (current-buffer))
				   (current-point))))

(define (current-line)
  (region-count-lines
   (make-region (backward-page (forward-page (current-point) 1 'LIMIT)
			       1 'LIMIT)
		(current-point))))

(define (region-count-pages region)
  (let ((end (region-end region)))
    (define (loop count start)
      (if (or (not start) (mark> start end))
	  count
	  (loop (1+ count) (forward-page start 1))))
    (loop 0 (region-start region))))

;;;; Indentation

(define (indent-to-left-margin argument)
  argument				;ignore
  (maybe-change-indentation (ref-variable "Left Margin")
			    (line-start (current-point) 0)))

(define-variable "Indent Line Procedure"
  "Procedure used to indent current line.
If this is the procedure INDENT-TO-LEFT-MARGIN,
\\[^R Indent for Tab] will insert tab characters rather than indenting."
  indent-to-left-margin)

(define-command ("^R Indent According to Mode" argument)
  "Indent line in proper way for current major mode.
The exact behavior of this command is determined
by the variable Indent Line Procedure."
  ((ref-variable "Indent Line Procedure") argument))

(define-command ("^R Indent for Tab" argument)
  "Indent line in proper way for current major mode.
The exact behavior of this command is determined
by the variable Indent Line Procedure."
  (if (eq? (ref-variable "Indent Line Procedure") indent-to-left-margin)
      (insert-chars #\Tab (or argument 1))
      ((ref-variable "Indent Line Procedure") argument)))

(define-command ("^R Tab" (argument 1))
  "Insert a tab character."
  (insert-chars #\Tab argument))

(define-command ("^R Indent New Line" argument)
  "Inserts newline, then indents the second line.
Any spaces before the inserted newline are deleted.
Uses Indent Line Procedure to do the indentation,
except that if there is a Fill Prefix it is used to indent.
An argument is passed on to Indent Line Procedure."
  (delete-horizontal-space)
  (^r-newline-command)
  (if (ref-variable "Fill Prefix")
      (region-insert-string! (current-point) (ref-variable "Fill Prefix"))
      (^r-indent-according-to-mode-command argument)))

(define-command ("Reindent then Newline and Indent")
  "Reindent the current line according to mode (like Tab), then insert
a newline, and indent the new line indent according to mode."
  (delete-horizontal-space)
  (^r-indent-according-to-mode-command false)
  (^r-newline-command)
  (^r-indent-according-to-mode-command false))

(define-command ("^R Newline" argument)
  "Insert newline, or move onto blank line.
A blank line is one containing only spaces and tabs
\(which are killed if we move onto it).  Single blank lines
\(followed by nonblank lines) are not eaten up this way.
An argument inhibits this."
  (cond ((not argument)
	 (if (line-end? (current-point))
	     (let ((m1 (line-start (current-point) 1)))
	       (if (and m1 (line-blank? m1)
			(let ((m2 (line-start m1 1)))
			  (and m2 (line-blank? m2))))
		   (begin (set-current-point! m1)
			  (delete-horizontal-space))
		   (insert-newlines 1)))
	     (insert-newlines 1)))
	(else
	 (insert-newlines argument))))

(define-command ("^R Split Line" (argument 1))
  "Move rest of this line vertically down.
Inserts a newline, and then enough tabs/spaces so that
what had been the rest of the current line is indented as much as
it had been.  Point does not move, except to skip over indentation
that originally followed it. 
With argument, makes extra blank lines in between."
  (set-current-point! (horizontal-space-end (current-point)))
  (let ((m* (mark-right-inserting (current-point))))
    (insert-newlines (max argument 1))
    (insert-horizontal-space (mark-column m*))
    (set-current-point! m*)))

(define-command ("^R Back to Indentation")
  "Move to end of this line's indentation."
  (set-current-point! (horizontal-space-end (line-start (current-point) 0))))

(define-command ("^R Delete Horizontal Space")
  "Delete all spaces and tabs around point."
  (delete-horizontal-space))

(define-command ("^R Just One Space")
  "Delete all spaces and tabs around point, leaving just one space."
  (delete-horizontal-space)
  (insert-chars #\Space 1))

(define-command ("^R Delete Blank Lines")
  "Kill all blank lines around this line's end.
If done on a non-blank line, kills all spaces and tabs at the end of
it, and all following blank lines (Lines are blank if they contain
only spaces and tabs).
If done on a blank line, deletes all preceding blank lines as well."
  (define (find-first-blank m1)
    (let ((m2 (line-start m1 -1)))
      (cond ((not m2) m1)
	    ((not (line-blank? m2)) m1)
	    (else (find-first-blank m2)))))
  (define (find-last-blank m1)
    (let ((m2 (line-start m1 1)))
      (cond ((not m2) m1)
	    ((not (line-blank? m2)) m1)
	    (else (find-last-blank m2)))))
  (region-delete!
   (let ((point (current-point)))
     (make-region (if (line-blank? point)
		      (find-first-blank (line-start point 0))
		      (horizontal-space-start (line-end point 0)))
		  (line-end (find-last-blank point) 0)))))

(define-command ("^R Delete Indentation" argument)
  "Kill newline and indentation at front of line.
Leaves one space in place of them.  With argument,
moves down one line first (killing newline after current line)."
  (set-current-point!
   (horizontal-space-start
    (line-end (current-point) (if (not argument) -1 0) 'ERROR)))
  (let ((point (current-point)))
    (region-delete! (make-region point (line-start point 1 'ERROR)))
    (if fill-prefix
	(let ((match (match-forward fill-prefix)))
	  (if match (delete-string match))))
    (delete-horizontal-space)
    (if (or (line-start? point)
	    (line-end? point)
	    (not (or (char-set-member? delete-indentation-right-protected
				       (mark-left-char point))
		     (char-set-member? delete-indentation-left-protected
				       (mark-right-char point)))))
	(insert-chars #\Space 1))))

(define-variable "Delete Indentation Right Protected"
  "^R Delete Indentation won't insert a space to the right of these."
  (char-set #\( #\,))

(define-variable "Delete Indentation Left Protected"
  "^R Delete Indentation won't insert a space to the left of these."
  (char-set #\)))

(define-variable "Indent Tabs Mode"
  "If #F, do not use tabs for indentation or horizontal spacing."
  true)

(define-command ("Indent Tabs Mode" argument)
  "Enables or disables use of tabs as indentation.
A positive argument turns use of tabs on;
zero or negative, turns it off.
With no argument, the mode is toggled."
  (set! indent-tabs-mode
	(if argument
	    (positive? argument)
	    (not indent-tabs-mode))))

(define-command ("^R Indent Region" argument)
  "Indent all lines between point and mark.
With argument, indents each line to exactly that column.
Otherwise, does Tab on each line.
A line is processed if its first character is in the region.
The mark is left after the last line processed."
  (cond ((not argument) (not-implemented))
	((not (negative? argument))
	 (current-region-of-lines
	  (lambda (start end)
	    (define (loop mark)
	      (change-indentation argument mark)
	      (if (not (mark= mark end))
		  (loop (mark-right-inserting (line-start mark 1)))))
	    (loop start))))))

(define-command ("^R Indent Rigidly" argument)
  "Shift text in region sideways as a unit.
All the lines in the region (first character between point and mark)
have their indentation incremented by the numeric argument
of this command (which may be negative).
Exception: lines containing just spaces and tabs become empty."
  (if argument
      (current-region-of-lines
       (lambda (start end)
	 (define (loop mark)
	   (if (line-blank? mark)
	       (delete-horizontal-space mark)
	       (change-indentation (max (+ argument (current-indentation mark))
					0)
				   mark))
	   (if (not (mark= mark end))
	       (loop (mark-right-inserting (line-start mark 1)))))
	 (loop start)))))

(define (current-region-of-lines receiver)
  (let ((r (current-region)))
    (let ((start (mark-right-inserting (line-start (region-start r) 0))))
      (receiver start
		(if (mark= start (line-start (region-end r) 0))
		    start
		    (mark-right-inserting
		     (line-start (region-end r)
				 (if (line-start? (region-end r)) -1 0))))))))

(define-variable "Tab Width"
  "Distance between tab stops (for display of tab characters), in columns."
  8)

(define-command ("Untabify")
  "Convert all tabs in region to multiple spaces, preserving column.
The variable Tabs Width controls action."
  (untabify-region (current-region)))

(define (untabify-region region)
  (let ((end (region-end region)))
    (define (loop start)
      (if (char-search-forward #\Tab start end)
	  (let ((tab (re-match-start 0))
		(next (mark-left-inserting (re-match-end 0))))
	    (let ((n-spaces (- (mark-column next) (mark-column tab))))
	      (delete-string tab next)
	      (insert-chars #\Space n-spaces next))
	    (loop next))))
    (loop (region-start region))))

(define-command ("Tabify")
  ""
  (not-implemented))

(define-command ("^R Indent Relative")
  "Indents the current line directly below the previous non blank line."
  (let ((point (current-point)))
    (let ((indentation (indentation-of-previous-non-blank-line point)))
      (cond ((not (= indentation (current-indentation point)))
	     (change-indentation indentation point))
	    ((line-start? (horizontal-space-start point))
	     (set-current-point! (horizontal-space-end point)))))))

(define (indentation-of-previous-non-blank-line mark)
  (let ((start (find-previous-non-blank-line mark)))
    (if start (current-indentation start) 0)))

(define-command ("^R Tab to Tab Stop")
  ""
  (not-implemented))

(define-command ("Edit Indented Text")
  ""
  (not-implemented))

(define-command ("Edit Tab Stops")
  ""
  (not-implemented))

(define-command ("Edit Tabular Text")
  ""
  (not-implemented))