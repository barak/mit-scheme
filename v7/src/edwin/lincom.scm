;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/lincom.scm,v 1.104 1989/08/08 10:06:12 cph Exp $
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
;;; NOTE: Parts of this program (Edwin) were created by translation
;;; from corresponding parts of GNU Emacs.  Users should be aware that
;;; the GNU GENERAL PUBLIC LICENSE may apply to these parts.  A copy
;;; of that license should have been included along with this file.
;;;

;;;; Line/Indentation Commands

(declare (usual-integrations))

;;;; Lines

(define-command count-lines-region
  "Type number of lines from point to mark."
  "r"
  (lambda (region)
    (message "Region has "
	     (write-to-string (region-count-lines region))
	     " lines")))

(define-command transpose-lines
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
  "p"
  (lambda (argument)
    (cond ((and (= argument 1) (group-end? (current-point)))
	   (if (not (line-start? (current-point)))
	       (insert-newlines 1))
	   (let ((region
		  (region-extract!
		   (make-region (forward-line (current-point) -2 'ERROR)
				(forward-line (current-point) -1 'ERROR)))))
	     (region-insert! (current-point) region)))
	  (else
	   (transpose-things forward-line argument)))))

;;;; Pages

(define-command forward-page
  "Move forward to page boundary.  With arg, repeat, or go back if negative.
A page boundary is any string in Page Delimiters, at a line's beginning."
  "p"
  (lambda (argument)
    (set-current-point! (forward-page (current-point) argument 'BEEP))))

(define-command backward-page
  "Move backward to page boundary.  With arg, repeat, or go fwd if negative.
A page boundary is any string in Page Delimiters, at a line's beginning."
  "p"
  (lambda (argument)
    (set-current-point! (backward-page (current-point) argument 'BEEP))))

(define-command mark-page
  "Put mark at end of page, point at beginning."
  "P"
  (lambda (argument)
    (let ((end (forward-page (current-point) (1+ (or argument 0)) 'LIMIT)))
      (set-current-region! (make-region (backward-page end 1 'LIMIT) end)))))

(define-command narrow-to-page
  "Make text outside current page invisible."
  "d"
  (lambda (mark)
    (region-clip! (page-interior-region mark))))

(define (page-interior-region point)
  (if (and (group-end? point)
	   (mark= (re-match-forward (ref-variable page-delimiter)
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

(define-command count-lines-page
  "Report number of lines on current page."
  "d"
  (lambda (point)
    (let ((end
	   (let ((end (forward-page point 1 'LIMIT)))
	     (if (group-end? end) end (line-start end 0)))))
      (let ((start (backward-page end 1 'LIMIT)))
	(message "Page has " (count-lines-string start end)
		 " lines (" (count-lines-string start point)
		 " + " (count-lines-string point end) ")")))))

(define (count-lines-string start end)
  (write-to-string (region-count-lines (make-region start end))))

(define-command what-page
  "Report page and line number of point."
  ()
  (lambda ()
    (without-group-clipped! (buffer-group (current-buffer))
      (lambda ()
	(message "Page " (write-to-string (current-page))
		 ", Line " (write-to-string (current-line)))))))

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

(define (indent-to-left-margin)
  (maybe-change-indentation (ref-variable left-margin)
			    (line-start (current-point) 0)))

(define-variable indent-line-procedure
  "Procedure used to indent current line.
If this is the procedure indent-to-left-margin,
\\[indent-for-tab-command] will insert tab characters rather than indenting."
  indent-to-left-margin)

(define-command indent-according-to-mode
  "Indent line in proper way for current major mode.
The exact behavior of this command is determined
by the variable indent-line-procedure."
  ()
  (lambda ()
    ((ref-variable indent-line-procedure))))

(define-command indent-for-tab-command
  "Indent line in proper way for current major mode.
The exact behavior of this command is determined
by the variable indent-line-procedure."
  "p"
  (lambda (argument)
    (let ((indent-line-procedure (ref-variable indent-line-procedure)))
      (if (eq? indent-line-procedure indent-to-left-margin)
	  (insert-chars #\Tab argument)
	  (indent-line-procedure)))))

(define-command newline-and-indent
  "Insert a newline, then indent according to major mode.
Indentation is done using the current indent-line-procedure,
except that if there is a fill-prefix it is used to indent.
In programming language modes, this is the same as TAB.
In some text modes, where TAB inserts a tab, this indents to the
specified left-margin column."
  ()
  (lambda ()
    (delete-horizontal-space)
    (insert-newlines 1)
    (let ((fill-prefix (ref-variable fill-prefix)))
      (if fill-prefix
	  (region-insert-string! (current-point) fill-prefix)
	  ((ref-command indent-according-to-mode))))))

(define-command reindent-then-newline-and-indent
  "Reindent the current line according to mode (like
\\[indent-according-to-mode]), then insert a newline,
and indent the new line indent according to mode."
  ()
  (lambda ()
    (delete-horizontal-space)
    ((ref-command indent-according-to-mode))
    ((ref-command newline))
    ((ref-command indent-according-to-mode))))

(define-command newline
  "Insert newline, or move onto blank line.
A blank line is one containing only spaces and tabs
\(which are killed if we move onto it).  Single blank lines
\(followed by nonblank lines) are not eaten up this way.
An argument inhibits this."
  "P"
  (lambda (argument)
    (cond ((not argument)
	   (if (line-end? (current-point))
	       (let ((m1 (line-start (current-point) 1)))
		 (if (and m1
			  (line-blank? m1)
			  (let ((m2 (line-start m1 1)))
			    (and m2
				 (line-blank? m2))))
		     (begin
		       (set-current-point! m1)
		       (delete-horizontal-space))
		     (insert-newlines 1)))
	       (insert-newlines 1)))
	  (else
	   (insert-newlines argument)))))

(define-command split-line
  "Move rest of this line vertically down.
Inserts a newline, and then enough tabs/spaces so that
what had been the rest of the current line is indented as much as
it had been.  Point does not move, except to skip over indentation
that originally followed it. 
With argument, makes extra blank lines in between."
  "p"
  (lambda (argument)
    (set-current-point! (horizontal-space-end (current-point)))
    (let ((m* (mark-right-inserting (current-point))))
      (insert-newlines (max argument 1))
      (insert-horizontal-space (mark-column m*))
      (set-current-point! m*))))

(define-command back-to-indentation
  "Move to end of this line's indentation."
  ()
  (lambda ()
    (set-current-point!
     (horizontal-space-end (line-start (current-point) 0)))))

(define-command delete-horizontal-space
  "Delete all spaces and tabs around point."
  ()
  delete-horizontal-space)

(define-command just-one-space
  "Delete all spaces and tabs around point, leaving just one space."
  ()
  (lambda ()
    (delete-horizontal-space)
    (insert-chars #\Space 1)))

(define-command delete-blank-lines
  "Kill all blank lines around this line's end.
If done on a non-blank line, kills all spaces and tabs at the end of
it, and all following blank lines (Lines are blank if they contain
only spaces and tabs).
If done on a blank line, deletes all preceding blank lines as well."
  ()
  (lambda ()
    (region-delete!
     (let ((point (current-point)))
       (make-region (if (line-blank? point)
			(let loop ((m1 (line-start point 0)))
			  (let ((m2 (line-start m1 -1)))
			    (if (and m2 (line-blank? m2))
				(loop m2)
				m1)))
			(horizontal-space-start (line-end point 0)))
		    (line-end (let loop ((m1 point))
				(let ((m2 (line-start m1 1)))
				  (if (and m2 (line-blank? m2))
				      (loop m2)
				      m1)))
			      0))))))

(define-command delete-indentation
  "Kill newline and indentation at front of line.
Leaves one space in place of them.  With argument,
moves down one line first (killing newline after current line)."
  "P"
  (lambda (argument)
    (set-current-point!
     (horizontal-space-start
      (line-end (current-point) (if (not argument) -1 0) 'ERROR)))
    (let ((point (current-point)))
      (region-delete! (make-region point (line-start point 1 'ERROR)))
      (if (ref-variable fill-prefix)
	  (let ((match (match-forward (ref-variable fill-prefix))))
	    (if match (delete-string match))))
      (delete-horizontal-space)
      (if (or (line-start? point)
	      (line-end? point)
	      (not (or (char-set-member?
			(ref-variable delete-indentation-right-protected)
			(mark-left-char point))
		       (char-set-member?
			(ref-variable delete-indentation-left-protected)
			(mark-right-char point)))))
	  (insert-chars #\Space 1)))))

(define-variable delete-indentation-right-protected
  "\\[delete-indentation] won't insert a space to the right of these."
  (char-set #\( #\,))

(define-variable delete-indentation-left-protected
  "\\[delete-indentation] won't insert a space to the left of these."
  (char-set #\)))

(define-variable-per-buffer tab-width
  "Distance between tab stops (for display of tab characters), in columns.
Automatically becomes local when set in any fashion."
  8)

(define-variable indent-tabs-mode
  "If false, do not use tabs for indentation or horizontal spacing."
  true)

(define-command indent-tabs-mode
  "Enables or disables use of tabs as indentation.
A positive argument turns use of tabs on;
zero or negative, turns it off.
With no argument, the mode is toggled."
  "P"
  (lambda (argument)
    (set-variable! indent-tabs-mode
		   (if argument
		       (positive? argument)
		       (not (ref-variable indent-tabs-mode))))))

(define-command insert-tab
  "Insert a tab character."
  ()
  (lambda ()
    (if (ref-variable indent-tabs-mode)
	(insert-char #\Tab)
	(maybe-change-column
	 (let ((tab-width (ref-variable tab-width)))
	   (* tab-width (1+ (quotient (current-column) tab-width))))))))

(define-command indent-region
  "Indent all lines between point and mark.
With argument, indents each line to exactly that column.
Otherwise, does Tab on each line.
A line is processed if its first character is in the region.
The mark is left after the last line processed."
  "P"
  (lambda (argument)
    (cond ((not argument)
	   (not-implemented))
	  ((not (negative? argument))
	   (current-region-of-lines
	    (lambda (start end)
	      (let loop ((mark start))
		(change-indentation argument mark)
		(if (not (mark= mark end))
		    (loop (mark-right-inserting (line-start mark 1)))))))))))

(define-command indent-rigidly
  "Shift text in region sideways as a unit.
All the lines in the region (first character between point and mark)
have their indentation incremented by the numeric argument
of this command (which may be negative).
Exception: lines containing just spaces and tabs become empty."
  "P"
  (lambda (argument)
    (if argument
	(current-region-of-lines
	 (lambda (start end)
	   (define (loop mark)
	     (if (line-blank? mark)
		 (delete-horizontal-space mark)
		 (change-indentation
		  (max (+ argument (current-indentation mark)) 0)
		  mark))
	     (if (not (mark= mark end))
		 (loop (mark-right-inserting (line-start mark 1)))))
	   (loop start))))))

(define (current-region-of-lines receiver)
  (let ((r (current-region)))
    (let ((start (mark-right-inserting (line-start (region-start r) 0))))
      (receiver start
		(if (mark= start (line-start (region-end r) 0))
		    start
		    (mark-right-inserting
		     (line-start (region-end r)
				 (if (line-start? (region-end r)) -1 0))))))))

(define (untabify-region region)
  (let ((end (region-end region)))
    (let loop ((start (region-start region)))
      (if (char-search-forward #\Tab start end)
	  (let ((tab (re-match-start 0))
		(next (mark-left-inserting (re-match-end 0))))
	    (let ((n-spaces (- (mark-column next) (mark-column tab))))
	      (delete-string tab next)
	      (insert-chars #\Space n-spaces next))
	    (loop next))))))

(define-command untabify
  "Convert all tabs in region to multiple spaces, preserving columns.
The variable tab-width controls the action."
  "r"
  untabify-region)

(define-command tabify
  "Convert multiple spaces in region to tabs when possible.
A group of spaces is partially replaced by tabs
when this can be done without changing the column they end at.
The variable tab-width controls the action."
  ()
  (lambda ()
    (not-implemented)))

(define-command indent-relative
  "Indents the current line directly below the previous non blank line."
  "d"
  (lambda (point)
    (let ((indentation (indentation-of-previous-non-blank-line point)))
      (cond ((not (= indentation (current-indentation point)))
	     (change-indentation indentation point))
	    ((line-start? (horizontal-space-start point))
	     (set-current-point! (horizontal-space-end point)))))))

(define (indentation-of-previous-non-blank-line mark)
  (let ((start (find-previous-non-blank-line mark)))
    (if start (current-indentation start) 0)))