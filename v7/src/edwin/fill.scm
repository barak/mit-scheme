;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/fill.scm,v 1.51 1991/10/03 20:48:44 cph Exp $
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

;;;; Text Fill Commands

(declare (usual-integrations))

(define-variable-per-buffer fill-column
  "Column beyond which automatic line-wrapping should happen.
Automatically becomes local when set in any fashion."
  70
  exact-nonnegative-integer?)

(define-command set-fill-column
  "Set fill-column to current column, or to argument if given.
fill-column's value is separate for each buffer."
  "P"
  (lambda (argument)
    (let ((column
	   (or (command-argument-value argument)
	       (current-column))))
      (set-variable! fill-column column)
      (message "fill-column set to " column))))

(define-variable-per-buffer fill-prefix
  "String for filling to insert at front of new line, or #f for none.
Setting this variable automatically makes it local to the current buffer."
  false
  string-or-false?)

(define-command set-fill-prefix
  "Set the fill-prefix to the current line up to point.
Filling expects lines to start with the fill prefix
and reinserts the fill prefix in each resulting line."
  "d"
  (lambda (point)
    (let ((string (extract-string (line-start point 0) point)))
      (if (string-null? string)
	  (begin
	    (set-variable! fill-prefix false)
	    (message "fill-prefix cancelled"))
	  (begin
	    (set-variable! fill-prefix string)
	    (message "fill-prefix: \"" string "\""))))))

(define-command fill-paragraph
  "Fill paragraph at or after point.
Prefix arg means justify as well."
  "d\nP"
  (lambda (point justify?)
    ((ref-command fill-region-as-paragraph) (paragraph-text-region point)
					    justify?)))

(define-command fill-region-as-paragraph
  "Fill region as one paragraph: break lines to fit fill-column.
Prefix arg means justify too."
  "r\nP"
  (lambda (region justify?)
    (let ((start (region-start region)))
      (fill-region-as-paragraph
       start
       (region-end region)
       (mark-local-ref start (ref-variable-object fill-prefix))
       (mark-local-ref start (ref-variable-object fill-column))
       justify?))))

(define-command fill-individual-paragraphs
  "Fill each paragraph in region according to its individual fill prefix."
  "r\nP"
  (lambda (region justify?)
    (let ((start (region-start region)))
      (fill-individual-paragraphs
       start
       (region-end region)
       (mark-local-ref start (ref-variable-object fill-column))
       justify?
       false))))

(define-command fill-region
  "Fill each of the paragraphs in the region.
Prefix arg means justify as well."
  "r\nP"
  (lambda (region justify?)
    (let ((start (region-start region)))
      (fill-region start
		   (region-end region)
		   (mark-local-ref start (ref-variable-object fill-prefix))
		   (mark-local-ref start (ref-variable-object fill-column))
		   justify?))))

(define-command justify-current-line
  "Add spaces to line point is in, so it ends at fill-column."
  "d"
  (lambda (point)
    (justify-line point
		  (mark-local-ref point (ref-variable-object fill-prefix))
		  (mark-local-ref point (ref-variable-object fill-column)))))

(define (fill-region-as-paragraph start end fill-prefix fill-column justify?)
  (let ((start (mark-right-inserting-copy (skip-chars-forward "\n" start end)))
	(end (mark-left-inserting-copy (skip-chars-backward "\n" end start))))
    (let ((point (mark-left-inserting-copy start)))
      ;; Delete the fill prefix from every line except the first.
      (if fill-prefix
	  (begin
	    (if (>= (string-length fill-prefix) fill-column)
		(editor-error "fill-prefix too long for specified width"))
	    (let ((m (match-forward fill-prefix start end false)))
	      (if m
		  (begin
		    (move-mark-to! point m)
		    (move-mark-to! start m))))
	    (let loop ()
	      (let ((m (char-search-forward #\newline point end)))
		(if m
		    (begin
		      (move-mark-to! point m)
		      (let ((m (match-forward fill-prefix point end false)))
			(if m
			    (delete-string point m)))
		      (loop)))))
	    (move-mark-to! point start)))
      ;; Make sure sentences ending at end of line get an extra space.
      (let loop ()
	(let ((m (re-search-forward "[.?!][])\"']*$" point end false)))
	  (if m
	      (begin
		(move-mark-to! point m)
		(insert-char #\space point)
		(loop)))))
      ;; Change all newlines to spaces.
      (move-mark-to! point start)
      (let loop ()
	(let ((m (char-search-forward #\newline point end)))
	  (if m
	      (begin
		(move-mark-to! point m)
		(delete-left-char point)
		(insert-char #\space point)
		(loop)))))
      ;; Flush excess spaces, except in the paragraph indentation.
      (move-mark-to! point (skip-chars-forward " \t" start end))
      (let loop ()
	(if (re-search-forward "   *" point end false)
	    (begin
	      (move-mark-to! point (delete-match))
	      (insert-string (if (fill:sentence-end? point start) "  " " ")
			     point)
	      (loop))))
      (delete-string (horizontal-space-start end) end)
      (insert-string "  " end)
      (move-mark-to! point start)
      (let loop ()
	(let ((target (move-to-column point fill-column)))
	  (if (mark>= target end)
	      (delete-string (horizontal-space-start end) end)
	      (begin
		(move-mark-to!
		 point
		 (let ((m (skip-chars-backward "^ \n" target point)))
		   (if (mark> m point)
		       m
		       (skip-chars-forward "^ \n" target end))))
		(if (mark< point end)
		    (begin
		      (delete-horizontal-space point)
		      (if (mark< point end) (insert-newline point))
		      (if justify?
			  (fill:call-with-line-marks (mark-1+ point)
						     fill-prefix
			    (lambda (start end)
			      (fill:justify-line start end fill-column))))
		      (if fill-prefix (insert-string fill-prefix point))))
		(loop)))))
      (mark-temporary! point)
      (mark-temporary! end)
      (mark-temporary! start))))

(define (fill-region start end fill-prefix fill-column justify?)
  (let ((start (mark-right-inserting-copy start))
	(end (mark-left-inserting-copy end))
	(point (mark-left-inserting-copy start))
	(pend (mark-left-inserting-copy start)))
    (let loop ()
      (if (mark< point end)
	  (begin
	    (move-mark-to! pend
			   (or (forward-one-paragraph point end fill-prefix)
			       end))
	    (if (mark>= (or (backward-one-paragraph pend start fill-prefix)
			    start)
			point)
		(fill-region-as-paragraph point
					  pend
					  fill-prefix
					  fill-column
					  justify?))
	    (move-mark-to! point pend)
	    (loop))))
    (mark-temporary! pend)
    (mark-temporary! point)
    (mark-temporary! end)
    (mark-temporary! start)))

(define (fill-individual-paragraphs start end fill-column justify? mail?)
  (let ((start (mark-right-inserting-copy start))
	(end (mark-left-inserting-copy end))
	(point (mark-left-inserting-copy start))
	(pend (mark-left-inserting-copy start)))
    (let loop ()
      (move-mark-to! point (skip-chars-forward " \t\n" point end))
      (if (mark< point end)
	  (let ((fill-prefix (extract-string (line-start point 0) point)))
	    (move-mark-to! pend
			   (or (forward-one-paragraph point end fill-prefix)
			       end))
	    (let ((m
		   (if mail?
		       (let loop ((m point))
			 (let ((m*
				(re-search-forward "^[ \t]*[^ \t\n]*:" m pend
						   false)))
			   (if m*
			       (let ((m* (line-end m* 0)))
				 (if (mark< m* pend)
				     (loop (mark1+ m*))
				     pend))
			       m)))
		       point)))
	      (if (mark= m point)
		  (begin
		    (fill-region-as-paragraph point pend
					      fill-prefix fill-column
					      justify?)
		    (move-mark-to! point pend)
		    (loop))
		  (begin
		    (move-mark-to! point m)
		    (loop)))))))
    (mark-temporary! pend)
    (mark-temporary! point)
    (mark-temporary! end)
    (mark-temporary! start)))

(define (justify-line mark fill-prefix fill-column)
  (fill:call-with-line-marks mark fill-prefix
    (lambda (start end)
      (let ((point (mark-left-inserting-copy start)))
	(let loop ()
	  (if (re-search-forward "   *" point end false)
	      (begin
		(move-mark-to! point (delete-match))
		(insert-string (if (fill:sentence-end? point start) "  " " ")
			       point)
		(loop))))
	(mark-temporary! point))
      (fill:justify-line start end fill-column))))

(define (fill:call-with-line-marks mark fill-prefix procedure)
  (let ((end (mark-left-inserting-copy (line-end mark 0))))
    (let ((start
	   (mark-right-inserting-copy
	    (skip-chars-forward
	     " \t"
	     (let ((start (line-start end 0)))
	       (or (and fill-prefix
			(match-forward fill-prefix start end false))
		   start))
	     end))))
      (procedure start end)
      (mark-temporary! start)
      (mark-temporary! end))))

(define (fill:justify-line start end fill-column)
  (let ((point (mark-right-inserting-copy end)))
    (do ((ncols (- fill-column (mark-column end)) (- ncols 1)))
	((<= ncols 0))
      (do ((i (+ 3 (random 3)) (- i 1)))
	  ((= i 0))
	(move-mark-to!
	 point
	 (skip-chars-backward " "
			      (or (char-search-backward #\space point start)
				  (char-search-backward #\space end start)
				  start)
			      start)))
      (insert-char #\space point))
    (mark-temporary! point)))

(define (fill:sentence-end? point start)
  (let ((m (skip-chars-backward "])\"'" point start)))
    (and (not (group-start? m))
	 (memv (extract-left-char m) '(#\. #\? #\!)))))

(define-command auto-fill-mode
  "Toggle auto-fill mode.
With argument, turn auto-fill mode on iff argument is positive."
  "P"
  (lambda (argument)
    (let ((argument (command-argument-value argument))
	  (mode (ref-mode-object auto-fill)))
      (cond ((and (or (not argument) (positive? argument))
		  (not (current-minor-mode? mode)))
	     (enable-current-minor-mode! mode))
	    ((and (or (not argument) (not (positive? argument)))
		  (current-minor-mode? mode))
	     (disable-current-minor-mode! mode))))))

(define-command auto-fill-space
  "Breaks the line if it exceeds the fill column, then inserts a space."
  "p"
  (lambda (argument)
    (insert-chars #\space argument)
    (auto-fill-break)))

(define-command auto-fill-newline
  "Breaks the line if it exceeds the fill column, then inserts a newline."
  "P"
  (lambda (argument)
    (auto-fill-break)
    ((ref-command newline) argument)))

(define-minor-mode auto-fill "Fill" "")
(define-key 'auto-fill #\space 'auto-fill-space)
(define-key 'auto-fill #\return 'auto-fill-newline)

(define (auto-fill-break)
  (let ((point (current-point)))
    (if (auto-fill-break? point)
	(if (re-search-backward "[^ \t][ \t]+"
				(move-to-column
				 point
				 (1+ (ref-variable fill-column)))
				(line-start point 0))
	    (with-current-point (re-match-end 0)
	      (ref-command indent-new-comment-line))))))

(define (auto-fill-break? point)
  (and (> (mark-column point) (ref-variable fill-column))
       (line-end? (horizontal-space-end point))))

(define-variable-per-buffer left-margin
  "Column for the default indent-line-function to indent to.
Linefeed indents to this column in Fundamental mode.
Automatically becomes local when set in any fashion."
  0
  exact-nonnegative-integer?)

(define (center-line mark)
  (let ((mark (mark-permanent! mark)))
    (delete-horizontal-space (line-start mark 0))
    (delete-horizontal-space (line-end mark 0))
    (let ((d (- (- (ref-variable fill-column) (ref-variable left-margin))
		(mark-column (line-end mark 0)))))
      (if (positive? d)
	  (insert-horizontal-space (+ (ref-variable left-margin)
				      (quotient d 2))
				   (line-start mark 0))))))

(define-command center-line
  "Center the line point is on, within the width specified by `fill-column'.
This means adjusting the indentation to match
the distance between the end of the text and `fill-column'."
  "d"
  center-line)