;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/fill.scm,v 1.44 1989/04/28 22:49:55 cph Rel $
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

;;;; Text Fill Commands

(declare (usual-integrations))

(define-command fill-paragraph
  "Fill this (or next) paragraph.
Point stays the same."
  ()
  (lambda ()
    (fill-region (paragraph-text-region (current-point)))))

(define-command fill-region
  "Fill text from point to mark."
  "r"
  (lambda (region)
    (fill-region region)))

(define-variable fill-column
  "Controls where \\[fill-paragraph] and auto-fill mode put the right margin."
  70)

(define-command set-fill-column
  "Set fill column to argument or current column.
If an argument is given, that is used.
Otherwise the current position of the cursor is used."
  "P"
  (lambda (argument)
    (let ((column (or argument (current-column))))
      (local-set-variable! fill-column column)
      (temporary-message "Fill column set to " (write-to-string column)))))

(define-variable fill-prefix
  "String for auto-fill to insert at start of new line, or #F."
  false)

(define-command set-fill-prefix
  "Set fill-prefix to text between point and start of line."
  ()
  (lambda ()
    (if (line-start? (current-point))
	(begin
	  (local-set-variable! fill-prefix false)
	  (temporary-message "Fill prefix cancelled"))
	(let ((string (extract-string (line-start (current-point) 0))))
	  (local-set-variable! fill-prefix string)
	  (temporary-message "Fill prefix now \""
			     (ref-variable fill-prefix)
			     "\"")))))

(define fill-region
  (let ()
    (define (fill-region-loop start)
      (if (not (group-end? start))
	  (begin
	    (if (ref-variable fill-prefix)
		(insert-string (ref-variable fill-prefix) start))
	    (let ((target (move-to-column start (ref-variable fill-column))))
	      (if (not (group-end? target))
		  (let ((end
			 (cond ((char-search-backward #\Space
						      (mark1+ target)
						      start)
				(re-match-end 0))
			       ((char-search-forward #\Space target)
				(re-match-start 0))
			       (else false))))
		    (if end
			(let ((start (mark-left-inserting end)))
			  (delete-horizontal-space start)
			  (insert-newline start)
			  (fill-region-loop start)))))))))

    (define (canonicalize-sentence-endings mark)
      (let ((ending (forward-sentence mark 1 false)))
	(if (and ending (not (group-end? ending)))
	    (if (char=? #\newline (mark-right-char ending))
		(let ((mark (mark-left-inserting ending)))
		  (insert-char #\Space mark)
		  (canonicalize-sentence-endings mark))
		(canonicalize-sentence-endings ending)))))

    (define (canonicalize-spacing mark)
      (if (char-search-forward #\newline mark)
	  (let ((mark (mark-left-inserting (re-match-start 0))))
	    (replace-next-char mark #\Space)
	    (remove-fill-prefix mark)
	    (canonicalize-spacing mark))))

    (define (remove-fill-prefix mark)
      (if (ref-variable fill-prefix)
	  (let ((end (match-forward (ref-variable fill-prefix) mark)))
	    (if end (delete-string mark end)))))

    (define (replace-next-char mark char)
      (delete-string mark (mark1+ mark))
      (insert-char char mark))

    (named-lambda (fill-region region)
      (let ((start (region-start region))
	    (end (region-end region)))
	(let ((start
	       (mark-right-inserting (skip-chars-forward "\n" start end)))
	      (end (mark-left-inserting (skip-chars-backward "\n" end start))))
	  (with-narrowed-region! (make-region start end)
				 (lambda ()
				   (canonicalize-sentence-endings start)
				   (remove-fill-prefix start)
				   (canonicalize-spacing start)
				   (delete-horizontal-space end)
				   (fill-region-loop start))))))))

(define-command auto-fill-mode
  "Toggle auto-fill mode.
With argument, turn auto-fill mode on iff argument is positive."
  "P"
  (lambda (argument)
    (let ((mode (ref-mode-object auto-fill)))
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
    (insert-chars #\Space argument)
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

(define-variable left-margin
  "The number of columns to indent each line."  0)

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