;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/fill.scm,v 1.41 1989/03/14 08:00:45 cph Exp $
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

;;;; Text Fill Commands

(declare (usual-integrations))

(define-command ("^R Fill Paragraph")
  "Fill this (or next) paragraph.
Point stays the same."
  (fill-region (paragraph-text-region (current-point))))

(define-command ("^R Fill Region")
  "Fill text from point to mark."
  (fill-region (current-region)))

(define-variable "Fill Column"
  "Controls where ^R Fill Paragraph and Auto Fill mode put the right margin."
  70)

(define-command ("^R Set Fill Column" argument)
  "Set fill column to argument or current column.
If an argument is given, that is used.
Otherwise the current position of the cursor is used."
  (local-set-variable! "Fill Column"
		       (or argument (current-column)))
  (temporary-message "Fill column set to "
		     (write-to-string (ref-variable "Fill Column"))))

(define-variable "Fill Prefix"
  "String for Auto Fill to insert at start of new line, or #F."
  false)

(define-command ("^R Set Fill Prefix")
  "Set fill prefix to text between point and start of line."
  (if (line-start? (current-point))
      (begin (local-set-variable! "Fill Prefix" false)
	     (temporary-message "Fill prefix cancelled"))
      (let ((string (extract-string (line-start (current-point) 0))))
	(local-set-variable! "Fill Prefix" string)
	(temporary-message "Fill prefix now \""
			   (ref-variable "Fill Prefix")
			   "\""))))

(define fill-region)
(let ()

(set! fill-region
(named-lambda (fill-region region)
  (let ((start (region-start region))
	(end (region-end region)))
    (let ((start (mark-right-inserting (skip-chars-forward "\n" start end)))
	  (end (mark-left-inserting (skip-chars-backward "\n" end start))))
      (with-narrowed-region! (make-region start end)
	(lambda ()
	  (canonicalize-sentence-endings start)
	  (remove-fill-prefix start)
	  (canonicalize-spacing start)
	  (delete-horizontal-space end)
	  (fill-region-loop start)))))))

(define (fill-region-loop start)
  (if (not (group-end? start))
      (begin
       (if (ref-variable "Fill Prefix")
	   (insert-string (ref-variable "Fill Prefix") start))
       (let ((target (move-to-column start (ref-variable "Fill Column"))))
	 (if (not (group-end? target))
	     (let ((end
		    (cond ((char-search-backward #\Space (mark1+ target) start)
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
  (if (ref-variable "Fill Prefix")
      (let ((end (match-forward (ref-variable "Fill Prefix") mark)))
	(if end (delete-string mark end)))))

(define (replace-next-char mark char)
  (delete-string mark (mark1+ mark))
  (insert-char char mark))

)

(define-command ("Auto Fill Mode" argument)
  "Toggle Auto Fill mode.
With argument, turn Auto Fill mode on iff argument is positive."
  (cond ((and (or (not argument) (positive? argument))
	      (not (current-minor-mode? fill-mode)))
	 (enable-current-minor-mode! fill-mode))
	((and (or (not argument) (not (positive? argument)))
	      (current-minor-mode? fill-mode))
	 (disable-current-minor-mode! fill-mode))))

(define-command ("^R Auto Fill Space" (argument 1))
  "Breaks the line if it exceeds the fill column, then inserts a space."
  (insert-chars #\Space argument)
  (auto-fill-break))

(define-command ("^R Auto Fill Newline" argument)
  "Breaks the line if it exceeds the fill column, then inserts a newline."
  (auto-fill-break)
  (^r-newline-command argument))

(define-minor-mode "Fill"
  "")

(define-key "Fill" #\Space "^R Auto Fill Space")
(define-key "Fill" #\Return "^R Auto Fill Newline")

(define (auto-fill-break)
  (let ((point (current-point)))
    (if (auto-fill-break? point)
	(if (re-search-backward "[^ \t][ \t]+"
				(move-to-column
				 point
				 (1+ (ref-variable "Fill Column")))
				(line-start point 0))
	    (with-current-point (re-match-end 0)
	      ^r-indent-new-comment-line-command)))))

(define (auto-fill-break? point)
  (and (> (mark-column point) (ref-variable "Fill Column"))
       (line-end? (horizontal-space-end point))))

(define-command ("^R Center Line")
  "Center this line's text within the line.
The width is Fill Column."
  (center-line (current-point)))

(define-variable "Left Margin"
  "The number of columns to indent each line."
  0)

(define (center-line mark)
  (mark-permanent! mark)
  (delete-horizontal-space (line-start mark 0))
  (delete-horizontal-space (line-end mark 0))
  (let ((d (- (- (ref-variable "Fill Column") (ref-variable "Left Margin"))
	      (mark-column (line-end mark 0)))))
    (if (positive? d)
	(insert-horizontal-space (+ (ref-variable "Left Margin")
				    (quotient d 2))
				 (line-start mark 0)))))