;;; -*-Scheme-*-
;;;
;;;	$Id: sort.scm,v 1.2 1992/11/20 18:23:27 cph Exp $
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

;;;; Sorting

(declare (usual-integrations))

(define (sort-region region reverse?
		     forward-record record-end
		     key-start key-end
		     compare)
  (let* ((start (region-start region))
	 (end (region-end region))
	 (delete-end (mark-right-inserting-copy end))
	 (unsorted-list
	  (identify-records region forward-record record-end))
	 (sorted-list
	  (sort
	   unsorted-list
	   (let ((order (if reverse?
			    not
			    identity-procedure)))
	     (lambda (element1 element2)
	       (order
		(let ((start1 (key-start (car element1)))
		      (start2 (key-start (car element2))))
		  (compare start1
			   (key-end start1)
			   start2
			   (key-end start2)))))))))
    (insert-reordered-region start end sorted-list unsorted-list)
    (kill-string start delete-end)
    (mark-temporary! delete-end)))

(define (identify-records region forward-record record-end)
  (let ((limit (region-end region)))
    (let next-record ((start (region-start region)))
      (if (and start (mark< start limit))
	  (let ((end (record-end start)))
	    (if (and end (mark< end limit))
		(cons (cons start end)
		      (next-record (forward-record end)))
		(list (cons start (region-end region)))))
	  '()))))

(define (insert-reordered-region start end sorted-list unsorted-list)
  (let ((insert-mark (mark-left-inserting-copy end)))
    (let next-element ((previous start)
		       (sorted-list sorted-list)
		       (unsorted-list unsorted-list))
      (if (null? sorted-list)
	  (if (mark< previous end)
	      (insert-string (extract-string previous end) insert-mark))
	  (begin
	    (if (mark< previous (caar unsorted-list))
		(insert-string (extract-string previous (caar unsorted-list))
			       insert-mark))
	    (insert-string
	     (extract-string (caar sorted-list)
			     (cdar sorted-list))
	     insert-mark)
	    (next-element (cdar unsorted-list)
			  (cdr sorted-list)
			  (cdr unsorted-list)))))
    (mark-temporary! delete-end)))

(define (sort-textual-comparison start1 end1 start2 end2)
  (string<? (extract-string start1 end1)
	    (extract-string start2 end2)))

(define (sort-numeric-comparison start1 end1 start2 end2)
  (let ((string1 (extract-string start1 end1))
	(string2 (extract-string start2 end2)))
    (let ((value1 (string->number string1))
	  (value2 (string->number string2)))
      (if (or (not value1) (not value2))
	  (string<? string1 string2)
	  (< value1 value2)))))

(define-command sort-lines
  "Sort lines in region in ascending order by comparing the text of
the lines.  Argument means sort in descending order."
  "P\nr"
  (lambda (reverse? region)
    (sort-region region reverse?
		 (lambda (mark) (forward-line mark 1))
		 (lambda (mark) (line-end mark 0))
		 identity-procedure
		 (lambda (mark) (line-end mark 0))
		 sort-textual-comparison)))

(define-command sort-paragraphs
  "Sort paragraphs in region in ascending order by comparing the text
of the paragraphs, not including blank lines at the beginning and end.
Argument means sort in descending order."
  "P\nr"
  (lambda (reverse? region)
    (sort-region region reverse?
		 (let ((end (region-end region)))
		   (lambda (mark)
		     (skip-chars-forward " \n\r\t\f" mark end false)))
		 paragraph-text-end
		 identity-procedure
		 (lambda (mark) (line-end mark 0))
		 sort-textual-comparison)))

(define-command sort-pages
  "Sort pages in region in ascending order by comparing the text of
the pages.  Argument means sort in descending order."
  "P\nr"
  (lambda (reverse? region)
    (let ((end (region-end region)))
      (sort-region region reverse?
		   (lambda (mark)
		     (skip-chars-forward
		      "\n\r"
		      (forward-one-page mark)
		      end
		      false))
		   (lambda (mark)
		     (re-match-forward "[^\f]*" mark end))
		   identity-procedure
		   (lambda (mark) (line-end mark 0))
		   sort-textual-comparison))))

(define ((sort-fields compare) field region)
  (if (zero? field) (editor-error "Field number must be non-zero."))
  (let ((end (line-end (region-end region) 0)))
    (sort-region
     region
     (negative? field)
     (lambda (mark) (forward-line mark 1))
     (lambda (mark) (line-end mark 0))
     (lambda (mark)
       (let next-whitespace
	   ((count (-1+ (abs field)))
	    (mark (or (skip-chars-forward " \t" mark end false)
		      mark)))
	 (if (zero? count)
	     mark
	     (let ((new-mark (re-match-forward "[^ \t]+[ \t]*" mark end)))
	       (if new-mark
		   (next-whitespace (-1+ count) new-mark)
		   mark)))))
     (lambda (mark)
       (or (re-match-forward "[^ \t]+" mark end)
	   mark))
     compare)))

(define-command sort-fields
  "Sort lines in region in ascending order by comparing the text of
the (ABS N)th whitespace-separated field of each line.  Negative N
argument means sort in descending order."
  "p\nr"
  (sort-fields sort-textual-comparison))

(define-command sort-numeric-fields
  "Sort lines in region in ascending order by comparing the numeric
value of the (ABS N)th whitespace-separated field of each line.
Negative N argument means sort in descending order."
  "p\nr"
  (sort-fields sort-numeric-comparison))

(define-command sort-columns
  "Sort lines in region in ascending order by comparing the text from
a fixed range of columns.  Argument means sort in descending order.
Range of columns is defined to start in whatever column the region
begins and end in whatever column region ends.  Beginning of first
line, even if it extends outside region, is sorted with the rest of
the line.  The last line is treated similarly."
  "P\nr"
  (lambda (reverse? region)
    (let* ((start (region-start region))
	   (end (region-end region))
	   (start-column (mark-column start))
	   (end-column (mark-column end)))
    (sort-region (make-region (line-start start 0)
			      (line-end end 0))
		 reverse?
		 (lambda (mark) (forward-line mark 1))
		 (lambda (mark) (line-end mark 0))
		 (lambda (mark) (move-to-column mark start-column))
		 (lambda (mark) (move-to-column mark end-column))
		 sort-textual-comparison))))