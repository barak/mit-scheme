;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/reccom.scm,v 1.13 1989/04/28 22:52:17 cph Rel $
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

;;;; Rectangle Commands

(declare (usual-integrations))

(define rectangle-ring (list 'RECTANGLE))

(define (delete-rectangle mark1 mark2 #!optional fill-flag move?) ;mark2 is always "point"
  (let ((fill-flag (and (not (default-object? fill-flag)) fill-flag))
	(move? (and (not (default-object? move?)) move?)))
    (let* ((mark-order (if (mark> mark1 mark2)
			   (cons mark2 mark1)
			   (cons mark1 mark2)))
	   (first (car mark-order))
	   (last (cdr mark-order))
	   (column-order (let ((c1 (mark-column first))
			       (c2 (mark-column last)))
			   (if (< c1 c2) (cons c1 c2) (cons c2 c1))))
	   (column1 (car column-order))
	   (column2 (cdr column-order))
	   (spacenum (- column2 column1))
	   (spacenum$ (make-string spacenum #\space)))
      (define (iter line-mark ring-list)
	(let ((perm-mark (if line-mark (mark-left-inserting line-mark) false)))
	  (if (or (not perm-mark) (mark> perm-mark last))
	      ring-list
	      (let* ((mark-1
		      (mark-permanent! (move-to-column perm-mark column1)))
		     (mark-2
		      (mark-permanent! (move-to-column perm-mark column2)))
		     (line$ (extract-string mark-1 mark-2)))
		(if (not move?) (delete-string mark-1 mark-2))
		(if fill-flag
		    (let ((colend (mark-column (line-end mark-1 0))))
		      (if (< colend column1)
			  (set! mark-1 (make-space-to-column column1 mark-1)))
		      (insert-string spacenum$ mark-1)))
		(iter (line-start perm-mark 1) (append ring-list (list line$)))))))
      (iter first (list spacenum)))))

(define-command kill-rectangle
  "Delete rectangle with corners at point and mark; save as last killed one."
  ()
  (lambda ()
    (set-cdr! rectangle-ring (delete-rectangle (current-mark) (current-point)))))

(define-command delete-rectangle
  "Delete (don't save) text in rectangle with point and mark as corners.
The same range of columns is deleted in each line
starting with the line where the region begins
and ending with the line where the region ends."
  ()
  (lambda ()
    (delete-rectangle (current-mark) (current-point))))

(define-command open-rectangle
  "Blank out rectangle with corners at point and mark, shifting text right.
The text previously in the region is not overwritten by the blanks,
but instead winds up to the right of the rectangle."
  ()
  (lambda ()
    (delete-rectangle (current-mark) (current-point) true true)))

(define-command clear-rectangle
  "Blank out rectangle with corners at point and mark.
The text previously in the region is overwritten by the blanks."
  ()
  (lambda ()
    (delete-rectangle (current-mark) (current-point) true)))

(define (make-space-to-column column mark)
  (let ((mark (mark-permanent! mark)))
    (change-column column mark)
    (line-end mark 0)))

(define (yank-rectangle rectangle point)
  (let ((goal (mark-column point)))
    (if (null? (cdr rectangle))
	(editor-error "No rectangle to yank.")
	(let ((columns (cadr rectangle)))
	  (define (iter line-mark before-line-mark insert$)
	    (if (not (null? insert$))
		(let* ((next$ (car insert$))
		       (sl (string-length next$))
		       (final$ (if (< sl columns) (string-append next$
								 (Make-string (- columns sl) #\space))
				   next$)) 
		       (end-of-line (if line-mark (mark-left-inserting line-mark)
					 (let () (insert-newline before-line-mark)
					      before-line-mark)))
		       (current-col (mark-column end-of-line)))
		  (insert-string final$
				 (if (< current-col goal)
				     (make-space-to-column goal end-of-line)
				     (move-to-column end-of-line goal)))
		  (iter (line-end end-of-line 1)
			end-of-line
			(cdr insert$)))))
	  (iter (line-end point 0) point (cddr rectangle))))))

(define-command yank-rectangle
  "Yank the last killed rectangle with upper left corner at point."
  ()
  (lambda ()
    (yank-rectangle rectangle-ring (current-point))))