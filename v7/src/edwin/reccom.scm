;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1986 Massachusetts Institute of Technology
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

;;;; Rectangle Commands

(declare (usual-integrations))
(using-syntax edwin-syntax-table

(define rectangle-ring (list 'RECTANGLE))

(define (delete-rectangle mark1 mark2 #!optional fill-flag move?) ;mark2 is always "point" 
  (if (unassigned? fill-flag) (set! fill-flag #!false))	;where applicable		     
  (if (unassigned? move?) (set! move? #!FALSE))
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
	 (spacenum$ (make-string spacenum #\space))
	 (newl (make-string 1 CHAR:NEWLINE)))
    (define (iter line-mark ring-list)
      (let ((perm-mark (if line-mark (mark-left-inserting line-mark) #!False)))
	(if (or (not perm-mark) (mark> perm-mark last))
	    ring-list
	    (let* ((mark-1 (mark-permanent! (move-to-column perm-mark column1)))
		   (mark-2 (mark-permanent! (move-to-column perm-mark column2)))
		   (line$ (extract-string mark-1 mark-2)))
	      (if (not move?) (delete-string mark-1 mark-2))
	      (if fill-flag
		  (let ((colend (mark-column (line-end mark-1 0))))
		    (if (< colend column1)
			(set! mark-1 (make-space-to-column column1 mark-1)))
		    (insert-string spacenum$ mark-1)))
	      (iter (line-start perm-mark 1) (append ring-list (list line$)))))))
    (iter first (list spacenum))))

(define-command ("Kill Rectangle" (argument 1))
  "Delete rectangle with corners at point and mark; save as last killed one."
  (set-cdr! rectangle-ring (delete-rectangle (current-mark) (current-point))))

(define-command ("Delete Rectangle" (argument 1))
  "Delete (don't save) text in rectangle with point and mark as corners.
The same range of columns is deleted in each line
starting with the line where the region begins
and ending with the line where the region ends."
  (delete-rectangle (current-mark) (current-point)))

(define-command ("Open Rectangle" (argument 1))
  "Blank out rectangle with corners at point and mark, shifting text right.
The text previously in the region is not overwritten by the blanks,
but instead winds up to the right of the rectangle."
  (delete-rectangle (current-mark) (current-point) #!TRUE #!TRUE))

(define-command ("Clear Rectangle" (argument 1))
  "Blank out rectangle with corners at point and mark.
The text previously in the region is overwritten by the blanks."
  (delete-rectangle (current-mark) (current-point) #!TRUE))

(define (make-space-to-column column mark) ;new make-space-to-column
  (mark-permanent! mark)
  (change-column column mark)
  (line-end mark 0))

(define (yank-rectangle rectangle point)
  (let ((goal (mark-column point))
	(newline$ (make-string 1 CHAR:NEWLINE)))
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

(define-command ("Yank Rectangle" (argument 1))
  "Yank the last killed rectangle with upper left corner at point."
  (yank-rectangle rectangle-ring (current-point)))

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: (access rectangle-package edwin-package)
;;; Scheme Syntax Table: edwin-syntax-table
;;; End:
