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
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Buffer Windows:  Mark <-> Coordinate Maps

(declare (usual-integrations)
	 (integrate-external "edb:bufwin.bin.0"))
(using-syntax class-syntax-table

(define-procedure buffer-window (%window-mark->x window mark)
  (car (%window-mark->coordinates window mark)))

(define-procedure buffer-window (%window-mark->y window mark)
  (cdr (%window-mark->coordinates window mark)))

(define-procedure buffer-window (%window-point-x window)
  (car (%window-mark->coordinates window point)))

(define-procedure buffer-window (%window-point-y window)
  (cdr (%window-mark->coordinates window point)))

(define-procedure buffer-window (%window-point-coordinates window)
  (%window-mark->coordinates window point))

(declare (integrate %window-mark->coordinates))

(define-procedure buffer-window (%window-mark->coordinates window mark)
  (declare (integrate window mark))
  (%window-index->coordinates window (mark-index mark)))

(define-procedure buffer-window (%window-coordinates->mark window x y)
  (let ((index (%window-coordinates->index window x y)))
    (and index (make-mark (buffer-group buffer) index))))

(define-procedure buffer-window (%window-index->coordinates window index)
  (let ((group (buffer-group buffer)))
    (define (search-upwards end y-end)
      (let ((start (line-start-index group end)))
	(let ((columns (group-column-length group start end 0)))
	  (let ((y-start (- y-end (column->y-size columns x-size))))
	    (if (<= start index)
		(done start columns y-start)
		(search-upwards (-1+ start) y-start))))))

    (define (search-downwards start y-start)
      (let ((end (line-end-index group start)))
	(let ((columns (group-column-length group start end 0)))
	  (if (<= index end)
	      (done start columns y-start)
	      (search-downwards (1+ end)
				(+ y-start
				   (column->y-size columns x-size)))))))

    (declare (integrate done))

    (define (done start columns y-start)
      (declare (integrate start columns y-start))
      (let ((xy
	     (column->coordinates columns
				  x-size
				  (group-column-length group start index 0))))
	(cons (car xy) (+ (cdr xy) y-start))))

    (let ((start (mark-index start-line-mark))
	  (end (mark-index end-line-mark)))
      (cond ((< index start)
	     (search-upwards (-1+ start)
			     (inferior-y-start (first-line-inferior window))))
	    ((> index end)
	     (search-downwards (1+ end)
			       (inferior-y-end last-line-inferior)))
	    (else
	     (let ((start (line-start-index group index)))
	       (done start
		     (group-column-length group start
					  (line-end-index group index) 0)
		     (inferior-y-start
		      (car (index->inferiors window index))))))))))

(define-procedure buffer-window (%window-coordinates->index window x y)
  (let ((group (buffer-group buffer)))
    (define (search-upwards start y-end)
      (and (not (group-start-index? group start))
	   (let ((end (-1+ start)))
	     (let ((start (line-start-index group end)))
	       (let ((y-start (- y-end (y-delta start end))))
		 (if (<= y-start y)
		     (done start end y-start)
		     (search-upwards start y-start)))))))

    (define (search-downwards end y-start)
      (and (not (group-end-index? group end))
	   (let ((start (1+ end)))
	     (let ((end (line-end-index group start)))
	       (let ((y-end (+ y-start (y-delta start end))))
		 (if (< y y-end)
		     (done start end y-start)
		     (search-downwards end y-end)))))))

    (declare (integrate y-delta done))

    (define (y-delta start end)
      (declare (integrate start end))
      (column->y-size (group-column-length group start end 0) x-size))

    (define (done start end y-start)
      (declare (integrate start end y-start))
      (group-column->index group start end 0
			   (coordinates->column x (- y y-start) x-size)))

    (let ((start (inferior-y-start (first-line-inferior window)))
	  (end (inferior-y-end last-line-inferior)))
      (cond ((< y start)
	     (search-upwards (mark-index start-line-mark) start))
	    ((>= y end)
	     (search-downwards (mark-index end-line-mark) end))
	    (else
	     (y->inferiors&index window y
	       (lambda (inferiors index)
		 (done index
		       (line-end-index group index)
		       (inferior-y-start (car inferiors))))))))))

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: (access window-package edwin-package)
;;; Scheme Syntax Table: class-syntax-table
;;; End:
