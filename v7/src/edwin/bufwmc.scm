;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/bufwmc.scm,v 1.9 1991/03/16 08:10:55 cph Exp $
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

;;;; Buffer Windows: Mark <-> Coordinate Maps

(declare (usual-integrations))

(define-integrable (buffer-window/mark->x window mark)
  (buffer-window/index->x window (mark-index mark)))

(define-integrable (buffer-window/mark->y window mark)
  (buffer-window/index->y window (mark-index mark)))

(define-integrable (buffer-window/mark->coordinates window mark)
  (buffer-window/index->coordinates window (mark-index mark)))

(define-integrable (buffer-window/point-x window)
  (buffer-window/index->x window (%window-point-index window)))

(define-integrable (buffer-window/point-y window)
  (buffer-window/index->y window (%window-point-index window)))

(define-integrable (buffer-window/point-coordinates window)
  (buffer-window/index->coordinates window (%window-point-index window)))

(define (buffer-window/index->x window index)
  (if (and (line-inferiors-valid? window)
	   (line-inferiors-contain-index? window index))
      (with-values (lambda () (find-inferior-containing-index window index))
	(lambda (inferior start)
	  (fix:+ (inferior-x-start inferior)
		 (string-base:index->x (inferior-window inferior)
				       (fix:- index start)))))
      (let ((start (%window-line-start-index window index)))
	(%window-column->x window
			   (%window-line-columns window start index)
			   (%window-column-length window start index 0)))))

(define (buffer-window/index->y window index)
  (if (and (line-inferiors-valid? window)
	   (line-inferiors-contain-index? window index))
      (with-values (lambda () (find-inferior-containing-index window index))
	(lambda (inferior start)
	  (fix:+ (inferior-y-start inferior)
		 (string-base:index->y (inferior-window inferior)
				       (fix:- index start)))))
      (begin
	(guarantee-start-mark! window)
	(predict-y window
		   (%window-start-line-index window)
		   (%window-start-line-y window)
		   index))))

(define (buffer-window/index->coordinates window index)
  (if (and (line-inferiors-valid? window)
	   (line-inferiors-contain-index? window index))
      (with-values (lambda () (find-inferior-containing-index window index))
	(lambda (inferior start)
	  (let ((xy
		 (string-base:index->coordinates (inferior-window inferior)
						 (fix:- index start))))
	    (cons (fix:+ (car xy) (inferior-x-start inferior))
		  (fix:+ (cdr xy) (inferior-y-start inferior))))))
      (begin
	(guarantee-start-mark! window)
	(let ((start (%window-line-start-index window index)))
	  (let ((xy
		 (%window-column->coordinates
		  window
		  (%window-line-columns window start index)
		  (%window-column-length window start index 0))))
	    (cons (car xy)
		  (fix:+ (cdr xy)
			 (predict-y window
				    (%window-start-line-index window)
				    (%window-start-line-y window)
				    start))))))))

(define (buffer-window/coordinates->mark window x y)
  (let ((index (buffer-window/coordinates->index window x y)))
    (and index
	 (make-mark (%window-group window) index))))

(define (buffer-window/coordinates->index window x y)
  (with-values
      (lambda ()
	(if (line-inferiors-valid? window)
	    (find-inferior-containing-y window y)
	    (values false false)))
    (lambda (inferior start)
      (if inferior
	  (fix:+ start
		 (string-base:coordinates->index
		  (inferior-window inferior)
		  x
		  (fix:- y (inferior-y-start inferior))))
	  (begin
	    (guarantee-start-mark! window)
	    (predict-index window
			   (%window-start-line-index window)
			   (%window-start-line-y window)
			   x
			   y))))))

(define (buffer-window/mark-visible? window mark)
  ;; True iff cursor at this position would be on-screen.
  (let ((index (mark-index mark)))
    (if (line-inferiors-valid? window)
	(and (line-inferiors-contain-index? window index)
	     (fix:<= (%window-start-index window) index)
	     (with-values
		 (lambda () (find-inferior-containing-index window index))
	       (lambda (inferior start)
		 (let ((limit
			(fix:- (window-y-size window)
			       (inferior-y-start inferior))))
		   (or (fix:< (inferior-y-size inferior) limit)
		       (fix:< (string-base:index->y (inferior-window inferior)
						    (fix:- index start))
			      limit))))))
	(begin
	  (guarantee-start-mark! window)
	  (predict-index-visible? window
				  (%window-start-line-index window)
				  (%window-start-line-y window)
				  index)))))

(define-integrable (line-inferiors-valid? window)
  (and (not (%window-start-changes-mark window))
       (not (%window-start-clip-mark window))
       (not (%window-point-moved? window))
       (not (%window-force-redraw? window))
       (%window-start-line-mark window)
       (fix:= (mark-position (%window-start-line-mark window))
	      (mark-position (%window-current-start-mark window)))))

(define-integrable (line-inferiors-contain-index? window index)
  (and (fix:<= (%window-current-start-index window) index)
       (fix:<= index (%window-current-end-index window))))

(define (find-inferior-containing-index window index)
  (let loop
      ((inferiors (%window-line-inferiors window))
       (start (%window-current-start-index window)))
    (let ((start* (fix:+ start (line-inferior-length (car inferiors)))))
      (if (fix:< index start*)
	  (values (car inferiors) start)
	  (loop (cdr inferiors) start*)))))

(define (find-inferior-containing-y window y)
  (let ((inferiors (%window-line-inferiors window)))
    (if (fix:< y (inferior-y-start (car inferiors)))
	(values false false)
	(let loop
	    ((inferiors inferiors)
	     (start (%window-current-start-index window)))
	  (cond ((fix:< y (%inferior-y-end (car inferiors)))
		 (values (car inferiors) start))
		((null? (cdr inferiors))
		 (values false false))
		(else
		 (loop (cdr inferiors)
		       (fix:+ start
			      (line-inferior-length (car inferiors))))))))))

(define (predict-y window start y index)
  ;; Assuming that the character at index START appears at coordinate
  ;; Y, return the coordinate for the character at INDEX.  START is
  ;; assumed to be a line start.
  (cond ((fix:= index start)
	 y)
	((fix:< index start)
	 (let loop ((start start) (y y))
	   (let* ((end (fix:- start 1))
		  (start (%window-line-start-index window end))
		  (columns (%window-column-length window start end 0))
		  (y (fix:- y (%window-column->y-size window columns))))
	     (if (fix:< index start)
		 (loop start y)
		 (fix:+ y (%window-line-y window columns start index))))))
	(else
	 (let loop ((start start) (y y))
	   (let* ((end (%window-line-end-index window start))
		  (columns (%window-column-length window start end 0)))
	     (if (fix:> index end)
		 (loop (fix:+ end 1)
		       (fix:+ y (%window-column->y-size window columns)))
		 (fix:+ y (%window-line-y window columns start index))))))))

(define (predict-y-limited window start y index yl yu)
  ;; Like PREDICT-Y, except returns #F if the result is not in the
  ;; range specified by YL and YU.  Prevents long search to find INDEX
  ;; when it is far away from the window.
  (cond ((fix:= index start)
	 (and (fix:<= yl y)
	      (fix:< y yu)
	      y))
	((fix:< index start)
	 (let loop ((start start) (y y))
	   (and (fix:<= yl y)
		(let* ((end (fix:- start 1))
		       (start (%window-line-start-index window end))
		       (columns (%window-column-length window start end 0))
		       (y (fix:- y (%window-column->y-size window columns))))
		  (if (fix:< index start)
		      (loop start y)
		      (let ((y
			     (fix:+ y
				    (%window-line-y window columns start
						    index))))
			(and (fix:<= yl y)
			     (fix:< y yu)
			     y)))))))
	(else
	 (let loop ((start start) (y y))
	   (and (fix:< y yu)
		(let* ((end (%window-line-end-index window start))
		       (columns (%window-column-length window start end 0)))
		  (if (fix:> index end)
		      (loop (fix:+ end 1)
			    (fix:+ y (%window-column->y-size window columns)))
		      (let ((y
			     (fix:+ y
				    (%window-line-y window columns start
						    index))))
			(and (fix:<= yl y)
			     (fix:< y yu)
			     y)))))))))

(define (predict-index-visible? window start y index)
  (and (fix:>= index start)
       (let ((y-size (window-y-size window)))
	 (let loop ((start start) (y y))
	   (and (fix:< y y-size)
		(let* ((end (%window-line-end-index window start))
		       (columns (%window-column-length window start end 0)))
		  (if (fix:> index end)
		      (loop (fix:+ end 1)
			    (fix:+ y (%window-column->y-size window columns)))
		      (let ((y
			     (fix:+
			      y
			      (%window-line-y window columns start index))))
			(and (fix:<= 0 y) (fix:< y y-size))))))))))

(define (predict-index window start y-start x y)
  ;; Assumes that START is a line start.
  (if (fix:< y y-start)
      (let loop ((start start) (y-start y-start))
	(and (not (%window-group-start-index? window start))
	     (let* ((end (fix:- start 1))
		    (start (%window-line-start-index window end))
		    (columns (%window-column-length window start end 0))
		    (y-start
		     (fix:- y-start (%window-column->y-size window columns))))
	       (if (fix:< y y-start)
		   (loop start y-start)
		   (%window-coordinates->index window start end columns
					       x (fix:- y y-start))))))
      (let loop ((start start) (y-start y-start))
	(let* ((end (%window-line-end-index window start))
	       (columns (%window-column-length window start end 0))
	       (y-end
		(fix:+ y-start (%window-column->y-size window columns))))
	  (if (fix:>= y y-end)
	      (and (not (%window-group-end-index? window end))
		   (loop (fix:+ end 1) y-end))
	      (%window-coordinates->index window start end columns
					  x (fix:- y y-start)))))))

(define (predict-start-line window index y)
  (let ((start (%window-line-start-index window index)))
    (let ((y
	   (fix:- y
		  (%window-line-y window
				  (%window-line-columns window start index)
				  start
				  index))))
      (cond ((fix:= y 0)
	     (values start y))
	    ((fix:< y 0)
	     (let loop ((start start) (y y))
	       (let* ((end (%window-line-end-index window start))
		      (columns (%window-column-length window start end 0))
		      (y-end
		       (fix:+ y (%window-column->y-size window columns))))
		 (if (and (fix:<= y-end 0)
			  (not (%window-group-end-index? window end)))
		     (loop (fix:+ end 1) y-end)
		     (values start y)))))
	    (else
	     (let loop ((start start) (y y))
	       (if (%window-group-start-index? window start)
		   (values start 0)
		   (let* ((end (fix:- start 1))
			  (start (%window-line-start-index window end))
			  (columns (%window-column-length window start end 0))
			  (y-start
			   (fix:- y (%window-column->y-size window columns))))
		     (if (fix:<= y-start 0)
			 (values start y-start)
			 (loop start y-start))))))))))

(define (predict-start-index window start y-start)
  ;; Assumes (AND (%WINDOW-LINE-START-INDEX? WINDOW START) (<= Y-START 0))
  (if (fix:= 0 y-start)
      start
      (let ((end (%window-line-end-index window start))
	    (y (fix:- 0 y-start)))
	(let ((length (%window-column-length window start end 0)))
	  (let ((index
		 (%window-coordinates->index window start end length 0 y)))
	    (if (let ((xy
		       (%window-index->coordinates window start length index)))
		  (and (fix:= (car xy) 0)
		       (fix:= (cdr xy) y)))
		index
		(fix:+ index 1)))))))

(define (compute-start-index inferior start)
  (let ((y-start (inferior-y-start inferior)))
    (if (fix:= 0 y-start)
	start
	(let ((window (inferior-window inferior))
	      (y (fix:- 0 y-start)))
	  (let ((index (string-base:coordinates->index window 0 y)))
	    (if (let ((xy (string-base:index->coordinates window index)))
		  (and (fix:= (car xy) 0)
		       (fix:= (cdr xy) y)))
		(fix:+ start index)
		(fix:+ (fix:+ start index) 1)))))))

(define-integrable (%window-column-length window start end column)
  (group-column-length (%window-group window) start end column))

(define-integrable (%window-column->index window start end column-start column)
  (group-column->index (%window-group window) start end column-start column))

(define-integrable (%window-line-columns window start index)
  (%window-column-length window start (%window-line-end-index window index) 0))

(define-integrable (%window-line-y window columns start index)
  (%window-column->y window
		     columns
		     (%window-column-length window start index 0)))

(define-integrable (%window-column->y-size window column-size)
  (column->y-size column-size
		  (window-x-size window)
		  (%window-truncate-lines? window)))

(define-integrable (%window-column->x window column-size column)
  (column->x column-size
	     (window-x-size window)
	     (%window-truncate-lines? window)
	     column))

(define-integrable (%window-column->y window column-size column)
  (column->y column-size
	     (window-x-size window)
	     (%window-truncate-lines? window)
	     column))

(define-integrable (%window-column->coordinates window column-size column)
  (column->coordinates column-size
		       (window-x-size window)
		       (%window-truncate-lines? window)
		       column))

(define (%window-coordinates->index window start end column-length x y)
  (%window-column->index
   window start end 0
   (let ((column (coordinates->column x y (window-x-size window))))
     (if (fix:< column column-length)
	 column
	 column-length))))

(define-integrable (%window-index->coordinates window start column-length
					       index)
  (%window-column->coordinates window
			       column-length
			       (%window-column-length window start index 0)))