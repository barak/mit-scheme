;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/bufwfs.scm,v 1.12 1991/04/01 19:46:00 cph Exp $
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

;;;; Buffer Windows: Fill and Scroll

(declare (usual-integrations))

(define (fill-top window start)
  (let ((group (%window-group window))
	(start-column 0)
	(tab-width (%window-tab-width window))
	(truncate-lines? (%window-truncate-lines? window))
	(x-size (window-x-size window)))
    (let loop
	((outline (o3-outline start))
	 (index (o3-index start))
	 (y (o3-y start)))
      (if (fix:<= y 0)
	  (begin
	    (set-o3-outline! start outline)
	    (set-o3-index! start index)
	    (set-o3-y! start y))
	  (let* ((end-index (fix:- index 1))
		 (start-index (%window-line-start-index window end-index))
		 (end-column
		  (group-columns group start-index end-index
				 start-column tab-width))
		 (y-size (column->y-size end-column x-size truncate-lines?))
		 (y (fix:- y y-size)))
	    (draw-region! window
			  group start-index end-index
			  start-column
			  y y-size)
	    (loop (make-outline window (fix:- end-index start-index) y-size
				false outline)
		  start-index
		  y))))))

(define (fill-middle window top-end bot-start)
  (let ((group (%window-group window))
	(start-column 0)
	(tab-width (%window-tab-width window))
	(truncate-lines? (%window-truncate-lines? window))
	(x-size (window-x-size window))
	(bot-start-index (o3-index bot-start)))
    (let loop
	((outline (o3-outline top-end))
	 (index (o3-index top-end))
	 (y (o3-y top-end)))
      (let ((start-index (fix:+ index 1)))
	(if (fix:< start-index bot-start-index)
	    (let ((index&column
		   (group-line-columns group start-index bot-start-index
				       start-column tab-width)))
	      (let ((end-index (car index&column))
		    (end-column (cdr index&column)))
		(let ((y-size
		       (column->y-size end-column x-size truncate-lines?)))
		  (draw-region! window
				group start-index end-index
				start-column
				y y-size)
		  (loop (make-outline window
				      (fix:- end-index start-index)
				      y-size
				      outline
				      false)
			end-index
			(fix:+ y y-size)))))
	    (begin
	      (if (not (fix:= start-index bot-start-index))
		  (error "Mismatched indexes:" start-index bot-start-index))
	      (if (not (fix:= y (o3-y bot-start)))
		  (error "Mismatched y coordinates:" y (o3-y bot-start)))
	      (set-outline-next! outline (o3-outline bot-start))
	      (set-outline-previous! (o3-outline bot-start) outline)))))))

(define (fill-bottom window end)
  (let ((group (%window-group window))
	(start-column 0)
	(tab-width (%window-tab-width window))
	(truncate-lines? (%window-truncate-lines? window))
	(x-size (window-x-size window))
	(y-size (window-y-size window))
	(group-end (%window-group-end-index window)))
    (let loop
	((outline (o3-outline end))
	 (index (o3-index end))
	 (y (o3-y end)))
      (if (or (fix:>= index group-end) (fix:>= y y-size))
	  (begin
	    (set-o3-outline! end outline)
	    (set-o3-index! end index)
	    (set-o3-y! end y))
	  (let ((start-index (fix:+ index 1)))
	    (let ((index&column
		   (group-line-columns group start-index group-end
				       start-column tab-width)))
	      (let ((end-index (car index&column))
		    (end-column (cdr index&column)))
		(let ((y-size
		       (column->y-size end-column x-size truncate-lines?)))
		  (draw-region! window
				group start-index end-index
				start-column
				y y-size)
		  (loop (make-outline window
				      (fix:- end-index start-index)
				      y-size
				      outline
				      false)
			end-index
			(fix:+ y y-size))))))))))

(define (generate-outlines window start end)
  (let ((group (%window-group window))
	(start-column 0)
	(tab-width (%window-tab-width window))
	(truncate-lines? (%window-truncate-lines? window))
	(x-size (window-x-size window))
	(y-size (window-y-size window))
	(group-end (%window-group-end-index window)))
    (let loop ((outline false) (start-index (o3-index start)) (y (o3-y start)))
      (let ((index&column
	     (group-line-columns group start-index group-end
				 start-column tab-width)))
	(let ((end-index (car index&column))
	      (end-column (cdr index&column)))
	  (let ((line-y (column->y-size end-column x-size truncate-lines?)))
	    (draw-region! window
			  group start-index end-index
			  start-column
			  y line-y)
	    (let ((outline*
		   (make-outline window
				 (fix:- end-index start-index)
				 line-y
				 outline
				 false))
		  (y (fix:+ y line-y)))
	      (if (not outline)
		  (set-o3-outline! start outline*))
	      (if (or (fix:>= end-index group-end) (fix:>= y y-size))
		  (begin
		    (set-o3-outline! end outline*)
		    (set-o3-index! end end-index)
		    (set-o3-y! end y))
		  (loop outline* (fix:+ end-index 1) y)))))))))

(define (draw-region! window
		      group start-index end-index
		      start-column
		      y y-size)
  (clip-window-region-1 (fix:- (%window-saved-yl window) y)
			(fix:- (%window-saved-yu window) y)
			y-size
    (lambda (yl yu)
      (let ((screen (%window-saved-screen window))
	    (xl
	     (fix:+ (%window-saved-x-start window)
		    (%window-saved-xl window)))
	    (xu
	     (fix:+ (%window-saved-x-start window)
		    (%window-saved-xu window)))
	    (y-start (fix:+ (%window-saved-y-start window) y))
	    (truncate-lines? (%window-truncate-lines? window))
	    (tab-width (%window-tab-width window))
	    (results substring-image-results))
	(let ((xm (fix:- xu 1))
	      (yl (fix:+ y-start yl))
	      (yu (fix:+ y-start yu)))
	  (let ((columns (fix:- xm xl)))
	    (let loop
		((index start-index)
		 (column-offset (fix:- start-column xl))
		 (partial 0)
		 (y 0))
	      (if (fix:< y yu)
		  (let ((line
			 ;; If line is clipped off top of window, draw
			 ;; it anyway so that index and column
			 ;; calculations get done.  Might as well use
			 ;; first visible line for image output so as
			 ;; to avoid consing a dummy image buffer.
			 (screen-get-output-line screen
						 (if (fix:< y yl) yl y)
						 xl xu false)))
		    (let ((fill-line
			   (lambda (index xl)
			     (group-image! group index end-index
					   line xl xm
					   tab-width column-offset results)
			     (cond ((fix:= (vector-ref results 0) end-index)
				    (do ((x (vector-ref results 1)
					    (fix:+ x 1)))
					((fix:= x xu))
				      (string-set! line x #\space)))
				   (truncate-lines?
				    (string-set! line xm #\$))
				   (else
				    (string-set! line xm #\\)
				    (loop (vector-ref results 0)
					  (fix:+ column-offset columns)
					  (vector-ref results 2)
					  (fix:+ y 1)))))))
		      (if (fix:= partial 0)
			  (fill-line index xl)
			  (begin
			    (partial-image! (group-right-char group index)
					    partial
					    line xl xm
					    tab-width)
			    (if (fix:> partial columns)
				(begin
				  (string-set! line xm #\\)
				  (loop index
					(fix:+ column-offset columns)
					(fix:- partial columns)
					(fix:+ y 1)))
				(fill-line (fix:+ index 1)
					   (fix:+ xl partial)))))))))))))))

(define (scroll-lines-up window start end new-start-y)
  (if (fix:>= new-start-y 0)
      (%scroll-lines-up window start end new-start-y)
      (let ((start-outline (o3-outline start))
	    (amount (fix:- (o3-y start) new-start-y)))
	(if (fix:<= (fix:- (o3-y end) amount) 0)
	    (begin
	      (deallocate-outlines! window start-outline (o3-outline end))
	      (deallocate-o3! window start)
	      (deallocate-o3! window end)
	      false)
	    (let loop
		((outline start-outline)
		 (index (o3-index start))
		 (new-start-y new-start-y))
	      (let ((new-end-y (fix:+ new-start-y (outline-y-size outline))))
		(cond ((fix:< new-end-y 0)
		       (loop (outline-next outline)
			     (fix:+ index
				    (fix:+ (outline-index-length outline) 1))
			     new-end-y))
		      ((fix:> new-end-y 0)
		       (set-o3-outline! start outline)
		       (set-o3-index! start index)
		       (set-o3-y! start (fix:+ new-start-y amount))
		       (if (not (eq? start-outline outline))
			   (deallocate-outlines! window
						 start-outline
						 (outline-previous outline)))
		       (%scroll-lines-up window start end new-start-y))
		      (else
		       (set-o3-outline! start (outline-next outline))
		       (set-o3-index!
			start
			(fix:+ (fix:+ index (outline-index-length outline))
			       1))
		       (set-o3-y! start amount)
		       (deallocate-outlines! window start-outline outline)
		       (%scroll-lines-up window start end new-end-y)))))))))

(define (%scroll-lines-up window start end new-start-y)
  (let ((yl (o3-y start))
	(yu (o3-y end)))
    (let ((amount (fix:- yl new-start-y)))
      (if (and (fix:< yl (%window-saved-yu window))
	       (fix:< (%window-saved-yl window) yu)
	       (let ((yl (fix:max (%window-saved-yl window) new-start-y))
		     (yu (fix:min (%window-saved-yu window) yu)))
		 (and (fix:< amount (fix:- yu yl))
		      (screen-scroll-lines-up
		       (%window-saved-screen window)
		       (fix:+ (%window-saved-xl window)
			      (%window-saved-x-start window))
		       (fix:+ (%window-saved-xu window)
			      (%window-saved-x-start window))
		       (fix:+ yl (%window-saved-y-start window))
		       (fix:+ yu (%window-saved-y-start window))
		       amount))))
	  (begin
	    (set-o3-y! start new-start-y)
	    (set-o3-y! end (fix:- yu amount))
	    true)
	  (begin
	    (deallocate-outlines! window (o3-outline start) (o3-outline end))
	    (deallocate-o3! window start)
	    (deallocate-o3! window end)
	    false)))))

(define (scroll-lines-down window start end new-start-y)
  (let ((y-size (window-y-size window))
	(start-outline (o3-outline start))
	(end-outline (o3-outline end)))
    (if (fix:>= new-start-y y-size)
	(begin
	  (deallocate-outlines! window start-outline end-outline)
	  (deallocate-o3! window start)
	  (deallocate-o3! window end)
	  false)
	(begin
	  (let loop
	      ((outline start-outline)
	       (start-index (o3-index start))
	       (start-y new-start-y))
	    (let ((end-y (fix:+ start-y (outline-y-size outline))))
	      (cond ((fix:>= end-y y-size)
		     (if (not (eq? outline end-outline))
			 (deallocate-outlines! window
					       (outline-next outline)
					       end-outline))
		     (set-o3-outline! end outline)
		     (set-o3-index! end
				    (fix:+ start-index
					   (outline-index-length outline)))
		     (set-o3-y! end
				(fix:- end-y
				       (fix:- new-start-y (o3-y start)))))
		    ((not (eq? outline end-outline))
		     (loop (outline-next outline)
			   (fix:+ (fix:+ start-index
					 (outline-index-length outline))
				  1)
			   end-y)))))
	  (%scroll-lines-down window start end new-start-y)))))

(define (%scroll-lines-down window start end new-start-y)
  (let ((yl (o3-y start))
	(yu (o3-y end)))
    (let ((amount (fix:- new-start-y yl)))
      (if (and (fix:< yl (%window-saved-yu window))
	       (fix:< (%window-saved-yl window) yu)
	       (let ((yl (fix:max (%window-saved-yl window) yl))
		     (yu
		      (fix:min (%window-saved-yu window) (fix:+ yu amount))))
		 (and (fix:< amount (fix:- yu yl))
		      (screen-scroll-lines-down
		       (%window-saved-screen window)
		       (fix:+ (%window-saved-xl window)
			      (%window-saved-x-start window))
		       (fix:+ (%window-saved-xu window)
			      (%window-saved-x-start window))
		       (fix:+ yl (%window-saved-y-start window))
		       (fix:+ yu (%window-saved-y-start window))
		       amount))))
	  (begin
	    (set-o3-y! start new-start-y)
	    (set-o3-y! end (fix:+ yu amount))
	    true)
	  (begin
	    (deallocate-outlines! window (o3-outline start) (o3-outline end))
	    (deallocate-o3! window start)
	    (deallocate-o3! window end)
	    false)))))