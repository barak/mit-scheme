;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/bufwfs.scm,v 1.10 1991/03/22 00:30:55 cph Exp $
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

(define (fill-top window inferiors start)
  ;; Assumes non-null INFERIORS.
  (let loop
      ((inferiors inferiors)
       (start start)
       (y-start (inferior-y-start (car inferiors))))
    (if (fix:<= y-start 0)
	inferiors
	(let* ((end (fix:- start 1))
	       (start (%window-line-start-index window end))
	       (inferior
		(let ((string (%window-extract-string window start end)))
		  (make-line-inferior
		   window
		   string
		   (string-image string 0 (%window-tab-width window)))))
	       (y-start (fix:- y-start (inferior-y-size inferior))))
	  (%set-inferior-y-start! inferior y-start)
	  (loop (cons inferior inferiors) start y-start)))))

(define (fill-middle! window
		      top-inferiors top-start
		      bottom-inferiors bottom-start)
  ;; Assumes non-null TOP-INFERIORS and BOTTOM-INFERIORS.
  (let ((group (%window-group window))
	(end (%window-group-end-index window))
	(tab-width (%window-tab-width window)))
    (let loop ((inferiors top-inferiors) (start top-start))
      (let ((start (fix:+ start (line-inferior-length (car inferiors)))))
	(if (not (null? (cdr inferiors)))
	    (loop (cdr inferiors) start)
	    (set-cdr!
	     inferiors
	     (let loop
		 ((start start) (y-start (%inferior-y-end (car inferiors))))
	       (if (fix:= start bottom-start)
		   bottom-inferiors
		   (let ((image&index
			  (group-line-image group start end 0 tab-width)))
		     (let ((inferior
			    (make-line-inferior
			     window
			     (group-extract-string group
						   start
						   (cdr image&index))
			     (car image&index))))
		       (%set-inferior-y-start! inferior y-start)
		       (cons
			inferior
			(loop (fix:+ (cdr image&index) 1)
			      (fix:+ y-start
				     (inferior-y-size inferior)))))))))))))
  top-inferiors)

(define (fill-bottom! window inferiors start)
  ;; Assumes non-null INFERIORS.
  (let loop ((inferiors inferiors) (start start))
    (let ((end
	   (fix:+ start
		  (string-base:string-length
		   (inferior-window (car inferiors))))))
      (if (not (null? (cdr inferiors)))
	  (loop (cdr inferiors) (fix:+ end 1))
	  (let ((y-start (%inferior-y-end (car inferiors))))
	    (if (or (%window-group-end-index? window end)
		    (fix:>= y-start (window-y-size window)))
		(set-current-end-index! window end)
		(set-cdr! inferiors
			  (generate-line-inferiors window
						   (fix:+ end 1)
						   y-start)))))))
  inferiors)

(define (generate-line-inferiors window start y-start)
  ;; Assumes (FIX:< Y-START (WINDOW-Y-SIZE WINDOW))
  (let ((y-size (window-y-size window))
	(group (%window-group window))
	(end (%window-group-end-index window))
	(tab-width (%window-tab-width window)))
    (let loop ((y-start y-start) (start start))
      (let ((image&index (group-line-image group start end 0 tab-width)))
	(let ((inferior
	       (make-line-inferior window
				   (group-extract-string group
							 start
							 (cdr image&index))
				   (car image&index))))
	  (%set-inferior-y-start! inferior y-start)
	  (cons inferior
		(let ((y-start (fix:+ y-start (inferior-y-size inferior))))
		  (if (and (fix:< (cdr image&index) end)
			   (fix:< y-start y-size))
		      (loop y-start (fix:+ (cdr image&index) 1))
		      (begin
			(set-current-end-index! window (cdr image&index))
			'())))))))))

(define (scroll-lines! window inferiors start y-start)
  (cond ((or (null? inferiors)
	     (fix:= y-start (inferior-y-start (car inferiors))))
	 (values inferiors start))
	((fix:< y-start (inferior-y-start (car inferiors)))
	 (scroll-lines-up! window inferiors start y-start))
	(else
	 (values (scroll-lines-down! window inferiors y-start) start))))

(define (scroll-lines-up! window inferiors start y-start)
  (let ((do-scroll
	 (lambda (inferiors start y-start)
	   (%scroll-lines-up! window inferiors y-start)
	   (values inferiors start))))
    (if (fix:>= y-start 0)
	(do-scroll inferiors start y-start)
	(let loop ((inferiors inferiors) (start start) (y-start y-start))
	  (cond ((null? inferiors)
		 (values '() start))
		((fix:= y-start 0)
		 (do-scroll inferiors start y-start))
		(else
		 (let ((y-end
			(fix:+ y-start (inferior-y-size (car inferiors)))))
		   (if (fix:> y-end 0)
		       (do-scroll inferiors start y-start)
		       (loop (cdr inferiors)
			     (fix:+ start
				    (line-inferior-length (car inferiors)))
			     y-end)))))))))

(define (scroll-lines-down! window inferiors y-start)
  (let ((y-size (window-y-size window)))
    (if (or (null? inferiors)
	    (fix:>= y-start y-size))
	'()
	(begin
	  (let loop ((inferiors inferiors) (y-start y-start))
	    (if (not (null? (cdr inferiors)))
		(let ((y-end
		       (fix:+ y-start (inferior-y-size (car inferiors)))))
		  (if (fix:>= y-end y-size)
		      (set-cdr! inferiors '())
		      (loop (cdr inferiors) y-end)))))
	  (%scroll-lines-down! window inferiors y-start)
	  inferiors))))

(define (%scroll-lines-down! window inferiors y-start)
  (adjust-scrolled-inferiors!
   window
   inferiors
   y-start
   (let ((yl (inferior-y-start (car inferiors)))
	 (yu (%inferior-y-end (car (last-pair inferiors)))))
     (let ((amount (fix:- y-start yl)))
       (and (fix:< yl (%window-saved-yu window))
	    (fix:< (%window-saved-yl window) yu)
	    (let ((yl (fix:max (%window-saved-yl window) yl))
		  (yu (fix:min (%window-saved-yu window) (fix:+ yu amount))))
	      (and (fix:< amount (fix:- yu yl))
		   (screen-scroll-lines-down
		    (%window-saved-screen window)
		    (fix:+ (%window-saved-xl window)
			   (%window-saved-x-start window))
		    (fix:+ (%window-saved-xu window)
			   (%window-saved-x-start window))
		    (fix:+ yl (%window-saved-y-start window))
		    (fix:+ yu (%window-saved-y-start window))
		    amount))))))))

(define (%scroll-lines-up! window inferiors y-start)
  (adjust-scrolled-inferiors!
   window
   inferiors
   y-start
   (let ((yl (inferior-y-start (car inferiors)))
	 (yu (%inferior-y-end (car (last-pair inferiors)))))
     (let ((amount (fix:- yl y-start)))
       (and (fix:< yl (%window-saved-yu window))
	    (fix:< (%window-saved-yl window) yu)
	    (let ((yl (fix:max (%window-saved-yl window) y-start))
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
		    amount))))))))

(define (adjust-scrolled-inferiors! window inferiors y-start scrolled?)
  (let ((y-size (window-y-size window)))
    (let loop ((inferiors inferiors) (y-start y-start))
      (if (not (null? inferiors))
	  (begin
	    (%set-inferior-y-start! (car inferiors) y-start)
	    (let ((y-end (fix:+ y-start (inferior-y-size (car inferiors)))))
	      (if (or (not scrolled?)
		      (fix:<= y-end y-size))
		  (inferior-needs-redisplay! (car inferiors)))
	      (loop (cdr inferiors) y-end)))))))