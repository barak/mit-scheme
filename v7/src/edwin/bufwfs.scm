;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/bufwfs.scm,v 1.8 1990/10/09 16:23:21 cph Exp $
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

;;;; Buffer Windows:  Fill and Scroll

(declare (usual-integrations))

;;;; Fill

(define (fill-top! window inferiors start fill-bottom?)
  (with-instance-variables buffer-window window (inferiors start fill-bottom?)
    ;; INFERIORS is assumed to be not '(), and START is the start index
    ;; of the first inferior in that list.  FILL-BOTTOM?, if true, means
    ;; try to fill the bottom of INFERIORS after filling the top.
    (let ((group (buffer-group buffer)))
      (define (do-bottom! inferiors start)
	(if (null? (cdr inferiors))
	    (set-cdr! inferiors
		      (fill-bottom window
				   (inferior-y-end (car inferiors))
				   (line-end-index group start)))
	    (do-bottom! (cdr inferiors)
			(fix:+ start (line-inferior-length inferiors)))))
      (let loop
	  ((y-start (inferior-y-start (car inferiors)))
	   (start start)
	   (inferiors inferiors))
	(cond ((not (fix:positive? y-start))
	       (if fill-bottom? (do-bottom! inferiors start))
	       (set-line-inferiors! window inferiors start))
	      ((group-start-index? group start)
	       (set-line-inferiors! window
				    (scroll-lines-up! window inferiors 0 start)
				    start))
	      (else
	       (let ((end (fix:-1+ start)))
		 (let ((start (line-start-index group end)))
		   (let ((inferior (make-line-inferior window start end)))
		     (let ((y-start
			    (fix:- y-start (inferior-y-size inferior))))
		       (set-inferior-start! inferior 0 y-start)
		       (loop y-start start (cons inferior inferiors))))))))))))

(define (fill-bottom window y-end end-index)
  (with-instance-variables buffer-window window (y-end end-index)
    ;; Generates a list of inferiors which will be appended to a list
    ;; ending in Y-END and END-INDEX.
    (let ((group (buffer-group buffer)))
      (let loop ((y-start y-end) (end end-index))
	(if (or (not (fix:< y-start y-size))
		(group-end-index? group end))
	    '()
	    (let ((start (fix:1+ end)))
	      (let ((end (line-end-index group start)))
		(let ((inferior (make-line-inferior window start end)))
		  (set-inferior-start! inferior 0 y-start)
		  (cons inferior (loop (inferior-y-end inferior) end))))))))))

(define (fill-middle! window y-end end-index tail tail-start-index)
  (with-instance-variables buffer-window window
			   (y-end end-index tail tail-start-index)
    ;; Generates a list of inferiors which will be appended to a list
    ;; ending in Y-END and END-INDEX.  TAIL will be appended to the
    ;; generated list if it is visible, and scrolled up or down as
    ;; needed.  TAIL-START-INDEX says where TAIL begins.  It is assumed
    ;; that (> TAIL-START-INDEX END-INDEX), and that TAIL is non-'().
    (let ((group (buffer-group buffer)))
      (let loop ((y-end y-end) (end end-index))
	(let ((start (fix:1+ end)))
	  (cond ((fix:= start tail-start-index)
		 (let ((old-y-end (inferior-y-start (car tail))))
		   (cond ((fix:> y-end old-y-end)
			  (scroll-lines-down! window tail y-end))
			 ((fix:< y-end old-y-end)
			  (scroll-lines-up! window tail y-end start))
			 (else tail))))
		((not (fix:< y-end y-size)) '())
		(else
		 (let ((end (line-end-index group start)))
		   (let ((inferior (make-line-inferior window start end)))
		     (set-inferior-start! inferior 0 y-end)
		     (cons inferior
			   (loop (inferior-y-end inferior) end)))))))))))

;;;; Scroll

(define (%set-window-start-mark! window mark force?)
  (let ((start-y (%window-mark->y window mark)))
    (and (or force?
	     (let ((point-y (fix:- (%window-point-y window) start-y)))
	       (and (not (fix:negative? point-y))
		    (fix:< point-y (window-y-size window)))))
	 (begin
	   (%window-scroll-y-relative! window start-y)
	   true))))

(define (%window-scroll-y-absolute! window y-point)
  (with-instance-variables buffer-window window (y-point)
    (%window-scroll-y-relative! window
				(fix:- (%window-point-y window) y-point))))

(define (%window-scroll-y-relative! window y-delta)
  (with-instance-variables buffer-window window (y-delta)
    (cond ((fix:negative? y-delta)
	   (let ((y-start
		  (fix:- (inferior-y-start (car line-inferiors)) y-delta)))
	     (if (fix:< y-start y-size)
		 (fill-top! window
			    (scroll-lines-down! window line-inferiors y-start)
			    (mark-index start-line-mark)
			    false)
		 (redraw-at! window
			     (or (%window-coordinates->mark window 0 y-delta)
				 (buffer-start buffer))))))
	  ((fix:positive? y-delta)
	   (let ((inferiors (y->inferiors window y-delta)))
	     (if inferiors
		 (let ((start (inferiors->index window inferiors)))
		   (set-line-inferiors!
		    window
		    (scroll-lines-up! window
				      inferiors
				      (fix:- (inferior-y-start (car inferiors))
					     y-delta)
				      start)
		    start))
		 (redraw-at! window
			     (or (%window-coordinates->mark window 0 y-delta)
				 (buffer-end buffer)))))))
    (everything-changed!
     window
     (lambda (window)
       (let ((y
	      (if (fix:positive? y-delta)
		  0
		  (fix:-1+ (window-y-size window)))))
	 (%set-buffer-point! buffer (%window-coordinates->mark window 0 y))
	 (set! point (buffer-point buffer))
	 (set-inferior-start! cursor-inferior 0 y)
	 (set! point-moved? false)
	 (window-modeline-event! superior 'WINDOW-SCROLLED))))))

(define (redraw-at! window mark)
  (with-instance-variables buffer-window window (mark)
    (%set-buffer-point! buffer mark)
    (set! point (buffer-point buffer))
    (redraw-screen! window 0)))

(define (scroll-lines-down! window inferiors y-start)
  ;; Returns new list of new inferiors.
  (with-instance-variables buffer-window window (inferiors y-start)
    (let ((scrolled?
	   (let ((yl (inferior-y-start (car inferiors))))
	     (let ((amount (fix:- y-start yl)))
	       (and (fix:< yl saved-yu)
		    (fix:< amount (fix:- saved-yu saved-yl))
		    (screen-scroll-lines-down! saved-screen
					       (fix:+ saved-xl saved-x-start)
					       (fix:+ saved-xu saved-x-start)
					       (fix:+ (fix:max yl saved-yl)
						      saved-y-start)
					       (fix:+ saved-yu saved-y-start)
					       amount))))))
      (let loop ((inferiors inferiors) (y-start y-start))
	(%set-inferior-y-start! (car inferiors) y-start)
	(if (not scrolled?)
	    (inferior-needs-redisplay! (car inferiors)))
	(cons (car inferiors)
	      (let ((inferiors (cdr inferiors))
		    (y-start (inferior-y-end (car inferiors))))
		(if (or (null? inferiors)
			(not (fix:< y-start y-size)))
		    '()
		    (loop inferiors y-start))))))))

(define (scroll-lines-up! window inferiors y-start start-index)
  ;; Returns new list of new inferiors.
  (with-instance-variables buffer-window window (inferiors y-start start-index)
    (let ((scrolled?
	   (let ((yl (inferior-y-start (car inferiors))))
	     (let ((amount (fix:- yl y-start)))
	       (and (fix:< yl saved-yu)
		    (fix:< amount (fix:- saved-yu saved-yl))
		    (screen-scroll-lines-up! saved-screen
					     (fix:+ saved-xl saved-x-start)
					     (fix:+ saved-xu saved-x-start)
					     (fix:+ (fix:max y-start saved-yl)
						    saved-y-start)
					     (fix:+ saved-yu saved-y-start)
					     amount))))))
      (let loop
	  ((inferiors inferiors) (y-start y-start) (start-index start-index))
	(%set-inferior-y-start! (car inferiors) y-start)
	(if (not scrolled?)
	    (inferior-needs-redisplay! (car inferiors)))
	(cons (car inferiors)
	      (let ((y-start (inferior-y-end (car inferiors))))
		(cond ((null? (cdr inferiors))
		       (fill-bottom window
				    y-start
				    (line-end-index (buffer-group buffer)
						    start-index)))
		      ((fix:< y-start y-size)
		       (loop (cdr inferiors)
			     y-start
			     (fix:+ start-index
				    (line-inferior-length inferiors))))
		      (else '()))))))))

(define-integrable (fix:max x y)
  (if (fix:> x y) x y))