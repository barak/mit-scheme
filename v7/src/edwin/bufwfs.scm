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

;;;; Buffer Windows:  Fill and Scroll

(declare (usual-integrations)
	 )
(using-syntax class-syntax-table

;;;; Fill

(define-procedure buffer-window (fill-top! window inferiors start fill-bottom?)
  ;; INFERIORS is assumed to be not '(), and START is the start index
  ;; of the first inferior in that list.  FILL-BOTTOM?, if true, means
  ;; try to fill the bottom of INFERIORS after filling the top.

  (let ((group (buffer-group buffer)))
    (define (loop y-start start inferiors)
      (cond ((<= y-start 0)
	     (if fill-bottom? (do-bottom! inferiors start))
	     (set-line-inferiors! window inferiors start))
	    ((group-start-index? group start)
	     (set-line-inferiors! window
				  (scroll-lines-up! window inferiors 0 start)
				  start))
	    (else
	     (let ((end (-1+ start)))
	       (let ((start (line-start-index group end)))
		 (let ((inferior (make-line-inferior window start end)))
		   (let ((y-start (- y-start (inferior-y-size inferior))))
		     (set-inferior-start! inferior 0 y-start)
		     (loop y-start start (cons inferior inferiors)))))))))

    (define (do-bottom! inferiors start)
      (if (null? (cdr inferiors))
	  (set-cdr! inferiors
		    (fill-bottom window
				 (inferior-y-end (car inferiors))
				 (line-end-index group start)))
	  (do-bottom! (cdr inferiors)
		      (+ start (line-inferior-length inferiors)))))

    (loop (inferior-y-start (car inferiors)) start inferiors)))

(define-procedure buffer-window (fill-bottom window y-end end-index)
  ;; Generates a list of inferiors which will be appended to a list
  ;; ending in Y-END and END-INDEX.

  (let ((group (buffer-group buffer)))
    (define (loop y-start end)
      (if (or (>= y-start y-size)
	      (group-end-index? group end))
	  '()
	  (let ((start (1+ end)))
	    (let ((end (line-end-index group start)))
	      (let ((inferior (make-line-inferior window start end)))
		(set-inferior-start! inferior 0 y-start)
		(cons inferior (loop (inferior-y-end inferior) end)))))))
    (loop y-end end-index)))

(define-procedure buffer-window (fill-middle! window y-end end-index
					      tail tail-start-index)
  ;; Generates a list of inferiors which will be appended to a list
  ;; ending in Y-END and END-INDEX.  TAIL will be appended to the
  ;; generated list if it is visible, and scrolled up or down as
  ;; needed.  TAIL-START-INDEX says where TAIL begins.  It is assumed
  ;; that (> TAIL-START-INDEX END-INDEX), and that TAIL is non-'().

  (let ((group (buffer-group buffer)))
    (define (loop y-end end)
      (let ((start (1+ end)))
	(cond ((= start tail-start-index)
	       (let ((old-y-end (inferior-y-start (car tail))))
		 (cond ((> y-end old-y-end)
			(scroll-lines-down! window tail y-end))
		       ((< y-end old-y-end)
			(scroll-lines-up! window tail y-end start))
		       (else tail))))
	      ((>= y-end y-size) '())
	      (else
	       (let ((end (line-end-index group start)))
		 (let ((inferior (make-line-inferior window start end)))
		   (set-inferior-start! inferior 0 y-end)
		   (cons inferior
			 (loop (inferior-y-end inferior) end))))))))
    (loop y-end end-index)))

;;;; Scroll

(define (%set-window-start-mark! window mark force?)
  (let ((start-y (%window-mark->y window mark)))
    (and (or force?
	     (let ((point-y (- (%window-point-y window) start-y)))
	       (and (not (negative? point-y))
		    (< point-y (window-y-size window)))))
	 (begin (%window-scroll-y-relative! window start-y)
		#!TRUE))))

(define-procedure buffer-window (%window-scroll-y-absolute! window y-point)
  (%window-scroll-y-relative! window (- (%window-point-y window) y-point)))

(define-procedure buffer-window (%window-scroll-y-relative! window y-delta)
  (define-procedure buffer-window (scrolled-point-offscreen window)
    (let ((y (if (positive? y-delta) 0 (-1+ (window-y-size window)))))
      (%set-buffer-point! buffer (%window-coordinates->mark window 0 y))
      (set! point (buffer-point buffer))
      (set-inferior-start! cursor-inferior 0 y)
      (set-buffer-cursor-y! buffer y)
      (set! point-moved? #!FALSE)
      (window-modeline-event! superior 'WINDOW-SCROLLED)))

  (cond ((negative? y-delta)
	 (let ((y-start (- (inferior-y-start (car line-inferiors)) y-delta)))
	   (if (< y-start y-size)
	       (fill-top! window
			  (scroll-lines-down! window line-inferiors y-start)
			  (mark-index start-line-mark)
			  #!FALSE)
	       (redraw-at! window
			   (or (%window-coordinates->mark window 0 y-delta)
			       (buffer-start buffer))))))
	((positive? y-delta)
	 (let ((inferiors (y->inferiors window y-delta)))
	   (if inferiors
	       (let ((start (inferiors->index window inferiors)))
		 (set-line-inferiors!
		  window
		  (scroll-lines-up! window
				    inferiors
				    (- (inferior-y-start (car inferiors))
				       y-delta)
				    start)
		  start))
	       (redraw-at! window
			   (or (%window-coordinates->mark window 0 y-delta)
			       (buffer-end buffer)))))))
  (everything-changed! window scrolled-point-offscreen))

(define-procedure buffer-window (redraw-at! window mark)
  (%set-buffer-point! buffer mark)
  (set! point (buffer-point buffer))
  (redraw-screen! window 0))

(define-procedure buffer-window (scroll-lines-down! window inferiors y-start)

  ;; Returns new list of new inferiors.

  ;; "Fast scroll" can be invoked if the lines in the buffer are
  ;; the full width of the screen and the screen image is correct.
  ;; If the buffer-window width is the same size as the-alpha-window width
  ;; then it is assumed that the line windows can be simply scrolled.
  ;; If the redisplay flag for the buffer-window is off, then the image
  ;; on the screen should be correct.

  (let ((absolute-start (inferior-absolute-position (car inferiors)
						    (lambda (x y) y)
						    (lambda () #f))))
    (let ((fast-scroll? (and (= x-size (window-x-size the-alpha-window))
			     (false? (car (inferior-redisplay-flags
					   (car inferiors))))
			     (not (false? absolute-start))))
	  (starting-line (inferior-y-start (car inferiors))))
    
      (define (loop inferiors y-start)
	(if (or (null? inferiors)
		(>= y-start y-size))
	    '()
	    (begin ((if fast-scroll? 
			set-inferior-start-no-redisplay!
			set-inferior-start!)
		    (car inferiors) 0 y-start)
		   (cons (car inferiors)
			 (loop (cdr inferiors)
			       (inferior-y-end (car inferiors)))))))

      (let ((value (loop inferiors y-start)))
	;; Now update the display
	(if fast-scroll?
	    (screen-scroll-region-down! the-alpha-screen
					(- y-start starting-line)
					absolute-start
					(+ absolute-start
					   (- y-size starting-line))))
	value))))

(define-procedure buffer-window
		  (scroll-lines-up! window inferiors y-start start-index)

  (let ((absolute-start (inferior-absolute-position (car inferiors)
						    (lambda (x y) y)
						    (lambda () #f))))
    (let ((fast-scroll? (and (= x-size (window-x-size the-alpha-window))
			     (false? (car (inferior-redisplay-flags
					   (car inferiors))))
			     (not (false? absolute-start))))
	  (starting-line (inferior-y-start (car inferiors))))
		  
      (define (loop inferiors y-start start-index)
	((if fast-scroll? 
	     set-inferior-start-no-redisplay!
	     set-inferior-start!)
	 (car inferiors) 0 y-start)
	(cons (car inferiors)
	      (if (null? (cdr inferiors))
		  (fill-bottom window
			       (inferior-y-end (car inferiors))
			       (line-end-index (buffer-group buffer)
					       start-index))
		  (let ((y-start (inferior-y-end (car inferiors))))
		    (if (>= y-start y-size)
			'()
			(loop (cdr inferiors)
			      y-start
			      (+ start-index
				 (line-inferior-length inferiors))))))))
      (let ((value (loop inferiors y-start start-index)))
	(if fast-scroll?
	    (screen-scroll-region-up! the-alpha-screen
				      (- starting-line y-start)
				      (- absolute-start
					 (- starting-line y-start))
				      (+ absolute-start
					 (- y-size starting-line))))
	value))))

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: (access window-package edwin-package)
;;; Scheme Syntax Table: class-syntax-table
;;; End:
