;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/bufwiu.scm,v 1.16 1991/03/22 00:31:07 cph Exp $
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

;;;; Buffer Windows: Image Update

(declare (usual-integrations))

;;;; Insert/Delete

(define (make-changes-daemon window)
  ;; It is assumed that the insert daemon is called after the
  ;; insertion has been performed, and the delete daemon before the
  ;; deletion has been performed.  It is also assumed that interrupts
  ;; are disabled.
  (lambda (group start end)
    (if (%window-debug-trace window)
	((%window-debug-trace window) 'window window 'change-daemon
				      group start end))
    ;; Record changes that intersect the current line inferiors.
    (if (and (not (%window-force-redraw? window))
	     (fix:<= (%window-current-start-index window) end)
	     (fix:<= start (%window-current-end-index window)))
	;; We can compare marks by their positions here because
	;; the marks being compared have the same
	;; LEFT-INSERTING? flag.
	(let ((start
	       (group-index->position-integrable group start false))
	      (end (group-index->position-integrable group end true)))
	  (if (not (%window-start-changes-mark window))
	      (begin
		(%set-window-start-changes-mark!
		 window
		 (%%make-permanent-mark group start false))
		(%set-window-end-changes-mark!
		 window
		 (%%make-permanent-mark group end true)))
	      (begin
		(if (fix:< start
			   (mark-position
			    (%window-start-changes-mark window)))
		    (set-mark-position!
		     (%window-start-changes-mark window)
		     start))
		(if (fix:> end
			   (mark-position
			    (%window-end-changes-mark window)))
		    (set-mark-position! (%window-end-changes-mark window)
					end))))
	  (window-needs-redisplay! window)))
    ;; If this change affects where the window starts, choose a
    ;; new place to start it.
    (if (%window-start-line-mark window)
	(begin
	  (if (let ((wlstart (%window-start-line-index window))
		    (wstart (%window-start-index window)))
		(and (if (fix:= wlstart wstart)
			 (fix:< start wstart)
			 (fix:<= start wstart))
		     (fix:<= wlstart end)))
	      (begin
		(clear-start-mark! window)
		(window-needs-redisplay! window)))
	  (if (and (not (eq? (%window-point-moved? window)
			     'SINCE-START-SET))
		   (let ((point (%window-point-index window)))
		     (and (fix:<= start point)
			  (fix:<= point end))))
	      (%set-window-point-moved?! window 'SINCE-START-SET))))))

;;;; Clip

(define (make-clip-daemon window)
  ;; It is assumed that the clip daemon is called before the clipping
  ;; has been performed.  It is also assumed that interrupts are
  ;; disabled.
  (lambda (group start end)
    (if (not (%window-force-redraw? window))
	(begin
	  (if (%window-debug-trace window)
	      ((%window-debug-trace window) 'window window 'clip-daemon
					    group start end))
	  (if (not (%window-start-clip-mark window))
	      (begin
		(%set-window-start-clip-mark!
		 window
		 (%make-permanent-mark group
				       (group-display-start-index group)
				       true))
		(%set-window-end-clip-mark!
		 window
		 (%make-permanent-mark group
				       (group-display-end-index group)
				       false))))
	  (let ((start (group-index->position-integrable group start true))
		(end (group-index->position-integrable group end false)))
	    ;; We can compare marks by their positions here because the
	    ;; marks being compared have the same LEFT-INSERTING? flag.
	    (if (fix:> start (mark-position (%window-start-clip-mark window)))
		(set-mark-position! (%window-start-clip-mark window) start))
	    (if (fix:< end (mark-position (%window-end-clip-mark window)))
		(set-mark-position! (%window-end-clip-mark window) end)))
	  (if (and (not (window-needs-redisplay? window))
		   (or (fix:>= (%window-start-clip-index window)
			       (%window-current-start-index window))
		       (fix:<= (%window-end-clip-index window)
			       (%window-current-end-index window))))
	      (window-needs-redisplay! window))))
    (if (and (%window-start-line-mark window)
	     (or (fix:>= start (%window-start-line-index window))
		 (fix:< end (%window-start-index window))))
	(begin
	  (clear-start-mark! window)
	  (window-needs-redisplay! window)))
    (let ((point (%window-point-index window)))
      (cond ((fix:< point start)
	     (%set-window-point-index! window start)
	     (%set-window-point-moved?! window 'SINCE-START-SET))
	    ((fix:< end point)
	     (%set-window-point-index! window end)
	     (%set-window-point-moved?! window 'SINCE-START-SET))))))

;;;; Update

(define (recompute-image! window)
  (%guarantee-start-mark! window)
  (if (%window-force-redraw? window)
      (begin
	(%set-window-force-redraw?! window false)
	(%recache-window-buffer-local-variables! window)
	(preserve-nothing! window))
      (let ((start (%window-current-start-index window))
	    (end (%window-current-end-index window)))
	(cond ((and (%window-start-clip-mark window)
		    (or (fix:< start (%window-group-start-index window))
			(fix:< (%window-group-start-index window)
			       (%window-start-clip-index window))
			(fix:< (%window-group-end-index window) end)
			(fix:< (%window-end-clip-index window)
			       (%window-group-end-index window))))
	       (preserve-nothing! window))
	      ((%window-start-changes-mark window)
	       (let ((start-changes
		      (let ((start-changes
			     (%window-start-changes-index window)))
			(%window-line-start-index window start-changes)))
		     (end-changes
		      (let ((end-changes (%window-end-changes-index window)))
			(%window-line-end-index window end-changes))))
		 (if (fix:<= start-changes start)
		     (if (fix:< end-changes end)
			 (preserve-contiguous-region!
			  window
			  (cdr
			   (changed-inferiors-tail
			    (%window-line-inferiors window)
			    end
			    end-changes))
			  (fix:+ end-changes 1))
			 (preserve-nothing! window))
		     (if (fix:< end-changes end)
			 (preserve-top-and-bottom! window
						   start start-changes
						   end-changes end)
			 (let ((inferiors (%window-line-inferiors window)))
			   (set-cdr! (unchanged-inferiors-tail inferiors
							       start
							       start-changes)
				     '())
			   (preserve-contiguous-region! window
							inferiors
							start))))))
	      (else
	       (preserve-all! window start))))))

(define-integrable (preserve-nothing! window)
  (set-line-inferiors!
   window
   (generate-line-inferiors window
			    (%window-start-line-index window)
			    (%window-start-line-y window))))

(define (preserve-contiguous-region! window inferiors start)
  (let ((wlstart (%window-start-line-index window))
	(wlsy (%window-start-line-y window)))
    (set-line-inferiors!
     window
     (with-values
	 (lambda () (maybe-scroll window inferiors start wlstart wlsy))
       (lambda (inferiors start)
	 (if (null? inferiors)
	     (generate-line-inferiors window wlstart wlsy)
	     (fill-edges! window inferiors start)))))))

(define-integrable (fill-edges! window inferiors start)
  (fill-top window (fill-bottom! window inferiors start) start))

(define (preserve-all! window start)
  (let ((wlstart (%window-start-line-index window))
	(wlsy (%window-start-line-y window))
	(inferiors (%window-line-inferiors window)))
    (let ((regenerate
	   (lambda ()
	     (set-line-inferiors!
	      window
	      (generate-line-inferiors window wlstart wlsy))))
	  (scroll-down
	   (lambda (y-start)
	     (set-line-inferiors!
	      window
	      (let ((inferiors (scroll-lines-down! window inferiors y-start)))
		(if (null? inferiors)
		    (generate-line-inferiors window wlstart wlsy)
		    (begin
		      (let ((end
			     (let loop ((inferiors inferiors) (start start))
			       (if (null? (cdr inferiors))
				   (%window-line-end-index window start)
				   (loop (cdr inferiors)
					 (fix:+ start
						(line-inferior-length
						 (car inferiors))))))))
			;; SET-CURRENT-END-INDEX! is integrable
			(set-current-end-index! window end))
		      (fill-top window inferiors start)))))))
	  (scroll-up
	   (lambda (y-start)
	     (set-line-inferiors!
	      window
	      (with-values
		  (lambda () (scroll-lines-up! window inferiors start y-start))
		(lambda (inferiors start)
		  (if (null? inferiors)
		      (generate-line-inferiors window wlstart wlsy)
		      (fill-bottom! window inferiors start))))))))
      (cond ((fix:= wlstart start)
	     (let ((y-start (inferior-y-start (car inferiors))))
	       (cond ((fix:= wlsy y-start)
		      (%clear-window-outstanding-changes! window)
		      (if (%window-point-moved? window)
			  (begin
			    (%set-window-point-moved?! window false)
			    (update-cursor! window))))
		     ((fix:< wlsy y-start)
		      (scroll-up wlsy))
		     (else
		      (scroll-down wlsy)))))
	    ((fix:< wlstart start)
	     (let ((y
		    (predict-y-limited window wlstart wlsy start
				       (inferior-y-start (car inferiors))
				       (window-y-size window))))
	       (if (not y)
		   (regenerate)
		   (scroll-down y))))
	    (else
	     (let ((y
		    (predict-y-limited
		     window wlstart wlsy start
		     (fix:- 1
			    (fix:- (inferior-y-end (car (last-pair inferiors)))
				   (inferior-y-start (car inferiors))))
		     1)))
	       (if (not y)
		   (regenerate)
		   (scroll-up y))))))))

(define (preserve-top-and-bottom! window start start-changes end-changes end)
  (let ((wlstart (%window-start-line-index window))
	(wlsy (%window-start-line-y window))
	(top-inferiors (%window-line-inferiors window)))
    (let* ((top-tail
	    (unchanged-inferiors-tail top-inferiors start start-changes))
	   (middle-tail
	    (changed-inferiors-tail (cdr top-tail) end end-changes))
	   (bottom-inferiors (cdr middle-tail)))
      (set-cdr! top-tail '())
      (set-cdr! middle-tail '())
      (with-values
	  (lambda ()
	    (maybe-scroll window top-inferiors start wlstart wlsy))
	(lambda (top-inferiors top-start)
	  (with-values
	      (lambda ()
		(maybe-scroll window bottom-inferiors (fix:+ end-changes 1)
			      wlstart wlsy))
	    (lambda (bottom-inferiors bottom-start)
	      (set-line-inferiors!
	       window
	       (if (null? top-inferiors)
		   (if (null? bottom-inferiors)
		       (generate-line-inferiors window wlstart wlsy)
		       (fill-edges! window bottom-inferiors bottom-start))
		   (if (null? bottom-inferiors)
		       (fill-edges! window top-inferiors top-start)
		       (fill-top window
				 (fill-middle! window
					       top-inferiors
					       top-start
					       (fill-bottom! window
							     bottom-inferiors
							     bottom-start)
					       bottom-start)
				 top-start)))))))))))

(define (maybe-scroll window inferiors start wlstart wlsy)
  (let ((y
	 (predict-y-limited
	  window
	  wlstart
	  wlsy
	  start
	  (fix:- 1
		 (fix:- (inferior-y-end (car (last-pair inferiors)))
			(inferior-y-start (car inferiors))))
	  (window-y-size window))))
    (if (not y)
	(values '() start)
	(scroll-lines! window inferiors start y))))

(define (changed-inferiors-tail inferiors end end-changes)
  (let find-end
      ((inferiors inferiors)
       (find-end-changes
	(lambda (end)
	  end
	  (error "can't find END-CHANGES"))))
    (if (null? inferiors)
	(find-end-changes end)
	(find-end (cdr inferiors)
		  (lambda (end)
		    (if (fix:= end end-changes)
			inferiors
			(find-end-changes
			 (fix:- end
				(line-inferior-length (car inferiors))))))))))

(define (unchanged-inferiors-tail inferiors start start-changes)
  (let loop ((inferiors inferiors) (start start))
    (let ((start-next (fix:+ start (line-inferior-length (car inferiors)))))
      (cond ((fix:>= start-next start-changes)
	     inferiors)
	    ((null? (cdr inferiors))
	     (error "can't find START-CHANGES"))
	    (else
	     (loop (cdr inferiors) start-next))))))

;;;; Direct Output

;;; The direct output procedures are hairy and should be used only
;;; under restricted conditions.  In particular, the cursor may not be
;;; at the right margin (for insert and forward) or the left margin
;;; (for backward), and the character being inserted must be an
;;; ordinary graphic character.  For insert, the buffer must be
;;; modifiable, and the modeline must already show that it has been
;;; modified.  None of the procedures may be used if the window needs
;;; redisplay.

(define (buffer-window/needs-redisplay? window)
  (if (or (window-needs-redisplay? window)
	  (not (%window-saved-screen window))
	  (screen-needs-update? (%window-saved-screen window)))
      true
      false))

(define (buffer-window/direct-output-forward-char! window)
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window
				    'direct-output-forward-char!))
  (without-interrupts
   (lambda ()
     (%set-window-point-index! window (fix:+ (%window-point-index window) 1))
     (let ((x-start
	    (fix:1+ (inferior-x-start (%window-cursor-inferior window))))
	   (y-start (inferior-y-start (%window-cursor-inferior window))))
       (screen-direct-output-move-cursor
	(%window-saved-screen window)
	(fix:+ (%window-saved-x-start window) x-start)
	(fix:+ (%window-saved-y-start window) y-start))
       (%set-inferior-x-start! (%window-cursor-inferior window) x-start)))))

(define (buffer-window/direct-output-backward-char! window)
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window
				    'direct-output-backward-char!))
  (without-interrupts
   (lambda ()
     (%set-window-point-index! window (fix:- (%window-point-index window) 1))
     (let ((x-start
	    (fix:-1+ (inferior-x-start (%window-cursor-inferior window))))
	   (y-start (inferior-y-start (%window-cursor-inferior window))))
       (screen-direct-output-move-cursor
	(%window-saved-screen window)
	(fix:+ (%window-saved-x-start window) x-start)
	(fix:+ (%window-saved-y-start window) y-start))
       (%set-inferior-x-start! (%window-cursor-inferior window) x-start)))))

(define (buffer-window/home-cursor! window)
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window 'home-cursor!))
  (if (and (%window-saved-screen window)
	   (fix:<= (%window-saved-xl window) 0)
	   (fix:< 0 (%window-saved-xu window))
	   (fix:<= (%window-saved-yl window) 0)
	   (fix:< 0 (%window-saved-yu window)))
      (without-interrupts
       (lambda ()
	 (screen-direct-output-move-cursor (%window-saved-screen window)
					   (%window-saved-x-start window)
					   (%window-saved-y-start window))))))

(define (buffer-window/direct-output-insert-char! window char)
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window
				    'direct-output-insert-char! char))
  (without-interrupts
   (lambda ()
     (%group-insert-char! (%window-group window)
			  (%window-point-index window)
			  char)
     (let ((x-start (inferior-x-start (%window-cursor-inferior window)))
	   (y-start (inferior-y-start (%window-cursor-inferior window))))
       (screen-direct-output-char
	(%window-saved-screen window)
	(fix:+ (%window-saved-x-start window) x-start)
	(fix:+ (%window-saved-y-start window) y-start)
	char
	false)
       (string-base:direct-output-insert-char!
	(direct-output-line-window window y-start)
	x-start
	char)
       (%set-inferior-x-start! (%window-cursor-inferior window)
			       (fix:+ x-start 1))))))

(define (buffer-window/direct-output-insert-substring! window string start end)
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window
				    'direct-output-insert-substring!
				    (string-copy string) start end))
  (without-interrupts
   (lambda ()
     (%group-insert-substring! (%window-group window)
			       (%window-point-index window)
			       string start end)
     (let ((x-start (inferior-x-start (%window-cursor-inferior window)))
	   (y-start (inferior-y-start (%window-cursor-inferior window)))
	   (length (fix:- end start)))
       (screen-direct-output-substring
	(%window-saved-screen window)
	(fix:+ (%window-saved-x-start window) x-start)
	(fix:+ (%window-saved-y-start window) y-start)
	string start end
	false)
       (string-base:direct-output-insert-substring!
	(direct-output-line-window window y-start)
	x-start
	string start end)
       (%set-inferior-x-start! (%window-cursor-inferior window)
			       (fix:+ x-start length))))))

(define (direct-output-line-window window y)
  (let loop ((inferiors (%window-line-inferiors window)))
    (if (fix:< y (%inferior-y-end (car inferiors)))
	(inferior-window (car inferiors))
	(loop (cdr inferiors)))))

(define (buffer-window/direct-output-insert-newline! window)
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window
				    'direct-output-insert-newline!))
  (without-interrupts
   (lambda ()
     (%group-insert-char! (%window-group window)
			  (%window-point-index window)
			  #\newline)
     (let ((y-start
	    (fix:+ (inferior-y-start (%window-cursor-inferior window)) 1)))
       (let ((inferior (make-inferior window string-base)))
	 (%set-inferior-x-start! inferior 0)
	 (%set-inferior-y-start! inferior y-start)
	 (%set-window-x-size! (inferior-window inferior)
			      (window-x-size window))
	 (set-cdr! (last-pair (%window-line-inferiors window)) (list inferior))
	 (string-base:direct-output-insert-newline!
	  (inferior-window inferior)))
       (let ((inferior (%window-blank-inferior window))
	     (y-end (fix:+ y-start 1)))
	 (if (fix:< y-end (window-y-size window))
	     (begin
	       (%set-inferior-y-size! inferior
				      (fix:- (window-y-size window) y-end))
	       (%set-inferior-y-start! inferior y-end))
	     (begin
	       (%set-inferior-x-start! inferior false)
	       (%set-inferior-y-start! inferior false))))
       (%set-inferior-x-start! (%window-cursor-inferior window) 0)
       (%set-inferior-y-start! (%window-cursor-inferior window) y-start)
       (screen-direct-output-move-cursor (%window-saved-screen window)
					 (%window-saved-x-start window)
					 (fix:+ (%window-saved-y-start window)
						y-start))))))