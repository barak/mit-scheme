;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/bufwiu.scm,v 1.19 1991/04/02 19:55:27 cph Exp $
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
    ;; Record changes that intersect the current outlines.
    (if (and (not (%window-force-redraw? window))
	     (fix:<= (%window-current-start-index window) end)
	     (fix:<= start (%window-current-end-index window)))
	(begin
	  (if (not (%window-start-changes-mark window))
	      (begin
		(%set-window-start-changes-mark!
		 window
		 (make-permanent-mark group start false))
		(%set-window-end-changes-mark!
		 window
		 (make-permanent-mark group end true)))
	      (begin
		(if (fix:< start (%window-start-changes-index window))
		    (set-mark-index! (%window-start-changes-mark window)
				     start))
		(if (fix:> end (%window-end-changes-index window))
		    (set-mark-index! (%window-end-changes-mark window) end))))
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
	  (if (and (not (eq? (%window-point-moved? window) 'SINCE-START-SET))
		   (fix:<= start (%window-point-index window))
		   (fix:<= (%window-point-index window) end))
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
		 (make-permanent-mark group
				      (group-display-start-index group)
				      true))
		(%set-window-end-clip-mark!
		 window
		 (make-permanent-mark group
				      (group-display-end-index group)
				      false))))
	  (begin
	    (if (fix:> start (%window-start-clip-index window))
		(set-mark-index! (%window-start-clip-mark window) start))
	    (if (fix:< end (%window-end-clip-index window))
		(set-mark-index! (%window-end-clip-mark window) end)))
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

(define (update-outlines! window)
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
			 (preserve-bottom! window end-changes end)
			 (preserve-nothing! window))
		     (if (fix:< end-changes end)
			 (preserve-top-and-bottom! window
						   start start-changes
						   end-changes end)
			 (preserve-top! window start start-changes)))))
	      (else
	       (preserve-all! window start end))))))

(define-integrable (preserve-nothing! window)
  (regenerate-outlines window
		       (%window-start-line-index window)
		       (%window-start-line-y window)))

(define (preserve-top! window start start-changes)
  (let ((start-outline (%window-start-outline window))
	(start-y (%window-current-start-y window)))
    (let ((last-unchanged
	   (last-unchanged-outline start-outline
				   start
				   start-changes)))
      (deallocate-outlines! window
			    (outline-next last-unchanged)
			    (%window-end-outline window))
      (preserve-contiguous-region! window
				   (make-o3 window start-outline start start-y)
				   (make-o3 window
					    last-unchanged
					    (fix:- start-changes 1)
					    (outline-end-y start-outline
							   start-y))))))

(define (preserve-bottom! window end-changes end)
  (let ((end-outline (%window-end-outline window))
	(end-y (%window-current-end-y window)))
    (let ((first-unchanged
	   (first-unchanged-outline end-outline end end-changes)))
      (if (not (eq? first-unchanged (%window-start-outline window)))
	  (deallocate-outlines! window
				(%window-start-outline window)
				(outline-previous first-unchanged)))
      (preserve-contiguous-region! window
				   (make-o3 window
					    first-unchanged
					    (fix:+ end-changes 1)
					    (outline-start-y end-outline
							     end-y))
				   (make-o3 window end-outline end end-y)))))

(define (preserve-contiguous-region! window start end)
  (let ((wlstart (%window-start-line-index window))
	(wlsy (%window-start-line-y window)))
    (if (maybe-scroll window start end wlstart wlsy)
	(fill-edges window start end)
	(regenerate-outlines window wlstart wlsy))))

(define (preserve-top-and-bottom! window start start-changes end-changes end)
  (let ((wlstart (%window-start-line-index window))
	(wlsy (%window-start-line-y window))
	(top-head (%window-start-outline window))
	(bot-tail (%window-end-outline window))
	(top-start-y (%window-current-start-y window))
	(bot-end-y (%window-current-end-y window)))
    (let ((top-tail (last-unchanged-outline top-head start start-changes))
	  (bot-head (first-unchanged-outline bot-tail end end-changes)))
      (deallocate-outlines! window
			    (outline-next top-tail)
			    (outline-previous bot-head))
      (let ((top-start (make-o3 window top-head start top-start-y))
	    (top-end
	     (make-o3 window
		      top-tail
		      (fix:- start-changes 1)
		      (outline-end-y top-head top-start-y)))
	    (bot-start
	     (make-o3 window
		      bot-head
		      (fix:+ end-changes 1)
		      (outline-start-y bot-tail bot-end-y)))
	    (bot-end (make-o3 window bot-tail end bot-end-y)))
	(if (maybe-scroll window top-start top-end wlstart wlsy)
	    (if (maybe-scroll window bot-start bot-end wlstart wlsy)
		(begin
		  (fill-middle window top-end bot-start)
		  (deallocate-o3! window top-end)
		  (deallocate-o3! window bot-start)
		  (fill-edges window top-start bot-end))
		(fill-edges window top-start top-end))
	    (if (maybe-scroll window bot-start bot-end wlstart wlsy)
		(fill-edges window bot-start bot-end)
		(regenerate-outlines window wlstart wlsy)))))))

(define (preserve-all! window start-index end-index)
  (let ((wlstart (%window-start-line-index window))
	(wlsy (%window-start-line-y window))
	(start-y (%window-current-start-y window))
	(end-y (%window-current-end-y window)))
    (let ((scroll-down
	   (lambda (y)
	     (let ((start
		    (make-o3 window
			     (%window-start-outline window)
			     start-index
			     start-y))
		   (end
		    (make-o3 window
			     (%window-end-outline window)
			     end-index
			     end-y)))
	       (if (scroll-lines-down window start end y)
		   (begin
		     (fill-top window start)
		     (set-outlines! window start end))
		   (regenerate-outlines window wlstart wlsy)))))
	  (scroll-up
	   (lambda (y)
	     (let ((start
		    (make-o3 window
			     (%window-start-outline window)
			     start-index
			     start-y))
		   (end
		    (make-o3 window
			     (%window-end-outline window)
			     end-index
			     end-y)))
	       (if (scroll-lines-up window start end y)
		   (begin
		     (fill-bottom window end)
		     (set-outlines! window start end))
		   (regenerate-outlines window wlstart wlsy))))))
      (cond ((fix:= wlstart start-index)
	     (cond ((fix:= wlsy start-y)
		    (%clear-window-outstanding-changes! window)
		    (if (%window-point-moved? window)
			(begin
			  (%set-window-point-moved?! window false)
			  (update-cursor! window))))
		   ((fix:< wlsy start-y)
		    (scroll-up wlsy))
		   (else
		    (scroll-down wlsy))))
	    ((fix:< wlstart start-index)
	     (let ((y
		    (predict-y-limited window wlstart wlsy start-index start-y
				       (window-y-size window))))
	       (if (not y)
		   (regenerate-outlines window wlstart wlsy)
		   (scroll-down y))))
	    (else
	     (let ((y
		    (predict-y-limited window wlstart wlsy start-index
				       (fix:- 1 (fix:- end-y start-y))
				       1)))
	       (if (not y)
		   (regenerate-outlines window wlstart wlsy)
		   (scroll-up y))))))))

(define (first-unchanged-outline end-outline end end-changes)
  (let loop ((outline end-outline) (end end))
    (let ((end-next (fix:- end (fix:+ (outline-index-length outline) 1))))
      (if (fix:> end-next end-changes)
	  (begin
	    (if (not (outline-previous outline))
		(error "can't find END-CHANGES"))
	    (loop (outline-previous outline) end-next))
	  (begin
	    (if (not (fix:= end-next end-changes))
		(error "overshot END-CHANGES" end-next end-changes))
	    outline)))))

(define (last-unchanged-outline start-outline start start-changes)
  (let loop ((outline start-outline) (start start))
    (let ((start-next (fix:+ start (fix:+ (outline-index-length outline) 1))))
      (if (fix:< start-next start-changes)
	  (begin
	    (if (not (outline-next outline))
		(error "can't find START-CHANGES"))
	    (loop (outline-next outline) start-next))
	  (begin
	    (if (not (fix:= start-next start-changes))
		(error "overshot START-CHANGES" start-next start-changes))
	    outline)))))

(define (regenerate-outlines window wlstart wlsy)
  (let ((start (make-o3 window false wlstart wlsy))
	(end (make-o3 window false false false)))
    (generate-outlines window start end)
    (set-outlines! window start end)))

(define-integrable (fill-edges window start end)
  (fill-top window start)
  (fill-bottom window end)
  (set-outlines! window start end))

(define (maybe-scroll window start end wlstart wlsy)
  (let ((y
	 (predict-y-limited window wlstart wlsy
			    (o3-index start)
			    (fix:- 1 (fix:- (o3-y end) (o3-y start)))
			    (window-y-size window))))
    (cond ((not y)
	   (deallocate-outlines! window (o3-outline start) (o3-outline end))
	   (deallocate-o3! window start)
	   (deallocate-o3! window end)
	   false)
	  ((fix:= (o3-y start) y)
	   true)
	  ((fix:< (o3-y start) y)
	   (scroll-lines-down window start end y))
	  (else
	   (scroll-lines-up window start end y)))))

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
	    (fix:+ (inferior-x-start (%window-cursor-inferior window)) 1))
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
	    (fix:- (inferior-x-start (%window-cursor-inferior window)) 1))
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
       (let ((outline (direct-output-outline window y-start)))
	 (set-outline-index-length! outline
				    (fix:+ (outline-index-length outline) 1)))
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
       (let ((outline (direct-output-outline window y-start)))
	 (set-outline-index-length! outline
				    (fix:+ (outline-index-length outline)
					   length)))
       (%set-inferior-x-start! (%window-cursor-inferior window)
			       (fix:+ x-start length))))))

(define (buffer-window/direct-output-insert-newline! window)
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window
				    'direct-output-insert-newline!))
  (without-interrupts
   (lambda ()
     (%group-insert-char! (%window-group window)
			  (%window-point-index window)
			  #\newline)
     (let ((end-y (%window-current-end-y window)))
       (screen-direct-output-move-cursor (%window-saved-screen window)
					 (%window-saved-x-start window)
					 (fix:+ (%window-saved-y-start window)
						end-y))
       (%set-window-end-outline!
	window
	(make-outline window 0 1 (%window-end-outline window) false))
       (%set-window-current-end-y! window (fix:+ end-y 1))
       (update-blank-inferior! window false)
       (%set-inferior-x-start! (%window-cursor-inferior window) 0)
       (%set-inferior-y-start! (%window-cursor-inferior window) end-y)))))

(define (direct-output-outline window y)
  (let loop
      ((outline (%window-start-outline window))
       (start-y (%window-current-start-y window)))
    (let ((end-y (fix:+ start-y (outline-y-size outline))))
      (if (fix:< y end-y)
	  outline
	  (loop (outline-next outline) end-y)))))