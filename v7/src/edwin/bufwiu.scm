;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/bufwiu.scm,v 1.12 1989/08/14 09:22:07 cph Exp $
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

;;;; Buffer Windows:  Image Update

(declare (usual-integrations))

;;;; Insert/Delete/Clip

;;; It is assumed that the insert daemon is called after the insertion
;;; has been performed, and the delete daemon before the deletion has
;;; been performed.  It is also assumed that interrupts are disabled.

(define (make-changes-daemon window)
  (lambda (group start end)
    (with-instance-variables buffer-window window (group start end)
      (let ((start (group-index->position group start false))
	    (end (group-index->position group end true)))
	(cond ((not start-changes-mark)
	       (set! start-changes-mark
		     (%make-permanent-mark group start false))
	       (set! end-changes-mark (%make-permanent-mark group end true)))
	      ((fix:< start (mark-position start-changes-mark))
	       (set-mark-position! start-changes-mark start))
	      ((fix:> end (mark-position end-changes-mark))
	       (set-mark-position! end-changes-mark end)))
	(if (and (not (car redisplay-flags))
		 (not (fix:< end (mark-position start-line-mark)))
		 (not (fix:> start (mark-position end-mark))))
	    (setup-redisplay-flags! redisplay-flags))))))

;;; It is assumed that the clip daemon is called before the clipping
;;; has been performed, so that we can get the old clipping limits.

(define (make-clip-daemon window)
  (lambda (group start end)
    (with-instance-variables buffer-window window (group start end)
      (if (not start-clip-mark)
	  (begin
	    (set! start-clip-mark (group-display-start group))
	    (set! end-clip-mark (group-display-end group))))
      (if (not (car redisplay-flags))
	  (let ((start (group-index->position group start false))
		(end (group-index->position group end true))
		(window-start (mark-position start-line-mark))
		(window-end (mark-position end-mark)))
	    (if (or (fix:> start window-start)
		    (fix:< end window-end)
		    (and (fix:< start window-start)
			 (fix:= window-start (mark-position start-clip-mark)))
		    (and (fix:> end window-end)
			 (fix:= window-end (mark-position end-clip-mark))))
		(setup-redisplay-flags! redisplay-flags)))))))

(define (update-buffer-window! window screen x-start y-start
			       xl xu yl yu display-style)
  ;; The primary update entry.
  (recompute-image! window)
  (update-inferiors! window screen x-start y-start xl xu yl yu display-style))

(define (maybe-recompute-image! window)
  (with-instance-variables buffer-window window ()
    ;; Used to guarantee everything updated before certain operations.
    (if (car redisplay-flags)
	(recompute-image! window))))

(define (recompute-image! window)
  (with-instance-variables buffer-window window ()
    (without-interrupts (lambda () (%recompute-image! window)))))

(define (%recompute-image! window)
  (with-instance-variables buffer-window window ()
    (cond ((not force-redraw?)
	   (let ((group (mark-group start-mark))
		 (start-line (mark-index start-line-mark))
		 (start (mark-index start-mark))
		 (end (mark-index end-mark))
		 (point-index (mark-index point)))
	     (if start-clip-mark
		 (let ((new-clip-start (group-start-index group))
		       (new-clip-end (group-end-index group)))
		   (cond ((fix:< point-index new-clip-start)
			  (%set-buffer-point! buffer
					      (group-display-start group))
			  (set! point (buffer-point buffer)))
			 ((fix:> point-index new-clip-end)
			  (%set-buffer-point! buffer (group-display-end group))
			  (set! point (buffer-point buffer))))
		   (cond ((fix:> new-clip-start start-line)
			  (%window-redraw! window false))
			 ((or (fix:< new-clip-end end)
			      (and (fix:< new-clip-start start-line)
				   (fix:= start-line
					  (mark-index start-clip-mark)))
			      (and (fix:> new-clip-end end)
				   (fix:= end (mark-index end-clip-mark))))
			  (%window-redraw! window
					   (and (not start-changes-mark)
						(not (fix:< point-index start))
						(not (fix:> point-index end))
						(%window-point-y window))))
			 (else
			  (destroy-mark! start-clip-mark)
			  (set! start-clip-mark false)
			  (destroy-mark! end-clip-mark)
			  (set! end-clip-mark false)))))
	     (if start-changes-mark
		 (let ((start-changes (mark-index start-changes-mark))
		       (end-changes (mark-index end-changes-mark)))
		   (if (and (not (fix:< end-changes start-line))
			    (not (fix:> start-changes end)))
		       (if (not (fix:> start-changes start))
			   (if (fix:< end-changes end)
			       (recompute-image!:top-changed window)
			       (%window-redraw! window false))
			   (if (not (fix:< end-changes end))
			       (recompute-image!:bottom-changed window)
			       (recompute-image!:middle-changed window)))
		       (begin
			 (destroy-mark! start-changes-mark)
			 (set! start-changes-mark false)
			 (destroy-mark! end-changes-mark)
			 (set! end-changes-mark false))))))
	   (if point-moved?
	       (update-cursor! window maybe-recenter!)))
	  ((eq? 'START force-redraw?)
	   (%window-redraw-preserving-start! window))
	  ((eq? 'POINT force-redraw?)
	   (%window-redraw! window (%window-point-y window)))
	  ((eq? 'BUFFER-CURSOR-Y force-redraw?)
	   (%window-redraw! window (%window-buffer-cursor-y window)))
	  ((eq? 'CENTER force-redraw?)
	   (%window-redraw! window (%window-y-center window)))
	  ((and (object-type? (ucode-type fixnum) force-redraw?)
		(not (fix:negative? force-redraw?))
		(fix:< force-redraw? y-size))
	   (%window-redraw! window force-redraw?))
	  (else
	   (%window-redraw! window (%window-y-center window))))))

(define (recompute-image!:top-changed window)
  (with-instance-variables buffer-window window ()
    (let ((inferiors (end-changes-inferiors window))
	  (group (mark-group end-changes-mark))
	  (index (mark-index end-changes-mark)))
      (let ((start-index (line-start-index group index)))
	(set-line-window-string!
	 (inferior-window (car inferiors))
	 (group-extract-string group start-index (line-end-index group index))
	 truncate-lines?)
	(fill-top! window inferiors start-index true)))
    (everything-changed! window maybe-recenter!)))

(define (recompute-image!:bottom-changed window)
  (with-instance-variables buffer-window window ()
    (let ((inferiors (start-changes-inferiors window))
	  (group (mark-group start-changes-mark))
	  (index (mark-index start-changes-mark)))
      (let ((end-index (line-end-index group index)))
	(set-line-window-string!
	 (inferior-window (car inferiors))
	 (group-extract-string group (line-start-index group index) end-index)
	 truncate-lines?)
	(set-cdr! inferiors
		  (fill-bottom window
			       (inferior-y-end (car inferiors))
			       end-index))))
    (everything-changed! window maybe-recenter!)))

(define (recompute-image!:middle-changed window)
  (with-instance-variables buffer-window window ()
    (let ((start-inferiors (start-changes-inferiors window))
	  (end-inferiors (end-changes-inferiors window))
	  (group (buffer-group buffer))
	  (start-index (mark-index start-changes-mark))
	  (end-index (mark-index end-changes-mark)))
      (let ((start-start (line-start-index group start-index))
	    (start-end (line-end-index group start-index))
	    (end-start (line-start-index group end-index))
	    (end-end (line-end-index group end-index)))
	(if (eq? start-inferiors end-inferiors)
	    (if (fix:= start-start end-start)

  ;; In this case, the changed region was a single line before the
  ;; changes, and is still a single line now.  All we need do is redraw
  ;; the line and then scroll the rest up or down if the y-size of the
  ;; line has been changed.
  (let ((y-end (inferior-y-end (car start-inferiors))))
    (set-line-window-string!
     (inferior-window (car start-inferiors))
     (group-extract-string group start-start start-end)
     truncate-lines?)
    (let ((y-end* (inferior-y-end (car start-inferiors))))
      (if (fix:= y-end y-end*)
	  (maybe-marks-changed! window start-inferiors y-end*)
	  (begin
	    (set-cdr! start-inferiors
		      (cond ((fix:< y-end y-end*)
			     (scroll-lines-down! window
						 (cdr start-inferiors)
						 y-end*))
			    ((not (null? (cdr start-inferiors)))
			     (scroll-lines-up! window
					       (cdr start-inferiors)
					       y-end*
					       (fix:1+ start-end)))
			    (else
			     (fill-bottom window y-end* start-end))))
	    (everything-changed! window maybe-recenter!)))))

  ;; Here, the changed region used to be a single line, and now is
  ;; several, so we need to insert a bunch of new lines.
  (begin
   (set-line-window-string! (inferior-window (car start-inferiors))
			    (group-extract-string group start-start start-end)
			    truncate-lines?)
   (set-cdr! start-inferiors
	     (if (null? (cdr start-inferiors))
		 (fill-bottom window
			      (inferior-y-end (car start-inferiors))
			      start-end)
		 (fill-middle! window
			       (inferior-y-end (car start-inferiors))
			       start-end
			       (cdr start-inferiors)
			       (fix:1+ end-end))))
   (everything-changed! window maybe-recenter!))
  )
;;; continued on next page...

;;; ...continued from previous page

  (if (fix:= start-start end-start)

  ;; The changed region used to be multiple lines and is now just one.
  ;; We must scroll the bottom of the screen up to fill in.
  (begin
   (set-line-window-string! (inferior-window (car start-inferiors))
			    (group-extract-string group start-start start-end)
			    truncate-lines?)
   (set-cdr! start-inferiors
	     (if (null? (cdr end-inferiors))
		 (fill-bottom window
			      (inferior-y-end (car start-inferiors))
			      start-end)
		 (scroll-lines-up! window
				   (cdr end-inferiors)
				   (inferior-y-end (car start-inferiors))
				   (fix:1+ start-end))))
   (everything-changed! window maybe-recenter!))

  ;; The most general case, we must refill the center of the screen.
  (begin
    (set-line-window-string!
     (inferior-window (car start-inferiors))
     (group-extract-string group start-start start-end)
     truncate-lines?)
    (let ((old-y-end (inferior-y-end (car end-inferiors))))
      (set-line-window-string! (inferior-window (car end-inferiors))
			       (group-extract-string group end-start end-end)
			       truncate-lines?)
      (let ((y-end (inferior-y-end (car end-inferiors)))
	    (tail (cdr end-inferiors)))
	(cond ((fix:> y-end old-y-end)
	       (set-cdr! end-inferiors (scroll-lines-down! window tail y-end)))
	      ((fix:< y-end old-y-end)
	       (set-cdr! end-inferiors
			 (scroll-lines-up! window
					   tail
					   y-end
					   (fix:1+ end-end)))))))
    (set-cdr! start-inferiors
	      (fill-middle! window
			    (inferior-y-end (car start-inferiors))
			    start-end
			    end-inferiors
			    end-start))
    (everything-changed! window maybe-recenter!))

  ))))))

;;;; Direct Update/Output Support

;;; The direct output procedures are hairy and should be used only
;;; under restricted conditions.  In particular, the cursor may not be
;;; at the right margin (for insert and forward) or the left margin
;;; (for backward), and the character being inserted must be an
;;; ordinary graphic character.  For insert, the buffer must be
;;; modifiable, and the modeline must already show that it has been
;;; modified.  None of the procedures may be used if the window needs
;;; redisplay.
;;; They must be called without interrupts.

(define (%window-direct-update! window display-style)
  (with-instance-variables buffer-window window (display-style)
    (if (not saved-screen)
	(error "Window needs normal redisplay -- can't direct update" window))
    (and (with-screen-in-update! saved-screen
	   (lambda ()
	     (update-buffer-window! window saved-screen
				    saved-x-start saved-y-start
				    saved-xl saved-xu saved-yl saved-yu
				    display-style)))
	 (begin
	   (set-car! redisplay-flags false)
	   true))))

(define (%direct-output-forward-character! window)
  (with-instance-variables buffer-window window ()
   (%set-buffer-point! buffer (mark1+ point))
   (set! point (buffer-point buffer))
   (let ((x-start (fix:1+ (inferior-x-start cursor-inferior)))
	 (y-start (inferior-y-start cursor-inferior)))
     (screen-write-cursor! saved-screen
			   (fix:+ saved-x-start x-start)
			   (fix:+ saved-y-start y-start))
     (screen-flush! saved-screen)
     (%set-inferior-x-start! cursor-inferior x-start))))

(define (%direct-output-backward-character! window)
  (with-instance-variables buffer-window window ()
   (%set-buffer-point! buffer (mark-1+ point))
   (set! point (buffer-point buffer))
   (let ((x-start (fix:-1+ (inferior-x-start cursor-inferior)))
	 (y-start (inferior-y-start cursor-inferior)))
     (screen-write-cursor! saved-screen
			   (fix:+ saved-x-start x-start)
			   (fix:+ saved-y-start y-start))
     (screen-flush! saved-screen)
     (%set-inferior-x-start! cursor-inferior x-start))))

(define (%direct-output-insert-char! window char)
  (with-instance-variables buffer-window window (char)
   (let ((x-start (inferior-x-start cursor-inferior))
	 (y-start (inferior-y-start cursor-inferior)))
     (let ((x (fix:+ saved-x-start x-start))
	   (y (fix:+ saved-y-start y-start)))
       (screen-write-char! saved-screen x y char)
       (screen-write-cursor! saved-screen (fix:1+ x) y)
       (screen-flush! saved-screen))
     (line-window-direct-output-insert-char!
      (inferior-window (car (y->inferiors window y-start)))
      x-start
      char)
     (%set-inferior-x-start! cursor-inferior (fix:1+ x-start)))))

(define (%direct-output-insert-newline! window)
  (with-instance-variables buffer-window window ()
   (let ((y-start (fix:1+ (inferior-y-start cursor-inferior))))
     (let ((inferior (make-inferior window line-window)))
       (%set-inferior-x-start! inferior 0)
       (%set-inferior-y-start! inferior y-start)
       (set-cdr! (last-pair line-inferiors) (list inferior))
       (set! last-line-inferior inferior)
       (line-window-direct-output-insert-newline!
	(inferior-window inferior)))
     (let ((y-end (fix:1+ y-start)))
       (if (fix:< y-end y-size)
	   (begin
	     (%set-inferior-y-size! blank-inferior (fix:- y-size y-end))
	     (%set-inferior-y-start! blank-inferior y-end))
	   (begin
	     (%set-inferior-x-start! blank-inferior false)
	     (%set-inferior-y-start! blank-inferior false))))
     (%set-inferior-x-start! cursor-inferior 0)
     (%set-inferior-y-start! cursor-inferior y-start)
     (screen-write-cursor! saved-screen
			   saved-x-start
			   (fix:+ saved-y-start y-start))
     (screen-flush! saved-screen))))

(define (%direct-output-insert-substring! window string start end)
  (with-instance-variables buffer-window window (string start end)
   (let ((x-start (inferior-x-start cursor-inferior))
	 (y-start (inferior-y-start cursor-inferior))
	 (length (fix:- end start)))
     (let ((x (fix:+ saved-x-start x-start))
	   (y (fix:+ saved-y-start y-start)))
       (screen-write-substring! saved-screen x y string start end)
       (screen-write-cursor! saved-screen (fix:+ x length) y)
       (screen-flush! saved-screen))
     (line-window-direct-output-insert-substring!
      (inferior-window (car (y->inferiors window y-start)))
      x-start
      string start end)
     (%set-inferior-x-start! cursor-inferior (fix:+ x-start length)))))