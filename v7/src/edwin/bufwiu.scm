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

;;;; Buffer Windows:  Image Update

(declare (usual-integrations)
	 )
(using-syntax class-syntax-table

;;;; Insert/Delete/Clip

;;; It is assumed that the insert daemon is called after the insertion
;;; has been performed, and the delete daemon before the deletion has
;;; been performed.  It is also assumed that interrupts are disabled.

(define-procedure buffer-window ((make-changes-daemon window) group start end)
  (cond (start-changes-mark
	 (cond ((< start (mark-index start-changes-mark))
		(set! start-changes-mark
		      (%make-permanent-mark group start #!FALSE)))
	       ((> end (mark-index end-changes-mark))
		(set! end-changes-mark
		      (%make-permanent-mark group end #!TRUE)))))
	(else
	 (set! start-changes-mark (%make-permanent-mark group start #!FALSE))
	 (set! end-changes-mark (%make-permanent-mark group end #!TRUE))))
  (if (and (>= end (mark-index start-line-mark))
	   (<= start (mark-index end-mark)))
      (setup-redisplay-flags! redisplay-flags)))

;;; It is assumed that the clip daemon is called before the clipping
;;; has been performed, so that we can get the old clipping limits.

(define-procedure buffer-window ((make-clip-daemon window) group start end)
  (if (not start-clip-mark)
      (begin (set! start-clip-mark (group-display-start group))
	     (set! end-clip-mark (group-display-end group))))
  (let ((window-start (mark-index start-line-mark))
	(window-end (mark-index end-mark)))
    (if (or (> start window-start)
	    (< end window-end)
	    (and (< start window-start)
		 (= window-start (mark-index start-clip-mark)))
	    (and (> end window-end)
		 (= window-end (mark-index end-clip-mark))))
	(setup-redisplay-flags! redisplay-flags))))

(define (update-buffer-window! window screen x-start y-start
			       xl xu yl yu display-style)
  ;; The primary update entry.
  (recompute-image! window)
  (update-inferiors! window screen x-start y-start xl xu yl yu display-style))

(define-procedure buffer-window (maybe-recompute-image! window)
  ;; Used to guarantee everything updated before certain operations.
  (if (car redisplay-flags)
      (recompute-image! window)))

(define-procedure buffer-window (recompute-image! window)
  (without-interrupts
   (lambda ()
     (%recompute-image! window))))

(define-procedure buffer-window (%recompute-image! window)
  (let ((group (mark-group start-mark))
	(start-line (mark-index start-line-mark))
	(start (mark-index start-mark))
	(end (mark-index end-mark))
	(point-index (mark-index point)))
    (if start-clip-mark
	(let ((new-clip-start (group-start-index group))
	      (new-clip-end (group-end-index group)))
	  (cond ((< point-index new-clip-start)
		 (%set-buffer-point! buffer (group-display-start group))
		 (set! point (buffer-point buffer)))
		((> point-index new-clip-end)
		 (%set-buffer-point! buffer (group-display-end group))
		 (set! point (buffer-point buffer))))
	  (cond ((> new-clip-start start-line)
		 (%window-redraw! window #!FALSE))
		((or (< new-clip-end end)
		     (and (< new-clip-start start-line)
			  (= start-line (mark-index start-clip-mark)))
		     (and (> new-clip-end end)
			  (= end (mark-index end-clip-mark))))
		 (%window-redraw!
		  window
		  (and (not start-changes-mark)
		       (>= point-index start)
		       (<= point-index end)
		       (%window-point-y window))))
		(else
		 (set! start-clip-mark #!FALSE)
		 (set! end-clip-mark #!FALSE)))))
    (if start-changes-mark
	(let ((start-changes (mark-index start-changes-mark))
	      (end-changes (mark-index end-changes-mark)))
	  (if (and (>= end-changes start-line)
		   (<= start-changes end))
	      (cond ((<= start-changes start)
		     (cond ((< end-changes end)
			    (recompute-image!:top-changed window))
			   (else
			    (%window-redraw! window #!FALSE))))
		    ((>= end-changes end)
		     (recompute-image!:bottom-changed window))
		    (else
		     (recompute-image!:middle-changed window)))
	      (begin (set! start-changes-mark #!FALSE)
		     (set! end-changes-mark #!FALSE))))))
  (if point-moved?
      (update-cursor! window maybe-recenter!)))

(define-procedure buffer-window (recompute-image!:top-changed window)
  (let ((inferiors (end-changes-inferiors window))
	(group (mark-group end-changes-mark))
	(index (mark-index end-changes-mark)))
    (let ((start-index (line-start-index group index)))
      (set-line-window-string!
       (inferior-window (car inferiors))
       (group-extract-string group start-index (line-end-index group index)))
      (fill-top! window inferiors start-index #!TRUE)))
  (everything-changed! window maybe-recenter!))

(define-procedure buffer-window (recompute-image!:bottom-changed window)
  (let ((inferiors (start-changes-inferiors window))
	(group (mark-group start-changes-mark))
	(index (mark-index start-changes-mark)))
    (let ((end-index (line-end-index group index)))
      (set-line-window-string!
       (inferior-window (car inferiors))
       (group-extract-string group
			     (line-start-index group index)
			     end-index))
      (set-cdr! inferiors
		(fill-bottom window
			     (inferior-y-end (car inferiors))
			     end-index))))
  (everything-changed! window maybe-recenter!))

(define-procedure buffer-window (recompute-image!:middle-changed window)
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
	  (if (= start-start end-start)

;;; In this case, the changed region was a single line before the
;;; changes, and is still a single line now.  All we need do is redraw
;;; the line and then scroll the rest up or down if the y-size of the
;;; line has been changed.
(let ((y-end (inferior-y-end (car start-inferiors))))
  (set-line-window-string! (inferior-window (car start-inferiors))
			   (group-extract-string group start-start start-end))
  (let ((y-end* (inferior-y-end (car start-inferiors))))
    (if (= y-end y-end*)
	(maybe-marks-changed! window start-inferiors y-end*)
	(begin (set-cdr!
		start-inferiors
		(cond ((< y-end y-end*)
		       (scroll-lines-down! window
					   (cdr start-inferiors)
					   y-end*))
		      ((not (null? (cdr start-inferiors)))
		       (scroll-lines-up! window
					 (cdr start-inferiors)
					 y-end*
					 (1+ start-end)))
		      (else
		       (fill-bottom window y-end* start-end))))
	       (everything-changed! window maybe-recenter!)))))

;;; Here, the changed region used to be a single line, and now is
;;; several, so we need to insert a bunch of new lines.
(begin
 (set-line-window-string! (inferior-window (car start-inferiors))
			  (group-extract-string group start-start start-end))
 (set-cdr! start-inferiors
	   (if (null? (cdr start-inferiors))
	       (fill-bottom window
			    (inferior-y-end (car start-inferiors))
			    start-end)
	       (fill-middle! window
			     (inferior-y-end (car start-inferiors))
			     start-end
			     (cdr start-inferiors)
			     (1+ end-end))))
 (everything-changed! window maybe-recenter!))

)
(if (= start-start end-start)

;;; The changed region used to be multiple lines and is now just one.
;;; We must scroll the bottom of the screen up to fill in.
(begin
 (set-line-window-string! (inferior-window (car start-inferiors))
			  (group-extract-string group start-start start-end))
 (set-cdr! start-inferiors
	   (if (null? (cdr end-inferiors))
	       (fill-bottom window
			    (inferior-y-end (car start-inferiors))
			    start-end)
	       (scroll-lines-up! window
				 (cdr end-inferiors)
				 (inferior-y-end (car start-inferiors))
				 (1+ start-end))))
 (everything-changed! window maybe-recenter!))

;;; The most general case, we must refill the center of the screen.
(begin
  (set-line-window-string! (inferior-window (car start-inferiors))
			   (group-extract-string group
						 start-start start-end))
  (let ((old-y-end (inferior-y-end (car end-inferiors))))
    (set-line-window-string! (inferior-window (car end-inferiors))
			     (group-extract-string group
						   end-start end-end))
    (let ((y-end (inferior-y-end (car end-inferiors)))
	  (tail (cdr end-inferiors)))
      (cond ((> y-end old-y-end)
	     (set-cdr! end-inferiors (scroll-lines-down! window tail y-end)))
	    ((< y-end old-y-end)
	     (set-cdr! end-inferiors
		       (scroll-lines-up! window tail y-end (1+ end-end)))))))
  (set-cdr! start-inferiors
	    (fill-middle! window
			  (inferior-y-end (car start-inferiors))
			  start-end
			  end-inferiors
			  end-start))
  (everything-changed! window maybe-recenter!))

)))))

;;;; Direct Update/Output Support

;;; The direct output procedures are hairy and should be used only
;;; under restricted conditions.  In particular, the cursor may not be
;;; at the right margin (for insert and forward) or the left margin
;;; (for backward), and the character being inserted must be an
;;; ordinary graphic character.  For insert, the buffer must be
;;; modifiable, and the modeline must already show that it has been
;;; modified.  None of the procedures may be used if the window needs
;;; redisplay.

(define-procedure buffer-window (%window-direct-update! window display-style)
  (if (not saved-screen)
      (error "Window needs normal redisplay -- can't direct update" window))
  (and (update-buffer-window! window saved-screen saved-x-start saved-y-start
			      saved-xl saved-xu saved-yl saved-yu
			      display-style)
       (begin (set-car! redisplay-flags #!FALSE)
	      #!TRUE)))

(define-procedure buffer-window (%window-needs-redisplay? window)
  (car redisplay-flags))

(define-procedure buffer-window (%direct-output-forward-character! window)
  (without-interrupts
   (lambda ()
     (%set-buffer-point! buffer (mark1+ point))
     (set! point (buffer-point buffer))
     (let ((x-start (1+ (inferior-x-start cursor-inferior)))
	   (y-start (inferior-y-start cursor-inferior)))
       (screen-write-cursor! saved-screen
			     (+ saved-x-start x-start)
			     (+ saved-y-start y-start))
       (%set-inferior-x-start! cursor-inferior x-start)))))

(define-procedure buffer-window (%direct-output-backward-character! window)
  (without-interrupts
   (lambda ()
     (%set-buffer-point! buffer (mark-1+ point))
     (set! point (buffer-point buffer))
     (let ((x-start (-1+ (inferior-x-start cursor-inferior)))
	   (y-start (inferior-y-start cursor-inferior)))
       (screen-write-cursor! saved-screen
			     (+ saved-x-start x-start)
			     (+ saved-y-start y-start))
       (%set-inferior-x-start! cursor-inferior x-start)))))

(define-procedure buffer-window (%direct-output-insert-char! window char)
  (without-interrupts
   (lambda ()
     (let ((x-start (inferior-x-start cursor-inferior))
	   (y-start (inferior-y-start cursor-inferior)))
       (let ((x (+ saved-x-start x-start))
	     (y (+ saved-y-start y-start)))
	 (screen-write-char! saved-screen x y char)
	 (screen-write-cursor! saved-screen (1+ x) y))
       (line-window-direct-output-insert-char!
	(inferior-window (car (y->inferiors window y-start)))
	x-start
	char)
       (%set-inferior-x-start! cursor-inferior (1+ x-start))))))

(define-procedure buffer-window (%direct-output-insert-newline! window)
  (without-interrupts
   (lambda ()
     (let ((y-start (1+ (inferior-y-start cursor-inferior))))
       (let ((inferior (make-inferior window line-window)))
	 (%set-inferior-x-start! inferior 0)
	 (%set-inferior-y-start! inferior y-start)
	 (set-cdr! (last-pair line-inferiors) (list inferior))
	 (set! last-line-inferior inferior)
	 (line-window-direct-output-insert-newline!
	  (inferior-window inferior)))
       (let ((y-end (1+ y-start)))
	 (if (< y-end y-size)
	     (begin (%set-inferior-y-size! blank-inferior (- y-size y-end))
		    (%set-inferior-y-start! blank-inferior y-end))
	     (begin (%set-inferior-x-start! blank-inferior #!FALSE)
		    (%set-inferior-y-start! blank-inferior #!FALSE))))
       (%set-inferior-x-start! cursor-inferior 0)
       (%set-inferior-y-start! cursor-inferior y-start)
       (screen-write-cursor! saved-screen
			     saved-x-start
			     (+ saved-y-start y-start))))))

(define-procedure buffer-window
		  (%direct-output-insert-substring! window string start end)
  (without-interrupts
   (lambda ()
     (let ((x-start (inferior-x-start cursor-inferior))
	   (y-start (inferior-y-start cursor-inferior))
	   (length (- end start)))
       (let ((x (+ saved-x-start x-start))
	     (y (+ saved-y-start y-start)))
	 (screen-write-substring! saved-screen x y string start end)
	 (screen-write-cursor! saved-screen (+ x length) y))
       (line-window-direct-output-insert-substring!
	(inferior-window (car (y->inferiors window y-start)))
	x-start string start end)
       (%set-inferior-x-start! cursor-inferior (+ x-start length))))))

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: (access window-package edwin-package)
;;; Scheme Syntax Table: class-syntax-table
;;; End:
