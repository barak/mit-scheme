;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/bufwin.scm,v 1.283 1989/08/14 10:23:32 cph Exp $
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

;;;; Buffer Windows:  Base

(declare (usual-integrations))

;;; The following instance variables contain marks which must -NEVER-
;;; be passed to anyone who will keep a pointer to them.  The reason
;;; is that the `mark-temporary!' operation is called on these marks,
;;; which invalidates them as soon as some change happens to the
;;; buffer.  Remember, you were warned!
;;;
;;; start-line-mark
;;; start-mark
;;; end-mark
;;; end-line-mark
;;; start-changes-mark
;;; end-changes-mark
;;; start-clip-mark
;;; end-clip-mark

(define-class buffer-window vanilla-window
  (buffer point changes-daemon clip-daemon
	  cursor-inferior blank-inferior
	  line-inferiors last-line-inferior
	  start-line-mark start-mark end-mark end-line-mark
	  start-changes-mark end-changes-mark point-moved?
	  start-clip-mark end-clip-mark
	  saved-screen saved-x-start saved-y-start
	  saved-xl saved-xu saved-yl saved-yu
	  override-inferior truncate-lines? force-redraw?))

(define-method buffer-window (:initialize! window window*)
  (usual=> window :initialize! window*)
  (set! cursor-inferior (make-inferior window cursor-window))
  (set! blank-inferior (make-inferior window blank-window))
  (set! changes-daemon (make-changes-daemon window))
  (set! clip-daemon (make-clip-daemon window))
  (set! override-inferior false)
  (set! force-redraw? 'CENTER)
  unspecific)

(define-method buffer-window (:kill! window)
  (delete-window-buffer! window)
  (usual=> window :kill!))

(define-method buffer-window (:update-display! window screen x-start y-start
					       xl xu yl yu display-style)
  (set! saved-screen screen)
  (set! saved-x-start x-start) (set! saved-y-start y-start)
  (set! saved-xl xl) (set! saved-xu xu) (set! saved-yl yl) (set! saved-yu yu)
  (update-buffer-window! window screen x-start y-start
			 xl xu yl yu display-style))

(define-method buffer-window (:salvage! window)
  (%set-buffer-point! buffer
		      (make-mark (buffer-group buffer)
				 (group-start-index (buffer-group buffer))))
  (set! point (buffer-point buffer))
  (window-modeline-event! superior 'SALVAGE)
  (%window-redraw! window false))

(define (set-buffer-window-size! window x y)
  (with-instance-variables buffer-window window (x y)
    (set! saved-screen false)
    (let ((old-y y-size))
      (usual=> window :set-size! x y)
      ;; Preserve point y unless it is offscreen now.
      (%window-setup-truncate-lines! window false)
      (%window-force-redraw! window (and old-y (%window-cursor-y window))))))

(define-method buffer-window :set-size!
  set-buffer-window-size!)

(define-method buffer-window (:set-x-size! window x)
  (set-buffer-window-size! window x y-size))

(define-method buffer-window (:set-y-size! window y)
  (set-buffer-window-size! window x-size y))

(define (%window-setup-truncate-lines! window redraw-type)
  (with-instance-variables buffer-window window ()
    (if (not (within-editor?))
	(begin
	  (set! truncate-lines? (ref-variable truncate-lines))
	  unspecific)
	(let ((new-truncate-lines?
	       (or (and (variable-local-value
			 buffer
			 (ref-variable-object truncate-partial-width-windows))
			(window-has-horizontal-neighbor? superior))
		   (variable-local-value
		    buffer
		    (ref-variable-object truncate-lines)))))
	  (if (not (boolean=? truncate-lines? new-truncate-lines?))
	      (begin
		(set! truncate-lines? new-truncate-lines?)
		(if (and redraw-type (not force-redraw?))
		    (%window-force-redraw! window redraw-type))))))))

(define-variable-per-buffer truncate-lines
  "*True means do not display continuation lines;
give each line of text one screen line.
Automatically becomes local when set in any fashion.

Note that this is overridden by the variable
truncate-partial-width-windows if that variable is true
and this buffer is not full-screen width."
  false)

(define-variable truncate-partial-width-windows
  "*True means truncate lines in all windows less than full screen wide."
  true)

(let ((setup-truncate-lines!
       (lambda (variable)
	 variable			;ignore
	 (for-each window-setup-truncate-lines! (all-windows)))))
  (add-variable-assignment-daemon!
   (ref-variable-object truncate-lines)
   setup-truncate-lines!)
  (add-variable-assignment-daemon!
   (ref-variable-object truncate-partial-width-windows)
   setup-truncate-lines!))

;;;; Group Operations

;;; These are identical to the operations of the same name used
;;; elsewhere in the editor, except that they clip at the display clip
;;; limits rather than the text clip limits.

(define-integrable (group-start-index group)
  (mark-index (group-display-start group)))

(define-integrable (group-end-index group)
  (mark-index (group-display-end group)))

(define-integrable (group-start-index? group index)
  (not (fix:> index (group-start-index group))))

(define-integrable (group-end-index? group index)
  (not (fix:< index (group-end-index group))))

(define (line-start-index group index)
  (let ((limit (group-start-index group)))
    (or (%find-previous-newline group index limit)
	limit)))

(define (line-end-index group index)
  (let ((limit (group-end-index group)))
    (or (%find-next-newline group index limit)
	limit)))

(define (line-start-index? group index)
  (or (group-start-index? group index)
      (char=? (group-left-char group index) #\newline)))

(define (line-end-index? group index)
  (or (group-end-index? group index)
      (char=? (group-right-char group index) #\newline)))

(define (clip-mark-to-display window mark)
  (with-instance-variables buffer-window window (mark)
    (if (not (mark? mark))
	(error "Argument not a mark" mark))
    (if (not (mark~ point mark))
	(error "Mark not within displayed buffer" mark))
    (let ((group (mark-group mark))
	  (index (mark-index mark)))
      (cond ((group-start-index? group index) (group-display-start group))
	    ((group-end-index? group index) (group-display-end group))
	    (else mark)))))

;;;; Buffer and Point

(define-integrable (%window-buffer window)
  (with-instance-variables buffer-window window ()
    buffer))

(define (%window-buffer-cursor-y window)
  (with-instance-variables buffer-window window ()
    (let ((py (buffer-cursor-y buffer)))
      (and py
	   (begin
	     (set-buffer-cursor-y! buffer false)
	     (and (fix:= (car py) (mark-index point))
		  (fix:< (cdr py) y-size)
		  (cdr py)))))))

(define (%set-window-buffer! window new-buffer)
  (with-instance-variables buffer-window window (new-buffer)
    (if (not (buffer? new-buffer)) (error "Argument not a buffer" new-buffer))
    (set-buffer-cursor-y! buffer
			  (let ((y (%window-cursor-y window)))
			    (and y (cons (mark-index point) y))))
    (delete-window-buffer! window)
    (initial-buffer! window new-buffer)
    (window-modeline-event! superior 'NEW-BUFFER)
    (%window-force-redraw! window (%window-buffer-cursor-y window))))

(define (initial-buffer! window new-buffer)
  (with-instance-variables buffer-window window (new-buffer)
    (set! buffer new-buffer)
    (add-buffer-window! buffer superior)
    (let ((group (buffer-group buffer)))
      (add-group-delete-daemon! group changes-daemon)
      (add-group-insert-daemon! group changes-daemon)
      (add-group-clip-daemon! group clip-daemon)
      (let ((point (mark-index (buffer-point buffer)))
	    (start (group-start-index group))
	    (end (group-end-index group)))
	(cond ((fix:< point start)
	       (%set-buffer-point! buffer (make-mark group start)))
	      ((fix:> point end)
	       (%set-buffer-point! buffer (make-mark group end))))))
    (set! point (buffer-point buffer))
    unspecific))

(define (delete-window-buffer! window)
  (with-instance-variables buffer-window window ()
    (let ((group (buffer-group buffer)))
      (remove-group-delete-daemon! group changes-daemon)
      (remove-group-insert-daemon! group changes-daemon)
      (remove-group-clip-daemon! group clip-daemon))
    (remove-buffer-window! buffer superior)))

(define-integrable (%window-point window)
  (with-instance-variables buffer-window window ()
    point))

(define (%set-window-point! window mark)
  (with-instance-variables buffer-window window (mark)
    (%set-buffer-point! buffer mark)
    (set! point (buffer-point buffer))
    (set! point-moved? true)
    (setup-redisplay-flags! redisplay-flags)))

(define-integrable (%window-cursor window)
  (with-instance-variables buffer-window window ()
    (inferior-window cursor-inferior)))

(define (%window-cursor-y window)
  (with-instance-variables buffer-window window ()
    (let ((y (inferior-y-start cursor-inferior)))
      (and y (fix:< y y-size) y))))

;;;; Override Message

;;; This is used to display messages over the typein window.

(define (set-override-message! window message)
  (with-instance-variables buffer-window window (message)
    (if (not override-inferior)
	(begin
	  (set! override-inferior (make-inferior window line-window))
	  (set! inferiors
		(list override-inferior cursor-inferior blank-inferior))
	  (set-inferior-start! override-inferior 0 0)))
    (let ((override-window (inferior-window override-inferior)))
      (set-line-window-string! override-window message truncate-lines?)
      (set-inferior-position!
       cursor-inferior
       (string-base:index->coordinates override-window
				       (string-length message))))
    (set-blank-inferior-start! window (inferior-y-end override-inferior))))

(define (clear-override-message! window)
  (with-instance-variables buffer-window window ()
    (if override-inferior
	(begin
	  (set! override-inferior false)
	  (set! inferiors
		(cons* cursor-inferior blank-inferior line-inferiors))
	  (set-inferior-position! cursor-inferior
				  (%window-mark->coordinates window point))
	  (blank-inferior-changed! window)
	  (for-each inferior-needs-redisplay! inferiors)))))

(define (home-cursor! window)
  (with-instance-variables buffer-window window ()
    (screen-write-cursor! saved-screen saved-x-start saved-y-start)
    (screen-flush! saved-screen)))

;;;; Inferiors

(define (make-line-inferior window start end)
  (with-instance-variables buffer-window window (start end)
    (let ((inferior (make-inferior window line-window)))
      (set-line-window-string! (inferior-window inferior)
			       (group-extract-string (buffer-group buffer)
						     start end)
			       truncate-lines?)
      inferior)))

(define-integrable (first-line-inferior window)
  (with-instance-variables buffer-window window ()
    (car line-inferiors)))

(define-integrable (line-inferior-length inferiors)
  (fix:1+ (line-window-length (inferior-window (car inferiors)))))

(define-integrable (blank-inferior-changed! window)
  (with-instance-variables buffer-window window ()
    (if (not override-inferior)
	(set-blank-inferior-start! window
				   (inferior-y-end last-line-inferior)))))

(define-integrable (set-blank-inferior-start! window y-end)
  (with-instance-variables buffer-window window (y-end)
    (if (fix:< y-end y-size)
	(begin
	  (set-inferior-size! blank-inferior x-size (fix:- y-size y-end))
	  (set-inferior-start! blank-inferior 0 y-end))
	(set-inferior-start! blank-inferior false false))))

(define-integrable (set-line-inferiors! window inferiors start)
  (with-instance-variables buffer-window window (inferiors start)
    (set! line-inferiors inferiors)
    (destroy-mark! start-line-mark)
    (set! start-line-mark
	  (%make-permanent-mark (buffer-group buffer) start false))
    unspecific))

(define (line-inferiors-changed! window)
  (with-instance-variables buffer-window window ()
    (define (loop inferiors start)
      (if (null? (cdr inferiors))
	  (begin
	    (set! last-line-inferior (car inferiors))
	    (destroy-mark! end-line-mark)
	    (set! end-line-mark
		  (let ((group (buffer-group buffer)))
		    (%make-permanent-mark group
					  (line-end-index group start)
					  true))))
	  (loop (cdr inferiors)
		(fix:+ start (line-inferior-length inferiors)))))
    (loop line-inferiors (mark-index start-line-mark))
    (if (not override-inferior)
	(set! inferiors (cons* cursor-inferior blank-inferior line-inferiors)))
    unspecific))

(define (y->inferiors window y)
  (with-instance-variables buffer-window window (y)
    (define (loop previous-inferiors inferiors)
      (cond ((fix:< y (inferior-y-start (car inferiors))) previous-inferiors)
	    ((null? (cdr inferiors))
	     (and (fix:< y (inferior-y-end (car inferiors)))
		  inferiors))
	    (else (loop inferiors (cdr inferiors)))))
    (loop false line-inferiors)))

(define (index->inferiors window index)
  (with-instance-variables buffer-window window (index)
    ;; Assumes that (>= INDEX (MARK-INDEX START-LINE-MARK)).
    (define (loop inferiors start)
      (let ((new-start (fix:+ start (line-inferior-length inferiors))))
	(if (fix:< index new-start)
	    inferiors
	    (and (not (null? (cdr inferiors)))
		 (loop (cdr inferiors) new-start)))))
    (loop line-inferiors (mark-index start-line-mark))))

(define (inferiors->index window inferiors)
  (with-instance-variables buffer-window window (inferiors)
    ;; Assumes that INFERIORS is a tail of LINE-INFERIORS.
    (define (loop inferiors* start)
      (if (eq? inferiors inferiors*)
	  start
	  (loop (cdr inferiors*)
		(fix:+ start (line-inferior-length inferiors*)))))
    (loop line-inferiors (mark-index start-line-mark))))

(define (y->inferiors&index window y receiver)
  (with-instance-variables buffer-window window (y receiver)
    ;; This is used for scrolling.
    (define (loop inferiors start previous-inferiors previous-start)
      (cond ((fix:< y (inferior-y-start (car inferiors)))
	     (receiver previous-inferiors previous-start))
	    ((null? (cdr inferiors))
	     (and (fix:< y (inferior-y-end (car inferiors)))
		  (receiver inferiors start)))
	    (else
	     (loop (cdr inferiors)
		   (fix:+ start (line-inferior-length inferiors))
		   inferiors
		   start))))
    (loop line-inferiors (mark-index start-line-mark) false false)))

(define (start-changes-inferiors window)
  (with-instance-variables buffer-window window ()
    ;; Assumes that (MARK<= START-LINE-MARK START-CHANGES-MARK).
    ;; Guarantees to return non-'() result.
    (or (index->inferiors window (mark-index start-changes-mark))
	(error "Can't find START-CHANGES"))))

(define (end-changes-inferiors window)
  (with-instance-variables buffer-window window ()
    ;; Assumes that (MARK<= END-CHANGES-MARK END-LINE-MARK).
    ;; Guarantees to return non-'() result.
    (let ((index (mark-index end-changes-mark)))
      (define (loop inferiors not-found)
	(if (null? inferiors)
	    (not-found (mark-index end-line-mark))
	    (loop (cdr inferiors)
	      (lambda (end)
		(let ((new-end (fix:- end (line-inferior-length inferiors))))
		  (if (fix:< new-end index)
		      inferiors
		      (not-found new-end)))))))
      (loop line-inferiors
	(lambda (end)
	  end				;ignore
	  (error "Can't find END-CHANGES"))))))

;;;; Changes

(define (update-cursor! window if-not-visible)
  (with-instance-variables buffer-window window (if-not-invisible)
    (if (%window-mark-visible? window point)
	(begin
	  (set-inferior-position! cursor-inferior
				  (%window-mark->coordinates window point))
	  (set! point-moved? false)
	  (window-modeline-event! superior 'CURSOR-MOVED))
	(if-not-visible window))))

(define (maybe-recenter! window)
  (with-instance-variables buffer-window window ()
    (let ((threshold (ref-variable scroll-step))
	  (recenter!
	   (lambda ()
	     (%window-redraw! window (%window-y-center window)))))
      (if (not (object-type? (ucode-type fixnum) threshold))
	  (error "Not a small integer" threshold))
      (if (fix:zero? threshold)
	  (recenter!)
	  (if (fix:< (mark-index point) (mark-index start-mark))
	      (let ((limit
		     (%window-coordinates->index window
						 0
						 (fix:- 0 threshold))))
		(if (or (not limit)
			(not (fix:< (mark-index point) limit)))
		    (%window-scroll-y-relative! window
						(%window-point-y window))
		    (recenter!)))
	      (let ((limit
		     (%window-coordinates->index window
						 0
						 (fix:+ (window-y-size window)
							threshold))))
		(if (or (not limit) (fix:< (mark-index point) limit))
		    (%window-scroll-y-relative!
		     window
		     (fix:- (%window-point-y window)
			    (fix:-1+ (window-y-size window))))
		    (recenter!))))))))

(define-variable scroll-step
  "*The number of lines to try scrolling a window by when point moves out.
If that fails to bring point back on screen, point is centered instead.
If this is zero, point is always centered after it moves off screen."
  0)

(define (%window-force-redraw! window redraw-type)
  (with-instance-variables buffer-window window ()
    (set! force-redraw? (or redraw-type 'CENTER))
    (setup-redisplay-flags! redisplay-flags)))

(define (%window-redraw-preserving-start! window)
  (with-instance-variables buffer-window window ()
    (let ((group (mark-group start-mark))
	  (start-line (mark-index start-line-mark)))
      (let ((start (if truncate-lines? start-line (mark-index start-mark)))
	    (end (line-end-index group start-line)))
	(let ((inferior (make-line-inferior window start-line end)))
	  (set-inferior-start!
	   inferior
	   0
	   (fix:- 0
		  (string-base:index->y (inferior-window inferior)
					(fix:- start start-line))))
	  (set-line-inferiors!
	   window
	   (cons inferior (fill-bottom window (inferior-y-end inferior) end))
	   start)))))
  (everything-changed! window maybe-recenter!))

(define (%window-redraw! window y)
  (with-instance-variables buffer-window window (y)
    (redraw-screen! window
		    (if (not y)
			(%window-y-center window)
			(begin
			  (if (or (fix:< y 0)
				  (not (fix:< y y-size)))
			      (error "Attempt to scroll point off window" y))
			  y))))
  (everything-changed! window
    (lambda (w)
      (error "%WINDOW-REDRAW! left point offscreen -- get a wizard" w))))

(define (redraw-screen! window y)
  (with-instance-variables buffer-window window (y)
    (let ((group (mark-group point))
	  (index (mark-index point)))
      (let ((start (line-start-index group index)))
	(let ((inferior
	       (make-line-inferior window start (line-end-index group index))))
	  (set-inferior-start!
	   inferior
	   0
	   (fix:- y
		  (string-base:index->y (inferior-window inferior)
					(fix:- index start))))
	  (fill-top! window (list inferior) start true))))))

(define (everything-changed! window if-not-visible)
  (with-instance-variables buffer-window window (if-not-visible)
    (no-outstanding-changes! window)
    (line-inferiors-changed! window)
    (blank-inferior-changed! window)
    (start-mark-changed! window)
    (end-mark-changed! window)
    (update-cursor! window if-not-visible)))

(define (maybe-marks-changed! window inferiors y-end)
  (with-instance-variables buffer-window window (inferiors y-end)
    (no-outstanding-changes! window)
    (if (and (eq? inferiors line-inferiors)
	     (fix:negative? (inferior-y-start (car inferiors))))
	(start-mark-changed! window))
    (if (and (null? (cdr inferiors))
	     (fix:> y-end y-size))
	(end-mark-changed! window))
    (update-cursor! window maybe-recenter!)))

(define (no-outstanding-changes! window)
  (with-instance-variables buffer-window window ()
    (destroy-mark! start-changes-mark)
    (set! start-changes-mark false)
    (destroy-mark! end-changes-mark)
    (set! end-changes-mark false)
    (destroy-mark! start-clip-mark)
    (set! start-clip-mark false)
    (destroy-mark! end-clip-mark)
    (set! end-clip-mark false)
    (set! force-redraw? false)
    unspecific))

(define (start-mark-changed! window)
  (with-instance-variables buffer-window window ()
    (destroy-mark! start-mark)
    (set! start-mark
	  (%make-permanent-mark
	   (buffer-group buffer)
	   (fix:+ (mark-index start-line-mark)
		  (let ((inferior (first-line-inferior window)))
		    (string-base:coordinates->index
		     (inferior-window inferior)
		     0
		     (fix:- 0 (inferior-y-start inferior)))))
	   false))
    (window-modeline-event! superior 'START-MARK-CHANGED!)))

(define (end-mark-changed! window)
  (with-instance-variables buffer-window window ()
    (destroy-mark! end-mark)
    (set! end-mark
	  (let ((group (buffer-group buffer)))
	    (%make-permanent-mark
	     group
	     (fix:+ (line-start-index group (mark-index end-line-mark))
		    (string-base:coordinates->index
		     (inferior-window last-line-inferior)
		     (fix:-1+ x-size)
		     (fix:-1+
		      (fix:- (min y-size (inferior-y-end last-line-inferior))
			     (inferior-y-start last-line-inferior)))))
	     true)))
    (window-modeline-event! superior 'END-MARK-CHANGED!)))

(define (destroy-mark! mark)
  (if mark
      (mark-temporary! mark)))

(define-integrable (%window-start-index window)
  (with-instance-variables buffer-window window ()
    (mark-index start-mark)))

(define-integrable (%window-end-index window)
  (with-instance-variables buffer-window window ()
    (mark-index end-mark)))

(define-integrable (%window-mark-visible? window mark)
  (with-instance-variables buffer-window window (mark)
    (and (mark<= start-mark mark)
	 (mark<= mark end-mark))))

(define (%window-y-center window)
  (with-instance-variables buffer-window window ()
    (let ((result
	   (integer-round
	    (* y-size
	       (inexact->exact (round (ref-variable cursor-centering-point))))
	    100)))
      (cond ((fix:< result 0) 0)
	    ((fix:< result y-size) result)
	    (else (fix:-1+ y-size))))))

(define-variable cursor-centering-point
  "The distance from the top of the window at which to center the point.
This number is a percentage, where 0 is the window's top and 100 the bottom."
  50)