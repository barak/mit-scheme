;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/bufwin.scm,v 1.277 1989/04/20 08:12:57 cph Exp $
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

;;;; Buffer Windows:  Base

(declare (usual-integrations))

(define-class buffer-window vanilla-window
  (buffer point changes-daemon clip-daemon
	  cursor-inferior blank-inferior
	  line-inferiors last-line-inferior
	  start-line-mark start-mark end-mark end-line-mark
	  start-changes-mark end-changes-mark point-moved?
	  start-clip-mark end-clip-mark
	  saved-screen saved-x-start saved-y-start
	  saved-xl saved-xu saved-yl saved-yu
	  override-inferior))

(define-method buffer-window (:initialize! window window*)
  (usual=> window :initialize! window*)
  (set! cursor-inferior (make-inferior window cursor-window))
  (set! blank-inferior (make-inferior window blank-window))
  (set! changes-daemon (make-changes-daemon window))
  (set! clip-daemon (make-clip-daemon window))
  (set! override-inferior false)
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

(define (set-buffer-window-size! window x y)
  (with-instance-variables buffer-window window (x y)
    (set! saved-screen false)
    (%window-redraw! window
		     (let ((old-y y-size))
		       (usual=> window :set-size! x y)
		       ;; Preserve point y unless it is offscreen now.
		       (or (and old-y
				(let ((y (inferior-y-start cursor-inferior)))
				  (and (< y y-size) y)))
			   (let ((y (buffer-cursor-y buffer)))
			     (and y (< y y-size) y)))))))

(define-method buffer-window :set-size!
  set-buffer-window-size!)

(define-method buffer-window (:set-x-size! window x)
  (set-buffer-window-size! window x y-size))

(define-method buffer-window (:set-y-size! window y)
  (set-buffer-window-size! window x-size y))

;;;; Group Operations

;;; These are identical to the operations of the same name used
;;; elsewhere in the editor, except that they clip at the display clip
;;; limits rather than the text clip limits.

(define-integrable (group-start-index group)
  (mark-index (group-display-start group)))

(define-integrable (group-end-index group)
  (mark-index (group-display-end group)))

(define-integrable (group-start-index? group index)
  (<= index (group-start-index group)))

(define-integrable (group-end-index? group index)
  (>= index (group-end-index group)))

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

(define (%set-window-buffer! window new-buffer)
  (with-instance-variables buffer-window window (new-buffer)
    (if (not (buffer? new-buffer)) (error "Argument not a buffer" new-buffer))
    (delete-window-buffer! window)
    (initial-buffer! window new-buffer)
    (window-modeline-event! superior 'NEW-BUFFER)
    (%window-redraw! window
		     (let ((y (buffer-cursor-y buffer)))
		       (and y (< y y-size) y)))))

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
	(cond ((< point start)
	       (%set-buffer-point! buffer (make-mark group start)))
	      ((> point end)
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

(define-method buffer-window (:salvage! window)
  (%set-buffer-point! buffer
		      (make-mark (buffer-group buffer)
				 (group-start-index (buffer-group buffer))))
  (set! point (buffer-point buffer))
  (window-modeline-event! superior 'SALVAGE)
  (%window-redraw! window false))

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
      (set-line-window-string! override-window message)
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
	  (let ((coordinates (%window-mark->coordinates window point)))
	    (set-inferior-position! cursor-inferior coordinates)
	    (set-buffer-cursor-y! buffer (cdr coordinates)))
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
						     start end))
      inferior)))

(define-integrable (first-line-inferior window)
  (with-instance-variables buffer-window window ()
    (car line-inferiors)))

(define-integrable (line-inferior-length inferiors)
  (1+ (line-window-length (inferior-window (car inferiors)))))

(define-integrable (blank-inferior-changed! window)
  (with-instance-variables buffer-window window ()
    (if (not override-inferior)
	(set-blank-inferior-start! window
				   (inferior-y-end last-line-inferior)))))

(define-integrable (set-blank-inferior-start! window y-end)
  (with-instance-variables buffer-window window (y-end)
    (if (< y-end y-size)
	(begin
	  (set-inferior-size! blank-inferior x-size (- y-size y-end))
	  (set-inferior-start! blank-inferior 0 y-end))
	(set-inferior-start! blank-inferior false false))))

(define-integrable (set-line-inferiors! window inferiors start)
  (with-instance-variables buffer-window window (inferiors start)
    (set! line-inferiors inferiors)
    (set! start-line-mark
	  (%make-permanent-mark (buffer-group buffer) start false))
    unspecific))

(define (line-inferiors-changed! window)
  (with-instance-variables buffer-window window ()
    (define (loop inferiors start)
      (if (null? (cdr inferiors))
	  (begin (set! last-line-inferior (car inferiors))
		 (set! end-line-mark
		       (let ((group (buffer-group buffer)))
			 (%make-permanent-mark group
					       (line-end-index group start)
					       true))))
	  (loop (cdr inferiors)
		(+ start (line-inferior-length inferiors)))))
    (loop line-inferiors (mark-index start-line-mark))
    (if (not override-inferior)
	(set! inferiors (cons* cursor-inferior blank-inferior line-inferiors)))
    unspecific))

(define (y->inferiors window y)
  (with-instance-variables buffer-window window (y)
    (define (loop previous-inferiors inferiors)
      (cond ((< y (inferior-y-start (car inferiors))) previous-inferiors)
	    ((null? (cdr inferiors))
	     (and (< y (inferior-y-end (car inferiors)))
		  inferiors))
	    (else (loop inferiors (cdr inferiors)))))
    (loop false line-inferiors)))

(define (index->inferiors window index)
  (with-instance-variables buffer-window window (index)
    ;; Assumes that (>= INDEX (MARK-INDEX START-LINE-MARK)).
    (define (loop inferiors start)
      (let ((new-start (+ start (line-inferior-length inferiors))))
	(if (< index new-start)
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
		(+ start (line-inferior-length inferiors*)))))
    (loop line-inferiors (mark-index start-line-mark))))

(define (y->inferiors&index window y receiver)
  (with-instance-variables buffer-window window (y receiver)
    ;; This is used for scrolling.
    (define (loop inferiors start previous-inferiors previous-start)
      (cond ((< y (inferior-y-start (car inferiors)))
	     (receiver previous-inferiors previous-start))
	    ((null? (cdr inferiors))
	     (and (< y (inferior-y-end (car inferiors)))
		  (receiver inferiors start)))
	    (else
	     (loop (cdr inferiors) (+ start (line-inferior-length inferiors))
		   inferiors start))))
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
		(let ((new-end (- end (line-inferior-length inferiors))))
		  (if (< new-end index)
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
	(let ((coordinates (%window-mark->coordinates window point)))
	  (set-inferior-position! cursor-inferior coordinates)
	  (set-buffer-cursor-y! buffer (cdr coordinates))
	  (set! point-moved? false)
	  (window-modeline-event! superior 'CURSOR-MOVED))
	(if-not-visible window))))

(define (maybe-recenter! window)
  (with-instance-variables buffer-window window ()
    (let ((threshold (ref-variable cursor-centering-threshold)))
      (if (zero? threshold)
	  (%window-redraw! window (%window-y-center window))
	  (if (< (mark-index point) (mark-index start-mark))
	      (let ((limit
		     (%window-coordinates->index window 0 (- threshold))))
		(if (or (not limit)
			(>= (mark-index point) limit))
		    (%window-scroll-y-relative! window
						(%window-point-y window))
		    (%window-redraw! window (%window-y-center window))))
	      (let ((limit
		     (%window-coordinates->index window
						 0
						 (+ (window-y-size window)
						    threshold))))
		(if (or (not limit)
			(< (mark-index point) limit))
		    (%window-scroll-y-relative!
		     window
		     (- (%window-point-y window) (-1+ (window-y-size window))))
		    (%window-redraw! window (%window-y-center window)))))))))

(define (%window-redraw! window y)
  (with-instance-variables buffer-window window (y)
    (redraw-screen! window
		    (if (not y)
			(%window-y-center window)
			(begin
			  (if (or (< y 0) (>= y y-size))
			      (error "Attempt to scroll point off window" y))
			  y)))
    (everything-changed! window
      (lambda (w)
	(error "%WINDOW-REDRAW! left point offscreen -- get a wizard" w)))))

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
	   (- y
	      (string-base:index->y (inferior-window inferior)
				    (- index start))))
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
	     (negative? (inferior-y-start (car inferiors))))
	(start-mark-changed! window))
    (if (and (null? (cdr inferiors))
	     (> y-end y-size))
	(end-mark-changed! window))
    (update-cursor! window maybe-recenter!)))

(define (no-outstanding-changes! window)
  (with-instance-variables buffer-window window ()
    (set! start-changes-mark false)
    (set! end-changes-mark false)
    (set! start-clip-mark false)
    (set! end-clip-mark false)    unspecific))

(define (start-mark-changed! window)
  (with-instance-variables buffer-window window ()
    (set! start-mark
	  (%make-permanent-mark
	   (buffer-group buffer)
	   (+ (mark-index start-line-mark)
	      (let ((inferior (first-line-inferior window)))
		(string-base:coordinates->index
		 (inferior-window inferior)
		 0
		 (- (inferior-y-start inferior)))))
	   false))
    (window-modeline-event! superior 'START-MARK-CHANGED!)))

(define (end-mark-changed! window)
  (with-instance-variables buffer-window window ()
    (set! end-mark
	  (let ((group (buffer-group buffer)))
	    (%make-permanent-mark
	     group
	     (+ (line-start-index group (mark-index end-line-mark))
		(string-base:coordinates->index
		 (inferior-window last-line-inferior)
		 (-1+ x-size)
		 (-1+ (- (min y-size (inferior-y-end last-line-inferior))
			 (inferior-y-start last-line-inferior)))))
	     true)))
    (window-modeline-event! superior 'END-MARK-CHANGED!)))

(define-integrable (%window-start-mark window)
  (with-instance-variables buffer-window window ()
    start-mark))

(define-integrable (%window-end-mark window)
  (with-instance-variables buffer-window window ()
    end-mark))
(define-integrable (%window-mark-visible? window mark)
  (with-instance-variables buffer-window window (mark)
    (and (mark<= start-mark mark)
	 (mark<= mark end-mark))))

(define (%window-y-center window)
  (with-instance-variables buffer-window window ()
    (let ((qr
	   (integer-divide (* y-size (ref-variable cursor-centering-point))
			   100)))
      (if (< (integer-divide-remainder qr) 50)
	  (integer-divide-quotient qr)
	  (1+ (integer-divide-quotient qr))))))