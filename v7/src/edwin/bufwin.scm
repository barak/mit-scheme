;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/bufwin.scm,v 1.291 1991/04/01 10:06:30 cph Exp $
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

;;;; Buffer Windows: Base

(declare (usual-integrations))

;;; The following instance variables contain permanent marks, which
;;; must be copied if they are passed to someone outside the buffer
;;; window abstraction, because they are modified by side-effect.
;;;
;;; current-start-mark
;;; current-end-mark
;;; start-mark
;;; start-line-mark
;;; start-changes-mark
;;; end-changes-mark
;;; start-clip-mark
;;; end-clip-mark

(define-class buffer-window vanilla-window
  (
   ;; The buffer being displayed in this window.
   buffer

   ;; Caches for the values of buffer-local variables that are needed
   ;; for redisplay.
   truncate-lines?
   tab-width

   ;; The point marker in this window.
   point

   ;; This is the inferior window (of class CURSOR-WINDOW) that
   ;; displays the cursor for this window.
   cursor-inferior

   ;; This is the inferior window (of class BLANK-WINDOW) that keeps
   ;; the bottom of the window clear when there is no text in it.
   ;; This is only used when the end of the buffer is visible in the
   ;; window.  When not in use, it is moved offscreen so the window
   ;; clipping will prevent it from being updated.
   blank-inferior

   ;; The topmost and bottommost OUTLINE structures for this window,
   ;; respectively.  If only one line is shown, these are EQ?.
   start-outline
   end-outline

   ;; A previously allocated OUTLINE structure that is available for
   ;; reallocation.  Any other free OUTLINE structures are chained to
   ;; this one through its NEXT field.
   free-outline

   ;; A permanent right-inserting mark at the beginning of the text
   ;; line modelled by START-OUTLINE.
   current-start-mark

   ;; A permanent left-inserting mark at the end of the text line
   ;; modelled by END-OUTLINE.
   current-end-mark

   ;; The Y position, relative to the window, of the top edge of
   ;; START-OUTLINE.  A non-positive number.
   current-start-y

   ;; The Y position, relative to the window, of the bottom edge of
   ;; END-OUTLINE.  A positive number.
   current-end-y

   ;; A previously allocated O3 structure that is available for
   ;; reallocation.  Any other free O3 structures are chained to this
   ;; one through its OUTLINE field.
   free-o3

   ;; This is normally #F.  However, when the normal display of the
   ;; buffer is overridden by a one-line message, as is commonly done
   ;; for the typein window, this variable contains the message
   ;; string.
   override-string

   ;; This permanent mark is the smallest that is visible in the
   ;; window.  If the window's start is not known, this is #F.
   start-mark

   ;; This permanent mark is at the beginning of the line containing
   ;; START-MARK.  It is #F if START-MARK is.  Note that this is the
   ;; same as CURRENT-START-MARK at the end of a display update, and
   ;; is changed due to point motion and scrolling.
   start-line-mark

   ;; This is the Y coordinate of START-LINE-MARK.  It is undefined if
   ;; START-LINE-MARK is #F, otherwise it is guaranteed to be
   ;; non-positive.
   start-line-y

   ;; This contains the daemon that is invoked when insertions or
   ;; deletions are performed on the buffer.
   changes-daemon

   ;; These variables delimit the region of the buffer that has been
   ;; affected by insertions or deletions since the last display
   ;; update.  If no changes have occurred, they are #F.
   start-changes-mark
   end-changes-mark

   ;; This contains the daemon that is invoked when the buffer's
   ;; display clipping is changed.
   clip-daemon

   ;; These variables delimit the region of the buffer that has been
   ;; unaffected by clipping since the last display update.  If the
   ;; clipping has not changed since then, they are #F.
   start-clip-mark
   end-clip-mark

   ;; If true, this flag indicates that point has moved since the last
   ;; time that START-LINE-MARK was set.
   point-moved?

   ;; If true, this flag indicates that the window should be entirely
   ;; redrawn at the next display update.
   force-redraw?

   ;; These variables record where the last display update drew its
   ;; output.  SAVED-SCREEN is the screen on which it occurred.
   ;; SAVED-X-START and SAVED-Y-START is the position, in the screen's
   ;; coordinates, at which the window was located.  SAVED-XL,
   ;; SAVED-XU, SAVED-YL, and SAVED-YU (window's coordinates) delimit
   ;; the rectangular portion of the window that was drawn.
   saved-screen
   saved-x-start
   saved-y-start
   saved-xl
   saved-xu
   saved-yl
   saved-yu

   ;; This variable, if not #F, is a procedure that is called at
   ;; interesting times to generate a debugging trace.
   debug-trace))

;;;; Instance Variable Accessors

(define-integrable (%window-buffer window)
  (with-instance-variables buffer-window window () buffer))

(define-integrable (%window-group window)
  (buffer-group (%window-buffer window)))

(define-integrable (%set-window-buffer! window buffer*)
  (with-instance-variables buffer-window window (buffer*)
    (set! buffer buffer*)))

(define-integrable (%window-truncate-lines? window)
  (with-instance-variables buffer-window window () truncate-lines?))

(define-integrable (%set-window-truncate-lines?! window truncate-lines?*)
  (with-instance-variables buffer-window window (truncate-lines?*)
    (set! truncate-lines? truncate-lines?*)))

(define-integrable (%window-tab-width window)
  (with-instance-variables buffer-window window () tab-width))

(define-integrable (%set-window-tab-width! window tab-width*)
  (with-instance-variables buffer-window window (tab-width*)
    (set! tab-width tab-width*)))

(define-integrable (%window-point window)
  (with-instance-variables buffer-window window () point))

(define-integrable (%window-point-index window)
  (mark-index-integrable (%window-point window)))

(define-integrable (%set-window-point! window point*)
  (with-instance-variables buffer-window window (point*)
    (set! point point*)))

(define-integrable (%set-window-point-index! window index)
  (%set-window-point! window
		      (%make-permanent-mark (%window-group window)
					    index
					    true)))

(define-integrable (%window-cursor-inferior window)
  (with-instance-variables buffer-window window () cursor-inferior))

(define-integrable (%set-window-cursor-inferior! window inferior)
  (with-instance-variables buffer-window window (inferior)
    (set! cursor-inferior inferior)))

(define-integrable (%window-blank-inferior window)
  (with-instance-variables buffer-window window () blank-inferior))

(define-integrable (%set-window-blank-inferior! window inferior)
  (with-instance-variables buffer-window window (inferior)
    (set! blank-inferior inferior)))

(define-integrable (%window-start-outline window)
  (with-instance-variables buffer-window window () start-outline))

(define-integrable (%set-window-start-outline! window outline)
  (with-instance-variables buffer-window window (outline)
    (set! start-outline outline)))

(define-integrable (%window-end-outline window)
  (with-instance-variables buffer-window window () end-outline))

(define-integrable (%set-window-end-outline! window outline)
  (with-instance-variables buffer-window window (outline)
    (set! end-outline outline)))

(define-integrable (%window-free-outline window)
  (with-instance-variables buffer-window window () free-outline))

(define-integrable (%set-window-free-outline! window outline)
  (with-instance-variables buffer-window window (outline)
    (set! free-outline outline)))

(define-integrable (%window-current-start-mark window)
  (with-instance-variables buffer-window window () current-start-mark))

(define-integrable (%window-current-start-index window)
  (mark-index-integrable (%window-current-start-mark window)))

(define-integrable (%set-window-current-start-mark! window mark)
  (with-instance-variables buffer-window window (mark)
    (set! current-start-mark mark)))

(define-integrable (%window-current-end-mark window)
  (with-instance-variables buffer-window window () current-end-mark))

(define-integrable (%window-current-end-index window)
  (mark-index-integrable (%window-current-end-mark window)))

(define-integrable (%set-window-current-end-mark! window mark)
  (with-instance-variables buffer-window window (mark)
    (set! current-end-mark mark)))

(define-integrable (%window-current-start-y window)
  (with-instance-variables buffer-window window () current-start-y))

(define-integrable (%set-window-current-start-y! window y)
  (with-instance-variables buffer-window window (y)
    (set! current-start-y y)))

(define-integrable (%window-current-end-y window)
  (with-instance-variables buffer-window window () current-end-y))

(define-integrable (%set-window-current-end-y! window y)
  (with-instance-variables buffer-window window (y)
    (set! current-end-y y)))

(define-integrable (%window-free-o3 window)
  (with-instance-variables buffer-window window () free-o3))

(define-integrable (%set-window-free-o3! window o3)
  (with-instance-variables buffer-window window (o3)
    (set! free-o3 o3)))

(define-integrable (%window-override-string window)
  (with-instance-variables buffer-window window () override-string))

(define-integrable (%set-window-override-string! window string)
  (with-instance-variables buffer-window window (string)
    (set! override-string string)))

(define-integrable (%window-start-mark window)
  (with-instance-variables buffer-window window () start-mark))

(define-integrable (%window-start-index window)
  (mark-index-integrable (%window-start-mark window)))

(define-integrable (%set-window-start-mark! window mark)
  (with-instance-variables buffer-window window (mark)
    (set! start-mark mark)))

(define-integrable (%window-start-line-mark window)
  (with-instance-variables buffer-window window () start-line-mark))

(define-integrable (%window-start-line-index window)
  (mark-index-integrable (%window-start-line-mark window)))

(define-integrable (%set-window-start-line-mark! window mark)
  (with-instance-variables buffer-window window (mark)
    (set! start-line-mark mark)))

(define-integrable (%window-start-line-y window)
  (with-instance-variables buffer-window window () start-line-y))

(define-integrable (%set-window-start-line-y! window y)
  (with-instance-variables buffer-window window (y)
    (set! start-line-y y)))

(define-integrable (%window-changes-daemon window)
  (with-instance-variables buffer-window window () changes-daemon))

(define-integrable (%set-window-changes-daemon! window daemon)
  (with-instance-variables buffer-window window (daemon)
    (set! changes-daemon daemon)))

(define-integrable (%window-start-changes-mark window)
  (with-instance-variables buffer-window window () start-changes-mark))

(define-integrable (%window-start-changes-index window)
  (mark-index-integrable (%window-start-changes-mark window)))

(define-integrable (%set-window-start-changes-mark! window mark)
  (with-instance-variables buffer-window window (mark)
    (set! start-changes-mark mark)))

(define-integrable (%window-end-changes-mark window)
  (with-instance-variables buffer-window window () end-changes-mark))

(define-integrable (%window-end-changes-index window)
  (mark-index-integrable (%window-end-changes-mark window)))

(define-integrable (%set-window-end-changes-mark! window mark)
  (with-instance-variables buffer-window window (mark)
    (set! end-changes-mark mark)))

(define-integrable (%window-clip-daemon window)
  (with-instance-variables buffer-window window () clip-daemon))

(define-integrable (%set-window-clip-daemon! window daemon)
  (with-instance-variables buffer-window window (daemon)
    (set! clip-daemon daemon)))

(define-integrable (%window-start-clip-mark window)
  (with-instance-variables buffer-window window () start-clip-mark))

(define-integrable (%window-start-clip-index window)
  (mark-index-integrable (%window-start-clip-mark window)))

(define-integrable (%set-window-start-clip-mark! window mark)
  (with-instance-variables buffer-window window (mark)
    (set! start-clip-mark mark)))

(define-integrable (%window-end-clip-mark window)
  (with-instance-variables buffer-window window () end-clip-mark))

(define-integrable (%window-end-clip-index window)
  (mark-index-integrable (%window-end-clip-mark window)))

(define-integrable (%set-window-end-clip-mark! window mark)
  (with-instance-variables buffer-window window (mark)
    (set! end-clip-mark mark)))

(define-integrable (%window-point-moved? window)
  (with-instance-variables buffer-window window () point-moved?))

(define-integrable (%set-window-point-moved?! window point-moved?*)
  (with-instance-variables buffer-window window (point-moved?*)
    (set! point-moved? point-moved?*)))

(define-integrable (%window-force-redraw? window)
  (with-instance-variables buffer-window window () force-redraw?))

(define-integrable (%set-window-force-redraw?! window force-redraw?*)
  (with-instance-variables buffer-window window (force-redraw?*)
    (set! force-redraw? force-redraw?*)))

(define-integrable (%window-saved-screen window)
  (with-instance-variables buffer-window window () saved-screen))

(define-integrable (%set-window-saved-screen! window screen)
  (with-instance-variables buffer-window window (screen)
    (set! saved-screen screen)))

(define-integrable (%window-saved-x-start window)
  (with-instance-variables buffer-window window () saved-x-start))

(define-integrable (%set-window-saved-x-start! window x-start)
  (with-instance-variables buffer-window window (x-start)
    (set! saved-x-start x-start)))

(define-integrable (%window-saved-y-start window)
  (with-instance-variables buffer-window window () saved-y-start))

(define-integrable (%set-window-saved-y-start! window y-start)
  (with-instance-variables buffer-window window (y-start)
    (set! saved-y-start y-start)))

(define-integrable (%window-saved-xl window)
  (with-instance-variables buffer-window window () saved-xl))

(define-integrable (%set-window-saved-xl! window xl)
  (with-instance-variables buffer-window window (xl)
    (set! saved-xl xl)))

(define-integrable (%window-saved-xu window)
  (with-instance-variables buffer-window window () saved-xu))

(define-integrable (%set-window-saved-xu! window xu)
  (with-instance-variables buffer-window window (xu)
    (set! saved-xu xu)))

(define-integrable (%window-saved-yl window)
  (with-instance-variables buffer-window window () saved-yl))

(define-integrable (%set-window-saved-yl! window yl)
  (with-instance-variables buffer-window window (yl)
    (set! saved-yl yl)))

(define-integrable (%window-saved-yu window)
  (with-instance-variables buffer-window window () saved-yu))

(define-integrable (%set-window-saved-yu! window yu)
  (with-instance-variables buffer-window window (yu)
    (set! saved-yu yu)))

(define-integrable (%window-debug-trace window)
  (with-instance-variables buffer-window window () debug-trace))

(define-integrable (%set-window-debug-trace! window procedure)
  (with-instance-variables buffer-window window (procedure)
    (set! debug-trace procedure)))

;;;; Outlines

(define-structure (outline (constructor %make-outline))
  ;; The number of characters in the text line.  This is exclusive of
  ;; the newlines at the line's beginning and end, if any.
  index-length

  ;; The number of screen lines that are occupied by this text line.
  y-size

  ;; A pointer to the previous outline structure, the one representing
  ;; the text line that appears directly above this line.
  previous

  ;; A pointer to the next outline structure, the one representing the
  ;; text line that appears directly below this line.
  next)

(define (make-outline window index-length y-size previous next)
  (let ((outline
	 (let ((outline (%window-free-outline window)))
	   (if (%window-free-outline window)
	       (begin
		 (let ((free (outline-next outline)))
		   (if free (set-outline-previous! free false))
		   (%set-window-free-outline! window free))
		 (set-outline-index-length! outline index-length)
		 (set-outline-y-size! outline y-size)
		 (set-outline-previous! outline previous)
		 (set-outline-next! outline next)
		 outline)
	       (%make-outline index-length y-size previous next)))))
    (if previous (set-outline-next! previous outline))
    (if next (set-outline-previous! next outline))
    outline))

(define (deallocate-outlines! window start-outline end-outline)
  (let ((free-outline (%window-free-outline window)))
    (if (outline-next end-outline)
	(set-outline-previous! (outline-next end-outline) false))
    (set-outline-next! end-outline free-outline)
    (if free-outline
	(set-outline-previous! free-outline end-outline)))
  (if (outline-previous start-outline)
      (set-outline-next! (outline-previous start-outline) false))
  (set-outline-previous! start-outline false)
  (%set-window-free-outline! window start-outline))

(define-integrable (outline-last outline)
  (do ((outline outline (outline-next outline)))
      ((not (outline-next outline)) outline)))

(define-integrable (outline-end-y outline start-y)
  (do ((outline outline (outline-next outline))
       (y start-y (fix:+ y (outline-y-size outline))))
      ((not outline) y)))

(define-integrable (outline-start-y outline end-y)
  (do ((outline outline (outline-previous outline))
       (y end-y (fix:- y (outline-y-size outline))))
      ((not outline) y)))

(define-structure (o3
		   (constructor %make-o3)
		   (print-procedure
		    (unparser/standard-method 'O3
		      (lambda (state o3)
			(unparse-string state "index: ")
			(unparse-object state (o3-index o3))
			(unparse-string state " y: ")
			(unparse-object state (o3-y o3))
			(unparse-string state " ")
			(unparse-object state (o3-outline o3))))))
  outline
  index
  y)

(define (make-o3 window outline index y)
  (let ((o3 (%window-free-o3 window)))
    (if o3
	(begin
	  (%set-window-free-o3! window (o3-outline o3))
	  (set-o3-outline! o3 outline)
	  (set-o3-index! o3 index)
	  (set-o3-y! o3 y)
	  o3)
	(%make-o3 outline index y))))

(define (deallocate-o3! window o3)
  (set-o3-outline! o3 (%window-free-o3 window))
  (%set-window-free-o3! window o3))

;;;; Narrowing

(define-integrable (%window-group-start-mark window)
  (group-display-start (%window-group window)))

(define-integrable (%window-group-end-mark window)
  (group-display-end (%window-group window)))

(define-integrable (%window-group-start-index window)
  (group-position->index-integrable
   (%window-group window)
   (mark-position (group-display-start (%window-group window)))))

(define-integrable (%window-group-end-index window)
  (group-position->index-integrable
   (%window-group window)
   (mark-position (group-display-end (%window-group window)))))

(define-integrable (%window-group-start-index? window index)
  (fix:<= index (%window-group-start-index window)))

(define-integrable (%window-group-end-index? window index)
  (fix:>= index (%window-group-end-index window)))

(define-integrable (%window-line-start-index window index)
  (let ((start (%window-group-start-index window)))
    (or (%find-previous-newline (%window-group window) index start)
	start)))

(define-integrable (%window-line-end-index window index)
  (let ((end (%window-group-end-index window)))
    (or (%find-next-newline (%window-group window) index end)
	end)))

(define (%window-line-start-index? window index)
  (or (%window-group-start-index? window index)
      (char=? (string-ref (group-text (%window-group window))
			  (fix:-1+ (group-index->position-integrable
				    (%window-group window)
				    index
				    false)))
	      #\newline)))

(define (%window-line-end-index? window index)
  (or (%window-group-end-index? window index)
      (char=? (string-ref (group-text (%window-group window))
			  (group-index->position-integrable
			   (%window-group window)
			   index
			   true))
	      #\newline)))

(define (clip-mark-to-display window mark)
  (if (not (mark? mark))
      (error:wrong-type-argument mark "mark" 'CLIP-MARK-TO-DISPLAY))
  (if (and (%window-point window)
	   (not (mark~ (%window-point window) mark)))
      (error:bad-range-argument mark 'CLIP-MARK-TO-DISPLAY))
  (cond ((group-display-start-index? (mark-group mark) (mark-index mark))
	 (group-display-start (mark-group mark)))
	((group-display-end-index? (mark-group mark) (mark-index mark))
	 (group-display-end (mark-group mark)))
	(else
	 mark)))

;;;; Utilities

(define-integrable (%window-extract-string window start end)
  (group-extract-string (%window-group window) start end))

(define-integrable (%window-modeline-event! window type)
  (window-modeline-event! (window-superior window) type))

(define-integrable (fix:max x y)
  (if (fix:> x y) x y))

(define-integrable (fix:min x y)
  (if (fix:< x y) x y))

;;;; Standard Methods

(define-method buffer-window (:initialize! window window*)
  (usual=> window :initialize! window*)
  (%reset-window-structures! window)
  (%clear-window-buffer-state! window))

(define-method buffer-window (:kill! window)
  (without-interrupts (lambda () (%unset-window-buffer! window)))
  (usual=> window :kill!))

(define-method buffer-window (:salvage! window)
  (without-interrupts
   (lambda ()
     (%set-window-point-index! window (%window-group-start-index window))
     (%set-window-point-moved?! window 'SINCE-START-SET)
     (%reset-window-structures! window)
     (buffer-window/redraw! window))))

(define-method buffer-window (:set-size! window x y)
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window 'set-size! x y))
  (buffer-window/redraw! window)
  (%release-window-outlines! window)
  (set-window-size! window x y)
  (%set-window-point-moved?! window 'SINCE-START-SET))

(define-method buffer-window (:set-x-size! window x)
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window 'set-x-size! x))
  (buffer-window/redraw! window)
  (set-window-x-size! window x)
  (%set-window-point-moved?! window 'SINCE-START-SET))

(define-method buffer-window (:set-y-size! window y)
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window 'set-y-size! y))
  (buffer-window/redraw! window)
  (%release-window-outlines! window)
  (set-window-y-size! window y)
  (%set-window-point-moved?! window 'SINCE-START-SET))

;;;; Update

(define (buffer-window:update-display! window screen x-start y-start
				       xl xu yl yu display-style)
  ;; Assumes that interrupts are disabled.
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window ':update-display!
				    screen x-start y-start xl xu yl yu
				    display-style))
  (%set-window-saved-screen! window screen)
  (%set-window-saved-x-start! window x-start)
  (%set-window-saved-y-start! window y-start)
  (%set-window-saved-xl! window xl)
  (%set-window-saved-xu! window xu)
  (%set-window-saved-yl! window yl)
  (%set-window-saved-yu! window yu)
  (update-buffer-window! window screen x-start y-start xl xu yl yu
			 display-style))

(define-method buffer-window :update-display!
  buffer-window:update-display!)

(define (buffer-window/direct-update! window display-style)
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window 'direct-update!
				    display-style))
  (and (%window-saved-screen window)
       (with-screen-in-update (%window-saved-screen window) display-style
	 (lambda ()
	   (let ((finished?
		  (update-buffer-window! window
					 (%window-saved-screen window)
					 (%window-saved-x-start window)
					 (%window-saved-y-start window)
					 (%window-saved-xl window)
					 (%window-saved-xu window)
					 (%window-saved-yl window)
					 (%window-saved-yu window)
					 display-style)))
	     (if finished?
		 (set-car! (window-redisplay-flags window) false))
	     finished?)))))

(define (update-buffer-window! window screen x-start y-start xl xu yl yu
			       display-style)
  (if (%window-override-string window)
      (update-override-string! window screen x-start y-start xl xu yl yu)
      (update-outlines! window))
  (and (update-inferior! (%window-blank-inferior window)
			 screen x-start y-start xl xu yl yu display-style
			 blank-window:update-display!)
       (update-inferior! (%window-cursor-inferior window)
			 screen x-start y-start xl xu yl yu display-style
			 cursor-window:update-display!)))

(define (buffer-window/redraw! window)
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window 'force-redraw!))
  (without-interrupts
   (lambda ()
     (%set-window-force-redraw?! window true)
     (%clear-window-incremental-redisplay-state! window)
     (window-needs-redisplay! window))))

(define (buffer-window/cursor-enable! window)
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window 'cursor-enable!))
  (=> (inferior-window (%window-cursor-inferior window)) :enable!))

(define (buffer-window/cursor-disable! window)
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window 'cursor-disable!))
  (=> (inferior-window (%window-cursor-inferior window)) :disable!))

;;;; Window State

(define (%reset-window-structures! window)
  (set-window-inferiors! window '())
  (%set-window-cursor-inferior! window (make-inferior window cursor-window))
  (%set-window-blank-inferior! window (make-inferior window blank-window))
  (%release-window-outlines! window)
  (%set-window-free-o3! window false)
  (%set-window-override-string! window false)
  (%set-window-changes-daemon! window (make-changes-daemon window))
  (%set-window-clip-daemon! window (make-clip-daemon window))
  (%set-window-debug-trace! window false))

(define (%release-window-outlines! window)
  (%set-window-start-outline! window false)
  (%set-window-end-outline! window false)
  (%set-window-free-outline! window false))

(define (%clear-window-buffer-state! window)
  (%set-window-buffer! window false)
  (%set-window-point! window false)
  (if (%window-start-line-mark window)
      (clear-start-mark! window))
  (%set-window-point-moved?! window false)
  (%clear-window-incremental-redisplay-state! window))

(define (%clear-window-incremental-redisplay-state! window)
  (if (%window-start-outline window)
      (begin
	(deallocate-outlines! window
			      (%window-start-outline window)
			      (%window-end-outline window))
	(%set-window-start-outline! window false)
	(%set-window-end-outline! window false)))
  (if (%window-current-start-mark window)
      (begin
	(mark-temporary! (%window-current-start-mark window))
	(%set-window-current-start-mark! window false)
	(mark-temporary! (%window-current-end-mark window))
	(%set-window-current-end-mark! window false)))
  (%set-window-saved-screen! window false)
  (%clear-window-outstanding-changes! window))

(define-integrable (%clear-window-outstanding-changes! window)
  (if (%window-start-changes-mark window)
      (begin
	(mark-temporary! (%window-start-changes-mark window))
	(%set-window-start-changes-mark! window false)
	(mark-temporary! (%window-end-changes-mark window))
	(%set-window-end-changes-mark! window false)))
  (if (%window-start-clip-mark window)
      (begin
	(mark-temporary! (%window-start-clip-mark window))
	(%set-window-start-clip-mark! window false)
	(mark-temporary! (%window-end-clip-mark window))
	(%set-window-end-clip-mark! window false))))

(define (%recache-window-buffer-local-variables! window)
  (let ((buffer (%window-buffer window)))
    (%set-window-truncate-lines?!
     window
     (or (variable-local-value buffer (ref-variable-object truncate-lines))
	 (and (variable-local-value
	       buffer
	       (ref-variable-object truncate-partial-width-windows))
	      (window-has-horizontal-neighbor? (window-superior window)))))
    (%set-window-tab-width!
     window
     (variable-local-value buffer (ref-variable-object tab-width)))))

;;;; Buffer and Point

(define-integrable (buffer-window/buffer window)
  (%window-buffer window))

(define (buffer-window/set-buffer! window new-buffer)
  ;; Interrupts must be disabled when this is called.
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window 'set-buffer! new-buffer))
  (if (not (buffer? new-buffer))
      (error:wrong-type-argument new-buffer "buffer" 'SET-WINDOW-BUFFER!))
  (if (%window-buffer window)
      (%unset-window-buffer! window))
  (%set-window-buffer! window new-buffer)
  (let ((group (%window-group window))
	(changes-daemon (%window-changes-daemon window)))
    (add-group-delete-daemon! group changes-daemon)
    (add-group-insert-daemon! group changes-daemon)
    (add-group-clip-daemon! group (%window-clip-daemon window))
    (%set-window-point-index! window (mark-index (group-point group))))
  (if (buffer-display-start new-buffer)
      (set-new-coordinates! window
			    (mark-index (buffer-display-start new-buffer))
			    0
			    false))
  (buffer-window/redraw! window))

(define (%unset-window-buffer! window)
  ;; Interrupts must be disabled when this is called.
  (let ((buffer (%window-buffer window)))
    (if (%window-debug-trace window)
	((%window-debug-trace window) 'window window 'unset-buffer! buffer))
    (set-buffer-display-start!
     buffer
     (mark-permanent! (buffer-window/start-mark window)))
    (%set-buffer-point! buffer (buffer-window/point window)))
  (let ((group (%window-group window))
	(changes-daemon (%window-changes-daemon window)))
    (remove-group-delete-daemon! group changes-daemon)
    (remove-group-insert-daemon! group changes-daemon)
    (remove-group-clip-daemon! group (%window-clip-daemon window)))
  (%clear-window-buffer-state! window))

(define-integrable (buffer-window/point window)
  (%window-point window))

(define (buffer-window/set-point! window mark)
  (let ((mark (clip-mark-to-display window mark)))
    (if (%window-debug-trace window)
	((%window-debug-trace window) 'window window 'set-point! mark))
    (without-interrupts
     (lambda ()
       (%set-window-point-index! window (mark-index mark))
       (%set-window-point-moved?! window 'SINCE-START-SET)
       (%set-buffer-point! (%window-buffer window) mark)
       (window-needs-redisplay! window)))))

;;;; Start Mark

(define (buffer-window/start-mark window)
  (guarantee-start-mark! window)
  (mark-temporary-copy (%window-start-mark window)))

(define (buffer-window/set-start-mark! window mark force?)
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window 'set-start-mark! mark))
  (set-new-coordinates! window
			(mark-index (clip-mark-to-display window mark))
			0
			(and force? (buffer-window/y-center window))))

(define (buffer-window/scroll-y-relative! window y-delta)
  (if (not (fix:= y-delta 0))
      (begin
	(if (%window-debug-trace window)
	    ((%window-debug-trace window) 'window window 'scroll-y-relative!
					  y-delta))
	(guarantee-start-mark! window)
	;; if (> Y-DELTA 0) and line inferiors valid, use them.
	(set-new-coordinates! window
			      (%window-start-line-index window)
			      (fix:- (%window-start-line-y window) y-delta)
			      (if (fix:> y-delta 0)
				  0
				  (fix:- (window-y-size window) 1))))))

(define (set-new-coordinates! window index y point-y)
  (with-values (lambda () (predict-start-line window index y))
    (lambda (start y-start)
      (cond ((predict-index-visible? window start y-start
				     (%window-point-index window))
	     (without-interrupts
	      (lambda ()
		(set-start-mark! window start y-start))))
	    (point-y
	     (without-interrupts
	      (lambda ()
		(%set-window-point-index!
		 window
		 (or (predict-index window start y-start 0 point-y)
		     (%window-group-end-index window)))
		(set-start-mark! window start y-start))))))))

(define (buffer-window/scroll-y-absolute! window y-point)
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window 'scroll-y-absolute!
				    y-point))
  (if (not (and (fix:<= 0 y-point)
		(fix:< y-point (window-y-size window))))
      (error:bad-range-argument y-point 'WINDOW-SCROLL-Y-ABSOLUTE!))
  (with-values
      (lambda ()
	(predict-start-line window (%window-point-index window) y-point))
    (lambda (start y-start)
      (without-interrupts
       (lambda ()
	 (set-start-mark! window start y-start))))))

(define (set-start-mark! window start-line y-start)
  (if (fix:= y-start 0)
      (if (%window-start-line-mark window)
	  (begin
	    (set-mark-index-integrable! (%window-start-line-mark window)
					start-line)
	    (if (not (eq? (%window-start-line-mark window)
			  (%window-start-mark window)))
		(begin
		  (mark-temporary! (%window-start-mark window))
		  (%set-window-start-mark! window
					   (%window-start-line-mark window)))))
	  (let ((mark
		 (%make-permanent-mark (%window-group window)
				       start-line
				       false)))
	    (%set-window-start-line-mark! window mark)
	    (%set-window-start-mark! window mark)))
      (let ((start (predict-start-index window start-line y-start)))
	(if (%window-start-line-mark window)
	    (begin
	      (set-mark-index-integrable! (%window-start-line-mark window)
					  start-line)
	      (if (eq? (%window-start-line-mark window)
		       (%window-start-mark window))
		  (%set-window-start-mark!
		   window
		   (%make-permanent-mark (%window-group window) start false))
		  (set-mark-index-integrable! (%window-start-mark window)
					      start)))
	    (let ((group (%window-group window)))
	      (%set-window-start-line-mark!
	       window
	       (%make-permanent-mark group start-line false))
	      (%set-window-start-mark!
	       window
	       (%make-permanent-mark group start false))))))
  (%set-window-start-line-y! window y-start)
  (if (eq? (%window-point-moved? window) 'SINCE-START-SET)
      (%set-window-point-moved?! window true))
  (window-needs-redisplay! window))

(define-integrable (clear-start-mark! window)
  (mark-temporary! (%window-start-line-mark window))
  (%set-window-start-line-mark! window false)
  (mark-temporary! (%window-start-mark window))
  (%set-window-start-mark! window false)
  (%set-window-start-line-y! window 0))

(define (guarantee-start-mark! window)
  (without-interrupts (lambda () (%guarantee-start-mark! window))))

(define (%guarantee-start-mark! window)
  (let ((index-at!
	 (lambda (index y)
	   (with-values (lambda () (predict-start-line window index y))
	     (lambda (start y-start)
	       (set-start-mark! window start y-start))))))
    (if (not (%window-start-line-mark window))
	(index-at! (%window-point-index window)
		   (buffer-window/y-center window))
	(let ((start-line (%window-start-line-index window)))
	  (cond ((not (%window-line-start-index? window start-line))
		 (index-at! (%window-start-index window) 0))
		((eq? (%window-point-moved? window) 'SINCE-START-SET)
		 (let ((point (%window-point-index window)))
		   (if (or (%window-start-clip-mark window)
			   (%window-start-changes-mark window)
			   (not (%window-current-start-mark window))
			   (fix:< point (%window-current-start-index window))
			   (fix:> point (%window-current-end-index window))
			   (fix:< (%window-current-start-y window) 0)
			   (fix:> (%window-current-end-y window)
				  (window-y-size window)))
		       (let ((start-y (%window-start-line-y window))
			     (y-size (window-y-size window))
			     (scroll-step (ref-variable scroll-step)))
			 (if (fix:= 0 scroll-step)
			     (if (not (predict-y-limited window start-line
							 start-y point
							 0 y-size))
				 (index-at! point
					    (buffer-window/y-center window)))
			     (let ((y
				    (predict-y-limited window start-line
						       start-y point
						       (fix:- 0 scroll-step)
						       (fix:+ y-size
							      scroll-step))))
			       (cond ((not y)
				      (index-at!
				       point
				       (buffer-window/y-center window)))
				     ((fix:< y 0)
				      (index-at! point (fix:+ y scroll-step)))
				     ((fix:>= y y-size)
				      (index-at!
				       point
				       (fix:- y scroll-step)))))))))))))))

(define-variable scroll-step
  "*The number of lines to try scrolling a window by when point moves out.
If that fails to bring point back on screen, point is centered instead.
If this is zero, point is always centered after it moves off screen."
  0
  (lambda (scroll-step)
    (and (fix:fixnum? scroll-step)
	 (fix:>= scroll-step 0))))

(define (buffer-window/y-center window)
  (let ((y-size (window-y-size window)))
    (let ((result
	   (round->exact
	    (* y-size (/ (ref-variable cursor-centering-point) 100)))))
      (if (< result y-size)
	  result
	  (- y-size 1)))))

(define-variable cursor-centering-point
  "The distance from the top of the window at which to center the point.
This number is a percentage, where 0 is the window's top and 100 the bottom."
  50
  (lambda (cursor-centering-point)
    (and (real? cursor-centering-point)
	 (<= 0 cursor-centering-point 100))))

;;;; Override Message

(define (buffer-window/override-message window)
  (%window-override-string window))

(define (buffer-window/set-override-message! window message)
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window 'set-override-message!
				    message))
  (without-interrupts
   (lambda ()
     (%set-window-override-string! window message)
     (window-needs-redisplay! window))))

(define (buffer-window/clear-override-message! window)
  (if (%window-override-string window)
      (begin
	(if (%window-debug-trace window)
	    ((%window-debug-trace window) 'window window
					  'clear-override-message!))
	(without-interrupts
	 (lambda ()
	   (%set-window-override-string! window false)
	   (update-blank-inferior! window true)
	   (update-cursor! window)
	   (window-needs-redisplay! window))))))

(define (update-override-string! window screen x-start y-start xl xu yl yu)
  ;; This should probably update like any other string, paying
  ;; attention to TRUNCATE-LINES? and going to multiple lines if
  ;; necessary.  For now we'll force it to be truncated to a single
  ;; line, which is fine as long as the minibuffer is only one line.
  (if (and (fix:= yl 0) (not (fix:= yu 0)))
      (let ((string (%window-override-string window))
	    (xl (fix:+ x-start xl))
	    (xu (fix:+ x-start xu))
	    (results substring-image-results))
	(let ((end (string-length string))
	      (line
	       (screen-get-output-line screen (fix:+ y-start yl) xl xu
				       false)))
	  (substring-image! string 0 end
			    line xl (fix:- xu 1)
			    false 0 results)
	  (if (fix:= (vector-ref results 0) end)
	      (do ((x (vector-ref results 1) (fix:+ x 1)))
		  ((fix:= x xu))
		(string-set! line x #\space))
	      (string-set! line (fix:- xu 1) #\$))
	  (set-inferior-start! (%window-cursor-inferior window)
			       (vector-ref results 1)
			       0))))
  (%update-blank-inferior! window 1 true))

;;;; Update Finalization

(define (set-outlines! window start end)
  (%set-window-start-outline! window (o3-outline start))
  (%set-window-end-outline! window (o3-outline end))
  (if (%window-current-start-mark window)
      (begin
	(set-mark-index-integrable! (%window-current-start-mark window)
				    (o3-index start))
	(set-mark-index-integrable! (%window-current-end-mark window)
				    (o3-index end)))
      (begin
	(%set-window-current-start-mark!
	 window
	 (%make-permanent-mark (%window-group window) (o3-index start) false))
	(%set-window-current-end-mark!
	 window
	 (%make-permanent-mark (%window-group window) (o3-index end) true))))
  (%set-window-current-start-y! window (o3-y start))
  (%set-window-current-end-y! window (o3-y end))
  (deallocate-o3! window start)
  (deallocate-o3! window end)
  (%clear-window-outstanding-changes! window)
  (update-blank-inferior! window true)
  (update-cursor! window)
  (%window-modeline-event! window 'SET-OUTLINES))

(define (update-blank-inferior! window signal?)
  (%update-blank-inferior! window (%window-current-end-y window) signal?))

(define (%update-blank-inferior! window end-y signal?)
  (let ((inferior (%window-blank-inferior window)))
    (if (fix:< end-y (window-y-size window))
	(begin
	  (%set-window-x-size! (inferior-window inferior)
			       (window-x-size window))
	  (%set-window-y-size! (inferior-window inferior)
			       (fix:- (window-y-size window) end-y))
	  (%set-inferior-x-start! inferior 0)
	  (%set-inferior-y-start! inferior end-y)
	  (if signal?
	      (setup-redisplay-flags! (inferior-redisplay-flags inferior))))
	(begin
	  (%set-inferior-x-start! inferior false)
	  (%set-inferior-y-start! inferior false)))))

(define (update-cursor! window)
  (let ((xy (buffer-window/point-coordinates window)))
    (if (not (and (fix:<= 0 (car xy))
		  (fix:< (car xy) (window-x-size window))
		  (fix:<= 0 (cdr xy))
		  (fix:< (cdr xy) (window-y-size window))))
	(error "point not visible at end of redisplay"))
    (set-inferior-position! (%window-cursor-inferior window) xy)))