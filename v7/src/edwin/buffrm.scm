;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/buffrm.scm,v 1.41 1992/03/13 10:52:38 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989-92 Massachusetts Institute of Technology
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

;;;; Buffer Frames

(declare (usual-integrations))

(define-class buffer-frame combination-leaf-window
  (
   ;; The inferior (of type BUFFER-WINDOW) that displays the buffer's
   ;; text.
   text-inferior

   ;; The inferior (of type MODELINE-WINDOW) that displays the
   ;; modeline.  May be #F if this window has no modeline (e.g. a
   ;; typein window).
   modeline-inferior

   ;; The inferior (of type VERTICAL-BORDER-WINDOW) that draws a
   ;; vertical border on the right-hand side of the window when this
   ;; window has a neighbor to its right.
   border-inferior

   ;; A nonnegative integer that is updated when this window is
   ;; selected.  This updating is performed by the editor frame that
   ;; this window is a part of.
   last-select-time
   ))

(define-method buffer-frame (:make-leaf frame)
  (let ((frame* (=> superior :make-inferior buffer-frame)))
    (set-buffer-frame-size! frame* (window-x-size frame) (window-y-size frame))
    (set-window-buffer! frame* (window-buffer frame))
    (initial-modeline! frame* modeline-inferior)
    frame*))

(define-method buffer-frame (:initialize! frame window*)
  (usual=> frame :initialize! window*)
  (set! text-inferior (make-inferior frame buffer-window))
  (set! border-inferior (make-inferior frame vertical-border-window))
  (set! last-select-time 0))

(define-method buffer-frame (:kill! window)
  (remove-buffer-window! (window-buffer window) window)
  (usual=> window :kill!))

(define-method buffer-frame (:update-display! window screen x-start y-start
					      xl xu yl yu display-style)
  ;; Assumes that interrupts are disabled.
  (if (or display-style (inferior-needs-redisplay? text-inferior))
      (update-inferior! text-inferior screen x-start y-start
			xl xu yl yu display-style
			buffer-window:update-display!))
  (if (and modeline-inferior
	   (or display-style (inferior-needs-redisplay? modeline-inferior)))
      (update-inferior! modeline-inferior screen x-start y-start
			xl xu yl yu display-style
			modeline-window:update-display!))
  (if (or display-style (inferior-needs-redisplay? border-inferior))
      (update-inferior! border-inferior screen x-start y-start
			xl xu yl yu display-style
			vertical-border-window:update-display!))
  true)

(define (initial-modeline! frame modeline?)
  ;; **** Kludge: The text-inferior will generate modeline events, so
  ;; if the modeline gets redisplayed first it will be left with its
  ;; redisplay-flag set but its superior's redisplay-flag cleared.
  (with-instance-variables buffer-frame frame (modeline?)
    (if modeline?
	(begin
	  (set! modeline-inferior (make-inferior frame modeline-window))
	  (set! inferiors
		(append! (delq! modeline-inferior inferiors)
			 (list modeline-inferior))))
	(set! modeline-inferior false))))

(define-integrable (frame-text-inferior frame)
  (with-instance-variables buffer-frame frame ()
    (inferior-window text-inferior)))

(define-method buffer-frame (:set-size! window x y)
  (set-buffer-frame-size! window x y))

(define-method buffer-frame (:set-x-size! window x)
  (set-buffer-frame-size! window x y-size))

(define-method buffer-frame (:set-y-size! window y)
  (set-buffer-frame-size! window x-size y))

(define (set-buffer-frame-size! window x y)
  (with-instance-variables buffer-frame window (x y)
    (usual=> window :set-size! x y)
    (if modeline-inferior
	(begin
	  (set! y (- y (inferior-y-size modeline-inferior)))
	  (set-inferior-start! modeline-inferior 0 y)
	  (set-inferior-x-size! modeline-inferior x)))
    (if (window-has-right-neighbor? window)
	(begin
	  (set! x (- x (inferior-x-size border-inferior)))
	  (set-inferior-start! border-inferior x 0)
	  (set-inferior-y-size! border-inferior y))
	(set-inferior-start! border-inferior false false))
    (set-inferior-start! text-inferior 0 0)
    (set-inferior-size! text-inferior x y)))

(define-method buffer-frame (:minimum-x-size window)
  (if (window-has-right-neighbor? window)
      (+ (ref-variable window-min-width)
	 (inferior-x-size border-inferior))
      (ref-variable window-min-width)))

(define-method buffer-frame (:minimum-y-size window)
  (if modeline-inferior
      (+ (ref-variable window-min-height)
	 (inferior-y-size modeline-inferior))
      (ref-variable window-min-height)))

;;;; External Entries

(define-integrable (buffer-frame? object)
  (object-of-class? buffer-frame object))

(define (make-buffer-frame superior new-buffer modeline?)
  (let ((frame (=> superior :make-inferior buffer-frame)))
    (set-window-buffer! frame new-buffer)
    (initial-modeline! frame modeline?)
    frame))

(define-integrable (buffer-frame-x-size frame)
  (window-x-size (frame-text-inferior frame)))

(define-integrable (buffer-frame-y-size frame)
  (window-y-size (frame-text-inferior frame)))

(define-integrable (buffer-frame-needs-redisplay? frame)
  (buffer-window/needs-redisplay? (frame-text-inferior frame)))

(define-integrable (window-cursor-enable! frame)
  (buffer-window/cursor-enable! (frame-text-inferior frame)))

(define-integrable (window-cursor-disable! frame)
  (buffer-window/cursor-disable! (frame-text-inferior frame)))

(define-integrable (window-select-time frame)
  (with-instance-variables buffer-frame frame ()
    last-select-time))

(define-integrable (set-window-select-time! frame time)
  (with-instance-variables buffer-frame frame (time)
    (set! last-select-time time)))

(define-integrable (window-buffer frame)
  (buffer-window/buffer (frame-text-inferior frame)))

(define (set-window-buffer! frame buffer)
  ;; BUFFER-WINDOW/SET-BUFFER! expects to have interrupts locked here.
  (without-interrupts
   (lambda ()
     ;; Someday this will bite someone...
     (if (and (string-ci=? (buffer-name buffer) "bluffer")
	      (null? (buffer-windows buffer)))
	 (buffer-reset! buffer))
     (if (window-buffer frame)
	 (remove-buffer-window! (window-buffer frame) frame))
     (buffer-window/set-buffer! (frame-text-inferior frame) buffer)
     (add-buffer-window! buffer frame))))

(define-integrable (window-point frame)
  (buffer-window/point (frame-text-inferior frame)))

(define-integrable (set-window-point! frame mark)
  (buffer-window/set-point! (frame-text-inferior frame) mark))

(define-integrable (window-redraw! frame)
  (buffer-window/redraw! (frame-text-inferior frame)))

(define (window-modeline-event! frame type)
  (with-instance-variables buffer-frame frame (type)
    (if modeline-inferior
	(modeline-window:event! (inferior-window modeline-inferior) type)))
  (screen-modeline-event! (window-screen frame) frame type))

(define-integrable (window-override-message window)
  (buffer-window/override-message (frame-text-inferior window)))

(define-integrable (window-set-override-message! window message)
  (buffer-window/set-override-message! (frame-text-inferior window) message))

(define-integrable (window-clear-override-message! window)
  (buffer-window/clear-override-message! (frame-text-inferior window)))

(define-integrable (window-direct-update! frame display-style)
  (buffer-window/direct-update! (frame-text-inferior frame) display-style))

(define-integrable (window-home-cursor! window)
  (buffer-window/home-cursor! (frame-text-inferior window)))

(define-integrable (window-direct-output-forward-char! frame)
  (buffer-window/direct-output-forward-char! (frame-text-inferior frame)))

(define-integrable (window-direct-output-backward-char! frame)
  (buffer-window/direct-output-backward-char! (frame-text-inferior frame)))

(define-integrable (window-direct-output-insert-char! frame char)
  (buffer-window/direct-output-insert-char! (frame-text-inferior frame) char))

(define-integrable (window-direct-output-insert-newline! frame)
  (buffer-window/direct-output-insert-newline! (frame-text-inferior frame)))

(define-integrable (window-direct-output-insert-substring! frame
							   string start end)
  (buffer-window/direct-output-insert-substring! (frame-text-inferior frame)
						 string start end))

(define-integrable (window-scroll-y-absolute! frame y-point)
  (buffer-window/scroll-y-absolute! (frame-text-inferior frame) y-point))

(define-integrable (window-scroll-y-relative! frame delta)
  (buffer-window/scroll-y-relative! (frame-text-inferior frame) delta))

(define-integrable (set-window-start-mark! frame mark force?)
  (buffer-window/set-start-mark! (frame-text-inferior frame) mark force?))

(define-integrable (window-y-center frame)
  (buffer-window/y-center (frame-text-inferior frame)))

(define-integrable (window-start-mark frame)
  (buffer-window/start-mark (frame-text-inferior frame)))

(define-integrable (window-mark-visible? frame mark)
  (buffer-window/mark-visible? (frame-text-inferior frame) mark))

(define-integrable (window-mark->x frame mark)
  (buffer-window/mark->x (frame-text-inferior frame) mark))

(define-integrable (window-mark->y frame mark)
  (buffer-window/mark->y (frame-text-inferior frame) mark))

(define-integrable (window-mark->coordinates frame mark)
  (buffer-window/mark->coordinates (frame-text-inferior frame) mark))

(define-integrable (window-point-x frame)
  (buffer-window/point-x (frame-text-inferior frame)))

(define-integrable (window-point-y frame)
  (buffer-window/point-y (frame-text-inferior frame)))

(define-integrable (window-point-coordinates frame)
  (buffer-window/point-coordinates (frame-text-inferior frame)))

(define-integrable (window-coordinates->mark frame x y)
  (buffer-window/coordinates->mark (frame-text-inferior frame) x y))

(define-integrable (set-window-debug-trace! frame debug-trace)
  (%set-window-debug-trace! (frame-text-inferior frame) debug-trace))

(define-variable-per-buffer truncate-lines
  "True means do not display continuation lines;
give each line of text one screen line.
Automatically becomes local when set in any fashion.

Note that this is overridden by the variable
truncate-partial-width-windows if that variable is true
and this buffer is not full-screen width."
  false
  boolean?)

(define-variable truncate-partial-width-windows
  "True means truncate lines in all windows less than full screen wide."
  true
  boolean?)

(define-variable-per-buffer tab-width
  "Distance between tab stops (for display of tab characters), in columns.
Automatically becomes local when set in any fashion."
  8
  exact-nonnegative-integer?)

(let ((setup-truncate-lines!
       (lambda (variable)
	 variable			;ignore
	 (for-each window-redraw! (window-list)))))
  (add-variable-assignment-daemon!
   (ref-variable-object truncate-lines)
   setup-truncate-lines!)
  (add-variable-assignment-daemon!
   (ref-variable-object truncate-partial-width-windows)
   setup-truncate-lines!)
  (add-variable-assignment-daemon!
   (ref-variable-object tab-width)
   setup-truncate-lines!))