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

;;;; Buffer Frames

(declare (usual-integrations)
	 )
(using-syntax class-syntax-table

(define-class buffer-frame combination-leaf-window
  (text-inferior border-inferior modeline-inferior last-select-time))

(define (buffer-frame? object)
  (object-of-class? buffer-frame object))

(define (make-buffer-frame superior new-buffer modeline?)
  (let ((frame (=> superior :make-inferior buffer-frame)))
    (initial-buffer! (frame-text-inferior frame) new-buffer)
    (initial-modeline! frame modeline?)
    frame))

(define-method buffer-frame (:make-leaf frame)
  (let ((frame* (=> superior :make-inferior buffer-frame)))
    (initial-buffer! (frame-text-inferior frame*) (window-buffer frame))
    (initial-modeline! frame* modeline-inferior)
    frame*))

(define-method buffer-frame (:initialize! frame window*)
  (usual=> frame :initialize! window*)
  (set! text-inferior (make-inferior frame buffer-window))
  (set! border-inferior (make-inferior frame vertical-border-window))
  (set! last-select-time 0))

;;; **** Kludge: The text-inferior will generate modeline events, so
;;; if the modeline gets redisplayed first it will be left with its
;;; redisplay-flag set but its superior's redisplay-flag cleared.

(define-procedure buffer-frame (initial-modeline! frame modeline?)
  (if modeline?
      (begin (set! modeline-inferior (make-inferior frame modeline-window))
	     (set! inferiors
		   (append! (delq! modeline-inferior inferiors)
			    (list modeline-inferior))))
      (set! modeline-inferior #!FALSE)))

(define-procedure buffer-frame (window-cursor frame)
  (%window-cursor (inferior-window text-inferior)))

(declare (integrate frame-text-inferior))
(define-procedure buffer-frame (frame-text-inferior frame)
  (declare (integrate frame))
  (inferior-window text-inferior))

(define-procedure buffer-frame (frame-modeline-inferior frame)
  (and modeline-inferior
       (inferior-window modeline-inferior)))

(define-procedure buffer-frame (window-modeline-event! frame type)
  (if modeline-inferior
      (=> (inferior-window modeline-inferior) :event! type)))

(define-procedure buffer-frame (window-select-time frame)
  last-select-time)

(define-procedure buffer-frame (set-window-select-time! frame time)
  (set! last-select-time time))

(define-procedure buffer-frame (set-buffer-frame-size! window x y)
  (usual=> window :set-size! x y)
  (if (window-has-right-neighbor? window)
      (let ((x* (- x (inferior-x-size border-inferior))))
	(set-inferior-start! border-inferior x* 0)
	(set-inferior-y-size! border-inferior y)
	(set! x x*))
      (set-inferior-start! border-inferior #!FALSE #!FALSE))
  (if modeline-inferior
      (let ((y* (- y (inferior-y-size modeline-inferior))))
	(set-inferior-start! modeline-inferior 0 y*)
	(set-inferior-x-size! modeline-inferior x)
	(set! y y*)))
  (set-inferior-start! text-inferior 0 0)
  (set-inferior-size! text-inferior x y))

(define-method buffer-frame :set-size!
  set-buffer-frame-size!)

(define-method buffer-frame (:set-x-size! window x)
  (set-buffer-frame-size! window x y-size))

(define-method buffer-frame (:set-y-size! window y)
  (set-buffer-frame-size! window x-size y))

(define-method buffer-frame (:minimum-x-size window)
  (if (window-has-right-neighbor? window)
      (+ (ref-variable "Window Minimum Width")
	 (inferior-x-size border-inferior))
      (ref-variable "Window Minimum Width")))

(define-method buffer-frame (:minimum-y-size window)
  (if modeline-inferior
      (+ (ref-variable "Window Minimum Height")
	 (inferior-y-size modeline-inferior))
      (ref-variable "Window Minimum Height")))

;;;; External Entries

(define (window-buffer frame)
  (%window-buffer (frame-text-inferior frame)))

(define (set-window-buffer! frame buffer)
  (if (and (string-ci=? (buffer-name buffer) "Bluffer")
	   (null? (buffer-windows buffer)))
      (buffer-reset! buffer))
  (%set-window-buffer! (frame-text-inferior frame) buffer))

(define (window-point frame)
  (%window-point (frame-text-inferior frame)))

(define (set-window-point! frame point)
  (let ((window (frame-text-inferior frame)))
    (%set-window-point! window (clip-mark-to-display window point))))

(define (window-redraw! frame #!optional preserve-point?)
  (if (unassigned? preserve-point?) (set! preserve-point? #!FALSE))
  (let ((window (frame-text-inferior frame)))
    (%window-redraw! window
		     (if preserve-point?
			 (%window-point-y window)
			 (%window-y-center window)))))

(define (window-direct-update! frame display-style)
  (%window-direct-update! (frame-text-inferior frame) display-style))

(define-procedure buffer-frame (window-needs-redisplay? frame)
  (car (inferior-redisplay-flags text-inferior)))

(define (direct-output-insert-char! frame char)
  (%direct-output-insert-char! (frame-text-inferior frame) char))

(define (direct-output-insert-newline! frame)
  (%direct-output-insert-newline! (frame-text-inferior frame)))

(define (direct-output-insert-substring! frame string start end)
  (%direct-output-insert-substring! (frame-text-inferior frame)
				    string start end))

(define (direct-output-forward-character! frame)
  (%direct-output-forward-character! (frame-text-inferior frame)))

(define (direct-output-backward-character! frame)
  (%direct-output-backward-character! (frame-text-inferior frame)))

(define (window-scroll-y-absolute! frame y-point)
  (let ((window (frame-text-inferior frame)))
    (maybe-recompute-image! window)
    (%window-scroll-y-absolute! window y-point)))

(define (window-scroll-y-relative! frame delta)
  (let ((window (frame-text-inferior frame)))
    (maybe-recompute-image! window)
    (%window-scroll-y-relative! window delta)))

(define (window-y-center frame)
  (%window-y-center (frame-text-inferior frame)))

(define (window-start-mark frame)
  (let ((window (frame-text-inferior frame)))
    (maybe-recompute-image! window)
    (%window-start-mark window)))

(define (set-window-start-mark! frame mark force?)
  (let ((window (frame-text-inferior frame)))
    (maybe-recompute-image! window)
    (%set-window-start-mark! window
			     (clip-mark-to-display window mark)
			     force?)))

(define (window-end-mark frame)
  (let ((window (frame-text-inferior frame)))
    (maybe-recompute-image! window)
    (%window-end-mark window)))
(define (window-mark-visible? frame mark)
  (let ((window (frame-text-inferior frame)))
    (maybe-recompute-image! window)
    (%window-mark-visible? window mark)))

(define (buffer-frame-x-size frame)
  (window-x-size (frame-text-inferior frame)))

(define (buffer-frame-y-size frame)
  (window-y-size (frame-text-inferior frame)))

(define (window-mark->x frame mark)
  (let ((window (frame-text-inferior frame)))
    (maybe-recompute-image! window)
    (%window-mark->x window (clip-mark-to-display window mark))))

(define (window-mark->y frame mark)
  (let ((window (frame-text-inferior frame)))
    (maybe-recompute-image! window)
    (%window-mark->y window (clip-mark-to-display window mark))))

(define (window-mark->coordinates frame mark)
  (let ((window (frame-text-inferior frame)))
    (maybe-recompute-image! window)
    (%window-mark->coordinates window (clip-mark-to-display window mark))))

(define (window-point-x frame)
  (let ((window (frame-text-inferior frame)))
    (maybe-recompute-image! window)
    (%window-point-x window)))

(define (window-point-y frame)
  (let ((window (frame-text-inferior frame)))
    (maybe-recompute-image! window)
    (%window-point-y window)))

(define (window-point-coordinates frame)
  (let ((window (frame-text-inferior frame)))
    (maybe-recompute-image! window)
    (%window-point-coordinates window)))

(define (window-coordinates->mark frame x y)
  (let ((window (frame-text-inferior frame)))
    (maybe-recompute-image! window)
    (%window-coordinates->mark window x y)))

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: (access window-package edwin-package)
;;; Scheme Syntax Table: class-syntax-table
;;; End:
