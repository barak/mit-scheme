;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/buffrm.scm,v 1.35 1990/10/03 04:54:12 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989, 1990 Massachusetts Institute of Technology
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
  (text-inferior border-inferior modeline-inferior last-select-time))

(define-integrable (buffer-frame? object)
  (object-of-class? buffer-frame object))

(define (make-buffer-frame superior new-buffer modeline?)
  (let ((frame (=> superior :make-inferior buffer-frame)))
    (let ((window (frame-text-inferior frame)))
      (initial-buffer! window new-buffer)
      (%window-setup-truncate-lines! window false))
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
  (set! last-select-time 0)
  unspecific)

;;; **** Kludge: The text-inferior will generate modeline events, so
;;; if the modeline gets redisplayed first it will be left with its
;;; redisplay-flag set but its superior's redisplay-flag cleared.

(define (initial-modeline! frame modeline?)
  (with-instance-variables buffer-frame frame (modeline?)
    (if modeline?
	(begin
	  (set! modeline-inferior (make-inferior frame modeline-window))
	  (set! inferiors
		(append! (delq! modeline-inferior inferiors)
			 (list modeline-inferior))))
	(set! modeline-inferior false))
    unspecific))

(define-integrable (window-cursor frame)
  (%window-cursor (frame-text-inferior frame)))

(define-integrable (frame-text-inferior frame)
  (with-instance-variables buffer-frame frame ()
    (inferior-window text-inferior)))

(define (frame-modeline-inferior frame)
  (with-instance-variables buffer-frame frame ()
    (and modeline-inferior
	 (inferior-window modeline-inferior))))

(define (window-select-time frame)
  (with-instance-variables buffer-frame frame ()
    last-select-time))

(define (set-window-select-time! frame time)
  (with-instance-variables buffer-frame frame (time)
    (set! last-select-time time)
    unspecific))

(define (set-buffer-frame-size! window x y)
  (with-instance-variables buffer-frame window (x y)
    (usual=> window :set-size! x y)
    (if (window-has-right-neighbor? window)
	(let ((x* (- x (inferior-x-size border-inferior))))
	  (set-inferior-start! border-inferior x* 0)
	  (set-inferior-y-size! border-inferior y)
	  (set! x x*))
	(set-inferior-start! border-inferior false false))
    (if modeline-inferior
	(let ((y* (- y (inferior-y-size modeline-inferior))))
	  (set-inferior-start! modeline-inferior 0 y*)
	  (set-inferior-x-size! modeline-inferior x)
	  (set! y y*)))
    (set-inferior-start! text-inferior 0 0)
    (set-inferior-size! text-inferior x y)))

(define-method buffer-frame :set-size!
  set-buffer-frame-size!)

(define-method buffer-frame (:set-x-size! window x)
  (set-buffer-frame-size! window x y-size))

(define-method buffer-frame (:set-y-size! window y)
  (set-buffer-frame-size! window x-size y))

(define-method buffer-frame (:minimum-x-size window)
  (if (window-has-right-neighbor? window)
      (+ (ref-variable window-minimum-width)
	 (inferior-x-size border-inferior))
      (ref-variable window-minimum-width)))

(define-method buffer-frame (:minimum-y-size window)
  (if modeline-inferior
      (+ (ref-variable window-minimum-height)
	 (inferior-y-size modeline-inferior))
      (ref-variable window-minimum-height)))

(define (buffer-frame-x-size frame)
  (window-x-size (frame-text-inferior frame)))

(define (buffer-frame-y-size frame)
  (window-y-size (frame-text-inferior frame)))

;;;; External Entries

(define-integrable (window-buffer frame)
  (%window-buffer (frame-text-inferior frame)))

(define (set-window-buffer! frame buffer)
  (if (and (string-ci=? (buffer-name buffer) "Bluffer")
	   (null? (buffer-windows buffer)))
      (buffer-reset! buffer))
  (%set-window-buffer! (frame-text-inferior frame) buffer))

(define-integrable (window-point frame)
  (%window-point (frame-text-inferior frame)))

(define (set-window-point! frame point)
  (let ((window (frame-text-inferior frame)))
    (%set-window-point! window (clip-mark-to-display window point))))

(define (window-redraw! frame redraw-type)
  (%window-force-redraw! (frame-text-inferior frame) redraw-type))

(define (window-redraw-preserving-point! frame)
  (let ((window (frame-text-inferior frame)))
    (%window-force-redraw! window (%window-point-y window))))

(define-integrable (window-needs-redisplay? frame)
  (with-instance-variables buffer-frame frame ()
    (car (inferior-redisplay-flags text-inferior))))

(define (window-modeline-event! frame type)
  (with-instance-variables buffer-frame frame (type)
    (if modeline-inferior
	(=> (inferior-window modeline-inferior) :event! type)))
  (screen-modeline-event! (window-screen frame) frame type))

(define-integrable (window-set-override-message! window message)
  (set-override-message! (frame-text-inferior window) message))

(define-integrable (window-clear-override-message! window)
  (clear-override-message! (frame-text-inferior window)))

(define-integrable (window-home-cursor! window)
  (home-cursor! (frame-text-inferior window)))

(define-integrable (window-direct-update! frame display-style)
  (%window-direct-update! (frame-text-inferior frame) display-style))

(define (window-direct-output-insert-char! frame char)
  (without-interrupts
   (lambda ()
     (let ((point (window-point frame)))
       (%group-insert-char! (mark-group point) (mark-index point) char))
     (%direct-output-insert-char! (frame-text-inferior frame) char))))

(define (window-direct-output-insert-newline! frame)
  (without-interrupts
   (lambda ()
     (let ((point (window-point frame)))
       (%group-insert-char! (mark-group point) (mark-index point) #\newline))
     (%direct-output-insert-newline! (frame-text-inferior frame)))))

(define (window-direct-output-insert-substring! frame string start end)
  (without-interrupts
   (lambda ()
     (let ((point (window-point frame)))
       (%group-insert-substring! (mark-group point) (mark-index point)
				 string start end))
     (%direct-output-insert-substring! (frame-text-inferior frame)
				       string start end))))

(define-integrable (window-direct-output-forward-char! frame)
  (without-interrupts
   (lambda ()
     (%direct-output-forward-character! (frame-text-inferior frame)))))

(define-integrable (window-direct-output-backward-char! frame)
  (without-interrupts
   (lambda ()
     (%direct-output-backward-character! (frame-text-inferior frame)))))

(define (window-scroll-y-absolute! frame y-point)
  (let ((window (frame-text-inferior frame)))
    (maybe-recompute-image! window)
    (%window-scroll-y-absolute! window y-point)))

(define (window-scroll-y-relative! frame delta)
  (let ((window (frame-text-inferior frame)))
    (maybe-recompute-image! window)
    (%window-scroll-y-relative! window delta)))

(define (set-window-start-mark! frame mark force?)
  (let ((window (frame-text-inferior frame)))
    (maybe-recompute-image! window)
    (%set-window-start-mark! window
			     (clip-mark-to-display window mark)
			     force?)))

(define-integrable (window-y-center frame)
  (%window-y-center (frame-text-inferior frame)))

(define (window-start-index frame)
  (let ((window (frame-text-inferior frame)))
    (maybe-recompute-image! window)
    (%window-start-index window)))

(define (window-end-index frame)
  (let ((window (frame-text-inferior frame)))
    (maybe-recompute-image! window)
    (%window-end-index window)))

(define (window-mark-visible? frame mark)
  (let ((window (frame-text-inferior frame)))
    (maybe-recompute-image! window)
    (%window-mark-visible? window mark)))

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

(define (window-setup-truncate-lines! frame)
  (%window-setup-truncate-lines! (frame-text-inferior frame) 'START))