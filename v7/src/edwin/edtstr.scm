;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1989 Massachusetts Institute of Technology
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

;;;; Editor Data Abstraction

(declare (usual-integrations))

(define-structure (editor (constructor %make-editor))
  (name false read-only true)
  (screens false)
  (current-frame-window false)
  (bufferset false read-only true)
  (kill-ring false read-only true)
  (char-history false read-only true)
  (button-event false)
  (frame-windows false))

(define (make-editor name screen)
  (let ((initial-buffer (make-buffer initial-buffer-name initial-buffer-mode)))
    (let ((bufferset (make-bufferset initial-buffer)))
      (let ((frame
	     (make-editor-frame screen
				initial-buffer
				(make-buffer " *Typein-0*"))))
	(set-screen-window! screen frame)
	(%make-editor name
		      (list screen)
		      frame
		      bufferset
		      (make-ring 10)
		      (make-ring 100)
		      false
		      (list frame))))))

(define (editor-add-screen! editor screen)
  (if (not (memq screen (editor-screens editor)))
      (set-editor-screens! editor
			   (cons screen
				 (editor-screens editor)))))

(define (editor-delete-screen! editor screen)
  (set-editor-screens! editor
		       (delq screen
			     (editor-screens editor))))

(define (editor-add-frame! editor screen)
  (if (not (memq screen (editor-frame-windows editor)))
      (set-editor-frame-windows! editor
			   (cons screen
				 (editor-frame-windows editor)))))

(define (editor-delete-frame! editor screen)
  (set-editor-frame-windows! editor
		       (delq screen
			     (editor-frame-windows editor))))

(define-integrable (current-screen)
  (editor-frame-screen (current-editor-frame)))

(define-integrable (all-screens)
  (editor-screens current-editor))

(define-integrable (current-editor-input-port)
  (editor-frame-input-port (current-editor-frame)))

(define-integrable (current-editor-frame)
  (editor-current-frame-window current-editor))

(define-integrable (all-editor-frames)
  (editor-frame-windows current-editor))

(define-integrable (all-windows)
  (append-map editor-frame-windows (all-editor-frames)))

(define-integrable (current-bufferset)
  (editor-bufferset current-editor))

(define-integrable (current-kill-ring)
  (editor-kill-ring current-editor))

(define-integrable (current-char-history)
  (editor-char-history current-editor))

(define-structure (button-event
		   (conc-name button-event/))
  (window false read-only true)
  (x false read-only true)
  (y false read-only true))

(define (current-button-event)
  (or (editor-button-event current-editor)
      ;; Create a "dummy" event at point.
      (let ((window (current-window)))
	(let ((coordinates (window-point-coordinates window)))
	  (make-button-event window
			     (car coordinates)
			     (cdr coordinates))))))

(define (with-current-button-event button-event thunk)
  (let ((old-button-event))
    (dynamic-wind
     (lambda ()
       (set! old-button-event (editor-button-event current-editor))
       (set-editor-button-event! current-editor button-event)
       (set! button-event false)
       unspecific)
     thunk
     (lambda ()
       (set! button-event (editor-button-event current-editor))
       (set-editor-button-event! current-editor old-button-event)
       (set! old-button-event false)
       unspecific))))