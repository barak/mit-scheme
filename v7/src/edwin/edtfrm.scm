;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/edtfrm.scm,v 1.82 1990/10/06 00:15:44 cph Exp $
;;;
;;;	Copyright (c) 1985, 1989, 1990 Massachusetts Institute of Technology
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

;;;; Editor Frame

(declare (usual-integrations))

;;; Editor Frame

(define-class editor-frame vanilla-window
  (screen
   root-inferior
   typein-inferior
   selected-window
   cursor-window
   select-time
   properties))

(define (make-editor-frame root-screen main-buffer typein-buffer)
  (let ((window (make-object editor-frame)))
    (with-instance-variables editor-frame
			     window
			     (root-screen main-buffer typein-buffer)
      (set! superior false)
      (set! x-size (screen-x-size root-screen))
      (set! y-size (screen-y-size root-screen))
      (set! redisplay-flags (list false))
      (set! inferiors '())
      (set! properties (make-1d-table))
      (let ((main-window (make-buffer-frame window main-buffer true))
	    (typein-window (make-buffer-frame window typein-buffer false)))
	(set! screen root-screen)
	(set! root-inferior (find-inferior inferiors main-window))
	(set! typein-inferior (find-inferior inferiors typein-window))
	(set! selected-window main-window)
	(set! cursor-window main-window)
	(set! select-time 2)
	(set-window-select-time! main-window 1)
	(=> (window-cursor main-window) :enable!))
      (set-editor-frame-size! window x-size y-size))
    window))

(define (editor-frame-update-display! window display-style)
  ;; Returns true if update is successfully completed (or unnecessary).
  (with-instance-variables editor-frame window (display-style)
    (with-screen-in-update! screen
      (lambda ()
	(if (and (not display-style)
		 (not (car redisplay-flags)))
	    true
	    (let ((finished?
		   (update-inferiors! window screen 0 0
				      0 x-size 0 y-size
				      display-style)))
	      (if finished?
		  (set-car! redisplay-flags false))
	      finished?))))))

(define (set-editor-frame-size! window x y)
  (with-instance-variables editor-frame window (x y)
    (usual=> window :set-size! x y)
    (set-inferior-start! root-inferior 0 0)
    (let ((y* (- y typein-y-size)))
      (set-inferior-start! typein-inferior 0 y*)
      (set-inferior-size! root-inferior x y*))
    (set-inferior-size! typein-inferior x-size typein-y-size)))

(define-method editor-frame :set-size!
  set-editor-frame-size!)

(define typein-y-size 1)

(define-method editor-frame (:new-root-window! window window*)
  (set! root-inferior (find-inferior inferiors window*))
  unspecific)

(define-integrable (editor-frame-window0 window)
  (with-instance-variables editor-frame window ()
    (window0 (inferior-window root-inferior))))

(define-integrable (editor-frame-typein-window window)
  (with-instance-variables editor-frame window ()
    (inferior-window typein-inferior)))

(define-integrable (editor-frame-selected-window window)
  (with-instance-variables editor-frame window ()
    selected-window))

(define-integrable (editor-frame-cursor-window window)
  (with-instance-variables editor-frame window ()
    cursor-window))

(define-integrable (editor-frame-root-window window)
  (with-instance-variables editor-frame window ()
    (inferior-window root-inferior)))

(define-integrable (editor-frame-screen window)
  (with-instance-variables editor-frame window ()
    screen))

(define-integrable (editor-frame-properties window)
  (with-instance-variables editor-frame window ()
    properties))

(define (editor-frame-windows window)
  (cons (editor-frame-typein-window window)
	(let ((start (editor-frame-window0 window)))
	  (let loop ((window start))
	    (cons window
		  (let ((window (window1+ window)))
		    (if (eq? window start)
			'()
			(loop window))))))))

(define (editor-frame-select-window! window window*)
  (with-instance-variables editor-frame window (window*)
    (if (not (buffer-frame? window*))
	(error "Attempt to select non-window" window*))
    (=> (window-cursor cursor-window) :disable!)
    (set! selected-window window*)
    (set-window-select-time! window* select-time)
    (set! select-time (1+ select-time))
    (set! cursor-window window*)
    (=> (window-cursor cursor-window) :enable!)))

(define (editor-frame-select-cursor! window window*)
  (with-instance-variables editor-frame window (window*)
    (if (not (buffer-frame? window*))
	(error "Attempt to select non-window" window*))
    (=> (window-cursor cursor-window) :disable!)
    (set! cursor-window window*)
    (=> (window-cursor cursor-window) :enable!)))

(define-method editor-frame (:button-event! editor-frame button x y)
  (with-values
      (lambda ()
	(inferior-containing-coordinates editor-frame x y buffer-frame?))
    (lambda (frame relative-x relative-y)
      (if frame
	  ;; Make sure the event is inside the text portion of the
	  ;; buffer, not in the modeline or other decoration.
	  (cond ((and (< -1 relative-x (buffer-frame-x-size frame))
		      (< -1 relative-y (buffer-frame-y-size frame)))
		 (let ((command
			(comtab-entry (buffer-comtabs (window-buffer frame))
				      button)))
		   (cond (command
			  (with-current-button-event
			   (make-button-event frame relative-x relative-y)
			   (lambda () (execute-command command))))
			 ((button/down? button)
			  (editor-beep)))))
		((button/down? button)
		 (editor-beep)))))))