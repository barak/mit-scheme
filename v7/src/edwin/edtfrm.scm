;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1985 Massachusetts Institute of Technology
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

;;;; Editor Frame

(declare (usual-integrations)
	 (integrate-external "edb:window.bin.0"))
(using-syntax class-syntax-table

;;; Editor Frame

(define-class editor-frame vanilla-window
  (root-inferior typein-inferior selected-window cursor-window select-time))

(define (make-editor-frame superior x-start y-start x-size y-size
			   editor-name main-buffer typein-buffer)
  (let ((window (=> superior :make-inferior editor-frame)))
    (let ((main-window (make-buffer-frame window main-buffer #!TRUE))
	  (typein-window (make-buffer-frame window typein-buffer #!FALSE)))
      (with-instance-variables editor-frame window
	(set! root-inferior (find-inferior inferiors main-window))
	(set! typein-inferior (find-inferior inferiors typein-window))
	(set! selected-window main-window)
	(set! cursor-window main-window)
	(set! select-time 2))
      (set-window-select-time! main-window 1)
      (=> (window-cursor main-window) :enable!))
    (=> window :set-size! x-size y-size)
    (=> superior :set-inferior-start! window x-start y-start)
    window))

(define-method editor-frame (:set-size! window x y)
  (usual=> window :set-size! x y)
  (set-inferior-start! root-inferior 0 0)
  (let ((y* (- y typein-y-size)))
    (set-inferior-start! typein-inferior 0 y*)
    (set-inferior-size! root-inferior x y*))
  (set-inferior-size! typein-inferior x-size typein-y-size))

(define typein-y-size 1)

(define-method editor-frame (:new-root-window! window window*)
  (set! root-inferior (find-inferior inferiors window*)))

(define-procedure editor-frame (editor-frame-window0 window)
  (window0 (inferior-window root-inferior)))

(define-procedure editor-frame (editor-frame-typein-window window)
  (inferior-window typein-inferior))

(define-procedure editor-frame (editor-frame-selected-window window)
  selected-window)

(define-procedure editor-frame (editor-frame-cursor-window window)
  cursor-window)

(define-procedure editor-frame (editor-frame-select-window! window window*)
  (if (not (buffer-frame? window*))
      (error "Attempt to select non-window" window*))
  (=> (window-cursor cursor-window) :disable!)
  (set! selected-window window*)
  (set-window-select-time! window* select-time)
  (set! select-time (1+ select-time))
  (set! cursor-window window*)
  (=> (window-cursor cursor-window) :enable!))

(define-procedure editor-frame (editor-frame-select-cursor! window window*)
  (if (not (buffer-frame? window*))
      (error "Attempt to select non-window" window*))
  (=> (window-cursor cursor-window) :disable!)
  (set! cursor-window window*)
  (=> (window-cursor cursor-window) :enable!))

;;; end USING-SYNTAX
)