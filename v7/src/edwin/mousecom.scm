;;; -*-Scheme-*-
;;;
;;;	$Id: mousecom.scm,v 1.1 1994/10/25 01:46:12 adams Exp $
;;;
;;;	Copyright (c) 1989-92 Massachusetts Institute of Technology
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
;;;
;;;; Mouse Commands

(define-command mouse-select
  "Select window the mouse is on."
  ()
  (lambda ()
    (select-window (button-event/window (current-button-event)))))

(define-command mouse-keep-one-window
  "Select window mouse is on, then kill all other windows."
  ()
  (lambda ()
    ((ref-command mouse-select))
    ((ref-command delete-other-windows))))

(define-command mouse-select-and-split
  "Select window mouse is on, then split it vertically in half."
  ()
  (lambda ()
    ((ref-command mouse-select))
    ((ref-command split-window-vertically) false)))

(define-command mouse-set-point
  "Select window mouse is on, and move point to mouse position."
  ()
  (lambda ()
    (let ((button-event (current-button-event)))
      (let ((window (button-event/window button-event)))
	(select-window window)
	(set-current-point!
	 (or (window-coordinates->mark window
				       (button-event/x button-event)
				       (button-event/y button-event))
	     (buffer-end (window-buffer window))))))))

(define-command mouse-set-mark
  "Select window mouse is on, and set mark at mouse position.
Display cursor at that position for a second."
  ()
  (lambda ()
    (let ((button-event (current-button-event)))
      (let ((window (button-event/window button-event)))
	(select-window window)
	(let ((mark
	       (or (window-coordinates->mark window
					     (button-event/x button-event)
					     (button-event/y button-event))
		   (buffer-end (window-buffer window)))))
	  (push-current-mark! mark)
	  (mark-flash mark))))))

(define-command mouse-show-event
  "Show the mouse position in the minibuffer."
  ()
  (lambda ()
    (let ((button-event (current-button-event)))
      (message "window: " (button-event/window button-event)
	       " x: " (button-event/x button-event)
	       " y: " (button-event/y button-event)))))

(define-command mouse-ignore
  "Don't do anything."
  ()
  (lambda () unspecific))

(define button1-down (make-down-button 0))
(define button2-down (make-down-button 1))
(define button3-down (make-down-button 2))
(define button4-down (make-down-button 3))
(define button5-down (make-down-button 4))
(define button1-up (make-up-button 0))
(define button2-up (make-up-button 1))
(define button3-up (make-up-button 2))
(define button4-up (make-up-button 3))
(define button5-up (make-up-button 4))
