;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/screen.scm,v 1.83 1990/10/06 00:16:20 cph Exp $
;;;
;;;	Copyright (c) 1989, 1990 Massachusetts Institute of Technology
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

;;;; Screen Abstraction

(declare (usual-integrations))

(define-structure (screen
		   (constructor make-screen
				(state
				 operation/beep
				 operation/discard!
				 operation/enter!
				 operation/exit!
				 operation/finish-update!
				 operation/flush!
				 operation/inverse-video!
				 operation/modeline-event!
				 operation/normal-video!
				 operation/start-update!
				 operation/subscreen-clear!
				 operation/wipe!
				 operation/write-char!
				 operation/write-cursor!
				 operation/write-substring!
				 operation/write-substrings!
				 x-size
				 y-size)))
  (state false read-only true)
  (operation/beep false read-only true)
  (operation/discard! false read-only true)
  (operation/enter! false read-only true)
  (operation/exit! false read-only true)
  (operation/finish-update! false read-only true)
  (operation/flush! false read-only true)
  (operation/inverse-video! false read-only true)
  (operation/modeline-event! false read-only true)
  (operation/normal-video! false read-only true)
  (operation/start-update! false read-only true)
  (operation/subscreen-clear! false read-only true)
  (operation/wipe! false read-only true)
  (operation/write-char! false read-only true)
  (operation/write-cursor! false read-only true)
  (operation/write-substring! false read-only true)
  (operation/write-substrings! false read-only true)
  (operation/x-size false read-only true)
  (operation/y-size false read-only true)
  (root-window false)
  (in-update? false)
  (x-size false)
  (y-size false)
  (highlight? false))

(define (initialize-screen-root-window! screen bufferset buffer)
  (set-screen-root-window!
   screen
   (make-editor-frame
    screen
    buffer
    (bufferset-find-or-create-buffer bufferset (make-typein-buffer-name 0)))))

(define (using-screen screen thunk)
  (dynamic-wind (lambda ()
		  ((screen-operation/enter! screen) screen))
		thunk
		(lambda ()
		  ((screen-operation/exit! screen) screen))))   

(define (with-screen-in-update! screen thunk)
  (call-with-current-continuation
   (lambda (continuation)
     (let ((old-flag)
	   (new-flag true)
	   (transition
	    (lambda (old new)
	      (if old
		  (if (not new)
		      (begin
			((screen-operation/finish-update! screen) screen)
			(set-screen-in-update?! screen false)))
		  (if new
		      (begin
			((screen-operation/start-update! screen) screen)
			(set-screen-in-update?! screen continuation)))))))
       (dynamic-wind (lambda ()
		       (set! old-flag (screen-in-update? screen))
		       (transition old-flag new-flag))
		     thunk
		     (lambda ()
		       (set! new-flag (screen-in-update? screen))
		       (transition new-flag old-flag)))))))

(define (with-screen-inverse-video! screen thunk)
  (let ((old-highlight?)
	(new-highlight? true)
	(transition
	 (lambda (old new)
	   (if old
	       (if (not new)
		   (begin
		     ((screen-operation/normal-video! screen) screen)
		     (set-screen-highlight?! screen false)))
	       (if new
		   (begin
		     ((screen-operation/inverse-video! screen) screen)
		     (set-screen-highlight?! screen true)))))))
    (dynamic-wind (lambda ()
		    (set! old-highlight? (screen-highlight? screen))
		    (transition old-highlight? new-highlight?))
		  thunk
		  (lambda ()
		    (set! new-highlight? (screen-highlight? screen))
		    (transition new-highlight? old-highlight?)))))

(define (screen-beep screen)
  ((screen-operation/beep screen) screen))

(define (screen-flush! screen)
  ((screen-operation/flush! screen) screen))

(define (subscreen-clear! screen xl xu yl yu)
  ((screen-operation/subscreen-clear! screen) screen xl xu yl yu))

(define (screen-write-cursor! screen x y)
  ((screen-operation/write-cursor! screen) screen x y))

(define (screen-write-char! screen x y char)
  ((screen-operation/write-char! screen) screen x y char))

(define (screen-write-substring! screen x y string start end)
  ((screen-operation/write-substring! screen) screen x y string start end))

(define (screen-write-substrings! screen x y strings bil biu bjl bju)
  ((screen-operation/write-substrings! screen)
   screen x y strings bil biu bjl bju))

(define (screen-enter! screen)
  ((screen-operation/enter! screen) screen))

(define (screen-exit! screen)
  ((screen-operation/exit! screen) screen))

(define (screen-discard! screen)
  (for-each (lambda (window) (send window ':kill!))
	    (screen-window-list screen))
  ((screen-operation/discard! screen) screen))

(define (screen-modeline-event! screen window type)
  ((screen-operation/modeline-event! screen) screen window type))

(define-integrable (screen-selected-window screen)
  (editor-frame-selected-window (screen-root-window screen)))

(define-integrable (screen-select-window! screen window)
  (editor-frame-select-window! (screen-root-window screen) window)
  (screen-modeline-event! screen window 'SELECT-WINDOW))

(define-integrable (screen-select-cursor! screen window)
  (editor-frame-select-cursor! (screen-root-window screen) window))

(define-integrable (screen-window-list screen)
  (editor-frame-windows (screen-root-window screen)))

(define-integrable (screen-window0 screen)
  (editor-frame-window0 (screen-root-window screen)))

(define-integrable (screen-typein-window screen)
  (editor-frame-typein-window (screen-root-window screen)))

(define (window-screen window)
  (editor-frame-screen (window-root-window window)))

(define (update-screen! screen display-style)
  (if display-style ((screen-operation/wipe! screen) screen))
  (editor-frame-update-display! (screen-root-window screen) display-style))