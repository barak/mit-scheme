;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/screen.scm,v 1.81 1989/04/28 22:53:06 cph Rel $
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

;;;; Screen Abstraction

(declare (usual-integrations))

(define-structure (screen
		   (constructor make-screen
				(state
				 operation/beep
				 operation/finish-update!
				 operation/flush!
				 operation/inverse-video!
				 operation/start-update!
				 operation/subscreen-clear!
				 operation/write-char!
				 operation/write-cursor!
				 operation/write-substring!
				 operation/write-substrings!
				 operation/x-size
				 operation/y-size
				 operation/wipe!
				 operation/enter!
				 operation/exit!
				 operation/discard!)))
  (state false read-only true)
  (operation/beep false read-only true)
  (operation/finish-update! false read-only true)
  (operation/flush! false read-only true)
  (operation/inverse-video! false read-only true)
  (operation/start-update! false read-only true)
  (operation/subscreen-clear! false read-only true)
  (operation/write-char! false read-only true)
  (operation/write-cursor! false read-only true)
  (operation/write-substring! false read-only true)
  (operation/write-substrings! false read-only true)
  (operation/x-size false read-only true)
  (operation/y-size false read-only true)
  (operation/wipe! false read-only true)
  (operation/enter! false read-only true)
  (operation/exit! false read-only true)
  (operation/discard! false read-only true)
  (window false)
  (in-update? false))

(define (using-screen screen thunk)
  (dynamic-wind (lambda ()
		  ((screen-operation/enter! screen) screen))
		thunk
		(lambda ()
		  ((screen-operation/exit! screen) screen))))   

(define (with-screen-in-update! screen thunk)
  (let ((old-flag)
	(new-flag true))
    (dynamic-wind (lambda ()
		    ((screen-operation/start-update! screen) screen)
		    (set! old-flag (screen-in-update? screen))
		    (set-screen-in-update?! screen new-flag))
		  thunk
		  (lambda ()
		    (set! new-flag (screen-in-update? screen))
		    (set-screen-in-update?! screen old-flag)
		    ((screen-operation/finish-update! screen) screen)))))

(define (screen-x-size screen)
  ((screen-operation/x-size screen) screen))

(define (screen-y-size screen)
  ((screen-operation/y-size screen) screen))

(define (screen-beep screen)
  ((screen-operation/beep screen) screen))

(define (screen-flush! screen)
  ((screen-operation/flush! screen) screen))

(define (screen-inverse-video! screen highlight?)
  ((screen-operation/inverse-video! screen) screen highlight?))

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

(define (screen-wipe! screen)
  ((screen-operation/wipe! screen) screen))

(define (screen-enter! screen)
  ((screen-operation/enter! screen) screen))

(define (screen-exit! screen)
  ((screen-operation/exit! screen) screen))

(define (screen-discard! screen)
  ((screen-operation/discard! screen) screen))