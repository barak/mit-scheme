;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/xterm.scm,v 1.2 1989/03/30 16:40:21 jinx Exp $
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

;;;; X Terminal

(declare (usual-integrations))

(define-primitives
  (xterm-open-display 1)
  (xterm-close-display 1)
  (xterm-close-all-displays 0)
  (xterm-open-window 3)
  (xterm-close-window 1)
  (xterm-map 1)
  (xterm-unmap 1)
  (xterm-x-size 1)
  (xterm-y-size 1)
  (xterm-read-event-flags! 1)
  (xterm-beep 1)
  (xterm-flush 1)
  (xterm-write-cursor! 3)
  (xterm-write-char! 5)
  (xterm-write-substring! 7)
  (xterm-clear-rectangle! 6)
  (xterm-read-chars 2))

(define-structure (xterm-screen-state
		   (constructor make-xterm-screen-state (xterm))
		   (conc-name xterm-screen-state/))
  (xterm false read-only true)
  (highlight 0))

(define (make-xterm-screen #!optional geometry)
  (make-screen (make-xterm-screen-state
		(xterm-open-window (or (get-X-display)
				       (error "unable to open display"))
				   (and (not (default-object? geometry))
					geometry)
				   false))
	       xterm-screen/beep
	       xterm-screen/finish-update!
	       xterm-screen/flush!
	       xterm-screen/inverse-video!
	       xterm-screen/start-update!
	       xterm-screen/subscreen-clear!
	       xterm-screen/write-char!
	       xterm-screen/write-cursor!
	       xterm-screen/write-substring!
	       xterm-screen/write-substrings!
	       xterm-screen/x-size
	       xterm-screen/y-size
	       xterm-screen/wipe!
	       xterm-screen/enter!
	       xterm-screen/exit!
	       xterm-screen/discard!))

(define-integrable (screen-xterm screen)
  (xterm-screen-state/xterm (screen-state screen)))

(define-integrable (screen-highlight screen)
  (xterm-screen-state/highlight (screen-state screen)))

(define-integrable (set-screen-highlight! screen highlight)
  (set-xterm-screen-state/highlight! (screen-state screen) highlight))

(define (xterm-screen/start-update! screen)
  (xterm-screen/process-events! screen))

(define (xterm-screen/finish-update! screen)
  (xterm-flush (screen-xterm screen)))

(define (xterm-screen/beep screen)
  (let ((xterm (screen-xterm screen)))
    (xterm-beep xterm)
    (xterm-flush xterm)))

(define (xterm-screen/flush! screen)
  (xterm-flush (screen-xterm screen)))

(define (xterm-screen/inverse-video! screen highlight?)
  (let ((result (not (zero? (screen-highlight screen)))))
    (set-screen-highlight! screen (if highlight? 1 0))
    result))

(define (xterm-screen/write-char! screen x y char)
  (xterm-write-char! (screen-xterm screen) x y char (screen-highlight screen)))

(define (xterm-screen/write-cursor! screen x y)
  (xterm-write-cursor! (screen-xterm screen) x y))

(define (xterm-screen/write-substring! screen x y string start end)
  (xterm-write-substring! (screen-xterm screen) x y string start end
			  (screen-highlight screen)))

(define (xterm-screen/write-substrings! screen x y strings bil biu bjl bju)
  (let ((xterm (screen-xterm screen))
	(highlight (screen-highlight screen)))
    (clip (xterm-x-size xterm) x bil biu
      (lambda (bxl ail aiu)
	(clip (xterm-y-size xterm) y bjl bju
	  (lambda (byl ajl aju)
	    (let loop ((y byl) (j ajl))
	      (if (< j aju)
		  (begin
		    (xterm-write-substring! xterm
					    bxl y
					    (vector-ref strings j)
					    ail aiu
					    highlight)
		    (loop (1+ y) (1+ j)))))))))))

(define (clip axu x bil biu receiver)
  (let ((ail (- bil x)))
    (if (< ail biu)
	(let ((aiu (+ ail axu)))
	  (cond ((not (positive? x))
		 (receiver 0 ail (if (< aiu biu) aiu biu)))
		((< x axu)
		 (receiver x bil (if (< aiu biu) aiu biu))))))))

(define (xterm-screen/subscreen-clear! screen xl xu yl yu)
  (xterm-clear-rectangle! (screen-xterm screen) xl xu yl yu
			  (screen-highlight screen)))

(define (xterm-screen/x-size screen)
  (xterm-x-size (screen-xterm screen)))

(define (xterm-screen/y-size screen)
  (xterm-y-size (screen-xterm screen)))

(define (xterm-screen/wipe! screen)
  screen				; ignored
  unspecific)

(define (xterm-screen/enter! screen)
  screen				; ignored
  unspecific)

(define (xterm-screen/exit! screen)
  screen				; ignored
  unspecific)

(define (xterm-screen/discard! screen)
  screen				; ignored
  (close-X-display))

;;;; Input Port

(define (make-xterm-input-port screen)
  (input-port/copy xterm-input-port-template
		   (make-xterm-input-port-state screen)))

(define-structure (xterm-input-port-state
		   (constructor make-xterm-input-port-state (screen))
		   (conc-name xterm-input-port-state/))
  (screen false read-only true)
  (buffer "")
  (index 0))

(define (operation/char-ready? port interval)
  (let ((state (input-port/state port)))
    (if (< (xterm-input-port-state/index state)
	   (string-length (xterm-input-port-state/buffer state)))
	true
	(let ((buffer
	       (xterm-screen/read-chars (xterm-input-port-state/screen state)
					interval)))
	  (and buffer
	       (begin
		 (check-for-interrupts! state buffer 0)
		 true))))))

(define (operation/peek-char port)
  (let ((state (input-port/state port)))
    (let ((buffer (xterm-input-port-state/buffer state))
	  (index (xterm-input-port-state/index state)))
      (if (< index (string-length buffer))
	  (string-ref buffer index)
	  (refill-buffer! state 0)))))

(define (operation/discard-char port)
  (let ((state (input-port/state port)))
    (set-xterm-input-port-state/index!
     state
     (1+ (xterm-input-port-state/index state)))))

(define (operation/read-char port)
  (let ((state (input-port/state port)))
    (let ((buffer (xterm-input-port-state/buffer state))
	  (index (xterm-input-port-state/index state)))
      (if (< index (string-length buffer))
	  (begin
	    (set-xterm-input-port-state/index! state (1+ index))
	    (string-ref buffer index))
	  (refill-buffer! state 1)))))

(define (operation/print-self state port)
  (unparse-string state "from screen ")
  (unparse-object state
		  (xterm-input-port-state/screen (input-port/state port))))

(define xterm-input-port-template
  (make-input-port `((CHAR-READY? ,operation/char-ready?)
		     (DISCARD-CHAR ,operation/discard-char)
		     (PEEK-CHAR ,operation/peek-char)
		     (PRINT-SELF ,operation/print-self)
		     (READ-CHAR ,operation/read-char))
		   false))

(define (refill-buffer! state index)
  (let ((screen (xterm-input-port-state/screen state)))
    (let loop ()
      (let ((buffer (xterm-screen/read-chars screen false)))
	(if (not buffer)
	    (loop)
	    (begin
	      (check-for-interrupts! state buffer index)
	      (string-ref buffer 0)))))))

(define (xterm-screen/read-chars screen interval)
  (let ((result (xterm-read-chars (screen-xterm screen) interval)))
    (if (and (not (screen-in-update? screen))
	     (xterm-screen/process-events! screen))
	(update-screen! screen false))
    result))

(define (xterm-screen/process-events! screen)
  (let ((xterm (screen-xterm screen)))
    (and (odd? (xterm-read-event-flags! xterm))
	 (let ((window (screen-window screen)))
	   (and window
		(send window ':set-size!
		      (xterm-x-size xterm)
		      (xterm-y-size xterm))
		true)))))

(define (check-for-interrupts! state buffer index)
  (set-xterm-input-port-state/buffer! state buffer)
  (let ((^g-index
	 (and signal-interrupts?
	      (string-find-previous-char buffer #\BEL))))
    (if ^g-index
	(begin
	  (set-xterm-input-port-state/index! state (1+ ^g-index))
	  (signal-interrupt!))
	(set-xterm-input-port-state/index! state index))))

(define signal-interrupts?)
(define pending-interrupt?)

(define (signal-interrupt!)
  (editor-beep)
  (temporary-message "Quit")
  (set! pending-interrupt? false)
  (^G-signal))

(define (with-editor-interrupts-from-X thunk)
  (fluid-let ((signal-interrupts? true)
	      (pending-interrupt? false))
    (thunk)))

(define (with-X-interrupts-enabled thunk)
  (bind-signal-interrupts? true thunk))

(define (with-X-interrupts-disabled thunk)
  (bind-signal-interrupts? false thunk))

(define (bind-signal-interrupts? new-mask thunk)
  (let ((old-mask))
    (dynamic-wind (lambda ()
		    (set! old-mask signal-interrupts?)
		    (set! signal-interrupts? new-mask)
		    (if (and new-mask pending-interrupt?)
			(signal-interrupt!)))
		  thunk
		  (lambda ()
		    (set! new-mask signal-interrupts?)
		    (set! signal-interrupts? old-mask)
		    (if (and old-mask pending-interrupt?)
			(signal-interrupt!))))))

;;;; Display description for X displays

(define X-display)
(define X-display-data)

(define (get-X-display)
  (if (and (not (unassigned? X-display-data))
	   X-display-data)
      X-display-data
      (let ((display (xterm-open-display false)))
	(set! X-display-data display)
	display)))      

(define (close-X-display)
  (xterm-close-all-displays)
  (set! X-display-data false)
  unspecific)

(define (initialize-package!)
  (set! X-display
	(make-display get-X-display
		      make-xterm-screen
		      make-xterm-input-port
		      with-editor-interrupts-from-X
		      with-X-interrupts-enabled
		      with-X-interrupts-disabled)))
