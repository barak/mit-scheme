;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/xterm.scm,v 1.11 1990/10/06 00:16:37 cph Exp $
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

;;;; X Terminal

(declare (usual-integrations))

(define-primitives
  (x-open-display 1)
  (x-close-all-displays 0)
  (x-close-display 1)
  (x-close-window 1)
  (x-display-flush 1)
  (x-display-process-events 2)
  (x-display-sync 2)
  (x-window-beep 1)
  (x-window-clear 1)
  (x-window-display 1)
  (x-window-set-event-mask 2)
  (x-window-set-icon-name 2)
  (x-window-set-name 2)
  (xterm-clear-rectangle! 6)
  (xterm-draw-cursor 1)
  (xterm-erase-cursor 1)
  (xterm-open-window 3)
  (xterm-restore-contents 6)
  (xterm-save-contents 5)
  (xterm-scroll-lines-down 7)
  (xterm-scroll-lines-up 7)
  (xterm-set-size 3)
  (xterm-write-char! 5)
  (xterm-write-cursor! 3)
  (xterm-write-substring! 7)
  (xterm-x-size 1)
  (xterm-y-size 1))

(define-structure (xterm-screen-state
		   (constructor make-xterm-screen-state (xterm display))
		   (conc-name xterm-screen-state/))
  (xterm false read-only true)
  (display false read-only true)
  (redisplay-flag true))

(define screen-list)

(define (make-xterm-screen #!optional geometry)
  (let ((screen
	 (let ((xterm
		(xterm-open-window (or (get-x-display)
				       (error "unable to open display"))
				   (and (not (default-object? geometry))
					geometry)
				   false)))
	   (x-window-set-event-mask xterm event-mask)
	   (make-screen (make-xterm-screen-state xterm
						 (x-window-display xterm))
			xterm-screen/beep
			xterm-screen/discard!
			xterm-screen/enter!
			xterm-screen/exit!
			xterm-screen/finish-update!
			xterm-screen/flush!
			xterm-screen/inverse-video!
			xterm-screen/modeline-event!
			xterm-screen/normal-video!
			xterm-screen/start-update!
			xterm-screen/subscreen-clear!
			xterm-screen/wipe!
			xterm-screen/write-char!
			xterm-screen/write-cursor!
			xterm-screen/write-substring!
			xterm-screen/write-substrings!
			(xterm-x-size xterm)
			(xterm-y-size xterm)))))
    (set! screen-list (cons screen screen-list))
    screen))

(define-integrable (screen-xterm screen)
  (xterm-screen-state/xterm (screen-state screen)))

(define-integrable (screen-display screen)
  (xterm-screen-state/display (screen-state screen)))

(define-integrable (screen-highlight screen)
  (if (screen-highlight? screen) 1 0))

(define-integrable (screen-redisplay-flag screen)
  (xterm-screen-state/redisplay-flag (screen-state screen)))

(define-integrable (set-screen-redisplay-flag! screen flag)
  (set-xterm-screen-state/redisplay-flag! (screen-state screen) flag))

(define (xterm->screen xterm)
  (let loop ((screens screen-list))
    (and (not (null? screens))
	 (if (eqv? xterm (screen-xterm (car screens)))
	     (car screens)
	     (loop (cdr screens))))))

(define (xterm-screen/start-update! screen)
  (xterm-erase-cursor (screen-xterm screen)))

(define (xterm-screen/finish-update! screen)
  (xterm-draw-cursor (screen-xterm screen))
  (if (screen-redisplay-flag screen)
      (begin
	(update-xterm-screen-names! screen)
	(set-screen-redisplay-flag! screen false)))
  (xterm-screen/flush! screen))

(define (xterm-screen/discard! screen)
  (set! screen-list (delq! screen screen-list))
  (x-close-window (screen-xterm screen)))

(define (xterm-screen/modeline-event! screen window type)
  window type				; ignored
  (set-screen-redisplay-flag! screen true))

(define (xterm-screen/enter! screen)
  screen				; ignored
  unspecific)

(define (xterm-screen/exit! screen)
  screen				; ignored
  unspecific)

(define (xterm-screen/inverse-video! screen)
  screen				; ignored
  unspecific)

(define (xterm-screen/normal-video! screen)
  screen				; ignored
  unspecific)

(define (xterm-screen/beep screen)
  (x-window-beep (screen-xterm screen))
  (xterm-screen/flush! screen))

(define-integrable (xterm-screen/flush! screen)
  (x-display-flush (screen-display screen)))

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
    (clip (screen-x-size screen) x bil biu
      (lambda (bxl ail aiu)
	(clip (screen-y-size screen) y bjl bju
	  (lambda (byl ajl aju)
	    (let loop ((y byl) (j ajl))
	      (if (fix:< j aju)
		  (begin
		    (xterm-write-substring! xterm
					    bxl y
					    (vector-ref strings j)
					    ail aiu
					    highlight)
		    (loop (fix:1+ y) (fix:1+ j)))))))))))

(define (clip axu x bil biu receiver)
  (let ((ail (fix:- bil x)))
    (if (fix:< ail biu)
	(let ((aiu (fix:+ ail axu)))
	  (cond ((not (fix:positive? x))
		 (receiver 0 ail (if (fix:< aiu biu) aiu biu)))
		((fix:< x axu)
		 (receiver x bil (if (fix:< aiu biu) aiu biu))))))))

(define (xterm-screen/subscreen-clear! screen xl xu yl yu)
  (xterm-clear-rectangle! (screen-xterm screen) xl xu yl yu
			  (screen-highlight screen)))

(define (xterm-screen/wipe! screen)
  (x-window-clear (screen-xterm screen)))

;;;; Input Port

(define (make-xterm-input-port screen)
  (input-port/copy xterm-input-port-template
		   (make-xterm-input-port-state (screen-display screen))))

(define-structure (xterm-input-port-state
		   (constructor make-xterm-input-port-state (display))
		   (conc-name xterm-input-port-state/))
  (display false read-only true)
  (buffer "")
  (index 0)
  ;; If we receive a non-keypress event while in a display update, we
  ;; stash it here and abort the update.
  (pending-event false))

(define (operation/char-ready? port interval)
  (let ((state (input-port/state port)))
    (if (< (xterm-input-port-state/index state)
	   (string-length (xterm-input-port-state/buffer state)))
	true
	(xterm-read-chars! state (+ (real-time-clock) interval)))))

(define (operation/peek-char port)
  (let ((state (input-port/state port)))
    (let ((buffer (xterm-input-port-state/buffer state))
	  (index (xterm-input-port-state/index state)))
      (if (< index (string-length buffer))
	  (string-ref buffer index)
	  (let ((buffer (xterm-read-chars! state false)))
	    (and buffer
		 (string-ref buffer 0)))))))

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
	  (let ((buffer (xterm-read-chars! state false)))
	    (and buffer
		 (begin
		   (set-xterm-input-port-state/index! state 1)
		   (string-ref buffer 0))))))))

(define (operation/print-self state port)
  (unparse-string state "from display ")
  (unparse-object state
		  (xterm-input-port-state/display (input-port/state port))))

(define xterm-input-port-template
  (make-input-port `((CHAR-READY? ,operation/char-ready?)
		     (DISCARD-CHAR ,operation/discard-char)
		     (PEEK-CHAR ,operation/peek-char)
		     (PRINT-SELF ,operation/print-self)
		     (READ-CHAR ,operation/read-char))
		   false))

;;;; Event Handling

(define (xterm-read-chars! state time-limit)
  (let ((display (xterm-input-port-state/display state)))
    (letrec
	((loop
	  (lambda ()
	    (let ((event (x-display-process-events display time-limit)))
	      (cond ((not event)
		     false)
		    ((= (vector-ref event 0) event-type:key-press)
		     (let ((buffer (vector-ref event 2)))
		       (set-xterm-input-port-state/buffer! state buffer)
		       (set-xterm-input-port-state/index! state 0)
		       (if signal-interrupts?
			   (let ((^g-index
				  (string-find-previous-char buffer #\BEL)))
			     (if ^g-index
				 (begin
				   (set-xterm-input-port-state/index!
				    state (1+ ^g-index))
				   (signal-interrupt!)))))
		       buffer))
		    (else
		     (process-special-event event))))))
	 (process-special-event
	  (lambda (event)
	    (let ((handler (vector-ref event-handlers (vector-ref event 0)))
		  (screen (xterm->screen (vector-ref event 1))))
	      (if (and handler screen)
		  (begin
		    (let ((continuation (screen-in-update? screen)))
		      (if continuation
			  (begin
			    (set-xterm-input-port-state/pending-event! state
								       event)
			    (continuation false))))
		    (handler screen event))))
	    (loop))))
      (let ((event (xterm-input-port-state/pending-event state)))
	(if event
	    (begin
	      (set-xterm-input-port-state/pending-event! state false)
	      (process-special-event event))
	    (loop))))))

(define signal-interrupts?)
(define pending-interrupt?)

(define (signal-interrupt!)
  (editor-beep)
  (temporary-message "Quit")
  (set! pending-interrupt? false)
  (^G-signal))

(define (with-editor-interrupts-from-x thunk)
  (fluid-let ((signal-interrupts? true)
	      (pending-interrupt? false))
    (thunk)))

(define (with-x-interrupts-enabled thunk)
  (bind-signal-interrupts? true thunk))

(define (with-x-interrupts-disabled thunk)
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

;;; The values of these flags must be equal to the corresponding event
;;; types in "microcode/x11base.c"

(define-integrable event-type:button-down 0)
(define-integrable event-type:button-up 1)
(define-integrable event-type:configure 2)
(define-integrable event-type:enter 3)		
(define-integrable event-type:focus-in 4)	
(define-integrable event-type:focus-out 5)		
(define-integrable event-type:key-press 6)		
(define-integrable event-type:leave 7)		
(define-integrable event-type:motion 8) 
(define-integrable number-of-event-types 9)

;; This mask contains button-down, button-up, configure, focus-in, and
;; key-press.
(define-integrable event-mask #x057)

(define event-handlers
  (make-vector number-of-event-types false))

(define-integrable (define-event-handler event-type handler)
  (vector-set! event-handlers event-type handler))

(define-event-handler event-type:configure
  (lambda (screen event)
    (let ((x-size (vector-ref event 2))
	  (y-size (vector-ref event 3)))
      (if (not (and (= x-size (screen-x-size screen))
		    (= y-size (screen-y-size screen))))
	  (begin
	    (set-screen-x-size! screen x-size)
	    (set-screen-y-size! screen y-size)
	    (send (screen-root-window screen) ':set-size! x-size y-size)
	    (update-screen! screen true))))))

(define-event-handler event-type:button-down
  (lambda (screen event)
    (send (screen-root-window screen) ':button-event!
	  (make-down-button (vector-ref event 4))
	  (vector-ref event 2)
	  (vector-ref event 3))
    (update-screen! screen false)))

(define-event-handler event-type:button-up
  (lambda (screen event)
    (send (screen-root-window screen) ':button-event!
	  (make-up-button (vector-ref event 4))
	  (vector-ref event 2)
	  (vector-ref event 3))
    (update-screen! screen false)))

(define-event-handler event-type:focus-in
  (lambda (screen event)
    event
    (if (not (selected-screen? screen))
	(command-reader/reset-and-execute
	 (lambda ()
	   (select-screen screen))))))

(define x-display-type)
(define x-display-data)

(define (get-x-display)
  (or x-display-data
      (let ((display (x-open-display false)))
	(set! x-display-data display)
	display)))

(define (initialize-package!)
  (set! screen-list '())
  (set! x-display-type
	(make-display-type 'X
			   get-x-display
			   make-xterm-screen
			   make-xterm-input-port
			   with-editor-interrupts-from-x
			   with-x-interrupts-enabled
			   with-x-interrupts-disabled))
  (set! x-display-data false)
  unspecific)