;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/xterm.scm,v 1.9 1990/08/31 20:13:06 markf Exp $
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

;;;; X Terminal

(declare (usual-integrations))

(define-primitives
  (x-open-display 1)
  (x-close-display 1)
  (x-close-all-displays 0)
  (x-close-window 1)
  (x-window-beep 1)
  (x-window-flush 1)
  (x-window-read-event-flags! 1)
  (xterm-open-window 3)
  (xterm-x-size 1)
  (xterm-y-size 1)
  (xterm-set-size 3)
  (xterm-write-cursor! 3)
  (xterm-write-char! 5)
  (xterm-write-substring! 7)
  (xterm-clear-rectangle! 6)
  (xterm-read-chars 2)
  (xterm-button 1)
  (xterm-pointer-x 1)
  (xterm-pointer-y 1)
  (x-dequeue-global-event 0)
  (x-window-pixel-coord->char-coord 2)
  (x-set-window-name 2)
  (x-set-icon-name 2))

(define-structure (xterm-screen-state
		   (constructor make-xterm-screen-state (xterm))
		   (conc-name xterm-screen-state/))
  (xterm false read-only true)
  (highlight 0))

(define (make-xterm-screen #!optional geometry)
  (let* ((xterm (xterm-open-window (or (get-x-display)
				       (error "unable to open display"))
				   (and (not (default-object? geometry))
					geometry)
				   false))
	 (screen (make-screen (make-xterm-screen-state xterm)
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
			      xterm-screen/discard!)))
    (add-to-xterm-screen-alist xterm screen)
    screen))

(define-integrable (screen-xterm screen)
  (xterm-screen-state/xterm (screen-state screen)))

(define-integrable (screen-highlight screen)
  (xterm-screen-state/highlight (screen-state screen)))

(define xterm-screen-alist '())

(define (add-to-xterm-screen-alist xterm screen)
  (set! xterm-screen-alist (cons (cons xterm screen) xterm-screen-alist)))

(define (xterm->screen xterm)
  (let ((entry (assv xterm xterm-screen-alist)))
    (and entry (cdr entry))))

(define-integrable (set-screen-highlight! screen highlight)
  (set-xterm-screen-state/highlight! (screen-state screen) highlight))

(define (xterm-screen/start-update! screen)
  screen				;ignored
  unspecific)

(define (xterm-screen/finish-update! screen)
  (x-window-flush (screen-xterm screen)))

(define (xterm-screen/beep screen)
  (let ((xterm (screen-xterm screen)))
    (x-window-beep xterm)
    (x-window-flush xterm)))

(define (xterm-screen/flush! screen)
  (x-window-flush (screen-xterm screen)))

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
  (if (not (eq? screen (current-screen)))
      (change-screen screen))
  unspecific)

(define (xterm-screen/exit! screen)
  screen				; ignored
  unspecific)

(define (xterm-screen/discard! screen)
  screen				; ignored
  (close-x-display))

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
	       (let ((screen (xterm-input-port-state/screen state)))
		 (if (zero? interval)
		     (xterm-screen/read-chars screen 0)
		     (let loop ((interval interval))
		       (let ((result
			      (xterm-screen/read-chars screen interval)))
			 (if (integer? result)
			     (loop result)
			     result)))))))
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
    (let ((buffer (xterm-screen/read-chars screen #f)))
      (and buffer
	   (begin
	     (check-for-interrupts! state buffer index)
	     (string-ref buffer 0))))))

(define (xterm-screen/read-chars screen interval)
  (let ((result (xterm-read-chars (screen-xterm screen) interval)))
    (if (and (not (screen-in-update? screen))
	     (xterm-process-events!))
	(update-screens! false))
    result))

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


;;; The values of these flags must be equal to the corresponding 
;;; event types in microcode/x11.h

(define-integrable x-event-type:unknown 0)
(define-integrable x-event-type:resized 1)
(define-integrable x-event-type:button-down 2)
(define-integrable x-event-type:button-up 3)		
(define-integrable x-event-type:focus_in 4)	
(define-integrable x-event-type:focus_out 5)		
(define-integrable x-event-type:enter 6)		
(define-integrable x-event-type:leave 7)		
(define-integrable x-event-type:motion 8)		
(define-integrable x-event-type:configure 9)			
(define-integrable x-event-type:map 10)		
(define-integrable x-event-type:unmap 11)		
(define-integrable x-event-type:expose 12)
(define-integrable x-event-type:no_expose 13) 
(define-integrable x-event-type:graphics_expose 14) 
(define-integrable x-event-type:key_press 15) 

(define-integrable xterm-number-of-event-types 16)

(define-integrable event-type car)
(define-integrable event-xterm cadr)
(define-integrable event-extra cddr)

(define (xterm-process-events!)
  (let ((event (x-dequeue-global-event)))
    (and event
	 (let loop ((event event))
	   (if (null? event)
	       true
	       (let ((event-type (event-type event))
		     (screen (xterm->screen (event-xterm event)))
		     (extra (event-extra event)))
		 (let ((handler (x-event-type->handler event-type)))
		   (if handler (apply handler screen extra))
		   (if (eq? event-type x-event-type:key_press)
		       true
		       (loop (x-dequeue-global-event))))))))))

(define xterm-event-handlers
  (make-vector xterm-number-of-event-types false))

(define-integrable (x-event-type->handler event-type)
  (vector-ref xterm-event-handlers event-type))

(define (define-xterm-event-handler event handler)
  (vector-set! xterm-event-handlers event handler)
  unspecific)

(define-xterm-event-handler x-event-type:configure
  (lambda (screen)
    (let ((xterm (screen-xterm screen)))
      (send (screen-window screen) ':set-size!
	    (xterm-x-size xterm)
	    (xterm-y-size xterm)))))

(define-xterm-event-handler x-event-type:button-down
  (lambda (screen button x y)
    (let ((character-coords
	  (x-window-pixel-coord->char-coord
	   (screen-xterm screen)
	   (cons x y))))
      (send (screen-window screen) ':button-event!
	    (button-downify button)
	    (car character-coords)
	    (cdr character-coords)))))

(define-xterm-event-handler x-event-type:button-up
  (lambda (screen button x y)
    (let ((character-coords
	  (x-window-pixel-coord->char-coord
	   (screen-xterm screen)
	   (cons x y))))
      (send (screen-window screen) ':button-event!
	    (button-upify button)
	    (car character-coords)
	    (cdr character-coords)))))

(define-xterm-event-handler x-event-type:focus_in
  (lambda (screen)
    (xterm-screen/enter! screen)))


(define button1-down)
(define button2-down)
(define button3-down)
(define button4-down)
(define button5-down)
(define button1-up)
(define button2-up)
(define button3-up)
(define button4-up)
(define button5-up)

;;;; Display description for X displays

(define x-display-type)
(define x-display-data false)

(define (get-x-display)
  (or x-display-data
      (let ((display (x-open-display false)))
	(set! x-display-data display)
	display)))      

(define (close-x-display)
  (x-close-all-displays)
  (set! x-display-data false)
  unspecific)

(define x-display-type-name 'X)

(define (initialize-package!)
  (set! x-display-type
	(make-display-type x-display-type-name
			   get-x-display
			   make-xterm-screen
			   make-xterm-input-port
			   with-editor-interrupts-from-x
			   with-x-interrupts-enabled
			   with-x-interrupts-disabled))
  (initialize-buttons! 5)
  (set! button1-down (button-downify 0))
  (set! button2-down (button-downify 1))
  (set! button3-down (button-downify 2))
  (set! button4-down (button-downify 3))
  (set! button5-down (button-downify 4))
  (set! button1-up (button-upify 0))
  (set! button2-up (button-upify 1))
  (set! button3-up (button-upify 2))
  (set! button4-up (button-upify 3))
  (set! button5-up (button-upify 4))
  unspecific)
