;;; -*-Scheme-*-
;;;
;;;	$Id: win32.scm,v 1.7 1997/01/02 04:39:45 cph Exp $
;;;
;;;	Copyright (c) 1994-97 Massachusetts Institute of Technology
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

;;;;Win32 Terminal
;;; package (edwin screen win32)

(declare (usual-integrations))

(define-primitives
  (win32-screen-char-dimensions 1)
  (win32-screen-clear-rectangle! 6)
  (win32-screen-create! 2)
  (win32-screen-current-focus 0)
  (win32-screen-get-event 1)
  (win32-screen-invalidate-rect! 5)
  (win32-screen-move-cursor! 3)
  (win32-screen-set-background-color! 2)
  (win32-screen-set-default-font! 1)
  (win32-screen-set-font! 2)
  (win32-screen-set-foreground-color! 2)
  (win32-screen-set-icon! 2)
  (win32-screen-show-cursor! 2)
  (win32-screen-size 1)
  (win32-screen-vertical-scroll! 6)
  (win32-screen-write-char! 5)
  (win32-screen-write-substring! 7))

(define-integrable event:process-output 16)
(define-integrable event:process-status 32)
(define-integrable event:inferior-thread-output 64)

(define win32-screens '())

;;(define (debug . details)
;;  (pp details console-output-port))

(define-structure (win32-screen-state
		   (constructor make-win32-screen-state (handle))
		   (conc-name state/))
  (handle false read-only true)
  (cursor-x -1) ; cached position, -1 if we dont know
  (cursor-y -1) ; ditto
  ;; This rect is the bounding box of a sequence of updates.  RECT-TOP is #F
  ;; if no box has been established, which implies that the screen needs no
  ;; update.
  (rect-top #F)
  (rect-bottom 0)
  (rect-right 0)
  (rect-left 0)
  (redisplay-title? #F)
  (name #F))

(define-integrable (screen-redisplay-title? screen)
  (state/redisplay-title? (screen-state screen)))

(define-integrable (set-screen-redisplay-title?! screen flag)
  (set-state/redisplay-title?! (screen-state screen) flag))

(define (make-win32-screen)
  (let* ((window (win32-screen-create! 0 win32-screen-features-mask))
	 (icon   (load-icon (get-handle 0) "EDWIN_ICON"))
	 (width.height (win32-screen-size window)))
    (set-window-text window "Edwin")
    (win32-screen-set-icon! window icon)
    ;; The first time (re)entering edwin we make the master tty window iconic:
    (if (null? win32-screens)
	(show-window (get-handle 1) SW_SHOWMINNOACTIVE))
    (let ((screen
	   (make-screen (make-win32-screen-state window)
			win32-screen/beep
			win32-screen/clear-line!
			win32-screen/clear-rectangle!
			win32-screen/clear-screen!
			win32-screen/discard!
			win32-screen/enter!
			win32-screen/exit!
			win32-screen/flush!
			win32-screen/modeline-event!
			false
			win32-screen/scroll-lines-down!
			win32-screen/scroll-lines-up!
			win32-screen/wrap-update!
			win32-screen/write-char!
			win32-screen/write-cursor!
			win32-screen/write-substring!
			8
			(car width.height)
			(cdr width.height))))
      (set! win32-screens (cons screen win32-screens))
      (set! input-screen #F)
      ;;(debug 'CREATE screen)
      screen)))

(define (win32-screen/beep screen)
  screen
  (message-beep -1))

(define (expand-rect screen top bottom left right)
  (define-integrable (min u v)  (if (fix:< u v) u v))
  (define-integrable (max u v)  (if (fix:> u v) u v))
  (define (set-rect! state top bottom left right)
    (set-state/rect-top!    state top)
    (set-state/rect-bottom! state bottom)
    (set-state/rect-left!   state left)
    (set-state/rect-right!  state right))

  (let ((state  (screen-state screen)))
    (if (state/rect-top state)
	(set-rect! state
		   (min top    (state/rect-top state))
		   (max bottom (state/rect-bottom state))
		   (min left   (state/rect-left state))
		   (max right  (state/rect-right state)))
	(set-rect! state top bottom left right))))

(define (invalidate-invalid-region! screen)
  (let ((state  (screen-state screen)))
    (if (state/rect-top state)
	(begin
	  (win32-screen-invalidate-rect!
	   (screen->handle screen)
	   (state/rect-top state)
	   (fix:+ (state/rect-bottom state) 1)
	   (state/rect-left state)
	   (fix:+ (state/rect-right state) 1))))))

(define-integrable (set-screen-cursor-position! screen x y)
  (set-state/cursor-x! (screen-state screen) x)
  (set-state/cursor-y! (screen-state screen) y))

(define (win32-screen/clear-line! screen x y first-unused-x)
  (win32-screen-clear-rectangle! (screen->handle screen)
				 x first-unused-x y (fix:1+ y)
				 0))

(define (win32-screen/clear-rectangle! screen xl xu yl yu highlight)
  (win32-screen-clear-rectangle! (screen->handle screen)
				 xl xu yl yu
				 (if highlight 1 0)))

(define (win32-screen/clear-screen! screen)
  (let* ((handle  (screen->handle screen))
	 (w.h     (win32-screen-size handle)))
    (win32-screen-clear-rectangle! handle 0 (car w.h) 0 (cdr w.h)  0)))

(define (win32-screen/discard! screen)
  ;;(debug 'DISCARD screen)
  (destroy-window (screen->handle screen))
  (set! win32-screens (delq screen win32-screens)))

(define (win32-screen/enter! screen)
  (set! input-screen #F)
  (set-screen-cursor-position! screen -1 -1)
  (set-active-window (screen->handle screen))
  (win32-screen-show-cursor! (screen->handle screen) #T))

(define (win32-screen/exit! screen)
  (win32-screen-show-cursor! (screen->handle screen) #F)
  (set! input-screen #F))

(define (win32-screen/modeline-event! screen window type)
  window type				; ignored
  (set-screen-redisplay-title?! screen true))

(define (win32-screen/scroll-lines-down! screen xl xu yl yu amount)
  (and #F
       (win32-screen-vertical-scroll! (screen->handle screen)
				      xl xu yl yu (fix:+ yl amount))))

(define (win32-screen/scroll-lines-up! screen xl xu yl yu amount)
  (and #F
       (win32-screen-vertical-scroll! (screen->handle screen)
				      xl xu amount yu 0)
       (win32-screen-vertical-scroll! (screen->handle screen)
				      xl xu yl yu (fix:- yl amount))))

(define (win32-screen/flush! screen)
  ;; Win32 API call causes any pending painting to be done
  (update-window (screen->handle screen))
  #F)

(define (win32-screen/wrap-update! screen thunk)
  (let ((finished? false))
    (dynamic-wind
     (lambda ()
       (set-state/rect-top! (screen-state screen) #F))
     (lambda ()
       (let ((result (thunk)))
	 (set! finished? result)
	 result))
     (lambda ()
       ;; invalidate the region that this update affected, and then flush
       (invalidate-invalid-region! screen)
       (if (and finished? (screen-redisplay-title? screen))
	   (begin
	     (update-win32-screen-name! screen)
	     (set-screen-redisplay-title?! screen false)))
       (win32-screen/flush! screen)))))

(define (win32-screen/write-char! screen x y char highlight)
  (win32-screen-write-char! (screen->handle screen) x y
			    (char->integer char)
			    (if highlight 1 0))
  (if (char-graphic? char)
      (set-screen-cursor-position! screen (fix:+ x 1) y)
      (set-screen-cursor-position! screen -1 -1)))

(define (win32-screen/write-substring! screen x y string start end highlight)
  ;;(debug 'substring x y string start end)
  (win32-screen-write-substring!
   (screen->handle screen) x y string start end
   (if highlight 1 0))
  (expand-rect screen x (fix:+ x (fix:- end start)) y y))

(define (win32-screen/write-cursor! screen x y)
  (let ((state  (screen-state screen)))
    (if (or (not (fix:= (state/cursor-x state) x))
	    (not (fix:= (state/cursor-y state) y)))
	(let ((handle  (screen->handle screen)))
	  (win32-screen-move-cursor! handle x y)
	  (set-screen-cursor-position! screen x y)))))

;; Mask bits:  VK coded special keys, Edwin mode,
;;    mouse, key, resize and close events
(define-integrable win32-screen-features-mask #x140F)

(define (screen->handle screen)
  (if (memq screen win32-screens)
      (state/handle (screen-state screen))
      (error "Screen has unexpectedly vanished" screen)))

(define (handle->win32-screen handle)
  (list-search-positive win32-screens
    (lambda (screen) (eqv? handle (state/handle (screen-state screen))))))

(define win32-display-type)

(define (win32-screen-available?)
  (implemented-primitive-procedure? win32-screen-create!))

(define (initialize-package!)
  (set! win32-display-type
	(make-display-type 'win32
			   true        ; multiple screens?
			   win32-screen-available?
			   (lambda geometry
			     geometry
			     (make-win32-screen))
			   get-win32-input-operations
			   with-editor-interrupts-from-win32
			   with-win32-interrupts-enabled
			   with-win32-interrupts-disabled))
  (add-event-receiver! event:before-exit
		       (lambda ()
			 (for-each screen-discard! win32-screens)))
  unspecific)

(define (with-editor-interrupts-from-win32 receiver)
  (fluid-let ((signal-interrupts? #t))
    (dynamic-wind
     (lambda () '())
     (lambda () (receiver (lambda (thunk) (thunk)) '()))
     (lambda () '()))))

(define (with-win32-interrupts-enabled thunk)
  (with-signal-interrupts true thunk))

(define (with-win32-interrupts-disabled thunk)
  (with-signal-interrupts false thunk))

(define (with-signal-interrupts enabled? thunk)
  (let ((old))
    (dynamic-wind (lambda ()
		    (set! old signal-interrupts?)
		    (set! signal-interrupts? enabled?)
		    unspecific)
		  thunk
		  (lambda ()
		    (set! enabled? signal-interrupts?)
		    (set! signal-interrupts? old)
		    unspecific))))

(define (signal-interrupt!)
  (editor-beep)
  (temporary-message "Quit")
  (^G-signal))

(define signal-interrupts? #f)

(define-integrable (some-bits? mask item)
  (not (fix:= 0 (fix:and mask item))))

(define (process-mouse-event screen event)
  screen
  (make-input-event 'BUTTON
		    execute-button-command
		    screen
		    ((if (= (vector-ref event 5) 0)
			 make-down-button
			 make-up-button)
		     (cond ((some-bits? #x1 (vector-ref event 4))  0)
			   ((some-bits? #x2 (vector-ref event 4))  2)
			   ((some-bits? #x4 (vector-ref event 4))  1)
			   (else 0)))
		    (vector-ref event 2)
		    (vector-ref event 1)))

(define (process-resize-event screen event)
  event
  (make-input-event 'SET-SCREEN-SIZE
		    (lambda (screen)
		      (let ((w.h (win32-screen-size (screen->handle screen))))
			(if (not (and (= (car w.h) (screen-x-size screen))
				      (= (cdr w.h) (screen-y-size screen))))
			    (begin
			      (set-screen-size! screen (car w.h) (cdr w.h))
			      (update-screen! screen #T)))))
		    screen))

(define (process-close-event screen event)
  event
  (cond ((screen-deleted? screen)  #F)
	((= (length win32-screens) 1)
	 (make-input-event 'EXIT save-buffers-and-exit #F "Scheme"
			   exit-scheme))
	(else
	 (make-input-event 'DELETE-SCREEN delete-screen! screen))))

(define (process-key-event event)
  (let* ((key        (vector-ref event 5))
	 (cont-state (vector-ref event 4))
	 (alt?       (some-bits? #x1 cont-state))
	 (control?   (some-bits? #x2 cont-state))
	 (shift?     (some-bits? #x4 cont-state)))
    (let ((result
	   (cond ((fix:= key -1)
		  (let ((vk-code (vector-ref event 2))
			(bucky-bits
			 (+ (if alt? 1 0)     ; M-
			    (if control? 2 0) ; C-
			    (if shift? 4 0)   ; S-
			    )))
		    (win32-make-special-key vk-code bucky-bits)))
		 ((and control? alt?)
		  (char-control-metafy (integer->char key)))
		 (alt?
		  (char-metafy (integer->char key)))
		 ;;((and control? (eq? key 32))
		 ;; #\c-space)
		 (control?
		  (char-controlify (integer->char key)))
		 (else
		  (integer->char key)))))
      result)))

(define (get-win32-input-operations screen)

  screen ; ignored

  (set! input-screen #f)

  (define-integrable (win32-resize-event? event)
    (and (vector? event) (fix:= (vector-ref event 0) 1)))

  (define-integrable (win32-key-event? event)
    (and (vector? event) (fix:= (vector-ref event 0) 2)))

  (define-integrable (win32-mouse-event? event)
    (and (vector? event) (fix:= (vector-ref event 0) 4)))

  (define-integrable (win32-close-event? event)
    (and (vector? event) (fix:= (vector-ref event 0) 8)))

  (define-integrable (change-event? event)  (fix:fixnum? event))

  (define (process-event event)
    (cond ((win32-key-event? event)
	   (let ((key (process-key-event event)))
	     (if (and signal-interrupts?
		      (eq? key #\BEL))
		 (begin
		   (signal-interrupt!)
		   #f)
		 key)))
	  ((win32-mouse-event? event)
	   (process-mouse-event input-screen event))
	  ((win32-resize-event? event)
	   (process-resize-event input-screen event))
	  ((win32-close-event? event)
	   (process-close-event input-screen event))
	  ((input-event? event)
	   event)
	  (else #f)))

  (define (pce-event flag)
    (make-input-event (if (eq? flag 'FORCE-RETURN) 'RETURN 'UPDATE)
		      update-screens!
		      #f))

  (define (get-next-event block?)
    (let loop ()
      (let ((event  (read-event block?)))
	(cond ((not event)
	       #F)
	      ((change-event? event)
	       (let ((flag  (process-change-event event)))
		 (if flag
		     (pce-event flag)
		     (loop))))
	      (else
	       (or (process-event event)
		   (loop)))))))

  (define (guarantee-result)
    (or (get-next-event #T)
	(error "#F returned from blocking read")))

  (let* ((pending-result #F)

	 (probe
	  (lambda (block?)
	    (let ((result  (get-next-event block?)))
	      (if result
		  (set! pending-result result))
	      result)))

	 (halt-update?
	  (lambda ()
	    (or pending-result
		(probe 'IN-UPDATE))))
	 (peek-no-hang
	  (lambda ()
	    (or pending-result
		(probe #F))))
	 (peek
	  (lambda ()
	    (or pending-result
		(let ((result  (guarantee-result)))
		  (set! pending-result result)
		  result))))
	 (read
	  (lambda ()
	    (cond (pending-result
		   => (lambda (result)
			(set! pending-result #F)
			result))
		  (else
		   (guarantee-result))))))

    (values halt-update?
	    peek-no-hang
	    peek
	    read)))

;; The INPUT-SCREEN is the current screen from which we are processing
;; input events.  When a different screen (or some window from sone
;; other application) may have been selected, INPUT-SCREEN is set to
;; #F.  This causes READ-EVENT-1 to hunt for a screen from which it
;; can take input events.  This is a crock.  An improvement would be
;; to put the input events for Edwin screens into a common queue, and
;; invent an new `select-screen' event That in turn would require
;; implementing the queues separately from the window but it would
;; move the place at which the process should be suspended to a single
;; place (WIN32-SCREEN-GET-EVENT), allowing a WIN32(C?) event and

(define input-screen)

(define-integrable interrupt-mask/gc+win32
  ;; Include INTERRUPT-BIT/GLOBAL-1 so that messages are dispatched to
  ;; the screen by the interrupt-handler.
  ;;(fix:or interrupt-mask/gc-ok interrupt-bit/global-1)
  15)

(define (read-event block?)
  (let ((handle (and input-screen (screen->handle input-screen))))
    (if (eq? block? 'IN-UPDATE)
	(read-event-2 handle)
	(read-event-1 handle block?))))

(define (read-event-1 handle block?)
  (or (read-event-2 handle)
      (let loop ()
	(let ((mask (set-interrupt-enables! interrupt-mask/gc+win32)))
	  (cond (inferior-thread-changes?
		 (set-interrupt-enables! mask)
		 event:inferior-thread-output)
		((process-output-available?)
		 (set-interrupt-enables! mask)
		 event:process-output)
		((process-status-changes?)
		 (set-interrupt-enables! mask)
		 event:process-status)
		(else
		 (let ((handle* (win32-screen-current-focus)))
		   (if (eqv? handle handle*)
		       (let ((flag
			      (test-for-input-on-descriptor
			       ;; console-channel-descriptor here
			       ;; means "input from message queue".
			       console-channel-descriptor block?)))
			 (set-interrupt-enables! mask)
			 (case flag
			   ((#F) #f)
			   ((PROCESS-STATUS-CHANGE) event:process-status)
			   ((INTERRUPT) (loop))
			   (else (read-event-1 handle block?))))
		       (let ((screen* (handle->win32-screen handle*)))
			 (set-interrupt-enables! mask)
			 (if screen*
			     (begin
			       (set! input-screen screen*)
			       (make-input-event 'SELECT-SCREEN
						 select-screen
						 screen*))
			     (and block?
				  (read-event-1 handle block?))))))))))))

(define (read-event-2 handle)
  (and handle
       (let ((mask (set-interrupt-enables! interrupt-mask/gc+win32)))
	 (let ((result (win32-screen-get-event handle)))
	   (set-interrupt-enables! mask)
	   result))))

(define (process-change-event event)
  (cond ((fix:= event event:process-output) (accept-process-output))
	((fix:= event event:process-status) (handle-process-status-changes))
	((fix:= event event:inferior-thread-output) (accept-thread-output))
	(else (error "Illegal change event:" event))))

(define-integrable (screen-name screen)
  (state/name (screen-state screen)))

(define-integrable (set-screen-name! screen name)
  (set-state/name! (screen-state screen) name))

(define (win32-screen/set-name! screen name)
  (let ((name* (screen-name screen)))
    (if (or (not name*) (not (string=? name name*)))
	(begin
	  (set-screen-name! screen name)
	  (set-window-text (screen->handle screen) name)))))

(define (win32-screen/set-font! screen font)
  (let ((x-size (screen-x-size screen))
	(y-size (screen-y-size screen)))
    (win32-screen-set-font! (screen->handle screen) font)
    (win32-screen/set-size! screen x-size y-size)))

(define (win32-screen/set-icon! screen icon)
  (win32-screen-set-icon! (screen->handle screen) icon))

(define (win32-screen/set-foreground-color! screen color)
  (win32-screen-set-foreground-color! (screen->handle screen) color))

(define (win32-screen/set-background-color! screen color)
  (win32-screen-set-background-color! (screen->handle screen) color))

(define (win32-screen/set-size! screen width height)
  (let ((handle (screen->handle screen)))
    (let ((rect
	   (let ((x.y (win32-screen-char-dimensions handle)))
	     (make-rect 0 0 (* width (car x.y)) (* height (cdr x.y))))))
      (adjust-window-rect rect
			  WS_OVERLAPPEDWINDOW
			  (not (= 0 (get-menu handle))))
      (set-window-pos handle 0 0 0
		      (- (rect/right rect) (rect/left rect))
		      (- (rect/bottom rect) (rect/top rect))
		      (+ SWP_NOMOVE SWP_NOZORDER)))))

(define (win32-screen/set-position! screen x y)
  (set-window-pos (screen->handle screen) 0 x y 0 0
		  (+ SWP_NOSIZE SWP_NOZORDER)))

(define (win32-screen/get-position screen)
  (let ((rect (make-rect 0 0 0 0)))
    (get-window-rect (screen->handle screen) rect)
    (values (rect/left rect) (rect/top rect)
	    (rect/right rect) (rect/bottom rect))))

(define (win32-screen/get-client-size screen)
  (let ((rect (make-rect 0 0 0 0)))
    (get-client-rect (screen->handle screen) rect)
    (values (rect/right rect) (rect/bottom rect))))