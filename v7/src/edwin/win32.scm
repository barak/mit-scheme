;;; -*-Scheme-*-
;;;
;;;	$Id: win32.scm,v 1.1 1994/10/25 01:46:12 adams Exp $
;;;
;;;	Copyright (c) 1989-93 Massachusetts Institute of Technology
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
  (nt-get-event 1)
  (nt-peek-event 1)
  (prim-win32-screen/clear-rectangle 6)
  (prim-win32-screen/discard 1)
  (prim-win32-screen/invalidate-rect 5)
  (prim-win32-screen/vertical-scroll 6)
  (prim-win32-screen/screen-writechar 5)
  (prim-win32-screen/screen-move-cursor 3)
  (prim-win32-screen/screen-x-size 1)
  (prim-win32-screen/screen-y-size 1)
  (prim-win32-screen/create-screen 3)
  (prim-win32-screen/write-substring 7)
  (prim-win32-screen/show-cursor 2))

(define-integrable event:process-output 16)
(define-integrable event:process-status 32)
(define-integrable event:inferior-thread-output 64)

(define win32-screens '())

(define-structure (win32-screen-state
		   (constructor make-win32-screen-state (handle))
		   (conc-name win32-screen-state/))
  (handle false read-only true)
  (cursor-x 0) ; cached position, -1 if we dont know
  (cursor-y 0) ; ditto
  ;; This rect is the bounding box of a sequence of updates.  RECT-TOP is #F
  ;; if no box has been established.
  (rect-top #F)
  (rect-bottom 0)
  (rect-right 0)
  (rect-left 0)
  (update? false)
  (state 'OPEN))
  

(define (make-win32-screen handle)
  (let ((screen
	 (make-screen (make-win32-screen-state handle)
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
		      (prim-win32-screen/screen-x-size handle)
		      (prim-win32-screen/screen-y-size handle))))
    (set! win32-screens (cons screen win32-screens))
    screen))

(define (win32-screen/beep screen)
  screen
  (message-beep -1))


(define-integrable (set-rect! state top bottom left right)
  (set-win32-screen-state/rect-top!    state top)
  (set-win32-screen-state/rect-bottom! state bottom)
  (set-win32-screen-state/rect-left!   state left)
  (set-win32-screen-state/rect-right!  state right))

(define (expand-rect screen top bottom left right)
  ;; Defined here because the system ones are not integrated:
  (define-integrable (min u v)  (if (fix:< u v) u v))
  (define-integrable (max u v)  (if (fix:> u v) u v))
  (let ((state  (screen-state screen)))
    (if (win32-screen-state/rect-top state)
	(set-rect! state
		   (min top    (win32-screen-state/rect-top state))
		   (max bottom (win32-screen-state/rect-bottom state))
		   (min left   (win32-screen-state/rect-left state))
		   (max right  (win32-screen-state/rect-right state)))
	(set-rect! state top bottom left right))))


(define (flush-invalid-region screen)
  (let ((state  (screen-state screen)))
    (if (win32-screen-state/rect-top state)
	(begin
	  (prim-win32-screen/invalidate-rect
	   (win32-screen->handle screen)
	   (win32-screen-state/rect-top state)
	   (+ (win32-screen-state/rect-bottom state) 1)
	   (win32-screen-state/rect-left state)
	   (+ (win32-screen-state/rect-right state) 1))
	  (set-win32-screen-state/update?! state #f)))))


(define-integrable (set-screen-cursor-position! screen x y)
  (set-win32-screen-state/cursor-x! (screen-state screen) x)
  (set-win32-screen-state/cursor-y! (screen-state screen) y))


(define (win32-screen/clear-line! screen x y first-unused-x)
  (prim-win32-screen/clear-rectangle (win32-screen->handle screen)
				     x first-unused-x y (fix:1+ y)
				     0))

(define (win32-screen/clear-rectangle! screen xl xu yl yu highlight)
  (prim-win32-screen/clear-rectangle (win32-screen->handle screen)
				     xl xu yl yu
				     (if highlight 1 0)))

(define (win32-screen/clear-screen! screen)
  (prim-win32-screen/clear-rectangle (win32-screen->handle screen)
				     0 (win32-x-size screen)
				     0 (win32-y-size screen)
				     0))

(define (win32-screen/discard! screen)
  (set! win32-screens (delq screen win32-screens))
  (destroy-window (win32-screen->handle screen)))

(define (win32-screen/enter! screen)
  (set-screen-cursor-position! screen -1 -1)
  (prim-win32-screen/show-cursor (win32-screen->handle screen) #T))

(define (win32-screen/exit! screen)
  screen
  unspecific)  

(define (win32-screen/flush! screen)
  screen
  unspecific) 

(define (win32-screen/modeline-event! screen window type)
  window type screen)

(define (win32-screen/scroll-lines-down! screen xl xu yl yu amount)
  (and #F
       (prim-win32-screen/vertical-scroll (win32-screen->handle screen)
					  xl xu yl yu (+ yl amount))))

(define (win32-screen/scroll-lines-up! screen xl xu yl yu amount)
  (and #F
       (prim-win32-screen/vertical-scroll (win32-screen->handle screen)
					  xl xu amount yu 0)
       (prim-win32-screen/vertical-scroll (win32-screen->handle screen)
					  xl xu yl yu (- yl amount))))


(define (win32-screen/wrap-update! screen thunk)
  (let ((finished? false))
    (dynamic-wind
     (lambda ()
       (prim-win32-screen/show-cursor (win32-screen->handle screen) #F)
       (set-win32-screen-state/rect-top! (screen-state screen) #F))
     (lambda ()
       (let ((result (thunk)))
	 (set! finished? result)
	 result))
     (lambda ()
       (if finished?
	   (begin
	     (prim-win32-screen/show-cursor (win32-screen->handle screen) #T)))
       (if (win32-screen-state/update? (screen-state screen))
	   (flush-invalid-region screen))))))

(define (win32-screen/write-char! screen x y char highlight)
  (prim-win32-screen/screen-writechar (win32-screen->handle screen) x y 
				      (char->integer char)
				      (if highlight 1 0))
  (if (char-graphic? char)
      (set-screen-cursor-position! screen (+ x 1) y)
      (set-screen-cursor-position! screen -1 -1)))

(define (win32-screen/write-substring! screen x y string start end highlight)
  (if (= start end) '()
      (begin
	(prim-win32-screen/write-substring
	 (win32-screen->handle screen) x y string start end
	 (if highlight 1 0))
	(win32-screen/write-cursor! screen (+ x (- end start)) y)
	(expand-rect screen x (+ x (- end start)) y y)
	(set-win32-screen-state/update?! (screen-state screen) #t))))


;;(define (win32-screen/write-cursor! screen x y)
;;  (begin 
;;    (prim-win32-screen/screen-move-cursor (win32-screen->handle screen) x y)
;;    (set-screen-cursor-position! screen x y)))

(define (win32-screen/write-cursor! screen x y)
  (let ((state  (screen-state screen)))
    (if (or (not (= (win32-screen-state/cursor-x state) x))
	    (not (= (win32-screen-state/cursor-y state) y)))
	(let ((handle  (win32-screen->handle screen)))
	  (prim-win32-screen/screen-move-cursor handle x y)
	  (set-screen-cursor-position! screen x y)
	  (prim-win32-screen/invalidate-rect handle x (+ x 1) y (+ y 1))))))


(define (win32-x-size screen)
  (prim-win32-screen/screen-x-size (win32-screen->handle screen)))

(define (win32-y-size screen)
  (prim-win32-screen/screen-y-size (win32-screen->handle screen)))

(define-integrable (win32-key-event? event)
  (and (vector? event)
       (fix:= (vector-ref event 0) 2)))

(define (win32-mouse-event? event)
  (and (vector? event)
       (fix:= (vector-ref event 0) 4)))

(define-integrable (win32-resize-event? event)
  (and (vector? event)
       (fix:= (vector-ref event 0) 1)))

(define-integrable (change-event? event)
  (fix:fixnum? event))

(define-integrable (win32-close-event? event)
  (and (vector? event)
       (fix:= (vector-ref event 0) 8)))

(define (win32-screen->handle screen)
  (if (memq screen win32-screens)
      (win32-screen-state/handle (screen-state screen))
      (let ((window (prim-win32-screen/create-screen
		     0 2751 (get-handle 1))))
	(set-window-text window "Edwin")
	(make-win32-screen window)
	window)))

(define win32-display-type)

(define (initialize-package!)
  (set! win32-display-type 
	(make-display-type 'win32
			   true
			   true
			   (lambda geometry
			     geometry
			     (let ((window (prim-win32-screen/create-screen
					    0 2751 (get-handle 1))))
			       (set-window-text window "Edwin")
			       (make-win32-screen window)))
			   get-win32-input-operations
			   with-editor-interrupts-from-win32
			   with-win32-interrupts-enabled
			   with-win32-interrupts-disabled))
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

(define-integrable (some-bits? mask item) (not (fix:= 0 (fix:and mask item))))

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
  (set-screen-size! screen
		    (win32-x-size screen)
		    (win32-y-size screen))
  (update-screen! screen #f) 
  #f)

(define (process-close-event screen event)
  event
  (and (not (screen-deleted? screen))
       (make-input-event 'DELETE-SCREEN delete-screen! screen)))


(define (give-up-time-slice!)
  (if (other-running-threads?)
      (yield-current-thread)       ; yield to scheme threads
      (sleep 1)))                  ; ... or to win32 threads

;;(define (win32-char event)
;;  (let ((key (vector-ref event 5))
;;	(cont-state (vector-ref event 4)))
;;    (cond ((not (fix:= (fix:and cont-state 514) 0))
;;	   (char-metafy (integer->char key)))
;;	  ((and (not (fix:= (fix:and cont-state 514) 0))
;;		(fix:= (fix:and cont-state 8) 8))
;;	   (char-control-metafy (integer->char key)))
;;	  ((fix:= (fix:and cont-state 8) 8)
;;	   (integer->char key))
;;	  (else
;;	   (integer->char key)))))


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
      ;;(frob-trace (with-output-to-string
      ;;              (lambda ()
      ;;                (display event)
      ;;                (display "   ")
      ;;                (display `((m ,alt?) (c ,control?) (s ,shift?)))
      ;;                (display "\r\n=> ")
      ;;                (write result))))
      result)))

	
(define (get-win32-input-operations screen)
  (let ((screen-handle  (win32-screen->handle screen))
	(pending-result #F))
    (let* ((read-event 
	    (lambda (block?)
	      (let ((event (read-event-1 screen-handle block?)))
		event)))

	   (process-event
	    (lambda (event)
	      (cond ((win32-key-event? event)
		     (let ((key (process-key-event event)))
		       (if (and signal-interrupts?
				(eq? key #\BEL))
			   (begin
			     (signal-interrupt!)
			     #f)
			   key)))
		    ((win32-mouse-event? event)
		     (process-mouse-event screen event))
		    ((win32-resize-event? event)
		     (process-resize-event screen event))
		    ((win32-close-event? event)
		     (process-close-event screen event))
		    (else #f))))

	   (get-next-event
	    (lambda (block?)
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
			     (loop))))))))

	   (probe
	    (lambda (block?)
	      (let ((result  (get-next-event block?)))
		(if result
		    (set! pending-result result))
		result)))

	   (guarantee-result
	    (lambda ()
	      (or (get-next-event #T)
		  (error "#F returned from blocking read"))))

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
	      read))))


(define (read-event-1 screen-handle block?)
  (let loop ()
    (let ((interrupt-mask (set-interrupt-enables! 5 #|interrupt-mask/gc-ok|# )))
      (if (eq? block? 'IN-UPDATE)
	  (let ((result  (nt-get-event screen-handle)))
	    (set-interrupt-enables! interrupt-mask)
	    result)
	  (cond (inferior-thread-changes?
		 (set-interrupt-enables! interrupt-mask)
		 event:inferior-thread-output)
		((process-output-available?)
		 (set-interrupt-enables! interrupt-mask)
		 event:process-output)
		((process-status-changes?)
		 (set-interrupt-enables! interrupt-mask)
		 event:process-status)
		(else
		 (let ((result (nt-get-event screen-handle)))
		   (set-interrupt-enables! interrupt-mask)
		   ;; in lieu of blocking we give up our timeslice.
		   (if (and (not result)
			    block?)
		       (begin
			 (give-up-time-slice!)
			 (loop))
		       result))))))))

(define (pce-event flag)
  (make-input-event (if (eq? flag 'FORCE-RETURN) 'RETURN 'UPDATE)
		    update-screens!
		    #f))


(define (process-change-event event)
  (cond ((fix:= event event:process-output) (accept-process-output))
	((fix:= event event:process-status) (handle-process-status-changes))
	((fix:= event event:inferior-thread-output) (accept-thread-output))
	(else (error "Illegal change event:" event))))
