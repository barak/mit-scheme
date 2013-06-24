;;; -*- Scheme -*-

#| ******************************

MIT-XHOOKS defines the level of the system that handles event
processing, and the manipulation of UITK objects just above the X
level (defined in MIT-XLIB).  This layer will differ between
MIT-Scheme and Scheme-to-C

This file tries to include all the functions that actually call X so
that the other parts of the system can be rebuilt on a different
substrate. 

  ****************************** |#

#|
Not used?

(define (with-window display title desired-size context receiver)
  ;; Call RECEIVER with UITKwindow and actual size
  (let* ((window (create-top-level-x-window
		  display title desired-size context)))
    (XMapWindow display window)
    (report-window-size display window receiver)))

(define (report-window-size display window receiver)
  (get-window-attributes display window
   (lambda (x y width height . others)
     others
     (receiver (make-uitkwindow display window)
	       (make-size width height)
	       x
	       y))))


(define (create-top-level-x-window display title desired-size context)
  (let ((window
	 (XCreateSimpleWindow
	  display
	  (XDefaultRootWindow display)	; Parent is root window
	  0				; X
	  0				; Y
	  (size.width desired-size)	; Width
	  (size.height desired-size)	; Height
	  (context.BorderWidth context)
	  (context.Border context)
	  (context.Background context))))
    (XStoreName display window title)
    window))


(define (destroy-window w)
  (let ((Xwindow (UITKWindow.XWindow w)))
    (XDestroyWindow (UITKWindow.XDisplay w) Xwindow)))





|#


;;;;UITK main loop

#| In general, the system will have two threads running -- the
ordinary REP and the UITK thread, which processes events for the
widgets.

When an event is signalled, it is placed (at interrupt level) on a
queue, which is processed at user level by the UITK thread main loop
|#

;;;UITK thread will wake up at at least this interval, since it needs
;;;to clean up objects labelld for destruction by the GC, even if
;;;there are no events to process.

(define *UITK-INTERVAL* (* 30 1000))	; 30 seconds, in milliseconds

(define uitk-queue 'later)		;code that processes events
(define idle-queue 'later)		;not used in MIT version
(define the-agenda 'later)		;processing scheduled by AFTER-DELAY
(define uitk-thread 'later)
(define more-work-to-do #F)
(define uitk-timer #F)

#| #############################################################
This is some debugging stuff for probing the space usage.
|# 


(DEFINE LOOP-COUNTER 0)
(DEFINE EVENT-COUNTER 0)
(DEFINE MORE-COUNTER 0)
(DEFINE SUSPEND-COUNTER 0)
(DEFINE LOOP-TRACE)
(DEFINE READ-QUEUE-TRACE)
(DEFINE RUN-QUEUE-TRACE)
(define ALLOW-FREE-TRACE? #T)

(define (clear-counters!)
  (SET! LOOP-COUNTER 0)
  (SET! EVENT-COUNTER 0)
  (SET! MORE-COUNTER 0)
  (SET! SUSPEND-COUNTER 0)
  0)

(define ignore-repl #F)

(define (show-counters)
  (pp
   `(events: , event-counter loop: ,loop-counter more: ,more-counter suspend: ,suspend-counter)))

(define (make-free-trace n)
  (cons 0 (make-vector n #f)))

(define (copy-free-traces)
  (fluid-let ((allow-free-trace? #f))
    (vector (cons (car loop-trace) (vector-copy (cdr loop-trace)))
	    (cons (car read-queue-trace) (vector-copy (cdr read-queue-trace)))
	    (cons (car run-queue-trace) (vector-copy (cdr run-queue-trace))))))

(define (record-free-pointer trace)
  (if allow-free-trace?
      (let-syntax ((ucode-primitive
		    (sc-macro-transformer
		     (lambda (form environment)
		       environment
		       (apply make-primitive-procedure (cdr form))))))
	(vector-set! (cdr trace)
		     (car trace)
		     ((ucode-primitive primitive-get-free 1) 26))
	(set-car! trace
		  (if (fix:= (fix:+ (car trace) 1) (vector-length (cdr trace)))
		      0
		      (fix:+ (car trace) 1))))))

#| #############################################
end of debugging stuff

|#


(define (make-uitk-thread)
  (set! uitk-thread
	(create-thread (create-thread-continuation) thread-start))
  (kick-uitk-thread))

(define initial-thread-state 'later)

(define (thread-start)
  (call-with-current-continuation
   (lambda (start-up)
     (set! initial-thread-state start-up)
     (uitk-thread-main-loop))))

(define (restart-uitk)
  (restart-thread uitk-thread #T (lambda () (initial-thread-state 'go))))

(let-syntax ((last-reference
	      (sc-macro-transformer
	       (lambda (form environment)
		 (let ((variable (close-syntax (cadr form) environment)))
		   `(LET ((FOO ,variable))
		      (SET! ,variable #F)
		      FOO))))))

  (define (uitk-thread-main-loop)
    (define (flush-all-displays)
      (for-each flush-queued-output
		(protection-list-referenced-elements
		 display-protection-list)))
    (define (run thunk) (thunk))
    (SET! LOOP-COUNTER 0)
    (SET! EVENT-COUNTER 0)
    (SET! MORE-COUNTER 0)
    (SET! SUSPEND-COUNTER 0)
    (let process-loop ()
      (SET! LOOP-COUNTER (+ 1 LOOP-COUNTER))
      (block-thread-events)
      (set! more-work-to-do #F)
      ;; Read out the event/idle/delayed thunks
      (let ((events (read-and-empty-queue! uitk-queue))
	    (idle (read-and-empty-queue! idle-queue))
	    (delayed (read-and-empty-agenda! the-agenda))
	    )
	(unblock-thread-events)
	;;process the thinks that were read, and clear the variables so
	;; the thunks can GC away after they are run.
	(for-each run (last-reference events))
	(for-each run (last-reference idle))
	(for-each run (last-reference delayed))
	;; Allow tk to do its pending events (includes handling callbacks)
	(tk-doevents)
	;;check if a GC has occurred (the GC daemon sets the flag) and
	;;finalize the GC'd objects.
	(if (with-absolutely-no-interrupts
	     (lambda ()
	       (let ((result *UITK:GC-HAS-OCCURRED?*))
		 (set! *UITK:GC-HAS-OCCURRED?* #F)
		 result)))
	    (begin			; Clean up after GC
	      (finalize-uitk-objects)
	      (close-lost-displays-daemon)))
	(let ((more? (begin (block-thread-events) more-work-to-do)))
	  ;; MORE? is #T if work arrived while we were handling the
	  ;; previously grabbed event/idle thunks
	  (flush-all-displays)
	  (IF (OR MORE? IGNORE-REPL)
	      (begin
		(unblock-thread-events)
		(SET! MORE-COUNTER (+ 1 MORE-COUNTER))
		(process-loop))		; Don't give up CPU yet
	      (begin
		(let ((tk-wake-up (get-interval-to-tk-wakeup))
		      (delayed-wake-up (get-interval-to-next-delayed-event)))
		  ;;get time to wake up to for next TK event or
		  ;;delayed event
		  (let ((wake-up (if tk-wake-up
				     (if delayed-wake-up
					 (min tk-wake-up delayed-wake-up)
					 tk-wake-up)
				     delayed-wake-up)))
		    ;;flush the current timer event if there is one
		    ;;and register the next actual time to wake up
		    (if uitk-timer (deregister-timer-event uitk-timer))
		    (set! uitk-timer
			  (register-timer-event (if wake-up
						    (min wake-up *UITK-INTERVAL*)
						    *UITK-INTERVAL*)
						(lambda () (set! uitk-timer #F))))))
		;;now go to sleep. The timer event, or an X event,
		;;will wake us up.  We suspend with events still
		;;blocked to avoid an interrupt hole, whereby an
		;;event is delivered but doesn't wake us up.
		;;Suspending atomically unblocks events in the right
		;;way to prevent this.
		(suspend-current-thread)
		(SET! SUSPEND-COUNTER (+ SUSPEND-COUNTER 1))
		;;(allow-thread-event-delivery)
		(unblock-thread-events)
		(process-loop))))))
    ))

(define (with-uitk-thread-errors-captured thunk)
  (define newline-string "
")
  (call-with-current-continuation
   (lambda (exit-continuation)
     (fluid-let
	 ((standard-error-handler
	   (lambda (error-condition)
	     (fluid-let ((standard-error-handler standard-error-handler))
	       (newline)
	       (newline)
	       (display
		(string-append
		 ";Error in UITK thread:" newline-string
		 ";" (condition/report-string error-condition)
		 newline-string
		 ";To debug, type (debug (unhash "
		 (number->string
		  (hash (condition/continuation error-condition)))
		 "))"))
	       (newline)
	       (newline)
	       (exit-continuation 'punt-o-rama)))))
       (thunk)))))

;;;This forces the UITK thread to wake up
(define kick-uitk-thread
  (let ((*uitk-thread-kicked?* #F))
    (lambda ()
      (if (not *uitk-thread-kicked?*)
	  (begin
	    (set! *uitk-thread-kicked?* #T)
	    (when-idle!
	     ;; When-Idle! will make the thread awaken
	     (lambda ()
	       (set! *uitk-thread-kicked?* #F))))))))

;;; Redefine hook found in mit-xlib.  Running the UITK loop will flush
;;; all displays.
(define flush-display-hook kick-uitk-thread)

(define (when-idle! thunk)
  (signal-thread-event
   uitk-thread
   (lambda ()
     ;; Interrupt level
     (set! more-work-to-do #T)
     (enqueue! idle-queue thunk))))


;;; Registering events for processing

#| For each display connection, we have a permanently
   registered request to process input from a particular file.
   FORK-TO-WAIT-ON creates such a registration.  When events come in
   on the display connection, the CHILD-WORK-CODE is enqueued for
   user-level execution.  If the CHILD-WORK-CODE has been GCed away,
   then code to deregister the handler is executed.
|#

(define fork-to-wait-on
  (let ()
    ;; This group of procedures can NOT be lexically nested inside of
    ;; fork-to-wait-on because we want the link from the enqueued
    ;; thunk to child-work-code to be a weak pointer.  
    ;; Thus child-work-code should not be lexically visible to
    ;; these procedures.  If we had a strong pointer, then the
    ;; registry would point to the child work code and hence to the
    ;; application, so applications could never be GCd.
    (define (try-to-run weak)
      (lambda ()
	(let ((code (weak-car weak))
	      (wcdr (weak-cdr weak)))
	  (if (and code (not (scxl-destroyed? (weak-car wcdr))))
	      (begin
		;; Reinstall interrupt handler, then run user code
		(register-input-thread-event
		 (XConnectionNumber (weak-car wcdr))
		 uitk-thread (weak-cdr wcdr))
		(code))))))
    (define (call-if-still-there weak)
      ;; WEAK is a weak-list:
      ;;   (desired-code-thunk display #F)
      ;; In normal use, desired-code-thunk is #F iff the application
      ;; has vanished.  This code creates a procedure to run at
      ;; interrupt level, replaces the #F with the handler, and
      ;; returns the handler to the caller.
      (let ((result
	     (lambda ()
	       ;; Interrupt level
	       (let ((code (weak-car weak)))
		 (if code
		     (begin
		       (set! more-work-to-do #T)
		       (enqueue! uitk-queue (try-to-run weak))
		       'done
		       ))))))
	(weak-set-cdr! (weak-cdr weak) result)
	result))
    (lambda (display child-work-code child-idle-code)
      child-idle-code			; Not used by MIT Scheme
      (let ((file (XConnectionNumber display))
	    (weak (weak-cons child-work-code (weak-cons display #F))))
	(without-interrupts
	 (lambda ()
	   (register-input-thread-event
	    file uitk-thread (call-if-still-there weak))))))))

(define (destroy-registration registration)
  (deregister-input-thread-event registration)
  'OK)

(define (shut-down-event-server display-number)
  (deregister-input-descriptor-events (%XConnectionNumber display-number)))


;;;Delayed events

;;; Schedule an action to be done later in the UITK thread
;;; Implementation uses agendas from the 6.001 book


(define (after-delay delay action-thunk)	; delay in secs
  (let ((now (real-time-clock)))
    (signal-thread-event
     uitk-thread
     (lambda ()
       ;; Interrupt level
       (set! more-work-to-do #T)
       (add-to-agenda! (+ (* delay 1000) now)	; in msecs
		       action-thunk
		       the-agenda)))))

(define (make-agenda)
  (list '*agenda*))

(define (segments agenda) (cdr agenda))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (set-segments! agenda segments) (set-cdr! agenda segments))
(define (empty-segments? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
	(enqueue! (segment-queue (car segments))
		  action)
	(let ((rest (cdr segments)))
	  (if (or (null? rest)
		  (> (segment-time (car rest)) time))
	      (insert-new-time! time action agenda)
	      (add-to-segments! rest)))))
  (without-interrupts
   (lambda ()
     (let ((segs (segments agenda)))
       (if (null? segs)
	   (insert-new-time! time action agenda)
	   (add-to-segments! segs))))))


(define (insert-new-time! time action agenda)
  (let ((segs (segments agenda))
	(q (make-queue)))
    (enqueue! q action)
    (let ((new-segment (make-time-segment time q)))
      (if (null? segs)
	  (set-segments! agenda (list new-segment))
	  (set-cdr! segs
		    (cons new-segment (cdr segs)))))))

(define (read-and-empty-agenda! agenda)
  (let ((now (real-time-clock)))
    (define (find-all-events-up-to-now events)
      (if (empty-segments? agenda)
	  events
	  (let ((current-segment (first-segment agenda)))
	    (if (> (segment-time current-segment) now)
		events
		(let ((q (segment-queue current-segment)))
		  (if (empty-queue? q)
		      (begin (set-segments! agenda (rest-segments agenda))
			     (find-all-events-up-to-now events))
		      (find-all-events-up-to-now
		       (append events (list (dequeue! q))))))))))
    (without-interrupts
     (lambda ()
       (find-all-events-up-to-now '())))))

(define (empty-agenda? agenda)
  (without-interrupts
   (lambda ()
     (or (empty-segments? agenda)
	 (and (empty-queue? (segment-queue (first-segment agenda)))
	      (null? (rest-segments agenda)))))))

(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (current-time agenda)
  (without-interrupts
   (lambda ()
     (and (not (null? (segments agenda)))
	  (segment-time (first-segment agenda))))))

(define (get-interval-to-next-delayed-event)
  (let ((agenda-time (current-time the-agenda)))
    (and agenda-time
	 (- agenda-time (real-time-clock)))))

;;; make-enqueueable-thunk is unused.  Part of an alternate
;;; implementation, where the thunk doesn't hold on to the
;;; application, so that the application can GC away even if there are
;;; events scheduled. 

(define make-enqueueable-thunk
  (let ()
    (define (try-to-run weak)    
      (lambda ()
	(let ((code (weak-car weak)))
	  (if code
	  (debug-print code)
	  (debug-print 'vanished))
	  (if code (code)))))
    (lambda (thunk)
      (try-to-run (weak-cons thunk 'IGNORED)))))


;;;; UITK objects.  We almost never work with bare X objects.



;;;convert an Xevent (string) to a UITK event structure.  This defines
;;;the dispatch only.  The actual make-event procedures for the
;;;various event types are defined in UITK.scm 

(define XEvent-><Event>
  (let ((X-Event-Converters
	 (make-vector LASTEVENT
		      (lambda (event)
			(decode-unknown-event event
			  (lambda (type serial sent? display window)
			    type serial sent? display
			    (make-unknown-event 'UNUSUAL event window)))))))

    (define (key name)
      (lambda (e)
	(decode-key-event e
          (lambda (type serial sent? display window root subwindow
			time x y RootX RootY state keycode SameScreen?)
	    type serial sent? display root subwindow
	    time RootX RootY state keycode SameScreen?
	    (make-point-event name e window (Make-Point X Y))))))
    (vector-set! X-Event-Converters KeyPress (key 'KEY-PRESS))
    (vector-set! X-Event-Converters KeyRelease (key 'KEY-RELEASE))

    (define (button name)
      (lambda (e)
	(decode-button-event e
          (lambda (type serial sent? display window root subwindow
			time x y RootX RootY state button SameScreen?)
	    type serial sent? display root subwindow
	    time RootX RootY state button SameScreen?
	    (make-point-event name e window (Make-Point X Y))))))
    (vector-set! X-Event-Converters ButtonPress (button 'BUTTON-PRESS))
    (vector-set! X-Event-Converters ButtonRelease (button 'BUTTON-RELEASE))

    (define (motion name)
      (lambda (e)
	(decode-motion-event e
	 (lambda (type serial sent? display window root subwindow
		  time x y RootX RootY state IsHint SameScreen?)
	   type serial sent? display window root
	   subwindow time RootX RootY state IsHint SameScreen?
	   (make-point-event name e window (Make-Point X Y))))))
    (vector-set! X-Event-Converters MotionNotify (motion 'POINTER-MOTION))

    (define (crossing name)
      (lambda (e)
	(decode-crossing-event
	 e
	 (lambda (type serial sent? display window root subwindow
		       time x y RootX RootY mode detail SameScreen?
		       Focus? state)
	   type serial sent? display root subwindow
	   time RootX RootY mode detail SameScreen? Focus? state
	   (make-point-event name e window (Make-Point X Y))))))
    (vector-set! X-Event-Converters EnterNotify (crossing 'ENTER))
    (vector-set! X-Event-Converters LeaveNotify (crossing 'LEAVE))

    ; (vector-set! X-Event-Converters ConfigureNotify ...)
    ; (vector-set! X-Event-Converters FocusIn ...)
    ; (vector-set! X-Event-Converters FocusOut ...)
    ; (vector-set! X-Event-Converters KeymapNotify ...)

    (define (expose-fn type-name)
      (lambda (e)
	(decode-expose-event
	 e
	 (lambda (type serial sent? display
		       window x y width height count)
	   type serial sent? display count width height
	   (make-rectangle-event
	                  type-name e window (Make-Point x y)
			  width height)))))
    (vector-set! X-Event-Converters Expose (expose-fn 'EXPOSURE))

    ; (vector-set! X-Event-Converters GraphicsExpose ...)
    ; (vector-set! X-Event-Converters NoExpose ...)
    ; (vector-set! X-Event-Converters VisibilityNotify ...)
    ; (vector-set! X-Event-Converters CreateNotify ...)
    ; (vector-set! X-Event-Converters DestroyNotify ...)
    ; (vector-set! X-Event-Converters UnmapNotify ...)
    ; (vector-set! X-Event-Converters MapNotify ...)
    ; (vector-set! X-Event-Converters MapRequest ...)
    ; (vector-set! X-Event-Converters ReparentNotify ...)
    ; (vector-set! X-Event-Converters ConfigureNotify ...)
    ; (vector-set! X-Event-Converters ConfigureRequest ...)
    ; (vector-set! X-Event-Converters GravityNotify ...)
    ; (vector-set! X-Event-Converters ResizeRequest ...)
    ; (vector-set! X-Event-Converters CirculateNotify ...)
    ; (vector-set! X-Event-Converters CirculateRequest ...)
    ; (vector-set! X-Event-Converters PropertyNotify ...)
    ; (vector-set! X-Event-Converters SelectionClear ...)
    ; (vector-set! X-Event-Converters SelectionRequest ...)
    ; (vector-set! X-Event-Converters SelectionNotify ...)
    ; (vector-set! X-Event-Converters ColormapNotify ...)
    ; (vector-set! X-Event-Converters ClientMessage ...)
    ; (vector-set! X-Event-Converters MappingNotify ...)
    (lambda (XEvent)
      ((vector-ref X-Event-Converters (xevent-type XEvent))
       XEvent))))

;;This places the XEvent in the given string.  It uses the side effect
;;to avoid allocating a new string and generating garbage in the inner
;;event processing loop

(define (get-x-event display event-string)
  (if (zero? (XPending display))
      #F
      (XNextEvent! display event-string)))



;;; open a display and return the  numeric hook

(define (open-display)
  (let ((xdisplay (XOpenDisplay "")))
    (if (or (and (number? xdisplay) (zero? xdisplay))
	    (and (pair? xdisplay) (number? (cdr xdisplay))
		 (zero? (cdr xdisplay))))
	(error 'OPEN-DISPLAY "Could not open display")
	xdisplay)))

(define (string->color display)
  (lambda (string)
    (let ((result
	   (XAllocNamedColor display
			     (XDefaultColormap display
					       (XDefaultScreen display))
			     string)))
      ;; Result is (Status ScreenColor ExactColor)
      (if (zero? (car result))
	  #F				; Error status
	  (list-ref result 1)))))


#| Fonts don't work yet
(define (string->font display)
  (lambda (string)
    (XLoadFont display string)))
|#


;;;; Event-sensitive windows.  

(define (Generate-Events! UITKWindow mask)
  (let ((attributes (XMake-SetWindowAttributes))
	(window (UITKWindow.XWindow UITKWindow))
	(display (UITKWindow.XDisplay UITKWindow)))
    (XSetWindowAttributes-Event_Mask! attributes mask)
    (XChangeWindowAttributes display window CWEventMask attributes))
    (let ((result (XGetWindowAttributes display window)))
      (if (= (list-ref result 0) 0)
	  (error 'GENERATE-EVENTS!
		 "XGetWindowAttributes failed ~A" result)
	  (list-ref result 1))))


(define (handler->sensitivity handler)
  (case (car handler)
    ((#T) NoEventMask)
    ((KEY-PRESS) KeyPressMask)
    ((KEY-RELEASE) KeyReleaseMask)
    ((BUTTON-PRESS) ButtonPressMask)
    ((BUTTON-RELEASE) ButtonReleaseMask)
    ((ENTER) EnterWindowMask)
    ((CONFIGURE-NOTIFY) StructureNotifyMask)
    ((LEAVE) LeaveWindowMask)
    ((POINTER-MOTION) PointerMotionMask)
     ; (bit-or PointerMotionMask PointerMotionHintMask)
    ((BUTTON-1-MOTION) Button1MotionMask)
    ((BUTTON-2-MOTION) Button2MotionMask)
    ((BUTTON-3-MOTION) Button3MotionMask)
    ((BUTTON-4-MOTION) Button4MotionMask)
    ((BUTTON-5-MOTION) Button5MotionMask)
    ((BUTTON-MOTION) ButtonMotionMask)
     ; (bit-or ButtonMotionMask PointerMotionHintMask)
    ((KEYMAP-STATE) KeyMapStateMask)
    ((EXPOSURE) ExposureMask)
    ((VISIBITY-CHANGE) VisibilityChangeMask)
    ((STRUCTURE-NOTIFY) StructureNotifyMask)
    ;; I don't understand ResizeRedirect or substructure stuff
    ((FOCUS-CHANGE) FocusChangeMask)
    ((PROPERTY) PropertyChangeMask)
    ;; Ignoring colormap and owner grab
    (else (error 'HANDLER->SENSITIVITY "Unknown event type ~A" (car handler)))
    ))

(define (bit-or . integers)
  (bit-string->unsigned-integer
   (reduce bit-string-or (unsigned-integer->bit-string 32 0)
	   (map (lambda (n) (unsigned-integer->bit-string 32 n))
		integers))))

;;;; UITK level "X" calls.  

#| In UITK, we almost never work with bare X objects.  Rather there
are two levels of embedding.  The first is the "wrapper" which is used
for garbage collection (see MIT-Xlib).  This wrapped object is then
embedded in a UITK structure that bundles together associated
information.  (For example, a UITKWindow holds both an X window and
its associated X display.)  Thus, a user-level procedure such as
Drawline, operates on UITK windows.  It is defined in terms of a lower
level XDrawline (which operates on wrapped windows) which in tern is
defined in terms of the X primitive %XDrawline. |#


;;;; Graphics contexts

(define (make-simple-graphics-context uitkwindow)
  (let ((dpy (UITKWindow.XDisplay uitkwindow))
	(win (UITKWindow.XWindow uitkwindow)))
    (XCreateGC dpy win 0 (xmake-gcvalues))))

(define (make-colored-graphics-context uitkwindow color-string)
  (let ((gc (make-simple-graphics-context uitkwindow))
	(dpy (UITKWindow.XDisplay uitkwindow)))
    (let ((color ((string->color dpy) color-string)))
      (if (color? color)
	  (begin
	    (XSetForeground dpy gc color)
	    gc)
	  (error 'make-colored-graphics-context
		 "Can't convert color name to value ~A"
		 color-string)))))

(define (DrawArc uitkwindow gc X Y Width Height angle1 angle2)
  (XDrawArc (UITKWindow.XDisplay uitkwindow)
	    (UITKWindow.XWindow uitkwindow)
	    gc x y width height angle1 angle2))

(define (DrawLine uitkwindow gc X1 Y1 X2 Y2)
  (XDrawLine (UITKWindow.XDisplay uitkwindow)
		  (UITKWindow.XWindow uitkwindow)
		  gc x1 y1 x2 y2))

(define (DrawRectangle uitkwindow gc X Y Width Height)
  (XDrawRectangle (UITKWindow.XDisplay uitkwindow)
		  (UITKWindow.XWindow uitkwindow)
		  gc x y width height))


(define (FillRectangle uitkwindow gc X Y Width Height)
  (XFillRectangle (UITKWindow.XDisplay uitkwindow)
		  (UITKWindow.XWindow uitkwindow)
		  gc x y width height))

(define (FillArc uitkwindow gc X Y Width Height angle1 angle2)
  (XFillArc (UITKWindow.XDisplay uitkwindow)
	    (UITKWindow.XWindow uitkwindow)
	    gc x y width height angle1 angle2))

(define (ClearArea uitkwindow X Y width height exposures?)
  (XClearArea (UITKWindow.XDisplay uitkwindow)
	      (UITKWindow.XWindow uitkwindow)
	      x y width height exposures?))

(define (flush-queued-output display)
  (xflush display))

(define (GetDefaultValue display application-name variable)
  (XGetDefault display application-name variable))

(define (Decode-Button-Event event receiver)
  (let ((vect (make-vector 15)))
    (XDecodeButtonEvent event vect)
    (apply receiver (vector->list vect))))

(define (Decode-Configure-Event event receiver)
  (let ((vect (make-vector 13)))
    (XDecodeConfigureEvent event vect)
    (apply receiver (vector->list vect))))

(define (Decode-Crossing-Event event receiver)
  (let ((vect (make-vector 17)))
    (XDecodeCrossingEvent event vect)
    (apply receiver (vector->list vect))))

(define (Decode-Expose-Event event receiver)
  (let ((vect (make-vector 10)))
    (XDecodeExposeEvent event vect)
    (apply receiver (vector->list vect))))

(define (Decode-Key-Event event receiver)
  (let ((vect (make-vector 15)))
    (XDecodeKeyEvent event vect)
    (apply receiver (vector->list vect))))

(define (Decode-Motion-Event event receiver)
  (let ((vect (make-vector 15)))
    (XDecodeMotionEvent event vect)
    (apply receiver (vector->list vect))))

(define (Decode-Unknown-Event event receiver)
  (let ((vect (make-vector 5)))
    (XDecodeUnknownEvent event vect)
    (apply receiver (vector->list vect))))

(define (XEvent-Type xevent)
  (Decode-Unknown-Event xevent
    (lambda (type . others)
      others				; Ignored
      type)))

(define (Decode-Window-Attributes attributes receiver)
  (let ((vect (make-vector 23)))
    (XDecodeWindowAttributes attributes vect)
    (apply receiver (vector->list vect))))

(define (Get-Window-Attributes display window receiver)
  (let ((attributes (list-ref (XGetWindowAttributes display window) 1)))
    (Decode-Window-Attributes attributes receiver)))

(define (Rectangle->XRegion rectangle)
  (MakeXRegion (Point.X (UITKRectangle.Offset rectangle))
	       (Point.Y (UITKRectangle.Offset rectangle))
	       (UITKRectangle.Width rectangle)
	       (UITKRectangle.Height rectangle)))

(define (MakeXRegion x y width height)
  (let ((region (XCreateRegion)))
    (XUnionRectSpecsWithRegion! x y width height region region)
    region))

(define (IntersectXRegions x-region-1 x-region-2)
  (let ((region (XCreateRegion)))
    (XIntersectRegion! x-region-1 x-region-2 region)
    region))

(define (UnionXRegions x-region-1 x-region-2)
  (let ((region (XCreateRegion)))
    (XUnionRegion! x-region-1 x-region-2 region)
    region))

(define (CopyXRegion region)
  (UnionXRegions (XCreateRegion) region))

(define (SubtractXRegions x-region-1 x-region-2)
  (let ((region (XCreateRegion)))
    (XSubtractRegion! x-region-1 x-region-2 region)
    region))

(define (SetClipXRegion window graphics-context XRegion)
  (XSetRegion (UITKWindow.XDisplay window)
	      graphics-context
	      XRegion))

;;;process a mouse drag.
;;;keep reading motion events and process them 
;;;stop when there is a button release

;;;This procedure is included here because of the X calls.  

(define (mouse-drag surface on-motion)
  ;; *** Maybe this should take an "other events handler" ***
  (let* ((UITKWindow (DrawingSurface.UITKWindow surface))
	 (xdisplay (uitkwindow.xdisplay UITKWindow)))
    (without-interrupts
     (lambda ()
       (let loop ()
	 (let* ((x-event (XNextEvent xdisplay))) ;**blocks?
	   (Decode-Unknown-Event
	    x-event
	    (lambda (type serial sent? display window)
	      serial sent? display window
	      (cond ((eq? type MotionNotify)
		     (decode-motion-event x-event
		      (lambda (type serial sent? display window root subwindow
			       time x y RootX RootY state IsHint SameScreen?)
			type serial sent? display window root subwindow
			time RootX RootY state IsHint SameScreen?
			(on-motion (make-point x y))))
		     (loop))
		    ((eq? type ButtonRelease) 'endloop)
		    (else (loop)))))))))))

;;; GC of UITK objects
;;; this uses the protrction list mechanism implemented in MIT-Xlib

(define uitk-protection-list 'later)

(define (when-unreferenced obj thunk)
  (add-to-protection-list! uitk-protection-list obj thunk))

(define (finalize-uitk-objects)
  (clean-lost-protected-objects uitk-protection-list
				(lambda (thunk) (thunk))))

(define (finalize-uitk-objects-later)
  (set! *UITK:GC-HAS-OCCURRED?* #T)
  ;; (when-idle! finalize-uitk-objects)
  ;; Handled in the main UITK thread loop.  Also calls the scxl daemon
  ;; there.
  )

;;; In generating hash numbers for callbacks, etc., we use a private
;;; hash table, separate from the system one.

(define *our-hash-table* 'later)
(define *UITK:GC-HAS-OCCURRED?* #F)

#|

Shutting down the event server may be necessary in UITK even though
the event server is shut down as soon as the UITK application is
destroyed, because the applcation and display may vanish on the same
GC.

We must explicitly destroy the tk-widgets for this display (since Xlib
doesn't know about them). The TK widgets must be destroyed BEFORE the
display is closed.

|#

(define (initialize-uitk!)
  (set! uitk-protection-list (make-protection-list))
  ;; THIS SHOULD BE PUT BACK WHEN remove-gc-daemon! GETS WRITTEN
  ;; (remove-gc-daemon! close-lost-displays-daemon)
  (add-gc-daemon! finalize-uitk-objects-later)
  (set! uitk-queue (make-queue))
  (set! idle-queue (make-queue))
  (set! the-agenda (make-agenda))
  (make-uitk-thread)
  (SCXL-Install-XCloseDisplay-Callback shut-down-event-server)
  (set! *our-hash-table* (hash-table/make 4001))
  )

(initialize-uitk!)
