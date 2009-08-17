;;;;; -*- Scheme -*-
;;;;; Basic objects for the Scheme User Interface Tool Kit
;;;; MIT/GNU Scheme Version derived from Scheme-To-C version 1.2

;;;; $Id: 9601c4aa42c00c4a8823fe3f6669c198952bb188 $

;;;; Application objects

(define (application->TKMainWindow obj) (Application%.TKMainWindow obj))
(define (application->Display obj) (Application%.Xdisplay obj))

(define (valid-color-for-application? app color-string)
  ((string->color (application->display app)) color-string))

(define (valid-color? color-string)
  ;; For default application
  ((string->color (application->display *the-default-application*))
   color-string))

(define (make-top-level-geometry-callback kid)
  ;; Is the TK-TOP-LEVEL-WINDOW required any more? --Jim
  (let ((my-screen-area #f))
    (lambda (configure-event)
      (Decode-Configure-Event
       Configure-Event
       (lambda (type serial send_event display event window x y width
		     height border-width above override-redirect)
	 type serial send_event display event window x y
	 border-width above override-redirect ; Not used
	 (let ((new-area (make-UITKRectangle
					; (make-point x y)
			  (make-point 0 0)
			  (make-size width height))))
	   (if (not (screen-area= new-area my-screen-area))
	       (begin
		 (%XClearWindow display window)
		 (assign-screen-area! kid #F)
		 (assign-screen-area! kid new-area)))
	   (set! my-screen-area new-area))
	 'DONE)))))

(define (valid-child? object)
  (or (interactor%? object)
      (box%? object)
      (arraybox%? object)
      (shape%? object)
      (tkwidget%? object)))

(define (application-add-child! application to-be-managed . child-name)
  ;; Name is an optional string that overrides the application's
  ;; name for providing a title to the child window
  (if (not (valid-child? to-be-managed))
      (error "APPLICATION-ADD-CHILD!: Bad UIObj" to-be-managed))
  (one-parent-only! to-be-managed application)
  (let ((really-adding? #F)
	(new-entry (cons to-be-managed 'TK-Top-Level-Window)))
    (update-locked-list!
     (Application%.%child-windows application)
     (lambda (kids)
       (if (assq to-be-managed kids)
	   kids
	   (begin
	     (set! really-adding? #T)
	     (cons new-entry kids)))))
    (if really-adding?
	(let ((Xdisplay (Application%.Xdisplay application))
	      (context (Application%.context application))
	      (top-level-geometry-callback
	       (make-top-level-geometry-callback to-be-managed))
	      (TKMainW
	       (application->TKMainWindow application))
	      (TKW (make-ToolKitWindow application #F #F)))
	  (let* ((drawing-surface (make-DrawingSurface TKW #F))
		 (tlwindow
		  (tk-create-top-level-window
		   TKMainW
		   (hash top-level-geometry-callback *our-hash-table*))))
	    (set-cdr! new-entry tlwindow)
	    (set-ToolKitWindow.TK-Window! TKW tlwindow)
	    (set-ToolKitWindow.Top-Level-Geometry-Callback!
	     TKW top-level-geometry-callback)
	    (set-context! to-be-managed context)
	    (assign-drawing-surface! to-be-managed drawing-surface)
	    (let ((desired-size (get-desired-size to-be-managed))
		  (window-name (tkwin.pathname tlwindow)))
	      (tk-invoke-command
	       'WM TKMainW
	       (list "title" window-name
		     (if (and (pair? child-name)
			      (string? (car child-name)))
			 (car child-name)
			 (Application%.application-name application))))
	      (let ((tlwindow-width
		     (number->string (or (size.width desired-size) 0)))
		    (tlwindow-height
		     (number->string (or (size.height desired-size) 0))))

		#|
		(tk-invoke-command
		 'WM TKMainW
		 (list "minsize" window-name "1" "1"))
		|#

		(tk-invoke-command
		 'WM TKMainW
		 (list "minsize" window-name tlwindow-width tlwindow-height))
		;;X signals errors if we don't do this, but I'm damned if I know why
		(tk-invoke-command
		 'WM TKMainW
		 (list "geometry" window-name
		       (string-append tlwindow-width "x" tlwindow-height)))
		)
	      (let ((kill-me
		     (lambda ()
		       (application-remove-destroyed-child! application to-be-managed)
		       )))
		(tk-invoke-command
		 'BIND TKMainW
		 (list window-name "<Destroy>"
		       (string-append
			"SchemeCallBack "
			(number->string (hash kill-me *our-hash-table*)))))
		(UIObj-protect-from-gc! to-be-managed kill-me))
	      ;; Events start being generated and handled in
	      ;; the other thread as soon as we map this
	      ;; window!  We must map the window before doing
	      ;; the MAKE-UITKWINDOW below, because TK
	      ;; doesn't create the X window until the widget
	      ;; is mapped.
	      (our-with-thread-mutex-locked
	       'add-child-locks-out-others
	       *event-processing-mutex*
	       (lambda ()
		 (tk-map-window tlwindow)
		 (let ((UITKWindow
			(make-uitkwindow
			 Xdisplay
			 (wrap-window Xdisplay
				      (tkwin.window tlwindow)))))
		   (set-DrawingSurface.UITKWindow!
		    drawing-surface UITKWindow)
		   (assign-drawing-surface!
		    to-be-managed drawing-surface))
		 ;; UITKWindow changed and some objects will
		 ;; need that rather than just the TK top
		 ;; level window.
		 (assign-screen-area!
		  to-be-managed
		  (make-UITKRectangle (make-point 0 0)
				      (tkwin->size tlwindow)))
		 ))))
	  #|
	  ;;let window resize when kid requests resize
	  ;;but this means that size is determined by kid -- not WM
	  ;;do we want both kinds of windows??
	  (on-geometry-change!
	   to-be-managed 'APPLICATION
	   (lambda (old-screen-area new-screen-area)
	     old-screen-area		;not used
	     (if (eq? new-screen-area #T) ;instigated by child
		 (let* ((desired-size (get-desired-size to-be-managed))
			(tlwindow-width
			 (number->string (or (size.width desired-size) 0)))
			(tlwindow-height
			 (number->string (or (size.height desired-size) 0)))
			(window-name
			 (tkwin.pathname
			  (ToolkitWindow.TK-window
			   (DrawingSurface.ToolkitWindow
			    (drawing-surface to-be-managed))))))
		   (tk-invoke-command
		    'WM TKMainW
		    (list "minsize" window-name tlwindow-width tlwindow-height))
		   (tk-invoke-command
		    'WM TKMainW
		    (list "geometry" window-name
			  (string-append tlwindow-width "x" tlwindow-height)))))))

	  |#

	  (on-death! to-be-managed 'APPLICATION
		     (lambda ()
		       (application-remove-child! application to-be-managed)))))
    'ADDED))
;;; More methods for Applications below
  
;;; More methods for Applications objects

(define (application-remove-child! Application to-be-unmanaged)
  ;; This is called by the generic REMOVE-CHILD! procedure.
  (let ((entry (with-locked-list
		(application%.%child-windows application)
		(lambda (kids) (assq to-be-unmanaged kids)))))
    (if (not entry)
	'NOT-A-CHILD
	(let ((tlwindow (cdr entry)))
	  ;; Just kill the TK Top Level window.  This will cause us to get a
	  ;; <Destroy> back from TK, which we process with
	  ;; Application-Remove-Destroyed-Child!, below.
	  (tk-invoke-command 'DESTROY
			     (Application->TKMainWindow Application)
			     (list (tkwin.pathname tlwindow)))
	  'REMOVED))))

(define (application-remove-destroyed-child! Application to-be-unmanaged)
  (if (not (valid-child? to-be-unmanaged))
      (error "APPLICATION-REMOVE-DESTROYED-CHILD!: Bad UIObj" to-be-unmanaged))
  (if (let ((OK? #T))
	(update-locked-list! (Application%.%child-windows Application)
			     (lambda (kids)
			       (if (assq to-be-unmanaged kids)
				   (del-assq! to-be-unmanaged kids)
				   (begin (set! OK? #F)
					  kids))))
	OK?)
      (begin
	(assign-drawing-surface! to-be-unmanaged 'RETRACTED)
	(forget! Application to-be-unmanaged)
	'REMOVED)
      'NOT-A-CHILD))

(define (make-destroy-<application>-related-objects disp registration mainwindow)
  ;; This code should not have lexical reference to the
  ;; Application, since it will run only after the Application
  ;; has vanished.
  (lambda ()
    (destroy-registration registration)
    (destroy-associated-tk-widgets (->xdisplay disp))
    (destroy-all-sensitive-surfaces-from-display disp)
    (tk-kill-application mainwindow)
    'done))

(define (application-maker application-name dsp TKmain context children code)
  ;; Can't be nested in MAKE-APPLICATION because it would lexically
  ;; capture the list of kids!
  (make-application%
   (make-UIObjInternals application-add-child!
			application-remove-child!
			UIObj-set-context!
			'invalid-application-1	; UIObj-assign-screen-area!
			'invalid-application-2	; UIObj-assign-drawing-surface!
			'invalid-application-3	; UIObj-point-within?
			'invalid-application-4	; UIObj-rectangle-overlaps?
			'invalid-application-5	; UIObj-handle-event
			'invalid-application-6	; UIObj-get-desired-size
			'invalid-application-7	; UIObj-assigned-screen-area
			'invalid-application-8	; UIObj-used-screen-area
			'invalid-application-9	; UIObj-set-assigned-screen-area!
			'invalid-application-10	; UIObj-set-used-screen-area!
			'invalid-application-11); UIObj-assign-glue!
   children
   code
   application-name
   dsp
   TKMain
   context))

(define (make-application application-name . kids)
  (let* ((dsp (open-display))
	 (context (create-default-context application-name dsp))
	 (me 'later)
	 (event-string (%XMake-Event)))
    (define (service-display-connection)
      ;; This code is run asynchronously when data arrives from
      ;; the display connection
      (define (process-event event)
	(for-each
	 (lambda (kid) (handle-event kid event))
	 (with-locked-list (Application%.%child-windows me)
			   (lambda (kids)
			     (let loop ((rest kids)
					(handled-by '()))
			       (cond ((null? rest) (reverse handled-by))
				     ((event-within? (caar rest) event)
				      (loop (cdr rest)
					    (cons (caar rest) handled-by)))
				     (else (loop (cdr rest) handled-by))))))))
      (let loop ((nextevent (get-x-event dsp event-string)))
	(if nextevent
	    (begin
	      (set! EVENT-COUNTER (+ 1 EVENT-COUNTER))
	     
	      (our-with-thread-mutex-locked
	       'process-event *event-processing-mutex*
	       (lambda ()
		 (if (not (tk-completely-handles-event? nextevent))
		     (process-event (XEvent-><Event> nextevent)))))
	      
	      (do-tk-callbacks)

	      (loop (get-x-event dsp event-string)))
	    'done))
      )
    (define (idle-work)
					; Not actually used by MIT version
      (debug-print 'idle-work 'never called!!!!)
      (flush-queued-output dsp)
      (tk-doevents))
    (let ((TKMainWindow (tk-init dsp)))
      (set! me (application-maker application-name dsp TKMainWindow
				  context (make-locked-list)
				  service-display-connection))
      (add-widget-list-for-display-number! (->xdisplay dsp))
      (for-each (lambda (kid) (add-child! me kid)) kids)
      (when-unreferenced
       me
       (make-destroy-<application>-related-objects
	dsp
	(fork-to-wait-on dsp service-display-connection idle-work)
	TKMainWindow))
      me))
  )

;;;; Interactive Geometry handlers ... low level version

(define (interactor-add-child! interact to-be-managed)
  (define (find-handler event-type handlers)
    ;; Returns a list of all handlers for this event-type
    (let loop ((rest handlers))
      (cond ((null? rest) '())
	    ((eq? event-type (caar rest))
	     (cons (cadr (car rest)) (loop (cdr rest))))
	    (else (loop (cdr rest))))))

  (if (not (valid-child? to-be-managed))
      (error "INTERACTOR-ADD-CHILD!: Bad UIObj" to-be-managed))
  (let ((sensitive-surfaces (Interactor%.sensitive-surface-map interact)))
    (if (not (assq to-be-managed sensitive-surfaces))
	(let* ((ss (create-sensitive-surface to-be-managed
					     (Interactor%.handlers interact)))
	       (entry `(,to-be-managed ,ss)))
	  (set-Interactor%.sensitive-surface-map! interact
						  (cons entry sensitive-surfaces))
	  (on-event! to-be-managed interact
		     (lambda (event)
		       (let* ((handlers (Interactor%.handlers interact))
			      (applicable-handlers
			       (find-handler (event.type event) handlers)))
			 (cond ((not (null? applicable-handlers))
				(for-each (lambda (handler) (handler event))
					  applicable-handlers))
			       ((assq #T handlers)
				=> (lambda (entry) ((cadr entry) event)))
			       (else #F)))
		       (event! interact event)))
	  (on-geometry-change! to-be-managed interact
			       (lambda (old-screen-area new-screen-area)
				 (if (and (not old-screen-area)
					  (not new-screen-area))
				     ;; When a drawing surface is set.
				     (set! ss
					   (change-sensitive-surface!
					    ss
					    to-be-managed))
				     (set-car! (cdr entry) ss))))))))

(define (interactor-remove-child! interact was-managed)
  (if (not (valid-child? was-managed))
      (error "INTERACTOR-REMOVE-CHILD!: Bad UIObj" to-be-managed))
  (forget! was-managed interact)
  (let ((ss (assq was-managed (Interactor%.sensitive-surface-map interact))))
    (if ss (destroy-sensitive-surface was-managed (cadr ss)))))

;; Interactor Maker
(define (interactor-maker alist-of-handlers)
  (make-Interactor%
   (make-UIObjInternals interactor-add-child!
			interactor-remove-child!
			UIObj-set-context! ; Defaults
			UIObj-assign-screen-area!
			UIObj-assign-drawing-surface!
			UIObj-point-within?
			UIObj-rectangle-overlaps?
			UIObj-handle-event
			UIObj-get-desired-size
			UIObj-assigned-screen-area
			UIObj-used-screen-area
			UIObj-set-assigned-screen-area!
			UIObj-set-used-screen-area!
			'invalid)
   alist-of-handlers))

(define (make-interactor objects alist-of-handlers)
  ;; Constructor for interactors
  (let ((me (interactor-maker alist-of-handlers)))
    (for-each (lambda (object) (add-child! me object)) objects)
    me)
  )

;;;; Higher level interactors

(define (handle-exposure object receiver)
  ;; Receiver will be called with the exposed rectangle
  (make-interactor
   (list object)
   `((EXPOSURE
      ,(lambda (event)
	 (receiver
	  (Make-UITKRectangle (Event.Offset Event)
			      (Make-Size (Event.Width Event)
					 (Event.Height Event))))))))
  'OK)

(define (handle-button-grab object which-buttons receiver)
  ;; Receiver is called with the buttons that were actually down and a
  ;; "while-grabbed" procedure which is expected to be tail-called by
  ;; receiver, specifying how to handle subsequent motion events and
  ;; motion termination.
  (make-interactor
   (list object)
   `((BUTTON-PRESS
      ,(lambda (event)
	 (decode-button-event
	  (Event.OS-Event event)
	  (lambda (type serial sent? display window root
			subwindow time x y RootX RootY state
			button SameScreen?)
	    type serial sent? display window root
	    subwindow time x y RootX RootY state
	    button SameScreen?
	    (if (or (= which-buttons ANYBUTTON)
		    (memv button which-buttons))
		(let* ((should-be-result (list 'foo))
		       (result
			(receiver event
				  (lambda (on-motion at-end)
				    (mouse-drag (drawing-surface object)
						on-motion)
				    (at-end)
				    should-be-result))))
		  (if (eq? result should-be-result)
		      'OK
		      (error "HANDLE-BUTTON-GRAB: Must tail call"))))))))
     (POINTER-MOTION ,(lambda (e) e 'IGNORE))
     (BUTTON-RELEASE ,(lambda (e) e 'IGNORE)))))

;;;; Support code for interaction managers:
;;;; Maps from DrawingSurface to Interactor to event masks

;; The global map ds->(<interactor>->eventmasks)
(define *all-sensitive-surfaces* '())

;; A Surface-Sensitivity specifies for a given drawing surface the
;; total event-generation mask for that surface and a list of
;; Sensitivity data structures. The mask here is the inclusive-OR of
;; all the masks in the Sensitivity data structures.

;; A Sensitivity maps a single handler to the list of event types it
;; is intended to handle.  For GC reasons, it only weakly holds the
;; handler itself, since these are included in the global
;; *all-sensitive-surfaces* list.

(define find-sensitivity
  ;; (find-sensitivity <interactor> list-of-sensitivities) =>
  ;;  sensitivity or #F
  ;; Or, in layman's terms, given a list of handler/description pairs
  ;; and a specific handler, find the description of that handler.
  (make-lookup
   (lambda (obj) (weak-car (Sensitivity.%weak-<interactor> obj)))))

(define find-ss
  ;; (find-ss drawing-surface list-of-Surface-Sensitivity)
  ;; returns a specific Surface-Sensitivity or #F
  (make-lookup
   (lambda (x) (weak-car (Surface-Sensitivity.Weak-Surface x)))))

(define (record-surface-sensitivity! surface interactor mask)
  (define (record-<interactor>-sensitivity! ss)
    (let* ((sensitivities (surface-sensitivity.sensitivities ss))
	   (entry (find-sensitivity interactor sensitivities)))
      (if entry
	  (set-sensitivity.masks! entry (cons mask (sensitivity.masks entry)))
	  (set-surface-sensitivity.sensitivities! ss
	    `(,(make-sensitivity (weak-cons interactor '()) (list mask))
	      ,@sensitivities))))
    ;; Now tell the window system to set the event generation for this
    ;; particular drawing surface
    (reset-sensitivity! ss))
  (let ((sensitivity-of-surface
	 (or (find-ss surface *all-sensitive-surfaces*)
	     (let ((new-entry
		    (make-surface-sensitivity (weak-cons surface 'ignore)
					      NoEventMask '())))
	       (set! *all-sensitive-surfaces*
		     (cons new-entry *all-sensitive-surfaces*))
	       new-entry))))
    (record-<interactor>-sensitivity! sensitivity-of-surface)))

(define delete-<interactor>!
  (let ((del-sensitivity!
	 (del-op! (lambda (obj)
		    (weak-car (sensitivity.%weak-<interactor> obj)))))
	(del-ss! (del-op! surface-sensitivity.sensitivities)))
    (lambda (surface interactor)
      (let ((ss (find-ss surface *all-sensitive-surfaces*)))
	(if ss
	    (let ((new (del-sensitivity!
			interactor
			(surface-sensitivity.sensitivities ss))))
	      (if (null? new)
		  (set! *all-sensitive-surfaces*
			(del-ss! surface *all-sensitive-surfaces*))
		  (begin
		    (set-surface-sensitivity.sensitivities! ss new)
		    ;; Now tell the window system to set the event
		    ;; generation for this particular drawing surface
		    (reset-sensitivity! ss)))))))))

;;;; Continued ...

;;;; Support code for interactive geometry managers, continued

;;; When a surface is asked to generate events, we ask the toolkit to
;;; generate events if it is a toolkit window.  Otherwise, we ask the
;;; window system directly.  WE DO NOT DO BOTH.
;;;
;;; This lets people create windows from Scheme which don't have
;;; related toolkit windows, even though we haven't done that yet.

(define (reset-sensitivity! surface-sensitivity)
  ;; This tells the window system to actually update the event
  ;; generation mask for a given drawing surface.
  ;; NOTE: Whoever calls this is responsible for guaranteeing that the
  ;;       surface (which is weakly held) still exists.
  (let ((original (surface-sensitivity.mask surface-sensitivity)))
    (let loop ((s 0)
	       (rest (surface-sensitivity.sensitivities
		      surface-sensitivity)))
      (if (null? rest)
	  (begin
	    (set-surface-sensitivity.mask! surface-sensitivity s)
	    (if (not (= s original))
		(let ((Surface
		       (weak-car
			(surface-sensitivity.Weak-Surface
			 surface-sensitivity))))
		  (if Surface
		      (let ((TKWindow (DrawingSurface.ToolKitWindow Surface))
			    (UITKWindow (DrawingSurface.UITKWindow Surface)))
			(if TKWindow
			    (tk-generate-Scheme-event
			     s
			     (ToolKitWindow.TK-Window TKWindow))
			    (Generate-Events! UITKWindow s)))))))
	  (loop (apply bit-or s (sensitivity.masks (car rest)))
		(cdr rest))))))

(define (create-sensitive-surface UIObject handlers)
  ;; Given an object, return the Sensitive-Surface that will generate
  ;; these events.
  (let ((surface (Drawing-Surface UIObject)))
    (if (DrawingSurface? surface)
	(begin
	  (record-surface-sensitivity! surface UIObject
	    (if (null? handlers)
		0
		(apply bit-or (map handler->sensitivity handlers))))
	  (make-sensitive-surface surface handlers))
	(make-sensitive-surface #F handlers))))

(define (change-sensitive-surface! sensitive-surface UIObject)
  ;; If the drawing surface for an object changes, remove the old
  ;; record of handlers for that object (recorded on the old drawing
  ;; surface) and enter a new record on the current drawing surface.
  (let ((surface (Drawing-Surface UIObject))
	(old-surface
	 (sensitive-surface.DrawingSurface sensitive-surface)))
    (if (eq? surface old-surface)
	sensitive-surface
	(begin
	  (if (DrawingSurface? old-surface)
	      (destroy-sensitive-surface UIObject sensitive-surface))
	  (create-sensitive-surface
	   UIObject (sensitive-surface.handlers sensitive-surface))))))

(define (destroy-sensitive-surface interactor sensitive-surface)
  (let ((surface
	 (sensitive-surface.DrawingSurface sensitive-surface)))
    (delete-<interactor>! surface interactor)))

(define (destroy-all-sensitive-surfaces-from-display display)
  (set! *all-sensitive-surfaces*
	((list-deletor!
	  (lambda (surface-sensitivity)
	    (let ((surface
		   (weak-car (surface-sensitivity.Weak-Surface
			      surface-sensitivity))))
	      (or (not surface)
		  (eq? display
		       (Application->display
			(ToolKitWindow.Application
			 (drawingsurface.ToolKitWindow surface))))))))
	 *all-sensitive-surfaces*)))

;;;; Support for simplified user interface building.  We provide a
;;;; default application, and a procedure for adding new children to
;;;; it.

(define *the-default-application*
  (make-application "SWAT"))

;;; (Swat-Open obj1 ['-title "title1"] obj2 ['-title "title2"] ...)
;;; adds obj1, obj2, ... to the default application with the window
;;; titled by the string specified with the -title option.
;;; If no title option is specified, the window title will be the
;;; title of the application

(define (swat-open . objects-and-title-options)
  (apply swat-open-in-application
	 *the-default-application*
	 objects-and-title-options))

;;; (SWAT-OPEN-IN-APPLICATION app obj1 ['-title "title1"] obj2 ['-title "title2"] ...)
;;; is like swat-open, except for the speficifed application.

(define (swat-open-in-application app . objects-and-title-options)
  (let loop ((more-to-show objects-and-title-options))
    (if (null? more-to-show)
	'OK
	(let ((next-obj (car more-to-show))
	      (after-next (if (null? (cdr more-to-show))
			      #F
			      (cadr more-to-show))))
	  ;;look for -title following the object to show
	  (if (eq? after-next '-title)
	      (let ((specified-title
		     (if (null? (cddr more-to-show))
			 (error
			  "-title option given and no title specified -- SWAT-OPEN"
			  objects-and-title-options)
			 (caddr more-to-show))))
		;;if -title is there, next thing must be a string
		(if (string? specified-title)
		    (add-child! app
				next-obj
				specified-title)
		    (error "specified title is not a string -- SWAT-OPEN"
			   specified-title))
		(loop (cdddr more-to-show)))
	      ;;no -title specified -- use default naming
	      (begin (add-child! app next-obj)
		     (loop (cdr more-to-show))))))))

(define (swat-close . objs)
  (for-each (lambda (obj) (remove-child! *the-default-application* obj))
	    objs)
  'closed)