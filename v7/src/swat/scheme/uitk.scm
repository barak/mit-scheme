;;;;; -*- scheme -*-
;;;;;
;;;;; derived from uitk.sc,v 1.2 1993/02/25 14:13:22 jmiller exp $
;;;;; $id: uitk.scm,v 1.11 1993/02/26 15:10:23 jmiller exp jmiller $

(define debugging-port #f)

(define (debug-print . args)
  (let ((port (or debugging-port (current-output-port)))
	(string (with-output-to-string
		  (lambda ()
		    (display (cons 'debugging (cons (current-thread) args)))))))
    (without-interrupts
     (lambda () (display string port) (newline port)))))

;;;; notes

;;;; message flows define relationships.  normally, an operation that
;;;; changes state on an object will use one of these relationships to
;;;; alert other objects of the change.  this permits an external
;;;; constraint satisfaction system to propagate changes through the
;;;; system.  here are the flows currently
;;;; assumed:
;;;;     (1) geometry.  all object can report a desired size which
;;;;         includes stretch, shrink, and minimum size.  this is a
;;;;         pure query and does not establish a relationship.  the
;;;;         relationship commences with a call to either
;;;;         assign-screen-area! or assign-geometry!.  these
;;;;         specify an area to be used and alert any object
;;;;         monitoring for geometry changes.
;;;;     (2) events.  an object may have children to whom it reports
;;;;         events.  event directors take an event and a list of
;;;;         children and propagate the event to the correct child
;;;;         or take a default action.  this is a one-way interaction
;;;;         (i.e. children don't know about parents).
;;;;     (3) contexts.

;;;; i'd like to use tiny clos as a means for handling the private
;;;; slot in UIObj.  this allows the common operations to be as fast
;;;; as possible (i.e. not using generic dispatch) while still
;;;; permitting extensibility.  for the moment, however, i'm using
;;;; simple structures and type-specific operations.

;;;; when assign-screen-area! is called with #f instead of a screen
;;;; area it means that it has had its area retracted.  this happens
;;;; when the geometric parent is told to remove it as a child.  if it
;;;; has been using the parent's window, it better clean up -- this
;;;; may mean reparenting its own window to the root, i guess.

(define (sub-vectors point-1 point-2)
  (make-point
   (- (point.x point-1) (point.x point-2))
   (- (point.y point-1) (point.y point-2))))

(define (add-vectors point-1 point-2)
  (make-point
   (+ (point.x point-1) (point.x point-2))
   (+ (point.y point-1) (point.y point-2))))

(define (point= point1 point2)
  (or (eq? point1 point2)
      (and
       (= (point.x point1) (point.x point2))
       (= (point.y point1) (point.y point2)))))

(define (size= size1 size2)
  (or (eq? size1 size2)
      (and
       (= (size.width size1) (size.width size2))
       (= (size.height size1) (size.height size2)))))

(define (copy-rectangle rect)
  (vector-copy rect))

(define (UITKRectangle.Width rect)
  (size.width (UITKRectangle.Size rect)))

(define (UITKRectangle.Height rect)
  (size.height (UITKRectangle.Size rect)))

(define (rectangle= rect1 rect2)
  (or (eq? rect1 rect2)
      (and (point= (UITKRectangle.offset rect1) (UITKRectangle.offset rect2))
	   (size= (UITKRectangle.Size rect1) (UITKRectangle.Size rect2)))))

(define (screen-area= sa1 sa2)
  (or (and sa1 sa2 (rectangle= sa1 sa2))
      (and (not sa1) (not sa2))))

(define (translate-rectangle rect point)
  (and rect
       (make-UITKRectangle point (UITKRectangle.Size rect))))


;;;; event objects

(define (make-point-event type os-event window offset)
  (make-event 'point type os-event window offset 'invalid 'invalid))

(define (make-rectangle-event type os-event window offset width height)
  (make-event 'rectangle type os-event window offset width height))

(define (make-unknown-event type os-event window)
  (make-event 'unknown type os-event window 'invalid 'invalid 'invalid))

(define (point-event? obj)
  (and (event? obj)
       (eq? (event.point-or-rectangle? obj) 'point)))

(define (rectangle-event? obj)
  (and (event? obj)
       (eq? (event.point-or-rectangle? obj) 'rectangle)))


;;;; General support procedures

(define (make-lookup key-fn)
  (lambda (object list)
    (let loop ((list list))
      (cond ((null? list) #F)
	    ((eq? object (key-fn (car list))) (car list))
	    (else (loop (cdr list)))))))

(define (make-del-op! test?)
  (lambda (op)
    (lambda (key op-list)
      (define (loop previous current)
	(cond ((null? current) op-list)
	      ((test? (op (car current)) key)
	       (set-cdr! previous (cdr current))
	       op-list)
	      (else (loop current (cdr current)))))
      (cond ((null? op-list) '())
	    ((test? (op (car op-list)) key)
	     (cdr op-list))
	    (else (loop op-list (cdr op-list)))))))

(define del-op! (make-del-op! eq?))

(define del-assq! (del-op! car))
(define del-assv! ((make-del-op! eqv?) car))

(define (make-weak-lookup key-fn)
  (lambda (object list)
    (let loop ((list list))
      (cond ((null? list) #F)
	    ((eq? object (key-fn (weak-car list))) (weak-car list))
	    (else (loop (weak-cdr list)))))))

(define (make-weak-del-op! test?)
  (lambda (op)
    (lambda (key op-list)
      (define (loop previous current)
	(cond ((null? current) op-list)
	      ((test? (op (weak-car current)) key)
	       (weak-set-cdr! previous (weak-cdr current))
	       op-list)
	      (else (loop current (weak-cdr current)))))
      (cond ((null? op-list) '())
	    ((test? (op (weak-car op-list)) key)
	     (weak-cdr op-list))
	    (else (loop op-list (weak-cdr op-list)))))))

;;;; UI Objects

(define (one-parent-only! child object)
  (let ((child-guts (uiobjinternals child)))
    (if (UIObjInternals.already-have-a-parent? child-guts)
	(error
	 "ADD-CHILD!: Hal says 'success has many parents, but a UIObj has only one'"
	 object child)
	(set-UIObjInternals.already-have-a-parent?! child-guts #T))))

(define (get-UITKWindow obj)
  (let ((surface (drawing-surface obj)))
    (and (DrawingSurface? surface)
	 (DrawingSurface.UITKWindow surface))))

(define (DrawingSurface.Application ds)
  (ToolKitWindow.Application (DrawingSurface.ToolKitWindow ds)))


;;; The alerts are stored as alists with the key being, typically, the
;;; reason the alert was added.  This allows the alert to be removed
;;; if/when the reason is retracted.  The alert function is called
;;; with the reason as its argument.

(define make-add-alert!
  (let ((find-alert (make-lookup alert.reason)))
    (lambda (accessor mutator!)
      (lambda (object key alert-fn)
	(let* ((previous (accessor object))
	       (old-value (find-alert key previous)))
	  (if old-value
	      (begin
		;;(set-alert.function! old-value alert)
		;; (bkpt "gottcha in make-add-alert!")
		(debug-print 'gottcha!))
	      (mutator! object `(,(make-alert key alert-fn) ,@previous)))
	  'added)))))

(define make-remove-alert!
  (let ((del-alert! (del-op! alert.reason)))
    (lambda (accessor mutator!)
      (lambda (object key)
	(mutator! object (del-alert! key (accessor object)))
	'removed))))

(define (make-alert! arity accessor)
  ;; Arity is the arity expected of the alert function.  Some alerts
  ;; pass additional information -- geometry, for example, passes both
  ;; the previous screen-area and the new screen-area.  The alert
  ;; function can generally be assumed to have lexical access to both
  ;; the reason for the alert (specified when the alert is created)
  ;; and the object that generated the alert.
  (case arity
    ((0) (lambda (object)
	   (for-each (lambda (alert) ((alert.function alert)))
		     (accessor object))))
    ((1) (lambda (object arg)
	   (for-each (lambda (alert) ((alert.function alert) arg))
		     (accessor object))))
    ((2) (lambda (object arg1 arg2)
	   (for-each (lambda (alert) ((alert.function alert) arg1 arg2))
		     (accessor object))))
    (else (lambda (object . args)
	     (for-each (lambda (alert) (apply (alert.function alert) args))
		       (accessor object))))))

;;; Geometry alerts:
;;;  Initiated when ASSIGN-SCREEN-AREA! is acted on by an object, by
;;;  calling
;;;    (GEOMETRY-CHANGE! object
;;;                      old-used-screen-area new-used-screen-area)
;;;  An alert is added by calling
;;;    (ON-GEOMETRY-CHANGE! object reason 
;;;       (lambda (old new) ...))
;;;  The new-used-screen-area may be #T indicating that an object is
;;;  requesting a new area, or it may be #F or an actual area
;;;  indicating that it has been given (via ASSIGN-SCREEN-AREA!) a
;;;  specific area to use.
(define on-geometry-change!
  (make-add-alert! %geometry-alerts set-%geometry-alerts!))
(define forget-geometry-change!
  (make-remove-alert! %geometry-alerts set-%geometry-alerts!))
(define geometry-change! (make-alert! 2 %geometry-alerts))

;;; Event alerts:
;;;  Initiated when HANDLE-EVENT is acted on by an object, by calling
;;;    (EVENT! object event)
;;;  An alert is added by calling
;;;    (ON-EVENT! object reason 
;;;      (lambda (event) ...))
(define on-event!
  (make-add-alert! %event-alerts set-%event-alerts!))
(define forget-event!
  (make-remove-alert! %event-alerts set-%event-alerts!))
(define event! (make-alert! 1 %event-alerts))

;;; Context alerts:
;;;  Initiated when SET-CONTEXT! is acted on by an object, by calling
;;;    (CONTEXT-CHANGE! object new-context)
;;;  An alert is added by calling
;;;    (ON-CONTEXT-CHANGE! object reason 
;;;       (lambda (new-context) ...))
;;; NOTE: This protocol is not well worked out.
(define on-context-change!
  (make-add-alert! %context-alerts set-%context-alerts!))
(define forget-context-change!
  (make-remove-alert! %context-alerts set-%context-alerts!))
(define context-change! (make-alert! 1 %context-alerts))

;;; Death alerts:
;;;  Initiated when an object has decided it is dead by calling
;;;    (DEATH! object)
;;;  An alert is added by calling
;;;    (ON-DEATH! object reason (lambda () ...))
(define on-death!
  (make-add-alert! %death-alerts set-%death-alerts!))
(define forget-death-notification!
  (make-remove-alert! %death-alerts set-%death-alerts!))
(define death! (make-alert! 0 %death-alerts))

(define (forget! reporter reason)
  (forget-geometry-change! reporter reason)
  (forget-event! reporter reason)
  (forget-death-notification! reporter reason)
  (forget-context-change! reporter reason))

;;;; Queues for communication between interrupt level and user level

(define (empty-queue? queue)
  (without-interrupts
   (lambda ()
     (not (queue.%head queue)))))

(define (enqueue! queue value)
  (let ((element (list value)))
    (without-interrupts
     (lambda ()
       (if (queue.%head queue)
	   (set-cdr! (queue.%tail queue) element)
	   (set-queue.%head! queue element))
       (set-queue.%tail! queue element)))))

(define (dequeue! queue)
  ;; Not safe to use if the queue is empty!
  (without-interrupts
   (lambda ()
     (let* ((head (queue.%head queue))
	    (next (cdr head)))
       (if (null? next)
	   (begin
	     (set-queue.%head! queue #F)
	     (set-queue.%tail! queue #F))
	   (set-queue.%head! queue next))
       (car head)))))

(define (read-and-empty-queue! queue)
  ;; Returns a list of items, and leaves the queue empty
  (let ((quick-result
	 (without-interrupts
	  (lambda ()
	    (let ((result (queue.%head queue)))
	      (set-queue.%head! queue #F)
	      (set-queue.%tail! queue #F)
	      result)))))
    (or quick-result '())))


(define (update-locked-list! locked-list receiver)
  ;; Receiver gets the contents and returns a replacement
  (our-with-thread-mutex-locked
   'update-locked-list!
   (locked-list.%mutex locked-list)
    (lambda ()
      (set-locked-list.%data!
       locked-list
       (receiver (locked-list.%data locked-list)))
      ))
  'DONE)

(define (with-locked-list locked-list receiver)
  ;; Receiver gets the contents
  (our-with-thread-mutex-locked
   'with-locked-list
   (locked-list.%mutex locked-list)
    (lambda ()
      (receiver (locked-list.%data locked-list))
      )))

(define (our-with-thread-mutex-locked reason mutex thunk)
  reason
  (with-thread-mutex-locked mutex thunk))


;;; The default for these is just to do information propagation
;;; through the alert mechanism.

(define (UIObj-set-context! UIObj Context)
  (if (vector? Context)
      (context-change! UIObj Context)
      (error "UIOBJ-SET-CONTEXT!: Bad context" Context)))

(define (UIObj-assign-screen-area! UIObj Screen-Area)
  (if (or (UITKRectangle? Screen-Area)
	  (eq? #F Screen-Area))
      (begin
	(set-assigned-screen-area! UIObj Screen-Area)
	(let ((old (used-screen-area UIObj)))
	  (set-used-screen-area! UIObj screen-area)
	  (geometry-change! UIObj old screen-area))
	screen-area)
      (error "UIOBJ-ASSIGN-SCREEN-AREA!: Bad screen area" Screen-Area)))

(define (assign-location! object point)
  ;; There may be a better way to do this by making it part of the
  ;; geometry protocol.
  (assign-screen-area! object
    (translate-rectangle (used-screen-area object) point)))

(define (UIObj-assign-drawing-surface! UIObj Surface)
  (check-drawing-surface! UIObj Surface)
  (geometry-change! UIObj #F #F)
  'OK)

(define (check-drawing-surface! UIObj Surface)
  ;; Surface should be one of 'UNASSIGNED, 'RETRACTED, or a
  ;; DrawingSurface
  ;; This is used by internal routines that want to do the default
  ;; operation (UIObj-assign-drawing-surface!) but don't want to
  ;; announce the geometry change yet.
  (let ((old (Drawing-Surface UIObj)))
    (cond ((eq? old Surface) 'UNCHANGED)
	  ((or (eq? Surface 'RETRACTED)
	       (eq? old 'UNASSIGNED))
	   (set-drawing-surface! UIObj  Surface)
	   'CHANGED)
	  (else
	   (error "UIOBJ-ASSIGN-DRAWING-SURFACE!: Can't change surface"
		  UIObj old surface)))))

(define (assign-geometry! UIObj Surface Rectangle)
  (assign-drawing-surface! UIObj surface)
  (assign-screen-area! UIObj rectangle))

(define (point-in-rectangle? point rect-offset width height)
  (let ((rect-x (Point.X rect-offset))
	(rect-y (Point.Y rect-offset))
	(x (Point.X point))
	(y (Point.Y point)))
    (and (<= rect-x X)
	 (< X (+ rect-x Width))
	 (<= rect-Y Y)
	 (< Y (+ rect-Y Height)))))

(define (rectangle-overlaps-rectangle? p w h p2 w2 h2)
  (define (rectangles-overlap? LowEdge LowDelta HighEdge)
    (<= HighEdge (+ LowEdge LowDelta)))
  (let ((x (Point.X p))
	(y (Point.Y p))
	(x2 (Point.X p2))
	(y2 (Point.Y p2)))
    (and (if (< X X2)
	     (rectangles-overlap? X W X2)
	     (rectangles-overlap? X2 W2 X))
	 (if (< Y Y2)
	     (rectangles-overlap? Y H Y2)
	     (Rectangles-Overlap? Y2 H2 Y)))))

(define (uiobj-point-within? UIObj Point)
  (if (vector? Point)
      (let ((screen-area (Used-Screen-Area UIObj)))
	(and screen-area
	     (let ((Offset (UITKRectangle.Offset screen-area))
		   (Height (UITKRectangle.Height screen-area))
		   (Width (UITKRectangle.Width screen-area)))
	       (point-in-rectangle? Point Offset Width Height))))
      (error "UIOBJ-POINT-WITHIN?: Bad point" point)))

(define (UIObj-rectangle-overlaps? UIObj P1 W1 H1)
  (if (not (vector? P1))
      (error "UIOBJ-RECTANGLE-OVERLAPS?: Bad point" P1))
  (if (not (number? W1))
      (error "UIOBJ-RECTANGLE-OVERLAPS?: Bad width" W1))
  (if (not (number? H1))
      (error "UIOBJ-RECTANGLE-OVERLAPS?: Bad height" H1))
  (let ((screen-area (Used-Screen-Area UIObj)))
    (and
     screen-area
     (let ((P2 (UITKRectangle.Offset screen-area))
	   (H2 (UITKRectangle.Height screen-area))
	   (W2 (UITKRectangle.Width screen-area)))
       (rectangle-overlaps-rectangle? p1 w1 h1 p2 w2 h2)))))


(define (event-within? UIObj Event)
  (cond ((point-event? event)
	 (point-event-within? UIObj event))
	((rectangle-event? event)
	 (rectangle-event-within? UIObj event))
	(else (error "EVENT-WITHIN?: Bad event" event))))

(define (point-event-within? UIObj Event)
  (let ((window (Get-UITKWindow UIObj)))
    (and window
	 (= (->XWindow (UITKWindow.xwindow window))
	    (Event.Window Event))
	 (point-within? UIObj (event.Offset event)))))

(define (rectangle-event-within? UIObj Event)
  (let ((window (Get-UITKWindow UIObj)))
    (and window
	 (= (->XWindow (UITKWindow.xwindow window))
	    (Event.Window Event))
	 (Rectangle-Overlaps? UIObj
			      (event.Offset event)
			      (event.Width event)
			      (event.Height event)))))

(define (UIObj-handle-event UIObj Event)
  (if (event? event)
      (if (event-within? UIObj event)
	  (event! UIObj event))
      (error "UIOBJ-HANDLE-EVENT: Bad event" event)))

(define (UIObj-get-desired-size object)
  (define (->size datum)
    (or datum (make-size 0 0)))
  (->size (%desired-size object)))


;;; Default assigned-screen-area and used-screen-area (accessors and
;;; mutators) simply look in or modify the appropriate slots in the
;;; structure.  
(define (UIObj-assigned-screen-area UIObj)
  (UIObjInternals.assigned-screen-area (UIObjInternals UIObj)))

(define (UIObj-set-assigned-screen-area! UIObj Screen-area)
  (set-UIObjInternals.assigned-screen-area! (UIObjInternals UIObj)
					    Screen-Area))

(define (UIObj-used-screen-area UIObj)
  (UIObjInternals.used-screen-area (UIObjInternals UIObj)))

(define (UIObj-set-used-screen-area! UIObj Screen-Area)
  (set-UIObjInternals.used-screen-area! (UIObjInternals UIObj)
					Screen-Area))

(define (UIObj-protect-from-gc! UIObj stuff)
  (let ((crud (crud-that-I-dont-want-to-gc-away UIObj)))
    (set-car! crud (cons stuff (car crud))))
  'done)

(define (UIObj-unprotect-from-gc! UIObj stuff)
  (let ((crud (crud-that-I-dont-want-to-gc-away UIObj)))  
    (set-car! crud (delq! stuff (car crud))))
  'done)



;;;; Context procedures

(define (create-default-context name display)
  ;; Looks in appropriate customization locations to create a default
  ;; context for the application specified by NAME
  (define (convert converter predicate)
    (lambda (default)
      (define (get-default)
	(cond ((procedure? default) (default))
	      ((string? default) (converter default))
	      (else default)))
      (lambda (string)
	(if (and (string? string) (not (zero? (string-length string))))
	    (let ((result (converter string)))
	      (if (predicate result)
		  result
		  (get-default)))
	    (get-default)))))
  ;;;**** this doesn't make sense to me.  What are the predicates testing, really?
  ;;; changed XLoadFont to return a wrapped object,so string->font will also

  (let ((->symbol (convert string->symbol symbol?))
	(->number (convert string->number number?))
	(->color  (convert (string->color display) color?))
	;; (->font   (convert (string->font display) font?))
	;; (->cursor (convert string->cursor cursor?))
	)
    (apply make-context
	   (map (lambda (entry)
		  (let ((converter (car entry))
			(string (cadr entry)))
		    ;;;;********beware: getdefaultvalue is returning an unwrapped object!!
		    ;;;; fix this to add the wrappers
		    (converter (GetDefaultValue display name string))))
		`((,(->color "White") "ActiveBackground")
		  (,(->color "Black") "ActiveForeground")
		  (,(->symbol 'nw) "Anchor")
		  (,(->color "Black") "Background")
		  (,(->color "White") "Border")
		  (,(->number 0) "BorderWidth")
		  ;; (,(->cursor "Block") "Cursor")
		  ;; (,(->font #F) "Font")
		  (,(->color "White") "Foreground")
		  (,(->symbol 'raised) "Relief"))))))
