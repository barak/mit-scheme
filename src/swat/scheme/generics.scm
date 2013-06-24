;;; -*- Scheme -*-

(define-integrable (uiobjinternals uiobj)
  (vector-ref uiobj uiobjinternals-index))  

;;; Generic operations:
;;;   ADD-CHILD!, REMOVE-CHILD!, SET-CONTEXT!, ASSIGN-SCREEN-AREA!,
;;;   ASSIGN-DRAWING-SURFACE!, HANDLE-EVENT,
;;;   GET-DESIRED-SIZE, GET-DESIRED-SIZE, EVENT-WITHIN?

(define (add-child! object child . others)
  (if (null? others)
      ((UIObjInternals.Add-Child!-Procedure (uiobjinternals object))
       object
       child)
      (apply 
       (UIObjInternals.Add-Child!-Procedure (uiobjinternals object))
       object child others)))

(define (REMOVE-CHILD! Object Child)
  ((UIObjInternals.Remove-Child!-Procedure (uiobjinternals object))
   Object
   child))

(define (SET-CONTEXT! Object Context)
  ((UIObjInternals.Set-Context!-Procedure (uiobjinternals object))
   Object Context))

(define (ASSIGN-SCREEN-AREA! Object Screen-area)
  ((UIObjInternals.Assign-Screen-Area!-Procedure (uiobjinternals object))
   Object Screen-Area))

(define (weak-delq! item items)
  ;; Cleans out #F entries in the list as it goes
  (let loop ((previous #F)
	     (items* items))
    (cond ((weak-pair? items*)
	   (if (or (null? (weak-car items*))
		   (eq? (weak-car items*) item))
	       (begin
		 (if previous
		     (weak-set-cdr! previous (weak-cdr items*))
		     (set! items (weak-cdr items*)))
		 (loop previous (weak-cdr items*)))
	       (loop items* (weak-cdr items*))))
	  ((null? items*) items)
	  (else
	   (error:wrong-type-argument items "weak pair" 'weak-delq!)))))

(define (ASSIGN-DRAWING-SURFACE! Object Surface)
  (let ((old (drawing-surface object)))
    ((UIObjInternals.Assign-Drawing-Surface!-Procedure (uiobjinternals object))
     Object Surface)
    (if (eq? Surface 'RETRACTED)
	(begin
	  (set-assigned-screen-area! Object #F)
	  (set-used-screen-area! Object #F)))
    (if (not (eq? old Surface))
	(begin
	  (if (DrawingSurface? old)
	      (set-DrawingSurface.Weak-List-of-Widgets!
	       old
	       (weak-delq! object (DrawingSurface.Weak-List-of-Widgets old))))
	  (if (DrawingSurface? Surface)
	      (set-DrawingSurface.Weak-List-of-Widgets!
	       Surface (weak-cons object
				  (DrawingSurface.Weak-List-of-Widgets
				   Surface))))))
    'OK))

(define (POINT-WITHIN? Object Point)
  ((UIObjInternals.Point-Within?-Procedure (uiobjinternals object))
   Object Point))

(define (RECTANGLE-OVERLAPS? Object Point Width Height)
  ((UIObjInternals.Rectangle-Overlaps?-Procedure (uiobjinternals object))
   Object Point Width Height))

(define (HANDLE-EVENT Object Event)
  ((UIObjInternals.Handle-Event-Procedure (uiobjinternals object))
   Object Event))

(define (GET-DESIRED-SIZE Object)
  ((UIObjInternals.Get-Desired-Size-Procedure (uiobjinternals object))
   Object))

(define (ASSIGNED-SCREEN-AREA Object)
  ((UIObjInternals.ASSIGNED-SCREEN-AREA-Procedure (uiobjinternals object))
   Object))

(define (USED-SCREEN-AREA Object)
  ((UIObjInternals.Used-SCREEN-AREA-Procedure (uiobjinternals object))
   Object))

(define (SET-ASSIGNED-SCREEN-AREA! Object Screen-area)
  ((UIObjInternals.Set-ASSIGNED-SCREEN-AREA!-Procedure (uiobjinternals object))
   Object Screen-area))

(define (SET-USED-SCREEN-AREA! Object Screen-area)
  ((UIObjInternals.Set-Used-SCREEN-AREA!-Procedure (uiobjinternals object))
   Object Screen-Area))

(define (ASSIGN-GLUE! Object)
  ((UIObjInternals.Assign-Glue!-Procedure (uiobjinternals object))
   Object))


(define (%geometry-alerts UIObj)
  (UIObjInternals.%geometry-alerts (UIObjInternals UIObj)))

(define (set-%geometry-alerts! UIObj new-value)
  (set-UIObjInternals.%geometry-alerts! (UIObjInternals UIObj)
					new-value))

(define (%event-alerts UIObj)
  (UIObjInternals.%event-alerts (UIObjInternals UIObj)))

(define (set-%event-alerts! UIObj new-value)
  (set-UIObjInternals.%event-alerts! (UIObjInternals UIObj)
				     new-value))

(define (%context-alerts UIObj)
  (UIObjInternals.%context-alerts (UIObjInternals UIObj)))

(define (set-%context-alerts! UIObj new-value)
  (set-UIObjInternals.%context-alerts! (UIObjInternals UIObj)
				       new-value))

(define (%death-alerts UIObj)
  (UIObjInternals.%death-alerts (UIObjInternals UIObj)))

(define (set-%death-alerts! UIObj new-value)
  (set-UIObjInternals.%death-alerts! (UIObjInternals UIObj)
				     new-value))

(define (clip-region UIObj)
  (UIObjInternals.clip-region (UIObjInternals UIObj)))

(define (set-clip-region! UIObj new-value)
  (set-UIObjInternals.clip-region! (UIObjInternals UIObj)
				   new-value))

(define (drawing-surface UIObj)
  (UIObjInternals.drawing-surface (UIObjInternals UIObj)))

(define (set-drawing-surface! UIObj new-value)
  (set-UIObjInternals.drawing-surface! (UIObjInternals UIObj)
				       new-value))

(define (%desired-size UIObj)
  (UIObjInternals.%desired-size (UIObjInternals UIObj)))

(define (set-%desired-size! UIObj new-value)
  (set-UIObjInternals.%desired-size! (UIObjInternals UIObj)
				     new-value))

(define (%vglue UIObj)
  (UIObjInternals.%vglue (UIObjInternals UIObj)))

(define (set-%vglue! UIObj new-value)
  (set-UIObjInternals.%vglue! (UIObjInternals UIObj) new-value))

(define (%hglue UIObj)
  (UIObjInternals.%hglue (UIObjInternals UIObj)))

(define (set-%hglue! UIObj new-value)
  (set-UIObjInternals.%hglue! (UIObjInternals UIObj) new-value))

(define (crud-that-I-dont-want-to-gc-away UIObj)
  (UIObjInternals.crud-that-I-dont-want-to-gc-away (UIObjInternals UIObj)))

(define (set-crud-that-I-dont-want-to-gc-away! UIObj new-value)
  (set-UIObjInternals.crud-that-I-dont-want-to-gc-away!
   (UIObjInternals UIObj) new-value))


;;; procedures that are generic over CanvasItem, CanvasItemGroup, MenuItem,
;;; TextTag, and TKWidget

(define (valid-non-widget? obj)
  (or (CanvasItem? obj)
      (CanvasItemGroup? obj)
      (MenuItem? obj)
      (TextTag? obj)))

(define (ASK-WIDGET Object Command)
  (cond ((TkWidget%? Object)
	 ((TKwidget%.ask-widget-procedure Object) Object Command))
	((valid-non-widget? Object)
	 ((vector-ref object ask-widget-procedure-index) object command))
	(else (error "ASK-WIDGET: Not a valid Tk widget" Object))))

(define (ADD-EVENT-HANDLER! Object Event-type Handler . Substitutions)
  (cond ((TkWidget%? Object)
	 ((TKwidget%.add-event-handler!-procedure Object)
	  Object Event-type Handler substitutions))
	((valid-non-widget? Object)
	 ((vector-ref object add-event-handler!-procedure-index)
	  Object Event-type Handler substitutions))
	(else (error "ADD-EVENT-HANDLER!: Can't add an event handler to" Object))))

;;; set-callback! is also generic over active variables

(define (SET-CALLBACK! Object Callback)
  (cond ((TkWidget%? Object)
	 ((TKWidget%.set-callback!-procedure Object) Object Callback))
	((TK-variable? Object)
	 (set-active-variable-callback! Object Callback))
	((valid-non-widget? Object)
	 ((vector-ref object set-callback!-procedure-index) Object Callback))
	(else (error "SET-CALLBACK!: Can't set a callback for" Object))))



