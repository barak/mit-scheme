;;;;; -*- Scheme -*-
;;;;;
;;;;; derived from uitk.scm at MIT on April 24, 1993
;;;;; $Id: bfb86181216dd2d6f0107ec41a34c600b4160647 $

;;;; Commonly used structure definitions.  They are here so that the
;;;; macro versions can be loaded to improve performance.

(scc-define-structure UITKWindow
  xdisplay				; X display connection
  xwindow)				; X window ID

(scc-define-structure ToolKitWindow	; For TK or whatever
  Application				; For GC protection of TK Widgets
  Top-Level-Geometry-Callback		; From TK to us (for GC, too)
  TK-Window)				; *Un*wrapped!

(scc-define-structure DrawingSurface
  ToolKitWindow
  UITKWindow
  (Weak-List-of-Widgets '()))		; Ones that use this surface

(scc-define-structure Point X Y)

(scc-define-structure UITKRectangle Offset Size)

(scc-define-structure Size Width Height)

(scc-define-structure context
  activebackground activeforeground anchor background
  border borderwidth
  ;; cursor
  ;; font
  foreground relief)

(scc-define-structure alert reason function)

(scc-define-structure queue
  (%head #F)
  (%tail #F))

(scc-define-structure locked-list
  (%mutex (make-thread-mutex))
  (%data '()))

(scc-define-structure surface-sensitivity
  Weak-Surface
  Mask
  Sensitivities)

(scc-define-structure sensitivity
  %weak-<interactor>
  Masks)

(scc-define-structure sensitive-surface
  DrawingSurface
  Handlers) 

(scc-define-structure TK-variable
  application		      
  tk-name
  callback	;on writes
  )

(scc-define-structure scxl-wrapper
  type
  wrapped-object
  strong-dependents
  other-stuff)


(scc-define-structure Event
  Point-or-rectangle?
  Type
  OS-Event
  Window
  Offset
  Width
  Height)

(scc-define-structure Glue
  minsize
  class
  value)

;;; Applications, interactors, boxes, shapes, tkwidgets
;;; all share these internals.  These slots will be the first thing in
;;; the structure.

(scc-define-structure UIObjInternals
  Add-Child!-procedure
  Remove-Child!-procedure
  Set-Context!-procedure
  Assign-Screen-Area!-procedure
  Assign-Drawing-Surface!-procedure
  Point-Within?-procedure
  Rectangle-Overlaps?-procedure
  Handle-Event-procedure
  Get-Desired-Size-procedure
  Assigned-Screen-Area-procedure
  Used-Screen-Area-procedure
  Set-Assigned-Screen-Area!-procedure
  Set-Used-Screen-Area!-procedure
  Assign-Glue!-procedure
  (%geometry-alerts '())
  (%event-alerts '())
  (%context-alerts '())
  (%death-alerts '())
  (Assigned-Screen-Area #F)
  (Used-Screen-Area #F)
  (clip-region #F)
  (drawing-surface 'UNASSIGNED)
  (%desired-size #F)
  (%vglue #F)		; for boxes, shapes
  (%hglue #F)		; and tkwidgets only
  ;; Hal says:
  ;; "Success has many parents, but a UIObj has only one"
  (already-have-a-parent? #F)
  ;; Second '() is a special list for canvas and text items, that
  ;; might need to be un-gc-protected all at once.
  (crud-that-I-dont-want-to-gc-away (cons '() '()))
  )

;;;Generic procedures on objects that have UIObj internals

(define-integrable uiobjinternals-index 1)


