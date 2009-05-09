;;; -*- Scheme -*-

;;; More structures.  With all this macro expansion, the compiler runs
;;; out of space if they are all in one file.

(scc-define-structure Application%
  UIObjInternals
  %child-windows     	; Locked list of all children
  %%%code%%%	       	; Asynchronous code to be
                        ; executed when data arrives
                        ; from the display connection
  application-name
  Xdisplay	       	; A SCXL-wrapped display
  TKMainWindow
  context)

(scc-define-structure Interactor%
  UIObjInternals
  handlers
  ;; Map from children of the interactor -- things it manages --
  ;; to Sensitive-Surface data structures.  These are implemented
  ;; at the end of the file and maintain the correspondence
  ;; between drawing surfaces, event generation masks, and
  ;; <interactor> objects.
  (sensitive-surface-map '()))

(scc-define-structure Shape%
  UIObjInternals
  x-draw
  calculate-used-screen-area
  color
  point-within?
  rectangle-overlaps?
  (x-erase #F)
  gc-function
  (graphics-context #F))

(scc-define-structure Box%
  UIObjInternals
  sizer
  arranger
  get-hglue
  get-vglue
  (kids '()))

(scc-define-structure ArrayBox%
  UIObjInternals
  kids-lists
  (kids '()))

(scc-define-structure TKWidget%
  UIObjInternals
  Ask-Widget-procedure
  Add-Event-Handler!-procedure
  Set-Callback!-procedure
  (deferred-ask-widget-commands '())
  (how-to-make-me 'later)
  ;; Parent-Window -- stored as assigned screen area
  (Set-Glue!-procedure 'later)
  (%c-callback 'later)
  (%binding-callbacks '())
  (%scheme-geometry-manager 'later)
  (%scheme-callback-hash 'later)
  (%callback #F)
  (%callback-command #F)
  (%children '())
  (handle #F)
  (do-screen-area? 'later)
  ;; The following is weird.  TK does finalization of some TK objects
  ;; (sub-menus bit us ...) and we can *not* destroy these on our own.
  ;; We simply reflect the ownership (via add-child!) to prevent them
  ;; from being GCed away if the TK parent exists.  And the child
  ;; better hold on to the parent, too.
  (do-not-gc-protect #F))

(scc-define-structure CanvasItem
  Ask-Widget-procedure
  Add-Event-Handler!-procedure
  Set-Callback!-procedure
  Name
  Canvas
  %binding-callbacks)

(scc-define-structure CanvasItemGroup
  Ask-Widget-procedure
  Add-Event-Handler!-procedure
  Set-Callback!-procedure
  Tag
  Canvas
  %binding-callbacks)

(scc-define-structure MenuRecord Menu Items)

(scc-define-structure MenuItem
  ask-widget-procedure
  Add-event-handler!-procedure
  Set-callback!-procedure
  MenuRecord
  %callback
  index)

(scc-define-structure TextTag
  ask-widget-procedure
  Add-event-handler!-procedure
  Set-callback!-procedure
  Name
  Text
  Callbacks)

;;; procedures that are generic over CanvasItem, CanvasTag, MenuItem,
;;; TextTag, and TKWidget

(define-integrable ask-widget-procedure-index 1)
(define-integrable add-event-handler!-procedure-index 2)
(define-integrable set-callback!-procedure-index 3)



