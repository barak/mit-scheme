;;;;; -*- Scheme -*-
;;;;; $Id$
;;;;; derived from button.sc,v 1.2 1993/02/26 00:49:36 jmiller Exp $

;;;;;;;;;;  Widget Definitions

(define tk-widget->pathname
  (let ((->handle (lambda (widget)
		    (if (not (tkwidget%? widget))
			(pp `(tkh 2 ,widget)))
		    (TKWidget%.handle widget))))
    (lambda (tk-widget)
      (tkwin.pathname (tk-widget.tkwin (->handle tk-widget))))))

(define (tkwin->size tkwindow)
  (make-size (tkwin.width tkwindow) (tkwin.height tkwindow)))

#| display->tk-widgets maps a display-number to a protection list,
   which is a list of weak-pairs.  Each pair is <WRAPPED-OBJ or #F .
   cell with (handle or #F)> #F on the left means that the Scheme
   object has been lost.  #F on the right means that the TK widget has
   been closed.  In UITK, both the wrapped object and the TK object
   should be destroyed when the Scheme reference is lost.  However,
   when we close a display the TK objects may still be around.  The
   Scheme objects should not be around if we close the display from
   GC, but (brave) users may wish to close displays explicitly.  |#

(define (destroy-associated-tk-widgets display-number)
  (let ((tk-widgets (find-tk-protection-list-from-number display-number)))
    (if tk-widgets
	(begin
	  ;;canonical Lisp bug #27: we need to do the set! here
	  (set! display->tk-widgets
		(del-assv! display-number display->tk-widgets))
	  (for-each
	   (lambda (entry)
	     ;; An entry is either the left side of the pair described
	     ;; above, or the (former) contents of the cell on the
	     ;; right.
	     (if (SCXL-WRAPPED? entry)
		 (tk-widget-destroy entry)
		 (%tk-really-destroy-widget entry)))
	   (protection-list-all-elements tk-widgets
					 atomic-read-and-clear-cell!)))))
  (%tkDeleteDisplay display-number)	; Make TK forget the display exists
  'done)

(define (TKWidget-assign-drawing-surface! me Surface)
  (let ((old (Drawing-Surface me))
	(kids (TKWidget%.%children me)))
    (cond
     ;; Three kinds of Surface: 'RETRACTED, 'UNASSIGNED, or DrawingSurface?
     ;; The default is 'UNASSIGNED and this is NOT a legal value to
     ;; assign later!
     ((and (eq? Surface 'UNASSIGNED)
	   (eq? old 'UNASSIGNED))
      'nothing-to-do)
     ((eq? Surface 'RETRACTED)
      (set-drawing-surface! me 'RETRACTED)
      (if (DrawingSurface? old)
	  (tk-widget-destroy (TKWidget%.handle me)))
      (if (not (eq? old 'RETRACTED)) (death! me)))
     ((not (DrawingSurface? Surface))
      (error "TKWIDGET-ASSIGN-DRAWING-SURFACE!: Bad surface" Surface))
     ;; All of the kids need to receive a drawing surface now.  It
     ;; should be on the same application and XDisplay as me, but
     ;; should use my own XWindow for the parent, and my TK Window as
     ;; the TK Parent window.  Drawing surfaces are assigned TWICE:
     ;; first with a valid ToolKitWindow, and then with both a
     ;; ToolKitWindow and a UITKWindow (see baseobj.scm).  We mimic
     ;; that here.
     ((eq? old 'UNASSIGNED)
      ;; Create the widget drawing surface is first assigned
      (set-drawing-surface! me Surface)
      (let ((wrapped-tk-widget ((TKWidget%.how-to-make-me me) Surface))
	    (application (DrawingSurface.Application Surface)))
#|	
	(define (kill-me)
	  (debug-print '<destroy> 'call 'back me)
	  (set-drawing-surface! me 'RETRACTED)
	  (death! me))
	(UIObj-Protect-From-GC! me kill-me)
	(tk-invoke-command
	 'BIND (Application->TKMainWindow application)
	 (list (tk-widget->pathname me) "<Destroy>"
	       (string-append "+SchemeCallBack "
			      (number->string
			       (hash kill-me *our-hash-table*)))))
|#
	(set-TKWidget%.handle! me wrapped-tk-widget)
	(if (TKWidget%.do-not-gc-protect me)
	    (remove-from-protection-list!
	     (find-tk-protection-list-from-number
	      (->xdisplay
	       (Application->Display Application)))
	     wrapped-tk-widget)))
      (let ((kid-surface (make-DrawingSurface
			  (make-ToolKitWindow
			   (DrawingSurface.Application Surface)
			   #F
			   (tk-widget.tkwin (TKWidget%.handle me)))
			  #F)))
	(for-each (lambda (kid) (ASSIGN-DRAWING-SURFACE! kid kid-surface))
		  kids))
      (for-each (lambda (thunk) (thunk))
		(reverse
		 (TKWidget%.deferred-ask-widget-commands me)))
      (set-TKWidget%.deferred-ask-widget-commands! me '())
      (geometry-change! me #F #F))
     ((eq? old Surface)
      ;; 2nd pass now modifies that drawing surface to reflect a
      ;; possibly updated UITK window
      (if (not (null? kids))
	  (let ((new (drawing-surface (car kids))))
	    (set-DrawingSurface.UITKWindow!
	     new
	     (make-UITKWindow
	      (UITKWindow.XDisplay (DrawingSurface.UITKWindow Surface))
	      (tkwin.window (tk-widget.tkwin (TKWidget%.handle me)))))
	    (for-each (lambda (kid) (ASSIGN-DRAWING-SURFACE! kid new))
		      kids)))
      (geometry-change! me #F #F))
     (else
      (error "TKWIDGET-ASSIGN-DRAWING-SURFACE!: Can't change surface"
	     old Surface))))
  'OK)

(define (TKWidget-add-child! me kid)
  (one-parent-only! kid me)
  (let ((current-kids (TKWidget%.%children me)))
    (if (not (null? current-kids))
	(assign-drawing-surface! kid (drawing-surface (car current-kids)))
	(let ((Surface (drawing-surface me)))
	  (if (not (DrawingSurface? Surface))
	      (assign-drawing-surface! kid Surface)
	      (let* ((parent-tkwin (tk-widget.tkwin (TKWidget%.handle me)))
		     (kid-surface
		      (make-DrawingSurface
		       (make-ToolKitWindow
			(DrawingSurface.Application Surface)
			#F		; Top-level geometry callback
			parent-tkwin)
		       (make-UITKWindow
			(UITKWindow.XDisplay (DrawingSurface.UITKWindow Surface))
			(tkwin.window parent-tkwin)))))
		(assign-drawing-surface! kid kid-surface)))))
    (set-TKWidget%.%children! me (cons kid current-kids))))
  
(define (TKWidget-assign-screen-area! me screen-area)
  (cond ((vector? screen-area)
	 (if (TKWidget%.do-screen-area? me)
	     (let ((tk-handle (TKWidget%.handle me)))
	       (if (not (assigned-screen-area me))
		   (begin
		     (tk-map-window (tk-widget.tkwin tk-handle))
		     (tk-manage-geometry tk-handle
					 (TKWidget%.%scheme-geometry-manager me))))
	       (tk-move-resize-widget tk-handle screen-area)))
	 (geometry-change! me screen-area screen-area)
	 screen-area)
	((not Screen-Area)
	 (if (TKWidget%.do-screen-area? me)
	     (let ((tk-handle (TKWidget%.handle me)))
	       (TK-Unmap-Window (tk-widget.tkwin tk-handle))
	       (tk-manage-geometry tk-handle #F)))
	 (geometry-change! me screen-area screen-area)
	 screen-area)
	(else
	 (error "TKWIDGET-ASSIGN-SCREEN-AREA!: Bad screen-area" screen-area))))

(define (widget->screen-area widget)
  (let ((tkwin (tk-widget.tkwin (TKWidget%.handle widget))))
    (and (tkwin.ismapped? tkwin)
	 (make-UITKRectangle
	  (make-point (tkwin.x tkwin) (tkwin.y tkwin))
	  (make-size (tkwin.width tkwin)
		     (tkwin.height tkwin))))))

(define (TKWidget-assigned-screen-area me)
  (widget->screen-area me))
(define (TKWidget-used-screen-area me)
  (widget->screen-area me))

(define (TKWidget-set-assigned-screen-area! me anything)
  (if (and (eq? (drawing-surface me) 'RETRACTED)
	   (eq? anything #F))
      'OK
      (error "You can't set the screen area for a TK Widget" me anything)))
(define (TkWidget-set-used-screen-area! me anything)
  (if (and (eq? (drawing-surface me) 'RETRACTED)
	   (eq? anything #F))
      'OK
      (error "You can't set the screen area for a TK Widget" me anything)))

(define (TKWidget-get-desired-size object)
  (widget->size object tkwin->requested-size))

(define (TKWidget-assign-glue! me)
  (let* ((size (get-desired-size me))
	 (my-width (size.Width size))
	 (my-height (size.Height size)))
    ((TKWidget%.set-glue!-procedure me) me my-width my-height)))

(define (maybe-defer me command)
  (if (not (tkwidget%? me))
      (pp `(tkh 3 ,me)))
  (if (TKWidget%.handle me)
      (command)
      (defer me command)))

(define (defer me command)
  (set-TKWidget%.deferred-ask-widget-commands!
   me
   (cons command (TKWidget%.deferred-ask-widget-commands me)))
  #F)
  
(define (TKWidget-add-event-handler! object event handler substitutions)
  ;;for example
  ;; (add-event-handler! obj "<ButtonPress>"
  ;;     (lambda (path button) ....)
  ;;     "%W" "%b")
  ;; see TK manual (bind) for what these %frobs mean
  ;;hang this on the widget to GC protect it
  (let ((handler (proc-with-transformed-args handler substitutions)))
    (set-TKWidget%.%binding-callbacks!
     object
     (cons handler (TKWidget%.%binding-callbacks object)))
    (let ((command
	   (lambda ()
	     (tk-invoke-command
	      'bind
	      (application->TKMainWindow
	       (DrawingSurface.Application
		(Drawing-Surface object)))
	      (list
	       (tk-widget->pathname object)
	       event
	       ;; "event" should be a string because TCL is
	       ;; case sensitive, (e.g. <ButtonPress>)
	       (apply
		string-append
		"SchemeCallBack "
		(number->string (hash handler *our-hash-table*))
		(map (lambda (s)
		       (string-append " " s))
		     substitutions)))))))
      (maybe-defer object command))))

(define (TKWidget-ask-widget me arg-list)
  ;; If the widget doesn't have a drawing surface yet, then the TK
  ;; object hasn't been created and we can't actually execute the
  ;; command.
  (let ((command
	 (lambda ()
	   (if (not (tkwidget%? me))
	       (pp `(tkh 1 ,me)))
	   (if (SCXL-DESTROYED? (tkwidget%.handle me))
	       'punt
	       (tcl-global-eval
		(ToolKitWindow.Application
		 (DrawingSurface.ToolKitWindow (drawing-surface me)))
		(tk-widget->pathname me)
		arg-list)))))
    (maybe-defer me command)))


(define (current-size widget)
  (widget->size widget tkwin->size))

(define (widget->size widget ->size)
  (let ((h (TKWidget%.handle widget)))
    (if h
	(->size (tk-widget.tkwin h))
	(error "widget->size: widget not instantiated" widget))))

(define (tk-has-requested-new-size object)
  (let ((old-size (current-size object))
	(new-size (get-desired-size object)))
    (if (and (= (size.height old-size) (size.height new-size))
	     (= (size.width old-size) (size.width new-size)))
	'OK
	(geometry-change! object (used-screen-area object) #T))))

;; method to attach callback to the widget
(define (TKWidget-set-callback! me proc)
  (set-TKWidget%.%callback! me proc)
  (if (TKWidget%.%callback-command me)
      ((TKWidget%.%callback-command me) me (TKWidget%.%scheme-callback-hash me))
      (error "SET-CALLBACK!: not allowed" me proc)))

;; TKWidget Maker
(define (TKWidget-maker)
  (make-tkwidget%
   (make-UIObjInternals TKWidget-add-child!
			'invalid	; Remove-Child!-procedure
			UIObj-set-context!
			TKWidget-assign-screen-area!
			TKWidget-assign-drawing-surface!
			UIObj-point-within?
			UIObj-rectangle-overlaps?
			UIObj-handle-event
			TKWidget-get-desired-size
			TKWidget-assigned-screen-area
			TKWidget-used-screen-area
			TKWidget-set-assigned-screen-area!
			TKWidget-set-used-screen-area!
			TKWidget-assign-glue!)
   TKWidget-ask-widget
   TKWidget-add-event-handler!
   TKWidget-set-callback!))

(define (tkwin->requested-size tkwindow)
  (make-size (tkwin.req-width tkwindow)
	     (tkwin.req-height tkwindow)))

(define (make-TK-widget type widget-maker callback-command do-screen-area? set-glue!)
  ;; The Main widget-maker
  (lambda args
    (let ((me (TKWidget-maker)))
      (let ((geometry-callback
	     (lambda () (tk-has-requested-new-size me)))
	    (the-real-callback
	     (lambda args
	       (apply (TKWidget%.%callback me) args))))
	(let ((%scheme-callback-hash
	       (hash the-real-callback *our-hash-table*)))
	  (set-TKWidget%.%c-callback! me the-real-callback)
	  (set-TKWidget%.%scheme-geometry-manager! me geometry-callback)
	  (set-TKWidget%.%scheme-callback-hash! me %scheme-callback-hash)
	  (set-TKWidget%.%callback-command! me callback-command)
	  (set-TKWidget%.how-to-make-me!
	   me
	   (lambda (parent-drawing-surface)
	     (apply widget-maker parent-drawing-surface (tk-gen-name type) args)))
	  (set-TKWidget%.do-screen-area?! me do-screen-area?)
	  (set-TKWidget%.set-glue!-procedure! me set-glue!)
	  me))))
  )

(define (make-arg-transformers argspecs)
  (let ((id (lambda (x) x)))
    (map (lambda (spec)
	   (if (member spec
		       '("%b" "%c" "%h" "%k" "%t" "%w" "%x" "%y" "%X" "%Y"))
	       string->number
	       id))
	 argspecs)))

(define (proc-with-transformed-args proc argspecs)
  (let ((transformers
	 (make-arg-transformers argspecs)))
    (lambda args
      (apply proc (map (lambda (t a) (t a))
		       transformers args)))))



(define button-stretch 2)
(define canvas-stretch 20)
(define entry-height-stretch 1)

(define make-button
  (let ((maker (make-tk-widget
		"button" tk-make-button
		(lambda (button scheme-callback-hash-number)
		  (ask-widget
		   button
		   `(configure -command
			       ,(string-append
				 "SchemeCallBack "
				 (number->string
				  scheme-callback-hash-number)))))
		#T
		(lambda (button button-width button-height)
		  (set-%hglue! button (make-fill-glue button-width button-stretch))
		  (set-%vglue! button (make-fill-glue button-height button-stretch))
		  ))))
    (lambda options		;but don't use -callback!
      (let ((configure-options
	     (if (null? options)
		 '()
		 (car options))))
      (let ((button (maker)))
	(ask-widget button `(configure ,@configure-options))
	button)))))

(define remember-on-canvas!
  (let ((fix! (lambda (widget value)
		(set-TKWidget%.%binding-callbacks! widget value)))
	(fetch (lambda (widget) (TKWidget%.%binding-callbacks widget))))
    (lambda (canvas thing-to-remember)
      (fix! canvas (cons thing-to-remember (fetch canvas))))))

(define make-canvas
  (let ((maker (make-tk-widget
		"canvas" tk-make-canvas #F #T
		(lambda (canvas canvas-width canvas-height)
		  (set-%hglue! canvas (make-fill-glue canvas-width canvas-stretch))
		  (set-%vglue! canvas (make-fill-glue canvas-height canvas-stretch))))))
    (lambda options		
      (let ((configure-options
	     (if (null? options)
		 '()
		 (car options))))
	(let ((canvas (maker)))
	  (ask-widget canvas `(configure ,@configure-options))
	  canvas)))))
    
(define make-checkbutton
  (let ((maker (make-tk-widget
		"checkbutton" tk-make-checkbutton
		(lambda (checkbutton scheme-callback-hash-number)
		  (ask-widget
		   checkbutton
		   `(configure -command
			       ,(string-append
				 "SchemeCallBack "
				 (number->string
				  scheme-callback-hash-number)))))
		#T
		(lambda (checkbutton cb-width cb-height)
		  (set-%hglue! checkbutton (make-fill-glue cb-width button-stretch))
		  (set-%vglue! checkbutton (make-fill-glue cb-height button-stretch))))))
    (lambda options		
      (let ((configure-options
	     (if (null? options)
		 '()
		 (car options))))
	(let ((checkbutton (maker)))
	  (ask-widget checkbutton `(configure ,@configure-options))
	  checkbutton)))))

(define make-entry
  (let ((maker
	 (make-tk-widget
	  "entry"
	  tk-make-entry
	  (lambda (entry scheme-callback-hash-number)
	    (ask-widget
	     entry
	     `(configure -Scrollcommand
			 ,(string-append "SchemeCallBack "
					 (number->string
					  scheme-callback-hash-number)))))
	  #T
	  (lambda (entry entry-width entry-height)
	    (set-%hglue! entry (make-fill-glue entry-width button-stretch))
	    (set-%vglue! entry (make-fill-glue entry-height entry-height-stretch))))))
    (lambda options		
      (let ((configure-options
	     (if (null? options)
		 '()
		 (car options))))
	(let ((entry (maker)))
	  (ask-widget entry `(configure ,@configure-options))
	  entry)))))

(define make-label
  (let ((maker (make-tk-widget
		"label" tk-make-label 
		#F			; No callbacks allowed
		#T			; Normal screen-area handling
		(lambda (label label-width label-height)
		  (set-%hglue! label (make-fill-glue label-width button-stretch))
		  (set-%vglue! label (make-fill-glue label-height button-stretch))))))
    (lambda options		
      (let ((configure-options
	     (if (null? options)
		 '()
		 (car options))))
	(let ((label (maker)))
	  (ask-widget label `(configure ,@configure-options))
	  label)))))

(define make-listbox
  (let ((maker (make-tk-widget
		"listbox" tk-make-listbox
		#F			; No callbacks allowed
		#T			; Normal screen-area handling
		(lambda (listbox listbox-width listbox-height)
		  (set-%hglue! listbox (make-fill-glue listbox-width button-stretch))
		  (set-%vglue! listbox (make-fill-glue listbox-height button-stretch))))))
    (lambda options		
      (let ((configure-options
	     (if (null? options)
		 '()
		 (car options))))
	(let ((listbox (maker)))
	  (ask-widget listbox `(configure ,@configure-options))
	  listbox)))))

(define make-menu
  (let ((maker (make-tk-widget
		"menu" tk-make-menu
		#F			; No callbacks allowed
		#F			; No screen area assignment code
		(lambda (menu menu-width menu-height)
		  (set-%hglue! menu (make-fill-glue menu-width button-stretch))
		  (set-%vglue! text (make-fill-glue menu-height button-stretch))))))
    (lambda options
      (let ((configure-options
	     (if (null? options)
		 '()
		 (car options))))
	(let* ((menu (maker))
	       (mr (make-menurecord menu '())))
	  (set! *all-menus* (weak-cons mr *all-menus*))
	  (uiobj-protect-from-gc! menu mr)
	  (ask-widget menu `(configure ,@configure-options))
	  menu)))))

(define make-menubutton
  (let ((maker (make-tk-widget
		"menubutton"
		tk-make-menubutton
		#F			; No callbacks allowed
		#T			; Normal screen-area handling
		(lambda (menubutton mb-width mb-height)
		  (set-%hglue! menubutton (make-fill-glue mb-width button-stretch))
		  (set-%vglue! menubutton (make-fill-glue mb-height button-stretch))))))
    (lambda (menu . options)		; options can't include -menu
      (let ((configure-options
	     (if (null? options)
		 '()
		 (car options))))
	(let ((menubutton (maker)))
	  (add-child! menubutton menu)
	  (ask-widget menubutton `(configure ,@configure-options))
	  (ask-widget menubutton
		      `(configure -menu
				  ,(lambda () (tk-widget->pathname menu))))
	  menubutton)))))

(define make-message
  (let ((maker (make-tk-widget
		"message" tk-make-message
		#F			; No callbacks allowed
		#T			; Normal screen-area handling
		(lambda (message message-width message-height)
		  (set-%hglue! message (make-fill-glue message-width button-stretch))
		  (set-%vglue! message (make-fill-glue message-height button-stretch))))))
    (lambda options		
      (let ((configure-options
	     (if (null? options)
		 '()
		 (car options))))
	(let ((message (maker)))
	  (ask-widget message `(configure ,@configure-options))
	  message)))))

(define make-radiobutton
  (let ((maker (make-tk-widget
		"radiobutton" tk-make-radiobutton
		(lambda (radiobutton scheme-callback-hash-number)
		  (ask-widget
		   radiobutton
		   `(configure -command
			       ,(string-append
				 "SchemeCallBack "
				 (number->string
				  scheme-callback-hash-number)))))
		#T
		(lambda (radiobutton rb-width rb-height)
		  (set-%hglue! radiobutton (make-fill-glue rb-width button-stretch))
		  (set-%vglue! radiobutton (make-fill-glue rb-height button-stretch))))))
    (lambda options		
      (let ((configure-options
	     (if (null? options)
		 '()
		 (car options))))
	(let ((radiobutton (maker)))
	  (ask-widget radiobutton `(configure ,@configure-options))
	  radiobutton)))))

(define (after-last-space string)
  (let ((index (string-find-previous-char string #\Space)))
    (if index
	(substring string (+ index 1) (string-length string))
	(error "String does not contain a space" string))))

(define (get-tk-widget-orientation tk-widget)
  ;; returns 'v or 'h.
  (string->symbol
   (string-downcase
    (substring
     (after-last-space
      (ask-widget tk-widget '(configure -orient)))
     0
     1))))

(define make-scale
  (let ((maker
	 (make-tk-widget
	  "scale"
	  tk-make-scale
	  (lambda (scale scheme-callback-hash-number)
	    (ask-widget scale
			`(configure -command
				    ,(string-append
				      "SchemeCallBack "
				      (number->string
				       scheme-callback-hash-number)))))
	  #T
	  (lambda (scale scale-width scale-height)
	    (let ((orientation (get-tk-widget-orientation scale)))
	      (cond ((eq? orientation 'v)
		     (set-%hglue! scale (make-rigid-glue scale-width 1))
		     (set-%vglue! scale (make-fill-glue scale-height button-stretch)))
		    ((eq? orientation 'h)
		     (set-%hglue! scale (make-fill-glue scale-width button-stretch))
		     (set-%vglue! scale (make-rigid-glue scale-height 1)))
		    (else (error "Bad orientation: Must be 'h or 'v" orientation))))))))
    (lambda options
      (let ((configure-options
	     (if (null? options)
		 '()
		 (car options))))
	(let ((scale (maker)))
	  (ask-widget scale `(configure ,@configure-options))
	  scale)))))

(define make-scrollbar
  (let ((maker
	 (make-tk-widget
	  "scrollbar"
	  tk-make-scrollbar
	  (lambda (scrollbar scheme-callback-hash-number)
	    (ask-widget
	     scrollbar
	     `(configure -command
			 ,(string-append "SchemeCallBack "
					 (number->string
					  scheme-callback-hash-number)))))
	  #T
	  (lambda (scrollbar scrollbar-width scrollbar-height)
	    (let ((orientation (get-tk-widget-orientation scrollbar)))
	      (cond ((eq? orientation 'v)
		     (set-%hglue! scrollbar (make-rigid-glue scrollbar-width 1))
		     (set-%vglue! scrollbar (make-fil-glue scrollbar-height 1)))
		    ((eq? orientation 'h)
		     (set-%hglue! scrollbar (make-fil-glue scrollbar-width 1))
		     (set-%vglue! scrollbar (make-rigid-glue scrollbar-height 1)))
		    (else (error "Bad orientation: must be 'h or 'v" orientation))))))))
    (lambda options
      (let ((configure-options
	     (if (null? options)
		 '()
		 (car options))))
	(let ((scrollbar (maker)))
	  (ask-widget scrollbar `(configure ,@configure-options))
	  scrollbar)))))

(define make-text
  (let ((maker (make-tk-widget
		"text"
		tk-make-text
		#F	; No callbacks allowed
		#T	; Normal screen-area handling
		(lambda (text text-width text-height)
		  (set-%hglue! text (make-fill-glue text-width canvas-stretch))
		  (set-%vglue! text (make-fill-glue text-height canvas-stretch))))))
    (lambda options		
      (let ((configure-options
	     (if (null? options)
		 '()
		 (car options))))
	(let ((text (maker)))
	  (ask-widget text `(configure ,@configure-options))
	  text)))))

;;; TK has "active variables"

;;; Ask-widget should be changed so that TK-variables get replaced by
;;; their names [WHAT DOES THIS MEAN?? -- Hal]

(define (make-active-variable . application)
  (let ((application (if (null? application)
			 *the-default-application*
			 (car application))))
    (let* ((tk-name (tk-gen-name "variable"))
	   (variable (make-TK-variable application tk-name #F)))
      (UIObj-protect-from-gc! application variable)
      variable)))

(define (active-variable-value var)
  (tcl-global-eval
   (TK-variable.application var)
   "expr"
   (list (string-append "$" (TK-variable.tk-name var)))))

(define (set-active-variable! var value)
  (tcl-global-eval
   (TK-variable.application var)
   "set"
   (list (TK-variable.tk-name var) value)))

(define (set-active-variable-callback! var thunk)
  (let ((app (TK-variable.application var))
	(name (TK-variable.tk-name var))
	(cb (TK-variable.callback var)))
    (if cb
	(tcl-global-eval
	 app "trace"
	 (list "vdelete" name "w" `("SchemeCallBack"
				    ,(object-hash cb *our-hash-table*)))))
    (let ((the-callback
	   (lambda (arg1 arg2 arg3)	;tcl generates these
	     arg1 arg2 arg3		;but we'll ignore them
	     (thunk))))	
      (tcl-global-eval
       app "trace"
       (list "variable" name "w"
	     `("SchemeCallBack"
	       ,(object-hash the-callback *our-hash-table*))
	     ))
      (set-TK-variable.callback! var the-callback))))

(define (checkbutton-variable-on? var)
  (equal? (active-variable-value var) "1"))


(define (initialize-widgets!)
  (SCXL-Install-XCloseDisplay-Callback
   destroy-associated-tk-widgets))

(initialize-widgets!)
