; -*- Scheme -*-
;;;;; Tk interface code for a button
;;; $Id: widget-mit.scm,v 1.2 1996/11/14 22:14:06 adams Exp $

;;; Lowest-level makers for various kinds of TK widgets.  These call C
;;; primitives in widget-c-mit.c, and are called by the higher-level
;;; widget object makers in widget.scm


(define-primitives
  (%tkDeleteDisplay 1)
  (%tkDestroyWidget 1)  
  (%tkMakeButton 2)
  (%tkMakeCanvas 2)
  (%tkMakeCheckbutton 2)
  (%tkMakeEntry 2)
  (%tkMakeLabel 2)
  (%tkMakeListbox 2)
  (%tkMakeMenu 2)
  (%tkMakeMenubutton 2)
  (%tkMakeMessage 2)
  (%tkMakeRadiobutton 2)
  (%tkMakeScale 2)
  (%tkMakeScrollBar 2)
  (%tkMakeText 2)
  )

#| Widgets hold strong pointers to the display and the application.
Therefore, when we GC away the display or applcation, we can assume
that there are no pointers to the widgets still around.  However, the
C end may need to be closed |#

;;; display->tk-widgets is a map that associates to each display a
;;; protection list of the tk-widgets for the display
(define display->tk-widgets 'INITIALIZED-LATER)

(define (add-widget-list-for-display-number! display-number)
  (set! display->tk-widgets
	(cons (cons display-number (make-protection-list))
	      display->tk-widgets)))

(define (find-tk-protection-list display)
  (find-tk-protection-list-from-number (->xdisplay display)))

(define (find-tk-protection-list-from-number number)
  (let ((list (assv number display->tk-widgets)))
    (and (pair? list)
	 (cdr list))))

;;; The item on the protection list is a cell containing the widget
;;; pointer.  This permits us to mark the cell when the C object is
;;; destroyed so we don't try to destroy it twice.

(define (Wrap-TK-widget surface name maker)
  (let ((ToolKitParent (DrawingSurface.ToolKitWindow surface)))
    ;; Note that the DrawingSurface's UITKWindow may not yet exist.
    (let ((parent-tk-window (ToolKitWindow.TK-window ToolKitParent))
	  (application (ToolKitWindow.Application ToolKitParent)))
      (let ((display (Application->Display application)))
	(let ((new-window-name
	       (string-append
		(tkwin.pathname parent-tk-window) "." name)))
	  (let ((wrapped-object #F))
	    (define (kill-me)
	      ;; Called when the object is destroyed
	      (SCXL-DESTROY! wrapped-object))
	    (set! wrapped-object
		  (SCXL-WRAP
		   (or (find-tk-protection-list display)
		       (error "No tk-protection-list for this display" display))
		   'tk-widget
		   (tk-op
		    (lambda ()
		      (maker parent-tk-window new-window-name)))
		   (list display surface kill-me) ;strong dependents
		   ))
	    (tk-invoke-command
	     'BIND (Application->TKMainWindow application)
	     (list new-window-name "<Destroy>"
		   (string-append "SchemeCallBack "
				  (number->string
				   (hash kill-me *our-hash-table*)))))
	    wrapped-object))))))

(define (widget/widget widget)
  (type-check-wrapped-object 'tk-widget widget)
  (SCXL-UNWRAP widget (lambda (w) w)))

(define ->widget widget/widget)

(define (%tk-really-destroy-widget handle)
  ;; Given a lowest-level TK handle or #F
  (if handle (tk-op (lambda() (%TkDestroyWidget handle))))
  'destroyed)

(define tk-widget-destroy
  ;; This will actually close the TK widget only if the wrapper isn't
  ;; already marked destroyed.  But that should be OK -- we shouldn't
  ;; be able to find a wrapper that's destroyed if the contents
  ;; haven't been closed.
  (wrap-with-SCXL-DESTROY! 1 0
   (lambda (scxl-wrapped-widget)
     (tk-op (lambda ()
	      (%tkDestroyWidget (->widget scxl-wrapped-widget)))))))

(define (tk-delete-display disp)
  (tk-op (lambda () (%tkDeleteDisplay (->Xdisplay disp)))))

(define (tk-make-button drawing-surface name)
  (Wrap-TK-widget drawing-surface name
		  (lambda (parent-tk-window real-name)
		    (%tkMakeButton parent-tk-window real-name))))

(define (tk-make-canvas drawing-surface name)
  (Wrap-TK-widget drawing-surface name
		  (lambda (parent-tk-window real-name)
		    (%tkMakeCanvas parent-tk-window real-name))))

(define (tk-make-checkbutton drawing-surface name)
  (Wrap-TK-widget drawing-surface name
		  (lambda (parent-tk-window real-name)
		    (%tkMakeCheckButton parent-tk-window real-name))))

(define (tk-make-entry drawing-surface name)
  (Wrap-TK-widget drawing-surface name
		  (lambda (parent-tk-window real-name)
		    (%tkMakeEntry parent-tk-window real-name))))

(define (tk-make-label drawing-surface name)
  (Wrap-TK-widget drawing-surface name
		  (lambda (parent-tk-window real-name)
		    (%tkMakeLabel parent-tk-window real-name))))

(define (tk-make-listbox drawing-surface name)
  (Wrap-TK-widget drawing-surface name
		  (lambda (parent-tk-window real-name)
		    (%tkMakeListbox parent-tk-window real-name))))

(define (tk-make-menu drawing-surface name)
  (Wrap-TK-widget drawing-surface name
		  (lambda (parent-tk-window real-name)
		    (%tkMakeMenu parent-tk-window real-name))))

(define (tk-make-menubutton drawing-surface name)
  (Wrap-TK-widget drawing-surface name
		  (lambda (parent-tk-window real-name)
		    (%tkMakeMenuButton parent-tk-window real-name))))

(define (tk-make-message drawing-surface name)
  (Wrap-TK-widget drawing-surface name
		  (lambda (parent-tk-window real-name)
		    (%tkMakeMessage parent-tk-window real-name))))

(define (tk-make-radiobutton drawing-surface name)
  (Wrap-TK-widget drawing-surface name
		  (lambda (parent-tk-window real-name)
		    (%tkMakeRadioButton parent-tk-window real-name))))

(define (tk-make-scale drawing-surface name)
  (Wrap-TK-widget drawing-surface name
		  (lambda (parent-tk-window real-name)
		    (%tkMakeScale parent-tk-window real-name))))

(define (tk-make-scrollbar drawing-surface name)
  (Wrap-TK-widget drawing-surface name
		  (lambda (parent-tk-window real-name)
		    (%tkMakeScrollBar parent-tk-window real-name))))

(define (tk-make-text drawing-surface name)
  (Wrap-TK-widget drawing-surface name
		  (lambda (parent-tk-window real-name)
		    (%tkMakeText parent-tk-window real-name))))

(define (initialize-mit-widgets!)
    (set! display->tk-widgets '()))

(initialize-mit-widgets!)
