;;; -*- Scheme -*-

;;; Canvases can be scrollable: we can create them with two scrollbars,
;;; one on the right and one on the bottom.

(define (make-scrollable-canvas . options)
  (let ((canvas (apply make-canvas options))
	(vscroll (make-scrollbar '(-orient vert)))
	(hscroll (make-scrollbar '(-orient horiz))))
    (let ((v-command
	   (lambda ()
	     (ask-widget
	      vscroll
	      `(configure -command
			  ,(string-append (tk-widget->pathname canvas) " yview")))))
	  (h-command
	   (lambda ()
	     (ask-widget
	      hscroll
	      `(configure -command
			  ,(string-append (tk-widget->pathname canvas) " xview")))))
	  (c-command
	   (lambda ()
	     (maybe-defer
	      vscroll
	      (lambda ()
		(ask-widget
		 canvas
		 `(configure
                   -xscroll
	           ,(string-append (tk-widget->pathname hscroll) " set")
                   -yscroll
		   ,(string-append (tk-widget->pathname vscroll) " set"))))))))
      (defer canvas v-command)
      (defer canvas h-command)
      (defer hscroll c-command)
      (make-vbox (make-hbox canvas vscroll) hscroll))))

(define (scrollable-canvas-canvas scrollable-canvas)
  (let ((top-row (car (box-children scrollable-canvas))))
    (car (box-children top-row))))

(define (scrollable-canvas-vscroll scrollable-canvas)
  (let ((top-row (car (box-children scrollable-canvas))))
    (cadr (box-children top-row))))

(define (scrollable-canvas-hscroll scrollable-canvas)
  (cadr (box-children scrollable-canvas)))


;;; Canvas has special protect-from-gc! procedures

(define (canvas-protect-from-gc! canvas stuff)
  (let ((crud (crud-that-I-dont-want-to-gc-away canvas)))
    (set-cdr! crud (cons stuff (cdr crud))))
  'done)

(define (canvas-unprotect-from-gc! canvas stuff)
  (let ((crud (crud-that-I-dont-want-to-gc-away canvas)))  
    (set-cdr! crud (delq! stuff (cdr crud))))
  'done)  

(define (canvas-flush-protect-list! canvas)
  (let ((crud (crud-that-I-dont-want-to-gc-away canvas)))  
    (set-cdr! crud '()))
  'done)  


;;; CanvasItem structure

(define (make-canvas-item name canvas)
  (if (not (TKWidget%.handle canvas))
      (error "You must OPEN the canvas before you can make an item on it"))
  (let ((item (make-canvasitem canvasitem-ask-widget
			       canvasitem-add-event-handler!
			       'invalid
			       name
			       canvas
			       '())))
    (canvas-protect-from-gc! canvas item)
    item))

(define (canvasitem-add-event-handler! item event handler substitutions)
  (let ((canvas (CanvasItem.canvas item))
	(handler (proc-with-transformed-args handler substitutions)))
    (set-canvasitem.%binding-callbacks!
     item
     (cons handler (canvasitem.%binding-callbacks item)))
    (ask-widget canvas
		`(bind
		  ,(CanvasItem.name item)
		  ,event
		  ("SchemeCallBack" ,(hash handler *our-hash-table*)
				    ,@substitutions)))))

;;; The following assumes that the commands which explicitly mention
;;; canvas items mention them only as their second argument. This is
;;; true for most of the commands (e.g., itemconfigure, move, raise);
;;; but select, for example, is an exception. Do we care about those,
;;; anyway?  Same is true for the <CanvasItemGroup> version.

(define (canvasitem-ask-widget me arg-list)
  (let* ((name   (CanvasItem.name  me))
	 (canvas (CanvasItem.canvas me))
	 (command (car arg-list))
	 (new-arg-list (cons (if (eq? command 'configure)
				 'itemconfigure
				 command)
			     (cons name (cdr arg-list)))))
    (let ((result (ask-widget canvas new-arg-list)))
      (if (eq? command 'delete)
	  (canvas-unprotect-from-gc! canvas me))
      result)))


;;; CanvasItemGroup structure, for grouping (tagging) canvas items together.

(define (make-canvas-item-group canvas list-of-canvas-items)
  (let ((tag (tk-gen-name "CanvasItemGroup")))
    (for-each (lambda (item)
		(if (eq? canvas (CanvasItem.canvas item))
		    (ask-widget item `(configure -tags ,tag))
		    (error "MAKE-CANVAS-ITEM-GROUP: not a canvas item on canvas"
			   canvas item)))
	      list-of-canvas-items)
    (let ((CanvasItemGroup (make-CanvasItemGroup CanvasItemGroup-ask-widget
						 CanvasItemGroup-add-event-handler!
						 'invalid
						 tag
						 canvas
						 '())))
      (canvas-protect-from-gc! canvas CanvasItemGroup)
      CanvasItemGroup)))


(define (add-to-canvas-item-group tag new-item)
  (if (eq? (CanvasItem.canvas new-item)
	   (CanvasItemGroup.canvas tag))
      (ask-widget new-item `(configure -tags ,(CanvasItemGroup.tag tag)))
      (error "ADD-TO-CANVAS-ITEM-GROUP: not a canvas item on canvas"
	     canvas new-item)))

(define (merge-canvas-item-groups canvas destructive? . tags)
  (let ((new-tag (tk-gen-name "CanvasItemGroup")))
    (for-each
     (lambda (tag)
       (cond ((eq? (CanvasItemGroup.canvas tag) canvas)
	      (let ((tk-tag (CanvasItemGroup.tag tag)))
		(ask-widget canvas `(addtag ,new-tag withtag ,tk-tag))
		;; If destructive? is true, the old tags are
		;; destroyed. Otherwise, they are kept. The old tags
		;; take precedence in case of conflicting event handlers. 
		(if destructive?
		    (begin
		      (ask-widget canvas `(dtag ,tk-tag))
		      (canvas-unprotect-from-gc! canvas tag)))))
	     (else
	      (error "MERGE-CANVAS-ITEM-GROUPS: not a canvas tag on canvas"
		     canvas tag))))
     tags)
    (let ((CanvasItemGroup (make-CanvasItemGroup CanvasItemGroup-ask-widget
				     CanvasItemGroup-add-event-handler!
				     'invalid
				     new-tag
				     canvas
				     '())))
      (canvas-protect-from-gc! canvas CanvasItemGroup)
      CanvasItemGroup)))

(define (CanvasItemGroup-add-event-handler! tag event handler substitutions)
  ;; to handle tagged canvas items
  (let ((canvas (CanvasItemGroup.canvas tag))
	(handler (proc-with-transformed-args handler substitutions)))
    (set-CanvasItemGroup.%binding-callbacks!
     tag
     (cons handler (CanvasItemGroup.%binding-callbacks tag)))
    (ask-widget canvas
		`(bind
		  ,(CanvasItemGroup.tag tag)
		  ,event
		  ("SchemeCallBack" ,(hash handler *our-hash-table*)
				    ,@substitutions)))))

(define (CanvasItemGroup-ask-widget tag arg-list)
  ;; to handle tagged canvas items
  (let* ((tag-name (CanvasItemGroup.tag    tag))
	 (canvas   (CanvasItemGroup.canvas tag))
	 (command  (car arg-list))
	 (new-arg-list (cons (if (eq? command 'configure)
				 'itemconfigure
				 command)
			     (cons tag-name (cdr arg-list)))))
    (let ((result (ask-widget canvas new-arg-list)))
      (if (eq? command 'delete)
	  (canvas-unprotect-from-gc! canvas tag))
      result)))


;;; This is how the user creates canvas items, e.g.
;;; (define george (make-arc-on-canvas c 200 200 250 250))

(define (make-arc-on-canvas canvas x1 y1 x2 y2 . options)
  (let ((configure-options (if (null? options) '() (car options))))
    (make-canvas-item
     (ask-widget canvas `(create arc ,x1 ,y1 ,x2 ,y2 ,@configure-options))
     canvas)))

(define (make-bitmap-on-canvas canvas bitmap-filename-string x y . options)
  (if (not (file-exists? bitmap-filename-string))
      (error "MAKE-BITMAP-ON-CANVAS: Bad file name" bitmap-filename-string))
  (let ((configure-options (if (null? options) '() (car options))))
    (make-canvas-item
     (ask-widget canvas
		 `(create bitmap ,x ,y
			  -bitmap ,(string-append "@" bitmap-filename-string)
			  ,@configure-options))
     canvas)))
    
(define (make-line-on-canvas canvas x1 y1 x2 y2 . opt-args)
  (let loop ((opt-args opt-args) (xy-list '()) (configure-options '()))
    (if (null? opt-args)
	(if (odd? (length xy-list))
	    (error "MAKE-LINE:  Missing a y coordinate"
		   (append (list x1 y1 x2 y2) xy-list))
	    (make-canvas-item
	     (ask-widget canvas `(create line ,x1 ,y1 ,x2 ,y2 ,@xy-list
					 ,@configure-options))
	     canvas))
	(let ((next-arg (car opt-args)))
	  (if (list? next-arg)
	      (loop (cdr opt-args) xy-list next-arg)
	      (loop (cdr opt-args)
		    (append xy-list (list next-arg))
		    configure-options))))))

(define (make-oval-on-canvas canvas x1 y1 x2 y2 . options)
  (let ((configure-options (if (null? options) '() (car options))))
    (make-canvas-item
     (ask-widget canvas `(create oval ,x1 ,y1 ,x2 ,y2 ,@configure-options))
     canvas)))

(define (make-polygon-on-canvas canvas x1 y1 x2 y2 x3 y3 . opt-args)
  (let loop ((opt-args opt-args) (xy-list '()) (configure-options '()))
    (if (null? opt-args)
	(if (odd? (length xy-list))
	    (error "MAKE-POLYGON:  Missing a y coordinate"
		   (append (list x1 y1 x2 y2 x3 y3) xy-list))
	    (make-canvas-item
	     (ask-widget canvas `(create polygon ,x1 ,y1 ,x2 ,y2 ,x3 ,y3
					 ,@xy-list ,@configure-options))
	     canvas))
	(let ((next-arg (car opt-args)))
	  (if (list? next-arg)
	      (loop (cdr opt-args) xy-list next-arg)
	      (loop (cdr opt-args)
		    (append xy-list (list next-arg))
		    configure-options))))))

(define (make-rectangle-on-canvas canvas x1 y1 x2 y2 . options)
  (let ((configure-options (if (null? options) '() (car options))))
    (make-canvas-item
     (ask-widget canvas `(create rectangle ,x1 ,y1 ,x2 ,y2 ,@configure-options))
     canvas)))

(define (make-text-on-canvas canvas x y . options)
  (let ((configure-options (if (null? options) '() (car options))))
    (make-canvas-item
     (ask-widget canvas `(create text ,x ,y ,@configure-options))
     canvas)))

(define (make-widget-on-canvas canvas widget x y . options)
  (let ((configure-options (if (null? options) '() (car options))))
    (add-child! canvas widget)
    (make-canvas-item
     (ask-widget
      canvas
      `(create window ,x ,y
	       -window ,(lambda () (tk-widget->pathname widget))
	       ,@configure-options))
     canvas)))

