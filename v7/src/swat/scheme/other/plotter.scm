;;; -*- Scheme -*-

(declare (usual-integrations))

;;;; Plotting Package for Scheme Widget Application Toolkit

;;; Working from the Scheme Prompt

;;;(PLOTTER) 
;;;  Creates a new plotter.
;;; 
;;;     Example: (define p (plotter))
;;;
;;;(PLOT plotter . options)
;;;  The options list sequentially describes one or more curves to be
;;;  plotted, in the following manner: 
;;;
;;;    (PLOT plotter
;;;          <function1> '<option> <value> '<option> <value> ... ;first curve 
;;;          <function2> '<option> <value> ...                   ;second curve
;;;          ...
;;;          ...)
;;;  Returns a single curve if only one function is specified, and a
;;;  list of curves if more than one function is supplied.
;;;
;;;     Example: (define c0 (plot p sin 'xmin -10 'xmax 5))
;;;              (define c1&2 (plot p cos 'pt-style 0 tan 'pt-style 5))
;;;
;;;  The first parameter to PLOT after plotter must always be a
;;;  function. Curve-specific options affect only the function they
;;;  follow, and thus can and should be repeated.  Any instance of a
;;;  global option after the first will be ignored. 
;;;
;;;  Global options and arguments: 
;;;    'XMIN:  The minimum value of x to be displayed on the plot.
;;;            The default is 0. 
;;;    'XMAX:  The maximum value of x to be displayed on the plot.
;;;            The default is 1. 
;;;    'YMIN:  The minimum value of y to be displayed on the plot.
;;;            If not specified, the plot will be automatically scaled.
;;;    'YMAX:  The maximum value of y to be displayed on the plot.
;;;            If not specified, the plot will be automatically scaled.
;;;    'AXIS-X:  The value of x at which the y-axis will be drawn.
;;;              The default is 0. 
;;;    'AXIS-Y:  The value of y at which the x-axis will be drawn.
;;;              The default is 0. 
;;;    'XTICKS:  A list of pairs describing ticks on the x axis.  The
;;;              car of each pair is the value of x at which to make
;;;              the tick.  The cdr is a string to be displayed as a
;;;              label.  The procedure MAKE-VALS can be used to return
;;;              a list of values for labels at regular intervals. If
;;;              not specified, only the extreme values will be labeled. 
;;;    'YTICKS:  A list of pairs describing ticks on the y axis.  Same
;;;              format as XTICKS.  If not specified, only the extreme
;;;              values will be labeled. 
;;;
;;;  Curve-specific options and arguments
;;;    'NUM-PTS:  The number of points to be calculated for the curve.
;;;               The default is one for every 10 pixels.
;;;    'PT-STYLE:  A number representing the style in which the curve
;;;                will be drawn: 
;;;                  0  --  lines to the x-axis
;;;                  1  --  large unfilled circles
;;;                  2  --  large unfilled squares
;;;                  3  --  x's
;;;                  4  --  +'s
;;;                  5  --  small filled circles
;;;                  6  --  small filled squares
;;;                  7  --  dots
;;;                  10  --  large unfilled circles with lines to the x-axis
;;;                  20  --  large unfilled squares with lines to the x-axis
;;;                  30  --  x's with lines to the x-axis
;;;                  40  --  +'s with lines to the x-axis
;;;                  50  --  small filled circles with lines to the x-axis
;;;                  60  --  small filled squares with lines to the x-axis
;;;                  100  --  lines between successive points
;;;                The default for the first curve is 0, and for all
;;;                others 100. 
;;;    'COLOR:  The color of the curve, as a string or color-value.
;;;             The default for the first curve is black, and for all
;;;             others gray. 
;;;    'SHOW-VALS:  A list of values of x at which to label the
;;;                 corresponding value of y.  The procedure
;;;                 MAKE-VALS can be used to return a list of values
;;;                 at regular intervals.  The default is null.
;;;
;;;
;;;(SET-PLOTTER-PARAMS plotter '<option> <value> ... '<option> <value>)
;;;  Options are the same as global options in PLOT.  This does
;;;  basically the same thing as PLOT, but no *new* curve is drawn.
;;;  Parameters are reset and all the existing (non-cleared) curves
;;;  are redrawn.  Thus, an alternative way to write the example above
;;;  is: 
;;;
;;;     Example: (set-plotter-params p 'xmin -10 'xmax 5)
;;;              (define c0 (plot p sin))
;;;
;;;(RESET-PLOTTER-PARAMS plotter)
;;;  Resets plotter's parameters to default params (the ones you see
;;;  when the plotter first comes up).
;;;
;;;
;;;(MAKE-VALS min max spacing . centered?)
;;;  Returns a list of pairs that can be used for 'XTICKS 'YTICKS, or
;;;  'SHOW-VALS. If centered? is #t, the ticks will be centered about
;;;  0, with a tick at 0.  Otherwise, the ticks will begin at the min
;;;  value. 
;;; 
;;;     Example: (define c0 (plot p sin 'xmin -5 'xmax 5
;;;                                     'xticks (make-vals -5 5 1)))
;;;
;;;(CHANGE-COLOR curve color)
;;;  Changes the color of the given curve and replots the curve.
;;;  Replots the curve if it's not cleared.
;;;
;;;(CHANGE-PT-STYLE curve pt-style)
;;;  Changes the point style of the given curve and replots the curve.
;;;  Replots the curve if it's not cleared.
;;;
;;;(CHANGE-NUM-PTS curve num-pts)
;;;  Changes the number of points calculated for the given curve and
;;;  replots the curve. Replots the curve if it's not cleared.
;;;
;;;
;;;(CLEAR-CURVE curve)
;;;  Clears the given curve from the screen without deleting the curve
;;;  from the plotter.
;;;
;;;(PLOT-CURVE curve)
;;;  Replots the curve that has been cleared.
;;;
;;;(DELETE-CURVE curve)
;;;  Deletes the given curve from the plotter.
;;;
;;;(ADD-SHOW-VALS curve show-vals)
;;;  Add show-vals to a curve.
;;;
;;;(CLEAR-SHOW-VALS curve)
;;;  Clears all the curve's show vals, w/o deleting them from the curve.
;;;
;;;(DRAW-SHOW-VALS curve)
;;;  Redraws the cleared show-vals.
;;;
;;;(DELETE-SHOW-VALS curve)
;;;  Clears the curve's show-vals and deletes them from a curve.
;;;
;;;
;;;(ADD-XTICKS plotter xticks)
;;;  Adds the specified xticks.
;;;
;;;(ADD-YTICKS plotter yticks)
;;;  Adds the specified yticks.
;;;
;;;(CLEAR-TICKS plotter)
;;;  Clears ticks from the axes of the plotter, without deleting them
;;;  from the plotter.
;;;
;;;(DRAW-TICKS plotter)
;;;  Redraws the cleared ticks.
;;;
;;;(DELETE-TICKS plotter)
;;;  Clears ticks from the axes of the plotter and deletes them from
;;;  the plotter.
;;;
;;;
;;;(CLEAR-PLOTTER plotter)
;;;  Clears all plotter's curves and ticks.
;;;
;;;(REPLOT plotter)
;;;  Redraws all plotter's curves and ticks (including the cleared ones).
;;;
;;;(RESET-PLOTTER plotter)
;;;  Deletes all plotter's curves and ticks.



;;;-------------------
;;; Interface Monster
;;;-------------------

;;; Customizable Variables

(define button-background-color "yellow")
(define button-active-background-color "red")
(define button-active-foreground-color "white")
(define canvas-background-color "white")
(define canvas-width 500)
(define canvas-height 300)
(define canvas-border-size 15)
(define font "-Adobe-Helvetica-Bold-R-Normal--*-100-*")

(define tick-precision 2)
(define vals-precision 2)

(define curve-max-num-pts 200)

(define plotter-default-num-pts 50)
(define plotter-default-pt-style 100)
(define plotter-default-curve-color "black")
(define plotter-default-xmin -5)
(define plotter-default-xmax 5)
(define plotter-default-ymin -1)
(define plotter-default-ymax 1)
(define plotter-default-axis-x 0)
(define plotter-default-axis-y 0)
(define plotter-default-xticks '())
(define plotter-default-yticks '())


(define (plotter)
  (let* ((plot-app (make-application "Plotter"))
	 (plotter
	  (make-plot-canvas canvas-width canvas-height canvas-background-color))
	 (plot-canvas (plotter 'the-canvas))
	 (func-button (make-button '(-text "Function")))
	 (func-box #f)
	 (options-menu (make-menu))
	 (options-button (make-menubutton options-menu '(-text "Options")))
	 (precision (add-to-menu options-menu 'command '-label "Precision"))
	 (prec-box #f)
	 (range (add-to-menu options-menu 'command '-label "Range"))
	 (range-box #f)
	 (plot-button (make-button '(-text "Plot")))
	 (reset-button (make-button '(-text "Reset")))
	 (button-box (make-hbox func-button options-button plot-button reset-button))
	 (interface (make-vbox plot-canvas button-box)))

      (for-each (lambda (button)
		  (ask-widget
		   button
		   `(configure -background ,button-background-color
			       -activebackground ,button-active-background-color
			       -activeforeground ,button-active-foreground-color)))
		(list func-button options-button plot-button reset-button))
    
      (for-each (lambda (button)
		  (ask-widget
		   button
		   `(configure -background ,button-background-color
			       -activebackground ,button-background-color)))
		(list range precision))
    
      (add-event-handler! plot-canvas "<Configure>" (plotter 'handle-resize))

      (set-callback!
       func-button
       (lambda ()
	 (if (not func-box)
	     (let ((new-func-box (make-func-box plot-app plotter)))
	       (on-death! new-func-box 'func-dead (lambda () (set! func-box #f)))
	       (set! func-box new-func-box)))))

      (set-callback!
       precision
       (lambda ()
	 (if (not prec-box)
	     (let ((new-prec-box (make-prec-box plot-app plotter)))
	       (on-death! new-prec-box 'prec-dead (lambda () (set! prec-box #f)))
	       (set! prec-box new-prec-box)))))

      (set-callback!
       range
       (lambda ()
	 (if (not range-box)
	     (let ((new-range-box (make-range-box plot-app plotter)))
	       (on-death! new-range-box 'range-dead (lambda () (set! range-box #f)))
	       (set! range-box new-range-box)))))

      (set-callback! plot-button (lambda () (plotter 'plot-current-func)))
      (set-callback! reset-button (lambda () (plotter 'clear-curves)))

      (on-death! interface 'interface-dead 
		 (lambda ()
		   (if func-box  (remove-child! plot-app func-box))
		   (if range-box (remove-child! plot-app range-box))
		   (if prec-box  (remove-child! plot-app prec-box))))
      
      (swat-open-in-application plot-app interface)
      plotter))

(define (make-func-box plot-app plotter)
  (let* ((func-entry (make-entry `(-width 40 -background ,canvas-background-color)))
	 (func-ok-button
	  (make-button
	   `(-text "Ok" -background ,button-background-color
	     -activebackground ,button-active-background-color
	     -activeforeground ,button-active-foreground-color)))
	 (func-box (make-hbox func-entry func-ok-button)))
    (define (function-callback)
      (let ((exp (ask-widget func-entry '(get))))
	(if (not (string-null? exp))
	    ;; Of course, this could get an error while evaling; maybe
	    ;; need something more clever.
	    (let ((proc (eval (with-input-from-string exp read)
			      user-initial-environment)))
	      (if (not (procedure? proc))
		  (error "Not a procedure" proc)
		  ((plotter 'set-function) proc))))))
    (add-event-handler! func-entry "<KeyPress> <Return>" function-callback)
    (set-callback! func-ok-button function-callback)
    (swat-open-in-application plot-app func-box '-title "Enter a function of x")
    func-box))

(define (make-prec-box plot-app plotter)
  (let* ((prec-scale
	  (make-scale `(-from 0 -to ,curve-max-num-pts -orient horizontal
		        -length ,(inexact->exact (* 1.5 curve-max-num-pts))
			-background ,canvas-background-color
			-sliderforeground ,button-background-color
			-activeforeground ,button-active-background-color)))
	 (prec-redraw
	  (make-button `(-text "Redraw Curves" -background ,button-background-color
			 -activebackground ,button-active-background-color
			 -activeforeground ,button-active-foreground-color)))
	 (prec-box (make-vbox prec-scale prec-redraw)))
    (ask-widget prec-scale `(set ,(plotter 'default-num-pts)))
    (add-event-handler!
     prec-scale
     "<ButtonRelease-1>"
     (lambda ()
       ((plotter 'set-default-num-pts)
	(string->number (ask-widget prec-scale '(get))))))
    (set-callback! prec-redraw (lambda () (plotter 'plot-curves)))
    (swat-open-in-application plot-app prec-box '-title "Number of points:")
    prec-box))

(define (make-range-box plot-app plotter)
  (let* ((range-ok-button
	  (make-button `(-text "Ok" -background ,button-background-color
			 -activebackground ,button-active-background-color
			 -activeforeground ,button-active-foreground-color)))
	 (xmin-text (make-active-variable plot-app))
	 (xmax-text (make-active-variable plot-app))
	 (ymin-text (make-active-variable plot-app))
	 (ymax-text (make-active-variable plot-app))
	 (xmin-entry (make-entry `(-textvariable ,xmin-text)))
	 (xmax-entry (make-entry `(-textvariable ,xmax-text)))
	 (ymin-entry (make-entry `(-textvariable ,ymin-text)))
	 (ymax-entry (make-entry `(-textvariable ,ymax-text)))
	 (x-label (make-label '(-text "Values of x:")))
	 (xmin-label (make-label '(-text "From")))
	 (xmax-label (make-label '(-text "To")))
	 (y-label (make-label '(-text "Values of y:")))
	 (ymin-label (make-label '(-text "From")))
	 (ymax-label (make-label '(-text "To")))
	 (x-box
	  (make-vbox x-label
		     (make-hbox xmin-label xmin-entry xmax-label xmax-entry)))
	 (y-box
	  (make-vbox y-label
		     (make-hbox ymin-label ymin-entry ymax-label ymax-entry)))
	 (range-box (make-hbox (make-vbox x-box y-box) range-ok-button)))
    (for-each (lambda (label)
		(ask-widget label `(configure -background ,canvas-background-color)))
	      (list x-label xmin-label xmax-label y-label ymin-label ymax-label))
    (for-each (lambda (entry)
		;; background color?
		(ask-widget entry `(configure -width 5)))
	      (list xmin-entry xmax-entry ymin-entry ymax-entry))
    (set-callback!
     range-ok-button
     (lambda ()
       (let ((xmin (plotter 'xmin))
	     (xmax (plotter 'xmax))
	     (ymin (plotter 'ymin))
	     (ymax (plotter 'ymax))
	     (new-xmin (string->number (ask-widget xmin-entry '(get))))
	     (new-xmax (string->number (ask-widget xmax-entry '(get))))
	     (new-ymin (string->number (ask-widget ymin-entry '(get))))
	     (new-ymax (string->number (ask-widget ymax-entry '(get)))))
	 (if (not (and (eqv? xmin new-xmin)
		       (eqv? xmax new-xmax)
		       (eqv? ymin new-ymin)
		       (eqv? ymax new-ymax)))
	     (begin
	       ((plotter 'set-xmin) new-xmin)
	       ((plotter 'set-xmax) new-xmax)
	       ((plotter 'set-ymin) new-ymin)
	       ((plotter 'set-ymax) new-ymax)
	       (plotter 'clear)
	       (draw-axes plotter)
	       (plotter 'plot-curves))))))
    (swat-open-in-application plot-app range-box '-title "Range")
    (set-active-variable! xmin-text (plotter 'xmin))
    (set-active-variable! xmax-text (plotter 'xmax))
    (set-active-variable! ymin-text (plotter 'ymin))
    (set-active-variable! ymax-text (plotter 'ymax))
    range-box))


;;;-------------
;;; The Plotter
;;;-------------

(define (make-plot-canvas hsize vsize bgrnd-color)
  (let ((default-num-pts plotter-default-num-pts)
	(default-pt-style plotter-default-pt-style)
	(default-color plotter-default-curve-color)
	(xmin plotter-default-xmin)
	(xmax plotter-default-xmax)
	(ymin plotter-default-ymin)
	(ymax plotter-default-ymax)
	(yaxis.xval plotter-default-axis-x)
	(xaxis.yval plotter-default-axis-y)
	(xticks plotter-default-xticks)
	(yticks plotter-default-yticks)
	(current-func #f)
	(current-func-curve #f)
	(curve-list '())
	(resize-flag #f))
    (let* ((the-canvas (make-canvas `(-width ,hsize -height ,vsize
				      -background ,bgrnd-color)))
	   (axes-tag (make-canvas-item-group the-canvas '()))
	   (ticks-tag (make-canvas-item-group the-canvas '())))
      (define (plotter messg)
	(case messg
	  ((hsize) hsize)
	  ((vsize) vsize)
	  ((the-canvas) the-canvas)
	  ((curve-list) curve-list)
	  ((default-num-pts) default-num-pts)
	  ((set-default-num-pts)
	   (lambda (new-num-pts) (set! default-num-pts new-num-pts)))
	  ((default-pt-style) default-pt-style)
	  ((set-default-pt-style)
	   (lambda (new-pt-style) (set! default-pt-style new-pt-style)))
	  ((default-color) default-color)
	  ((set-default-color)
	   (lambda (new-color) (set! default-color new-color)))
	  ((function) current-func)
	  ((set-function)
	   (lambda (func)
	     (set! current-func-curve #f)
	     (set! current-func func)))
	  ((xmin) xmin)
	  ((set-xmin) (lambda (new-xmin) (set! xmin new-xmin)))
	  ((xmax) xmax)
	  ((set-xmax) (lambda (new-xmax) (set! xmax new-xmax)))
	  ((ymin) ymin)
	  ((set-ymin) (lambda (new-ymin) (set! ymin new-ymin)))
	  ((ymax) ymax)
	  ((set-ymax) (lambda (new-ymax) (set! ymax new-ymax)))
	  ((xaxis.yval) xaxis.yval)
	  ((yaxis.xval) yaxis.xval)
	  ((xaxis.y)
	   (let ((y-range (- ymax ymin)))
	     (if (= y-range 0)
		 (error "ymin and ymax are the same--MAKE-PLOT-CANVAS" ymin)
		 (+ (* (exact->inexact (/ (- (* canvas-border-size 2) vsize)
					  y-range))
		       (- xaxis.yval ymin))
		    vsize
		    (- canvas-border-size)))))
	  ((yaxis.x)
	   (let ((x-range (- xmax xmin)))
	     (if (= x-range 0)
		 (error "xmin and xmax are the same--MAKE-PLOT-CANVAS" xmin)
		 (+ (* (exact->inexact (/ (- hsize (* canvas-border-size 2))
					  (- xmax xmin)))
		       (- yaxis.xval xmin))
		    canvas-border-size))))
	  ((xticks) xticks)
	  ((set-xticks) (lambda (new-xticks) (set! xticks new-xticks)))
	  ((yticks) yticks)
	  ((set-yticks) (lambda (new-yticks) (set! yticks new-yticks)))
	  ((axes-tag) axes-tag)
	  ((ticks-tag) ticks-tag)
	  ((set-params)
	   (lambda (new-xmin new-xmax new-ymin new-ymax
			     new-yaxis.xval new-xaxis.yval new-xticks new-yticks)
	     (set! xmin new-xmin)
	     (set! xmax new-xmax)
	     (set! ymin new-ymin)
	     (set! ymax new-ymax)
	     (set! yaxis.xval new-yaxis.xval)
	     (set! xaxis.yval new-xaxis.yval)
	     (set! xticks new-xticks)
	     (set! yticks new-yticks)
	     'set))
	  ((x:val->pix) (x:val->pix xmin xmax hsize))
	  ((y:val->pix) (y:val->pix ymin ymax vsize))
	  ((add-curve)
	   (lambda (curve) (set! curve-list (append curve-list (list curve)))))
	  ((plot-current-func)
	   (if (and current-func (not current-func-curve))
	       (let ((new-curve
		      (make-curve plotter current-func default-pt-style
				  default-num-pts default-color #f)))
		 (set! current-func-curve new-curve)
		 (set! curve-list (cons new-curve curve-list))
		 (new-curve 'plot))))
	  ((plot-curves)
	   (for-each (lambda (curve)
		       (if (not (curve 'cleared?))
			   (curve 'plot)))
		     curve-list)
	   'plotted)
	  ((clear)
	   (ask-widget the-canvas '(delete all))
	   'cleared)
	  ((clear-curves)
	   (for-each (lambda (curve) (curve 'clear)) curve-list)
	   'cleared)
	  ((delete-curve)
	   (lambda (curve)
	     (curve 'clear)
	     (set! curve-list (delq curve curve-list))
	     'deleted))
	  ((delete-curves)
	   (for-each (lambda (curve) (curve 'clear)) curve-list)
	   (set! curve-list #f)
	   'deleted)
	  ((clear-axes)
	   (ask-widget axes-tag '(delete))
	   'cleared)
	  ((clear-ticks)
	   (ask-widget ticks-tag '(delete))
	   'cleared)
	  ((delete-ticks)
	   (set! xticks '())
	   (set! yticks '())
	   (ask-widget ticks-tag '(delete))
	   'deleted)
	  ((handle-resize)
	   (lambda ()
	     ;; For some reason, the "<Configure>" event gets generated
	     ;; twice per window resize -- so skip one of them.
	     (if (not resize-flag)
		 (set! resize-flag #t)
		 (begin
		   (set! resize-flag #f)
		   (ask-widget the-canvas '(delete all))
		   (let ((old-width hsize)
			 (width (UITKRectangle.width
				 (assigned-screen-area the-canvas)))
			 (height (UITKRectangle.height
				  (assigned-screen-area the-canvas))))
		     (set! hsize width)
		     (set! vsize height)
		     (set! default-num-pts (round (* default-num-pts
						     (/ width old-width))))
		     (draw-axes plotter)
		     (for-each
		      (lambda (curve)
			(curve-scale-num-pts!
			 curve (exact->inexact (/ width old-width)))
			(if (not (curve 'cleared?))
			    (begin (curve 'clear)
				   (curve 'plot))))
		      curve-list))))))
	  (else (error "Bad message--PLOTTER" messg))))
      plotter)))

(define ((x:val->pix xmin xmax hsize) x)
  (+ (* (exact->inexact
	 (/ (- hsize (* canvas-border-size 2))
	    (- xmax xmin)))
	(- x xmin))
     canvas-border-size))

(define ((y:val->pix ymin ymax vsize) y)
  (+ (* (exact->inexact
	 (/ (- (* canvas-border-size 2) vsize)
	    (- ymax ymin)))
	(- y ymin))
     vsize
     (- canvas-border-size)))

(define (draw-xticks plotter)
  (let ((xticks (plotter 'xticks)))
    (if xticks
	(let ((plot-canvas (plotter 'the-canvas))
	      (x:val->pix (plotter 'x:val->pix))
	      (xmin (plotter 'xmin))
	      (xmax (plotter 'xmax))
	      (xaxis.y (plotter 'xaxis.y))
	      (ticks-tag (plotter 'ticks-tag))
	      (factor (expt 10 tick-precision)))
	  (for-each
	   (lambda (tick)
	     (if (> xmax tick xmin)
		 (let ((val (x:val->pix tick))
		       (tag (swat:number->string
			     (/ (truncate (* factor tick)) factor))))
		   (add-to-canvas-item-group
		    ticks-tag
		    (make-line-on-canvas plot-canvas
					 val (- xaxis.y 4)
					 val (+ xaxis.y 4)))
		   (add-to-canvas-item-group
		    ticks-tag
		    (make-text-on-canvas plot-canvas
					 val (- xaxis.y 9)
					 `(-text ,tag -font ,font))))))
	   xticks))))
  'drawn)

(define (draw-yticks plotter)
  (let ((yticks (plotter 'yticks)))
    (if yticks
	(let ((plot-canvas (plotter 'the-canvas))
	      (y:val->pix (plotter 'y:val->pix))
	      (ymin (plotter 'ymin))
	      (ymax (plotter 'ymax))
	      (yaxis.x (plotter 'yaxis.x))
	      (ticks-tag (plotter 'ticks-tag))
	      (factor (expt 10 tick-precision)))
	  (for-each
	   (lambda (tick)
	     (if (> ymax tick ymin)
		 (let ((val (y:val->pix tick))
		       (tag (swat:number->string
			     (/ (truncate (* factor tick)) factor))))
		   (add-to-canvas-item-group
		    ticks-tag
		    (make-line-on-canvas plot-canvas
					 (- yaxis.x 4) val
					 (+ yaxis.x 4) val))
		   (add-to-canvas-item-group
		    ticks-tag
		    (make-text-on-canvas plot-canvas
					 (+ yaxis.x 6) val
					 `(-text ,tag -anchor w
						 -font ,font))))))
	   yticks))))
  'drawn)

(define (draw-axes plotter)
  (let* ((plot-canvas (plotter 'the-canvas))
	 (hsize (plotter 'hsize))
	 (vsize (plotter 'vsize))
	 (xmin  (plotter 'xmin))
	 (xmax  (plotter 'xmax))
	 (ymin  (plotter 'ymin))
	 (ymax  (plotter 'ymax))
	 (xaxis.yval (plotter 'xaxis.yval))
	 (yaxis.xval (plotter 'yaxis.xval))
	 (xaxis.y (plotter 'xaxis.y))
	 (yaxis.x (plotter 'yaxis.x))
	 (axes-tag (plotter 'axes-tag))
	 (trim 3)
	 (x-.x trim)
	 (x+.x (- hsize trim))
	 (y-.y trim)
	 (y+.y (- vsize trim)))
    (if (>= ymax xaxis.yval ymin)
	(begin
	  (add-to-canvas-item-group
	   axes-tag
	   (make-line-on-canvas plot-canvas x+.x xaxis.y x-.x xaxis.y '(-arrow both)))
	  (draw-xticks plotter)
	  (make-text-on-canvas plot-canvas
			       (- hsize trim) (- xaxis.y trim)
			       `(-text ,(swat:number->string xmax) -anchor se)) ;
	  (make-text-on-canvas plot-canvas
			       trim (- xaxis.y trim)
			       `(-text ,(swat:number->string xmin) -anchor sw))))
    (if (>= xmax yaxis.xval xmin)
	(begin
	  (add-to-canvas-item-group
	   axes-tag
	   (make-line-on-canvas plot-canvas yaxis.x y+.y yaxis.x y-.y '(-arrow both)))
	  (draw-yticks plotter)
	  (let ((factor (expt 10 tick-precision)))
	    (make-text-on-canvas plot-canvas
				 (+ yaxis.x 8) trim
				 `(-text ,(swat:number->string
					   (/ (round (* ymax factor)) factor))
					 -anchor nw))
	    (make-text-on-canvas plot-canvas
				 (+ yaxis.x 8) vsize
				 `(-text ,(swat:number->string
					   (/ (round (* ymin factor)) factor))
					 -anchor sw)))))
    'done))

;;;--------
;;; Curves
;;;--------

(define (make-curve plotter function pt-style num-pts color show-vals)
  (let* ((plot-canvas (plotter 'the-canvas))
	 (curve-tag (make-canvas-item-group plot-canvas '()))
	 (outline-tag (make-canvas-item-group plot-canvas '()))
	 (vals-tag (make-canvas-item-group plot-canvas '()))
	 (cleared? #f))
    (lambda (messg)
      (case messg
	((plotter) plotter)
	((num-pts) num-pts)
	((set-num-pts) (lambda (new-num-pts) (set! num-pts new-num-pts)))
	((show-vals) show-vals)
	((set-show-vals) (lambda (new-vals) (set! show-vals new-vals)))
	((cleared?) cleared?)
	((change-pt-style)
	 (lambda (new-pt-style)
	   (cond ((pt-style? new-pt-style)
		  (set! pt-style new-pt-style))
		 (else (write-line "Not a style--MAKE-CURVE") pt-style))))
	((change-color)
	 (lambda (new-color)
	   (set! color new-color)
	   (if (not cleared?)
	       (begin
		 (ask-widget curve-tag `(configure -fill ,color))
		 (ask-widget outline-tag `(configure -outline ,color))
		 (ask-widget vals-tag `(configure -fill ,color))))))
	((get-extreme-vals)
	 (lambda (min max)
	   (get-extreme-vals function min max num-pts)))
	((plot)
	 (graph function plotter curve-tag outline-tag pt-style num-pts color)
	 (if show-vals
	     (graph-vals function plotter show-vals vals-tag color))
	 (set! cleared? #f)
	 'plotted)
	((draw-vals)
	 (if show-vals
	     (graph-vals function plotter show-vals vals-tag color))
	 'drawn)
	((clear-vals)
	 (ask-widget vals-tag '(delete))
	 'cleared)
	((delete-vals)
	 (ask-widget vals-tag '(delete))
	 (set! show-vals #f)
	 'removed)
	((clear)
	 (ask-widget curve-tag '(delete))
	 (ask-widget outline-tag '(delete))
	 (ask-widget vals-tag '(delete))
	 (set! cleared? #t)
	 'cleared)
	(else (error "Bad message--MAKE-CURVE" messg))))))

(define (get-extreme-vals function min max num-pts)
  (let* ((factor (expt 10 vals-precision))
	 (first-val (function min))
	 (min-val first-val)
	 (max-val first-val)
	 (step (exact->inexact (/ (- max min) num-pts))))
    (define (calculate x)
      (let ((val (function x)))
	(cond ((> x max)
	       (list (/ (round (* min-val factor)) factor)
		     (/ (round (* max-val factor)) factor)))
	      ((< val min-val) (set! min-val val)
			       (calculate (+ x step)))
	      ((> val max-val) (set! max-val val)
			       (calculate (+ x step)))
	      (else (calculate (+ x step))))))
    (calculate (+ min step))))

(define (pt-style? val)
  (memv val '(0 1 2 3 4 5 6 7 10 20 30 40 50 60 100)))

(define (curve-scale-num-pts! curve factor)
  ((curve 'set-num-pts) (round (* (curve 'num-pts) factor))))

(define (maybe-replot-curve curve)
  (if (not (curve 'cleared?))
      (begin (curve 'clear)
	     (curve'plot))))

(define (graph function plotter curve-tag outline-tag pt-style num-pts color)
  (let ((plot-canvas (plotter 'the-canvas))
	(xmin (plotter 'xmin))
	(xmax (plotter 'xmax))
	(xaxis.yval (plotter 'xaxis.yval))
	(x:val->pix (plotter 'x:val->pix))
	(y:val->pix (plotter 'y:val->pix)))
    (let ((xaxis.y (y:val->pix xaxis.yval)))

      (define (draw-0 x y)
	  (add-to-canvas-item-group
	   curve-tag (make-line-on-canvas plot-canvas x xaxis.y x y)))
      (define (draw-1 x y)
	(add-to-canvas-item-group
	 outline-tag
	 (make-oval-on-canvas plot-canvas (- x 3) (- y 3) (+ x 3) (+ y 3))))
      (define (draw-2 x y)
	(add-to-canvas-item-group
	 outline-tag 
	 (make-rectangle-on-canvas plot-canvas (- x 3) (- y 3) (+ x 3) (+ y 3))))
      (define (draw-3 x y)
	(add-to-canvas-item-group
	 curve-tag 
	 (make-line-on-canvas plot-canvas (- x 2) (- y 2) (+ x 3) (+ y 3)))
	(add-to-canvas-item-group
	 curve-tag
	 (make-line-on-canvas plot-canvas (+ x 2) (- y 2) (- x 2) (+ y 2))))
      (define (draw-4 x y)
	(add-to-canvas-item-group
	 curve-tag (make-line-on-canvas plot-canvas x (- y 2) x (+ y 3)))
	(add-to-canvas-item-group
	 curve-tag (make-line-on-canvas plot-canvas (- x 2) y (+ x 3) y)))
      (define (draw-5 x y)
	(let ((seg (make-oval-on-canvas plot-canvas
					(- x 2) (- y 2) (+ x 2) (+ y 2))))
	  (add-to-canvas-item-group curve-tag seg)
	  (add-to-canvas-item-group outline-tag seg)))
      (define (draw-6 x y)
	(let ((seg (make-rectangle-on-canvas plot-canvas
					     (- x 2) (- y 2) (+ x 2) (+ y 2))))
	  (add-to-canvas-item-group curve-tag seg)
	  (add-to-canvas-item-group outline-tag seg)))
      (define (draw-7 x y)
	(add-to-canvas-item-group
	 curve-tag (make-text-on-canvas plot-canvas x (- y 2) '(-text "."))))
      (define (draw-10 x y)
	(add-to-canvas-item-group
	 curve-tag (make-line-on-canvas plot-canvas x xaxis.y x (+ y 3)))
	(add-to-canvas-item-group
	 outline-tag (make-oval-on-canvas
		      plot-canvas (- x 3) (- y 3) (+ x 3) (+ y 3))))
      (define (draw-20 x y)
	(add-to-canvas-item-group
	 curve-tag (make-line-on-canvas plot-canvas x xaxis.y x (+ y 2)))
	(add-to-canvas-item-group
	 outline-tag
	 (make-rectangle-on-canvas plot-canvas
				   (- x 3) (- y 3) (+ x 3) (+ y 3))))
      (define (draw-30 x y)
	(add-to-canvas-item-group
	 curve-tag (make-line-on-canvas plot-canvas
					 (- x 2) (- y 2) (+ x 3) (+ y 3)))
	(add-to-canvas-item-group
	 curve-tag (make-line-on-canvas plot-canvas
					 (+ x 2) (- y 2) (- x 2) (+ y 2)))
	(add-to-canvas-item-group
	 curve-tag (make-line-on-canvas plot-canvas x xaxis.y x y)))
      (define (draw-40 x y)
	(add-to-canvas-item-group
	 curve-tag (make-line-on-canvas plot-canvas x (- y 2) x xaxis.y))
	(add-to-canvas-item-group
	 curve-tag (make-line-on-canvas plot-canvas (- x 2) y (+ x 3) y)))
      (define (draw-50 x y)
	(let ((seg1 (make-oval-on-canvas plot-canvas
					 (- x 2) (- y 2) (+ x 2) (+ y 2)))
	      (seg2 (make-line-on-canvas plot-canvas x xaxis.y x (+ y 2))))
	  (add-to-canvas-item-group outline-tag seg1)
	  (add-to-canvas-item-group curve-tag seg1)
	  (add-to-canvas-item-group curve-tag seg2)))
      (define (draw-60 x y)
	(let ((seg1 (make-rectangle-on-canvas plot-canvas
					      (- x 2) (- y 2) (+ x 2) (+ y 2)))
	      (seg2 (make-line-on-canvas plot-canvas x xaxis.y x (+ y 2))))
	  (add-to-canvas-item-group outline-tag seg1)
	  (add-to-canvas-item-group curve-tag seg1)
	  (add-to-canvas-item-group curve-tag seg2)))

      (define (draw-dispatch pt-style)
	(cond ((= pt-style 0) draw-0)
	      ((= pt-style 1) draw-1)
	      ((= pt-style 2) draw-2)
	      ((= pt-style 3) draw-3)
	      ((= pt-style 4) draw-4)
	      ((= pt-style 5) draw-5)
	      ((= pt-style 6) draw-6)
	      ((= pt-style 7) draw-7)
	      ((= pt-style 10) draw-10)
	      ((= pt-style 20) draw-20)
	      ((= pt-style 30) draw-30)
	      ((= pt-style 40) draw-40)
	      ((= pt-style 50) draw-50)
	      ((= pt-style 60) draw-60)))

      (let* ((draw (draw-dispatch pt-style))
	     (xstep (exact->inexact (/ (- xmax xmin) num-pts))))
	(define (calc-100 last-x last-y x y)
	  (if (not (> x xmax))
	      (let ((segment
		     (make-line-on-canvas plot-canvas
					  (x:val->pix last-x)
					  (y:val->pix last-y)
					  (x:val->pix x)
					  (y:val->pix y))))
		(add-to-canvas-item-group curve-tag segment)
		(calc-100 x y (+ x xstep) (function (+ x xstep))))))
	(define (calculate x y)
	  (if (not (> x xmax))
	      (begin (draw (x:val->pix x) (y:val->pix y))
		     (calculate (+ x xstep) (function (+ x xstep))))))

	(if (= pt-style 100)
	    (calc-100 xmin (function xmin) (+ xmin xstep) (function (+ xmin xstep)))
	    (calculate xmin (function xmin)))
	(ask-widget curve-tag `(configure -fill ,color))
	(ask-widget outline-tag `(configure -outline ,color))))))

(define (graph-vals function plotter show-vals vals-tag color)
  (let ((factor (expt 10 vals-precision))
	(x:val->pix (plotter 'x:val->pix))
	(y:val->pix (plotter 'y:val->pix))
	(plot-canvas (plotter 'the-canvas)))
    (let marker ((show-vals show-vals))
      (if (not (null? show-vals))
	  (let* ((x-val (car show-vals))
		 (x (x:val->pix x-val))
		 (y-val (function x-val))
		 (y (y:val->pix y-val))
		 (pos-y? (>= y-val (plotter 'xaxis.yval))))
	    (add-to-canvas-item-group
	     vals-tag
	     (make-text-on-canvas
	      plot-canvas x (if pos-y? (- y 3) (+ y 6))
	      `(-text ,(swat:number->string (/ (round (* y-val factor)) factor))
		-anchor ,(if pos-y? 's 'n))))
	    (add-to-canvas-item-group
	     vals-tag
	     (make-text-on-canvas plot-canvas x y '(-text "|")))
	    (marker (cdr show-vals)))))
    (ask-widget vals-tag `(configure -fill ,color))))


;;;-------------------------
;;; Scheme-prompt Interface
;;;-------------------------

(define (plot plotter . spec-list)
  (define (package-curves arg-list)
    (let package-loop ((result (list (car arg-list)))
		       (rest (cdr arg-list)))
      (cond ((null? rest) (list (reverse result)))
	    ((procedure? (car rest))
	     (cons (reverse result) (package-curves rest)))
	    (else (package-loop (cons (car rest) result) (cdr rest))))))
  (if (not (null? spec-list))
      (let* ((curve-desc-list (package-curves spec-list))
	     (old-xmin (plotter 'xmin))
	     (old-xmax (plotter 'xmax))
	     (old-ymin (plotter 'ymin))
	     (old-ymax (plotter 'ymax))
	     (old-axis-y (plotter 'xaxis.yval))
	     (old-axis-x (plotter 'yaxis.xval))
	     (old-xticks (plotter 'xticks))
	     (old-yticks (plotter 'yticks))
	     (xmin~ #f) (axis-x~ #f) (num-pts~ #f)
	     (xmax~ #f) (axis-y~ #f) (pt-style~ #f) 
	     (ymin~ #f) (xticks~ #f) (color~ #f)
	     (ymax~ #f) (yticks~ #f) (show-vals~ #f)
	     (default-num-pts  (plotter 'default-num-pts))
	     (default-pt-style (plotter 'default-pt-style))
	     (default-color    (plotter 'default-color))
	     (curve-list '()))

	(define (process-next-curve curve-desc)
	  (let ((f (car curve-desc))
		(curve-options (cdr curve-desc)))
	    (let curve-loop ((curve-options curve-options))
	      (if (not (null? curve-options))
		  (let ((option-name (car curve-options)))
		    (cond ((not (symbol? option-name))
			   (error "Bad option--PLOT" option-name))
			  ((null? (cdr curve-options))
			   (error "PLOT: No value specified for option"
				  option-name))
			  (else
			   (let ((option-value (cadr curve-options)))
			     (process-option option-name option-value)
			     (curve-loop (cddr curve-options))))))))
	    (make-curve plotter
			f
			(or pt-style~ default-pt-style)
			(or num-pts~ default-num-pts)
			(or color~ default-color)
			show-vals~)))

	(define (process-option name value)
	  (case name
	    ;; global options
	    ((xmin) (if (not xmin~) (set! xmin~ value)))
	    ((xmax) (if (not xmax~) (set! xmax~ value)))
	    ((ymin) (if (not ymin~) (set! ymin~ value)))
	    ((ymax) (if (not ymax~) (set! ymax~ value)))
	    ((axis-x) (if (not axis-x~) (set! axis-x~ value)))
	    ((axis-y) (if (not axis-y~) (set! axis-y~ value)))
	    ((xticks) (if (not xticks~) (set! xticks~ value)))
	    ((yticks) (if (not yticks~) (set! xticks~ value)))
	    ;; curve-specific options
	    ((num-pts) (set! num-pts~ value))
	    ((pt-style) (set! pt-style~ value))
	    ((color) (set! color~ value))
	    ((show-vals) (set! show-vals~ value))
	    (else (error "Illegal option--PLOT" name))))

	(define (reset-options!)
	  (set! num-pts~ #f)
	  (set! pt-style~ #f)
	  (set! color~ #f)
	  (set! show-vals~ #f))

	  (let process-loop ((curve-desc-list (reverse curve-desc-list)))
	    (if (not (null? curve-desc-list))
		(let ((new-curve (process-next-curve (car curve-desc-list))))
		  ((plotter 'add-curve) new-curve)
		  (set! curve-list (cons new-curve curve-list))
		  (reset-options!)
		  (process-loop (cdr curve-desc-list)))))
	
	  (let* ((xmin (or xmin~ old-xmin))
		 (xmax (or xmax~ old-xmax))
		 (get-extremes
		  (lambda (xmin xmax)
		    (map (lambda (curve) ((curve 'get-extreme-vals) xmin xmax))
			 curve-list)))
		 (extremes #f)
		 (ymin
		  (or ymin~
		      (min
		       old-ymin
		       (let ((xtremes (get-extremes xmin xmax)))
			 (set! extremes xtremes)
			 (apply min (cons 0 (map (lambda (e) (car e)) xtremes)))))))
		 (ymax
		  (or ymax~
		      (max
		       old-ymax
		       (let ((xtremes
			      (if extremes extremes (get-extremes xmin xmax))))
			 (apply max (cons 0 (map (lambda (e) (cadr e)) xtremes)))))))
		 (axis-y (or axis-y~ old-axis-y))
		 (axis-x (or axis-x~ old-axis-x)))

	    (if (and (= xmin old-xmin)
		     (= xmax old-xmax)
		     (= ymin old-ymin)
		     (= ymax old-ymax)
		     (= axis-x old-axis-x)
		     (= axis-y old-axis-y)
		     (equal? xticks~ old-xticks)
		     (equal? yticks~ old-yticks))
		;; only plot the new curves
		(for-each (lambda (new-curve) (new-curve 'plot))
			  curve-list)
	        ;; if a global param changed, replot everything
		(begin
		  ((plotter 'set-params)
		   xmin xmax ymin ymax axis-x axis-y xticks~ yticks~)
		  (plotter 'clear)
		  (draw-axes plotter)
		  (plotter 'plot-curves)))
	    
	    ;; return the curve if there's only one, list of curves if more.
	    (and (pair? curve-list)
		 (if (= (length curve-list) 1)
		     (car curve-list)
		     curve-list))))))

(define (set-plotter-params plotter . spec-list)
  (let ((xmin (plotter 'xmin))
	(xmax (plotter 'xmax))
	(ymin (plotter 'ymin))
	(ymax (plotter 'ymax))
	(axis-x (plotter 'yaxis.xval))
	(axis-y (plotter 'xaxis.yval))
	(xticks (plotter 'xticks))
	(yticks (plotter 'yticks)))
    (define (process-option name value)
      (case name
	;; global options
	((xmin) (set! xmin value))
	((xmax) (set! xmax value))
	((ymin) (set! ymin value))
	((ymax) (set! ymax value))
	((axis-x) (set! axis-x value))
	((axis-y) (set! axis-y value))
	((xticks) (set! xticks value))
	((yticks) (set! xticks value))
	(else (error "Illegal option--SET-PLOTTER-PARAMS" name))))
    (let process-loop ((options spec-list))
      (if (not (null? options))
	  (let ((option-name (car options)))
	    (cond ((not (symbol? option-name))
		   (error "Bad option--PLOT" option-name))
		  ((null? (cdr options))
		   (error "SET-PLOTTER-PARAMS: No value specified for option"
			  option-name))
		  (else
		   (let ((option-value (cadr options)))
		     (process-option option-name option-value)
		     (process-loop (cddr options))))))))
    ((plotter 'set-params) xmin xmax ymin ymax axis-x axis-y xticks yticks)
    (plotter 'clear)
    (draw-axes plotter)
    (plotter 'plot-curves)))

(define (reset-plotter-params plotter)
  (apply set-plotter-params
	 (list 'xmin plotter-default-xmin
	       'xmax plotter-default-xmax
	       'ymin plotter-default-ymin
	       'ymax plotter-default-ymax
	       'axis-x plotter-default-axis-x
	       'axis-y plotter-default-axis-y
	       'xticks plotter-default-xticks
	       'yticks plotter-default-yticks)))


(define (make-vals min max spacing . center?)
  (let ((min (if center? (* spacing (round (/ min spacing))) min)))
    (define (tick-maker val)
      (if (> val max)
	  '()
	  (cons val (tick-maker (+ val spacing)))))
    (tick-maker min)))


(define (change-color curve color)
  ((curve 'change-color) color))
  
(define (change-pt-style curve pt-style)
  ((curve 'change-pt-style) pt-style)
  (maybe-replot-curve curve))

(define (change-num-pts curve num-pts)
  ((curve 'set-num-pts) num-pts)
  (maybe-replot-curve curve))

(define (clear-curve curve)
  (curve 'clear))

(define (plot-curve curve)
  (if (curve 'cleared?)
      (curve 'plot)))

(define (delete-curve curve)
  (((curve 'plotter) 'delete-curve) curve))

(define (add-show-vals curve show-vals)
  (curve 'clear-vals)
  ((curve 'set-show-vals)
   (append (curve 'show-vals) show-vals))
  (curve 'draw-vals))

(define (clear-show-vals curve)
  (curve 'clear-vals))

(define (draw-show-vals curve)
  (curve 'draw-vals))

(define (delete-show-vals curve)
  (curve 'delete-vals))

			 
(define (add-xticks plotter xticks)
  ((plotter 'set-xticks)
   (append (plotter 'xticks) xticks))
  (plotter 'clear-axes)
  (draw-axes plotter))

(define (add-yticks plotter yticks)
  ((plotter 'set-yticks)
   (append (plotter 'xticks) yticks))
  (plotter 'clear-axes)
  (draw-axes plotter))

(define (clear-ticks plotter)
  (plotter 'clear-ticks))

(define (draw-ticks plotter)
  (draw-xticks plotter)
  (draw-yticks plotter))

(define (delete-ticks plotter)
  (plotter 'delete-ticks))

(define (clear-plotter plotter)
  (plotter 'clear-curves)
  (plotter 'clear-ticks))

(define (replot plotter)
  (draw-ticks plotter)
  (for-each plot-curve (plotter 'curve-list))
  'replotted)

(define (reset-plotter plotter)
  (plotter 'delete-curves)
  (plotter 'delete-ticks)
  (plotter 'clear)
  (draw-axes plotter)
  'reset)
  
