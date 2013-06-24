;;; -*- Scheme -*-

(declare (usual-integrations))

;;;Demo of DT frequency response by frobbing poles and zeros

(define half-window-size 200)
(define zero-size 5)
(define pole-size 4)
(define trim 10)
(define zero-color "violetred")
(define pole-color "blue")
(define canvas-color "white")
(define text-font "CourR12")


(define symbol-font
  "-adobe-symbol-medium-r-normal--14-100-100-100-p-85-adobe-fontspecific")

(define tracking-coords? #F)
(define time-to-update-plot? #F)
(define LOCATION 'later)	; active variable
(define all-zeros '())		; alist of zeros(objects)/coords
(define all-poles '())  	; alist of poles(objects)/coords

(define number-of-points 100)
(define max-w  3.14159)

;;hack to print numbers to three decimals
(define (unsigned->string n)
  (let* ((int-part (floor n))
	 (frac-part (- n int-part))
	 (dec (floor->exact (* frac-part 1000)))
	 (string-dec (number->string dec))
	 (padded-string-dec
	  (cond ((< dec 10) (string-append "00" string-dec))
		((< dec 100) (string-append "0" string-dec))
		(else string-dec))))
    (string-append (number->string (floor->exact int-part))
		   "."
		   padded-string-dec)))

(define (our-cx->string z)
  (let* ((r (real-part z))
	 (i (imag-part z))
	 (rs (unsigned->string (abs r)))
	 (is (unsigned->string (abs i)))
	 (signed-r
	  (if (< r 0)
	      (string-append "-" rs)
	      rs))
	 (signed-i
	  (if (< i 0)
	      (string-append "-" is)
	      (string-append "+" is))))
    (string-append signed-r signed-i "j")))

(define (our-real->string r)
  (let* ((rs (unsigned->string (abs r)))
	 (signed-r
	  (if (< r 0)
	      (string-append "-" rs)
	      rs)))
    signed-r))


(define (z->canvas-coords z)
  (let ((x (real-part z))
	(y (imag-part z)))
    (list 
     (round->exact
      (+ (* x (- half-window-size (* 2 trim)))
	 half-window-size))
     (round->exact
      (+ (* y (- (* 2 trim) half-window-size))
	 half-window-size)))))

(define (canvas-coords->z xy)
  (let ((x (exact->inexact (car xy)))
	(y (exact->inexact (cadr xy))))
    (let ((real (/ (- x half-window-size)
		   (- half-window-size (* 2 trim))))
	  (imag (/ (- y half-window-size)
		   (- (* 2 trim) half-window-size))))
      (+ real (* imag +i)))))

;;; Pole/Zero Movement
(define (move-with-conjugate-pair pole-zero obj1 obj2)
  (let ((last-x 'later)
	(last-y 'later))
    (define (keep-track-of-coords x y)
      (set! last-x x)
      (set! last-y y)
      (if tracking-coords?
	  (let ((z (canvas-coords->z (list last-x last-y))))
	    (set-active-variable! LOCATION (our-cx->string z)))))
    (define (store-coords)
      (let* ((zero-entry (assq obj1 all-zeros))
	     (obj1-entry
	      (if zero-entry zero-entry (assq obj1 all-poles)))
	     (obj2-entry
	      (if zero-entry
		  (assq obj2 all-zeros)
		  (assq obj2 all-poles)))
	     (z (canvas-coords->z (list last-x last-y))))
	(set-cdr! obj1-entry z)
	(set-cdr! obj2-entry (conjugate z))))
    (add-event-handler!
     obj1
     "<ButtonPress-1>"
     (lambda (x y)
       (set! time-to-update-plot? #F)
       (keep-track-of-coords x y))
     "%x" "%y")
    (add-event-handler!
     obj1
     "<ButtonRelease-1>"
     (lambda ()
       (store-coords)
       (maybe-update-plot (pole-zero 'graph-canvas))
       ))
    (add-event-handler!
     obj1
     "<B1-Motion>"
     (lambda (x y)
       (ask-widget obj1 `(move ,(- x last-x) ,(- y last-y)))
       (ask-widget obj2 `(move ,(- x last-x) ,(- last-y y)))
       (keep-track-of-coords x y))
     "%x" "%y")))

(define (move-by-itself pole-zero obj)
  (let ((last-x 'later))
    (define (keep-track-of-coords x)
      (set! last-x x)
      (let ((z (canvas-coords->z (list last-x half-window-size))))
	(if tracking-coords?
	    (set-active-variable! LOCATION (our-real->string z)))))
    (define (store-coords)
      (let ((entry
	     (let ((zero (assq obj all-zeros)))
	       (if zero zero (assq obj all-poles))))
	    (z (real-part (canvas-coords->z (list last-x 0)))))
	(set-cdr! entry z)))
    (add-event-handler!
     obj
     "<ButtonPress-1>"
     (lambda (x)
       (set! time-to-update-plot? #F)
       (keep-track-of-coords x))
     "%x")
    (add-event-handler!
     obj
     "<ButtonRelease-1>"
     (lambda ()
       (store-coords)
       (maybe-update-plot (pole-zero 'graph-canvas))
       ))
    (add-event-handler!
     obj
     "<B1-Motion>"
     (lambda (x)
       (ask-widget obj `(move ,(- x last-x) 0))
       (keep-track-of-coords x))
     "%x")))


;;; This isn't quite right.  Time-to-update-plot? might be set to #F
;;; and then back to #T inside the 2 sec interval, so the update will
;;; come too soon.
(define (maybe-update-plot graph-canvas)
  (set! time-to-update-plot? #T)
  (after-delay
   2
   (lambda ()
     (if time-to-update-plot?
	 (plot-pole-zero graph-canvas)))))


;;; Zeros
(define (make-zero canvas xy)
  (let ((x (car xy))
	(y (cadr xy)))
    (let ((zero
	   (make-oval-on-canvas canvas
				(- x zero-size) (- y zero-size)
				(+ x zero-size) (+ y zero-size))))
      (set! all-zeros (cons (cons zero (canvas-coords->z xy))
			    all-zeros))
      (ask-widget zero `(configure -outline ,zero-color -fill ,canvas-color -width 2))
      zero)))

(define (make-single-zero pole-zero x)
  (let ((canvas (pole-zero 'diagram-canvas)))
    (let ((z (make-zero canvas (list x half-window-size))))
      (move-by-itself pole-zero z)
      z)))

(define (make-zero-pair pole-zero x y)
  (let ((canvas (pole-zero 'diagram-canvas)))
    (let ((zero (canvas-coords->z (list x y))))
      (let ((other-pos
	     (z->canvas-coords (conjugate zero))))
	(let ((z1 (make-zero canvas (list x y)))
	      (z2 (make-zero canvas other-pos)))
	  (move-with-conjugate-pair pole-zero z1 z2)
	  (move-with-conjugate-pair pole-zero z2 z1))))))


;;; Poles
(define (make-pole canvas xy)
  (let ((x (car xy))
	(y (cadr xy)))
    (let* ((line1
	    (make-line-on-canvas canvas
				 (- x pole-size) (- y pole-size)
				 (+ x pole-size) (+ y pole-size)))
	   (line2 
	    (make-line-on-canvas canvas
				 (- x pole-size) (+ y pole-size)
				 (+ x pole-size) (- y pole-size)))
	   (pole (make-canvas-item-group canvas (list line1 line2))))
      (set! all-poles (cons (cons pole (canvas-coords->z xy))
			    all-poles))
      (ask-widget pole `(configure -fill ,pole-color -width 2))
      pole)))

(define (make-single-pole pole-zero x)
  (let ((canvas (pole-zero 'diagram-canvas)))
    (let ((p (make-pole canvas (list x half-window-size))))
      (move-by-itself pole-zero p)
      p)))

(define (make-pole-pair pole-zero x y)
  (let ((canvas (pole-zero 'diagram-canvas)))
    (let ((pole (canvas-coords->z (list x y))))
      (let ((other-pos
	     (z->canvas-coords (conjugate pole))))
	(let ((p1 (make-pole canvas (list x y)))
	      (p2 (make-pole canvas other-pos)))
	  (move-with-conjugate-pair pole-zero p1 p2)
	  (move-with-conjugate-pair pole-zero p2 p1))))))

;;; Button that switches from one label to another

(define (make-switch color to-switch)
  ;;to-switch is list ((text command) (text command))
  (let ((n (length to-switch))
	(button (make-button))
	(state #F))
    (define (switch-to-state i)
      (set! state i)
      ((cadr (list-ref to-switch i)))
      (ask-widget button `(configure -text ,(car (list-ref to-switch i)))))
    (ask-widget button `(configure -background ,color))
    (switch-to-state 0)
    (set-callback! button
		   (lambda ()
		     (switch-to-state (modulo (+ state 1) n))))
    button))

;;; Demo
(define (make-pole-zero)
  (set! all-zeros '())
  (set! all-poles '())
  (let ((diagram-canvas (make-canvas `(-width ,(* 2 half-window-size)
				       -height ,(* 2 half-window-size))))
	(graph-canvas #F)
	(pz 'later)
	(shape-size 'later)
	(single-maker 'later)
	(pair-maker 'later))
    
    (define (switch-to-zeros)
      (set! shape-size zero-size)
      (set! single-maker make-single-zero)
      (set! pair-maker make-zero-pair))

    (define (switch-to-poles)
      (set! shape-size pole-size)
      (set! single-maker make-single-pole)
      (set! pair-maker make-pole-pair))

    (let* ((maker-button (make-switch "yellow"
				      `(("Zeros" ,switch-to-zeros)
					("Poles" ,switch-to-poles))))
	   (clear-button (make-button '(-text "Clear")))
	   (show-coords? (make-active-variable))
	   (coords-button
	    (make-checkbutton `(-text "Show Coords?" -variable ,show-coords?)))
	   (coords-display (make-label))
	   (plot-button (make-button '(-text "Plot"))))

      (set! LOCATION (make-active-variable))
      (ask-widget coords-display `(configure -width 13 -background ,canvas-color
					     -relief sunken -textvariable ,LOCATION
					     -font ,text-font))
      (for-each (lambda (b)
		  (ask-widget b `(configure -background "yellow" -font ,text-font)))
		(list maker-button clear-button  coords-button plot-button))
      (ask-widget diagram-canvas `(configure -background ,canvas-color))
      (on-death! diagram-canvas 'little-brother-canvas
		 (lambda () (if graph-canvas (swat-close graph-canvas))))
      
      (set-callback!
       clear-button
       (lambda ()
	 (for-each (lambda (entry) (ask-widget (car entry) '(delete)))
		   all-zeros)
	 (for-each (lambda (entry) (ask-widget (car entry) '(delete)))
		   all-poles)
	 (set-active-variable! LOCATION "")
	 (cond (graph-canvas
		(ask-widget graph-canvas '(delete all))
		(draw-axes graph-canvas)))
	 (set! all-zeros '())
	 (set! all-poles '())))

      (set-callback!
       coords-button
       (lambda ()
	 (if (checkbutton-variable-on? show-coords?)
	     (set! tracking-coords? #T)
	     (begin
	       (set-active-variable! LOCATION "")
	       (set! tracking-coords? #F)))))
      (set-callback!
       plot-button
       (lambda ()
	 (cond ((not graph-canvas)
		(set! graph-canvas
		      (make-canvas `(-width ,(* 2 half-window-size)
				     -height ,(* 2 half-window-size))))
		(ask-widget graph-canvas `(configure -background ,canvas-color))
		(swat-open graph-canvas '-title "Magnitude of Frequency Response")
		(on-death! graph-canvas 'big-brother-canvas
			   (lambda () (set! graph-canvas #F)))))
	 (plot-pole-zero graph-canvas)))
      (add-event-handler!
       diagram-canvas
       "<Double-ButtonPress-1>"
       (lambda (x y)
	 (set! time-to-update-plot? #F)
	 (if (< (abs (- y half-window-size)) shape-size)
	     (single-maker pz x)
	     (pair-maker pz x y)))
       "%x" "%y")

      (let ((me (make-vbox diagram-canvas
			   (make-hbox maker-button clear-button coords-button
				      coords-display plot-button))))
	(swat-open me '-title "Pole-Zero Diagram")
	(let ((x-axis (make-line-on-canvas
		       diagram-canvas
		       trim half-window-size
		       (- (* 2 half-window-size) trim) half-window-size))
	      (y-axis (make-line-on-canvas
		       diagram-canvas
		       half-window-size trim
		       half-window-size (- (* 2 half-window-size) trim)))
	      (unit-circle (make-oval-on-canvas
			    diagram-canvas
			    (* 2 trim) (* 2 trim)
			    (* 2 (- half-window-size trim))
			    (* 2 (- half-window-size trim)))))
	  (ask-widget x-axis '(configure -arrow last))
	  (ask-widget y-axis '(configure -arrow first))
	  (ask-widget unit-circle '(configure -outline "gray")))

	(set! pz
	      (lambda (message)
		      (case message
			((graph-canvas) graph-canvas)
			((diagram-canvas) diagram-canvas)
			((add-zero)
			 (lambda (z)
			   (let ((xy (z->canvas-coords z)))
			     (if (= (imag-part z) 0)
				 (make-single-zero pz (car xy))
				 (make-zero-pair pz (car xy) (cadr xy))))))
			((add-pole)
			 (lambda (p)
			   (let ((xy (z->canvas-coords p)))
			     (if (= (imag-part p) 0)
				 (make-single-pole pz (car xy))
				 (make-pole-pair pz (car xy) (cadr xy))))))
			(else "Unknown message -- MAKE-POLE-ZERO" message))))
	pz))))
	

(define (add-butterworth-poles pole-zero-diagram n)
  (define pi (* (atan 1 1) 4))
  (define (make-index-list n start)
    (if (> start n)
	'()
	(cons start (make-index-list n (+ start 1)))))
  (let ((index-list (make-index-list n (+ (ceiling->exact (/ n 2)) 1)))
	(w (exp (/ (* 2 +i pi) (* 2 n)))))
    (for-each (lambda (pole)
		((pole-zero-diagram 'add-pole) pole))
	      (map (lambda (s)
		     (let ((t 1))
		       (/ (+ 1 (* (/ t 2) s))
			  (- 1 (* (/ t 2) s)))))
		   (map (lambda (k) (expt w (- k .5)))
			index-list)))
    (let loop ((z 1))
      (if (> z n)
	  'done
	  (begin ((pole-zero-diagram 'add-zero) -1)
		 (loop (1+ z)))))
    (plot-pole-zero (pole-zero-diagram 'graph-canvas))))
	  

(define (plot-pole-zero graph-canvas)
  (cond (graph-canvas
	 (ask-widget graph-canvas '(delete all))
	 (draw-axes graph-canvas)
	 (plot-magnitude graph-canvas))))
      
(define (plot-magnitude graph-canvas)
  (let ((zero-locations (map cdr all-zeros))
	(pole-locations (map cdr all-poles)))
    (let ((fcn
	   (lambda (x)
	     (let ((jw (exp (* x +i))))
	       (let ((numer
		      (apply * (map (lambda (z) (magnitude (- jw z)))
				    zero-locations)))
		     (denom
		      (apply * (map (lambda (z) (magnitude (- jw z)))
				    pole-locations))))
		 (if (< denom 1.e-10)
		     1.e5
		     (/ numer denom)))))))
      (plot-graph-on-canvas
       graph-canvas
       (let loop ((index 0) (points '()))
	 (if (> index number-of-points)
	     points
	     (let ((w (* index (/ max-w number-of-points))))
	       (loop (+ index 1)
		     (cons (cons w (fcn w)) points)))))))))

(define (plot-graph-on-canvas canvas graph)
  (let* ((maxval (apply max (map cdr graph)))
	 (canvas-points
	  (map (lambda (graph-point)
		 (magnitude-coords->canvas-coords graph-point maxval))
	       graph)))
    (let loop ((rest-points (cdr canvas-points))
	       (this-point (car canvas-points)))
      (if (null? rest-points)
	  'done
	  (let ((next-point (car rest-points)))
	    (make-line-on-canvas canvas
				 (car this-point)
				 (cdr this-point)
				 (car next-point)
				 (cdr next-point))
	    (loop (cdr rest-points)
		  (car rest-points)))))
    (let ((maxval-display
	   (make-text-on-canvas
	    canvas (* 3 trim) (* 2 trim) `(-text ,(our-real->string maxval)))))
      (ask-widget maxval-display
		  `(configure -anchor sw -font ,symbol-font)))))


(define (magnitude-coords->canvas-coords xy max-mag)
  (let ((x (car xy))
	(y (cdr xy)))
    (cons (round->exact (+ (* x (/ (- (* 2 half-window-size) (* 4 trim)) max-w))
			   (* 2 trim)))
	  (round->exact (+ (* y (/ (- (* 4 trim) (* 2 half-window-size)) max-mag))
			   (* 2 (- half-window-size trim)))))))

	  
(define (draw-axes graph-canvas)
  (let ((x-axis (make-line-on-canvas
		 graph-canvas
		 trim (* 2 (- half-window-size trim))
		 (- (* 2 half-window-size) trim)
		 (* 2 (- half-window-size trim))))
	(y-axis (make-line-on-canvas
		 graph-canvas
		 (* 2 trim) trim
		 (* 2 trim) (- (* 2 half-window-size) trim)))
	(pi (make-text-on-canvas
	     graph-canvas
	     (* 2 (- half-window-size trim)) (- (* 2 half-window-size) trim)
	     '(-text "p"))))
    (ask-widget x-axis '(configure -arrow last))
    (ask-widget y-axis '(configure -arrow first))
    (ask-widget pi `(configure -anchor e -font ,symbol-font))))

