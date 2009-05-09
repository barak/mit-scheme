;;; -*- Scheme -*-

;; Test direct to TCL

(define (do* args)
  (tcl-global-eval *the-default-application* (car args) (cdr args)))

(define (many arg-list) (for-each do* arg-list))

;; Test the Button widget

(define but1)
(define (btest1)
  (set! but1 (make-button '(-text "Hello Jim"
			    -foreground blue -activeforeground black
			    -background yellow -activebackground orange)))
  (set-callback! but1
		 (lambda ()
		   (display "Ouch!")
		   (newline)))
  (swat-open but1 '-title "Test Scheme Application"))

(define v1)
(define v2)
(define v3)
(define h1)
(define h2)
(define h3)
(define leftframe)
(define rightframe)
(define frame)
(define (btest2)
  (set! v1 (make-button '(-text "Hello Jim"
			  -foreground blue
			  -activeforeground "hot pink")))
  (set! v2 (make-button '(-text "Goodbye and Good Luck")))
  (set! v3 (make-button '(-text "Jane")))
  
  (set! leftframe (make-vbox v1 v2 v3))
  
  (set! h1 (make-button '(-text "Button 1")))
  (set! h2 (make-button '(-text "Button 2")))
  (set! h3 (make-button '(-text "Button 3")))
  
  (set! rightframe (make-hbox h1 h2 h3))
  
  (set! frame (make-hbox leftframe rightframe))
  
  (set-callback! v1 (lambda ()
		      (display "First button in Vbox")
		      (newline)))
  (set-callback! h1
		 (lambda ()
		   (display "First button in Hbox")
		   (newline)))
  (swat-open frame '-title "Test Scheme Application"))


(define but3)
(define (btest3)
  (set! but3 (make-button '(-text "hello there" -background yellow)))
  (set-callback!
   but3
   (lambda ()
     (after-delay 2
		  (lambda ()
		    (ask-widget but3 '(configure -background red))))
     (after-delay 4
		  (lambda ()
		    (ask-widget but3 '(configure -background yellow))))))
  (swat-open but3 '-title "btest3"))

(define (show-window-attributes uitkwindow)
  (let ((attributes (list-ref (XGetWindowAttributes
			       (uitkwindow.xdisplay uitkwindow)
			       (uitkwindow.xwindow uitkwindow)) 1)))
    (Decode-Window-Attributes attributes
      (lambda (x y width height border_width depth visual root class
	       bit_gravity win_gravity backing_store backing_planes
	       backing_pixel save_under colormap map_installed
	       map_state all_event_masks your_event_mask
	       do_not_propogate_mask override_redirect screen)
	depth visual root class bit_gravity win_gravity backing_store
	backing_planes backing_pixel save_under colormap map_installed
	map_state override_redirect screen
	(write-line (list 'x x 'y y 'width width
			  'height height 'b-width border_width))
	(write-line (list 'all (number->string all_event_masks 16)
			  'event (number->string your_event_mask 16)
			  'do-not-prop do_not_propogate_mask))))))

(define scale1)
(define (stest1)
  (set! scale1 (make-scale '(-from 0 -to 99
			     -foreground blue -activeforeground red
			     -background yellow -length 500 -orient horiz)))
#|  (set-callback! scale1
		 (lambda (value)
		   (display (list "Ouch!" value))
		   (newline)))
|#
  (swat-open scale1 '-title "Scale Test Scheme Application"))

(define scale2)
(define (stest2)
  (set! scale2 (make-scale '(-from 0 -to 99 -length 500 -orient horiz
			     -foreground blue -activeforeground red
			     -background yellow)))
  (set-callback! scale2 (lambda (value) 'ignore))
  (swat-open scale2 '-title "Scale Test Scheme Application"))

(define e)
(define (etest1)
  (set! e (make-entry '(-width 30 -relief sunken
			-foreground blue -background yellow)))
  (swat-open e '-title "Entry Test"))

(define sb)
(define (sbtest1)
  (set! sb (make-scrollbar '(-width 20 -orient vertical -relief sunken
			     -foreground blue -background yellow)))
  (swat-open sb '-title "Scrollbar Test"))

(define (sbtest2)
  (let* ((scroll (make-scrollbar '(-width 30 -orient horizontal -relief sunken
				   -foreground blue -background yellow)))
	 (entry (make-entry '(-width 45)))
	 (me (make-vbox entry scroll)))
    (swat-open me '-title "Scrollbar Test")
    me))

(define (suicide-button)
  (let ((sbut (make-button '(-text "Kill Me"))))
    (set-callback! sbut
		   (lambda ()
		     (swat-close sbut)))
  (swat-open sbut)))


;;;scheduling the update for when-idle makes things a littel better, but not much.
;;;GC still interferes

(define (sbtest3)
  ;; A "better" version is in sbtest4
  (let ((app (make-application "Play")))
    (let ((hscroll (make-scrollbar '(-width 20 -orient horizontal)))
	  (vscroll (make-scrollbar '(-width 20 -orient vertical)))
	  (big (make-rect 500 600 "blue"))
	  (small (make-oval 30 40 "yellow"))
	  (scrolly 0)
	  (scrollx 0)
	  (x 0)
	  (y 0))
      (define (update-vert)
	(if (not (= y scrolly))
	    (begin (set! y scrolly)
		   (shape-draw big)
		   (assign-location! small (make-point x y))
		   (shape-draw small)
		   (ask-widget vscroll `(set 600 40 ,y ,(+ y 40))))))
      (define (update-horiz)
	(if (not (= x scrollx))
	    (begin (set! x scrollx)
		   (shape-draw big)
		   (assign-location! small (make-point x y))
		   (shape-draw small)
		   (ask-widget hscroll `(set 500 30 ,x ,(+ x 30))))))

      (ask-widget vscroll '(set 600 40 0 40))
      (ask-widget hscroll '(set 500 30 0 30))
      (let ((hb (make-hbox big vscroll)))
	(let ((vb (make-vbox hb hscroll)))
	  (add-child! app vb)
	  ))

      (set-callback! vscroll
		     (lambda (value)
		       (let* ((n (string->number value))
			      ;;keep small bar totally on screen
			      (n (max 0 n))
			      (n (min 560 n)))
			 (set! scrolly n)
			 (when-idle! update-vert))))
      (set-callback! hscroll
		     (lambda (value)
		       (let* ((n (string->number value))
			      ;;keep small bar totally on screen
			      (n (max 0 n))
			      (n (min 470 n)))
			 (set! scrollx n)
			 (when-idle! update-horiz))))
      (on-geometry-change!
       big
       'ignore
       (lambda (old-screen-area new-screen-area)
	 (assign-geometry! small
			   (drawing-surface big)
			   (if new-screen-area
			       (copy-rectangle new-screen-area)
			       new-screen-area))
	 (shape-draw big)
	 (shape-draw small)))
      (handle-exposure big (lambda (rect)
			     (shape-draw big (rectangle->xregion rect))
			     (shape-draw small)))
      (when-idle! (lambda () (shape-draw small)))
      `((app ,app)
	(hscroll ,hscroll)
	(vscroll ,vscroll)
	(big ,big)
	(small ,small)))))

(define (compress-deferred-processing on-callback at-idle)
  (let ((scheduled? #F))
    (lambda args
      (if (not scheduled?)
	  (begin
	    (when-idle!
	     (lambda ()
	       (set! scheduled? #F)
	       (at-idle)))
	    (set! scheduled? #T)))
      (apply on-callback args))))

(define (sbtest4)
  (let ((app (make-application "Play")))
    (let ((hscroll (make-scrollbar '(-width 20 -orient horizontal)))
	  (vscroll (make-scrollbar '(-width 20 -orient vertical)))
	  (big (make-rect 500 600 "blue"))
	  (small (make-oval 30 40 "yellow"))
	  (scrolly 0)
	  (scrollx 0)
	  (x 0)
	  (y 0))

      (define (update-vert)
	(if (not (= y scrolly))
	    (begin (set! y scrolly)
		   (shape-draw big)
		   (assign-location! small (make-point x y))
		   (shape-draw small)
		   (ask-widget vscroll `(set 600 40 ,y ,(+ y 40))))))

      (define (update-horiz)
	(if (not (= x scrollx))
	    (begin (set! x scrollx)
		   (shape-draw big)
		   (assign-location! small (make-point x y))
		   (shape-draw small)
		   (ask-widget hscroll `(set 500 30 ,x ,(+ x 30))))))

      (ask-widget vscroll '(set 600 40 0 40))
      (ask-widget hscroll '(set 500 30 0 30))
      (add-child! app (make-vbox (make-hbox big vscroll)
				 hscroll))
      (set-callback! vscroll
	(compress-deferred-processing
	 (lambda (value)
	   (let* ((n (string->number value))
		  ;;keep small bar totally on screen
		  (n (max 0 n))
		  (n (min 560 n)))
	     (set! scrolly n)))
	 update-vert))

      (set-callback! hscroll
	(compress-deferred-processing
	 (lambda (value)
	   (let* ((n (string->number value))
		  ;;keep small bar totally on screen
		  (n (max 0 n))
		  (n (min 470 n)))
	     (set! scrollx n)))
	 update-horiz))

      (on-geometry-change!
       big
       'ignore
       (lambda (old-screen-area new-screen-area)
	 (assign-geometry! small
			   (drawing-surface big)
			   (if new-screen-area
			       (copy-rectangle new-screen-area)
			       new-screen-area))
	 (shape-draw big)
	 (shape-draw small)))

      (handle-exposure big (lambda (rect)
			     (shape-draw big (rectangle->xregion rect))
			     (shape-draw small)))

      (when-idle! (lambda () (shape-draw small)))
      `((app ,app)
	(hscroll ,hscroll)
	(vscroll ,vscroll)
	(big ,big)
	(small ,small)))))

;;; canvas tests
(define (canvas-test)
  (let ((c (make-canvas '(-width 400 -height 300 -background "light gray"))))
    (swat-open c)
    (let ((george
           (make-arc-on-canvas c 200 200 250 250
                               `(-fill pink -outline black
                                 -width 2 -start 0 -extent 300)))
          (message (make-text-on-canvas c 100 100 '(-text "Hello there")))
          (last-x #F)
          (last-y #F)
          (start 0))
      (add-event-handler! george "<Enter>"
       (lambda () (ask-widget george '(configure -fill yellow))))
      (add-event-handler! george "<Leave>"
       (lambda () (ask-widget george '(configure -fill pink))))
      (add-event-handler! george "<ButtonPress-1>"
       (lambda (x y)
         (ask-widget message
          `(configure -text "OUCH!"
                      -font "-Adobe-Helvetica-Bold-R-Normal--*-240-*"))
         (set! start (modulo (+ start 30) 360))
         (ask-widget george `(configure -start ,start))
         (set! last-x x)
         (set! last-y y))
       "%x" "%y")
      (add-event-handler! george "<ButtonRelease-1>"
       (lambda ()
         (ask-widget message
          '(configure -text "Hello there"
                      -font "-Adobe-Helvetica-Bold-R-Normal--*-120-*"))))
      (add-event-handler! george "<Button1-Motion>"
       (lambda (x y)
         (ask-widget george `(move ,(- x last-x) ,(- y last-y)))
         (set! last-x x)
         (set! last-y y))
       "%x" "%y")
      )))


(define (doodle-test)
  (let ((c (make-canvas '(-width 400 -height 300
                          -background "light gray")))				 
	(last-x 0)
	(last-y 0))
    (add-event-handler! c
			"<ButtonPress-1>"
			(lambda (x y)
			  (ask-widget c `(delete all))
			  (set! last-x x)
			  (set! last-y y))
			"%x" "%y")
    (add-event-handler! c
			"<B1-Motion>"
			(lambda (x y)
			  (let ((line (make-line-on-canvas c last-x last-y x y)))
			    (ask-widget line '(configure -width 1))
			    (set! last-x x)
			    (set! last-y y)))
			"%x" "%y")
    (swat-open c '-title "Canvas Drawing")))


(define (ctest)
  (let ((a (make-application "Canvas Items: IQ Test")))
    (let ((c (make-canvas '(-width 400 -height 300))))
      (ask-widget c '(configure -background "light gray"))
      (add-child! a c)
      
      (let* ((r1 (make-rectangle-on-canvas c  20  20  60  60))
	     (r2 (make-rectangle-on-canvas c  40  40  80  80))
	     (r3 (make-rectangle-on-canvas c  60  60 100 100))

 	     (m1 (make-text-on-canvas      c 300  20 '(-text "CANVAS EVENTS:")))
	     (m2 (make-text-on-canvas      c 300  40 '(-text "B1: east")))
	     (m3 (make-text-on-canvas      c 300  60 '(-text "B3: west")))
	     (m4 (make-text-on-canvas      c 300  80 '(-text "B2,B1: south")))
	     (m5 (make-text-on-canvas      c 300 100 '(-text "B2,B3: north")))
	     (m6 (make-text-on-canvas      c 300 140 '(-text "TAG EVENTS:")))
	     (m7 (make-text-on-canvas      c 300 160 '(-text "B1: red")))
	     (m8 (make-text-on-canvas      c 300 180 '(-text "B2: green")))
	     (m9 (make-text-on-canvas      c 300 200 '(-text "B3: blue")))

	     (t1 (make-canvas-item-group          c (list r1 r2 r3)))
	     (t2 (make-canvas-item-group          c (list m2 m3 m4 m5 m7 m8 m9)))
	     )
	(ask-widget r1 '(configure -fill red))
	(ask-widget r2 '(configure -fill green))
	(ask-widget r3 '(configure -fill blue))
	(ask-widget t2 '(configure -anchor n -fill maroon))


	(add-event-handler! c
			    "<ButtonPress-1>"
			    (lambda ()
			      (ask-widget t1 `(move 10 0))))
	(add-event-handler! c
			    "<ButtonPress-3>"
			    (lambda ()
			      (ask-widget t1 `(move -10 0))))
	(add-event-handler! c
			    "<ButtonPress-2><ButtonPress-1>"
			    (lambda ()
			      (ask-widget t1 `(move 0 10))))
	(add-event-handler! c
			    "<ButtonPress-2><ButtonPress-3>"
			    (lambda ()
			      (ask-widget t1 `(move 0 -10))))

	(add-event-handler! t1
			    "<ButtonPress-1>"
			    (lambda ()
			      (ask-widget r1 `(move 10 10))))
	(add-event-handler! t1
			    "<ButtonPress-2>"
			    (lambda ()
			      (ask-widget r2 `(move 10 10))))
	(add-event-handler! t1
			    "<ButtonPress-3>"
			    (lambda ()
			      (ask-widget r3 `(move 10 10))))

	(add-event-handler! r1
			    "<Enter>"
			    (lambda ()
			      (ask-widget r1 `(raise))))
	(add-event-handler! r2
			    "<Enter>"
			    (lambda ()
			      (ask-widget r2 `(raise))))
	(add-event-handler! r3
			    "<Enter>"
			    (lambda ()
			      (ask-widget r3 `(raise))))
	
	(add-event-handler! t2
			    "<Enter>"
			    (lambda ()
			      (ask-widget t2 `(configure -fill violetred))))
	(add-event-handler! t2
			    "<Leave>"
			    (lambda ()
			      (ask-widget t2 `(configure -fill maroon))))

	c))))

(define (ctest1)
  (let* ((app (make-application "Canvas Widget"))
	 (canvas (make-canvas '(-width 300 -height 300)))
	 (button1 (make-button '(-text "Toggle background color")))
	 (vbox (make-vbox canvas button1)))
    (ask-widget canvas '(configure -background yellow))
    (ask-widget button1 '(configure -background cyan
				    -activebackground "light blue"))
    (add-child! app vbox)
    (set-callback!
     button1
     (lambda ()
       (let* ((button2 (make-button '(-text "Yellow")))
	      (button3 (make-button '(-text "Red"))))
	 (ask-widget button2 '(configure -background "light blue"
					 -activebackground cyan))
	 (ask-widget button3 '(configure -background "light blue"
					 -activebackground cyan))
	 (let* ((cb2 (make-widget-on-canvas canvas button2 200 250))
		(cb3 (make-widget-on-canvas canvas button3 250 250))
		(both (make-canvas-item-group canvas (list cb2 cb3))))
	   (set-callback! button2
			  (lambda ()
			    (ask-widget canvas '(configure -background yellow))
			    (ask-widget both '(delete))))
	   (set-callback! button3
			  (lambda ()
			    (ask-widget canvas '(configure -background red))
			    (ask-widget both '(delete))))))))
    (list app vbox)))


(define (menu-test)
  ;; No menu button
  (let ((application (make-application "Menu Test")))
    (define start-button (make-button '(-text "Go!")))
    (define main (make-menu))
    (define sub (make-menu))
    (define button-1 (add-to-menu main 'command '-background "blue" '-label "Close"))
    (define button-2 (add-to-menu main 'command '-background "yellow" '-label "Two"))
    (define button-3 (add-sub-menu main sub '-background "blue" '-label "More..."))
    (define sub-1 (add-to-menu sub 'command '-background "blue" '-label "A"))
    (define sub-2 (add-to-menu sub 'command '-background "blue" '-label "B"))
    (define sub-3 (add-to-menu sub 'command '-background "blue" '-label "C"))
    (define sub-4 (add-to-menu sub 'command '-background "blue" '-label "D"))
    (ask-widget start-button
		'(configure -background yellow
			    -font
			    "-adobe-helvetica-bold-r-normal--34-240-100-100-p-182-iso8859-1"))
    (add-child! application start-button)
    (add-child! start-button main)
    (set-callback! button-1 (lambda () (ask-widget main '(unpost))))
    (set-callback! button-2 (lambda () (write-line 'Two)))
    (set-callback! button-3 (lambda () (write-line 'Three)))
    (set-callback! sub-1 (lambda () (write-line 'A)))
    (set-callback! sub-2 (lambda () (write-line 'B)))
    (set-callback! sub-3 (lambda () (write-line 'C)))
    (set-callback! sub-4 (lambda () (write-line 'D)))
    (set-callback! start-button (lambda () (ask-widget main '(post 10 20))))
    (lambda ()
      (list application start-button main sub
	    button-1 button-2 button-3
	    sub-1 sub-2 sub-3 sub-4))))

(define (menu-test2)
  (let ((application (make-application "Menu Test")))
    (define main (make-menu))
    (define start-button (make-menubutton main '(-text "Go!")))
    (define sub (make-menu))
    (define button-1 (add-to-menu main 'command '-background "blue" '-label "Close"))
    (define button-2 (add-to-menu main 'command '-background "yellow" '-label "Two"))
    (define button-3 (add-sub-menu main sub '-background "blue" '-label "More..."))
    (define sub-1 (add-to-menu sub 'command '-background "blue" '-label "A"))
    (define sub-2 (add-to-menu sub 'command '-background "blue" '-label "B"))
    (define sub-3 (add-to-menu sub 'command '-background "blue" '-label "C"))
    (define sub-4 (add-to-menu sub 'command '-background "blue" '-label "D"))
    (add-child! application start-button)
    (set-callback! button-1 (lambda () (ask-widget main '(unpost))))
    (set-callback! button-2 (lambda () (write-line 'Two)))
    (set-callback! button-3 (lambda () (write-line 'Three)))
    (set-callback! sub-1 (lambda () (write-line 'A)))
    (set-callback! sub-2 (lambda () (write-line 'B)))
    (set-callback! sub-3 (lambda () (write-line 'C)))
    (set-callback! sub-4 (lambda () (write-line 'D)))
    (lambda ()
      (list application start-button main sub
	    button-1 button-2 button-3
	    sub-1 sub-2 sub-3 sub-4))))

(define (menu-test3)
  (let ((application (make-application "Menu Test")))
    (define main (make-menu))
    (define start-button (make-menubutton main '(-text "Go!")))
    (define button-1 (add-to-menu main 'command '-background "blue" '-label "Close"))
    (define button-2 (add-to-menu main 'command '-background "yellow" '-label "Two"))
    (add-child! application start-button)
    (set-callback! button-1 (lambda () (ask-widget main '(unpost))))
    (set-callback! button-2 (lambda () (write-line 'Two)))
    (lambda ()
      (list application start-button main 
	    button-1 button-2))))


(define (mac)
  (let* ((a (make-application "Microslop Word"))
	 (t (make-text))
	 (sb (make-scrollbar '(-width 20 -orient vertical)))
	 (file-menu (make-menu))
	 (file-mb (make-menubutton file-menu '(-text "File")))
	 (save-menu (make-menu))
	 (edit-menu (make-menu))
	 (edit-mb (make-menubutton edit-menu '(-text "Edit")))
	 (tools-menu (make-menu))
	 (tools-mb (make-menubutton tools-menu '(-text "Tools")))
	 )

    (for-each (lambda (m)
		(ask-widget m '(configure -background white
					  -activebackground red)))
	      (list file-menu save-menu edit-menu tools-menu
		    file-mb edit-mb tools-mb))
    (for-each (lambda (b)
		(ask-widget b '(configure -relief raised)))
	      (list file-mb edit-mb tools-mb))

    (ask-widget t '(configure -background white -wrap word -width 40))
    (ask-widget sb '(configure -background red))
    (ask-widget sb '(set 1000 400 0 400))

    (set-callback!
     sb
     (lambda (value)
       (let* ((n (string->number value)))
	 (when-idle!
	  (lambda () (ask-widget sb `(set 1000 400 ,n ,(+ n 400))))))))

    (let ((me (make-vbox (make-hbox file-mb edit-mb tools-mb)
			 (make-hbox t sb))))
      (add-child! a me)

      (add-to-menu file-menu 'command '-label "Open")
      (add-to-menu file-menu 'command '-label "Close")
      (add-sub-menu file-menu save-menu '-label "Save...")
      (add-to-menu save-menu 'command '-label "There is no salvation")
      (add-to-menu save-menu 'command '-label "Jesus saves")
      (add-to-menu file-menu 'command '-label "Exit")

      (let ((paste-button (add-to-menu edit-menu 'command '-label "Paste"))
	    (cut-button (add-to-menu edit-menu 'command '-label "Cut"))
	    (deleted-text #F))
	(set-callback! cut-button
		       (lambda ()
			 (set! deleted-text
			       (ask-widget t '(get sel.first sel.last)))
			 (ask-widget t '(delete sel.first sel.last))))
	(set-callback! paste-button
		       (lambda ()
			 (if deleted-text
			     (ask-widget t `(insert insert ,deleted-text))))))

      (add-to-menu edit-menu 'command '-label "Staple")
      (add-to-menu edit-menu 'command '-label "Mutilate")

      (add-to-menu tools-menu 'command '-label "Hammer")
      (add-to-menu tools-menu 'command '-label "Saw")

      me)))

(define all-tags '())
(define all-demos '())
(define (browser)
  (let* ((a (make-application "Demo Browser"))
	 (t (make-text))
	 (sb (make-scrollbar '(-width 20 -orient vertical)))
	 (m (make-menu))
	 (mb (make-menubutton m '(-text "Widgets"))))

    (define (switch-to-widget-demos list-of-demos)
      (for-each (lambda (tag) (ask-widget tag '(delete))) all-tags)
      (set! all-tags '())
      (ask-widget t '(delete "1.0" end))
      (for-each
       (lambda (demo-thunk-name)
	 (ask-widget t `(insert insert ,demo-thunk-name))
	 (ask-widget t '(insert insert " "))
	 (let ((tag (make-text-tag t "insert linestart" "insert-1c")))
	   ;; gc protect
	   (set! all-tags (cons tag all-tags))
	   (add-event-handler!
	    tag "<Enter>"
	    (lambda () (ask-widget tag '(configure -foreground violetred))))
	   (add-event-handler!
	    tag "<Leave>"
	    (lambda () (ask-widget tag '(configure -foreground blue))))
	   (add-event-handler!
	    tag
	    "<Button-1>"
	    (lambda ()
	      (ask-widget tag '(configure -underline 1))
	      (let ((demo-thunk
		     (ask-widget
		      t
		      `(get ,(string-append (TextTag.name tag) ".first")
			    ,(string-append (TextTag.name tag) ".last")))))
		;; gc protect
		(set! all-demos (cons ((eval (string->symbol demo-thunk) 
					     user-initial-environment))
				      all-demos))
		))))
	 (ask-widget t '(insert insert "\n")))
       list-of-demos))

    (ask-widget t `(configure -width 20 -height 10
			      -background "white" -foreground blue))
    (for-each (lambda (x)
		(ask-widget x '(configure -background blue -foreground white
					  -activebackground white
					  -activeforeground blue)))
	      (list m mb))
    #|
    (set-callback! sb
		   (lambda (n)
		     (ask-widget t `(yview -pickplace ,n))))

    |#

    (let ((me (make-vbox mb (make-hbox t sb))))
      (add-child! a me)

      (ask-widget t `(configure -yscrollcommand
				,(string-append (tk-widget->pathname sb) " set")))
      (ask-widget sb `(configure
		       -command
		       ,(string-append
			 (tk-widget->pathname t)
			 " yview -pickplace")))

      (let* ((picture-button   (add-to-menu m 'command '-label "Picture"))
	     (button-button    (add-to-menu m 'command '-label "Button"))
	     (scale-button     (add-to-menu m 'command '-label "Scale"))
	     (entry-button     (add-to-menu m 'command '-label "Entry"))
	     (scrollbar-button (add-to-menu m 'command '-label "Scrollbar"))
	     (canvas-button    (add-to-menu m 'command '-label "Canvas"))
	     (menu-button      (add-to-menu m 'command '-label "Menu"))
	     (text-button      (add-to-menu m 'command '-label "Text Widget"))
	     (animation-button (add-to-menu m 'command '-label "Animation")))

	(set-callback! picture-button
		       (lambda () (switch-to-widget-demos
				   (list "test1" "test1a" "test2" "test3"))))
	(set-callback! button-button
		       (lambda () (switch-to-widget-demos
				   (list "btest1" "btest2" "btest3"))))
	(set-callback! scale-button
		       (lambda () (switch-to-widget-demos (list "stest1" "stest2"))))
	(set-callback! entry-button
		       (lambda () (switch-to-widget-demos (list "etest1"))))
	(set-callback! scrollbar-button
		       (lambda () (switch-to-widget-demos
				   (list "sbtest1" "sbtest2" "sbtest3"))))
	(set-callback! canvas-button
		       (lambda () (switch-to-widget-demos
				   (list "canvas-test" "doodle-test"
					 "ctest" "ctest1"))))
	(set-callback! menu-button
		       (lambda () (switch-to-widget-demos
				   (list "menu-test" "menu-test2"))))
	(set-callback! text-button
		       (lambda () (switch-to-widget-demos (list "mac" "browser"))))
	(set-callback! animation-button
		       (lambda () (switch-to-widget-demos
				   (list "btest3" "animation" "biff" "melt"
					 "balls"))))
	
	me))))


(define app4)
(define c4)
(define but4)
(define (animation)
  (set! app4 (make-application "Animation"))
  (set! c4 (make-canvas '(-background white -width 200 -height 200)))
  (set! but4 (make-button '(-text "START" -background maroon -foreground white
			    -activebackground "hot pink")))
  (add-child! app4 c4)
  (make-widget-on-canvas c4 but4 25 185)
  (set-callback!
   but4
   (lambda ()		   
     (let ((rect (make-rectangle-on-canvas c4 10 10 40 40)))
       (ask-widget rect '(configure -fill red))
       (let loop ((position 10))
	 (if (> position 200)
	     (ask-widget rect '(delete))
	     (begin (ask-widget rect '(move 1 1))
		    (after-delay .05 (lambda () (loop (1+ position))))))))))
  c4)


(define app5)
(define c5)
(define (biff)
  (set! app5 (make-application "biff"))
  (set! c5 (make-canvas '(-background black -width 70 -height 70)))
  (add-child! app5 c5)
  (let* ((file1 "/usr/local/lib/tk/demos/bitmaps/flagdown")
	 (file2 "/usr/local/lib/tk/demos/bitmaps/flagup")
	 (current-bitmap-filename file1)
	 (current-bknd "black")
	 (current-fgnd "cyan"))
    (define (toggle-bitmap)
      (let ((old-bknd current-bknd)
	    (old-fgnd current-fgnd))
	(set! current-bknd old-fgnd)
	(set! current-fgnd old-bknd))
      (if (equal? current-bitmap-filename file1)
	  (set! current-bitmap-filename file2)
	  (set! current-bitmap-filename file1)))
    (define (make-flag)
      (let ((flag (make-bitmap-on-canvas c5 current-bitmap-filename 35 35)))
	(ask-widget flag `(configure -background ,current-bknd
				     -foreground ,current-fgnd))
	flag))

    (let ((flag (make-flag)))
      (define (toggle)
	(ask-widget flag '(delete))
	(toggle-bitmap)
	(ask-widget c5 `(configure -background ,current-bknd))
	(set! flag (make-flag)))
      (after-delay
       1
       (lambda ()
	 (let loop ((count 0))
	   (if (> count 10)
	       'done
	       (after-delay 1
			    (lambda ()
			      (toggle)
			      (loop (1+ count))))))))
      (add-event-handler! c5 "<Any-ButtonPress>" toggle))))


(define app6)
(define c6)
(define but6)
(define (melt)
  (define (generate-vertical-line x)
    (let* ((length (random 30))
	   (start (- (random 30) 30))
	   (end (+ start length))
	   (line (make-line-on-canvas c6 x start x end)))
      (ask-widget line `(configure -fill white))
      line))
  
  (set! app6 (make-application "Mind Melt"))
  (set! c6 (make-canvas '(-background black -width 200 -height 200)))
  (set! but6 (make-button '(-text "MELT"
			    -background black -foreground red
			    -activebackground black -activeforeground red)))
  (let ((me (make-vbox c6 but6)))
    (add-child! app6 me)
    (set-callback!
     but6
     (lambda ()
       (let* ((lines (let loop ((x 0) (lines '()))
		       (if (> x 200)
			   lines
			   (loop (+ x 2) (cons (generate-vertical-line x) lines)))))
	      (tag (make-canvas-item-group c6 lines)))
	 (let loop ((position 0))
	   (if (> position 230)
	       (ask-widget tag '(delete))
	       (after-delay .000005
			    (lambda ()
			      (for-each (lambda (line)
					  (ask-widget line `(move 0 ,(random 10))))
					lines)
			      (loop (1+ position)))))))))
    me))


(define (balls)
  (define canvas-width 300)
  (define canvas-height 300)
  (define min-ball-size 1)
  (define max-ball-size 30)
  (define min-delta 1)
  (define max-delta 8)
  (define a (/ (- min-delta max-delta) (- max-ball-size min-ball-size)))
  (define b (- min-delta (* a max-ball-size)))
  (define (pick-random-color)
    (define list-of-colors
      '("green" "yellow" "red" "blue" "hot pink" "orange" "cyan"
	"maroon" "skyblue" "firebrick" "aquamarine" "violet"
	"violetred" "navyblue" "darkslateblue" "pink"))
    (list-ref list-of-colors (random (length list-of-colors))))

  (define (pick-true-or-false)
    (list-ref '(#T #F) (random 2)))

  (define (make-ball canvas diameter startx starty)
    (let* ((radius (round->exact (/ diameter 2)))
	   (centerx (+ startx radius))
	   (centery (+ starty radius))
	   (the-ball
	    (make-oval-on-canvas canvas startx starty
				 (+ startx diameter)
				 (+ starty diameter)))
	   (increasing-x? (pick-true-or-false))
	   (increasing-y? (pick-true-or-false))
	   (speed-factor (round->exact (+ (* a diameter) b))))
      (lambda (m)
	(case m
	  ((the-ball) the-ball)
	  ((centerx) centerx)
	  ((centery) centery)
	  ((set-centerx!) (lambda (x) (set! centerx x)))
	  ((set-centery!) (lambda (y) (set! centery y)))
	  ((+x?) increasing-x?)
	  ((+y?) increasing-y?)
	  ((set+x?!) (lambda (boolean) (set! increasing-x? boolean)))
	  ((set+y?!) (lambda (boolean) (set! increasing-y? boolean)))
	  ((diameter) diameter)
	  ((speed-factor) speed-factor)))))

  (define (generate-ball canvas)
    (let* ((diameter (+ min-ball-size (random (- max-ball-size min-ball-size))))
	   (startx (min (random canvas-width) (- canvas-width diameter)))
	   (starty (min (random canvas-height) (- canvas-height diameter)))
	   (ball (make-ball canvas diameter startx starty)))
      (ask-widget (ball 'the-ball) `(configure -fill ,(pick-random-color)))
      ball))

  (define (generate-move ball)
    (let* ((centerx (ball 'centerx))
	   (centery (ball 'centery))
	   (diameter (ball 'diameter))
	   (radius (round->exact (/ diameter 2)))
	   (startx (- centerx radius))
	   (endx (+ centerx radius))
	   (starty (- centery radius))
	   (endy (+ centery radius))
	   (dx-sign (cond ((<= startx 0)
			   ((ball 'set+x?!) #T)
			   1)
			  ((>= endx canvas-width)
			   ((ball 'set+x?!) #F)
			   -1)
			  ((ball '+x?) 1)
			  (else -1)))
	   (dy-sign (cond ((<= starty 0)
			   ((ball 'set+y?!) #T)
			   1)
			  ((>= endy canvas-height)
			   ((ball 'set+y?!) #F)
			   -1)
			  ((ball '+y?) 1)
			  (else -1)))
	   (dx (* dx-sign (max 1 (random (ball 'speed-factor)))))
	   (dy (* dy-sign (max 1 (random (ball 'speed-factor))))))
      ((ball 'set-centerx!) (+ centerx dx))
      ((ball 'set-centery!) (+ centery dy))
      `(move ,dx ,dy)))
  
  (let ((app7 (make-application "Bouncing Balls"))
	(c7 (make-canvas `(-background black -width ,canvas-width
				       -height ,canvas-height))))
    (add-child! app7 c7)
  
    (let ((balls
	   (let loop ((num 0) (balls '()))
	     (if (> num (+ 10 (random 10)))
		 balls
		 (loop (1+ num) (cons (generate-ball c7) balls))))))
      (let ((go
	     (lambda ()
	       (let loop ()
		 (for-each
		  (lambda (ball)
		    (ask-widget (ball 'the-ball) (generate-move ball)))
		  balls)
		 (after-delay 0.0005 loop)))))
	(after-delay 1 go)
	(list app7 c7 balls go)))))


(define (canvas-sb-test)
  (let ((sc (make-scrollable-canvas
	     '(-width 500 -height 500
	       -scrollregion (0 0 1000 1000)))))
    (swat-open sc)
    (ask-widget (scrollable-canvas-hscroll sc)
		`(configure -foreground "yellow"))
    (ask-widget (scrollable-canvas-vscroll sc)
		`(configure -foreground "yellow"))
    (make-rectangle-on-canvas (scrollable-canvas-canvas sc)
			      100 100 400 400
			      '(-fill red))
    sc))



(define (text-sb-test)
  (let ((st (make-scrollable-text)))
    (swat-open st)
    (ask-widget (scrollable-text-vscroll st)
		'(configure -foreground red))
    st))
    





