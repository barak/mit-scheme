;;; -*- Scheme -*-

;; to make this possible to debug

; (set! *unparser-list-breadth-limit* 10)
; (set! *unparser-list-depth-limit* 10)


;; GC stress test

(define (a)
  (gc-flip)
  (gc-flip)
  (kick-uitk-thread)
  (gc-flip)
  (gc-flip)
  (kick-uitk-thread))

(define (foo test n)
  (if (> n 0)
      (begin
	(test)
	(foo test (- n 1)))))

(define (foo2 test n)
  (if (> n 0)
      (begin
	(display n)
	(test)
	(gc-flip)
	(foo2 test (- n 1)))))


;; Support for GC debugging

(define gctr (make-primitive-procedure 'gc-trace-references))
(define refs (make-vector 40))

(define (go obj)
  (gctr obj refs)
  (gc-flip)
  (write-line (list (vector-ref refs 0) (map object-type (vector->list refs)))))

(define (get n) (vector-ref refs n))


;; Test the Rectangle widget

;; (define application (make-application "Test Scheme Application"))

(define (make-picture)
  (define v1 (make-self-painting-rectangle 50 30 "yellow"))
  (define v2 (make-self-painting-rectangle 100 10 "blue"))
  (define v3 (make-self-painting-rectangle 10 100 "orange"))
  
  (define topframe (make-vbox v1 v2 v3))
  
  (define h1 (make-self-painting-rectangle 10 10 "white"))
  (define h2 (make-self-painting-rectangle 20 20 "gold"))
  (define h3 (make-self-painting-rectangle 30 30 "green"))
  
  (define bottomframe (make-hbox h1 h2 h3))
  
  (make-hbox topframe bottomframe))

(define (make-bad-picture)
  (define v1 (make-rect 50 30 "yellow"))
  (define v2 (make-rect 100 10 "blue"))
  (define v3 (make-rect 10 100 "orange"))
  
  (define topframe (make-vbox v1 v2 v3))
  
  (define h1 (make-rect 10 10 "white"))
  (define h2 (make-rect 20 20 "gold"))
  (define h3 (make-rect 30 30 "green"))
  (set! green h3)
  
  (define bottomframe (make-hbox h1 h2 h3))
  
  (make-hbox topframe bottomframe))

(define (simple-picture)
 (swat-open (make-self-painting-rectangle 50 30 "yellow")))

(define (test0)
  (simple-picture)
  3)

(define (test1)
  (swat-open (make-picture))
  3)

(define (test1a)
  (swat-open (make-picture))
  (swat-open (make-picture))
  3)

(define app2)
(define (test2)
  (set! app2  (make-application "Test2-2 "))
  (add-child! app2 (make-picture))
  (swat-open (make-picture))
  (swat-open (make-picture))
  (swat-open (make-picture))
  (swat-open (make-picture))
  (swat-open (make-picture))
  (swat-open (make-picture))
  (add-child! app2 (make-picture))
  (add-child! app2 (make-picture))
  (add-child! app2 (make-picture))
  (add-child! app2 (make-picture))
  3)

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

(define debug-test3)
(define (test3)
  (let* ((surface (make-shape-surface 600 600 "white" "red"))
	 (button1 (make-button '(-text "PANIC!")))
	 (button2 (make-button '(-text "SET COLOR")))
	 (button3 (make-switch
		   "yellow"
		   `(("Ovals" ,(lambda () (surface 'ovals)))
		     ("Rectangles" ,(lambda () (surface 'rectangles))))))
	 (button4 (make-switch
		   "yellow"
		   `(("Outlined" ,(lambda () (surface 'outlined)))
		     ("Filled" ,(lambda () (surface 'filled))))))
	 (e (make-entry '(-width 10))))
    (define (handle-bad-color)
      (ask-widget button2 '(configure -background red))
      (ask-widget button2 '(flash))
      (ask-widget button2 '(flash))
      (ask-widget e `(delete 0 end))
      (ask-widget e `(insert 0 red))
      ((surface 'set-color!) "red"))
    (define (change-color)
      (let ((new-color (ask-widget e '(get))))
	(if (valid-color? new-color)
	    (begin ((surface 'set-color!) new-color)
		   (ask-widget button2
			       `(configure -background ,new-color)))
	    (handle-bad-color))))
    (set! debug-test3 (lambda () #f))
    (ask-widget button1 '(configure
			  -background green -activebackground red
			  -font "-adobe-helvetica-bold-r-normal--34-240-100-100-p-182-iso8859-1"))
    (ask-widget button2 '(configure -background red -activebackground white))
    (ask-widget e '(configure -background white -foreground black -relief sunken))
    (ask-widget e '(insert 0 red))
    (set-callback! button1 (lambda () (surface 'clear)))
    (set-callback! button2 change-color)
    (let ((me (make-vbox (surface 'the-surface)
			 (make-hbox button1 e
				    button2 button3 button4))))
      (swat-open me '-title "Featureless Drawing Program")
      me)))




