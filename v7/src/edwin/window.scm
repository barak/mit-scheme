;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1986 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Window System

(declare (usual-integrations)
	 )
(using-syntax class-syntax-table

;;;  Based on WINDOW-WIN, designed by RMS.
;;;  See ED:-WINOPS.TXT for more information.

;;; The convention of using method names like :FOO is somewhat
;;; arbitrary.  However, methods without the prefix ":" are intended
;;; to be internal (non-public) methods.

;;; Procedural covers are used as the ultimate outside interface to
;;; the window system, since that minimizes dependence on the
;;; syntactic details of the class/object system.

;;; It is assumed in several places that all windows keep the
;;; following instance variables updated:  SUPERIOR, X-SIZE, and
;;; Y-SIZE.  Thus these are normally accessed using procedure calls or
;;; instance variable references, rather than the more cumbersome
;;; method invocation.  However, these instance variables are always
;;; set by a method defined on the window itself.

;;;; Vanilla Window

(define-class vanilla-window ()
  (superior x-size y-size redisplay-flags inferiors))

(declare (integrate window-superior
		    window-x-size %set-window-x-size!
		    window-y-size %set-window-y-size!
		    window-redisplay-flags window-inferiors
		    window-inferior? window-inferior))

(define-procedure vanilla-window (window-initialize! window window*)
  (set! superior window*)
  (set! redisplay-flags (=> superior :inferior-redisplay-flags window))
  (set! inferiors '()))

(define-procedure vanilla-window (window-kill! window)
  (for-each (lambda (inferior) (=> (inferior-window inferior) :kill!))
	    inferiors))

(define-procedure vanilla-window (window-superior window)
  (declare (integrate window))
  superior)

(define-procedure vanilla-window (set-window-superior! window window*)
  (set! superior window*)
  (set! redisplay-flags (=> window* :inferior-redisplay-flags window))
  (setup-redisplay-flags! redisplay-flags)
  (for-each (lambda (inferior)
	      (set-inferior-redisplay-flags! inferior
					     (cons #!FALSE redisplay-flags))
	      (=> (inferior-window inferior) :set-superior! window))
	    inferiors))

(define-procedure vanilla-window (window-x-size window)
  (declare (integrate window))
  x-size)

(define-procedure vanilla-window (%set-window-x-size! window x)
  (declare (integrate window x))
  (set! x-size x))

(define-procedure vanilla-window (set-window-x-size! window x)
  (%set-window-x-size! window x)
  (setup-redisplay-flags! redisplay-flags))

(define-procedure vanilla-window (window-y-size window)
  (declare (integrate window))
  y-size)

(define-procedure vanilla-window (%set-window-y-size! window y)
  (declare (integrate window y))
  (set! y-size y))

(define-procedure vanilla-window (set-window-y-size! window y)
  (%set-window-y-size! window y)
  (setup-redisplay-flags! redisplay-flags))

(define-procedure vanilla-window (window-size window receiver)
  (receiver x-size y-size))

(define-procedure vanilla-window (set-window-size! window x y)
  (set! x-size x)
  (set! y-size y)
  (setup-redisplay-flags! redisplay-flags))

(define-procedure vanilla-window (window-absolute-position window receiver 
							   fail)
  (if (eq? window the-alpha-window)
      (receiver 0 0)
      (=> superior :inferior-absolute-position window receiver fail)))

(define-procedure vanilla-window (window-redisplay-flags window)
  (declare (integrate window))
  redisplay-flags)

(define-procedure vanilla-window (window-inferiors window)
  (declare (integrate window))
  inferiors)

(define-procedure vanilla-window (window-inferior? window window*)
  (declare (integrate window window*))
  (find-inferior? inferiors window*))

(define-procedure vanilla-window (window-inferior window window*)
  (declare (integrate window window*))
  (find-inferior inferiors window*))

(define-procedure vanilla-window (make-inferior window class)
  (let ((window* (make-object class)))
    (let ((inferior
	   (cons window*
		 (vector #!FALSE #!FALSE
			 (cons #!FALSE redisplay-flags)))))
      (set! inferiors (cons inferior inferiors))
      (=> window* :initialize! window)
      inferior)))

(define-procedure vanilla-window (add-inferior! window window*)
  (set! inferiors
	(cons (cons window*
		    (vector #!FALSE #!FALSE
			    (cons #!FALSE redisplay-flags)))
	      inferiors))
  (=> window* :set-superior! window))

(define-procedure vanilla-window (delete-inferior! window window*)
  (set! inferiors
	(delq! (find-inferior inferiors window*)
	       inferiors)))

(define-procedure vanilla-window (replace-inferior! window old new)
  (set-inferior-window! (find-inferior inferiors old) new)
  (=> new :set-superior! window))

;;; Returns #!TRUE if the redisplay finished, #!FALSE if aborted.
;;; Notice that the :update-display! operation is assumed to return
;;; the same value.  This is used to control the setting of the
;;; redisplay flags.

(define-procedure vanilla-window
		  (update-inferiors! window screen x-start y-start
				     xl xu yl yu display-style)
  (define (loop inferiors)
    (or (null? inferiors)
	(let ((window (inferior-window (car inferiors)))
	      (xi (inferior-x-start (car inferiors)))
	      (yi (inferior-y-start (car inferiors)))
	      (flags (inferior-redisplay-flags (car inferiors))))
	  (if (and (or display-style (car flags))
		   xi yi)
	      (and (or display-style (not (keyboard-active? 0)))
		   (clip-window-region xl xu yl yu
				       xi (window-x-size window)
				       yi (window-y-size window)
		     (lambda (xl xu yl yu)
		       (=> window :update-display!
			   screen (+ x-start xi) (+ y-start yi)
			   xl xu yl yu display-style)))
		   (begin (set-car! flags #!FALSE)
			  (loop (cdr inferiors))))
	      (begin (set-car! flags #!FALSE)
		     (loop (cdr inferiors)))))))
  (loop inferiors))

(define (clip-window-region xl xu yl yu xi xs yi ys receiver)
  (clip-window-region-1 (- xl xi) (- xu xi) xs
    (lambda (xl xu)
      (clip-window-region-1 (- yl yi) (- yu yi) ys
	(lambda (yl yu)
	  (receiver xl xu yl yu))))))

(define (clip-window-region-1 al au bs receiver)
  (if (positive? al)
      (if (<= al bs)
	  (receiver al (if (< bs au) bs au))
	  #!TRUE)
      (if (positive? au)
	  (receiver 0 (if (< bs au) bs au))
	  #!TRUE)))

(define-procedure vanilla-window (salvage-inferiors! window)
  (define (loop inferiors)
    (if (not (null? inferiors))
	(begin (=> (inferior-window (car inferiors)) :salvage!)
	       (loop (cdr inferiors)))))
  (loop inferiors))

;;;; Standard Methods
;;;  All windows should support these operations

(define-method vanilla-window :initialize! window-initialize!)
(define-method vanilla-window :kill! window-kill!)
(define-method vanilla-window :superior window-superior)
(define-method vanilla-window :set-superior! set-window-superior!)
(define-method vanilla-window :x-size window-x-size)
(define-method vanilla-window :set-x-size! set-window-x-size!)
(define-method vanilla-window :y-size window-y-size)
(define-method vanilla-window :set-y-size! set-window-y-size!)
(define-method vanilla-window :size window-size)
(define-method vanilla-window :set-size! set-window-size!)

(define-method vanilla-window (:make-inferior window class)
  (inferior-window (make-inferior window class)))

(define-method vanilla-window :add-inferior! add-inferior!)
(define-method vanilla-window :delete-inferior! delete-inferior!)
(define-method vanilla-window :replace-inferior! replace-inferior!)
(define-method vanilla-window :update-display! update-inferiors!)
(define-method vanilla-window :salvage! salvage-inferiors!)

;;;; Operations on Inferiors

(define-method vanilla-window (:inferior-redisplay-flags window window*)
  (inferior-redisplay-flags (find-inferior inferiors window*)))

(define-method vanilla-window (:inferior-needs-redisplay! window window*)
  (inferior-needs-redisplay! (find-inferior inferiors window*)))

(define-method vanilla-window (:inferior-position window window*)
  (inferior-position (find-inferior inferiors window*)))

(define-method vanilla-window (:set-inferior-position! window window* position)
  (set-inferior-position! (find-inferior inferiors window*) position))

(define-method vanilla-window (:inferior-x-start window window*)
  (inferior-x-start (find-inferior inferiors window*)))

(define-method vanilla-window (:set-inferior-x-start! window window* x-start)
  (set-inferior-x-start! (find-inferior inferiors window*) x-start))

(define-method vanilla-window (:inferior-x-end window window*)
  (inferior-x-end (find-inferior inferiors window*)))

(define-method vanilla-window (:set-inferior-x-end! window window* x-end)
  (set-inferior-x-end! (find-inferior inferiors window*) x-end))

(define-method vanilla-window (:inferior-y-start window window*)
  (inferior-y-start (find-inferior inferiors window*)))

(define-method vanilla-window (:set-inferior-y-start! window window* y-start)
  (set-inferior-y-start! (find-inferior inferiors window*) y-start))

(define-method vanilla-window (:inferior-y-end window window*)
  (inferior-y-end (find-inferior inferiors window*)))

(define-method vanilla-window (:set-inferior-y-end! window window* y-end)
  (set-inferior-y-end! (find-inferior inferiors window*) y-end))

(define-method vanilla-window (:inferior-start window window* receiver)
  (inferior-start (find-inferior inferiors window*) receiver))

(define-method vanilla-window (:set-inferior-start! window window* x y)
  (set-inferior-start! (find-inferior inferiors window*) x y))

(define-method vanilla-window (:inferior-absolute-position window window*
							   receiver fail)
  (inferior-absolute-position (find-inferior inferiors window*) receiver fail))


;;;; Inferiors

(define (inferior-position inferior)
  (and (inferior-x-start inferior)
       (inferior-y-start inferior)
       (cons (inferior-x-start inferior)
	     (inferior-y-start inferior))))

(define (set-inferior-position! inferior position)
  (if (not position)
      (set-inferior-start! inferior #!FALSE #!FALSE)
      (set-inferior-start! inferior (car position) (cdr position))))

(define (inferior-absolute-position inferior receiver fail)
  (if (and (inferior-x-start inferior)
	   (inferior-y-start inferior))
      (window-absolute-position (window-superior (inferior-window inferior))
	(lambda (x y)
	  (receiver
	   (+ x (inferior-x-start inferior))
	   (+ y (inferior-y-start inferior))))
	fail)
      (fail)))

(define (inferior-needs-redisplay! inferior)
  (if (and (inferior-x-start inferior)
	   (inferior-y-start inferior))
      (setup-redisplay-flags! (inferior-redisplay-flags inferior))
      (set-car! (inferior-redisplay-flags inferior) #!FALSE)))

(define (setup-redisplay-flags! flags)
  (if (not (or (null? flags) (car flags)))
      (begin (set-car! flags #!TRUE)
	     (setup-redisplay-flags! (cdr flags)))))

(declare (integrate inferior-x-size %set-inferior-x-size! set-inferior-x-size!
		    inferior-y-size %set-inferior-y-size! set-inferior-y-size!
		    inferior-size set-inferior-size!))

(define (inferior-x-size inferior)
  (declare (integrate inferior))
  (window-x-size (inferior-window inferior)))

(define (%set-inferior-x-size! inferior x)
  (declare (integrate inferior x))
  (%set-window-x-size! (inferior-window inferior) x))

(define (set-inferior-x-size! inferior x)
  (declare (integrate inferior x))
  (=> (inferior-window inferior) :set-x-size! x))

(define (inferior-y-size inferior)
  (declare (integrate inferior))
  (window-y-size (inferior-window inferior)))

(define (%set-inferior-y-size! inferior y)
  (declare (integrate inferior y))
  (%set-window-y-size! (inferior-window inferior) y))

(define (set-inferior-y-size! inferior y)
  (declare (integrate inferior y))
  (=> (inferior-window inferior) :set-y-size! y))

(define (inferior-size inferior receiver)
  (declare (integrate inferior receiver))
  (window-size (inferior-window inferior) receiver))

(define (set-inferior-size! inferior x y)
  (declare (integrate inferior x y))
  (=> (inferior-window inferior) :set-size! x y))

(declare (integrate find-inferior find-inferior?
		    inferior-window set-inferior-window!
		    inferior-x-start %set-inferior-x-start!
		    inferior-y-start %set-inferior-y-start!
		    inferior-redisplay-flags set-inferior-redisplay-flags!))

(define (find-inferior? inferiors window)
  (declare (integrate inferiors window))
  (assq window inferiors))

(define (find-inferior inferiors window)
  (declare (integrate inferiors window))
  (or (find-inferior? inferiors window)
      (error "Window is not an inferior" window)))

(define inferior-window
  car)

(define set-inferior-window!
  set-car!)

(define (inferior-x-start inferior)
  (declare (integrate inferior))
  (vector-ref (cdr inferior) 0))

(define (%set-inferior-x-start! inferior x-start)
  (vector-set! (cdr inferior) 0 x-start))

(define (set-inferior-x-start! inferior x-start)
  (%set-inferior-x-start! inferior x-start)
  (inferior-needs-redisplay! inferior))

(define (inferior-x-end inferior)
  (let ((x-start (inferior-x-start inferior)))
    (and x-start (+ x-start (inferior-x-size inferior)))))

(define (set-inferior-x-end! inferior x-end)
  (set-inferior-x-start! inferior (- x-end (inferior-x-size inferior))))

(define (inferior-y-start inferior)
  (declare (integrate inferior))
  (vector-ref (cdr inferior) 1))

(define (%set-inferior-y-start! inferior y-start)
  (vector-set! (cdr inferior) 1 y-start))

(define (set-inferior-y-start! inferior y-start)
  (%set-inferior-y-start! inferior y-start)
  (inferior-needs-redisplay! inferior))

(define (inferior-y-end inferior)
  (let ((y-start (inferior-y-start inferior)))
    (and y-start (+ y-start (inferior-y-size inferior)))))

(define (set-inferior-y-end! inferior y-end)
  (set-inferior-y-start! inferior (- y-end (inferior-y-size inferior))))
(define (inferior-start inferior receiver)
  (receiver (inferior-x-start inferior)
	    (inferior-y-start inferior)))

(define (set-inferior-start-no-redisplay! inferior x-start y-start)
  (vector-set! (cdr inferior) 0 x-start)
  (vector-set! (cdr inferior) 1 y-start))

(define (set-inferior-start! inferior x-start y-start)
  (set-inferior-start-no-redisplay! inferior x-start y-start)
  (inferior-needs-redisplay! inferior))

(define (inferior-redisplay-flags inferior)
  (declare (integrate inferior))
  (vector-ref (cdr inferior) 2))

(define (set-inferior-redisplay-flags! inferior flags)
  (declare (integrate inferior flags))
  (vector-set! (cdr inferior) 2 flags))

;;;; Root Window

(define the-alpha-window)

(define (reset-alpha-window!)
  (set! the-alpha-window (make-object vanilla-window))
  (with-instance-variables vanilla-window the-alpha-window
    (set! superior #!FALSE)
    (set! x-size (screen-x-size the-alpha-screen))
    (set! y-size (screen-y-size the-alpha-screen))
    (set! redisplay-flags (list #!FALSE))
    (set! inferiors '())))

(define (update-alpha-window! #!optional display-style)
  (with-instance-variables vanilla-window the-alpha-window
    (if (and (or display-style (car redisplay-flags))
	     (=> the-alpha-window :update-display! the-alpha-screen 0 0
		 0 x-size 0 y-size display-style))
	(set-car! redisplay-flags #!FALSE))))

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: (access window-package edwin-package)
;;; Scheme Syntax Table: class-syntax-table
;;; End:
