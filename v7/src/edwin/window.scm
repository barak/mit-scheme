;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/window.scm,v 1.146 1989/04/28 22:54:38 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989 Massachusetts Institute of Technology
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
;;; NOTE: Parts of this program (Edwin) were created by translation
;;; from corresponding parts of GNU Emacs.  Users should be aware that
;;; the GNU GENERAL PUBLIC LICENSE may apply to these parts.  A copy
;;; of that license should have been included along with this file.
;;;

;;;; Window System

(declare (usual-integrations))

;;;  Based on WINDOW-WIN, designed by RMS.
;;;  See WINOPS.TXT for more information.

;;; The convention of using method names like :FOO is somewhat
;;; arbitrary.  However, methods without the prefix ":" are intended
;;; to be internal (non-public) methods.

;;; Procedural covers are used as the ultimate outside interface to
;;; the window system, since that minimizes dependence on the
;;; syntactic details of the class/object system.

;;; It is assumed in several places that all windows keep the
;;; following instance variables updated: SUPERIOR, X-SIZE, and
;;; Y-SIZE.  Thus these are normally accessed using procedure calls or
;;; instance variable references, rather than the more cumbersome
;;; method invocation.  However, these instance variables are always
;;; set by a method defined on the window itself.

;;;; Vanilla Window

(define-class vanilla-window ()
  (superior x-size y-size redisplay-flags inferiors))

(define (window-initialize! window window*)
  (with-instance-variables vanilla-window window (window*)
    (set! superior window*)
    (set! redisplay-flags (=> superior :inferior-redisplay-flags window))
    (set! inferiors '())
    unspecific))

(define (window-kill! window)
  (for-each-inferior-window window (lambda (window) (=> window :kill!))))

(define-integrable (window-superior window)
  (with-instance-variables vanilla-window window () superior))

(define (set-window-superior! window window*)
  (with-instance-variables vanilla-window window (window*)
    (set! superior window*)
    (set! redisplay-flags (=> window* :inferior-redisplay-flags window))
    (setup-redisplay-flags! redisplay-flags)
    (for-each (lambda (inferior)
		(set-inferior-redisplay-flags! inferior
					       (cons false redisplay-flags))
		(=> (inferior-window inferior) :set-superior! window))
	      inferiors)))

(define (window-root-window window)
  (with-instance-variables vanilla-window window ()
    (if superior (window-root-window superior) window)))

(define-integrable (window-x-size window)
  (with-instance-variables vanilla-window window () x-size))

(define (set-window-x-size! window x)
  (with-instance-variables vanilla-window window (x)
    (%set-window-x-size! window x)
    (setup-redisplay-flags! redisplay-flags)))

(define-integrable (%set-window-x-size! window x)
  (with-instance-variables vanilla-window window (x)
    (set! x-size x)
    unspecific))

(define-integrable (window-y-size window)
  (with-instance-variables vanilla-window window () y-size))

(define (set-window-y-size! window y)
  (with-instance-variables vanilla-window window (y)
    (%set-window-y-size! window y)
    (setup-redisplay-flags! redisplay-flags)))

(define-integrable (%set-window-y-size! window y)
  (with-instance-variables vanilla-window window (y)
    (set! y-size y)
    unspecific))

(define (window-size window receiver)
  (with-instance-variables vanilla-window window (receiver)
    (receiver x-size y-size)))

(define (set-window-size! window x y)
  (with-instance-variables vanilla-window window (x y)
    (set! x-size x)
    (set! y-size y)
    (setup-redisplay-flags! redisplay-flags)))

(define-integrable (window-redisplay-flags window)
  (with-instance-variables vanilla-window window () redisplay-flags))

(define-integrable (%window-needs-redisplay? window)
  (with-instance-variables vanilla-window window () (car redisplay-flags)))

(define-integrable (window-inferiors window)
  (with-instance-variables vanilla-window window () inferiors))

(define-integrable (window-inferior? window window*)
  (with-instance-variables vanilla-window window (window*)
    (find-inferior? inferiors window*)))

(define-integrable (window-inferior window window*)
  (with-instance-variables vanilla-window window (window*)
    (find-inferior inferiors window*)))

(define (for-each-inferior window procedure)
  (with-instance-variables vanilla-window window (procedure)
    (let loop ((inferiors inferiors))
      (if (not (null? inferiors))
	  (begin
	    (procedure (car inferiors))
	    (loop (cdr inferiors)))))))

(define (for-each-inferior-window window procedure)
  (for-each-inferior window
    (lambda (inferior) (procedure (inferior-window inferior)))))

(define (make-inferior window class)
  (with-instance-variables vanilla-window window (class)
    (let ((window* (make-object class)))
      (let ((inferior
	     (cons window*
		   (vector false
			   false
			   (cons false redisplay-flags)))))
	(set! inferiors (cons inferior inferiors))
	(=> window* :initialize! window)
	inferior))))

(define (add-inferior! window window*)
  (with-instance-variables vanilla-window window (window*)
    (set! inferiors
	  (cons (cons window*
		      (vector false
			      false
			      (cons false redisplay-flags)))
		inferiors))
    (=> window* :set-superior! window)))

(define (delete-inferior! window window*)
  (with-instance-variables vanilla-window window (window*)
    (set! inferiors
	  (delq! (find-inferior inferiors window*)
		 inferiors))))

(define (replace-inferior! window old new)
  (with-instance-variables vanilla-window window (old new)
    (set-inferior-window! (find-inferior inferiors old) new)
    (=> new :set-superior! window)))

;;; Returns #T if the redisplay finished, #F if aborted.
;;; Notice that the :UPDATE-DISPLAY! operation is assumed to return
;;; the same value.  This is used to control the setting of the
;;; redisplay flags.

(define (update-inferiors! window screen x-start y-start xl xu yl yu
			   display-style)
  (with-instance-variables vanilla-window window
			   (screen x-start y-start xl xu yl yu display-style)
    (let loop ((inferiors inferiors))
      (if (null? inferiors)
	  true
	  (let ((window (inferior-window (car inferiors)))
		(xi (inferior-x-start (car inferiors)))
		(yi (inferior-y-start (car inferiors)))
		(flags (inferior-redisplay-flags (car inferiors))))
	    (let ((continue
		   (lambda ()
		     (set-car! flags false)
		     (loop (cdr inferiors)))))
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
		       (continue))
		  (continue))))))))

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
	  true)
      (if (positive? au)
	  (receiver 0 (if (< bs au) bs au))
	  true)))

(define (salvage-inferiors! window)
  (for-each-inferior-window window (lambda (window) (=> window :salvage!))))

;;;; Standard Methods
;;;  All windows support these operations

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

;;;; Inferiors

(define (inferior-position inferior)
  (and (inferior-x-start inferior)
       (inferior-y-start inferior)
       (cons (inferior-x-start inferior)
	     (inferior-y-start inferior))))

(define (set-inferior-position! inferior position)
  (if (not position)
      (set-inferior-start! inferior false false)
      (set-inferior-start! inferior (car position) (cdr position))))

(define (inferior-needs-redisplay! inferior)
  (if (and (inferior-x-start inferior)
	   (inferior-y-start inferior))
      (setup-redisplay-flags! (inferior-redisplay-flags inferior))
      (set-car! (inferior-redisplay-flags inferior) false))
  unspecific)

(define (setup-redisplay-flags! flags)
  (if (not (or (null? flags) (car flags)))
      (begin
	(set-car! flags true)
	(setup-redisplay-flags! (cdr flags)))))

(define-integrable (inferior-x-size inferior)
  (window-x-size (inferior-window inferior)))

(define-integrable (%set-inferior-x-size! inferior x)
  (%set-window-x-size! (inferior-window inferior) x))

(define-integrable (set-inferior-x-size! inferior x)
  (=> (inferior-window inferior) :set-x-size! x))

(define-integrable (inferior-y-size inferior)
  (window-y-size (inferior-window inferior)))

(define-integrable (%set-inferior-y-size! inferior y)
  (%set-window-y-size! (inferior-window inferior) y))

(define-integrable (set-inferior-y-size! inferior y)
  (=> (inferior-window inferior) :set-y-size! y))

(define-integrable (inferior-size inferior receiver)
  (window-size (inferior-window inferior) receiver))

(define-integrable (set-inferior-size! inferior x y)
  (=> (inferior-window inferior) :set-size! x y))

(define-integrable (find-inferior? inferiors window)
  (assq window inferiors))

(define-integrable (find-inferior inferiors window)
  (or (find-inferior? inferiors window)
      (error "Window is not an inferior" window)))

(define-integrable inferior-window car)
(define-integrable set-inferior-window! set-car!)

(define-integrable (inferior-x-start inferior)
  (vector-ref (cdr inferior) 0))

(define-integrable (%set-inferior-x-start! inferior x-start)
  (vector-set! (cdr inferior) 0 x-start)
  unspecific)

(define (set-inferior-x-start! inferior x-start)
  (%set-inferior-x-start! inferior x-start)
  (inferior-needs-redisplay! inferior))

(define (inferior-x-end inferior)
  (let ((x-start (inferior-x-start inferior)))
    (and x-start
	 (+ x-start (inferior-x-size inferior)))))

(define (set-inferior-x-end! inferior x-end)
  (set-inferior-x-start! inferior (- x-end (inferior-x-size inferior))))

(define-integrable (inferior-y-start inferior)
  (vector-ref (cdr inferior) 1))

(define-integrable (%set-inferior-y-start! inferior y-start)
  (vector-set! (cdr inferior) 1 y-start)
  unspecific)

(define (set-inferior-y-start! inferior y-start)
  (%set-inferior-y-start! inferior y-start)
  (inferior-needs-redisplay! inferior))

(define (inferior-y-end inferior)
  (let ((y-start (inferior-y-start inferior)))
    (and y-start
	 (+ y-start (inferior-y-size inferior)))))

(define (set-inferior-y-end! inferior y-end)
  (set-inferior-y-start! inferior (- y-end (inferior-y-size inferior))))
(define (inferior-start inferior receiver)
  (receiver (inferior-x-start inferior)
	    (inferior-y-start inferior)))

(define (set-inferior-start! inferior x-start y-start)
  (vector-set! (cdr inferior) 0 x-start)
  (vector-set! (cdr inferior) 1 y-start)
  (inferior-needs-redisplay! inferior))

(define-integrable (inferior-redisplay-flags inferior)
  (vector-ref (cdr inferior) 2))

(define-integrable (set-inferior-redisplay-flags! inferior flags)
  (vector-set! (cdr inferior) 2 flags)
  unspecific)