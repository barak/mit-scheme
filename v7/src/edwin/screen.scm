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
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Virtual Screen Abstraction

(declare (usual-integrations))

(define screen?)
(define screen-x-size)
(define screen-y-size)
(define the-alpha-screen)
(define subscreen)
(define screen-inverse-video!)
(define screen-clear!)
(define subscreen-clear!)
(define screen-write-cursor!)
(define screen-write-char!)
(define screen-write-substring!)
(define screen-write-substrings!)
(let ()
(let-syntax ((make-primitive
	      (macro (name)
		(make-primitive-procedure name))))

(set! screen-inverse-video!
  (make-primitive screen-inverse-video!))

(define %screen-write-cursor!
  (make-primitive screen-write-cursor!))

(define %screen-write-ascii!
  (make-primitive screen-write-character!))

(define %screen-write-substring!
  (make-primitive screen-write-substring!))

(define %subscreen-clear!
  (make-primitive subscreen-clear!))

(define (make-screen axl axu ayl ayu)
  (vector screen-tag axl axu ayl ayu))

(define screen-tag "Screen")

(set! screen?
(named-lambda (screen? object)
  (and (vector? object)
       (not (zero? (vector-length object)))
       (eq? (vector-ref object 0) screen-tag))))

(set! screen-x-size
(named-lambda (screen-x-size screen)
  (- (vector-ref screen 2) (vector-ref screen 1))))

(set! screen-y-size
(named-lambda (screen-y-size screen)
  (- (vector-ref screen 4) (vector-ref screen 3))))

;;; Majorly bummed in two ways: (1) all clipping has been removed, on
;;; the assumption that the window system will never write outside the
;;; bounds of the screen, and (2) the only screen ever used is
;;; `the-alpha-screen', so that no offsets are needed.

(set! the-alpha-screen
      (make-screen 0 ((make-primitive screen-x-size))
		   0 ((make-primitive screen-y-size))))

(set! subscreen-clear!
(named-lambda (subscreen-clear! screen xl xu yl yu)
  (%subscreen-clear! xl yl xu yu)))

(set! screen-write-cursor!
(named-lambda (screen-write-cursor! screen x y)
  (%screen-write-cursor! x y)))

(set! screen-write-char!
(named-lambda (screen-write-char! screen x y char)
  (%screen-write-ascii! x y (char->ascii char))))

(set! screen-write-substring!
(named-lambda (screen-write-substring! screen x y string bil biu)
  (%screen-write-substring! x y string bil biu)))

(set! screen-write-substrings!
(named-lambda (screen-write-substrings! screen x y strings bil biu bjl bju)
  (with-screen screen
    (lambda (axl axu ayl ayu)
      (clip axl axu x bil biu
	(lambda (bxl ail aiu)
	  (clip ayl ayu y bjl bju
	    (lambda (byl ajl aju)
	      (define (loop y j)
		(if (< j aju)
		    (begin (%screen-write-substring! bxl y
						     (vector-ref strings j)
						     ail aiu)
			   (loop (1+ y) (1+ j)))))
	      (loop byl ajl)))))))))

(define (clip axl axu x bil biu receiver)
  (let ((ail (- bil x)))
    (if (< ail biu)
	(let ((aiu (+ ail (- axu axl))))
	  (if (positive? x)
	      (let ((bxl (+ x axl)))
		(if (< bxl axu)
		    (receiver bxl bil (if (< aiu biu) aiu biu))))
	      (receiver axl ail (if (< aiu biu) aiu biu)))))))

(define (with-screen screen receiver)
  (receiver (vector-ref screen 1)
	    (vector-ref screen 2)
	    (vector-ref screen 3)
	    (vector-ref screen 4)))

#| Old code with full clipping and screen hackery.

(set! subscreen
(named-lambda (subscreen screen xl xu yl yu)
  (with-screen screen
    (lambda (axl axu ayl ayu)
      (let ((bxl (+ xl axl))
	    (bxu (+ xu axl))
	    (byl (+ yl ayl))
	    (byu (+ yu ayl)))
	(make-screen (max axl bxl)
		     (min axu bxu)
		     (max ayl byl)
		     (min ayu byu)))))))

(set! screen-clear!
(named-lambda (screen-clear! screen)
  (with-screen screen
    (lambda (axl axu ayl ayu)
      (%subscreen-clear! axl ayl axu ayu)))))

(set! subscreen-clear!
(named-lambda (subscreen-clear! screen xl xu yl yu)
  (with-screen screen
    (lambda (axl axu ayl ayu)
      (let ((bxl (+ xl axl))
	    (bxu (+ xu axl))
	    (byl (+ yl ayl))
	    (byu (+ yu ayl)))
	(%subscreen-clear! (if (> axl bxl) axl bxl)
			   (if (> ayl byl) ayl byl)
			   (if (< axu bxu) axu bxu)
			   (if (< ayu byu) ayu byu)))))))

(set! screen-write-cursor!
(named-lambda (screen-write-cursor! screen x y)
  (with-screen screen
    (lambda (axl axu ayl ayu)
      (let ((bxl (+ axl x))
	    (byl (+ ayl y)))
	(if (and (not (negative? x)) (< bxl axu)
		 (not (negative? y)) (< byl ayu))
	    (%screen-write-cursor! bxl byl)))))))

(set! screen-write-char!
(named-lambda (screen-write-char! screen x y char)
  (with-screen screen
    (lambda (axl axu ayl ayu)
      (let ((bxl (+ axl x))
	    (byl (+ ayl y)))
	(if (and (not (negative? x)) (< bxl axu)
		 (not (negative? y)) (< byl ayu))
	    (%screen-write-ascii! bxl byl (char->ascii char))))))))

(set! screen-write-substring!
(named-lambda (screen-write-substring! screen x y string bil biu)
  (with-screen screen
    (lambda (axl axu ayl ayu)
      (clip axl axu x bil biu
	(lambda (bxl ail aiu)
	  (let ((byl (+ ayl y)))
	    (if (and (not (negative? y)) (< byl ayu))
		(%screen-write-substring! bxl byl string ail aiu)))))))))

|#

))

(define the-alpha-screen-x-size
  (screen-x-size the-alpha-screen))

(define the-alpha-screen-y-size
  (screen-y-size the-alpha-screen))

(define (screen-write-string! screen x y string)
  (screen-write-substring! screen x y string 0 (string-length string)))

(define (screen-write-strings! screen x y strings)
  (screen-write-substrings! screen x y strings
			    0 (string-length (vector-ref strings 0))
			    0 (vector-length strings)))

;;; Edwin Variables:
;;; Scheme Environment: (access window-package edwin-package)
;;; End:
