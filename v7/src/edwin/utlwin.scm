;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/utlwin.scm,v 1.50 1989/03/14 08:03:43 cph Exp $
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

;;;; Utility Windows

(declare (usual-integrations))

;;;; String Window
;;;  This "mixin" defines a common base from which 2D text string
;;;  windows can be built.  Mostly, it provides standard procedures
;;;  from which methods can be built.

(define-class string-base vanilla-window
  (image representation))

(define-method string-base (:update-display! window screen x-start y-start
					     xl xu yl yu display-style)
  window display-style			;ignore
  (cond ((pair? representation)
	 (cond ((not (cdr representation))
		;; disable clipping.
		(subscreen-clear! screen
				  x-start (+ x-start xu)
				  y-start (+ y-start yu))
#|
		(subscreen-clear! screen
				  (+ x-start xl) (+ x-start xu)
				  (+ y-start yl) (+ y-start yu))|#
		)
	       ((< yl yu)
		(let ((start (cdr representation))
		      (end (string-length (car representation)))
		      (ayu (+ y-start yu)))
		  ;; disable clipping.
		  (if (not (zero? start))
		      (subscreen-clear! screen
					x-start (+ x-start start)
					y-start ayu))
		  (screen-write-substring! screen
					   (+ x-start start) y-start
					   (car representation)
					   start end)
		  (subscreen-clear! screen
				    (+ x-start end) (+ x-start x-size)
				    y-start ayu)#|
		  (if (not (zero? start))
		      (clip-window-region-1 xl xu start
			(lambda (xl xu)
			  (subscreen-clear! screen
					    (+ x-start xl) (+ x-start xu)
					    ayl ayu))))
		  (clip-window-region-1 (- xl start) (- xu start) (- end start)
		    (lambda (xl xu)
		      (let ((xl* (+ xl start)))
			(screen-write-substring! screen
						 (+ x-start xl*) ayl
						 (car representation)
						 xl* (+ xu start)))))
		  (clip-window-region-1 (- xl end) (- xu end) (- x-size end)
		    (lambda (xl xu)
		      (let ((x-start (+ x-start end)))
			(subscreen-clear! screen
					  (+ x-start xl) (+ x-start xu)
					  ayl ayu))))|#
		  ))))
	(else
	 (screen-write-substrings! screen (+ x-start xl) (+ y-start yl)
				   representation xl xu yl yu)))
  true)

(define (string-base:set-size-given-x! window x)
  (with-instance-variables string-base window (x)
    (set! x-size x)
    (set! y-size (string-base:desired-y-size window x))
    (string-base:refresh! window)))

(define (string-base:set-size-given-y! window y)
  (with-instance-variables string-base window (y)
    (set! x-size (string-base:desired-x-size window y))
    (set! y-size y)
    (string-base:refresh! window)))

(define-integrable (string-base:desired-x-size window y-size)
  (with-instance-variables string-base window (y-size)
    (column->x-size (image-column-size image) y-size)))

(define-integrable (string-base:desired-y-size window x-size)
  (with-instance-variables string-base window (x-size)
    (column->y-size (image-column-size image) x-size)))

(define (string-base:index->coordinates window index)
  (with-instance-variables string-base window (index)
    (column->coordinates (image-column-size image)
			 x-size
			 (image-index->column image index))))

(define (string-base:index->x window index)
  (with-instance-variables string-base window (index)
    (column->x (image-column-size image)
	       x-size
	       (image-index->column image index))))

(define (string-base:index->y window index)
  (with-instance-variables string-base window (index)
    (column->y (image-column-size image)
	       x-size
	       (image-index->column image index))))

(define (string-base:coordinates->index window x y)
  (with-instance-variables string-base window (x y)
    (image-column->index image
			 (min (coordinates->column x y x-size)
			      (image-column-size image)))))

(define (column->x-size column-size y-size)
  ;; Assume Y-SIZE > 0.
  (let ((qr (integer-divide column-size y-size)))
    (if (zero? (integer-divide-remainder qr))
	(integer-divide-quotient qr)
	(1+ (integer-divide-quotient qr)))))

(define (column->y-size column-size x-size)
  ;; Assume X-SIZE > 1.
  (if (zero? column-size)
      1
      (let ((qr (integer-divide column-size (-1+ x-size))))
	(if (zero? (integer-divide-remainder qr))
	    (integer-divide-quotient qr)
	    (1+ (integer-divide-quotient qr))))))

(define (column->coordinates column-size x-size column)
  (let ((-1+x-size (-1+ x-size)))
    (if (< column -1+x-size)
	(cons column 0)
	(let ((qr (integer-divide column -1+x-size)))
	  (if (and (zero? (integer-divide-remainder qr))
		   (= column column-size))
	      (cons -1+x-size
		    (-1+ (integer-divide-quotient qr)))
	      (cons (integer-divide-remainder qr)
		    (integer-divide-quotient qr)))))))

(define (column->x column-size x-size column)
  (let ((-1+x-size (-1+ x-size)))
    (if (< column -1+x-size)
	column
	(let ((r (remainder column -1+x-size)))
	  (if (and (zero? r) (= column column-size))
	      -1+x-size
	      r)))))

(define (column->y column-size x-size column)
  (let ((-1+x-size (-1+ x-size)))
    (if (< column -1+x-size)
	0
	(let ((qr (integer-divide column -1+x-size)))
	  (if (and (zero? (integer-divide-remainder qr))
		   (= column column-size))
	      (-1+ (integer-divide-quotient qr))
	      (integer-divide-quotient qr))))))

(define-integrable (coordinates->column x y x-size)
  (+ x (* y (-1+ x-size))))

(define (string-base:direct-output-insert-char! window x char)
  (with-instance-variables string-base window (x char)
    (if (pair? representation)
	(begin
	  (set-car! representation
		    (string-append-char (car representation) char))
	  (if (and (not (cdr representation))
		   (not (char=? char #\Space)))
	      (set-cdr! representation x)))
	(string-set! (vector-ref representation (-1+ y-size)) x char))))

(define (string-base:direct-output-insert-newline! window)
  (with-instance-variables string-base window ()
    (set! y-size 1)
    (set! representation (cons "" false))))

(define (string-base:direct-output-insert-substring! window x string start end)
  (with-instance-variables string-base window (x string start end)
    (if (pair? representation)
	(begin
	  (set-car! representation
		    (string-append-substring (car representation)
					     string start end))
	  (if (not (cdr representation))
	      (let ((index
		     (substring-find-next-char-in-set string start end
						      char-set:not-space)))
		(if index
		    (set-cdr! representation (+ x index))))))
	(substring-move-right! string start end
			       (vector-ref representation (-1+ y-size)) x))))

(define (string-base:refresh! window)
  (with-instance-variables string-base window ()
    (let ((string (image-representation image)))
      (let ((column-size (string-length string)))
	(if (< column-size x-size)
	    (let ((start 
		   (string-find-next-char-in-set string char-set:not-space)))
	      (if (not (and (pair? representation)
			    (string=? (car representation) string)
			    (eqv? (cdr representation) start)))
		  (begin (set! representation (cons string start))
			 (setup-redisplay-flags! redisplay-flags))))
	    (let ((rep (make-vector y-size '()))
		  (x-max (-1+ x-size)))
	      (define (loop start y)
		(let ((s (string-allocate x-size))
		      (end (+ start x-max)))
		  (vector-set! rep y s)
		  (cond ((<= column-size end)
			 (substring-move-right! string start column-size s 0)
			 (substring-fill! s (- column-size start) x-size
					  #\space))
			(else
			 (substring-move-right! string start end s 0)
			 (string-set! s x-max #\\)
			 (loop end (1+ y))))))
	      (loop 0 0)
	      (set! representation rep)
	      (setup-redisplay-flags! redisplay-flags)))))))
;;;; Blank Window

(define-class blank-window vanilla-window
  ())

(define-method blank-window (:update-display! window screen x-start y-start
					      xl xu yl yu display-style)
  window display-style			;ignore
  (subscreen-clear! screen
		    (+ x-start xl) (+ x-start xu)
		    (+ y-start yl) (+ y-start yu))
  true)

;;;; Vertical Border Window

(define-class vertical-border-window vanilla-window
  ())

(define-method vertical-border-window (:initialize! window window*)
  (usual=> window :initialize! window*)
  (set! x-size 1)
  unspecific)

(define-method vertical-border-window (:set-x-size! window x)
  window				;ignore
  (error "Can't change the x-size of a vertical border window" x))

(define-method vertical-border-window (:set-size! window x y)
  (if (not (= x 1))
      (error "x-size of a vertical border window must be 1" x))
  (set! x-size x)
  (set! y-size y)
  (setup-redisplay-flags! redisplay-flags))

(define-method vertical-border-window
	       (:update-display! window screen x-start y-start
				 xl xu yl yu display-style)
  display-style				;ignore
  (if (< xl xu)
      (clip-window-region-1 yl yu y-size
	(lambda (yl yu)
	  (let ((xl (+ x-start xl))
		(yu (+ y-start yu)))
	    (let loop ((y (+ y-start yl)))
	      (if (< y yu)
		  (begin
		    (screen-write-char! screen xl y #\|)
		    (loop (1+ y)))))))))
  true)

;;;; Cursor Window

(define-class cursor-window vanilla-window
  (enabled?))

(define-method cursor-window (:initialize! window window*)
  (usual=> window :initialize! window*)
  (set! x-size 1)
  (set! y-size 1)
  (set! enabled? false)
  unspecific)

(define-method cursor-window (:set-x-size! window x)
  window				;ignore
  (error "Can't change the size of a cursor window" x))

(define-method cursor-window (:set-y-size! window y)
  window				;ignore
  (error "Can't change the size of a cursor window" y))

(define-method cursor-window (:set-size! window x y)
  window				;ignore
  (error "Can't change the size of a cursor window" x y))

(define-method cursor-window (:update-display! window screen x-start y-start
					       xl xu yl yu display-style)
  display-style				;ignore
  (if (and enabled? (< xl xu) (< yl yu))      (screen-write-cursor! screen x-start y-start))
  true)

(define-method cursor-window (:enable! window)
  (set! enabled? true)
  (setup-redisplay-flags! redisplay-flags))

(define-method cursor-window (:disable! window)
  (set! enabled? false)
  (set-car! redisplay-flags false)
  unspecific)