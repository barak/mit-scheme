;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/utlwin.scm,v 1.55 1990/11/02 03:24:51 cph Rel $
;;;
;;;	Copyright (c) 1986, 1989, 1990 Massachusetts Institute of Technology
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

;;;; Utility Windows

(declare (usual-integrations))

;;;; String Window
;;;  This "mixin" defines a common base from which 2D text string
;;;  windows can be built.  Mostly, it provides standard procedures
;;;  from which methods can be built.

(define-class string-base vanilla-window
  (image representation truncate-lines?))

(define-integrable (string-base:representation window)
  (with-instance-variables string-base window () representation))

(define (string-base:update-display! window screen x-start y-start
				     xl xu yl yu display-style)
  display-style				;ignore
  (let ((representation (string-base:representation window)))
    (cond ((false? representation)
	   (screen-clear-rectangle screen
				   x-start (fix:+ x-start xu)
				   y-start (fix:+ y-start yu)
				   false))
	  ((string? representation)
	   (screen-output-substring screen x-start y-start
				    representation
				    0 (string-length representation) false))
	  (else
	   (clip (screen-x-size screen) (fix:+ x-start xl) xl xu
	     (lambda (x il iu)
	       (clip (screen-y-size screen) (fix:+ y-start yl) yl yu
		 (lambda (y jl ju)
		   (let loop ((y y) (j jl))
		     (if (fix:< j ju)
			 (begin
			   (screen-output-substring screen x y
						    (vector-ref representation
								j)
						    il iu false)
			   (loop (fix:1+ y) (fix:1+ j))))))))))))
  true)

(define (clip axu x bil biu receiver)
  (let ((ail (fix:- bil x)))
    (if (fix:< ail biu)
	(let ((aiu (fix:+ ail axu)))
	  (cond ((fix:<= x 0)
		 (receiver 0 ail (if (fix:< aiu biu) aiu biu)))
		((fix:< x axu)
		 (receiver x bil (if (fix:< aiu biu) aiu biu))))))))

(define-method string-base :update-display!
  string-base:update-display!)

(define (string-base:set-size-given-x! window x *truncate-lines?)
  (with-instance-variables string-base window (x *truncate-lines?)
    (set! truncate-lines? *truncate-lines?)
    (set! x-size x)
    (set! y-size (string-base:desired-y-size window x))
    (string-base:refresh! window)))

(define (string-base:set-size-given-y! window y *truncate-lines?)
  (with-instance-variables string-base window (y *truncate-lines?)
    (set! truncate-lines? *truncate-lines?)
    (set! x-size (string-base:desired-x-size window y))
    (set! y-size y)
    (string-base:refresh! window)))

(define (string-base:desired-x-size window y-size)
  (with-instance-variables string-base window (y-size)
    (column->x-size (image-column-size image) y-size truncate-lines?)))

(define (string-base:desired-y-size window x-size)
  (with-instance-variables string-base window (x-size)
    (column->y-size (image-column-size image) x-size truncate-lines?)))

(define (string-base:index->coordinates window index)
  (with-instance-variables string-base window (index)
    (column->coordinates (image-column-size image)
			 x-size
			 truncate-lines?
			 (image-index->column image index))))

(define (string-base:index->x window index)
  (with-instance-variables string-base window (index)
    (column->x (image-column-size image)
	       x-size
	       truncate-lines?
	       (image-index->column image index))))

(define (string-base:index->y window index)
  (with-instance-variables string-base window (index)
    (column->y (image-column-size image)
	       x-size
	       truncate-lines?
	       (image-index->column image index))))

(define (string-base:coordinates->index window x y)
  (with-instance-variables string-base window (x y)
    (image-column->index image
			 (let ((column (coordinates->column x y x-size))
			       (size (image-column-size image)))
			   (if (fix:< column size)
			       column
			       size)))))

(define (column->x-size column-size y-size truncate-lines?)
  ;; Assume Y-SIZE > 0.
  (if truncate-lines?
      column-size
      (let ((qr (integer-divide column-size y-size)))
	(if (fix:= (integer-divide-remainder qr) 0)
	    (integer-divide-quotient qr)
	    (fix:1+ (integer-divide-quotient qr))))))

(define (column->y-size column-size x-size truncate-lines?)
  ;; Assume X-SIZE > 1.
  (if (or truncate-lines? (fix:< column-size x-size))
      1
      (let ((qr (integer-divide column-size (fix:-1+ x-size))))
	(if (fix:= (integer-divide-remainder qr) 0)
	    (integer-divide-quotient qr)
	    (fix:1+ (integer-divide-quotient qr))))))

(define (column->coordinates column-size x-size truncate-lines? column)
  (let ((-1+x-size (fix:-1+ x-size)))
    (cond ((fix:< column -1+x-size)
	   (cons column 0))
	  (truncate-lines?
	   (cons -1+x-size 0))
	  (else
	   (let ((qr (integer-divide column -1+x-size)))
	     (if (and (fix:= (integer-divide-remainder qr) 0)
		      (fix:= column column-size))
		 (cons -1+x-size
		       (fix:-1+ (integer-divide-quotient qr)))
		 (cons (integer-divide-remainder qr)
		       (integer-divide-quotient qr))))))))

(define (column->x column-size x-size truncate-lines? column)
  (let ((-1+x-size (fix:-1+ x-size)))
    (cond ((fix:< column -1+x-size)
	   column)
	  (truncate-lines?
	   -1+x-size)
	  (else
	   (let ((r (remainder column -1+x-size)))
	     (if (and (fix:= r 0) (fix:= column column-size))
		 -1+x-size
		 r))))))

(define (column->y column-size x-size truncate-lines? column)
  (if (or truncate-lines? (fix:< column (fix:-1+ x-size)))
      0
      (let ((qr (integer-divide column (fix:-1+ x-size))))
	(if (and (fix:= (integer-divide-remainder qr) 0)
		 (fix:= column column-size))
	    (fix:-1+ (integer-divide-quotient qr))
	    (integer-divide-quotient qr)))))

(define-integrable (coordinates->column x y x-size)
  (fix:+ x (fix:* y (fix:-1+ x-size))))

(define (string-base:direct-output-insert-char! window x char)
  (with-instance-variables string-base window (x char)
    (image-direct-output-insert-char! image char)
    (cond ((false? representation)
	   (let ((s (string-allocate x-size)))
	     (string-fill! s #\space)
	     (string-set! s x char)
	     (set! representation s)))
	  ((string? representation)
	   (string-set! representation x char))
	  (else
	   (string-set! (vector-ref representation (fix:-1+ y-size))
			x
			char)))))

(define (string-base:direct-output-insert-newline! window)
  (with-instance-variables string-base window ()
    (set! image (make-null-image))
    (set! y-size 1)
    (set! representation false)))

(define (string-base:direct-output-insert-substring! window x string start end)
  (with-instance-variables string-base window (x string start end)
    (image-direct-output-insert-substring! image string start end)
    (cond ((false? representation)
	   (let ((s (string-allocate x-size)))
	     (substring-fill! s 0 x #\space)
	     (substring-move-left! string start end s x)
	     (substring-fill! s (fix:+ x (fix:- end start)) x-size #\space)
	     (set! representation s)))
	  ((string? representation)
	   (substring-move-left! string start end representation x))
	  (else
	   (substring-move-left! string start end
				 (vector-ref representation (fix:-1+ y-size))
				 x)))))

(define (string-base:refresh! window)
  (with-instance-variables string-base window ()
    (let ((string (image-representation image)))
      (let ((column-size (string-length string)))
	(cond ((fix:= column-size 0)
	       (set! representation false))
	      ((fix:< column-size x-size)
	       (let ((s (string-allocate x-size)))
		 (substring-move-left! string 0 column-size s 0)
		 (substring-fill! s column-size x-size #\space)
		 (set! representation s)))
	      (truncate-lines?
	       (let ((s (string-allocate x-size))
		     (x-max (fix:-1+ x-size)))
		 (substring-move-left! string 0 x-max s 0)
		 (string-set! s x-max #\$)
		 (set! representation s)))
	      (else
	       (let ((rep (make-vector y-size '()))
		     (x-max (fix:-1+ x-size)))
		 (let loop ((start 0) (y 0))
		   (let ((s (string-allocate x-size))
			 (end (fix:+ start x-max)))
		     (vector-set! rep y s)
		     (if (fix:> column-size end)
			 (begin
			   (substring-move-left! string start end s 0)
			   (string-set! s x-max #\\)
			   (loop end (fix:1+ y)))
			 (begin
			   (substring-move-left! string start column-size s 0)
			   (substring-fill! s
					    (fix:- column-size start)
					    x-size
					    #\space)))))
		 (set! representation rep))))))
    (setup-redisplay-flags! redisplay-flags)))

;;;; Blank Window

(define-class blank-window vanilla-window
  ())

(define (blank-window:update-display! window screen x-start y-start
				      xl xu yl yu display-style)
  window display-style			;ignore
  (screen-clear-rectangle screen
			  (fix:+ x-start xl) (fix:+ x-start xu)
			  (fix:+ y-start yl) (fix:+ y-start yu)
			  false)
  true)

(define-method blank-window :update-display!
  blank-window:update-display!)

;;;; Vertical Border Window

(define-class vertical-border-window vanilla-window
  ())

(define-method vertical-border-window (:initialize! window window*)
  (usual=> window :initialize! window*)
  (set! x-size 1))

(define-method vertical-border-window (:set-x-size! window x)
  window				;ignore
  (error "Can't change the x-size of a vertical border window" x))

(define-method vertical-border-window (:set-size! window x y)
  (if (not (fix:= x 1))
      (error "Can't change the x-size of a vertical border window" x))
  (set! x-size x)
  (set! y-size y)
  (setup-redisplay-flags! redisplay-flags))

(define (vertical-border-window:update-display! window screen x-start y-start
						xl xu yl yu display-style)
  display-style				;ignore
  (if (fix:< xl xu)
      (clip-window-region-1 yl yu (window-y-size window)
	(lambda (yl yu)
	  (let ((xl (fix:+ x-start xl))
		(yu (fix:+ y-start yu)))
	    (let loop ((y (fix:+ y-start yl)))
	      (if (fix:< y yu)
		  (begin
		    (screen-output-char screen xl y #\| false)
		    (loop (fix:+ y 1)))))))))
  true)

(define-method vertical-border-window :update-display!
  vertical-border-window:update-display!)

;;;; Cursor Window

(define-class cursor-window vanilla-window
  (enabled?))

(define-method cursor-window (:initialize! window window*)
  (usual=> window :initialize! window*)
  (set! x-size 1)
  (set! y-size 1)
  (set! enabled? false))

(define-method cursor-window (:set-x-size! window x)
  window				;ignore
  (error "Can't change the size of a cursor window" x))

(define-method cursor-window (:set-y-size! window y)
  window				;ignore
  (error "Can't change the size of a cursor window" y))

(define-method cursor-window (:set-size! window x y)
  window				;ignore
  (error "Can't change the size of a cursor window" x y))

(define (cursor-window:update-display! window screen x-start y-start
				       xl xu yl yu display-style)
  display-style				;ignore
  (if (and (with-instance-variables cursor-window window () enabled?)
	   (fix:< xl xu)
	   (fix:< yl yu))
      (screen-move-cursor screen x-start y-start))
  true)

(define-method cursor-window :update-display!
  cursor-window:update-display!)

(define-method cursor-window (:enable! window)
  (set! enabled? true)
  (setup-redisplay-flags! redisplay-flags))

(define-method cursor-window (:disable! window)
  (set! enabled? false)
  (set-car! redisplay-flags false))