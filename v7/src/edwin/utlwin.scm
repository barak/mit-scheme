;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/utlwin.scm,v 1.56 1991/03/22 00:33:14 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989-91 Massachusetts Institute of Technology
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
  (string string-len string-max-length
   image image-length image-max-length
   truncate-lines? tab-width representation))

(define-integrable (string-base:string window)
  (with-instance-variables string-base window () string))

(define-integrable (string-base:string-length window)
  (with-instance-variables string-base window () string-len))

(define-integrable (string-base:image window)
  (with-instance-variables string-base window () image))

(define-integrable (string-base:image-length window)
  (with-instance-variables string-base window () image-length))

(define-integrable (string-base:representation window)
  (with-instance-variables string-base window () representation))

(define (string-base:update-display! window screen x-start y-start
				     xl xu yl yu display-style)
  display-style				;ignore
  (declare (integrate-operator clip))
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

(define (string-base:initialize! window *string *image
				 *x-size *truncate-lines? *tab-width)
  (let ((*string-length (string-length *string))
	(*image-length (string-length *image)))
    (with-instance-variables string-base window
	(*string *image *image-length *truncate-lines? *tab-width *x-size)
      (set! string *string)
      (set! string-len *string-length)
      (set! string-max-length *string-length)
      (set! image *image)
      (set! image-length *image-length)
      (set! image-max-length *image-length)
      (set! truncate-lines? *truncate-lines?)
      (set! tab-width *tab-width)
      (set! x-size *x-size)
      (set! y-size (column->y-size *image-length *x-size *truncate-lines?))
      (string-base:refresh! window))))

(define (string-base:index->coordinates window index)
  (with-instance-variables string-base window (index)
    (column->coordinates image-length
			 x-size
			 truncate-lines?
			 (substring-columns string 0 index 0 tab-width))))

(define (string-base:index->x window index)
  (with-instance-variables string-base window (index)
    (column->x image-length
	       x-size
	       truncate-lines?
	       (substring-columns string 0 index 0 tab-width))))

(define (string-base:index->y window index)
  (with-instance-variables string-base window (index)
    (column->y image-length
	       x-size
	       truncate-lines?
	       (substring-columns string 0 index 0 tab-width))))

(define (string-base:coordinates->index window x y)
  (with-instance-variables string-base window (x y)
    (substring-column->index string 0 string-len 0 tab-width
			     (let ((column (coordinates->column x y x-size)))
			       (if (fix:< column image-length)
				   column
				   image-length)))))

(define (column->x-size column-size y-size truncate-lines?)
  ;; Assume Y-SIZE > 0.
  (cond (truncate-lines?
	 column-size)
	((fix:= (fix:remainder column-size y-size) 0)
	 (fix:quotient column-size y-size))
	(else
	 (fix:+ (fix:quotient column-size y-size) 1))))

(define (column->y-size column-size x-size truncate-lines?)
  ;; Assume X-SIZE > 1.
  (cond ((or truncate-lines? (fix:< column-size x-size))
	 1)
	((fix:= (fix:remainder column-size (fix:- x-size 1)) 0)
	 (fix:quotient column-size (fix:- x-size 1)))
	(else
	 (fix:+ (fix:quotient column-size (fix:- x-size 1)) 1))))

(define (column->coordinates column-size x-size truncate-lines? column)
  (let ((-1+x-size (fix:- x-size 1)))
    (cond ((fix:< column -1+x-size)
	   (cons column 0))
	  (truncate-lines?
	   (cons -1+x-size 0))
	  ((and (fix:= (fix:remainder column -1+x-size) 0)
		(fix:= column column-size))
	   (cons -1+x-size
		 (fix:-1+ (fix:quotient column -1+x-size))))
	  (else
	   (cons (fix:remainder column -1+x-size)
		 (fix:quotient column -1+x-size))))))

(define (column->x column-size x-size truncate-lines? column)
  (let ((-1+x-size (fix:- x-size 1)))
    (cond ((fix:< column -1+x-size)
	   column)
	  (truncate-lines?
	   -1+x-size)
	  ((and (fix:= (fix:remainder column -1+x-size) 0)
		(fix:= column column-size))
	   -1+x-size)
	  (else
	   (fix:remainder column -1+x-size)))))

(define (column->y column-size x-size truncate-lines? column)
  (cond ((or truncate-lines? (fix:< column (fix:- x-size 1)))
	 0)
	((and (fix:= (fix:remainder column (fix:- x-size 1)) 0)
	      (fix:= column column-size))
	 (fix:- (fix:quotient column (fix:- x-size 1)) 1))
	(else
	 (fix:quotient column (fix:- x-size 1)))))

(define-integrable (coordinates->column x y x-size)
  (fix:+ x (fix:* y (fix:- x-size 1))))

(define (string-base:direct-output-insert-char! window x char)
  (with-instance-variables string-base window (x char)
    (if (fix:= string-len string-max-length)
	(string-base:grow-image! window 1))
    (string-set! string string-len char)
    (set! string-len (fix:+ string-len 1))
    (string-set! image image-length char)
    (set! image-length (fix:+ image-length 1))
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

(define (string-base:direct-output-insert-substring! window x string start end)
  (with-instance-variables string-base window (x string start end)
    (let ((len (fix:- end start)))
      (let ((*string-len (fix:+ string-len len)))
	(if (fix:< string-max-length *string-len)
	    (string-base:grow-image! window len))
	(substring-move-right! string start end image string-len)
	(set! string-len *string-len))
      (substring-move-right! string start end image image-length)
      (set! image-length (fix:+ image-length len)))
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

(define (string-base:grow-image! window delta)
  (let ((delta (fix:+ delta 16)))
    (with-instance-variables string-base window (delta)
      (let ((new-max-length (fix:+ string-max-length delta)))
	(set! string
	      (let ((*string (make-string new-max-length)))
		(substring-move-right! string 0 string-len *string 0)
		*string))
	(set! string-max-length new-max-length))
      (let ((new-max-length (fix:+ image-max-length delta)))
	(set! image
	      (let ((*image (make-string new-max-length)))
		(substring-move-right! image 0 image-length *image 0)
		*image))
	(set! image-max-length new-max-length)))))

(define (string-base:direct-output-insert-newline! window)
  (with-instance-variables string-base window ()
    (set! string "")
    (set! string-len 0)
    (set! string-max-length 0)
    (set! image "")
    (set! image-length 0)
    (set! image-max-length 0)
    (set! y-size 1)
    (set! representation false)))

(define (string-base:refresh! window)
  (with-instance-variables string-base window ()
    (cond ((fix:= image-length 0)
	   (set! representation false))
	  ((fix:< image-length x-size)
	   (let ((s (string-allocate x-size)))
	     (substring-move-left! image 0 image-length s 0)
	     (substring-fill! s image-length x-size #\space)
	     (set! representation s)))
	  (truncate-lines?
	   (let ((s (string-allocate x-size))
		 (x-max (fix:- x-size 1)))
	     (substring-move-left! image 0 x-max s 0)
	     (string-set! s x-max #\$)
	     (set! representation s)))
	  (else
	   (let ((rep (make-vector y-size '()))
		 (x-max (fix:- x-size 1)))
	     (let loop ((start 0) (y 0))
	       (let ((s (string-allocate x-size))
		     (end (fix:+ start x-max)))
		 (vector-set! rep y s)
		 (if (fix:> image-length end)
		     (begin
		       (substring-move-left! image start end s 0)
		       (string-set! s x-max #\\)
		       (loop end (fix:+ 1 y)))
		     (begin
		       (substring-move-left! image start image-length s 0)
		       (substring-fill! s
					(fix:- image-length start)
					x-size
					#\space)))))
	     (set! representation rep))))
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