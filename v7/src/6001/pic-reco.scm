#| -*-Scheme-*-

$Id: pic-reco.scm,v 1.6 1993/11/17 22:58:44 adams Exp $

Copyright (c) 1993 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;; Representation of pictures using records

(declare (usual-integrations))

(define picture-type (make-record-type 
		      'picture 
		      '(width
			height
			data
			min
			max 
			image)))

(define %make-picture (record-constructor picture-type '(width height)))

(define %picture-min (record-accessor picture-type 'min))
(define %picture-max (record-accessor picture-type 'max))
(define %picture-set-data! (record-updater picture-type 'data))
(define %picture-set-image! (record-updater picture-type 'image))
(define %picture-set-min! (record-updater picture-type 'min))
(define %picture-set-max! (record-updater picture-type 'max))

(define (make-picture width height #!optional initial-val)
  (let ((pic (%make-picture width height))
	(initial-val (if (default-object? initial-val)
			 0.
			 (exact->inexact initial-val))))
    (%picture-set-min! pic initial-val)
    (%picture-set-max! pic initial-val)
    (%picture-set-data! pic 
			(make-initialized-vector
			 height
			 (lambda (n)
			   n	; ignored
			   (make-floating-vector width initial-val))))
    (%picture-set-image! pic #f)
    pic))

(define picture? (record-predicate picture-type))

(define picture-width
  (record-accessor picture-type 'width))

(define picture-height
  (record-accessor picture-type 'height))

(define picture-data
  (record-accessor picture-type 'data))

(define picture-image
  (record-accessor picture-type 'image))

(define (picture-set-image! picture image)
  (let ((img (picture-image picture)))
    (if (image? img)
	(image/destroy img))
    (%picture-set-image! picture image)))

(define (picture-min picture)
  (let ((pic-min (%picture-min picture)))
    (if (not pic-min) 
	(begin (find-min-max picture)
	       (%picture-min picture))
	pic-min)))

(define (picture-max picture)
  (let ((pic-max (%picture-max picture)))
    (if (not pic-max) 
	(begin (find-min-max picture)
	       (%picture-max picture))
	pic-max)))

(define (make-picture-referencer bad-type-predicate bad-range-signal)
  (lambda (picture x y)
    (cond ((bad-type-predicate x)
	   (error:wrong-type-argument x "picture X coordinate" 'PICTURE-REF))
	  ((bad-type-predicate y)
	   (error:wrong-type-argument y "picture Y coordinate" 'PICTURE-REF))
	  ((not (and (fix:>= x 0)
		     (fix:< x (picture-width picture))))
	   (bad-range-signal x 'PICTURE-REF))
	  ((not (and (fix:>= y 0)
		     (fix:< y (picture-height picture))))
	   (bad-range-signal y 'PICTURE-REF))
	  (else
	   (floating-vector-ref
	    (vector-ref (picture-data picture) y) x)))))

(define (make-picture-setter bad-type-predicate bad-range-signal)
  (lambda (picture x y value)
    (cond ((bad-type-predicate x)
	   (error:wrong-type-argument x "picture X coordinate" 'PICTURE-SET!))
	  ((bad-type-predicate y)
	   (error:wrong-type-argument y "picture Y coordinate" 'PICTURE-SET!))
	  ((not (and (fix:>= x 0)
		     (fix:< x (picture-width picture))))
	   (bad-range-signal x 'PICTURE-SET!))
	  ((not (and (fix:>= y 0)
		     (fix:< y (picture-height picture))))
	   (bad-range-signal y 'PICTURE-SET!))
	  (else
	   (floating-vector-set! (vector-ref (picture-data picture) y)
			x (exact->inexact value))
	   (invalidate-cached-values picture)))))

(define picture-ref (make-picture-referencer
		     (lambda (var)
		       (declare (integrate var))
		       (not (fix:fixnum? var)))
		     error:bad-range-argument))

(define no-error-picture-ref (make-picture-referencer
			  (lambda (var)
			    (declare (integrate var))
			    var  ;ignored
			    false)
			  (lambda (var proc-name)
			    var proc-name   ;ignored
			    false)))

(define picture-set! (make-picture-setter
		      (lambda (var)
			(declare (integrate var))
			(not (fix:fixnum? var)))
		      error:bad-range-argument))

(define no-error-picture-set! (make-picture-setter
			   (lambda (var)
			     (declare (integrate var))
			     var  ;ignored
			     false)
			   (lambda (var proc-name)
			     var proc-name  ;ignored 
			     false)))

(define (picture-map! picture fn)
  (let ((picdata (picture-data picture))
	(width (picture-width picture))
	(height (picture-height picture)))
    (let y-loop ((y 0))
      (if (< y height)
	  (let ((yth-row (vector-ref picdata y)))
	    (let x-loop ((x 0))
	      (if (< x width)
		  (begin (floating-vector-set! yth-row x 
				      (exact->inexact 
				       (fn x y)))
			 (x-loop (1+ x)))
		  (y-loop (1+ y))))))
      (invalidate-cached-values picture))))

(define (picture-set-data! picture data)
  (%picture-set-data! picture data)
  (invalidate-cached-values picture))

;;; Note that picture-data and picture-set-data! are both unsafe operations
;;; in the sense that both of them do not ensure that only floating point 
;;; numbers are ever stored in the picture array.


(define (invalidate-cached-values picture)
  (%picture-set-min! picture #f)
  (%picture-set-max! picture #f)
  (let ((img (picture-image picture)))
    (if (image? img)
	(image/destroy img))
    (%picture-set-image! picture '())))

(define (find-min-max picture)
  (let* ((picdata (picture-data picture))
	 (width (picture-width picture))
	 (height (picture-height picture))
	 (current-min (floating-vector-ref (vector-ref picdata 0) 0))
	 (current-max current-min))
    (let y-loop ((y 0))
      (if (< y height)
	  (let ((yth-row (vector-ref picdata y)))
	    (let x-loop ((x 0))
	      (if (< x width)
		  (let ((v (floating-vector-ref yth-row x)))
		    (set! current-min (min current-min v))
		    (set! current-max (max current-max v))
		    (x-loop (1+ x)))
		  (y-loop (1+ y)))))))
    (%picture-set-min! picture current-min)
    (%picture-set-max! picture current-max)))
