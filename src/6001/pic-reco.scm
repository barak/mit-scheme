#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;; Representation of pictures using records

(declare (usual-integrations))

(define-record-type <picture>
    (%make-picture width height)
    picture?
  (width picture-width)
  (height picture-height)
  (data picture-data %picture-set-data!)
  (min %picture-min %picture-set-min!)
  (max %picture-max %picture-set-max!)
  (image picture-image %picture-set-image!))

(define (make-picture width height #!optional initial-val)
  (let ((pic (%make-picture width height))
	(initial-val
	 (if (default-object? initial-val)
	     0.
	     (exact->inexact initial-val))))
    (%picture-set-min! pic initial-val)
    (%picture-set-max! pic initial-val)
    (%picture-set-data! pic 
			(make-initialized-vector height
			  (lambda (n)
			    n
			    (flo:make-vector width initial-val))))
    (%picture-set-image! pic #f)
    pic))

(define (picture-set-data! picture data)
  (%picture-set-data! picture data)
  (invalidate-cached-values picture))

(define (picture-min picture)
  (or (%picture-min picture)
      (begin
	(find-min-max picture)
	(%picture-min picture))))

(define (picture-max picture)
  (or (%picture-max picture)
      (begin
	(find-min-max picture)
	(%picture-max picture))))

(define (picture-set-image! picture image)
  (let ((img (picture-image picture)))
    (if (image? img)
	(image/destroy img))
    (%picture-set-image! picture image)))

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
	   (flo:vector-ref
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
	   (flo:vector-set! (vector-ref (picture-data picture) y)
			    x (exact->inexact value))
	   (invalidate-cached-values picture)))))

(define picture-ref
  (make-picture-referencer (lambda (var) (not (fix:fixnum? var)))
			   error:bad-range-argument))

(define no-error-picture-ref
  (make-picture-referencer (lambda (var) var #f)
			   (lambda (var caller) var caller #f)))

(define picture-set!
  (make-picture-setter (lambda (var) (not (fix:fixnum? var)))
		       error:bad-range-argument))

(define no-error-picture-set!
  (make-picture-setter (lambda (var) var #f)
		       (lambda (var caller) var caller #f)))

(define (picture-map! picture fn)
  (let ((picdata (picture-data picture))
	(width (picture-width picture))
	(height (picture-height picture)))
    (let y-loop ((y 0))
      (if (< y height)
	  (let ((yth-row (vector-ref picdata y)))
	    (let x-loop ((x 0))
	      (if (< x width)
		  (begin (flo:vector-set! yth-row x 
					  (exact->inexact (fn x y)))
			 (x-loop (1+ x)))
		  (y-loop (1+ y))))))
      (invalidate-cached-values picture))))

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
	 (current-min (flo:vector-ref (vector-ref picdata 0) 0))
	 (current-max current-min))
    (let y-loop ((y 0))
      (if (< y height)
	  (let ((yth-row (vector-ref picdata y)))
	    (let x-loop ((x 0))
	      (if (< x width)
		  (let ((v (flo:vector-ref yth-row x)))
		    (set! current-min (min current-min v))
		    (set! current-max (max current-max v))
		    (x-loop (1+ x)))
		  (y-loop (1+ y)))))))
    (%picture-set-min! picture current-min)
    (%picture-set-max! picture current-max)))