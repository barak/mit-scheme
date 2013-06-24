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

;;; Operations for manipulating pictures

(declare (usual-integrations))

(define-integrable (in-rect? x y width height)
  (and (fix:< -1 x) (fix:< x width) (fix:< -1 y) (fix:< y height)))

(define make-pt cons)
(define xcor car)
(define ycor cdr)

(define (picture-overlap pic1 pic2 u v)
  (let* ((wid1 (picture-width pic1))
	 (hgt1 (picture-height pic1))
	 (p1-data (picture-data pic1))
	 (wid2 (picture-width pic2))
	 (hgt2 (picture-height pic2))
	 (p2-data (picture-data pic2))
	 (u (floor->exact u))
	 (v (floor->exact v))
	 (lf (min 0 u)) 
	 (dn (min 0 v))
	 (rt (max wid2 (fix:+ wid1 u))) 
	 (up (max hgt2 (fix:+ hgt1 v)))
	 (p1x-offset (fix:- u lf))
	 (p1y-offset (fix:- v dn))
	 (new-min (min (picture-min pic1) (picture-min pic2)))
	 (new-pic (make-picture (fix:- rt lf) (fix:- up dn) new-min))
	 (new-data (picture-data new-pic)))

;; place pic2 in its proper place on the resulting picture
    (let y-loop ((y 0))
      (if (fix:< y hgt2)
	  (let* ((p2-yth-row (vector-ref p2-data y))
		 (new-yth-row (vector-ref new-data (fix:- y dn)))) 
	    (let x-loop ((x 0))
	      (if (fix:< x wid2)
		  (begin  
		    (flo:vector-set! new-yth-row (fix:- x lf) 
				     (flo:vector-ref p2-yth-row x))
		    (x-loop (fix:+ x 1)))
		  (y-loop (fix:+ y 1)))))))

    ;; overlay pic1 in its proper location in the result
    (let y-loop ((y 0))
      (if (fix:< y hgt1)
	  (let* ((p1-yth-row (vector-ref p1-data y))
		 (new-yth-row (vector-ref new-data (fix:+ y p1y-offset)))) 
	    (let x-loop ((x 0))
	      (if (fix:< x wid1)
		  (begin  
		    (flo:vector-set! new-yth-row (fix:+ x p1x-offset) 
				     (flo:vector-ref p1-yth-row x))
		    (x-loop (fix:+ x 1)))
		  (y-loop (fix:+ y 1)))))))
    (picture-set-data! new-pic new-data)
    new-pic))

(define (picture-paste! pic1 pic2 u v)
  (let ((wid1 (picture-width pic1))
	(hgt1 (picture-height pic1))
	(p1-data (picture-data pic1))
	(wid2 (picture-width pic2))
	(hgt2 (picture-height pic2))
	(p2-data (picture-data pic2))
	(u (floor->exact u))
	(v (floor->exact v)))
  (if (in-rect? u v wid2 hgt2)
      (if (and (fix:<= (fix:+ u wid1) wid2) (fix:<= (fix:+ v hgt1) hgt2))
	  (let y-loop ((y 0))
	    (if (fix:< y hgt1)
		(let ((p1-yth-row (vector-ref p1-data y))
		      (p2-yth-row (vector-ref p2-data (fix:+ y v))))
		  (let x-loop ((x 0))
		    (if (fix:< x wid1)
			(begin
			  (flo:vector-set! p2-yth-row (fix:+ x u)
					   (flo:vector-ref p1-yth-row x))
			  (x-loop (fix:+ x 1)))
			(y-loop (fix:+ y 1))))))
	    (picture-set-data! pic2 p2-data))
	  (error "Picture too large -- PICTURE-PASTE!"))
      (error "Coordinates out of bounds -- PICTURE-PASTE!"))))

(define (picture-cut pic u v cut-wid cut-hgt)
  (let* ((wid (picture-width pic))
	 (hgt (picture-height pic))
	 (data (picture-data pic))
	 (u (floor->exact u))
	 (v (floor->exact v))
	 (cut-wid (floor->exact cut-wid))
	 (cut-hgt (floor->exact cut-hgt))
	 (new-pic (make-picture cut-wid cut-hgt))
	 (new-data (picture-data new-pic)))
    (if (not (in-rect? u v wid hgt))
	(error "Coordinates out of bounds -- PICTURE-CUT")) 
    (if (not (fix:<= (fix:+ u cut-wid) wid)) 
	(error:bad-range-argument cut-wid 'PICTURE-CUT))
    (if (not (fix:<= (fix:+ v cut-hgt) hgt)) 
	(error:bad-range-argument cut-hgt 'PICTURE-CUT))
    (let y-loop ((y 0))
      (if (fix:< y cut-hgt)
	  (let ((new-yth-row (vector-ref new-data y))
		(old-yth-row (vector-ref data (fix:+ v y))))
	    (let x-loop ((x 0))
	      (if (fix:< x cut-wid)
		  (begin
		    (flo:vector-set! new-yth-row x
				     (flo:vector-ref old-yth-row (fix:+ u x)))
		    (x-loop (fix:+ x 1)))
		  (y-loop (fix:+ y 1))))))
      (picture-set-data! new-pic new-data)
      new-pic)))

(define (picture-scale pic xsf ysf)
  (let* ((wid (floor->exact (* xsf (picture-width pic))))
	 (hgt (floor->exact (* ysf (picture-height pic))))
	 (data (picture-data pic))
	 (new-pic (make-picture wid hgt))
	 (new-data (picture-data new-pic))
	 (->discrete-y (if (flo:> ysf 1.)
			   floor->exact
			   ceiling->exact))
	 (->discrete-x (if (flo:> xsf 1.)
			   floor->exact
			   ceiling->exact)))
    (let y-loop ((ny 0) (old-y-index -1))
      (if (fix:< ny hgt)
	  (let ((y-index (->discrete-y (/ ny ysf))))
	    (if (fix:= y-index old-y-index)  ; don't recompute the row
		(flo:vector-set! new-data ny
				 (flo:vector-copy
				  (vector-ref new-data (fix:- ny 1))))
		(let ((yth-row (vector-ref data y-index))
		      (new-yth-row (vector-ref new-data ny)))
		  (let x-loop ((nx 0))
		    (if (fix:< nx wid)
			(begin
			  (flo:vector-set!
			   new-yth-row nx
			   (flo:vector-ref yth-row 
					   (->discrete-x (/ nx xsf))))
			  (x-loop (fix:+ nx 1)))))))
	    (y-loop (fix:+ ny 1) y-index))))
    (picture-set-data! new-pic new-data)
    new-pic))

(define (picture-rotate pic angle)
  (define (rotate-pt-by theta)
    (lambda (x y)
      (let ((c (cos theta)) (s (sin theta)))
	(make-pt (- (* c x) (* s y))
		 (+ (* s x) (* c y))))))

  (define (close-enough? a b)
    (fix:= (round->exact a) (round->exact b)))

  (let* ((wid (picture-width pic))
	 (hgt (picture-height pic))
	 (data (picture-data pic))
	 (pic-min (picture-min pic))
	 (lf (lo-bound wid))
	 (rt (fix:- (up-bound wid) 1))
	 (dn (lo-bound hgt))
	 (up (fix:- (up-bound hgt) 1))
	 (rotate-by-angle (rotate-pt-by angle))
	 (rotate-by-neg-angle (rotate-pt-by (- angle)))
	 (ll (rotate-by-angle lf dn))        ;rotate each
	 (lr (rotate-by-angle rt dn))        ;corner
	 (ul (rotate-by-angle lf up))        ;of the
	 (ur (rotate-by-angle rt up))        ;picture
	 (lx (min (xcor ll) (xcor lr) (xcor ul) (xcor ur)))   ;compute
	 (ly (min (ycor ll) (ycor lr) (ycor ul) (ycor ur)))   ;extreme
	 (ux (max (xcor ll) (xcor lr) (xcor ul) (xcor ur)))   ;coordinate
	 (uy (max (ycor ll) (ycor lr) (ycor ul) (ycor ur)))   ;values
	 (new-wid (round->exact (1+ (- ux lx))))
	 (new-hgt (round->exact (1+ (- uy ly))))
	 (nx-max (fix:- new-wid 1))
	 (ny-max (fix:- new-hgt 1))
	 (new-lf (lo-bound new-wid))
	 (new-dn (lo-bound new-hgt))
	 (new-pic (make-picture new-wid new-hgt))
	 (new-data (picture-data new-pic)))
    ;; Special cases are rotations of 90 degrees (both directions) and 180
    ;; degrees.

    (cond ((and (close-enough? (xcor ur) ux)    ; check for 
		(close-enough? (ycor ur) uy))   ; 0 degrees
	   (set! new-data (make-initialized-vector
			   new-hgt
			   (lambda (n)
			     (flo:vector-copy
			      (vector-ref data n))))))

	  ((and (close-enough? (xcor ur) lx)   ; check for 
		(close-enough? (ycor ur) uy))  ; 90 degrees anti-clockwise
	   (let y-loop ((ny 0))
	     (if (fix:< ny new-hgt)
		 (let ((yth-row (vector-ref new-data ny)))
		   (let x-loop ((nx 0))
		     (if (fix:< nx new-wid)
			 (begin
			   (flo:vector-set! yth-row nx
					    (flo:vector-ref 
					     (vector-ref
					      data (fix:- nx-max nx))
					     ny))
			   (x-loop (fix:+ nx 1)))
			 (y-loop (fix:+ ny 1))))))))

	  ((and (close-enough? (xcor ur) ux)   ; check for
		(close-enough? (ycor ur) ly))  ; 90 degrees clockwise
	   (let y-loop ((ny 0))
	     (if (fix:< ny new-hgt)
		 (let ((yth-row (vector-ref new-data ny)))
		   (let x-loop ((nx 0))
		     (if (fix:< nx new-wid)
			 (begin
			   (flo:vector-set! yth-row nx
					    (flo:vector-ref 
					     (vector-ref data nx) 
					     (fix:- ny-max ny)))
			   (x-loop (fix:+ nx 1)))
			 (y-loop (fix:+ ny 1))))))))

	  ((and (close-enough? (xcor ur) lx)  ; check for
		(close-enough? (ycor ur) ly)) ; 180 degrees
	   (let y-loop ((ny 0))
	     (if (fix:< ny new-hgt)
		 (begin
		   (flo:vector-set! new-data ny 
				    (list->vector
				     (reverse 
				      (vector->list 
				       (vector-ref data
						   (fix:- ny-max ny))))))
		   (y-loop (fix:+ ny 1))))))

	  (else
	   (let* ((rot-bot-lef (rotate-by-neg-angle new-lf new-dn))
		  (x-start (exact->inexact 
			    (- (xcor rot-bot-lef) lf)))  ; in "vector 
		  (y-start (exact->inexact
			    (- (ycor rot-bot-lef) dn)))  ; coordinates"
		  (c (cos angle))
		  (s (sin angle))) 
	     (let y-loop ((ny 0) (outer-x x-start) (outer-y y-start))
	       (if (fix:< ny new-hgt)
		   (let ((nyth-row (vector-ref new-data ny)))
		     (let x-loop ((nx 0) (inner-x outer-x) (inner-y outer-y))
		       (if (fix:< nx new-wid)
			   (let ((x (round->exact inner-x))
				 (y (round->exact inner-y)))
			     (flo:vector-set! nyth-row nx
					      (if (in-rect? x y wid hgt)
						  (flo:vector-ref
						   (vector-ref data y) x)
						  pic-min))
			     (x-loop (fix:+ nx 1) 
				     (flo:+ inner-x c) (flo:- inner-y s)))
			   (y-loop (fix:+ ny 1) 
				   (flo:+ outer-x s) 
				   (flo:+ outer-y c))))))))))
    (picture-set-data! new-pic new-data)
    new-pic))

(define (picture-v-reflect pic)
  (let* ((wid (picture-width pic))
	 (hgt (picture-height pic))
	 (data (picture-data pic))
	 (new-pic (make-picture wid hgt))
	 (new-data (picture-data new-pic))
	 (y-max (fix:- hgt 1)))
    (let y-loop ((y 0))
      (if (fix:< y hgt)
	  (begin
	    (vector-set! new-data y 
			 (flo:vector-copy
			  (vector-ref data (fix:- y-max y))))
	    (y-loop (fix:+ y 1)))))
    (picture-set-data! new-pic new-data)
    new-pic))
	  
(define (picture-h-reflect pic)
  (let* ((wid (picture-width pic))
	 (hgt (picture-height pic))
	 (data (picture-data pic))
	 (new-pic (make-picture wid hgt))
	 (new-data (picture-data new-pic)))
    (let y-loop ((y 0))
      (if (fix:< y hgt)
	  (begin
	    (vector-set! new-data y
			 (flo:vector-reverse (vector-ref data y)))
	    (y-loop (fix:+ y 1)))))
    (picture-set-data! new-pic new-data)
    new-pic))

(define (flo:vector-reverse vector)
  (let* ((length (flo:vector-length vector))
	 (new-vector (flo:vector-cons length))
	 (length-1 (- length 1)))
    (do 
	((i 0 (+ i 1)))
	((= i length))
      (flo:vector-set! new-vector i 
		       (flo:vector-ref vector (- length-1 i))))
    new-vector))

