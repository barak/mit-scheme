#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/6001/pic-imag.scm,v 1.2 1992/04/13 19:19:45 hal Exp $

Copyright (c) 1991-92 Massachusetts Institute of Technology

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

;;;; 6.001 Images

(declare (usual-integrations))

;;; Procedure to build an image given a picture and the magnification factors

(define (build-image pic window h-sf v-sf pic-min pic-max)
  (let* ((gray-map (n-gray-map window))
	 (pic-height (picture-height pic))	;py
	 (pic-width (picture-width pic))	;x
	 (pic-data (picture-data pic))
	 (image-width (fix:* h-sf pic-width)) ;x
	 (image-height (fix:* v-sf pic-height)) ;iy
	 (image (graphics-operation window 'create-image
				    image-width image-height))
	 (byte-string (make-string (fix:* image-width image-height)))
	 (py-max (- pic-height 1))
	 (rect-index-height (fix:* v-sf image-width))
	 (range (flo:- pic-max pic-min))
	 (mul (if (flo:< range 1e-12)
		  0.
		  (/ (string-length gray-map)
		     (flo:* (flo:+ 1. 7.142e-8)	; 1+epsilon
			    range)))))

    ;; The range was slightly adjusted so that an illegal grey level would
    ;; never be generated. epsilon was carefully chosen so that no error would
    ;; be incurred in transforming to actual grey levels up to a gray-levels
    ;; of 2^24. In general, choose epsilon such that:
    ;;            gray-levels < (/ (1+ epsilon) epsilon)

    (cond ((and (fix:= 1 h-sf) (fix:= 1 v-sf))
	   (let y-loop ((py py-max) (iy-index 0))
	     (if (fix:<= 0 py)
		 (begin
		   (let ((pic-row (floating-vector-ref pic-data py)))
		     (let x-loop ((px 0))
		       (if (fix:< px pic-width)
			   (begin
			     (vector-8b-set!
			      byte-string
			      (fix:+ px iy-index)
			      (vector-8b-ref
			       gray-map
			       (flo:floor->exact
				(flo:* mul 
				       (flo:- (floating-vector-ref pic-row px) 
					      pic-min)))))
			     (x-loop (fix:+ px 1))))))
		   (y-loop (fix:- py 1) (fix:+ iy-index rect-index-height))))))

	  ((and (fix:= 2 h-sf) (fix:= 2 v-sf))
	   (let y-loop ((py py-max) (iy-index 0))
	     (if (fix:<= 0 py)
		 (let ((pic-row (floating-vector-ref pic-data py)))
		   (let x-loop ((px 0) (ix 0))
		     (if (fix:< px pic-width)
			 (let* ((n-is-0 (fix:+ ix iy-index))
				(n-is-1 (fix:+ n-is-0 image-width))
				(v
				 (vector-8b-ref
				  gray-map
				  (flo:floor->exact
				   (flo:* mul 
					  (flo:- (floating-vector-ref pic-row
								      px)
						 pic-min))))))
			   (vector-8b-set! byte-string n-is-0 v)
			   (vector-8b-set! byte-string (fix:+ n-is-0 1) v)
			   (vector-8b-set! byte-string n-is-1 v)
			   (vector-8b-set! byte-string (fix:+ n-is-1 1) v)
			   (x-loop (fix:+ px 1) (fix:+ ix h-sf)))
			 (y-loop (fix:- py 1) 
				 (fix:+ iy-index rect-index-height))))))))

	  ((and (fix:= 3 h-sf) (fix:= 3 v-sf))
	   (let y-loop ((py py-max) (iy-index 0))
	     (if (fix:<= 0 py)
		 (let ((pic-row (floating-vector-ref pic-data py)))
		   (let x-loop ((px 0) (ix 0))
		     (if (fix:< px pic-width)
			 (let* ((row0 (fix:+ ix iy-index))
				(row1 (fix:+ row0 image-width))
				(row2 (fix:+ row1 image-width))
				(v
				 (vector-8b-ref
				  gray-map
				  (flo:floor->exact
				   (flo:* mul 
					  (flo:- (floating-vector-ref pic-row
								      px)
						 pic-min))))))
			   (vector-8b-set! byte-string row0 v)
			   (vector-8b-set! byte-string (fix:+ row0 1) v)
			   (vector-8b-set! byte-string (fix:+ row0 2) v)
			   (vector-8b-set! byte-string row1 v)
			   (vector-8b-set! byte-string (fix:+ row1 1) v)
			   (vector-8b-set! byte-string (fix:+ row1 2) v)
			   (vector-8b-set! byte-string row2 v)
			   (vector-8b-set! byte-string (fix:+ row2 1) v)
			   (vector-8b-set! byte-string (fix:+ row2 2) v)
			   (x-loop (fix:+ px 1) (fix:+ ix h-sf)))
			 (y-loop (fix:- py 1) 
				 (fix:+ iy-index rect-index-height))))))))

	  ((and (fix:= 4 h-sf) (fix:= 4 v-sf))
	   (let y-loop ((py py-max) (iy-index 0))
	     (if (fix:<= 0 py)
		 (let ((pic-row (floating-vector-ref pic-data py)))
		   (let x-loop ((px 0) (ix 0))
		     (if (fix:< px pic-width)
			 (let* ((row0 (fix:+ ix iy-index))
				(row1 (fix:+ row0 image-width))
				(row2 (fix:+ row1 image-width))
				(row3 (fix:+ row2 image-width))
				(v
				 (vector-8b-ref
				  gray-map
				  (flo:floor->exact
				   (flo:* mul 
					  (flo:- (floating-vector-ref pic-row
								      px)
						 pic-min))))))
			   (vector-8b-set! byte-string row0 v)
			   (vector-8b-set! byte-string (fix:+ row0 1) v)
			   (vector-8b-set! byte-string (fix:+ row0 2) v)
			   (vector-8b-set! byte-string (fix:+ row0 3) v)
			   (vector-8b-set! byte-string row1 v)
			   (vector-8b-set! byte-string (fix:+ row1 1) v)
			   (vector-8b-set! byte-string (fix:+ row1 2) v)
			   (vector-8b-set! byte-string (fix:+ row1 3) v)
			   (vector-8b-set! byte-string row2 v)
			   (vector-8b-set! byte-string (fix:+ row2 1) v)
			   (vector-8b-set! byte-string (fix:+ row2 2) v)
			   (vector-8b-set! byte-string (fix:+ row2 3) v)
			   (vector-8b-set! byte-string row3 v)
			   (vector-8b-set! byte-string (fix:+ row3 1) v)
			   (vector-8b-set! byte-string (fix:+ row3 2) v)
			   (vector-8b-set! byte-string (fix:+ row3 3) v)
			   (x-loop (fix:+ px 1) (fix:+ ix h-sf)))
			 (y-loop (fix:- py 1) 
				 (fix:+ iy-index rect-index-height))))))))

	  (else 
	   (let y-loop ((py py-max) (iy-index 0))
	     (if (fix:<= 0 py)
		 (let ((pic-row (floating-vector-ref pic-data py)))
		   (let x-loop ((px 0) (ix 0))
		     (if (fix:< px pic-width)
			 (let* ((v
				 (vector-8b-ref
				  gray-map
				  (flo:floor->exact
				   (flo:* mul 
					  (flo:- (floating-vector-ref pic-row
								      px)
						 pic-min)))))
				(n-start (fix:+ ix iy-index))
				(n-end (fix:+ n-start rect-index-height)))
			   (let n-loop ((n n-start))
			     (if (fix:< n n-end)
				 (let ((m-end (fix:+ n h-sf)))
				   (let m-loop ((m n))
				     (if (fix:< m m-end)
					 (begin
					   (vector-8b-set! byte-string
							   m v)
					   (m-loop (fix:+ m 1)))
					 (n-loop (fix:+ n image-width)))))
				 (x-loop (fix:+ px 1) (fix:+ ix h-sf)))))
			 (y-loop (fix:- py 1) 
				 (fix:+ iy-index rect-index-height)))))))))
    
    (x-image/fill-from-byte-vector image byte-string)
    (1d-table/put! (graphics-device/properties window) image #t)
    image))

