#| -*-Scheme-*-

$Id: pic-imag.scm,v 1.8 1997/12/30 05:43:34 cph Exp $

Copyright (c) 1991-97 Massachusetts Institute of Technology

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
	 (image-depth (graphics-operation window 'IMAGE-DEPTH))
	 (image (image/create window image-width image-height))
	 (pixels
	  (if (<= image-depth 8)
	      (make-string (fix:* image-width image-height))
	      (make-vector (fix:* image-width image-height))))
	 (write-pixel
	  (if (<= image-depth 8)
	      vector-8b-set!
	      vector-set!))
	 (py-max (- pic-height 1))
	 (rect-index-height (fix:* v-sf image-width))
	 (binner (cutoff-binner .01 pic-min pic-max (vector-length gray-map)))
	 (gray-pixel
	  (lambda (pixel-value)
	    (vector-ref gray-map (binner pixel-value)))))

    (cond ((and (fix:= 1 h-sf) (fix:= 1 v-sf))
	   (let y-loop ((py py-max) (iy-index 0))
	     (if (fix:<= 0 py)
		 (begin
		   (let ((pic-row (vector-ref pic-data py)))
		     (let x-loop ((px 0))
		       (if (fix:< px pic-width)
			   (begin
			     (write-pixel
			      pixels
			      (fix:+ px iy-index)
			      (gray-pixel (flo:vector-ref pic-row px)))
			     (x-loop (fix:+ px 1))))))
		   (y-loop (fix:- py 1) (fix:+ iy-index rect-index-height))))))

	  ((and (fix:= 2 h-sf) (fix:= 2 v-sf))
	   (let y-loop ((py py-max) (iy-index 0))
	     (if (fix:<= 0 py)
		 (let ((pic-row (vector-ref pic-data py)))
		   (let x-loop ((px 0) (ix 0))
		     (if (fix:< px pic-width)
			 (let* ((n-is-0 (fix:+ ix iy-index))
				(n-is-1 (fix:+ n-is-0 image-width))
				(v (gray-pixel (flo:vector-ref pic-row px))))
			   (write-pixel pixels n-is-0 v)
			   (write-pixel pixels (fix:+ n-is-0 1) v)
			   (write-pixel pixels n-is-1 v)
			   (write-pixel pixels (fix:+ n-is-1 1) v)
			   (x-loop (fix:+ px 1) (fix:+ ix h-sf)))
			 (y-loop (fix:- py 1) 
				 (fix:+ iy-index rect-index-height))))))))

	  ((and (fix:= 3 h-sf) (fix:= 3 v-sf))
	   (let y-loop ((py py-max) (iy-index 0))
	     (if (fix:<= 0 py)
		 (let ((pic-row (vector-ref pic-data py)))
		   (let x-loop ((px 0) (ix 0))
		     (if (fix:< px pic-width)
			 (let* ((row0 (fix:+ ix iy-index))
				(row1 (fix:+ row0 image-width))
				(row2 (fix:+ row1 image-width))
				(v (gray-pixel (flo:vector-ref pic-row px))))
			   (write-pixel pixels row0 v)
			   (write-pixel pixels (fix:+ row0 1) v)
			   (write-pixel pixels (fix:+ row0 2) v)
			   (write-pixel pixels row1 v)
			   (write-pixel pixels (fix:+ row1 1) v)
			   (write-pixel pixels (fix:+ row1 2) v)
			   (write-pixel pixels row2 v)
			   (write-pixel pixels (fix:+ row2 1) v)
			   (write-pixel pixels (fix:+ row2 2) v)
			   (x-loop (fix:+ px 1) (fix:+ ix h-sf)))
			 (y-loop (fix:- py 1) 
				 (fix:+ iy-index rect-index-height))))))))

	  ((and (fix:= 4 h-sf) (fix:= 4 v-sf))
	   (let y-loop ((py py-max) (iy-index 0))
	     (if (fix:<= 0 py)
		 (let ((pic-row (vector-ref pic-data py)))
		   (let x-loop ((px 0) (ix 0))
		     (if (fix:< px pic-width)
			 (let* ((row0 (fix:+ ix iy-index))
				(row1 (fix:+ row0 image-width))
				(row2 (fix:+ row1 image-width))
				(row3 (fix:+ row2 image-width))
				(v (gray-pixel (flo:vector-ref pic-row px))))
			   (write-pixel pixels row0 v)
			   (write-pixel pixels (fix:+ row0 1) v)
			   (write-pixel pixels (fix:+ row0 2) v)
			   (write-pixel pixels (fix:+ row0 3) v)
			   (write-pixel pixels row1 v)
			   (write-pixel pixels (fix:+ row1 1) v)
			   (write-pixel pixels (fix:+ row1 2) v)
			   (write-pixel pixels (fix:+ row1 3) v)
			   (write-pixel pixels row2 v)
			   (write-pixel pixels (fix:+ row2 1) v)
			   (write-pixel pixels (fix:+ row2 2) v)
			   (write-pixel pixels (fix:+ row2 3) v)
			   (write-pixel pixels row3 v)
			   (write-pixel pixels (fix:+ row3 1) v)
			   (write-pixel pixels (fix:+ row3 2) v)
			   (write-pixel pixels (fix:+ row3 3) v)
			   (x-loop (fix:+ px 1) (fix:+ ix h-sf)))
			 (y-loop (fix:- py 1) 
				 (fix:+ iy-index rect-index-height))))))))

	  (else 
	   (let y-loop ((py py-max) (iy-index 0))
	     (if (fix:<= 0 py)
		 (let ((pic-row (vector-ref pic-data py)))
		   (let x-loop ((px 0) (ix 0))
		     (if (fix:< px pic-width)
			 (let* ((v (gray-pixel (flo:vector-ref pic-row px)))
				(n-start (fix:+ ix iy-index))
				(n-end (fix:+ n-start rect-index-height)))
			   (let n-loop ((n n-start))
			     (if (fix:< n n-end)
				 (let ((m-end (fix:+ n h-sf)))
				   (let m-loop ((m n))
				     (if (fix:< m m-end)
					 (begin
					   (write-pixel pixels m v)
					   (m-loop (fix:+ m 1)))
					 (n-loop (fix:+ n image-width)))))
				 (x-loop (fix:+ px 1) (fix:+ ix h-sf)))))
			 (y-loop (fix:- py 1) 
				 (fix:+ iy-index rect-index-height)))))))))
    ;; Kludge: IMAGE/FILL-FROM-BYTE-VECTOR should take an argument
    ;; that specifies what color a given byte in PIXELS maps to.
    ;; OS/2 requires this information, so we supply it here.
    (if (eq? 'OS/2 microcode-id/operating-system)
	(os2-image/set-colormap image (os2-image-colormap)))
    (image/fill-from-byte-vector image pixels)
    (1d-table/put! (graphics-device/properties window) image (cons h-sf v-sf))
    image))