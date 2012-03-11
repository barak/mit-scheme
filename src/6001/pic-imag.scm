#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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
	 (use-string?
	  (for-all? (vector->list gray-map)
	    (lambda (n)
	      (<= 0 n 255))))
	 (image (image/create window image-width image-height))
	 (pixels
	  (if use-string?
	      (make-string (fix:* image-width image-height))
	      (make-vector (fix:* image-width image-height))))
	 (write-pixel (if use-string? vector-8b-set! vector-set!))
	 (py-max (- pic-height 1))
	 (rect-index-height (fix:* v-sf image-width))
	 (binner
	  (cutoff-binner .01 pic-min pic-max (vector-length gray-map)))
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