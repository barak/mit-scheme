;;; Procedure to build an image given a picture and the magnification factors

(declare (usual-integrations))

(define (build-image pic window h-sf v-sf pic-min pic-max)
  (let* ((colormap-size (colormap-size (get-visual-info window)))
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
		  (/ colormap-size (flo:* (flo:+ 1. 7.142e-8)  ; 1+epsilon
					  range))))) 

    ;; The range was slightly adjusted so that an illegal grey level would
    ;; never be generated. epsilon was carefully chosen so that no error would
    ;; be incurred in transforming to actual grey levels up to a colormap-size
    ;; of 2^24. In general, choose epsilon such that:
    ;;            colormap-size < (/ (1+ epsilon) epsilon)
    
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
			      (flo:floor->exact
			       (flo:* mul 
				      (flo:- (floating-vector-ref pic-row px) 
					     pic-min))))
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
				(v (flo:floor->exact
				    (flo:* mul 
					   (flo:- (floating-vector-ref pic-row px) 
						  pic-min)))))
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
				(v (flo:floor->exact
				    (flo:* mul 
					   (flo:- (floating-vector-ref pic-row px) 
						  pic-min)))))
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
				(v (flo:floor->exact
				    (flo:* mul 
					   (flo:- (floating-vector-ref pic-row px) 
						  pic-min)))))
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
			 (let* ((v (flo:floor->exact
				    (flo:* mul 
					   (flo:- (floating-vector-ref pic-row px) 
						  pic-min))))
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

