
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
	  (let* ((p2-yth-row (floating-vector-ref p2-data y))
		 (new-yth-row (floating-vector-ref new-data (fix:- y dn)))) 
	    (let x-loop ((x 0))
	      (if (fix:< x wid2)
		  (begin  
		    (floating-vector-set! new-yth-row (fix:- x lf) 
				 (floating-vector-ref p2-yth-row x))
		    (x-loop (fix:+ x 1)))
		  (y-loop (fix:+ y 1)))))))

    ;; overlay pic1 in its proper location in the result
    (let y-loop ((y 0))
      (if (fix:< y hgt1)
	  (let* ((p1-yth-row (floating-vector-ref p1-data y))
		 (new-yth-row (floating-vector-ref new-data
						   (fix:+ y p1y-offset)))) 
	    (let x-loop ((x 0))
	      (if (fix:< x wid1)
		  (begin  
		    (floating-vector-set! new-yth-row (fix:+ x p1x-offset) 
				 (floating-vector-ref p1-yth-row x))
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
		(let ((p1-yth-row (floating-vector-ref p1-data y))
		      (p2-yth-row (floating-vector-ref p2-data (fix:+ y v))))
		  (let x-loop ((x 0))
		    (if (fix:< x wid1)
			(begin
			  (floating-vector-set! p2-yth-row (fix:+ x u)
				       (floating-vector-ref p1-yth-row x))
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
	  (let ((new-yth-row (floating-vector-ref new-data y))
		(old-yth-row (floating-vector-ref data (fix:+ v y))))
	    (let x-loop ((x 0))
	      (if (fix:< x cut-wid)
		  (begin
		    (floating-vector-set! new-yth-row x
				 (floating-vector-ref old-yth-row (fix:+ u x)))
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
		(floating-vector-set! new-data ny
			     (floating-vector-copy
			      (floating-vector-ref new-data (fix:- ny 1))))
		(let ((yth-row (floating-vector-ref data y-index))
		      (new-yth-row (floating-vector-ref new-data ny)))
		  (let x-loop ((nx 0))
		    (if (fix:< nx wid)
			(begin
			  (floating-vector-set! new-yth-row nx
				       (floating-vector-ref yth-row 
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
			     (floating-vector-copy
			      (floating-vector-ref data n))))))

	  ((and (close-enough? (xcor ur) lx)   ; check for 
		(close-enough? (ycor ur) uy))  ; 90 degrees anti-clockwise
	   (let y-loop ((ny 0))
	     (if (fix:< ny new-hgt)
		 (let ((yth-row (floating-vector-ref new-data ny)))
		   (let x-loop ((nx 0))
		     (if (fix:< nx new-wid)
			 (begin
			   (floating-vector-set! yth-row nx
					(floating-vector-ref 
					 (floating-vector-ref
					  data (fix:- nx-max nx))
					 ny))
			   (x-loop (fix:+ nx 1)))
			 (y-loop (fix:+ ny 1))))))))

	  ((and (close-enough? (xcor ur) ux)   ; check for
		(close-enough? (ycor ur) ly))  ; 90 degrees clockwise
	   (let y-loop ((ny 0))
	     (if (fix:< ny new-hgt)
		 (let ((yth-row (floating-vector-ref new-data ny)))
		   (let x-loop ((nx 0))
		     (if (fix:< nx new-wid)
			 (begin
			   (floating-vector-set! yth-row nx
					(floating-vector-ref 
					 (floating-vector-ref data nx) 
					 (fix:- ny-max ny)))
			   (x-loop (fix:+ nx 1)))
			 (y-loop (fix:+ ny 1))))))))

	  ((and (close-enough? (xcor ur) lx)  ; check for
		(close-enough? (ycor ur) ly)) ; 180 degrees
	   (let y-loop ((ny 0))
	     (if (fix:< ny new-hgt)
		 (begin
		   (floating-vector-set! new-data ny 
				(list->vector
				 (reverse 
				  (vector->list 
				   (floating-vector-ref data
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
		   (let ((nyth-row (floating-vector-ref new-data ny)))
		     (let x-loop ((nx 0) (inner-x outer-x) (inner-y outer-y))
		       (if (fix:< nx new-wid)
			   (let ((x (round->exact inner-x))
				 (y (round->exact inner-y)))
			     (floating-vector-set! nyth-row nx
					  (if (in-rect? x y wid hgt)
					      (floating-vector-ref
					       (floating-vector-ref data y) x)
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
	    (floating-vector-set! new-data y 
			 (floating-vector-copy
			  (floating-vector-ref data (fix:- y-max y))))
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
	    (floating-vector-set! new-data y
			 (list->vector 
			  (reverse 
			   (vector->list (floating-vector-ref data y)))))
	    (y-loop (fix:+ y 1)))))
    (picture-set-data! new-pic new-data)
    new-pic))

