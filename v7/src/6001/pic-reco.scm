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
    (if (x-image? img)
	(x-image/destroy img))
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
	    (floating-vector-ref (picture-data picture) y) x)))))

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
	   (floating-vector-set! (floating-vector-ref (picture-data picture) y)
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
	  (let ((yth-row (floating-vector-ref picdata y)))
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
    (if (x-image? img)
	(x-image/destroy img))
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
