;; Procedures to read a file in raw pgm format into a picture

(declare (usual-integrations))

(define (pgm-file->picture filename)
  (let* ((path-name (->pathname filename))
	 (file-name (if (pathname-type path-name)
			path-name
			(if (file-exists? path-name)
			    path-name
			    (pathname-new-type path-name "pgm")))))
    
  (call-with-input-file file-name (lambda (port)
				   (get-body port (get-header port))))))

(define (get-line port result)   ; read a line of data
  (let ((c (read-char port)))
    (cond ((eof-object? c) (reverse result))
	  ((eq? c #\Linefeed) 
	   (let ((res (reverse result)))
	     (if (eq? (car res) #\#)  ; ignore comments
		 (get-line port '())
		 res)))
	  (else (get-line port (cons c result))))))

(define (get-header port)
  (let* ((type (list->string (get-line port '())))
	 (dims (list->string (get-line port '())))
	 (no-of-greys (string->number 
		       (list->string (get-line port '()))))
	 (spc-index (string-find-next-char dims #\space))
	 (length (string->number 
		  (string-head dims spc-index)))
	 (width (string->number
		 (string-tail dims (1+ spc-index)))))
    (if (not (equal? type "P5"))  ; P5 is the magic number for raw PGM format
	(error "Unrecognized format. (Convert to raw PGM) -- PICTURE-READ")
	(vector type length width no-of-greys))))

(define (get-body port attributes)
  (let* ((length (vector-ref attributes 1))
	 (width (vector-ref attributes 2))
	 (max-grey (vector-ref attributes 3))
	 (pic (make-picture length width))
	 (data (make-initialized-vector 
		width
		(lambda (index)
		  index		; ignored
		  (make-floating-vector length 0.)))))     ;initialize to blank

    (side-effecting-iter
     width
     (lambda (n)
       (let ((nth-row (floating-vector-ref data (- width n 1))))
	 (side-effecting-iter
	  length
	  (lambda (m)
	    (floating-vector-set! nth-row m 
			 (exact->inexact (char->ascii (read-char port)))))))))
    (picture-set-data! pic data)
;;    (%picture-set-min! pic 0.)    ; set min, max of picture according to
;;    (%picture-set-max! pic (exact->inexact max-grey))  ; information in file
    pic))

;;; Procedure to read in a picture that was previously saved using 
;;; picture-write.

(define (picture-read filename)
  (let* ((path-name (->pathname filename))
	 (pic-mimic (fasload (if (pathname-type path-name)
				 path-name
				 (if (file-exists? path-name)
				     path-name
				     (pathname-new-type path-name "pic"))))))
    (if (not (record? pic-mimic))
	(error "Object loaded is not a picture -- PICTURE-READ"))

    (define mimic-type (record-type-descriptor pic-mimic))

    (if (not (equal? (record-type-field-names mimic-type)
		     (record-type-field-names picture-type)))
	(error "Object loaded is not a picture -- PICTURE-READ"))
	 
    (define mimic-width (record-accessor mimic-type 'width))
    (define mimic-height (record-accessor mimic-type 'height))
    (define mimic-data (record-accessor mimic-type 'data))
    (define mimic-min (record-accessor mimic-type 'min))
    (define mimic-max (record-accessor mimic-type 'max))

    (define new-pic (make-picture (mimic-width pic-mimic) 
				  (mimic-height pic-mimic)))
    (picture-set-data! new-pic (mimic-data pic-mimic))
    (%picture-set-min! new-pic (mimic-min pic-mimic))
    (%picture-set-max! new-pic (mimic-max pic-mimic))
    new-pic))

